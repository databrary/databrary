ALTER TABLE "volume_access" DROP CONSTRAINT "volume_access_check_full1";
-- expanding full2 for party 0 would be too complicated
ALTER TABLE "volume_access" DROP CONSTRAINT "volume_access_check_full2";
ALTER TABLE "volume_access" ADD CONSTRAINT "volume_access_check_full3"
  CHECK ( party = -1 OR party = 0 OR share_full is null );

CREATE OR REPLACE VIEW "volume_access_view" ("volume", "party", "access", "share_full") AS
-- considering access provided to a party on a volume by multiple sources,
--  select the highest access provided from the sources
WITH
-- vap = parent party's volume access extended down
--  to some descendant child, using the appropriately capped
--  permission from the descending chain.
vap AS (
  SELECT acc.volume
       , aut.parent
       , aut.child
       , LEAST(acc.children,
               -- Below might be a shortcut to approximate parent = group 0
               --   or group 1, since PUBLIC is used with group -1 and SHARED
               --   is used with group 0. Needs confirmation.
               CASE WHEN acc.children <= 'SHARED'
               -- Use child's inherited permission to the data this antecdent group
               --  can reach
               THEN aut.site
               -- Use permission directly delegated to child on data the parent (a person)
               --  can reach
               ELSE aut.member END)
           as result_perm
       -- share_full policy value is unconditionally transferred down, as is,
       -- from parent to descendant
       , acc.share_full
  FROM volume_access acc
    JOIN authorize_view aut ON party = parent
), 
-- vap_max = parent, child combination representing the parent
--   who is providing the highest permission to a child for a given volume
vap_max AS (
  SELECT volume
       , max(parent) as mparent -- arbitrary tie breaker
       , child
  FROM vap
  WHERE NOT EXISTS
    (SELECT *
     FROM vap AS v2
     WHERE (vap.volume, vap.child) = (v2.volume, v2.child)
     AND v2.result_perm > vap.result_perm)
  GROUP BY volume, child
)
SELECT volume
     , party
     , individual
     , share_full
FROM volume_access
UNION ALL
SELECT mx.volume
     , mx.child
     , vap.result_perm
     , vap.share_full
FROM vap_max AS mx
  JOIN vap on (mx.volume, mx.mparent, mx.child) = (vap.volume, vap.parent, vap.child);

-- migrate all existing entries such that share_full = false for db community group (0)
--  when share_full = false for anonymous/nobody group (-1)
update volume_access
set share_full = false
where (party, individual, children) = (0, 'SHARED', 'SHARED')
and exists
  (select *
   from volume_access va
   where (va.volume, va.party, va.individual, va.children, va.share_full)
     = (volume_access.volume, -1, 'PUBLIC', 'PUBLIC', false));

-- migrate all existing entries such that share_full = true for db community group (0)
--  when there is no entry indicating sharing is restricted for anonymous/nobody group (-1)
update volume_access
set share_full = true
where (party, individual, children) = (0, 'SHARED', 'SHARED')
and not exists
  (select *
   from volume_access va
   where (va.volume, va.party, va.individual, va.children, va.share_full)
     = (volume_access.volume, -1, 'PUBLIC', 'PUBLIC', false));
