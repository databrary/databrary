ALTER TABLE "volume_access" DROP CONSTRAINT "volume_access_check_full1";
ALTER TABLE "volume_access" DROP CONSTRAINT "volume_access_check_full2";
ALTER TABLE "volume_access" ADD CONSTRAINT "volume_access_check_full3"
  CHECK ( party = -1 OR party = 0 OR share_full is null );
-- expanding full2 for party 0 would be too complicated

CREATE OR REPLACE VIEW "volume_access_view" ("volume", "party", "access", "share_full") AS
            -- vap = volume access with computed permission for each child
        WITH vap AS (
           SELECT volume
                , parent
                , child
                -- either
                --  1. provide permission granted to children of this parent on this volume, when this is most restrictive
                --  2. when permission to children on this volume is granted with a low level typically associated with the nobody or databrary group,
                --       then use the child's site permission
                --  3. when permission to children on this volume is granted with a higher level typically associated with a specific user,
                --       then use the child's permission on the parent's data
                , LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END) as result_perm
                -- share_full policy value is unconditionally transferred down, as is, from parent to children
                , share_full
           FROM volume_access
             JOIN authorize_view ON party = parent), 
           -- vap_max = parent, child combination representing the parent
           --   who is providing the highest permission to child for a given volume
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
           GROUP BY volume, child)
    
        SELECT volume, party, individual, share_full FROM volume_access

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
     = (volume, -1, 'PUBLIC', 'PUBLIC', false));

-- migrate all existing entries such that share_full = true for db community group (0)
--  when there is no entry indicating sharing is restricted for anonymous/nobody group (-1)
update volume_access
set share_full = true
where (party, individual, children) = (0, 'SHARED', 'SHARED')
and not exists
  (select *
   from volume_access va
   where (va.volume, va.party, va.individual, va.children, va.share_full)
     = (volume, -1, 'PUBLIC', 'PUBLIC', false));
