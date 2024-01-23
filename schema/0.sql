-- This is currently the complete, authoritative schema, as it is easier to
-- understand all in one place.

-- Whenever you make changes to this file, you must also write a new evolution
-- in evolutions/default and check the result using "tools/runsql check".
-- Note that this while this file is valid SQL, evolutions require semi-colons
-- to be doubled when they do not terminate statements.

-- A general convention is that hard-coded fixtures get non-positive ids.

----------------------------------------------------------- utilities

CREATE FUNCTION cast_int ("input" text) RETURNS integer LANGUAGE plpgsql IMMUTABLE STRICT AS $$
DECLARE
	i integer;
BEGIN
	SELECT input::integer INTO i;
	RETURN i;
EXCEPTION WHEN invalid_text_representation THEN
	RETURN NULL;
END; $$;

CREATE FUNCTION singleton (int4) RETURNS int4range LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT int4range($1, $1, '[]') $$;

----------------------------------------------------------- auditing

CREATE SCHEMA audit;

CREATE TYPE audit_action AS ENUM ('attempt', 'open', 'close', 'add', 'change', 'remove', 'superuser');
COMMENT ON TYPE audit_action IS 'The various activities for which we keep audit records (in audit or a derived table).  This is not kept in the audit schema due to jdbc type search limitations.';

CREATE FUNCTION audit.SET_PRIVILEGES (name) RETURNS void LANGUAGE plpgsql AS $set$ BEGIN
	EXECUTE $$REVOKE UPDATE, DELETE, TRUNCATE ON TABLE audit.$$ || quote_ident($1) || $$ FROM $$ || quote_ident(current_user);
END; $set$;
COMMENT ON FUNCTION audit.SET_PRIVILEGES (name) IS 'REVOKE UPDATE, DELETE, TRUNCATE ON TABLE audit.$1 FROM current_user.  Unfortunately you cannot remove default privileges per-schema, so we do this instead for each audit table.  The function is necessary to interpolate current_user.';

CREATE TABLE audit."audit" (
	"audit_time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"audit_user" int NOT NULL, -- References "account" ("party"),
	"audit_ip" inet NOT NULL,
	"audit_action" audit_action NOT NULL
) WITH (OIDS = FALSE);
COMMENT ON TABLE audit."audit" IS 'Logs of all activities on the site, including access and modifications to any data. Each table has an associated audit table inheriting from this one.';
SELECT audit.SET_PRIVILEGES ('audit');
CREATE INDEX "audit_login_idx" ON audit."audit" ("audit_user", "audit_time") WHERE "audit_action" IN ('attempt', 'open');
COMMENT ON INDEX audit."audit_login_idx" IS 'Allow efficient determination of recent login attempts for security.';

CREATE FUNCTION audit.CREATE_TABLE (name, "parent" name = 'audit') RETURNS void LANGUAGE plpgsql AS $create$
DECLARE
	table_name CONSTANT text := quote_ident($1);
BEGIN
	EXECUTE $$CREATE TABLE audit.$$ || table_name || $$ (LIKE public.$$ || table_name || $$) INHERITS (audit. $$ || quote_ident(parent) || $$) WITH (OIDS = FALSE)$$;
	PERFORM audit.SET_PRIVILEGES($1);
END; $create$;
COMMENT ON FUNCTION audit.CREATE_TABLE (name, name) IS 'Create an audit.$1 table mirroring public.$1.';

----------------------------------------------------------- users

CREATE TABLE "party" (
	"id" serial NOT NULL Primary Key,
	"sortname" text NOT NULL,
	"prename" text,
	"orcid" char(16),
	"affiliation" text,
	"url" text
);
ALTER TABLE "party"
	ALTER "sortname" SET STORAGE EXTERNAL,
	ALTER "affiliation" SET STORAGE EXTERNAL,
	ALTER "prename" SET STORAGE EXTERNAL;
COMMENT ON TABLE "party" IS 'Users, groups, organizations, and other logical identities';
COMMENT ON COLUMN "party"."orcid" IS 'http://en.wikipedia.org/wiki/ORCID';

-- special parties (SERIAL starts at 1):
INSERT INTO "party" VALUES (-1, 'Everybody'); -- NOBODY
INSERT INTO "party" VALUES (0, 'Databrary'); -- ROOT

SELECT audit.CREATE_TABLE ('party');


CREATE TABLE "account" (
	"id" integer NOT NULL Primary Key References "party",
	"email" varchar(256) NOT NULL, -- split out (multiple/user)?
	"password" varchar(60), -- standard unix-style hash, currently $2a$ bcrypt
	"openid" varchar(256) -- split out (multiple/user)?
);
ALTER TABLE "account"
	ALTER "email" SET STORAGE EXTERNAL,
	ALTER "password" SET STORAGE EXTERNAL;
COMMENT ON TABLE "account" IS 'Login information for parties associated with registered individuals.';

SELECT audit.CREATE_TABLE ('account');

----------------------------------------------------------- permissions

CREATE TYPE permission AS ENUM ('NONE', -- stand-in for NULL due to LEAST semantics
	'PUBLIC', 	-- read access to metadata and PUBLIC data
	'SHARED',	-- read access to SHARED data
	'READ', 	-- full read access to all data
	'EDIT', 	-- view and edit all data
	'ADMIN' 	-- perform administrative tasks on site/target such as changing permissions
);
COMMENT ON TYPE permission IS 'Levels of access parties can have to the site data.';

CREATE TYPE release AS ENUM (
	'PRIVATE', 	-- no sharing beyond those with full access
	'SHARED', 	-- restricted sharing to authorized users
	'EXCERPTS', 	-- SHARED, but excerpts may be shown externally
	'PUBLIC' 	-- shared publicly with anyone
);
COMMENT ON TYPE release IS 'Levels at which participants or researchers may choose to share data.';

CREATE FUNCTION "read_release" ("p" permission) RETURNS release LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE
		WHEN p >= 'READ' THEN 'PRIVATE'::release -- should include NULL
		WHEN p >= 'SHARED' THEN 'SHARED'::release
		WHEN p >= 'PUBLIC' THEN 'PUBLIC'::release
	END
$$;
COMMENT ON FUNCTION "read_release" (permission) IS 'Minimum release level readable at the given permission level.';

CREATE FUNCTION "read_permission" ("r" release) RETURNS permission LANGUAGE sql IMMUTABLE CALLED ON NULL INPUT AS $$
	SELECT CASE
		WHEN r >= 'PUBLIC' THEN 'PUBLIC'::permission
		WHEN r >= 'SHARED' THEN 'SHARED'::permission
		ELSE 'READ'::permission
	END
$$;
COMMENT ON FUNCTION "read_permission" (release) IS 'Minimum permission level to read a data object with the given release.';

CREATE FUNCTION "check_permission" ("p" permission, "r" release) RETURNS boolean LANGUAGE sql IMMUTABLE AS $$
	SELECT p >= read_permission(r)
$$;
COMMENT ON FUNCTION "check_permission" (permission, release) IS 'Effective permission level on a data object with the given permission level and release.';

CREATE TABLE "authorize" (
	"child" integer NOT NULL References "party" ON DELETE CASCADE,
	"parent" integer NOT NULL References "party",
	"site" permission NOT NULL DEFAULT 'NONE',
	"member" permission NOT NULL DEFAULT 'NONE',
	"expires" timestamp,
	Primary Key ("parent", "child"),
	Check ("child" <> "parent" AND "child" > 0 AND "parent" >= 0)
);
COMMENT ON TABLE "authorize" IS 'Relationships and permissions granted between parties';
COMMENT ON COLUMN "authorize"."child" IS 'Party granted permissions';
COMMENT ON COLUMN "authorize"."parent" IS 'Party granting permissions';
COMMENT ON COLUMN "authorize"."site" IS 'Level of site access granted to child, inherited (but degraded) from parent';
COMMENT ON COLUMN "authorize"."member" IS 'Level of permission granted to the child as a member of the parent''s group';

SELECT audit.CREATE_TABLE ('authorize');
CREATE INDEX "authorize_activity_idx" ON audit."authorize" ("audit_time" DESC) WHERE "audit_action" IN ('add', 'change') AND "site" > 'NONE';

CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize
		UNION
		SELECT a.child, aa.parent, CASE
		         WHEN aa.site = 'ADMIN' THEN LEAST(a.site, 'EDIT')
			 WHEN aa.site = 'EDIT' THEN LEAST(a.site, 'READ')
			 ELSE LEAST(aa.site, a.site, 'PUBLIC')
		       END, 'NONE', LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent
	) SELECT * FROM aa
	UNION ALL SELECT id, id, 'ADMIN', 'ADMIN', NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, 'ADMIN', 'NONE', NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");

CREATE FUNCTION "authorize_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "authorize_inherit";
	RETURN null;
END; $$;
CREATE TRIGGER "authorize_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "authorize" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();
CREATE TRIGGER "party_created" AFTER INSERT ON "party" FOR EACH STATEMENT EXECUTE PROCEDURE "authorize_refresh" ();

CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP;
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';

CREATE VIEW "authorize_view" ("child", "parent", "site", "member") AS
	SELECT child, parent, MAX(site), MAX(member)
	  FROM authorize_inherit
	 WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';


----------------------------------------------------------- volumes

CREATE TABLE "volume" (
	"id" serial NOT NULL Primary Key,
	"name" text NOT NULL,
	"body" text,
	"alias" varchar(64)
);
ALTER TABLE "volume"
	ALTER "name" SET STORAGE EXTERNAL,
	ALTER "alias" SET STORAGE EXTERNAL;
COMMENT ON TABLE "volume" IS 'Basic organizational unit for data.';
COMMENT ON COLUMN "volume"."alias" IS 'Short, internal, code name for this volume, for contributors to reference their own data.';

SELECT audit.CREATE_TABLE ('volume');
ALTER TABLE audit."volume" ALTER "name" DROP NOT NULL;
CREATE INDEX "volume_creation_idx" ON audit."volume" ("id") WHERE "audit_action" = 'add';
COMMENT ON INDEX audit."volume_creation_idx" IS 'Allow efficient retrieval of volume creation information, specifically date.';

CREATE FUNCTION "volume_creation" ("volume" integer) RETURNS timestamp LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."volume" WHERE "id" = $1 AND "audit_action" = 'add' $$;

CREATE TABLE "volume_access" (
	"volume" integer NOT NULL References "volume",
	"party" integer NOT NULL References "party",
	"individual" permission NOT NULL DEFAULT 'NONE',
	"children" permission NOT NULL DEFAULT 'NONE',
	Check ("individual" >= "children"),
	Primary Key ("volume", "party")
);
COMMENT ON TABLE "volume_access" IS 'Permissions over volumes assigned to users.';

SELECT audit.CREATE_TABLE ('volume_access');
CREATE INDEX "volume_share_activity_idx" ON audit."volume_access" ("audit_time" DESC) WHERE "audit_action" = 'add' AND "party" = 0 AND "children" > 'NONE';

CREATE VIEW "volume_access_view" ("volume", "party", "access") AS
	SELECT volume, party, individual FROM volume_access
	UNION ALL
	SELECT volume, child, MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END))
	  FROM volume_access JOIN authorize_view ON party = parent GROUP BY volume, child;
COMMENT ON VIEW "volume_access_view" IS 'Expanded list of effective volume access.';

CREATE FUNCTION "volume_access_check" ("volume" integer, "party" integer) RETURNS permission LANGUAGE sql STABLE AS $$
	SELECT access FROM volume_access_view WHERE volume = $1 AND party = $2 LIMIT 1
$$;
COMMENT ON FUNCTION "volume_access_check" (integer, integer) IS 'Permission level the party has on the given volume, either directly, delegated, or inherited.';

CREATE TABLE "volume_doi" (
	"volume" integer NOT NULL Unique References "volume",
	"doi" varchar(16) NOT NULL Unique
);
COMMENT ON TABLE "volume_doi" IS 'DOIs issued for volumes (currently via EZID).';

CREATE TABLE "volume_link" (
	"volume" integer NOT NULL References "volume",
	"head" text NOT NULL,
	"url" text NOT NULL,
	Unique ("volume", "url")
);
COMMENT ON TABLE "volume_link" IS 'Links from volumes to externals resources.';

SELECT audit.CREATE_TABLE ('volume_link');

CREATE TABLE "volume_citation" (
	"volume" integer NOT NULL Unique References "volume",
	"head" text NOT NULL,
	"url" text,
	"year" smallint Check ("year" BETWEEN 1900 AND 2900)
); -- INHERITS ("volume_link");
COMMENT ON TABLE "volume_citation" IS 'Publications/products corresponding to study volumes.';

SELECT audit.CREATE_TABLE ('volume_citation');

CREATE TABLE "funder" (
	"fundref_id" bigint NOT NULL Primary Key,
	"name" text NOT NULL,
	"party" integer References "party"
);
COMMENT ON TABLE "funder" IS 'Sources of funding, basically a mirror of fundref data, with local party associations (primarily for transition).';
COMMENT ON COLUMN "funder"."fundref_id" IS 'Identifiers from fundref.org, under the http://dx.doi.org/10.13039/ namespace. Specifications suggest these may not be numeric, but they seem to be.';

CREATE TABLE "volume_funding" (
	"volume" integer NOT NULL References "volume",
	"funder" bigint NOT NULL References "funder",
	"awards" text[] NOT NULL Default '{}',
	Primary Key ("volume", "funder")
);
COMMENT ON TABLE "volume_funding" IS 'Funding sources associated with a volume, based on fundref.org.';
COMMENT ON COLUMN "volume_funding"."awards" IS 'Individual grant identifiers associated with this funder.';

----------------------------------------------------------- time intervals

CREATE FUNCTION "interval_mi_epoch" (interval, interval) RETURNS double precision LANGUAGE sql IMMUTABLE STRICT AS 
	$$ SELECT date_part('epoch', interval_mi($1, $2)) $$;
CREATE TYPE segment AS RANGE (
	SUBTYPE = interval HOUR TO SECOND (3),
	SUBTYPE_DIFF = "interval_mi_epoch"
);
COMMENT ON TYPE "segment" IS 'Intervals of time, used primarily for representing clips of timeseries data.';

CREATE FUNCTION "segment" (interval) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT segment('0', $1) $$;
COMMENT ON FUNCTION "segment" (interval) IS 'The segment [0,X) but strict in X.';
CREATE FUNCTION "duration" (segment) RETURNS interval HOUR TO SECOND (3) LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT CASE WHEN isempty($1) THEN '0' ELSE interval_mi(upper($1), lower($1)) END $$;
COMMENT ON FUNCTION "duration" (segment) IS 'Determine the length of a segment, or NULL if unbounded.';
CREATE FUNCTION "singleton" (interval HOUR TO SECOND (3)) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT segment($1, $1, '[]') $$;
CREATE FUNCTION "singleton" (segment) RETURNS interval LANGUAGE sql IMMUTABLE STRICT AS
	$$ SELECT lower($1) WHERE lower_inc($1) AND upper_inc($1) AND lower($1) = upper($1) $$;
COMMENT ON FUNCTION "singleton" (segment) IS 'Determine if a segment represents a single point and return it, or NULL if not.';

CREATE FUNCTION "segment_shift" (segment, interval) RETURNS segment LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE WHEN isempty($1) THEN 'empty' ELSE
		segment(lower($1) + $2, upper($1) + $2,
			CASE WHEN lower_inc($1) THEN '[' ELSE '(' END || CASE WHEN upper_inc($1) THEN ']' ELSE ')' END)
	END
$$;
COMMENT ON FUNCTION "segment_shift" (segment, interval) IS 'Shift both end points of a segment by the specified interval.';

-- this probably needs an order by segment for most uses:
CREATE AGGREGATE "segment_union" (segment) (SFUNC = range_union, STYPE = segment, INITCOND = 'empty');
CREATE AGGREGATE "segment_intersect" (segment) (SFUNC = range_intersect, STYPE = segment, INITCOND = '(,)');

CREATE FUNCTION "segments" (segment) RETURNS segment[] LANGUAGE sql IMMUTABLE STRICT AS $$
	SELECT CASE WHEN isempty($1) THEN ARRAY[]::segment[] ELSE ARRAY[$1] END
$$;
-- this needs to be done as SU, so we use a placeholder:
CREATE FUNCTION segments_union(segment[], segment[]) RETURNS segment[] IMMUTABLE STRICT LANGUAGE -- C AS 'pgranges.so', 'ranges_union';
	sql AS $$ SELECT NULL::segment[] $$;
CREATE FUNCTION segments_union(segment[], segment) RETURNS segment[] IMMUTABLE STRICT LANGUAGE -- C AS 'pgranges.so', 'ranges_union1';
	sql AS $$ SELECT segments_union($1, segments($2)) $$;
CREATE AGGREGATE "segments_union" (segment) (SFUNC = segments_union, STYPE = segment[], INITCOND = '{}');

----------------------------------------------------------- containers

CREATE TABLE "container" (
	"id" serial NOT NULL Primary Key,
	"volume" integer NOT NULL References "volume",
	"top" boolean NOT NULL DEFAULT false,
	"name" text,
	"date" date
);
ALTER TABLE "container"
	ALTER "name" SET STORAGE EXTERNAL;
CREATE INDEX ON "container" ("volume");
CREATE INDEX "container_top_idx" ON "container" ("volume") WHERE "top";
COMMENT ON TABLE "container" IS 'Organizational unit within volume containing related files (with common annotations), often corresponding to an individual data session (single visit/acquisition/participant/group/day).';

SELECT audit.CREATE_TABLE ('container');

CREATE FUNCTION "container_top_create" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO container (volume, top) VALUES (NEW.id, true);
	RETURN null;
END; $$;
CREATE TRIGGER "container_top_create" AFTER INSERT ON "volume" FOR EACH ROW EXECUTE PROCEDURE "container_top_create" ();
COMMENT ON TRIGGER "container_top_create" ON "volume" IS 'Always create a top container for each volume.  Unfortunately nothing currently prevents them from being removed/changed.';


----------------------------------------------------------- slots

CREATE TABLE "slot" ( -- ABSTRACT
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	Check (false) NO INHERIT
);
ALTER TABLE "slot"
	ALTER "segment" SET STORAGE PLAIN;
COMMENT ON TABLE "slot" IS 'Generic table for objects associated with a temporal sub-sections of a container.  Inherit from this table to use the functions below.';

SELECT audit.CREATE_TABLE ('slot');

CREATE TABLE "slot_release" (
	"container" integer NOT NULL References "container" ON DELETE CASCADE,
	"segment" segment NOT NULL,
	"release" release NOT NULL,
	Primary Key ("container", "segment"),
	Exclude USING gist (singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
COMMENT ON TABLE "slot_release" IS 'Sharing/release permissions granted by participants on (portions of) contained data.';

SELECT audit.CREATE_TABLE ('slot_release', 'slot');


----------------------------------------------------------- studies

CREATE TABLE "volume_inclusion" (
	"volume" integer NOT NULL References "volume",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("volume", "container") -- this may be too strong but seems reasonable
) INHERITS ("slot");
COMMENT ON TABLE "volume_inclusion" IS 'Inclusions of slots (sessions) from "dataset" (provider) volumes in "study" (consumer/reuse) volumes.';

SELECT audit.CREATE_TABLE ('volume_inclusion', 'slot');


----------------------------------------------------------- assets

CREATE TABLE "format" (
	"id" smallserial NOT NULL Primary Key,
	"mimetype" varchar(128) NOT NULL Unique,
	"extension" varchar(8),
	"name" text NOT NULL
);
ALTER TABLE "format"
	ALTER "mimetype" SET STORAGE EXTERNAL,
	ALTER "extension" SET STORAGE EXTERNAL;
COMMENT ON TABLE "format" IS 'Possible types for assets, sufficient for producing download headers.';

-- The above video format will change to reflect internal storage, these are used for uploaded files:
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/plain', 'txt', 'Plain text');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/csv', 'csv', 'Comma-separated values');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/html', 'html', 'Hypertext markup');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/rtf', 'rtf', 'Rich text format');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('image/png', 'png', 'Portable network graphics');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/pdf', 'pdf', 'Portable document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/msword', 'doc', 'Microsoft Word document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.text', 'odf', 'OpenDocument text');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.wordprocessingml.document', 'docx', 'Microsoft Word (Office Open XML) document');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-excel', 'xls', 'Microsoft Excel spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.spreadsheet', 'ods', 'OpenDocument spreadsheet');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'xlsx', 'Microsoft Excel (Office Open XML) workbook');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.ms-powerpoint', 'ppt', 'Microsoft PowerPoint presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.oasis.opendocument.presentation', 'odp', 'OpenDocument presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.openxmlformats-officedocument.presentationml.presentation', 'pptx', 'Microsoft PowerPoint (Office Open XML) presentation');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/vnd.datavyu', 'opf', 'Datavyu');
SELECT nextval('format_id_seq'); -- placeholder for old video/mp4
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/webm', 'webm', 'WebM video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mpeg', 'mpg', 'MPEG program stream (MPEG-1/MPEG-2 video)');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/quicktime', 'mov', 'QuickTime video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/mp2t', 'mts', 'MPEG transport stream');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/avi', 'avi', 'Audio Video Interleave');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('application/x-spss-sav', 'sav', 'SPSS System File');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('audio/wav', 'wav', 'Waveform audio');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('video/x-ms-wmv', 'wmv', 'Windows Media Video');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('text/x-chat', 'cha', 'Codes for the Human Analysis of Transcripts');

-- The privledged formats with special handling (image and video for now) have hard-coded IDs:
INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-800, 'video/mp4', 'mp4', 'MPEG-4 video');
INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-700, 'image/jpeg', 'jpg', 'JPEG image');

CREATE TABLE "asset" (
	"id" serial Primary Key,
	"volume" integer NOT NULL References "volume",
	"format" smallint NOT NULL References "format",
	"release" release,
	"duration" interval HOUR TO SECOND (3) Check ("duration" > interval '0'),
	"name" text,
	"sha1" bytea Check (octet_length("sha1") = 20),
	"size" bigint Check ("size" >= 0)
);
ALTER TABLE "asset"
	ALTER "name" SET STORAGE EXTERNAL,
	ALTER "sha1" SET STORAGE EXTERNAL;
COMMENT ON TABLE "asset" IS 'Assets reflecting files in primary storage.';

SELECT audit.CREATE_TABLE ('asset');
ALTER TABLE audit."asset" ALTER "sha1" DROP NOT NULL;

CREATE INDEX "asset_creation_idx" ON audit."asset" ("id") WHERE "audit_action" = 'add';
COMMENT ON INDEX audit."asset_creation_idx" IS 'Allow efficient retrieval of asset creation information, specifically date.';

CREATE TABLE "slot_asset" (
	"asset" integer NOT NULL Primary Key References "asset",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL
) INHERITS ("slot");
CREATE INDEX "slot_asset_slot_idx" ON "slot_asset" ("container", "segment");
COMMENT ON TABLE "slot_asset" IS 'Attachment point of assets, which, in the case of timeseries data, should match asset.duration.';

SELECT audit.CREATE_TABLE ('slot_asset', 'slot');

CREATE TABLE "asset_revision" (
	"orig" integer NOT NULL References "asset" ON DELETE CASCADE,
	"asset" integer Unique NOT NULL References "asset" ON DELETE CASCADE
	-- Check ("orig" < "asset") -- this would be nice, but we have some ingests that were done the other way
);
COMMENT ON TABLE "asset_revision" IS 'Assets that reflect different versions of the same content, either generated automatically from reformatting or a replacement provided by the user.';

CREATE RECURSIVE VIEW "asset_revisions" ("orig", "asset") AS
	SELECT * FROM asset_revision
	UNION
	SELECT o.orig, a.asset FROM asset_revision o JOIN asset_revisions a ON o.asset = a.orig;
COMMENT ON VIEW "asset_revisions" IS 'Transitive closure of asset_revision.';

CREATE FUNCTION "asset_supersede" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM asset FROM asset_revision WHERE orig = asset_new;
	IF FOUND THEN
		RAISE 'Asset % already superseded', asset_new;
	END IF;
	INSERT INTO asset_revision VALUES (asset_old, asset_new);
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;
END; $$;


CREATE TABLE "excerpt" (
	"asset" integer NOT NULL References "slot_asset" ON UPDATE CASCADE ON DELETE CASCADE,
	"segment" segment NOT NULL Check (NOT isempty("segment")),
	"release" release,
	Primary Key ("asset", "segment"),
	Exclude USING gist (singleton("asset") WITH =, "segment" WITH &&)
);
ALTER TABLE "excerpt"
	ALTER "segment" SET STORAGE PLAIN;
COMMENT ON TABLE "excerpt" IS 'Asset segments that have been selected for rerelease and/or top-level display.';
COMMENT ON COLUMN "excerpt"."segment" IS 'Segment within slot_asset.container space (not asset).';
COMMENT ON COLUMN "excerpt"."release" IS 'Override (by relaxing only) asset''s original release.';

SELECT audit.CREATE_TABLE ('excerpt');

CREATE FUNCTION "excerpt_shift" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	-- if we ever support "stretching" timeseries this will be wrong
	shift interval := lower(NEW.segment) - lower(OLD.segment);
BEGIN
	IF NEW.segment = OLD.segment THEN
	ELSIF shift IS NULL THEN
		DELETE FROM excerpt WHERE asset = NEW.asset AND segment <> '(,)';
	ELSE
		UPDATE excerpt SET segment = segment_shift(segment, shift) WHERE asset = NEW.asset;
	END IF;
	RETURN null;
END; $$;
CREATE TRIGGER "excerpt_shift" AFTER UPDATE OF "segment" ON "slot_asset" FOR EACH ROW EXECUTE PROCEDURE "excerpt_shift" ();
COMMENT ON TRIGGER "excerpt_shift" ON "slot_asset" IS 'Move or clear excerpts on repositioning of asset, just based on lower bound.';


CREATE TABLE "transcode" (
	"asset" integer NOT NULL Primary Key References "asset" ON DELETE CASCADE,
	"owner" integer NOT NULL References "party",
	"orig" integer NOT NULL References "asset" ON DELETE CASCADE,
	"segment" segment NOT NULL Default '(,)',
	"options" text[] NOT NULL Default '{}',
	"start" timestamp Default now(),
	"process" integer,
	"log" text
) INHERITS ("asset_revision");
COMMENT ON TABLE "transcode" IS 'Format conversions that are being or have been applied to transform in input asset.';

----------------------------------------------------------- comments

CREATE TABLE "comment" (
	"id" serial NOT NULL Primary Key,
	"who" integer NOT NULL References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	"time" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"text" text NOT NULL,
	"parent" integer References "comment",
	Check ("parent" < "id")
) INHERITS ("slot");
CREATE INDEX "comment_slot_idx" ON "comment" ("container", "segment");
CREATE INDEX ON "comment" ("who");
CREATE INDEX ON "comment" ("parent");
COMMENT ON TABLE "comment" IS 'Free-text comments on objects (unaudited, immutable).';

CREATE MATERIALIZED VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
CREATE INDEX "comment_thread_slot_idx" ON "comment_thread" ("container", "segment");
CREATE INDEX ON "comment_thread" ("who");
COMMENT ON MATERIALIZED VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

CREATE FUNCTION "comment_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	REFRESH MATERIALIZED VIEW "comment_thread";
	RETURN null;
END; $$;
CREATE TRIGGER "comment_changed" AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON "comment" FOR EACH STATEMENT EXECUTE PROCEDURE "comment_refresh" ();

----------------------------------------------------------- tags

CREATE TABLE "tag" (
	"id" serial NOT NULL Primary Key,
	"name" varchar(32) NOT NULL Unique Check ("name" ~ '^[a-z][-a-z ]+[a-z]$')
);
ALTER TABLE "tag"
	ALTER "name" SET STORAGE EXTERNAL;
COMMENT ON TABLE "tag" IS 'Tag/keywords that can be applied to objects.';

CREATE FUNCTION "get_tag" ("tag_name" varchar(32)) RETURNS integer STRICT LANGUAGE plpgsql AS $$
DECLARE
	tag_id integer;
BEGIN
	LOOP
		SELECT id INTO tag_id FROM tag WHERE name = tag_name;
		IF FOUND THEN
			RETURN tag_id;
		END IF;
		BEGIN
			INSERT INTO tag (name) VALUES (tag_name) RETURNING id INTO tag_id;
			RETURN tag_id;
		EXCEPTION WHEN unique_violation THEN
		END;
	END LOOP;
END; $$;


CREATE TABLE "tag_use" (
	"tag" integer NOT NULL References "tag",
	"who" integer References "account",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("tag", "who", "container", "segment"),
	Exclude USING gist (singleton("tag") WITH =, singleton("who") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX ON "tag_use" ("who");
CREATE INDEX "tag_use_slot_idx" ON "tag_use" ("container", "segment");
COMMENT ON TABLE "tag_use" IS 'Applications of tags to slots.';

CREATE TABLE "keyword_use" (
	Primary Key ("tag", "container", "segment"),
	Exclude USING gist (singleton("tag") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("tag_use");
CREATE INDEX "keyword_use_slot_idx" ON "keyword_use" ("container", "segment");
COMMENT ON TABLE "keyword_use" IS 'Special "keyword" tags editable as volume data.';


----------------------------------------------------------- records

CREATE TABLE "record_category" (
	"id" smallserial Primary Key,
	"name" varchar(64) NOT NULL Unique
);
ALTER TABLE "record_category"
	ALTER "name" SET STORAGE EXTERNAL;
COMMENT ON TABLE "record_category" IS 'Types of records that are relevant for data organization.';
INSERT INTO "record_category" ("id", "name") VALUES (-500, 'participant');
INSERT INTO "record_category" ("id", "name") VALUES (-200, 'group');
INSERT INTO "record_category" ("id", "name") VALUES (-800, 'pilot');
INSERT INTO "record_category" ("id", "name") VALUES (-700, 'exclusion');
INSERT INTO "record_category" ("id", "name") VALUES (-400, 'condition');
INSERT INTO "record_category" ("id", "name") VALUES (-300, 'task');
INSERT INTO "record_category" ("id", "name") VALUES (-100, 'context');

CREATE TABLE "record" (
	"id" serial NOT NULL Primary Key,
	"volume" integer NOT NULL References "volume",
	"category" smallint References "record_category" ON UPDATE CASCADE ON DELETE SET NULL
);
CREATE INDEX ON "record" ("volume");
COMMENT ON TABLE "record" IS 'Sets of metadata measurements organized into or applying to a single cohesive unit.  These belong to the object(s) they''re attached to, which are expected to be within a single volume.';

SELECT audit.CREATE_TABLE ('record');

CREATE TYPE data_type AS ENUM ('text', 'number', 'date');
COMMENT ON TYPE data_type IS 'Types of measurement data corresponding to measure_* tables.';

CREATE TABLE "metric" (
	"id" serial Primary Key,
	"name" varchar(64) NOT NULL Unique,
	"release" release,
	"type" data_type NOT NULL,
	"options" text[],
	"assumed" text
);
ALTER TABLE "metric"
	ALTER "name" SET STORAGE EXTERNAL;
COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.';
COMMENT ON COLUMN "metric"."options" IS '(Suggested) options for text enumerations, not enforced.';
INSERT INTO "metric" ("id", "name", "release", "type") VALUES (-900, 'ID', 'EXCERPTS', 'text');
INSERT INTO "metric" ("id", "name", "type") VALUES (-590, 'birthdate', 'date');
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-550, 'race', 'EXCERPTS', 'text', ARRAY['American Indian or Alaska Native','Asian','Native Hawaiian or Other Pacific Islander','Black or African American','White','Multiple']);
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-540, 'ethnicity', 'EXCERPTS', 'text', ARRAY['Not Hispanic or Latino','Hispanic or Latino']);
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-580, 'gender', 'EXCERPTS', 'text', ARRAY['Female','Male']);
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-140, 'state', 'EXCERPTS', 'text', ARRAY['AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','MD','MA','MI','MN','MS','MO','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']);
INSERT INTO "metric" ("id", "name", "release", "type") VALUES (-90, 'info', 'PUBLIC', 'text');
INSERT INTO "metric" ("id", "name", "release", "type") VALUES (-600, 'description', 'PUBLIC', 'text');
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-700, 'reason', 'EXCERPTS', 'text', ARRAY['Did not meet inclusion criteria','Procedural/experimenter error','Withdrew/fussy/tired','Outlier']);
INSERT INTO "metric" ("id", "name", "release", "type", "options") VALUES (-180, 'setting', 'PUBLIC', 'text', ARRAY['Lab','Home','Classroom','Outdoor','Clinic']);
INSERT INTO "metric" ("id", "name", "release", "type") VALUES (-650, 'summary', 'PUBLIC', 'text');
INSERT INTO "metric" ("id", "name", "type", "assumed") VALUES (-520, 'disability', 'text', 'typical');
INSERT INTO "metric" ("id", "name", "release", "type", "assumed") VALUES (-510, 'language', 'EXCERPTS', 'text', 'English');
INSERT INTO "metric" ("id", "name", "release", "type", "assumed") VALUES (-150, 'country', 'EXCERPTS', 'text', 'US');

CREATE TABLE "record_template" (
	"category" smallint References "record_category" ON UPDATE CASCADE ON DELETE CASCADE,
	"metric" integer References "metric" ON UPDATE CASCADE ON DELETE CASCADE,
	"ident" boolean NOT NULL Default false,
	Primary Key ("category", "metric")
);
COMMENT ON TABLE "record_template" IS 'Default set of measures defining a given record category.';
INSERT INTO "record_template" VALUES (-500, -590);
INSERT INTO "record_template" VALUES (-500, -580);
INSERT INTO "record_template" VALUES (-500, -550);
INSERT INTO "record_template" VALUES (-500, -540);
INSERT INTO "record_template" VALUES (-300, -600);
INSERT INTO "record_template" VALUES (-500, -900, true);
INSERT INTO "record_template" VALUES (-200, -900, true);
INSERT INTO "record_template" VALUES (-400, -900, true);
INSERT INTO "record_template" VALUES (-300, -900, true);
INSERT INTO "record_template" VALUES (-700, -700, true);
INSERT INTO "record_template" VALUES (-100, -180, true);
INSERT INTO "record_template" VALUES (-100, -140, true);
INSERT INTO "record_template" VALUES (-100, -150, true);

CREATE TABLE "measure" ( -- ABSTRACT
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = table_name
	Primary Key ("record", "metric"),
	Check (false) NO INHERIT
);
COMMENT ON TABLE "measure" IS 'Abstract parent of all measure tables containing data values.';

CREATE TABLE "measure_text" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "text"
	"datum" text NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_number" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "number"
	"datum" numeric NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

CREATE TABLE "measure_date" (
	"record" integer NOT NULL References "record" ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE, -- WHERE kind = "date"
	"datum" date NOT NULL,
	Primary Key ("record", "metric")
) INHERITS ("measure");

SELECT audit.CREATE_TABLE ('measure');
ALTER TABLE audit."measure" ADD "datum" text NOT NULL;

CREATE FUNCTION audit."measure_i" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	IF NEW.audit_time IS NULL THEN
		NEW.audit_time := CURRENT_TIMESTAMP;
	END IF;
	INSERT INTO audit."measure" SELECT NEW.*;
	RETURN NEW;
END; $$;
CREATE VIEW audit."measure_text" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_text" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_number" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_number" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_date" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_date" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();

CREATE VIEW "measure_view" AS
	SELECT record, metric, datum FROM measure_text UNION ALL
	SELECT record, metric, text(datum) FROM measure_number UNION ALL
	SELECT record, metric, text(datum) FROM measure_date;
COMMENT ON VIEW "measure_view" IS 'Data from all measure tables, coerced to text.';

CREATE VIEW "measures" ("record", "measures") AS
	SELECT record, array_agg(metric || ':' || datum ORDER BY metric) FROM measure_view GROUP BY record;
COMMENT ON VIEW "measures" IS 'All measures for each record aggregated into a single array.';

CREATE VIEW "measure_all" ("record", "metric", "datum_text", "datum_number", "datum_date") AS
	SELECT record, metric, datum, NULL::numeric, NULL::date FROM measure_text UNION ALL
	SELECT record, metric, NULL, datum, NULL FROM measure_number UNION ALL
	SELECT record, metric, NULL, NULL, datum FROM measure_date;
COMMENT ON VIEW "measure_all" IS 'Data from all measure tables, coerced to text.';

CREATE TABLE "record_measures" (
	"id" integer NOT NULL Primary Key References "record" ON UPDATE CASCADE ON DELETE CASCADE,
	"volume" integer NOT NULL,
	"category" smallint,
	"measures" text[] NOT NULL Default '{}'
);
CREATE INDEX ON "record_measures" ("volume");
COMMENT ON TABLE "record_measures" IS 'Automatically updated table representing "record JOIN measures".';

-- record_measures triggers
CREATE FUNCTION "record_measures_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	TRUNCATE record_measures;
	INSERT INTO record_measures SELECT id, volume, category, COALESCE(measures, '{}') FROM record LEFT JOIN measures ON id = record;
	RETURN null;
END; $$;

CREATE FUNCTION "record_measures_ri" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	INSERT INTO record_measures SELECT NEW.*;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ri" ();

CREATE FUNCTION "record_measures_ru" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET volume = NEW.volume, category = NEW.category WHERE id = NEW.id;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "record" FOR EACH ROW EXECUTE PROCEDURE "record_measures_ru" ();

CREATE FUNCTION "record_measures_mi" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = NEW.record;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();
CREATE TRIGGER "record_measures_i" AFTER INSERT ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mi" ();

CREATE FUNCTION "record_measures_mu" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id IN (OLD.record, NEW.record);
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();
CREATE TRIGGER "record_measures_u" AFTER UPDATE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_mu" ();

CREATE FUNCTION "record_measures_md" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	UPDATE record_measures SET measures = '{}' WHERE id = OLD.record;
	UPDATE record_measures SET measures = measures.measures FROM measures WHERE record = id AND id = OLD.record;
	RETURN null;
END; $$;
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_text" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_number" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();
CREATE TRIGGER "record_measures_d" AFTER DELETE ON "measure_date" FOR EACH ROW EXECUTE PROCEDURE "record_measures_md" ();

CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_text" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_number" EXECUTE PROCEDURE "record_measures_refresh" ();
CREATE TRIGGER "record_measures_t" AFTER TRUNCATE ON "measure_date" EXECUTE PROCEDURE "record_measures_refresh" ();


CREATE TABLE "slot_record" (
	"record" integer NOT NULL References "record",
	"container" integer NOT NULL References "container",
	"segment" segment NOT NULL,
	Primary Key ("record", "container", "segment"),
	Exclude USING gist (singleton("record") WITH =, singleton("container") WITH =, "segment" WITH &&)
) INHERITS ("slot");
CREATE INDEX "slot_record_slot_idx" ON "slot_record" ("container", "segment");
COMMENT ON TABLE "slot_record" IS 'Attachment of records to slots.';

SELECT audit.CREATE_TABLE ('slot_record', 'slot');

CREATE FUNCTION "record_release" ("record" integer) RETURNS release LANGUAGE sql STABLE STRICT AS
	$$ SELECT MAX(release) FROM slot_record JOIN slot_release ON slot_record.container = slot_release.container AND slot_record.segment <@ slot_release.segment WHERE record = $1 $$;
COMMENT ON FUNCTION "record_release" (integer) IS 'Effective (maximal) release level granted on the specified record.';

CREATE FUNCTION "record_release_party" ("record" integer, "party" integer) RETURNS boolean LANGUAGE sql STABLE STRICT AS
	$$ SELECT check_permission(volume_access_check(volume, $2), record_release($1)) FROM record WHERE id = $1 $$;
COMMENT ON FUNCTION "record_release_party" (integer, integer) IS 'This function has no (good) reason for existing.';

CREATE FUNCTION "record_daterange" ("record" integer) RETURNS daterange LANGUAGE sql STABLE STRICT AS $$
	SELECT daterange(min(date), max(date), '[]') 
	  FROM slot_record
	  JOIN container ON slot_record.container = container.id
	 WHERE record = $1
$$;
COMMENT ON FUNCTION "record_daterange" (integer) IS 'Range of container dates covered by the given record.';

----------------------------------------------------------- tokens

CREATE TABLE "token" (
	"token" char(32) Primary Key,
	"expires" timestamp NOT NULL,
	Check (false) NO INHERIT
);
ALTER TABLE "token"
	ALTER "token" SET STORAGE EXTERNAL;
COMMENT ON TABLE "token" IS 'Generic tokens issued to automatically perform actions such as logins or authorizations.';

CREATE TABLE "account_token" (
	"token" char(32) Primary Key,
	"expires" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP + interval '1 week',
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	Check (false) NO INHERIT
) INHERITS ("token");
COMMENT ON TABLE "account_token" IS 'Generic tokens associated with particular accounts.';

CREATE TABLE "login_token" (
	"token" char(32) Primary Key,
	"expires" timestamp NOT NULL,
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	"password" boolean NOT NULL DEFAULT false
) INHERITS ("account_token");
CREATE UNIQUE INDEX "login_token_account_idx" ON "login_token" ("account") WHERE "password";
COMMENT ON TABLE "login_token" IS 'Tokens issued to automatically login/register users or reset passwords.';

CREATE TABLE "session" (
	"token" char(32) Primary Key,
	"expires" timestamp NOT NULL,
	"account" integer NOT NULL References "account" ON DELETE CASCADE
) INHERITS ("account_token");
COMMENT ON TABLE "session" IS 'Tokens associated with currently logged-in sessions.';

CREATE TABLE "upload" (
	"token" char(32) Primary Key,
	"expires" timestamp NOT NULL,
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	"filename" text NOT NULL
) INHERITS ("account_token");
COMMENT ON TABLE "upload" IS 'Tokens issued to track active uploads.';

----------------------------------------------------------- avatars

CREATE TABLE "avatar" (
	"party" integer NOT NULL Primary Key References "party" ON DELETE CASCADE,
	"asset" integer NOT NULL References "asset"
);
COMMENT ON TABLE "avatar" IS 'Image assets used to represent parties on the site.  These assets are expected to be in the CORE volume.';

SELECT audit.CREATE_TABLE ('avatar');

----------------------------------------------------------- search

CREATE AGGREGATE "tsvector_agg" (tsvector) (SFUNC = tsvector_concat, STYPE = tsvector, INITCOND = '');

CREATE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || sortname FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.release >= 'EXCERPTS'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here
COMMENT ON VIEW "volume_text" IS 'All (searchable) text data associated with a volume.';

CREATE TABLE "volume_text_idx" (
	"volume" integer Primary Key References "volume" ON DELETE CASCADE ON UPDATE CASCADE,
	"ts" tsvector NOT NULL
);
COMMENT ON TABLE "volume_text_idx" IS 'Overall tsvector for each volume, automatically updated to represent "SELECT volume, tsvector_agg(to_tsvector(text)) FROM volume_text GROUP BY volume".';
CREATE INDEX ON "volume_text_idx" USING gin("ts");

CREATE FUNCTION "volume_text_refresh" (volume_id integer = NULL) RETURNS void LANGUAGE plpgsql AS $$ BEGIN
	IF volume_id IS NULL THEN
		TRUNCATE volume_text_idx;
		INSERT INTO volume_text_idx SELECT volume, tsvector_agg(to_tsvector('english', text)) FROM volume_text GROUP BY volume;
	ELSE
		DELETE FROM volume_text_idx WHERE volume = volume_id;
		INSERT INTO volume_text_idx VALUES (volume_id, (SELECT tsvector_agg(to_tsvector('english', text)) FROM volume_text WHERE volume = volume_id));
	END IF;
END; $$;
CREATE FUNCTION "volume_text_changed" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	vol integer;
BEGIN
	IF TG_TABLE_NAME = 'volume' THEN
		vol = NEW.id;
	ELSE
		vol = NEW.volume;
	END IF;
	PERFORM volume_text_refresh(vol);
	RETURN null;
END; $$;
CREATE TRIGGER "volume_changed_text" AFTER INSERT OR UPDATE OF "name", "body" ON "volume" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();
CREATE TRIGGER "volume_citation_changed_text" AFTER INSERT OR UPDATE OF "head", "year" ON "volume_citation" FOR EACH ROW EXECUTE PROCEDURE "volume_text_changed" ();

----------------------------------------------------------- analytics

CREATE TABLE audit."analytic" (
	"route" text NOT NULL,
	"data" json NOT NULL
) INHERITS (audit."audit");
COMMENT ON TABLE audit."analytic" IS 'Analytics data collected and reported by the browser.';


----------------------------------------------------------- bootstrap/test data

INSERT INTO party (id, prename, sortname, affiliation) VALUES (1, 'Admin', 'Admin', 'Databrary');
INSERT INTO party (id, prename, sortname, affiliation) VALUES (3, 'Lisa', 'Steiger', 'Databrary');
INSERT INTO party (id, prename, sortname, affiliation) VALUES (5, 'Karen', 'Adolph', 'New York University');
INSERT INTO party (id, prename, sortname, affiliation) VALUES (6, 'Rick', 'Gilmore', 'Penn State University');
INSERT INTO party (id, prename, sortname, affiliation) VALUES (7, 'Testarosa', 'Tesla', 'Carnegie Melon');
SELECT setval('party_id_seq', 7);

INSERT INTO account (id, email, password) VALUES (1, 'admin@databrary.org', '$2b$12$EqM7gziee86vJZLNInxFgeU7pnZWSm3G7aHuH6NH8mUI/nqTTCRvy');
INSERT INTO account (id, email, openid) VALUES (3, 'lisa@databrary.org', NULL);
INSERT INTO account (id, email, password) VALUES (7, 'test@databrary.org', '$2b$12$EqM7gziee86vJZLNInxFgeU7pnZWSm3G7aHuH6NH8mUI/nqTTCRvy');

INSERT INTO authorize (child, parent, site, member) VALUES (1, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, site, member) VALUES (3, 0, 'ADMIN', 'ADMIN');
INSERT INTO authorize (child, parent, site, member) VALUES (7, 0, 'ADMIN', 'ADMIN');

INSERT INTO volume (id, name, body) VALUES (1, 'Databrary', 'Databrary is an open data library for developmental science. Share video, audio, and related metadata. Discover more, faster.
Most developmental scientists rely on video recordings to capture the complexity and richness of behavior. However, researchers rarely share video data, and this has impeded scientific progress. By creating the cyber-infrastructure and community to enable open video sharing, the Databrary project aims to facilitate deeper, richer, and broader understanding of behavior.
The Databrary project is dedicated to transforming the culture of developmental science by building a community of researchers committed to open video data sharing, training a new generation of developmental scientists and empowering them with an unprecedented set of tools for discovery, and raising the profile of behavioral science by bolstering interest in and support for scientific research among the general public.');
SELECT setval('volume_id_seq', 1);
INSERT INTO "volume_doi" VALUES (1, '10.17910/B7159Q');

INSERT INTO volume_access (volume, party, individual, children) VALUES (1, 1, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, individual, children) VALUES (1, 3, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, individual, children) VALUES (1, 7, 'ADMIN', 'NONE');
INSERT INTO volume_access (volume, party, individual, children) VALUES (1, -1, 'PUBLIC', 'PUBLIC');

INSERT INTO asset (id, volume, format, release, duration, name, sha1) VALUES (1, 1, -800, 'PUBLIC', interval '40', 'counting', '\x3dda3931202cbe06a9e4bbb5f0873c879121ef0a');
INSERT INTO slot_asset VALUES (1, '[0,40)'::segment, 1);
SELECT setval('asset_id_seq', 1);
SELECT setval('container_id_seq', 2);

-- special volumes (SERIAL starts at 1), done after container triggers:
INSERT INTO "volume" (id, name) VALUES (0, 'Core'); -- CORE
INSERT INTO "volume_access" VALUES (0, -1, 'PUBLIC', 'PUBLIC');


----------------------------------------------------------- ingest records

CREATE SCHEMA ingest;

CREATE TABLE ingest."asset" (
	"id" integer NOT NULL Unique References "asset" ON DELETE CASCADE,
	"file" text NOT NULL Unique
);
ALTER TABLE ingest."asset"
	ALTER "file" SET STORAGE EXTERNAL;

CREATE TABLE ingest."container" (
	"id" integer NOT NULL Unique References "container" ON DELETE CASCADE,
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE,
	"key" text NOT NULL,
	Unique ("volume", "key")
);
ALTER TABLE ingest."container"
	ALTER "key" SET STORAGE EXTERNAL;

CREATE TABLE ingest."record" (
	"id" integer NOT NULL Unique References "record" ON DELETE CASCADE,
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE,
	"key" text NOT NULL,
	Unique ("volume", "key")
);
ALTER TABLE ingest."record"
	ALTER "key" SET STORAGE EXTERNAL;
