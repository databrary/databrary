// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package audit

import (
	"bytes"
	"database/sql"
	"fmt"
	"reflect"
	"strings"
	"sync"
	"time"

	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/sqlboiler/boil"
	"github.com/databrary/sqlboiler/queries"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/databrary/sqlboiler/strmangle"
	"github.com/pkg/errors"
)

// Excerpt is an object representing the database table.
type Excerpt struct {
	AuditTime   time.Time                `db:"audit_time" json:"excerpt_audit_time"`
	AuditUser   int                      `db:"audit_user" json:"excerpt_audit_user"`
	AuditIP     custom_types.Inet        `db:"audit_ip" json:"excerpt_audit_ip"`
	AuditAction custom_types.Action      `db:"audit_action" json:"excerpt_audit_action"`
	Asset       int                      `db:"asset" json:"excerpt_asset"`
	Segment     custom_types.Segment     `db:"segment" json:"excerpt_segment"`
	Release     custom_types.NullRelease `db:"release" json:"excerpt_release,omitempty"`

	R *excerptR `db:"-" json:"-"`
	L excerptL  `db:"-" json:"-"`
}

// excerptR is where relationships are stored.
type excerptR struct {
}

// excerptL is where Load methods for each relationship are stored.
type excerptL struct{}

var (
	excerptColumns               = []string{"audit_time", "audit_user", "audit_ip", "audit_action", "asset", "segment", "release"}
	excerptColumnsWithoutDefault = []string{"audit_user", "audit_ip", "audit_action", "asset", "segment", "release"}
	excerptColumnsWithDefault    = []string{"audit_time"}
	excerptColumnsWithCustom     = []string{"audit_ip", "audit_action", "segment", "release"}
)

type (
	// ExcerptSlice is an alias for a slice of pointers to Excerpt.
	// This should generally be used opposed to []Excerpt.
	ExcerptSlice []*Excerpt
	// ExcerptHook is the signature for custom Excerpt hook methods
	ExcerptHook func(boil.Executor, *Excerpt) error

	excerptQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	excerptType    = reflect.TypeOf(&Excerpt{})
	excerptMapping = queries.MakeStructMapping(excerptType)

	excerptInsertCacheMut sync.RWMutex
	excerptInsertCache    = make(map[string]insertCache)
	excerptUpdateCacheMut sync.RWMutex
	excerptUpdateCache    = make(map[string]updateCache)
	excerptUpsertCacheMut sync.RWMutex
	excerptUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var excerptBeforeInsertHooks []ExcerptHook
var excerptBeforeUpdateHooks []ExcerptHook
var excerptBeforeDeleteHooks []ExcerptHook
var excerptBeforeUpsertHooks []ExcerptHook

var excerptAfterInsertHooks []ExcerptHook
var excerptAfterSelectHooks []ExcerptHook
var excerptAfterUpdateHooks []ExcerptHook
var excerptAfterDeleteHooks []ExcerptHook
var excerptAfterUpsertHooks []ExcerptHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Excerpt) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Excerpt) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Excerpt) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Excerpt) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Excerpt) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Excerpt) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Excerpt) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Excerpt) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Excerpt) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range excerptAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddExcerptHook registers your hook function for all future operations.
func AddExcerptHook(hookPoint boil.HookPoint, excerptHook ExcerptHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		excerptBeforeInsertHooks = append(excerptBeforeInsertHooks, excerptHook)
	case boil.BeforeUpdateHook:
		excerptBeforeUpdateHooks = append(excerptBeforeUpdateHooks, excerptHook)
	case boil.BeforeDeleteHook:
		excerptBeforeDeleteHooks = append(excerptBeforeDeleteHooks, excerptHook)
	case boil.BeforeUpsertHook:
		excerptBeforeUpsertHooks = append(excerptBeforeUpsertHooks, excerptHook)
	case boil.AfterInsertHook:
		excerptAfterInsertHooks = append(excerptAfterInsertHooks, excerptHook)
	case boil.AfterSelectHook:
		excerptAfterSelectHooks = append(excerptAfterSelectHooks, excerptHook)
	case boil.AfterUpdateHook:
		excerptAfterUpdateHooks = append(excerptAfterUpdateHooks, excerptHook)
	case boil.AfterDeleteHook:
		excerptAfterDeleteHooks = append(excerptAfterDeleteHooks, excerptHook)
	case boil.AfterUpsertHook:
		excerptAfterUpsertHooks = append(excerptAfterUpsertHooks, excerptHook)
	}
}

// OneP returns a single excerpt record from the query, and panics on error.
func (q excerptQuery) OneP() *Excerpt {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single excerpt record from the query.
func (q excerptQuery) One() (*Excerpt, error) {
	o := &Excerpt{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "audit: failed to execute a one query for excerpt")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Excerpt records from the query, and panics on error.
func (q excerptQuery) AllP() ExcerptSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Excerpt records from the query.
func (q excerptQuery) All() (ExcerptSlice, error) {
	var o ExcerptSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "audit: failed to assign all query results to Excerpt slice")
	}

	if len(excerptAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Excerpt records in the query, and panics on error.
func (q excerptQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Excerpt records in the query.
func (q excerptQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "audit: failed to count excerpt rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q excerptQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q excerptQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "audit: failed to check if excerpt exists")
	}

	return count > 0, nil
}

// ExcerptsG retrieves all records.
func ExcerptsG(mods ...qm.QueryMod) excerptQuery {
	return Excerpts(boil.GetDB(), mods...)
}

// Excerpts retrieves all the records using an executor.
func Excerpts(exec boil.Executor, mods ...qm.QueryMod) excerptQuery {
	mods = append(mods, qm.From("\"audit\".\"excerpt\""))
	return excerptQuery{NewQuery(exec, mods...)}
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Excerpt) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Excerpt) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Excerpt) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Excerpt) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("audit: no excerpt provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(excerptColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	excerptInsertCacheMut.RLock()
	cache, cached := excerptInsertCache[key]
	excerptInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			excerptColumns,
			excerptColumnsWithDefault,
			excerptColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(excerptType, excerptMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(excerptType, excerptMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"audit\".\"excerpt\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"audit\".\"excerpt\" DEFAULT VALUES"
		}

		if len(cache.retMapping) != 0 {
			cache.query += fmt.Sprintf(" RETURNING \"%s\"", strings.Join(returnColumns, "\",\""))
		}
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, vals)
	}

	if len(cache.retMapping) != 0 {
		err = exec.QueryRow(cache.query, vals...).Scan(queries.PtrsFromMapping(value, cache.retMapping)...)
	} else {
		_, err = exec.Exec(cache.query, vals...)
	}

	if err != nil {
		return errors.Wrap(err, "audit: unable to insert into excerpt")
	}

	if !cached {
		excerptInsertCacheMut.Lock()
		excerptInsertCache[key] = cache
		excerptInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}
