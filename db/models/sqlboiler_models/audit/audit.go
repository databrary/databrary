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

// Audit is an object representing the database table.
type Audit struct {
	AuditTime   time.Time           `db:"audit_time" json:"audit_audit_time"`
	AuditUser   int                 `db:"audit_user" json:"audit_audit_user"`
	AuditIP     custom_types.Inet   `db:"audit_ip" json:"audit_audit_ip"`
	AuditAction custom_types.Action `db:"audit_action" json:"audit_audit_action"`

	R *auditR `db:"-" json:"-"`
	L auditL  `db:"-" json:"-"`
}

// auditR is where relationships are stored.
type auditR struct {
}

// auditL is where Load methods for each relationship are stored.
type auditL struct{}

var (
	auditColumns               = []string{"audit_time", "audit_user", "audit_ip", "audit_action"}
	auditColumnsWithoutDefault = []string{"audit_user", "audit_ip", "audit_action"}
	auditColumnsWithDefault    = []string{"audit_time"}
	auditColumnsWithCustom     = []string{"audit_ip", "audit_action"}
)

type (
	// AuditSlice is an alias for a slice of pointers to Audit.
	// This should generally be used opposed to []Audit.
	AuditSlice []*Audit
	// AuditHook is the signature for custom Audit hook methods
	AuditHook func(boil.Executor, *Audit) error

	auditQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	auditType    = reflect.TypeOf(&Audit{})
	auditMapping = queries.MakeStructMapping(auditType)

	auditInsertCacheMut sync.RWMutex
	auditInsertCache    = make(map[string]insertCache)
	auditUpdateCacheMut sync.RWMutex
	auditUpdateCache    = make(map[string]updateCache)
	auditUpsertCacheMut sync.RWMutex
	auditUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var auditBeforeInsertHooks []AuditHook
var auditBeforeUpdateHooks []AuditHook
var auditBeforeDeleteHooks []AuditHook
var auditBeforeUpsertHooks []AuditHook

var auditAfterInsertHooks []AuditHook
var auditAfterSelectHooks []AuditHook
var auditAfterUpdateHooks []AuditHook
var auditAfterDeleteHooks []AuditHook
var auditAfterUpsertHooks []AuditHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Audit) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range auditBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Audit) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range auditBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Audit) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range auditBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Audit) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range auditBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Audit) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range auditAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Audit) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range auditAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Audit) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range auditAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Audit) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range auditAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Audit) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range auditAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddAuditHook registers your hook function for all future operations.
func AddAuditHook(hookPoint boil.HookPoint, auditHook AuditHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		auditBeforeInsertHooks = append(auditBeforeInsertHooks, auditHook)
	case boil.BeforeUpdateHook:
		auditBeforeUpdateHooks = append(auditBeforeUpdateHooks, auditHook)
	case boil.BeforeDeleteHook:
		auditBeforeDeleteHooks = append(auditBeforeDeleteHooks, auditHook)
	case boil.BeforeUpsertHook:
		auditBeforeUpsertHooks = append(auditBeforeUpsertHooks, auditHook)
	case boil.AfterInsertHook:
		auditAfterInsertHooks = append(auditAfterInsertHooks, auditHook)
	case boil.AfterSelectHook:
		auditAfterSelectHooks = append(auditAfterSelectHooks, auditHook)
	case boil.AfterUpdateHook:
		auditAfterUpdateHooks = append(auditAfterUpdateHooks, auditHook)
	case boil.AfterDeleteHook:
		auditAfterDeleteHooks = append(auditAfterDeleteHooks, auditHook)
	case boil.AfterUpsertHook:
		auditAfterUpsertHooks = append(auditAfterUpsertHooks, auditHook)
	}
}

// OneP returns a single audit record from the query, and panics on error.
func (q auditQuery) OneP() *Audit {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single audit record from the query.
func (q auditQuery) One() (*Audit, error) {
	o := &Audit{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "audit: failed to execute a one query for audit")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Audit records from the query, and panics on error.
func (q auditQuery) AllP() AuditSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Audit records from the query.
func (q auditQuery) All() (AuditSlice, error) {
	var o AuditSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "audit: failed to assign all query results to Audit slice")
	}

	if len(auditAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Audit records in the query, and panics on error.
func (q auditQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Audit records in the query.
func (q auditQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "audit: failed to count audit rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q auditQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q auditQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "audit: failed to check if audit exists")
	}

	return count > 0, nil
}

// AuditsG retrieves all records.
func AuditsG(mods ...qm.QueryMod) auditQuery {
	return Audits(boil.GetDB(), mods...)
}

// Audits retrieves all the records using an executor.
func Audits(exec boil.Executor, mods ...qm.QueryMod) auditQuery {
	mods = append(mods, qm.From("\"audit\".\"audit\""))
	return auditQuery{NewQuery(exec, mods...)}
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Audit) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Audit) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Audit) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Audit) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("audit: no audit provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(auditColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	auditInsertCacheMut.RLock()
	cache, cached := auditInsertCache[key]
	auditInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			auditColumns,
			auditColumnsWithDefault,
			auditColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(auditType, auditMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(auditType, auditMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"audit\".\"audit\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"audit\".\"audit\" DEFAULT VALUES"
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
		return errors.Wrap(err, "audit: unable to insert into audit")
	}

	if !cached {
		auditInsertCacheMut.Lock()
		auditInsertCache[key] = cache
		auditInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}
