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
	"gopkg.in/nullbio/null.v6"
)

// MeasureNumeric is an object representing the database view.
type MeasureNumeric struct {
	AuditTime   null.Time               `db:"audit_time" json:"measureNumeric_audit_time,omitempty"`
	AuditUser   null.Int                `db:"audit_user" json:"measureNumeric_audit_user,omitempty"`
	AuditIP     custom_types.NullInet   `db:"audit_ip" json:"measureNumeric_audit_ip,omitempty"`
	AuditAction custom_types.NullAction `db:"audit_action" json:"measureNumeric_audit_action,omitempty"`
	Record      null.Int                `db:"record" json:"measureNumeric_record,omitempty"`
	Metric      null.Int                `db:"metric" json:"measureNumeric_metric,omitempty"`
	Datum       null.String             `db:"datum" json:"measureNumeric_datum,omitempty"`

	R *measureNumericR `db:"-" json:"-"`
	L measureNumericL  `db:"-" json:"-"`
}

// measureNumericR is where relationships are stored.
type measureNumericR struct {
}

// measureNumericL is where Load methods for each relationship are stored.
type measureNumericL struct{}

var (
	measureNumericColumns               = []string{"audit_time", "audit_user", "audit_ip", "audit_action", "record", "metric", "datum"}
	measureNumericColumnsWithoutDefault = []string{"audit_time", "audit_user", "audit_ip", "audit_action", "record", "metric", "datum"}
	measureNumericColumnsWithDefault    = []string{}
	measureNumericColumnsWithCustom     = []string{"audit_ip", "audit_action"}
)

type (
	// MeasureNumericSlice is an alias for a slice of pointers to MeasureNumeric.
	// This should generally be used opposed to []MeasureNumeric.
	MeasureNumericSlice []*MeasureNumeric
	// MeasureNumericHook is the signature for custom MeasureNumeric hook methods
	MeasureNumericHook func(boil.Executor, *MeasureNumeric) error

	measureNumericQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	measureNumericType    = reflect.TypeOf(&MeasureNumeric{})
	measureNumericMapping = queries.MakeStructMapping(measureNumericType)

	measureNumericInsertCacheMut sync.RWMutex
	measureNumericInsertCache    = make(map[string]insertCache)
	measureNumericUpdateCacheMut sync.RWMutex
	measureNumericUpdateCache    = make(map[string]updateCache)
	measureNumericUpsertCacheMut sync.RWMutex
	measureNumericUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var measureNumericBeforeInsertHooks []MeasureNumericHook
var measureNumericBeforeUpdateHooks []MeasureNumericHook
var measureNumericBeforeDeleteHooks []MeasureNumericHook
var measureNumericBeforeUpsertHooks []MeasureNumericHook

var measureNumericAfterInsertHooks []MeasureNumericHook
var measureNumericAfterSelectHooks []MeasureNumericHook
var measureNumericAfterUpdateHooks []MeasureNumericHook
var measureNumericAfterDeleteHooks []MeasureNumericHook
var measureNumericAfterUpsertHooks []MeasureNumericHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *MeasureNumeric) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *MeasureNumeric) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *MeasureNumeric) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *MeasureNumeric) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *MeasureNumeric) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *MeasureNumeric) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *MeasureNumeric) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *MeasureNumeric) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *MeasureNumeric) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range measureNumericAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddMeasureNumericHook registers your hook function for all future operations.
func AddMeasureNumericHook(hookPoint boil.HookPoint, measureNumericHook MeasureNumericHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		measureNumericBeforeInsertHooks = append(measureNumericBeforeInsertHooks, measureNumericHook)
	case boil.BeforeUpdateHook:
		measureNumericBeforeUpdateHooks = append(measureNumericBeforeUpdateHooks, measureNumericHook)
	case boil.BeforeDeleteHook:
		measureNumericBeforeDeleteHooks = append(measureNumericBeforeDeleteHooks, measureNumericHook)
	case boil.BeforeUpsertHook:
		measureNumericBeforeUpsertHooks = append(measureNumericBeforeUpsertHooks, measureNumericHook)
	case boil.AfterInsertHook:
		measureNumericAfterInsertHooks = append(measureNumericAfterInsertHooks, measureNumericHook)
	case boil.AfterSelectHook:
		measureNumericAfterSelectHooks = append(measureNumericAfterSelectHooks, measureNumericHook)
	case boil.AfterUpdateHook:
		measureNumericAfterUpdateHooks = append(measureNumericAfterUpdateHooks, measureNumericHook)
	case boil.AfterDeleteHook:
		measureNumericAfterDeleteHooks = append(measureNumericAfterDeleteHooks, measureNumericHook)
	case boil.AfterUpsertHook:
		measureNumericAfterUpsertHooks = append(measureNumericAfterUpsertHooks, measureNumericHook)
	}
}

// OneP returns a single measureNumeric record from the query, and panics on error.
func (q measureNumericQuery) OneP() *MeasureNumeric {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single measureNumeric record from the query.
func (q measureNumericQuery) One() (*MeasureNumeric, error) {
	o := &MeasureNumeric{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "audit: failed to execute a one query for measure_numeric")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all MeasureNumeric records from the query, and panics on error.
func (q measureNumericQuery) AllP() MeasureNumericSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all MeasureNumeric records from the query.
func (q measureNumericQuery) All() (MeasureNumericSlice, error) {
	var o MeasureNumericSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "audit: failed to assign all query results to MeasureNumeric slice")
	}

	if len(measureNumericAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all MeasureNumeric records in the query, and panics on error.
func (q measureNumericQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all MeasureNumeric records in the query.
func (q measureNumericQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "audit: failed to count measure_numeric rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q measureNumericQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q measureNumericQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "audit: failed to check if measure_numeric exists")
	}

	return count > 0, nil
}

// MeasureNumericsG retrieves all records.
func MeasureNumericsG(mods ...qm.QueryMod) measureNumericQuery {
	return MeasureNumerics(boil.GetDB(), mods...)
}

// MeasureNumerics retrieves all the records using an executor.
func MeasureNumerics(exec boil.Executor, mods ...qm.QueryMod) measureNumericQuery {
	mods = append(mods, qm.From("\"audit\".\"measure_numeric\""))
	return measureNumericQuery{NewQuery(exec, mods...)}
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *MeasureNumeric) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *MeasureNumeric) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *MeasureNumeric) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *MeasureNumeric) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("audit: no measure_numeric provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(measureNumericColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	measureNumericInsertCacheMut.RLock()
	cache, cached := measureNumericInsertCache[key]
	measureNumericInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			measureNumericColumns,
			measureNumericColumnsWithDefault,
			measureNumericColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(measureNumericType, measureNumericMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(measureNumericType, measureNumericMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"audit\".\"measure_numeric\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"audit\".\"measure_numeric\" DEFAULT VALUES"
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
		return errors.Wrap(err, "audit: unable to insert into measure_numeric")
	}

	if !cached {
		measureNumericInsertCacheMut.Lock()
		measureNumericInsertCache[key] = cache
		measureNumericInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}
