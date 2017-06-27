// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package public

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

// Notify is an object representing the database table.
type Notify struct {
	Target   int                         `db:"target" json:"notify_target"`
	Notice   int16                       `db:"notice" json:"notify_notice"`
	Delivery custom_types.NoticeDelivery `db:"delivery" json:"notify_delivery"`

	R *notifyR `db:"-" json:"-"`
	L notifyL  `db:"-" json:"-"`
}

// notifyR is where relationships are stored.
type notifyR struct {
	Notice *Notice
	Target *Account
}

// notifyL is where Load methods for each relationship are stored.
type notifyL struct{}

var (
	notifyColumns               = []string{"target", "notice", "delivery"}
	notifyColumnsWithoutDefault = []string{"target", "notice", "delivery"}
	notifyColumnsWithDefault    = []string{}
	notifyColumnsWithCustom     = []string{"delivery"}

	notifyPrimaryKeyColumns = []string{"target", "notice"}
)

type (
	// NotifySlice is an alias for a slice of pointers to Notify.
	// This should generally be used opposed to []Notify.
	NotifySlice []*Notify
	// NotifyHook is the signature for custom Notify hook methods
	NotifyHook func(boil.Executor, *Notify) error

	notifyQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	notifyType    = reflect.TypeOf(&Notify{})
	notifyMapping = queries.MakeStructMapping(notifyType)

	notifyPrimaryKeyMapping, _ = queries.BindMapping(notifyType, notifyMapping, notifyPrimaryKeyColumns)

	notifyInsertCacheMut sync.RWMutex
	notifyInsertCache    = make(map[string]insertCache)
	notifyUpdateCacheMut sync.RWMutex
	notifyUpdateCache    = make(map[string]updateCache)
	notifyUpsertCacheMut sync.RWMutex
	notifyUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var notifyBeforeInsertHooks []NotifyHook
var notifyBeforeUpdateHooks []NotifyHook
var notifyBeforeDeleteHooks []NotifyHook
var notifyBeforeUpsertHooks []NotifyHook

var notifyAfterInsertHooks []NotifyHook
var notifyAfterSelectHooks []NotifyHook
var notifyAfterUpdateHooks []NotifyHook
var notifyAfterDeleteHooks []NotifyHook
var notifyAfterUpsertHooks []NotifyHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Notify) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Notify) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Notify) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Notify) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Notify) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Notify) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Notify) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Notify) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Notify) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range notifyAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddNotifyHook registers your hook function for all future operations.
func AddNotifyHook(hookPoint boil.HookPoint, notifyHook NotifyHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		notifyBeforeInsertHooks = append(notifyBeforeInsertHooks, notifyHook)
	case boil.BeforeUpdateHook:
		notifyBeforeUpdateHooks = append(notifyBeforeUpdateHooks, notifyHook)
	case boil.BeforeDeleteHook:
		notifyBeforeDeleteHooks = append(notifyBeforeDeleteHooks, notifyHook)
	case boil.BeforeUpsertHook:
		notifyBeforeUpsertHooks = append(notifyBeforeUpsertHooks, notifyHook)
	case boil.AfterInsertHook:
		notifyAfterInsertHooks = append(notifyAfterInsertHooks, notifyHook)
	case boil.AfterSelectHook:
		notifyAfterSelectHooks = append(notifyAfterSelectHooks, notifyHook)
	case boil.AfterUpdateHook:
		notifyAfterUpdateHooks = append(notifyAfterUpdateHooks, notifyHook)
	case boil.AfterDeleteHook:
		notifyAfterDeleteHooks = append(notifyAfterDeleteHooks, notifyHook)
	case boil.AfterUpsertHook:
		notifyAfterUpsertHooks = append(notifyAfterUpsertHooks, notifyHook)
	}
}

// OneP returns a single notify record from the query, and panics on error.
func (q notifyQuery) OneP() *Notify {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single notify record from the query.
func (q notifyQuery) One() (*Notify, error) {
	o := &Notify{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for notify")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Notify records from the query, and panics on error.
func (q notifyQuery) AllP() NotifySlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Notify records from the query.
func (q notifyQuery) All() (NotifySlice, error) {
	var o NotifySlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Notify slice")
	}

	if len(notifyAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Notify records in the query, and panics on error.
func (q notifyQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Notify records in the query.
func (q notifyQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count notify rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q notifyQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q notifyQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if notify exists")
	}

	return count > 0, nil
}

// NoticeG pointed to by the foreign key.
func (o *Notify) NoticeG(mods ...qm.QueryMod) noticeQuery {
	return o.NoticeByFk(boil.GetDB(), mods...)
}

// Notice pointed to by the foreign key.
func (o *Notify) NoticeByFk(exec boil.Executor, mods ...qm.QueryMod) noticeQuery {
	queryMods := []qm.QueryMod{
		qm.Where("id=?", o.Notice),
	}

	queryMods = append(queryMods, mods...)

	query := Notices(exec, queryMods...)
	queries.SetFrom(query.Query, "\"notice\"")

	return query
}

// TargetG pointed to by the foreign key.
func (o *Notify) TargetG(mods ...qm.QueryMod) accountQuery {
	return o.TargetByFk(boil.GetDB(), mods...)
}

// Target pointed to by the foreign key.
func (o *Notify) TargetByFk(exec boil.Executor, mods ...qm.QueryMod) accountQuery {
	queryMods := []qm.QueryMod{
		qm.Where("id=?", o.Target),
	}

	queryMods = append(queryMods, mods...)

	query := Accounts(exec, queryMods...)
	queries.SetFrom(query.Query, "\"account\"")

	return query
}

// LoadNotice allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (notifyL) LoadNotice(e boil.Executor, singular bool, maybeNotify interface{}) error {
	var slice []*Notify
	var object *Notify

	count := 1
	if singular {
		object = maybeNotify.(*Notify)
	} else {
		slice = *maybeNotify.(*NotifySlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &notifyR{}
		}
		args[0] = object.Notice
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &notifyR{}
			}
			args[i] = obj.Notice
		}
	}

	query := fmt.Sprintf(
		"select * from \"notice\" where \"id\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)

	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load Notice")
	}
	defer results.Close()

	var resultSlice []*Notice
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice Notice")
	}

	if len(notifyAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}

	if len(resultSlice) == 0 {
		return nil
	}

	if singular {
		object.R.Notice = resultSlice[0]
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.Notice == foreign.ID {
				local.R.Notice = foreign
				break
			}
		}
	}

	return nil
}

// LoadTarget allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (notifyL) LoadTarget(e boil.Executor, singular bool, maybeNotify interface{}) error {
	var slice []*Notify
	var object *Notify

	count := 1
	if singular {
		object = maybeNotify.(*Notify)
	} else {
		slice = *maybeNotify.(*NotifySlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &notifyR{}
		}
		args[0] = object.Target
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &notifyR{}
			}
			args[i] = obj.Target
		}
	}

	query := fmt.Sprintf(
		"select * from \"account\" where \"id\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)

	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load Account")
	}
	defer results.Close()

	var resultSlice []*Account
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice Account")
	}

	if len(notifyAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}

	if len(resultSlice) == 0 {
		return nil
	}

	if singular {
		object.R.Target = resultSlice[0]
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.Target == foreign.ID {
				local.R.Target = foreign
				break
			}
		}
	}

	return nil
}

// SetNoticeG of the notify to the related item.
// Sets o.R.Notice to related.
// Adds o to related.R.Notifies.
// Uses the global database handle.
func (o *Notify) SetNoticeG(insert bool, related *Notice) error {
	return o.SetNotice(boil.GetDB(), insert, related)
}

// SetNoticeP of the notify to the related item.
// Sets o.R.Notice to related.
// Adds o to related.R.Notifies.
// Panics on error.
func (o *Notify) SetNoticeP(exec boil.Executor, insert bool, related *Notice) {
	if err := o.SetNotice(exec, insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetNoticeGP of the notify to the related item.
// Sets o.R.Notice to related.
// Adds o to related.R.Notifies.
// Uses the global database handle and panics on error.
func (o *Notify) SetNoticeGP(insert bool, related *Notice) {
	if err := o.SetNotice(boil.GetDB(), insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetNotice of the notify to the related item.
// Sets o.R.Notice to related.
// Adds o to related.R.Notifies.
func (o *Notify) SetNotice(exec boil.Executor, insert bool, related *Notice) error {
	var err error
	if insert {
		if err = related.Insert(exec); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"notify\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"notice"}),
		strmangle.WhereClause("\"", "\"", 2, notifyPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.Target, o.Notice}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, updateQuery)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	if _, err = exec.Exec(updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.Notice = related.ID

	if o.R == nil {
		o.R = &notifyR{
			Notice: related,
		}
	} else {
		o.R.Notice = related
	}

	if related.R == nil {
		related.R = &noticeR{
			Notifies: NotifySlice{o},
		}
	} else {
		related.R.Notifies = append(related.R.Notifies, o)
	}

	return nil
}

// SetTargetG of the notify to the related item.
// Sets o.R.Target to related.
// Adds o to related.R.TargetNotifies.
// Uses the global database handle.
func (o *Notify) SetTargetG(insert bool, related *Account) error {
	return o.SetTarget(boil.GetDB(), insert, related)
}

// SetTargetP of the notify to the related item.
// Sets o.R.Target to related.
// Adds o to related.R.TargetNotifies.
// Panics on error.
func (o *Notify) SetTargetP(exec boil.Executor, insert bool, related *Account) {
	if err := o.SetTarget(exec, insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetTargetGP of the notify to the related item.
// Sets o.R.Target to related.
// Adds o to related.R.TargetNotifies.
// Uses the global database handle and panics on error.
func (o *Notify) SetTargetGP(insert bool, related *Account) {
	if err := o.SetTarget(boil.GetDB(), insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetTarget of the notify to the related item.
// Sets o.R.Target to related.
// Adds o to related.R.TargetNotifies.
func (o *Notify) SetTarget(exec boil.Executor, insert bool, related *Account) error {
	var err error
	if insert {
		if err = related.Insert(exec); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"notify\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"target"}),
		strmangle.WhereClause("\"", "\"", 2, notifyPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.Target, o.Notice}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, updateQuery)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	if _, err = exec.Exec(updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.Target = related.ID

	if o.R == nil {
		o.R = &notifyR{
			Target: related,
		}
	} else {
		o.R.Target = related
	}

	if related.R == nil {
		related.R = &accountR{
			TargetNotifies: NotifySlice{o},
		}
	} else {
		related.R.TargetNotifies = append(related.R.TargetNotifies, o)
	}

	return nil
}

// NotifiesG retrieves all records.
func NotifiesG(mods ...qm.QueryMod) notifyQuery {
	return Notifies(boil.GetDB(), mods...)
}

// Notifies retrieves all the records using an executor.
func Notifies(exec boil.Executor, mods ...qm.QueryMod) notifyQuery {
	mods = append(mods, qm.From("\"notify\""))
	return notifyQuery{NewQuery(exec, mods...)}
}

// FindNotifyG retrieves a single record by ID.
func FindNotifyG(target int, notice int16, selectCols ...string) (*Notify, error) {
	return FindNotify(boil.GetDB(), target, notice, selectCols...)
}

// FindNotifyGP retrieves a single record by ID, and panics on error.
func FindNotifyGP(target int, notice int16, selectCols ...string) *Notify {
	retobj, err := FindNotify(boil.GetDB(), target, notice, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindNotify retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindNotify(exec boil.Executor, target int, notice int16, selectCols ...string) (*Notify, error) {
	notifyObj := &Notify{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"notify\" where \"target\"=$1 AND \"notice\"=$2", sel,
	)

	q := queries.Raw(exec, query, target, notice)

	err := q.Bind(notifyObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from notify")
	}

	return notifyObj, nil
}

// FindNotifyP retrieves a single record by ID with an executor, and panics on error.
func FindNotifyP(exec boil.Executor, target int, notice int16, selectCols ...string) *Notify {
	retobj, err := FindNotify(exec, target, notice, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Notify) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Notify) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Notify) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Notify) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no notify provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(notifyColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	notifyInsertCacheMut.RLock()
	cache, cached := notifyInsertCache[key]
	notifyInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			notifyColumns,
			notifyColumnsWithDefault,
			notifyColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(notifyType, notifyMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(notifyType, notifyMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"notify\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"notify\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into notify")
	}

	if !cached {
		notifyInsertCacheMut.Lock()
		notifyInsertCache[key] = cache
		notifyInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single Notify record. See Update for
// whitelist behavior description.
func (o *Notify) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single Notify record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *Notify) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the Notify, and panics on error.
// See Update for whitelist behavior description.
func (o *Notify) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the Notify.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *Notify) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	notifyUpdateCacheMut.RLock()
	cache, cached := notifyUpdateCache[key]
	notifyUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(notifyColumns, notifyPrimaryKeyColumns, whitelist)
		if len(wl) == 0 {
			return errors.New("models: unable to update notify, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"notify\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, notifyPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(notifyType, notifyMapping, append(wl, notifyPrimaryKeyColumns...))
		if err != nil {
			return err
		}
	}

	values := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), cache.valueMapping)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	_, err = exec.Exec(cache.query, values...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update notify row")
	}

	if !cached {
		notifyUpdateCacheMut.Lock()
		notifyUpdateCache[key] = cache
		notifyUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q notifyQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q notifyQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for notify")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o NotifySlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o NotifySlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o NotifySlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o NotifySlice) UpdateAll(exec boil.Executor, cols M) error {
	ln := int64(len(o))
	if ln == 0 {
		return nil
	}

	if len(cols) == 0 {
		return errors.New("models: update all requires at least one column argument")
	}

	colNames := make([]string, len(cols))
	args := make([]interface{}, len(cols))

	i := 0
	for name, value := range cols {
		colNames[i] = name
		args[i] = value
		i++
	}

	// Append all of the primary key values for each column
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), notifyPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"UPDATE \"notify\" SET %s WHERE (\"target\",\"notice\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(notifyPrimaryKeyColumns), len(colNames)+1, len(notifyPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in notify slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *Notify) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *Notify) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *Notify) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *Notify) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no notify provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(notifyColumnsWithDefault, o)

	// Build cache key in-line uglily - mysql vs postgres problems
	buf := strmangle.GetBuffer()
	if updateOnConflict {
		buf.WriteByte('t')
	} else {
		buf.WriteByte('f')
	}
	buf.WriteByte('.')
	for _, c := range conflictColumns {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range updateColumns {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range whitelist {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range nzDefaults {
		buf.WriteString(c)
	}
	key := buf.String()
	strmangle.PutBuffer(buf)

	notifyUpsertCacheMut.RLock()
	cache, cached := notifyUpsertCache[key]
	notifyUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			notifyColumns,
			notifyColumnsWithDefault,
			notifyColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			notifyColumns,
			notifyPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert notify, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(notifyPrimaryKeyColumns))
			copy(conflict, notifyPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"notify\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(notifyType, notifyMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(notifyType, notifyMapping, ret)
			if err != nil {
				return err
			}
		}
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)
	var returns []interface{}
	if len(cache.retMapping) != 0 {
		returns = queries.PtrsFromMapping(value, cache.retMapping)
	}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, cache.query)
		fmt.Fprintln(boil.DebugWriter, vals)
	}

	if len(cache.retMapping) != 0 {
		err = exec.QueryRow(cache.query, vals...).Scan(returns...)
		if err == sql.ErrNoRows {
			err = nil // Postgres doesn't return anything when there's no update
		}
	} else {
		_, err = exec.Exec(cache.query, vals...)
	}
	if err != nil {
		return errors.Wrap(err, "models: unable to upsert notify")
	}

	if !cached {
		notifyUpsertCacheMut.Lock()
		notifyUpsertCache[key] = cache
		notifyUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single Notify record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Notify) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single Notify record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *Notify) DeleteG() error {
	if o == nil {
		return errors.New("models: no Notify provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single Notify record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Notify) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single Notify record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Notify) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Notify provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), notifyPrimaryKeyMapping)
	query := "DELETE FROM \"notify\" WHERE \"target\"=$1 AND \"notice\"=$2"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from notify")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q notifyQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q notifyQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no notifyQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from notify")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o NotifySlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o NotifySlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no Notify slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o NotifySlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o NotifySlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Notify slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(notifyBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), notifyPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"DELETE FROM \"notify\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, notifyPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(notifyPrimaryKeyColumns), 1, len(notifyPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from notify slice")
	}

	if len(notifyAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *Notify) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *Notify) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *Notify) ReloadG() error {
	if o == nil {
		return errors.New("models: no Notify provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *Notify) Reload(exec boil.Executor) error {
	ret, err := FindNotify(exec, o.Target, o.Notice)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *NotifySlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *NotifySlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *NotifySlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty NotifySlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *NotifySlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	notifies := NotifySlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), notifyPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"SELECT \"notify\".* FROM \"notify\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, notifyPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(notifyPrimaryKeyColumns), 1, len(notifyPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, query, args...)

	err := q.Bind(&notifies)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in NotifySlice")
	}

	*o = notifies

	return nil
}

// NotifyExists checks if the Notify row exists.
func NotifyExists(exec boil.Executor, target int, notice int16) (bool, error) {
	var exists bool

	query := "select exists(select 1 from \"notify\" where \"target\"=$1 AND \"notice\"=$2 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, target, notice)
	}

	row := exec.QueryRow(query, target, notice)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if notify exists")
	}

	return exists, nil
}

// NotifyExistsG checks if the Notify row exists.
func NotifyExistsG(target int, notice int16) (bool, error) {
	return NotifyExists(boil.GetDB(), target, notice)
}

// NotifyExistsGP checks if the Notify row exists. Panics on error.
func NotifyExistsGP(target int, notice int16) bool {
	e, err := NotifyExists(boil.GetDB(), target, notice)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// NotifyExistsP checks if the Notify row exists. Panics on error.
func NotifyExistsP(exec boil.Executor, target int, notice int16) bool {
	e, err := NotifyExists(exec, target, notice)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
