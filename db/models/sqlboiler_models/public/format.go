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

	"github.com/databrary/sqlboiler/boil"
	"github.com/databrary/sqlboiler/queries"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/databrary/sqlboiler/strmangle"
	"github.com/databrary/sqlboiler/types"
	"github.com/pkg/errors"
)

// Format is an object representing the database table.
type Format struct {
	ID        int16             `db:"id" json:"format_id"`
	Mimetype  string            `db:"mimetype" json:"format_mimetype"`
	Extension types.StringArray `db:"extension" json:"format_extension"`
	Name      string            `db:"name" json:"format_name"`

	R *formatR `db:"-" json:"-"`
	L formatL  `db:"-" json:"-"`
}

// formatR is where relationships are stored.
type formatR struct {
	Assets AssetSlice
}

// formatL is where Load methods for each relationship are stored.
type formatL struct{}

var (
	formatColumns               = []string{"id", "mimetype", "extension", "name"}
	formatColumnsWithoutDefault = []string{"mimetype", "extension", "name"}
	formatColumnsWithDefault    = []string{"id"}
	formatColumnsWithCustom     = []string{}

	formatPrimaryKeyColumns = []string{"id"}
)

type (
	// FormatSlice is an alias for a slice of pointers to Format.
	// This should generally be used opposed to []Format.
	FormatSlice []*Format
	// FormatHook is the signature for custom Format hook methods
	FormatHook func(boil.Executor, *Format) error

	formatQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	formatType    = reflect.TypeOf(&Format{})
	formatMapping = queries.MakeStructMapping(formatType)

	formatPrimaryKeyMapping, _ = queries.BindMapping(formatType, formatMapping, formatPrimaryKeyColumns)

	formatInsertCacheMut sync.RWMutex
	formatInsertCache    = make(map[string]insertCache)
	formatUpdateCacheMut sync.RWMutex
	formatUpdateCache    = make(map[string]updateCache)
	formatUpsertCacheMut sync.RWMutex
	formatUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var formatBeforeInsertHooks []FormatHook
var formatBeforeUpdateHooks []FormatHook
var formatBeforeDeleteHooks []FormatHook
var formatBeforeUpsertHooks []FormatHook

var formatAfterInsertHooks []FormatHook
var formatAfterSelectHooks []FormatHook
var formatAfterUpdateHooks []FormatHook
var formatAfterDeleteHooks []FormatHook
var formatAfterUpsertHooks []FormatHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Format) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range formatBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Format) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range formatBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Format) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range formatBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Format) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range formatBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Format) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range formatAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Format) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range formatAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Format) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range formatAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Format) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range formatAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Format) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range formatAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddFormatHook registers your hook function for all future operations.
func AddFormatHook(hookPoint boil.HookPoint, formatHook FormatHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		formatBeforeInsertHooks = append(formatBeforeInsertHooks, formatHook)
	case boil.BeforeUpdateHook:
		formatBeforeUpdateHooks = append(formatBeforeUpdateHooks, formatHook)
	case boil.BeforeDeleteHook:
		formatBeforeDeleteHooks = append(formatBeforeDeleteHooks, formatHook)
	case boil.BeforeUpsertHook:
		formatBeforeUpsertHooks = append(formatBeforeUpsertHooks, formatHook)
	case boil.AfterInsertHook:
		formatAfterInsertHooks = append(formatAfterInsertHooks, formatHook)
	case boil.AfterSelectHook:
		formatAfterSelectHooks = append(formatAfterSelectHooks, formatHook)
	case boil.AfterUpdateHook:
		formatAfterUpdateHooks = append(formatAfterUpdateHooks, formatHook)
	case boil.AfterDeleteHook:
		formatAfterDeleteHooks = append(formatAfterDeleteHooks, formatHook)
	case boil.AfterUpsertHook:
		formatAfterUpsertHooks = append(formatAfterUpsertHooks, formatHook)
	}
}

// OneP returns a single format record from the query, and panics on error.
func (q formatQuery) OneP() *Format {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single format record from the query.
func (q formatQuery) One() (*Format, error) {
	o := &Format{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for format")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Format records from the query, and panics on error.
func (q formatQuery) AllP() FormatSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Format records from the query.
func (q formatQuery) All() (FormatSlice, error) {
	var o FormatSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Format slice")
	}

	if len(formatAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Format records in the query, and panics on error.
func (q formatQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Format records in the query.
func (q formatQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count format rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q formatQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q formatQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if format exists")
	}

	return count > 0, nil
}

// AssetsG retrieves all the asset's asset.
func (o *Format) AssetsG(mods ...qm.QueryMod) assetQuery {
	return o.AssetsByFk(boil.GetDB(), mods...)
}

// Assets retrieves all the asset's asset with an executor.
func (o *Format) AssetsByFk(exec boil.Executor, mods ...qm.QueryMod) assetQuery {
	queryMods := []qm.QueryMod{
		qm.Select("\"a\".*"),
	}

	if len(mods) != 0 {
		queryMods = append(queryMods, mods...)
	}

	queryMods = append(queryMods,
		qm.Where("\"a\".\"format\"=?", o.ID),
	)

	query := Assets(exec, queryMods...)
	queries.SetFrom(query.Query, "\"asset\" as \"a\"")
	return query
}

// LoadAssets allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (formatL) LoadAssets(e boil.Executor, singular bool, maybeFormat interface{}) error {
	var slice []*Format
	var object *Format

	count := 1
	if singular {
		object = maybeFormat.(*Format)
	} else {
		slice = *maybeFormat.(*FormatSlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &formatR{}
		}
		args[0] = object.ID
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &formatR{}
			}
			args[i] = obj.ID
		}
	}

	query := fmt.Sprintf(
		"select * from \"asset\" where \"format\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)
	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load asset")
	}
	defer results.Close()

	var resultSlice []*Asset
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice asset")
	}

	if len(assetAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}
	if singular {
		object.R.Assets = resultSlice
		return nil
	}

	for _, foreign := range resultSlice {
		for _, local := range slice {
			if local.ID == foreign.Format {
				local.R.Assets = append(local.R.Assets, foreign)
				break
			}
		}
	}

	return nil
}

// AddAssetsG adds the given related objects to the existing relationships
// of the format, optionally inserting them as new records.
// Appends related to o.R.Assets.
// Sets related.R.Format appropriately.
// Uses the global database handle.
func (o *Format) AddAssetsG(insert bool, related ...*Asset) error {
	return o.AddAssets(boil.GetDB(), insert, related...)
}

// AddAssetsP adds the given related objects to the existing relationships
// of the format, optionally inserting them as new records.
// Appends related to o.R.Assets.
// Sets related.R.Format appropriately.
// Panics on error.
func (o *Format) AddAssetsP(exec boil.Executor, insert bool, related ...*Asset) {
	if err := o.AddAssets(exec, insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddAssetsGP adds the given related objects to the existing relationships
// of the format, optionally inserting them as new records.
// Appends related to o.R.Assets.
// Sets related.R.Format appropriately.
// Uses the global database handle and panics on error.
func (o *Format) AddAssetsGP(insert bool, related ...*Asset) {
	if err := o.AddAssets(boil.GetDB(), insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddAssets adds the given related objects to the existing relationships
// of the format, optionally inserting them as new records.
// Appends related to o.R.Assets.
// Sets related.R.Format appropriately.
func (o *Format) AddAssets(exec boil.Executor, insert bool, related ...*Asset) error {
	var err error
	for _, rel := range related {
		if insert {
			rel.Format = o.ID
			if err = rel.Insert(exec); err != nil {
				return errors.Wrap(err, "failed to insert into foreign table")
			}
		} else {
			updateQuery := fmt.Sprintf(
				"UPDATE \"asset\" SET %s WHERE %s",
				strmangle.SetParamNames("\"", "\"", 1, []string{"format"}),
				strmangle.WhereClause("\"", "\"", 2, assetPrimaryKeyColumns),
			)
			values := []interface{}{o.ID, rel.ID}

			if boil.DebugMode {
				fmt.Fprintln(boil.DebugWriter, updateQuery)
				fmt.Fprintln(boil.DebugWriter, values)
			}

			if _, err = exec.Exec(updateQuery, values...); err != nil {
				return errors.Wrap(err, "failed to update foreign table")
			}

			rel.Format = o.ID
		}
	}

	if o.R == nil {
		o.R = &formatR{
			Assets: related,
		}
	} else {
		o.R.Assets = append(o.R.Assets, related...)
	}

	for _, rel := range related {
		if rel.R == nil {
			rel.R = &assetR{
				Format: o,
			}
		} else {
			rel.R.Format = o
		}
	}
	return nil
}

// FormatsG retrieves all records.
func FormatsG(mods ...qm.QueryMod) formatQuery {
	return Formats(boil.GetDB(), mods...)
}

// Formats retrieves all the records using an executor.
func Formats(exec boil.Executor, mods ...qm.QueryMod) formatQuery {
	mods = append(mods, qm.From("\"format\""))
	return formatQuery{NewQuery(exec, mods...)}
}

// FindFormatG retrieves a single record by ID.
func FindFormatG(id int16, selectCols ...string) (*Format, error) {
	return FindFormat(boil.GetDB(), id, selectCols...)
}

// FindFormatGP retrieves a single record by ID, and panics on error.
func FindFormatGP(id int16, selectCols ...string) *Format {
	retobj, err := FindFormat(boil.GetDB(), id, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindFormat retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindFormat(exec boil.Executor, id int16, selectCols ...string) (*Format, error) {
	formatObj := &Format{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"format\" where \"id\"=$1", sel,
	)

	q := queries.Raw(exec, query, id)

	err := q.Bind(formatObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from format")
	}

	return formatObj, nil
}

// FindFormatP retrieves a single record by ID with an executor, and panics on error.
func FindFormatP(exec boil.Executor, id int16, selectCols ...string) *Format {
	retobj, err := FindFormat(exec, id, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Format) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Format) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Format) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Format) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no format provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(formatColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	formatInsertCacheMut.RLock()
	cache, cached := formatInsertCache[key]
	formatInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			formatColumns,
			formatColumnsWithDefault,
			formatColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(formatType, formatMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(formatType, formatMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"format\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"format\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into format")
	}

	if !cached {
		formatInsertCacheMut.Lock()
		formatInsertCache[key] = cache
		formatInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single Format record. See Update for
// whitelist behavior description.
func (o *Format) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single Format record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *Format) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the Format, and panics on error.
// See Update for whitelist behavior description.
func (o *Format) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the Format.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *Format) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	formatUpdateCacheMut.RLock()
	cache, cached := formatUpdateCache[key]
	formatUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(formatColumns, formatPrimaryKeyColumns, whitelist)
		if len(wl) == 0 {
			return errors.New("models: unable to update format, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"format\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, formatPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(formatType, formatMapping, append(wl, formatPrimaryKeyColumns...))
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
		return errors.Wrap(err, "models: unable to update format row")
	}

	if !cached {
		formatUpdateCacheMut.Lock()
		formatUpdateCache[key] = cache
		formatUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q formatQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q formatQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for format")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o FormatSlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o FormatSlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o FormatSlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o FormatSlice) UpdateAll(exec boil.Executor, cols M) error {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), formatPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"UPDATE \"format\" SET %s WHERE (\"id\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(formatPrimaryKeyColumns), len(colNames)+1, len(formatPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in format slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *Format) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *Format) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *Format) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *Format) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no format provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(formatColumnsWithDefault, o)

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

	formatUpsertCacheMut.RLock()
	cache, cached := formatUpsertCache[key]
	formatUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			formatColumns,
			formatColumnsWithDefault,
			formatColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			formatColumns,
			formatPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert format, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(formatPrimaryKeyColumns))
			copy(conflict, formatPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"format\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(formatType, formatMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(formatType, formatMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert format")
	}

	if !cached {
		formatUpsertCacheMut.Lock()
		formatUpsertCache[key] = cache
		formatUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single Format record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Format) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single Format record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *Format) DeleteG() error {
	if o == nil {
		return errors.New("models: no Format provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single Format record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Format) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single Format record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Format) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Format provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), formatPrimaryKeyMapping)
	query := "DELETE FROM \"format\" WHERE \"id\"=$1"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from format")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q formatQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q formatQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no formatQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from format")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o FormatSlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o FormatSlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no Format slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o FormatSlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o FormatSlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Format slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(formatBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), formatPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"DELETE FROM \"format\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, formatPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(formatPrimaryKeyColumns), 1, len(formatPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from format slice")
	}

	if len(formatAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *Format) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *Format) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *Format) ReloadG() error {
	if o == nil {
		return errors.New("models: no Format provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *Format) Reload(exec boil.Executor) error {
	ret, err := FindFormat(exec, o.ID)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *FormatSlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *FormatSlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *FormatSlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty FormatSlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *FormatSlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	formats := FormatSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), formatPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"SELECT \"format\".* FROM \"format\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, formatPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(formatPrimaryKeyColumns), 1, len(formatPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, query, args...)

	err := q.Bind(&formats)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in FormatSlice")
	}

	*o = formats

	return nil
}

// FormatExists checks if the Format row exists.
func FormatExists(exec boil.Executor, id int16) (bool, error) {
	var exists bool

	query := "select exists(select 1 from \"format\" where \"id\"=$1 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, id)
	}

	row := exec.QueryRow(query, id)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if format exists")
	}

	return exists, nil
}

// FormatExistsG checks if the Format row exists.
func FormatExistsG(id int16) (bool, error) {
	return FormatExists(boil.GetDB(), id)
}

// FormatExistsGP checks if the Format row exists. Panics on error.
func FormatExistsGP(id int16) bool {
	e, err := FormatExists(boil.GetDB(), id)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// FormatExistsP checks if the Format row exists. Panics on error.
func FormatExistsP(exec boil.Executor, id int16) bool {
	e, err := FormatExists(exec, id)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
