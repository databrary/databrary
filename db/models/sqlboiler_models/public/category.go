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
	"github.com/pkg/errors"
	"gopkg.in/nullbio/null.v6"
)

// Category is an object representing the database table.
type Category struct {
	ID          int16       `db:"id" json:"category_id"`
	Name        string      `db:"name" json:"category_name"`
	Description null.String `db:"description" json:"category_description,omitempty"`

	R *categoryR `db:"-" json:"-"`
	L categoryL  `db:"-" json:"-"`
}

// categoryR is where relationships are stored.
type categoryR struct {
	Metrics MetricSlice
	Records RecordSlice
}

// categoryL is where Load methods for each relationship are stored.
type categoryL struct{}

var (
	categoryColumns               = []string{"id", "name", "description"}
	categoryColumnsWithoutDefault = []string{"name", "description"}
	categoryColumnsWithDefault    = []string{"id"}
	categoryColumnsWithCustom     = []string{}

	categoryPrimaryKeyColumns = []string{"id"}
)

type (
	// CategorySlice is an alias for a slice of pointers to Category.
	// This should generally be used opposed to []Category.
	CategorySlice []*Category
	// CategoryHook is the signature for custom Category hook methods
	CategoryHook func(boil.Executor, *Category) error

	categoryQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	categoryType    = reflect.TypeOf(&Category{})
	categoryMapping = queries.MakeStructMapping(categoryType)

	categoryPrimaryKeyMapping, _ = queries.BindMapping(categoryType, categoryMapping, categoryPrimaryKeyColumns)

	categoryInsertCacheMut sync.RWMutex
	categoryInsertCache    = make(map[string]insertCache)
	categoryUpdateCacheMut sync.RWMutex
	categoryUpdateCache    = make(map[string]updateCache)
	categoryUpsertCacheMut sync.RWMutex
	categoryUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var categoryBeforeInsertHooks []CategoryHook
var categoryBeforeUpdateHooks []CategoryHook
var categoryBeforeDeleteHooks []CategoryHook
var categoryBeforeUpsertHooks []CategoryHook

var categoryAfterInsertHooks []CategoryHook
var categoryAfterSelectHooks []CategoryHook
var categoryAfterUpdateHooks []CategoryHook
var categoryAfterDeleteHooks []CategoryHook
var categoryAfterUpsertHooks []CategoryHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Category) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Category) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Category) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Category) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Category) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Category) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Category) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Category) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Category) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range categoryAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddCategoryHook registers your hook function for all future operations.
func AddCategoryHook(hookPoint boil.HookPoint, categoryHook CategoryHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		categoryBeforeInsertHooks = append(categoryBeforeInsertHooks, categoryHook)
	case boil.BeforeUpdateHook:
		categoryBeforeUpdateHooks = append(categoryBeforeUpdateHooks, categoryHook)
	case boil.BeforeDeleteHook:
		categoryBeforeDeleteHooks = append(categoryBeforeDeleteHooks, categoryHook)
	case boil.BeforeUpsertHook:
		categoryBeforeUpsertHooks = append(categoryBeforeUpsertHooks, categoryHook)
	case boil.AfterInsertHook:
		categoryAfterInsertHooks = append(categoryAfterInsertHooks, categoryHook)
	case boil.AfterSelectHook:
		categoryAfterSelectHooks = append(categoryAfterSelectHooks, categoryHook)
	case boil.AfterUpdateHook:
		categoryAfterUpdateHooks = append(categoryAfterUpdateHooks, categoryHook)
	case boil.AfterDeleteHook:
		categoryAfterDeleteHooks = append(categoryAfterDeleteHooks, categoryHook)
	case boil.AfterUpsertHook:
		categoryAfterUpsertHooks = append(categoryAfterUpsertHooks, categoryHook)
	}
}

// OneP returns a single category record from the query, and panics on error.
func (q categoryQuery) OneP() *Category {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single category record from the query.
func (q categoryQuery) One() (*Category, error) {
	o := &Category{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for category")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all Category records from the query, and panics on error.
func (q categoryQuery) AllP() CategorySlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all Category records from the query.
func (q categoryQuery) All() (CategorySlice, error) {
	var o CategorySlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Category slice")
	}

	if len(categoryAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all Category records in the query, and panics on error.
func (q categoryQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all Category records in the query.
func (q categoryQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count category rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q categoryQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q categoryQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if category exists")
	}

	return count > 0, nil
}

// MetricsG retrieves all the metric's metric.
func (o *Category) MetricsG(mods ...qm.QueryMod) metricQuery {
	return o.MetricsByFk(boil.GetDB(), mods...)
}

// Metrics retrieves all the metric's metric with an executor.
func (o *Category) MetricsByFk(exec boil.Executor, mods ...qm.QueryMod) metricQuery {
	queryMods := []qm.QueryMod{
		qm.Select("\"a\".*"),
	}

	if len(mods) != 0 {
		queryMods = append(queryMods, mods...)
	}

	queryMods = append(queryMods,
		qm.Where("\"a\".\"category\"=?", o.ID),
	)

	query := Metrics(exec, queryMods...)
	queries.SetFrom(query.Query, "\"metric\" as \"a\"")
	return query
}

// RecordsG retrieves all the record's record.
func (o *Category) RecordsG(mods ...qm.QueryMod) recordQuery {
	return o.RecordsByFk(boil.GetDB(), mods...)
}

// Records retrieves all the record's record with an executor.
func (o *Category) RecordsByFk(exec boil.Executor, mods ...qm.QueryMod) recordQuery {
	queryMods := []qm.QueryMod{
		qm.Select("\"a\".*"),
	}

	if len(mods) != 0 {
		queryMods = append(queryMods, mods...)
	}

	queryMods = append(queryMods,
		qm.Where("\"a\".\"category\"=?", o.ID),
	)

	query := Records(exec, queryMods...)
	queries.SetFrom(query.Query, "\"record\" as \"a\"")
	return query
}

// LoadMetrics allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (categoryL) LoadMetrics(e boil.Executor, singular bool, maybeCategory interface{}) error {
	var slice []*Category
	var object *Category

	count := 1
	if singular {
		object = maybeCategory.(*Category)
	} else {
		slice = *maybeCategory.(*CategorySlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &categoryR{}
		}
		args[0] = object.ID
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &categoryR{}
			}
			args[i] = obj.ID
		}
	}

	query := fmt.Sprintf(
		"select * from \"metric\" where \"category\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)
	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load metric")
	}
	defer results.Close()

	var resultSlice []*Metric
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice metric")
	}

	if len(metricAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}
	if singular {
		object.R.Metrics = resultSlice
		return nil
	}

	for _, foreign := range resultSlice {
		for _, local := range slice {
			if local.ID == foreign.Category {
				local.R.Metrics = append(local.R.Metrics, foreign)
				break
			}
		}
	}

	return nil
}

// LoadRecords allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (categoryL) LoadRecords(e boil.Executor, singular bool, maybeCategory interface{}) error {
	var slice []*Category
	var object *Category

	count := 1
	if singular {
		object = maybeCategory.(*Category)
	} else {
		slice = *maybeCategory.(*CategorySlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &categoryR{}
		}
		args[0] = object.ID
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &categoryR{}
			}
			args[i] = obj.ID
		}
	}

	query := fmt.Sprintf(
		"select * from \"record\" where \"category\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)
	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load record")
	}
	defer results.Close()

	var resultSlice []*Record
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice record")
	}

	if len(recordAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(e); err != nil {
				return err
			}
		}
	}
	if singular {
		object.R.Records = resultSlice
		return nil
	}

	for _, foreign := range resultSlice {
		for _, local := range slice {
			if local.ID == foreign.Category {
				local.R.Records = append(local.R.Records, foreign)
				break
			}
		}
	}

	return nil
}

// AddMetricsG adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Metrics.
// Sets related.R.Category appropriately.
// Uses the global database handle.
func (o *Category) AddMetricsG(insert bool, related ...*Metric) error {
	return o.AddMetrics(boil.GetDB(), insert, related...)
}

// AddMetricsP adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Metrics.
// Sets related.R.Category appropriately.
// Panics on error.
func (o *Category) AddMetricsP(exec boil.Executor, insert bool, related ...*Metric) {
	if err := o.AddMetrics(exec, insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddMetricsGP adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Metrics.
// Sets related.R.Category appropriately.
// Uses the global database handle and panics on error.
func (o *Category) AddMetricsGP(insert bool, related ...*Metric) {
	if err := o.AddMetrics(boil.GetDB(), insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddMetrics adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Metrics.
// Sets related.R.Category appropriately.
func (o *Category) AddMetrics(exec boil.Executor, insert bool, related ...*Metric) error {
	var err error
	for _, rel := range related {
		if insert {
			rel.Category = o.ID
			if err = rel.Insert(exec); err != nil {
				return errors.Wrap(err, "failed to insert into foreign table")
			}
		} else {
			updateQuery := fmt.Sprintf(
				"UPDATE \"metric\" SET %s WHERE %s",
				strmangle.SetParamNames("\"", "\"", 1, []string{"category"}),
				strmangle.WhereClause("\"", "\"", 2, metricPrimaryKeyColumns),
			)
			values := []interface{}{o.ID, rel.ID}

			if boil.DebugMode {
				fmt.Fprintln(boil.DebugWriter, updateQuery)
				fmt.Fprintln(boil.DebugWriter, values)
			}

			if _, err = exec.Exec(updateQuery, values...); err != nil {
				return errors.Wrap(err, "failed to update foreign table")
			}

			rel.Category = o.ID
		}
	}

	if o.R == nil {
		o.R = &categoryR{
			Metrics: related,
		}
	} else {
		o.R.Metrics = append(o.R.Metrics, related...)
	}

	for _, rel := range related {
		if rel.R == nil {
			rel.R = &metricR{
				Category: o,
			}
		} else {
			rel.R.Category = o
		}
	}
	return nil
}

// AddRecordsG adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Records.
// Sets related.R.Category appropriately.
// Uses the global database handle.
func (o *Category) AddRecordsG(insert bool, related ...*Record) error {
	return o.AddRecords(boil.GetDB(), insert, related...)
}

// AddRecordsP adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Records.
// Sets related.R.Category appropriately.
// Panics on error.
func (o *Category) AddRecordsP(exec boil.Executor, insert bool, related ...*Record) {
	if err := o.AddRecords(exec, insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddRecordsGP adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Records.
// Sets related.R.Category appropriately.
// Uses the global database handle and panics on error.
func (o *Category) AddRecordsGP(insert bool, related ...*Record) {
	if err := o.AddRecords(boil.GetDB(), insert, related...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// AddRecords adds the given related objects to the existing relationships
// of the category, optionally inserting them as new records.
// Appends related to o.R.Records.
// Sets related.R.Category appropriately.
func (o *Category) AddRecords(exec boil.Executor, insert bool, related ...*Record) error {
	var err error
	for _, rel := range related {
		if insert {
			rel.Category = o.ID
			if err = rel.Insert(exec); err != nil {
				return errors.Wrap(err, "failed to insert into foreign table")
			}
		} else {
			updateQuery := fmt.Sprintf(
				"UPDATE \"record\" SET %s WHERE %s",
				strmangle.SetParamNames("\"", "\"", 1, []string{"category"}),
				strmangle.WhereClause("\"", "\"", 2, recordPrimaryKeyColumns),
			)
			values := []interface{}{o.ID, rel.ID}

			if boil.DebugMode {
				fmt.Fprintln(boil.DebugWriter, updateQuery)
				fmt.Fprintln(boil.DebugWriter, values)
			}

			if _, err = exec.Exec(updateQuery, values...); err != nil {
				return errors.Wrap(err, "failed to update foreign table")
			}

			rel.Category = o.ID
		}
	}

	if o.R == nil {
		o.R = &categoryR{
			Records: related,
		}
	} else {
		o.R.Records = append(o.R.Records, related...)
	}

	for _, rel := range related {
		if rel.R == nil {
			rel.R = &recordR{
				Category: o,
			}
		} else {
			rel.R.Category = o
		}
	}
	return nil
}

// CategoriesG retrieves all records.
func CategoriesG(mods ...qm.QueryMod) categoryQuery {
	return Categories(boil.GetDB(), mods...)
}

// Categories retrieves all the records using an executor.
func Categories(exec boil.Executor, mods ...qm.QueryMod) categoryQuery {
	mods = append(mods, qm.From("\"category\""))
	return categoryQuery{NewQuery(exec, mods...)}
}

// FindCategoryG retrieves a single record by ID.
func FindCategoryG(id int16, selectCols ...string) (*Category, error) {
	return FindCategory(boil.GetDB(), id, selectCols...)
}

// FindCategoryGP retrieves a single record by ID, and panics on error.
func FindCategoryGP(id int16, selectCols ...string) *Category {
	retobj, err := FindCategory(boil.GetDB(), id, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindCategory retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindCategory(exec boil.Executor, id int16, selectCols ...string) (*Category, error) {
	categoryObj := &Category{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"category\" where \"id\"=$1", sel,
	)

	q := queries.Raw(exec, query, id)

	err := q.Bind(categoryObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from category")
	}

	return categoryObj, nil
}

// FindCategoryP retrieves a single record by ID with an executor, and panics on error.
func FindCategoryP(exec boil.Executor, id int16, selectCols ...string) *Category {
	retobj, err := FindCategory(exec, id, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *Category) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *Category) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *Category) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *Category) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no category provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(categoryColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	categoryInsertCacheMut.RLock()
	cache, cached := categoryInsertCache[key]
	categoryInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			categoryColumns,
			categoryColumnsWithDefault,
			categoryColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(categoryType, categoryMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(categoryType, categoryMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"category\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"category\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into category")
	}

	if !cached {
		categoryInsertCacheMut.Lock()
		categoryInsertCache[key] = cache
		categoryInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single Category record. See Update for
// whitelist behavior description.
func (o *Category) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single Category record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *Category) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the Category, and panics on error.
// See Update for whitelist behavior description.
func (o *Category) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the Category.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *Category) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	categoryUpdateCacheMut.RLock()
	cache, cached := categoryUpdateCache[key]
	categoryUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(categoryColumns, categoryPrimaryKeyColumns, whitelist)
		if len(wl) == 0 {
			return errors.New("models: unable to update category, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"category\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, categoryPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(categoryType, categoryMapping, append(wl, categoryPrimaryKeyColumns...))
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
		return errors.Wrap(err, "models: unable to update category row")
	}

	if !cached {
		categoryUpdateCacheMut.Lock()
		categoryUpdateCache[key] = cache
		categoryUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q categoryQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q categoryQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for category")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o CategorySlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o CategorySlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o CategorySlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o CategorySlice) UpdateAll(exec boil.Executor, cols M) error {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), categoryPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"UPDATE \"category\" SET %s WHERE (\"id\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(categoryPrimaryKeyColumns), len(colNames)+1, len(categoryPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in category slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *Category) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *Category) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *Category) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *Category) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no category provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(categoryColumnsWithDefault, o)

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

	categoryUpsertCacheMut.RLock()
	cache, cached := categoryUpsertCache[key]
	categoryUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			categoryColumns,
			categoryColumnsWithDefault,
			categoryColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			categoryColumns,
			categoryPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert category, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(categoryPrimaryKeyColumns))
			copy(conflict, categoryPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"category\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(categoryType, categoryMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(categoryType, categoryMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert category")
	}

	if !cached {
		categoryUpsertCacheMut.Lock()
		categoryUpsertCache[key] = cache
		categoryUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single Category record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Category) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single Category record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *Category) DeleteG() error {
	if o == nil {
		return errors.New("models: no Category provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single Category record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *Category) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single Category record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Category) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Category provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), categoryPrimaryKeyMapping)
	query := "DELETE FROM \"category\" WHERE \"id\"=$1"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from category")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q categoryQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q categoryQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no categoryQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from category")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o CategorySlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o CategorySlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no Category slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o CategorySlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o CategorySlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no Category slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(categoryBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), categoryPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"DELETE FROM \"category\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, categoryPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(categoryPrimaryKeyColumns), 1, len(categoryPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from category slice")
	}

	if len(categoryAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *Category) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *Category) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *Category) ReloadG() error {
	if o == nil {
		return errors.New("models: no Category provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *Category) Reload(exec boil.Executor) error {
	ret, err := FindCategory(exec, o.ID)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *CategorySlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *CategorySlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *CategorySlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty CategorySlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *CategorySlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	categories := CategorySlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), categoryPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"SELECT \"category\".* FROM \"category\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, categoryPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(categoryPrimaryKeyColumns), 1, len(categoryPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, query, args...)

	err := q.Bind(&categories)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in CategorySlice")
	}

	*o = categories

	return nil
}

// CategoryExists checks if the Category row exists.
func CategoryExists(exec boil.Executor, id int16) (bool, error) {
	var exists bool

	query := "select exists(select 1 from \"category\" where \"id\"=$1 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, id)
	}

	row := exec.QueryRow(query, id)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if category exists")
	}

	return exists, nil
}

// CategoryExistsG checks if the Category row exists.
func CategoryExistsG(id int16) (bool, error) {
	return CategoryExists(boil.GetDB(), id)
}

// CategoryExistsGP checks if the Category row exists. Panics on error.
func CategoryExistsGP(id int16) bool {
	e, err := CategoryExists(boil.GetDB(), id)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// CategoryExistsP checks if the Category row exists. Panics on error.
func CategoryExistsP(exec boil.Executor, id int16) bool {
	e, err := CategoryExists(exec, id)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
