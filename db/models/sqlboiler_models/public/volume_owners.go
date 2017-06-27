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

// VolumeOwner is an object representing the database table.
type VolumeOwner struct {
	Volume int               `db:"volume" json:"volumeOwner_volume"`
	Owners types.StringArray `db:"owners" json:"volumeOwner_owners"`

	R *volumeOwnerR `db:"-" json:"-"`
	L volumeOwnerL  `db:"-" json:"-"`
}

// volumeOwnerR is where relationships are stored.
type volumeOwnerR struct {
	Volume *Volume
}

// volumeOwnerL is where Load methods for each relationship are stored.
type volumeOwnerL struct{}

var (
	volumeOwnerColumns               = []string{"volume", "owners"}
	volumeOwnerColumnsWithoutDefault = []string{"volume"}
	volumeOwnerColumnsWithDefault    = []string{"owners"}
	volumeOwnerColumnsWithCustom     = []string{}

	volumeOwnerPrimaryKeyColumns = []string{"volume"}
)

type (
	// VolumeOwnerSlice is an alias for a slice of pointers to VolumeOwner.
	// This should generally be used opposed to []VolumeOwner.
	VolumeOwnerSlice []*VolumeOwner
	// VolumeOwnerHook is the signature for custom VolumeOwner hook methods
	VolumeOwnerHook func(boil.Executor, *VolumeOwner) error

	volumeOwnerQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	volumeOwnerType    = reflect.TypeOf(&VolumeOwner{})
	volumeOwnerMapping = queries.MakeStructMapping(volumeOwnerType)

	volumeOwnerPrimaryKeyMapping, _ = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, volumeOwnerPrimaryKeyColumns)

	volumeOwnerInsertCacheMut sync.RWMutex
	volumeOwnerInsertCache    = make(map[string]insertCache)
	volumeOwnerUpdateCacheMut sync.RWMutex
	volumeOwnerUpdateCache    = make(map[string]updateCache)
	volumeOwnerUpsertCacheMut sync.RWMutex
	volumeOwnerUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var volumeOwnerBeforeInsertHooks []VolumeOwnerHook
var volumeOwnerBeforeUpdateHooks []VolumeOwnerHook
var volumeOwnerBeforeDeleteHooks []VolumeOwnerHook
var volumeOwnerBeforeUpsertHooks []VolumeOwnerHook

var volumeOwnerAfterInsertHooks []VolumeOwnerHook
var volumeOwnerAfterSelectHooks []VolumeOwnerHook
var volumeOwnerAfterUpdateHooks []VolumeOwnerHook
var volumeOwnerAfterDeleteHooks []VolumeOwnerHook
var volumeOwnerAfterUpsertHooks []VolumeOwnerHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *VolumeOwner) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *VolumeOwner) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *VolumeOwner) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *VolumeOwner) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *VolumeOwner) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *VolumeOwner) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *VolumeOwner) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *VolumeOwner) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *VolumeOwner) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range volumeOwnerAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddVolumeOwnerHook registers your hook function for all future operations.
func AddVolumeOwnerHook(hookPoint boil.HookPoint, volumeOwnerHook VolumeOwnerHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		volumeOwnerBeforeInsertHooks = append(volumeOwnerBeforeInsertHooks, volumeOwnerHook)
	case boil.BeforeUpdateHook:
		volumeOwnerBeforeUpdateHooks = append(volumeOwnerBeforeUpdateHooks, volumeOwnerHook)
	case boil.BeforeDeleteHook:
		volumeOwnerBeforeDeleteHooks = append(volumeOwnerBeforeDeleteHooks, volumeOwnerHook)
	case boil.BeforeUpsertHook:
		volumeOwnerBeforeUpsertHooks = append(volumeOwnerBeforeUpsertHooks, volumeOwnerHook)
	case boil.AfterInsertHook:
		volumeOwnerAfterInsertHooks = append(volumeOwnerAfterInsertHooks, volumeOwnerHook)
	case boil.AfterSelectHook:
		volumeOwnerAfterSelectHooks = append(volumeOwnerAfterSelectHooks, volumeOwnerHook)
	case boil.AfterUpdateHook:
		volumeOwnerAfterUpdateHooks = append(volumeOwnerAfterUpdateHooks, volumeOwnerHook)
	case boil.AfterDeleteHook:
		volumeOwnerAfterDeleteHooks = append(volumeOwnerAfterDeleteHooks, volumeOwnerHook)
	case boil.AfterUpsertHook:
		volumeOwnerAfterUpsertHooks = append(volumeOwnerAfterUpsertHooks, volumeOwnerHook)
	}
}

// OneP returns a single volumeOwner record from the query, and panics on error.
func (q volumeOwnerQuery) OneP() *VolumeOwner {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single volumeOwner record from the query.
func (q volumeOwnerQuery) One() (*VolumeOwner, error) {
	o := &VolumeOwner{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for volume_owners")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all VolumeOwner records from the query, and panics on error.
func (q volumeOwnerQuery) AllP() VolumeOwnerSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all VolumeOwner records from the query.
func (q volumeOwnerQuery) All() (VolumeOwnerSlice, error) {
	var o VolumeOwnerSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to VolumeOwner slice")
	}

	if len(volumeOwnerAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all VolumeOwner records in the query, and panics on error.
func (q volumeOwnerQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all VolumeOwner records in the query.
func (q volumeOwnerQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count volume_owners rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q volumeOwnerQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q volumeOwnerQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if volume_owners exists")
	}

	return count > 0, nil
}

// VolumeG pointed to by the foreign key.
func (o *VolumeOwner) VolumeG(mods ...qm.QueryMod) volumeQuery {
	return o.VolumeByFk(boil.GetDB(), mods...)
}

// Volume pointed to by the foreign key.
func (o *VolumeOwner) VolumeByFk(exec boil.Executor, mods ...qm.QueryMod) volumeQuery {
	queryMods := []qm.QueryMod{
		qm.Where("id=?", o.Volume),
	}

	queryMods = append(queryMods, mods...)

	query := Volumes(exec, queryMods...)
	queries.SetFrom(query.Query, "\"volume\"")

	return query
}

// LoadVolume allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (volumeOwnerL) LoadVolume(e boil.Executor, singular bool, maybeVolumeOwner interface{}) error {
	var slice []*VolumeOwner
	var object *VolumeOwner

	count := 1
	if singular {
		object = maybeVolumeOwner.(*VolumeOwner)
	} else {
		slice = *maybeVolumeOwner.(*VolumeOwnerSlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &volumeOwnerR{}
		}
		args[0] = object.Volume
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &volumeOwnerR{}
			}
			args[i] = obj.Volume
		}
	}

	query := fmt.Sprintf(
		"select * from \"volume\" where \"id\" in (%s)",
		strmangle.Placeholders(dialect.IndexPlaceholders, count, 1, 1),
	)

	if boil.DebugMode {
		fmt.Fprintf(boil.DebugWriter, "%s\n%v\n", query, args)
	}

	results, err := e.Query(query, args...)
	if err != nil {
		return errors.Wrap(err, "failed to eager load Volume")
	}
	defer results.Close()

	var resultSlice []*Volume
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice Volume")
	}

	if len(volumeOwnerAfterSelectHooks) != 0 {
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
		object.R.Volume = resultSlice[0]
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.Volume == foreign.ID {
				local.R.Volume = foreign
				break
			}
		}
	}

	return nil
}

// SetVolumeG of the volume_owner to the related item.
// Sets o.R.Volume to related.
// Adds o to related.R.VolumeOwner.
// Uses the global database handle.
func (o *VolumeOwner) SetVolumeG(insert bool, related *Volume) error {
	return o.SetVolume(boil.GetDB(), insert, related)
}

// SetVolumeP of the volume_owner to the related item.
// Sets o.R.Volume to related.
// Adds o to related.R.VolumeOwner.
// Panics on error.
func (o *VolumeOwner) SetVolumeP(exec boil.Executor, insert bool, related *Volume) {
	if err := o.SetVolume(exec, insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetVolumeGP of the volume_owner to the related item.
// Sets o.R.Volume to related.
// Adds o to related.R.VolumeOwner.
// Uses the global database handle and panics on error.
func (o *VolumeOwner) SetVolumeGP(insert bool, related *Volume) {
	if err := o.SetVolume(boil.GetDB(), insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetVolume of the volume_owner to the related item.
// Sets o.R.Volume to related.
// Adds o to related.R.VolumeOwner.
func (o *VolumeOwner) SetVolume(exec boil.Executor, insert bool, related *Volume) error {
	var err error
	if insert {
		if err = related.Insert(exec); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"volume_owners\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"volume"}),
		strmangle.WhereClause("\"", "\"", 2, volumeOwnerPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.Volume}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, updateQuery)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	if _, err = exec.Exec(updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.Volume = related.ID

	if o.R == nil {
		o.R = &volumeOwnerR{
			Volume: related,
		}
	} else {
		o.R.Volume = related
	}

	if related.R == nil {
		related.R = &volumeR{
			VolumeOwner: o,
		}
	} else {
		related.R.VolumeOwner = o
	}

	return nil
}

// VolumeOwnersG retrieves all records.
func VolumeOwnersG(mods ...qm.QueryMod) volumeOwnerQuery {
	return VolumeOwners(boil.GetDB(), mods...)
}

// VolumeOwners retrieves all the records using an executor.
func VolumeOwners(exec boil.Executor, mods ...qm.QueryMod) volumeOwnerQuery {
	mods = append(mods, qm.From("\"volume_owners\""))
	return volumeOwnerQuery{NewQuery(exec, mods...)}
}

// FindVolumeOwnerG retrieves a single record by ID.
func FindVolumeOwnerG(volume int, selectCols ...string) (*VolumeOwner, error) {
	return FindVolumeOwner(boil.GetDB(), volume, selectCols...)
}

// FindVolumeOwnerGP retrieves a single record by ID, and panics on error.
func FindVolumeOwnerGP(volume int, selectCols ...string) *VolumeOwner {
	retobj, err := FindVolumeOwner(boil.GetDB(), volume, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindVolumeOwner retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindVolumeOwner(exec boil.Executor, volume int, selectCols ...string) (*VolumeOwner, error) {
	volumeOwnerObj := &VolumeOwner{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"volume_owners\" where \"volume\"=$1", sel,
	)

	q := queries.Raw(exec, query, volume)

	err := q.Bind(volumeOwnerObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from volume_owners")
	}

	return volumeOwnerObj, nil
}

// FindVolumeOwnerP retrieves a single record by ID with an executor, and panics on error.
func FindVolumeOwnerP(exec boil.Executor, volume int, selectCols ...string) *VolumeOwner {
	retobj, err := FindVolumeOwner(exec, volume, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *VolumeOwner) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *VolumeOwner) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *VolumeOwner) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *VolumeOwner) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no volume_owners provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(volumeOwnerColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	volumeOwnerInsertCacheMut.RLock()
	cache, cached := volumeOwnerInsertCache[key]
	volumeOwnerInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			volumeOwnerColumns,
			volumeOwnerColumnsWithDefault,
			volumeOwnerColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"volume_owners\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"volume_owners\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into volume_owners")
	}

	if !cached {
		volumeOwnerInsertCacheMut.Lock()
		volumeOwnerInsertCache[key] = cache
		volumeOwnerInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single VolumeOwner record. See Update for
// whitelist behavior description.
func (o *VolumeOwner) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single VolumeOwner record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *VolumeOwner) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the VolumeOwner, and panics on error.
// See Update for whitelist behavior description.
func (o *VolumeOwner) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the VolumeOwner.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *VolumeOwner) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	volumeOwnerUpdateCacheMut.RLock()
	cache, cached := volumeOwnerUpdateCache[key]
	volumeOwnerUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(volumeOwnerColumns, volumeOwnerPrimaryKeyColumns, whitelist)
		if len(wl) == 0 {
			return errors.New("models: unable to update volume_owners, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"volume_owners\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, volumeOwnerPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, append(wl, volumeOwnerPrimaryKeyColumns...))
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
		return errors.Wrap(err, "models: unable to update volume_owners row")
	}

	if !cached {
		volumeOwnerUpdateCacheMut.Lock()
		volumeOwnerUpdateCache[key] = cache
		volumeOwnerUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q volumeOwnerQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q volumeOwnerQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for volume_owners")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o VolumeOwnerSlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o VolumeOwnerSlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o VolumeOwnerSlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o VolumeOwnerSlice) UpdateAll(exec boil.Executor, cols M) error {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), volumeOwnerPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"UPDATE \"volume_owners\" SET %s WHERE (\"volume\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(volumeOwnerPrimaryKeyColumns), len(colNames)+1, len(volumeOwnerPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in volumeOwner slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *VolumeOwner) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *VolumeOwner) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *VolumeOwner) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *VolumeOwner) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no volume_owners provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(volumeOwnerColumnsWithDefault, o)

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

	volumeOwnerUpsertCacheMut.RLock()
	cache, cached := volumeOwnerUpsertCache[key]
	volumeOwnerUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			volumeOwnerColumns,
			volumeOwnerColumnsWithDefault,
			volumeOwnerColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			volumeOwnerColumns,
			volumeOwnerPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert volume_owners, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(volumeOwnerPrimaryKeyColumns))
			copy(conflict, volumeOwnerPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"volume_owners\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(volumeOwnerType, volumeOwnerMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert volume_owners")
	}

	if !cached {
		volumeOwnerUpsertCacheMut.Lock()
		volumeOwnerUpsertCache[key] = cache
		volumeOwnerUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single VolumeOwner record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *VolumeOwner) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single VolumeOwner record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *VolumeOwner) DeleteG() error {
	if o == nil {
		return errors.New("models: no VolumeOwner provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single VolumeOwner record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *VolumeOwner) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single VolumeOwner record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *VolumeOwner) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no VolumeOwner provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), volumeOwnerPrimaryKeyMapping)
	query := "DELETE FROM \"volume_owners\" WHERE \"volume\"=$1"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from volume_owners")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q volumeOwnerQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q volumeOwnerQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no volumeOwnerQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from volume_owners")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o VolumeOwnerSlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o VolumeOwnerSlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no VolumeOwner slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o VolumeOwnerSlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o VolumeOwnerSlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no VolumeOwner slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(volumeOwnerBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), volumeOwnerPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"DELETE FROM \"volume_owners\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, volumeOwnerPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(volumeOwnerPrimaryKeyColumns), 1, len(volumeOwnerPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from volumeOwner slice")
	}

	if len(volumeOwnerAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *VolumeOwner) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *VolumeOwner) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *VolumeOwner) ReloadG() error {
	if o == nil {
		return errors.New("models: no VolumeOwner provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *VolumeOwner) Reload(exec boil.Executor) error {
	ret, err := FindVolumeOwner(exec, o.Volume)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *VolumeOwnerSlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *VolumeOwnerSlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *VolumeOwnerSlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty VolumeOwnerSlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *VolumeOwnerSlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	volumeOwners := VolumeOwnerSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), volumeOwnerPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"SELECT \"volume_owners\".* FROM \"volume_owners\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, volumeOwnerPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(volumeOwnerPrimaryKeyColumns), 1, len(volumeOwnerPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, query, args...)

	err := q.Bind(&volumeOwners)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in VolumeOwnerSlice")
	}

	*o = volumeOwners

	return nil
}

// VolumeOwnerExists checks if the VolumeOwner row exists.
func VolumeOwnerExists(exec boil.Executor, volume int) (bool, error) {
	var exists bool

	query := "select exists(select 1 from \"volume_owners\" where \"volume\"=$1 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, volume)
	}

	row := exec.QueryRow(query, volume)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if volume_owners exists")
	}

	return exists, nil
}

// VolumeOwnerExistsG checks if the VolumeOwner row exists.
func VolumeOwnerExistsG(volume int) (bool, error) {
	return VolumeOwnerExists(boil.GetDB(), volume)
}

// VolumeOwnerExistsGP checks if the VolumeOwner row exists. Panics on error.
func VolumeOwnerExistsGP(volume int) bool {
	e, err := VolumeOwnerExists(boil.GetDB(), volume)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// VolumeOwnerExistsP checks if the VolumeOwner row exists. Panics on error.
func VolumeOwnerExistsP(exec boil.Executor, volume int) bool {
	e, err := VolumeOwnerExists(exec, volume)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
