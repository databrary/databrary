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
)

// LoginToken is an object representing the database table.
type LoginToken struct {
	Token    string    `db:"token" json:"loginToken_token"`
	Expires  time.Time `db:"expires" json:"loginToken_expires"`
	Account  int       `db:"account" json:"loginToken_account"`
	Password bool      `db:"password" json:"loginToken_password"`

	R *loginTokenR `db:"-" json:"-"`
	L loginTokenL  `db:"-" json:"-"`
}

// loginTokenR is where relationships are stored.
type loginTokenR struct {
	Account *Account
}

// loginTokenL is where Load methods for each relationship are stored.
type loginTokenL struct{}

var (
	loginTokenColumns               = []string{"token", "expires", "account", "password"}
	loginTokenColumnsWithoutDefault = []string{"token", "account"}
	loginTokenColumnsWithDefault    = []string{"expires", "password"}
	loginTokenColumnsWithCustom     = []string{}

	loginTokenPrimaryKeyColumns = []string{"token"}
)

type (
	// LoginTokenSlice is an alias for a slice of pointers to LoginToken.
	// This should generally be used opposed to []LoginToken.
	LoginTokenSlice []*LoginToken
	// LoginTokenHook is the signature for custom LoginToken hook methods
	LoginTokenHook func(boil.Executor, *LoginToken) error

	loginTokenQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	loginTokenType    = reflect.TypeOf(&LoginToken{})
	loginTokenMapping = queries.MakeStructMapping(loginTokenType)

	loginTokenPrimaryKeyMapping, _ = queries.BindMapping(loginTokenType, loginTokenMapping, loginTokenPrimaryKeyColumns)

	loginTokenInsertCacheMut sync.RWMutex
	loginTokenInsertCache    = make(map[string]insertCache)
	loginTokenUpdateCacheMut sync.RWMutex
	loginTokenUpdateCache    = make(map[string]updateCache)
	loginTokenUpsertCacheMut sync.RWMutex
	loginTokenUpsertCache    = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force bytes in case of primary key column that uses []byte (for relationship compares)
	_ = bytes.MinRead
)
var loginTokenBeforeInsertHooks []LoginTokenHook
var loginTokenBeforeUpdateHooks []LoginTokenHook
var loginTokenBeforeDeleteHooks []LoginTokenHook
var loginTokenBeforeUpsertHooks []LoginTokenHook

var loginTokenAfterInsertHooks []LoginTokenHook
var loginTokenAfterSelectHooks []LoginTokenHook
var loginTokenAfterUpdateHooks []LoginTokenHook
var loginTokenAfterDeleteHooks []LoginTokenHook
var loginTokenAfterUpsertHooks []LoginTokenHook

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *LoginToken) doBeforeInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenBeforeInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *LoginToken) doBeforeUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenBeforeUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *LoginToken) doBeforeDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenBeforeDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *LoginToken) doBeforeUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenBeforeUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *LoginToken) doAfterInsertHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenAfterInsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterSelectHooks executes all "after Select" hooks.
func (o *LoginToken) doAfterSelectHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenAfterSelectHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *LoginToken) doAfterUpdateHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenAfterUpdateHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *LoginToken) doAfterDeleteHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenAfterDeleteHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *LoginToken) doAfterUpsertHooks(exec boil.Executor) (err error) {
	for _, hook := range loginTokenAfterUpsertHooks {
		if err := hook(exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddLoginTokenHook registers your hook function for all future operations.
func AddLoginTokenHook(hookPoint boil.HookPoint, loginTokenHook LoginTokenHook) {
	switch hookPoint {
	case boil.BeforeInsertHook:
		loginTokenBeforeInsertHooks = append(loginTokenBeforeInsertHooks, loginTokenHook)
	case boil.BeforeUpdateHook:
		loginTokenBeforeUpdateHooks = append(loginTokenBeforeUpdateHooks, loginTokenHook)
	case boil.BeforeDeleteHook:
		loginTokenBeforeDeleteHooks = append(loginTokenBeforeDeleteHooks, loginTokenHook)
	case boil.BeforeUpsertHook:
		loginTokenBeforeUpsertHooks = append(loginTokenBeforeUpsertHooks, loginTokenHook)
	case boil.AfterInsertHook:
		loginTokenAfterInsertHooks = append(loginTokenAfterInsertHooks, loginTokenHook)
	case boil.AfterSelectHook:
		loginTokenAfterSelectHooks = append(loginTokenAfterSelectHooks, loginTokenHook)
	case boil.AfterUpdateHook:
		loginTokenAfterUpdateHooks = append(loginTokenAfterUpdateHooks, loginTokenHook)
	case boil.AfterDeleteHook:
		loginTokenAfterDeleteHooks = append(loginTokenAfterDeleteHooks, loginTokenHook)
	case boil.AfterUpsertHook:
		loginTokenAfterUpsertHooks = append(loginTokenAfterUpsertHooks, loginTokenHook)
	}
}

// OneP returns a single loginToken record from the query, and panics on error.
func (q loginTokenQuery) OneP() *LoginToken {
	o, err := q.One()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// One returns a single loginToken record from the query.
func (q loginTokenQuery) One() (*LoginToken, error) {
	o := &LoginToken{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(o)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for login_token")
	}

	if err := o.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
		return o, err
	}

	return o, nil
}

// AllP returns all LoginToken records from the query, and panics on error.
func (q loginTokenQuery) AllP() LoginTokenSlice {
	o, err := q.All()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return o
}

// All returns all LoginToken records from the query.
func (q loginTokenQuery) All() (LoginTokenSlice, error) {
	var o LoginTokenSlice

	err := q.Bind(&o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to LoginToken slice")
	}

	if len(loginTokenAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(queries.GetExecutor(q.Query)); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// CountP returns the count of all LoginToken records in the query, and panics on error.
func (q loginTokenQuery) CountP() int64 {
	c, err := q.Count()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return c
}

// Count returns the count of all LoginToken records in the query.
func (q loginTokenQuery) Count() (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count login_token rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table, and panics on error.
func (q loginTokenQuery) ExistsP() bool {
	e, err := q.Exists()
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// Exists checks if the row exists in the table.
func (q loginTokenQuery) Exists() (bool, error) {
	var count int64

	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRow().Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if login_token exists")
	}

	return count > 0, nil
}

// AccountG pointed to by the foreign key.
func (o *LoginToken) AccountG(mods ...qm.QueryMod) accountQuery {
	return o.AccountByFk(boil.GetDB(), mods...)
}

// Account pointed to by the foreign key.
func (o *LoginToken) AccountByFk(exec boil.Executor, mods ...qm.QueryMod) accountQuery {
	queryMods := []qm.QueryMod{
		qm.Where("id=?", o.Account),
	}

	queryMods = append(queryMods, mods...)

	query := Accounts(exec, queryMods...)
	queries.SetFrom(query.Query, "\"account\"")

	return query
}

// LoadAccount allows an eager lookup of values, cached into the
// loaded structs of the objects.
func (loginTokenL) LoadAccount(e boil.Executor, singular bool, maybeLoginToken interface{}) error {
	var slice []*LoginToken
	var object *LoginToken

	count := 1
	if singular {
		object = maybeLoginToken.(*LoginToken)
	} else {
		slice = *maybeLoginToken.(*LoginTokenSlice)
		count = len(slice)
	}

	args := make([]interface{}, count)
	if singular {
		if object.R == nil {
			object.R = &loginTokenR{}
		}
		args[0] = object.Account
	} else {
		for i, obj := range slice {
			if obj.R == nil {
				obj.R = &loginTokenR{}
			}
			args[i] = obj.Account
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

	if len(loginTokenAfterSelectHooks) != 0 {
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
		object.R.Account = resultSlice[0]
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.Account == foreign.ID {
				local.R.Account = foreign
				break
			}
		}
	}

	return nil
}

// SetAccountG of the login_token to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.LoginToken.
// Uses the global database handle.
func (o *LoginToken) SetAccountG(insert bool, related *Account) error {
	return o.SetAccount(boil.GetDB(), insert, related)
}

// SetAccountP of the login_token to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.LoginToken.
// Panics on error.
func (o *LoginToken) SetAccountP(exec boil.Executor, insert bool, related *Account) {
	if err := o.SetAccount(exec, insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetAccountGP of the login_token to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.LoginToken.
// Uses the global database handle and panics on error.
func (o *LoginToken) SetAccountGP(insert bool, related *Account) {
	if err := o.SetAccount(boil.GetDB(), insert, related); err != nil {
		panic(boil.WrapErr(err))
	}
}

// SetAccount of the login_token to the related item.
// Sets o.R.Account to related.
// Adds o to related.R.LoginToken.
func (o *LoginToken) SetAccount(exec boil.Executor, insert bool, related *Account) error {
	var err error
	if insert {
		if err = related.Insert(exec); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"login_token\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"account"}),
		strmangle.WhereClause("\"", "\"", 2, loginTokenPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.Token}

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, updateQuery)
		fmt.Fprintln(boil.DebugWriter, values)
	}

	if _, err = exec.Exec(updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.Account = related.ID

	if o.R == nil {
		o.R = &loginTokenR{
			Account: related,
		}
	} else {
		o.R.Account = related
	}

	if related.R == nil {
		related.R = &accountR{
			LoginToken: o,
		}
	} else {
		related.R.LoginToken = o
	}

	return nil
}

// LoginTokensG retrieves all records.
func LoginTokensG(mods ...qm.QueryMod) loginTokenQuery {
	return LoginTokens(boil.GetDB(), mods...)
}

// LoginTokens retrieves all the records using an executor.
func LoginTokens(exec boil.Executor, mods ...qm.QueryMod) loginTokenQuery {
	mods = append(mods, qm.From("\"login_token\""))
	return loginTokenQuery{NewQuery(exec, mods...)}
}

// FindLoginTokenG retrieves a single record by ID.
func FindLoginTokenG(token string, selectCols ...string) (*LoginToken, error) {
	return FindLoginToken(boil.GetDB(), token, selectCols...)
}

// FindLoginTokenGP retrieves a single record by ID, and panics on error.
func FindLoginTokenGP(token string, selectCols ...string) *LoginToken {
	retobj, err := FindLoginToken(boil.GetDB(), token, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// FindLoginToken retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindLoginToken(exec boil.Executor, token string, selectCols ...string) (*LoginToken, error) {
	loginTokenObj := &LoginToken{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"login_token\" where \"token\"=$1", sel,
	)

	q := queries.Raw(exec, query, token)

	err := q.Bind(loginTokenObj)
	if err != nil {
		if errors.Cause(err) == sql.ErrNoRows {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from login_token")
	}

	return loginTokenObj, nil
}

// FindLoginTokenP retrieves a single record by ID with an executor, and panics on error.
func FindLoginTokenP(exec boil.Executor, token string, selectCols ...string) *LoginToken {
	retobj, err := FindLoginToken(exec, token, selectCols...)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return retobj
}

// InsertG a single record. See Insert for whitelist behavior description.
func (o *LoginToken) InsertG(whitelist ...string) error {
	return o.Insert(boil.GetDB(), whitelist...)
}

// InsertGP a single record, and panics on error. See Insert for whitelist
// behavior description.
func (o *LoginToken) InsertGP(whitelist ...string) {
	if err := o.Insert(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// InsertP a single record using an executor, and panics on error. See Insert
// for whitelist behavior description.
func (o *LoginToken) InsertP(exec boil.Executor, whitelist ...string) {
	if err := o.Insert(exec, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Insert a single record using an executor.
// Whitelist behavior: If a whitelist is provided, only those columns supplied are inserted
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns without a default value are included (i.e. name, age)
// - All columns with a default, but non-zero are included (i.e. health = 75)
func (o *LoginToken) Insert(exec boil.Executor, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no login_token provided for insertion")
	}

	var err error

	if err := o.doBeforeInsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(loginTokenColumnsWithDefault, o)

	key := makeCacheKey(whitelist, nzDefaults)
	loginTokenInsertCacheMut.RLock()
	cache, cached := loginTokenInsertCache[key]
	loginTokenInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := strmangle.InsertColumnSet(
			loginTokenColumns,
			loginTokenColumnsWithDefault,
			loginTokenColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)

		cache.valueMapping, err = queries.BindMapping(loginTokenType, loginTokenMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(loginTokenType, loginTokenMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"login_token\" (\"%s\") VALUES (%s)", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.IndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"login_token\" DEFAULT VALUES"
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
		return errors.Wrap(err, "models: unable to insert into login_token")
	}

	if !cached {
		loginTokenInsertCacheMut.Lock()
		loginTokenInsertCache[key] = cache
		loginTokenInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(exec)
}

// UpdateG a single LoginToken record. See Update for
// whitelist behavior description.
func (o *LoginToken) UpdateG(whitelist ...string) error {
	return o.Update(boil.GetDB(), whitelist...)
}

// UpdateGP a single LoginToken record.
// UpdateGP takes a whitelist of column names that should be updated.
// Panics on error. See Update for whitelist behavior description.
func (o *LoginToken) UpdateGP(whitelist ...string) {
	if err := o.Update(boil.GetDB(), whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateP uses an executor to update the LoginToken, and panics on error.
// See Update for whitelist behavior description.
func (o *LoginToken) UpdateP(exec boil.Executor, whitelist ...string) {
	err := o.Update(exec, whitelist...)
	if err != nil {
		panic(boil.WrapErr(err))
	}
}

// Update uses an executor to update the LoginToken.
// Whitelist behavior: If a whitelist is provided, only the columns given are updated.
// No whitelist behavior: Without a whitelist, columns are inferred by the following rules:
// - All columns are inferred to start with
// - All primary keys are subtracted from this set
// Update does not automatically update the record in case of default values. Use .Reload()
// to refresh the records.
func (o *LoginToken) Update(exec boil.Executor, whitelist ...string) error {
	var err error
	if err = o.doBeforeUpdateHooks(exec); err != nil {
		return err
	}
	key := makeCacheKey(whitelist, nil)
	loginTokenUpdateCacheMut.RLock()
	cache, cached := loginTokenUpdateCache[key]
	loginTokenUpdateCacheMut.RUnlock()

	if !cached {
		wl := strmangle.UpdateColumnSet(loginTokenColumns, loginTokenPrimaryKeyColumns, whitelist)
		if len(wl) == 0 {
			return errors.New("models: unable to update login_token, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"login_token\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, loginTokenPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(loginTokenType, loginTokenMapping, append(wl, loginTokenPrimaryKeyColumns...))
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
		return errors.Wrap(err, "models: unable to update login_token row")
	}

	if !cached {
		loginTokenUpdateCacheMut.Lock()
		loginTokenUpdateCache[key] = cache
		loginTokenUpdateCacheMut.Unlock()
	}

	return o.doAfterUpdateHooks(exec)
}

// UpdateAllP updates all rows with matching column names, and panics on error.
func (q loginTokenQuery) UpdateAllP(cols M) {
	if err := q.UpdateAll(cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values.
func (q loginTokenQuery) UpdateAll(cols M) error {
	queries.SetUpdate(q.Query, cols)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to update all for login_token")
	}

	return nil
}

// UpdateAllG updates all rows with the specified column values.
func (o LoginTokenSlice) UpdateAllG(cols M) error {
	return o.UpdateAll(boil.GetDB(), cols)
}

// UpdateAllGP updates all rows with the specified column values, and panics on error.
func (o LoginTokenSlice) UpdateAllGP(cols M) {
	if err := o.UpdateAll(boil.GetDB(), cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAllP updates all rows with the specified column values, and panics on error.
func (o LoginTokenSlice) UpdateAllP(exec boil.Executor, cols M) {
	if err := o.UpdateAll(exec, cols); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o LoginTokenSlice) UpdateAll(exec boil.Executor, cols M) error {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), loginTokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"UPDATE \"login_token\" SET %s WHERE (\"token\") IN (%s)",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(loginTokenPrimaryKeyColumns), len(colNames)+1, len(loginTokenPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to update all in loginToken slice")
	}

	return nil
}

// UpsertG attempts an insert, and does an update or ignore on conflict.
func (o *LoginToken) UpsertG(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	return o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...)
}

// UpsertGP attempts an insert, and does an update or ignore on conflict. Panics on error.
func (o *LoginToken) UpsertGP(updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(boil.GetDB(), updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// UpsertP attempts an insert using an executor, and does an update or ignore on conflict.
// UpsertP panics on error.
func (o *LoginToken) UpsertP(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) {
	if err := o.Upsert(exec, updateOnConflict, conflictColumns, updateColumns, whitelist...); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
func (o *LoginToken) Upsert(exec boil.Executor, updateOnConflict bool, conflictColumns []string, updateColumns []string, whitelist ...string) error {
	if o == nil {
		return errors.New("models: no login_token provided for upsert")
	}

	if err := o.doBeforeUpsertHooks(exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(loginTokenColumnsWithDefault, o)

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

	loginTokenUpsertCacheMut.RLock()
	cache, cached := loginTokenUpsertCache[key]
	loginTokenUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		var ret []string
		whitelist, ret = strmangle.InsertColumnSet(
			loginTokenColumns,
			loginTokenColumnsWithDefault,
			loginTokenColumnsWithoutDefault,
			nzDefaults,
			whitelist,
		)
		update := strmangle.UpdateColumnSet(
			loginTokenColumns,
			loginTokenPrimaryKeyColumns,
			updateColumns,
		)
		if len(update) == 0 {
			return errors.New("models: unable to upsert login_token, could not build update column list")
		}

		conflict := conflictColumns
		if len(conflict) == 0 {
			conflict = make([]string, len(loginTokenPrimaryKeyColumns))
			copy(conflict, loginTokenPrimaryKeyColumns)
		}
		cache.query = queries.BuildUpsertQueryPostgres(dialect, "\"login_token\"", updateOnConflict, ret, update, conflict, whitelist)

		cache.valueMapping, err = queries.BindMapping(loginTokenType, loginTokenMapping, whitelist)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(loginTokenType, loginTokenMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert login_token")
	}

	if !cached {
		loginTokenUpsertCacheMut.Lock()
		loginTokenUpsertCache[key] = cache
		loginTokenUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(exec)
}

// DeleteP deletes a single LoginToken record with an executor.
// DeleteP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *LoginToken) DeleteP(exec boil.Executor) {
	if err := o.Delete(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteG deletes a single LoginToken record.
// DeleteG will match against the primary key column to find the record to delete.
func (o *LoginToken) DeleteG() error {
	if o == nil {
		return errors.New("models: no LoginToken provided for deletion")
	}

	return o.Delete(boil.GetDB())
}

// DeleteGP deletes a single LoginToken record.
// DeleteGP will match against the primary key column to find the record to delete.
// Panics on error.
func (o *LoginToken) DeleteGP() {
	if err := o.DeleteG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// Delete deletes a single LoginToken record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *LoginToken) Delete(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no LoginToken provided for delete")
	}

	if err := o.doBeforeDeleteHooks(exec); err != nil {
		return err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), loginTokenPrimaryKeyMapping)
	query := "DELETE FROM \"login_token\" WHERE \"token\"=$1"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args...)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete from login_token")
	}

	if err := o.doAfterDeleteHooks(exec); err != nil {
		return err
	}

	return nil
}

// DeleteAllP deletes all rows, and panics on error.
func (q loginTokenQuery) DeleteAllP() {
	if err := q.DeleteAll(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all matching rows.
func (q loginTokenQuery) DeleteAll() error {
	if q.Query == nil {
		return errors.New("models: no loginTokenQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	_, err := q.Query.Exec()
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from login_token")
	}

	return nil
}

// DeleteAllGP deletes all rows in the slice, and panics on error.
func (o LoginTokenSlice) DeleteAllGP() {
	if err := o.DeleteAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAllG deletes all rows in the slice.
func (o LoginTokenSlice) DeleteAllG() error {
	if o == nil {
		return errors.New("models: no LoginToken slice provided for delete all")
	}
	return o.DeleteAll(boil.GetDB())
}

// DeleteAllP deletes all rows in the slice, using an executor, and panics on error.
func (o LoginTokenSlice) DeleteAllP(exec boil.Executor) {
	if err := o.DeleteAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o LoginTokenSlice) DeleteAll(exec boil.Executor) error {
	if o == nil {
		return errors.New("models: no LoginToken slice provided for delete all")
	}

	if len(o) == 0 {
		return nil
	}

	if len(loginTokenBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), loginTokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"DELETE FROM \"login_token\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, loginTokenPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(o)*len(loginTokenPrimaryKeyColumns), 1, len(loginTokenPrimaryKeyColumns)),
	)

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, args)
	}

	_, err := exec.Exec(query, args...)
	if err != nil {
		return errors.Wrap(err, "models: unable to delete all from loginToken slice")
	}

	if len(loginTokenAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(exec); err != nil {
				return err
			}
		}
	}

	return nil
}

// ReloadGP refetches the object from the database and panics on error.
func (o *LoginToken) ReloadGP() {
	if err := o.ReloadG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadP refetches the object from the database with an executor. Panics on error.
func (o *LoginToken) ReloadP(exec boil.Executor) {
	if err := o.Reload(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadG refetches the object from the database using the primary keys.
func (o *LoginToken) ReloadG() error {
	if o == nil {
		return errors.New("models: no LoginToken provided for reload")
	}

	return o.Reload(boil.GetDB())
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *LoginToken) Reload(exec boil.Executor) error {
	ret, err := FindLoginToken(exec, o.Token)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAllGP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *LoginTokenSlice) ReloadAllGP() {
	if err := o.ReloadAllG(); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllP refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
// Panics on error.
func (o *LoginTokenSlice) ReloadAllP(exec boil.Executor) {
	if err := o.ReloadAll(exec); err != nil {
		panic(boil.WrapErr(err))
	}
}

// ReloadAllG refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *LoginTokenSlice) ReloadAllG() error {
	if o == nil {
		return errors.New("models: empty LoginTokenSlice provided for reload all")
	}

	return o.ReloadAll(boil.GetDB())
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *LoginTokenSlice) ReloadAll(exec boil.Executor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	loginTokens := LoginTokenSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), loginTokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	query := fmt.Sprintf(
		"SELECT \"login_token\".* FROM \"login_token\" WHERE (%s) IN (%s)",
		strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, loginTokenPrimaryKeyColumns), ","),
		strmangle.Placeholders(dialect.IndexPlaceholders, len(*o)*len(loginTokenPrimaryKeyColumns), 1, len(loginTokenPrimaryKeyColumns)),
	)

	q := queries.Raw(exec, query, args...)

	err := q.Bind(&loginTokens)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in LoginTokenSlice")
	}

	*o = loginTokens

	return nil
}

// LoginTokenExists checks if the LoginToken row exists.
func LoginTokenExists(exec boil.Executor, token string) (bool, error) {
	var exists bool

	query := "select exists(select 1 from \"login_token\" where \"token\"=$1 limit 1)"

	if boil.DebugMode {
		fmt.Fprintln(boil.DebugWriter, query)
		fmt.Fprintln(boil.DebugWriter, token)
	}

	row := exec.QueryRow(query, token)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if login_token exists")
	}

	return exists, nil
}

// LoginTokenExistsG checks if the LoginToken row exists.
func LoginTokenExistsG(token string) (bool, error) {
	return LoginTokenExists(boil.GetDB(), token)
}

// LoginTokenExistsGP checks if the LoginToken row exists. Panics on error.
func LoginTokenExistsGP(token string) bool {
	e, err := LoginTokenExists(boil.GetDB(), token)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}

// LoginTokenExistsP checks if the LoginToken row exists. Panics on error.
func LoginTokenExistsP(exec boil.Executor, token string) bool {
	e, err := LoginTokenExists(exec, token)
	if err != nil {
		panic(boil.WrapErr(err))
	}

	return e
}
