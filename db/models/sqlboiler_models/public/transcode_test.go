// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package public

import (
	"bytes"
	"os"
	"os/exec"
	"reflect"
	"sort"
	"strings"
	"testing"

	"github.com/databrary/databrary/db/models/custom_types"
	"github.com/databrary/sqlboiler/boil"
	"github.com/databrary/sqlboiler/randomize"
	"github.com/databrary/sqlboiler/strmangle"
	"github.com/pmezard/go-difflib/difflib"
)

func testTranscodes(t *testing.T) {
	t.Parallel()

	query := Transcodes(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testTranscodesLive(t *testing.T) {
	all, err := Transcodes(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all Transcodes err: ", err)
	}
	tx, err := dbMain.liveTestDbConn.Begin()
	if err != nil {
		t.Fatalf("failed to begin transaction: ", err)
	}
	for _, v := range all {
		err := v.Insert(tx)
		if err != nil {
			t.Fatalf("failed to failed to insert %s because of %s", v, err)
		}

	}
	err = tx.Commit()
	if err != nil {
		t.Fatalf("failed to commit transaction: ", err)
	}
	bf := &bytes.Buffer{}
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM transcode) TO STDOUT" -d `, dbMain.DbName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bf
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from live db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from live db because of %s", err)
	}
	bg := &bytes.Buffer{}
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM transcode) TO STDOUT" -d `, dbMain.LiveTestDBName)
	dumpCmd.Env = append(os.Environ(), dbMain.pgEnv()...)
	dumpCmd.Stdout = bg
	err = dumpCmd.Start()
	if err != nil {
		t.Fatalf("failed to start dump from test db because of %s", err)
	}
	dumpCmd.Wait()
	if err != nil {
		t.Fatalf("failed to wait dump from test db because of %s", err)
	}
	bfslice := sort.StringSlice(difflib.SplitLines(bf.String()))
	gfslice := sort.StringSlice(difflib.SplitLines(bg.String()))
	bfslice.Sort()
	gfslice.Sort()
	diff := difflib.ContextDiff{
		A:        bfslice,
		B:        gfslice,
		FromFile: "databrary",
		ToFile:   "test",
		Context:  1,
	}
	result, _ := difflib.GetContextDiffString(diff)
	if len(result) > 0 {
		t.Fatalf("TranscodesLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

func testTranscodesDelete(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = transcode.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTranscodesQueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Transcodes(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTranscodesSliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TranscodeSlice{transcode}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTranscodesExists(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := TranscodeExists(tx, transcode.Asset)
	if err != nil {
		t.Errorf("Unable to check if Transcode exists: %s", err)
	}
	if !e {
		t.Errorf("Expected TranscodeExistsG to return true, but got false.")
	}
}

func testTranscodesFind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	transcodeFound, err := FindTranscode(tx, transcode.Asset)
	if err != nil {
		t.Error(err)
	}

	if transcodeFound == nil {
		t.Error("want a record, got nil")
	}
}

func testTranscodesBind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Transcodes(tx).Bind(transcode); err != nil {
		t.Error(err)
	}
}

func testTranscodesOne(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := Transcodes(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testTranscodesAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcodeOne := &Transcode{}
	transcodeTwo := &Transcode{}
	if err = randomize.Struct(seed, transcodeOne, transcodeDBTypes, false, transcodeColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	if err = randomize.Struct(seed, transcodeTwo, transcodeDBTypes, false, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcodeOne.Segment = custom_types.SegmentRandom()
	transcodeTwo.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcodeOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = transcodeTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Transcodes(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testTranscodesCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcodeOne := &Transcode{}
	transcodeTwo := &Transcode{}
	if err = randomize.Struct(seed, transcodeOne, transcodeDBTypes, false, transcodeColumnsWithCustom...); err != nil {

		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	if err = randomize.Struct(seed, transcodeTwo, transcodeDBTypes, false, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcodeOne.Segment = custom_types.SegmentRandom()
	transcodeTwo.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcodeOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = transcodeTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func transcodeBeforeInsertHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeAfterInsertHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeAfterSelectHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeBeforeUpdateHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeAfterUpdateHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeBeforeDeleteHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeAfterDeleteHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeBeforeUpsertHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func transcodeAfterUpsertHook(e boil.Executor, o *Transcode) error {
	*o = Transcode{}
	return nil
}

func testTranscodesHooks(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	empty := &Transcode{}

	AddTranscodeHook(boil.BeforeInsertHook, transcodeBeforeInsertHook)
	if err = transcode.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", transcode)
	}
	transcodeBeforeInsertHooks = []TranscodeHook{}

	AddTranscodeHook(boil.AfterInsertHook, transcodeAfterInsertHook)
	if err = transcode.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", transcode)
	}
	transcodeAfterInsertHooks = []TranscodeHook{}

	AddTranscodeHook(boil.AfterSelectHook, transcodeAfterSelectHook)
	if err = transcode.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", transcode)
	}
	transcodeAfterSelectHooks = []TranscodeHook{}

	AddTranscodeHook(boil.BeforeUpdateHook, transcodeBeforeUpdateHook)
	if err = transcode.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", transcode)
	}
	transcodeBeforeUpdateHooks = []TranscodeHook{}

	AddTranscodeHook(boil.AfterUpdateHook, transcodeAfterUpdateHook)
	if err = transcode.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", transcode)
	}
	transcodeAfterUpdateHooks = []TranscodeHook{}

	AddTranscodeHook(boil.BeforeDeleteHook, transcodeBeforeDeleteHook)
	if err = transcode.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", transcode)
	}
	transcodeBeforeDeleteHooks = []TranscodeHook{}

	AddTranscodeHook(boil.AfterDeleteHook, transcodeAfterDeleteHook)
	if err = transcode.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", transcode)
	}
	transcodeAfterDeleteHooks = []TranscodeHook{}

	AddTranscodeHook(boil.BeforeUpsertHook, transcodeBeforeUpsertHook)
	if err = transcode.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", transcode)
	}
	transcodeBeforeUpsertHooks = []TranscodeHook{}

	AddTranscodeHook(boil.AfterUpsertHook, transcodeAfterUpsertHook)
	if err = transcode.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(transcode, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", transcode)
	}
	transcodeAfterUpsertHooks = []TranscodeHook{}
}
func testTranscodesInsert(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTranscodesInsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx, transcodeColumns...); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTranscodeToOneAssetUsingAsset(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Asset
	var local Transcode

	foreignBlacklist := assetColumnsWithDefault
	foreignBlacklist = append(foreignBlacklist, assetColumnsWithCustom...)

	if err := randomize.Struct(seed, &foreign, assetDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	foreign.Release = custom_types.NullReleaseRandom()
	foreign.Duration = custom_types.NullIntervalRandom()

	localBlacklist := transcodeColumnsWithDefault
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, transcodeDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	local.Segment = custom_types.SegmentRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Asset = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.AssetByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := TranscodeSlice{&local}
	if err = local.L.LoadAsset(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Asset == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Asset = nil
	if err = local.L.LoadAsset(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Asset == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testTranscodeToOneAssetUsingOrig(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Asset
	var local Transcode

	foreignBlacklist := assetColumnsWithDefault
	foreignBlacklist = append(foreignBlacklist, assetColumnsWithCustom...)

	if err := randomize.Struct(seed, &foreign, assetDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	foreign.Release = custom_types.NullReleaseRandom()
	foreign.Duration = custom_types.NullIntervalRandom()

	localBlacklist := transcodeColumnsWithDefault
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, transcodeDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	local.Segment = custom_types.SegmentRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Orig = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.OrigByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := TranscodeSlice{&local}
	if err = local.L.LoadOrig(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Orig == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Orig = nil
	if err = local.L.LoadOrig(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Orig == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testTranscodeToOneAccountUsingOwner(t *testing.T) {
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var foreign Account
	var local Transcode

	foreignBlacklist := accountColumnsWithDefault
	if err := randomize.Struct(seed, &foreign, accountDBTypes, true, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Account struct: %s", err)
	}
	localBlacklist := transcodeColumnsWithDefault
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &local, transcodeDBTypes, true, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	local.Segment = custom_types.SegmentRandom()

	if err := foreign.Insert(tx); err != nil {
		t.Fatal(err)
	}

	local.Owner = foreign.ID
	if err := local.Insert(tx); err != nil {
		t.Fatal(err)
	}

	check, err := local.OwnerByFk(tx).One()
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	slice := TranscodeSlice{&local}
	if err = local.L.LoadOwner(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if local.R.Owner == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.Owner = nil
	if err = local.L.LoadOwner(tx, true, &local); err != nil {
		t.Fatal(err)
	}
	if local.R.Owner == nil {
		t.Error("struct should have been eager loaded")
	}
}

func testTranscodeToOneSetOpAssetUsingAsset(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Transcode
	var b, c Asset

	foreignBlacklist := strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)
	foreignBlacklist = append(foreignBlacklist, assetColumnsWithCustom...)

	if err := randomize.Struct(seed, &b, assetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, assetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	b.Release = custom_types.NullReleaseRandom()
	c.Release = custom_types.NullReleaseRandom()
	b.Duration = custom_types.NullIntervalRandom()
	c.Duration = custom_types.NullIntervalRandom()

	localBlacklist := strmangle.SetComplement(transcodePrimaryKeyColumns, transcodeColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, transcodeDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	a.Segment = custom_types.SegmentRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Asset{&b, &c} {
		err = a.SetAsset(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Asset != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.Transcode != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Asset != x.ID {
			t.Error("foreign key was wrong value", a.Asset)
		}

		if exists, err := TranscodeExists(tx, a.Asset); err != nil {
			t.Fatal(err)
		} else if !exists {
			t.Error("want 'a' to exist")
		}

	}
}
func testTranscodeToOneSetOpAssetUsingOrig(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Transcode
	var b, c Asset

	foreignBlacklist := strmangle.SetComplement(assetPrimaryKeyColumns, assetColumnsWithoutDefault)
	foreignBlacklist = append(foreignBlacklist, assetColumnsWithCustom...)

	if err := randomize.Struct(seed, &b, assetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, assetDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Asset struct: %s", err)
	}
	b.Release = custom_types.NullReleaseRandom()
	c.Release = custom_types.NullReleaseRandom()
	b.Duration = custom_types.NullIntervalRandom()
	c.Duration = custom_types.NullIntervalRandom()

	localBlacklist := strmangle.SetComplement(transcodePrimaryKeyColumns, transcodeColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, transcodeDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	a.Segment = custom_types.SegmentRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Asset{&b, &c} {
		err = a.SetOrig(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Orig != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.OrigTranscodes[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Orig != x.ID {
			t.Error("foreign key was wrong value", a.Orig)
		}

		zero := reflect.Zero(reflect.TypeOf(a.Orig))
		reflect.Indirect(reflect.ValueOf(&a.Orig)).Set(zero)

		if err = a.Reload(tx); err != nil {
			t.Fatal("failed to reload", err)
		}

		if a.Orig != x.ID {
			t.Error("foreign key was wrong value", a.Orig, x.ID)
		}
	}
}
func testTranscodeToOneSetOpAccountUsingOwner(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Transcode
	var b, c Account

	foreignBlacklist := strmangle.SetComplement(accountPrimaryKeyColumns, accountColumnsWithoutDefault)
	if err := randomize.Struct(seed, &b, accountDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Account struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, accountDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Account struct: %s", err)
	}
	localBlacklist := strmangle.SetComplement(transcodePrimaryKeyColumns, transcodeColumnsWithoutDefault)
	localBlacklist = append(localBlacklist, transcodeColumnsWithCustom...)

	if err := randomize.Struct(seed, &a, transcodeDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}
	a.Segment = custom_types.SegmentRandom()

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*Account{&b, &c} {
		err = a.SetOwner(tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.Owner != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.OwnerTranscodes[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.Owner != x.ID {
			t.Error("foreign key was wrong value", a.Owner)
		}

		zero := reflect.Zero(reflect.TypeOf(a.Owner))
		reflect.Indirect(reflect.ValueOf(&a.Owner)).Set(zero)

		if err = a.Reload(tx); err != nil {
			t.Fatal("failed to reload", err)
		}

		if a.Owner != x.ID {
			t.Error("foreign key was wrong value", a.Owner, x.ID)
		}
	}
}

func testTranscodesReload(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = transcode.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testTranscodesReloadAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TranscodeSlice{transcode}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}

func testTranscodesSelect(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Transcodes(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	transcodeDBTypes = map[string]string{`Asset`: `integer`, `Log`: `text`, `Options`: `ARRAYtext`, `Orig`: `integer`, `Owner`: `integer`, `Process`: `integer`, `Segment`: `USER-DEFINED`, `Start`: `timestamp with time zone`}
	_                = bytes.MinRead
)

func testTranscodesUpdate(t *testing.T) {
	t.Parallel()

	if len(transcodeColumns) == len(transcodePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := transcodeColumnsWithDefault
	blacklist = append(blacklist, transcodeColumnsWithCustom...)

	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	if err = transcode.Update(tx); err != nil {
		t.Error(err)
	}
}

func testTranscodesSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(transcodeColumns) == len(transcodePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := transcodePrimaryKeyColumns
	blacklist = append(blacklist, transcodeColumnsWithCustom...)

	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(transcodeColumns, transcodePrimaryKeyColumns) {
		fields = transcodeColumns
	} else {
		fields = strmangle.SetComplement(
			transcodeColumns,
			transcodePrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(transcode))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := TranscodeSlice{transcode}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}

func testTranscodesUpsert(t *testing.T) {
	t.Parallel()

	if len(transcodeColumns) == len(transcodePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	transcode := &Transcode{}
	if err = randomize.Struct(seed, transcode, transcodeDBTypes, true, transcodeColumnsWithCustom...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = transcode.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert Transcode: %s", err)
	}

	count, err := Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	blacklist := transcodePrimaryKeyColumns

	blacklist = append(blacklist, transcodeColumnsWithCustom...)

	if err = randomize.Struct(seed, transcode, transcodeDBTypes, false, blacklist...); err != nil {
		t.Errorf("Unable to randomize Transcode struct: %s", err)
	}

	transcode.Segment = custom_types.SegmentRandom()

	if err = transcode.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert Transcode: %s", err)
	}

	count, err = Transcodes(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
