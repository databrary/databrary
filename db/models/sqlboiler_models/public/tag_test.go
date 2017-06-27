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

func testTags(t *testing.T) {
	t.Parallel()

	query := Tags(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testTagsLive(t *testing.T) {
	all, err := Tags(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all Tags err: ", err)
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
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM tag) TO STDOUT" -d `, dbMain.DbName)
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
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM tag) TO STDOUT" -d `, dbMain.LiveTestDBName)
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
		t.Fatalf("TagsLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

func testTagsDelete(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = tag.Delete(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTagsQueryDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Tags(tx).DeleteAll(); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTagsSliceDeleteAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TagSlice{tag}

	if err = slice.DeleteAll(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testTagsExists(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	e, err := TagExists(tx, tag.ID)
	if err != nil {
		t.Errorf("Unable to check if Tag exists: %s", err)
	}
	if !e {
		t.Errorf("Expected TagExistsG to return true, but got false.")
	}
}

func testTagsFind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	tagFound, err := FindTag(tx, tag.ID)
	if err != nil {
		t.Error(err)
	}

	if tagFound == nil {
		t.Error("want a record, got nil")
	}
}

func testTagsBind(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = Tags(tx).Bind(tag); err != nil {
		t.Error(err)
	}
}

func testTagsOne(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	if x, err := Tags(tx).One(); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testTagsAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tagOne := &Tag{}
	tagTwo := &Tag{}
	if err = randomize.Struct(seed, tagOne, tagDBTypes, false, tagColumnsWithDefault...); err != nil {

		t.Errorf("Unable to randomize Tag struct: %s", err)
	}
	if err = randomize.Struct(seed, tagTwo, tagDBTypes, false, tagColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tagOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = tagTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Tags(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testTagsCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tagOne := &Tag{}
	tagTwo := &Tag{}
	if err = randomize.Struct(seed, tagOne, tagDBTypes, false, tagColumnsWithDefault...); err != nil {

		t.Errorf("Unable to randomize Tag struct: %s", err)
	}
	if err = randomize.Struct(seed, tagTwo, tagDBTypes, false, tagColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tagOne.Insert(tx); err != nil {
		t.Error(err)
	}
	if err = tagTwo.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func tagBeforeInsertHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagAfterInsertHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagAfterSelectHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagBeforeUpdateHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagAfterUpdateHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagBeforeDeleteHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagAfterDeleteHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagBeforeUpsertHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func tagAfterUpsertHook(e boil.Executor, o *Tag) error {
	*o = Tag{}
	return nil
}

func testTagsHooks(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	empty := &Tag{}

	AddTagHook(boil.BeforeInsertHook, tagBeforeInsertHook)
	if err = tag.doBeforeInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", tag)
	}
	tagBeforeInsertHooks = []TagHook{}

	AddTagHook(boil.AfterInsertHook, tagAfterInsertHook)
	if err = tag.doAfterInsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", tag)
	}
	tagAfterInsertHooks = []TagHook{}

	AddTagHook(boil.AfterSelectHook, tagAfterSelectHook)
	if err = tag.doAfterSelectHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", tag)
	}
	tagAfterSelectHooks = []TagHook{}

	AddTagHook(boil.BeforeUpdateHook, tagBeforeUpdateHook)
	if err = tag.doBeforeUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", tag)
	}
	tagBeforeUpdateHooks = []TagHook{}

	AddTagHook(boil.AfterUpdateHook, tagAfterUpdateHook)
	if err = tag.doAfterUpdateHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", tag)
	}
	tagAfterUpdateHooks = []TagHook{}

	AddTagHook(boil.BeforeDeleteHook, tagBeforeDeleteHook)
	if err = tag.doBeforeDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", tag)
	}
	tagBeforeDeleteHooks = []TagHook{}

	AddTagHook(boil.AfterDeleteHook, tagAfterDeleteHook)
	if err = tag.doAfterDeleteHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", tag)
	}
	tagAfterDeleteHooks = []TagHook{}

	AddTagHook(boil.BeforeUpsertHook, tagBeforeUpsertHook)
	if err = tag.doBeforeUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", tag)
	}
	tagBeforeUpsertHooks = []TagHook{}

	AddTagHook(boil.AfterUpsertHook, tagAfterUpsertHook)
	if err = tag.doAfterUpsertHooks(nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(tag, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", tag)
	}
	tagAfterUpsertHooks = []TagHook{}
}
func testTagsInsert(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTagsInsertWhitelist(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx, tagColumns...); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testTagToManyTagUses(t *testing.T) {
	var err error
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Tag
	var b, c TagUse

	foreignBlacklist := tagUseColumnsWithDefault
	foreignBlacklist = append(foreignBlacklist, tagUseColumnsWithCustom...)

	if err := randomize.Struct(seed, &b, tagUseDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize TagUse struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, tagUseDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize TagUse struct: %s", err)
	}
	b.Segment = custom_types.SegmentRandom()
	c.Segment = custom_types.SegmentRandom()

	localBlacklist := tagColumnsWithDefault
	if err := randomize.Struct(seed, &a, tagDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	b.Tag = a.ID
	c.Tag = a.ID
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	tagUse, err := a.TagUsesByFk(tx).All()
	if err != nil {
		t.Fatal(err)
	}

	bFound, cFound := false, false
	for _, v := range tagUse {
		if v.Tag == b.Tag {
			bFound = true
		}
		if v.Tag == c.Tag {
			cFound = true
		}
	}

	if !bFound {
		t.Error("expected to find b")
	}
	if !cFound {
		t.Error("expected to find c")
	}

	slice := TagSlice{&a}
	if err = a.L.LoadTagUses(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.TagUses); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	a.R.TagUses = nil
	if err = a.L.LoadTagUses(tx, true, &a); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.TagUses); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	if t.Failed() {
		t.Logf("%#v", tagUse)
	}
}

func testTagToManyNotifications(t *testing.T) {
	var err error
	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	seed := randomize.NewSeed()

	var a Tag
	var b, c Notification

	foreignBlacklist := notificationColumnsWithDefault
	foreignBlacklist = append(foreignBlacklist, notificationColumnsWithCustom...)

	if err := randomize.Struct(seed, &b, notificationDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Notification struct: %s", err)
	}
	if err := randomize.Struct(seed, &c, notificationDBTypes, false, foreignBlacklist...); err != nil {
		t.Errorf("Unable to randomize Notification struct: %s", err)
	}
	b.Delivered = custom_types.NoticeDeliveryRandom()
	c.Delivered = custom_types.NoticeDeliveryRandom()
	b.Permission = custom_types.NullPermissionRandom()
	c.Permission = custom_types.NullPermissionRandom()
	b.Segment = custom_types.NullSegmentRandom()
	c.Segment = custom_types.NullSegmentRandom()
	b.Release = custom_types.NullReleaseRandom()
	c.Release = custom_types.NullReleaseRandom()

	localBlacklist := tagColumnsWithDefault
	if err := randomize.Struct(seed, &a, tagDBTypes, false, localBlacklist...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	b.Tag.Valid = true
	c.Tag.Valid = true
	b.Tag.Int = a.ID
	c.Tag.Int = a.ID
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	notification, err := a.NotificationsByFk(tx).All()
	if err != nil {
		t.Fatal(err)
	}

	bFound, cFound := false, false
	for _, v := range notification {
		if v.Tag.Int == b.Tag.Int {
			bFound = true
		}
		if v.Tag.Int == c.Tag.Int {
			cFound = true
		}
	}

	if !bFound {
		t.Error("expected to find b")
	}
	if !cFound {
		t.Error("expected to find c")
	}

	slice := TagSlice{&a}
	if err = a.L.LoadNotifications(tx, false, &slice); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.Notifications); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	a.R.Notifications = nil
	if err = a.L.LoadNotifications(tx, true, &a); err != nil {
		t.Fatal(err)
	}
	if got := len(a.R.Notifications); got != 2 {
		t.Error("number of eager loaded records wrong, got:", got)
	}

	if t.Failed() {
		t.Logf("%#v", notification)
	}
}

func testTagToManyAddOpTagUses(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a Tag
	var b, c, d, e TagUse

	seed := randomize.NewSeed()
	localComplelementList := strmangle.SetComplement(tagPrimaryKeyColumns, tagColumnsWithoutDefault)
	if err = randomize.Struct(seed, &a, tagDBTypes, false, localComplelementList...); err != nil {
		t.Fatal(err)
	}

	foreignComplementList := strmangle.SetComplement(tagUsePrimaryKeyColumns, tagUseColumnsWithoutDefault)
	foreignComplementList = append(foreignComplementList, tagUseColumnsWithCustom...)

	foreigners := []*TagUse{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, tagUseDBTypes, false, foreignComplementList...); err != nil {
			t.Fatal(err)
		}
		x.Segment = custom_types.SegmentRandom()

	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	foreignersSplitByInsertion := [][]*TagUse{
		{&b, &c},
		{&d, &e},
	}

	for i, x := range foreignersSplitByInsertion {
		err = a.AddTagUses(tx, i != 0, x...)
		if err != nil {
			t.Fatal(err)
		}

		first := x[0]
		second := x[1]

		if a.ID != first.Tag {
			t.Error("foreign key was wrong value", a.ID, first.Tag)
		}
		if a.ID != second.Tag {
			t.Error("foreign key was wrong value", a.ID, second.Tag)
		}

		if first.R.Tag != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}
		if second.R.Tag != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}

		if a.R.TagUses[i*2] != first {
			t.Error("relationship struct slice not set to correct value")
		}
		if a.R.TagUses[i*2+1] != second {
			t.Error("relationship struct slice not set to correct value")
		}

		count, err := a.TagUsesByFk(tx).Count()
		if err != nil {
			t.Fatal(err)
		}
		if want := int64((i + 1) * 2); count != want {
			t.Error("want", want, "got", count)
		}
	}
}
func testTagToManyAddOpNotifications(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a Tag
	var b, c, d, e Notification

	seed := randomize.NewSeed()
	localComplelementList := strmangle.SetComplement(tagPrimaryKeyColumns, tagColumnsWithoutDefault)
	if err = randomize.Struct(seed, &a, tagDBTypes, false, localComplelementList...); err != nil {
		t.Fatal(err)
	}

	foreignComplementList := strmangle.SetComplement(notificationPrimaryKeyColumns, notificationColumnsWithoutDefault)
	foreignComplementList = append(foreignComplementList, notificationColumnsWithCustom...)

	foreigners := []*Notification{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, notificationDBTypes, false, foreignComplementList...); err != nil {
			t.Fatal(err)
		}
		x.Delivered = custom_types.NoticeDeliveryRandom()
		x.Permission = custom_types.NullPermissionRandom()
		x.Segment = custom_types.NullSegmentRandom()
		x.Release = custom_types.NullReleaseRandom()

	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	foreignersSplitByInsertion := [][]*Notification{
		{&b, &c},
		{&d, &e},
	}

	for i, x := range foreignersSplitByInsertion {
		err = a.AddNotifications(tx, i != 0, x...)
		if err != nil {
			t.Fatal(err)
		}

		first := x[0]
		second := x[1]

		if a.ID != first.Tag.Int {
			t.Error("foreign key was wrong value", a.ID, first.Tag.Int)
		}
		if a.ID != second.Tag.Int {
			t.Error("foreign key was wrong value", a.ID, second.Tag.Int)
		}

		if first.R.Tag != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}
		if second.R.Tag != &a {
			t.Error("relationship was not added properly to the foreign slice")
		}

		if a.R.Notifications[i*2] != first {
			t.Error("relationship struct slice not set to correct value")
		}
		if a.R.Notifications[i*2+1] != second {
			t.Error("relationship struct slice not set to correct value")
		}

		count, err := a.NotificationsByFk(tx).Count()
		if err != nil {
			t.Fatal(err)
		}
		if want := int64((i + 1) * 2); count != want {
			t.Error("want", want, "got", count)
		}
	}
}

func testTagToManySetOpNotifications(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a Tag
	var b, c, d, e Notification

	seed := randomize.NewSeed()
	localComplelementList := strmangle.SetComplement(tagPrimaryKeyColumns, tagColumnsWithoutDefault)
	if err = randomize.Struct(seed, &a, tagDBTypes, false, localComplelementList...); err != nil {
		t.Fatal(err)
	}

	foreignComplementList := strmangle.SetComplement(notificationPrimaryKeyColumns, notificationColumnsWithoutDefault)
	foreignComplementList = append(foreignComplementList, notificationColumnsWithCustom...)

	foreigners := []*Notification{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, notificationDBTypes, false, foreignComplementList...); err != nil {
			t.Fatal(err)
		}
		x.Delivered = custom_types.NoticeDeliveryRandom()
		x.Permission = custom_types.NullPermissionRandom()
		x.Segment = custom_types.NullSegmentRandom()
		x.Release = custom_types.NullReleaseRandom()

	}

	if err = a.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(tx); err != nil {
		t.Fatal(err)
	}
	if err = c.Insert(tx); err != nil {
		t.Fatal(err)
	}

	err = a.SetNotifications(tx, false, &b, &c)
	if err != nil {
		t.Fatal(err)
	}

	count, err := a.NotificationsByFk(tx).Count()
	if err != nil {
		t.Fatal(err)
	}
	if count != 2 {
		t.Error("count was wrong:", count)
	}

	err = a.SetNotifications(tx, true, &d, &e)
	if err != nil {
		t.Fatal(err)
	}

	count, err = a.NotificationsByFk(tx).Count()
	if err != nil {
		t.Fatal(err)
	}
	if count != 2 {
		t.Error("count was wrong:", count)
	}

	if b.Tag.Valid {
		t.Error("want b's foreign key value to be nil")
	}
	if c.Tag.Valid {
		t.Error("want c's foreign key value to be nil")
	}
	if a.ID != d.Tag.Int {
		t.Error("foreign key was wrong value", a.ID, d.Tag.Int)
	}
	if a.ID != e.Tag.Int {
		t.Error("foreign key was wrong value", a.ID, e.Tag.Int)
	}

	if b.R.Tag != nil {
		t.Error("relationship was not removed properly from the foreign struct")
	}
	if c.R.Tag != nil {
		t.Error("relationship was not removed properly from the foreign struct")
	}
	if d.R.Tag != &a {
		t.Error("relationship was not added properly to the foreign struct")
	}
	if e.R.Tag != &a {
		t.Error("relationship was not added properly to the foreign struct")
	}

	if a.R.Notifications[0] != &d {
		t.Error("relationship struct slice not set to correct value")
	}
	if a.R.Notifications[1] != &e {
		t.Error("relationship struct slice not set to correct value")
	}
}

func testTagToManyRemoveOpNotifications(t *testing.T) {
	var err error

	tx := MustTx(boil.Begin())
	defer tx.Rollback()

	var a Tag
	var b, c, d, e Notification

	seed := randomize.NewSeed()
	localComplelementList := strmangle.SetComplement(tagPrimaryKeyColumns, tagColumnsWithoutDefault)
	if err = randomize.Struct(seed, &a, tagDBTypes, false, localComplelementList...); err != nil {
		t.Fatal(err)
	}

	foreignComplementList := strmangle.SetComplement(notificationPrimaryKeyColumns, notificationColumnsWithoutDefault)
	foreignComplementList = append(foreignComplementList, notificationColumnsWithCustom...)

	foreigners := []*Notification{&b, &c, &d, &e}
	for _, x := range foreigners {
		if err = randomize.Struct(seed, x, notificationDBTypes, false, foreignComplementList...); err != nil {
			t.Fatal(err)
		}
		x.Delivered = custom_types.NoticeDeliveryRandom()
		x.Permission = custom_types.NullPermissionRandom()
		x.Segment = custom_types.NullSegmentRandom()
		x.Release = custom_types.NullReleaseRandom()

	}

	if err := a.Insert(tx); err != nil {
		t.Fatal(err)
	}

	err = a.AddNotifications(tx, true, foreigners...)
	if err != nil {
		t.Fatal(err)
	}

	count, err := a.NotificationsByFk(tx).Count()
	if err != nil {
		t.Fatal(err)
	}
	if count != 4 {
		t.Error("count was wrong:", count)
	}

	err = a.RemoveNotifications(tx, foreigners[:2]...)
	if err != nil {
		t.Fatal(err)
	}

	count, err = a.NotificationsByFk(tx).Count()
	if err != nil {
		t.Fatal(err)
	}
	if count != 2 {
		t.Error("count was wrong:", count)
	}

	if b.Tag.Valid {
		t.Error("want b's foreign key value to be nil")
	}
	if c.Tag.Valid {
		t.Error("want c's foreign key value to be nil")
	}

	if b.R.Tag != nil {
		t.Error("relationship was not removed properly from the foreign struct")
	}
	if c.R.Tag != nil {
		t.Error("relationship was not removed properly from the foreign struct")
	}
	if d.R.Tag != &a {
		t.Error("relationship to a should have been preserved")
	}
	if e.R.Tag != &a {
		t.Error("relationship to a should have been preserved")
	}

	if len(a.R.Notifications) != 2 {
		t.Error("should have preserved two relationships")
	}

	// Removal doesn't do a stable deletion for performance so we have to flip the order
	if a.R.Notifications[1] != &d {
		t.Error("relationship to d should have been preserved")
	}
	if a.R.Notifications[0] != &e {
		t.Error("relationship to e should have been preserved")
	}
}

func testTagsReload(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	if err = tag.Reload(tx); err != nil {
		t.Error(err)
	}
}

func testTagsReloadAll(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	slice := TagSlice{tag}

	if err = slice.ReloadAll(tx); err != nil {
		t.Error(err)
	}
}

func testTagsSelect(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	slice, err := Tags(tx).All()
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	tagDBTypes = map[string]string{`ID`: `integer`, `Name`: `character varying`}
	_          = bytes.MinRead
)

func testTagsUpdate(t *testing.T) {
	t.Parallel()

	if len(tagColumns) == len(tagPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := tagColumnsWithDefault

	if err = randomize.Struct(seed, tag, tagDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	if err = tag.Update(tx); err != nil {
		t.Error(err)
	}
}

func testTagsSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(tagColumns) == len(tagPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Insert(tx); err != nil {
		t.Error(err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	blacklist := tagPrimaryKeyColumns

	if err = randomize.Struct(seed, tag, tagDBTypes, true, blacklist...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(tagColumns, tagPrimaryKeyColumns) {
		fields = tagColumns
	} else {
		fields = strmangle.SetComplement(
			tagColumns,
			tagPrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(tag))
	updateMap := M{}
	for _, col := range fields {
		updateMap[col] = value.FieldByName(strmangle.TitleCase(col)).Interface()
	}

	slice := TagSlice{tag}
	if err = slice.UpdateAll(tx, updateMap); err != nil {
		t.Error(err)
	}
}

func testTagsUpsert(t *testing.T) {
	t.Parallel()

	if len(tagColumns) == len(tagPrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	var err error
	seed := randomize.NewSeed()
	tag := &Tag{}
	if err = randomize.Struct(seed, tag, tagDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	tx := MustTx(boil.Begin())
	defer tx.Rollback()
	if err = tag.Upsert(tx, false, nil, nil); err != nil {
		t.Errorf("Unable to upsert Tag: %s", err)
	}

	count, err := Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	blacklist := tagPrimaryKeyColumns

	if err = randomize.Struct(seed, tag, tagDBTypes, false, blacklist...); err != nil {
		t.Errorf("Unable to randomize Tag struct: %s", err)
	}

	if err = tag.Upsert(tx, true, nil, nil); err != nil {
		t.Errorf("Unable to upsert Tag: %s", err)
	}

	count, err = Tags(tx).Count()
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}
