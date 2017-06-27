// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package audit

import (
	"fmt"
	"os"
	"testing"
)

// This test suite runs each operation test in parallel.
// Example, if your database has 3 tables, the suite will run:
// table1, table2 and table3 Delete in parallel
// table1, table2 and table3 Insert in parallel, and so forth.
// It does NOT run each operation group in parallel.
// Separating the tests thusly grants avoidance of Postgres deadlocks.
func TestParent(t *testing.T) {}

func TestDelete(t *testing.T) {}

func TestQueryDeleteAll(t *testing.T) {}

func TestSliceDeleteAll(t *testing.T) {}

func TestExists(t *testing.T) {}

func TestFind(t *testing.T) {}

func TestBind(t *testing.T) {}

func TestOne(t *testing.T) {}

func TestAll(t *testing.T) {}

func TestCount(t *testing.T) {}

func TestHooks(t *testing.T) {}

func TestInsert(t *testing.T) {}

// TestToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestToOne(t *testing.T) {}

// TestOneToOne tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOne(t *testing.T) {}

// TestToMany tests cannot be run in parallel
// or deadlocks can occur.
func TestToMany(t *testing.T) {}

// TestToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneSet(t *testing.T) {}

// TestToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToOneRemove(t *testing.T) {}

// TestOneToOneSet tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneSet(t *testing.T) {}

// TestOneToOneRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestOneToOneRemove(t *testing.T) {}

// TestToManyAdd tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyAdd(t *testing.T) {}

// TestToManySet tests cannot be run in parallel
// or deadlocks can occur.
func TestToManySet(t *testing.T) {}

// TestToManyRemove tests cannot be run in parallel
// or deadlocks can occur.
func TestToManyRemove(t *testing.T) {}

func TestReload(t *testing.T) {}

func TestReloadAll(t *testing.T) {}

func TestSelect(t *testing.T) {}

func TestUpdate(t *testing.T) {}

func TestSliceUpdateAll(t *testing.T) {}

func TestUpsert(t *testing.T) {}

func TestLive(t *testing.T) {
	if err := dbMain.setupLiveTest(); err != nil {
		fmt.Println("Unable to execute setupLiveTest:", err)
		os.Exit(-4)
	}
	var err error
	dbMain.liveTestDbConn, err = dbMain.conn(dbMain.LiveTestDBName)
	if err != nil {
		fmt.Println("failed to get test connection:", err)
	}
	dbMain.liveDbConn, err = dbMain.conn(dbMain.DbName)
	if err != nil {
		fmt.Println("failed to get live connection:", err)
	}
	t.Run("Containers", testContainersLive)
	t.Run("Accounts", testAccountsLive)
	t.Run("Analytics", testAnalyticsLive)
	t.Run("Assets", testAssetsLive)
	t.Run("Excerpts", testExcerptsLive)
	t.Run("Parties", testPartiesLive)
	t.Run("Measures", testMeasuresLive)
	t.Run("SlotAssets", testSlotAssetsLive)
	t.Run("Records", testRecordsLive)
	t.Run("Audits", testAuditsLive)
	t.Run("MeasureDates", testMeasureDatesLive)
	t.Run("Slots", testSlotsLive)
	t.Run("MeasureNumerics", testMeasureNumericsLive)
	t.Run("MeasureTexts", testMeasureTextsLive)
	t.Run("SlotReleases", testSlotReleasesLive)
	t.Run("SlotRecords", testSlotRecordsLive)
	t.Run("VolumeAccesses", testVolumeAccessesLive)
	t.Run("VolumeCitations", testVolumeCitationsLive)
	t.Run("VolumeInclusions", testVolumeInclusionsLive)
	t.Run("VolumeLinks", testVolumeLinksLive)
	t.Run("Volumes", testVolumesLive)
	t.Run("Authorizes", testAuthorizesLive)
	t.Run("Avatars", testAvatarsLive)

}
