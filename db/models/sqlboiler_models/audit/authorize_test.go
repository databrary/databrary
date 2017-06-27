// This file is generated by SQLBoiler (https://github.com/databrary/sqlboiler)
// and is meant to be re-generated in place and/or deleted at any time.
// EDIT AT YOUR OWN RISK

package audit

import (
	"bytes"
	"os"
	"os/exec"
	"sort"
	"strings"
	"testing"

	"github.com/pmezard/go-difflib/difflib"
)

func testAuthorizes(t *testing.T) {
	t.Parallel()

	query := Authorizes(nil)

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testAuthorizesLive(t *testing.T) {
	all, err := Authorizes(dbMain.liveDbConn).All()
	if err != nil {
		t.Fatalf("failed to get all Authorizes err: ", err)
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
	dumpCmd := exec.Command("psql", `-c "COPY (SELECT * FROM authorize) TO STDOUT" -d `, dbMain.DbName)
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
	dumpCmd = exec.Command("psql", `-c "COPY (SELECT * FROM authorize) TO STDOUT" -d `, dbMain.LiveTestDBName)
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
		t.Fatalf("AuthorizesLive failed but it's probably trivial: %s", strings.Replace(result, "\t", " ", -1))
	}

}

var (
	authorizeDBTypes = map[string]string{`AuditAction`: `enum.action('attempt','open','close','add','change','remove','superuser')`, `AuditIP`: `inet`, `AuditTime`: `timestamp with time zone`, `AuditUser`: `integer`, `Child`: `integer`, `Expires`: `timestamp with time zone`, `Member`: `enum.permission('NONE','PUBLIC','SHARED','READ','EDIT','ADMIN')`, `Parent`: `integer`, `Site`: `enum.permission('NONE','PUBLIC','SHARED','READ','EDIT','ADMIN')`}
	_                = bytes.MinRead
)
