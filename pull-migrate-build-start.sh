#!/bin/bash
set -e
build_user="centos"
clone_path="/home/$build_user/src/databrary"
exe_dir="/home/$build_user/.cabal/bin"
data_basedir="/home/$build_user/.cabal/share/x86_64-linux-ghc-7.10.3"

cd $clone_path
echo "=== Check branch matches required branch in ../databrary-branch.txt"
echo "Reading branch from ../databrary-branch.txt"
branch=`cat ../databrary-branch.txt | tr -d '[:space:]'` # trims any whitespace char
current_branch=`git rev-parse --abbrev-ref HEAD`
echo "Current branch is $current_branch."
if [ "$branch" != "$current_branch" ]
then
  echo "Expected branch to be $branch. Please correct."
  exit 1
fi

echo "=== Current working copy status"
git status

echo "=== Stash, pull latest from $branch"
git stash save # need in case there were manual, conflicting changes to prevent pulling from succeeding
git pull

echo "=== Run new db migrations, build, install"
# ./dev
builtexe=`ls -t $exe_dir/databrary-* | head -1` #extract exact version from git describe instead

echo "=== Starting $builtexe"
ls databrary.conf
databrary_datadir="$data_basedir/databrary-1" `$builtexe`
