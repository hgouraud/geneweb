#!/bin/sh

# This script assumes that the distribution folder,
# (or a link to it) is installed at the Document root
# of the Web server (Apache or other).
# Please update the search for DocumentRoot according to
# your understanding of other environments.

# The purpose of the script is to install
# - gwd.cgi and test.cgi in the cgi-bin folder
# - a test base in the bases folder

OS_ENV=`uname` 

# Find the document root under Apache

DISTRIB_NAME="distribution"
BASES=$WEB_ROOT/$DISTRIB_NAME/bases
echo "Bases: $BASES"
BIN_DIR=$WEB_ROOT/$DISTRIB_NAME/gw

# Copy test base elements
rm -f -R $BASES/test.*
cp test.gwf $BASES
if ! [ -d $BASES/src ]; then
  mkdir $BASES/src
fi
if ! [ -d $BASES/src/test ]; then
  mkdir $BASES/src/test
fi
if ! [ -d $BASES/src/test/images ]; then
  mkdir $BASES/src/test/images
fi

if ! [ -d $BASES/images ]; then
  mkdir $BASES/images
fi
if ! [ -d $BASES/images/test ]; then
  mkdir $BASES/images/test
fi

cd ./install-cgi

cp Lenna.jpg $BASES/src/test/images/aatest.jpg
cp Lenna.jpg $BASES/images/test/tiny.0.mouse.jpg
cp Lenna-full.jpg $BASES/src/test/images/
cp Lenna-icon.jpg $BASES/src/test/images/

$BIN_DIR/gwc -f -o $BASES/test test.gw
rm -f test.cgo

open "http://localhost:2317/test?"

