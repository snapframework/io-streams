#!/bin/sh

set -e

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

./dist/build/testsuite/testsuite -j4 -a1000 $*

# cabal test --show-details=always --test-options="-j4 -a1000 $*"

DIR=dist/hpc

rm -Rf $DIR
mkdir -p $DIR

EXCLUDES='Main
System.IO.Streams.Tests.Attoparsec
System.IO.Streams.Tests.Builder
System.IO.Streams.Tests.ByteString
System.IO.Streams.Tests.Combinators
System.IO.Streams.Tests.Common
System.IO.Streams.Tests.File
System.IO.Streams.Tests.Handle
System.IO.Streams.Tests.Internal
System.IO.Streams.Tests.List
System.IO.Streams.Tests.Text
System.IO.Streams.Tests.Vector
System.IO.Streams.Tests.Zlib
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir=$DIR testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $DIR.
EOF
