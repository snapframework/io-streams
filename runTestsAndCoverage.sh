#!/bin/sh

set -e

NEWSTYLE=$(([ -d ./dist-newstyle ] && echo "1") || echo "0")
DIST=$(if [ 1 == $NEWSTYLE ]; then
           echo ./dist-newstyle/build/io-streams*;
       else
           echo ./dist
       fi)

echo HAVE DIST $DIST
echo HAVE NEWSTYLE $NEWSTYLE

TESTSUITE="./${DIST}/build/testsuite/testsuite"

[ -f "${TESTSUITE}" ] || (
    echo "No testsuite executable at $TESTSUITE."; exit 1
)

export LC_ALL=C
export LANG=C

rm -f testsuite.tix

$TESTSUITE -j4 -a1000 $*

# cabal test --show-details=always --test-options="-j4 -a1000 $*"

HPCDIR="${DIST}/hpc"

rm -Rf $HPCDIR
mkdir -p $HPCDIR

EXCLUDES='Main
System.IO.Streams.Tests.Attoparsec
System.IO.Streams.Tests.Builder
System.IO.Streams.Tests.ByteString
System.IO.Streams.Tests.Combinators
System.IO.Streams.Tests.Concurrent
System.IO.Streams.Tests.Common
System.IO.Streams.Tests.Debug
System.IO.Streams.Tests.File
System.IO.Streams.Tests.Handle
System.IO.Streams.Tests.Internal
System.IO.Streams.Tests.List
System.IO.Streams.Tests.Network
System.IO.Streams.Tests.Process
System.IO.Streams.Tests.Text
System.IO.Streams.Tests.Vector
System.IO.Streams.Tests.Zlib
'

EXCL=""

for m in $EXCLUDES; do
    EXCL="$EXCL --exclude=$m"
done

hpc markup $EXCL --destdir="$HPCDIR" testsuite >/dev/null 2>&1

rm -f testsuite.tix

cat <<EOF

Test coverage report written to $HPCDIR.
EOF
