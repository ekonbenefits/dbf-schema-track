#!/bin/sh
</your/path/>dbf-schema-track.exe <DBFNAME1> <DBFNAME2>
RESULT=$?
[ $RESULT -ne 0 ] && exit 1
exit 0