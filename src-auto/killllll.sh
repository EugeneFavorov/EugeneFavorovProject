#!/bin/sh
uuu=`whoami`
#ps -eo pid,user,command | grep $uuu | grep "\-b \-p kd8.p \-param" | grep -v grep | wc -l
kill -9 `ps -eo pid,user,command | grep -i $uuu | grep "\-b \-p kd8.p \-param" | grep -i $1 | grep -v grep  | awk '{print $1}'`



