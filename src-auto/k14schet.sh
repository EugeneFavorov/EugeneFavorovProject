#!/bin/sh
uuu=`whoami`
ps -eo pid,user,command | grep $uuu | grep "\-b \-p kd8.p \-param" | grep -v grep | wc -l




