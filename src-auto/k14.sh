#!/bin/bash
#
#  1 - ��砫�� ��⮪
#  2 - ������  ��⮪
#  3 - ������⢮ ��⮪��
#  4 - 䨫���
#  5 - ᯨ᮪ �࠭���権 �१ ;
#  6 - �
#  7 - ���थ��
#echo $1
for i in $(seq $1 $2); do
/home2/bis/quit41d/src-auto/bq41d  -b -p kd8.p -param "$1,$2,$i,$3,$4,$5,$6,$7" &
#echo $4 $i $3 $5
done
