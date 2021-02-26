/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-1996 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  KEY-TST.P
      Comment:  ����� ���� ��楢��� ���
         Uses:  -
      Used BY:  key-tst.i, acct-cr.p
      Created:  5/06/1996 Mike
     Modified:  28/08/96 Serge in-acct -> decimal
     Modified:  09/07/97 Mike  �᫨ 7,8 ᨬ��� ���-9 = "00", � �� ���
     Modified:  15.08.2002 kraw 0008155 ����� ��� ����� ᮤ�ঠ�� �㪢�

     ���� �����뢠���� �� 3-� ���筮�� ���� ����� � ����.
     �᫨ 3-� ���筮�� ���� ���, �� �������� �� 9-���筮��.
*/

&GLOB ZERO-ACCT "00000000000000000000"

{globals.i}             /* �������� ��६���� ��ᨨ. */

DEF INPUT  PARAM in-acct LIKE acct.acct NO-UNDO.
DEF INPUT  PARAM in-mfo  LIKE op-bank.bank-code NO-UNDO.
DEF OUTPUT PARAM key AS INT64 NO-UNDO.
/* ��⠢�� ���� ���� */

if length(trim(in-mfo)) = 3 then do:
   in-mfo = FGetSetting("����", ?, "").
end.

/*  
 in-mfo = FGetSetting("����", ?, "").
*/

/* ����� ��⠢�� ���� ���� */
{pick-val.i}

DEF VAR skey         AS CHAR  NO-UNDO.
DEF VAR strkey       AS CHAR  NO-UNDO.
DEF VAR keystr       AS CHAR  NO-UNDO.
DEF VAR i            AS INT64   NO-UNDO.
DEF VAR bank-mfo-9   AS CHAR  NO-UNDO.

ASSIGN
   bank-mfo-9  =  FGetSetting("�������","","")
                        /* � �� ०���, ��� ᮤ�ন� ���䨪�.
                        ** ��� �᪮७��, ��१���� ���䨪� ᤥ��� �������. */
   in-acct     =  TRIM (ENTRY (1, in-acct, "@"))
   keystr      =  "71371371371371371371371"
   key         =  ?
   skey        =  ""
   in-acct     =  SUBSTRING ({&ZERO-ACCT},1,20 - LENGTH(in-acct)) + in-acct
.

IF in-acct EQ {&ZERO-ACCT} THEN RETURN.

IF length(in-mfo) = 3 THEN
   skey = in-mfo.
ELSE DO:
  IF in-mfo = ? OR length(in-mfo) EQ 1 THEN DO:  /* ��� ���� ��� ���� ���祩
                                                 ᢮�� ��⮢ */
    FIND banks-code WHERE banks-code.bank-code-type = "���-9"
                      AND banks-code.bank-code = bank-mfo-9 NO-LOCK.
    FIND FIRST banks OF banks-code NO-LOCK.
  END.
  ELSE DO:
      {getbank.i banks in-mfo '���-9'}
      {getcode.i banks "���-3"}
      IF AVAIL banks-code THEN
         skey = banks-code.bank-code.
      ELSE DO:
         {getcode.i banks "���-9"}
      END.
  END.

  IF skey = "" AND AVAIL banks THEN DO:
    IF banks.flag-rkc OR
       SUBSTRING(string(INT64(banks-code.bank-code),"999999999"),7,2) = "00"
    THEN
      skey = "0" + substr(string(INT64(banks-code.bank-code),"999999999"),5,2).
    ELSE
      skey = SUBSTRING(string(INT64(banks-code.bank-code),"999999999"),7,3).
  END.
END.

strkey = substr(in-acct,6,1).
IF strkey < "0" OR strkey > "9" THEN DO:
  CASE strkey:
    WHEN "�" OR WHEN "A" THEN strkey = "0".
    WHEN "�" OR WHEN "B" THEN strkey = "1".
    WHEN "�" OR WHEN "C" THEN strkey = "2".
    WHEN "�" OR WHEN "E" THEN strkey = "3".
    WHEN "�" OR WHEN "H" THEN strkey = "4".
    WHEN "�" OR WHEN "K" THEN strkey = "5".
    WHEN "�" OR WHEN "M" THEN strkey = "6".
    WHEN "�" OR WHEN "P" THEN strkey = "7".
    WHEN "�" OR WHEN "T" THEN strkey = "8".
    WHEN "�" OR WHEN "X" THEN strkey = "9".
  END.
  substr(in-acct,6,1) = strkey.
END.
skey = skey + in-acct.
IF length(skey) <> 23 THEN RETURN.

key = 0.
DO i = 1 TO 23:
  IF i <> 12 THEN key = key + INT64(substr(skey, i, 1)) * INT64(substr(keystr, i, 1)).
END.

key = (key * 3) mod 10.
