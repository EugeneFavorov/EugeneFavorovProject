/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-1996 ��� "������᪨� ���ଠ樮���� ��⥬�"
     Filename:  shwacct3.p
      Comment:  ��ᬮ�� � ����� ��������ᮢ�� ��⮢, ࠡ�⠮�� � 㪠�����
                蠡����� ���. �室�騩 ��ࠬ��� - ��� 蠡���� ���. �ᯮ�� -
                ����� � ��楤�� ᯨᠭ�� � ����⥪� 2. ������砥��� �१
                �������⥫�� ४����� 蠡���� �஢���� � ����� "��抠�".
                ��� ����࠮��� �� ᫥��饬� �������:
                  ����࠮��� �� ��������ᮢ�� ���, ࠡ�⠮騥 �� 㪠�������
                蠡���� ���, �� ����� �⢥砥� ����� ���㤭�� ���
                ���稭����� ��� ���㤭���.
         Uses:  -
      Used by:  gcrddec.p
      Created:  07/12/1999 Kostik
     Modified:  10/10/2002 kraw (0008664) - ��ॢ����� �� �������᪨� 䨫����
*/

{globals.i}
{flt-file.i NEW}
{sh-defs.i}
{chkacces.i}
def input parameter in-code-value like code.code no-undo.

def var vacct-cat   like acct.acct-cat no-undo.
def var long-acct   as char format "x(24)" no-undo.
def var comment_str as char label "                                   "  no-undo.
def var users       as char no-undo.
def var lstcont     as char initial "�����" no-undo.

DEF VAR in-acct-cat AS   CHARACTER           NO-UNDO.
DEF VAR out-str     AS   CHARACTER           NO-UNDO.
DEF VAR name        AS   CHARACTER EXTENT 2  NO-UNDO.

DEF BUFFER bacct FOR acct.
DEF VAR vdebug AS log NO-UNDO.

def temp-table tmpacct no-undo
 field acct      like acct.acct
 field currency  like acct.curr
 field cust-cat  like acct.cust-cat
 field cust-id   like acct.cust-id
 field rec-oa    as recid /* ��� �᫫�� �� ��������ᮢ�� ��� */
 field cust-name as character format "x(40)" label "������������"
 field obal      like acct-pos.balance
 field fobal  as log
 field rec-ba as recid /* ��� �᫫�� �� �����ᮢ�� ��� */
 field bbal   like acct-pos.balance
 field fbbal  as log

 index acct          is primary acct currency
 index wacct         fobal fbbal acct currency
 index cust-cat      cust-cat
                     cust-id
                     fobal
                     fbbal
                     acct
                     currency
 index obal          obal
 index cust-cat-obal cust-cat
                     cust-id
                     obal
.

DEF BUFFER flt-acct FOR acct.

FIND code WHERE code.class EQ "�������"
            AND code.code  EQ in-code-value
                           NO-LOCK NO-ERROR.
IF NOT AVAIL code THEN RETURN.
{get-fmt.i &obj='" + code.misc[8] + ""-Acct-Fmt"" + "'}

ASSIGN
  vacct-cat = code.misc[8]
  users = USERID("bisquit") + "," + getslaves()
  .

DEF VAR mAcctFltN  AS INT64 INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHAR         NO-UNDO.

DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "�� ��⠬ ��"
        VIEW-AS COMBO-BOX INNER-LINES 3
        LIST-ITEMS "�� ��⠬ ��","�� ��⠬ �⤥����� ��", "�� �ᥬ ��⠬" 
        DROP-DOWN-LIST
        NO-UNDO.



FORM
   vAcctFlt   LABEL  "������ �� ��⠬ " 
              FORMAT "x(23)"
              HELP   "������ �� ��⠬ ����⥪� �����஢����� ��⮢"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE "���⠭���� �� ����⥪�".

ON GO OF FRAME frAcct DO:

   CASE vAcctFlt:SCREEN-VALUE IN FRAME frAcct:
      WHEN "�� ��⠬ ��" THEN
      DO:
         mAcctFltN = 1.
      END.
      WHEN "�� ��⠬ �⤥����� ��" THEN
      DO:
         mUsrBranch = GetUserBranchID(userid("bisquit")).
         mAcctFltN = 2.
      END.
      WHEN "�� �ᥬ ��⠬" THEN
      DO:
         mAcctFltN = 3.
      END.
   END CASE.
END.

UPDATE vAcctFlt
WITH FRAME frAcct.

/* ���������� �६����� ⠡��� */
FOR EACH bal-acct WHERE bal-acct.kau-id EQ code.code NO-LOCK,
    EACH acct WHERE (acct.kau-id EQ ? OR acct.kau-id EQ "")
                AND acct.acct-cat EQ code.misc[8]
                AND acct.bal-acct EQ bal-acct.bal-acct
/*              AND CAN-DO(users,acct.user-id)                   */
                AND (   (acct.user-id   EQ USERID('bisquit')  AND
                         mAcctFltN      EQ 1)                            
                     OR (acct.branch-id EQ mUsrBranch         AND          
                         mAcctFltN      EQ 2)                          
                     OR (mAcctFltN      EQ 3)                           
                    )                                                
              AND acct.filial-id EQ shfilial NO-LOCK:

   RUN CreateTmpAcct.

END.
FOR EACH acct WHERE acct.kau-id   EQ CODE.CODE
                AND acct.acct-cat EQ code.misc[8]
/*              AND CAN-DO(users,acct.user-id)     */
                AND (   (acct.user-id   EQ USERID('bisquit')  AND
                         mAcctFltN      EQ 1)                            
                     OR (acct.branch-id EQ mUsrBranch         AND          
                         mAcctFltN      EQ 2)                          
                     OR (mAcctFltN      EQ 3)                           
                    )                                                
              AND acct.filial-id EQ shfilial NO-LOCK:

   RUN CreateTmpAcct.

END.


FORM
   tmpacct.acct
   tmpacct.currency
   tmpacct.obal
WITH FRAME browse1
     TITLE COLOR BRIGHT-WHILE "[ ������� ����� ]"
     WIDTH 78
.
FORM
   tmpacct.acct
   tmpacct.currency
   tmpacct.cust-name LABEL "������������"
                     FORMAT "x(40)"
WITH FRAME browse2
     TITLE COLOR BRIGHT-WHILE "[ ������� ����� ]"
     WIDTH 78
.

RELEASE tmpacct.
RELEASE flt-acct.

{qrdef.i
&buff-list = "tmpacct"
&need-buff-list = "tmpacct"
}

RUN shwacct-flt.

{navigate.cqr
   &oh6      = "�F6 䨫���"
   &oth6     = "flt-file.f6 "
   &file     = "tmpacct"
   &files    = "tmpacct"
   &filt     = YES
   &maxoq    = 1
   &avfile   = "tmpacct "
   &maxfrm   = 2
   &bf1      = "tmpacct.acct tmpacct.currency tmpacct.obal "
   &bf2      = "tmpacct.acct tmpacct.currency tmpacct.cust-name "
   &workfile = "/*"
   &nodel    = "/*"
   &look     = "shwacct3.nav "
   &return   = "shwacct3.ret "
   &oh2      = "�F3"
   &oth4     = "find.cqr "
      &find1 = "match.cqr &sfld=""acct"" "
      &find2 = "match.cqr &sfld=""cust-name"" "
      &find3 = "search2.cqr &operator="EQ" &sfld=""obal"" "
}

PROCEDURE shwacct-flt:
   MAIN-BLOCK:
   DO 
      ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
      ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

      {flt-file.add
         &cat        = 1
         &tablef     = "'tmpacct'"
         &include    = "'acct,currency,obal'"
         &sortf      = "'*'"
         &labelt     = "'����⥪� N2'"
         &double     = "'obal'"
         &classf     = "'tmpacct'"
      }
    
      {flt-file.atr
         &asgn      = YES
         &XCODE     = "'acct'"
         &a-label   = "'���:'"
         &a-basic   = "'acct'"
         &a-help    = "'���'"
         &a-procename = "'browseldvar'"
         &a-param   = "'number,accto,,,,2'"
      
      }

      {flt-file.atr
         &asgn      = YES
         &XCODE     = "'currency'"
         &a-label   = "'�����:'"
         &a-basic   = "'currency'"
         &a-help    = "'�����'"
       
      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'obal1'"
         &a-datatype   = "'DECIMAL'"   
         &a-format     = "'>>>>,>>>,>>>,>>9.99'"
         &a-initial    = "'0.00'"
         &a-label      = "'�㬬� ��:'"
         &a-help       = "'�㬬� ��'"
         &a-basic   = "'obal'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'obal2'"
         &a-datatype   = "'DECIMAL'"
         &a-format     = "'>>>>,>>>,>>>,>>9.99'"
         &a-initial    = "'999,999,999,999.99'"   
         &a-label      = "'��:'"
         &a-help       = "'�㬬� ��'"
         &a-basic   = "'obal'"

      }

      {flt-file.end}

   END.
END PROCEDURE.

PROCEDURE CreateTmpAcct:
   {getcust.i &name=name &Offinn="/*"}
   CREATE tmpacct.
   ASSIGN
     tmpacct.rec-oa    = RECID(acct)
     tmpacct.acct      = acct.acct
     tmpacct.currency  = acct.curr
     tmpacct.cust-cat  = acct.cust-cat
     tmpacct.cust-id   = acct.cust-id
     tmpacct.cust-name = name[1] + " " + name[2]
   .
   RUN acct-pos IN h_base (tmpacct.acct, tmpacct.currency, gend-date, gend-date, "�").
   tmpacct.obal = IF tmpacct.currency > "" THEN sh-val
                                       ELSE sh-bal.
   IF tmpacct.obal NE 0 THEN tmpacct.fobal = yes.
END PROCEDURE.
