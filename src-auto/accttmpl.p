/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACCTTMPL.P
      Comment: ��楤�� ��ࠡ�⪨ 蠡���� ��� (ᮧ�����,����). ��ࠡ�⪠
               ��� ������� �⭮�⥫쭮 �������.
   Parameters: RECID(op-template) - ID 蠡���� ���, � ���஬ ��������
                                    ��ࠬ���� ���
               RECID(loan)        - ID ������� �⭮�⥫쭮 ���ண� �������
                                    ��ࠡ�⪠ 蠡���� ���
               DATE               - ��� ������ ���
         Uses: accttmpl.lib
      Used by:
      Created: 30.10.2001 Kostik
     Modified: 02.07.2002 18:34 KSV      (0007589) ��������� ᮧ����� ���.४����⮢, �࠭����� ��
                                         ��. ⠡��� tAttr (������ � ACCTTMPL.LIB).
     Modified: 27.02.2003 ����   - ��᫥ ��।������ ஫� ��� ���������
                                    �஢�ઠ ������ ��� � ⠪�� ஫��
     Modified: 17.03.2003 14:29 kolal    ������� ���筨� branch-id.
                                         �᫨ � ������� ���� ���祭��
                                         �� branch-id � ������ �� ��.
                                         ��� 13858.
     Modified: 28.03.2003 12:18 ilvi     �� ��� 11822
     Modified: 31.03.2003 18:38 ilvi     �� ��� 11822
     Modified: 21.05.2003 12:38 ilvi     �� ��� 11822
     Modified: 10.11.2005 ZIAL (0043043) ���᫥��� ��� � �ᯮ�짮������
                                         �㭪樨 PLACCT
     Modified: 30.01.2008 jadv (0077751) ��ࠡ�⪠ ����, �᫨ ��� १�ࢠ ������,
                                         ���뢠�� ���� ���.
     Modified: 27.02.2008 jadv (0047838) �� ᮧ������ ��� � ���������� �� 蠡���� ஫��.
     Modified:29/12/2016 kraw (0275914) ���뢠�� ���� ����� ��� � ��᪥
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get tmess}
{intrface.get pbase}
{intrface.get ovl}
{intrface.get xclass}

DEFINE INPUT PARAMETER iRidCond          AS CHAR  NO-UNDO. /* ��� ��।�� ���⥪��. */
DEFINE INPUT PARAMETER in-rid-loan       AS RECID NO-UNDO.
DEFINE INPUT PARAMETER in-op-date        AS DATE  NO-UNDO.

DEFINE VARIABLE vKodDoxRash AS CHARACTER NO-UNDO. /* ����窠 ��� ��ॢ��᫥��� tv-acct-mask */

/* ��६���� ��� Check-Acct */
DEF VAR vField       AS CHAR   NO-UNDO. /* ��� ���� �訡��. */
DEF VAR vUpdValid    AS INT64  NO-UNDO. /* ��� �訡��. */
DEF VAR in-rid-templ AS RECID  NO-UNDO.
DEF VAR vChkLA       AS LOG    NO-UNDO. /* ������ �� �஢��� �� loan-acct. */
DEF VAR vTokacct     AS CHAR   NO-UNDO.
DEF VAR vTokidx      AS CHAR   NO-UNDO.
DEF VAR vToklen      AS CHAR   NO-UNDO.

DEF VAR vMethodAfter AS CHARACTER NO-UNDO.
DEF VAR vMethodAfterOb AS CHARACTER NO-UNDO.
DEF VAR vMethodAfterKr AS CHARACTER NO-UNDO.
DEF VAR vListOb        AS CHARACTER NO-UNDO.
DEF VAR vListKr        AS CHARACTER NO-UNDO.

/* ��� ��।������ �����஢������ ������ */
DEF VAR mClient-Id   AS INT64 NO-UNDO .
DEF VAR mClient-Type AS CHARACTER NO-UNDO .
DEF VAR mClient-Name AS CHARACTER NO-UNDO .
DEF VAR mBlock       AS CHARACTER NO-UNDO .

DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItem   AS INTEGER   NO-UNDO.


IF NUM-ENTRIES (iRidCond, CHR (1))  GT 1
THEN ASSIGN
   vChkLA         =  NOT (ENTRY (2, iRidCond, CHR (1))  EQ "���")
   in-rid-templ   =  INT64 (ENTRY (1, iRidCond, CHR (1)))
.
ELSE
   ASSIGN
      vChkLA       = YES
      in-rid-templ = INT64 (iRidCond)
   .

FIND loan WHERE RECID(loan) EQ in-rid-loan
                          NO-LOCK NO-ERROR.

{intrface.get flt}
{tmprecid.def &NSH=YES}  /* �����䨪��� �����      */

&GLOB skip-acct
&GLOB NO_BAL2ACCT YES /*�� �� ����।�������� acct.number*/

DEF BUFFER bacct FOR acct.
DEF VAR mOtdel AS CHAR NO-UNDO.

{accttmpl.lib}

IF AVAILABLE loan THEN
   mOtdel = loan.branch-id.
IF mOtdel EQ "" THEN
   mOtdel = TRIM(GetUserBranchId(userid("bisquit"))).

RUN Get-Xattr-Templ(in-rid-templ).

/* �᫨ ������ �������஢�� , ᮮ�騬 �� �⮬ , ��� ��㤭��� ���  ⨯ ='�।��'.*/
IF AVAILABLE loan AND tv-Acct-type = '�।��'
THEN DO:
   ASSIGN
      mClient-Id    = loan.cust-id
      mClient-Type  = loan.cust-cat
      mBlock = ""
   .
   CASE mClient-Type :
      WHEN '�' THEN
         mBlock = GetXAttrValue ('person', string(mClient-Id),'����') .
      WHEN '�' THEN
         mBlock = GetXAttrValue ('cust-corp', string(mClient-Id),'����') .
      WHEN '�' THEN
         mBlock = GetXAttrValue ('banks', string(mClient-Id),'����') .
   END CASE.
   IF mBlock = "��"
   THEN DO:
      RUN RE_CLIENT_FULL ( mClient-Type, mClient-Id, INPUT-OUTPUT  mClient-Name ) .
      RUN Fill-SysMes("","","", SUBSTITUTE("��������!!!  ������ &1 &2 - �������஢��. ��㤭� ��� ��⮬���᪨ �� ᮧ������." ,mClient-Id , mClient-Name )) .
   END.
END.


/* ��ࠡ�⪠ ��� ���.
** otdel ��।������ � Get-Xattr-Templ �� �� branch-id ����� acct-templ-umc
** �᫨ �� �� 㪠��� �����㥬 �� ��ன �奬�
*/
IF tv-otdel NE ""
THEN DO:
   RUN GetOtdel.

   IF RETURN-VALUE NE ""
   THEN RETURN "-1".
   ELSE mOtdel = tv-otdel.
END.

IF tv-acct-type EQ "" OR tv-acct-type EQ ? THEN
DO:
   RUN Fill-SysMes("","","","��� 蠡���� � " + STRING(tv-op-template) +
                   "�࠭���樨 '" + tv-op-kind + "'" + "�� 㪠���� ஫� ���.").
   RETURN.
END.

/* ��⮬ ��� �⮣� ᤥ���� ����ன�� */
FIND FIRST loan-acct WHERE
           loan-acct.contract  EQ loan.contract
       AND loan-acct.cont-code EQ loan.cont-code
       AND loan-acct.acct-type EQ tv-Acct-type
   NO-LOCK NO-ERROR.

/* �᫨ ����室��� �ந������� �஢�ન �� ����稥 ���, �
** ᭠砫� ���� ������ ���� �� ⠪�� */
IF    vChkLA
  AND AVAIL loan-acct THEN
DO:
      /* �஢��塞 ���� �ਢ離� ���. �᫨ ��� �������, 祬 �ந�������� ������, �
      ** �㣠���� � �� ���뢥� ���  */
   IF loan-acct.since GT in-op-date THEN
   DO:
      RUN Fill-SysMes("","","","������ ��� � ஫�� '" + tv-Acct-type +
                      "' �ਢ易��� ����� ������� ��⮩, 祬 ��� ����樨.").
      RETURN.
   END.
      /* �஢��塞 ���. �᫨ �� �����, � �ய�᪠�� ᮧ����� */
   IF CAN-FIND(FIRST acct WHERE acct.acct       EQ loan-acct.acct
                            AND acct.currency   EQ loan-acct.currency
                            AND acct.close-date EQ ?)
   THEN
      RETURN.
   ELSE
   DO:
         /* �᫨ �ਢ易��� � �������� ��� � �������� ஫�� �������� �������, � */
      IF CAN-FIND(FIRST acct WHERE acct.acct       EQ loan-acct.acct
                               AND acct.currency   EQ loan-acct.currency
                               AND acct.close-date NE ?)
      THEN DO:
            /*  �᫨ ஫� �� ������� � ���祭�� �����筮�� ��ࠬ��� ������, � ��� �� ���뢠����. */
         IF NOT CAN-DO(FGetSetting("������","",?),tv-Acct-type) THEN
            RETURN.
      END.
   END.
END.

IF AVAILABLE loan THEN
   Set_Loan(loan.contract,loan.cont-code).

RUN SetSysConf IN h_base ("LoanRecid-Acct19", STRING(in-rid-loan)).
RUN Convert-Param.
RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").


RUN Search-Acct.   /*����� ᤥ���� ⠪: �᫨ ��� �� ������, � ��楤��
                     ����� tv-Create-Find = "���������" � ᮧ������
                     ���� ���. */
mStrTMP = GetSysConf("LoanAcctTmplMaskOldAcct").

IF INDEX(tv-Acct-Mask, "�") GT 0
   AND LENGTH(mStrTMP) EQ 20
   THEN
DO:

   DO mItem = 1 TO LENGTH(tv-Acct-Mask):
      IF SUBSTRING(tv-Acct-Mask, mItem, 1) EQ "�" THEN
         SUBSTRING(tv-Acct-Mask, mItem, 1) = SUBSTRING(mStrTMP, mItem, 1).
   END.
   RUN SetSysConf IN h_base ("LoanAcctTmplMaskOldAcct", "").
END.

RUN Create-Acct(OUTPUT mess).
IF mess NE "" THEN
   RUN Fill-SysMes("","","","������ � " + STRING(tv-op-template) +
                   " �࠭���樨 '" + tv-op-kind + "':" + "~n" + mess).

IF AVAIL acct THEN
DO:
/*  acct.number = TRIM(ENTRY(1, acct.acct, "@")).
�� ⥯��� ������ RUN Create-Acct., � acct NO-LOCK */

  /* ���࠭�� ��뫪� �� �������  ��� ᯥ� ��ࠡ�⪨ � ��楤��� �஢�ન ��⮢ , � ��⭮�� ��� �訡�� ACCT19 */
  RUN SetSysConf IN h_base ("LoanRecid-Acct19", string(in-rid-loan)).

  /* �஢��窨, ���⠭���� ���� �᫨ ���� � ��祥 */
  RUN Check-Acct IN h_acct (BUFFER acct,
                            OUTPUT vField,
                            OUTPUT vUpdValid ).
  RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").
  IF vUpdValid NE 0
      THEN RETURN "-1".

  FOR EACH tAttr WHERE tAttr.fAttrVal <> ? AND tAttr.fAttrVal <> "":
    IF UpdateSigns ("acct",acct.acct + "," + acct.currency,tAttr.fAttrName,tAttr.fAttrVal,
                   isXAttrIndexed(acct.class-code,tAttr.fAttrName)) <> YES THEN
    DO:
      mess = "�訡�� ᮧ����� ���.४����� " + tAttr.fAttrName + " (" + tAttr.fAttrVal + ")".
      RETURN "-1".
    END.
  END. /* End of FOR */

   /* ��ॢ���塞 tv-acct-mask ���� �� �뫮 ��������� ��⮢
      � �����४�� �����ᮢ� ��⮬ �/��� ����⮩ */
   RUN "FindAcctMask" (tv-Cr-Class-Code,
                       tv-Bal-Acct,
                       INPUT-OUTPUT tv-Acct-Mask,
                       INPUT-OUTPUT vKodDoxRash ) NO-ERROR.
   RUN GetAcctMask IN h_acct
                  (INPUT tv-Acct-Mask,
                   OUTPUT vTokacct,
                   OUTPUT vTokidx,
                   OUTPUT vToklen) NO-ERROR.
END.

  CREATE tt-editacct.
   ASSIGN
      tt-editacct.num       = GetNumAcct()
      tt-editacct.rid       = IF AVAIL acct THEN RECID(acct)
                                            ELSE ?
      tt-editacct.acct-type = tv-Acct-type
      tt-editacct.edit      = IF AVAIL acct AND tv-Create-Find EQ "���������" THEN yes
                                                                              ELSE no
      tt-editacct.find-acct = IF mess BEGINS "FIND"     THEN yes ELSE no
      tt-editacct.find-res  = 0
      tt-editacct.name      = IF tv-Acct-name NE "" THEN tv-Acct-name
                                                    ELSE "����:"
      tt-editacct.maskedit  = vTokacct
      tt-editacct.fndstat   = tv-Create-Find EQ "���������"
      tt-editacct.acct-code = tv-Acct-code
   .
/*   MESSAGE "�� ���� ��।����� ���!"    SKIP
           "�࠭�����: '" tv-op-kind "'"
           "蠡���  �"  tv-op-template
   VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/

IF AVAIL acct THEN
DO:
   vMethodAfter = GetXAttrValueEx("op-template", tv-op-kind + "," + STRING(tv-op-template), "RunAfterCreate", "").
   IF vMethodAfter NE "" THEN
   DO:
      vListOb = ENTRY(1,vMethodAfter,")").
      vListOb = SUBSTRING(vListOb, INDEX(vListOb,"(") + 1 ).
      vMethodAfterOb = ENTRY(1,vMethodAfter,"(").
      IF SearchPFile(vMethodAfterOb) THEN
         RUN VALUE(vMethodAfterOb + ".p") (in-rid-loan,RECID(acct),tv-Acct-type,vLISTOb).
      
      vLISTKr = ENTRY(2,vMethodAfter,";").
      vMethodAfterKr = ENTRY(1,vLISTKr,"(").
      vLISTKr = SUBSTRING(vLISTKr, INDEX(vLISTKr,"(") + 1, INDEX(vLISTKr,")") - INDEX(vLISTKr,"(") - 1).
      IF SearchPFile(vMethodAfterKr) THEN
         RUN VALUE(vMethodAfterKr + ".p") (in-rid-loan,RECID(acct),tv-Acct-type,vLISTKr).
   END.
END.

{intrface.del}          /* ���㧪� �����㬥����. */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/12/2015 18:55:02.925+04:00' */
/* $LINTUSER='kraw' */
/* $LINTMODE='1' */
/* $LINTFILE='accttmpl.p' */
/*prosigno3HNI/PxiUnmhH93ISZfdQ*/