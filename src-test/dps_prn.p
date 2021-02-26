/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1997 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: DPS_PRN.P
      Comment: ����� ��� ���� ���㬥�⮢ �� ����� DPS.
   Parameters:
      Created: Om 16/06/99
     Modified: Om 30/06/99
     Modified: Om 12/07/99
     Modified: Om 23/07/99
     Modified: Om 19/06/2000
     Modified: Om 06/06/2001 ��ࠡ�⪠:
                                - �������� ���� "⥪_��".
                                - ��ࠢ��� �ଠ� �⮡ࠦ���� %% �⠢�� � "���_��".
     Modified: Om 07/06/2001 �訡��:
                                - "⥪_��" �� ��ࠡ��뢠�� ��������� ���⮪.
     Modified: Om 09/08/2001 �訡��:
                                - "⥪_��" �� ���뢠�� �஫������ ������.
     Modified: 10/10/2002 kraw (0010361) �㭪�� "��_���_�"
     Modified: 10/10/2002 kraw (0010899) �㭪�� "���_��". ��� �㬬� ������.
     Modified: 10/10/2002 kraw (0010899) �㭪�� "���_��" �१ �㭪�� "�������"
     Modified: 03/02/2004 21-30 Laav   ��� 0025657. � �����, �����饬 ����, 
                                ��������� ��ࠡ�⪠ �ଠ� �뢮�� � ����ᨬ��� 
                                �� ����� ��ப� � ���ᮬ (��������� �㭪�� 
                                if..then..else ��᫥ ��ਡ�� format)   
     Modified: 08/07/2005 koav ��������� �㭪�� "���������" - ����� 㪠������� 
                                                  ���.४����� �����稪�       
     Modified: 13.10.2006 18:18 OZMI (0062669)
*/
Form "~n@(#) dps_prn.p 1.0 Om 16/06/99 Om 19/06/2000 "
with frame sccs-id stream-io width 250.

{norm.i}
{globals.i}
{ksh-defs.i new}
{inst_prn.i new}
/* �ᯮ����⥫�� �㭪樨 �� ������� */
{dpsproc.def}


{f_for_t.i} /* �㭪樨 ��� ࠡ��� � ���������ﬨ */

{intrface.get dps}
{intrface.get cust}     /* ������⥪� ��� ࠡ��� � �����⠬�. */
{intrface.get date}     /* �����㬥��� ��� ࠡ��� � ��⠬�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get strng}
printres = no.

DEF OUTPUT PARAM  Xresult AS DECIMAL NO-UNDO.
DEF INPUT  PARAM  Xdate1  AS DATE    NO-UNDO.
DEF INPUT  PARAM  Xdate   AS DATE    NO-UNDO.
DEF INPUT  PARAM  strpar  AS CHAR    NO-UNDO.

DEF SHARED VAR rid_loan AS RECID. /* RECID LOAN-a */

DEF VAR out_str AS CHAR FORMAT "x(70)" NO-UNDO. /* ��ப� ��� ���� � ��⮪ */
DEF VAR mont_h  AS CHAR
INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������" NO-UNDO.
DEF VAR kod_ost AS CHAR NO-UNDO. /* ��� ���⪠ */
DEF VAR l_acct  AS CHAR NO-UNDO. /* ���� ��� */
DEF VAR in_kau  AS CHAR NO-UNDO.
DEF VAR rab_str AS CHAR NO-UNDO.
DEF VAR mParam2 AS CHAR NO-UNDO.
DEF VAR COMMAND AS CHAR NO-UNDO. /* ����室���� �㭪�� */
DEF VAR max_len AS INT64  NO-UNDO. /* ���ᨬ��쭠� ����� ���� */
DEF VAR offset  AS INT64  NO-UNDO. /* ���饭�� �⭮�⥫쭮 ������ ��� */
DEF VAR in-surrogate AS CHAR NO-UNDO. /* ��� ���᪠ �������⥫쭮�� ४����� */
DEF VAR mReturn AS CHAR   NO-UNDO.

DEF VAR c1 AS CHAR NO-UNDO. /* ��� �뢮�� �㬬� �ய���� */
DEF VAR c2 AS CHAR NO-UNDO. /* ��� �뢮�� �㬬� �ய���� */
DEF VAR i  AS INT64  NO-UNDO. /* �ᯮ����⥫�� ���稪 */
DEF VAR j  AS INT64  NO-UNDO. /* �ᯮ����⥫�� ���稪 */
DEF VAR num_templ AS INT64 NO-UNDO. /* ����� 蠡���� � �࠭���樨 */

DEFINE VAR vCommChar     AS CHAR  NO-UNDO.  /* ��� �����ᨨ */
DEFINE VAR vProlOpenDate AS DATE  NO-UNDO.  /* ��� ������ */
DEFINE VAR vOpKindChar   AS CHAR  NO-UNDO.  /* ��� ���㠫쭮� �࠭����� */
DEFINE VAR vOpTemplInt   AS INT64   NO-UNDO.  /* ����� 蠡���� ����窨 */
DEFINE VAR vTemplRecid   AS RECID NO-UNDO.  /* Recid 蠡���� ����窨 */
DEF VAR delta AS INT64.
DEF VAR d AS DATE NO-UNDO.
DEF VAR dep_period AS CHAR NO-UNDO.
DEF VAR vBankMFO AS CHARACTER NO-UNDO.
DEF VAR ksh-bal1 AS DECIMAL   NO-UNDO. /* ��� �࠭���� ������ */
DEF VAR logTrim AS LOG NO-UNDO INIT NO.
DEF VAR chTrim AS CHAR NO-UNDO.
DEF VAR iIndex AS INT64 NO-UNDO.

DEF VAR mSubs  AS CHAR   NO-UNDO.
DEF VAR vCurr     AS CHARACTER INIT ? NO-UNDO.
DEF VAR vParentCC AS CHARACTER NO-UNDO.
DEF VAR vLimitProl AS INT64 NO-UNDO.
DEF VAR vUnit      AS CHARACTER NO-UNDO.
DEF VAR vUnitStart AS CHARACTER NO-UNDO.
DEF VAR mLnend-date   AS DATE        NO-UNDO.
DEF VAR vInterest AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPersSurr AS CHARACTER    NO-UNDO.
DEFINE VARIABLE vDogFlag  AS LOGICAL      NO-UNDO.
DEF VAR SubStatus AS CHAR NO-UNDO.
DEF VAR logReturn AS LOGICAL INIT YES NO-UNDO.

DEF BUFFER b-person FOR person. /* ���������� ����. */

IF strpar MATCHES "*trim*" AND 
   NOT strpar MATCHES "*trimstr*" THEN 
DO:
   logTrim = YES.
   iIndex = INDEX(strpar,",").
   chTrim = SUBSTRING(strpar,1,iIndex - 1).
   strpar = SUBSTRING(strpar,iIndex + 1,LENGTH(strpar) - iIndex).
END.


FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) FORWARD.

FUNCTION trimaddress RETURNS CHAR (INPUT in-address AS CHAR) FORWARD.

FIND FIRST loan WHERE RECID (loan) EQ rid_loan NO-LOCK NO-ERROR.

RUN GetBaseAcctRole (rid_loan,
                     xDate,
                     OUTPUT l_acct).       
RUN GetBaseKodOst (l_acct,
                   OUTPUT kod_ost).

FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                  AND loan-acct.since     LE xdate
                                  NO-LOCK NO-ERROR.
CASE loan.cust-cat:
   WHEN "�" THEN
      FIND FIRST person    WHERE
         person.person     EQ loan.cust-id
      NO-LOCK NO-ERROR.
   WHEN "�" THEN
      FIND FIRST cust-corp WHERE
         cust-corp.cust-id EQ loan.cust-id
      NO-LOCK NO-ERROR.
END CASE.
ASSIGN
    COMMAND = ENTRY(1,STRPAR)
/*     max_len = INT64(if num-entries(STRPAR) ge 2 then entry(1,STRPAR) else '0') */
/*     offset  = INT64(if num-entries(STRPAR) ge 3 then entry(1,STRPAR) else '0') */
.

IF RetString THEN
DO:
   {setdest2.i &stream="stream fil" &filename="_spool1.tmp"} 
END.
   

FUNCTION dat_ RETURN CHAR (INPUT d AS INT64,
              INPUT p AS CHAR):
    DEF VAR yy AS CHAR EXTENT 3 INIT ['���','����','���'] NO-UNDO.
    DEF VAR mm AS CHAR EXTENT 3 INIT ['�����','�����','����楢'] NO-UNDO.
    DEF VAR dd AS CHAR EXTENT 3 INIT ['����','���','����'] NO-UNDO.
    d = IF d GT 3 THEN 3 ELSE d.
    IF p EQ '�' THEN RETURN yy[d].
    ELSE IF p EQ '�' THEN RETURN mm[d].
    ELSE IF p EQ '�' THEN RETURN dd[d].
END FUNCTION.

FUNCTION c_center RETURN CHAR (INPUT i_string AS CHAR):
    RETURN FILL(" ", INT64(TRUNCATE((PrinterWidth) / 2, 0))
                - INT64(TRUNCATE((LENGTH(TRIM(i_string))) / 2, 0)))
                + TRIM(i_string).
END FUNCTION.

IF COMMAND MATCHES "��_���*_�" THEN
DO:
   IF LENGTH(COMMAND) > 9 THEN
   DO:
      rab_str = SUBSTR(COMMAND,8,1).
      COMMAND = "��_���_�".
   END.
   ELSE rab_str = gop-status.
END.

ELSE IF CAN-DO("��_���2,��_���2�ய,�㬢��2�ய2,��_�㬏2",COMMAND) THEN
DO:
   rab_str = "�".
END.

ELSE IF COMMAND BEGINS "��_���_����" THEN DO:
   IF NUM-ENTRIES(COMMAND, "@") GE 3 THEN
   DO:
       rab_str = ENTRY(3, COMMAND, "@").
   END.
   ELSE rab_str = gop-status. 
   ASSIGN 
      
      vCurr   = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                   ELSE ENTRY(2, COMMAND, "@")
      COMMAND = ENTRY(1, COMMAND, "@")
   .
END.


ELSE IF COMMAND BEGINS "��_���" THEN
DO:
   IF LENGTH(COMMAND) > 7 THEN
   DO:
      rab_str = SUBSTR(COMMAND,8,1).
      COMMAND = SUBSTR(COMMAND,1,7).
   END.
   ELSE rab_str = gop-status.

END.

ELSE IF COMMAND BEGINS "����㬬�" THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") GE 2 THEN
      ASSIGN 
         vCurr   = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                      ELSE ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
END.

ELSE IF COMMAND BEGINS "�뤠�" THEN
DO:
   IF LENGTH(COMMAND) GT 5 THEN
   DO:
      ASSIGN
         rab_str = SUBSTR(COMMAND,7,LENGTH(COMMAND))
         COMMAND = SUBSTR(COMMAND,1,5).
   END.
   ELSE
      ASSIGN rab_str = ?.
END.

ELSE IF COMMAND BEGINS "���_��"
        AND NOT (COMMAND  BEGINS "���_��_�ய")  THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") LE 1 THEN
   DO:   
      IF LENGTH(COMMAND) > 6 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,6).
      END.
      ELSE rab_str = "99999".
   END.
   ELSE DO:
      IF LENGTH(ENTRY(1, COMMAND, "@")) > 6 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(ENTRY(1, COMMAND, "@"),7,1))).
      END.
      ELSE
         ASSIGN 
            rab_str = "99999"
      .                  

      ASSIGN 
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = SUBSTR(ENTRY(1, COMMAND, "@"),1,6)
         .
   END.
END.
ELSE IF COMMAND BEGINS "���_��_�ய" THEN
DO:
   IF NUM-ENTRIES(COMMAND, "@") LE 1 THEN
   DO:   
      IF LENGTH(COMMAND) > 11 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,11).
      END.
      ELSE rab_str = "99999".
   END.
   ELSE DO:
      IF LENGTH(ENTRY(1, COMMAND, "@")) > 11 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(ENTRY(1, COMMAND, "@"),12,1))).
      END.
      ELSE
         ASSIGN 
            rab_str = "99999"
      .                  

      ASSIGN 
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = SUBSTR(ENTRY(1, COMMAND, "@"),1,11)
         .
   END.
END.

ELSE IF COMMAND BEGINS "⥪_��" 
     AND NOT (COMMAND  BEGINS "⥪_��_�ய") THEN DO:
   
   IF LENGTH(COMMAND) > 7 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,8,1))).
         COMMAND = SUBSTR(COMMAND,1,7).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "⥪_��_�ய" THEN DO:
   
   IF LENGTH(COMMAND) > 12 THEN
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,13,1))).
         COMMAND = SUBSTR(COMMAND,1,12).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "���_��" THEN
DO:   
   IF NUM-ENTRIES(COMMAND, "@") > 1 THEN 
   DO:   
      ASSIGN
         rab_str = ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
   END.   
END.

ELSE IF COMMAND BEGINS "⥪_��" THEN
DO:
   rab_str = "99999".
   IF NUM-ENTRIES(COMMAND, "@") GE 2 THEN
      ASSIGN
         vCurr = IF ENTRY(2, COMMAND, "@") EQ "810" THEN ""
                                                    ELSE ENTRY(2, COMMAND, "@")
         COMMAND = ENTRY(1, COMMAND, "@")
      .
   IF LENGTH(COMMAND) = 7 THEN  /*��᫥���� ᨬ��� - �筮��� �뢮��, �᪫�稫� ⥪_��_�ய*/
      DO:
         rab_str = FILL("9",INT64(SUBSTR(COMMAND,7,1))).
         COMMAND = SUBSTR(COMMAND,1,6).
      END.
      ELSE rab_str = "99999".
END.

ELSE IF COMMAND BEGINS "���_�ய����" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"|") GE 2
   THEN 
      rab_str = ENTRY(2,COMMAND,"|").
   
   IF rab_str EQ ""   
   THEN
      rab_str = STRING(TODAY).
   
   IF NUM-ENTRIES(COMMAND,"|") GE 3
      THEN mParam2 = ENTRY(3,COMMAND,"|").

   COMMAND = "���_�ய����".
END.

ELSE IF COMMAND BEGINS "������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").         
   COMMAND = "������".
END.

ELSE IF COMMAND BEGINS "���ᔋ" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   IF NUM-ENTRIES(COMMAND,"@") GE 3 THEN 
      mParam2 = ENTRY(3,COMMAND,"@").
   COMMAND = "���ᔋ".
END.

ELSE IF COMMAND BEGINS "������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "������".
END.

ELSE IF COMMAND BEGINS "������┋" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "������┋".
END.

ELSE IF COMMAND BEGINS "��⠐�����" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "��⠐�����".
END.

ELSE IF COMMAND BEGINS "���⮐�����" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "���⮐�����".
END.

ELSE IF COMMAND BEGINS "��������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "��������".
END.

ELSE IF COMMAND BEGINS "��������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "��������".
END.

ELSE IF COMMAND BEGINS "��⠂뤄����" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "��⠂뤄����".
END.

ELSE IF COMMAND BEGINS "����뤄����" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "����뤄����".
END.

ELSE IF COMMAND BEGINS "������������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "������������".
END.

ELSE IF COMMAND BEGINS "��������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "��������".
END.

ELSE IF COMMAND BEGINS "���������┋" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "���������┋".
END.

ELSE IF COMMAND BEGINS "��������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").      
   COMMAND = "��������".
END.

ELSE IF COMMAND BEGINS "��������" THEN 
DO:
   IF NUM-ENTRIES(COMMAND,"@") GE 2 THEN 
      rab_str = ENTRY(2,COMMAND,"@").   
   COMMAND = "��������".
END.

ELSE IF COMMAND EQ "���_����_�" THEN
DO:
   vDogFlag = YES.
   COMMAND = "���_����".
END.

ELSE IF COMMAND EQ "������_����_�" THEN
DO:
   vDogFlag = YES.
   COMMAND = "����_�����".
END.

ELSE IF COMMAND BEGINS "������" THEN
DO:
   IF NUM-ENTRIES(COMMAND,'@') EQ 2 THEN
   DO:
      ASSIGN
      rab_str = ENTRY(2,COMMAND,'@')
      COMMAND = ENTRY(1,COMMAND,'@').
   END.
END.

CASE COMMAND:

WHEN "CustStatName" THEN 
DO:
   FOR FIRST cust-corp WHERE
             cust-corp.cust-id             EQ loan.cust-id
         AND LENGTH (cust-corp.cust-stat)  GT 0
         AND loan.cust-cat                 EQ "�"
   NO-LOCK,
   FIRST code WHERE code.class  EQ "����।�"
                AND code.val    EQ cust-corp.cust-stat
   NO-LOCK:
      mReturn = code.name.
   END.   
END.

WHEN "���_���" THEN
   DO:
   IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
      d = loan.open-date.
      ELSE RUN deposit-start-date in h_dpspc (RECID(loan),
                                                      Xdate,
                                                      OUTPUT d).
      delta = 0.
   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.
   IF NOT RetString THEN
      mReturn = (IF logTrim 
                 THEN TrimFormula(chTrim,
                                  STRING(d + delta, '99.99.9999')) 
                 ELSE STRING(d + delta, '99.99.9999')).
   ELSE
      mReturn = STRING(DAY(d + delta)) + " " + 
                       ENTRY(MONTH(d + delta), mont_h) +
                       STRING(YEAR(d + delta), " 9999 �.").
END.

WHEN "����_���" THEN /* ��� ��筮�� ������ */
DO:
   IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
      d = loan.end-date.
   ELSE RUN deposit-end-date in h_dpspc (RECID(loan),
                                                Xdate,
                                                GetXattrValueEx("loan",
                                                                loan.contract
                                                                + ","
                                                                + loan.cont-code,
                                                                "dep_period",
                                                                ?),
                                                OUTPUT d)
                                                .
   delta = 0.

   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.

   IF NOT RetString THEN
      mReturn = (IF logTrim 
                 THEN TrimFormula(chTrim,
                                  STRING(d + delta, '99.99.9999')) 
                 ELSE STRING(d + delta, '99.99.9999')).
   ELSE
      mReturn = STRING(DAY(d + delta)) + " " + 
                       ENTRY(MONTH(d + delta), mont_h) +
                       STRING(YEAR(d + delta), " 9999 �.").
END.

WHEN "���_����" THEN /* ���� ���ࠧ������� ����� */
DO:
   IF vDogFlag THEN
      vUnit = loan.branch-id.
   ELSE
      vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "�⤥�����")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "����_��").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* �᫨ 室�� �� ���� */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "����_��").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("����_��",?,"").
END.

WHEN "��_����" THEN /* �㪮����⥫� ���ࠧ������� ����� */
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "�⤥�����")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "����㪏���").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* �᫨ 室�� �� ���� */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "����㪏���").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("�����",?,"").
END.
WHEN "����_�����" THEN
DO:
   IF vDogFlag THEN
      vUnit = loan.branch-id.
   ELSE
      vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "�⤥�����")).
   IF {assigned vUnit} THEN
   DO:
      FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
      IF AVAIL branch THEN
         mReturn = branch.name.
   END.
END.
WHEN "���ࠧ�������" THEN
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "�⤥�����")).   
   IF vUnit EQ "*" THEN
      vUnit = sh-branch-id.
   FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
   IF AVAIL branch THEN
      mReturn = branch.Branch-Id.

END.
WHEN "��������_���_���" THEN
DO:
   RUN Get-beg-date-prol in h_dpspc (rid_loan,
                                     xDate,
                                     OUTPUT vProlOpenDate,
                                     OUTPUT mLnend-date).

   mReturn = loan.cont-code + " �� " + STRING(vProlOpenDate).
END.
WHEN "����_��_����" THEN /* ��������� �㪮����⥫� ���ࠧ������� */
DO:
   vUnit = TRIM(GetXAttrValue("_user",USERID('bisquit'), "�⤥�����")).
   IF {assigned vUnit}
   THEN DO:
      mReturn = GetXAttrValue("branch", vUnit, "������㪏���").
      vUnitStart = vUnit.
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN LEAVE.
         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* �᫨ 室�� �� ���� */
         IF vUnit EQ vUnitStart THEN LEAVE.
         mReturn = GetXAttrValue("branch", vUnit, "������㪏���").
      END.
   END.
   IF NOT {assigned mReturn} THEN
      mReturn = FGetSetting("�������",?,"").
END.

/* ���㬥�� �� �᭮����� ���ண� ������� �㪮����⥫� �⤥����� ����� */
WHEN "����᭮�����" THEN 
DO:
   vUnit = TRIM(GetXAttrValue("_user",
                              USERID("bisquit"), 
                              "�⤥�����")).
   IF {assigned vUnit} THEN 
   DO:
      mReturn = GetXAttrValue("branch", vUnit, "����᭐㪏���").
      vUnitStart = vUnit.

      BRNCHS:
      REPEAT WHILE NOT {assigned mReturn}:
         FIND FIRST branch WHERE branch.Branch-Id EQ vUnit 
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(branch) THEN 
            LEAVE BRNCHS.

         vUnit = ENTRY (1, branch.Parent-Id, ";").
         /* �᫨ 室�� �� ���� */
         IF vUnit EQ vUnitStart THEN 
            LEAVE BRNCHS.
         mReturn = GetXAttrValue("branch", 
                                 vUnit, 
                                 "����᭐㪏���").
      END.
   END.
END.

WHEN "த��" THEN
DO:
	IF person.gender EQ TRUE THEN 
		mReturn = "�".
	ELSE
		mReturn = "��".
END.

WHEN "���_஦�" THEN /* ��� ஦����� �������� ��⭮�� ������ */
DO:
   IF person.birthday NE ? THEN
      mReturn = STRING(person.birthday,'99.99.9999').
END.

WHEN "���_஦��" THEN /* ��� ஦����� �������� ��⭮�� ������ */
DO:
   IF person.birthday NE ? THEN
      mReturn = STRING( person.birthday, '99.99.9999' ).
END.

WHEN "���_஦�2" THEN /* ��� ஦����� �������� ��⭮�� ������ �ଠ� 99 ����� 9999 ����*/
DO:
   IF person.birthday NE ? THEN
      mReturn = string(day(person.birthday),'99') + " " + entry(month(person.birthday),"ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
	  ������,�����,�������") + " " + string(year(person.birthday)) + " ����".
END.
   
WHEN "����_஦�" THEN /* ���� ஦����� �������� ��⭮�� ������ */
DO:
   mReturn = GetXAttrValue("person", string(person.person-id) ,"BirthPlace").
END.

WHEN "ࠡ��" THEN /* ���� ࠡ��� � ��������� �������� ��⭮�� ������ */
DO:
   mReturn = GetXAttrValue("person", string(person.person-id) ,"ࠡ��").
END.

WHEN "���_��" THEN /* ��� ������ ��������� ��� */
DO:
   FIND FIRST acct WHERE acct.acct     EQ loan-acct.acct
                     AND acct.currency EQ loan-acct.currency
        NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
      mReturn = STRING(DAY(acct.open-date)) + " " + 
                       ENTRY(MONTH(acct.open-date), mont_h) +
                       STRING(YEAR(acct.open-date), " 9999 �.").
END.

WHEN "���" THEN /* ��� �����稪� */
DO:
   CASE loan.cust-cat:
      WHEN "�" THEN      
         mReturn  =  IF LENGTH (person.inn)     GT 0 THEN person.inn    ELSE "".
      WHEN "�" THEN
         mReturn  =  IF LENGTH (cust-corp.inn)  GT 0 THEN cust-corp.inn ELSE "".
   END CASE.
END.

/*----------------------------------------------------
  ��� ������ (����� ������ ����� ������ �।�⢠)
----------------------------------------------------*/
WHEN "���_�����" THEN
DO:
   RUN deposit-return-date in h_dpspc (RECID(loan),
                                               Xdate,
                                               Get-Dep-Period(loan.contract
                                                              + ","
                                                              + loan.cont-code),
                                               OUTPUT d)
                                               .
    mReturn = IF logTrim 
              THEN TrimFormula(chTrim,
                               STRING(d, '99.99.9999')) 
              ELSE STRING(d, '99.99.9999').
END.

WHEN "��ਮ�" THEN
DO:
   delta = 0.
   IF NUM-ENTRIES(strpar) = 2 THEN
   DO ON ERROR UNDO, LEAVE:
      delta = INT64(ENTRY(2, strpar)).
   END.
   mReturn = IF logTrim 
             THEN TrimFormula(chTrim,
                              STRING(loan.end-date 
                                     - loan.open-date 
                                     + delta)) 
             ELSE STRING(loan.end-date 
                         - loan.open-date 
                         + delta).
END.

WHEN "��ਮ��" THEN
DO:
   delta = IF NUM-ENTRIES(strpar) EQ 2 THEN 
              INT64(ENTRY(2, strpar))
           ELSE 
              0.

   i = loan.end-date - loan.open-date + delta.    
   run amtstr ( DEC(i), FALSE, OUTPUT mReturn, OUTPUT c1). 
   i = 0.
   c1 = "".
   delta = 0.
END.
/*-------------------------------------------------------------------
  �ப ������ �ய���� - �⮫쪮 ���, �⮫쪮 ����楢 � �⮫쪮 ����
--------------------------------------------------------------------*/
WHEN "��ਮ�2" THEN
DO:
   IF loan.end-date EQ ? THEN
   DO:
      c2 = "�� ����ॡ������" .
   END. ELSE
   DO:
      DEF VAR yy AS INT64 NO-UNDO.
      DEF VAR mm AS INT64 NO-UNDO.
      DEF VAR dd AS INT64 NO-UNDO.

      IF Old_dps(BUFFER loan,OUTPUT dep_period) THEN
         c1 = "�=" + STRING(loan.end-date - loan.open-date).
      ELSE
         c1 = GetXattrValueEx("loan",
                              loan.contract
                              + ","
                              + loan.cont-code,
                              "dep_period",
                              ?)
                              .
      RUN period2dmy(c1, OUTPUT yy, OUTPUT mm, OUTPUT dd).

      c2 = "".
      IF yy > 0 THEN
      DO:
         i = yy.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(yy) + " " + dat_(i, "�") + " ".
      END.

      IF mm > 0 THEN
      DO:
         i = mm.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(mm) + " " + dat_(i, "�") + " ".
      END.

      IF dd > 0 THEN
      DO:
         i = dd.
         i = IF (i GE 5 AND i LE 20) OR i EQ (INT64((i / 10)) * 10)
               THEN 3
               ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
         c2 = c2 + STRING(dd) + " " + dat_(i, "�").
      END.
   END.
   mReturn = TRIM(c2).
END.

WHEN "��_���" THEN
   RUN ��_���("��_���",OUTPUT mReturn).

/* �㬬� ������ ��� �㬬� �ய���� */
WHEN "��_���_����" THEN
DO:
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC 
                        AND loan.parent-contract  EQ "dps" 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR. 
      RUN GetBaseAcctRole (RECID(loan),
                           xDate,
                           OUTPUT l_acct).       
      RUN GetBaseKodOst (l_acct,
                         OUTPUT kod_ost).
      FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                    AND loan-acct.since     LE xdate
                                    NO-LOCK NO-ERROR.
   
      IF AVAIL loan-acct THEN
      DO:
         in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
         RUN kau-pos.p(loan-acct.acct,
                       loan-acct.currency,
                       xdate,
                       xdate,
                       rab_str,
                       in_kau)
                       .
                       
         /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
         IF RETURN-VALUE EQ "�訡��" THEN
            ASSIGN Xresult = -2.
    
         IF ksh-bal = 0 AND ksh-val = 0 THEN
            RUN get-kauost-trans(loan.contract,
                                 loan.cont-code,
                                 "*",
                                 loan-acct.acct-type,
                                 "",
                                 "",
                                 kod_ost,
                                 end-date,
                                 end-date,
                                 gop-status,
                                 OUTPUT ksh-bal). 
         
         FIND FIRST code WHERE code.class EQ '�������'
                           AND code.code  EQ acct.kau-id NO-LOCK NO-ERROR.
         SubStatus = GetEntries(2,code.misc[4],",",""). 
                      
         IF     SubStatus NE "" 
            AND SubStatus GE rab_str THEN
         DO:
            FOR EACH op-entry WHERE (op-entry.acct-cr   EQ acct.acct
                                 AND op-entry.op-date   LE Xdate
                                 AND op-entry.op-status GE rab_str
                                 AND op-entry.op-status LT SubStatus) 
                                 OR (op-entry.acct-db   EQ acct.acct
                                 AND op-entry.op-date   LE Xdate
                                 AND op-entry.op-status GE rab_str
                                 AND op-entry.op-status LT SubStatus)                       
            NO-LOCK:
               IF acct.currency NE "" THEN
               DO:
                  IF op-entry.acct-db EQ acct.acct THEN 
                     ksh-val = ksh-val + op-entry.amt-cur.
                  IF op-entry.acct-cr EQ acct.acct THEN
                     ksh-val = ksh-val - op-entry.amt-cur.         
               END.    
               IF acct.currency EQ "" THEN
               DO:
                  IF op-entry.acct-db EQ acct.acct THEN 
                     ksh-bal = ksh-bal + op-entry.amt-rub.
                  IF op-entry.acct-cr EQ acct.acct THEN
                     ksh-bal = ksh-bal - op-entry.amt-rub.
               END.                                 
            END.
            IF Xresult EQ -2 AND (ksh-bal NE 0 OR ksh-val NE 0)  THEN 
               Xresult = 0.    
         END.
         
         ksh-bal = IF loan.currency  EQ ''
                   THEN (IF acct.side EQ '�'
                         THEN ksh-bal
                         ELSE ksh-bal * (-1))
                   ELSE (IF acct.side EQ '�'
                         THEN ksh-val
                         ELSE ksh-val * (-1)).
         mReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
   END.
END.

/* ��������: �㬬� ������ */
WHEN "��_���_����_�ய" THEN
DO:
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC
                        AND loan.parent-contract  EQ "dps" 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR. 
      RUN GetBaseAcctRole (RECID(loan),
                           xDate,
                           OUTPUT l_acct).       
      RUN GetBaseKodOst (l_acct,
                         OUTPUT kod_ost).
      FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                    AND loan-acct.since     LE xdate
                                    NO-LOCK NO-ERROR.   
      IF AVAIL loan-acct THEN
      DO:
         in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
         FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
            RUN kau-pos.p(loan-acct.acct,
                         loan-acct.currency,
                         xdate,
                         xdate,
                         rab_str,
                         in_kau).
                         
         /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
         IF RETURN-VALUE EQ "�訡��" THEN
            ASSIGN Xresult = -2.
    
         IF ksh-bal = 0 AND ksh-val = 0 THEN
            RUN get-kauost-trans(loan.contract,
                                 loan.cont-code,
                                 "*",
                                 loan-acct.acct-type,
                                 "",
                                 "",
                                 kod_ost,
                                 end-date,
                                 end-date,
                                 gop-status,
                                 OUTPUT ksh-bal). 
            
            FIND FIRST code WHERE code.class EQ '�������'
                              AND code.code  EQ acct.kau-id NO-LOCK NO-ERROR.
            SubStatus = GetEntries(2,code.misc[4],",",""). 
                       
            IF     SubStatus NE "" 
               AND SubStatus GE rab_str THEN
            DO:
               FOR EACH op-entry WHERE (op-entry.acct-cr   EQ acct.acct
                                    AND op-entry.op-date   LE Xdate
                                    AND op-entry.op-status GE rab_str
                                    AND op-entry.op-status LT SubStatus) 
                                    OR (op-entry.acct-db   EQ acct.acct
                                    AND op-entry.op-date   LE Xdate
                                    AND op-entry.op-status GE rab_str
                                    AND op-entry.op-status LT SubStatus)                       
               NO-LOCK:
                  IF acct.currency NE "" THEN
                  DO:
                     IF op-entry.acct-db EQ acct.acct THEN 
                        ksh-val = ksh-val + op-entry.amt-cur.
                     IF op-entry.acct-cr EQ acct.acct THEN
                        ksh-val = ksh-val - op-entry.amt-cur.      
                  END.    
                  IF acct.currency EQ "" THEN
                  DO:
                     IF op-entry.acct-db EQ acct.acct THEN 
                        ksh-bal = ksh-bal + op-entry.amt-rub.
                     IF op-entry.acct-cr EQ acct.acct THEN
                        ksh-bal = ksh-bal - op-entry.amt-rub.
                  END.                                 
               END.
               IF Xresult EQ -2 AND (ksh-bal NE 0 OR ksh-val NE 0) THEN 
                  Xresult = 0.     
            END.
            
            ksh-bal = IF loan.currency  EQ ''
                     THEN (IF acct.side EQ '�'
                           THEN ksh-bal
                           ELSE ksh-bal * (-1))
                     ELSE (IF acct.side EQ '�'
                           THEN ksh-val
                           ELSE ksh-val * (-1)).
            RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
    
            c1 = c1 + " " + c2.

           mReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
   END.
END.

/* �㬬� ������ ��� �㬬� �ய���� */
WHEN "��_���2" THEN
DO:
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    rab_str,
                    in_kau)
                    .
      
      /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
      IF RETURN-VALUE EQ "�訡��" THEN
         ASSIGN Xresult = -2.

      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal). 

      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ '�'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ '�'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
      mReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')) .
   END.
END.

/* �㬬� ������ ��� �㬬� �ய���� � ����������� "��" � "���"*/
WHEN "�㬢��2�ய2" THEN
   RUN �㬂���ய("�㬢��2�ய2",OUTPUT mReturn).

WHEN "��_�㬏2" THEN
   RUN ��_���("��_�㬏2", OUTPUT mReturn).

/* ��������: �㬬� ������ */
WHEN "��_���2�ய" THEN
   RUN �㬂���ய("��_���2�ய",OUTPUT mReturn).

WHEN "��_���_�" THEN
DO:
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    gop-status,
                    in_kau).
      /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
      IF RETURN-VALUE EQ "�訡��" THEN
         ASSIGN Xresult = -2.
      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ '�'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ '�'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
      RUN "x-amtstr.p" (ksh-bal,
                        loan-acct.currency,
                        YES,
                        YES,
                        OUTPUT c1,
                        OUTPUT c2)
                        .
      c1 = c1 + " " + c2.
      mReturn = c_center(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99'))  + 
                (IF NOT RetString THEN "~n~n" ELSE "") +
                 c_center(c1).

   END.
END.

WHEN "���_��" THEN
DO:   
   IF rab_str EQ "" THEN
      FIND FIRST currency WHERE currency.currency EQ loan-acct.currency
           NO-LOCK NO-ERROR.
   ELSE DO:
      ASSIGN vCurr = rab_str.
      vCurr = IF rab_str EQ "810" THEN ""
                                  ELSE rab_str.

      IF vCurr NE ? THEN DO:
         ASSIGN vParentCC = loan.cont-code.          
         FIND FIRST loan WHERE loan.contract         EQ "dps"
                           AND loan.parent-cont-code EQ vParentCC
                           AND loan.parent-contract  EQ "dps" 
                           AND loan.currency         EQ vCurr 
                           NO-LOCK NO-ERROR.        
         IF AVAIL loan THEN
         DO:            
            FIND FIRST currency WHERE currency.currency EQ vCurr
               NO-LOCK NO-ERROR. 

         END.
      END.
   END.

   IF AVAIL currency THEN
      mReturn = TRIM(currency.name-currenc).
END.

WHEN "���_���" THEN
DO:
   FIND FIRST currency WHERE currency.currency EQ loan-acct.currency
                       NO-LOCK NO-ERROR.
   IF AVAIL currency THEN
   DO:
      IF loan-acct.currency = "840"
      THEN mReturn =  "�������� ���".
      ELSE IF loan-acct.currency = ""
           THEN mReturn = "������".
      ELSE IF loan-acct.currency = "276" OR loan-acct.currency = "280"
           THEN mReturn = "�������� ������".
      ELSE mReturn = currency.name-currenc.
   END.
END.

WHEN "���_��"   /* ��砫쭠� �⠢��             */ THEN RUN ������� ('commission', NO,  OUTPUT mReturn).
WHEN "���_��_�ய" /* ��砫쭠� �⠢�� �ய���� */ THEN RUN ������� ('commission', YES, OUTPUT mReturn).
WHEN "⥪_��"   /*          ���.�⠢�� ⥪��� */ THEN RUN ������� ('commission', NO,  OUTPUT mReturn).
WHEN "⥪_��"  /* ���䭠� ���.�⠢�� ⥪��� */ THEN RUN ������� ("pen-commi" , NO,  OUTPUT mReturn).
WHEN "⥪_��_�ய" /*������ �ᠢ�� �ய����*/     THEN RUN ������� ('commission', YES, OUTPUT mReturn).
WHEN "⥪_��_�ய"  /* ���䭠� ���.�⠢�� �ய���� */ THEN RUN ������� ("pen-commi" , YES,  OUTPUT mReturn).

WHEN "䨮" THEN
DO:
   out_str = IF NOT RetString THEN CAPS(person.name-last + " " + person.first-name)
                              ELSE person.name-last + " " + person.first-name.
   IF     NOT RetString 
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim,out_str).
   ELSE mReturn = out_str.
END.
WHEN "䨮���" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "��������",
                            person.name-last + " " + person.first-names).
END.
WHEN "䨮���" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "��������",
                            person.name-last + " " + person.first-names).
END.
WHEN "䨮����" THEN
DO:
   mReturn = GetXAttrValueEx("person",
                            STRING(person.person-id),
                            "���������",
                            person.name-last + " " + person.first-names).

END.

WHEN "䠬����" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim, person.name-last). 
   ELSE mReturn = person.name-last.

WHEN "���_���" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim, person.first-name). 
   ELSE mReturn = person.first-name.

WHEN "����" THEN
DO:
   DEF VAR vDefinedLength AS INT64.
   DEF VAR vAddressString AS CHAR.
   
   RUN RetAdr.p(person.person-id,"�","����ய",?,OUTPUT vAddressString).     /* aa4 -- ����⠢��� � ����� "�.", "��.", � �.�. */
  /* vAddressString = trimaddress(person.address[1])
                               + TRIM(person.address[2]). */                         
   vDefinedLength = INT64(ENTRY(2,strpar)) NO-ERROR.                       
   IF NOT RetString THEN DO:   
      IF NOT ERROR-STATUS:ERROR /*�஢�ઠ ��ࠬ��� ������⢠ ᨬ����� �� �ଠ�. */
            AND vDefinedLength GT 0 THEN        /*�᫨ �� ������⥫쭮�, � ���⠥� �� ���� ��ப�*/
         mReturn =       /*��।��塞 �ଠ� ��� ���� (�. ��� 25657)*/
            IF INT64(ENTRY(2,strpar)) > LENGTH(vAddressString) /*�᫨ ��� ���� �⢥���� ����� 祬 ���� ᨬ�����,*/
               THEN STRING(vAddressString ,  "x("+ STRING(LENGTH(vAddressString)) + ")") /*� �뢮��� ⮫쪮 ���� (��� �஡���� �ࠢ�),*/
               ELSE STRING(vAddressString ,  "x(" + ENTRY(2,strpar) + ")"). /*� ��⨢��� ��砥 ���� �뢮����� � ��ப� �������� �����.*/      
      ELSE
         DO: /*������뢠�� �� ���� ��ப�. ��� 15409*/
              vAddressString = SUBSTRING(vAddressString,
                                       1,
                                       (PrinterWidth - LENGTH(sh-context)))
                                       .
               mReturn = STRING (vAddressString, "x(" + STRING(LENGTH(vAddressString)) + ")").
         END.
   END.
   ELSE 
      mReturn = vAddressString.
END.

WHEN "����_1" THEN
DO:
   mReturn = STRING( trimaddress(person.address[1]),"x(35)").
   IF person.address[2] NE "" 
   THEN ASSIGN
      mReturn = mReturn + "~n" + FILL(" ",43) + "�����������������������������������"
      mReturn = mReturn + "~n" + FILL(" ",43) + person.address[2].
   .
END.

WHEN "����_2" THEN
DO:
   out_str = trimaddress(fGetStrAdr(person.address[1] + person.address[2])).
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,SUBSTRING(out_str,1,35)). 
      ELSE mReturn = SUBSTRING(out_str,1,35).
END.

WHEN "����_21" THEN
DO:
   out_str = trimaddress(fGetStrAdr(person.address[1] + person.address[2])).
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,SUBSTRING(out_str,36,35)). 
      ELSE mReturn = SUBSTRING(out_str,36,35).
END.

WHEN "⥫" THEN DO:
   mReturn = ENTRY(1,person.phone[1]).
   {additem.i mReturn ENTRY(2,person.phone[1])}
   {additem.i mReturn ENTRY(1,person.phone[2])}
   {additem.i mReturn ENTRY(2,person.phone[2])}
END.

WHEN "���㬥��" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,out_str). 
      ELSE mReturn = STRING( out_str, "x(78)").
END.


WHEN "���������" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.class-code     EQ "p-cust-ident"
                           AND cust-ident.cust-code-type EQ person.document-id
                           AND cust-ident.cust-cat       EQ "�"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date LE xDate)
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = substr(cust-ident.cust-code,1,5).

END.

WHEN "����������" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.class-code     EQ "p-cust-ident"
                           AND cust-ident.cust-code-type EQ person.document-id
                           AND cust-ident.cust-cat       EQ "�"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date LE xDate)
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = substr(cust-ident.cust-code,6,7).
END.
WHEN "���������" THEN
DO:
   FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                           AND (   cust-ident.close-date EQ ?
                           OR cust-ident.close-date GE xdate)
                           AND cust-ident.cust-cat       EQ "�"
                           AND cust-ident.cust-id        EQ person.person-id
                           AND cust-ident.cust-code-type EQ person.document-id
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
      mReturn = GetXAttrValue("cust-ident",
                              cust-ident.cust-code-type + "," 
                            + cust-ident.cust-code + ","
                            + STRING(cust-ident.cust-type-num),
                              "���ࠧ�").
END.

WHEN  "����㪐" THEN DO:
   out_str = FGetSetting("�����", "", "") .
   RUN corr-bin.p (INPUT "�䨮", INPUT-OUTPUT out_str).
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, out_str). 
    ELSE mReturn = out_str.
 END.   
WHEN  "������㪐" THEN DO:
   out_str = FGetSetting("�������", "", "").
   RUN corr-bin.p (INPUT "�", INPUT-OUTPUT out_str).
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, out_str). 
    ELSE mReturn = out_str.
 END.
WHEN  "�������" THEN 
    IF     NOT RetString
      AND logTrim 
    THEN mReturn = TrimFormula(chTrim, FGetSetting("�������", "", "")). 
    ELSE mReturn = FGetSetting("�������", "", "").

WHEN "���㬥��_1" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   IF LENGTH(out_str) LE 41 THEN
      mReturn = STRING(out_str , "x(41)").
   ELSE ASSIGN
      mReturn = SUBSTRING(person.issue,1,41) + "~n" 
      mReturn = mReturn + FILL(" ",37) + "�����������������������������������������" + "~n" 
      mReturn = mReturn + FILL(" ",37) + (SUBSTRING(person.issue,42,41)) + "~n" 
      mReturn = mReturn + FILL(" ",37) + "�����������������������������������������" + "~n" 
      mReturn = mReturn + FILL(" ",37) + (SUBSTRING(person.issue,83,41)) .
   .
END.

WHEN "���㬥��_2" THEN
DO:
   out_str = person.document-id + " N " + person.document.
   mReturn = SUBSTRING(out_str,1,35).
   IF LENGTH(out_str) > 35  THEN
      mReturn = mReturn + "~n" + FILL(" ",43) + SUBSTRING(out_str,36,35).
END.

WHEN "�뤠�" THEN
   RUN PrintIssue (COMMAND, rab_str, logTrim, OUTPUT mReturn).

WHEN "�����_���" THEN
      mReturn = STRING (IF AVAIL loan-acct
                        THEN DelFilFromAcct(loan-acct.acct)
                        ELSE "",
                       "x(20)").

WHEN "஫�_���" THEN
DO:
   mReturn = STRING (IF loan.end-date EQ ?
                     THEN '�� ����ॡ������'
                     ELSE '�������� ����',
                    "x(20)").
END.

WHEN "�������" THEN
   IF     NOT RetString
      AND logTrim 
   THEN mReturn = TrimFormula(chTrim,loan.doc-ref). 
   ELSE mReturn = loan.doc-ref.

WHEN "�ࠦ�" THEN
DO:
   IF AVAIL person THEN
      FIND FIRST country WHERE country.country-id EQ person.country-id
           NO-LOCK NO-ERROR.
   mReturn =  (IF AVAIL country
               THEN TRIM(country.country-name)
               ELSE "").
END.

WHEN "��த" THEN
DO:
   ASSIGN vBankMFO = FGetSettingEx("�������",?,?,YES).
   FIND banks-code WHERE banks-code.bank-code-type EQ "���-9" AND
                         banks-code.bank-code      EQ vBankMFO
                   NO-LOCK NO-ERROR.
   FIND banks OF banks-code NO-LOCK NO-ERROR.
   IF AVAIL banks THEN
      IF NOT RetString THEN
         mReturn = STRING ("   �. " + banks.town , "x(27)").
      ELSE
         mReturn = "   �. " + banks.town.
END.

WHEN "�ப_�஫" THEN
DO:
   FIND FIRST op-template WHERE op-template.op-template EQ loan.op-template
                          AND   op-template.op-kind     EQ loan.op-kind
                          NO-LOCK NO-ERROR.
   in-surrogate = op-template.op-kind + ',' + STRING(op-templ.op-templ).

   IF AvailXattr("op-template",in-surrogate,"prol-kind") THEN
      in-surrogate  = GetXattrValue("op-template",in-surrogate,"prol-kind").

   ELSE
   DO:
      i = loan.end-date - loan.open-date.
      num_templ = IF (i GE 5 AND i LE 20) OR
                      i EQ (INT64((i / 10)) * 10)
                  THEN 3
                  ELSE ((i - TRUNCATE(i / 10, 0) * 10) / 3) + 1.
      out_str = STRING (i,'>>>9') + ' ' + dat_(num_templ,'�').
      IF logTrim 
         THEN PUT STREAM fil UNFORMATTED TrimFormula(chTrim,out_str). 
         ELSE PUT STREAM fil out_str FORMAT "x(30)".
      RETURN.
   END.

   FIND FIRST op-kind WHERE op-kind.op-kind EQ in-surrogate NO-LOCK NO-ERROR.

   num_templ = get_op-templ(op-kind.op-kind,'loan','').

   FIND FIRST op-template OF op-kind
        WHERE op-template.op-kind     EQ op-kind.op-kind
          AND op-template.op-template EQ num_templ
              NO-LOCK NO-ERROR.

   in-surrogate = op-template.op-kind + ',' + STRING(op-templ.op-templ).

   IF AvailXattr("op-template",in-surrogate,"dep-period")  THEN
   DO:
      in-surrogate = GetXattrValue("op-template",in-surrogate,"dep-period").

         out_str = "".
      DO i = 1 TO NUM-ENTRIES(in-surrogate):
         out_str = out_str + ' ' + SUBSTR(ENTRY(i,in-surrogate),3).
         IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) EQ 1 THEN
             out_str = out_str + ' ' + dat_(1,SUBSTR(ENTRY(i,in-surrogate),1,1)).
         ELSE IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) GE 2 AND
             INT64(SUBSTR(ENTRY(i,in-surrogate),3)) LE 4 THEN
             out_str = out_str + ' ' + dat_(2,SUBSTR(ENTRY(i,in-surrogate),1,1)).
         ELSE IF INT64(SUBSTR(ENTRY(i,in-surrogate),3)) GE 5 AND
             INT64(SUBSTR(ENTRY(i,in-surrogate),3)) LE 20 THEN
             out_str = out_str + ' ' + dat_(3,SUBSTR(ENTRY(i,in-surrogate),1,1)).
      END.
   END.
   ELSE
     out_str = "�� ����ॡ������".
   IF logTrim 
      THEN mReturn = TrimFormula(chTrim,out_str). 
      ELSE mReturn = STRING( out_str , "x(30)").
END.
/* �������쭠� �㬬� ���.����� */
WHEN "����㬬�"  THEN
DO:
  IF vCurr NE ? AND AVAIL loan THEN
  DO:
     ASSIGN vParentCC = loan.cont-code. 
     FIND FIRST loan WHERE loan.contract         EQ "dps"
                       AND loan.parent-cont-code EQ vParentCC 
                       AND loan.parent-contract  EQ "dps"
                       AND loan.currency         EQ vCurr 
                       NO-LOCK NO-ERROR. 
  END.

  RUN get_last_param in h_dpspc (RECID(loan),
                                         xdate1,
                                         xdate,
                                         "����㬬�",
                                          OUTPUT out_str).

  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* ��������� ��᭨����� ���⮪ �� ������ */
WHEN "������" THEN
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"������",?).
  IF out_str = ? THEN

     RUN get_last_param in h_dpspc (rid_loan,
                                    xdate1,
                                    xdate,
                                    "������",
                                    OUTPUT out_str).
   mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* �㬬� �ய���� - �������쭠� �㬬� ���.����� */
WHEN "����㬬�����"  THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                         xdate1,
                                         xdate,
                                         "����㬬�",
                                          OUTPUT out_str).
   RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency,
                    YES,
                    YES,
                    OUTPUT c1,
                    OUTPUT c2).
   
   mReturn = TRIM(c1 + " " + c2).
END.

/* �㬬� �ய���� - ��������� ��᭨����� ���⮪ �� ������ */
WHEN "�����⏐��" THEN 
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"������",?).
  IF out_str = ? THEN

     RUN get_last_param in h_dpspc (rid_loan,
                                            xdate1,
                                            xdate,
                                            "������",
                                            OUTPUT out_str).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).    
END.

/* ���ᨬ���� ���⮪ �� ������ */
WHEN "������" THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "������",
                                          OUTPUT out_str).
   IF out_str = ? THEN
      out_str = GetXattrValueEx("loan",
                                loan.contract + "," + loan.cont-code,
                                "������",
                                ?).
  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* �㬬� �ய���� - ���ᨬ���� ��᭨����� ���⮪ �� ������ */
WHEN "�����⏐��" THEN 
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "������",
                                          OUTPUT out_str).
  IF out_str = ? THEN
     out_str = GetXattrValueEx("loan",
                               loan.contract + "," + loan.cont-code,
                               "������",
                               ?).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).
END.

/* ���ᨬ���� �����⨬� ���⮪ �� ������ */
WHEN "���ᄮ����" THEN
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"���ᄮ����",?).
  mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>>>>>>9.99")).
END.

/* �㬬� �ய���� - ���ᨬ���� �����⨬� ���⮪ �� ������ */
WHEN "���ᄮ����" THEN 
DO:
  out_str = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"���ᄮ����",?).
  RUN "x-amtstr.p" (DECIMAL(out_str),
                    loan.currency, 
                    YES, 
                    YES, 
                    OUTPUT c1, 
                    OUTPUT c2).
  mReturn = TRIM(c1 + " " + c2).
END.
/* �������쭠� �㬬� ��ࢮ��砫쭮�� ����� */
WHEN "������" THEN
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "������",
                                          OUTPUT out_str).
   mReturn = TRIM(STRING(DECIMAL(out_str), ">>>>>>>>>9.99")).
END.

/* �㬬� �ய���� - �������쭠� �㬬� ��ࢮ��砫쭮�� ����� */
WHEN "����������" THEN 
DO:
   RUN get_last_param in h_dpspc (rid_loan,
                                          xdate1,
                                          xdate,
                                          "������",
                                          OUTPUT out_str).
   RUN "x-amtstr.p" (DECIMAL(out_str),
                     loan.currency, 
                     YES, 
                     YES, 
                     OUTPUT c1, 
                     OUTPUT c2).
   mReturn = TRIM(c1 + " " + c2).
END.


/* ����� � ������� 㪠������� ���. ४����� �����稪� */
WHEN "���������" THEN 
DO:
   mReturn = GetXAttrValue("person",string(person.person-id) ,ENTRY(2,strpar)).
     /* ��� ENTRY(2,strpar) - ����祭�� ���. ४����� */
END.

/* ������� 3�� ��� */
WHEN "������" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.name-last.
     
END.

/* ��� ����⢮ 3�� ��� */
WHEN "����璋" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.first-names.
END.

/* ���� 3�� ��� */
WHEN "���ᒋ" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.address[1].
END.

/* ��� ���㬥�� 3�� ��� */
WHEN "��������" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE
   DO:
      FIND FIRST code WHERE CODE.code = person.document-id
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE code THEN
         mReturn = "".
      ELSE
         mReturn = CODE.name.
   END.
END.

/* ����� ���㬥�� 3�� ��� */
WHEN "����������" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.document.
END.

/* ��� �뤠� ���㬥�� 3�� ��� */
WHEN "�뤠������" THEN 
DO:
   RUN Get3Person IN h_dps (rid_loan, OUTPUT c1, OUTPUT c2).
   FIND FIRST person WHERE person.person-id = INT64(c1)
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE person THEN 
      mReturn = "".
   ELSE 
      mReturn = person.issue.
END.

/*��� ����砭�� ��ਮ�� ����������*/
WHEN "����珮����" THEN 
DO:
    DEF VAR mLim-date AS DATE NO-UNDO.
    DEF VAR mMess AS CHAR NO-UNDO INIT "".
    RUN end_doloan_dps in h_dpspc (RECID(loan),
                                       xDate,
                                       OUTPUT mLim-date,
                                       OUTPUT mMess).

    IF mLim-date NE ? THEN
        mReturn = STRING(mLim-date, "99/99/9999").
END.

WHEN "����஫�" THEN
DO:
   ASSIGN vParentCC = loan.cont-code. 
   FIND FIRST loan WHERE loan.contract         EQ "dps"
                     AND loan.parent-cont-code EQ vParentCC 
                     AND loan.parent-contract  EQ "dps"
                     NO-LOCK NO-ERROR. 
   /*�஢�ઠ ��࠭�祭�� ���-�� �஫����権 �१ �����䨪���
   �᫨ ��뫪� �� �����䨪��� ��� - � �஢��塞 ����� ��⥬ */
   RUN  get_limitprol_from_code in h_dpspc (RECID(loan),
                                                    XDate,
                                                    OUTPUT vLimitProl). 
   IF vLimitProl = ? THEN 
      /*�� 蠡���� ������ �࠭���樨 ������ ��� �� ����� ������ - ४����� limitprol*/
      RUN get-param-const in h_dpspc (RECID(loan),
                                              "limitprol",
                                              OUTPUT vLimitProl).
   IF vLimitProl <> ? THEN
      mReturn = STRING(vLimitProl).
END.

WHEN "��뤄����" THEN
DO:
   FIND FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                          AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                          AND cust-role.Class-Code EQ "����७���_���"
      NO-LOCK NO-ERROR.
   IF     AVAIL cust-role
      AND cust-role.cust-cat EQ "�" 
   THEN 
      mReturn = fGetDocIssueDate (INT64(cust-role.cust-id)).
   ELSE
      mReturn = FILL("_",10).
END.
      
WHEN "����������" THEN
DO:
   FIND FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                          AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                          AND cust-role.Class-Code EQ "����७���_���"
      NO-LOCK NO-ERROR.
   IF     AVAIL cust-role
      AND cust-role.cust-cat EQ "�" 
   THEN
      FIND FIRST b-person WHERE b-person.person-id EQ INT64(cust-role.cust-id)
         NO-LOCK NO-ERROR.
   IF AVAIL b-person 
   THEN 
      mReturn = b-person.document.
   ELSE
      mReturn = FILL("_",20).
END.

WHEN "���_�ய����" THEN
DO:
   mReturn = getDateString(DATE(rab_str), mParam2 EQ "��").
END. 

WHEN "inst_prn" THEN
DO:
   gClass = loan.class-code.
   gSurrogate = loan.contract + "," + loan.cont-code.
   gPrepareInstance = REPLACE(ENTRY(2,strpar),":",",").
   ENTRY(NUM-ENTRIES(gPrepareInstance),gPrepareInstance) = "".
   strpar = SUBSTRING(strpar,INDEX(strpar,",") + 1).
   strpar = REPLACE(strpar,"<<","(").
   strpar = REPLACE(strpar,">>",")").
   run inst_prn.p(
      OUTPUT Xresult,
      Xdate1,
      Xdate,
      strpar).
   mReturn = RETURN-VALUE.
END. 

WHEN "���_���" THEN
   DO:
	  d = DATE(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"��⠍�珥��","")).
	  mReturn = STRING(DAY(d)) + " " + 
                    ENTRY(MONTH(d), mont_h) +
                    STRING(YEAR(d), " 9999 �.").
END.

WHEN "������" THEN
DO:
   IF rab_str EQ "" THEN
      mReturn = loan.cont-type.
   ELSE DO:
      FIND FIRST code WHERE code.class  EQ 'cont-type'
                        AND code.parent EQ 'cont-type'
                        AND code.code   EQ loan.cont-type
      NO-LOCK NO-ERROR.
        IF AVAIL code THEN
        DO:
           CASE rab_str:
              WHEN "����" THEN 
                 mReturn = code.name.
              WHEN "��" THEN
                 mReturn = code.val.
           END CASE.
        END.
   END.
END.

WHEN "������" THEN
DO:

   /*RUN GetAdrCode (loan.cust-cat + "," + STRING(loan.cust-id),
                   rab_str,
                   OUTPUT mReturn).*/
 
  RUN RetAdr.p(loan.cust-id,loan.cust-cat,rab_str,loan.open-date,OUTPUT mReturn). /* ayv - �������� �ଠ�஢���� ����*/
        IF mReturn EQ '' and rab_str = '�������' THEN DO: RUN RetAdr.p(loan.cust-id,loan.cust-cat,'����ய',loan.open-date,OUTPUT mReturn). /*ZSS �� ������� �᫨ ���� 䠪� ���⮩ � ���� �ய���*/
        END.

END.

/* ��६���� ��� ࠡ��� � ��ꥪ⠬� �� ������ */
WHEN "���ᔋ" THEN
DO:
   RUN GetPersType (rid_loan,                    
                    mParam2,
                    OUTPUT mPersSurr).
   RUN GetAdrCode (mPersSurr,
                   rab_str,
                   OUTPUT mReturn).
END.

WHEN "������" THEN
DO:
   RUN GetSignsPers ("�������",
                     rab_str,
                     OUTPUT mReturn).
END.

WHEN "������┋" THEN
   RUN GetSignsPers ("�������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��⠐�����" THEN
   RUN GetSignsPers ("�������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "���⮐�����" THEN
   RUN GetSignsPers ("���⮐���",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��������" THEN
   RUN GetSignsPers ("������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��������" THEN
   RUN GetSignsPers ("��������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��⠂뤄����" THEN
   RUN GetSignsPers ("��⠂뤄��",
                     rab_str,
                     OUTPUT mReturn).

WHEN "����뤄����" THEN
   RUN GetSignsPers ("����뤄��",
                     rab_str,
                     OUTPUT mReturn).

WHEN "������������" THEN
   RUN GetSignsPers ("����������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��������" THEN
   RUN GetSignsPers ("������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "���������┋" THEN
   RUN GetSignsPers ("����������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��������" THEN
   RUN GetSignsPers ("������",
                     rab_str,
                     OUTPUT mReturn).

WHEN "��������" THEN
   RUN GetSignsPers ("������",
                     rab_str,
                     OUTPUT mReturn).

/* �����頥� X �᫨ ����� �� ����ॡ������ */
WHEN "�������" THEN
DO:
   IF loan.end-date EQ ? THEN
      mReturn = "X".
   ELSE
      mReturn = " ".
END.

/* �����頥� X �᫨ ����� ������ ᮮ⢥����� ��᪥ */
WHEN "����������" THEN
DO:
   IF loan.class-code MATCHES ENTRY(2,strpar) THEN
      mReturn = "X".
   ELSE
      mReturn = " ".
END.

END CASE.


IF COMMAND BEGINS("�����_�ਪ�") THEN
DO:
   vCurr = ?.

   IF LENGTH(COMMAND) > 11 THEN
   DO:
      mSubs = SUBSTR(COMMAND, 13, (LENGTH(COMMAND) - 13) + 
                        IF SUBSTR(COMMAND, LENGTH(COMMAND)) EQ ")" THEN 0
                                                                   ELSE 1).
      COMMAND = SUBSTR(COMMAND, 1 , 11).

      IF NUM-ENTRIES(mSubs, "@") GT 1 THEN
         ASSIGN 
             vCurr = IF ENTRY(2, mSubs,"@") EQ "810" THEN ""
                                                     ELSE ENTRY(2, mSubs, "@")
             mSubs = ENTRY(1, mSubs,"@") 
             .
   END.
   IF INDEX(mSubs, "_") NE 0 THEN
      mSubs = REPLACE(mSubs, "_", "-"). 

   FIND FIRST loan WHERE 
        RECID(loan) EQ rid_loan NO-LOCK NO-ERROR.   
   IF vCurr NE ? AND AVAIL loan THEN DO:
      ASSIGN vParentCC = loan.cont-code. 
      FIND FIRST loan WHERE loan.contract         EQ "dps"
                        AND loan.parent-cont-code EQ vParentCC 
                        AND loan.currency         EQ vCurr 
                        NO-LOCK NO-ERROR.                           
   END.
   IF AVAIL loan THEN
   DO:      
      
      FIND LAST loan-acct WHERE
             loan-acct.contract  EQ "DPS"
         AND loan-acct.acct-type EQ mSubs
         AND loan-acct.cont-code EQ loan.cont-code
         AND loan-acct.since     LE xdate
      NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN
      DO:      
         mReturn = DelFilFromAcct(loan-acct.acct).
      END.
   END.
END.

IF CAN-DO("䨮��,���ᄋ,��������,����������,��뤠������,��뤄����,�������", COMMAND) THEN
   RUN GetSignsPers(COMMAND, "����७���_���",OUTPUT mReturn).

IF CAN-DO("䨮��,���ፋ,��������,����������,��뤠������,��뤄����,�������,���", COMMAND) THEN
   RUN GetSignsPers(COMMAND, "��᫥����������",OUTPUT mReturn).
   
IF CAN-DO("��������,�������殐�,�������������,������������栐�,��������������,��⠄����������,�᭮���������", COMMAND) THEN
   RUN GetDocSotr(COMMAND, OUTPUT mReturn).   

/*�᫨ ���⠫� �� ������ � �㫥��� �㬬��, � ���� �訡��, � ���� �⮡� �� �� �뫮*/
IF xResult EQ -2 AND NOT RetString THEN
DO:
   xResult = 0.
   IF logReturn THEN
   RETURN "".
END.
IF NOT RetString 
THEN      
   PUT STREAM fil UNFORMATTED mReturn.
ELSE
   RETURN mReturn.

/* ��楤�� ����祭�� ४����⮢ ����७���� ��� ��� ��᫥����� */
PROCEDURE GetSignsPers:
   DEF INPUT  PARAM iCommand AS CHAR NO-UNDO.
   DEF INPUT  PARAM iCode    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oReturn  AS CHAR NO-UNDO.

   DEF BUFFER person       FOR person.
   DEF BUFFER cust-role    FOR cust-role.
   DEF BUFFER cust-ident   FOR cust-ident.
   DEF BUFFER country      FOR country.

   FOR FIRST cust-role         WHERE
             cust-role.file-name  EQ "loan"
         AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
         AND cust-role.class-code EQ iCode
      NO-LOCK,
      FIRST  person            WHERE
             person.person-id     EQ INT64(CUST-ROLE.cust-id)
      NO-LOCK:
                        /* ��।��塞 ��. */
      IF iCommand BEGINS "��|"
         THEN oReturn   =  GetXAttrValueEx (
                              "person",
                              STRING (person.person-id),
                              ENTRY (2, iCommand, "|"),
                              "").
                        /* ���� �������. */
      CASE iCommand:
      WHEN "䨮��"      THEN
         oReturn = person.name-last + " " + person.first-names.
      WHEN "���ᄋ"    THEN
         oReturn = person.address[1].
      WHEN "��������" THEN
         oReturn = GetCodeName("�������", person.document-id).
      WHEN "����������" THEN
         oReturn = person.document.
      WHEN "��뤠������" THEN
         oReturn = person.issue.
      WHEN "��뤄����" THEN
         oReturn = GetXAttrValue("person",
                                 STRING(person.person-id),
                                 "Document4Date_vid"
                                 ).
      WHEN "�������"   THEN
         oReturn =  STRING(person.birthday,"99/99/9999").
      WHEN "䨮��"      THEN
         oReturn = person.name-last + " " + person.first-names.
      WHEN "���ፋ"    THEN
         oReturn = person.address[1].
      WHEN "��������" THEN
         oReturn = GetCodeName("�������", person.document-id).
      WHEN "����������" THEN
         oReturn = person.document.
      WHEN "��뤠������" THEN
         oReturn = person.issue.
      WHEN "���" THEN
         oReturn = GetXAttrValueEx("cust-role", 
                                    STRING(cust-role.cust-role-id), 
                                    "heir-part", 
                                    "").
      WHEN "��뤄����" THEN
         oReturn =  GetXAttrValue("person",
                                  STRING(person.person-id),
                                  "Document4Date_vid"
                                  ).
      WHEN "�������"   THEN
         oReturn = string(person.birthday,"99/99/9999").

      WHEN "�������" THEN
         oReturn = person.name-last.
      WHEN "�������" THEN
         oReturn = person.first-names.
      WHEN "���⮐���" THEN
         oReturn = GetXAttrValue("person",
                                 STRING(person.person-id),
                                 "BirthPlace").
      WHEN "������" THEN
         oReturn = GetCodeName("�������", person.document-id).
      WHEN "��������" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "�"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = cust-ident.cust-code.
         END.
      END.
      WHEN "����뤄��" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "�"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = cust-ident.issue.
         END.
      END.
      WHEN "��⠂뤄��" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "�"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = STRING(cust-ident.open-date, "99/99/9999").
         END.
      END.
      WHEN "����������" THEN
      DO:
         FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-ident"
                                 AND (   cust-ident.close-date EQ ?
                                      OR cust-ident.close-date GE xdate)
                                 AND cust-ident.cust-cat       EQ "�"
                                 AND cust-ident.cust-id        EQ person.person-id
                                 AND cust-ident.cust-code-type EQ person.document-id
            NO-LOCK NO-ERROR.
         IF AVAIL cust-ident THEN
         DO:
            oReturn = GetXAttrValue("cust-ident",
                                    cust-ident.cust-code-type + "," 
                                       + cust-ident.cust-code + ","
                                       + STRING(cust-ident.cust-type-num),
                                    "���ࠧ�").
         END.
      END.
      WHEN "������" THEN
         oReturn = ENTRY(1, person.phone[1]).
      WHEN "����������" THEN
         oReturn = ENTRY(2, person.phone[1]).
      WHEN "������" THEN
         oReturn = ENTRY(1, person.phone[2]).
      WHEN "������" THEN
      DO:
         oReturn = ENTRY(2, person.phone[2]).
         IF NOT {assigned oReturn} THEN
            oReturn = GetXattrValueEx ("person",
                                       STRING (person.person-id),
                                       "cell-phone",
                                       ?).
      END.
      END CASE.
   END.

   RETURN.
END PROCEDURE.

PROCEDURE �������:
  DEFINE INPUT  PARAMETER in-commission AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER iPropis       AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER oReturn       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE c_date                AS DATE      NO-UNDO. /*��� ��
                                                                ������ ���� ���� �����ᨨ*/
  DEFINE VARIABLE c_stat                AS CHARACTER NO-UNDO. /*�����, ��
                                                                ���஬� ����� �������*/
  DEF VAR mMinOst AS DEC NO-UNDO. /*��������� ���⮪ ������*/
  DEF VAR vSinceDate AS DATE NO-UNDO.
  DEF VAR vDateEnd   AS DATE NO-UNDO.
  DEF VAR vBonusRate AS DEC  NO-UNDO.

  ASSIGN
     c_date = xdate
     c_date = loan.open-date  WHEN command BEGINS "���_��"
     c_stat = gop-status
     c_stat = ENTRY(2,strpar) WHEN NUM-ENTRIES(strpar) GE 2
  .

   IF COMMAND BEGINS "���_��" AND c_stat NE gop-status 
   THEN logReturn = NO.

  IF vCurr NE ? AND AVAIL loan THEN DO:
     ASSIGN vParentCC = loan.cont-code. 
     FIND FIRST loan WHERE loan.contract         EQ "dps"
                       AND loan.parent-cont-code EQ vParentCC 
                       AND loan.currency         EQ vCurr 
                       NO-LOCK NO-ERROR. 
     RUN GetBaseAcctRole (RECID(loan),
                          c_date,
                          OUTPUT l_acct).       
     RUN GetBaseKodOst (l_acct,
                        OUTPUT kod_ost).
     FIND LAST loan-acct OF loan WHERE loan-acct.acct-type EQ l_acct
                                   AND loan-acct.since     LE c_date
                                   NO-LOCK NO-ERROR.
  END.
  IF COMMAND BEGINS "���_��_�ய" THEN
  DO:
     RUN Get-beg-date-prol IN h_dpspc (RECID(loan),
                                       c_date,
                                       OUTPUT c_date,
                                       OUTPUT vDateEnd).
  END.
  IF AVAIL loan-acct THEN
  DO:
  /* ����祭�� ���� �����ᨨ */
  IF in-commission = "commission" THEN 
    RUN Get_Last_Commi     in h_dpspc (RECID(loan), c_date, c_date, OUTPUT vCommChar).
  ELSE 
    RUN Get_Last_Pen-Commi in h_dpspc (RECID(loan), c_date, c_date, OUTPUT vCommChar).

  /* �饬 ��� */
  FIND FIRST acct OF loan-acct
  NO-LOCK NO-ERROR.

  in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.

  ASSIGN vSinceDate = c_date.
  /*�饬 ��室 �� ����� �ࠧ� ��᫥ �ਢ離� ���*/
  FIND FIRST kau-entry WHERE
         kau-entry.acct = loan-acct.acct
     AND kau-entry.currency = loan.currency
     AND kau-entry.op-status >= gop-status
     AND NOT kau-entry.debit
     AND kau-entry.op-date >= vSinceDate
     AND kau-entry.kau = in_kau 
     NO-LOCK NO-ERROR.
  IF AVAILABLE kau-entry THEN 
  DO:
     FIND FIRST op OF kau-entry 
        NO-LOCK NO-ERROR.
     IF AVAILABLE op 
     /*�������� ��� ���㬥�� ������ ᮢ������ � ��⮩ ������!!!*/
        AND op.contract-date = c_date THEN 
     DO:
        vSinceDate = kau-entry.op-date.
     END.
  END.

  RUN kau-pos.p (loan-acct.acct,
                 loan-acct.currency,
                 vSinceDate,
                 vSinceDate,
                 c_stat,
                 in_kau).
  /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
  IF RETURN-VALUE EQ "�訡��" THEN
     ASSIGN Xresult = -2.

  ASSIGN
    ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE '�'
    ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE '�'
  .

  IF loan.end-date EQ ? THEN
     delta = 0. /* ����� �� ����ॡ������ */
  ELSE
     RUN deposit-dep-period in h_dpspc (RECID(loan),
                                                c_date,
                                                Get-Dep-Period(loan.contract + "," + loan.cont-code),
                                                OUTPUT delta).

  
     
     
 
  
  RUN Get_Last_Inter IN h_dpspc (RECID(loan),c_date,c_date,OUTPUT vInterest).
  IF vInterest <> ?  AND vInterest <> '?'
  THEN DO:
      {findsch.i &dir=LAST &sch=vInterest &since1 =" <= c_date"}
      IF AVAILABLE interest-sch-line AND
         interest-sch-line.proc-name  BEGINS "nchmin_o"
      THEN DO:
          mMinOst = DEC(GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"������",?)) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
              ASSIGN  mMinOst = 0.
              IF mMinOst NE ? THEN
              ASSIGN  ksh-bal = mMinOst 
                      ksh-val = mMinOst.
      END.
      
  END.




  {findcom1.i
      &dir       = LAST
      &comm-rate = comm-rate
      &rcom      = vCommChar
      &rsum      = ksh-bal
      &since1    = " le c_date "
      &vPeriodInt = delta
  }

  RUN getbonusrate(c_date, OUTPUT vBonusRate).

  IF AVAIL comm-rate THEN 
     IF NOT iPropis THEN
        oReturn = TRIM(STRING(comm-rate.rate-comm + vBonusRate,'zzzz9.' + rab_str)) .
     ELSE
     DO:
        RUN procprop.p (trim(string(comm-rate.rate-comm + vBonusRate,'zzzz9.' + rab_str)),
                        OUTPUT oReturn).
     END.

  RELEASE op-templ.
  END. /*AVAIL loan-acct */
END PROCEDURE.

PROCEDURE getbonusrate:
   DEF INPUT  PARAM iDate AS DATE  NO-UNDO .
   DEF OUTPUT PARAM oBonusRate AS DEC NO-UNDO .

   DEF VAR vBonus AS INT64 NO-UNDO.
   DEF VAR vCommiBonus AS CHAR NO-UNDO.
   DEF BUFFER bLoan-cond  FOR loan-cond.
   DEF BUFFER bBonus-rate FOR comm-rate.
   
   FIND LAST bLoan-cond WHERE bLoan-cond.contract EQ loan.contract AND
                bLoan-cond.cont-code EQ loan.cont-code AND
                bLoan-cond.since LE iDate 
                NO-LOCK NO-ERROR.
   IF AVAIL bLoan-cond THEN
   DO:          
      vBonus = INT64(GetXattrValueEx('loan-cond',bLoan-cond.contract + "," + bLoan-cond.cont-code + "," +
                               string(YEAR (bLoan-cond.since),"9999") + 
                               string(MONTH(bLoan-cond.since),"99"  ) +
                               string(DAY  (bLoan-cond.since),"99"  ),
                               "�����", "0")) NO-ERROR.      
      IF ERROR-STATUS:ERROR OR vBonus = ? THEN
         vBonus = 0. 
      END.
   IF vBonus GT 0 THEN
   DO:
      vCommiBonus = fGetSetting("�������������", ?, "").
      IF vCommiBonus NE "" THEN
      DO:
         { findcom1.i
            &dir=last
            &rsum=0
            &comm-rate=bBonus-rate
            &rcom=vCommiBonus
            &since1=" le iDate "
         }

         IF AVAILABLE bBonus-rate THEN
            oBonusRate = vBonus * bBonus-rate.rate-comm.
      END.
   END.
END PROCEDURE.

PROCEDURE period2dmy :
/*------------------------------------------------------------------------------
  Purpose:   "����஢��" ��ਮ��
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM in-str AS CHAR.
  DEFINE OUTPUT PARAM out-yy AS INT64.
  DEFINE OUTPUT PARAM out-mm AS INT64.
  DEFINE OUTPUT PARAM out-dd AS INT64.

  DEF VAR j AS INT64.
  DEF VAR s2 AS CHAR.

  ASSIGN
    out-yy = 0
    out-mm = 0
    out-dd = 0
  .

  REPEAT j = 1 TO NUM-ENTRIES(in-str) :
    s2 = ENTRY(j, in-str).
    IF NUM-ENTRIES(s2, "=") = 2 THEN DO:
      CASE ENTRY(1, s2, "=") :
        WHEN "�" THEN out-yy = INT64(ENTRY(2, s2, "=")) NO-ERROR.
        WHEN "�" THEN out-mm = INT64(ENTRY(2, s2, "=")) NO-ERROR.
        WHEN "�" THEN out-dd = INT64(ENTRY(2, s2, "=")) NO-ERROR.
      END CASE.
    END.
  END.
END PROCEDURE.

FUNCTION trimaddress RETURNS CHAR (INPUT in-address AS CHAR):
   DEF VAR t-s AS CHAR NO-UNDO.
   DEF VAR s AS CHAR NO-UNDO.
      DO i = 1 TO NUM-ENTRIES(in-address):
         s = TRIM(ENTRY(i,in-address)).
         IF s = "" THEN NEXT.
         {additem.i t-s s}
      END.
   RETURN t-s.
END.

FUNCTION TrimFormula RETURNS CHAR(INPUT FormatTrim AS CHAR, INPUT cValue AS CHAR) :
   CASE FormatTrim:
      WHEN "trim"  THEN cValue = TRIM(cValue).
      WHEN "ltrim" THEN cValue = LEFT-TRIM(cValue).
      WHEN "rtrim" THEN cValue = RIGHT-TRIM(cValue).
   END CASE.
   RETURN cValue.
END.

PROCEDURE PrintIssue:
   DEF INPUT  PARAM iCommand AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iSubStr  AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iLogTrim AS LOGICAL   NO-UNDO.
   DEF OUTPUT PARAM oReturn  AS CHARACTER NO-UNDO.

   DEF VAR vDate AS DATE NO-UNDO.
   DEF VAR vType AS INT64 NO-UNDO.
   DEF VAR vTmpStr AS CHARACTER NO-UNDO.

   IF iSubStr MATCHES "*���" THEN
      ASSIGN vDate = DATE(GetXattrValueEx("person", 
                                          STRING(person.person-id),
                                          "Document4Date_Vid",
                                          "")).

   ASSIGN vTmpStr = person.issue + IF vDate NE ? THEN (", " + STRING(vDate)) 
                                                            ELSE "".

          vTmpStr = REPLACE(vTmpStr,CHR(10),''). /*zss 㤠����� ��७�� ��ப�(�� ���⠥��� � ���.���)*/

   IF (iSubStr EQ ? OR iSubStr EQ "���") AND NOT RetString THEN
      ASSIGN vType = 0.
   ELSE IF iSubStr BEGINS "1" AND NOT RetString THEN
      ASSIGN vType = 1.
   ELSE IF iSubStr BEGINS "2" AND NOT (iSubStr BEGINS "21") AND NOT RetString THEN
      ASSIGN vType = 2.
   ELSE IF iSubStr BEGINS "21" AND NOT RetString THEN
      ASSIGN vType = 21.
   ELSE IF RetString THEN
      ASSIGN vType = 3.
   ELSE
      RETURN.

   CASE vType:
      WHEN 0 THEN
      DO:
         ASSIGN vTmpStr = person.issue + IF vDate NE ? THEN (", " + STRING(vDate)) 
                                                            ELSE "".
         IF LENGTH(vTmpStr) LE 78 THEN   
         DO:
            IF iLogTrim THEN 
               oReturn =  TrimFormula(chTrim,vTmpStr). 
            ELSE 
               oReturn = vTmpStr.
      
         END.      
         ELSE
            IF vDate EQ ? OR (vDate NE ? AND
                              LENGTH(SUBSTRING(vTmpStr,79)) GE 10) THEN
            DO:
               oReturn = SUBSTRING(vTmpStr,1,78)                                                          + "~n" 
                       + "������������������������������������������������������������������������������" + "~n" 
                       + SUBSTRING(vTmpStr,79).
            END.
            ELSE 
            DO:
               oReturn = SUBSTRING(vTmpStr,1,LENGTH(vTmpStr) - 10)                                        + "~n"
                       + "������������������������������������������������������������������������������" + "~n"
                       + SUBSTRING(vTmpStr,LENGTH(vTmpStr) - 9).
            END.
      END.

      WHEN 1 THEN
      DO:
         oReturn = SUBSTRING(vTmpStr,1,41).
         IF LENGTH(vTmpStr) GT 41 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "�����������������������������������������" 
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,42,41).
         IF LENGTH(vTmpStr) GT 83 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "�����������������������������������������"
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,84,41).
         IF LENGTH(person.issue) GT 125 THEN
            oReturn = oReturn + "~n" + FILL(" ",37) + "�����������������������������������������"
                              + "~n" + FILL(" ",37) + SUBSTRING(vTmpStr,126,41).
      END.

      WHEN 2 THEN
      DO:
         IF iLogTrim 
            THEN oReturn = TrimFormula(chTrim,SUBSTRING(vTmpStr,1,35)). 
            ELSE oReturn = SUBSTRING(vTmpStr,1,35).
      END.
      
      WHEN 21 THEN
      DO:
         IF iLogTrim 
            THEN oReturn = TrimFormula(chTrim,SUBSTRING(vTmpStr,36,35)). 
            ELSE oReturn = SUBSTRING(vTmpStr,36,35).
      END.
      WHEN 3 THEN
         oReturn = vTmpStr.
   
   END CASE.
END PROCEDURE.

PROCEDURE �㬂���ய:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* �������� ���� */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.
   
   IF AVAIL loan-acct THEN
   DO:
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
      RUN kau-pos.p(loan-acct.acct,
                    loan-acct.currency,
                    xdate,
                    xdate,
                    rab_str,
                    in_kau).
   
      /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
      IF RETURN-VALUE EQ "�訡��" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ksh-bal = IF loan.currency  EQ ''
                THEN (IF acct.side EQ '�'
                      THEN ksh-bal
                      ELSE ksh-bal * (-1))
                ELSE (IF acct.side EQ '�'
                      THEN ksh-val
                      ELSE ksh-val * (-1)).
   
      IF iFrml EQ "��_���2�ய" THEN
      DO:
         RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
         c1 = c1 + " " + c2.
         oReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
   
         IF NOT RetString THEN
            oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " ��. " + STRING(ksh-bal1,'99') + " ���. ".
         ELSE
            oReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
   END.
END PROCEDURE.

PROCEDURE �㬂���ய2:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* �������� ���� */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE c_date     AS DATE NO-UNDO. /*��� �� ������ ���� ����*/
   DEFINE VARIABLE vSinceDate AS DATE NO-UNDO.
   DEFINE BUFFER  xloan-cond FOR loan-cond.
   
   ASSIGN c_date = xdate
          ksh-bal = 0.0 .
   
   /* �饬 ᭠砫� �᫮��� � �� ��� ���.४����� */
   IF c_date LT loan.open-date
   THEN c_date = loan.open-date.
   FIND LAST xloan-cond WHERE xloan-cond.contract = loan.contract
      AND xloan-cond.cont-code = loan.cont-code
      AND xloan-cond.since <= c_date
      NO-LOCK NO-ERROR.
   IF NOT AVAIL xloan-cond
   THEN
   FIND FIRST xloan-cond WHERE xloan-cond.contract = loan.contract
     AND xloan-cond.cont-code = loan.cont-code
     AND xloan-cond.since > c_date
     NO-LOCK NO-ERROR.   
  IF AVAIL  xloan-cond THEN DO :
      ksh-bal =  DECIMAL(GetXattrValueEx("loan-cond",
               xloan-cond.contract + "," 
               + xloan-cond.cont-code + "," +
               string(YEAR (xloan-cond.since),"9999") + 
               string(MONTH(xloan-cond.since),"99"  ) +
               string(DAY  (xloan-cond.since),"99"  ),
               "�㬬����",?)) NO-ERROR.
     IF ERROR-STATUS:ERROR 
     THEN ASSIGN ksh-bal = 0.0 .          
  END.   
  IF NOT {assigned string(ksh-bal)}
  THEN ksh-bal = 0 .           
  IF ksh-bal = 0 AND AVAIL loan-acct THEN
   DO:
     c_date = xdate .
      /* �饬 ��� */
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
   
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
   
      ASSIGN vSinceDate = c_date.
   
      /*�饬 ��室 �� ����� �ࠧ� ��᫥ �ਢ離� ���*/
      FIND FIRST kau-entry WHERE kau-entry.acct = loan-acct.acct
                             AND kau-entry.currency = loan.currency
                             AND kau-entry.op-status >= gop-status
                             AND NOT kau-entry.debit
                             AND kau-entry.op-date >= vSinceDate
                             AND kau-entry.kau = in_kau NO-LOCK NO-ERROR.
      IF AVAILABLE kau-entry THEN
      DO:
         FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
         IF AVAIL op AND op.contract-date = c_date THEN
            vSinceDate = kau-entry.op-date.
      END.
   
      RUN kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     vSinceDate,
                     vSinceDate,
                     rab_str,
                     in_kau).
   
      /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
      IF {&RETURN_VALUE} EQ "�訡��" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ASSIGN
         ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE '�'
         ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE '�'
      .
   
  END . 

/*****/   
   
      IF iFrml EQ "��_���2�ய" THEN
      DO:
         RUN "x-amtstr.p" (ksh-bal,
                             loan-acct.currency,
                             YES,
                             YES,
                             OUTPUT c1,
                             OUTPUT c2).
         c1 = c1 + " " + c2.
         oReturn = IF logTrim THEN TrimFormula(chTrim,c1) ELSE c1.
      END.
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
   
         IF NOT RetString THEN
            oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " ��. " + STRING(ksh-bal1,'99') + " ���. ".
         ELSE
            oReturn = TRIM(STRING(ksh-bal,'>>>,>>>,>>>,>>9.99')).
      END.
/*  END. */
END PROCEDURE.

PROCEDURE ��_���:
   DEFINE INPUT  PARAMETER iFrml    AS CHARACTER   NO-UNDO. /* �������� ���� */
   DEFINE OUTPUT PARAMETER oReturn  AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE c_date     AS DATE NO-UNDO. /*��� �� ������ ���� ����*/
   DEFINE VARIABLE vSinceDate AS DATE NO-UNDO.
   
   ASSIGN c_date = xdate.
   IF AVAIL loan-acct THEN
   DO:
      /* �饬 ��� */
      FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
   
      in_kau = loan.contract + ',' + loan.cont-code + ',' + kod_ost.
   
      ASSIGN vSinceDate = c_date.
   
      /*�饬 ��室 �� ����� �ࠧ� ��᫥ �ਢ離� ���*/
      FIND FIRST kau-entry WHERE kau-entry.acct = loan-acct.acct
                             AND kau-entry.currency = loan.currency
                             AND kau-entry.op-status >= gop-status
                             AND NOT kau-entry.debit
                             AND kau-entry.op-date >= vSinceDate
                             AND kau-entry.kau = in_kau NO-LOCK NO-ERROR.
      IF AVAILABLE kau-entry THEN
      DO:
         FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
         IF AVAIL op AND op.contract-date = c_date THEN
            vSinceDate = kau-entry.op-date.
      END.
   
      RUN kau-pos.p (loan-acct.acct,
                     loan-acct.currency,
                     vSinceDate,
                     vSinceDate,
                     rab_str,
                     in_kau).
   
      /*�᫨ ���⠥� �� ������ � �㫥��� �㬬��, � ��������� �訡��, �� ���� ����*/
      IF RETURN-VALUE EQ "�訡��" THEN
         ASSIGN Xresult = -2.
   
      IF ksh-bal = 0 AND ksh-val = 0 THEN
         RUN get-kauost-trans(loan.contract,
                              loan.cont-code,
                              "*",
                              loan-acct.acct-type,
                              "",
                              "",
                              kod_ost,
                              end-date,
                              end-date,
                              gop-status,
                              OUTPUT ksh-bal).
   
      ASSIGN
         ksh-bal = ksh-bal * (-1) WHEN loan.currency EQ '' AND acct.side NE '�'
         ksh-bal = ksh-val * (-1) WHEN loan.currency NE '' AND acct.side NE '�'
      .
   
      RUN "x-amtstr.p" (ksh-bal,
                        loan-acct.currency,
                        YES,
                        YES,
                        OUTPUT c1,
                        OUTPUT c2).
      c1 = c1 + " " + c2.
   
      IF iFrml EQ "��_���" THEN
         oReturn = STRING(ksh-bal,'>>>,>>>,>>>,>>9.99') + "~n"
                 + FILL(" ",8) +  "����������������������������������������������������������������������"
                 + "~n" + c1 .
      ELSE
      DO:
         ksh-bal1 = (ksh-bal - TRUNC(ksh-bal,0)) * 100.
         oReturn = TRIM(STRING(TRUNC(ksh-bal,0),'>>>,>>>,>>>,>>9')) + " ��. " + STRING(ksh-bal1,'99') + " ���. " + "~n"
                 + FILL(" ",8) + "����������������������������������������������������������������������"
                 + "~n" + c1.
      END.
   END.
END PROCEDURE.

PROCEDURE GetAdrCode.
   DEFINE INPUT  PARAMETER iCliSurr AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAdrCode AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oAdr     AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vTmpStr AS CHARACTER   NO-UNDO.   
   
   DEF BUFFER cust-ident FOR cust-ident.

   FIND FIRST cust-ident WHERE cust-ident.Class-code     EQ "p-cust-adr"
                           AND cust-ident.cust-code-type EQ iAdrCode
                           AND cust-ident.cust-cat       EQ ENTRY(1, iCliSurr)
                           AND cust-ident.cust-id        EQ INT64(ENTRY(2, iCliSurr))
                           AND (   cust-ident.close-date EQ ?
                                OR cust-ident.close-date GE xDate)
      NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN
   DO:
      vTmpStr = GetAdrStr(cust-ident.issue, "������").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "ࠩ��").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "��த").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "�㭪�").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "㫨�").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "���").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "��஥���").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "�����").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
      vTmpStr = GetAdrStr(cust-ident.issue, "������").
      IF {assigned vTmpStr} THEN
         oAdr = oAdr + ", " + vTmpStr.
   END.      

END PROCEDURE.

PROCEDURE GetPersType.
   DEFINE INPUT  PARAMETER iLoanRID    AS RECID       NO-UNDO.   
   DEFINE INPUT  PARAMETER iPersType   AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPersSurr   AS CHARACTER   NO-UNDO.

   DEF BUFFER loan      FOR loan.
   DEF BUFFER cust-role FOR cust-role.   

   FOR FIRST loan WHERE RECID(loan) EQ iLoanRID
      NO-LOCK,
       FIRST cust-role WHERE cust-role.file-name  EQ "loan"
                         AND cust-role.surrogate  EQ loan.contract + "," + loan.cont-code
                         AND cust-role.class-code EQ iPersType
                         AND cust-role.cust-cat   EQ "�"
      NO-LOCK:

      oPersSurr = "�," + cust-role.cust-id.
   END.


END PROCEDURE. 

PROCEDURE GetDocSotr.
   DEFINE INPUT  PARAMETER iCommand  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oReturn   AS CHARACTER   NO-UNDO.
   
   DEF VAR vCode    AS CHAR   NO-UNDO.
   DEF VAR vBegDate AS DATE   NO-UNDO.
   DEF VAR vEndDate AS DATE   NO-UNDO.
   DEF VAR vHandl   AS HANDLE NO-UNDO.
   DEF VAR vCheck   AS LOGIC  INIT  YES  NO-UNDO.
   
   DEF BUFFER b1_code FOR code.

   vHandl = session:last-procedure.
   DO WHILE VALID-HANDLE(vHandl):
      IF vHandl:file-name EQ 'l-sotr.p' THEN
         LEAVE.
      vHandl = vHandl:prev-sibling .
   END.

   IF VALID-HANDLE(vHandl) THEN
      vCode = vHandl:private-data.
   MAIN:
   DO 
      ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN:
     
      IF NOT ({assigned vCode}) THEN
      DO TRANSACTION: 
         pick-value = "".
         RUN browseld.p ('����_���',
            'class' + CHR(1) + 'parent' + CHR(1) + 'misc[1]',
            '����_���' + CHR(1) + '����_���'
             + CHR(1) + GetXAttrValue("_user",USERID('bisquit'),"�⤥�����"),
            '', 4).
         IF {assigned pick-value} THEN
            vCode = pick-value.
         ELSE DO:
            IF VALID-HANDLE(vHandl) THEN
            vHandl:private-data = "esc".
            oReturn = "esc".
         END.
      END.
      ELSE  /* ��࠭����� ���祭�� ����஫�஢��� �� �㤥� */
            vCheck = NO.                                

      IF {assigned vCode} THEN
      DO:
         FIND FIRST _User WHERE _User._userid EQ vCode
         NO-LOCK NO-ERROR.
         IF AVAIL _User THEN
         DO:
            FIND FIRST b1_code WHERE b1_code.class  EQ "����_���"
                                 AND b1_code.parent EQ "����_���"
                                 AND b1_code.code   EQ vCode
               NO-LOCK NO-ERROR.
            IF AVAIL b1_code THEN
            DO:
               ASSIGN
                  vBegDate = DATE(b1_code.misc[5])
                  vEndDate = DATE(b1_code.misc[6])
                  NO-ERROR.

               IF vCheck THEN
               DO:  
                  IF vBegDate GT gend-date THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess ("","",4,
                        "�� ����㯨� �ப �������稩.~n����� ��㣮� 㯮�����祭��� ���?" ).
                     IF pick-value EQ "yes" THEN
                     DO:
                        vCode = "".
                        UNDO MAIN, RETRY MAIN.
                     END.   
                  END.
                  IF vEndDate LT gend-date THEN
                  DO:
                     RUN Fill-SysMes IN h_tmess ("","",4,
                        "��⥪ �ப �������稩.~n����� ��㣮� 㯮�����祭��� ���?" ).
                     IF pick-value EQ "yes" THEN
                     DO:
                        vCode = "".
                        UNDO MAIN, RETRY MAIN.
                     END.   
                  END.
               END.

               IF VALID-HANDLE(vHandl) THEN
                  vHandl:private-data = vCode.

               CASE iCommand:
                  WHEN "��������" THEN
                     oReturn = b1_code.name.
                  WHEN "�������殐�" THEN
                     oReturn = b1_code.description[3].
                  WHEN "�������������" THEN
                     oReturn = b1_code.misc[3].
                  WHEN "��������������" THEN
                     oReturn = b1_code.val.
                  WHEN "��⠄����������" THEN
                     oReturn = STRING(vBegDate,"99/99/9999").
                  WHEN "�᭮���������" THEN
                     oReturn = b1_code.misc[4].
                  WHEN "������������栐�" THEN
                     oReturn = b1_code.misc[7].
               END CASE.
            END.
         END.
      END.
   END.        
END PROCEDURE.
{intrface.del}
