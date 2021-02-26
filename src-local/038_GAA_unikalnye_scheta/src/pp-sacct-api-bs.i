/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-SACCT-API-BS.I
      Comment: ������⥪� ��楤�� API �㦡� "��楢� ���" - BS (������-������)
   Parameters:
         Uses:
      Used by:
      Created: 18.10.2004 16:22 MDY �� pp-acct.p      
     Modified: 25.11.2004 12:55 KSV      (0038814) ��������� ᨬ����筠�
                                         ��楤�� AcctFree, �ᮢ��������
                                         ����� ��१�ࢨ஢������ ��� �
                                         ������� ��楤��� AcctKeep.
     Modified: 07.12.2004 11:45 KSV      (0038814) ��楤��� ������樨 ���
                                         ��७�ᥭ� �� pp-acct.p
     Modified: 25.01.2005 13:39 KSV      (0038814) ����஭���஢��� � 4.1D
                                         Costing.
     Modified: 26.01.2005 17:31 KSV      (0038814) ��ࠢ���� ��楤��
                                         Check-Acct-Mask ��� ࠡ��� � ��᪠��,
                                         ������묨 � �����䨪���.
     Modified: 11.02.2005 12:50 MDY      0042688 - ��ࠢ���� ��.�訡�� - 2 ��㪨
     Modified: 15.02.2005 13:20 KSV      (0042889) ��ࠢ���� �訡�� �
                                         Check-Acct-Mask ��� ������᪮��
                                         �����䨪��� � ��᪠�� ��⮢.
     Modified: 18.02.2005 10:39 KSV      (0043089) �஢�ઠ ��� ��
                                         ᮮ⢥��⢨� ��᪥ �᪫�祭� ��
                                         Check-Acct.
     Modified: 04.03.2005 Om  �訡��.
                        ��� ���樠����樨 ��६����� otdel.i ����室��� ����稥
                        ��ॣ����஢������ ���짮��⥫� � ��⥬�.
     Modified: 14.05.2005 Om  ��ࠡ�⪠.
                        ��������� ��楤�� GetNameAcctExCorp - ��� ��⮢ ��. ���
                        �ନ��� ������������ �� ���� cust-corp.name-short.
     Modified: 14.05.2005 SADM  �訡��.
                        ��楤�� Check-Acct-Key , ������� �⢥� ��-㬮�砭��
                        �� ������ ���� - � "��⠢���" �� "�� ��⠢����".
                        ��������� ��ࠡ�⪠ ⨯� ᮮ�饭�� "�訡��" ���
                        ���������� ����饭�� ����� ��� � ������ ���祬.
     Modified: 25.07.2005 16:16 MDY      37923
     Modified: 10.08.2005 15:04 KSV      (0037923) ��������� ���樠������
                                         ४����� acct.kau-id �� ����奬�.
     Modified: 19/01/99 Serge ��楤�� GetAcctMask ��뢠���� �� Acct-Cr,
                              �᫨ xattrid �� ������, � ��뢠���� XattrAll.
     Modified: 12/02/99 Lera  �訡�� �� ᮧ����� ����� ��� ����� ������� � ������ ��楤�� � ��ࠢ��� � ��뢠����, � ⠬
                              �� ��ࠡ����. ��� �⮣� ���� 㪠���� &ONerror - ��� ��६����� ���� �訡�� (input-output ��ࠬ���).
                              Return-value �� �⮬ "error".
     Modified: 17/01/2003 kraw (13482) 㢥��祭�� ����� ���ᨢ� cnt-srt
     Modified: 03.03.2003 17:55 KSV      (0012920) ��������� �������⥫쭠�
                                         ��ࠬ��ਧ��� ��⮤� �뤥����� ���稪�
                                         �१ ��堭��� SysConf.
                                         �������� ��孠���, ��������騩
                                         �ᯮ�짮���� � ���뢠���� ���
                                         ��।������� ���祭�� ���稪�. ��
                                         ��楤��� Acct-CR, SetDefinedCntParam �
                                         GetDefinedCounter.
                                         ������祭 ��堭��� ���樠����樨 ���.
                                         ४����⮢. ��. ������ BAL2ACCT.I �
                                         ��楤��� Acct-CR
                                         ��������� ��ࠬ��ਧ��� ����⠭�,
                                         �室��� � ���� ���. ��. ������
                                         ACCTTOK.DEF � ��楤��� Acct-CR.
                                         �ᯮ�짮����� ��⮤� XATTRALL ���
                                         ���樠����樨 ४����⮢ ᮧ���������
                                         ��� �������� �� GetXAttrInit. ��.
                                         ��楤��� Acct-Cr � GetAcctMask.
     Modified: 04.03.2003 14:03 KSV      (0012920) � ��ࠬ���� ��⮤� ���稪�
                                         ������� ����� ���樨 ������ ���,
                                         ����� ����� ������ �� ����砥��
                                         ���稪.
     Modified: 04.03.2003 15:45 KSV      (0012920) ��������� �९����୮�
                                         �몫�祭�� �맮�� ��楤���
                                         BalToAcct_Xattr. ��. ��楤���
                                         Acct-CR.
     Modified: 16.04.2003 11:39 KSV      (0012920) � ��楤��� Acct-Cr �
                                         ��ࠡ��稪 ⮪���� TOK_USR, ���������
                                         ��।�� ��ࠬ��� � ��ନ஢����
                                         ����஬ ���.
     Modified: 16.04.2003 15:47 KSV      (0012920) � ��楤��� Acct-Cr
                                         ��������� ������������ �㬥�樨
                                         ��⮢ � �ந����쭮�� �����. �����,
                                         � ���ண� ��稭����� �㬥���
                                         �������� �� ४����� ����� ���
                                         ������稪.
     Modified: 21.06.2003       AVAL     (0013453) � ��楤�� Acct-Cr ���������
                                         ��ࠡ�⪠ ⮪��� "�" ��� ����
                                         ��室��/���室��.
     Modified: 02.06.2005       kraw (0047326)   branch-list = otdel-list � SetOtdelVariables
     Modified: 10.08.2006 Om  ��ࠡ�⪠.
                        1. � ��楤��� Check-Acct �������� �맮� ��⮤� CHKUPD.
                        2. �������� �������� ��� ��६�����. ��� ᤥ���� SHARED.
                           ��稭� � ॠ����樨 ��⮤�� CHKUPD.
     Modified: 29.10.2007 muta 0082120  ��������� ��楤�� GetAcctQty - ������ GetAcctCur.
                               ��� ���⮪ �� acct-qty (���⮪ ��室���� � ���� qty)
     Modified: 05.04.2011 Malik 0148047  
                                1. � ��楤�� CreateAcctNumber  㪠����� STRING(key,"9") 
                                   �������� �� STRING(key, FILL ("9", toklen[{&TOK_KEY_IDX}])).
                                2. � ��楤�� Check-Acct ��ப�  
                                  SUBSTRING(STRING(acct.acct),1,5) 
                                  �������� �� 
                                  SUBSTRING(STRING(acct.acct),INDEX(tokacct,{&TOK_BAL}),toklen[{&TOK_BAL_IDX}])
                                  ��� �� ��ப� SUBSTRING(acct.acct,6,3) NE acct.currency
                                  �������� �� 
                                  SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) NE acct.currency        
                                3. ��������� ��楤�� FillAcctMask ��� �ᯮ�짮����� � Check-Acct-Key
     Modified: 29/07/2011 Mixey: (0152425)  1. ��ࠢ�� ��楤��� FillAcctMask: 
                                            ������� ���������� ��᪨ ��� ��� �����䨪��� '��᪨��⮢'.
                                            2. ��ࠢ�� ��楤��� Check-acct:  
                                            ��ࠫ �஢��� ��� �����䨪��� '��᪨��⮢'.
                                            �஢��塞 �����ᮢ� ��� � ᮮ⢥��⢨� � ��᪮� �� �� �᪫���.
     Modified: 31.10.2012 Malik (0183446) � ��楤�� Check-Acct-Key ��������� �᫮���, ��� �᪫�祭�� ⠪�� �訡��:
                                ** Starting position for SUBSTRING, OVERLAY, etc. must be 1 or greater.                                            
                                
*/

{accttok.def}           /* ������� ����⠭� �ᯮ��㥬�� � ��᪥. */
{get_dob.i}             /* ��� ��᫥����� �����⮣� ��. */
{workplan.def} 

PROCEDURE SetOtdelVariables PRIVATE.
                        /* �.�. ���� ShFilial ��।������ ⮫쪮 ��᫥
                        ** ������, � ��室���� ����� �᪠�� */
   dob = fGetLastClsDate (?,"*").
                        /* �᫨ ��� ��᫥����� �����⮣� �� �� ��।�����,
                        ** � ���樠�����㥬 ⥪�饩 ��⮩.
                        ** (TODAY - 1) ����室��, �.�. nextdob.i ����᫮���
                        ** �ਡ����� ���� ���� ���।. */
   IF dob =  ?
      THEN dob = (TODAY - 1).
                        /* ���� ���। ��ࢮ�� ࠡ�祣� ��
                        ** �� ���� dob. */
   {nextdob.i}

   RUN ACGetKeyProg(vclass, OUTPUT keyprog).
   ASSIGN
      no-key      = FGetSetting ( "���砍��", ?, "" )
      my-key      = FGetSetting ( "����", ?, "" )
      local-cur   = FGetSetting ( "�����悠�", ?, "{&in-NC-Code}" )
      depobal     = (FGetSetting("depo-bal",?,?) = "��").
   .

   RETURN.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ������� �� ����奬� ��⮤, ������騩 ����
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE ACGetKeyProg:
   DEFINE INPUT  PARAMETER vclass  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER keyprog AS CHARACTER  NO-UNDO.
   keyprog = GET-CLASS-METHOD(vclass, "U1").
   if keyprog = ? then keyprog = "key-tst".
END PROCEDURE.

PROCEDURE Acct-Cr.
   DEF INPUT         PARAM       in-bal-acct LIKE acct.bal-acct   NO-UNDO.
   DEF INPUT         PARAM       in-currency LIKE acct.currency   NO-UNDO.
   DEF INPUT-OUTPUT  PARAMETER   vAcct       LIKE acct.acct       NO-UNDO.
   DEFINE PARAMETER BUFFER acct FOR acct.

   DEFINE VARIABLE ac      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRet    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vErrMsg AS CHARACTER   NO-UNDO.

   RUN CreateAcctNumber (vClass,
                         in-bal-acct,
                         in-currency,
                         vAcct-Cat,
                         acctmask,
                         acct.branch-id,
                         acct.cust-cat,
                         acct.cust-id,
                         mSymbPU,
                         acct.acct,
                         OUTPUT ac,
                         OUTPUT vErrMsg).

   vAcct = ac.
   vRet = RETURN-VALUE.
   IF vErrMsg <> "" THEN
      RUN Fill-SysMes IN h_tmess (ENTRY(1,vErrMsg,"|"),
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 2
                                    THEN ENTRY(2,vErrMsg,"|")
                                    ELSE "",
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 3
                                    THEN ENTRY(3,vErrMsg,"|")
                                    ELSE "-1",
                                  IF NUM-ENTRIES(vErrMsg, "|") >= 4
                                    THEN ENTRY(4,vErrMsg,"|")
                                    ELSE "�訡�� �����樨 ����� ���").
   RETURN vRet.
END PROCEDURE.

/*-------Commented by Malik----------------------------------------------------
  Purpose:     ��楤�� ���������� ��६����� �� ��᪥ ���
  Parameters:  iMask - ��᪠
               
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE FillAcctMask:
   DEFINE INPUT  PARAMETER iMask     AS CHARACTER   NO-UNDO.

   DEF VAR  sTokidx     AS CHAR    NO-UNDO.
   DEF VAR  sToklen     AS CHAR    NO-UNDO.
   DEF VAR i            AS INT64     NO-UNDO.
   /* Commented by Mixey:  29/07/2011 (0152425) ������� ���������� ��᪨ ���
                                                ��� �����䨪��� '��᪨��⮢' */ 
   IF CAN-FIND(FIRST code WHERE code.class =  "" AND code.code =  iMask) THEN iMask = acctmask.
   RUN GetAcctMask (INPUT iMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.      
   IF ERROR-STATUS:ERROR THEN DO:
      RETURN.
   END. 

   DO i = 1 TO EXTENT(tokidx):
       tokidx[i] = INT64(ENTRY(i,sTokidx)).
       IF i <= EXTENT(toklen) THEN DO:
           toklen[i]= INT64(ENTRY(i,sToklen)).
       END.
   END.
END PROCEDURE.

/*
   ��楤�� �।�����祭� ��� ������� ������⢠ ᢮������ ���祭�� ����稪�
   �� ����⨨ ����. ��ࠬ����:
    - iAcct     - 䠪��᪨� �����, ����� ����稫 ���� ��᫥ ����⠭���� ���
                  ᨬ����� ��᪨;
    - iCurrency - ��� ������;
    - iAcctCat  - ��⥣��� ����;
    - iBalAcct  - ���� 2-�� ���浪�;
    - iClass    - ����� ���뢠����� ���� �� ����奬�;
    - iMask     - ��室��� ��᪠, � ���ன ���뢠��� ����.
   �����묨 ������� � ���祭�� ����稪�, ����� ���� 㦥 ������⢮���� �
   ��������� ���� � ⠪��� ४����⠬�, ���� �ᯮ�짮���� � �������
   �����䨪��� ��⠐���ࢠ ��� ��⮢, ������� �� ������ ��᪥.
   ���४⭮��� ���祢��� ࠧ�鸞 � ������� �����䨪��� ��⠐���ࢠ ��
   �஢������ �� ᮮ�ࠦ���� �ந�����⥫쭮��.
   �����頥��� �᫮ ᢮������ ���祭�� ����稪� (0, �᫨ ᢮������ ���), ����
   -1 � ��砥 �訡�� � ��ࠬ����.
*/
PROCEDURE CalcNumFreeCounters PRIVATE:
   DEFINE INPUT  PARAMETER iAcct     LIKE acct.acct       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency LIKE acct.currency   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat  LIKE acct.acct-cat   NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct  LIKE acct.bal-acct   NO-UNDO.
   DEFINE INPUT  PARAMETER iClass    LIKE acct.class-code NO-UNDO.
   DEFINE INPUT  PARAMETER iMask     AS   CHARACTER       NO-UNDO.
   DEFINE OUTPUT PARAMETER oNumFree  AS   INT64           NO-UNDO INITIAL -1.

   DEFINE BUFFER acct FOR acct.
   DEFINE BUFFER code FOR code.

   DEFINE VARIABLE vI     AS INT64     NO-UNDO.
   DEFINE VARIABLE vN     AS INT64     NO-UNDO INITIAL 1.
   DEFINE VARIABLE vIsCnt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vC     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vS     AS CHARACTER NO-UNDO.

   DO vI = MINIMUM(LENGTH(iAcct), LENGTH(iMask)) TO 1 BY -1:
      ASSIGN
         vC     = SUBSTRING(iMask, vI, 1)
         vIsCnt = (vC = {&TOK_CNT})
         vN     = vN * 10
                  WHEN vIsCnt
         vS     = SUBSTITUTE("&1&2",
                             IF vIsCnt OR vC = {&TOK_KEY}
                             THEN "."
                             ELSE SUBSTRING(iAcct, vI, 1),
                             vS)
      .
   END.
   FOR EACH acct WHERE
      acct.class-code = iClass    AND
      acct.currency   = iCurrency AND
      acct.acct-cat   = iAcctCat  AND
      acct.bal-acct   = iBalAcct  AND
      CAN-DO(vS, acct.acct)
   NO-LOCK:
      ACCUMULATE acct.acct (COUNT).
   END.
   FOR EACH code WHERE
      code.class  = "��⠐���ࢠ" AND
      code.parent = "��⠐���ࢠ" AND
      CAN-DO(vS, code.code)
   NO-LOCK:
      ACCUMULATE code.code (COUNT).
   END.
   oNumFree = vN - (ACCUM COUNT acct.acct) - (ACCUM COUNT code.code).
END PROCEDURE.

PROCEDURE CreateAcctNumber.
   DEFINE INPUT  PARAMETER iClass    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBal      AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iMask     AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBranchId AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat  AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId   AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iSymPU    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct     AS CHARACTER   NO-UNDO.

   DEFINE OUTPUT PARAMETER oAcct     AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oErrMsg   AS CHARACTER   NO-UNDO.

   DEF BUFFER xac FOR acct.

   DEF VAR ac           LIKE acct.acct NO-UNDO.
   DEF VAR jj           AS INT64     NO-UNDO.
   DEF VAR i            AS INT64     NO-UNDO.
   DEF VAR j            AS INT64     NO-UNDO.
   DEF VAR fnd-acct     AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR cnt-st       AS CHAR    NO-UNDO.
   DEF VAR ch           AS CHAR    NO-UNDO.
   DEF VAR cnt-srt      AS INT64     NO-UNDO EXTENT 11.
   DEF VAR vMinCnt      AS INT64     NO-UNDO.
   DEF VAR U2-proc      AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR U3-proc      AS CHAR    NO-UNDO INITIAL ?.
   DEF VAR vIntBranch   AS INT64     NO-UNDO.
   DEF VAR  sTokidx     AS CHAR    NO-UNDO.
   DEF VAR  sToklen     AS CHAR    NO-UNDO.
   DEF VAR tmp-parr     AS CHAR    NO-UNDO.
   DEF VAR vFullacct    AS CHAR    NO-UNDO.
   DEF VAR cnstacct     AS CHAR    NO-UNDO.
   DEF VAR vCntAcct     AS CHAR    NO-UNDO. /* ���浪��� ����� ���稪� � ����祭��� ���. */
   DEF VAR vPosAcct     AS INT64     NO-UNDO. /* ������ �� ���� � ���稪�. */
   DEF VAR vFlagUnk     AS LOGICAL NO-UNDO.
   DEF VAR vCnstIgnor   AS CHAR    NO-UNDO.
   DEF VAR vNumFreeCnt  AS INT64   NO-UNDO INITIAL ?.
/* ��⠢�� ���� ���� */
   DEF VAR mHandle      AS INT64   NO-UNDO.
   DEF VAR mac11        AS CHAR    NO-UNDO.
   DEFINE VARIABLE iStat    AS INT64     NO-UNDO.
/* ����� ��⠢�� ���� ���� */

   ASSIGN
      tmp-parr = GetSysConf ("AcctONcreate-acct")
      U2-proc  = GET-CLASS-METHOD(iClass, "U2")
   .
   IF       U2-proc <> ?
      AND   NOT SearchPfile (U2-proc)
   THEN DO:
      oErrMsg = "|acct04||%s=" + U2-proc.
      RETURN.
   END.
   U3-proc = GET-CLASS-METHOD(iClass, "U3").
   IF U3-proc <> ?
      AND NOT SearchPfile (U3-proc)
   THEN DO:
      oErrMsg = "|acct05||%s=" + U3-proc.
      RETURN.
   END.

   RUN GetAcctMask (INPUT iMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      oErrMsg = "||-1|" + GetErrMsg().
      RETURN.
   END.

   DO i = 1 TO EXTENT(tokidx):
       tokidx[i] = INT64(ENTRY(i,sTokidx)).
       IF i <= EXTENT(toklen) THEN DO:
           toklen[i]= INT64(ENTRY(i,sToklen)).
       END.
   END.
   ac = FILL("0", LENGTH(tokacct)).
   DO i = 1 TO LENGTH(tokacct):
      ch = SUBSTR(tokacct,i,1).
      IF  ch >= "0"
      AND ch <= "9"
         THEN OVERLAY(ac,i,1) = ch.
   END.

   FIND bal-acct WHERE bal-acct.bal-acct = iBal NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct THEN DO:
      oErrMsg = "||0|" + ERROR-STATUS:GET-MESSAGE(1).
      RETURN.
   END.
   iCurr = IF iCurr = ?
           THEN (IF bal-acct.foreign
                 THEN ?
                 ELSE "")
           ELSE IF  iCurr     = local-cur
                AND iAcctCat <> "d"
                THEN ""
                ELSE iCurr.
   jj = 0.

   /* Commented by KSV: �᫨ �� ����� ����� �������쭮 �������� ���稪,
   ** � ��稭��� �㬥��� � ����. � �� ��� �㬥������ � 1  */
   vMinCnt = INT64(GetXAttrInit(iClass,"������稪")) NO-ERROR.
   IF vMinCnt = ? THEN vMinCnt = 0.

   vCnstIgnor = {&TOK_CNT} + "," + {&TOK_KEY} + "," + {&TOK_ANY}.

   NEXTACCT:
   REPEAT
   ON ERROR  UNDO NEXTACCT, LEAVE NEXTACCT
   ON ENDKEY UNDO NEXTACCT, LEAVE NEXTACCT:
      /* ��� 2-�� ���浪� */
      IF toklen[{&TOK_BAL_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_BAL},
                           STRING(iBal,FILL("9",MAXIMUM(toklen[{&TOK_BAL_IDX}],LENGTH(STRING(iBal)))))).

      /* ����� */
      IF toklen[{&TOK_CUR_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CUR},
                           IF  iCurr <> ""
                           AND iCurr <> local-cur
                           AND iCurr <> ?
                              THEN iCurr
                              ELSE local-cur).

      /* 䨫��� */
      IF toklen[{&TOK_BRANCH_IDX}] > 0 THEN DO:
         cnt-st = iBranchId.
         IF cnt-st = ""
         OR cnt-st = ?
            THEN cnt-st = GetThisUserOtdel().
         vIntBranch = INT64(cnt-st) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            oErrMsg = "|acct06||".
            RETURN "exit".
         END.
         RUN SetAcct (INPUT-OUTPUT ac, {&TOK_BRANCH},
                      STRING(vIntBranch,FILL("9",MAXIMUM(toklen[{&TOK_BRANCH_IDX}],LENGTH(cnt-st))))).
      END.

      /* ��� ������ */
      IF toklen[{&TOK_CLIENT_IDX}] > 0 THEN DO:
         IF iCustCat <> "�"
            THEN cnt-st = STRING(iCustId).
            ELSE cnt-st = "".
         IF  cnt-st <> ""
         AND cnt-st <> ?
         AND cnt-st <> "0"
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CLIENT}, cnt-st).
      END.

      /* ��� */
      IF toklen[{&TOK_UNKCLI_IDX}] > 0 THEN DO:
         IF iCustCat <> "�" THEN DO:
            {getflagunk.i &only-np=YES &flag-unk="vFlagUnk"}
            IF vFlagUnk = NO THEN DO:
               oErrMsg = "|acct44||".
               RETURN "EXIT".
            END.
            cnt-st = GetUNK(iCustCat, iCustId).
         END.
         ELSE cnt-st = "".
         IF  cnt-st <> ""
         AND cnt-st <> ?
         AND cnt-st <> "0"
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_UNKCLI}, cnt-st).
      END.

      /* �� ᨬ��� */
      IF toklen[{&TOK_ANY_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_ANY}, FILL(" ",toklen[{&TOK_ANY_IDX}])).

      /* ����� ��室�� */
      IF toklen[{&TOK_PU_IDX}] > 0
         THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_PU}, FILL(iSymPU,toklen[{&TOK_PU_IDX}])).
  
      /* ���稪 */
      IF toklen[{&TOK_CNT_IDX}] > 0 THEN DO:
         /* �஢��塞, ���� �� ��।������ ���稪,
         ** � ����� �� ��� �ᯮ�짮����. */
         RUN GetDefinedCounter(iBal, iCurr, ac, tokacct, OUTPUT fnd-acct).
         /* �᫨ �ந��諠 "�ࠢ�塞��" �訡��,
         ** � �४�頥� ����⨥ ���. */
         IF RETURN-VALUE = "ERROR"
            THEN RETURN "EXIT".
         /* �᫨ ���稪 �� ��।����, �... */
         IF fnd-acct = ? THEN DO:
            cnt-st = "".
            IF U2-proc <> ? THEN DO:
               tmp-branch-id = iBranchId.
               /* ��ࠬ��ਧ㥬 ��⮤ ����祭�� ���稪�  */
               RUN SetSysConf IN h_base ("AcctAc",ac).
               RUN SetSysConf IN h_base ("AcctCustCat",iCustCat).
               RUN SetSysConf IN h_base ("AcctCustID",STRING(iCustId)).
               RUN SetSysConf IN h_base ("AcctMsk",iMask).
               RUN SetSysConf IN h_base ("AcctInc",STRING(jj)).
               /* �롮� ��⮤� �� �室�� ��ࠬ��ࠬ. */
               IF  tmp-parr <> ""
               AND tmp-parr <> ?
                  THEN RUN VALUE (U2-proc + ".p") (tmp-parr,        OUTPUT fnd-acct).
                  ELSE RUN VALUE (U2-proc + ".p") (iBal, iCurr, "", OUTPUT fnd-acct).
               /* ����뢠�� ��ࠬ���� ���稪� */
               RUN DeleteOldDataProtocol IN h_base ("AcctAc").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustCat").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustID").
               RUN DeleteOldDataProtocol IN h_base ("AcctMsk").
               RUN DeleteOldDataProtocol IN h_base ("AcctInc").
               /* �᫨ ���稪 �� ��।���� � 㪠��� ��⮤ U3,
               ** � �஡㥬 ��� ��������. */
               RUN SetSysConf IN h_base ("AcctAc",ac).
               RUN SetSysConf IN h_base ("AcctCustCat",iCustCat).
               RUN SetSysConf IN h_base ("AcctCustID",STRING(iCustId)).
               RUN SetSysConf IN h_base ("AcctMsk",iMask).
               RUN SetSysConf IN h_base ("AcctInc",STRING(jj)).
               IF  fnd-acct  = ?
               AND U3-proc  <> ? THEN DO:
                  RUN VALUE (U3-proc + ".p") (iBal, iCurr, iClass).
                  RUN VALUE (U2-proc + ".p") (iBal, iCurr, "", OUTPUT fnd-acct).
               END.
               RUN DeleteOldDataProtocol IN h_base ("AcctAc").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustCat").
               RUN DeleteOldDataProtocol IN h_base ("AcctCustID").
               RUN DeleteOldDataProtocol IN h_base ("AcctMsk").
               RUN DeleteOldDataProtocol IN h_base ("AcctInc").
               /* �᫨ ���稪 �� ��।����, � ��室�� � �訡���. */
               IF fnd-acct = ? THEN DO:
                  oErrMsg = "|acct07||".

                  dvptoolhelper:WriteDebug('�訡�� �����樨 ����� ���!', -1).
                  dvptoolhelper:WriteDebug('��� �訡��: acct07', -1).
                  dvptoolhelper:WriteDebug('����� ���: ' + iClass, -1).
                  dvptoolhelper:WriteDebug('��᪠ ���: ' + iMask, -1).
                  dvptoolhelper:WriteDebug('��⮤ U2: ' + U2-proc, -1).
                  dvptoolhelper:WriteDebug('��⮤ U3: ' + U3-proc, -1).
                  dvptoolhelper:WriteDebug('���⨯ ���: ' + ac, -1).

                  RETURN "EXIT".
               END.
            END.
            ELSE DO:
               ASSIGN
                  cnstacct = ac
                  j        = 0
                  cnt-srt  = 100
               .
               DO i = 1 TO LENGTH(ac):
                  IF LOOKUP(SUBSTR(tokacct,i,1), vCnstIgnor) > 0
                     THEN OVERLAY(cnstacct,i,1) = ".".
                  IF SUBSTR(tokacct,i,1) = {&TOK_CNT}
                     THEN ASSIGN
                        j           = j + 1
                        cnt-srt[j]  = i
                     .
               END.

               vfullacct = AddFilToAcct(iAcct,shFilial).
               FOR EACH xac WHERE xac.bal-acct = iBal
                              AND xac.currency = iCurr
                              AND xac.acct    <> ?
                              AND xac.acct    <> vFullAcct
                              AND xac.filial-id =  shFilial
                              AND CAN-DO(cnstacct, xac.number)
                  NO-LOCK
                  BY SUBSTR (xac.acct,cnt-srt[1],1) +
                     SUBSTR (xac.acct,cnt-srt[2],1) +
                     SUBSTR (xac.acct,cnt-srt[3],1) +
                     SUBSTR (xac.acct,cnt-srt[4],1) +
                     SUBSTR (xac.acct,cnt-srt[5],1) +
                     SUBSTR (xac.acct,cnt-srt[6],1) +
                     SUBSTR (xac.acct,cnt-srt[7],1) +
                     SUBSTR (xac.acct,cnt-srt[8],1) +
                     SUBSTR (xac.acct,cnt-srt[9],1) +
                     SUBSTR (xac.acct,cnt-srt[10],1) +
                     SUBSTR (xac.acct,cnt-srt[11],1)
                  DESCENDING:
                  LEAVE.
               END.
            END.
         END. /* fnd-acct = ? */

         q = DECIMAL(fnd-acct) NO-ERROR.
         IF fnd-acct <> ?
            THEN cnt-st = (IF ERROR-STATUS:ERROR
                           OR toklen[{&TOK_CNT_IDX}] < LENGTH(fnd-acct)
                              THEN fnd-acct
                              ELSE STRING(IF q = 0
                                             THEN MAXIMUM(vMinCnt,q)
                                             ELSE q,
                                          FILL("9",toklen[{&TOK_CNT_IDX}]))
                          ) NO-ERROR.
         ELSE DO:
            IF AVAIL xac THEN
            DO:
               vCntAcct = "".  
               /* �뤥�塞 �� ��� ����樨 �⭮��騥�� � ���稪�. */
               DO i=1 TO LENGTH(xac.number):
                   IF SUBSTRING(tokacct,i,1) =  {&TOK_CNT} THEN
                      vCntAcct = vCntAcct + SUBSTRING(xac.number,i,1). 
               END. 
               /* ��⠥��� �८�ࠧ����� � ���. */
               DECIMAL (vCntAcct) NO-ERROR.
               /* � ���稪� ���� ᨬ���� �⫨�� �� ���.
               ** �����塞 ᨬ���� �� ���� "0". */
               IF ERROR-STATUS:ERROR THEN
               DO vPosAcct = 1 TO LENGTH(vCntAcct):
                  IF SUBSTR(vCntAcct, vPosAcct, 1) < "0"
                  OR SUBSTR(vCntAcct, vPosAcct, 1) > "9"
                     THEN SUBSTR(vCntAcct, vPosAcct, 1) = "0".
               END.
            END.
            /* ����砥� ����� ���祭�� ���稪�. */
            cnt-st = STRING((IF AVAIL xac
                                THEN MAXIMUM(DECIMAL(vCntAcct) + 1,vMinCnt)
                                ELSE vMinCnt
                            ) + jj, FILL ("9", toklen[{&TOK_CNT_IDX}])
                           ) NO-ERROR.
         END.
                        /* �᫨ ���稪 �� ��᢮���� � �⪠�뢠�� ᮧ����� ���. */
         IF ERROR-STATUS:GET-MESSAGE(1) > "" THEN DO:
            oErrMsg = "|acct07||".

            dvptoolhelper:WriteDebug('�訡�� �����樨 ����� ���!', -1).
            dvptoolhelper:WriteDebug('��� �訡��: acct07', -1).
            dvptoolhelper:WriteDebug('����� ���: ' + iClass, -1).
            dvptoolhelper:WriteDebug('��᪠ ���: ' + iMask, -1).
            dvptoolhelper:WriteDebug('��⮤ U2: ' + U2-proc, -1).
            dvptoolhelper:WriteDebug('��⮤ U3: ' + U3-proc, -1).
            dvptoolhelper:WriteDebug('���⨯ ���: ' + ac, -1).

            RETURN "EXIT".
         END.

         RUN SetAcct (INPUT-OUTPUT ac, {&TOK_CNT}, cnt-st).
      END.

      /* ���� */
      IF  toklen[{&TOK_KEY_IDX}] <> 0
      AND my-key <> ?
      AND NOT CAN-DO(no-key, STRING(bal-acct.bal-acct)) THEN DO:
         IF NOT SearchPfile (keyprog) THEN DO:
            oErrMsg = "|acct08||%s=" + keyprog.
            RETURN.
         END.
         RUN VALUE(keyprog + ".p") (ac, my-key, output key).
         IF key <> ?
            THEN RUN SetAcct (INPUT-OUTPUT ac, {&TOK_KEY}, STRING(key, FILL ("9", toklen[{&TOK_KEY_IDX}]))).
      END.

      /* ᨬ��� ����祭�� �� ��楤��� �ନ஢���� ����� ��楢��� ��� */
      IF toklen[{&TOK_USR_IDX}] > 0 THEN DO:
         IF U2-proc <> ? THEN DO:
            /* Commented by KSV: ��।��� � ��楤��� �����樨 ���
            ** ����� 㦥 ��ନ஢������ ��� */
            RUN SetSysConf IN h_base ("AcctNumber",ac).

            IF  tmp-parr <> ""
            AND tmp-parr <> ?
               THEN RUN VALUE(U2-proc + ".p") (tmp-parr, OUTPUT fnd-acct).
               ELSE RUN Value(U2-proc + ".p") (iBal,iCurr,"", OUTPUT fnd-acct).

            /* Commented by KSV: ����塞 �������� ��ࠬ��� */
            RUN DeleteOldDataProtocol IN h_base ("AcctNumber").

            IF fnd-acct = ? THEN DO:
              RUN VALUE(U3-proc + ".p") (iBal,iCurr,iClass).
              RUN VALUE(U2-proc + ".p") (iBal,iCurr,"", OUTPUT fnd-acct).
              IF fnd-acct = ? THEN RETURN "exit".
            END.
         END.
         IF fnd-acct <> ? THEN ac = fnd-acct.
      END.

      /* �஢��塞 ����稥 ᢮������ ���祭�� ����稪�, �᫨ ��� �� �஢��﫨. */
      IF vNumFreeCnt = ? THEN DO:
         RUN CalcNumFreeCounters IN THIS-PROCEDURE (ac,
                                                    iCurr,
                                                    iAcctCat,
                                                    iBal,
                                                    iClass,
                                                    iMask,
                                                    OUTPUT vNumFreeCnt).
         IF vNumFreeCnt = 0 THEN DO:
            oErrMsg = IF CAN-FIND (FIRST code WHERE
                                      code.class = "��⠐���ࢠ" AND
                                      code.code  = AddFilToAcct(ac, shFilial)
                                   NO-LOCK)
                      THEN "�� ���祭�� ���� �ᯮ�������, ���� ��१�ࢨ஢���."
                      ELSE "".
            oErrMsg = "|acct60||%s=" + oErrMsg.
            LEAVE NEXTACCT.
         END.
      END.

      /* �஢�ઠ �� ������⢨� ����� ��� � १�ࢥ ��⮢. */
      /* ��⠢�� ���� ���� */
      RUN STORED-PROCEDURE IS_ACCT_EXISTS mHandle = PROC-HANDLE
         (
         INPUT  PARAM P_ACCT = ac,
         OUTPUT PARAM IS_ACCT_EXISTS = ?
         ).
      CLOSE STORED-PROC IS_ACCT_EXISTS iStat = PROC-STATUS.
      IF iStat = 0 THEN
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "~nac = " + ac + " iStat = " + STRING(iStat) + " IS_ACCT_EXISTS = " + STRING(IS_ACCT_EXISTS)).
         IF IS_ACCT_EXISTS EQ 1 THEN
         DO:
            jj = jj + 1.
            NEXT NEXTACCT.
         END.
      END.
      ELSE
      DO:
         RUN dbgprint.p(PROGRAM-NAME(1) + " line = {&line-number}",
         "iStat = " + STRING(iStat)).
         oErrMsg = "||-1|" + ERROR-STATUS:GET-MESSAGE(1).
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* ����� ��⠢�� ���� ���� */

      IF CAN-FIND (FIRST CODE WHERE code.class = "��⠐���ࢠ"
                                AND code.code  = AddFilToAcct(ac, shFilial))
      THEN DO:
         /* �᫨ ��� ����ᥭ � �����䨪��� "��� १�ࢠ",
         ** � � ��᪥ ��� ���稪�,
         ** � �뤠�� ᮮ�饭�� � ��室��.*/
         IF toklen[{&TOK_CNT_IDX}] = 0 THEN DO:
            oErrMsg = "|acct02||%s=" + ac.
            LEAVE NEXTACCT.
         END.
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* �஢�ઠ �� ������ ��� � ����祭�� ����஬. */
      IF CAN-FIND (FIRST acct WHERE acct.filial-id = shFilial
                                AND acct.number    = ac
                                AND acct.curr      = iCurr)
      THEN DO:
         /* �᫨ ⠪�� ��� 㦥 �������
         ** � ���稪� � ����� �� �।�ᬮ�७�,
         ** � �뤠�� ᮮ�饭�� � ��室��.  */
         IF toklen[{&TOK_CNT_IDX}] = 0 THEN DO:
            oErrMsg = "|acct07||".
            LEAVE NEXTACCT.
         END.
         /* �᫨ ���稪 �������,
         ** � ���६����㥬 ��� � �����塞 横�. */
         jj = jj + 1.
         NEXT NEXTACCT.
      END.
      /* ��ନ�㥬 ����� ��� � ��室��. */
      oAcct = ac.
      LEAVE NEXTACCT.
   END.
END PROCEDURE.

/* �뤠�� ���� �� ������
   iClass - ����� ��� , ���ਬ�� acctb
   oMask  - ��᪠ ��� */
PROCEDURE GetAcctClassMaskString:
   DEFINE INPUT  PARAMETER iClass LIKE acct.class-code NO-UNDO.
   DEFINE OUTPUT PARAMETER oMask  AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE vMask AS CHARACTER NO-UNDO.
   DEFINE BUFFER bCode FOR code.

   vMask = GetXattrInit(iClass,"acct").
   IF NOT {assigned vMask} THEN RETURN.

   /* Commented by KSV: ��।��塞, ���� �� ��᪠ �����䨪��஬.
   ** �᫨ � ��᪥ ��� ����� �����䨪���, � �饬 �� ���祭�� �⮣�
   ** �����䨪��� � �� ���� ��।��塞 ����� ���. �.�. ������ ���ᯥ稢�����
   ** �᫮���, �� � �����䨪��� ᮤ�ঠ��� ࠢ��業�� �� ����� ��᪨ */
   IF NUM-ENTRIES(vMask) = 1 THEN
      FIND FIRST bCode WHERE
         bCode.class  = vMask AND
         bCode.parent = vMask AND
         bCode.val    > ""    NO-LOCK NO-ERROR.
   ELSE
      FIND FIRST bCode WHERE
         bCode.class  = ENTRY(1,vMask) AND
         bCode.parent = ENTRY(2,vMask) AND
         bCode.val    > ""    NO-LOCK NO-ERROR.

   /* Commented by KSV: �᫨ ��᪠ ������ ��� �����䨪���, ��६ ��ࢮ�
   ** ���祭�� �� �����䨪��� */
   IF AVAIL bCode THEN vMask = bCode.val.

   /* Commented by KSV: � ��砥, �᫨ ��� �����䨪��� ������ ᫮����
   ** ������ ��६ ⠪� ���祭�� �� 㬮�砭�� */
   IF NOT AVAILABLE bCode AND
      CAN-FIND(FIRST bCode WHERE bCode.class = ENTRY(1,vMask)) THEN
      vMask = "��������������������".

   oMask = vMask.
END PROCEDURE.

PROCEDURE GetAcctMask:
   DEFINE INPUT  PARAMETER iAcctMask  AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokAcct   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokIdx    AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER oTokLen    AS CHAR NO-UNDO.

   DEFINE VAR vTokIdx   LIKE tokidx NO-UNDO.
   DEFINE VAR vTokLen   LIKE toklen NO-UNDO.
   DEFINE VAR vTokList  AS CHAR INIT {&TOK_LIST_EQ} NO-UNDO.
   DEFINE VAR vMaskPos  AS INT64  NO-UNDO.
   DEFINE VAR vMaskSym  AS CHAR     NO-UNDO.
   DEFINE VAR vIdxPos   AS INT64  NO-UNDO.
   DEFINE VAR vSymCode  AS INT64  NO-UNDO.
   DEFINE VAR vBlockStr AS CHAR     NO-UNDO.
   DEFINE VAR vBlockBeg AS INT64  NO-UNDO.
   DEFINE VAR vBlockEnd AS INT64  NO-UNDO.
   DEFINE VAR vBlockPos AS INT64  NO-UNDO.

   ASSIGN
      oTokAcct = ""
      vTokLen = 0
      vTokIdx = 0
   .

   DO vMaskPos = 1 TO LENGTH(iAcctMask):
      vMaskSym = SUBSTR(iAcctMask,vMaskPos,1).
      IF  vMaskSym >= "0"
      AND vMaskSym <= "9" THEN DO:
         ASSIGN
            vIdxPos  = vIdxPos  + 1
            oTokAcct = oTokAcct + vMaskSym
         .
         NEXT.
      END.
      vSymCode = LOOKUP(vMaskSym,vTokList).
      IF vSymCode = 0 THEN DO:
         /* �訡��, ��� ⠪��� ᨬ���� */
         RUN Fill-SysMes("","acct09","","%s=" + vMaskSym + "%s=" + iAcctMask).
         RETURN.
      END.
      ELSE IF vSymCode = {&TOK_BRACKET_IDX} THEN DO:
         /* ���� [ ... ] */
         ASSIGN
            vMaskSym  = SUBSTR(iAcctMask,vMaskPos - 1,1)
            vSymCode  = LOOKUP(vMaskSym,vTokList)
            vBlockStr = ENTRY(1,SUBSTR(iAcctMask,vMaskPos + 1),"]")
            vMaskPos  = vMaskPos + LENGTH(vBlockStr) + 1
            vBlockBeg = INT64(ENTRY(1,vBlockStr,"-"))
            vBlockEnd = (IF NUM-ENTRIES(vBlockStr,"-") = 1
                         THEN vBlockBeg
                         ELSE INT64(ENTRY(2,vBlockStr,"-")))
         .
         DO vBlockPos = vBlockBeg TO vBlockEnd:
            ASSIGN
               oTokAcct         = oTokAcct + vMaskSym
               vIdxPos          = vIdxPos  + 1
               vTokIdx[vIdxPos] = vBlockPos
            .
         END.
         vTokLen[vSymCode] = vTokLen[vSymCode] + vBlockEnd - vBlockBeg + 1.
      END.
      ELSE IF (    vMaskPos < LENGTH(iAcctMask)
               AND SUBSTR(iAcctMask, vMaskPos + 1, 1) <> {&TOK_BRACKET})
           OR vMaskPos = LENGTH(iAcctMask) THEN ASSIGN
         /* ���� ᨬ��� */
         vIdxPos           = vIdxPos           + 1
         oTokAcct          = oTokAcct          + vMaskSym
         vTokLen[vSymCode] = vTokLen[vSymCode] + 1
         vTokIdx[vIdxPos]  = vTokLen[vSymCode]
      .
   END.
   DO vIdxPos = 1 TO EXTENT(vtokidx):
      oTokIdx = oTokIdx + STRING(vtokidx[vIdxPos]) + ','.
      IF vIdxPos <= EXTENT(vtoklen)
         THEN oTokLen = oTokLen + STRING(vtoklen[vIdxPos]) + ','.
   END.
END PROCEDURE.

PROCEDURE SetAcct.
   DEFINE INPUT-OUTPUT PARAMETER ac AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER toktype   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER tok       AS CHARACTER NO-UNDO.

   DEFINE VAR i        AS INT64 NO-UNDO.
   DEFINE VAR vMAXPOS  AS INT64 NO-UNDO.

   vMAXPOS = -1.
   DO i = 1 TO LENGTH(tokacct):
      ASSIGN
         vMAXPOS = tokidx[i] WHEN SUBSTR(tokacct,i,1) =  toktype
                              AND INT64(tokidx[i])  >  vMAXPOS
      .
   END.
   IF vMAXPOS - LENGTH(tok) >  0 THEN
   tok = FILL("0",vMAXPOS - LENGTH(tok)) + tok.

   DO i = 1 TO LENGTH(tokacct):
      IF SUBSTR(tokacct,i,1) =  toktype THEN
      OVERLAY(ac,i,1) = SUBSTR(tok,tokidx[i],1).
   END.
END PROCEDURE.

{bal2acct.i} /* �����㬥��� ����஢���� ���.४����⮢ */

/*------------------------------------------------------------------------------
  Purpose:     ��⠭�������� ��ࠬ���� ���稪�.
  Parameters:  iCounter - ����⥫�� ���稪 ��� ���
               iErrProc - ��� ��楤��� ᮤ�ঠ騩 ��楤��� ���⭮�� �맮��
                          SetFoundAcct, ����᪠���� � ��砥, �᫨ ��� � ⠪��
                          ���稪�� ������� � ����� ���� �ᯮ�짮��� �����
                          ������ ������ ���
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE SetDefinedCntParam:
   DEFINE INPUT  PARAMETER iCounter AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iErrProc AS HANDLE     NO-UNDO.

   ASSIGN
      mDefinedCounter = iCounter
      mDefinedErrProc = iErrProc.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ��⠥��� �ᯮ�짮���� ��࠭�� ��।����� ���稪 ���
               ᮧ��������� ���.
  Parameters:  iBalAcct    - ����� ��� 2-�� ���浪� ��� ���뢠����� ���
               iCurrency   - ��� ������ ��� ���뢠����� ���
               iAcctNum    - ����� ��ନ஢������ ���
               iAcctMsk    - ��᪠, �ᯮ��㥬�� ��� ������ ���
               oCounter    - ���祭�� ���稪�
  Notes:       �᫨ ��� � �ॡ㥬� ���稪�� 㦥 �������, � �।�ᬮ�७
               �맮� ��楤��� ���⭮�� �맮��, ����� ����� ��������
               "ERROR", �.�. �४��� ����⨥ ������ ���.
------------------------------------------------------------------------------*/
PROCEDURE GetDefinedCounter:
   DEFINE INPUT  PARAMETER iBalAcct  AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctNum  AS CHARACTER        NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctMsk  AS CHARACTER        NO-UNDO.
   DEFINE OUTPUT PARAMETER oCounter  AS CHARACTER INIT ? NO-UNDO.

   DEFINE VARIABLE vMskCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vMskLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vMinPos AS INT64    NO-UNDO.
   DEFINE VARIABLE vMaxPos AS INT64    NO-UNDO.
   DEFINE VARIABLE vCntStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCntInt AS INT64    NO-UNDO.
   DEFINE VARIABLE vResult AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bAcct FOR acct.

   MAIN:
   DO ON ERROR UNDO MAIN, LEAVE MAIN:
      /* �஢��塞, ��।���� �� ���稪  */
      IF mDefinedCounter = ? THEN LEAVE MAIN.

      vMinPos = INDEX  (iAcctMsk,{&TOK_CNT}).
      vMaxPos = R-INDEX(iAcctMsk,{&TOK_CNT}).

      /* �஢��塞, ���� ��  � ��᪥ ��� ���� ��� ���稪� */
      IF vMinPos = 0 THEN LEAVE MAIN.

      vCntStr = SUBSTR(iAcctMsk,vMinPos,vMaxPos - vMinPos + 1).

      /* �஢��塞, ᮢ������ �� ����� ��������� ���稪� � �������� */
      IF LENGTH(vCntStr) <> LENGTH(mDefinedCounter) THEN
      DO:
         /* �஢��塞, ���� �� ���稪 �᫮�. ���᫮�� ���稪�
         ** �� �����ন������ */
         vCntInt = INT64(mDefinedCounter) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE MAIN.

         /* �஢��塞, ����� �� ���稪 �ᯮ�짮��� ��� ���� ���ଠ樨 */
         IF LENGTH(STRING(vCntInt)) > LENGTH(vCntStr) THEN LEAVE MAIN.

         /* ��ࠢ������ ���稪 ��� �ॡ㥬�� ����� */
         mDefinedCounter = STRING(vCntInt,FILL("9",LENGTH(vCntStr))).
      END.

      vMskLen = LENGTH(iAcctMsk).
      vCntInt = 1.

      /* ������� ���� ��� � ����祭�� � ��� ���稪�� */
      DO vMskCnt = 1 TO vMskLen:
         IF CAN-DO("�,�",SUBSTR(iAcctMsk,vMskCnt,1)) THEN
            SUBSTR(iAcctNum,vMskCnt,1) = ".".
         IF SUBSTR(iAcctMsk,vMskCnt,1) = {&TOK_CNT} THEN
         DO:
            SUBSTR(iAcctNum,vMskCnt,1) = SUBSTR(mDefinedCounter,vCntInt,1).
            vCntInt = vCntInt + 1.
         END.
      END.

      /* �஢��塞, ��� �� ��� � ⠪�� ��᪮� � �� */
      FOR FIRST bAcct WHERE
         bAcct.bal-acct =        iBalAcct  AND
         bAcct.currency =        iCurrency AND
         bAcct.acct     MATCHES  iAcctNum  NO-LOCK:

         /* �஢��塞, ���� �� ��ࠡ��稪 ��� ���������� ��� */
         IF VALID-HANDLE(mDefinedErrProc) THEN
         DO:
            /* �஢��塞, 㤮���⢮��� �������� ��� ��뢠���� ��楤���,
            ** �᫨ - ��, � ��� ������ �������� "ERROR" � ᮧ����� ������
            ** ��� �㤥� �४�饭�  */
            RUN SetFoundAcct IN mDefinedErrProc (bAcct.acct) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN vResult = RETURN-VALUE.
         END.
         LEAVE MAIN.
      END. /* End of FOR */

      /* �����頥� ���稪 */
      oCounter = mDefinedCounter.
   END. /* MAIN: */

   /* ����뢠�� ���稪 � ��楤��� ���⭮�� �맮��, �⮡� ��� ��砩�� ��
   ** �뫨 �ᯮ�짮���� ����୮ */
   ASSIGN
     mDefinedCounter = ?
     mDefinedErrProc = ?.

   RETURN vResult.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �������� ���
  Parameters:  iClass      - ����� ᮧ��������� ���
               iBal        - ����� ��� 2-�� ���浪�
               iCurr       - ��� ������
               iCustCat    - ⨯ ������ (�,�,�)
               iCustID     - �����䨪��� ������
               iOpenDate   - ��� ������ ���
               oAcct       - ����� ᮧ������� ���
                             ? - �訡�� ᮧ����� ���
  Notes: �������� !!! �� MAKEACCT ������ ���������� ������ �� ���������������
         ����� ����� � ����� �������/�������� . ���� ��� ���������� ����� -
         ���� ��������� cm_acct_cr()
------------------------------------------------------------------------------*/
PROCEDURE MakeAcct:
   DEF INPUT  PARAM iClass          AS CHAR   NO-UNDO. /* ��易⥫�� */
   DEF INPUT  PARAM iBal            AS INT64    NO-UNDO. /* ��易⥫�� */
   DEF INPUT  PARAM iCurr           AS CHAR   NO-UNDO. /* ��易⥫�� */
   DEF INPUT  PARAM iCustCat        AS CHAR   NO-UNDO. /* ��易⥫�� */
   DEF INPUT  PARAM iCustID         AS INT64    NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iOpenDate       AS DATE   NO-UNDO. /* ����易⥫�� */
   DEF OUTPUT PARAM oAcct           AS CHAR            /* �⪠�뢠��� */
                                    INIT ?.
   DEF        PARAM BUFFER acct     FOR acct.          /* ���� ���. */
   DEF INPUT  PARAM iAcctMask       AS CHAR   NO-UNDO
                                    FORMAT "X(25)".
   DEF INPUT  PARAM iKodDoxRash     AS CHAR   NO-UNDO
                                    FORMAT "X(5)".
   DEF INPUT  PARAM iDetails        AS CHAR   NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iKauId          AS CHAR   NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iContract       AS CHAR   NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iUserId         AS CHAR   NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iBranchId       AS CHAR   NO-UNDO. /* ����易⥫�� */
   DEF INPUT  PARAM iCopyBalXattr   AS LOG    NO-UNDO. /* ����஢��� �� � bal-acct*/

   DEF VAR vAcct LIKE acct.acct NO-UNDO. /* ����� ���. */
   DEF VAR vDate-In AS DATE NO-UNDO.
   DEFINE VARIABLE vOK AS LOGICAL NO-UNDO.

   ASSIGN
      vClass    = iClass
      vAcct-Cat = GetXAttrInit(iClass,"acct-cat")
   .
   FIND FIRST bal-acct WHERE
      bal-acct.bal-acct =  iBal
   NO-LOCK NO-ERROR.
   {was-err.i}

   IF NOT fBalAcctWorkPlan(string(iBal)) THEN DO:

      IF FGetSetting("�302�","ContrWorkPlan","") =  "�।�०�����" THEN
         IF SESSION:BATCH-MODE AND SESSION:CLIENT-TYPE <> "WEBSPEED"
         THEN MESSAGE "��� ��ண� ���浪� "  + string(iBal) + " �� ����祭 � ࠡ�稩 ���� ��⮢.".
         ELSE RUN Fill-SysMes("","","0","��� ��ண� ���浪� "  + STRING(iBal) + " �� ����祭 � ࠡ�稩 ���� ��⮢.").
      IF FGetSetting("�302�","ContrWorkPlan","") =  "�����" THEN
         RETURN ERROR "��� ��ண� ���浪� "  + STRING(iBal) + " �� ����祭 � ࠡ�稩 ���� ��⮢.".

   END.

   RUN ACGetKeyProg(vclass, OUTPUT keyprog).
   ASSIGN
                        /* ��� ���.��� ������ ���� �� ��.��ࠬ����*/
      mSymbPU  =  iKodDoxRash
                        /* ��᪠ ������ ���� �� ��.��ࠬ����
                        **  �� ^^^^^ �㦭� ������� ࠭�� ����᪮� FindAcctMask */
      acctmask =  iAcctMask
   .
   TR:
   DO TRANSACTION
   ON ERROR UNDO TR, LEAVE TR
   ON STOP  UNDO TR, LEAVE TR:
                        /* �᫨ ��� �� ᮧ���, � ᮧ���� ���. */
      IF NOT AVAILABLE acct
      THEN DO:
         CREATE acct NO-ERROR.
         {was-err.i &LBL=TR }
      END.
                        /* ���樠������ ���祭�ﬨ. */
      ASSIGN
         acct.class-code   =  iClass
         acct.bal-acct     =  iBal
         acct.currency     =  iCurr
         acct.cust-cat     =  iCustCat
         acct.cust-id      =  IF iCustCat    <> '�'   THEN iCustID   ELSE 0
         acct.open-date    =  IF iOpenDate   <> ?     THEN iOpenDate ELSE dob
         acct.contract     =  IF       iContract <> ?
                                 AND   iContract <> ''
                                 THEN iContract
                                 ELSE IF bal-acct.contract <> ""
                                    THEN bal-acct.contract
                                    ELSE acct.contract
         acct.user-id      =  IF    iUserId  =  ""
                                 OR iUserId  =  ?
                                 THEN USERID ("bisquit")
                                 ELSE iUserId
         acct.side         =  bal-acct.side
                                 WHEN LOOKUP(bal-acct.side, '�,�,��') <> 0
         acct.acct-cat     =  bal-acct.acct-cat
         acct.rate-type    =  IF acct.currency <> ""
                                 THEN "����"
                                 ELSE ""
         acct.kau-id       =  IF       iKauId <> ?
                                 AND   iKauId <> ''
                                 THEN iKauId
                                 ELSE ""
      NO-ERROR.
      {was-err.i &LBL=TR }
      ASSIGN
         acct.branch-id    =  IF    iBranchId =  ""
                                 OR iBranchId =  ?
                                 THEN TRIM (GetUserBranchId (USERID ("bisquit")))
                                 ELSE iBranchId
      NO-ERROR.
      {was-err.i &LBL=TR }
      IF LOOKUP(acct.cust-cat,"�,�,�") > 0 THEN DO:
      vDate-In = DATE(getValueAttr(getCustClass(acct.cust-cat),
                      STRING(acct.cust-id),
                     "date-in")) NO-ERROR.
      {was-err.i &LBL=TR }
       IF vDate-In > acct.open-date THEN
           UNDO TR, RETURN ERROR '��������! ��� ������ ��� �� ����� ���� ����� ���� ॣ����樨 ������!'.
      END.

      IF GetCode ("�������", acct.kau-id) =  ?
         THEN acct.kau-id  = GetXAttrInit(vClass,"kau-id").
                        /* ��ନ஢���� ����� ���. */
      vAcct = ''.
      RUN Acct-Cr (iBal, iCurr, INPUT-OUTPUT vAcct, BUFFER acct) NO-ERROR.
      {was-err.i LBL=TR}

      vAcct = IF GetSysConf("PlacementFO_RSHB_acct") <> ? THEN GetSysConf("PlacementFO_RSHB_acct")
                                                          ELSE vAcct.
      IF    vAcct =  ?
         OR vAcct =  ''
         THEN  UNDO TR, RETURN ERROR '�訡�� � ��楤�� ���������� ����� ��� '.
      ASSIGN
         acct.acct   =  vAcct
         acct.number =  vAcct
      NO-ERROR.
      {was-err.i &LBL=TR }
                        /* �맮� �ਣ��� �� WRITE. */
      VALIDATE acct NO-ERROR.
      {was-err.i &LBL=TR }
                        /* �������� �� �� ���. */
      IF {assigned iDetails} THEN DO:
         RUN SetAcctDetails IN THIS-PROCEDURE (BUFFER acct,
                                               iOpenDate,
                                               iDetails,
                                               OUTPUT vOK).
         IF vOK <> YES THEN DO:
            vErrStr = GetErrMsg() NO-ERROR.
            UNDO TR, RETURN ERROR vErrStr.
         END.
      END.
      RUN MakeXattr (acct.acct, acct.currency, iCopyBalXattr) NO-ERROR.
      IF ERROR-STATUS:ERROR
         THEN RETURN ERROR RETURN-VALUE.
                        /* ���࠭塞 ����� ���. */
      oAcct = acct.acct.
   END.
   RETURN.
END PROCEDURE.

/* �������� �� ��� ���. */
PROCEDURE MakeXattr.
   DEF INPUT  PARAM iAcct        AS CHAR   NO-UNDO. /* ���. */
   DEF INPUT  PARAM iCurrency    AS CHAR   NO-UNDO. /* �����. */
   DEF INPUT  PARAM iCopyXattr   AS LOG    NO-UNDO. /* ����஢��� �� �� � ��� 2-�� ���浪�. */

   DEF BUFFER acct FOR acct. /* ���������� ����. */

   FIND FIRST acct WHERE
            acct.acct      =  iAcct
      AND   acct.currency  =  iCurrency
   NO-LOCK NO-ERROR.

   IF AVAIL acct THEN
   BLCK:
   DO TRANSACTION:
                        /* ������� �� ���㤭�� ����訩 ���. */
      UpdateSigns (
         acct.class-code,
         acct.acct + "," + acct.currency,
         "���������",
         USERID ("bisquit"),
         ?
      ).
      /* �����㥬 �� � ��� 2-�� ���浪� ��� �
      ** �ᯮ�짮������ �����䨪��� ��᪨��᫥�. */
      IF iCopyXattr <> FALSE
      THEN DO:
         RUN BalToAcct_Xattr (RECID (acct), "*", YES, YES) NO-ERROR.
         {was-err.i &LBL=BLCK}
      END.
   END.
   RETURN.
END PROCEDURE.

/* ����祭�� �ଠ� ���. */
FUNCTION GetAcctFmtEx RETURN CHAR
   (INPUT iAcctCat  AS CHAR,   /*��⥣��� ���*/
    INPUT iDefFmt   AS CHAR,   /*���祭�� �� 㬮�砭�� */
    INPUT iMessMode AS LOG):   /*�뢮���� ᮮ�饭�� ? */

   DEFINE VARIABLE vAcctFmt AS CHARACTER NO-UNDO. /* ��ଠ� ���. */

   vAcctFmt = FGetSettingEx ("Output-Formats", iAcctCat + "-Acct-Fmt" ,iDefFmt,iMessMode).
   IF iDefFmt  =  ? AND
      vAcctFmt =  ?
      THEN vAcctFmt = FGetSetting ("Output-Formats",
                                   "-Acct-Fmt",
                                   "xxxxxxxxxxxxxxxxxxxx").

   RETURN vAcctFmt.
END FUNCTION.


FUNCTION GetAcctFmt RETURN CHAR (
   INPUT iAcctCat AS CHAR        /* ��⥣��� ���. */
   ):

   RETURN GetAcctFmtEx(iAcctCat,?,no).

END FUNCTION.

/* ����祭�� 梥� �⮡ࠦ���� ���⪠. */
FUNCTION GetBalColorBuffer RETURN CHAR (
   INPUT iHAcct   AS HANDLE,     /* �����⥫� �� ����� ���. */
   INPUT iBalance AS DECIMAL     /* ���祭�� ���⪠ � ������. */
):
   DEF VAR vAcctPosColor   AS CHAR   NO-UNDO. /* ���� ���⪠. */
   DEF VAR vAcctPosCode    AS CHAR   NO-UNDO. /* ��� ���⪠ (��ଠ���/����襭��). */

                        /* ����祭�� ���� 梥�. */
   vAcctPosCode =
      IF    iBalance =  ?
         OR NOT AcctLookBuffer (iHAcct)
         THEN "NR"
         ELSE IF iHAcct:BUFFER-FIELD ("side"):BUFFER-VALUE =  "�"
            THEN IF iBalance >= 0
               THEN "AN"
               ELSE "ABB"
            ELSE IF iHAcct:BUFFER-FIELD ("side"):BUFFER-VALUE =  "�"
               THEN IF iBalance <= 0
                  THEN "PN"
                  ELSE "PBB"
               ELSE "NR".
                        /* ���� ����ன�� 梥� �� ����. */
   vAcctPosColor = fGetSetting ("AcctPosColors", vAcctPosCode, ?).
                        /* ��ନ஢���� 梥� �� ���� ���⪠,
                        ** �᫨ �� ��⠭����� ��. */
   IF vAcctPosColor =  ?
   THEN CASE vAcctPosCode:
      WHEN "AN"   THEN vAcctPosColor = "bright-red".
      WHEN "ABB"  THEN vAcctPosColor = "blink-bright-red".
      WHEN "PN"   THEN vAcctPosColor = "bright-cyan".
      WHEN "PBB"  THEN vAcctPosColor = "blink-bright-cyan".
      WHEN "NR"   THEN vAcctPosColor = "bright-yellow".
   END CASE.
   RETURN vAcctPosColor.
END FUNCTION.

/* ����祭�� 梥� ��� �� 㪠��⥫� �� ���. */
FUNCTION GetAcctColorBuffer RETURN CHAR (
   INPUT iHAcct   AS HANDLE,     /* �����⥫� �� ����� ���. */
   INPUT iDate    AS DATE        /* ��� ��।������ 梥� */
):
   RETURN IF     (   BlockAcct (iHAcct:BUFFER-FIELD ("acct"):BUFFER-VALUE + ',' + iHAcct:BUFFER-FIELD ("currency"):BUFFER-VALUE,
                                DATETIME(iDate + 1) - 1
                                ) <> ""
                  OR ChkAcctCart(iHAcct))
             AND {assigned mColorBlockAcct}
          THEN mColorBlockAcct
          ELSE  IF     iHAcct:BUFFER-FIELD ("close-date"):BUFFER-VALUE <> ?
                   AND {assigned mColorCloseAcct}
          THEN mColorCloseAcct
          ELSE "normal".

END FUNCTION.
/* ����祭�� 梥� �⮡ࠦ���� ���⪠ */
FUNCTION GetBalColor RETURN CHAR (
   BUFFER bacct FOR acct,        /* ����� ��� */
   INPUT iBalance AS DECIMAL     /* ������ (���� ? �᫨ ���) */
):
   RETURN GetBalColorBuffer ((BUFFER bAcct:HANDLE), iBalance).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �믮���� १�ࢨ஢���� ����� ��� ��� ���쭥�襣�
               �ᯮ�짮�����. ��१�ࢨ஢���� ����� ��� �� ����� ����
               �ᯮ�짮��� ����୮.
  Parameters:  iAcct - ����� ���
               oOk   - 䫠� ������: YES - ��� ��१�ࢨ஢�� �ᯥ譮
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE AcctKeep:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS LOGICAL.

   DEFINE BUFFER CODE FOR CODE.

   IF GetCodeEx("��⠐���ࢠ",iAcct,?) <> ? THEN
   DO:
      oOk = YES.
      RETURN .
   END.


   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      CREATE CODE.
      ASSIGN
         CODE.class  = "��⠐���ࢠ"
         CODE.parent = "��⠐���ࢠ"
         CODE.code   = iAcct.
      oOk = YES.

      RELEASE code.
   END.  /* End of TR BLOCK */

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �믮���� �᢮�������� ��१�ࢨ஢������ ����� ���.
  Parameters:  iAcct - ����� ���
               oOk   - 䫠� ������: YES - ��� �᢮������ �ᯥ譮
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE AcctFree:
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS LOGICAL    .

   DEFINE BUFFER CODE FOR CODE.

   IF GetCodeEx("��⠐���ࢠ",iAcct,?) = ? THEN
   DO:
      oOk = YES.
      RETURN .
   END.


   TR:
   DO TRANSACTION ON ERROR UNDO TR,LEAVE TR
      ON STOP UNDO TR,LEAVE TR:
      FIND FIRST CODE WHERE
         CODE.class  = "��⠐���ࢠ" AND
         CODE.parent = "��⠐���ࢠ" AND
         CODE.code   = iAcct NO-ERROR.
      DELETE CODE.
      oOk = YES.
   END.  /* End of TR BLOCK */

END PROCEDURE.

/**************************************
 * ��������� ��������� �������� ����� *
 *************************************/
/*------------------------------------------------------------------------------
  Purpose:     �஢�ઠ ⨯� ������ ��楢��� ���
  Parameters:  iBalAcct - ��� 2-�� ���浪�
               iCustCat - ⨯ ������
               oOk      - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Cust-Cat:
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO. /* ��� 1�� ���浪� ��� ��� */
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER b-loan   FOR loan.

   DEF VAR vTmp     AS CHARACTER NO-UNDO .
   DEF VAR vStrLoan AS CHARACTER NO-UNDO .

   /* Commented by KSV: �饬 ��� 2-�� ���浪� � �ࠢ������ �����⨬� ⨯�
   ** �����⮢ */
   FIND bal-acct WHERE bal-acct.bal-acct = iBalAcct NO-LOCK NO-ERROR.
   IF AVAILABLE bal-acct             AND
      LENGTH(bal-acct.cust-cat) = 1  AND
      iCustCat <> bal-acct.cust-cat  THEN
   DO:
      /* ����� ������� �����뢠���� ⮫쪮 ��� �࠭���権   . ���⠢����� � credacct.p */
      vTmp  = GetSysConf ("LoanRecid-Acct19") .
      RUN DeleteOldDataProtocol IN h_base ("LoanRecid-Acct19").
      vStrLoan = "".
      IF vTmp <> ""  and vTmp <>  ? THEN DO:
         FIND FIRST b-loan WHERE
                    RECID(b-loan)  = INT64(vTmp)
                    NO-LOCK NO-ERROR .
         IF AVAILABLE b-loan THEN
            vStrLoan = " ������� : " + b-loan.doc-ref.
      END.

      RUN Fill-SysMes ("","acct19","",
      substitute("%s=&1%s=&2  ��楢�� ���: &3 &4"  , iCustCat , bal-acct.cust-cat, iAcct , vStrLoan )) .
      
      RETURN.
   END.
   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢�ઠ ��� 2-�� ���浪�
  Parameters:  iBalAcct - ��� 2-�� ���浪�
               iAcctCat - ��⥣��� ��� ��楢��� ���
               iCustCat - ⨯ ������ ��楢��� ���
               oOk      - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Bal-Acct:
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO. /* ��� 1�� ���浪� ��� ��� */
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER bal-acct FOR bal-acct.

   FIND bal-acct WHERE bal-acct.bal-acct = iBalAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bal-acct then
   DO:
      RUN Fill-SysMes("","acct17","",
                      "%s=" + IF iBalAcct = ?
                              THEN "?"
                              ELSE STRING(iBalAcct)).
      RETURN.
   END.

   IF bal-acct.acct-cat <> iAcctCat THEN
   DO:
      RUN Fill-SysMes("","acct18","",
                      "%s=" + iAcctCat +
                      "%s=" + bal-acct.acct-cat).
      RETURN.
   END.

   RUN Check-Acct-Cust-Cat(iBalAcct,iCustCat, iAcct, OUTPUT oOk).

   RETURN .
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢�ઠ ������ ��楢��� ���
  Parameters:  iCurrency  - ��� ������
               iAcctCat   - ��⥣��� ���
               oOk        - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Currency:
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk         AS INT64    NO-UNDO INIT -1.

   IF iAcctCat <> "d" THEN
   DO:
      IF NOT CAN-FIND(currency WHERE currency.currency = iCurrency) THEN
      DO:
         RUN Fill-SysMes("","acct20","","%s=" + iCurrency).
         RETURN.
      END.
   END.
   ELSE
   DO:
      IF NOT CAN-FIND(sec-code WHERE sec-code.sec-code = iCurrency) THEN
      DO:
         RUN Fill-SysMes("","acct21","","%s=" + iCurrency).
         RETURN.
      END.
   END.

   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢�ઠ ��୮�� ���
  Parameters:  iContrAcct - ���� ���
               iCurrency  - ��� ������
               iBalAcct   - ��� 2-�� ���浪�
               iSide      - �ਧ��� ��⨢/���ᨢ
               iAcctCat   - ��⥣��� ���
               oOk        - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Contr-Acct:
   DEFINE INPUT  PARAMETER iContrAcct  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct    AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk         AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER xacct FOR acct.
   DEFINE BUFFER CODE  FOR CODE.

   IF NOT {assigned iContrAcct} THEN
   DO:
      oOk = 0.
      RETURN .
   END.

   /* �饬 ���� ���� ��� */
   {find-act.i
      &bact = xacct
      &acct = iContrAcct
      &curr = iCurrency
   }

   IF NOT AVAIL xacct THEN
   DO:
      RUN Fill-SysMes("","acct22","","%s=" + iContrAcct +
                                     "%s=" + iCurrency).
      RETURN.
   END.
  
   IF xacct.currency <> iCurrency THEN
   DO:
      RUN Fill-SysMes("","acct22","","%s=" + iContrAcct +
                                     "%s=" + iCurrency).
      RETURN.
   END.

   IF xacct.acct-cat <> iAcctCat THEN
   DO:
      RUN Fill-SysMes("","acct23","","%s=" + iContrAcct +
                                     "%s=" + xacct.acct-cat +
                                     "%s=" + iAcctCat).
      RETURN.
   END.

   IF xacct.side  =  iSide THEN
   DO:
      RUN Fill-SysMes("","acct24","","%s=" + iContrAcct +
                                     "%s=" + xacct.side +
                                     "%s=" + iSide).
      RETURN.
   END.

   /* �஢�ઠ �ࠢ�筨�� ����� ��⮢ */
   FIND FIRST CODE WHERE
       code.class =  "Dual-bal-acct"  AND
      (code.code =  STRING(iBalAcct)  AND code.val  =  STRING(xacct.bal-acct) OR
       code.val  =  STRING(iBalAcct)  AND code.code =  STRING(xacct.bal-acct))
      NO-LOCK NO-ERROR.

   IF NOT AVAIL code THEN
   DO:
      RUN Fill-SysMes("","acct25","","%s=" + STRING(iBalAcct) +
                                     "%s=" + STRING(xacct.bal-acct)).
      RETURN.
   END.

   oOk = 0.

END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢�ઠ �ਧ���� ��⨢/���ᨢ
  Parameters:  iCheckOp - ⨯ �஢�ન ���㬥�⮢
               iSide    - �ਧ��� ��⨢/���ᨢ
               oOk      - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Acct-Check-Side:
   DEFINE INPUT  PARAMETER iCheckOp AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSide    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO INIT -1.

   DEFINE BUFFER CODE FOR CODE.
   FIND code WHERE
      code.class = "check-op" AND
      code.code  = iCheckOp NO-LOCK NO-ERROR.

   IF {assigned iCheckOp} AND
      AVAIL code          AND
      NOT CAN-DO(code.val, iSide) THEN
   DO:
      RUN Fill-SysMes("","acct27","","%s=" + iSide +
                                     "%s=" + CODE.val).
      RETURN.
   END.
   oOk = 0.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢��塞 ��� �� ᮮ⢥�ᢨ� ��᪥
  Parameters:  acct - ���� ��楢��� ���
               oOk   - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Mask:
   DEFINE PARAMETER BUFFER acct     FOR acct.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO.

   DEFINE VARIABLE vMask      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCnt       AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum       AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen       AS INT64    NO-UNDO.
   DEFINE VARIABLE vAcctLen   AS INT64    NO-UNDO.
   DEFINE VARIABLE vTokAcct   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTokidxs   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vToklens   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vHCode     AS HANDLE     NO-UNDO.

   /* ��६ ���� ��� */
   RUN GetAcctClassMaskString (acct.class-code, OUTPUT vMask).

   IF NOT {assigned vMask} THEN RETURN.

   /* Commented by KSV: ���ᨬ ���� ��� */
   RUN GetAcctMask(vMask,OUTPUT vTokacct,OUTPUT vTokidxs,OUTPUT vToklens).


   /* Commented by KSV: ��।��塞 ����� ��� �� ��᪥ */
   vLen = LENGTH(vTokacct).

   vAcctLen = LENGTH(acct.number).

   /* Commented by KSV: �஢�ઠ ����� ����� ��� �� ᮮ⢥�ᢨ� ��᪥ */
   IF vLen = ? OR vLen <> vAcctLen THEN
   DO:
      oOk = -1.
      RUN Fill-SysMes("","acct28","","%s=" + acct.number +
                                     "%s=" + (IF vAcctLen = ?  THEN "?" ELSE STRING(vAcctLen)) +
                                     "%s=" + (IF vLen = ?      THEN "?" ELSE STRING(vLen))).

   END.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     ������ �஢�ઠ ���
  Parameters:  acct  - ���� ��楢��� ���
               oAttr - ��ਡ�� ��楢��� ���, �� ��襤訩 ��������
               oOk   - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct:
   DEFINE PARAMETER BUFFER acct  FOR acct.
   DEFINE OUTPUT PARAMETER oAttr AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk   AS INT64    NO-UNDO INIT -1.

   DEFINE VARIABLE vOk        AS INT64    NO-UNDO.
   DEFINE VARIABLE vlOk       AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE proc-name  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vProcName  AS CHAR       NO-UNDO. /* ��楤�� ��⮤�. */
   DEFINE VARIABLE vUpdValid  AS INT64        NO-UNDO. /* ��� �訡��. */
   DEFINE VARIABLE vLocalCur  AS CHAR       NO-UNDO.
   DEFINE VARIABLE vMask      AS CHAR       NO-UNDO.
   DEFINE VARIABLE sTokidx    AS CHAR       NO-UNDO.
   DEFINE VARIABLE sToklen    AS CHAR       NO-UNDO.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER op-entry FOR op-entry.
   DEFINE BUFFER bacct    FOR acct.
   DEFINE BUFFER banks    FOR banks.

   IF NOT AVAILABLE acct THEN RETURN.

   /* Commented by KSV: �஢��塞 ��� 2-�� ���浪�  */
   RUN Check-Acct-Bal-Acct(acct.bal-acct,
                           acct.acct-cat,
                           acct.cust-cat,
                           acct.acct,
                           OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "bal-acct".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 ������ ���  */
   RUN Check-Acct-Currency(acct.currency,
                           acct.acct-cat,
                           OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "currency".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 ���� ��� */
   RUN Check-Contr-Acct(acct.contr-acct,
                        acct.currency,
                        acct.bal-acct,
                        acct.side,
                        acct.acct-cat,
                        OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "contr-acct".
      RETURN.
   END.

   /*
   /* Commented by KSV: �஢��塞 ��� �� ᮮ⢥�ᢨ� ��᪥ */
   RUN Check-Acct-Mask(BUFFER acct,
                       OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "acct".
      RETURN.
   END.
   */

   /* Commented by KSV: �஢��塞 �ਧ��� ��⨢/���ᨢ */
   RUN Acct-Check-Side(acct.check-op,
                       acct.side,
                       OUTPUT vOk).
   IF vOk <> 0 THEN
   DO:
      oAttr = "check-op".
      RETURN .
   END.

   /* ����稬 ���� */
   RUN GetAcctClassMaskString (acct.class-code, OUTPUT vMask).
   /* Commented by Malik �� �맮�� �� ���譥� ��楤��� ��������� ��砨, ����� ��᪠ ���� �� ���������, ������塞 ᠬ� */
   RUN GetAcctMask (INPUT vMask, OUTPUT tokacct,
                    OUTPUT sTokidx, OUTPUT sToklen) NO-ERROR.      
   RUN FillAcctMask (INPUT vMask) NO-ERROR.
   /* �஢��塞 ��� 2-�� ���浪� � ��楢��� ��� �᫨ �� ���� � ��᪥ */
   /* Commented by Mixey:  29/07/2011 (0152425) ��ࠫ �஢��� ��� �����䨪��� '��᪨��⮢'.
                                                �஢��塞 �����ᮢ� ��� � ᮮ⢥��⢨� � ��᪮� �� �� �᪫��� */ 
   
   IF  INDEX(tokacct,FILL({&TOK_BAL},5)) > 0 THEN DO:
      IF ((acct.acct-cat =  "d" AND depobal) OR
           acct.acct-cat <> "d" AND
           acct.acct-cat <> "n" AND
           acct.acct-cat <> "x") AND
         INT64(SUBSTRING(STRING(acct.acct),INDEX(tokacct,{&TOK_BAL}),toklen[{&TOK_BAL_IDX}])) <> acct.bal-acct THEN
      DO:
         RUN Fill-SysMes("","acct10","","%s=" + acct.Acct +
                                        "%s=" + STRING(acct.bal-acct)).
         oAttr = "acct".
         RETURN.
      END.
   END.

   /* �஢��塞 ������ �᫨ ��� ���� � ��᪥ ��� */
   IF INDEX(tokacct,{&TOK_CUR}) > 0 THEN DO:
      vLocalCur = FGetSetting("�����悠�", ?, "{&in-NC-Code}").
      IF acct.acct-cat <> "d"                         AND
         acct.acct-cat <> "n"                         AND
         acct.acct-cat <> "x"                         AND
         (SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) <> acct.currency   AND
          acct.currency            <> ""              OR
          SUBSTRING(acct.acct,INDEX(tokacct,{&TOK_CUR}),toklen[{&TOK_CUR_IDX}]) <> vLocalCur       AND
          acct.currency            =  "") THEN
      DO:

         RUN Fill-SysMes("","acct11","","%s=" + acct.Acct +
                                        "%s=" + acct.currency).
         oAttr = "acct".
         RETURN.
      END.
   END.

   /* Commented by KSV: �஢��塞 �ਧ��� ��⨢/���ᨢ �� ���� 2-�� ���浪� */
   FIND bal-acct OF acct NO-LOCK.
   IF INDEX(bal-acct.side, acct.side) =  0 THEN
   DO:
      RUN Fill-SysMes("","acct12","","%s=" + acct.Acct      +
                                     "%s=" + bal-acct.side  +
                                     "%s=" + acct.side).
      oAttr = "side".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 ������ �� ���� 2-�� ���浪� */
   IF bal-acct.foreign-curr <> ?     AND
      bal-acct.acct-cat     <> "d"   THEN
   DO:
      IF (NOT bal-acct.foreign-curr AND acct.currency >  "") OR
         (bal-acct.foreign-curr     AND acct.currency =  "") THEN
      DO:
         RUN Fill-SysMes("","acct13","","%s=" + acct.Acct).
         oAttr = "currency".
         RETURN.
      END.
   END.

   /* Commented by KSV: �஢��塞 ���� ������ */
   FIND FIRST op-entry WHERE
      op-entry.acct-db  =      acct.acct        AND
      op-entry.currency BEGINS acct.currency    AND
      op-entry.op-date  <      acct.open-date   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE op-entry THEN
      FIND FIRST op-entry WHERE
         op-entry.acct-cr  =      acct.acct        AND
         op-entry.currency BEGINS acct.currency    AND
         op-entry.op-date  <      acct.open-date   NO-LOCK NO-ERROR.
   IF AVAILABLE op-entry THEN
   DO:
      RUN Fill-SysMes("","acct14","","%s=" + acct.Acct).
      oAttr = "open-date".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 㭨���쭮��� ����� ��� */
   IF NOT type-curracct THEN
   DO:
      IF acct.currency =  "" THEN
         FIND FIRST bacct WHERE
            bacct.acct     =  acct.acct AND
            bacct.currency >  ""        NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST bacct WHERE
            bacct.acct     =  acct.acct AND
            bacct.currency =  ""        NO-LOCK NO-ERROR.
      IF AVAILABLE bacct THEN
      DO:
         RUN Fill-SysMes("","acct16","","%s=" + acct.Acct).
         oAttr = "currency".
         RETURN.
      END.
   END.

   /* ����� ��⮤� �஢�ન ���� ���.
   ** ��⮤ �஢�ન �� ����᪠���� ��� ��⮢, ��⥣�ਨ ������ 㪠���� � �� ���砍�⊠�� (vNoKeyCat).
   ** �᫨ �� ����, � �ய�᪠���� ⮫쪮 ���� ��� */
   IF NOT CAN-DO(IF {assigned vNoKeyCat} THEN vNoKeyCat ELSE "d",acct.acct-cat)
   THEN DO:
      vProcName = GET-CLASS-METHOD (acct.class-code, "Chkupd").
      IF vProcName <> ?
      THEN DO:
         IF NOT SearchPfile (vProcName)
         THEN DO:
            RUN Fill-SysMes (
               "", "", "0",
               "�� ���� �������� ��楤��� �஢�ન ��� " + vProcName +
               " - ��楤�� �� �������."
            ).
            RETURN.
         END.
         RUN VALUE (vProcName + ".p") (RECID (acct)).
         IF RETURN-VALUE <> "Ok"
            THEN RETURN.
      END.
      ELSE DO:
         RUN Check-Acct-Key (acct.class-code, acct.bal-acct, acct.acct, OUTPUT vUpdValid).
         IF vUpdValid <> 0
            THEN RETURN.
      END.
   END.
   /* Commented by KSV: �஢�ઠ ������ ���� */
   IF acct.cust-cat =  "�" THEN
   DO:
      FIND FIRST banks WHERE
         banks.bank-id =  acct.cust-id NO-LOCK.
      {run-meth.i '"banks"' "chkupd" '"cust-req"'} (RECID(banks)).
      IF RETURN-VALUE <> "" THEN RETURN.
   END.

   /* Commented by KSV: �஢��塞 ⨯ ���� */
   IF acct.acct-cat  <> "d" AND
      ((acct.currency =  "" AND     {assigned acct.rate-type})  OR
       (acct.currency <> "" AND NOT {assigned acct.rate-type})) THEN
   DO:
      RUN Fill-SysMes("","acct26","","%s=" + acct.Acct      +
                                     "%s=" + acct.rate-type +
                                     "%s=" + acct.currency).
      oAttr = "rate-type".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 ���ࠧ������� ��� */
   RUN CheckBranch IN h_brnch (acct.branch-id).
   IF RETURN-VALUE <> "" THEN
   DO:
      oAttr = "branch-id".
      RETURN.
   END.

   /* Commented by KSV: �஢��塞 �������� ������ ��� */
   RUN custools.p(acct.cust-cat,
                  acct.cust-id,
                  OUTPUT vlOk).
   IF vlOk THEN
   DO:
      oAttr = "cust-id".
      RETURN .
   END.

   /* Commented by KSV: �஢�ઠ ��ਮ�� ����⢨� ��� �⭮�⥫쭮 ��ਮ��
   ** ����⢨� ��� 2-�� ���浪� */
   vDate = DATE(GetXattrValue("bal-acct",STRING(acct.bal-acct),"open-date")) NO-ERROR.

   IF vDate > acct.open-date THEN
   DO:
      RUN Fill-SysMes("","acct30","", "%s=" + acct.acct +
                                      "%s=" + STRING(acct.open-date) +
                                      "%s=" + STRING(vDate)).
      oAttr = "open-date".
      RETURN.
   END.

   vDate = DATE(GetTempXattrValue("bal-acct",
                                  STRING(acct.bal-acct),
                                  "close-date")) NO-ERROR.
   IF vDate < acct.open-date THEN
   DO:
      RUN Fill-SysMes("","acct31","", "%s=" + acct.acct +
                                      "%s=" + STRING(acct.open-date) +
                                      "%s=" + STRING(vDate)).
      oAttr = "open-date".
      RETURN.
   END.


   IF acct.close-date <> ? THEN
   DO:
      IF vDate < acct.close-date THEN
      DO:
         RUN Fill-SysMes("","acct32","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(vDate)).
         oAttr = "close-date".
         RETURN.
      END.

      /* Commented by KSV: �஢��塞 ����稥 �஢���� ��᫥ ���� ������� */
      FIND LAST op-entry WHERE
         op-entry.acct-db   =      acct.acct       AND
         op-entry.currency  BEGINS acct.currency   AND
         NOT op-entry.op-status BEGINS '�'         AND
         op-entry.op-date   <> ?                   AND
         op-entry.op-date   >      acct.close-date NO-LOCK NO-ERROR.
      IF NOT AVAILABLE op-entry THEN
         FIND LAST op-entry WHERE
            op-entry.acct-cr   =      acct.acct       AND
            op-entry.currency  BEGINS acct.currency   AND
            NOT op-entry.op-status BEGINS '�'         AND
            op-entry.op-date   <> ?                   AND
            op-entry.op-date   >      acct.close-date NO-LOCK NO-ERROR.
      IF AVAILABLE op-entry THEN
      DO:
         RUN Fill-SysMes("","acct15","","%s=" + acct.Acct).
         oAttr = "close-date".
         RETURN.
      END.

      /* Commented by KSV: �஢��塞 ����稥 ����楯⮢����� �஢���� */
      FIND FIRST op-entry WHERE
         op-entry.acct-db  = acct.acct       AND
         op-entry.currency = acct.currency   AND
         op-entry.op-status < CHR(251)       AND
         NOT op-entry.op-status BEGINS '�'   AND
         op-entry.op-date   <> ?             
         USE-INDEX entry-db NO-LOCK NO-ERROR.
      IF NOT AVAIL op-entry THEN
         FIND FIRST op-entry WHERE
            op-entry.acct-cr  = acct.acct       AND
            op-entry.currency = acct.currency   AND
            op-entry.op-status < CHR(251)       AND
            NOT op-entry.op-status BEGINS '�'   AND
            op-entry.op-date   <> ?             
            USE-INDEX entry-cr NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN
      DO:
         FIND FIRST op OF op-entry NO-LOCK.
         RUN Fill-SysMes("","acct33","", "%s=" + acct.acct +
                                         "%s=" + IF op-entry.op-date <> ?
                                                 THEN STRING(op-entry.op-date)
                                                 ELSE STRING(op.doc-date)).
         oAttr = "close-date".
         RETURN.
      END. /* if avail op-entry then do: */

      /* Commented by KSV: �஢�ઠ ���⪮� � ��� �������� �� ���� */
      RUN acct-pos IN h_base (acct.acct,acct.currency,acct.close-date,?,'�').

      IF sh-bal <> 0 THEN
      DO:
         RUN Fill-SysMes("","acct34","", "%s=" + acct.acct +
                                         "%s=" + STRING(sh-bal) +
                                         "%s=" + (IF lastmove <> ?
                                                  THEN "�� " + STRING(lastmove)
                                                  ELSE "")).
         oAttr = "close-date".
         RETURN.
      END. /* if sh-bal <> 0 then do: */

      IF acct.close-date < lastmove THEN
      DO:
         RUN Fill-SysMes("","acct35","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(lastmove)).
         oAttr = "close-date".
         RETURN.
      END.

      IF acct.open-date > acct.close-date THEN
      DO:
         RUN Fill-SysMes("","acct36","", "%s=" + acct.acct +
                                         "%s=" + STRING(acct.close-date) +
                                         "%s=" + STRING(acct.open-date)).
         oAttr = "close-date".
         RETURN.
      END.
   END.

   oOk = 0.
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �஢���� ���� ���
  Parameters:  iClass   - ����� ���
               iBalAcct - ��� 2-�� ���浪�
               iAcct    - ��楢�� ���
               oOk      - 0 - �� ��
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE Check-Acct-Key:
   DEFINE INPUT  PARAMETER iClass   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iAcct    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oOk      AS INT64    NO-UNDO.

   DEFINE VARIABLE vProc   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKey    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctKey AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMask    AS CHARACTER  NO-UNDO.

   /* Commented by Malik �� �맮�� �� ���譥� ��楤��� ��������� ��砨, ����� ��᪠ ���� �� ���������, ������塞 ᠬ� */
   IF NOT {assigned tokacct} THEN DO:          
     vMask = GetXattrInit(iClass, "acct").
     IF NOT {assigned vMask} THEN RETURN.
     RUN FillAcctMask (INPUT vMask) NO-ERROR.
   END.
   /*Commented by Malik ��������� �᫮���, ��� ��������������� ⠪�� �訡��:
    ** Starting position for SUBSTRING, OVERLAY, etc. must be 1 or greater.*/
   IF my-key <> ? AND NOT CAN-DO(no-key, STRING(iBalAcct)) AND INDEX(tokacct,{&TOK_KEY}) > 0 THEN
   DO:
      vProc = GET-CLASS-METHOD(iClass, "U1").
      IF vProc = ? THEN keyprog = "key-tst".
      RUN VALUE(keyprog + ".p") (iAcct, my-key, OUTPUT vKey).
   /* Commented by Malik ������� ������ �஢�ન, ���� ����� ���� �� ⮫쪮 9-� ᨬ�����, �� ��易⥫쭮 1 ���, �஢��塞 �� ��᪥ */           
      vAcctKey = SUBSTRING(iAcct,INDEX(tokacct,{&TOK_KEY}),toklen[{&TOK_KEY_IDX}]).
      vKey = STRING(vKey, FILL ("9", toklen[{&TOK_KEY_IDX}])).
      IF vAcctKey <> vKey THEN
      DO:
         pick-value = "no".
         RUN Fill-SysMes("","acct29","", "%s=" + (IF iAcct = ? THEN "?" ELSE iAcct) +
                                         "%s=" + (IF vKey = ?  THEN "?" ELSE vKey) +
                                         "%s=" + (IF vAcctKey = ? THEN "?" ELSE vAcctKey)).
         IF pick-value <> "yes" OR GetCode("�����", "acct29") =  "-1" THEN
         DO:
            oOk = -1.
            RETURN .
         END.
      END.
   END.
END PROCEDURE.

/* ��ନ஢���� ������������ ���.
** ��� �ਫ��᪨� ��� �������� �ନ����� �� ���� name-short. */
PROCEDURE GetNameAcctExCorp.
   DEF PARAM BUFFER acct FOR acct.
   DEF OUTPUT PARAM oNameAcct AS CHAR   NO-UNDO.

   DEF VAR name AS CHAR EXTENT 2  NO-UNDO.

   DEF BUFFER cust-corp FOR cust-corp. /* ���������� ����. */
   IF acct.cust-cat <> "�"
   THEN DO:
      {getcust.i
         &name    = name
         &Offinn  = {comment}
      }
      oNameAcct = TRIM (NAME [1] + " " + NAME [2]).
   END.
   ELSE DO:
      IF NOT {assigned acct.details}
      THEN FOR FIRST cust-corp WHERE
         cust-corp.cust-id =  acct.cust-id
      NO-LOCK:
         oNameAcct = cust-corp.name-short.
      END.
      ELSE oNameAcct = acct.details.
   END.

   RETURN.
END PROCEDURE.

/* ��楤�� 㤠����� ���稪� (��⮤ U4).*/
PROCEDURE DelAcin.
   DEF INPUT  PARAM in-acct AS RECID  NO-UNDO.

   DEF VAR i         AS INT64    NO-UNDO.
   DEF VAR ch        AS CHAR   NO-UNDO.
   DEF VAR st        AS CHAR   NO-UNDO.

   DEF BUFFER acct            FOR acct.            /* ���������� ����. */
   DEF BUFFER bis-temp-table  FOR bis-temp-table.  /* ���������� ����. */

   BLCK_MAIN:
   DO
   ON ERROR  UNDO BLCK_MAIN, LEAVE BLCK_MAIN
   ON ENDKEY UNDO BLCK_MAIN, LEAVE BLCK_MAIN:

      FIND FIRST acct WHERE
         RECID (acct) =  in-acct
      NO-LOCK NO-ERROR.
      IF NOT AVAIL acct
         THEN LEAVE BLCK_MAIN.

      DO i = 1 TO LENGTH (tokacct):
         ch = SUBSTR (tokacct,i,1).
         IF ch =  "�"
            THEN st = st + SUBSTR (acct.acct, i, 1).
      END.
      FIND FIRST bis-temp-table WHERE
         bis-temp-table.surr  =  STRING (acct.bal-acct)  + "," +
                                 STRING (acct.currency)  + "," +
                                 (IF toklen[{&TOK_BRANCH_IDX}] <> 0
                                    THEN acct.branch-id
                                    ELSE dept.branch)    + "," +
                                 st
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bis-temp-table
         THEN DELETE bis-temp-table.
   END.
   RETURN.
END PROCEDURE.

/* ���� �㡫����� ���⪠ �� ���� �� ����. */
PROCEDURE GetAcctPos.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* �����⥫� �� ����� ���. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* ��� ���᪠ ���⪠. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* ���祭�� ���⪠. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* ��� ���⪠. */

   DEF BUFFER acct-pos FOR acct-pos. /* ���������� ����. */
                        /* ���� �㡫����� ���⪠. */
   FIND LAST acct-pos WHERE
            acct-pos.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
      AND   acct-pos.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
      AND   acct-pos.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* ��ନ஢���� ���⪠. */
   IF AVAIL acct-pos
   THEN ASSIGN
      oPos  = acct-pos.balance
      oDate = acct-pos.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* ���� ����⭮�� ���⪠ �� ���� �� ����. */
PROCEDURE GetAcctCur.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* �����⥫� �� ����� ���. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* ��� ���᪠ ���⪠. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* ���祭�� ���⪠. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* ��� ���⪠. */

   DEF BUFFER acct-cur FOR acct-cur. /* ���������� ����. */
                        /* ���� ����⭮�� ���⪠. */
   FIND LAST acct-cur WHERE
            acct-cur.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
      AND   acct-cur.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
      AND   acct-cur.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* ��ନ஢���� ���⪠. */
   IF AVAIL acct-cur
   THEN ASSIGN
      oPos  = acct-cur.balance
      oDate = acct-cur.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* ���� ���⪠ �� acct-qty �� ����. */
PROCEDURE GetAcctQty.
   DEF INPUT  PARAM iHAcct AS HANDLE NO-UNDO.   /* �����⥫� �� ����� ���. */
   DEF INPUT  PARAM iDate  AS DATE   NO-UNDO.   /* ��� ���᪠ ���⪠. */
   DEF OUTPUT PARAM oPos   AS DEC    NO-UNDO.   /* ���祭�� ���⪠. */
   DEF OUTPUT PARAM oDate  AS DATE   NO-UNDO.   /* ��� ���⪠. */

   DEF BUFFER acct-qty FOR acct-qty. /* ���������� ����. */
                        /* ���� ����⭮�� ���⪠. */
   FIND LAST acct-qty WHERE
             acct-qty.acct     =  STRING (iHAcct:BUFFER-FIELD("acct")    :BUFFER-VALUE)
       AND   acct-qty.currency =  STRING (iHAcct:BUFFER-FIELD("currency"):BUFFER-VALUE)
       AND   acct-qty.since    <= iDate
   NO-LOCK NO-ERROR.
                        /* ��ନ஢���� ���⪠. */
   IF AVAIL acct-qty
   THEN ASSIGN
      oPos  = acct-qty.qty
      oDate = acct-qty.since
   .
   ELSE ASSIGN
      oPos  = ?
      oDate = ?
   .
   RETURN.
END PROCEDURE.

/* ��楤�� ������ ���� ���.
** �ॡ���� ���������� ���ᨢ toklen. */
PROCEDURE RecalcKey:
   DEF INPUT  PARAM iClassCode   AS CHAR   NO-UNDO. /* ����� ��ꥪ�. */
   DEF INPUT  PARAM iBalAcct     AS INT64    NO-UNDO. /* ��� ��ண� ���浪�. */
   DEF INPUT  PARAM iNumber      AS CHAR   NO-UNDO. /* ����� ��� ��� �஢�ન. */
   DEF OUTPUT PARAM oNumber      AS CHAR   NO-UNDO. /* ��४��祢��� ���. */

   DEF VAR vKey      AS INT64    NO-UNDO. /* ���� ����. */
   DEF VAR vKeyOff   AS CHAR   NO-UNDO. /* ��᪠ �����ᮢ�� ��⮢ ��� ������ �� �������� ����. */
   DEF VAR vKeyOn    AS CHAR   NO-UNDO. /* ������� �� ���� � ���. */
   DEF VAR vKeyProg  AS CHAR   NO-UNDO. /* �ணࠬ�� ���� ����. */
   DEF VAR vMask     AS CHAR   NO-UNDO. /* ��᪠ ��� */

   ASSIGN
      vKeyOff  = FGetSetting ("���砍��", ?, "")
      vKeyOn   = FGetSetting ("����",     ?, "")
      oNumber  = iNumber
   .
                        /* ����祭�� ��楤��� ������ ���� ���. */
   RUN ACGetKeyProg (iClassCode, OUTPUT vKeyProg).
   /* Commented by Malik �� �맮�� �� ���譥� ��楤��� ��������� ��砨, ����� ��᪠ ���� �� ���������, ������塞 ᠬ� */
   vMask = GetXattrInit(iClassCode, "acct").
   RUN FillAcctMask (INPUT vMask) NO-ERROR.

   IF  toklen[{&TOK_KEY_IDX}]  > 0
   AND vKeyOn                 <> ""
   AND NOT CAN-DO(vKeyOff,STRING(iBalAcct))
   THEN DO:
      IF SearchPfile (vKeyProg) =  ?
      THEN DO:
         RUN Fill-SysMes IN h_tmess (
            "", "", "1",
            "�� ���� �������� ��楤��� ���� ���� " + keyprog + " - ��楤�� �� �������."
         ).
         RETURN.
      END.
      RUN VALUE (vKeyProg + ".p") (oNumber, vKeyOn, OUTPUT vKey).
      IF vKey <> ?
          THEN RUN SetAcct (INPUT-OUTPUT oNumber, {&TOK_KEY}, STRING(vkey, FILL ("9", toklen[{&TOK_KEY_IDX}]))).
   END.
   RETURN.
END PROCEDURE.

/* �஢�ઠ ���� User-Id �� ᮧ����� / ।���஢���� ���. */
PROCEDURE ChkUpdUser-Id.
   DEF INPUT  PARAM iUserId AS CHAR   NO-UNDO. /* ��� ���짮��⥫�. */

   DEF VAR vSlaves   AS CHAR   NO-UNDO. /* ���᮪ ���稭�����. */
   DEF VAR vErr      AS CHAR   NO-UNDO. /* ����� �訡��. */
                        /* �஢�ઠ �����⢫���� �᫨ ��⠭������ ��࠭�祭��
                        ** �� ��ᬮ�� �� ���㤭����. */
   IF getThisUserXAttrValue("��ᬮ������") =  "��"
   THEN DO:
      vSlaves = getSlaves () + "," + USERID ("bisquit").
      IF NOT CAN-DO (vSlaves, iUserId)
         THEN vErr = "����㤭�� ~"" + iUserId + "~" �� ���� ��訬 ���稭����.".
   END.
   RETURN vErr.
END PROCEDURE.
/* $LINTFILE='pp-sacct-api-bs.i' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='29/06/2017 11:21:34.460+03:00' */
/*prosignLqeZ7Twt0CXuo2xoFfAi3A*/