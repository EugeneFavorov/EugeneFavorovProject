/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: leg321p.fun (leg161p.fun)
      Comment: �뤠� ४����⮢ ������ �� ������
         Uses:
      Used BY:
               c321cou.p
               leg321e1.p
               leg321e2.p
               leg321ex_dml.p
               leg321p.p
               leg321qry.p
               legal321_mark.p
      Created: 
     Modified: 29.06.11 vpa 0148539
     Modified: 24.11.08 ler 0098338 - Legal207. ��������� 321-� (� 01.01.2009)
     Modified: 27.12.09 ler 0104584 - ��.321-�.Legal321. ����� ������ ����� (�.�. branch.�����_��)
     Modified: 16.03.09 ler 0104921 - ��.��ᯮ�� ���㬥�⮢ � DBF-䠩� � 01.01.09
                                      ��ࠡ�⠭ ���� ����� Legal321 ��� �����প� �ॡ������ ���㧪� � ������� � 01.01.09
     Modified: 05.11.09 ler 0119683 - ���⪠ �� �㦥���� ᨬ����� �����: VD_2, BP, PRIM*, DESCR*, NameU.
     Modified: 29.03.10 ler 0125739 - ��।������ փࠦ����⢠� � Legal321
     Modified: 13.04.10 ler 0126500 - ��. ��������� �����⬠ ��� ����� �SD� (���) � �VD1� (����� ���㬥��)
*/
/******************************************************************************/
&IF DEFINED(LEG321P-FUN-DEF) =  0 &THEN
{intrface.get lgadr}   
{intrface.get brnch} /* pp-brnch.p �����㬥��� ��� ࠡ��� � ⠡��窮� BRANCH */
{globals.i}
{clntcntr.def}
{clntcntr.fun}
{leg207c.ext}
{leg321f.i}
{intrface.get tmess} 
{intrface.get tmcod} 
DEFINE VARIABLE mVO AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKODCR AS CHARACTER NO-UNDO. /* VPA 0148539 */
DEFINE VARIABLE mIs643 AS LOGICAL INIT TRUE NO-UNDO. /* VPA 0148539 �᫨ TRUE � ��楤��� �� �������� */

FUNCTION DelWordsBenName RETURN CHAR (INPUT iName  AS CHARACTER) FORWARD.

FUNCTION GetValueTmp RETURNS CHARACTER PRIVATE 
   (INPUT iFile AS CHARACTER,
    INPUT iSurr AS CHARACTER,
    INPUT iCode AS CHARACTER,
    INPUT iDate AS DATE,
    INPUT iDefaultValue AS CHARACTER) FORWARD.

FUNCTION GetCustNameFormattedTMP RETURNS CHARACTER PRIVATE 
  (INPUT iCustCat  AS CHARACTER,
   INPUT iCustId   AS INT64,
   INPUT iDate     AS DATE) FORWARD.

FUNCTION fGetPRU CHARACTER (iValue-LegTerr AS CHARACTER):
   RETURN "0".                 /* �� ����� �६�� 80263
II. ������� ������ ���������� ���� �PRU� � ࠧ१� ���⭨��� ����樨:
1) �᫨ ���� �VO� = 7001, �
- ࠧ�� 1 ���� �PRU� �ਭ����� ���祭�� "1",
- � ࠧ�鸞� 2-6 ���� �PRU� ���⠢����� �᫮, 㪠������ � �.�. LegTerr,  ���⠢������ �� ��������� ��������. ��᫮, 㪠�뢠���� � ࠧ�鸞� 2-6 �����PRU� , ��室���� � ���祭�� �.�. LegTerr  ��᫥ ��ண� �� � �� �窨, � ������ ��稭����� � �� ���� �஬� �0� (�.�. LegTerr = *-*-000�᫮.0000000).
�ਬ��: 
�� ���⭨�� ����樨  �.�. LegTerr = TERR-LIST2-0000002.0 - �� ������� ���⭨�� �PRU� = 12
2) �᫨ ���� �VO� <> 7001, �  �PRU� = 0.
�ਬ�砭��. �.�. LegTerr ����஫������ - �᫨ ���⮩/������ ���.४. LegTerr �� ������ -
⠪�� ��⠭���������� "0".
*/
/* -------------------------------------------------------------------------- */
/*   IF mVO NE "7001" THEN                                                      */
/*      RETURN "0".                                                             */
/*   IF iValue-LegTerr = "" OR                                                  */
/*      NUM-ENTRIES(iValue-LegTerr, "-") NE 3 OR                                */
/*      NUM-ENTRIES(ENTRY(3, iValue-LegTerr, "-"), ".") NE 2                    */
/*   THEN                                                                       */
/*      RETURN "0".  /* ���⮩/������ ���.४. LegTerr �� ������ */          */
/*                                                                              */
/*   RETURN "1" + LEFT-TRIM(ENTRY(1, ENTRY(3, iValue-LegTerr, "-"), ".") , "0").*/
END FUNCTION.

FUNCTION fChk_country_signs CHARACTER (iCountry-Id AS CHARACTER):

   IF GetXAttrValueEx("country",iCountry-Id ,"�������⨥���","")   <> ""
   THEN
      RETURN "1".

   IF GetXAttrValueEx("country",iCountry-Id ,"�����_�ਧ���","")    <> ""
   THEN
      RETURN "2".

   IF GetXAttrValueEx("country",iCountry-Id ,"�ந�����","")      <> ""
   THEN
      RETURN "3".
   
RETURN "".

END FUNCTION.


FUNCTION fFindCountry LOGICAL (iCountry-Id AS CHARACTER):
   DEFINE VARIABLE vRetVal AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vSign   AS CHARACTER   NO-UNDO.
   DEFINE BUFFER country FOR country. 

   FIND FIRST country WHERE
              country.country-alt-id =  INT64(SUBSTRING(iCountry-Id,1,3))
   NO-LOCK NO-ERROR.
   IF    AVAIL country  
   THEN
   DO:
      vSign   = fChk_country_signs(country.country-id).
      vRetVal =  vSign <> "".  
                

   END.

   RETURN vRetVal.

END FUNCTION.  


FUNCTION fGetPRU_new CHARACTER (iID       AS INT64,
                                iCat      AS CHARACTER,
                                iCient    AS LOGICAL,
                                iKod      AS CHARACTER,
                                iDopKod   AS CHARACTER,
                                iKodCNB   AS CHARACTER
                                ):
   DEFINE VARIABLE vCntr   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vClass  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPRU    AS CHARACTER   INIT "0"   NO-UNDO.
   DEFINE VARIABLE vTerr   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodCN  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodCR  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodCNB AS CHARACTER   NO-UNDO.
                                                
   DEFINE BUFFER code          FOR code.
   DEFINE BUFFER bMainDataLine FOR DataLine.

   IF    iKod BEGINS "70"
     AND iCient   
   THEN
   DO:
      CASE iCat :
         WHEN "�" THEN vClass = "person".
         WHEN "�" THEN vClass = "cust-corp".
         WHEN "�" THEN vClass = "banks".
      END CASE. 
                                                              
      vTerr = ENTRY(1,GetXattrValueEx(vClass,STRING(iID),"LegTerr","" )).
      IF vTerr <> "" THEN
      DO:
         FIND FIRST code WHERE
                    code.class =  "TerrBlack"
                AND code.code  =  ENTRY(1,vTerr)
         NO-LOCK NO-ERROR.
         IF AVAIL code THEN  
            vPRU = "1" + code.description[3].
         END.
      END.

   ELSE IF   CAN-DO("30*",iKod) 
          OR CAN-DO("30*",iDopKod) 
   THEN DO:   
      vCodCR = GetChckAttr(iID,iCat,"KodCR").
      vPRU = IF fFindCountry(vCodCR) THEN "1" + vCodCR ELSE "".
    
      IF NOT {assigned vPRU} THEN
      DO:
         vCodCN = GetChckAttr(iID,iCat,"KodCN").
         vPRU = IF fFindCountry(vCodCN) THEN "2" + vCodCN ELSE "".
      END.
               
      IF    NOT {assigned vPRU} 
        AND iKodCNB <> "0" THEN
      DO:
         IF fChk_country_signs(iKodCNB) <> "" THEN
            vPRU = "3" + iKodCNB. 
      END.
      IF NOT {assigned vPRU} THEN
         vPRU = "0".
   END.  

   RETURN vPRU. 
END FUNCTION.


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateSendDataEx:
   DEFINE INPUT  PARAMETER ipOP       AS INT64 NO-UNDO.
   DEFINE OUTPUT PARAMETER opClntSurr AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER opCntrSurr AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER opOpKind   AS INT64 NO-UNDO.
   DEFINE INPUT  PARAMETER ipMessMode AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vLeave AS LOGICAL    NO-UNDO INIT NO.
   IF GetSysConf("SpecSEND") =  "CNTR" THEN
   DO:
      opCntrSurr = "0" + CHR(1) + "0" + CHR(1) + "0".
      RETURN .
   END.
   IF GetSysConf("SpecSEND") =  "OURBANK" THEN
   DO:   
      RUN normdbg IN h_debug (0,"�����","���⥫�騪�� �� ����樨 " + vSurr + " ���� ��� ����").
      opClntSurr = "�," + STRING(vSELFBankID) + "," .
      RETURN .
   END.

   /* ��窠 �室� ��� ����஭��� ��楤��� ��।������ ���⥫�騪� */
   IF LOOKUP ("CreateSendDataExternal", THIS-PROCEDURE:INTERNAL-ENTRIES) >  0 
   THEN RUN CreateSendDataExternal (       ipOp,
                                           ipMessMode,
                                    OUTPUT opClntSurr,
                                    OUTPUT opCntrSurr,
                                    OUTPUT opOpKind,
                                    OUTPUT vLeave).
   IF vLeave AND opOpKind =  {&CLNT-NO} THEN DO:
      RUN normdbg IN h_debug (0,"�訡��","���������� ��।����� ���⥫�騪� ��� ����樨 " + vSurr).
      RETURN "no".
   END.
   IF vLeave THEN RETURN "".

/* IF NOT vLeave THEN */
   IF vDocKindKAS THEN
      opOpKind = DefineReceiver(ipOp,ipMessMode).    /* �����⥫�            */
   ELSE
      opOpKind = DefineSender (ipOp,ipMessMode).     /* ���⥫�騪            */

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CreateSendDataEx","opOpKind = " + string(opOpKind)).
   &ENDIF

   IF   opOpKind =  {&CLNT-NO} 
     OR opOpKind =  {&CNTR-NO}   
   THEN DO:
      IF     AVAIL bAcctDb 
        AND  bAcctDb.side =  "�"
        AND  CAN-DO(vContrMBR,bAcctDb.contract)  

      THEN
      DO:
         opClntSurr = bAcctDb.Cust-Cat         + "," + 
                      STRING(bAcctDb.Cust-ID)  + "," + 
                      bAcctDb.acct.
         RETURN.
      END.
      ELSE 
      IF    AVAIL bAcctCr 
        AND bAcctCr.side =  "�"
        AND CAN-DO(vContrMBR,bAcctCr.contract)  
      THEN
      DO:
         opClntSurr = bAcctCr.Cust-Cat         + "," + 
                      STRING(bAcctCr.Cust-ID)  + "," + 
                      bAcctDb.acct.
         RETURN.
      END.
   END.

   CASE opOpKind:
      WHEN {&CLNT-NO}    THEN DO:
         RUN normdbg IN h_debug (0,"�訡��","���������� ��।����� ���⥫�騪� ��� ����樨 " + vSurr).
         RETURN "no".
      END.
      WHEN {&BANK-DEBET}  OR
      WHEN {&BANK-ACTDB}  OR
      WHEN {&BANK-CREDIT} THEN DO:
         RUN normdbg IN h_debug (0,"�����","���⥫�騪�� �� ����樨 " + vSurr + " ���� ��� ����").
         opClntSurr = "�," + string(vSELFBankID) + "," + bAcctChk.acct.
      END.
      WHEN {&CLNT-DEBET}  OR
      WHEN {&CLNT-ACTDB}  OR
      WHEN {&CLNT-SEND}   OR
      WHEN {&CLNT-CREDIT} THEN DO:
         opClntSurr = bAcctChk.Cust-Cat + "," + string(bAcctChk.Cust-ID) + "," + bAcctChk.acct.
      END.
      WHEN {&CNTR-SEND}  THEN DO:
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateSendDataEx","CNTR-SEND").
         &ENDIF
         opCntrSurr = GetXAttrValue("op",string(bop.op),"acct-send")    + chr(1) +
                      GetXAttrValue("op",string(bop.op),"name-send")    + chr(1) +
                      GetXAttrValue("op",string(bop.op),"inn-send")     + chr(1) +
                      GetXAttrValue("op",string(bop.op),"country-send") + chr(1) +
                      GetSendBank  (bop.op,vDocKind,ipMessMode).
      END.
      WHEN {&CNTR-BEN}  THEN DO:
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateSendDataEx","CNTR-BEN").
         &ENDIF
         opCntrSurr = CheckEmpty(bop.ben-acct)                    + chr(1) +
                      DelWordsBenName(CheckEmpty(bop.name-ben))   + chr(1) +
                      CheckEmpty(bop.inn)                         + chr(1) +
                      GetXAttrValue("op",string(bop.op),"country-send") + chr(1) +
                      GetSendBank  (bop.op,vDocKind,ipMessMode).
      END.
      WHEN {&CNTR-NO} THEN DO:
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateSendDataEx","CNTR-NO").
         &ENDIF
      END.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CreateSendDataEx","opClntSurr = " + opClntSurr +
                                     " opCntrSurr = " + opCntrSurr).
   &ENDIF

   RETURN "".
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateSendData:
   DEFINE INPUT  PARAMETER ipOP       AS INT64 NO-UNDO.
   DEFINE OUTPUT PARAMETER opClntSurr AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER opCntrSurr AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER ipMessMode AS LOGICAL NO-UNDO.

   DEFINE VAR vOpKind AS INT64 NO-UNDO.

   RUN CreateSendDataEx (INPUT  ipOP,
                         OUTPUT opClntSurr,
                         OUTPUT opCntrSurr,
                         OUTPUT vOpKind,
                         INPUT  ipMessMode).
   RETURN RETURN-VALUE.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateRecvDataEx:
   DEFINE INPUT  PARAMETER ipOP       AS INT64         NO-UNDO.
   DEFINE OUTPUT PARAMETER opClntSurr AS CHAR    INIT "" NO-UNDO.
   DEFINE OUTPUT PARAMETER opCntrSurr AS CHAR    INIT "" NO-UNDO.
   DEFINE OUTPUT PARAMETER opOpKind   AS INT64         NO-UNDO.
   DEFINE INPUT  PARAMETER ipMessMode AS LOGICAL         NO-UNDO.

   DEFINE VARIABLE vLeave AS LOGICAL NO-UNDO INIT NO.

   IF GetSysConf("SpecRECV") =  "CNTR" THEN
   DO:
      opCntrSurr = "0" + CHR(1) + "0" + CHR(1) + "0".
      RETURN.
   END.
   IF GetSysConf("SpecRECV") =  "OURBANK" THEN
   DO:   
      RUN normdbg IN h_debug (0,"�����","�����⥫�� �� ����樨 " + vSurr + " ���� ��� ����").
      opClntSurr = "�," + string(vSELFBankID) + "," .
      RETURN opClntSurr.
   END.

   /* ��窠 �室� ��� ����஭��� ��楤��� ��।������ �����⥫� */
   IF LOOKUP ("CreateRecvDataExternal", THIS-PROCEDURE:INTERNAL-ENTRIES) >  0 
   THEN RUN CreateRecvDataExternal (       ipOp,
                                           ipMessMode,
                                    OUTPUT opClntSurr,
                                    OUTPUT opCntrSurr,
                                    OUTPUT opOpKind,
                                    OUTPUT vLeave).
   IF vLeave AND opOpKind =  {&CLNT-NO} THEN DO:
      RUN normdbg IN h_debug (0,"�訡��","���������� ��।����� �����⥫� ��� ����樨 " + vSurr).
      RETURN "no".
   END.
   IF vLeave AND GetEntries(3, opClntSurr, ",", "") =  "00000000000000000000" THEN
      RUN normdbg IN h_debug (0,"�訡��","��� �����⥫� �� ��।���� - ��� ����樨 " + vSurr).
   IF vLeave THEN RETURN "".

/*    IF NOT vLeave THEN */
   IF vDocKindKAS THEN
      opOpKind = DefineSender (ipOp,ipMessMode).     /* ���⥫�騪            */
   ELSE
      opOpKind = DefineReceiver(ipOp,ipMessMode).    /* �����⥫�            */



   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CreateRecvDataEx","opOpKind = " + string(opOpKind)).
   &ENDIF

   IF   opOpKind =  {&CLNT-NO} 
     OR opOpKind =  {&CNTR-NO}
   THEN DO:
      IF     AVAIL bAcctDb
        AND  bAcctDb.side =  "�"
        AND CAN-DO(vContrMBR,bAcctDb.contract)  
      THEN
      DO:
         opClntSurr = bAcctDb.Cust-Cat         + "," + 
                      STRING(bAcctDb.Cust-ID)  + "," + 
                      bAcctDb.acct.
         RETURN.
      END.
      ELSE 
      IF    AVAIL bAcctCr
        AND bAcctCr.side =  "�"
        AND CAN-DO(vContrMBR,bAcctCr.contract)  
      THEN
      DO:
         opClntSurr = bAcctCr.Cust-Cat         + "," + 
                      STRING(bAcctCr.Cust-ID)  + "," + 
                      bAcctDb.acct.
         RETURN.
      END.
   END.

   CASE opOpKind:
      WHEN {&CLNT-NO}    THEN DO:
         RUN normdbg IN h_debug (0,"�訡��","���������� ��।����� �����⥫� ��� ����樨 " + vSurr).
         RETURN "no".
      END.
      WHEN {&BANK-DEBET}  OR
      WHEN {&BANK-CREDIT} OR
      WHEN {&BANK-ACTCR}  THEN DO:
         RUN normdbg IN h_debug (0,"�����","�����⥫�� �� ����樨 " + vSurr + " ���� ��� ����").
         opClntSurr = "�," + string(vSELFBankID) + "," + bAcctChk.acct.
      END.
      WHEN {&CLNT-CREDIT} OR
      WHEN {&CLNT-ACTCR}  OR
      WHEN {&CLNT-DEBET}  OR
      WHEN {&CLNT-RECV}   THEN DO:
         opClntSurr = bAcctChk.Cust-Cat + "," + string(bAcctChk.Cust-ID) + "," + bAcctChk.acct.
      END.
      WHEN {&CNTR-SEND}  THEN DO:
         opCntrSurr = GetXAttrValue("op",string(bop.op),"acct-rec")     + chr(1) +
                      GetXAttrValue("op",string(bop.op),"name-rec")     + chr(1) +
                      GetXAttrValue("op",string(bop.op),"inn-rec")      + chr(1) +
                      GetXAttrValue("op",string(bop.op),"country-rec")  + chr(1) +
                      GetRecvBank  (bop.op,vDocKind,ipMessMode).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateRecvDataEx","CNTR-SEND").
         &ENDIF
      END.
      WHEN {&CNTR-BEN}  THEN DO:
         opCntrSurr = CheckEmpty(bop.ben-acct)                       + chr(1) +
                      DelWordsBenName(CheckEmpty(bop.name-ben))      + chr(1) +
                      CheckEmpty(bop.inn)                            + chr(1) +
                      GetXAttrValue("op",string(bop.op),"country-rec")  + chr(1) +
                      GetRecvBank  (bop.op,vDocKind,ipMessMode).
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateRecvDataEx","CNTR-BEN").
         &ENDIF
      END.
      WHEN {&CNTR-NO} THEN DO:
         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("CreateRecvDataEx","CNTR-NO").
         &ENDIF
      END.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("CreateRecvDataEx","opClntSurr = " + opClntSurr +
                                     " opCntrSurr = " + opCntrSurr).
   &ENDIF

   RETURN "".
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE CreateRecvData:
   DEFINE INPUT  PARAMETER ipOP       AS INT64 NO-UNDO.
   DEFINE OUTPUT PARAMETER opClntSurr AS CHAR    NO-UNDO.
   DEFINE OUTPUT PARAMETER opCntrSurr AS CHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER ipMessMode AS LOGICAL NO-UNDO.

   DEFINE VAR vOpKind AS INT64 NO-UNDO.

   RUN CreateRecvDataEx (INPUT  ipOP,
                         OUTPUT opClntSurr,
                         OUTPUT opCntrSurr,
                         OUTPUT vOpKind,
                         INPUT  ipMessMode).
   RETURN RETURN-VALUE.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*       ������� �஢�७��� ���祭�� ��ਡ�� ���⭨�� ���⮢            */
/*----------------------------------------------------------------------------*/
FUNCTION  GetChckAttr RETURN CHAR (INPUT ipCustID  AS INT64,
                                   INPUT ipCustCat AS CHAR,
                                   INPUT ipRqst    AS CHAR):
   DEFINE VAR vBuffer AS CHAR NO-UNDO.


   IF ipCustID =  0 AND ipCustCat =  "" AND CAN-DO("AMR,ADRESS", ipRqst) THEN 
      RETURN  "0~n" + "0~n" + "0~n" + "0~n" + "0~n" + "0~n" + "0~n". 

   IF ipCustID =  0 AND ipCustCat =  "" THEN 
      RETURN (IF NOT CAN-DO("AMR,ADRESS", ipRqst) 
                 AND CAN-DO("VD_3,VD_6,VD_7,MC_2,MC_3,GR", ipRqst)
              THEN /* EmptyDate(today) */ "01/01/2099"
              ELSE "0").

   vBuffer = GetCustAttr(ipCustID,ipCustCat,ipRqst).
   IF NOT CAN-DO("AMR,ADRESS", ipRqst) THEN  /* ,MC_ */
      vBuffer = ClearExtSym(vBuffer).

   RETURN (IF vBuffer =  "" OR vBuffer =  ? 
           THEN 
             (IF NOT CAN-DO("AMR,ADRESS", ipRqst)
                 AND CAN-DO("VD_3,VD_6,VD_7,MC_2,MC_3,GR", ipRqst)
              THEN "01/01/2099"
              ELSE "0")
           ELSE vBuffer).

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*       �����頥� ���祭�� ��ਡ�⮢ ���⨭�� ���⮢                     */
/*----------------------------------------------------------------------------*/
FUNCTION  GetCustAttr RETURN CHAR (INPUT ipCustID  AS INT64,
                                   INPUT ipCustCat AS CHAR,
                                   INPUT ipRqst    AS CHAR):

   RUN GetCustBuffer (INPUT ipCustID,
                      INPUT ipCustCat).

   IF RETURN-VALUE <> "" THEN
      RETURN "".

   IF INDEX(ipRqst, "?KODCR") > 0 THEN
      RETURN GetKODCR-(ipCustID, ipCustCat, ipRqst).

   CASE ipCustCat:
      WHEN "�" THEN RETURN GetCustCorpAttr(ipRqst).
      WHEN "�" THEN RETURN GetCustPersAttr(ipRqst).
      WHEN "�" THEN RETURN GetCustBankAttr(ipRqst).
      WHEN "�" THEN RETURN GetCustInnrAttr(ipRqst).
      OTHERWISE     RETURN "".
   END.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*  �����頥� ���祭�� ��ਡ�⮢ �ਤ��᪮�� ���                           */
/*----------------------------------------------------------------------------*/
FUNCTION GetCustCorpAttr RETURN CHAR (INPUT ipRqst AS CHAR):
   DEFINE VAR vSurr    AS CHAR NO-UNDO.
   DEFINE VAR vReg     AS CHAR NO-UNDO.
   DEFINE VAR vDate    AS CHAR NO-UNDO.
   DEFINE VAR vPredp   AS LOG  NO-UNDO.
   DEFINE VAR vType    AS CHAR NO-UNDO.
   DEFINE VAR vCard    AS CHAR NO-UNDO.
   DEFINE VAR vOGRN    AS CHAR NO-UNDO.        
   DEFINE VAR vName     AS CHAR NO-UNDO.        

&SCOPED-DEFINE gTestCustGr getXattrValueEx("cust-corp", STRING(bCust.Cust-ID), "country-id2", bCust.Country-ID)
/* &SCOPED-DEFINE gTestCustGr bCust.Country-ID */
   vPredp =  getXAttrValue ("cust-corp",STRING (bCust.cust-id),"�।��") >  "".

   CASE ipRqst:
      WHEN "TU"     THEN RETURN IF vPredp 
                                THEN "3" 
                                ELSE "1".
      WHEN "PRU"    THEN RETURN fGetPRU(GetXAttrValueEx("cust-corp", STRING(bCust.cust-id), "LegTerr", "")).
      WHEN "NAMEU"  THEN DO:
         IF NOT CAN-DO(FGetSetting("�⠭���","����ᔋ��",""),bCust.cust-stat)
            AND NOT vPredp THEN
            vName = GetCustNameFormattedTmP("�", bCust.cust-id,gend-date). 
         ELSE
            vName = GetValueTmp("cust-corp", 
                                STRING(bCust.cust-id),
                                "Name-Corp",
                                gend-date,
                                bCust.name-corp).
         RETURN ClearExtSym(vName).
      END.
      WHEN "KODCR"  THEN RETURN f-KODC(      "�����",  "�", bCust.cust-id).
      WHEN "KODCN"  THEN 
         IF vPredp THEN
         DO:
            vType = {&gTestCustGr}.
            FIND country WHERE country.country-id =  vType NO-LOCK NO-ERROR.
            RETURN IF AVAIL country 
                   THEN STRING(country.country-alt-id, "999") + "00"
                   ELSE "". /* "0" + "00". */
         END.
         ELSE
            RETURN f-KODC (     "�������", "�", bCust.cust-id).
      WHEN "AMR"    THEN RETURN f-AMR_ADRESS("�����",   "�", bCust.cust-id). /* ��� �� ��� � ����� */
      WHEN "ADRESS" THEN RETURN f-AMR_ADRESS("�������", "�", bCust.cust-id).
      WHEN "KD"     THEN RETURN IF vPredp  
                                THEN GetCodeMisc("�������", 
                                     getXAttrValue("cust-corp",STRING(bCust.Cust-ID), "Document-ID"), 6)
                                ELSE "0".
      WHEN "SD"     THEN RETURN IF vPredp 
                                THEN f-SD(getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "Document",    ""),
                                         getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "document-id", ""))
                                ELSE bCust.Okpo.
      WHEN "RG"     THEN DO:
           vOGRN = GetXAttrValueEx("cust-corp",
                                   STRING(bCust.cust-id),
                                   "������������", "").
                  RETURN IF {assigned vOGRN} THEN
                            vOGRN
                         ELSE
                            getXattrValueEx("cust-corp", STRING(bCust.Cust-ID), "����", 
                                getXattrValueEx("cust-corp", STRING(bCust.Cust-id), "RegNum", "")).
      END.
      WHEN "VD_1"   THEN RETURN IF vPredp 
                                THEN f-VD_1(getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "Document",    ""),
                                            getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "document-id", ""))
                                ELSE "".
      WHEN "VD_2"   THEN RETURN IF vPredp
                                THEN ClearExtSym(getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "issue", ""))
                                ELSE "".
      WHEN "VD_3"   THEN RETURN IF vPredp 
                                THEN getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "Document4Date_vid", "01/01/2099")
                                ELSE "01/01/2099".
      WHEN "VD_4"   THEN 
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN ""
                   ELSE GetCodeEx("VisaType", getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "VisaType", ""), "").
/* GetCodeMisc("�������", getXAttrValue("cust-corp", STRING(bCust.Cust-ID), "VisaType"), 6). */
         ELSE 
            RETURN "".
      WHEN "VD_5"   THEN
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN ""
                   ELSE REPLACE(getXAttrValue("cust-corp", STRING(bCust.Cust-ID), "VisaNum"),
                                " ", "").
         ELSE 
            RETURN "".
      WHEN "VD_6"   THEN
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN "01/01/2099"
                   ELSE getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "�����ࠢ�ॡ�", "01/01/2099").
         ELSE 
            RETURN "01/01/2099".
      WHEN "VD_7"   THEN
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN "01/01/2099"
                   ELSE getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "�����ࠢ�ॡ��", "01/01/2099").
         ELSE 
            RETURN "01/01/2099".
      WHEN "MC_1"        THEN
         IF vPredp  THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN ""
                   ELSE REPLACE(getXAttrValue("cust-corp", STRING(bCust.Cust-ID), "��������"), " ", "").
         ELSE 
            RETURN "".
      WHEN "MC_2"   THEN
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN "01/01/2099"
                   ELSE getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "�����ॡ뢑", "01/01/2099").
         ELSE 
            RETURN "01/01/2099".
      WHEN "MC_3"   THEN
      DO:
         IF vPredp THEN
            RETURN IF {&gTestCustGr} =  "RUS" OR {&gTestCustGr} =  ?     /* १����� */
                   THEN "01/01/2099"
                   ELSE getXAttrValueEx("cust-corp", STRING(bCust.Cust-ID), "�����ॡ뢏�", "01/01/2099").
         ELSE 
            RETURN "01/01/2099".
      END.
      WHEN "BP"     THEN RETURN IF vPredp 
                            THEN ClearExtSym(getXattrValueEx("cust-corp", STRING(bCust.Cust-id), "BirthPlace", ""))
                            ELSE "".
      WHEN "ND"     THEN DO:
             /* VPA 0148539 */
         mKODCR = f-KODC(      "�����",  "�", bCust.cust-id).
                 IF TRIM(mKODCR) BEGINS "643" THEN mIs643 = TRUE. /* VPA 0148539 ��⠥� �� ��஬� */
                 ELSE mIs643 = FALSE.
         /* ---------- */            
                RETURN FormatINN(bCust.Inn,IF vPredp 
                                                    THEN "3" 
                                                    ELSE "1").
      END.
      WHEN "GR"     THEN DO:
         /* ���砫� ��६ �� ���������⠑ */
         vDate = GetXAttrValueEx("cust-corp",
                                 STRING (bCust.cust-id),
                                 "���������⠑","").
         IF NOT {assigned vDate} THEN
           vDate = GetXAttrValue("cust-corp",
                                 STRING (bCust.cust-id),
                                 IF vPredp
                                 THEN "BirthDay"
                                 ELSE "RegDate").
         IF vDate =  ""
            THEN RETURN "". /* {&E-DATE}. */
            ELSE RETURN vDate.
      END.
      WHEN "ACC_B"  THEN RETURN bAcctChk.acct.
      WHEN "RESRV"  THEN IF vPredp 
                         THEN
                         DO:
                            IF bCust.Country-ID =  "RUS" OR
                               bCust.Country-ID =  ?     THEN       /* १�����   */
                               RETURN "0".
                            ELSE DO:                                /* ��१����� */
                               vCard = GetXAttrValue("cust-corp",
                                                     STRING (bCust.Cust-ID),
                                                    "��������").
                               IF NOT {assigned vCard} 
                               THEN RETURN "".
                               ELSE RETURN "1" + " " + vCard.
                            END.
                         END.
                         ELSE RETURN "".
      WHEN "GRIP"   THEN RETURN IF vPredp 
                                THEN GetXAttrValueEx("cust-corp", STRING (bCust.Cust-ID), "��⠂�।",
                                     GetXAttrValueEx("cust-corp", STRING (bCust.Cust-ID), "RegDate", "01/01/2099"))
                                ELSE "01/01/2099".
      WHEN "CNTFACT" THEN RETURN GetFactCntr(bCust.Cust-ID,  "�").
   END CASE.

   RETURN "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*   �����頥� ���祭�� ��ਡ�⮢ 䨧��᪮�� ���                           */
/*----------------------------------------------------------------------------*/
FUNCTION GetCustPersAttr RETURN CHAR (INPUT ipRqst AS CHAR):
   DEFINE VAR vSurr     AS CHAR  NO-UNDO.
   DEFINE VAR vCard     AS CHAR  NO-UNDO.
   DEFINE VAR vType     AS CHAR  NO-UNDO.
   DEFINE VAR vPredp           AS LOG   NO-UNDO.
   DEFINE VAR vOGRN                AS CHAR  NO-UNDO.
   
&SCOPED-DEFINE gTestPersGr getXattrValueEx("person", STRING(bPers.person-id), "country-id2", bPers.Country-ID)
   vPredp = getXattrValue("person", STRING(bPers.person-id), "�।��") >  "".

   CASE ipRqst:
      WHEN "TU"     THEN RETURN IF GetXAttrValue("person", STRING(bPers.person-id), "�।��") BEGINS "�।"
                                THEN "3"
                                ELSE "2".
      WHEN "PRU"    THEN RETURN fGetPRU(GetXAttrValueEx("person", STRING(bPers.person-id), "LegTerr", "")).
      WHEN "NAMEU"  THEN RETURN 
         ClearExtSym(GetValueTmp("person", 
                                 STRING(bPers.person-id), 
                                 "Name-Last",
                                 gend-date,
                                 bPers.name-last)
            + " " + GetValueTmp("person", 
                                STRING(bPers.person-id),
                                "first-names",
                                gend-date,
                                bPers.first-names)).

      WHEN "KODCR"  THEN RETURN f-KODC ("����ய", "�", bPers.person-id).
      WHEN "KODCN"  THEN
      DO:
         vSurr = IF GetXAttrValueEx("person", STRING(bPers.person-id), "�����_����", "") <> "" 
                 THEN "01"
                 ELSE "".
         vSurr = IF vSurr =  "" 
                 THEN (IF GetXAttrValue("person", STRING(bPers.person-id), "�⥯�����_����") <> ""
                       THEN "02"
                       ELSE "00"
                      )
                 ELSE vSurr.
/*       FIND country WHERE country.country-id EQ bPers.country-id NO-LOCK NO-ERROR. */
         vType =  {&gTestPersGr}.
         FIND country WHERE country.country-id =  vType NO-LOCK NO-ERROR.
         RETURN IF AVAIL country 
                THEN STRING(country.country-alt-id, "999") + vSurr
                ELSE "0" + vSurr.
      END.
      WHEN "AMR"    THEN RETURN f-AMR_ADRESS("����ய", "�", bPers.person-id).
      WHEN "ADRESS" THEN RETURN f-AMR_ADRESS("�������", "�", bPers.person-id).
      WHEN "KD"     THEN RETURN GetCodeMisc("�������", bPers.Document-ID, 6).
/* getXAttrValue("person", STRING(bPers.person-id), "Document-ID"), 6). */
      WHEN "SD"     THEN RETURN f-SD(bPers.Document, bPers.document-id).
      WHEN "RG"     THEN
         RETURN IF vPredp 
                THEN getXattrValueEx("person", STRING(bPers.person-id), "����", 
                     getXattrValueEx("person", STRING(bPers.person-id), "RegNum", ""))
                ELSE "".
      WHEN "VD_1" THEN RETURN f-VD_1(bPers.Document, bPers.document-id).
      WHEN "VD_2" THEN RETURN ClearExtSym(bPers.issue). /* getXAttrValueEx("person", STRING(bPers.person-id), "issue", ""). */
      WHEN "VD_3" THEN 
      DO:         
         FIND FIRST cust-ident WHERE cust-ident.cust-code =  bPers.document AND 
                                     cust-ident.cust-code-type =  bPers.document-id
         NO-LOCK NO-ERROR.
         RETURN IF AVAIL cust-ident
                THEN STRING(cust-ident.open-date, "99/99/9999")
                ELSE "01/01/2099".
      END.
      WHEN "VD_4" THEN
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN ""
                ELSE GetCodeEx("VisaType", getXAttrValueEx("person", STRING(bPers.person-id), "VisaType", ""), "").
      WHEN "VD_5" THEN 
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN ""
                ELSE REPLACE(getXAttrValue("person", STRING(bPers.person-id), "VisaNum"), " ", "").
      WHEN "VD_6" THEN 
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN "01/01/2099"
                ELSE getXAttrValueEx("person", STRING(bPers.person-id), "�����ࠢ�ॡ�", "01/01/2099").
      WHEN "VD_7" THEN
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN "01/01/2099"
                ELSE getXAttrValueEx("person", STRING(bPers.person-id), "�����ࠢ�ॡ��", "01/01/2099").
      WHEN "MC_1" THEN
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN ""
                ELSE REPLACE(getXAttrValue("person", STRING(bPers.person-id), "��������"), " ", "").
      WHEN "MC_2" THEN
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN "01/01/2099"
                ELSE getXAttrValueEx("person", STRING(bPers.person-id), "�����ॡ뢑", "01/01/2099").
      WHEN "MC_3" THEN 
         RETURN IF {&gTestPersGr} =  "RUS" OR {&gTestPersGr} =  ?     /* १����� */
                THEN "01/01/2099"
                ELSE getXAttrValueEx("person", STRING(bPers.person-id), "�����ॡ뢏�", "01/01/2099").
      WHEN "BP"   THEN RETURN ClearExtSym(getXattrValueEx("person", STRING(bPers.person-id), "BirthPlace", "")).
      WHEN "ND"   THEN
      DO: 
         mKODCR = f-KODC ("����ய", "�", bPers.person-id).
         IF TRIM(mKODCR) BEGINS "643" THEN mIs643 = TRUE.
                 ELSE mIs643 = FALSE.        
         RETURN FormatINN(bPers.Inn,  /* "2" */
                              (IF GetXAttrValue("person", STRING(bPers.person-id), "�।��") BEGINS "�।"
                              THEN "3"
                              ELSE "2")).
      END.
      WHEN "GR"   THEN IF bPers.Birthday =  ?
                            THEN RETURN "". /* {&E-DATE}. */
                            ELSE RETURN string(bPers.Birthday,"99/99/9999").
/* ??? GetCustPersAttr("ACC_B") �� ��. */
      WHEN "ACC_B"  THEN RETURN bAcctChk.acct.
      WHEN "RESRV"  THEN 
      DO:
         IF bPers.Country-ID =  "RUS" OR bPers.Country-ID =  ?      /* १�����   */
         THEN
            RETURN "0".
         ELSE DO:                                                   /* ��१����� */
            vCard = GetXAttrValue("person",
                                  string(bPers.Person-ID),
                                  "��������").
            IF NOT {assigned vCard} THEN RETURN "".
            RETURN "1" + " " + vCard.
         END.
      END.
      WHEN "GRIP"   THEN RETURN IF vPredp 
                                THEN getXattrValueEx("person", STRING(bPers.person-id), "��⠂�।", "01/01/2099")
                                ELSE "01/01/2099".
      WHEN "CNTFACT" THEN RETURN GetFactCntr(bPers.person-id,  "�").
   END CASE.                                     /* CASE ipRqst:              */
   RETURN "".

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*   �����頥� ���祭�� ��ਡ�⮢ ������-�����                              */
/*----------------------------------------------------------------------------*/
FUNCTION GetCustBankAttr RETURN CHAR (INPUT ipRqst AS CHAR):
   DEFINE VAR vDate AS CHAR NO-UNDO.
   DEFINE VAR vReg  AS CHAR NO-UNDO.

   CASE ipRqst:
      WHEN "TU"     THEN RETURN "1".
      WHEN "PRU"    THEN RETURN fGetPRU(GetXAttrValueEx("banks", STRING(bBank.bank-id), "LegTerr", "")).
      
      /* WHEN "NAMEU"  THEN RETURN              23.01.2018 PEO
         TRIM(GetValueTmp("banks", 
                          STRING(bBank.bank-id),
                          "Name",
                          gend-date,
                          bBank.name)).*/
                          
            
      WHEN "NAMEU"  THEN Do: IF bBank.bank-id NE 140543 then 
                                 RETURN  TRIM(GetValueTmp("banks", 
                                    STRING(bBank.bank-id),
                                    "Name",
                                    gend-date,
                                    bBank.name))  + '/��� ' + '"' + '���� ����' + '"'.
                             else RETURN TRIM(GetValueTmp("banks", 
                                    STRING(bBank.bank-id),
                                    "Name",
                                    gend-date,
                                    bBank.name)).
                            end.        
                          

/*
      WHEN "KODCR"  THEN RETURN GetCustCountry(GetXAttrValue("banks",
                                                             string(bBank.bank-id),
                                                             "country-id2")).
      WHEN "KODCN"  THEN RETURN GetCustCountry(bBank.Country-ID).
*/
      WHEN "KODCR"  THEN RETURN f-KODC(      "�����",   "�", bBank.bank-id).
      WHEN "KODCN"  THEN RETURN f-KODC(      "�������", "�", bBank.bank-id).
      WHEN "AMR"    THEN RETURN f-AMR_ADRESS("�����",   "�", bBank.bank-id).
      WHEN "ADRESS" THEN RETURN f-AMR_ADRESS("�������", "�", bBank.bank-id).
      WHEN "KD"     THEN RETURN "".
      WHEN "SD"     THEN RETURN GetXAttrValue("banks",
                                              string(bBank.bank-id),
                                              "okpo").
      WHEN "RG"     THEN RETURN GetXAttrValueEx("banks",string(bBank.bank-id), "����", 
                                GetXAttrValueEx("banks", string(bBank.bank-id), "RegNum", "")).
      WHEN "VD_1"      THEN RETURN "".
      WHEN "VD_2"      THEN RETURN "".
      WHEN "VD_3"      THEN RETURN "01/01/2099".
      WHEN "VD_4"      THEN RETURN "".
      WHEN "VD_5"      THEN RETURN "".
      WHEN "VD_6"      THEN RETURN "01/01/2099".
      WHEN "VD_7"      THEN RETURN "01/01/2099".
      WHEN "MC_1"      THEN RETURN "".
      WHEN "MC_2"      THEN RETURN "01/01/2099".
      WHEN "MC_3"      THEN RETURN "01/01/2099".
      WHEN "BP"        THEN RETURN "".
      WHEN "ND"     THEN DO:
            /* VPA 0148539 */
             mKODCR = f-KODC(      "�����",   "�", bBank.bank-id).
                 IF TRIM(mKODCR) BEGINS "643" THEN mIs643=TRUE. /* VPA 0148539 ��⠥� �� ��஬� */
                 ELSE mIs643=FALSE.
                 /* --------- */
             RETURN FormatINN(GetBankID(bBank.bank-id,"���"),"1").
          END.
      WHEN "GR"     THEN 
      DO:
         vDate = GetXAttrValue("banks",
                               string(bBank.bank-id),
                               "RegDate").
         IF vDate =  ""
            THEN RETURN "". /* {&E-DATE}. */
            ELSE RETURN vDate.
      END.
      WHEN "ACC_B"  THEN RETURN bAcctChk.acct.
      WHEN "RESRV"  THEN RETURN "".
      WHEN "GRIP"   THEN RETURN "01/01/2099".
      WHEN "CNTFACT" THEN RETURN GetFactCntr( bBank.bank-id,  "�").
   END.

   RETURN "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*       �����頥� ��ਡ��� ᮡ�⢥���� ��⮢ �����                         */
/*----------------------------------------------------------------------------*/
FUNCTION GetCustInnrAttr RETURN CHAR (INPUT ipRqst AS CHAR):
   DEFINE VAR vInn  AS CHAR NO-UNDO.
   DEFINE VAR vOkpo AS CHAR NO-UNDO.
   DEFINE VAR vName AS CHAR NO-UNDO.
   
   DEF BUFFER branch FOR branch.

   CASE ipRqst:
      WHEN "TU"     THEN RETURN "1".
      WHEN "PRU"    THEN RETURN "0".
      WHEN "NAMEU"  THEN DO:
         vName = TRIM({banknm.lf bInnr}).
         IF NOT {assigned vName} THEN
            vName = FGetSetting("����", ?, "").
         IF NOT {assigned vName} THEN DO:
            FIND FIRST branch WHERE 
                       branch.branch-id =  vBranchCode NO-LOCK NO-ERROR.
            IF AVAIL branch THEN
               vName = branch.NAME.
         END.
         RETURN vName.
      END.
      WHEN "KODCR"  THEN RETURN f-KODC(      "�����",   "�", bInnr.bank-id).
      WHEN "KODCN"  THEN RETURN f-KODC(      "�������", "�", bInnr.bank-id).
      WHEN "AMR"    THEN RETURN f-AMR_ADRESS("�����",   "�", bInnr.bank-id).
      WHEN "ADRESS" THEN RETURN f-AMR_ADRESS("�������", "�", bInnr.bank-id).
/*
      WHEN "AMR"    THEN DO:
         vOkpo = GetCountryName(bInnr.Country-ID) +
                 GetXAttrValue("banks",
                               STRING(bInnr.bank-id),
                               "RegPlace").         
         IF NOT {assigned vOkpo} THEN
            vOkpo = getBranchXAttrValue(vBranchCode,"����_��").
         RETURN vOkpo.
      END.
      WHEN "ADRESS" THEN DO:
         vOkpo = GetCountryName(bInnr.Country-ID) +
                 bInnr.town-type + " " + bInnr.town + "," + bInnr.mail-address.
         IF NOT {assigned vOkpo} THEN DO:
            vOkpo = FgetSetting("����_��",?,"").
            IF NOT {assigned vOkpo} THEN DO:
               FIND FIRST branch WHERE 
                          branch.branch-id EQ vBranchCode NO-LOCK NO-ERROR.
               IF AVAIL branch THEN
                  vOkpo = branch.address.
            END.
         END.
         RETURN vOkpo.
      END.
*/      
      WHEN "KD"     THEN RETURN "".
      WHEN "SD"     THEN 
      DO:
         vOkpo = GetXAttrValue("banks", string(bInnr.bank-id), "okpo").
         IF NOT {assigned vOkpo} THEN
            vOkpo = getBranchXAttrValue(vBranchCode,"����").
         RETURN vOkpo.
      END.
      WHEN "RG"     THEN RETURN GetXAttrValueEx("banks", STRING(bInnr.bank-id), "����", 
                                getBranchXAttrValue(vBranchCode,"����")).
      WHEN "VD_1"        THEN RETURN "".
      WHEN "VD_2"        THEN RETURN "".
      WHEN "VD_3"        THEN RETURN "01/01/2099".
      WHEN "VD_4"        THEN RETURN "".
      WHEN "VD_5"        THEN RETURN "".
      WHEN "VD_6"        THEN RETURN "01/01/2099".
      WHEN "VD_7"        THEN RETURN "01/01/2099".
      WHEN "MC_1"        THEN RETURN "".
      WHEN "MC_2"        THEN RETURN "01/01/2099".
      WHEN "MC_3"        THEN RETURN "01/01/2099".
      WHEN "BP"          THEN RETURN "".
      WHEN "ND"     THEN DO:
         vInn = GetBankID(bInnr.bank-id,         /* �� �����䨪��஢        */
                          "���").
         IF NOT {assigned vInn} THEN             /* �� ����������           */
            vInn = getBranchXAttrValue(vBranchCode,"���").
         RETURN FormatINN(vInn,"1").             /* ��ଠ�஢���             */
      END.

      WHEN "GR"     THEN DO:
         vOkpo = GetBankID(bInnr.bank-id,         /* �� �����䨪��஢        */
                          "RegDate").
         IF vOkpo =  ""
            THEN RETURN "". /* {&E-DATE}. */
            ELSE RETURN vOkpo.

      END.
      WHEN "ACC_B"  THEN RETURN bAcctChk.acct.
      WHEN "RESRV"  THEN RETURN "".
      WHEN "GRIP"   THEN RETURN "01/01/2099".
   END.
   RETURN "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*          ������ �� ���� ��। ��ࠡ�⪮�                              */
/*----------------------------------------------------------------------------*/
PROCEDURE PrepareCustBuffer:
   RELEASE bCust.
   RELEASE bPers.
   RELEASE bBank.
   RELEASE bInnr.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*          ��⠭����� ���� ��� �⥭�� ��ਡ�⮢ ������                     */
/*----------------------------------------------------------------------------*/
PROCEDURE GetCustBuffer:
   DEFINE INPUT PARAMETER ipCustID  AS INT64 NO-UNDO.
   DEFINE INPUT PARAMETER ipCustCat AS CHAR    NO-UNDO.

   CASE ipCustCat:
      WHEN "�" THEN DO:
         IF NOT AVAILABLE(bCust)
            THEN FIND FIRST bCust WHERE bCust.cust-id   =  ipCustID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(bCust)
            THEN RETURN "BAD".
      END.
      WHEN "�" THEN DO:
         IF NOT AVAILABLE(bPers)
            THEN FIND FIRST bPers WHERE bPers.person-id =  ipCustID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(bPers)
            THEN RETURN "BAD".
      END.
      WHEN "�" THEN DO:
         IF NOT AVAILABLE(bBank)
            THEN FIND FIRST bBank WHERE bBank.bank-id   =  ipCustID NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(bBank)
            THEN RETURN "BAD".
      END.
      WHEN "�" THEN DO:
         IF NOT AVAILABLE bInnr THEN
            FIND FIRST bInnr WHERE               /* �� ��אַ� ��뫪�          */
                       bInnr.bank-id =  branch.bank-id NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(bInnr) THEN DO:        /* �� ���� ���-9 � �����.   */
            {getbank.i bInnr vBankCode ""���-9""}
         END.
         IF NOT AVAILABLE(bInnr) THEN
            RETURN "BAD".
      END.
      OTHERWISE RETURN "BAD".
   END.
   RETURN "".
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetBankCode RETURN CHAR (BUFFER iBanks FOR banks,
                                  INPUT  iCode   AS  CHAR):
   FIND FIRST banks-code WHERE banks-code.bank-id        =  iBanks.bank-id
                           AND banks-code.bank-code-type =  iCode
              NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(banks-code) THEN DO:
      IF VALID-HANDLE(h_debug) THEN DO:
         RUN normdbg IN h_debug (0,"�訡��",
                                   "�� ������� �����䨪��� " + iCode).
         RUN normdbg IN h_debug (0,"","��� ����� "                + iBanks.Name).
      END.
      RETURN "0".
   END.
   ELSE
      RETURN banks-code.bank-code.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetSelfData RETURN CHAR (INPUT  ipRegNum     AS CHAR,
                                  INPUT  ipOKATO      AS CHAR,
                                  INPUT  ipBankCode   AS CHAR,
                                  INPUT  ipBranch     AS CHAR /* branch.branch-id */):
/* DEFINE BUFFER banks FOR banks. */
   DEFINE BUFFER branch FOR branch.
   DEFINE VAR vBankData AS CHAR        NO-UNDO.
   DEFINE VAR vREGN     AS CHAR        NO-UNDO.
   DEFINE VAR oTrnz     AS LOGICAL     NO-UNDO.
   DEFINE VAR vOKATO    AS CHAR        NO-UNDO.
   DEFINE VAR vBankCode AS CHAR        NO-UNDO.
   DEFINE VAR vType     AS CHAR        NO-UNDO.
   DEFINE VAR vCode     AS CHAR        NO-UNDO.

   DEFINE VAR vMainREGN AS CHAR        NO-UNDO.  /* REGN */
   DEFINE VAR vMainINN  AS CHAR        NO-UNDO.  /* ND_KO */
   DEFINE VAR vTmp      AS CHARACTER   NO-UNDO.
   DEFINE BUFFER banks FOR banks.

   ASSIGN     
      /*  ��� ��������� �⤥����� ��室�� �� "REGN" � "���" */
      vMainREGN = GetEntries(1, GetXAttrValueEx("branch", ipBranch, "REGN", ""), "/", "0")
      vMainINN  = GetXAttrValueEx("branch", ipBranch, "���",  "")
      oTrnz     = (FGetSetting("Legal207","�࠭���","���") =  "��")
      vREGN     = GetEntries(1,ipRegNum,"/","0")
      vBankCode = FGetSetting("Legal207","�࠭��⁈�",?)
      vBankData = FILL(",",9)
   .
   &IF DEFINED(p3063u) &THEN
   FIND branch WHERE branch.Branch-Id =  ipBranch NO-LOCK NO-ERROR.
   IF AVAIL branch THEN
      oTrnz     = IF        branch.Branch-Type =  "10"  THEN NO ELSE YES.
   &ENDIF
/*
- ք�� ��।�� � �������� �⤥������ 
�KTU_S�: �� ��.������� ���� �������� �⤥����� � ����� ��� 
   ���祭�� �.�. branch.�����_�� , 
   �᫨ �� �������� � branch.���������
�KTU_SS�: ���祭�� �.�. branch.�����_��, 
   �᫨ �� �������� � �.�. branch.��������� (䨫����, �ନ���饣� ������ �����

- ք�� ��।�� � ����
�KTU_S�: ���祭�� �.�. branch.�����_��, �᫨ �� �������� � �.�. branch.��������� 
      (䨫����, �ନ���饣� ������ �����), �᫨ ���. ४�. �� ���������, � - �� ���������  
�KTU_SS�: ���⠢���� �0�
*/    

   IF oTrnz THEN DO: /* 䨫��� ��।��� �� ᠬ - ��।�� � ������. �⤥����� */
       {getbank.i banks vREGN "'REGN'" }
        IF AVAIL banks THEN vTmp = GetXAttrValueEx("banks", string(banks.bank-id), "region", "").
        ELSE vTmp = "".
         ASSIGN    
            vTmp =  IF vTmp <> "" THEN SUBSTRING(vTmp,max(LENGTH(vTmp) - 1,1)) ELSE ""
            ENTRY(1,vBankData) = vMainREGN                                            /* 1 REGN    */
            ENTRY(2,vBankData) = vMainINN                                             /* 2 ND_KO   */
/*          ENTRY(3,vBankData) = GetXAttrValueEx("branch", vTmp, "���������", "")     /* 3 KTU_S   */ */
            ENTRY(3,vBankData) =  vTmp  /* ������ �⤥����� KTU_S  */
            ENTRY(4,vBankData) = IF AVAIL banks THEN GetBankCode(BUFFER banks,"���-9") ELSE ""   /* 4 BIC_S   */
            ENTRY(5,vBankData) = "0"                                                  /* 5 NUMBF_S */
/*          ENTRY(6,vBankData) = ipOKATO                                              /* 6 KTU_SS  */ */
            ENTRY(6,vBankData) = SUBSTRING(  /* 䨫����, �ନ�. �����. ����� */
                                 GetXAttrValueEx("branch", ipBranch, "�����_��",      /* 6 KTU_SS  */
                                 GetXAttrValueEx("branch", ipBranch, "���������", "")
                                                ), 1, 2
                                          )
            ENTRY(7,vBankData) = GetXAttrValueEx("branch", ipBranch, "�������",  "")  /* 7 BIK_SS  */
            ENTRY(8,vBankData) = GetEntries(2, ipRegNum, "/", "0")                    /* 8 NUMBF_SS*/
/*          GetEntries(2, GetBankCode(BUFFER banks,"REGN"), "/", "0") */
            ENTRY(9,vBankData) = "1".
         .
   END.
   ELSE                               /* 䨫��� ��।��� ᠬ - ��।�� � ��� */
   DO:
      FIND branch WHERE branch.Branch-Id =  ipBranch.
      ASSIGN 
         ENTRY(1,vBankData) = vMainREGN 
         ENTRY(2,vBankData) = vMainINN  
/*       ENTRY(3,vBankData) = ipOKATO */
         ENTRY(3,vBankData) = SUBSTRING(  /* 䨫����, �ନ�. �����. ����� */
                              GetXAttrValueEx("branch", ipBranch, "�����_��",    
                              GetXAttrValueEx("branch", ipBranch, "���������", 
                                              FGetSetting("���������", ?, "") )
                                             ), 1, 2
                                       )
         ENTRY(4,vBankData) = GetXAttrValueEx("branch", ipBranch, "�������", FGetSetting("�������", ?, ""))
         ENTRY(5,vBankData) = GetEntries(2, GetXAttrValueEx("branch", branch.Branch-Id, "REGN", ""), "/", "0") 
         ENTRY(6,vBankData) = "0"
         ENTRY(7,vBankData) = "0"
         ENTRY(8,vBankData) = "0"
         ENTRY(9,vBankData) = "0"
      .
   END.

   RETURN vBankData.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetCountryName RETURN CHAR (INPUT iCountryID AS CHAR):
   DEFINE VAR vCountryID AS CHAR NO-UNDO.

   vCountryID = GetCustCountry(iCountryID).
   IF vCountryID <> "0" THEN
      FIND FIRST country WHERE
                 country.country-alt-id =  INT64(SUBSTRING(vCountryID,1,3))
                 NO-LOCK NO-ERROR.
   ELSE
      RELEASE country.

   RETURN IF AVAILABLE(country)
             THEN TRIM(country.country-name) + ","
             ELSE "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetCustCountry RETURN CHAR (INPUT ipCountryID AS CHAR):
   IF ipCountryID =  "" OR
      ipCountryID =  ?  THEN ipCountryID = "RUS".
   FIND FIRST country WHERE country.country-id =  ipCountryID NO-LOCK NO-ERROR.
   RETURN IF AVAILABLE(country)
             THEN string(country.country-alt-id,"999") + "00"
             ELSE "0".
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetCntrChrByInt RETURN CHAR (INPUT iCountryID AS CHAR):

   FIND FIRST country WHERE
              country.country-alt-id =  INT64(iCountryID)
              NO-LOCK NO-ERROR.

   RETURN IF AVAILABLE(country)
             THEN country.country-id
             ELSE ?.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetBankID RETURN CHAR (INPUT ipBankID AS INT64,
                                INPUT ipCode   AS CHAR):
   DEFINE BUFFER banks-code FOR banks-code.
   IF ipCode =  "���" THEN
      RETURN GetBankInn ("bank-id", STRING (ipBankId)).
   ELSE
   DO:
      FIND FIRST banks-code WHERE banks-code.bank-id        =  ipBankID
                              AND banks-code.bank-code-type =  ipCode
                                  NO-LOCK NO-ERROR.
      IF AVAILABLE(banks-code)
         THEN RETURN banks-code.bank-code.
         ELSE RETURN "".
   END.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION TestINN RETURN LOGICAL (INPUT ipINN  AS CHAR,
                                 INPUT ipType AS CHAR):
DEFINE VAR vINN      AS CHAR INIT ""   NO-UNDO.
DEFINE VAR vFormat   AS CHAR           NO-UNDO.
/* VPA 0148539 �᫨ KODCR �� ��稭����� � ��� 643, � mIs643=TRUE � ��⠥� ��-��஬�*/
/* ���� ��� �����⨬�� ���祭�� 5-� ��� 10-⨧��筮� �᫮ */

IF mIs643 THEN DO: /* VPA 0148539 ��⠥� ��-��஬�*/
   IF LENGTH(ipINN) <> 12 AND LENGTH(ipINN) <> 10 THEN
      RETURN FALSE.
END.
ELSE DO:
   mIs643 = TRUE. /* �� ��砩 �᫨ �뫥�� */
   IF LENGTH(ipINN) <> 10 AND LENGTH(ipINN) <> 5 AND ipType <> "2" THEN
      RETURN FALSE.
      
   mIs643 = FALSE. /* �����頥� �।��饥 ���祭�� */
END.
IF mIs643 THEN vFormat = IF LENGTH(ipINN) = 12 THEN "999999999999" /* 12 */ ELSE "9999999999". /* 10 */ 
ELSE
DO:
   IF ipType = "2" THEN
      vFormat = "999999999999".
   ELSE    
      vFormat = IF LENGTH(ipINN) = 10 THEN "9999999999" /* 10 */ ELSE "99999". /* 5 */
   
END.    
   vINN    = STRING(DECIMAL(ipINN), vFormat) NO-ERROR.
/* vINN:
   ?     - ipINN ������� vFormat
   ""    - ipINN ����஢�� ᮤ�ন���
   "0.." - ipINN ���� vFormat
*/
IF mIs643 THEN
   IF SUBSTRING(vINN, 1, 1) =  "?" OR SUBSTRING(vINN, 1, 1) =  "" 
      OR (ipType = "1" /* ��.��� */ AND LENGTH(ipINN) <> 10) 
   THEN
      RETURN FALSE.

IF NOT mIs643 THEN DO:
   mIs643=TRUE.
   IF ( SUBSTRING(vINN, 1, 1) =  "?" AND ipType <> "2") OR
      ( SUBSTRING(vINN, 1, 1) =  "" AND ipType <> "2" ) OR 
      (ipType = "1" /* ��.��� */ AND LENGTH(ipINN) <> 10 AND LENGTH(ipINN) <> 5 ) 
   THEN
      RETURN FALSE.
END.
   RETURN TRUE.
END.
/*----------------------------------------------------------------------------*/
FUNCTION FormatINN RETURN CHAR (INPUT ipINN  AS CHAR,
                                INPUT ipType AS CHAR):
DEFINE VAR vAddTxt1  AS CHAR         NO-UNDO.
DEFINE VAR vAddTxt2  AS CHAR         NO-UNDO.
   CASE ipType:
      WHEN "1" THEN    /* �ਤ��. ���, ������-����, "���" ���� (ᮡ��. ���) */
         ASSIGN
            vAddTxt1 = "��� ��. ���"
            vAddTxt2 = "10".
      WHEN "2" THEN    /* 䨧.��� (����� ��� -  12 ᨬ�����) */
         ASSIGN
            vAddTxt1 = "��� 䨧. ���"
            vAddTxt2 = "12".
      WHEN "3" THEN    /* 䨧.���-�।��. (����� ��� - 10/12 ᨬ�����) */ 
         ASSIGN
            vAddTxt1 = "��� 䨧. ���-�।��."
            vAddTxt2 = "10/12".
      OTHERWISE        /* 4 ��஭��� ���⭨� (����� ���� 䨧��殬) 10/12 ᨬ����� */        
         ASSIGN
            vAddTxt1 = "��� ��஭���� ��-��"
            vAddTxt2 = "10/12".
   END CASE.
   IF NOT TestINN(ipINN, ipType) THEN
      RUN OutErrMsg ("������ ��� " + vAddTxt1 + " ~"" + ipINN + "~" - �.�. " + vAddTxt2 + " ���.").

   RETURN ipINN.
END.
/*----------------------------------------------------------------------------*/
PROCEDURE OutErrMsg:
   DEFINE INPUT PARAMETER iErrMsg AS CHARACTER NO-UNDO.
   IF VALID-HANDLE(h_debug)
      THEN RUN normdbg IN h_debug (0, "�訡��", iErrMsg).
      ELSE MESSAGE COLOR ERROR iErrMsg VIEW-AS ALERT-BOX ERROR.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION ClearExtSym RETURN CHAR (INPUT ipDirty AS CHAR):
   ipDirty = replace(ipDirty, CHR(10), " ").
   ipDirty = replace(ipDirty, CHR(13), " ").
   ipDirty = replace(ipDirty, CHR(9) , " ").
   RETURN ipDirty.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION DateToDBF RETURN CHAR (INPUT ipDate AS DATE):
   RETURN (IF ipDate =  ?
              THEN FILL(" ",8)
              ELSE STRING(YEAR (ipDate),"9999") +
                   STRING(MONTH(ipDate),"99"  ) +
                   STRING(DAY  (ipDate),"99"  )).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION DBFToDate RETURN DATE (INPUT ipDate AS CHAR):
   RETURN DATE(SUBSTRING(ipDate,7,2) + "/" +
               SUBSTRING(ipDate,5,2) + "/" +
               SUBSTRING(ipDate,1,4)).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CurrencyOp RETURN CHAR (INPUT ipCurrency AS CHAR):
   RETURN IF ipCurrency      =  "" OR
             CheckGold(ipCurrency)
             THEN "643"                 /* ?????????????????????????????????? */
             ELSE ipCurrency.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CheckEmpty RETURN CHAR (INPUT ipValue AS CHAR):
   RETURN IF ipValue =  ? THEN "" ELSE ipValue.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION EmptyDate RETURN CHAR (INPUT ipDate AS DATE):
   RETURN FILL(" ",8).
/*   {&E-DATE}.*/
/*
   return "2099" +
           string(month(ipDate),"99") +
           string(day  (ipDate),"99").
*/
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetDefaultCause RETURN CHAR (INPUT iDefault AS CHAR):

   DEFINE BUFFER code FOR code.
   FOR FIRST code WHERE
             code.class =  "���।��⎃��"
         AND {assigned code.val}
             NO-LOCK:
      RETURN code.code.
   END.

   RETURN iDefault.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* ��� ��࠭� �� 䠪��᪮�� ���� ������                                 */
/*----------------------------------------------------------------------------*/
FUNCTION GetFactCntr RETURN CHAR (INPUT ipCustID  AS INT64,
                                  INPUT ipCustCat AS CHAR):
   DEFINE BUFFER cust-iden FOR cust-iden.
   DEF VAR vKodcn AS CHAR NO-UNDO.
   FIND LAST cust-ident WHERE
            cust-ident.Class-code = "p-cust-adr"
        AND cust-ident.cust-cat   =  ipCustCat
        AND cust-ident.cust-id    =  ipCustID
        AND cust-ident.open-date  <= gend-date
        AND cust-ident.cust-code-type =  "�������"
        AND (   cust-ident.close-date =  ?
            OR cust-ident.close-date >= gend-date)
   NO-LOCK NO-ERROR.
   IF AVAIL cust-ident THEN DO:
      vKodcn = GetXattrValue("cust-ident",cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num),"country-id").
      IF vKodcn <> "" THEN
      DO:
         FIND FIRST country WHERE country.country-id =  vKodcn NO-LOCK NO-ERROR.
         IF AVAIL country THEN 
            RETURN STRING(country.country-alt-id).
      END.
   END.
RETURN "".
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �� op.name-ben 㤠�塞 ᨬ����, ����᫥��� � �� "�᪫�����������"       */
/*----------------------------------------------------------------------------*/
FUNCTION DelWordsBenName RETURN CHAR (INPUT iName  AS CHARACTER):
   
   DEFINE VARIABLE vI        AS INT64       NO-UNDO.
   DEFINE VARIABLE vName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDelWords AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTmpStr   AS CHARACTER   NO-UNDO.

   vDelWords = FGetSetting("Legal207","�᪫�����������","").

   DO vI = 1 TO NUM-ENTRIES(iName, " "):
      ASSIGN
         vTmpStr = ENTRY(vI, iName, " ")                        
         vName   = vName + (IF vTmpStr <> "" THEN (vTmpStr + " ") ELSE "") .
   END.

   vName = " " + vName + " ".
   DO vI = 1 TO NUM-ENTRIES(vDelWords):
      vTmpStr = " " + TRIM(ENTRY(vI,vDelWords)) + " ".
      IF INDEX (vName,vTmpStr) >= 0  THEN
         vName = " " + TRIM(REPLACE(vName,vTmpStr, " ")) + " ".
   END.

   RETURN TRIM(vName).

END FUNCTION.



/*----------------------------------------------------------------------------*/
/* ���������� ᢥ����� � ������ � ���. ४����⮢ ���㬥��                  */
/*----------------------------------------------------------------------------*/
FUNCTION GetChckAttrByOP CHARACTER (iOp  AS INT64,
                                    iFLd AS CHARACTER):
   
   DEFINE VARIABLE vRetVal  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTmpStr  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOPSurr  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumpp   AS INT64       NO-UNDO.
   DEFINE VARIABLE vDateTmp AS DATE        NO-UNDO.

   DEF BUFFER bfop FOR op.
   
   vOPSurr = STRING(iOp).
   CASE iFLd:
      
      WHEN "KODCR" THEN DO:
         vRetVal = GetCustCountry(GetXattrValueEx("op",vOPSurr,"country-pers","")).
      END.

      WHEN "AMR"   THEN DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"����","").
         IF {assigned vTmpStr} THEN DO:

            vRetVal = "0~n" +                                           /*�� ��*/                              
                      GetEntries(2,vTmpStr,",","0")        + "~n" +    

                      TRIM(GetEntries(3,vTmpStr,",","0")   + ","  +     
                           GetEntries(4,vTmpStr,",","0"))  + "~n" +  

                      GetEntries(5,vTmpStr,",","0")        + "~n" +  
                      GetEntries(6,vTmpStr,",","0")        + "~n" +   

                      TRIM( GetEntries(7,vTmpStr,",","0")  + ","  +        
                            GetEntries(9,vTmpStr,",","0")) + "~n" + 

                      GetEntries(8,vTmpStr,",","0") .                      
         END.
         ELSE 
            vRetVal = "0~n0~n0~n0~n0~n0~n0".
      END.

      WHEN "KODCN" THEN 
         vRetVal = "0".
              
      WHEN "ADRESS" THEN 
      DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"����","").
         IF {assigned vTmpStr} THEN DO:

            vRetVal = "0~n" + 
                      GetEntries(2,vTmpStr,",","0")        + "~n" + 

                      TRIM(GetEntries(3,vTmpStr,",","0")   + ","  + 
                           GetEntries(4,vTmpStr,",","0"))  + "~n" +  

                      GetEntries(5,vTmpStr,",","0")        + "~n" + 
                      GetEntries(6,vTmpStr,",","0")        + "~n" + 

                      TRIM( GetEntries(7,vTmpStr,",","0")  + ","  + 
                            GetEntries(9,vTmpStr,",","0")) + "~n" + 
                      GetEntries(8,vTmpStr,",","0") . 
         END.
         ELSE 
            vRetVal = "0~n0~n0~n0~n0~n0~n0".
      END.

      WHEN "KD" 
      THEN DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"document-id","").
         vRetVal = GetCodeMisc("�������",vTmpStr,6).
      END.

      WHEN "SD"   
      THEN DO:
         vRetVal = f-SD(GetXattrValueEx("op",vOPSurr,"����",""),
                        GetXattrValueEx("op",vOPSurr,"document-id","")).

      END.

      WHEN "VD_1" 
      THEN DO:
         vRetVal = f-VD_1(GetXattrValueEx("op",vOPSurr,"����",""),
                          GetXattrValueEx("op",vOPSurr,"document-id","")).

      END.

      WHEN "ND"  
      THEN DO:
         FIND FIRST bfop WHERE bfop.op =  iOp NO-LOCK NO-ERROR.
         IF AVAIL bfop THEN
         vRetVal = bfop.inn.
      END.

      WHEN "VD_2" 
      THEN DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"cust-doc-who","").
         vRetVal = TRIM( GetEntries(1,vTmpStr,",","") + "," + 
                         GetEntries(2,vTmpStr,",","")," ,").
         vRetVal = REPLACE(vRetVal, "~n" , "").


      END.
      WHEN "VD_3" 
      THEN DO:
         vTmpStr  = GetXattrValueEx("op",vOPSurr,"cust-doc-who","").
         vRetVal  = TRIM( GetEntries(3,vTmpStr,",","")).
         vDateTmp = DATE (vRetVal) NO-ERROR.
         
         IF ERROR-STATUS:ERROR OR vDateTmp =  ? THEN
             vRetVal = "01/01/2099".

      END.
      WHEN "VD_4" 
      THEN DO:
          
         vTmpStr = GetXattrValueEx("op",vOPSurr,"���㬄��","").

         IF GetEntries(1,vTmpStr,",","") =  "����抠��" THEN
            vTmpStr = GetEntries(2,vTmpStr,",","").   
         ELSE 
            vTmpStr = GetEntries(1,vTmpStr,",","").

         vRetVal = GetCodeMisc("�������",vTmpStr,6).
      END.

      WHEN "VD_5" 
      THEN DO:
         
         vTmpStr = GetXattrValueEx("op",vOPSurr,"���㬄��","").
         IF GetEntries(1,vTmpStr,",","") =  "����抠��" THEN
            vRetVal = GetEntries(2,GetXattrValueEx("op",vOPSurr,"���������",""),
                                 ",","").
         ELSE 
            vRetVal = GetEntries(1,GetXattrValueEx("op",vOPSurr,"���������",""),
                                 ",","").

      END.

      WHEN "VD_6" 
      THEN DO:
          
         vTmpStr = GetXattrValueEx("op",vOPSurr,"���㬄��","").
         IF GetEntries(1,vTmpStr,",","") =  "����抠��" THEN
            vRetVal = GetEntries(2,GetXattrValueEx("op",vOPSurr,"��⠄�����",""),
                                 ",","").
         ELSE 
            vRetVal = GetEntries(1,GetXattrValueEx("op",vOPSurr,"��⠄�����",""),
                                 ",","").
         vDateTmp = DATE (vRetVal) NO-ERROR.

         IF ERROR-STATUS:ERROR OR vDateTmp =  ? THEN
            vRetVal = "01/01/2099".


      END.


      WHEN "MC_1" 
      THEN DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"���㬄��","").
         vNumpp = LOOKUP("����抠��",vTmpStr).
         IF vNumpp >  0 THEN
           vRetVal = GetEntries(vNumpp,GetXattrValueEx("op",vOPSurr,"��������� ",""),
                                 ",","").
      END.

      WHEN "MC_2" 
      THEN DO:
         vTmpStr = GetXattrValueEx("op",vOPSurr,"���㬄��","").
         vNumpp  = LOOKUP("����抠��",vTmpStr).
         IF vNumpp >  0 THEN
           vRetVal = GetEntries(vNumpp,GetXattrValueEx("op",vOPSurr,"��⠄�����",""),
                                 ",","").
         vDateTmp = DATE (vRetVal) NO-ERROR.

         IF ERROR-STATUS:ERROR OR vDateTmp =  ? THEN
            vRetVal = "01/01/2099".
      END.
      WHEN "MC_3" OR WHEN "VD_7" 
         THEN vRetVal = "01/01/2099".

      WHEN "GR"    
      THEN DO:
         vRetVal = GetXattrValueEx("op",vOPSurr,"Birthday","").
         vDateTmp = DATE (vRetVal) NO-ERROR.

         IF ERROR-STATUS:ERROR OR vDateTmp =  ? THEN
            vRetVal = "01/01/2099".
      END.

      WHEN "BP"
      THEN DO:
         vRetVal = GetXattrValueEx("op",vOPSurr,"BirthPlace","").
      END.

   END CASE.

   vRetVal = IF NOT {assigned vRetVal} THEN "0" ELSE vRetVal.
   RETURN vRetVal.

END FUNCTION.

FUNCTION IsSpecKod CHARACTER(iOp      AS INT64,
                             iOp-date AS DATE):

   DEFINE VARIABLE vCod     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodeOne AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI       AS INT64       NO-UNDO.
   DEFINE VARIABLE vTmpStr  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vTrList  AS CHARACTER   NO-UNDO.

   DEF BUFFER bfop FOR op.

   FIND FIRST bfop WHERE 
              bfop.op =  iOp
   NO-LOCK NO-ERROR.
   IF NOT AVAIL op THEN 
      RETURN "".

   vTmpStr = GetXAttrValueEx("op",STRING(iOp),"���⥫�騪","").
   IF vTmpStr <> "" THEN
      RETURN "SEND".

   vTmpStr = GetXAttrValueEx("op",STRING(iOp),"�����⥫�","").
   IF vTmpStr <> "" THEN
      RETURN "RECV".

   vCod = GetXAttrValue("op",STRING(iOp),"��������").
   
   DO vI = 1 TO NUM-ENTRIES(vCod):
      vCodeOne = ENTRY(vI, vCod).
      vTmpStr  = getTCodeFld ("Val","����뢑���", vCodeOne + ",SEND", iOp-date). 
      vTrList  = ENTRY(2, vTmpStr,"|") NO-ERROR.
      IF CAN-DO(vTrList,bfop.op-kind) THEN
         RETURN "SEND".
      ELSE DO:
         vTmpStr = getTCodeFld ("Val","����뢑���", vCodeOne + ",RECV", iOp-date). 
         vTrList= ENTRY(2, vTmpStr,"|") NO-ERROR.
         IF CAN-DO(vTrList,bfop.op-kind) THEN
            RETURN "RECV".
      END.
   END.
   RETURN "".
END FUNCTION.

FUNCTION GetCustNameFormattedTMP RETURNS CHARACTER PRIVATE 
  (INPUT iCustCat  AS CHARACTER,
   INPUT iCustId   AS INT64,
   INPUT iDate     AS DATE):

   DEFINE VARIABLE vFormatNaim AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEntry1     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEntry2     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDefFN      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNameShort  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNameCorp   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vName       AS CHARACTER NO-UNDO.

   DEFINE BUFFER xcust-corp FOR cust-corp.

   IF iCustCat <> "�" THEN RETURN ERROR.

   IF  {assigned iCustCat}
   AND iCustId > 0 THEN DO:
      vFormatNaim = GetXAttrValueEx("cust-corp", STRING(iCustId), "��ଠ⍠��", "").
      IF NOT {assigned vFormatNaim}
      THEN ASSIGN
         vEntry1 = ?
         vEntry2 = ?
      .
      ELSE ASSIGN
         vEntry1 = ENTRY(1,vFormatNaim)
         vEntry2 = (IF NUM-ENTRIES(vFormatNaim) >= 2 THEN ENTRY(2,vFormatNaim) ELSE ?)
      .

      IF   NOT {assigned vEntry1}
        OR NOT CAN-DO("�,�",vEntry1)
        OR NOT {assigned vEntry2}
        OR NOT CAN-DO("�,�",vEntry2)
      THEN DO:
         vDefFN = FGetSetting("��ଠ⍠��",?,"").
         IF   NOT {assigned vEntry1}
           OR NOT CAN-DO("�,�",vEntry1)
         THEN DO:
            vEntry1 = ENTRY(1,vDefFN).
            IF   vEntry1 = ?
              OR NOT CAN-DO(",�,�",vEntry1)
            THEN vEntry1 = "�".
         END.
         IF   NOT {assigned vEntry2}
           OR NOT CAN-DO("�,�",vEntry2)
         THEN DO:
            vEntry2 = (IF NUM-ENTRIES(vDefFN) >= 2 THEN ENTRY(2,vDefFN) ELSE ?).
            IF   NOT {assigned vEntry2}
              OR NOT CAN-DO("�,�",vEntry2)
            THEN vEntry2 = "�".
         END.
      END.

      FIND FIRST xcust-corp WHERE xcust-corp.cust-id = iCustId NO-LOCK NO-ERROR.
      IF AVAIL xcust-corp THEN DO:
         IF {assigned xcust-corp.cust-stat} THEN DO:
            IF vEntry1 = "�"
            THEN DO:
               vCode = GetCodeVal("����।�",TRIM(xcust-corp.cust-stat)).
               vCode = GetCodeName("����।�",vCode).
               IF {assigned vCode}
               THEN vName = vCode.
               ELSE vName = xcust-corp.cust-stat.
            END.
            ELSE IF vEntry1 =  "�" THEN
               vName = xcust-corp.cust-stat.
            ELSE
               vName = "".
         END.
         ELSE vName = "".
         IF {assigned vName} THEN vName = vName + " ".
         vNameCorp = GetValueTmp("cust-corp", 
                                 STRING(xcust-corp.cust-id), 
                                 "name-corp",
                                 iDate,
                                 xcust-corp.name-corp).
         IF  vEntry2 = "�"
         AND {assigned vNameCorp}
         THEN DO:
            IF vNameCorp BEGINS vName THEN
               vName = vNameCorp.
            ELSE
               vName = vName + vNameCorp.
         END.
         ELSE DO: 
            vNameShort = GetValueTmp("cust-corp", 
                                     STRING(xcust-corp.cust-id), 
                                     "name-short",
                                     iDate,
                                     xcust-corp.name-short).
            IF vNameShort BEGINS vName THEN
               vName = vNameShort.
            ELSE
               vName = vName + vNameShort.
         END.
      END.
      ELSE vName = "".
   END.

   RETURN vName.
END FUNCTION.

FUNCTION GetValueTmp RETURNS CHARACTER PRIVATE 
   (INPUT iFile AS CHARACTER,
    INPUT iSurr AS CHARACTER,
    INPUT iCode AS CHARACTER,
    INPUT iDate AS DATE,
    INPUT iDefaultValue AS CHARACTER):

   DEFINE VARIABLE vValue AS CHARACTER   NO-UNDO INIT "".
   DEFINE BUFFER   tmpsigns FOR tmpsigns.
   FIND LAST tmpsigns WHERE 
             tmpsigns.file-name  =  iFile
         AND tmpsigns.code       =  iCode
         AND tmpsigns.surrogate  =  iSurr
         AND tmpsigns.since      <= iDate
   NO-LOCK NO-ERROR.
                                       
   IF AVAIL tmpsigns THEN
      vValue = IF tmpsigns.code-value <> "" 
               THEN tmpsigns.code-value 
               ELSE tmpsigns.xattr-value.

   IF NOT {assigned vValue} THEN 
      vValue = iDefaultValue.

   RETURN vValue.

END FUNCTION.

/******************************************************************************/
/* �㭪�� �����頥� ���� �� ���祭�� �� "Legal207.�।�␮�슫����" 
   ᨭ⠪�� ��: ⨯_������1:ᯨ᮪_஫��1;⨯_������2:ᯨ᮪_஫��2;⨯_������3:ᯨ᮪_஫��3
   ��६����: i(1..2)  i=1 ⨯ ������ "�,�,�"
                        i=2 ᯨ᮪ ஫�� ��� ��।�������� ⨯� ������ �१ �������
               j(1..3)  ����� ���� "⨯_������:ᯨ᮪_஫��" �� ᯨ᪠, ࠧ����⥫� ";" */
/******************************************************************************/
FUNCTION GetPredRoleClient RETURNS CHAR 
   (INPUT i AS INT64,  INPUT j AS INT64):
   IF TRIM(ENTRY(i,ENTRY(j,FGetSetting("Legal207", "�।�␮�슫����", ""),";"),":")) <> ? 
   THEN 
     RETURN TRIM(ENTRY(i,ENTRY(j,FGetSetting("Legal207", "�।�␮�슫����", ""),";"),":")).
   ELSE 
     RETURN "".
END FUNCTION.

&GLOB LEG321P-FUN-DEF YES
&ENDIF
/******************************************************************************/
/* $LINTFILE='leg321p.fun' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq/' */
/* $LINTUSER='osov' */
/* $LINTDATE='17/11/2017 09:49:45.987+03:00' */
/*prosign9a3CxH2OWP//IXLWh8FdNw*/