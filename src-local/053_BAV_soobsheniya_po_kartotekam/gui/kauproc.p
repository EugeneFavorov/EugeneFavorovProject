{globals.i}

/* +++ kauproc.p was humbly modified by (c)blodd converter v.1.11 on 8/15/2017 6:12am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: kauproc.p
      Comment: ����⨥ ��� �� ����⥪� 1,2
   Parameters: ���
         Uses:
      Used by: 
      Created: 
     Modified: 01.06.2004 abko 0029139 ���� ��� �� ����2 � ��⮬ ���ࠧ�������
     Modified: 05.07.2004 abko 0021763 ���������� ��易⥫��� ���.४�. �� ����⨨
                          ��� ��⮬���᪮�
     Modified: 22.09.2005 kraw (0049978)
     Modified: 19.02.2008 muta 0088009 �� ।���஢���� ����� ��� �� ����⨨
                          ESC � �⪠�� �� ��࠭���� ��� �� ������ ���� ᮧ���.
     Modified: 13/07/2009 kraw (0090110) � Create_Acct_Term ��ࠡ��뢠�� ⮪�� "�" (��ਫ���᪮�)�
*/
{globals.i}
DEF VAR vclass    LIKE class.class-code NO-UNDO.
DEF VAR vacct-cat LIKE acct.acct-cat    NO-UNDO.
DEF VAR isCreate  AS LOGICAL      NO-UNDO.
DEF VAR in-bal-acct AS INT64    NO-UNDO.
DEFINE BUFFER inacct FOR acct.

DEF VAR MaskPod  AS CHARACTER INITIAL "*" NO-UNDO.
DEF VAR MaskSrok AS CHARACTER INITIAL "��.��.����" NO-UNDO.
DEF VAR oresult  AS INT64   NO-UNDO.
DEFINE VARIABLE  mTmp-acct AS CHARACTER  NO-UNDO.
DEFINE VARIABLE in-op-date AS DATE        NO-UNDO. 
{wclass.i}
{intrface.get acct}     /* ������⥪� ��� ࠡ��� � ��⠬�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get db2l}

{intrface.get rights}
{branch.pro}
{details.def} 
{topkind.def}

/*----------------------------------------------------------------------------*/

FUNCTION GetKauIdByContract RETURN CHARACTER PRIVATE
    (INPUT iContract AS CHARACTER):

    CASE iContract:
        WHEN "����1"    THEN RETURN "����-��1".
        WHEN "����2"    THEN RETURN "����-��2".
        WHEN "���⁫��" THEN RETURN "���⁫��".
        OTHERWISE            RETURN ?.
    END.
END FUNCTION.

FUNCTION getCardAcctXAttrName RETURN CHARACTER (INPUT iContract AS CHARACTER):
    RETURN SUBSTRING(iContract, 1, 5) + "�����".
END FUNCTION.

PROCEDURE GetPlanDate:
   DEFINE INPUT-OUTPUT PARAMETER ioPlanDate AS DATE NO-UNDO.

   RUN SetSysConf IN h_base ("�ப���⥦�", ?).
   RUN setplday.p (?,
                   "���☠��",
                   "��⠏���",
                   6).
   IF {&KEY_FUNCTION}({&LAST_KEY}) = "END-ERROR" THEN DO:
      pick-value = "no".
      ioPlanDate = ?.
   END.
   ELSE
      ioPlanDate = DATE(INT64(ENTRY(2,pick-value,".")),
                        INT64(ENTRY(3,pick-value,".")),
                        INT64(ENTRY(1,pick-value,".")))
      .
END PROCEDURE.

/*----------------------------------------------------------------------------*/

PROCEDURE MakeAcctByTerm:
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iPlanDate    AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   MaskSrok = FGetSetting("�⠭���", 
                          "���金1���ப��", 
                          "��.��.����").

   IF NUM-ENTRIES(MaskSrok,"|") = 2 THEN
      ASSIGN MaskPod  = ENTRY(1,MaskSrok,"|")
             MaskSrok = TRIM(ENTRY(2,MaskSrok,"|"))
      .

   RUN SetSysConf IN h_base ("�ப���⥦�",
                             STRING(YEAR(iPlanDate),"9999") + "." +
                             STRING(MONTH(iPlanDate),"99") + "." +
                             STRING(DAY(iPlanDate),"99")).

   RUN Create_Acct_Term (iIn-Rid,
                         iIn-contract,
                         iIn-op-date,
                         iPlanDate,                         
                         iEcho,
                         iRid,
                         OUTPUT oOut-Rid).
   /* ��⠭�������� �������� ����, �⮡� ��� �� ����訢����� ����୮ 
      � ��楤�� setplday.p
    */
   IF {&RETURN_VALUE} =  "ERROR" THEN
      RUN SetSysConf IN h_base ("�ப���⥦�",?).

END PROCEDURE.

/*----------------------------------------------------------------------------*/
PROCEDURE MakeAcctByCust.
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid        AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   RUN Create_Acct_Cust (iIn-Rid,
                         iIn-contract,
                         iIn-op-date,
                         iEcho,
                         iRid,
                         OUTPUT oOut-Rid).
END PROCEDURE.
/*----------------------------------------------------------------------------*/

PROCEDURE Create_acct:
   DEFINE INPUT  PARAMETER iIn-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.  /* ������ ��� */
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE vPlanDate AS DATE    NO-UNDO.
   DEFINE VARIABLE vCountEnd AS INT64 NO-UNDO.
   DEFINE VARIABLE vCount    AS INT64 NO-UNDO.
   DEFINE VARIABLE vCodeVal  AS CHAR    NO-UNDO.

   IF (FGetSetting("�⠭���", "����1���ப��", "") = "��")
      AND iIn-contract = "����1" THEN
   DO:
      /* ������ �ப ���⥦� */
      RUN GetPlanDate(INPUT-OUTPUT vPlanDate).
      IF vPlanDate =  ? THEN
         RETURN "ERROR".
      RUN MakeAcctByTerm (iIn-Rid,
                          iIn-Contract,
                          iIn-op-date,
                          vPlanDate,
                          YES,
                          iRid,
                          OUTPUT oOut-Rid).

   END.
   ELSE
      RUN MakeAcctByCust(iIn-Rid,
                         iIn-Contract,
                         iIn-op-date,
/* ������ ���� ����
                         YES,
*/                       NO,    /* �� ।���஢��� ��� ����⥪ */
/* ����� ������ ���� ���� */
                         iRid,
                         OUTPUT oOut-rid).
   RETURN {&RETURN_VALUE}.
END PROCEDURE. /* Create_acct */
/*----------------------------------------------------------------------------*/

PROCEDURE FindCardAcctByXAttr.
    DEFINE INPUT PARAMETER  iCode      AS CHARACTER       NO-UNDO.
    DEFINE PARAMETER BUFFER acct       FOR  acct.
    DEFINE PARAMETER BUFFER card-acct  FOR  acct.

    DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
    DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.

    vAcct = GetXAttrValueEx("acct",
                            GetSurrogateBuffer("acct", (BUFFER acct:HANDLE)),
                            getCardAcctXAttrName(iCode),
                            "").
    IF NUM-ENTRIES(vAcct) = 2 THEN
        ASSIGN
            vCurrency = ENTRY(2, vAcct)
            vAcct     = ENTRY(1, vAcct)
        .
    ELSE
        vCurrency = acct.currency.
    {find-act.i &bact     = card-acct
                &acct     = vAcct
                &curr     = vCurrency
                &AddWhere = "AND card-acct.acct-cat   = 'o'           ~
                             AND card-acct.cust-cat   = acct.cust-cat ~
                             AND card-acct.cust-id    = acct.cust-id  ~
                             AND card-acct.close-date = ?"}
END PROCEDURE.

PROCEDURE Create_acct_Cust:

   DEFINE INPUT  PARAMETER in-rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER in-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER in-op-date  AS DATE      NO-UNDO .
   DEFINE INPUT  PARAMETER iEcho       AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid        AS RECID     NO-UNDO.  /* RECID 蠡���� ��� */
   DEFINE OUTPUT PARAMETER out-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE in-title     AS   CHARACTER              NO-UNDO.
   DEFINE VARIABLE s_acct       LIKE acct.acct              NO-UNDO.
   DEFINE VARIABLE vChkBrnch    AS   LOGICAL    INITIAL NO  NO-UNDO.
   DEFINE VARIABLE vBrnch-id    AS   CHARACTER  INITIAL "*" NO-UNDO.
   DEFINE VARIABLE vDetails     AS   CHARACTER              NO-UNDO.
   DEFINE VARIABLE vTmpVal      AS   CHARACTER              NO-UNDO.
   DEFINE VARIABLE vRes         AS   LOGICAL                NO-UNDO.
/* ��⠢�� ���� ���� */
   DEFINE VARIABLE vAcctO       AS   CHARACTER              NO-UNDO.
/* ����� ��⠢�� ���� ���� */
   DEFINE VARIABLE vOpenByAcct  AS   LOGICAL             NO-UNDO.
   DEFINE VARIABLE vLinkAcct    AS   LOGICAL             NO-UNDO.

   DEFINE VARIABLE vKauId LIKE bal-acct.kau-id NO-UNDO.

   DEFINE VARIABLE mItem    AS INT64   NO-UNDO.
   DEFINE VARIABLE mItem1   AS INT64   NO-UNDO.

   DEFINE BUFFER acct FOR acct .
/* ��⠢�� ���� ���� */
   DEFINE BUFFER bacct FOR acct .
/* ����� ��⠢�� ���� ���� */
   DEFINE BUFFER bal-acct FOR bal-acct.

   vKauId = GetKauIdByContract(in-contract).

   CASE in-contract:
       WHEN "����1"    THEN
           ASSIGN
               in-title    = "����⥪� 1"
               in-bal-acct = 90901
           .
       WHEN "����2"    THEN
           ASSIGN
               in-title    = "����⥪� 2"
               in-bal-acct = 90902
           .
       WHEN "���⁫��" THEN
           ASSIGN
               in-title    = "����⥪� �����஢����� ��⮢"
               in-bal-acct = 90901
               in-contract = "���⁫"
           .
   END.
   IF in-contract =  "���⁫" THEN vOpenByAcct = FGetSetting("���⁫��","���⁑���","") =  "�� ����".
   vLinkAcct = FGetSetting("�⠭���","���↥�⪑�","") =  "��".

   FIND inacct WHERE
        RECID(inacct) =  in-rid
        NO-LOCK NO-ERROR.
   FIND FIRST bal-acct WHERE
              bal-acct.bal-acct =  in-bal-acct
        NO-LOCK NO-ERROR.

    IF CAN-DO("����1,����2", in-contract) THEN DO:
        vChkBrnch = FGetSetting("�⠭���",
                                in-contract + "����",
                                "���") = "��".
        vBrnch-id = IF vChkBrnch THEN inacct.branch-id ELSE "*".
    END.
    RUN FindCardAcctByXAttr(in-contract, BUFFER inacct, BUFFER acct).
/* ��⠢�� ���� ���� */
   IF (FGetSetting("�⠭���","�2����","��") EQ "��")
      OR in-contract NE "����2" THEN
   DO:
/* ����� ��⠢�� ���� ���� */
    IF AVAILABLE acct THEN
        s_acct = acct.acct.
    ELSE
    IF NOT (in-contract =  "���⁫" AND vOpenByAcct) THEN DO: /* ��� ��� � ��� ४����� */
      FIND acct WHERE
           acct.acct-cat   =  "o"
       AND acct.contract   =  in-contract
       AND acct.filial-id  =  ShFilial
       AND acct.currency   =  inacct.currency
       AND acct.cust-cat   =  inacct.cust-cat
       AND acct.cust-id    =  inacct.cust-id
       AND acct.close-date =  ?
       AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
            OR acct.branch-id =  inacct.branch-id) /*��� ��� ᮢ������ */
           NO-LOCK NO-ERROR.
      IF AMBIGUOUS acct THEN
      DO:
         pick-value = '' .
         RUN "acct(k).p" ( bal-acct.bal-acct,
                           vKauId,
                           inacct.cust-cat,
                           inacct.cust-id,
                           inacct.currency,
                           vBrnch-id,
                           4
                          ).
         IF     pick-value <> ''
            AND pick-value <> ?
         THEN
            {find-act.i
               &acct = ENTRY(1,pick-value)
               &curr = ENTRY(2,pick-value)
            }
         ELSE
            RETURN 'ERROR' .
      END.
   END.

   IF NOT AVAIL acct AND NOT (in-contract =  "���⁫" AND vOpenByAcct) THEN
      FIND FIRST acct WHERE
                 acct.acct-cat =  "o"
             AND acct.contract =  in-contract
             AND acct.currency =  inacct.currency
             AND acct.filial-id  =  ShFilial
             AND acct.cust-cat =  inacct.cust-cat
             AND acct.cust-id  =  inacct.cust-id
             AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
                  OR acct.branch-id =  inacct.branch-id) /*��� ��� ᮢ������ */
                 NO-LOCK NO-ERROR.
/* ��⠢�� ���� ���� */
   END.
   ELSE
   DO:

      IF NOT AVAIL acct THEN
      DO:
         vAcctO = "".
         FND-ACCT:
         FOR EACH acct WHERE
                  acct.acct-cat EQ "o"
              AND acct.contract EQ in-contract
              AND acct.currency EQ inacct.currency
              AND acct.filial-id  EQ ShFilial
              AND acct.cust-cat EQ inacct.cust-cat
              AND acct.cust-id  EQ inacct.cust-id
              AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
                   OR acct.branch-id EQ inacct.branch-id) /*��� ��� ᮢ������ */
              NO-LOCK:
         
              find first bacct WHERE
                         bacct.cust-cat EQ inacct.cust-cat
                     AND bacct.cust-id  EQ inacct.cust-id
                     AND bacct.acct-cat EQ "b" 
                     AND bacct.filial-id EQ ShFilial
                     AND can-find(signs where signs.file-name eq "acct" and
                                              signs.surrogate eq bacct.acct + "," + bacct.currency and
                                              signs.code eq "����2�����" and
                                              signs.xattr-value eq acct.acct + "," + acct.currency) no-lock no-error.
              IF NOT AVAIL bacct THEN 
              DO:
                vAcctO = acct.acct.
                LEAVE FND-ACCT.
              END.
         END.
         {find-act.i
            &acct = vAcctO
         }

      END.        
   END.
/* ����� ��⠢�� ���� ���� */

   IF AVAIL acct AND acct.close-date <> ? THEN
   DO:
      IF iEcho THEN
         DO:
            mblodd_char_Tmp01 = pick-value.
            RUN Fill-AlertSysMes IN h_tmess("","",4,"��� �� " + STRING(in-title) + CHR(32) + " " + STRING(acct.acct) + CHR(32) + ( IF acct.currency <> "" THEN "/" ELSE "") + STRING(acct.currency) + CHR(32) + "~n" + CHR(32) + " �� ��������ᮢ�� ��� " + STRING(acct.bal-acct) + " ������ !" + CHR(32) + "~n" + CHR(32) + "������ ��� ?").
            DEFINE VARIABLE choice AS LOGICAL NO-UNDO.
            choice = (pick-value = "YES").
            pick-value = mblodd_char_Tmp01.
         END.

      ELSE
         choice = YES.
      IF choice THEN
      DO:
         FIND FIRST acct WHERE
                    acct.acct-cat   =  "o"
                AND acct.contract   =  in-contract
                AND acct.filial-id  =  ShFilial
                AND acct.currency   =  inacct.currency
                AND acct.cust-cat   =  inacct.cust-cat
                AND acct.cust-id    =  inacct.cust-id
                AND acct.close-date <> ?
                AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
                     OR acct.branch-id =  inacct.branch-id) /*��� ��� ᮢ������ */
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         acct.close-date = ?.
      END.
      ELSE RELEASE acct.
   END.


   IF NOT AVAILABLE acct THEN 
   DO:
      {acc-file.i
         &access-class = "'accto'"
         &access-mode  = c
         &no-access    = "message '��� �� ' + in-title + ' � ������ ���������,' skip
                        '� ᮧ���� ��� �� �� ����� �ࠢ�.' VIEW-AS ALERT-BOX ERROR. RETURN 'ERROR' "
      }
      vclass = 'accto'.

      /* ����稬 ���� ��� �  tokacct */
      DEF VAR acctmask AS CHAR FORMAT "x(25)" NO-UNDO.
      DEF VAR tokacct AS CHAR FORMAT "x(30)" NO-UNDO.
      DEFINE VARIABLE vKodDoxRash AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sTokidx AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sToklen AS CHARACTER  NO-UNDO.

      RUN "FindAcctMask" IN h_acct 
         (
         vclass, 
         bal-acct.bal-acct,
         INPUT-OUTPUT acctmask,
         INPUT-OUTPUT vKodDoxRash 
         ) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      FIND FIRST op-template WHERE RECID(op-template) =  iRid NO-LOCK NO-ERROR.

      IF AVAILABLE op-template THEN
      DO:
         vDetails = GetXattrValue("op-template",
                                  op-template.op-kind + "," + string(op-template.op-template),
                                  "CustInterna").
         acctmask = GetXattrValueEx("op-template",
                                    op-template.op-kind + "," + string(op-template.op-template),
                                    "open_mask",
                                    acctmask).
      END.
      ELSE DO:
         vTmpVal = GetSysConf("������⠊2").
         IF {assigned vTmpVal} THEN vDetails = vTmpVal.
         vTmpVal = GetSysConf("��᪠��⠊2").
         IF {assigned vTmpVal} THEN acctmask = vTmpVal.
      END.
/* ��⠢�� ���� ���� */
      /* ���४�஢�� ��᪨ ��� �� */
      DEFINE VARIABLE cOFgroup  AS CHARACTER    NO-UNDO.
      cOFgroup = GetXattrValue("acct", inacct.acct + "," + inacct.currency, "groupOABS").
      cOFgroup = IF ((cOFgroup EQ "599") OR (cOFgroup EQ "596")) THEN "589" ELSE cOFgroup.
      IF {assigned cOFgroup} THEN SUBSTRING(acctmask, 10, 4) = "0" + cOFgroup.
/* ����� ��⠢�� ���� ���� */

      RUN GetAcctMask IN h_acct 
          (
          INPUT acctmask,
          OUTPUT tokacct,    
          OUTPUT sTokidx,    
          OUTPUT sToklen
          )
      NO-ERROR.      
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      IF INDEX(acctmask,"�") > 0 THEN DO:

         mItem1 = LENGTH(TRIM(delFilFromAcct(inacct.acct))).

         acctmask = tokacct.
         DO mItem = LENGTH(acctmask) TO 1 BY -1:

            IF SUBSTRING(acctmask, mItem, 1) =  "�" THEN
            DO:
               acctmask = ( IF mItem >  1 THEN SUBSTRING(acctmask, 1, mItem - 1)
                                       ELSE "")
                       + SUBSTRING(TRIM(delFilFromAcct(inacct.acct)), mItem1, 1)
                       + ( IF mItem <  LENGTH(acctmask) THEN SUBSTRING(acctmask, mItem + 1)
                                                     ELSE "").
               mItem1 = mItem1 - 1.
            END.
         END. 

         tokacct = REPLACE(tokacct,"�","�").
      END.

      IF {assigned vDetails} THEN DO:

         RUN SetSysConf IN h_base ("������", inacct.acct).
         RUN SetSysConf IN h_base ("�����₠�", inacct.currency).

         RUN ProcessDetails (?, input-output vDetails).

         RUN DeleteOldDataProtocol IN h_base ("������").
         RUN DeleteOldDataProtocol IN h_base ("�����₠�").
      END.

      ASSN-ACCT:
      DO
         ON ERROR  UNDO ASSN-ACCT, RETRY
         ON ENDKEY UNDO,           LEAVE
         WITH FRAME newacc:

         /* ��� ����⥪� �����஢����� ��⮢ �।��⠢��
            � 蠡���� ᮧ�������� ��������ᮢ�� ��⮢ */
         IF in-contract = "���⁫" THEN DO:
             {empty tOpKindParams}
             ASSIGN
                 vRes = TDAddParam("iCurrency", inacct.currency)        AND
                        TDAddParam("iCustCat" , inacct.cust-cat)        AND
                        TDAddParam("iCustId"  , STRING(inacct.cust-id)) AND
                        TDAddParam("iAcctMask", acctmask)               AND
                        TDAddParam("iRegimAuto", STRING(NOT iEcho))
             NO-ERROR.
             IF vRes THEN
                 RUN ex-trans.p("_CBLACTC",
                                in-op-date,
                                TABLE tOpKindParams,
                                OUTPUT vRes,
                                OUTPUT vTmpVal).
             ELSE
                 vTmpVal = "�訡�� ��।�� ��ࠬ��஢ " +
                           "� �࠭����� " + QUOTER("_CBLACTC").
             IF NOT vRes THEN
                 RETURN ERROR "ERROR: " + vTmpVal.
             vTmpVal = GetSysConf("_CBLACTC_acct").
             RUN DeleteOldDataProtocol IN h_base ("_CBLACTC_acct").
             IF {assigned vTmpVal} THEN DO:
                 {find-act.i &acct = vTmpVal
                             &curr = inacct.currency}
             END.
         END.
         ELSE DO:
             RUN Cm_acct_cr IN h_acct (
                    vclass,               /* iClass                  */  
                    bal-acct.bal-acct,    /* iBal                    */  
                    inacct.currency,      /* iCurr                   */  
                    inacct.cust-cat,      /* iCustCat                */  
                    inacct.cust-id,       /* iCustID                 */  
                    in-op-date,           /* iOpenDate               */  
                    OUTPUT mTmp-acct,     /* oAcct                   */  
                    BUFFER acct,           /* BUFFER iacct FOR acct . */  
                    acctmask,              /* iAcctMask               */  
                    vDetails,              /* iDetails                */  
                    vKauId,                /* iKauId                  */  
                    in-contract,           /* iContract               */  
                    USERID ('bisquit'),    /* iUserId                 */  
                    inacct.branch-id,           /* iBranchId               */  
                    YES                    /* iCopyBalXattr           */  
             ) NO-ERROR. 
             IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().
             isCreate = YES.
             IF iEcho THEN 
                RUN edtnacct.p ( in-title,
                                 RECID(acct),
                                 tokacct,
                                 OUTPUT isCreate
                               ). 

             IF NOT isCreate THEN UNDO ASSN-ACCT, LEAVE.
             RUN Create_xattr_mand (RECID(acct), OUTPUT oresult).
         END.
      END. /* ASSN-ACCT */
   END.

    IF NOT AVAILABLE acct THEN
        RETURN.
    out-rid = RECID(acct).
    IF NOT {assigned s_acct} THEN DO:
/* ������� ���� ����
        pick-value = "NO".
        IF (in-contract =  "���⁫" AND vOpenByAcct) OR vLinkAcct THEN pick-value = "YES".
        ELSE
        DO:
           IF GetSysConf ("CrdSilenceMode") = "Yes" THEN
              pick-value = "Yes".
           ELSE
              RUN Fill-AlertSysMes IN h_tmess ("",
                                            "",
                                            "4",
                                            "������� ������ ��� �����" +
                                            " �����ᮢ� ��⮬"          +
                                            " " + inacct.acct             +
                                            "~n� ��������ᮢ� ��⮬ "   +
                                            in-contract + " " + acct.acct +
                                            "?").
        END.
        IF KEYFUNCTION({&LAST_KEY}) <> "END-ERROR" AND pick-value = "YES" THEN
����� 㤠������� �ࠣ���� ���� ���� */
            IF NOT UpdateSigns(inacct.class-code,
                               GetSurrogateBuffer("acct",
                                                  (BUFFER inacct:HANDLE)),
                               getCardAcctXAttrName(in-contract),
                               GetSurrogateBuffer("acct",
                                                  (BUFFER acct:HANDLE)),
                               ?)
            THEN
                RUN Fill-SysMes IN h_tmess ("", "", "0", "���� �� ᮧ����").
/* ��⠢�� ���� ���� */
        /* �ਢ離� � ��㯯� � �� */
        IF {assigned cOFgroup}
        THEN DO:
            UpdateSigns ("acct", acct.acct + "," + acct.currency, "groupOABS", cOFgroup, YES).
            RUN CreateLinks("acct", "acct-group", acct.acct + "," + acct.currency, cOFgroup, acct.open-date, ?, "").
        END.
/* ����� ��⠢�� ���� ���� */
    END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/

/* ��楤�� ���/���뢠�� ��� �� ����⥪�� �� �ப�� (���. ४����� "�ப���⥦�") */
PROCEDURE Create_Acct_Term:
   DEFINE INPUT  PARAMETER iIn-Rid      AS RECID     NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-contract AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iIn-op-date  AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iPlanDate    AS DATE      NO-UNDO.
   DEFINE INPUT  PARAMETER iEcho        AS LOGICAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iRid         AS RECID     NO-UNDO.
   DEFINE OUTPUT PARAMETER oOut-rid     AS RECID     NO-UNDO.

   DEFINE VARIABLE vTitle      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpMask    AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE vMaskBranch AS CHARACTER INIT "" NO-UNDO.
   DEFINE VARIABLE i           AS INT64           NO-UNDO.
   DEFINE VARIABLE vIsCartAcct AS LOGICAL           NO-UNDO.
   DEFINE VARIABLE vacct_o     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vcurr_o     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vPlanDateSt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vChkBrnch   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vBrnch-id   AS CHARACTER  INIT "*"  NO-UNDO.
   DEFINE VARIABLE vDetails    AS CHARACTER            NO-UNDO.
   vPlanDateSt = STRING(iPlanDate,"99/99/9999").
   CASE MaskSrok:
      WHEN "��.��" THEN DO:
         vPlanDateSt = STRING(DAY(iPlanDate),"99") + "/" + STRING(MONTH(iPlanDate),"99").
      END.
      WHEN "��" THEN DO:
         vPlanDateSt = STRING(DAY(iPlanDate),"99") + "/".
      END.
   END CASE.

   IF iIn-contract =  "����1" THEN
      ASSIGN
         vTitle      = "����⥪� 1"
         in-bal-acct = 90901
      .
   FIND FIRST inacct WHERE RECID(inacct) =  iIn-rid
      NO-LOCK
      NO-ERROR.
   FIND FIRST bal-acct WHERE bal-acct.bal-acct =  in-bal-acct
      NO-LOCK
      NO-ERROR.

   RUN GetBranchParent(inacct.branch-id,INPUT-OUTPUT vTmpMask).
   
   DO i = 1 TO NUM-ENTRIES(vTmpMask):
      IF CAN-DO(MaskPod,ENTRY(i,vTmpMask)) THEN
      {additem.i vMaskBranch ENTRY(i,vTmpMask)}
   END.
   IF LOOKUP(inacct.branch-id,vMaskBranch) =  0 THEN
      vMaskBranch = inacct.branch-id + "," + vMaskBranch.
   
   /* �஢����� �ਭ���������� � ������ ���ࠧ������� ���. � ����. ��⮢ */
   vChkBrnch = FGetSetting("�⠭���","����1����", "���") = "��".
   vBrnch-id = ( IF vChkBrnch =  YES 
                THEN inacct.branch-id
                ELSE "*").

/* ���砫� ���饬 ������ ��� � ���ࠧ������� �����ᮢ��� ��� � ����� � �������� */
   vIsCartAcct = FALSE.
   DO i = 1 TO NUM-ENTRIES(vMaskBranch):
      vTmpMask = ENTRY(i,vMaskBranch).
      /* ������ �� ४����� */
      FOR EACH signs WHERE signs.file-name =  "acct"
                       AND signs.code      =  "�ப���⥦�"
                       AND ENTRY(2,signs.surrogate) =  inacct.currency
                       AND signs.code-val  BEGINS vPlanDateSt
          USE-INDEX allfield NO-LOCK,
          FIRST acct WHERE acct.branch-id =  vTmpMask          /*�஢�ਬ ���ࠧ������� */
                       AND acct.acct-cat  =  "o"               /*�஢�ਬ ��⥣���     */
                       AND acct.bal-acct  =  bal-acct.bal-acct /*�஢�ਬ ��� 2-�� ���浪� */
                       AND acct.acct      =  ENTRY(1,signs.surrogate)
                       AND acct.currency  =  inacct.currency
                       AND acct.cust-cat  =  '�'        /* ����७���  */
                       AND acct.close-date =  ?         /* �� ������� */
                       AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
                            OR acct.branch-id =  inacct.branch-id) /*��� ��� ᮢ������ */
              USE-INDEX branch-id NO-LOCK:

         vIsCartAcct = TRUE. /*����� ��諨 ���室�騩 ��� */
         LEAVE.
      END. /* FOR EACH */
      IF vIsCartAcct THEN
         LEAVE.
   END. /* DO TO NUM-ENTRIES(vTmpMask)*/

   IF NOT vIsCartAcct THEN
   DO i = 1 TO NUM-ENTRIES(vMaskBranch):
/* ⮦� �� ��� ��������*/
      vTmpMask = ENTRY(i,vMaskBranch).
      FOR EACH signs WHERE signs.file-name =  "acct"
                       AND signs.code      =  "�ப���⥦�"
                       AND ENTRY(2,signs.surrogate) =  inacct.currency
                       AND signs.code-val  BEGINS vPlanDateSt
          USE-INDEX allfield NO-LOCK,
          FIRST acct WHERE acct.branch-id =  vTmpMask          /*�஢�ਬ ���ࠧ������� */
                       AND acct.acct-cat  =  "o"               /*�஢�ਬ ��⥣���     */
                       AND acct.bal-acct  =  bal-acct.bal-acct /*�஢�ਬ ��� 2-�� ���浪� */
                       AND acct.acct      =  ENTRY(1,signs.surrogate)
                       AND acct.currency  =  inacct.currency
                       AND acct.cust-cat  =  '�'        /* ����७���  */
                       AND acct.close-date <> ?         /* ������� */
                       AND (   NOT vChkBrnch /* ��� �� �஢��塞 ���ࠧ������� */
                            OR acct.branch-id =  inacct.branch-id) /*��� ��� ᮢ������ */
              USE-INDEX branch-id NO-LOCK:

         vIsCartAcct = TRUE. /*����� ��諨 ���室�騩 ��� */
         LEAVE.
      END. /* FOR EACH */
      IF vIsCartAcct THEN
         LEAVE.
   END.

   IF AVAILABLE acct THEN
   DO:
      IF acct.close-date <> ? THEN
      DO:
         IF iEcho THEN
            DO:
               mblodd_char_Tmp01 = pick-value.
               RUN Fill-AlertSysMes IN h_tmess("","",4,"��� �� " + STRING(vTitle) + CHR(32) + " " + STRING(acct.acct) + CHR(32) + ( IF acct.currency <> "" THEN "/" ELSE "") + STRING(acct.currency) + CHR(32) + "~n" + CHR(32) + " �� ��������ᮢ�� ��� " + string(acct.bal-acct) + " ������ !" + CHR(32) + "~n" + CHR(32) + "������ ��� ?").
               DEFINE VARIABLE choice AS LOGICAL NO-UNDO.
               choice = (pick-value = "YES").
               pick-value = mblodd_char_Tmp01.
            END.

         ELSE choice = YES.
         IF choice THEN
         DO:
            FIND CURRENT acct
               EXCLUSIVE-LOCK
               NO-ERROR.
            acct.close-date = ? .
         END.
         ELSE DO:
            DO:
               mblodd_char_Tmp01 = pick-value.
               RUN Fill-AlertSysMes IN h_tmess("","",4,"������ ���� ��� ?").
               choice = (pick-value = "YES").
               pick-value = mblodd_char_Tmp01.
            END.

            IF NOT choice THEN
                   RETURN "ERROR".
         END.
      END.
   END.

   IF NOT AVAILABLE acct THEN
   DO:
      {acc-file.i
         &access-class = "'accto'"
         &access-mode  = c
         &no-access    = "message '��� �� ' + vTitle + ' �� ��� ���⥦� ���������,' skip
                        '� ᮧ���� ��� �� �� ����� �ࠢ�.' VIEW-AS ALERT-BOX ERROR. RETURN 'ERROR' "
      }   
      vclass = 'accto'.

      /* ����稬 ���� ��� �  tokacct */
      DEF VAR acctmask AS CHAR FORMAT "x(25)" NO-UNDO.
      DEF VAR tokacct AS CHAR FORMAT "x(30)" NO-UNDO.
      DEFINE VARIABLE vKodDoxRash AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sTokidx AS CHARACTER  NO-UNDO.
      DEFINE VARIABLE sToklen AS CHARACTER  NO-UNDO.


      RUN "FindAcctMask" IN h_acct 
         (
         vclass, 
         bal-acct.bal-acct,
         INPUT-OUTPUT acctmask,
         INPUT-OUTPUT vKodDoxRash 
         ) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      RUN GetAcctMask IN h_acct 
          (
          INPUT acctmask,
          OUTPUT tokacct,    
          OUTPUT sTokidx,    
          OUTPUT sToklen
          )
      NO-ERROR.      
      IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().

      FIND FIRST op-template WHERE RECID(op-template) =  iRid NO-LOCK NO-ERROR.
      IF AVAIL(op-template) THEN vDetails = GetXattrValue("op-template",op-template.op-kind + "," + string(op-template.op-template),"CustInterna").

      IF {assigned vDetails} THEN DO:

         RUN SetSysConf IN h_base ("������", inacct.acct).
         RUN SetSysConf IN h_base ("�����₠�", inacct.currency).

         RUN ProcessDetails (?, input-output vDetails).

         RUN DeleteOldDataProtocol IN h_base ("������").
         RUN DeleteOldDataProtocol IN h_base ("�����₠�").
      END.

      ASSN-ACCT:
      DO
         ON ERROR  UNDO ASSN-ACCT, RETRY
         ON ENDKEY UNDO,           LEAVE
         WITH FRAME newacc:

         RUN Cm_acct_cr IN h_acct (
                vclass,               /* iClass                  */  
                bal-acct.bal-acct,    /* iBal                    */  
                inacct.currency,      /* iCurr                   */  
                '�',                  /* iCustCat                */  
                ?,                    /* iCustID                 */  
                iIn-op-date,           /* iOpenDate               */  
                OUTPUT mTmp-acct,     /* oAcct                   */  
                BUFFER acct,           /* BUFFER iacct FOR acct . */  
                acctmask,              /* iAcctMask               */  
                IF NOT {assigned vDetails} THEN 
                    '�ப ���⥦� ' + STRING(iPlanDate) 
                   ELSE  vDetails,     /* iDetails                */  
                GetKauIdByContract(iIn-contract), /* iKauId                  */  
                iIn-contract,           /* iContract               */  
                USERID ('bisquit'),    /* iUserId                 */  
                inacct.branch-id,           /* iBranchId               */  
                YES                     /* iCopyBalXattr           */  
         ) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN "ERROR: " + GetErrMsg().
         isCreate = YES.
         IF iEcho THEN
            RUN edtnacct.p(vTitle, recid(acct), tokacct, OUTPUT isCreate).
         IF NOT isCreate THEN
            RETURN "ERROR".
         UpdateSigns("accto",
                     acct.acct + "," + acct.currency,
                     "�ப���⥦�",
                     STRING(iPlanDate,"99/99/9999"),?).
         RUN Create_xattr_mand (RECID(acct), OUTPUT oresult).
      END. /* ASSN-ACCT */
   END.
   IF AVAIL acct THEN
      oOut-rid = RECID(acct).
END PROCEDURE. /* Create_Acct_Term */

PROCEDURE Create_xattr_mand:
   DEFINE INPUT PARAMETER iAcctRid AS RECID NO-UNDO.
   DEFINE OUTPUT PARAMETER o{&LAST_KEY} AS INT64 NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   FIND FIRST acct WHERE RECID(acct) =  iAcctRid NO-LOCK NO-ERROR.

   /* �������� ��易⥫�� ��. �᫨ ��� ���� */
   RUN chsigns.p (acct.class-code,
                  "accto",
                  acct.acct + "," + acct.currency,
                  YES,
                  OUTPUT oresult).
   /* �᫨ ���� ������������ - ����᪠�� ।���஢����. */
/* ������ ���� ����
   IF oresult >  0
*/ IF oresult >  1
/* ����� ������ ���� ���� */
      THEN RUN "xattr-ed.p" (acct.class-code,
                             acct.acct + "," + acct.currency,
                             " ����� " + acct.acct, YES, 3).
   o{&LAST_KEY} = {&LAST_KEY}.
END PROCEDURE.
/* $LINTFILE='kauproc.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='20/06/2017 09:33:33.293+03:00' */
/*prosignm2gBWpzWLUkeR9gWA7m+/Q*/
/* --- kauproc.p was humbly modified by (c)blodd converter v.1.11 on 8/15/2017 6:12am --- */
