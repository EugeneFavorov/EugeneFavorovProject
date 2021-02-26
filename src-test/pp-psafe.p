/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: pp-psafe.p
      Comment: ������⥪� �㭪権 ����� ��� ࠡ��� � ᥩ䮢묨 �祪���.
   Parameters:
         Uses:
      Used by:
      Created: 05.05.2008     Sami
     Modified: 31/10/2008 kraw (0094516) ���ꥬ � �᭮���� �����
     Modified: 18/08/2009 kraw (0112993) ��_������
     Modified: 13/05/2010 kraw (0115865) ��_����_��� �����_��� ���_�������_���
     Modified: 02/06/2010 kraw (0129180) ��� ��䨪� ������ � ��_�������
     Modified: 28/06/2010 kraw (0130230) ����७���⥩ �� ������� � ������ ��� �. �. ��᪮�쪮.
     Modified: 13/07/2010 kraw (0115230) ��ࠡ�⪠ ��_���������
     Modified: 25/04/2013 kraw (0196813) ��ࠢ����� ���������_�����
*/

{ globals.i }
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get trans}    /* ������⥪� ��� ࠡ��� � ���஬ �⠭���⭮� �࠭���樨. */
{intrface.get card}     /* ������⥪� ��� ࠡ��� � ���⠬� � �����묨 ������ࠬ�. */
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get pbase}
{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{intrface.get corr}     /* ������⥪� ࠡ��� � ��⠬� ��� ��. */
{intrface.get data}     /* ������⥪� ��� ࠡ��� � ��易��� ����஬ ������. */
{intrface.get loan}    /* �����㬥��� ��� �࠭���権                  */
{intrface.get db2l}    /* �������᪠� ࠡ�� � ��                    */
{intrface.get tsafe}
{intrface.get kau}
{intrface.get strng}
{intrface.get xobj}
{intrface.get pbase}
{ksh-defs.i}


{dtterm.i}
DEFINE NEW SHARED VARIABLE mask AS CHARACTER INITIAL ? NO-UNDO.

DEFINE VARIABLE mProxySurr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mProxyTrSurr AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttDates
   FIELD contract AS CHARACTER
   FIELD cont-code AS CHARACTER
   FIELD beg-date  AS DATE
   FIELD end-date  AS DATE
   INDEX m1 contract cont-code.


DEFINE TEMP-TABLE ttProxies NO-UNDO
   FIELD fInitName    AS CHARACTER
   FIELD fCustId      AS CHARACTER
   FIELD fProxySurr   AS CHARACTER
   FIELD fProxy       AS CHARACTER
   FIELD fDet         AS CHARACTER
   FIELD fTrans       AS CHARACTER
   FIELD fProxyTrSurr AS CHARACTER
   INDEX CustId fCustId
   INDEX Proxy fProxy
.

DEFINE BUFFER bloan FOR loan. 


{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PSAFE"
   &LIBNAME       = "������⥪� �㭪権 ����� ��� �������� �����"
   &DESCRIPTION   = "����ন� �㭪樨� ����� ��� ᥩ䮢�� �祥�."
   }

{pfuncdef
   &NAME          = "��_����"
   &DESCRIPTION   = "��।���� ������."
   &PARAMETERS    = "Contract,Cont-Code"
   &RESULT        = "��� ������ (�᫨ �㡫�, � ����)"
   &SAMPLE        = "��_���('������','safe01') = 840 "
   }   
   DEFINE INPUT  PARAMETER iContract     AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCont-Code    AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract     EQ iContract
                       AND loan.cont-code    EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
         out_Result = loan.currency.
   END.
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_�������"
   &DESCRIPTION   = "��।���� amt-rub."
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "�����"
   &SAMPLE        = "��_�������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE  vVal AS CHARACTER NO-UNDO.
   DEFINE VARIABLE  vSum AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE  vSumNDS AS DECIMAL   NO-UNDO.

   DEFINE VARIABLE vBegDate AS DATE     NO-UNDO.
   DEFINE VARIABLE vEndDate AS DATE     NO-UNDO.
   DEFINE VARIABLE vCountTO AS INT64    NO-UNDO.
   DEFINE VARIABLE vCountLC AS INT64    NO-UNDO.

   DEFINE BUFFER xloan-cond FOR loan-cond.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract     EQ iContract
                       AND loan.cont-code    EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".
      END.

      IF iIdnt EQ 1 THEN
      DO:

         vCountTO = 0.

         FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                             AND term-obl.cont-code EQ iCont-Code
                             AND term-obl.idnt      EQ iIdnt 
            NO-LOCK:
               vCountTO = vCountTO + 1.
         END.

         vCountLC = 0.

         FOR EACH loan-cond WHERE loan-cond.contract  EQ iContract
                              AND loan-cond.cont-code EQ iCont-Code
            NO-LOCK:
               vCountLC = vCountLC + 1.
         END.

         IF vCountTO GT vCountLC THEN
         DO:

            FIND FIRST ttDates WHERE ttDates.contract  EQ iContract
                                 AND ttDates.cont-code EQ iCont-Code
               NO-ERROR.

            IF NOT AVAILABLE ttDates THEN
            DO:
               beg-date = GetBaseOpDate().
               end-date = GetBaseOpDate().

               DO WHILE YES:
                  {getdates.i
                     &noinit=YES
                  }

                  FIND LAST  loan-cond WHERE loan-cond.contract  EQ iContract
                                         AND loan-cond.cont-code EQ iCont-Code
                                         AND loan-cond.since     LE beg-date
                     NO-LOCK NO-ERROR.

                  IF AVAILABLE loan-cond THEN
                  DO:
                     vBegDate = loan-cond.since. 

                     FIND FIRST xloan-cond WHERE xloan-cond.contract  EQ iContract
                                             AND xloan-cond.cont-code EQ iCont-Code
                                             AND xloan-cond.since     GT loan-cond.since
                        NO-LOCK NO-ERROR.

                     IF AVAILABLE xloan-cond THEN
                     DO:
                        vEndDate = xloan-cond.since. 
                     END.
                     ELSE
                     DO:
                        vEndDate = loan.end-date. 
                     END.

                     IF end-date LE vEndDate THEN
                        LEAVE.
                  END.
/*
MESSAGE
beg-date SKIP
vBegDate SKIP
end-date SKIP
vEndDate SKIP
VIEW-AS ALERT-BOX. 
*/

                  MESSAGE "�������� ���ࢠ� �� �������� �� � ���� �᫮��� �������" VIEW-AS ALERT-BOX. 
               END.

               CREATE ttDates.
               ASSIGN
                  ttDates.contract  = iContract
                  ttDates.cont-code = iCont-Code
                  ttDates.beg-date  = beg-date
                  ttDates.end-date  = end-date
               .
            END.
            ELSE
               ASSIGN
                  beg-date  = ttDates.beg-date
                  end-date  = ttDates.end-date
               .
            vSum = 0.0.

            FOR EACH term-obl WHERE term-obl.contract  EQ iContract
                                AND term-obl.cont-code EQ iCont-Code
                                AND term-obl.idnt      EQ iIdnt 
                                AND term-obl.end-date  GE beg-date
                                AND term-obl.end-date  LE end-date
               NO-LOCK:
               vSum = vSum + term-obl.amt-rub.
            END.
            out_Result = vVal + "|" + STRING(vSum).
            RETURN.
         END.
      END.

      IF iIdnt = 1
      THEN FIND LAST term-obl WHERE term-obl.contract  EQ iContract
                                AND term-obl.cont-code EQ iCont-Code
                                AND term-obl.idnt      EQ iIdnt 
         NO-LOCK USE-INDEX primary NO-ERROR.
      ELSE FIND LAST term-obl WHERE term-obl.contract  EQ iContract
                                AND term-obl.cont-code EQ iCont-Code
                                AND term-obl.idnt      EQ iIdnt 
                                AND term-obl.end-date  EQ iEnd-date
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE term-obl THEN
      DO: 
         is-ok = 0.
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।���� ��䨪 "
                         + (IF iIdnt = 1 THEN "�������� ���⥦�� �� �����"
                                         ELSE "��� ��室�� �� ���� " + GetNullStr(STRING(iEnd-date))
                           ) ).
      END.
      ELSE
         vSum = term-obl.amt-rub.
         vSumNDS = term-obl.int-amt.

      out_Result = vVal + "|" + STRING(vSum) + "|" + STRING(vSumNDS).
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_�������_��"
   &DESCRIPTION   = "��।���� amt-rub."
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "����� �� ���.����."
   &SAMPLE        = "��_�������_��()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSum    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vNDSSum AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vNDS    AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.

      FIND FIRST bloan WHERE bloan.class-code       EQ "loan-rent-dop" 
                         AND bloan.parent-contract  EQ loan.contract
                         AND bloan.parent-cont-code EQ loan.cont-Code
                         AND bloan.doc-ref          EQ loan.doc-ref + " 1"
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE bloan THEN
      DO:      
         ASSIGN
           out_Result = ""
           is-ok      = 0
         .

         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।����� ���.ᮣ��襭��.").
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF bloan.currency NE "" THEN
            vVal = bloan.currency.
         ELSE
            vVal = "NO".
      END.

      FIND LAST loan-cond WHERE loan-cond.contract  EQ bloan.contract
                            AND loan-cond.cont-code EQ bloan.cont-code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ""
            is-ok       = 0
         .
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� ���.ᮣ��襭�� � �������� " + iContract + "," + iCont-Code
                         + " �� ��।����� �᫮���.").
         LEAVE PROC.         
      END.

      vNDS = GetXAttrValue("loan-cond",
                           loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                           "���").

      FIND LAST term-obl WHERE term-obl.contract  EQ bloan.contract
                           AND term-obl.cont-code EQ bloan.cont-Code
                           AND term-obl.idnt      EQ iIdnt 
                           AND term-obl.end-date  EQ iEnd-date
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE term-obl THEN
      DO: 
          is-ok = 0.
          RUN Fill-SysMes("",
                          "",
                          "0",
                          "�� ���.ᮣ��襭�� � �������� " + iContract + "," + iCont-Code
                          + " �� ��।���� ��䨪 "
                          + (IF iIdnt EQ 1 THEN "�������� ���⥦�� �� �����"
                                           ELSE "��� ��室��")
                          + " �� ���� " + GetNullStr(STRING(iEnd-date) )).
      END.
      ELSE ASSIGN
         vSum    = term-obl.amt-rub
         vNdsSum = term-obl.ratio.

      out_Result = vVal + "|" + STRING(vSum) + "|" + vNDS + "|" + STRING(vNDSSum).
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.



{pfuncdef
   &NAME          = "��_�������_loan-cond"
   &DESCRIPTION   = "��।���� ������ �������,�㬬�,�㬬�,���,������ ������."
   &PARAMETERS    = "Contract,Cont-Code,End-date"
   &RESULT        = "���|�����|�����|���|���"
   &SAMPLE        = "��_�������_loan-cond()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE  vVal   AS CHARACTER  NO-UNDO. /* ����� */
   DEFINE VARIABLE  vSumm  AS DECIMAL    NO-UNDO. /* �㬬� - ��� */
   DEFINE VARIABLE  vSumm1 AS DECIMAL    NO-UNDO. /* �㬬� ��� */
   DEFINE VARIABLE  vNDS   AS DECIMAL    NO-UNDO. /* ��� */
   DEFINE VARIABLE  vCurr  AS CHARACTER  NO-UNDO. /* ����� ������ */

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN 
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".
      END.

      FIND LAST loan-cond WHERE loan-cond.contract  EQ iContract
                            AND loan-cond.cont-code EQ iCont-Code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।����� �᫮���.").
         LEAVE PROC.         
      END.
      ELSE
      DO:
          vNDS   = DECIMAL(GetXAttrValue("loan-cond",
                                         loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                         "���")).
          vSumm1 = DECIMAL(GetXAttrValue("loan-cond",
                                         loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                         "sum")) * vNDS / (100 + vNDS).
          vSumm  = DECIMAL(GetXAttrValue("loan-cond",
                                         loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                         "sum")) - vSumm1.
          vCurr  =     GetXAttrValue("loan-cond",
                                     loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                     "currency").

          IF vCurr = "" THEN
            vCurr = "NO".

          out_Result = vVal + "|"
                     + STRING(vSumm) + "|"
                     + STRING(vSumm1) + "|"
                     + STRING(vNDS) + "|"
                     + vCurr.
      END.
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_�������_loan-cond2"
   &DESCRIPTION   = "��।���� ������,�㬬�,���."
   &PARAMETERS    = "Contract,Cont-Code,iCod,End-date"
   &RESULT        = "���|�����|���"
   &SAMPLE        = "��_�������_loan-cond2()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCod           AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSumm                 AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE iAmount               AS DECIMAL    NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:
      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN 
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".
      END.

      FIND LAST loan-cond WHERE loan-cond.contract  EQ iContract
                            AND loan-cond.cont-code EQ iCont-Code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।����� �᫮���.").
         LEAVE PROC.         
      END.
      ELSE
      DO:
         FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                               AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code 
                               AND comm-rate.since      LE loan.open-date
            NO-LOCK NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:
            FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                                  AND comm-rate.filial-id = shfilial
                                  AND comm-rate.branch-id = ""
                                  AND comm-rate.kau        EQ ""
                                  AND comm-rate.since      LE vGendDate
               NO-LOCK NO-ERROR.

            IF NOT AVAILABLE comm-rate THEN
            DO:
               ASSIGN
                  out_Result  = ?
                  is-ok       = -1
               .
               RUN Fill-SysMes("",
                               "",
                               "0",
                               "�� �������� " + iContract + "," + iCont-Code
                               + " �� ������� �������.").
            LEAVE PROC.
            END.
         END.

         IF AVAILABLE comm-rate THEN
         DO:

            IF STRING(comm-rate.rate-fixed) EQ "YES" THEN
            DO:
               vSumm =(iEnd-date - loan.end-date) * comm-rate.rate-comm.
               
               IF FGetSetting("���䍄�","",?) EQ "NO" THEN
               DO:
                  vSumm = MAX(comm-rate.min-value, 
                              (vSumm + (vSumm  * DECIMAL(GetXAttrValue("loan-cond",
                                                                       loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                                                       "���")) / 100))).
               END.
               ELSE
                 vSumm = MAX(comm-rate.min-value,vSumm).
               out_Result = vVal + "|"
                          + STRING(vSumm) + "|"
                          +  GetXAttrValue("loan-cond", 
                                           loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since), 
                                           "���").
            END.
            ELSE
            DO:
               vSumm = DECIMAL(GetXAttrValue("loan-cond",
                                             loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                             "PaySum")).

               CASE FGetSetting("�������","",?) :
                  WHEN "1" THEN
                    vSumm = MAX(comm-rate.min-value, (vSumm / 100 * comm-rate.rate-comm)).               
                  WHEN "2" THEN
                    vSumm = MAX(comm-rate.min-value, (vSumm / (loan.end-date - loan-cond.since) * comm-rate.rate-comm / 100 * (iEnd-date - loan.end-date))).                
                  OTHERWISE
                  DO: 
                     RUN CalcSumRent(BUFFER loan, BUFFER loan-cond, loan.end-date, iEnd-date, OUTPUT iAmount).
                     vSumm = MAX(comm-rate.min-value, (iAmount * comm-rate.rate-comm / 100)).              
                  END.
               END CASE.
               out_Result = vVal + "|"
                          + STRING(vSumm) + "|"
                          + GetXAttrValue("loan-cond",
                                          loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                          "���").
            END.
         END.
      END.
   END.  /* of PROC block */
   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "���_�������"
   &DESCRIPTION   = "��।���� amt-rub."
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "����� ������� ��������"
   &SAMPLE        = "���_�������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSum                  AS DECIMAL    NO-UNDO. 

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".
      END.

      FOR EACH  term-obl WHERE term-obl.contract  EQ iContract
                           AND term-obl.cont-code EQ iCont-Code
                           AND term-obl.idnt      EQ iIdnt 
                           AND term-obl.end-date  GE DATE(1,1,YEAR(iEnd-date) + 1)
         NO-LOCK:

         vSum = vSum + term-obl.amt-rub.
      END.

      IF vSum EQ 0 THEN
      DO:
         ASSIGN is-ok = 0.
         RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + 
                                   " �� ��।���� ��䨪 " +
                                   (IF iIdnt EQ 1 THEN "�������� ���⥦�� �� �����" ELSE "��� ��室��") +
                                   " �� ���騩 ���.").
      END.
      out_Result = vVal + "|" + STRING(vSum).
   END.  /* of PROC block */
   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_����������"
   &DESCRIPTION   = "��।���� amt-cur."
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "����� � ������"
   &SAMPLE        = "��_����������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64      NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:
        
      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
         DO:
            FIND LAST term-obl WHERE term-obl.contract  EQ iContract
                                 AND term-obl.cont-code EQ iCont-Code
                                 AND term-obl.idnt      EQ iIdnt
                                 AND term-obl.end-date  EQ iEnd-date
               NO-LOCK NO-ERROR.

            IF NOT AVAILABLE term-obl THEN
            DO:      
              is-ok = 0.
               RUN Fill-SysMes("",
                               "",
                               "0",
                               "�� �������� " + iContract + "," + iCont-Code
                               + " �� ��।���� ��䨪 "
                               + (IF iIdnt EQ 1 THEN "�������� ���⥦�� �� �����"
                                                ELSE "��� ��室��")
                               + " �� ���� " + GetNullStr(STRING(iEnd-date))).
            END.
            ELSE
               out_Result = term-obl.amt-rub.
         END.
         ELSE
            out_Result  = ?.
      END.
   END.

   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_����������_loan-cond"
   &DESCRIPTION   = "��।���� amt-cur."
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "����� � ������"
   &SAMPLE        = "��_����������_loan-cond()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64      NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:
        
      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
         DO:
            FIND LAST  loan-cond WHERE loan-cond.contract   EQ iContract
                                   AND loan-cond.cont-code  EQ iCont-Code 
                                   AND loan-cond.since      LE vGendDate
               NO-LOCK NO-ERROR.

            IF NOT AVAILABLE loan-cond THEN
            DO:      
               ASSIGN
                 out_Result = ?
                 is-ok      = -1
               .

               RUN Fill-SysMes("",
                               "",
                               "0",
                               "�� �������� " + iContract + "," + iCont-Code
                               + " �� ��।����� �᫮���").
               LEAVE PROC.         
            END.
            ELSE 
               out_Result = DECIMAL(GetXAttrValue("loan-cond",
                                                  loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                                  "sum")).
         END.
      ELSE
         out_Result  = ?.
      END.
   END.

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_������"
   &DESCRIPTION   = "��।���� �㬬� ������"
   &PARAMETERS    = "�� �������"
   &RESULT        = "����� �� ��������"
   &SAMPLE        = "��_������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCod           AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSumm                 AS DECIMAL    NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".

         FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                               AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
                               AND comm-rate.since      LE loan.open-date
            NO-LOCK NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:

            FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                                  AND comm-rate.filial-id = shfilial
                                  AND comm-rate.branch-id = ""
                                  AND comm-rate.kau        EQ ""
                                  AND comm-rate.since      LE vGendDate
               NO-LOCK NO-ERROR.

            IF NOT AVAILABLE comm-rate THEN
            DO:
               RUN Fill-SysMes("",
                               "",
                               "0",
                               "�� �������� " + iContract + "," + iCont-Code
                               + " �� ������� �������.").
               out_Result  = vVal + "|" + STRING(0).
               LEAVE PROC.
            END.
         END.

         IF AVAILABLE comm-rate THEN
         DO:

             IF STRING(comm-rate.rate-fixed) EQ "YES" THEN
             DO:
                out_Result = vVal + "|" + STRING(comm-rate.rate-comm).
             END.
             ELSE
             DO:
                DO WITH FRAME fr-beg
                   ROW 10 1 COLUMNS
                   NO-LABELS CENTERED OVERLAY
                   ON ERROR UNDO, LEAVE
                   ON ENDKEY UNDO, LEAVE:

                   PAUSE 0.
                   DISPLAY
                      vSumm
                   .

                   UPDATE vSumm
                      VIEW-AS FILL-IN
                      FORMAT  ">>>>>>>>>9"
                      LABEL "�㬬� ����"
                      HELP "������ �㬬� ������뢠���� ����."
                   EDITING:
                      READKEY.
                      APPLY LASTKEY.
                   END.
                END.
                                                            
                HIDE FRAME fr-beg.

                IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
                   RETURN "END-ERROR".

                vSumm = MAX(comm-rate.min-value, (vSumm * comm-rate.rate-comm / 100)).
                out_Result = vVal + "|" + STRING(vSumm).
             END.
         END.
      END.
   END.
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_���"
   &DESCRIPTION   = "��।���� ���"
   &PARAMETERS    = "�� �������"
   &RESULT        = "���"
   &SAMPLE        = "��_���()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSum                  AS DECIMAL    NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:

         IF loan.currency NE "" THEN
            vVal = loan.currency.
         ELSE
            vVal = "NO".
      END.
      FIND LAST term-obl WHERE term-obl.contract  EQ iContract
                           AND term-obl.cont-code EQ iCont-Code
                           AND term-obl.idnt      EQ 1 
                           AND term-obl.end-date  EQ iEnd-date
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE term-obl THEN
      DO: 
         is-ok = 0.
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।���� ��䨪 ��� ��� �� ���� " + GetNullStr(STRING(iEnd-date))).
      END.
      ELSE
         vSum = term-obl.int-amt.

      out_Result = vVal + "|" + STRING(vSum).
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_������"
   &DESCRIPTION   = "��।���� �ப �७�� � ����� �祩��."
   &PARAMETERS    = "�� �������"
   &RESULT        = "�ப"
   &SAMPLE        = "��_������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan-cond WHERE loan-cond.contract  EQ iContract
                            AND loan-cond.cont-code EQ iCont-Code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iCont-Code
                         + " �� ��।����� �᫮���").
         LEAVE PROC.         
      END.
      ELSE
      DO:
        out_Result = GetXAttrValue("loan-cond",
                                   loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                   "����ப").         
        out_Result = IF GetXAttrValue("loan-cond",
                                      loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                      "safe-num") EQ ?
                     THEN out_Result  + "|" + ""
                     ELSE out_Result + "|" + GetXAttrValue("loan-cond",
                                                           loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since),
                                                           "safe-num").
      END.
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "���������_�����"
   &DESCRIPTION   = "�����頥� ⨯ � ����� ������ - ��ঠ⥫� ����, ஫� ���������� ��� ������, ����� ��� 2-�� ���浪�, ���᫥��� �� �����䨪���� Decision Table"
   &PARAMETERS    = "���������� ������,������������� ������[,���� ��������[,~
������� ������� �� ������� / ����������� ���� = "������" ("���"/"������")[,~
�������� ���� �������� = ���[,~
��������� ��������� ���� �������� = � ����ᨬ��� �� ����஥筮�� ��ࠬ��� � ����� DTDetermCust (YES/NO)[,~
��� �������������� = DecisionTable]]]]]"
   &RESULT        = "��������� ��������� ����� �� ��������"
   &SAMPLE        = "���������_�����()"
   }   
   DEFINE INPUT PARAMETER iContract        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iContCode        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date        AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER iCalendWork      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIncludeOpenDate AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iDTDetermCust    AS LOGICAL  NO-UNDO.
   DEFINE INPUT PARAMETER iCode            AS CHARACTER NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result      AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok           AS INT64      NO-UNDO.

   DEFINE VARIABLE vAcctNum   AS INT64      NO-UNDO.
   DEFINE VARIABLE vAcctNum1  AS INT64      NO-UNDO.             
   DEFINE VARIABLE vCustCat   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCustId    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctType  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vTerm         AS CHARACTER INITIAL "" NO-UNDO.
   DEFINE VARIABLE vDTType       AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vDTKind       AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vDTTerm       AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vDTCust       AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vMaskInternal AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vYY           AS INT64                NO-UNDO.
   DEFINE VARIABLE vDD           AS INT64                NO-UNDO.
   DEFINE VARIABLE vStr          AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vIsAmbiguous  AS LOGICAL              NO-UNDO.
   DEFINE VARIABLE vCalend       AS LOGICAL              NO-UNDO.
   DEFINE VARIABLE vOffSet       AS INT64                NO-UNDO.
   DEFINE VARIABLE vHoliday      AS CHARACTER            NO-UNDO.
   DEFINE VARIABLE vFlag         AS LOGICAL              NO-UNDO.

   DEFINE BUFFER loan FOR loan.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      {pchkpar iContract}
      {pchkpar iContCode}
   
      ASSIGN
         iContract   = TRIM(TRIM(iContract,'"'),"'")
         iContCode   = TRIM(TRIM(iContCode,'"'),"'")
         iCode       = IF NOT {assigned iCode} THEN FGetSetting("�302�", "DecTbl", "DecisionTable") 
                                              ELSE TRIM(TRIM(iCode,'"'),"'")
         iCalendWork = TRIM(TRIM(iCalendWork,'"'),"'")
         vCalend     = iCalendWork EQ "���"
         vOffSet     = IF CAN-DO("��,Yes",iIncludeOpenDate) THEN 1
                                                            ELSE 0
         is-ok       = 0
         out_Result  = ?
      . 

      RUN RE_B_LOAN IN h_Loan (iContract,iContCode,BUFFER loan).

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.

      IF GetCode("",iCode) = ? THEN
      DO:      
         ASSIGN
           out_Result = ""
           is-ok      = -1
         .

         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� ��।��� ��� �����䨪��� ��� ���᪠ ���� ��ண� ���浪�.").

         LEAVE PROC.         
      END.

      IF iEnd-date EQ ? THEN
         iEnd-date = vGendDate.

      FOR EACH  cust-role WHERE cust-role.file-name        EQ "loan"
                            AND cust-role.surrogate        EQ loan.contract + "," + loan.cont-code 
                            AND cust-role.open-date        LE iEnd-date
                            AND (   cust-role.close-date   GE iEnd-date
                                 OR cust-role.close-date   EQ ? )
         NO-LOCK:
                            
         IF GetXAttrValueEx("cust-role",
                            STRING(cust-role.cust-role-id),
                            "��ঊ���",
                            "") EQ "��" THEN 
         DO:
            ASSIGN 
               vCustCat = cust-role.cust-cat
               vCustId  = cust-role.cust-id
            .
            LEAVE.
         END.                       
      END.

      IF NOT {assigned vCustCat } OR
         NOT {assigned vCustId } THEN
      DO:      
         ASSIGN
            out_Result = ""
            is-ok      = -1
         .
         RUN Fill-SysMes("",
                         "",
                         "0",
                         "�� �������� " + iContract + "," + iContCode + " �� ��।��� ��ঠ⥫� ����.").
         LEAVE PROC.         
      END.

      vAcctNum  = 0.

      FOR EACH  loan-acct OF loan WHERE loan-acct.acct-type BEGINS "�इ��" 
                                    AND loan-acct.acct-type NE     "�इ��"
                                    AND loan-acct.since     LE     iEnd-date
         NO-LOCK:
          
            vAcctNum1 = INT64  (
                         SUBSTRING(loan-acct.acct-type, 7)
                               ) NO-ERROR.

            IF vAcctNum1 GT 0 THEN
               vAcctNum = MAX(vAcctNum, vAcctNum1).
 

            FIND FIRST acct OF loan-acct WHERE acct.cust-cat EQ vCustCat
                                           AND acct.cust-id  EQ INT64  (vCustId)
               NO-LOCK NO-ERROR.

            IF AVAILABLE acct THEN
            DO:
               vAcctType = loan-acct.acct-type.
               LEAVE.
            END.
      END.

      IF NOT {assigned vAcctType} THEN
         vAcctType = "�इ��" + STRING(vAcctNum + 1).

      RUN DTCust(cust-role.cust-cat,
                 INT64  (cust-role.cust-id),
                 iDTDetermCust,
                 OUTPUT vDTcust).

      ASSIGN
         vDTType = GetXAttrValueEx("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "DTType",
                                   GetXAttrInit(loan.class-code,"DTType"))
         vDTKind = GetXAttrValueEx("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "DTKind",
                                   GetXAttrInit(loan.class-code,"DTKind"))
      .

      IF vDTType = ? OR vDTType = "" THEN
         vDTType = "*".

      IF vDTKind = ? OR vDTKind = "" THEN
         vDTKind = "*".

      IF vDTTerm = ? OR vDTTerm = "" THEN
         vDTTerm = "*".

      ASSIGN
         vIsAmbiguous  = vDTType EQ "*"
                      OR vDTKind EQ "*"
                      OR vDTTerm EQ "*"
                      OR vDTCust EQ "*"
         vMaskInternal = vAcctType + CHR(1)
                       + vDTType + CHR(1)
                       + vDTCust + CHR(1)
                       + vDTKind + CHR(1)
                       + vDTTerm
         vStr          = ""
         vFlag         = TRUE WHEN loan.contract EQ "���"
      .

      IF vFlag THEN
         vHoliday = FGetSetting("���","�ᯊ�����","Holiday").

      FOR EACH code WHERE code.class  EQ "DTTerm"
                      AND code.parent EQ "DTTerm"
         NO-LOCK:

         IF IS-Term( loan.open-date,
                     loan.end-date,
                     code.code + CHR(1) + vHoliday,
                     vCalend,
                     vOffSet,
                     OUTPUT vYY,
                     OUTPUT vDD)
         THEN
            {additem.i vStr code.code}
      END.

      ASSIGN
         ENTRY(5,vMaskInternal,CHR(1)) = vStr
         mask                          = vMaskInternal
      .
      RUN cbracct.p(iCode,iCode,"DecisionTable",0).

      IF     pick-value NE ?
         AND pick-value NE ""
         AND GetBufferValue("bal-acct",
                            "WHERE bal-acct.bal-acct EQ '" + TRIM(pick-value) + "'",
                            "bal-acct") NE ?
      THEN
         ASSIGN
            out_Result = vAcctType + "|" + vCustCat + "|" + vCustId + "|" + pick-value
            is-Ok      = 0
         .
      ELSE
      DO:
         ASSIGN
            out_Result = ""
            is-ok      = -1
         .

         RUN Fill-SysMes("","","0","�� 㤠���� ��।����� ���� 2-�� ���浪�."). 

         LEAVE PROC.
      END.

   END.  /* of PROC block */

   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "���_��"
   &DESCRIPTION   = "�����頥� � ���� ᥩ�� ����"
   &PARAMETERS    = "Contract,ContCode,End-date,Cat,Code"
   &RESULT        = "����"
   &SAMPLE        = "���_��()"
   }   
   DEFINE INPUT PARAMETER iContract        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iContCode        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date        AS DATE       NO-UNDO.
   DEFINE INPUT PARAMETER iCat             AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCode            AS INT64      NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result      AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok           AS INT64      NO-UNDO.

   DEFINE VARIABLE vAcct                   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vAcctCode               AS CHARACTER  NO-UNDO.
  
PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:
 
      FOR EACH  loan-acct WHERE loan-acct.contract  EQ iContract
                            AND loan-acct.cont-code EQ iContCode
                            AND loan-acct.acct-type BEGINS "�ऊ�" 
                            AND loan-acct.since     LE iEnd-date
         NO-LOCK:

         FIND FIRST acct OF loan-acct WHERE acct.cust-cat EQ iCat
                                        AND acct.cust-id  EQ iCode
            NO-LOCK NO-ERROR.

         IF AVAILABLE acct THEN
         DO:
            vAcct     = acct.acct.
            vAcctCode = loan-acct.acct-type.
            LEAVE.
         END.
      END.

      IF NOT {assigned vAcct} THEN
      DO:
         ASSIGN
           out_Result = ?
           is-ok      = -1
         .
         RUN Fill-SysMes("","","0","�� 㤠���� ��।����� ���� ��ঠ⥫� ����"). 
      END.
      ELSE
      DO:
         ASSIGN
            out_Result = vAcct + "|" + vAcctCode
         .
      END.

   END.  /* of PROC block */

   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_������"
   &DESCRIPTION   = "��।���� �������"
   &PARAMETERS    = "Contract,Cont-Code,Cod"
   &RESULT        = "�������|⨯"
   &SAMPLE        = "��_������('������','safe01','%��琥�') = 1.000|%"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCod           AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      ELSE
      DO:
         FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                               AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
                               AND comm-rate.since      LE loan.open-date
            NO-LOCK NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:
            FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                                  AND comm-rate.filial-id = shfilial
                                  AND comm-rate.branch-id = ""
                                  AND comm-rate.kau        EQ ""
                                  AND comm-rate.since      LE vGendDate
               NO-LOCK NO-ERROR.

            IF NOT AVAILABLE comm-rate THEN
            DO:
               ASSIGN
                  out_Result  = ?
                  is-ok       = -1
               .
               RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + 
                                   " �� ������� �������.").
               LEAVE PROC.
            END.
         END.

         IF AVAILABLE comm-rate THEN
         DO:
            out_Result = STRING(comm-rate.rate-comm) + '|' + STRING(comm-rate.rate-fixed) + '|' + STRING(comm-rate.min-value).
         END.
      END.
   END.
   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_����"
   &DESCRIPTION   = "��।���� �㬬� ������� �� ᬥ�� �᫮��� ������� �७��."
   &PARAMETERS    = "Contract,Cont-Code"
   &RESULT        = "����� �������"
   &SAMPLE        = "��_����('������','safe01')"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vSince                AS DATE       NO-UNDO.
   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSum                  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vSum1                 AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vSum2                 AS DECIMAL    NO-UNDO.

   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-cond FOR loan-cond.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND LAST loan WHERE loan.contract  EQ iContract
                       AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         LEAVE PROC.         
      END.

      vVal = loan.currency.

      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                            AND loan-cond.cont-code EQ loan.cont-code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + " �� ��।����� �᫮���.").
         LEAVE PROC.         
      END.

      vSince = loan-cond.since.
      RUN CalcSumRent(BUFFER loan, BUFFER loan-cond, loan-cond.since, loan.end-date, OUTPUT vSum2).

      FIND PREV loan-cond WHERE loan-cond.contract  EQ loan.contract
                            AND loan-cond.cont-code EQ loan.cont-code 
                            AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ?
            is-ok       = -1
         .
         RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + " �� ���﫮�� �᫮���.").
         LEAVE PROC.         
      END.

      RUN CalcSumRent(BUFFER loan, BUFFER loan-cond, vSince, loan.end-date, OUTPUT vSum1).

      IF vSum2 > vSum1 THEN vSum = vSum2 - vSum1.

      out_Result = vVal + "|" + STRING(vSum).
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_���������"
   &DESCRIPTION   = "�������� ����� �� ��� ���������"
   &PARAMETERS    = "���������� ��������,������������� ��������,��� �������� �� ���������,���� ����� ������� ��������,���� ������ ��������,���� ������� ��������~
                    [,���� ������ ������� ��������]"
   &RESULT        = "���|�����|���"
   &SAMPLE        = "��_���������('������','safe01','%�ए�',����(),AcctDb,AcctCr);"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCod           AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE       NO-UNDO.
   DEFINE INPUT PARAMETER iAcctDb        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iAcctCr        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iBeg-date      AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSumm                 AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vNDS                  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vTarifNDS             AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vLastSumm             AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vPaySumm              AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vBegDate              AS DATE       NO-UNDO.
   DEFINE VARIABLE vPeriod               AS INT64      NO-UNDO.

   DEFINE VARIABLE oConst    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vGendDate AS DATE NO-UNDO.

   RUN GetConstant IN h_pbase("__OPDATE", OUTPUT oConst).
   vGendDate = DATE(oConst) NO-ERROR.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND FIRST loan WHERE loan.contract  EQ iContract
                        AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN 
      DO:      
         ASSIGN
            out_Result = ""
            is-ok      = -1
         .
         LEAVE PROC.         
      END.

      FIND LAST  loan-cond WHERE loan-cond.contract  EQ iContract
                             AND loan-cond.cont-code EQ iCont-Code 
                             AND loan-cond.since     LE vGendDate
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan-cond THEN 
      DO: 
         ASSIGN
            out_Result  = ""
            is-ok       = 0
         .
         RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + " �� ��।����� �᫮���.").
         LEAVE PROC.         
      END.

      vVal     = IF loan.currency NE "" THEN loan.currency
                                        ELSE "NO".

      vBegDate = IF iBeg-date NE ? 
                 THEN MAX(iBeg-date, loan.end-date)
                 ELSE MAX(DATE(MONTH(vGEndDate),01,YEAR(vGEndDate)), loan.end-date).


      vPeriod  = iEnd-date - vBegDate.

      IF vBegDate > loan.end-date THEN vPeriod = vPeriod + 1.

      IF vPeriod LE 0 THEN
      DO:
         ASSIGN
            out_Result  = ""
            is-ok       = 0
         .
         RUN Fill-SysMes("","","0","��࠭� ����ୠ� ��� ����� ��ਮ�� ������ �� ������. " +
                         " ��砫� ��ਮ�� : " + STRING(vBegDate) + 
                         " ����� ��ਮ��(��� �����) : " + GetNullStr(STRING(iEnd-date))).
         vPeriod = 0.
      END.

      FIND  LAST comm-rate WHERE comm-rate.commission EQ iCod
                             AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code 
                             AND comm-rate.since      <= loan.open-date
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE comm-rate THEN
      DO:
         FIND LAST comm-rate WHERE comm-rate.commission EQ iCod
                               AND comm-rate.filial-id = shfilial
                               AND comm-rate.branch-id = ""
                               AND comm-rate.kau        EQ ""
                               AND comm-rate.since      LE vGendDate
            NO-LOCK NO-ERROR.

         IF NOT AVAILABLE comm-rate THEN
         DO:
            ASSIGN
               out_Result  = ""
               is-ok       = 0
            .
            RUN Fill-SysMes("","","0","�� �������� " + iContract + "," + iCont-Code + 
                        " �� ��।����� ������� � ����� " + iCod).
         END.
      END.

      IF AVAILABLE comm-rate THEN
      DO:
         vTarifNDS    = DECIMAL(GetXAttrValue("loan-cond", 
                                              loan-cond.contract + ","
                                              + loan-cond.cont-code + ","
                                              + STRING(loan-cond.since), 
                                              "���")).

         IF STRING(comm-rate.rate-fixed) EQ "YES" THEN
         DO: 
             vSumm = vPeriod * comm-rate.rate-comm.

             /* �᫨ ����஥�� ��ࠬ��� ���䍄��� = YES �,�㬬� ������ �㤥� ࠢ�� �㬬� �७���� ����� */
             IF FGetSetting("���䍄���","",?) EQ "YES" THEN
             DO:
                vNDS  = vSumm * vTarifNDS /(vTarifNDS + 100).
                vSumm = MAX(comm-rate.min-value, (vSumm - vNDS)).
             END.
             ELSE
               vSumm = MAX(comm-rate.min-value, vSumm).
         END.
         ELSE 
         DO:
            vPaySumm = DECIMAL(GetXAttrValue("loan-cond",
                                             loan-cond.contract  + ","
                                             + loan-cond.cont-code + ","
                                             + STRING(loan-cond.since),
                                             "PaySum"))
               NO-ERROR.

            IF vPaySumm EQ ? THEN
               vPaysumm = 0.

            CASE FGetSetting("�������","",?) :
               WHEN "1" THEN
                  vSumm = vPaySumm.               
               WHEN "2" THEN
                  vSumm = vPaySumm / (loan.end-date - loan-cond.since) * vPeriod NO-ERROR.                
               WHEN "3" THEN
               DO:

                  IF vBegDate GT loan.end-date THEN
                     iEnd-date = iEnd-date + 1.
                  RUN CalcSumRent(BUFFER loan, BUFFER loan-cond, vBegDate, iEnd-date , OUTPUT vSumm).
               END.
            END CASE.

            vNDS = vSumm * vTarifNDS /(vTarifNDS + 100).
            vSumm = MAX(comm-rate.min-value, ((vSumm - vNDS) * comm-rate.rate-comm / 100)).
         END.

         FOR EACH  op-entry WHERE op-entry.acct-db  EQ iAcctDb
                              AND op-entry.acct-cr  EQ iAcctCr
                              AND op-entry.kau-db   EQ (iContract + ',' + iCont-Code) NO-LOCK, 
             FIRST op OF op-entry WHERE op.op-date EQ vGendDate NO-LOCK:

            vLastSumm = vLastSumm + IF op-entry.currency EQ ""
                                    THEN op-entry.amt-rub 
                                    ELSE op-entry.amt-cur.
         END.
         vSumm = vSumm - vLastSumm.
      END.
      IF vSumm LT 0 OR vSumm EQ ? THEN
         vSumm = 0.

      out_Result = vVal + "|" + STRING(vSumm).

   END.  /* of PROC block */
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "�������_��_���"
   &DESCRIPTION   = "��।���� ���⮪ �� kau"
   &PARAMETERS    = "Acct,Kau"
   &RESULT        = "�������"
   &SAMPLE        = "�������_��_���(Acct,'�७��,' + safe-num)"
   }
   DEFINE INPUT PARAMETER iAcct          AS CHARACTER  NO-UNDO. /* ���� */
   DEFINE INPUT PARAMETER iKau           AS CHARACTER  NO-UNDO. /* ������ */

   DEFINE OUTPUT PARAMETER out_Result    AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.
   DEFINE VARIABLE vVal                  AS CHARACTER  NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      vVal = SUBSTR(iAcct,6,3).

      IF vVal EQ "810" THEN
         vVal = "".

      FIND FIRST acct WHERE acct.acct     EQ iAcct
                        AND acct.currency EQ vVal
         NO-LOCK NO-ERROR.

      IF AVAILABLE acct THEN
      DO:

         FIND FIRST kau WHERE kau.acct     EQ iAcct
                          AND kau.currency EQ vVal
                          AND kau.kau      EQ iKau
            NO-LOCK NO-ERROR.

         IF AVAILABLE kau THEN
         DO:

            IF vVal = "" THEN
            DO:

                IF acct.side EQ "�" THEN
                  out_Result = kau.balance * (-1).

                IF acct.side EQ "�" THEN
                  out_Result = kau.balance.
            END.
            ELSE
            DO:

                IF acct.side EQ "�" THEN
                  out_Result = kau.curr-bal * (-1).

                IF acct.side EQ "�" THEN
                   out_Result = kau.curr-bal.
            END.
         END.
         ELSE
         DO: 
            ASSIGN
               out_Result  = 0
               is-ok       = 0
            .
            RUN Fill-SysMes("","","0","�� ���� " + iAcct + " ��� �����⨪�.").
            LEAVE PROC.         
         END.
      END.
   END.  /* of PROC block */
   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��������_���"
   &DESCRIPTION   = "������� kau"
   &PARAMETERS    = "Acct,Kau,Op,Op-entry"
   &RESULT        = "������� ������ ���"
   &SAMPLE        = "��������_���(Acct,Kau,op,op-entry)"
   }
   DEFINE INPUT PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iKau           AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iOp            AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iOp-entry      AS INT64      NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND FIRST acct WHERE acct.acct EQ iAcct
        NO-LOCK NO-ERROR.

      IF AVAILABLE acct THEN
      DO:
         FIND FIRST op-entry WHERE op-entry.op       EQ iOp
                               AND op-entry.op-entry EQ iOp-entry
           NO-LOCK NO-ERROR.

         IF AVAILABLE op-entry THEN
         DO:
/* ------------- ACHTUNG! MINEN!!! --------------------*/            

            IF iAcct EQ op-entry.acct-cr THEN
            DO:
               {kau(op).fnd &op-entry=op-entry &db-cr=-cr}
               IF NOT AVAILABLE kau THEN
                 {kau(op).cr &op-entry=op-entry &db-cr=-cr}
               {kau(off).cal &op-entry=op-entry &ssum="- op-entry.amt-rub"
                 &inc=1 &scur="- op-entry.amt-cur"}
            END.
            IF iAcct EQ op-entry.acct-db THEN DO:
               {kau(op).fnd &op-entry=op-entry &db-cr=-db}
               IF NOT AVAILABLE kau THEN
                 {kau(op).cr &op-entry=op-entry &db-cr=-db }
                 {kau(off).cal &op-entry=op-entry &ssum=op-entry.amt-rub
                     &inc=1 &scur=op-entry.amt-cur}
            END.
/* ==================================================== */
            out_Result = YES.
         END.
         ELSE
            is-ok = 0.
      END.
      ELSE
      DO:
         is-ok = 0.
         RUN Fill-SysMes("","","0","�� ������ ���" + iAcct).
      END.
   END.  /* of PROC block */
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��������_���_���"
   &DESCRIPTION   = "������� kau"
   &PARAMETERS    = "Acct,Kau,Op,Op-entry"
   &RESULT        = "������� ������ ���"
   &SAMPLE        = "��������_���(Acct,Kau,op,op-entry)"
   }
   DEFINE INPUT PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iKau           AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iOp            AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iOp-entry      AS INT64      NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND FIRST acct WHERE acct.acct EQ iAcct
         NO-LOCK NO-ERROR.

      IF AVAILABLE acct THEN
      DO:

         FIND FIRST op-entry WHERE op-entry.op       EQ iOp
                               AND op-entry.op-entry EQ iOp-entry
            NO-LOCK NO-ERROR.

         IF AVAILABLE op-entry THEN
         DO:

/* ------------- ACHTUNG! MINEN!!! --------------------*/            
            IF iAcct EQ op-entry.acct-cr THEN DO:
                {kau(rub).fnd &op-entry=op-entry &db-cr=-cr}
                IF NOT AVAILABLE kau THEN
                  {kau(rub).cr &op-entry=op-entry &db-cr=-cr}
                {kau(rub).cal &op-entry=op-entry &ssum="- op-entry.amt-rub"
                  &inc=1 &scur="- op-entry.amt-cur"}
            END.
            IF iAcct EQ op-entry.acct-db THEN DO:
                {kau(rub).fnd &op-entry=op-entry &db-cr=-db}
                IF NOT AVAILABLE kau THEN
                  {kau(rub).cr &op-entry=op-entry &db-cr=-db }
                  {kau(rub).cal &op-entry=op-entry &ssum=op-entry.amt-rub
                      &inc=1 &scur=op-entry.amt-cur}
            END.
/* ==================================================== */
            out_Result = YES.
         END.
         ELSE
            is-ok = 0.
      END.
      ELSE
      DO:
         ASSIGN is-ok = 0.
         RUN Fill-SysMes("","","0","�� ������ ���" + iAcct).
      END.
   END.  /* of PROC block */
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��������_���2"
   &DESCRIPTION   = "�᫨ �� ᮧ���� ������ - ᮧ���� kau �� ��������, � ��⨢��� ��砥 ����� ���⮪ �� �㬬� �஢����"
   &PARAMETERS    = "Op,Op-entry,Surrogate"
   &RESULT        = "�᫨ �� ᮧ���� ������ - ᮧ���� kau �� ��������, � ��⨢��� ��砥 ����� ���⮪ �� �㬬� �஢����"
   &SAMPLE        = "��������_���2(op,op-entry,surrogate)"
   }
   DEFINE INPUT PARAMETER iOp            AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iOp-entry      AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iSurr          AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FOR FIRST op-entry WHERE op-entry.op       EQ iOp
                           AND op-entry.op-entry EQ iOp-entry
         NO-LOCK:

/* ------------- ACHTUNG! MINEN!!! --------------------*/            
          IF op-entry.kau-cr EQ iSurr THEN
          DO:
              {kau(op).fnd &op-entry=op-entry &db-cr=-cr}
              IF NOT AVAILABLE kau THEN
              {kau(op).cr &op-entry=op-entry &db-cr=-cr}
              {kau(off).cal &op-entry=op-entry &ssum="- op-entry.amt-rub" &inc=1 &scur="- op-entry.amt-cur"}
          END.
          
          IF op-entry.kau-db EQ iSurr THEN
          DO:
              {kau(op).fnd &op-entry=op-entry &db-cr=-db}
              IF NOT AVAILABLE kau THEN
              {kau(op).cr &op-entry=op-entry &db-cr=-db }
              {kau(off).cal &op-entry=op-entry &ssum=op-entry.amt-rub &inc=1 &scur=op-entry.amt-cur}
          END.
/* ==================================================== */
      END.

      ASSIGN
         out_Result = YES
         is-ok      = 0
      .

   END.  /* of PROC block */
   RETURN.
END PROCEDURE.



{pfuncdef
   &NAME          = "SURR_SAFE"
   &DESCRIPTION   = "���� ������� �� ����� �� ����७��� ⠡��� sysconf"
   &RESULT        = "�뤠�� ���ண�� �������, �᫨ ������� ������. �᫨ �� ������ - �஡��."
   &SAMPLE        = "SURR_SAFE()"
   }
   DEFINE OUTPUT PARAMETER out_Result AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64      NO-UNDO.

   DEFINE VARIABLE vContract AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vContCode AS CHARACTER  NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

       vContract = GetSysConf("SFContract").
       vContCode = GetSysConf("SFContCode").

       /* RUN Fill-SysMes("","","0",vContract + "," + vContCode). */

       IF {assigned vContract} AND
          {assigned vContCode} THEN
          FIND FIRST loan WHERE loan.contract  EQ vContract
                            AND loan.cont-code EQ vContCode
            NO-LOCK NO-ERROR.

       out_Result = IF AVAILABLE loan THEN loan.contract + "," + loan.cont-code
                                  ELSE "".
          
       is-ok = 0.

   END.  /* of PROC block */
   RETURN.
END PROCEDURE.


{pfuncdef
   &NAME          = "��_��������"
   &DESCRIPTION   = "����������� ����� �� ��������� �������� �� ��������� ����"
   &PARAMETERS    = "Contract,Cont-Code,AcctDb,AcctCr,OpCod,End-date"
   &RESULT        = "������|�����"
   &SAMPLE        = "��_���������()"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iAcctDb        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iAcctCr        AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iOpCod         AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iEnd-date      AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vSumm                 AS DECIMAL    NO-UNDO.

   DEFINE VARIABLE vIsKau-db             AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE visKau-cr             AS LOGICAL    NO-UNDO.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND FIRST loan WHERE loan.contract  EQ iContract
                        AND loan.cont-code EQ iCont-Code
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN 
      DO:      
         ASSIGN
            out_Result = ""
            is-ok      = -1
         .
         LEAVE PROC.         
      END.

      FOR EACH  op-entry WHERE op-entry.acct-db  EQ iAcctDb
                           AND op-entry.acct-cr  EQ iAcctCr
                           AND op-entry.currency EQ loan.currency
                           AND op-entry.op-cod   EQ iOpCod NO-LOCK,
          FIRST op OF op-entry WHERE op.op-date EQ iEnd-date
            NO-LOCK:

         IF GetXAttrValueEx("op",
                            STRING(op.op),
                            "cont-code",
                            "")             EQ iContract + "," + iCont-Code THEN
            vSumm = vSumm + IF op-entry.currency EQ "" THEN op-entry.amt-rub 
                                                       ELSE op-entry.amt-cur.
      END.

      IF vSumm LT 0 OR vSumm EQ ? THEN
         vSumm = 0.

      out_Result = loan.currency + "|" + STRING(vSumm).

   END.  /* of PROC block */
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_������"
   &DESCRIPTION   = "������ �������� ���� �� �������� � �������� �������"
   &PARAMETERS    = "Contract,Cont-Code,idnt,Beg-date,End-date"
   &RESULT        = "����"
   &SAMPLE        = "��_������('������','0001-2009/05',1,����(10/10/10), ����())"
   }   
   DEFINE INPUT PARAMETER iContract      AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iCont-Code     AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iIdnt          AS INT64      NO-UNDO.
   DEFINE INPUT PARAMETER iBegdate       AS DATE       NO-UNDO.
   DEFINE INPUT PARAMETER iEnddate       AS DATE       NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE BUFFER loan     FOR loan.
   DEFINE BUFFER term-obl FOR term-obl.

   FIND FIRST loan WHERE 
              loan.contract  EQ iContract
          AND loan.cont-code EQ iCont-Code
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE loan THEN DO:
      ASSIGN
         out_Result = ""
         is-ok      = -1
      .
      RETURN.
   END.

   FIND FIRST term-obl WHERE term-obl.cont-code EQ loan.cont-code
                         AND term-obl.contract  EQ loan.contract
                         AND term-obl.idnt      EQ iIdnt 
                         AND term-obl.end-date  LE iEndDate
                         AND term-obl.end-date  GE iBegDate
      NO-LOCK NO-ERROR.

   IF AVAIL term-obl THEN 
      out_Result = STRING(term-obl.end-date).
   ELSE 
      out_Result = ?.

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "��_����_���"
   &DESCRIPTION   = "��।���� ��������� ����樨 �� ����७����."
   &PARAMETERS    = "Contract,Cont-Code,<���������>"
   &RESULT        = "YES/NO"
   &SAMPLE        = "��_����_���('������','safe01','�ऊ���')"
   }   
   DEFINE INPUT  PARAMETER iContract     AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCont-Code    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iOpKonrDov    AS CHARACTER  NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS LOGICAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vVal                  AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vName                 AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vDocNum               AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vType                 AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vTypeName             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vInitName             AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vProxy                AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vDet                  AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vClName               AS CHARACTER  EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vTrans                AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vCustId               AS INT64               NO-UNDO.
   DEFINE VARIABLE vRules                AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vProxes               AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vYN                   AS LOGICAL             NO-UNDO.
   DEFINE VARIABLE vNameOp               AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE vOpDate               AS DATE                NO-UNDO.

   DEFINE VARIABLE mCustRoles            AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mCustRolesNames       AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mCustCurrent          AS CHARACTER           NO-UNDO.
   DEFINE VARIABLE mRecIdTT              AS RECID               NO-UNDO.

   DEFINE VARIABLE vItem                 AS INT64               NO-UNDO.
   DEFINE VARIABLE vStrTMP               AS CHARACTER           NO-UNDO.

   DEFINE BUFFER loan             FOR loan.
   DEFINE BUFFER xloan            FOR loan.
   DEFINE BUFFER cust-role        FOR cust-role.
   DEFINE BUFFER signs            FOR signs.
   DEFINE BUFFER xsigns           FOR signs.
   DEFINE BUFFER ysigns           FOR signs.
   DEFINE BUFFER loan-transaction FOR loan-transaction.
   DEFINE BUFFER code             FOR code.

   FORM
      SKIP(1)
      vNameOp                   LABEL "������"
                                FORMAT "x(50)"
      SKIP(1)
      vName                     LABEL "�७����"
                                HELP  "�७����"
                                FORMAT "x(50)"
      vDocNum                   LABEL "����� �������"
                                FORMAT "x(20)"
      vTypeName                 LABEL "��� ���樠��"
                                HELP  "��� ���樠�� ����樨(F1)"
                                FORMAT "x(20)"
      vInitName                 LABEL "���樠��"
                                FORMAT "x(50)"
      vProxy                    LABEL "����७�����"
      vDet                      LABEL "�ᮡ� �⬥⪨"
                                FORMAT "x(250)"
                                VIEW-AS FILL-IN SIZE 50 BY 3
      vTrans                    LABEL  "���� ����権"
                                FORMAT "x(50)"
   WITH FRAME edit TITLE COLOR bright-white "[ ����஫� �������稩 ]"
      1 DOWN CENTERED OVERLAY SIDE-LABELS 1 COL
      SIZE 77 BY 15
   .

   vRules  = FGetSetting("��������", "" , "--����--").
   vProxes = FGetSetting("���������⎯��", "" , "--����--").
   mProxySurr   = "".
   mProxyTrSurr = "".
   vOpDate      = GetBaseOpDate().


   ON "ENTER", "TAB" OF vTypeName IN FRAME edit
   DO:
      RETURN NO-APPLY.
   END.

   ON ANY-PRINTABLE OF vTypeName IN FRAME edit
   DO:
      RETURN NO-APPLY.
   END.

   ON "F1" OF vTypeName IN FRAME edit
   DO:
      RUN getclass.p("��� ���樠��",
                     "rent-cust",
                     NO,
                     "R",
                     4).

      IF (LASTKEY EQ KEYCODE("Enter") OR KEYFUNCTION(LASTKEY) EQ "GO") AND pick-value NE ? THEN
      DO:
         ASSIGN
            vType                  = pick-value
            vTypeName:SCREEN-VALUE = GetClassName(pick-value)
            vProxy:SCREEN-VALUE    = ""
            vDet:SCREEN-VALUE      = ""
            vInitName:SCREEN-VALUE = ""
            vTrans:SCREEN-VALUE    = ""
            vTrans                 = "".
         .

         IF CAN-DO(vRules, vType) THEN
         DO:
            vTrans:SCREEN-VALUE    = "*".
            vTrans                 = "*".
            RETURN.
         END.
         vInitName:SCREEN-VALUE = "".

         FOR EACH cust-role WHERE cust-role.file-name  EQ "loan"
                              AND cust-role.surrogate  EQ iContract + "," + iCont-Code
                              AND cust-role.class-code EQ pick-value
                              AND (   cust-role.close-date EQ ?
                                   OR cust-role.close-date GE vOpDate)
            NO-LOCK:

            RUN GetCustName IN h_base ( "�", 
                                        INT64  (cust-role.cust-id), 
                                        "",
                                        OUTPUT       vClName[1],
                                        OUTPUT       vClName[2],
                                        INPUT-OUTPUT vClName[3] ).

            FOR EACH xloan WHERE xloan.class-code   EQ "proxy-rent"
                             AND xloan.cust-cat     EQ "�"
                             AND xloan.cust-id      EQ vCustId
                             AND xloan.end-date     GE vOpDate
               NO-LOCK,
                EACH signs WHERE signs.file-name    EQ "loan"
                             AND signs.code         EQ "agent-id"
                             AND signs.surrogate    EQ xloan.contract + "," + xloan.cont-code
                             AND signs.code-value   EQ cust-role.cust-id
               NO-LOCK:
               mProxySurr   = xloan.contract + "," + xloan.cont-code.

               IF xloan.deal-type EQ YES THEN
               DO:
                  vTrans                 = "*".

                  CREATE ttProxies.

                  ASSIGN
                     ttProxies.fInitName = vClName[1] + " " + vClName[2]
                     ttProxies.fCustId   = cust-role.cust-id
                     ttProxies.fTrans    = "*"
                     ttProxies.fProxy       = xloan.doc-num
                  .
                  ttProxies.fDet         = GetXAttrValueEx("loan",
                                                           signs.surrogate,
                                                           "�ᮡ륎⬥⪨",
                                                           "").
               END.
               ELSE
               DO:
               
                  IF CAN-DO(vProxes, xloan.cont-type) THEN
                  DO:

                     FOR EACH loan-transaction WHERE loan-transaction.contract  EQ xloan.contract
                                                 AND loan-transaction.cont-code EQ xloan.cont-code
                        NO-LOCK,
                         EACH ysigns WHERE ysigns.file-name    EQ "loan-transaction"
                                       AND ysigns.code         EQ "loan-cont-code"
                                       AND ysigns.surrogate    EQ loan-transaction.contract + "," + loan-transaction.cont-code + "," + loan-transaction.trans-code
                                       AND ysigns.xattr-value  EQ iCont-Code
                        NO-LOCK,
                         EACH xsigns WHERE xsigns.file-name    EQ "loan-transaction"
                                       AND xsigns.code         EQ "loan-contract"
                                       AND xsigns.surrogate    EQ loan-transaction.contract + "," + loan-transaction.cont-code + "," + loan-transaction.trans-code
                                       AND xsigns.xattr-value  EQ iContract
                        NO-LOCK,
                         EACH code WHERE code.class EQ "�����_���뎯"
                                     AND CAN-DO( "safe", code.misc[1])
                                     AND code.code  EQ loan-transaction.trans-type
                        NO-LOCK:
                           mProxyTrSurr = loan-transaction.cont-code + "," + loan-transaction.trans-code + "," + loan-transaction.trans-code.

                           CREATE ttProxies.

                           ASSIGN
                              ttProxies.fInitName    = vClName[1] + " " + vClName[2]
                              ttProxies.fCustId      = cust-role.cust-id
                              ttProxies.fTrans       = code.description[1]
                              ttProxies.fProxySurr   = mProxyTrSurr
                              ttProxies.fProxy       = xloan.doc-num
                           .
                           ttProxies.fDet         = GetXAttrValueEx("loan",
                                                                    signs.surrogate,
                                                                    "�ᮡ륎⬥⪨",
                                                                    "").
                           vTrans                 = code.description[1].
                     END.
                  END.
               END.
            END.

         END.

         ASSIGN
            mCustRoles      = ""
            mCustRolesNames = ""
         .

         FOR EACH ttProxies GROUP BY fCustId:

            IF FIRST-OF(fCustId) THEN
            DO:
               {additem.i mCustRoles ttProxies.fCustId}
               {additem.i mCustRolesNames ttProxies.fInitName}
               mCustCurrent = ttProxies.fCustId.
            END.
         END.

         IF NUM-ENTRIES(mCustRoles) GT 1 THEN
         DO:
            RUN messmenu.p( 9,
                            "����७��� ���",
                            "�롥�� ���樠�� ����樨",
                            mCustRolesNames
                           ).

            mCustCurrent = ENTRY(INT64  (pick-value), mCustRoles).
            
         END.

         ASSIGN
            mCustRoles      = ""
            mCustRolesNames = ""
         .

         FOR EACH ttProxies WHERE ttProxies.fCustId EQ mCustCurrent GROUP BY fProxy:

            IF FIRST-OF(fProxy) THEN
            DO:
               {additem.i mCustRoles ttProxies.fProxy}
               {additem.i mCustRolesNames STRING(RECID(ttProxies))}
               mRecIdTT = RECID(ttProxies).
            END.
         END.

         IF NUM-ENTRIES(mCustRoles) GT 1 THEN
         DO:
            RUN messmenu.p( 9,
                            "����७�����",
                            "�롥�� ����७�����",
                            mCustRoles
                           ).

            mRecIdTT = INT64  (ENTRY(INT64  (pick-value), mCustRolesNames)).
            
         END.

         FIND FIRST ttProxies WHERE RECID(ttProxies) EQ mRecIdTT NO-ERROR.

         IF AVAILABLE ttProxies THEN
         DO:
            ASSIGN
               vInitName:SCREEN-VALUE = ttProxies.fInitName
               vProxy:SCREEN-VALUE    = ttProxies.fProxy
               vDet:SCREEN-VALUE      = ttProxies.fDet
               vTrans:SCREEN-VALUE    = ttProxies.fTrans
            .

            FOR EACH ttProxies WHERE ttProxies.fProxy EQ vProxy:SCREEN-VALUE:

               DO vItem = 1 TO NUM-ENTRIES(ttProxies.fTrans):

                  vStrTMP = ENTRY(vItem, ttProxies.fTrans).

                  IF LOOKUP(vStrTMP, vTrans:SCREEN-VALUE) EQ 0 THEN
                  DO:
                     {additem.i vTrans:SCREEN-VALUE vStrTMP}
                  END.
               END.
            END.
            vTrans = vTrans:SCREEN-VALUE.
         END.
      END.
   END.

PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      IF GetCodeEx("���������", iOpKonrDov, "") EQ "" THEN
      DO:
         out_Result  = YES.
         RETURN.
      END.

      vYN = NO.

      RUN message.p ("������ �� ����७����?","","QUESTION","YES-NO","", vYN).

      vYN = NOT (pick-value EQ "NO" OR pick-value EQ ?).

      IF NOT vYN THEN
      DO:
         out_Result  = YES.
         RETURN.
      END.

      FIND LAST loan WHERE loan.contract     EQ iContract
                       AND loan.cont-code    EQ iCont-Code
                       AND loan.class-code   EQ "loan-rent"
         NO-LOCK NO-ERROR.

      IF NOT AVAILABLE loan THEN
      DO:      
         ASSIGN
            out_Result  = NO
            is-ok       = -1
         .
         LEAVE PROC.         
      END.
      vDocNum = loan.doc-ref.
      RUN GetCustName IN h_base ( loan.cust-cat,
                                  loan.cust-id, 
                                  "",
                                  OUTPUT       vClName[1],
                                  OUTPUT       vClName[2],
                                  INPUT-OUTPUT vClName[3] ).
      vName   = vClName[1] + " " + vClName[2].
      vCustId = loan.cust-id.
      vNameOp = iOpKonrDov + " - " + GetCodeNameEx("���������", iOpKonrDov, "").

      DISPLAY
         vNameOp   
         vName    
         vDocNum  
         vTypename
         vInitName
         vProxy   
         vDet     
         vTrans
         WITH FRAME edit.

      SET
         vTypeName
         WITH FRAME edit.

      out_Result = CAN-DO(vTrans, iOpKonrDov).

      HIDE FRAME edit.

      IF NOT out_Result THEN
         MESSAGE "� ���樠�� ��� ����७���� �� �믮������ �⮩ ����樨"
         VIEW-AS ALERT-BOX ERROR. 
   END.
   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "�����_���"
   &DESCRIPTION   = "�����頥� ४������ ����७����"
   &PARAMETERS    = "���"
   &RESULT        = "��������"
   &SAMPLE        = "�����_���('���') - ����� ����७����~~n~
�����_���('���') - ��� �뤠� ����७����~~n~
�����_���('���') - ������������ ⨯� ����७����~~n~
�����_���('ID')  - id ����७���� ���~~n~
�����_���('SUR') - ���ண�� ����७����"
}   

   DEFINE INPUT PARAMETER iCode AS CHARACTER NO-UNDO.

   DEFINE OUTPUT PARAMETER out_Result    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok         AS INT64      NO-UNDO.

   DEFINE VARIABLE vContract  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vContCode  AS CHARACTER NO-UNDO.

   is-ok      = 0.
   out_Result = "".

   IF NUM-ENTRIES(mProxySurr) NE 2 THEN
      RETURN.

   vContract = ENTRY(1, mProxySurr).
   vContCode = ENTRY(2, mProxySurr).

   FIND LAST loan WHERE loan.contract  EQ vContract
                    AND loan.cont-code EQ vContCode
      NO-LOCK NO-ERROR.

   IF NOT AVAILABLE loan THEN
      RETURN.

   IF iCode BEGINS "���" THEN
   DO:
      out_Result = loan.doc-num.
      RETURN.
   END.

   IF iCode BEGINS "���" THEN
   DO:
      out_Result = STRING(loan.conf-date, "99/99/9999").
      RETURN.
   END.

   IF iCode BEGINS "���" THEN
   DO:
      out_Result = loan.cont-type.
      out_Result = GetCodeNameEx("�����_���", out_Result, "").
      RETURN.
   END.

   IF iCode BEGINS "ID" THEN
   DO:
      out_Result = GetXAttrValueEx("loan",
                                   mProxySurr,
                                   "agent-id",
                                   "").
      RETURN.
   END.

   IF iCode BEGINS "SUR" THEN
   DO:
      out_Result = mProxySurr.
      RETURN.
   END.

   RETURN.
END PROCEDURE.

{pfuncdef
   &NAME          = "���_�������_���"
   &DESCRIPTION   = "�����頥� ४������ ������ (䨧��᪮�� ���) ��� �७�� �祩��."
   &PARAMETERS    = "��� ��ꥪ�,��� ४�����"
   &RESULT        = "092384123"
   &SAMPLE        = "���_�������(123,'���') - ��� � ����� ���㬥��~~n~
���_�������(123,'���') - ��� �뤠� ���㬥��~~n~
���_�������(123,'���') - �� �뤠� ���㬥��"
   }
   DEFINE INPUT  PARAMETER iCustId    AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER iType      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64     NO-UNDO.

   DEFINE VARIABLE vCodes             AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER person FOR person.
   DEFINE BUFFER code   FOR code.
   {pchkpar iCustId iType}

   FIND FIRST person WHERE person.person-id EQ iCustId
      NO-LOCK NO-ERROR.

   IF NOT AVAILABLE person THEN
      RETURN.

   vCodes = "".

   FOR EACH code WHERE code.class  EQ "���넮���"
                   AND code.parent EQ "�������"
      NO-LOCK:
      {additem.i vCodes code.code}
   END.

   IF vCodes EQ "" THEN
      RETURN.

   FIND LAST cust-ident WHERE cust-ident.cust-cat EQ "�"
                          AND cust-ident.cust-id  EQ iCustId
                          AND CAN-DO(vCodes, cust-ident.cust-code-type)
      NO-LOCK NO-ERROR.

   IF NOT AVAILABLE cust-ident THEN
      RETURN.

   IF iType BEGINS "���" THEN
      out_Result = cust-ident.cust-code.

   IF iType BEGINS "���" THEN
      out_Result = STRING(cust-ident.open-date, "99/99/9999").

   IF iType BEGINS "���" THEN
      out_Result = cust-ident.issue.
      
   RETURN out_Result.
END PROCEDURE.


{pfuncdef
   &NAME          = "Idnt_term-obl"
   &DESCRIPTION   = "�뤠�� ���ண�� term-obl �� idnt"
   &PARAMETERS    = "Contract,Cont-Code,Idnt,End-date"
   &RESULT        = "��������"
   &SAMPLE        = "Idnt_term-obl('contract','cont-code',1,����())"
   }   
   DEFINE INPUT  PARAMETER iContract  LIKE loan.contract     NO-UNDO.
   DEFINE INPUT  PARAMETER iCont-Code LIKE loan.cont-code    NO-UNDO.
   DEFINE INPUT  PARAMETER iIdnt      LIKE term-obl.idnt     NO-UNDO.
   DEFINE INPUT  PARAMETER iEnd-date  LIKE term-obl.end-date NO-UNDO.
   DEFINE OUTPUT PARAMETER out_Result AS CHARACTER INIT "".
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 INIT -1.

   DEFINE BUFFER xloan     FOR loan.
   DEFINE BUFFER xterm-obl FOR term-obl.

   PROC:
   DO ON ERROR UNDO PROC, LEAVE PROC:

      FIND FIRST xloan WHERE xloan.contract  = iContract
                         AND xloan.cont-code = iCont-Code
      NO-LOCK NO-ERROR.
      IF AVAIL xloan THEN DO:
         IF xloan.currency = "" THEN DO:
            FIND LAST xterm-obl WHERE xterm-obl.contract  = iContract
                                  AND xterm-obl.cont-code = iCont-Code
                                  AND xterm-obl.idnt      = iIdnt
                                  AND xterm-obl.end-date  = iEnd-date
            NO-LOCK NO-ERROR.
            IF AVAIL xterm-obl THEN DO:
               out_Result = GetSurrogateBuffer("term-obl",(BUFFER xterm-obl:HANDLE)).
               IF out_Result = ? THEN out_Result = "".
            END.
         END.
      END.

      is-ok = 0.
   END.  /* of PROC block */

   RETURN.
END PROCEDURE.

