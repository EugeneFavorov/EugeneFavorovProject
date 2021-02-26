/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (c) 1992-2017 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-PDPS.P
      Comment: ������⥪� �㭪権 ����� ��� ������ �������
   Parameters: none
         Uses:
      Used by:
     Modified: 07.12.2004       MIOA     (0035880) ��������� �㭪��
                                         ���������_�������
     Modified: 07.12.2004       MIOA     (0035880) ��������� �㭪��
                                         ���������_����������

*/

{globals.i}
{ksh-defs.i new}
{dpsproc.def}
{intrface.get kau}
{intrface.get ltran}
{intrface.get tmess}
{intrface.get pbase}

/*****************************************************************************/
{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PDPS"
   &LIBNAME       = "������⥪� �㭪権 ����� ��� ������ �������"
   &DESCRIPTION   = "����ন� �㭪樨 ����� ��� ������ �������: �஢�ન �㬬, ���⪮�, � ��."
   }

/*****************************************************************************/
{pfuncdef
   &NAME          = "���������_������"
   &DESCRIPTION   = "�஢����, �⠭�� �� ���⮪ ������ ����� �������쭮�� ���⪠, �᫨ � ��� ᯨ��� 㪠������ �㬬�
                     (�᫨ ��楤�� ���㫠 0.00 - �� ����� ����, �� �� ����⨨ �㬬� �� ������ ��⠭���� 
                     ஢�� ��������� ���⮪, ���� �� ������ �� 㪠��� ४����� ������)"
   &PARAMETERS    = "���������� ��������, ����� ��������, ����, ������ �����, �����, ���� ������� �����"
   &RESULT        = "�����, ������� �� ������ �� ������������ �������"
   &SAMPLE        = "���������_������('dps','42301/1',42301810500020000053,'',100000000.00, 12/12/2004) = 500.000~~n~
�᫨ 12/12/2004 ������ � ������ 100000000.00 (� ����� ������) - �� ������ ��⠥��� �㬬�, ~
����� ����� �������쭮 �����⨬��� ���⪠ ��  500.00~~n~n~
���������_������('dps','42302/10',42302840500020000053,'840',100000000.00, 27/12/2004) = -1000.00~~n~
�᫨ 27/12/2004 ������ � ������ 100000000.00 (� ����� ������) - ���⮪ �� ������ �� �㤥� ����� �������쭮 �����⨬���~
(� ����� ������ �� 1000.00, �� ������ ������� ������)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   DEFINE VAR vMinOst  AS DECIMAL NO-UNDO.
   DEFINE VAR vBalance AS DECIMAL NO-UNDO.
   DEFINE VAR vKau     AS CHAR    NO-UNDO.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "�訡�� �� ��।������ ������".
     is-ok = -1.
     RETURN.
   END.
   

   
   RUN get_last_min_ost IN h_dpspc(RECID(b-loan),
                                   iOpDate,
                                   iOpDate,
                                   OUTPUT out_Result).
   /* �᫨ ���ᨬ���� ���⮪ ����� �� 㪠��� - �㤥� �����, �� �ॢ�襭�� ࠢ�� 0 */
   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.
   
   vMinOst = DEC(out_Result) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      out_Result = "0.00".
      is-ok = -1.
      RETURN.
   END.

   FIND LAST b-loan-acct OF b-loan 
                          WHERE b-loan-acct.acct      =  iAcct
                            AND b-loan-acct.currency  =  iCurrency
                            AND b-loan-acct.since     <= iOpDate
                            AND (b-loan-acct.acct-type = "loan-dps-p" OR 
                                 b-loan-acct.acct-type = "loan-dps-t")
                          NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan-acct THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","��� [" + iAcct + "] � ����� [" + iCurrency + 
                                 "] �� �ਭ������� ������ [" + iContract + "][" + iContCode + "]").
      is-ok = -1. 
      RETURN.
  END.
   
   CASE b-loan-acct.acct-type:
     WHEN "loan-dps-p" THEN DO:
       vKau = "��₪��".
     END.
     WHEN "loan-dps-t" THEN DO:
       vKau = "��₪��".
     END.
     OTHERWISE DO:
        RUN Fill-SysMes IN h_tmess("","","-1","��� [" + iAcct + "] � ����� [" + iCurrency + 
                                   "] �ਢ易� �� ������ � ஫�� [" + b-loan-acct.acct-type + 
                                   "]. ����� ஫� �� �����ন������ ������ �㭪樥�.").
        is-ok = -1.
        RETURN.
     END.
   END.
   
   RUN kau-pos IN h_kau(iAcct, iCurrency, iOpDate, iOpDate,
                        "�", iContract + ","  + iContCode + "," + vKau).
   vBalance = -1 * (IF iCurrency = "" THEN ksh-bal ELSE ksh-val).
   
   out_Result = STRING(vBalance - vMinOst - iSumm).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "���������_�������"
   &DESCRIPTION   = "�஢����, �㤥� �� �ॢ�襭 ���ᨬ���� ���⮪ ��⭮�� ������, �᫨ � ���� �������� 㪠������ �㬬�"
   &PARAMETERS    = "���������� ��������, ����� ��������, ����, ������ �����, �����, ���� �������� �����"
   &RESULT        = "����� ����������"
   &SAMPLE        = "���������_�������('dps','42301/1',42301810500020000053,'',100000000.00, 12/12/2004) = 500.000~~n~
�᫨ 12/12/2004 �������� �� ����� 100000000.00 (� ����� ������) - ���ᨬ��쭠� �㬬� ������ �㤥� �ॢ�襭� �� 500.00~~n~n~
���������_�������('dps','42302/10',42302840500020000053,'840',100000000.00, 27/12/2004) = -1000.00~~n~
�᫨ 27/12/2004 �������� �� ����� 100000000.00 (� ����� ������) - ���ᨬ��쭠� �㬬� ������ �� �㤥� �ॢ�襭� ~
(� �� �ॢ�襭�� ����� ����� �� 1000.00)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iAcct     AS CHAR NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   DEFINE VAR vMaxOst  AS DECIMAL NO-UNDO.
   DEFINE VAR vBalance AS DECIMAL NO-UNDO.
   DEFINE VAR vKau     AS CHAR    NO-UNDO.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "�訡�� �� ��।������ ������".
     is-ok = -1.
     RETURN.
   END.
   

   
   RUN get_last_max_ost IN h_dpspc(RECID(b-loan),OUTPUT out_Result).
   /* �᫨ ���ᨬ���� ���⮪ ����� �� 㪠��� - �㤥� �����, �� �ॢ�襭�� ࠢ�� 0 */
   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.
   
   vMaxOst = DEC(out_Result) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      out_Result = "0.00".
      is-ok = -1.
      RETURN.
   END.

   FIND LAST b-loan-acct OF b-loan 
                          WHERE b-loan-acct.acct     =  iAcct
                            AND b-loan-acct.currency =  iCurrency
                            AND b-loan-acct.since    <= iOpDate
                            AND (b-loan-acct.acct-type = "loan-dps-p" OR 
                                 b-loan-acct.acct-type = "loan-dps-t")
                          NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan-acct THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","��� [" + iAcct + "] � ����� [" + iCurrency + 
                                 "] �� �ਭ������� ������ [" + iContract + "][" + iContCode + "]").
      is-ok = -1. 
      RETURN.
  END.
   
   CASE b-loan-acct.acct-type:
     WHEN "loan-dps-p" THEN DO:
       vKau = "��₪��".
     END.
     WHEN "loan-dps-t" THEN DO:
       vKau = "��₪��".
     END.
     OTHERWISE DO:
        RUN Fill-SysMes IN h_tmess("","","-1","��� [" + iAcct + "] � ����� [" + iCurrency + 
                                   "] �ਢ易� �� ������ � ஫�� [" + b-loan-acct.acct-type + 
                                   "]. ����� ஫� �� �����ন������ ������ �㭪樥�.").
        is-ok = -1.
        RETURN.
     END.
   END.
   
   RUN kau-pos IN h_kau(iAcct, iCurrency, iOpDate, iOpDate,
                        "�", iContract + ","  + iContCode + "," + vKau).
   vBalance = -1 * (IF iCurrency = "" THEN ksh-bal ELSE ksh-val).
   
   out_Result = STRING(vBalance + iSumm - vMaxOst).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "���������_��������"
   &DESCRIPTION   = "�஢����, �����筠 �� �㬬� ����� (�᫨ ���� ��࠭�祭�� ��ࠬ��஬ ����㬬�)"
   &PARAMETERS    = "���������� ��������, ����� ��������, ���� �������� �����, �����"
   &RESULT        = ""
   &SAMPLE        = ""
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO INIT "���".
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan FOR loan.
   DEFINE VAR vMinSumm  AS DEC NO-UNDO.

   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     RUN Fill-SysMes IN h_tmess("","","-1","�訡�� �� ��।������ ������ [" + iContract + "][" + iContCode + "]").
     is-ok = -1.
     RETURN.
   END.
   
   RUN Get_Last_Param IN h_dpspc(RECID(b-loan), iOpDate, iOpDate,'����㬬�',OUTPUT out_Result).

   IF out_Result = ? OR out_Result = "" OR  out_Result = "?" THEN DO:
      out_Result = "0.00".
      RETURN.
   END.

   out_Result = STRING(DEC(out_Result) - iSumm).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "���������_����������"
   &DESCRIPTION   = "�஢����, ��⭠ �� �㬬�, ������ �� ��⠥��� ����� �� ����� (��� ᯨ���), ~
�����⨬�� �㬬� ��⭮�� ����������/ᯨᠭ��, 㪠������ �� ������. �᫨ �㬬� ��⭠ - �����頥� ~
������ ��ப�. �᫨ �㬬� ����⭠ - �����頥� ⥪�� �訡��. ��࠭��� ���� �������� ࠢ�� ��, �᫨ ᮧ������ ���㬥�� ~
ᯨᠭ��, � ��� - �᫨ ᮧ������ ���㬥�� �����ᥭ��."
   &PARAMETERS    = "���������� ��������, ����� ��������, ��� ���������, ���� ��������/������� �����, �����, ���� ��������"
   &RESULT        = "����� ������"
   &SAMPLE        = "���������_����������('dps','42301/1',3844095, 12/12/2004,1000.00,���) = '�㬬� ������ ���� ��⭠ 300'~~n~
�� ����砥�, �� � ����ன�� �㬬���⭏ 㪠���� ���祭�� 300 � �㬬� ���㬥�� ������ ���� ��⭠ 300 ~
(�� � ������ ��砥 ����୮) ~~n~n~
���������_����������('dps','42301/1',3844095, 12/12/2004,3000.00,���) = ''~~n~
����� �� �� - ����� �㬬� ��� ���ᥭ�� ���室��� (��⭠ 300)"
   }

   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iOp       AS INT64  NO-UNDO.
   DEF INPUT PARAM iOpDate   AS DATE NO-UNDO.
   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iFlag     AS LOG  NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-loan-acct FOR loan-acct.
   
   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN DO:
     out_Result = "�訡�� �� ��।������ ������".
     is-ok = -1.
     RETURN.
   END.
   
   out_Result = check_mod_amt(RECID(b-loan), iOp, iOpDate, iSumm, iFlag).
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "���������_�������"
   &DESCRIPTION   = "�஢�ઠ ����室����� ����஫� �㬬� ������ � �����䨪��� ����� (�� ��ࠬ��ࠬ 9���). ~
��� - ��� ������. 810, 840 � �.�. �᫨ ��ࠬ��� ��� ��।����� ������ �� ��諮�� - ��⠥�, �� ����஫� �� �ॡ����."
   &PARAMETERS    = "����� ������, ������ ������"
   &RESULT        = "��������� ���������� ���������� ���������"
   &SAMPLE        = "���������_�������(70000.00, '') = ��~~n~
- ����室��� �஢����, ���� �� � ��⥬� ������, �஢���騩 �����."
   }

   DEF INPUT PARAM iSumm     AS DEC  NO-UNDO.
   DEF INPUT PARAM iCurrency AS CHAR NO-UNDO.

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO INIT "���".
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEFINE BUFFER b-code FOR code.
   DEFINE VAR vSumm AS DECIMAL NO-UNDO INIT 0.00.
   
   IF iCurrency = "" THEN iCurrency = "810".
   
   FIND FIRST b-code WHERE b-code.class = "�����" 
                       AND b-code.code  = "9" + iCurrency
                     NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-code THEN RETURN. /* �� ��, ����஫� �� �⮩ ����� ��� */
   
   IF NUM-ENTRIES(b-code.val, CHR(1)) >= 16 THEN 
     vSumm = DECIMAL(ENTRY(16, b-code.val, CHR(1))).

   IF iSumm >= vSumm THEN out_Result = "��".
   RETURN.
END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "�⊠���"
   &DESCRIPTION   = "�����頥� ���� ��᫥���� ����⠫���樨 (�믫���) ��業⮢"
   &PARAMETERS    = "���������� ��������,������������� ��������[,����]"
   &RESULT        = "��� ��᫥���� ����⠫���樨 ��業⮢, �⭮�⥫쭮 ~
��।����� ����"
   &SAMPLE        = "�⊠���(�����,123,����())"
   }

   DEF INPUT  PARAM iContract   AS CHAR   NO-UNDO.  /* �����祭�� ������� */
   DEF INPUT  PARAM iContCode   AS CHAR   NO-UNDO.  /* ����� ������� */
   DEF INPUT  PARAM iDate       AS DATE   NO-UNDO.  /* ��� �⭮�⥫쭮, 
                                                       ���ன �������� ���
                                                       ���᫥��� ��業⮢ */

   DEF OUTPUT PARAM out_Result AS CHAR NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEF BUFFER b-loan-acct FOR loan-acct.

   DEF VAR vCntDate AS DATE        NO-UNDO. /* ��� ���� */
   DEF VAR vDtKap   AS DATE        NO-UNDO. /* ��� ��᫥���� ����⠫���樨 */
   DEF VAR vDtCond  AS DATE        NO-UNDO.
   DEF VAR vTmpDate AS DATE        NO-UNDO.
   DEF VAR vLocalPV AS CHARACTER   NO-UNDO.
   DEF VAR h_templ  AS HANDLE      NO-UNDO.   
   
   vLocalPV = "0".
   {pchkpar iContract iContCode}
   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      /*���� ������� �� �室�騬 ��ࠬ��ࠬ*/
      FIND FIRST loan WHERE loan.contract  EQ iContract
                        AND loan.cont-code EQ iContCode
         NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
      DO:
         is-ok = -1.
         RETURN.
         UNDO MAIN, LEAVE MAIN.      
      END.

      /* ��।������ ���� ���� */
      ASSIGN
         vCntDate = IF iDate NE DATE("") AND iDate NE ? THEN iDate
                                                        ELSE GetBaseOpDate()
         vDtKap = loan.open-date
         .

      /* ��।������ ���� ����⠫���樨 */
      RUN get_beg_kper IN h_dpspc (RECID(loan),
                                   vCntDate,
                                   ?,
                                   INPUT-OUTPUT vDtKap).

      IF vDtKap EQ ? THEN
         RUN get-beg-date-prol IN h_dpspc (RECID(loan),
                                           vCntDate,
                                           OUTPUT vCntDate,
                                           OUTPUT vTmpDate). 


      /*�饬 ��᫥���� �᫮��� � �ࠢ������ �� cond-cr-date  � ��⮩ ����⠫���樨*/
      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                            AND loan-cond.cont-code EQ loan.cont-code NO-LOCK NO-ERROR.


      IF AVAIL loan-cond THEN
         vDtCond = DATE(GetXAttrValueEX("loan-cond",
                                    loan-cond.contract + "," + loan-cond.cont-code + "," +  
                                    STRING(YEAR (loan-cond.since),"9999") + STRING(MONTH(loan-cond.since),"99" ) + STRING(DAY (loan-cond.since),"99" ),
                                   "cond-cr-date",
                                   "")).
      /*�饬 ��᫥���� ����⮢�� �㡠��������� �஢����*/
      IF vDtKap EQ loan.open-date OR vDtCond EQ vDtKap THEN
      DO:
         FOR EACH loan-acct OF loan WHERE 
                  loan-acct.acct-type = 'loan-dps-int' NO-LOCK:

            FOR LAST kau-entry WHERE kau-entry.acct = loan-acct.acct AND 
                                     kau-entry.currency = loan-acct.currency AND 
                                     kau-entry.kau BEGINS loan.contract + ',' + loan.cont-code + ','  AND 
                                     kau-entry.debit AND 
                                     kau-entry.op-date GE loan-acct.since NO-LOCK:
               FIND FIRST op OF kau-entry NO-LOCK NO-ERROR.
               IF AVAILABLE op THEN 
                  vDtKap = op.contract-date.
            END.
         END.
      END.

      vLocalPV = IF vDtKap NE ? THEN STRING(vDtKap, "99/99/9999")
                                ELSE STRING(vCntDate, "99/99/9999").
   END.

   out_Result = vLocalPV.
   RETURN.

END PROCEDURE.

/*****************************************************************************/
{pfuncdef
   &NAME          = "NomerVklada"
   &DESCRIPTION   = "�����頥� ����� ������"
   &PARAMETERS    = "���� �����,����� �����"
   &RESULT        = "����� ������"
   &SAMPLE        = "NomerVklada(loan-dps-p,42301810000000001225)"
   }

   DEF INPUT  PARAM iAcct-role   AS CHAR   NO-UNDO.  
   DEF INPUT  PARAM iAcct        AS CHAR   NO-UNDO.  
   DEF OUTPUT PARAM oUt_Result AS CHAR   NO-UNDO.
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO INIT 0.

   DEF VAR vLoanCount AS INT64 NO-UNDO.
   DEF VAR vAcct      AS CHAR  NO-UNDO.
   DEF VAR vCont-code AS CHAR  NO-UNDO.

   DEF BUFFER loan-acct FOR loan-acct.

   vAcct = iAcct.

   IF NUM-Entries(vAcct,"@") EQ 1 THEN
      vAcct = AddFilToAcct(vAcct, shFilial).

/* SORT-ACCESS loan-acct ��-�� ��㦤����� ���஢�� �� ������ ������� */
   FOR EACH loan-acct WHERE loan-acct.acct      EQ vAcct
                        AND loan-acct.acct-type EQ iAcct-role
      NO-LOCK BREAK BY loan-acct.cont-code:

      IF FIRST-OF(loan-acct.cont-code) THEN
      DO:
         vCont-code = loan-acct.cont-code.
         vLoanCount = vLoanCount + 1.
      END.
   END.

   IF vLoanCount EQ 1 THEN
      ASSIGN
         out_Result = IF ShMode THEN addFilToLoan(TRIM(vCont-code), shFilial) 
                                ELSE TRIM(vCont-code)
      .
   ELSE
      ASSIGN
         out_Result = ""
      .
   RETURN.

END PROCEDURE.
/* $LINTFILE='pp-pdps.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kuds' */
/* $LINTDATE='31/05/2016 11:30:54.737+03:00' */
/*prosignyHzpBATJlYJPCML80F+5hA*/