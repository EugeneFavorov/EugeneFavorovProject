
/* +++ get_date.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 2:02pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (c) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: GET_DATE.I

      Comment: ������⥪� ��� ࠡ��� � ��⠬�
   Parameters:
         Uses:
      Used by:
      Created: ??/??/???? kostik
     Modified: 20/01/2002 kostik 0004488 �����⨥ �� ����
     Modified: 08/07/2002 Lera   ��⨬����� ����.
     Modified: 10/06/2003 gorm   16619 ��� �� ���� ��� ��筮�� ������� � �����������,�������
                                 ⨯� "����⠫", � ������ ஫� ��� �� loan-dps-ts, � loan-dps-tsk
     Modified: 06/07/2004 mioa   ���᫠ �����㬥��� ��� ���� ���� ���᫥��� ��業⮢
                                 (���� �� �६����� ������⥪� date_lib.i, ��⠫�� - �� datenach.p)
     Modified: 04.03.2007 15:35 OZMI     (0070680)                         
*/

&GLOBAL-DEFINE nameproc "dps_gprc_buf,dps_kau.p,dps_gprc.p"
&GLOBAL-DEFINE recprol  'Prolong'
&GLOBAL-DEFINE loan_cond_since date(SUBSTRING(ENTRY(3,signs.surrogate),7,2) + SUBSTRING(ENTRY(3,signs.surrogate),5,2)                        + SUBSTRING(ENTRY(3,signs.surrogate),1,4))
&GLOBAL-DEFINE end-date 'EndDateBeforeProl'
&GLOBAL-DEFINE limitprol limitprol

{intrface.get "xclass"}
{intrface.get date}
{intrface.get dps}
{jloan_dps.i}
{ksh-defs.i new}
{intrface.get refer}

DEF VAR mDat-700 AS DATE NO-UNDO.
mDat-700 = DATE(fGetSetting("���_700",?,?)).

/* ���-⠡��� ���祭�� �� �� op-template */
DEFINE TEMP-TABLE ttOpTemplSign NO-UNDO
   FIELD Surr AS CHARACTER
   FIELD Code AS CHARACTER
   FIELD Val  AS CHARACTER
INDEX pi surr code.

/* ���-⠡��� ���祭��  */
DEFINE TEMP-TABLE ttDpsCache NO-UNDO
   FIELD File-ID AS CHARACTER
   FIELD Surr    AS CHARACTER
   FIELD Code    AS CHARACTER
   FIELD Val     AS CHARACTER
INDEX pi File-ID Surr Code.

/* ��।������ ��ࠬ��� �� 蠡���� �࠭���樨
** ���� ���祭�� �� �������⥫쭮�� ४����� ����窨 ������ */
FUNCTION GetOpTemlXattr RETURNS CHARACTER
   (INPUT iSurr    AS CHARACTER,
    INPUT iCode    AS CHARACTER,
    INPUT iDefault AS CHARACTER):
   
   FIND FIRST ttOpTemplSign WHERE
              ttOpTemplSign.Surr = iSurr
          AND ttOpTemplSign.Code = iCode
   NO-ERROR.
   IF NOT AVAIL ttOpTemplSign THEN DO:
      CREATE ttOpTemplSign.
      ASSIGN
         ttOpTemplSign.Surr = iSurr
         ttOpTemplSign.Code = iCode
         .
      ttOpTemplSign.Val = GetXattrValueEx("op-template",
                                          iSurr,
                                          iCode,
                                          iDefault).
   END.
   RETURN ttOpTemplSign.Val.
END FUNCTION.

/* ����祭�� �� �� ��� */
FUNCTION GetCacheSetting RETURNS CHARACTER
   (INPUT iSurr    AS CHARACTER,
    INPUT iCode    AS CHARACTER,
    iNPUT iVal     AS CHARACTER):
   
   FIND FIRST ttDpsCache WHERE ttDpsCache.File-ID EQ "setting"
                           AND ttDpsCache.Surr    EQ iSurr
                           AND ttDpsCache.code    EQ iCode
      NO-ERROR.
   IF NOT AVAIL ttDpsCache THEN
   DO:
      CREATE ttDpsCache.
      ASSIGN
         ttDpsCache.File-ID = "setting"
         ttDpsCache.Surr    = iSurr
         ttDpsCache.code    = iCode
         ttDpsCache.Val     = FgetSetting(iSurr, iCode, iVal).
   END.
   RETURN ttDpsCache.Val.
END FUNCTION.

/* ����祭�� Init �� ��� */
FUNCTION GetCacheInit RETURNS CHARACTER
   (INPUT iSurr    AS CHARACTER,
    INPUT iCode    AS CHARACTER):
   
   FIND FIRST ttDpsCache WHERE ttDpsCache.File-ID EQ "xattr"
                           AND ttDpsCache.Surr    EQ iSurr
                           AND ttDpsCache.code    EQ iCode
      NO-ERROR.
   IF NOT AVAIL ttDpsCache THEN
   DO:
      CREATE ttDpsCache.
      ASSIGN
         ttDpsCache.File-ID = "xattr"
         ttDpsCache.Surr    = iSurr
         ttDpsCache.code    = iCode
         ttDpsCache.Val     = GetXattrInit (iSurr, iCode).
   END.
   RETURN ttDpsCache.Val.
END FUNCTION.

/* ����祭�� Method �� ��� */
PROCEDURE GetCacheMethod PRIVATE:
   DEFINE INPUT  PARAMETER iSurr     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCode     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oNameProc AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oParams   AS CHARACTER NO-UNDO.
  
   FIND FIRST ttDpsCache WHERE ttDpsCache.File-ID EQ "Method"
                           AND ttDpsCache.Surr    EQ iSurr
                           AND ttDpsCache.code    EQ iCode
      NO-ERROR.
   IF AVAIL ttDpsCache THEN
      ASSIGN
         oNameProc = ENTRY(1, ttDpsCache.Val, CHR(1))
         oNameProc = ( IF oNameProc EQ "" THEN ? ELSE oNameProc)
         oParams   = ENTRY(2, ttDpsCache.Val, CHR(1)).
   ELSE
   DO:
      RUN GetClassMethod in h_xclass (iSurr,
                                      iCode,
                                      "","",
                                      output oNameProc,
                                      output oParams).    
      CREATE ttDpsCache.
      ASSIGN
         ttDpsCache.File-ID = "Method"
         ttDpsCache.Surr    = iSurr
         ttDpsCache.code    = iCode
         ttDpsCache.Val     = SUBSTITUTE("&1&2&3",
                                         IF oNameProc EQ ? THEN "" ELSE oNameProc,
                                         CHR(1),
                                         oParams).
   END.
END PROCEDURE.

/* ������ ���᫥���: */
/*   beg-delay */
FUNCTION GetBegDelay RETURNS INT64 
   (INPUT iOp-Kind AS CHARACTER):
   
   DEFINE BUFFER b-op-template FOR op-template.
   DEFINE VARIABLE vBegDelay AS CHARACTER NO-UNDO.

   FIND FIRST ttOpTemplSign WHERE
      ttOpTemplSign.Surr = iOp-Kind AND
      ttOpTemplSign.Code = "beg-delay"
      NO-ERROR.
   IF NOT AVAIL ttOpTemplSign THEN DO: 
      FIND FIRST b-op-template WHERE
         b-op-template.op-kind    = iOp-Kind AND
         b-op-template.Class-Code = "lcon-templ" 
         NO-LOCK NO-ERROR.
      IF AVAILABLE b-op-template THEN 
         vBegDelay = GetOpTemlXattr(
             iOp-Kind + "," + STRING(b-op-template.op-template),
             "beg-delay",
             "0").
      ELSE
         vBegDelay  = "0".
      
      CREATE ttOpTemplSign.
      ASSIGN
         ttOpTemplSign.Surr = iOp-Kind
         ttOpTemplSign.Code = "beg-delay"
         ttOpTemplSign.Val  = vBegDelay  
         .
   END.
   RETURN INT64(ttOpTemplSign.Val).
END FUNCTION.

/******************************************************************************/
/*                                                                            */
/*                     ���� ������� ��� ���������� ���������                  */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/*                            PROCEDURE BLOCK                                 */
/******************************************************************************/
/*  NAME:    PROCEDURE CheckChargeDate
    PURPOSE: �஢���� ���� iOpDate �� ���४⭮��� �⭮�⥫쭮 ��ਮ�� 
             ���᫥���. ���஡��� - � ���ᠭ�� ��ࠬ��஢
             
             ������� ��� ������ ChekPeriodNach, �� �ᯮ�짮����� 
             ���ன ����� ���� �������. 
             
    PARS:    iStartDate  - ���, �� ���ன �����뢠�� ��ਮ�
                           (���ਬ��, ��� ������ �������, ��� ��� 
                           �ନ஢���� �᫮���)
             iMDate      - ���� �����, � ����� �� �᫮��� ������ ���� 
                           ���᫥���
             iMonths     - ������⢮ ����楢, �१ ����� �� �᫮��� 
                           ������ ���� ���᫥���
             iOpDate     - �।���������� ��� ����樨. �� ᫥��� �஢����
                           �� ���४⭮���.
             oChargeDate - = iOpDate
                                �᫨ iOpDate - �ࠢ��쭠� ��� ���᫥���
                                (���室�� �� �⮣� ���� ��
                                iStartDate, iMonths � iMDay)
                           = ��㣮� (�ࠢ��쭮�) ��� (ᠬ�� ������� ᭨�� � 
                                iOpDate), �᫨ iOpDate �� ���室��
    RETURNS: oIsCorrect  - ����諠 iOpDate ��� ���.

    NOTES:   �।����������, �� iStartDate < iOpDate. �᫨ �� ⠪ - 
             �� ����, ��� �� �� �㤥� ࠡ����.

             �������� �ࠡ�⠥�, �᫨ ��� ࠢ��.

             ��筨��, ��� �� ������ ࠡ����, ����� ����� 
             iStartDate � iOpDate �� ��襫 �� ���� 楫� ��ਮ�.

             ����� ��୥��� ᠬ iStartDate.

*/
PROCEDURE CheckChargeDate:
DEFINE INPUT  PARAMETER iStartDate   AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER iMDay       AS INT64 NO-UNDO.
DEFINE INPUT  PARAMETER iMonths     AS INT64 NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate     AS DATE    NO-UNDO.
DEFINE OUTPUT PARAMETER oChargeDate AS DATE    NO-UNDO INIT ?.
DEFINE OUTPUT PARAMETER oIsCorrect  AS LOGICAL NO-UNDO INIT NO.

DEFINE VAR vMonthPeriod AS INT64 NO-UNDO.
DEFINE VAR vFullPeriod  AS INT64 NO-UNDO. 

DEFINE VAR vFullYears  AS INT64 NO-UNDO.
DEFINE VAR vFullMonths AS INT64 NO-UNDO.
   
   IF iMonths <= 0 OR iMonths = ? THEN RETURN.

   /* ��⠥� ࠧ���� � ������ ����� ��砫쭮� � ����筮� ��⠬� */
   vMonthPeriod = (YEAR(iOpDate) - YEAR(iStartDate)) * 12 + (MONTH(iOpDate) - MONTH(iStartDate)).
   /* �᫨ ��ਮ� ����稫�� <=�㫥�� (� ����, ��訢��� ��।��� ���᫥��� ࠭�� ��砫� ������),
     ����� �� ��⠥� ��ࢮ� ���᫥��� -> � 1 ��ਮ� ��-⠪� �㦭� �ਡ����� */
   IF vMonthPeriod <= 0 THEN vMonthPeriod = iMonths.
   
   /* ������ ᬮ�ਬ, ᪮�쪮 楫�� ��ਮ��� 㪫��뢠���� � ��� ࠧ���� �
      �ࠧ� ������뢠��, ᪮�쪮 ����楢 㪫��뢠���� � �� 楫� ��ਮ��... */
   vFullPeriod  = TRUNCATE(vMonthPeriod / iMonths, 0) * iMonths.
   /* ... � ᪮�쪮 ��⠫��� ����楢 �� ��᫥����� 楫��� ��ਮ��. */
   vMonthPeriod = vMonthPeriod - vFullPeriod. /* �� ��६����� ��� �� �७ �� �㦭� ����� */
   
   /* �ਡ���塞 � ��砫쭮� ��� 楫�� ������⢮ ��ਮ��� 
      � �ࠧ� ��⠥��� ���⠢��� �ࠢ���� ���� ����� */
   oChargeDate = date_correct(MONTH(iStartDate) + vFullPeriod, 0, 
                                 iMDay, 
                                 YEAR(iStartDate)).
   IF oChargeDate > iOpDate AND vFullPeriod / iMonths > 1 THEN /* �� 1 ��ਮ� ����� */
        oChargeDate = date_correct(MONTH(iStartDate) + vFullPeriod - iMonths, 0, 
                                 iMDay, 
                                 YEAR(iStartDate)).

   /* ������ � oChargeDate ����� ��᫥���� ���������� ����, ����� ��� ࠢ�� iOpDate
      �᫨ �� ࠢ�� - ����� �������� OK */
   IF oChargeDate = iOpDate THEN
     oIsCorrect = YES.
   RETURN.
END PROCEDURE. /* END OF PROCEDURE CheckChargeDate */

/******************************************************************************/
/*  NAME:    PROCEDURE DateOfCharge
    PURPOSE: ��楤�� ��।������ ���� ᫥���饣� ���᫥��� ��業⮢
             (�⭮�⥫쭮 �������� ����).
             
             ������� ��楤�ன datenach.p, ��ॢ������� � ������⥪�.
             ����� 0029876. ���� ��稭� �������, �� �� ���� �������⥫쭮� 
             �᪮७�� ���� ���⮢ �� 125� �ଥ.
             
             ������ ��ࠢ��� ���� �� ⠡��� op-template - ���� 㪠�뢠����
             ���� op-kind, ���஥ ���� ����� ���⪨� ������.
             
    PARS:    iLoanRecid - �������, ��� ���ண� ����뢠�� ���ࢠ�
             iCalcDate  - ��� ����
             iKau       - ��� ��� (���᫥��� ��業�� ��� ���⮪ �� ������)
             oBegDate   - ����祭��� ��� ��砫�    ���ࢠ��
             oEndDate   - ����祭��� ��� ����砭�� ���ࢠ��
    RETURNS: 
    NOTES:   
*/
PROCEDURE DateOfCharge:
DEFINE INPUT  PARAMETER iCalcDate      AS DATE  NO-UNDO.
DEFINE INPUT  PARAMETER iLoanCondRecid AS RECID NO-UNDO.
DEFINE OUTPUT PARAMETER oChargeDate    AS DATE  NO-UNDO.

DEFINE VAR vIntMonth   AS INT64    NO-UNDO.
DEFINE VAR vFlag       AS LOG    NO-UNDO.
DEFINE VAR vPeriod     AS CHAR   NO-UNDO.
DEFINE VAR vFirstDelay AS CHAR   NO-UNDO.
DEFINE VAR vBegDelay   AS INT64    NO-UNDO INIT 0.
DEFINE VAR vEndDate    AS DATE   NO-UNDO.
DEFINE VAR vChargeDate AS DATE   NO-UNDO.
DEFINE VAR vPeriodD    AS INT64    NO-UNDO. /* ��ਮ� ���᫥��� ��業⮢, �ᨫ ����� ������⢮� ���� */
DEFINE VAR vEndDt      AS DATE   NO-UNDO.
DEFINE VAR vStDate     AS DATE   NO-UNDO.

DEFINE BUFFER b-op-template FOR op-template.
DEFINE BUFFER b-loan-cond   FOR loan-cond.
DEFINE BUFFER b-loan        FOR loan.

   /* �஢��塞 ���४⭮��� �맮�� */
   FIND FIRST b-loan-cond WHERE RECID(b-loan-cond) = iLoanCondRecid 
                          NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan-cond THEN
   DO:
      oChargeDate = ?.
      RETURN.
   END.

   FOR FIRST b-loan FIELDS(contract cont-code op-kind op-template) WHERE
             b-loan.cont-code = b-loan-cond.cont-code 
         AND b-loan.contract  = b-loan-cond.contract
   NO-LOCK:
   END.
   IF NOT AVAILABLE b-loan THEN
   DO:
      oChargeDate = ?.
      RETURN.
   END.
   
   vPeriod = b-loan-cond.int-period.
   IF NOT {assigned vPeriod} THEN
   DO:
      oChargeDate = ?.
      RETURN.
   END.

   /* ������ ���᫥���: */
   ASSIGN 
      vBegDelay   = GetBegDelay(b-loan.op-kind)
      /*   first-delay - ��ࢮ��砫�� ᤢ�� ���᫥��� %% */
      vFirstDelay = GetOpTemlXattr(b-loan.op-kind + "," + STRING(b-loan.op-template),
                                   "first-delay", ?)
      vEndDate    = get-end-date(b-loan-cond.since,vFirstDelay)
   .
   /* �ᥣ�� ��।��塞 ᫥���饥 ���᫥��� �⭮�⥫쭮 ���� iCalcDate, 
      (������ᨬ� �뫮 ��� � ��� ���� ��� ���) */
   IF vFirstDelay <> ? AND vEndDate <> ? AND iCalcDate < vEndDate THEN
      iCalcDate = vEndDate.
  
   CASE vPeriod:
      /* �ந����쭮� (����� ������� � �� ����) */
      WHEN "�"  THEN oChargeDate = iCalcDate.
      /* ��� � ��� (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "�"  THEN DO:
         oChargeDate = date_correct(b-loan-cond.int-month,0,b-loan-cond.int-date,YEAR(iCalcDate)).
         IF oChargeDate < iCalcDate THEN 
            oChargeDate = date_correct(MONTH(oChargeDate),0,b-loan-cond.int-date,YEAR(oChargeDate) + 1).
      END.
      /* ����� ����⠫� */
      WHEN "��" THEN oChargeDate = end_of_quarter(iCalcDate).
      /*����� ���㣮���*/
      WHEN "���" THEN oChargeDate = end_of_halfyear(iCalcDate).
      /* ����� ����� */    
      WHEN "��" THEN oChargeDate = end_of_month(iCalcDate).
      /* ��� � ������� (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "��"  THEN DO:
         vIntMonth = IF b-loan-cond.int-month NE ? THEN b-loan-cond.int-month ELSE 0.
         vIntMonth = vIntMonth MOD 6.
         IF vIntMonth = 0 THEN vIntMonth = 6.
         /* �� ���᫥��� ���� ���᫥��� ��楤�ன half_year
            �� �������� �᫮���� (� ������, �᫨ b-loan-cond.int-month ��।����)
            ��ਮ� ���᫥��� �㤥� ��᪠������. �� ���� �������� �� ������訩 ��ਮ� ���᫥���, � 
            ᫥���騩 �� ���.
          
            ���⮬� �஢�ਬ, �� ���室�� �� ��� ���������� ��ਮ� ���᫥���,....  */
         oChargeDate = half_year(iCalcDate,b-loan-cond.int-date,vIntMonth - 6).
         /* �᫨ �।��騩 ��ਮ� �� ���室�� - ⮣�� ��६ ⥪�騩 */
         IF oChargeDate <= iCalcDate THEN DO:
            oChargeDate  = half_year(iCalcDate,b-loan-cond.int-date,vIntMonth).
         END.
      END.
      /* ��� � ����⠫ (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "�" THEN DO:
         vIntMonth = IF b-loan-cond.int-month NE ? THEN b-loan-cond.int-month ELSE 0.
         vIntMonth = vIntMonth MOD 3.
         IF vIntMonth = 0 THEN vIntMonth = 3.
         /* �� ���᫥��� ���� ���᫥��� ��楤�ன quarter
            �� �������� �᫮���� (� ������, �᫨ b-loan-cond.int-month ��।����)
            ��ਮ� ���᫥��� �㤥� ��᪠������. �� ���� �������� �� ������訩 ��ਮ� ���᫥���, � 
            ᫥���騩 �� ���.
            
            ���⮬� �஢�ਬ, �� ���室�� �� ��� ���������� ��ਮ� ���᫥���,....  */
         oChargeDate = quarter(iCalcDate,b-loan-cond.int-date,vIntMonth - 3).
         /* �᫨ �।��騩 ��ਮ� �� ���室�� - ⮣�� ��६ ⥪�騩 */
         IF oChargeDate <= iCalcDate THEN DO:
            oChargeDate  = quarter(iCalcDate,b-loan-cond.int-date,vIntMonth).
         END.
      END.
      /* �������筮 (㪠������� �᫠) */
      WHEN "�" THEN DO:
         RUN CheckChargeDate IN THIS-PROCEDURE ( b-loan-cond.since + vBegDelay,
                                                 b-loan-cond.int-date,
                                                 1,
                                                 iCalcDate,
                                                 OUTPUT oChargeDate,
                                                 OUTPUT vFlag ).
         IF oChargeDate < iCalcDate THEN
            oChargeDate = date_correct(MONTH(oChargeDate) + 1,0,b-loan-cond.int-date,YEAR(oChargeDate)).
       END.
       /* � ���� �ப� */
       WHEN "��" THEN DO:          
          RUN get-beg-date-prol(RECID(b-loan),
                                iCalcDate ,
                                OUTPUT vStDate, 
                                OUTPUT vEndDt).
          oChargeDate = vEndDt.
       END.
       WHEN "��" THEN
       DO:
          RUN get-beg-date-prol(RECID(b-loan),
                                iCalcDate ,
                                OUTPUT vStDate, 
                                OUTPUT vEndDt).
          oChargeDate = vStDate.
       END.
       OTHERWISE  DO:
          /* ���᫥��� �१ N-�� �᫮ ����楢 */
          IF CAN-DO("��[*]",vPeriod) THEN DO:
             vIntMonth = NumMonth(b-loan-cond.int-period).
             RUN CheckChargeDate IN THIS-PROCEDURE ( b-loan-cond.since + vBegDelay,
                                                     b-loan-cond.int-date,
                                                     vIntMonth,
                                                     iCalcDate,
                                                     OUTPUT oChargeDate,
                                                     OUTPUT vFlag).
             IF oChargeDate < iCalcDate THEN DO:
                oChargeDate = date_correct(MONTH(oChargeDate) + vIntMonth,0,b-loan-cond.int-date,YEAR(oChargeDate)).
             END.
          END.
          ELSE
          DO:             
             ASSIGN vPeriodD = INT64(vPeriod) NO-ERROR.            
             IF NOT ERROR-STATUS:ERROR THEN
             DO:                
                /* ���� ��᫥���� ���� ����⠫���樨 */
                RUN get_beg_date_per IN THIS-PROCEDURE (RECID(b-loan),
                                                        iCalcDate,
                                                        OUTPUT oChargeDate).
                vChargeDate = oChargeDate.
                RUN get_beg_kper IN THIS-PROCEDURE (RECID(b-loan), 
                                                    iCalcDate, 
                                                    ?, 
                                                    INPUT-OUTPUT oChargeDate).
                IF oChargeDate EQ vChargeDate THEN
                   oChargeDate = b-loan-cond.since.
                   
                /*�᫨ ��᫥ ��᫥���� ����⠫���樨 � ��� �뫠 �஫������, 
                  � ��� �㦭� ��� �஫����樨*/
                IF b-loan-cond.since GE oChargeDate THEN
                DO:
                   oChargeDate = b-loan-cond.since.
                   RUN correct_date (RECID(b-loan),
                                     INPUT-OUTPUT oChargeDate).
                   ASSIGN oChargeDate =  oChargeDate + vPeriodD.
                END.   
                ELSE
                   ASSIGN oChargeDate =  oChargeDate + vPeriodD.

                /* ����塞 �������� ���� ���᫥��� ��業⮢ �⭮�⥫쭮 ����⭮� */
                DO WHILE oChargeDate LT iCalcDate:
                   oChargeDate = oChargeDate + vPeriodD.
                END.
             END.
          END.
      END.
   END CASE.
END PROCEDURE. /* END OF PROCEDURE DateOfCharge */

/******************************************************************************/
/*  NAME:    PROCEDURE DateOfChargeBufs
    PURPOSE: ��楤�� ��।������ ���� ᫥���饣� ���᫥��� ��業⮢
             (�⭮�⥫쭮 �������� ����).

    PARS:    b-loan      - ���� �������, ��� ���ண� ����뢠�� ���ࢠ�
             b-loan-cond - ���� �᫮��� �������, ��� ���ண� ����뢠�� ���ࢠ�
             iCalcDate   - ��� ����
             oEndDate    - ����祭��� ��� ����砭�� ���ࢠ��
    RETURNS: 
    NOTES:   
*/
PROCEDURE DateOfChargeBufs:
   DEFINE INPUT  PARAMETER iCalcDate      AS DATE  NO-UNDO.
   DEFINE PARAMETER BUFFER b-loan         FOR loan.
   DEFINE PARAMETER BUFFER b-loan-cond    FOR loan-cond.
   DEFINE OUTPUT PARAMETER oChargeDate    AS DATE  NO-UNDO.

   DEFINE VAR vIntMonth   AS INT64 NO-UNDO.
   DEFINE VAR vFlag       AS LOG   NO-UNDO.
   DEFINE VAR vPeriod     AS CHAR  NO-UNDO.
   DEFINE VAR vFirstDelay AS CHAR  NO-UNDO.
   DEFINE VAR vBegDelay   AS INT64 NO-UNDO INIT 0.
   DEFINE VAR vEndDate    AS DATE  NO-UNDO.
   DEFINE VAR vChargeDate AS DATE  NO-UNDO.
   DEFINE VAR vPeriodD    AS INT64 NO-UNDO. /* ��ਮ� ���᫥��� ��業⮢, �ᨫ ����� ������⢮� ���� */
   DEFINE VAR vEndDt      AS DATE  NO-UNDO.
   DEFINE VAR vStDate     AS DATE  NO-UNDO.

   IF NOT AVAILABLE b-loan-cond OR NOT AVAILABLE b-loan THEN
   DO:
      oChargeDate = ?.
      RETURN.
   END.
   vPeriod = b-loan-cond.int-period.
   IF NOT {assigned vPeriod} THEN
   DO:
      oChargeDate = ?.
      RETURN.
   END.

   /* ������ ���᫥���: */
   ASSIGN 
      vBegDelay   = GetBegDelay(b-loan.op-kind)
      /*   first-delay - ��ࢮ��砫�� ᤢ�� ���᫥��� %% */
      vFirstDelay = GetOpTemlXattr(b-loan.op-kind + "," + STRING(b-loan.op-template),
                                   "first-delay", ?)
      vEndDate    = get-end-date(b-loan-cond.since,vFirstDelay)
   .
   /* �ᥣ�� ��।��塞 ᫥���饥 ���᫥��� �⭮�⥫쭮 ���� iCalcDate, 
      (������ᨬ� �뫮 ��� � ��� ���� ��� ���) */
   IF vFirstDelay <> ? AND vEndDate <> ? AND iCalcDate < vEndDate THEN
      iCalcDate = vEndDate.
 
   CASE vPeriod:
      /* �ந����쭮� (����� ������� � �� ����) */
      WHEN "�"  THEN 
         oChargeDate = iCalcDate.
      /* ��� � ��� (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "�"  THEN 
         DO:
            oChargeDate = date_correct(b-loan-cond.int-month,0,b-loan-cond.int-date,YEAR(iCalcDate)).
            IF oChargeDate < iCalcDate THEN 
               oChargeDate = date_correct(MONTH(oChargeDate),0,b-loan-cond.int-date,YEAR(oChargeDate) + 1).
         END.
      /* ����� ����⠫� */
      WHEN "��" THEN 
         oChargeDate = end_of_quarter(iCalcDate).
      /*����� ���㣮���*/
      WHEN "���" THEN 
         oChargeDate = end_of_halfyear(iCalcDate).
      /* ����� ����� */    
      WHEN "��" THEN 
         oChargeDate = end_of_month(iCalcDate).
      /* ��� � ������� (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "��"  THEN 
         DO:
            vIntMonth = IF b-loan-cond.int-month NE ? THEN b-loan-cond.int-month ELSE 0.
            vIntMonth = vIntMonth MOD 6.
            IF vIntMonth = 0 THEN vIntMonth = 6.
            /* �� ���᫥��� ���� ���᫥��� ��楤�ன half_year
               �� �������� �᫮���� (� ������, �᫨ b-loan-cond.int-month ��।����)
               ��ਮ� ���᫥��� �㤥� ��᪠������. �� ���� �������� �� ������訩 ��ਮ� ���᫥���, � 
               ᫥���騩 �� ���.
             
               ���⮬� �஢�ਬ, �� ���室�� �� ��� ���������� ��ਮ� ���᫥���,....  */
            oChargeDate = half_year(iCalcDate,b-loan-cond.int-date,vIntMonth - 6).
            /* �᫨ �।��騩 ��ਮ� �� ���室�� - ⮣�� ��६ ⥪�騩 */
            IF oChargeDate <= iCalcDate THEN 
            DO:
               oChargeDate  = half_year(iCalcDate,b-loan-cond.int-date,vIntMonth).
            END.
         END.
      /* ��� � ����⠫ (� 㪠����� ����� 㪠������� �᫠) */
      WHEN "�" THEN 
         DO:
            vIntMonth = IF b-loan-cond.int-month NE ? THEN b-loan-cond.int-month ELSE 0.
            vIntMonth = vIntMonth MOD 3.
            IF vIntMonth = 0 THEN vIntMonth = 3.
            /* �� ���᫥��� ���� ���᫥��� ��楤�ன quarter
               �� �������� �᫮���� (� ������, �᫨ b-loan-cond.int-month ��।����)
               ��ਮ� ���᫥��� �㤥� ��᪠������. �� ���� �������� �� ������訩 ��ਮ� ���᫥���, � 
               ᫥���騩 �� ���.
               
               ���⮬� �஢�ਬ, �� ���室�� �� ��� ���������� ��ਮ� ���᫥���,....  */
            oChargeDate = quarter(iCalcDate,b-loan-cond.int-date,vIntMonth - 3).
            /* �᫨ �।��騩 ��ਮ� �� ���室�� - ⮣�� ��६ ⥪�騩 */
            IF oChargeDate <= iCalcDate THEN 
            DO:
               oChargeDate  = quarter(iCalcDate,b-loan-cond.int-date,vIntMonth).
            END.
         END.
      /* �������筮 (㪠������� �᫠) */
      WHEN "�" THEN 
         DO:
            RUN CheckChargeDate IN THIS-PROCEDURE ( b-loan-cond.since + vBegDelay,
               b-loan-cond.int-date,
               1,
               iCalcDate,
               OUTPUT oChargeDate,
               OUTPUT vFlag ).
            IF oChargeDate < iCalcDate THEN
               oChargeDate = date_correct(MONTH(oChargeDate) + 1,0,b-loan-cond.int-date,YEAR(oChargeDate)).
         END.
      /* � ���� �ப� */
      WHEN "��" THEN 
         DO:          
            RUN get-beg-date-prol(RECID(b-loan),
               iCalcDate ,
               OUTPUT vStDate, 
               OUTPUT vEndDt).
            oChargeDate = vEndDt.
         END.
      WHEN "��" THEN
         DO:
            RUN get-beg-date-prol(RECID(b-loan),
               iCalcDate ,
               OUTPUT vStDate, 
               OUTPUT vEndDt).
            oChargeDate = vStDate.
         END.
      OTHERWISE  
      DO:
         /* ���᫥��� �१ N-�� �᫮ ����楢 */
         IF CAN-DO("��[*]",vPeriod) THEN 
         DO:
            vIntMonth = NumMonth(b-loan-cond.int-period).
            RUN CheckChargeDate IN THIS-PROCEDURE ( b-loan-cond.since + vBegDelay,
               b-loan-cond.int-date,
               vIntMonth,
               iCalcDate,
               OUTPUT oChargeDate,
               OUTPUT vFlag).
            IF oChargeDate < iCalcDate THEN 
            DO:
               oChargeDate = date_correct(MONTH(oChargeDate) + vIntMonth,0,b-loan-cond.int-date,YEAR(oChargeDate)).
            END.
         END.
         ELSE
         DO:             
            ASSIGN 
               vPeriodD = INT64(vPeriod) NO-ERROR.            
            IF NOT ERROR-STATUS:ERROR THEN
            DO:                
               /* ���� ��᫥���� ���� ����⠫���樨 */
               RUN get_beg_date_per IN THIS-PROCEDURE (RECID(b-loan),
                  iCalcDate,
                  OUTPUT oChargeDate).
               vChargeDate = oChargeDate.
               RUN get_beg_kper IN THIS-PROCEDURE (RECID(b-loan), 
                  iCalcDate, 
                  ?, 
                  INPUT-OUTPUT oChargeDate).
               IF oChargeDate EQ vChargeDate THEN
                  oChargeDate = b-loan-cond.since.
                   
               /*�᫨ ��᫥ ��᫥���� ����⠫���樨 � ��� �뫠 �஫������, 
                 � ��� �㦭� ��� �஫����樨*/
               IF b-loan-cond.since GE oChargeDate THEN
               DO:
                  oChargeDate = b-loan-cond.since.
                  RUN correct_date (RECID(b-loan),
                     INPUT-OUTPUT oChargeDate).
                  ASSIGN 
                     oChargeDate = oChargeDate + vPeriodD.
               END.   
               ELSE
                  ASSIGN oChargeDate = oChargeDate + vPeriodD.

               /* ����塞 �������� ���� ���᫥��� ��業⮢ �⭮�⥫쭮 ����⭮� */
               DO WHILE oChargeDate LT iCalcDate:
                  oChargeDate = oChargeDate + vPeriodD.
               END.
            END.
         END.
      END.
   END CASE.
END PROCEDURE. /* END OF PROCEDURE DateOfCharge */

/******************************************************************************/
/* �����頥� ��ਮ� ����� ���� ��᫥����� (�� ���� ����) 
   ���᫥��ﬨ ��業⮢
   
   ������ �� ����� ���������� �������� */
PROCEDURE GetLastChargePeriod:
DEFINE INPUT  PARAMETER iContract AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER iContCode AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER iOpDate   AS DATE    NO-UNDO. /* ��� ���� */
DEFINE OUTPUT PARAMETER oPeriod   AS INT64 NO-UNDO INIT ?.

DEFINE BUFFER b-loan FOR loan.
DEFINE VAR vDate1 AS DATE NO-UNDO. /* ��� ��᫥�����     ���᫥��� */
DEFINE VAR vDate2 AS DATE NO-UNDO. /* ��� �।��᫥����� ���᫥��� */

   FIND FIRST b-loan WHERE b-loan.contract  = iContract
                       AND b-loan.cont-code = iContCode NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-loan THEN RETURN.

   /* ����砥� ���� ��᫥����� ���᫥��� - ��⠥� �⭮�⥫쭮 iOpDate */
   RUN get-beg-date-all IN THIS-PROCEDURE (RECID(b-loan), iOpDate,    OUTPUT vDate1).
   IF vDate1 = ? THEN RETURN.
   /* ����砥� ���� �।��᫥����� ���᫥��� - 
         ��� �⮣� 
      ��⠥� ���� ��᫥�������� ���᫥��� �⭮�⥫쭮 ���� ��᫥����� ���᫥��� :))
      ���� �⭮�⥫쭮 (vDate1 - 1) */
   RUN get-beg-date-all IN THIS-PROCEDURE (RECID(b-loan), vDate1 - 1, OUTPUT vDate2).
   IF vDate2 = ? THEN RETURN.
   
   oPeriod = vDate1 - vDate2.
   RETURN.
END PROCEDURE. /* END OF PROCEDURE GetLastChargePeriod */

/******************************************************************************/
/* �����頥� ��ਮ� ����� ���� ��᫥����� (�� ���� ����) 
   ���᫥��ﬨ ��業⮢
   
   ������ �� �������� ����� - �ᯮ������ ��� ��।������� �஢����
   �������� �� ����������� ������
                            - �᫨ ������ �� ��諮�� 
                            - �ᯮ���� ��楤��� GetLastChargePeriod
   ���� ���� ������������ ��������� �� ������� � ������������
                            - � ��砥 �᫨ ���� �� ���ਨ �������� �ᯥ��.
   
   ��ࠬ��� iOpEntryS ����� ���:  ���_���㬥��,�����_�஢����
   �����_�஢���� ����� ���� �����. ����� �� �饬 ����� ���������� 
   �஢���� 㪠������� ���㬥��.          */
PROCEDURE GetLastChargePeriodS:
DEFINE INPUT  PARAMETER iContract AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER iContCode AS CHAR    NO-UNDO.
DEFINE INPUT  PARAMETER iOpEntryS AS CHAR    NO-UNDO. /* ���ண�� �஢���� */
DEFINE OUTPUT PARAMETER oPeriod   AS INT64 NO-UNDO INIT ?.

DEFINE BUFFER b-op        FOR op.
DEFINE BUFFER b-datablock FOR datablock.
DEFINE BUFFER b-dataline  FOR dataline.
/* ��६�����, �ᯮ��㥬�� ��� �⬥⪨ �� ������⢨� �����-���� ������, ����室���� ��� ���� */
DEFINE VAR vFound AS LOGICAL NO-UNDO INIT TRUE. 

   FIND FIRST b-op WHERE b-op.op = INT64(ENTRY(1, iOpEntryS)) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE b-op THEN RETURN. /* ��� ���� �ਢ�� ���㬥��! */

   FIND FIRST b-datablock WHERE b-datablock.dataclass-id = "dps-nach1"
                            AND b-datablock.beg-date     = b-op.contract-date
                            AND b-datablock.end-date     = b-op.contract-date
                            AND b-datablock.branch-id    = GetUserBranchId(b-op.user-id)
                          NO-LOCK NO-ERROR.
   IF AVAILABLE b-datablock THEN 
   DO:
      /* �饬 �����-����� ����� �஢����, �᫨ �� �� 㪠��� � ��ࠬ��� */
      IF NUM-ENTRIES(iOpEntryS) > 1 AND ENTRY(2,iOpEntryS) = "" THEN DO:
        FIND FIRST b-dataline WHERE b-dataline.data-id = b-datablock.data-id 
                                AND b-dataline.sym1    BEGINS iOpEntryS
                                AND b-dataline.sym2    BEGINS (iContract + "," + iContCode)
                              NO-LOCK NO-ERROR.
        IF AVAILABLE b-dataline THEN
          iOpEntryS = iOpEntryS + ENTRY(2,b-dataline.sym1).
        ELSE 
          vFound = FALSE.
      END.
      
      IF vFound THEN DO:
         oPeriod = 0.
         FOR EACH b-dataline WHERE b-dataline.data-id = b-datablock.data-id 
                               AND b-dataline.sym1    BEGINS iOpEntryS
                               AND b-dataline.sym2    BEGINS (iContract + "," + iContCode)
                               NO-LOCK:
            oPeriod = oPeriod + b-dataline.val[2].
         END.
         IF oPeriod = 0 THEN vFound = FALSE.
      END.
   END.
   ELSE vFound = FALSE.
   
   /* �᫨ � ���ਨ �� ������� ��࠭����� ���᫥��� - ����⠥��� �᪠�� ��
      ॠ�쭮� ��� ���㬥�� ��楤�ன GetLastChargePeriod */
   IF NOT vFound THEN 
      RUN GetLastChargePeriod IN THIS-PROCEDURE 
          (iContract, iContCode, b-op.op-date, OUTPUT oPeriod).

   RETURN.
END PROCEDURE. /* END OF PROCEDURE GetLastChargePeriodS */

/******************************************************************************/
/*                                                                            */
/*                             OLD BLOCK                                      */
/*                                                                            */
/******************************************************************************/


/* �㭪�� �������� ��।����� - �஫��������� �� ����� � ᮧ������ �᫮��� ��� ��� -
   �� ������ ���. ४�����  dep_period  ��� ���. ४�����  ������ */

FUNCTION New_Dps_Prol RETURNS LOG (
   BUFFER dps-loan FOR loan,
   OUTPUT dep_period AS CHARACTER,
   OUTPUT fl-old-dps AS LOGICAL ):

   DEFINE VARIABLE mNewTech AS CHARACTER NO-UNDO.

   fl-old-dps = NO .
   /*�᫨ �ᯮ������ "����� �孮�����" ��� ����� �������
  (������ �� �孮�����, ����ᠭ��� ��� ���, �� ��� ᤢ��� ���) SAP - 0020842 */
   RUN get-param-const-by-kind (dps-loan.class-code, dps-loan.op-kind,
      "������", OUTPUT mNewTech).
   dep_period = GetXattrValueEx("loan",dps-loan.contract + "," +
      dps-loan.cont-code, "dep_period",?).
   IF mNewTech = "��" OR dep_period = ?
      THEN fl-old-dps = YES .
   RETURN  (dep_period <> ?  OR  mNewTech = "��").
END FUNCTION.

/* ���㦨����� ������� ��� �� ����� �孮����� - ����஫� ���� ������
    �����頥� 1, �᫨  ��⠭����� ����஥�� ��ࠬ��� ���_700 �
    ��� �ॢ�蠥�, 㪠������ � ��� ���� �ᯮ������ ��� ࠧ�襭��
�஫����஢��� ����� � ��࠭�祭��� �᫠ �஫����権 � ��⠭�������� ����
������, ࠢ��� ��� ����砭�� �ப�.
   �����頥� -1 - ����� �஫��������� ����࠭�祭��� �᫮ ࠧ, ���� �᫨
   ��⠭����� ४����� limitprol � ��� ������ �।�� + 1 ����
   �⭮�⥫쭮 ����砭�� �ப�
   0 - �� �஫����樨 ���뢠����  ��⠭���� limitprol
*/

FUNCTION New_Dps RETURN INT64(in-op-date AS DATE) :
   IF mDat-700 <> ? AND in-op-date >= mDat-700 THEN
      RETURN 1.
   ELSE IF mdat-700 = ? THEN
      RETURN 0.
   ELSE
      RETURN -1  .
END.

/* ��।������ �����業� ��� �⠢�� ��� ������� �� ����_700  ������� */
FUNCTION stav-half RETURN INT64 (INPUT mBegDate AS DATE, mProlDate AS DATE):
   IF New_Dps(mBegDate) < 0
   THEN
      IF New_Dps(mProlDate) >= 0
      THEN RETURN  2.
   RETURN 1 .
END FUNCTION.

/*  ��।������ ���� , �� ������ ������ ������ ������� �� ���� �������  */
PROCEDURE Get_Date_Comm.
   DEF INPUT  PARAM  vRid AS RECID NO-UNDO .
   DEF INPUT  PARAM bDate AS DATE  NO-UNDO .
   DEF OUTPUT PARAM cDate AS DATE  NO-UNDO .     /* ���, �� ������ ������ ������ ������� */
   DEF OUTPUT PARAM eDate AS DATE  NO-UNDO .
   DEF VAR vMetod AS CHAR NO-UNDO .

   /*  �஢��塞, �� �� ������ */
   FIND loan WHERE RECID(loan) = vRid NO-LOCK NO-ERROR .
    vMetod = GetCacheInit (loan.class-code, '��⮤�����⠢') .
    IF {assigned vMetod} AND vMetod <> "?" THEN
    DO:
       /* ᭠砫� ���饬 �� ����७��� */
       IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES, vMetod) THEN
          RUN VALUE(vMetod) (RECID(loan),
                             bDate,
                             OUTPUT cDate,
                             OUTPUT eDate).
       ELSE
       DO:
          IF SearchPFile (vMetod) THEN 
          DO:
             RUN VALUE(vMetod + ".p") (RECID(loan),
                                       bDate,
                                       OUTPUT cDate,
                                       OUTPUT eDate).
             RETURN.
          END.
          ELSE 
          DO:
   RUN Fill-SysMes IN h_tmess ("","","0", "�� ������ ��⮤ ��� ��।������ ���� �⠢�� " + vMetod ).
   RETURN.
          END.
       END.
    END. /*����� ᢮� ��⮤*/
    ELSE DO:
       RUN get-beg-date-prol(vRid,bDate,output cDate, output eDate) .
       IF stav-half(loan.open-date,bDate) = 2
         AND mDat-700 < cDate
       THEN
          RUN get-beg-date-prol(vRid, mDat-700, OUTPUT cDate, OUTPUT eDate).
    END. /*��⮤ �� �����*/
END PROCEDURE .

/*
��⮤ - �ਬ�� ��� ������� �� ��⮤�����⠢ �� ����� ������.
�㦭�:�롨��� ��業��� �⠢�� �� ���� ������ �������, ��室� �� ���⪠ �� ���� 
������ � �祭�� ��ࢮ�� �ப� ������� � ��᫥ ��ࢮ� �஫����樨. 
��᫥ ��ன � ��᫥�����  �஫����権 �⠢�� ������ �롨����� �� ���� �஫����樨.
��� ��⮤ ࠡ�⠥� � � ⮬ ��砥, �᫨ �� �஫����樨 ������ �� ᮧ������ ����� �᫮���.
*/
PROCEDURE twice_first.
   DEF INPUT  PARAM  vRid AS RECID NO-UNDO .
   DEF INPUT  PARAM bDate AS DATE  NO-UNDO .
   DEF OUTPUT PARAM cDate AS DATE  NO-UNDO .     /* ���, �� ������ ������ ������ ������� */
   DEF OUTPUT PARAM eDate AS DATE  NO-UNDO .
   DEF VAR cDate1 AS DATE NO-UNDO.
   DEF VAR eDate1 AS DATE NO-UNDO.
   /*  �஢��塞, �� �� ������ */
   FIND loan WHERE RECID(loan) = vRid NO-LOCK NO-ERROR .

   RUN get-beg-date-prol(vRid,bDate,output cDate, output eDate) .
   IF cDate GT loan.open-date THEN DO:
      RUN get-beg-date-prol(vRid,cDate - 1,output cDate1, output eDate1) .
      IF cDate1 EQ loan.open-date THEN cDate = loan.open-date .
   END.
END PROCEDURE.

/*��।������ ���� �� ������ �㤥� �᪠���� ��業⭠� �⠢��*/
PROCEDURE Get_Date_Stav.
   DEF INPUT  PARAM  vRid      AS RECID NO-UNDO.
   DEF INPUT  PARAM  vDate     AS DATE  NO-UNDO.
   DEF OUTPUT PARAM  vDateStav AS DATE  NO-UNDO.
   DEF OUTPUT PARAM  vEndDate  AS DATE  NO-UNDO.
   
   DEF VAR mProl AS CHAR NO-UNDO.
   
   FIND FIRST loan WHERE RECID(loan) EQ vRid NO-LOCK NO-ERROR.
   RUN get-beg-date-prol(vRid,vDate,output vDateStav, output vEndDate).
   IF vDateStav NE loan.open-date THEN
   DO:
      FIND LAST loan-cond WHERE loan-cond.contract  EQ loan.contract
                            AND loan-cond.cont-code EQ loan.cont-code
                            AND loan-cond.since     EQ vDateStav
      NO-LOCK NO-ERROR.
      mProl = GetXAttrValue("loan-cond", 
                        loan-cond.contract + "," + loan-cond.cont-code + "," + string(YEAR(loan-cond.since),"9999") + string(MONTH(loan-cond.since),"99") + string(DAY(loan-cond.since),"99"),
                        "prolong").                                         
      IF mProl EQ "��" THEN
         vDateStav = loan-cond.since - 1.
   END.                                             
END PROCEDURE.

FUNCTION Old_Dps RETURNS LOG (
    BUFFER dps-loan FOR loan,
    OUTPUT dep_period AS CHAR):

    DEF VAR mNewTech AS CHAR NO-UNDO.
    DEF VAR fl-old-dps AS LOGICAL NO-UNDO .

    New_Dps_Prol(BUFFER dps-loan, OUTPUT dep_period, OUTPUT fl-old-dps) .
    IF New_Dps(dps-loan.open-date) > 0
    THEN RETURN YES .
    RETURN  fl-old-dps .
END FUNCTION.


/* �����頥� ��� �㡠������᪮�� ���⪠ ����樨,
** �� �࠭���樨 */
FUNCTION Get-Ost RETURNS CHAR (
    INPUT go-op-kind AS CHAR): /* ��� �࠭���樨 */

   DEFINE VAR vResult AS CHARACTER NO-UNDO.
   RUN Get-Ost_P(go-op-kind,OUTPUT vResult).
   RETURN vResult.
END FUNCTION.

PROCEDURE Get-Ost_P:
   DEFINE INPUT  PARAMETER iOpKind AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS CHARACTER NO-UNDO.

   DEF BUFFER go-tmp-op-kind     FOR op-kind.
   DEF BUFFER go-tmp-op-template FOR op-template.

   DEF VAR go-flag-nach  AS LOGICAL NO-UNDO. /* ���� ���᫥��� ��業⮢ */

    /* ���� �࠭���樨 */
   FIND go-tmp-op-kind WHERE
       go-tmp-op-kind.op-kind EQ iOpKind
   NO-LOCK NO-ERROR.

   oResult = ?.
   /* �᫨ �࠭����� �� �������, � ��室�� */
   IF NOT AVAIL go-tmp-op-kind
   THEN RETURN.
   /* �� �ᥬ 蠡����� �࠭���樨 */
   FOR EACH go-tmp-op-template OF go-tmp-op-kind
   NO-LOCK:
      /* �᫨ � �㬬� 㪠���� ��������� "*���*",
      ** � �� �࠭����� ���᫥��� ��業⮢ */
      IF go-tmp-op-template.amt-rub MATCHES '*���*'
      THEN go-flag-nach = yes.

      /* �᫨ � ���� "�।��", ��� ������ ������ॡ������,
      ** � ���᫥��� ��業⮢ �� ������ ������ॡ������ */
      IF go-flag-nach AND
         go-tmp-op-template.acct-cr MATCHES '*loan-dps-p*'
      THEN oResult = '����'.
      ELSE
      /* �᫨ � ���� "�।��" ��� ��筮�� ������,
      ** � ���᫥��� �� ��筮�� ������ */
      IF go-flag-nach AND
         go-tmp-op-template.acct-cr matches '*loan-dps-t*'
      THEN oResult = '�����1'.
      /* �᫨ � ���� "�।��" ��� �।���⥫쭮 ���᫥���� ��業⮢,
      ** � �� �।���⥫쭮� ���᫥��� ��業⮢ (�������筮� 39�) */
      ELSE
      IF     go-flag-nach AND
          (   go-tmp-op-template.acct-cr MATCHES '*loan-dps-int*'
           OR go-tmp-op-template.acct-cr MATCHES '*loan-dps-ink*')
      THEN oResult = '����'.
      IF oResult NE ? THEN RETURN.
   END.
END PROCEDURE.

/* ��।������ ���� ��. ���⪠ �� �����⭮�� 蠡���� */
FUNCTION GET-OST-TEMPL RETURNS CHAR (
    input go-op-kind as char,   /* ��� �࠭���樨 */
    input go_templ as INT64):     /* ����� ��᫥�㥬��� 蠡���� */
   DEFINE VAR vResult AS CHARACTER NO-UNDO.
   RUN GET-OST-TEMPL_P(go-op-kind,go_templ,OUTPUT vResult).
   RETURN vResult.
END FUNCTION.

PROCEDURE GET-OST-TEMPL_P:
   DEFINE INPUT  PARAMETER iOpKind     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iNumTempl   AS INT64   NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult     AS CHARACTER NO-UNDO.

   DEFINE BUFFER go-tmp-op-kind     FOR op-kind.
   DEFINE BUFFER go-tmp-op-template FOR op-template.

   DEFINE VARIABLE go-flag-nach        AS LOGICAL   NO-UNDO.    /* ���� ���᫥��� ��業⮢ */

   oResult = ?.
   /* ���� 蠡���� */
   FIND go-tmp-op-template
      WHERE go-tmp-op-template.op-kind     EQ iOpKind
        AND go-tmp-op-template.op-template EQ iNumTempl
   NO-LOCK NO-ERROR.

   /* �᫨ 蠡��� �� ������, � ��室�� */
   IF NOT AVAIL go-tmp-op-template
   THEN RETURN.

    /* �᫨ � �㬬� 㪠���� ��������� "*���*",
    ** � �� 蠡��� ���᫥��� ��業⮢ */
   IF go-tmp-op-template.amt-rub MATCHES '*���*'
   THEN go-flag-nach = YES.

    /* �᫨ � ���� "�।��", ��� ������ ������ॡ������,
    ** � ���᫥��� ��業⮢ �� ������ ������ॡ������ */
   IF go-flag-nach AND
      go-tmp-op-template.acct-cr MATCHES '*loan-dps-p*'
   THEN oResult = '����'.
   ELSE
   /* �᫨ � ���� "�।��" ��� ��筮�� ������,
   ** � ���᫥��� �� ��筮�� ������ */
   if go-flag-nach and
      go-tmp-op-template.acct-cr matches '*loan-dps-t*'
   then oResult = '�����1'.
   ELSE
   /* �᫨ � 蠡���� ��������� ��� �।���⥫쭮 ���᫥���� ��業⮢,
   ** � �� �।���⥫쭮� ���᫥��� ��業⮢ (�������筮� 39�) */
   IF go-flag-nach AND
      (go-tmp-op-template.acct-cr MATCHES '*loan-dps-int*' OR
       go-tmp-op-template.acct-db MATCHES '*loan-dps-int*' OR
       go-tmp-op-template.acct-cr MATCHES '*loan-dps-ink*' OR
       go-tmp-op-template.acct-db MATCHES '*loan-dps-ink*')
   THEN DO:
      if  go-tmp-op-template.acct-db matches '*loan-dps-p*' or
          go-tmp-op-template.acct-cr matches '*loan-dps-p*'
      then oResult = '�����'.
      else if go-tmp-op-template.acct-db matches '*loan-dps-t*' or
              go-tmp-op-template.acct-cr matches '*loan-dps-t*'
           then oResult = '�����1'.
           else oResult = '����'.
    END.
    RETURN.
END PROCEDURE.

/**
  mitr:
    �맮� get-end-date �१ ����奬�
    � ����� ������� ����� ���� 㪠��� ��⮤ get-end-date
    �᫨ ��⮤ �� 㪠���, ��뢠���� �㭪�� get-end-date
*/
PROCEDURE IGet-End-Date_:
  DEF INPUT PARAM in-class AS CHAR NO-UNDO.
  DEF INPUT PARAM in-date AS DATE NO-UNDO.
  DEF INPUT PARAM str-fmt AS CHAR NO-UNDO.
  DEF OUTPUT PARAM out-val AS DATE NO-UNDO.

  DEF VAR nameproc AS CHAR NO-UNDO.
  DEF VAR params AS CHAR NO-UNDO.
  DEF VAR d AS DATE INITIAL ? NO-UNDO.

     RUN GetClassMethod in h_xclass (in-class, 'get-end-date',
                                      "","",
                                      output nameproc,
                                      output params).
     IF nameproc EQ ? THEN DO:
        d = get-end-date (in-date, str-fmt).
      END. ELSE DO:
        RUN value(nameproc) (in-date, str-fmt, OUTPUT d) .
      END.
  out-val = d .
END PROCEDURE.


PROCEDURE IGet-End-Date:
  DEF INPUT PARAM rid AS recid.
  DEF INPUT PARAM in-date AS DATE.
  DEF INPUT PARAM str-fmt AS CHAR.
  DEF OUTPUT PARAM out-val AS DATE.

  DEF BUFFER loan$ FOR loan.
  DEF VAR nameproc AS CHAR NO-UNDO.
  DEF VAR params AS CHAR NO-UNDO.
  DEF VAR d AS DATE INITIAL ? NO-UNDO.

  find loan$ where recid(loan$) eq rid no-lock no-error .
  if avail loan$ then 
  RUN IGet-End-Date_(loan$.class-code,in-date,str-fmt,OUTPUT out-val) .
END PROCEDURE.


/**
  mitr:
    ����� get-end-date ��� �����࣡����
    ��뢠�� ⮫쪮 �१ ��⮤ get-end-date , ��।������ � ����奬�,
    ��祬 �맮� ��⮤� �����⢫���� � �㭪樨 Iget-end-date

*/
PROCEDURE Get-End-Date2:

   DEF INPUT PARAM in-date AS DATE.
   DEF INPUT PARAM str-fmt AS CHAR.
   DEF OUTPUT PARAM out-val AS DATE.

DEF VAR j AS INT64 NO-UNDO.
DEF VAR tmp-str AS CHAR NO-UNDO.
DEF VAR tmp-date AS DATE NO-UNDO.
DEF VAR num-year  AS INT64 NO-UNDO.
DEF VAR num-month AS INT64 NO-UNDO.
DEF VAR num-day   AS INT64 NO-UNDO.
def var num-date as INT64 no-undo .

IN-date = in-date + 1.
  DO j = 1 TO NUM-ENTRIES(str-fmt,','):
     tmp-str = ENTRY(j,str-fmt,',').
     IF NUM-ENTRIES(tmp-str,'=') NE 2 THEN NEXT.
     CASE TRIM(ENTRY(1,tmp-str,'=')):
        WHEN '�' OR WHEN '�' OR WHEN 'Y' OR WHEN 'y' THEN DO:
           num-year = num-year + INT64(TRIM(ENTRY(2,tmp-str,'='))).
        END.
        WHEN '�' OR WHEN '�' OR WHEN 'M' OR WHEN 'm' THEN DO:
           num-month = num-month + INT64(TRIM(ENTRY(2,tmp-str,'='))).
        END.
        WHEN '�' OR WHEN '�' OR WHEN 'D' OR WHEN 'd' THEN DO:
           num-day = num-day + INT64(TRIM(ENTRY(2,tmp-str,'='))).
        END.
     END CASE.
  END.

  num-year  = YEAR(in-date) + num-year +
              truncate(num-month / 12,0) +  truncate((num-month modulo 12 + month(in-date)) / 12,0) - ( IF
              (num-month  + month(in-date)) modulo 12 eq 0 then 1 else 0)  .

  num-month = (num-month  + MONTH(in-date)) modulo 12 .
  if num-month eq 0 then num-month = 12 .
  num-date = day(in-date) no-error.
  do on error undo,retry :
     hide message no-pause .
     if retry then do :
        num-date = num-date - 1.
     end.
     tmp-date  = DATE(num-month, num-date, num-year).
  end.
  IF tmp-date = in-date THEN DO:
    /* ��� �� � �� */
    tmp-date  = tmp-date + num-day - 1 .
  END. ELSE DO:
    tmp-date  = tmp-date + num-day .
  END.
  out-val = tmp-date  +  IF new_dps(in-date - 1) < 0 THEN 1 ELSE 0 .
END PROCEDURE.

/* �����頥� �������� ���� ���㬥��
** ���᫥��� ��業⮢ � �������� ���ࢠ�� ���,
** ��祬 �஢���� �뫮 �� ���᫥��� ��業⮢ ���।,
** �᫨ �뫮, � �����頥� ? */
FUNCTION GET-BEG-DATE-KL RETURNS DATE (
    INPUT str-kau     AS CHAR,  /* ��ப� � �ଠ�
                                ** "<�����祭��>,<�����>,[<�� ���⮪>]" */
    INPUT in-beg-date AS DATE,  /* ��� ������/�஫����樨 ������ */
    INPUT in-op-date  AS DATE): /* ��� ����樮����� ��� */

    DEF BUFFER loan-acct-p   FOR loan-acct.
    DEF BUFFER loan-acct-t   FOR loan-acct.
    DEF BUFFER buf-loan-acct FOR loan-acct.
    DEF BUFFER buf-loan      FOR loan.
    DEF BUFFER buf-lcond     FOR loan-cond.
    DEF BUFFER buf-op-entry  FOR op-entry.
    DEF BUFFER buf-op        FOR op.
    DEF BUFFER buf-templ     FOR op-template.

    DEF VAR in-surrogate AS CHARACTER NO-UNDO.
    DEF VAR str-ost      AS CHARACTER NO-UNDO.  /* ���� ᡠ����⨪� */
    DEF VAR tmp-str      AS CHARACTER NO-UNDO.
    DEF VAR d-delay      AS INT64   NO-UNDO.
    DEF VAR i            AS INT64   NO-UNDO.
    DEF VAR beg-date     AS DATE      NO-UNDO.
    DEF VAR fl           AS LOGICAL   NO-UNDO.

    /* ���� ������ */
    FIND buf-loan WHERE
        buf-loan.contract  EQ ENTRY(1,str-kau) AND
        buf-loan.cont-code EQ ENTRY(2,str-kau)
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-loan
    THEN RETURN ?.

    /* ���� �᫮��� ������� */
    FIND FIRST buf-lcond WHERE
        buf-lcond.contract  EQ buf-loan.contract AND
        buf-lcond.cont-code EQ buf-loan.cont-code
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-lcond
    THEN RETURN ?.

    /* ���� ����窨 ������ */
    FIND buf-templ WHERE
        buf-templ.op-kind  EQ buf-loan.op-kind AND
        buf-templ.op-templ EQ buf-loan.op-templ
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-templ
    THEN RETURN ?.

    /* �᫨ ����� ������ॡ������, � �饬 �᭮���� ��� �
    ** ��⠭�������� ᮮ⢥�����騩 ��� ���⪠ */
    IF buf-loan.end-date EQ ?
    THEN DO:
        FIND LAST loan-acct-p OF buf-loan WHERE
            loan-acct-p.acct-type EQ "loan-dps-p" AND
            loan-acct-p.since     LE in-op-date
        NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-acct-p
        THEN RETURN ?.

        str-ost = "����".
    END.
    /* �᫨ ����� ����,
    ** � �饬 ��� ��筮�� ������ � ������ ������ॡ����� */
    ELSE DO:
        /* ���� ��� ������ ������ॡ������ */
        FIND LAST loan-acct-p OF buf-loan WHERE
            loan-acct-p.acct-type EQ "loan-dps-p" AND
            loan-acct-p.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* ���� ��� ��筮�� ������ */
        FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-t" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� ������ ��� ��筮�� ������,
        ** �饬 ��� ��筮�� ������ � ���������ﬨ */
        IF NOT AVAIL loan-acct-t
        THEN FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-ts" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� ������ ��� ��筮�� ������,
        ** �饬 ��� ��筮�� ������ � ���������ﬨ ����⠫ */
        IF NOT AVAIL loan-acct-t
        THEN FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-tsk" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� �� ������, � ��室��. */
        IF NOT AVAIL loan-acct-t
        THEN RETURN ?.

        str-ost = "�����1".

        /* �᫨ ������ ��� ��筮�� ������,
        ** � ������塞 � ��� �㡠����⨪� �� ��業⠬ � ᯨ᮪ */
        IF AVAIL loan-acct-p
        THEN str-ost = str-ost + ",����".
    END.

    /* �᫨ �㡠����⨪� �� ��।�����, � ��室�� */
    IF str-ost = ""
    THEN RETURN ?.

    beg-date = in-beg-date.

    /* �� �ᥬ �㡠������᪨� ���⪠� */
    DO i = 1 TO NUM-ENTRIES(str-ost):

        /* ���� ��� ������ ������ॡ������ */
        IF ENTRY(i,str-ost) EQ "����"
        THEN FIND buf-loan-acct WHERE
            RECID(buf-loan-acct) EQ RECID(loan-acct-p)
        NO-LOCK NO-ERROR.
        /* ���� ���� ��� ��筮�� ������ */
        ELSE FIND buf-loan-acct WHERE
            RECID(buf-loan-acct) EQ RECID(loan-acct-t)
        NO-LOCK NO-ERROR.

        /* ��ନ஢���� ���ண�⭮� ��ப� */
        tmp-str = buf-loan.contract + "," +
                  buf-loan.cont-code + "," +
                  ( IF AVAIL loan-acct-t AND
                      loan-acct-t.acct-type BEGINS "loan-dps-ts"
                      THEN ","
                      ELSE "") +
                  ENTRY(i,str-ost).

        /* �� �ᥬ �㡠������᪨� �஢����� ���᫥���� ��業⮢,
        ** ����� �।����� ᮮ⢥�����騩 �᭮���� ��� ������
        ** (�������/���) ����� �� ���� ⥪�饣� ���भ� */
        FOR EACH kau-entry WHERE
            kau-entry.kau      EQ tmp-str AND
            kau-entry.kau-id   EQ buf-loan-acct.acct-type AND
            NOT kau-entry.debit AND
            kau-entry.acct     EQ buf-loan-acct.acct AND
            kau-entry.currency EQ buf-loan-acct.currency AND
            kau-entry.op-date  GE in-op-date
        NO-LOCK:

            /* ���� �������᪮� �஢���� �� ⥪�饩 �㡯஢���� */
            FIND buf-op-entry OF kau-entry
            NO-LOCK NO-ERROR.

            /* �᫨ �� ��७�� ��業⮢,
            ** � ��室 */
            IF AVAIL buf-op-entry AND
                NOT (buf-op-entry.kau-db EQ buf-loan.contract + "," +
                                       buf-loan.cont-code + "," +
                                       '����' 
                                       OR CAN-FIND(FIRST loan-acct OF buf-loan WHERE 
                                        loan-acct.acct EQ buf-op-entry.acct-db
                                        AND loan-acct.acct-type EQ 'loan-expens' )) 
                                        AND
                kau-entry.contract-date GE in-op-date
            THEN  RETURN ?.
        END.

        /* ��孮�����᪮� ���㫥��� ���� */
        RELEASE buf-op-entry.

        /* �� �ᥬ �㡠������᪨� �஢����� ���᫥���� ��業⮢,
        ** ����� �।����� ᮮ⢥�����騩 �᭮���� ��� ������
        ** (�������/���) �� ���� beg-date �� ⥪�饣� ���भ� */
        FOR EACH kau-entry WHERE
            kau-entry.kau      EQ tmp-str AND
            kau-entry.kau-id   EQ buf-loan-acct.acct-type AND
            NOT kau-entry.debit AND
            kau-entry.acct     EQ buf-loan-acct.acct AND
            kau-entry.currency EQ buf-loan-acct.currency AND
            kau-entry.op-date  GE beg-date and
            kau-entry.op-date  LE in-op-date
        NO-LOCK
        BREAK BY kau-entry.op-date
        DESCENDING:

            /* ���� �������᪮� �஢���� �� �㡯஢���� */
            FIND LAST buf-op-entry OF kau-entry
            NO-LOCK NO-ERROR.

            /* ���� ���㬥�� �� �㡯஢���� */
            FIND buf-op OF kau-entry
            NO-LOCK NO-ERROR.

            /* �᫨ ������� �஢���� � 㪠����� ����⮬,
            ** � ������� 䫠� */
            IF kau-entry.kau EQ str-kau
            THEN fl = YES.

            /* �᫨ �� ���᫥��� ��業⮢,
            ** � �뢠�������� �� 横��. */
            IF AVAIL buf-op-entry AND
                (buf-op-entry.kau-db EQ "" OR
                 buf-op-entry.kau-db EQ ?)
            THEN LEAVE.

            /* ����� ��祣� �� ��諨, �
            ** ���᪠�� ���� �������᪮� �஢���� */
            IF LAST(kau-entry.op-date) THEN RELEASE buf-op-entry.
        END.

        IF AVAIL buf-op-entry
        THEN DO:
            /* ��᢮���� �������� ���� ���㬥�� */
            beg-date = IF beg-date LT kau-entry.contract-date
                       THEN kau-entry.contract-date
                       ELSE beg-date.

        END.
    END.

    /* �᫨ �������� ��� ����樨 ᮢ������
    ** c ��⮩ ⥪�饣� (�஢��塞��� ��) � �� �஢��� ���������
    ** �������� ����� ���
    ** ��� �� �� ࠢ�� �������� ��� ���������� ���㬥��,
    ** � �����頥� ��������� ���� ���� ? */
    RETURN IF (beg-date EQ in-op-date AND
               NOT fl) OR
              beg-date NE in-op-date
           THEN beg-date
           ELSE ?.

END FUNCTION.

/* �����頥� ���� ��᫥����� ���᫥���/��७�� ��業⮢
** �� �� "�����" � ���ࢠ�� ��� */
FUNCTION GET-BEG-SIGNS RETURNS DATE(
    input str-kau    as char,   /* ���ண�� ����� "<�����祭��>,<�����>" */
    input beg-date   as date,   /* ��� ������/�஫����樨 ������ */
    input in-op-date as date):  /* �������� ��� ����樨 */

    def buffer buf-loan for loan.
    DEF VAR mPrName AS CHAR NO-UNDO.
    
    mPrName = IF NUM-ENTRIES(program-name(3)," ") = 2
              THEN ENTRY(2,program-name(3)," ")
              ELSE program-name(3).

    /* ���� ������ */
    find buf-loan where
        buf-loan.contract  eq entry(1,str-kau) and
        buf-loan.cont-code eq entry(2,str-kau)
    no-lock no-error.

    /* �� �� ���祭�� �� "�����". */
    for each signs use-index allfield where
        signs.code      eq '�����' and
        signs.file-name eq 'op' and
        signs.code-val  eq buf-loan.cont-code
    no-lock:

        /* ���� ���㬥�� �� ���஬ ��।�����
        ** ������ ���祭�� ४����� "�����" */
        find op where
            op.op eq INT64(signs.surrogate)
        no-lock no-error.

        /* �᫨ ������ ���㬥�� � �������� ��� ���㬥�� �����
        ** ���� ��砫� ��᫥�㥬��� ���ॢ��� � �᫨ �맮� ���� ��
        ** ��㧥� ���ﭨ� ������ � �������� ��� ���㬥�� �����
        ** ���� ����砭�� ��᫥�㥬��� ���ࢠ��,
        ** � �����頥� �������� ���� */

        if avail op and
           op.contract-date gt beg-date and
           op.op-status     gt '�' and
           ( IF CAN-DO({&nameproc}, mPrName) 
            then op.contract-date le in-op-date
            else true)
        then beg-date = op.contract-date.
    end.

    return beg-date.

END FUNCTION.

PROCEDURE Get-Beg-Signs-T:
    DEFINE INPUT  PARAMETER str-kau           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER beg-date          AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER in-op-date        AS DATE      NO-UNDO.
    DEFINE INPUT  PARAMETER in-op-transaction AS INT64   NO-UNDO.
    DEFINE OUTPUT PARAMETER oBeg-Date         AS DATE      NO-UNDO.

    DEF BUFFER buf-loan      FOR loan.
    DEF VAR mPrName AS CHAR NO-UNDO.

    mPrName = IF NUM-ENTRIES(program-name(3)," ") = 2
              THEN ENTRY(2,program-name(3)," ")
              ELSE program-name(3).

    FIND buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                    AND buf-loan.cont-code EQ ENTRY(2,str-kau)
    NO-LOCK NO-ERROR.
    FOR EACH signs USE-INDEX allfield
        WHERE signs.code      EQ "DrVklad"   
          AND signs.file-name EQ "op-entry"  
          AND signs.code-val  EQ buf-loan.cont-code
    NO-LOCK:
       FIND op WHERE op.op = INT64(ENTRY (1,signs.surrogate)) no-lock no-error.
       IF     AVAIL op
          AND op.contract-date  GT beg-date
          AND op.op-status      GT '�'
          AND op.op-transaction NE in-op-transaction
          AND ( IF CAN-DO({&nameproc}, mPrName)  THEN op.contract-date LE
          in-op-date ELSE TRUE)
          THEN beg-date = op.contract-date .
   END.
   oBeg-Date = beg-date.
END PROCEDURE.

FUNCTION GET-BEG-DATE RETURNS DATE (
    INPUT str-kau    AS CHAR,
    INPUT in-op-date AS DATE):

    DEF BUFFER loan-acct-int FOR loan-acct.
    DEF BUFFER loan-acct-p   FOR loan-acct.
    DEF BUFFER loan-acct-t   FOR loan-acct.
    DEF BUFFER buf-loan-acct FOR loan-acct.
    DEF BUFFER buf-loan      FOR loan.
    DEF BUFFER buf-lcond     FOR loan-cond.
    DEF BUFFER buf-op-entry  FOR op-entry.
    DEF BUFFER buf-op        FOR op.
    DEF BUFFER buf-templ     FOR op-template.

    DEF VAR in-surrogate AS CHARACTER NO-UNDO.
    DEF VAR str-ost      AS CHARACTER NO-UNDO.
    DEF VAR tmp-str      AS CHARACTER NO-UNDO.
    DEF VAR d-delay      AS INT64   NO-UNDO.
    DEF VAR i            AS INT64   NO-UNDO.
    DEF VAR beg-date     AS DATE      NO-UNDO.
    DEF VAR fl           AS LOGICAL   NO-UNDO.
    DEF VAR tmp-sig      AS CHAR NO-UNDO.

    /* ���� ������ */
    FIND buf-loan WHERE
        buf-loan.contract  EQ ENTRY(1,str-kau) AND
        buf-loan.cont-code EQ ENTRY(2,str-kau)
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-loan THEN RETURN ?.

    /* ���� �᫮��� */
    FIND FIRST buf-lcond WHERE
        buf-lcond.contract  EQ buf-loan.contract AND
        buf-lcond.cont-code EQ buf-loan.cont-code
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-lcond THEN RETURN ?.

    /* ���� 蠡���� ����窨 */
    FIND buf-templ WHERE
        buf-templ.op-kind  EQ buf-loan.op-kind AND
        buf-templ.op-templ EQ buf-loan.op-templ
    NO-LOCK NO-ERROR.
    IF NOT AVAIL buf-templ THEN RETURN ?.

    /* ���� ��� �।���⥫쭮 ���᫥���� ��業⮢ */
    FIND LAST loan-acct-int OF buf-loan WHERE
        loan-acct-int.acct-type EQ "loan-dps-int" AND
        loan-acct-int.since     LE in-op-date
    NO-LOCK NO-ERROR.

    /* �᫨ ����� ������ॡ������, � �饬 ��� ������ ������ॡ������
    ** � ��⠭�������� �� ���⮪ ���᫥��� ��業⮢ */
    IF buf-loan.end-date EQ ?
    THEN DO:

        FIND LAST loan-acct-p OF buf-loan WHERE
            loan-acct-p.acct-type EQ "loan-dps-p" AND
            loan-acct-p.since     LE in-op-date
        NO-LOCK NO-ERROR.
        IF NOT AVAIL loan-acct-p
        THEN RETURN ?.

        str-ost = "����".
    END.
    ELSE DO:
        /* �᫨ ���� �����, � �饬 ���
        ** ������ ������ॡ������ �
        ** ��� ��筮�� ������.
        ** ��⠭�������� ���/���� �㡠������᪮�� ���⪠ ���᫥��� %% */
        FIND LAST loan-acct-p OF buf-loan WHERE
            loan-acct-p.acct-type EQ "loan-dps-p" AND
            loan-acct-p.since     LE in-op-date
        NO-LOCK NO-ERROR.

        FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-t" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� ������ ��� ��筮�� ������,
        ** �饬 ��� ���������� */
        IF NOT AVAIL loan-acct-t
        THEN FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-ts" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� ������ ��� ��筮�� ������,
        ** �饬 ��� ���������� ����⠫ */
        IF NOT AVAIL loan-acct-t
        THEN FIND LAST loan-acct-t OF buf-loan WHERE
            loan-acct-t.acct-type EQ "loan-dps-tsk" AND
            loan-acct-t.since     LE in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �� ��筮�� ������ �� ������ �� ���� �� ��⮢
        ** (loan-dpsp-t ��� loan-dps-ts), � ��室�� */
        IF NOT AVAIL loan-acct-t THEN RETURN ?.

        str-ost = "�����1".

        IF AVAIL loan-acct-p
        THEN str-ost = str-ost + ",����".

    END.

    /* ��� �� ������� */
    IF str-ost = "" THEN RETURN ?.

    IF buf-loan.prolong EQ 0
    THEN beg-date = buf-lcond.since + d-delay.
    ELSE DO:

        /*  ��� ������-� �஡���, �� ⮫쪮 d-delay ���樠����஢�� 0
        ** � ����� ����� �� ��८�।������. */
        ASSIGN
            beg-date     = buf-loan.open-date + d-delay
            in-surrogate = buf-templ.op-kind + ',' + STRING(buf-templ.op-templ)
        .

        /* ���� �த����⥫쭮�� ������ */
        tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).

        /* ������ �������� ���� �஫����樨 */
        beg-date = Get-end-date (beg-date,tmp-sig).

        /* ������ �࠭����� �஫����樨 */
        tmp-sig = GetOpTemlXattr(in-surrogate,"prol-kind",?).

        /* ���� 蠡���� ����窨 �� �࠭���樨 �஫����樨 */
        IF tmp-sig NE ?
        THEN FIND buf-templ WHERE
            buf-templ.op-templ EQ buf-loan.op-templ AND
            buf-templ.op-kind  EQ tmp-sig
        NO-LOCK NO-ERROR.

        /* ��ନ஢���� ���ண�� 蠡���� */
        in-surrogate = buf-templ.op-kind  + ',' + STRING(buf-templ.op-templ).

        /* ���� �த����⥫쭮�� ������ */
        tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).

        /* �᫨ �� ������� �த����⥫쭮���,
        ** � �ନ஢���� ���ண� 蠡���� �� �࠭���樨 ������ */
        IF tmp-sig = ? THEN DO:
            in-surrogate = buf-loan.op-kind + ',' + STRING(buf-loan.op-template).
            tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).

        END.

        /* �᫨ �� �த����⥫쭮��� ������ ������,
        ** � ���⨬ �� 2� �� ���-�� �஫����権 ������ */
        IF tmp-sig NE ? THEN
        DO i = 2 TO buf-loan.prolong:
            beg-date = get-end-date(beg-date,tmp-sig).
        END.
    END.

    /* beg-date � ᥡ� �࠭�� �������� ���� ����砭�� � ��⮬ �஫����権 */

    /* �᫨ �������� ��� ����砭��/�஫����樨 ������
    ** ����� �������� ���� ����樨, � ��室�� */
    IF beg-date GT in-op-date THEN RETURN ?.

    /* ��ନ஢���� ���ண�� �।���⥫쭮�� ���᫥��� ��業⮢ */
    tmp-str  = buf-loan.contract + "," +
               buf-loan.cont-code + "," +
               "����".

    /* �᫨ ��।���� ��� �।���⥫쭮�� ���᫥��� ��業⮢,
    ** � �饬 ��ࢮ� �।���⥫쭮� ���᫥��� %%
    ** ����� �������� ���� ���㬥�� */
    IF AVAIL loan-acct-int
    THEN DO:

        /* ���� ��ࢮ�� ���᫥��� ��業⮢ */
        FIND FIRST kau-entry WHERE
            kau-entry.kau      EQ tmp-str AND
            kau-entry.kau-id   EQ "loan-dps-int" AND
            NOT kau-entry.debit AND
            kau-entry.acct     EQ loan-acct-int.acct AND
            kau-entry.currency EQ loan-acct-int.currency AND
            kau-entry.op-date  GT in-op-date
        NO-LOCK NO-ERROR.

        /* �᫨ �������� ��� ���㬥�� ����� �������� ���� ����樨,
        ** � ��室�� */
        IF AVAIL kau-entry
          AND kau-entry.contract-date  GT  in-op-date THEN RETURN ?.

    END.

    /* �� �ᥬ ��⠬ �।���⥫쭮�� ���᫥��� ��業⮢ */
    FOR EACH loan-acct-int OF buf-loan WHERE
        loan-acct-int.acct-type EQ 'loan-dps-int' AND
        loan-acct-int.since     LE in-op-date
    NO-LOCK:

        /* ���� ��᫥���� ������� � �������� ���
        ** �஢���� �।���⥫쭮�� ���᫥��� %% */
        FIND LAST kau-entry WHERE
            kau-entry.kau      EQ tmp-str        AND
            kau-entry.kau-id   EQ "loan-dps-int" AND
            NOT kau-entry.debit                  AND
            kau-entry.acct EQ loan-acct-int.acct AND
            kau-entry.currency EQ loan-acct-int.currency AND
            kau-entry.op-date  LE in-op-date
        NO-LOCK NO-ERROR.

        IF AVAIL kau-entry
        THEN DO:

            /* ���� �������᪮� �஢���� */
            FIND LAST buf-op-entry OF kau-entry
            NO-LOCK NO-ERROR.

            IF AVAIL buf-op-entry
            THEN DO:
                /* � beg-date ��ᢠ������� �������� ��� ���㬥��,
                ** � ��砥 �᫨ ��� ��ண� ����� �������� ����
                ** ����砭�� ������ */
                beg-date = IF beg-date LT kau-entry.contract-date
                           THEN kau-entry.contract-date
                           ELSE beg-date.
            END.
        END.
    END.

    /* ���४�஢�� beg-date
    ** �㤥� ᮤ�ঠ�� �������� ���� ��७�� %% */
    beg-date = Get-Beg-Date-Kl(str-kau, beg-date, in-op-date).

    /* ���४�஢�� beg-date.
    ** �᫨ ���᫥���/��७�� ��業⮢ ������� � �ᯮ�짮������ ��  "�����",
    ** � �㤥� ᮤ�ঠ�� ���� ��᫥����� ���᫥���/��७�� ��業⮢ */
    beg-date = Get-Beg-Signs (str-kau, beg-date, in-op-date).

    RETURN beg-date.

END FUNCTION.


FUNCTION chk_class_holiday RETURNS LOGICAL (INPUT in-class AS CHAR):
   /*����� �� ������� %% �� � �������� ���� ��� �⮣� �����*/

   IF GetCacheInit(in-class,"�����") = "��" THEN RETURN TRUE.
   ELSE RETURN FALSE.
END.

FUNCTION chk_date_holiday RETURNS LOGICAL (
   INPUT in-day     AS DATE,     /* ���� ��� �஢�ન */
   INPUT in-op-date AS DATE,     /* ����. ���� */
   INPUT iLoanSurr  AS CHAR,     /* ���ண�� ������� */
   INPUT in-class   AS CHAR      /* ����� ������� */
):

   DEF      VAR      d            AS DATE      NO-UNDO.
   DEF      VAR      vWorkGraf    AS CHARACTER NO-UNDO. /* ��䨪 */
   DEF      VAR      vIsWorkDay   AS LOGICAL   NO-UNDO. /* �ਧ��� ࠡ�祣� ��� */
   DEF      VAR      vResult      AS LOGICAL   NO-UNDO.
   DEFINE   VARIABLE vBrnchGraf   AS CHARACTER NO-UNDO.

   DEF BUFFER bLoan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      /*����� �� ������� %% �� � �������� ���� ��� �⮣� �����*/
      IF NOT chk_class_holiday(in-class) THEN       
         UNDO MAIN, LEAVE MAIN.               
   
      vWorkGraf  = GetWorkGraf(iLoanSurr,
                               in-class).

      FIND FIRST bLoan WHERE bLoan.contract  EQ ENTRY(1, iLoanSurr)
                         AND bLoan.cont-code EQ ENTRY(2, iLoanSurr)
         NO-LOCK NO-ERROR.      
      IF AVAIL bLoan THEN         
         vBrnchGraf = GetXAttrValue("branch",
                                    STRING(bLoan.branch-id),
                                    "���������").

      IF    {assigned vWorkGraf}
         OR {assigned vBrnchGraf} THEN
      DO:
         vIsWorkDay = IsWorkDayGraf (in-day, IF {assigned vBrnchGraf} THEN vBrnchGraf
                                                                      ELSE vWorkGraf).
         /*�᫨ �������� ��� ���᫥��� �� ��室��� - ������ � ���*/
         IF    vIsWorkDay 
            OR in-op-date LT In-day THEN                      
            UNDO MAIN, LEAVE MAIN.
         
         d = in-op-date.
         DO WHILE d GT in-day:
            d = d - 1.
            /*�᫨ ����� ���� ����� ࠡ�稩 - � ��� � ������*/
            vIsWorkDay = IsWorkDayGraf (d, IF {assigned vBrnchGraf} THEN vBrnchGraf
                                                                    ELSE vWorkGraf).
            IF vIsWorkDay THEN             
               UNDO MAIN, LEAVE MAIN.            
         END.
      END.
      ELSE
      DO:
         /*�᫨ �������� ��� ���᫥��� �� ��室��� - ������ � ���*/
         IF    (NOT {holiday.i in-day })
            OR in-op-date LT In-day THEN          
            UNDO MAIN, LEAVE MAIN.
         
         d = in-op-date.
         DO WHILE d GT in-day:
            d = d - 1.
            /*�᫨ ����� ���� ����� ࠡ�稩 - � ��� � ������*/
            IF NOT {holiday.i d} THEN             
               UNDO MAIN, LEAVE MAIN.            
         END.
      END.   
      vResult = YES.
   END.
   RETURN vResult.
END.

/* �஢����, ����� �� � ��� ���� �� ������� ������ ������� ��業�� */
FUNCTION Chk_Date RETURNS LOGICAL (INPUT in-recid   AS RECID,
   INPUT in-op-date AS DATE  ):

   DEF    BUFFER buf-loan-cond FOR loan-cond.
   DEF    BUFFER buf-loan      FOR loan.

   FIND buf-loan-cond WHERE RECID(buf-loan-cond ) EQ in-recid
      NO-LOCK NO-ERROR.   
   IF NOT AVAIL buf-loan-cond THEN 
      RETURN NO.
   
   FIND buf-loan WHERE buf-loan.contract  EQ buf-loan-cond.contract
      AND buf-loan.cont-code EQ buf-loan-cond.cont-code
      NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-loan THEN 
      RETURN NO.

   {chk_date.i}

END FUNCTION.

/* �஢����, ����� �� � ��� ���� �� ������� ������ ������� ��業�� */
FUNCTION Chk_Date_Bufs RETURNS LOGICAL (
   BUFFER buf-loan FOR loan,
   BUFFER buf-loan-cond FOR loan-cond,
   INPUT in-op-date AS DATE):

   IF NOT AVAILABLE buf-loan OR NOT AVAILABLE buf-loan-cond THEN
      RETURN FALSE.
   {chk_date.i}

END FUNCTION.

/*���� �த����⥫쭮�� ������ �� dep_period �� loan ��� �� 蠡���� �࠭���樨 */
FUNCTION Get-Dep-Period RETURNS CHAR (INPUT str-kau    AS CHAR).
   DEF BUFFER buf-loan      FOR loan.
   DEF BUFFER buf-templ     FOR op-template.

   DEF VAR in-surrogate AS CHARACTER NO-UNDO.
   DEF VAR tmp-sig      AS CHAR NO-UNDO.

   FIND buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                   AND buf-loan.cont-code EQ ENTRY(2,str-kau)
                                             NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-loan THEN RETURN ?.

   tmp-sig = GetXattrValueEx ("loan",str-kau,"dep_period",?) .
   if tmp-sig <> ? THEN RETURN tmp-sig.

   FIND buf-templ WHERE buf-templ.op-kind  EQ buf-loan.op-kind
                    AND buf-templ.op-templ EQ buf-loan.op-templ
                                           NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-templ THEN RETURN ?.
   in-surrogate  =  buf-templ.op-kind + ',' + string(buf-templ.op-templ) .
   tmp-sig = GetOpTemlXattr(in-surrogate,"prol-kind",?).
   IF tmp-sig NE ? AND tmp-sig NE "" AND buf-loan.prolong > 0 THEN
      FIND buf-templ WHERE buf-templ.op-templ EQ buf-loan.op-templ
                       AND buf-templ.op-kind  EQ tmp-sig
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-templ THEN RETURN ?.
   in-surrogate =  buf-loan.op-kind + ',' + string(buf-loan.op-template) .
   RETURN GetOpTemlXattr(in-surrogate,"dep-period",?).
END FUNCTION.

FUNCTION Get-Beg-Date-Fine RETURNS DATE (INPUT str-kau    AS CHAR,
                                    INPUT in-op-date AS DATE):

   DEF BUFFER buf-loan      FOR loan.
   DEF BUFFER buf-templ     FOR op-template.

   DEF VAR in-surrogate AS CHARACTER NO-UNDO.
   DEF VAR str-ost      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR tmp-str      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR d-delay      AS INT64 NO-UNDO.
   DEF VAR i            AS INT64 NO-UNDO.
   DEF VAR beg-date     AS DATE NO-UNDO.
   DEF VAR tmp-sig      AS CHAR NO-UNDO.


   FIND buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                   AND buf-loan.cont-code EQ ENTRY(2,str-kau)
                                             NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-loan THEN RETURN ?.
   FIND buf-templ WHERE buf-templ.op-kind  EQ buf-loan.op-kind
                    AND buf-templ.op-templ EQ buf-loan.op-templ
                                           NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-templ THEN RETURN ?.
   IF buf-loan.prolong EQ 0 THEN beg-date = buf-loan.open-date + d-delay .
   ELSE  DO:
      beg-date = buf-loan.open-date + d-delay .
      in-surrogate  =  buf-templ.op-kind + ',' + string(buf-templ.op-templ) .
      tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).
      beg-date = Get-end-date(beg-date,tmp-sig).
      tmp-sig = GetOpTemlXattr(in-surrogate,"prol-kind",?).

      IF tmp-sig ne ? THEN
         FIND buf-templ WHERE buf-templ.op-templ EQ buf-loan.op-templ
                          AND buf-templ.op-kind  EQ tmp-sig
                                                     NO-LOCK NO-ERROR.
      in-surrogate = buf-templ.op-kind  + ',' + string(buf-templ.op-templ).
      tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).

      IF tmp-sig = ? THEN DO:
         in-surrogate =  buf-loan.op-kind + ',' + string(buf-loan.op-template) .
         tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).
      END.
      IF tmp-sig ne ? THEN
      DO i = 2 TO buf-loan.prolong:
         beg-date = Get-end-date(beg-date,tmp-sig).
      END.
   END.   

   RETURN beg-date.   
END FUNCTION.

/* "�室" �।�� � ��� �।���⥫쭮 ���᫭����� ��業⮢,
** � ��⮬ ᬥ�� ���. */
FUNCTION Get-Beg-Date-Obl RETURNS DATE (
        INPUT str-kau    AS CHAR,
        INPUT in-op-date AS DATE):

   DEF BUFFER buf-loan      FOR loan.
   DEF BUFFER buf-templ     FOR op-template.

   DEF VAR in-surrogate AS CHARACTER   NO-UNDO.
   DEF VAR str-ost      AS CHARACTER   NO-UNDO.
   DEF VAR tmp-str      AS CHARACTER   NO-UNDO.
   DEF VAR d-delay      AS INT64     NO-UNDO.
   DEF VAR i            AS INT64     NO-UNDO.
   DEF VAR beg-date     AS DATE        NO-UNDO.
   DEF VAR endd         AS DATE        NO-UNDO.

   DEF BUFFER xkau-entry FOR kau-entry.
   DEF BUFFER xop        FOR op.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
   
      FIND FIRST buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                            AND buf-loan.cont-code EQ ENTRY(2,str-kau)
         NO-LOCK NO-ERROR.
      IF NOT AVAIL buf-loan THEN 
         UNDO MAIN, LEAVE MAIN.

      RUN get-beg-date-prol(RECID(buf-loan),
                            in-op-date,
                            OUTPUT beg-date, 
                            OUTPUT endd).
      /* ���४�஢��, �᫨ ��� ��८�ଫ���� ����砥��� � ���� ��ਮ� ���᫥���
       ��業⮢ */
      RUN correct_date(INPUT RECID(buf-loan),
                       INPUT-OUTPUT beg-date) .
   
      FOR EACH loan-acct WHERE (    loan-acct.contract  EQ buf-loan.contract
                                AND loan-acct.cont-code EQ buf-loan.cont-code
                                AND loan-acct.acct-type EQ "loan-dps-int" 
                                AND loan-acct.since     LE in-op-date)
                            OR (    loan-acct.contract  EQ buf-loan.contract
                                AND loan-acct.cont-code EQ buf-loan.cont-code
                                AND loan-acct.acct-type EQ "loan-dps-ink" 
                                AND loan-acct.since     LE in-op-date)
         NO-LOCK:

         FIND LAST kau-entry WHERE kau-entry.acct     EQ loan-acct.acct 
                               AND kau-entry.currency EQ loan-acct.currency 
                               AND kau-entry.kau-id   EQ loan-acct.acct-type 
                               AND kau-entry.kau      BEGINS buf-loan.contract + ',' +
                                                                buf-loan.cont-code + ',' 
                               AND ENTRY(NUM-ENTRIES(kau-entry.kau), kau-entry.kau) EQ "����"
                               AND kau-entry.debit 
                               AND kau-entry.op-date  LE in-op-date                               
         NO-LOCK NO-ERROR.
         IF AVAIL kau-entry THEN 
         DO:
            FIND FIRST xkau-entry WHERE xkau-entry.kau-id   EQ kau-entry.kau-id 
                                    AND xkau-entry.kau      EQ kau-entry.kau 
                                    AND xkau-entry.op       EQ kau-entry.op 
                                    AND NOT xkau-entry.debit
               NO-LOCK NO-ERROR.
            IF NOT AVAIL xkau-entry THEN 
            DO:
               IF kau-entry.contract-date GT beg-date THEN 
                  beg-date = kau-entry.contract-date.            
            END.
         END.
      END.
   END.

   RETURN beg-date.
END FUNCTION.

/* "�室" �।�� � ��� �।���⥫쭮 ���᫭����� ��業⮢,
** � ��⮬ ᬥ�� ���. */
PROCEDURE Get-Beg-Obl-T:
   DEF INPUT PARAM str-kau    AS CHAR NO-UNDO.
   DEF INPUT PARAM in-op-date AS DATE NO-UNDO.
   DEF INPUT PARAM in-op-transaction AS INT64 NO-UNDO.
   DEF OUTPUT PARAM beg-date AS DATE NO-UNDO.

   DEF BUFFER buf-loan      FOR loan.
   DEF BUFFER buf-templ     FOR op-template.

   DEF VAR in-surrogate AS CHARACTER NO-UNDO.
   DEF VAR str-ost      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR tmp-str      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR d-delay      AS INT64 NO-UNDO.
   DEF VAR i            AS INT64 NO-UNDO.
   def var endd as date no-undo .

   def buffer xkau-entry for kau-entry.
   def buffer xop        for op.

   FIND buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                   AND buf-loan.cont-code EQ ENTRY(2,str-kau)
                                             NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-loan THEN RETURN ?.

   run get-beg-date-prol(recid(buf-loan),in-op-date,output beg-date, output endd) .
   for each loan-acct of buf-loan where
        loan-acct.acct-type eq 'loan-dps-int' and
        loan-acct.since     le in-op-date
    no-lock:

        for each kau-entry where
            kau-entry.acct     eq loan-acct.acct and
            kau-entry.currency eq loan-acct.currency and
            kau-entry.kau-id   eq 'loan-dps-int' and
            kau-entry.kau      eq buf-loan.contract + ',' + buf-loan.cont-code + ',' + '����' and
            kau-entry.debit and
            kau-entry.op-date  le in-op-date
        no-lock by kau-entry.op-date DESCENDING:

              find first xkau-entry where
                   xkau-entry.kau-id   eq kau-entry.kau-id and
                   xkau-entry.kau      eq kau-entry.kau and
         xkau-entry.op       eq kau-entry.op /* and
         xkau-entry.op-entry eq kau-entry.op-entry */ and
                   not xkau-entry.debit
              no-lock no-error.
              if not avail xkau-entry
              then do:

                 find xop of kau-entry
                 no-lock no-error.
                 if avail xop and
                     xop.contract-date gt beg-date AND
                     xop.op-transaction <> in-op-transaction
                 then do:
                    beg-date = xop.contract-date.
                    leave.
                 end.
              end.
        END.
    end.

  /* return beg-date. */

END PROCEDURE.

procedure correct_date.
   def input param rid as recid no-undo .
   def input-output param begdate as date no-undo .
   def var str-par as char no-undo .
   def buffer loan for loan .

   find loan where recid(loan) = rid   no-lock no-error.

  if loan.prolong = 0 or loan.open-date = begdate then return .
  if not avail loan then return .
  /*run get_last_param (recid(loan),begdate,begdate,'���������ਏ஫', output str-par) .*/
  ASSIGN str-par = GetCacheInit (loan.class-code, '���������ਏ஫').

  if str-par = ? or str-par ='?'  then return.
  if str-par = '��'  then begdate = begdate - 1.

end procedure .

FUNCTION Get-End-Date-Obl RETURNS DATE (INPUT str-kau    AS CHAR,
                                        INPUT in-op-date AS DATE):
   DEF VAR in-surrogate AS CHARACTER NO-UNDO.
   DEF VAR str-ost      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR tmp-str      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR d-delay      AS INT64 NO-UNDO.
   DEF VAR i            AS INT64 NO-UNDO.
   DEF VAR beg-date     AS DATE    NO-UNDO.

   DEF BUFFER buf-loan     FOR loan.
   DEF BUFFER buf-templ    FOR op-template.
   DEF BUFFER xkau-entry   for kau-entry.
   DEF BUFFER xop          for op.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      FIND FIRST buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                            AND buf-loan.cont-code EQ ENTRY(2,str-kau)
         NO-LOCK NO-ERROR.
      IF NOT AVAIL buf-loan THEN 
         UNDO MAIN, LEAVE MAIN.
      FIND FIRST buf-templ WHERE buf-templ.op-kind  EQ buf-loan.op-kind
                             AND buf-templ.op-templ EQ buf-loan.op-templ
         NO-LOCK NO-ERROR.
      IF NOT AVAIL buf-templ THEN
         UNDO MAIN, LEAVE MAIN.

      FIND LAST loan-acct WHERE (loan-acct.contract  EQ buf-loan.contract
                                 AND loan-acct.cont-code EQ buf-loan.cont-code
                                 AND loan-acct.acct-type EQ "loan-dps-int" 
                                 AND loan-acct.since     LE in-op-date)
                             OR (    loan-acct.contract  EQ buf-loan.contract
                                 AND loan-acct.cont-code EQ buf-loan.cont-code
                                 AND loan-acct.acct-type EQ "loan-dps-ink" 
                                 AND loan-acct.since     LE in-op-date)
         NO-LOCK NO-ERROR.
      IF AVAIL loan-acct THEN 
      DO:
         FIND LAST kau-entry WHERE kau-entry.acct     EQ loan-acct.acct 
                               AND kau-entry.currency EQ loan-acct.currency 
                               AND kau-entry.kau-id   EQ loan-acct.acct-type 
                               AND kau-entry.kau      BEGINS buf-loan.contract + ',' +
                                                                buf-loan.cont-code + ',' 
                               AND ENTRY(NUM-ENTRIES(kau-entry.kau), kau-entry.kau) EQ "����"
                               AND NOT kau-entry.debit 
                               AND kau-entry.op-date  LE in-op-date 
                               AND NOT CAN-FIND(FIRST xkau-entry WHERE xkau-entry.kau-id   EQ kau-entry.kau-id 
                                                                   AND xkau-entry.kau      EQ kau-entry.kau 
                                                                   AND xkau-entry.op       EQ kau-entry.op 
                                                                   AND xkau-entry.op-entry EQ kau-entry.op-entry  
                                                                   AND xkau-entry.debit) 
            NO-LOCK NO-ERROR.

         IF AVAIL kau-entry THEN
            beg-date = kau-entry.contract-date .   
      END.
   END.

   RETURN beg-date.
end function .

procedure Get_Last_Acct_Type .
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def var in-surrogate as char no-undo .
 def var beg-dat as date no-undo .
 def var tmp-sig as char no-undo .

 def buffer loan for loan .

 find loan where recid(loan) eq  rid no-lock no-error .
 if not avail loan then return '?' .
 if loan.end-date  ne ? then do :
   find last loan-acct of loan where loan-acct.acct-type  eq 'loan-dps-t'
                                 and loan-acct.since le end-date no-lock no-error .
   if not avail loan-acct then return '?' .
   else return  loan-acct.acct-type.
 end.
 if loan.prolong eq 0 then do :
  find last loan-acct of loan where loan-acct.acct-type  eq 'loan-dps-p'
                                and loan-acct.since le end-date no-lock no-error .
   if not avail loan-acct then return '?' .
   else return  loan-acct.acct-type .
 end.
 in-surrogate  =  loan.op-kind + ',' + string(loan.op-templ) .
 tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).
 if tmp-sig ne ? then do :
  beg-dat = Get-end-date(loan.open-date,tmp-sig).
  if beg-dat le beg-date then do :
   find last loan-acct of loan where loan-acct.acct-type  eq 'loan-dps-p'
    and loan-acct.since le end-date no-lock no-error .
   if not avail loan-acct then return '?' .
   else return  loan-acct.acct-type .
  end.
  else do :
   find last loan-acct of loan where loan-acct.acct-type  eq 'loan-dps-t'
    and loan-acct.since le end-date no-lock no-error .
   if not avail loan-acct then return '?' .
   else return  loan-acct.acct-type .
  end.
 end.
 else return '?' .
end procedure .

procedure Get_Start_Date.
 def input param rid as recid no-undo.
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def output param start_date as date no-undo .
 find loan-acct where recid(loan-acct) eq rid no-lock no-error .
 if not avail loan-acct then return '?' .
 if loan-acct.since gt beg-date then
   start_date = loan-acct.since .
 else start_date = beg-date .
end procedure .

/* ��।������ ����㠫쭮�� �᫠ �஫����権 ������  �� ��������� �᫮��� � ���. ४����� - Prolong - ��*/
procedure get_prol.
     def input param rid as recid no-undo .
     def input param beg-date as date no-undo .
     def output param nn as INT64 init 0 no-undo .

     def buffer loan for loan .

    find loan where recid(loan) eq rid no-lock no-error .
      if not avail loan then return '?' .
      for each loan-cond  {wh-t &f=loan-cond &c="/*"} and loan-cond.since ge loan.open-date and loan-cond.since le beg-date no-lock
       :
        if GetXattrValueEx("loan-cond",loan-cond.contract  + "," +  loan-cond.cont-code + "," +   string(year(loan-cond.since),  "9999") +
               string(month(loan-cond.since), "99") +
               string(day(loan-cond.since),   "99"),{&recprol},?) = '��' then
            accum loan-cond.since (count) .
      end.
      nn = accum count loan-cond.since.
     return .
end procedure .

/* ��।������ ���� �஫����樨(��८����� ������) */
procedure get_beg_date_prol.
    def input param rid as recid no-undo .
    def input param beg-date as date no-undo .
    def output param dat-prol as date init ? no-undo .

    def buffer loan for loan .

    find loan where recid(loan) eq rid no-lock no-error .
    if not avail loan or loan.contract eq ? or loan.cont-code eq ? then return '?' .
    dat-prol = loan.open-date .
    for each signs where signs.file-name = 'loan-cond' and signs.code = {&recprol} and signs.code-val = '��' and
     signs.surrogate begins  loan.contract  + "," +  loan.cont-code + ","  no-lock :
     if {&loan_cond_since}  > beg-date then return .
     if  {&loan_cond_since} > dat-prol then  dat-prol =   {&loan_cond_since} .
    end.
end procedure .

/* ��।������ �⭮�⥫쭮� ���� ������� ������, ��� ���� ᫥����� �᫮��� ������ */
PROCEDURE get_end_date_prol .
   DEFINE INPUT PARAMETER rid AS RECID NO-UNDO .
   DEFINE INPUT PARAMETER beg-date AS DATE NO-UNDO .
   DEFINE OUTPUT PARAMETER end-date-prol AS DATE INIT ? NO-UNDO .

   DEFINE VARIABLE dat-t AS DATE NO-UNDO .
   DEFINE BUFFER loan      FOR loan .
   DEFINE BUFFER loan-cond FOR loan-cond .
   DEFINE BUFFER b-loan    FOR loan.

   FIND loan WHERE RECID(loan) EQ rid NO-LOCK NO-ERROR .
   IF NOT AVAILABLE loan THEN RETURN '?' .

   end-date-prol = loan.end-date .
&IF DEFINED(oracle) &THEN 
   IF loan.cont-code EQ ? THEN
   DO: 
      FIND FIRST b-loan WHERE RECID(b-loan) EQ rid EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE b-loan THEN
      ASSIGN 
         b-loan.cont-code = ''.
   END.
&ENDIF
   FOR EACH signs WHERE
      signs.file-name = 'loan-cond' AND
      signs.code = {&recprol} AND
      signs.code-val = '��' AND
      signs.surrogate BEGINS  loan.contract  + "," +  loan.cont-code + ","
      NO-LOCK,
      /* ᭠砫� ��।��塞 �� ���. ४������   - ��� �।��饣� ����砭�� ������ */
      FIRST loan-cond {wh-t &f=loan-cond &c="/*"} AND loan-cond.since = {&loan_cond_since}
      NO-LOCK:
         IF loan-cond.since < beg-date THEN NEXT .
         dat-t =  DATE(GetXattrValueEx("loan-cond",loan-cond.contract  + "," +  loan-cond.cont-code + "," +   string(YEAR(loan-cond.since),  "9999") +
            string(MONTH(loan-cond.since), "99") +
            string(DAY(loan-cond.since),"99"),{&end-date},?))  .
    
         end-date-prol = IF dat-t <> ? AND (dat-t < end-date-prol OR end-date-prol = ?)
            THEN dat-t
            ELSE IF dat-t = ? AND end-date-prol <> ? AND loan-cond.since < end-date-prol
            THEN loan-cond.since
            ELSE IF end-date-prol  = ?
            THEN loan-cond.since
            ELSE end-date-prol.
   END.
&IF DEFINED(oracle) &THEN
   IF loan.cont-code EQ '' THEN loan.cont-code = ?.
&ENDIF 
END PROCEDURE.

/* ��।������ ����㠫쭮� ���� ������ � ������� ������ �� 䨪�樨 �஫����権 �᫮��ﬨ */
procedure get-beg-date-prol-cond.
       def input param rid as recid  no-undo .
       def input param beg-date as date no-undo .
       def output param loan-beg-date as date init ? no-undo .
       def output param loan-end-date as date init ? no-undo .

       def buffer loan for loan .


       find loan where recid(loan) eq rid no-lock no-error .
       if not avail loan then return '?' .

       assign
        loan-beg-date = loan.open-date
        loan-end-date = loan.end-date.
       run  get_beg_date_prol(rid,beg-date, output loan-beg-date) .
       run  get_end_date_prol(rid, loan-beg-date + 1,output loan-end-date).
end procedure .

/* ��।������ ���祭�� ४�����, �������� ⮫쪮 �� �࠭���樨 ������
   ��� ����� ������ - ������⢮ �஫����権, ��� �࠭���樨 �஫����樨,
   ��� �࠭���樨 �஫����樨 �� ���௠��� ������⢠ �஫����権 */
PROCEDURE get-param-const.
   DEFINE INPUT PARAMETER rid AS RECID NO-UNDO .
   DEFINE INPUT PARAMETER cod-par AS CHARACTER NO-UNDO .
   DEFINE OUTPUT PARAMETER strpar AS CHARACTER INIT  ? NO-UNDO .

   DEFINE BUFFER xattr     FOR xattr .
   DEFINE BUFFER xop-templ FOR op-templ .
   DEFINE BUFFER loan      FOR loan .

   FIND loan WHERE RECID(loan) EQ rid NO-LOCK NO-ERROR .
   IF NOT AVAILABLE loan THEN
      RETURN.

   FIND FIRST ttOpTemplSign WHERE
      ttOpTemplSign.Surr = loan.class-code + "," +
                           ( IF loan.op-kind = ? THEN "?" ELSE loan.op-kind) AND
      ttOpTemplSign.Code = cod-par
      NO-ERROR.

   IF AVAIL ttOpTemplSign THEN 
      strpar = ttOpTemplSign.Val.
   ELSE 
   DO:
      IF loan.op-kind  <> ?  THEN 
      DO :
         FOR EACH xop-templ WHERE xop-templ.op-kind = loan.op-kind NO-LOCK :
            IF GetXclassProgress(xop-templ.cr-class-code) = 'loan'
               THEN 
            DO:
               strpar = GetOpTemlXattr(xop-templ.op-kind + ',' + 
                  STRING(xop-templ.op-template),cod-par,?).
               LEAVE.
            END.
         END.
      END.
      IF strpar = ? THEN DO:
         RUN GetXAttr IN h_xclass (loan.class-code, cod-par, BUFFER xattr).
         IF AVAILABLE xattr AND xattr.initial <> '' AND xattr.initial <> ?
            THEN strpar = xattr.initial .
      END.

      CREATE ttOpTemplSign.
      ASSIGN
         ttOpTemplSign.Surr = loan.class-code + "," +
                              ( IF loan.op-kind = ? THEN "?" ELSE loan.op-kind)
         ttOpTemplSign.Code = cod-par
         ttOpTemplSign.Val = strpar
      .
   END.

END PROCEDURE .

/* ��।������ ���祭�� ४�����, �������� ⮫쪮 �� �࠭���樨 ������
   ��� ����� ������ - ������⢮ �஫����権, ��� �࠭���樨 �஫����樨,
   ��� �࠭���樨 �஫����樨 �� ���௠��� ������⢠ �஫����権 */
PROCEDURE get-param-const-by-kind.
   DEFINE INPUT PARAMETER iClassCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iOpKind    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER cod-par    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER strpar    AS CHARACTER INIT  ? NO-UNDO.

   DEFINE BUFFER xattr     FOR xattr .
   DEFINE BUFFER xop-templ FOR op-templ .

   FIND FIRST ttOpTemplSign WHERE
      ttOpTemplSign.Surr = iClassCode + "," +
                           ( IF iOpKind = ? THEN "?" ELSE iOpKind) AND
      ttOpTemplSign.Code = cod-par
      NO-ERROR.

   IF AVAIL ttOpTemplSign THEN 
      strpar = ttOpTemplSign.Val.
   ELSE 
   DO:
      IF iOpKind <> ? THEN DO:
         FOR EACH xop-templ WHERE xop-templ.op-kind = iOpKind NO-LOCK :
            IF GetXclassProgress(xop-templ.cr-class-code) = 'loan'
            THEN DO:
               strpar = GetOpTemlXattr(xop-templ.op-kind + ',' + 
                                       STRING(xop-templ.op-template),
                                       cod-par,?).
               LEAVE.
            END.
         END.
      END.
      IF strpar = ? THEN 
      DO:
         RUN GetXAttr IN h_xclass (iClassCode, cod-par, BUFFER xattr).
         IF AVAILABLE xattr AND xattr.initial <> '' AND xattr.initial <> ?
            THEN strpar = xattr.initial .
      END.

      CREATE ttOpTemplSign.
      ASSIGN
         ttOpTemplSign.Surr = iClassCode + "," +
                              ( IF iOpKind = ? THEN "?" ELSE iOpKind)
         ttOpTemplSign.Code = cod-par
         ttOpTemplSign.Val  = strpar
         .
   END.

END PROCEDURE .


/* ��।������ ��ࠬ��� ������� , �஫�����㥬��� ������, �� 䨪�樨 �஫����権 �᫮��ﬨ */
PROCEDURE get_last_param_cond.
   DEFINE INPUT PARAMETER rid AS RECID NO-UNDO .
   DEFINE INPUT PARAMETER beg-date AS DATE NO-UNDO .
   DEFINE INPUT PARAMETER end-date AS DATE NO-UNDO .
   DEFINE INPUT PARAMETER par1 AS CHARACTER NO-UNDO .
   DEFINE OUTPUT PARAMETER in-commi LIKE commi.commi INIT ? NO-UNDO .

   DEFINE VARIABLE nn           AS INT64     NO-UNDO .
   DEFINE VARIABLE beg-dat      AS DATE      NO-UNDO .
   DEFINE VARIABLE in-class     AS CHARACTER NO-UNDO .
   DEFINE VARIABLE out-op-kind  AS CHARACTER NO-UNDO .
   DEFINE VARIABLE fl-ok-prol   AS LOGICAL   NO-UNDO .

   DEFINE VARIABLE vTmpBegDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vTmpEndDate  AS DATE      NO-UNDO.
   DEFINE VARIABLE vEndProlDate AS DATE      NO-UNDO.

   DEFINE BUFFER xattr FOR xattr .
   DEFINE BUFFER loan  FOR loan .

   FIND loan WHERE RECID(loan) EQ rid NO-LOCK NO-ERROR .
   IF NOT AVAILABLE loan THEN RETURN .

   /*���砫� ������ �� �������饬 �᫮��� - ����⪠ ���室� �� ����� �孮������*/
   FIND LAST loan-cond WHERE loan-cond.contract = loan.contract AND
      loan-cond.cont-code = loan.cont-code AND
      loan-cond.since <= end-date NO-LOCK NO-ERROR.
   IF AVAILABLE loan-cond THEN 
   DO:
      in-commi = GetXattrValueEx('loan-cond',
         loan-cond.contract  + "," +  
         loan-cond.cont-code + "," +   
         STRING(YEAR(loan-cond.since),  "9999") +
         STRING(MONTH(loan-cond.since), "99") +
         STRING(DAY(loan-cond.since),"99"),
         par1,
         ?) .
      IF in-commi NE ? THEN RETURN.
      /*� ⥯��� ������ �� ����� - � � ������� ���饥, ����� �᫮�� � ��� 
      ���� �������஢�����*/
      in-commi = GetCacheInit(loan-cond.class-code, par1).
      IF in-commi NE "" AND in-commi NE ? THEN RETURN.
   END.
   /*�᫨ �� �᫮��� ��祣� ��� - �饬 �⠭����� ��ࠧ��*/
   /* ��।������ �������⢠ �஫����権 */
   RUN get_prol(rid,end-date,OUTPUT nn).
   RUN Chk_Limit_Per(end-date,rid,nn,OUTPUT fl-ok-prol) .
   
   /* ��஥���塞 ���� ������� ����� �� ��� �஫����権 */
   RUN get_end_date_prol(rid,loan.open-date,OUTPUT beg-dat) .

   /* fl-ok-prol ����� ������ "������⢮ �஫������ ���௠��" ࠭��,
      祬 ��� ����⢨⥫쭮 ���௠���� (�� �ந�室�� � ⮬ ��砥, �᫨
      �� ���室�� �१ ���� ��������). � �⮬ ��砥 ��। ����祭��� 
      ��ࠬ��� �㦭� �஢����, ���諠 �� end-date �ࠧ ���� �஫����樨
      ��� �� ���. */
   vEndProlDate = DATE(GetCacheInit(loan.class-code,"��������")) NO-ERROR.
   IF vEndProlDate <> ? THEN
      RUN get-beg-date-prol (rid, 
         vEndProlDate + 1, 
         OUTPUT vTmpBegDate, 
         OUTPUT vTmpEndDate).
   /* ������ �ਧ��� "end-date ���諠 �१ ���� �஫����樨" �룫廊�
       ᫥���騬 ��ࠧ��:
        vEndProlDate = ? OR vTmpEndDate = ? OR end-date > vTmpEndDate */
   
   IF nn > 0 /*and limitprol <> ?*/ AND  NOT fl-ok-prol AND 
      (vEndProlDate = ? OR vTmpEndDate = ? OR end-date > vTmpEndDate) THEN
      RUN get-param-const-by-kind (loan.class-code, loan.op-kind,
                                   'prol-kind-pen', OUTPUT out-op-kind).
   ELSE IF nn > 0  OR beg-dat LT end-date THEN 
      DO :
         RUN get-param-const-by-kind (loan.class-code, loan.op-kind,
                                      'prol-kind', OUTPUT out-op-kind).
         IF out-op-kind = ? THEN
            out-op-kind = loan.op-kind .
      END.
      ELSE
         out-op-kind = loan.op-kind .     
         
   RUN get-param-const-by-kind (loan.class-code, out-op-kind,
                                par1, OUTPUT in-commi).

END PROCEDURE .

/* ��।������ ��ࠬ��� ������� �� 蠡���� � ��⮬ �஫����樨 */

procedure Get_Last_Param .
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def input param par1 as char no-undo .
 def output param in-commi like commi.commi no-undo .
 def var in-surrogate as char no-undo .
 def var tmp-sig as char no-undo .
 def var beg-dat as date no-undo .
 def buffer yop-templ for op-templ .
 def var nameproc as char no-undo .
 def var params as char no-undo .
 DEF VAR vDate1 AS DATE NO-UNDO .
 DEF VAR vDate2 AS DATE NO-UNDO .
 def buffer loan for loan .

 find loan where recid(loan) eq rid no-lock no-error .
 if not avail loan then return '?' .

 run  GetCacheMethod IN THIS-PROCEDURE (loan.class-code,'getparam',
                                  output nameproc,
                                  output params).

 /* ᥩ�� ⮫쪮 ����७��� ��楤�� , �� ���� �ᤥ���� ��騩 �맮� */

 if nameproc <> ? then  do:
   run  value(nameproc)(rid,beg-date,end-date,par1,output in-commi) .
   return .
 end.

 in-surrogate  =  loan.op-kind + ',' + string(loan.op-templ) .
 tmp-sig = GetOpTemlXattr(in-surrogate,"dep-period",?).

 if tmp-sig = ? then
  tmp-sig = GetXattrValueEx('loan',loan.contract + ',' + loan.cont-code,"dep_period",?) .

 if tmp-sig ne ? then do :
  beg-dat = Get-end-date(loan.open-date,tmp-sig).
 end.
 RUN get-beg-date-prol(rid, beg-date, OUTPUT vDate1, OUTPUT vDate2) .

 IF vDate1 NE loan.open-date  OR (loan.end-date <> ? and
 loan.end-date lt end-date)
 THEN DO :
      tmp-sig = GetOpTemlXattr(in-surrogate,"prol-kind",?).
      if tmp-sig ne ? then do :
         find yop-templ where yop-templ.op-templ eq loan.op-templ and
                              yop-templ.op-kind eq tmp-sig  no-lock no-error .
         if avail yop-templ then  in-surrogate = yop-templ.op-kind  + ',' + string(yop-templ.op-templ) .
      end.
    end.

 tmp-sig = GetOpTemlXattr(in-surrogate,par1,?).
 if tmp-sig ne ? then in-commi  = tmp-sig.
 else in-commi =  '?' .

end procedure .
/* ��।������ �஡��� */
procedure get_last_delay.
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def output param out-day as decimal init 0 no-undo .
 def var in-inter as char no-undo .

   run Get_Last_Param(rid,beg-date,end-date,'delay',output in-inter) .
   out-day = decimal(in-inter) no-error .
   if error-status:error or out-day = ? then out-day = 0.
end procedure.

/* ���� ���� �����ᨨ */
procedure Get_Last_Commi.
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def output param in-commi like commi.commi no-undo .

 run Get_Last_Param(rid,beg-date,end-date,'commission',output in-commi) .
end procedure.

/* ���� �奬� ���᫥��� */
procedure Get_Last_Inter.
    define input param rid      as recid no-undo.   /* recid(loan) */
    define input param beg-date as date  no-undo.   /* ��� "�" */
    define input param end-date as date  no-undo.   /* ��� "��" */
    define output param in-inter as char no-undo.   /* ��� �����ᨨ */

    run Get_Last_Param (rid,
                        beg-date,
                        end-date,
                        'interest',
                        output in-inter).
end procedure.

/* ���� ���䣭�� �����ᨨ */
procedure Get_Last_Pen-Commi.
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def output param in-commi like commi.commi no-undo .

 run Get_Last_Param(rid,beg-date,end-date,'pen-commi',output in-commi) .

end procedure.

/* ���� ���䭮� �奬� ���᫥��� ��業⮢ */
procedure Get_Last_Pen_Inter.
 def input param rid as recid no-undo .
 def input param beg-date as date no-undo .
 def input param end-date as date no-undo .
 def output param in-inter as char no-undo .

 run Get_Last_Param(rid,beg-date,end-date,'pen-interest',output in-inter) .

end procedure.

/* ��।������ �������쭮�� ���⪠ �� ������ �� ����.*/
PROCEDURE get_last_min_ost .
  DEF INPUT  PARAM iRid      AS RECID NO-UNDO .
  DEF INPUT  PARAM iBeg-date AS DATE NO-UNDO .
  DEF INPUT  PARAM iEnd-date AS DATE NO-UNDO .
  DEF OUTPUT PARAM oIn-inter AS CHAR INIT ?  NO-UNDO .

  DEF VAR in-surr AS CHAR NO-UNDO .
  DEF BUFFER loan FOR loan .

  FIND FIRST loan WHERE RECID(loan) = iRid NO-LOCK  NO-ERROR.
  IF AVAILABLE loan THEN DO:
     oIn-inter = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"������",?).
     IF oIn-inter = ? THEN
        RUN Get_Last_Param(iRid,iBeg-date,iEnd-date,'������',OUTPUT oIn-inter).
  END.
END PROCEDURE.


/* ��।������ �������쭮�� ����� */
PROCEDURE get_last_min_vzn.
   DEF INPUT PARAM rid AS RECID NO-UNDO.
   DEF OUTPUT PARAM in-inter AS CHAR INIT ? NO-UNDO.
   DEF VAR in-surr AS CHAR NO-UNDO.
   DEF BUFFER loan FOR loan.

   /* ��६ �� ������쭮� ��� */
   FIND FIRST loan WHERE RECID(loan) = rid NO-LOCK NO-ERROR.
   IF AVAILABLE loan THEN 
   DO:
      in-inter = GetXattrValueEx("loan",
                                 loan.contract + "," + loan.cont-code,
                                 "������",
                                 ?).
      IF in-inter = ? THEN
         RUN Get_Last_Param(rid,
                            gend-date,
                            gend-date,
                            '������',
                            OUTPUT in-inter).
   END.
END PROCEDURE.

/* ��।������ ���ᨬ��쭮�� ���⪠ �� ������ 
PROCEDURE get_last_max_ost ��७�ᥭ� � chktake.i �맮� �� h_ltran*/

/* ����祭�� �������쭮� �㬬� ����� �� ����� � ��⮬ �ᯮ�짮����� 
** 㭨���ᠫ쭮�� �ࠢ�筨�� ����㬬� */
PROCEDURE Get_Min_Summ.
   DEF INPUT  PARAM iRID      AS RECID          NO-UNDO. /* RECID ������ */
   DEF INPUT  PARAM iBegDate  AS DATE           NO-UNDO. /* ��� ��砫� ��ਮ�� */
   DEF INPUT  PARAM iEndDate  AS DATE           NO-UNDO. /* ��� ���� ��ਮ�� */
   DEF OUTPUT PARAM oSumm     AS DECIMAL INIT ? NO-UNDO. /* ����㬬� */

   DEF VAR vMinSummClass AS CHARACTER   NO-UNDO. /* ����� � 㭨����. �ࠢ. */
   DEF VAR vInter        AS CHARACTER   NO-UNDO. /* ������ �������쭮�� ���������� */
   DEF VAR vSummOst      AS DECIMAL     NO-UNDO. /* ���⮪ �� ������ */
   DEF VAR vCount        AS INTEGER     NO-UNDO. /* ���-�� ��������� indicate */
   DEF VAR vCurr         AS CHARACTER   NO-UNDO. /* ����� ������� */

   FIND FIRST loan WHERE RECID(loan) EQ iRID NO-LOCK NO-ERROR.
   IF AVAIL loan THEN 
   vCurr = loan.currency.
   IF vCurr EQ '?' OR vCurr EQ '' THEN 
      vCurr = FGetSetting("�����悠�",?,"810"). 
   
   /* ��।������ ���祭�� ����� � �ࠢ�筨�� ����㬬� (indicate) */
   RUN Get_Last_Param (iRID,
                       iBegDate,
                       iEndDate,
                       "����ጨ��㬬�",
                       OUTPUT vMinSummClass).
   IF    vMinSummClass EQ ?
      OR vMinsummClass EQ "?" THEN   
   DO:   
      /* �᫨ ���祭�� ��� - �饬 ���祭�� �� ४����� �� �࠭���樨 */
      RUN Get_Last_Param (iRID, 
                          iBegDate, 
                          iEndDate, 
                          "����㬬�", 
                          OUTPUT vInter).            
      IF    vInter EQ ? 
         OR vInter EQ "?" THEN
         vInter = "0".
   END.
   ELSE
   DO:      
      /* ��।������ ���⪠ �� ������ */
      RUN get-summ-beg-ost IN THIS-PROCEDURE  (iRID,
                                           iBegDate,
                                           iEndDate,
                                           OUTPUT vSummOst).
      /* ����祭�� ���祭�� �ࠢ�筨�� ����㬬� */
      vCount = GetRefCrVal ("����㬬�",
                            "�㬬�",
                            iEndDate,
                            vMinSummClass + "," + vCurr,
                            TEMP-TABLE ttIndicate:HANDLE).    
     
      /* �饬 �।� ��� � �������� ���室�饩 �㬬�� � ����砥� �� ���祭�� */         
      FIND LAST ttIndicate WHERE ttIndicate.fCrCode EQ "�㬬�"
                             AND ttIndicate.fDec    LE vSummOst
         NO-LOCK NO-ERROR.
      IF AVAIL ttIndicate THEN
         vInter = STRING(GetRefVal("����㬬�",
                                   iEndDate,
                                   vMinSummClass + "," + vCurr + "," + STRING(ttIndicate.fDec))).
   END.   
   oSumm = DEC(vInter) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      oSumm = ?.
END PROCEDURE.


PROCEDURE Get_Last_limit_summ:
DEFINE INPUT  PARAMETER rid      AS RECID   NO-UNDO.         /* RECID ������ */
DEFINE INPUT  PARAMETER beg-date AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER end-date AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER summ     AS DECIMAL NO-UNDO.         /* �㬬� ��� �஢�ન */
DEFINE OUTPUT PARAMETER fl       AS INT64 NO-UNDO INIT 0.  /* �襭�� */
   
DEFINE VAR in-inter AS CHAR NO-UNDO.
   RUN Get_Min_Summ IN THIS-PROCEDURE (rid, beg-date, end-date, OUTPUT in-inter).
   IF in-inter = ? OR in-inter = "?" THEN RETURN.
   IF DEC(in-inter) <= summ          THEN RETURN.
   RUN Fill-SysMes IN h_tmess ("","","0", "�㬬� ���������� ����� �������쭮�� ��⠭��������� ���祭��: " + in-inter ).
   fl = -1.
END PROCEDURE.

/*��楤�� ��� ����᪠ Get_Last_limit_summ � �஢�મ� ��� (��� 0231654)*/
PROCEDURE Get_Last_limit_summ_chk_acct:
DEFINE INPUT  PARAMETER acct     AS CHAR    NO-UNDO.    
DEFINE INPUT  PARAMETER rid      AS RECID   NO-UNDO.         /* RECID ������ */
DEFINE INPUT  PARAMETER beg-date AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER end-date AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER summ     AS DECIMAL NO-UNDO.         /* �㬬� ��� �஢�ન */  
DEFINE OUTPUT PARAMETER fl       AS INT64   NO-UNDO INIT 0.

DEFINE VARIABLE vAcct AS CHAR NO-UNDO.

ASSIGN
   vAcct = FGetSetting("��᪠�猨��㬬","",?).
   
IF     vAcct NE ?
   AND  NOT CAN-DO(vAcct, acct) THEN
    RUN Get_Last_limit_summ IN THIS-PROCEDURE (rid,beg-date,end-date,summ,output fl).
   
END PROCEDURE.    


/*��।������ �㬬� ���⪠ ⥪�饣� ��������� ���*/
PROCEDURE get-summ-ost.
   DEFINE INPUT  PARAMETER rid      AS RECID   NO-UNDO.  /* RecId ������ */
   DEFINE INPUT  PARAMETER beg-date AS DATE    NO-UNDO .
   DEFINE INPUT  PARAMETER end-date AS DATE    NO-UNDO .
   DEFINE OUTPUT PARAMETER oResult  AS DECIMAL NO-UNDO.

   DEFINE VARIABLE vBegDate AS DATE      NO-UNDO. /* ����㠫쭠� ��� ��砫� �������*/
   DEFINE VARIABLE vEndDate AS DATE      NO-UNDO. /* ����㠫쭠� ��� ����砭�� ������� */
   DEFINE VARIABLE vKauSurr AS CHARACTER NO-UNDO. /* ���ண�� ��� */
   DEFINE VARIABLE vMetod   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpLog  AS LOG NO-UNDO.
   DEFINE VARIABLE vProlong AS LOGICAL   NO-UNDO. /* �ਧ��� �஫����樨 */

   FIND FIRST loan WHERE
         RECID( loan ) = rid NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan THEN
      RETURN.

    /*��।������ ����㠫쭮� ���� ����砭�� ������*/
   RUN get-beg-date-prol ( rid,
      beg-date,
      OUTPUT vBegDate,
      OUTPUT vEndDate ).

   RUN Get_Last_Param  (rid,
                        beg-date,
                        beg-date,
                        '��挨����',
                        OUTPUT vMetod).

   ASSIGN
      vTmpLog  = (vMetod = "��")
      vProlong = IF GetSysConf("RunProl") EQ "YES" THEN YES
                                                   ELSE NO
      .

   FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract 
                         AND loan-acct.cont-code EQ loan.cont-code 
                         AND loan-acct.acct-type EQ ( IF vEndDate eq ? THEN "loan-dps-p" ELSE "loan-dps-t" ) 
                         AND ( IF vProlong THEN loan-acct.since LT beg-date ELSE loan-acct.since LE beg-date)
                         NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan-acct THEN
      oResult = 0.
   ELSE
   DO:
      vKauSurr = IF vEndDate = ? THEN
         loan.contract + ',' + loan.cont-code + ',' + "��₪��"
         ELSE
         loan.contract + ',' + loan.cont-code + ',' + "��₪��".

      RUN kau-pos.p( loan-acct.acct,
         loan.currency,
         gend-date,
         gend-date,
         "�",
         vKauSurr ).
      oResult = ABSOLUTE( IF loan-acct.currency EQ '' THEN ksh-bal ELSE ksh-val ).
      IF vTmpLog  THEN DO: /*���뢠�� ��騩 ���⮪, ������ %%. ���� ⮫쪮 ��� �����*/
          RUN kau-pos.p( loan-acct.acct,
             loan.currency,
             gend-date,
             gend-date,
             "�",
             loan.contract + ',' + loan.cont-code + ',' + "�����1").
          oResult =  oResult  +  ABSOLUTE( IF loan-acct.currency EQ '' THEN ksh-bal ELSE ksh-val ).

      END.
   END.
END PROCEDURE.

/*��।������ �㬬� ��ࢮ��砫쭮�� ���⪠ (��� �஫����樨) ������*/
PROCEDURE get-summ-beg-ost.
   DEFINE INPUT  PARAMETER rid      AS RECID   NO-UNDO.  /* RecId ������ */
   DEFINE INPUT  PARAMETER beg-date AS DATE    NO-UNDO .
   DEFINE INPUT  PARAMETER end-date AS DATE    NO-UNDO .
   DEFINE OUTPUT PARAMETER oResult  AS DECIMAL NO-UNDO.

   DEFINE VARIABLE vBegDate AS DATE      NO-UNDO. /* ����㠫쭠� ��� ��砫� �������*/
   DEFINE VARIABLE vEndDate AS DATE      NO-UNDO. /* ����㠫쭠� ��� ����砭�� ������� */
   DEFINE VARIABLE vKauSurr AS CHARACTER NO-UNDO. /* ���ண�� ��� */
   DEFINE VARIABLE vMetod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpLog AS LOG NO-UNDO.

   FIND FIRST loan WHERE
         RECID( loan ) = rid NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan THEN
      RETURN.

    /*��।������ ����㠫쭮� ���� ��砫� ࠡ��� ������*/
   RUN get-beg-date-prol ( rid,
      beg-date,
      OUTPUT vBegDate,
      OUTPUT vEndDate ).

   RUN Get_Last_Param  (rid,
                        beg-date,
                        beg-date,
                        '��挨����',
                        OUTPUT vMetod).

   vTmpLog = (vMetod = "��").
   

   FIND LAST  loan-acct WHERE
         loan-acct.contract eq loan.contract AND loan-acct.cont-code eq loan.cont-code AND
         loan-acct.acct-type eq ( IF vEndDate eq ? THEN "loan-dps-p" ELSE "loan-dps-t" ) and 
         loan-acct.since LE vBegDate
         NO-LOCK NO-ERROR.
   IF NOT AVAILABLE loan-acct THEN
      oResult = 0.
   ELSE
   DO:
      vKauSurr = IF vEndDate = ? THEN
         loan.contract + ',' + loan.cont-code + ',' + "��₪��"
         ELSE
         loan.contract + ',' + loan.cont-code + ',' + "��₪��".

      RUN kau-pos.p( loan-acct.acct,
         loan.currency,
         vBegDate,
         vBegDate,
         gop-status,
         vKauSurr ).

         oResult = ABSOLUTE( IF loan-acct.currency EQ '' THEN ksh-bal ELSE ksh-val ).

         IF vTmpLog  THEN DO: /*���뢠�� ��騩 ���⮪, ������ %%. ���� ⮫쪮 ��� �����*/
          RUN kau-pos.p( loan-acct.acct,
             loan.currency,
             vBegDate,
             vBegDate,
             "�",
             loan.contract + ',' + loan.cont-code + ',' + "�����1").
          oResult =  oResult  +  ABSOLUTE( IF loan-acct.currency EQ '' THEN ksh-bal ELSE ksh-val ).     
         END.
   END.
END PROCEDURE.


/* �஢�ઠ �㬬� �� ��������� ���⮪ */
procedure chek_summa.

    def input  param rid      as recid          no-undo. /* RecId ������ */
    def input  param beg-date as date           no-undo.
    def input  param end-date as date           no-undo.
    def input  param summ like op-entry.amt-rub no-undo. /* �㬬� ��� �஢�ન */
    def output param fl   as INT64 init 0       no-undo. /* �襭�� */

    def var in-inter as char no-undo.
    DEF VAR vResult AS DEC NO-UNDO.
    DEF VAR vErr AS LOG NO-UNDO INIT YES.
    DEF VAR vErr-ch AS CHAR NO-UNDO INIT YES.
    DEF VAR vMess   AS CHAR NO-UNDO. 

    run get_last_min_ost (rid,
                          beg-date,
                          end-date,
                          OUTPUT in-inter).
    if in-inter = ? OR in-inter = "?"
        then return.

    run Get_Last_Param(rid,beg-date,end-date,'�����������',output vErr-ch).
    vErr = (vErr-ch = "��").

    run get-summ-ost (rid,
                     beg-date,
                     end-date,
                     OUTPUT vResult).

    if dec(in-inter) > vResult + summ THEN DO:
       ASSIGN
          vMess = "�� ������ " + DelFilFromAcct(loan.cont-code) + 
                  " c㬬� ������ " +  TRIM(STRING((vResult + summ),">>>,>>>,>>9.99")) + 
                  " ����� �������쭮�� ��⠭��������� ���祭�� " + in-inter + ".".    
       IF vErr THEN DO:
          RUN Fill-SysMes ("", "", "-1", "�� ������ " + DelFilFromAcct(loan.cont-code) + 
                                         " c㬬� ������ " +  TRIM(STRING((vResult + summ),">>>,>>>,>>9.99")) + 
                                         " ����� �������쭮�� ��⠭��������� ���祭�� " + in-inter + ".").
           ASSIGN
              fl = -1.
           RETURN vMess.        
       END.
       ELSE
       DO:
          RUN Fill-SysMes ("", "", "4", "�� ������ " + DelFilFromAcct(loan.cont-code) + 
                                         " c㬬� ������ " +  TRIM(STRING((vResult + summ),">>>,>>>,>>9.99")) + 
                                         " ����� �������쭮�� ��⠭��������� ���祭�� " + in-inter + ".~n�த������?").
          IF pick-value = "yes" THEN
             RETURN.
          ELSE
          DO:
             ASSIGN 
                fl = -1.        
             RETURN vMess.
          END.              
       END.
    END.
end procedure.

/* �஢�ઠ ��ࢮ��砫쭮�� ����� */
PROCEDURE chek_summa_vzn.
   DEF INPUT  PARAM rid      AS   RECID                   NO-UNDO. /* RecId ������ */
   DEF INPUT  PARAM summ     LIKE op-entry.amt-rub        NO-UNDO. /* �㬬� ��� �஢�ન */
   DEF OUTPUT PARAM oMess    AS   CHAR                    NO-UNDO. /* �訡�� */

   DEF VAR in-inter AS CHAR NO-UNDO.

   RUN get_last_min_vzn (rid,
                         OUTPUT in-inter).
   IF in-inter = ? OR in-inter = "?" THEN 
      RETURN.

    IF DEC(in-inter) > summ THEN     
       oMess = "�㬬� ������ ������ ���� �� ����� " + in-inter +
               "! ������ ���४��� �㬬�.~n".    
    END PROCEDURE.

/*���ᨬ��쭠� �㬬� ����������.*/
PROCEDURE chek_summa_max_vzn.
   DEF INPUT  PARAM iRid      AS   RECID                   NO-UNDO. /* RecId ������ */
   DEF INPUT  PARAM iSumm     LIKE op-entry.amt-rub        NO-UNDO. /* �㬬� ��� �஢�ન */
   DEF OUTPUT PARAM oFl       AS   INT64              INIT 0 NO-UNDO. /* �襭�� */
   DEF OUTPUT PARAM oMess     AS   CHAR             INITIAL ? NO-UNDO. /* ᮮ�饭�� �� �訡�� */

   DEF VAR mInter AS CHAR NO-UNDO.
   DEF VAR mSumm  AS DEC  NO-UNDO. 
   DEF VAR i AS INT64 NO-UNDO INIT 0.


   FIND FIRST loan WHERE RECID(loan) = iRid
        NO-LOCK NO-ERROR.

  /*��� ��ࢮ�� ����� �� ������ ��ࠡ��뢠����*/
  FOR EACH loan-transaction OF loan NO-LOCK:
      i = i + 1.
  END.
  IF i < 2  THEN RETURN.

   IF NOT AVAILABLE loan THEN
       RETURN. 

   RUN Get_Last_Param(iRid,
                      gend-date,
                      gend-date,
                      "����㬬�",
                      OUTPUT mInter).

   mSumm = DEC(mInter) NO-ERROR.

   /*�� ��⠭������ ��� ��⠭������ ���ࠢ��쭮*/
   IF ERROR-STATUS:ERROR OR mSumm = ? THEN 
       RETURN.

   IF iSumm  > mSumm THEN DO: 
        oFl = -1.
        oMess = "�㬬� ���������� ����� ���������� ���祭��: " + mInter + ". ������ ����������.".
   END.
   

   RETURN.
END PROCEDURE.


/* �஢�ઠ �㬬� �� ���ᨬ���� ���⮪ 
PROCEDURE chek_summa_max ��७�ᥭ� � chktake.i �맮� �� h_ltran*/

/* ��।������ ����㠫쭮� ���� ������ ������ � ��⮬ �஫����権 */
procedure get-beg-date-prol .
  def input param rid1 as recid  no-undo .
  def input param beg-date as date no-undo .
  def output param loan-beg-date as date no-undo .
  def output param loan-end-date as date no-undo .

  def var in-surrogate as char no-undo .
  def var cod-par as char no-undo .
  def var cod-par1 as char no-undo .
  def var i as INT64 no-undo .
  def var cur-date as date no-undo .
  def var nameproc as char no-undo .
  def var params as char no-undo .
  def buffer f_loan for loan .
  def buffer buf-templ for op-templ .
  
  assign
   loan-beg-date = ?
   loan-end-date = ?.

  find f_loan where recid(f_loan) eq rid1 no-lock no-error .
  if not avail f_loan then return.
   run  GetCacheMethod IN THIS-PROCEDURE (f_loan.class-code,'getvdate',
                                   output nameproc,
                                   output params).
  /* ᥩ�� ⮫쪮 ����७��� ��楤�� , �� ���� �ᤥ���� ��騩 �맮� */
  if nameproc <> ? then  do:
     run  value(nameproc)(rid1,beg-date,output loan-beg-date, output loan-end-date) .
     return .
  end.
  assign
   loan-beg-date = f_loan.open-date
   loan-end-date = f_loan.end-date.

  /* ��������㥬  ��ࠬ��� � 蠡���� ������� */
  in-surrogate  =  f_loan.op-kind + ',' + string(f_loan.op-templ) .
  cod-par =  GetOpTemlXattr(in-surrogate, 'dep-period', ?).
  if cod-par eq ? or f_loan.prolong eq 0 then return .

  loan-end-date = Get-end-date(loan-beg-date,cod-par).
  if loan-end-date gt beg-date then return .

  loan-beg-date = loan-end-date .
  cod-par1 = cod-par .
  cod-par = GetOpTemlXattr(in-surrogate,'prol-kind', ?).
  if cod-par ne ? then do :
      in-surrogate = cod-par + ',' + string(f_loan.op-templ) .
      cod-par =  GetOpTemlXattr(in-surrogate, 'dep-period', ?).
      if cod-par eq ? then do :
        loan-end-date = f_loan.end-date .
        return .
      end.
  end.
  else cod-par = cod-par1 .

  cur-date = loan-beg-date.
  loan-beg-date =  Get-end-date(loan-beg-date,cod-par).
  DO i = 2 TO f_loan.prolong while loan-beg-date lt beg-date :
         cur-date = loan-beg-date .
         loan-beg-date = Get-end-date(loan-beg-date,cod-par).
  end.
  assign loan-end-date = loan-beg-date
         loan-beg-date = cur-date .

end procedure .

/* ��।������ ����㠫쭮� ���� ������ ������ � ��⮬ �஫����権 �� ������ loan */
PROCEDURE get-beg-date-prol-buf .
   DEFINE PARAMETER BUFFER f_loan        FOR loan.
   DEFINE INPUT  PARAMETER beg-date      AS DATE NO-UNDO .
   DEFINE OUTPUT PARAMETER loan-beg-date AS DATE NO-UNDO .
   DEFINE OUTPUT PARAMETER loan-end-date AS DATE NO-UNDO .

   DEFINE VARIABLE in-surrogate AS CHARACTER NO-UNDO .
   DEFINE VARIABLE cod-par      AS CHARACTER NO-UNDO .
   DEFINE VARIABLE cod-par1     AS CHARACTER NO-UNDO .
   DEFINE VARIABLE i            AS INT64     NO-UNDO .
   DEFINE VARIABLE cur-date     AS DATE      NO-UNDO .
   DEFINE VARIABLE nameproc     AS CHARACTER NO-UNDO .
   DEFINE VARIABLE params       AS CHARACTER NO-UNDO .
   DEFINE BUFFER buf-templ FOR op-templ .
  
   ASSIGN
      loan-beg-date = ?
      loan-end-date = ?.

   RUN GetCacheMethod IN THIS-PROCEDURE (f_loan.class-code,'getvdate',
      OUTPUT nameproc,
      OUTPUT params).
   /* ᥩ�� ⮫쪮 ����७��� ��楤�� , �� ���� �ᤥ���� ��騩 �맮� */
   IF nameproc <> ? THEN  
   DO:
      RUN VALUE(nameproc) (RECID(f_loan), beg-date,
                           OUTPUT loan-beg-date, OUTPUT loan-end-date).
      RETURN.
   END.
   ASSIGN
      loan-beg-date = f_loan.open-date
      loan-end-date = f_loan.end-date.

   /* ��������㥬  ��ࠬ��� � 蠡���� ������� */
   in-surrogate  =  f_loan.op-kind + ',' + string(f_loan.op-templ) .
   cod-par = GetOpTemlXattr(in-surrogate, 'dep-period', ?).
   IF cod-par EQ ? OR f_loan.prolong EQ 0 THEN RETURN .

   loan-end-date = Get-end-date(loan-beg-date,cod-par).
   IF loan-end-date GT beg-date THEN RETURN .

   loan-beg-date = loan-end-date .
   cod-par1 = cod-par .
   cod-par = GetOpTemlXattr(in-surrogate, 'prol-kind', ?).
   IF cod-par NE ? THEN 
   DO:
      in-surrogate = cod-par + ',' + string(f_loan.op-templ) .
      cod-par = GetOpTemlXattr(in-surrogate, 'dep-period', ?).
      IF cod-par EQ ? THEN 
      DO:
         loan-end-date = f_loan.end-date .
         RETURN .
      END.
   END.
   ELSE cod-par = cod-par1 .

   cur-date = loan-beg-date.
   loan-beg-date =  Get-end-date(loan-beg-date,cod-par).
   DO i = 2 TO f_loan.prolong WHILE loan-beg-date LT beg-date :
      cur-date = loan-beg-date .
      loan-beg-date = Get-end-date(loan-beg-date,cod-par).
   END.
   ASSIGN 
      loan-end-date = loan-beg-date
      loan-beg-date = cur-date .

END PROCEDURE.


FUNCTION KAU-Get-Beg-Date RETURNS DATE (INPUT str-kau    AS CHAR,
                                    INPUT in-op-date AS DATE):

   DEF BUFFER loan-acct-int FOR loan-acct.
   DEF BUFFER loan-acct-p   FOR loan-acct.
   DEF BUFFER loan-acct-t   FOR loan-acct.
   DEF BUFFER buf-loan-acct FOR loan-acct.
   DEF BUFFER buf-loan      FOR loan.
   DEF BUFFER buf-lcond     FOR loan-cond.
   DEF BUFFER buf-op-entry  FOR op-entry.
   DEF BUFFER buf-op        FOR op.
   DEF BUFFER buf-templ     FOR op-template.

   DEF VAR in-surrogate AS CHARACTER NO-UNDO.
   DEF VAR str-ost      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR tmp-str      AS CHARACTER INITIAL "" NO-UNDO.
   DEF VAR d-delay      AS INT64 NO-UNDO.
   DEF VAR i            AS INT64 NO-UNDO.
   DEF VAR beg-date     AS DATE NO-UNDO.
   def var endd as date no-undo .
   def var fl as logical init no no-undo .
   def var a-type like loan-acct.acct-type no-undo .
   FIND buf-loan WHERE buf-loan.contract  EQ ENTRY(1,str-kau)
                   AND buf-loan.cont-code EQ ENTRY(2,str-kau)
                                             NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-loan THEN RETURN ?.
   FIND FIRST buf-lcond WHERE buf-lcond.contract  EQ buf-loan.contract
                          AND buf-lcond.cont-code EQ buf-loan.cont-code
                                                       NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-lcond THEN RETURN ?.
   FIND buf-templ WHERE buf-templ.op-kind  EQ buf-loan.op-kind
                    AND buf-templ.op-templ EQ buf-loan.op-templ
                                           NO-LOCK NO-ERROR.
   IF NOT AVAIL buf-templ THEN RETURN ?.

   FIND LAST loan-acct-int OF buf-loan WHERE loan-acct-int.acct-type EQ "loan-dps-int"
                                         AND loan-acct-int.since     Le in-op-date
                                                                      NO-LOCK NO-ERROR.
   IF buf-loan.end-date EQ ? THEN DO:
      run  Get_Last_Acct_Type(recid(buf-loan),in-op-date,in-op-date) .
      a-type = {&RETURN_VALUE}.
      if {&RETURN_VALUE} eq '?' then  return ? .
      if a-type eq 'loan-dps-p' then do :
      FIND LAST loan-acct-p OF buf-loan WHERE loan-acct-p.acct-type EQ "loan-dps-p"
                                          AND loan-acct-p.since     LE in-op-date
                                                                   NO-LOCK NO-ERROR.
       IF NOT AVAIL loan-acct-p THEN RETURN ?.
       str-ost = "�����".
      end.
      if a-type  =  'loan-dps-t' then do :
                FIND LAST loan-acct-t OF buf-loan WHERE loan-acct-t.acct-type EQ "loan-dps-t"
                                          AND loan-acct-t.since     LE in-op-date
                                                                   NO-LOCK NO-ERROR.
                IF NOT AVAIL loan-acct-t THEN RETURN ?.
           str-ost = "�����1".
          FIND LAST loan-acct-p OF buf-loan WHERE loan-acct-p.acct-type EQ "loan-dps-p"
                                          AND loan-acct-p.since     LE in-op-date
                                                                   NO-LOCK NO-ERROR.
         IF AVAIL loan-acct-p THEN str-ost = str-ost + ",�����".

      end.
   END.
   ELSE DO:
      FIND LAST loan-acct-p OF buf-loan WHERE loan-acct-p.acct-type EQ "loan-dps-p"
                                          AND loan-acct-p.since     LE in-op-date
                                                                   NO-LOCK NO-ERROR.
      FIND LAST loan-acct-t OF buf-loan WHERE loan-acct-t.acct-type EQ "loan-dps-t"
                                          AND loan-acct-t.since     LE in-op-date
                                                                   NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-acct-t THEN RETURN ?.
      str-ost = "�����1".
      IF AVAIL loan-acct-p THEN str-ost = str-ost + ",�����".
   END.
   IF str-ost = "" THEN RETURN ?.
   run get-beg-date-prol(recid(buf-loan),in-op-date,output beg-date,output endd) .
   IF beg-date > in-op-date THEN RETURN ?.
   tmp-str  = buf-loan.contract + "," +  buf-loan.cont-code + "," + "����".
   for each  loan-acct-int OF buf-loan WHERE loan-acct-int.acct-type EQ "loan-dps-int"
                                         AND loan-acct-int.since     Le in-op-date
                                                                      NO-LOCK :
    FIND LAST kau-entry WHERE kau-entry.kau      EQ tmp-str
                            AND kau-entry.kau-id   EQ "loan-dps-int"
                            AND NOT kau-entry.debit
                            and kau-entry.acct eq loan-acct-int.acct
                            and kau-entry.currency eq loan-acct-int.currency
                            AND kau-entry.op-date  Le in-op-date
                                                             NO-LOCK NO-ERROR.


       IF AVAIL kau-entry THEN
         FIND LAST buf-op-entry OF kau-entry NO-LOCK NO-ERROR.
       IF AVAIL buf-op-entry THEN DO:
         beg-date = if beg-date lt kau-entry.contract-date and
                       kau-entry.contract-date le in-op-date 
                    then kau-entry.contract-date 
                    else beg-date.
       END.
     end.
  /*!! END. */
   DO i = 1 TO NUM-ENTRIES(str-ost):
      IF ENTRY(i,str-ost) EQ "�����" THEN
         FIND buf-loan-acct WHERE RECID(buf-loan-acct) EQ RECID(loan-acct-p)
                                                            NO-LOCK NO-ERROR.
      ELSE
         FIND buf-loan-acct WHERE RECID(buf-loan-acct) EQ RECID(loan-acct-t)
                                                            NO-LOCK NO-ERROR.

      tmp-str  = buf-loan.contract + "," +
                 buf-loan.cont-code + "," + ENTRY(i,str-ost).
      FOR EACH kau-entry WHERE kau-entry.kau      EQ tmp-str
                           AND kau-entry.kau-id   EQ buf-loan-acct.acct-type
                           AND NOT kau-entry.debit
                           AND kau-entry.acct     EQ buf-loan-acct.acct
                           AND kau-entry.currency EQ buf-loan-acct.currency
                           AND kau-entry.op-date  GE beg-date
                           AND kau-entry.op-date  LE in-op-date
                                        NO-LOCK
                                        BREAK BY kau-entry.op-date DESCENDING:
       FIND LAST buf-op-entry OF kau-entry NO-LOCK NO-ERROR.
       FIND buf-op OF kau-entry NO-LOCK NO-ERROR.
       if kau-entry.kau eq str-kau then fl = yes .
       IF avail buf-op-entry and
          (buf-op-entry.kau-db EQ "" OR buf-op-entry.kau-db EQ ?) THEN LEAVE.
       IF LAST(kau-entry.op-date) THEN RELEASE buf-op-entry.
      END.
      IF AVAIL buf-op-entry THEN DO:
       FIND buf-op OF buf-op-entry NO-LOCK NO-ERROR.
       beg-date = if beg-date lt buf-op.contract-date and
                     buf-op.contract-date le in-op-date then buf-op.contract-date else beg-date. .
      END.
   END.
    beg-date = Get-Beg-Signs(str-kau,beg-date,in-op-date).
   IF (beg-date EQ in-op-date and
       not fl) or beg-date ne in-op-date  THEN RETURN beg-date.
   else  return ? .
 END FUNCTION.
/* ���� ��᫥����� ���᫥��� �� ��� ���᫥���� ��業⮢ �� �᫮��� ��९ਢ離� ��⮢ */
 procedure get_beg_date_per .
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  def output param beg-date as date init ? no-undo .
  def var vEnd-date as date   no-undo. 
  def var rid2      as recid  no-undo.
  DEF VAR vNachSr   AS CHAR   NO-UNDO.
  def buffer xkau-entry for kau-entry .
  def buffer xloan-acct for loan-acct .
  def buffer loan for loan .

  find first loan where recid(loan) = rid1 no-lock no-error .
  if not avail loan then return .

  RUN get_last_param  (RECID(loan),
                       in-op-date,
                       in-op-date,
                       '���ᫍ�ப',
                       OUTPUT vNachSr).
  IF vNachSr EQ "��" THEN
  DO:
     IF GetCacheSetting("���ᫎ�஫", ?, ?) = "��" THEN
     DO:
        RUN get-beg-date-prol(rid1,
                              in-op-date,
                              OUTPUT beg-date,
                              OUTPUT vEnd-date). 
        IF beg-date GT loan.open-date THEN
           beg-date = beg-date - 1.
     END.
     ELSE
        beg-date = loan.open-date .

     RUN Get-Beg-Signs-T(loan.contract + ',' + loan.cont-code,
                         beg-date,
                         in-op-date,
                         ?,
                         OUTPUT beg-date).
  END.
  ELSE
  DO:
     IF GetCacheSetting("���ᫎ�஫", ?, ?) = "��" THEN
     DO:
        RUN get-beg-date-prol(rid1,
                              in-op-date,
                              OUTPUT beg-date,
                              OUTPUT vEnd-date). 
        IF beg-date GT loan.open-date THEN
           beg-date = beg-date - 1.
     END.
     ELSE beg-date = loan.open-date .
      rid2 = 0.
      for each loan-acct of loan where loan-acct.acct-type = 'loan-dps-int' and
                                       loan-acct.since le in-op-date no-lock:
        find last kau-entry where kau-entry.acct = loan-acct.acct and
                                  kau-entry.currency = loan-acct.currency and
                                  kau-entry.kau = loan.contract + ',' + loan.cont-code + ','  + '����'
            and not  kau-entry.debit and kau-entry.op-date le in-op-date no-lock no-error .
        /* ����஫� ��७�� � ��� �� ��� */
        release xkau-entry .
        if rid2 <> 0 and avail kau-entry then do :
          find xloan-acct where recid(xloan-acct) = rid2 no-lock no-error .
          find last xkau-entry  where xkau-entry.acct = xloan-acct.acct and
                                      xkau-entry.currency = xloan-acct.currency and
                                      xkau-entry.kau = loan.contract + ',' + loan.cont-code + ','  + '����'
            and  xkau-entry.debit and xkau-entry.op-date le in-op-date and xkau-entry.op = kau-entry.op          /*and xkau-entry.op-entry = kau-entry.op-entry */  no-lock no-error .
         end.
         if avail kau-entry and not avail xkau-entry then do :
           if kau-entry.contract-date > beg-date and kau-entry.contract-date <= in-op-date then 
              beg-date = kau-entry.contract-date .
         end.
         rid2 = recid(loan-acct) .
       end .
  END.
END.
/* ���� ��᫥����� ���᫥��� �����।�⢥��� �� ��� ������  �� �᫮��� ������ ��᪮�쪨� ��⮢ */

 procedure get_beg_date_kl_per .
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  def input-output param beg-date as date no-undo .

   def buffer xkau-entry for kau-entry .
   def buffer loan for loan .

   find first loan where recid(loan) = rid1 no-lock no-error .
   if not avail loan then return .

   for each loan-acct of loan where loan-acct.acct-type = 'loan-dps-t' and loan-acct.since le in-op-date no-lock :
     release xkau-entry .
     release op .
     for each kau-entry where kau-entry.acct = loan-acct.acct and
                              kau-entry.currency = loan-acct.currency and
                              kau-entry.kau = loan.contract + ',' + loan.cont-code + ','  + '�����1'
          and not  kau-entry.debit and kau-entry.op-date le in-op-date and
          kau-entry.op-date GE beg-date
          and kau-entry.op-date ge loan-acct.since           no-lock  :
        release xkau-entry .
      find FIRST op-entry of kau-entry no-lock no-error .
      IF AVAIL op-entry AND op-entry.acct-db NE ? THEN                     
         find last xkau-entry  where xkau-entry.debit
               and xkau-entry.op-entry = kau-entry.op-entry
               and xkau-entry.op = kau-entry.op
               and xkau-entry.kau
               begins loan.contract + ',' + loan.cont-code + ',' no-lock no-error.
      ELSE
         find last xkau-entry  where xkau-entry.debit
               and xkau-entry.op = kau-entry.op
               and xkau-entry.kau
               begins loan.contract + ',' + loan.cont-code + ',' no-lock no-error.
     if not avail xkau-entry   then   do:
       if kau-entry.contract-date > beg-date and kau-entry.contract-date <= in-op-date then 
          beg-date = kau-entry.contract-date .
     end.
   end.
  end.
  for each loan-acct of loan where loan-acct.acct-type = 'loan-dps-p' and loan-acct.since le in-op-date no-lock :
     release xkau-entry .
     release op .
    for each kau-entry where kau-entry.acct = loan-acct.acct and
                             kau-entry.currency = loan-acct.currency and
                             kau-entry.kau = loan.contract + ',' + loan.cont-code + ','  + '�����' and
                             not  kau-entry.debit and
                             kau-entry.op-date le in-op-date and
                             kau-entry.op-date GE beg-date  no-lock  :
       release xkau-entry.
       find FIRST op-entry of kau-entry no-lock no-error .
       IF AVAIL op-entry AND op-entry.acct-db NE ? THEN                     
          find last xkau-entry  where xkau-entry.debit
                and xkau-entry.op-entry = kau-entry.op-entry
                and xkau-entry.op = kau-entry.op
                and xkau-entry.kau
                begins loan.contract + ',' + loan.cont-code + ',' no-lock no-error.
       ELSE
          find last xkau-entry  where xkau-entry.debit
                and xkau-entry.op = kau-entry.op
                and xkau-entry.kau
                begins loan.contract + ',' + loan.cont-code + ',' no-lock no-error.
       if not avail xkau-entry   then  do:
       if kau-entry.contract-date > beg-date and kau-entry.contract-date <= in-op-date then 
          beg-date = kau-entry.contract-date .
     end.
   end.
  end.
 end procedure .

  /* ���� ��᫥����� ���᫥��� �����।�⢥��� �� ��� ������
    �� �᫮��� ������ ��᪮�쪨� ��⮢,
    � ��⮬ �����஢���� ⥪�饩 ����⠫���樨 */

 procedure get_beg_kper .
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  DEF INPUT PARAM in-op-transaction AS INT64 NO-UNDO.
  DEF INPUT-OUTPUT param beg-date as date no-undo .

   def buffer xkau-entry for kau-entry .
   DEF BUFFER b-loan-acct FOR loan-acct.
   def buffer loan for loan .

   DEF VAR vType-capital AS CHAR NO-UNDO.

   find first loan where recid(loan) = rid1 no-lock no-error .
   if not avail loan then return .

   {get_kper.i &cur-acct-type = 'loan-dps-t'
               &cur-nach = '�����1'
               &cur-transaction = in-op-transaction
                }

   {get_kper.i &cur-acct-type = 'loan-dps-p'
               &cur-nach = '�����'
               &cur-transaction = in-op-transaction
               }


   vType-capital = GetCacheSetting("����⠫������",?,?).
   /*�������� ����⠤����� ������� �� �१ ��� ������, ����⠥��� ��।���� �� ���� ��� �室  � ��� loan-dps-int*/
   IF vType-capital  NE ? THEN DO:
       FOR EACH loan-acct OF loan WHERE 
                loan-acct.acct-type = 'loan-dps-int' AND 
                loan-acct.since LE in-op-date NO-LOCK:
          RELEASE op .
          RELEASE op-entry.

          FOR EACH kau-entry WHERE kau-entry.acct = loan-acct.acct AND 
                                   kau-entry.currency = loan-acct.currency AND 
                                   kau-entry.kau = loan.contract + ',' + loan.cont-code + ',' + "����" AND 
                                   kau-entry.debit AND 
                                   kau-entry.op-date LE in-op-date AND 
                                   kau-entry.op-date GE beg-date AND 
                                   kau-entry.op-date GE loan-acct.since NO-LOCK:
             /*�饬 �஢����, �᫨ ��� ������� �� �㡯஢����� � १���� ����樨, ࠡ���� �� �㤥�*/
             FIND FIRST op-entry OF kau-entry NO-LOCK NO-ERROR.
             IF AVAILABLE op-entry THEN DO:
                 /*��� �� ����� ���� ᯨᠭ�� � 47411 �.� �ਢ易� �� ������ � ஫� ��� 㪠���� � ��*/
                 FIND FIRST b-loan-acct OF loan WHERE b-loan-acct.acct = op-entry.acct-cr
                                              AND     b-loan-acct.currency  = loan.currency NO-LOCK NO-ERROR.
                 IF AVAILABLE b-loan-acct AND CAN-DO(vType-capital, b-loan-acct.acct-type) THEN DO:
                     FIND op OF kau-entry NO-LOCK NO-ERROR.
                     IF AVAIL op AND op.contract-date > beg-date AND
                     op.op-transaction <> in-op-transaction
                     THEN beg-date = op.contract-date.
                 END.
             END.
          END.
       END.
   END.
   
 end procedure .

/* ��楤�� ���᪠ ��᫥���� ���� ���᫥��� ��業⮢ �� ���� */


procedure get-beg-date-all.
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  def output param beg-date as date init ? no-undo .

  def buffer f_loan for loan .
  run  get_beg_date_per(rid1,in-op-date,output beg-date) .
  if beg-date = ? then return .
  run get_beg_date_kl_per(rid1,in-op-date,input-output beg-date).
  find f_loan where recid(f_loan) = rid1 no-lock no-error.
  beg-date = Get-Beg-Signs(f_loan.contract + ',' + f_loan.cont-code,beg-date,in-op-date).
end.

/* ��楤�� ���᪠ ��᫥���� ���� ���᫥��� ��業⮢ �� ����
   � ��⮬ �����஢���� ⥪�饩 ����⠫���樨
   (��� ����������) */

procedure get-date-nach.
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  DEF INPUT PARAM in-op-transaction AS INT64 NO-UNDO.
  def output param beg-date as date init ? no-undo .

  def buffer f_loan for loan .
     /* ���� ��᫥���� ���� १��. ��業⮢ */

  run  get_beg_date_per(rid1,01/01/3000,output beg-date) .

  IF beg-date = ? OR beg-date >= in-op-date THEN RETURN.

     /* ���� ��᫥���� ���� ����⠫���樨 ��業⮢ */

  run get_beg_kper(rid1,01/01/3000, in-op-transaction, INPUT-OUTPUT beg-date).


  IF beg-date >= in-op-date THEN RETURN.

  find f_loan where recid(f_loan) = rid1 no-lock no-error.
  RUN Get-Beg-Signs-T(f_loan.contract + ',' + f_loan.cont-code,
                      beg-date,
                      in-op-date,
                      in-op-transaction,
                      OUTPUT beg-date).

end.

/*  �஢�ઠ ������⢨� ���᫥��� � ��⮩  > ���� �믮������ ����樨 */

procedure chek_no_nach .
  def input param rid1 as recid no-undo .
  def input param in-op-date as date no-undo .
  def var beg-date as date no-undo .
  def buffer loan for loan .

   find first loan where recid(loan) = rid1 no-lock no-error .
   if not avail loan then return .

   run  get_beg_date_per(recid(loan),01/01/3000,output beg-date).
   if beg-date > in-op-date then return "error" .          
   run   get_beg_date_kl_per(recid(loan),01/01/3000,input-output beg-date) .
   if beg-date > in-op-date then return "error" .
   return.
end.

/* ᯨ᮪ ��⮢ ��� ���᫥��� %% */
PROCEDURE get_acct:
   DEF INPUT PARAM  iRid      AS RECID NO-UNDO. /* RECID loan */
   DEF INPUT PARAM  iBegDate  AS DATE  NO-UNDO.
   DEF INPUT PARAM  iEndDate  AS DATE  NO-UNDO.
   DEF OUTPUT PARAM oList     AS CHAR  NO-UNDO.

   DEF VAR vAcctType AS CHAR NO-UNDO INIT 'loan-dps-t,loan-dps-p'.
   DEF VAR vI        AS INT64  NO-UNDO.
   DEF VAR mList     AS CHAR NO-UNDO.
   DEF VAR vNumType  AS INT64  NO-UNDO.

   DEFINE BUFFER loan FOR loan.

   MAIN:
   DO ON ERROR  UNDO MAIN, LEAVE MAIN
      ON ENDKEY UNDO MAIN, LEAVE MAIN: 

      FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN LEAVE MAIN.

       IF CAN-FIND (FIRST loan-trans WHERE OF loan) THEN /* ��� ������� � ����������� */
       DO:
          vAcctType = "loan-dps-ts,loan-dps-tsk,loan-dps-p".
          vNumType  = 3.
       END.
       ELSE
          vNumType  = 2.

      DO vI = 1 TO NUM-ENTRIES(vAcctType):
         IF     vI            EQ vNumType
            AND loan.end-date NE ? THEN
            LEAVE MAIN.

         RUN GetAcctList IN h_dps (iRid, iBegDate, iEndDate, ENTRY(vI, vAcctType), OUTPUT mList).
         IF {assigned mList} THEN
         DO:
            {additem.i oList mList}
         END.
      END.
   END.

   RETURN.
END PROCEDURE.

/*���� �������� ��⮢ �� ��ਮ�*/
PROCEDURE GET_acct_per:
   DEFINE INPUT  PARAMETER iloan      AS RECID NO-UNDO.
   DEFINE INPUT  PARAMETER iDateStart AS DATE  NO-UNDO.
   DEFINE INPUT  PARAMETER iDateEnd   AS DATE  NO-UNDO.
   DEFINE OUTPUT PARAMETER oStr-acct  AS CHAR  NO-UNDO INIT ?.

   DEFINE VARIABLE vDatePrSt     AS DATE        NO-UNDO.
   DEFINE VARIABLE vDatePrEnd    AS DATE        NO-UNDO.
   DEFINE VARIABLE vDats         AS DATE        NO-UNDO.
   DEFINE VARIABLE vAcct         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vstr-acct     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcct-p       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vNumEntr      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParAcct      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParAcctSince AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn        AS INTEGER     NO-UNDO.
   DEFINE VARIABLE vInn       AS INTEGER     NO-UNDO.
   DEFINE VARIABLE vflgt      AS LOGICAL     NO-UNDO.



   DEFINE BUFFER buf_loan-acct FOR loan-acct.
   DEFINE BUFFER loan FOR loan.

   b-MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      FIND FIRST loan WHERE RECID(loan)  EQ iLoan
         NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         UNDO b-Main, LEAVE b-Main.

      RUN get_acct IN THIS-PROCEDURE (RECID(loan),
                                      iDateStart,
                                      iDateEnd,
                                      OUTPUT ostr-acct).
      ostr-acct = TRIM (ostr-acct, ","). 
      DDx_:
      DO vIn = 1 TO NUM-ENTRIES(ostr-acct) BY 2:
         vAcct = ENTRY(vIn,ostr-acct).
         FIND FIRST buf_loan-acct WHERE buf_loan-acct.contract  EQ loan.contract 
                                    AND buf_loan-acct.cont-code EQ loan.cont-code 
                                    AND buf_loan-acct.acct      EQ vAcct
                                    AND buf_loan-acct.currency  EQ loan.currency 
            NO-LOCK NO-ERROR.

         IF NOT AVAIL buf_loan-acct
            OR buf_loan-acct.acct-type EQ "loan-dps-p" THEN /*��� loan-dps-p ⮫쪮 �ਢ易� � ������. �� �㤥� ࠡ���� ⮫쪮
            �� �஫����樨 �� �� ����ॡ������*/
         DO:
            /*�஢�ਬ, �� � ����訢���� �२�� ���� �஫������ �� �������. (�⭮�⥫쭮 ���� ��ਨ���)*/
            RUN get-beg-date-prol IN THIS-PROCEDURE (RECID(loan),
                                                     iDateEnd,
                                                     OUTPUT vDatePrSt,
                                                     OUTPUT vDatePrEnd).
            IF vDatePrEnd NE ? THEN
            DO:
               /*�஫����樨 �� �� ���. ��� loan-dpd-p �� �� �㦥�.*/
               /*������ ���*/
               ENTRY(vIn,ostr-acct)     = "".
               ENTRY(vIn + 1,ostr-acct) = "".
               ostr-acct = TRIM (REPLACE (ostr-acct, ",,", ","), ",").
               oStr-acct = TRIM (REPLACE (ostr-acct, ",,", ","), ",").
            END.
            ELSE
            DO:
               /*�஫������ �� �� ����. ��� loan-dps-p ��� ���� �ਢ易� � ������ �� �⮣� ������*/
               /*��।���� ����⢨⥫�� ��� loan-dps-p*/
               RUN GetBaseAcct IN h_dps (loan.contract,
                                         loan.cont-code,
                                         vDatePrSt,
                                         OUTPUT vAcct-p).
               ENTRY(vIn,ostr-acct)     = "".
               ENTRY(vIn + 1,ostr-acct) = "".
               ostr-acct = TRIM (REPLACE (ostr-acct, ",,", ","), ",").
               ostr-acct = TRIM (REPLACE (ostr-acct, ",,", ","), ",").
               vParAcct      = ENTRY (1,vAcct-p).
               vParAcctSince = STRING (vDatePrSt).
               {additem.i ostr-acct vParAcct}
               {additem.i ostr-acct vParAcctSince}
            END.
         END.
      END. /* vIn = 1 */
      ostr-acct = TRIM (ostr-acct, ","). /* �� ��直� ��砩 :) */

      /*�  ��ப� oStr-acct ����� ���� ��� 㦥 �� �������騥 �� ��������� ��ਮ�(��� �������騥  ��᫥ ��������� ��ਮ��). ���६ ��*/
      DO vInn = 0 TO NUM-ENTRIES (oStr-acct) - 1 BY 2: /*� ���� �� ��⠬*/
         /* ���砫� ������ ��� ����� ���� ����⢮���� ����� ��������� ��ਮ�� */
         vDats = DATE (ENTRY ( NUM-ENTRIES (oStr-acct) - vInn, oStr-acct)).
         IF vDats GT iDateEnd THEN
         DO:
            vParAcct      = STRING (NUM-ENTRIES (oStr-acct) - vInn).
            vParAcctSince = STRING (NUM-ENTRIES (oStr-acct) - vInn - 1).
            {additem.i vNumEntr vParAcct}
            {additem.i vNumEntr vParAcctSince}
         END.
         IF vDats LT iDateStart THEN /*�� ��� 㦥 �� �������� � ������� ��ਮ�*/
         DO:
            IF vflgt THEN
            DO:
               vParAcct      = STRING (NUM-ENTRIES (oStr-acct) - vInn).
               vParAcctSince = STRING (NUM-ENTRIES (oStr-acct) - vInn - 1).
               {additem.i vNumEntr vParAcct}
               {additem.i vNumEntr vParAcctSince}
            END.
            vflgt = YES.
         END.
         vNumEntr = TRIM (vNumEntr, ",").

      END.
      DO vInn = 1 TO NUM-ENTRIES (vNumEntr):
         ENTRY(INT (ENTRY(vInn,vNumEntr)),oStr-acct)  = "".
      END.
      DO vInn = 1 TO NUM-ENTRIES (vNumEntr):
         oStr-acct = TRIM (REPLACE (oStr-acct, ",,", ","), ",").
      END.
   END. /*b-main*/
END PROCEDURE.

/* �����頥� ᯨ᮪ ⨯� "���,��� ���" �� ஫� � ���ࢠ�� ��� */
PROCEDURE get_acct_role:
DEF INPUT PARAM  iRid      AS RECID NO-UNDO. /* RECID loan */
DEF INPUT PARAM  iBegDate  AS DATE  NO-UNDO.
DEF INPUT PARAM  iEndDate  AS DATE  NO-UNDO.
DEF INPUT PARAM  iAcctType AS CHAR  NO-UNDO.
DEF OUTPUT PARAM oList     AS CHAR  NO-UNDO.

   RUN GetAcctList IN h_dps (iRid, iBegDate, iEndDate, iAcctType, OUTPUT oList).

   RETURN.
END PROCEDURE.

/* �����頥� �ਧ��� Yes, �᫨ �࠭����� ������� */
PROCEDURE CHK_CLOSE:

    DEFINE INPUT  PARAM ipOpKindChar AS CHAR    NO-UNDO.
    DEFINE INPUT  PARAM ipClassChar  AS CHAR    NO-UNDO.
    DEFINE INPUT  PARAM ipStatusChar AS CHAR    NO-UNDO.
    DEFINE OUTPUT PARAM opCloseLog   AS LOGICAL NO-UNDO.

    DEFINE VAR vSurrChar AS CHAR NO-UNDO.
    DEFINE VAR in-class  AS CHAR NO-UNDO .
    DEFINE VAR tmp-sig   AS CHAR NO-UNDO.

    DEFINE BUFFER bop-kind  FOR op-kind.
    DEFINE BUFFER bop-templ FOR op-templ.

    FIND bop-kind WHERE
        bop-kind.op-kind EQ ipOpKindChar
    NO-LOCK NO-ERROR.
    IF NOT AVAIL bop-kind
    THEN RETURN.


    FOR EACH bop-templ WHERE bop-templ.op-kind = bop-kind.op-kind
       NO-LOCK :
         in-class = bop-templ.cr-class-code.
         RUN ffparent IN h_xclass  (INPUT-OUTPUT in-class, BUFFER class).
         IF in-class = 'loan' THEN LEAVE .
    END.
    IF NOT AVAIL bop-templ THEN RETURN.

    vSurrChar = bop-templ.op-kind + ',' + STRING(bop-templ.op-templ).
    tmp-sig = GetOpTemlXattr(vSurrChar,"loan-status",?).
    IF tmp-sig NE ? AND
       tmp-sig GT ipStatusChar
    THEN opCloseLog = YES.

END PROCEDURE.

/**
  mitr:
    �㭪�� - ��� ������ ������� (��� ������)-
    �.�. ���, � ���ன ������ ����� ��ᯮ�짮������ �।�⢠��
    �� ��筮� ������ ��᫥ ���祭�� �ப� �������
*/

PROCEDURE deposit-return-date:
  DEF INPUT PARAM rid AS RECID.
  DEF INPUT PARAM in-date AS DATE.
  DEF INPUT PARAM in-dep-period AS CHAR.
  DEF OUTPUT PARAM out-val AS DATE.

  DEF VAR d1 AS DATE NO-UNDO.
  DEF VAR d2 AS DATE NO-UNDO.
  DEF VAR str1 AS CHAR NO-UNDO .
  DEF BUFFER loan FOR loan.

  FIND loan WHERE RECID(loan) = rid NO-LOCK NO-ERROR.
  RUN  get-beg-date-prol (rid, in-date, OUTPUT d1, OUTPUT d2). /* ��।������ ����㠫쭮� ���� ������ � ��⮬ �஫����権 */

  RUN correct_date (rid, INPUT-OUTPUT d1).
  RUN IGet-end-date (rid, d1, in-dep-period, OUTPUT out-val).
  /* ���������� ������������� ��� ������ ������� �� 700 ����� ������ � ���
*/
  IF loan.open-date <> d1 THEN DO :
    out-val = out-val  +  IF NOT old_dps(buffer loan,output str1) THEN 1 ELSE 0 .
  END.
END PROCEDURE.


/**
  mitr:
    �㭪�� - ����� �ப� ������� (��� ������)
*/

PROCEDURE deposit-end-date:
   DEFINE INPUT PARAMETER rid AS RECID NO-UNDO.
   DEFINE INPUT PARAMETER in-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER in-dep-period AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER out-val AS DATE NO-UNDO.

   DEFINE BUFFER dps-loan FOR loan.
   DEFINE VARIABLE dep_per  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mNewTech AS CHARACTER NO-UNDO.
   DEFINE VARIABLE str1     AS CHARACTER NO-UNDO .

   RUN deposit-return-date(rid, in-date, in-dep-period, OUTPUT out-val).

   FIND dps-loan WHERE RECID(dps-loan) = rid NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dps-loan THEN RETURN.

   /*��� ������ ������� �ப ����砭�� � �ப ������ ᮢ������*/
   RUN get-param-const-by-kind (dps-loan.class-code, dps-loan.op-kind,
      "������", OUTPUT mNewTech).
   IF mNewTech = "��" OR
      Old_DPS(buffer dps-loan,OUTPUT str1)   THEN RETURN.
   /*��� ��� ������ -1 ���� */
   ELSE out-val = out-val - 1.

END PROCEDURE.

/**
  mitr:
    �㭪�� - ��� ��砫� �ப� (��� ������)
*/
PROCEDURE deposit-start-date :
  DEF INPUT PARAM rid AS RECID.
  DEF INPUT PARAM in-date AS DATE.
  DEF OUTPUT PARAM out-val AS DATE.

  DEF VAR d1 AS DATE NO-UNDO.
  DEF VAR d2 AS DATE NO-UNDO.

  RUN get-beg-date-prol (rid, in-date, OUTPUT d1, OUTPUT d2). /* ��।������ ����㠫쭮� ���� ������ � ��⮬ �஫����権 */
  RUN correct_date (rid, INPUT-OUTPUT d1).
  out-val = d1 + 1.
END PROCEDURE.

/**
  mitr:
    �㭪�� - �த����⥫쭮��� ������ � ����
*/
PROCEDURE deposit-dep-period:
  DEF INPUT PARAM rid AS RECID.
  DEF INPUT PARAM in-date AS DATE.
  DEF INPUT PARAM in-dep-period AS CHAR.
  DEF OUTPUT PARAM out-val AS INT64.

  DEF VAR d1 AS DATE NO-UNDO.
  DEF VAR d2 AS DATE NO-UNDO.
  RUN deposit-start-date (rid, in-date, OUTPUT d1).
  RUN deposit-end-date   (rid, in-date, in-dep-period, OUTPUT d2).
  out-val = d2 - d1 + 1.
END PROCEDURE.

/* �㭪�� �த����⥫쭮��� ������ � ���� */

 PROCEDURE depos-dep-period:
    DEF INPUT PARAM rid AS RECID no-undo.
    DEF INPUT PARAM in-date AS DATE no-undo .
    DEF OUTPUT PARAM out-val AS INT64 no-undo.

    DEF VAR d1 AS DATE NO-UNDO.
    DEF VAR d2 AS DATE NO-UNDO.
    DEF VAR fl1 AS LOGICAL NO-UNDO .

    DEF BUFFER dps-loan FOR loan.
    DEF VAR dep_per AS CHAR NO-UNDO.
    FIND dps-loan WHERE RECID(dps-loan) = rid NO-LOCK NO-ERROR.
    IF NOT AVAIL dps-loan THEN RETURN.


   IF new_dps_prol(BUFFER dps-loan, OUTPUT dep_per, OUTPUT fl1)
   THEN DO:
     RUN deposit-start-date (rid, in-date, OUTPUT d1).
     run get_end_date_prol (rid,d1 + 1,output d2) .
     out-val = d2 - d1 .
     IF Old_Dps(BUFFER dps-loan,OUTPUT dep_per) THEN out-val = out-val + 1.
     RETURN.
    END.
    RUN get-beg-date-prol(RECID(dps-loan),in-date,OUTPUT d1,OUTPUT d2) .
    IF d2 <> ?
    THEN  out-val = d2 - d1 .
    ELSE  out-val = 0.
 END PROCEDURE.

 PROCEDURE get_srok_vklad:
    DEF INPUT PARAM rid AS RECID.
    DEF INPUT PARAMETER in-date AS DATE.
    DEF OUTPUT PARAM out-val AS CHAR INIT "1".
    DEF VAR d AS INT64 NO-UNDO.
    DEF VAR str-par AS CHAR NO-UNDO.
    DEF VAR dep_per AS CHAR NO-UNDO INIT "".
    DEF VAR data-year AS DATE NO-UNDO.
    DEF VAR mFromOpen AS CHAR NO-UNDO. /*�ப � ��� ������ ������, � �०��� ।��樨 = 1 ����*/  
    DEF VAR d1 AS DATE NO-UNDO.
    DEF VAR d2 AS DATE NO-UNDO.

    /*����� �� ⮣� ⨯�*/
    FIND FIRST loan WHERE RECID(loan) = rid NO-LOCK.
    /*����㠫쭠� ��� ��砫� ������*/
    RUN  get-beg-date-prol (rid, in-date, OUTPUT d1, OUTPUT d2). /* ��।������ ����㠫쭮� ���� ������ � ��⮬ �஫����権 */   

    RUN get_last_param  (recid(loan),in-date,in-date,'�����惮�', output str-par) .
    RUN get_last_param  (recid(loan),in-date,in-date,'�����愭', output mFromOpen).
    IF mFromOpen = ? OR  mFromOpen = "?" OR 
      str-par = ? OR str-par = '?' THEN 
   RETURN.
    /*����� �� ⮩ �த����⥫쭮��*/
    dep_per = GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "dep_period",?).
    IF LOOKUP(dep_per,str-par,";") = 0 THEN RETURN.
    /*��� �஫����樨 ����� �室��� ��� �� �室��� � �ப ������*/
    RUN correct_date (rid, INPUT-OUTPUT d1).
    /*�த����⥫쭮��� �� ��砫�*/
    data-year = Get-end-date(d1,mFromOpen).
    /*����� �஫���� ����� ��।�������� ��ਮ��!*/
    IF data-year > in-date THEN out-val = "0". /*�� ����ॡ������*/
    ELSE out-val = "-1".                       /*1/2 �⠢�� ��筮�� ������*/
 END PROCEDURE.

 PROCEDURE get-beg-date-prol-vtb.
    def input param rid1 as recid  no-undo .
    def input param beg-date as date no-undo .
    def output param loan-beg-date as date no-undo .
    def output param loan-end-date as date no-undo .
    DEF VAR str-par AS CHAR NO-UNDO.
    FIND FIRST loan WHERE recid(loan) = rid1 NO-LOCK NO-ERROR.
    IF AVAILABLE loan THEN DO:
       /*� ��砥 ����� ������� ��� ������� �� ����700 � �஫����஢����� ��᫥ ����700
       ���⮪ ������ �� ���� ��᫥���� �஫����樨 �� ����700*/
       run Get_Date_Comm  (rid1, beg-date, OUTPUT loan-beg-date, OUTPUT loan-end-date) .
       RUN get_last_param  (rid1,beg-date,beg-date,'�㬬����', output str-par).
       /*��८�।������ ����㠫쭮� ���� ��砫� ������ ��� ���*/
       IF str-par = "��" THEN loan-beg-date = loan.open-date.
    END.
 END PROCEDURE.

/*
  ���� ��᫥����� ���᫥��� �� ��� ���᫥���� ��業⮢ �� �᫮��� ��९ਢ離� ��⮢
  ��।���� get_beg_date_per, �.� � ��� �����쭮 �ਢ�� ������ ࠡ���, ��� �� ����, �����
  ��� � ࠡ�⠥� �ࠢ��쭮, ����� ����, �� ������ ��� ࠡ�⠥� �����쭮 ����*/
PROCEDURE get_beg_date_per1.
  DEF INPUT  PARAM rid1       AS RECID       NO-UNDO.
  DEF INPUT  PARAM in-op-date AS DATE        NO-UNDO.
  DEF OUTPUT PARAM beg-date   AS DATE INIT ? NO-UNDO.

  DEF VAR rid2 AS RECID NO-UNDO.

  DEF BUFFER xkau-entry FOR kau-entry.
  DEF BUFFER xloan-acct FOR loan-acct.
  DEF BUFFER loan       FOR loan.


   FIND FIRST loan WHERE RECID(loan) = rid1 NO-LOCK NO-ERROR .
   IF NOT avail loan THEN RETURN .
   beg-date = loan.open-date .

   FOR EACH loan-acct OF loan WHERE
            loan-acct.acct-type EQ 'loan-dps-int'
        AND loan-acct.since     le in-op-date NO-LOCK :
     RELEASE xkau-entry .
     RELEASE op .
     FOR EACH kau-entry WHERE
              kau-entry.acct     = loan-acct.acct
          AND kau-entry.currency = loan-acct.currency
          AND kau-entry.kau      = loan.contract + ',' + loan.cont-code + ','  + '����'
          AND NOT kau-entry.debit
          AND kau-entry.op-date <= in-op-date
          AND kau-entry.op-date >  beg-date
          AND kau-entry.op-date >= loan-acct.since NO-LOCK:
        RELEASE xkau-entry .
      FIND op-entry OF kau-entry NO-LOCK NO-ERROR .
      FIND LAST xkau-entry  WHERE
                xkau-entry.debit
            AND ( IF AVAIL op-entry AND op-entry.acct-db <> ?
                 THEN xkau-entry.op-entry = kau-entry.op-entry
                 ELSE TRUE)
            AND xkau-entry.op  = kau-entry.op
            AND xkau-entry.kau = kau-entry.kau NO-LOCK NO-ERROR.
     IF NOT AVAIL xkau-entry THEN DO:
       IF     kau-entry.contract-date > beg-date
          AND kau-entry.contract-date <= in-op-date
       THEN beg-date = kau-entry.contract-date .
     END.
   END.
  END.
END PROCEDURE.

/*��।������ ���� �����ᨨ */
PROCEDURE Get-Date-Comm-Rate:
   DEF INPUT PARAM in-comm AS CHAR NO-UNDO.     /* ��� �����ᨨ */
   DEF INPUT PARAM rid AS RECID NO-UNDO.        /* ��뫪� �� ��� */
   DEF INPUT PARAM date-contr AS DATE NO-UNDO.  /* ��� ����砭�� ��ਮ��*/
   DEF INPUT PARAM dat-ref-old AS DATE NO-UNDO. /* �।. ��� ���. - 1 ���� */
   DEF INPUT PARAM dat-t-old AS DATE NO-UNDO.   /* �।. �������쭠� ��� */
   DEF OUTPUT PARAM dat-ref AS DATE NO-UNDO.    /* ��� �����ᨨ - 1 ���� */
   DEF OUTPUT PARAM dat-t AS DATE NO-UNDO.      /* �������쭠� ��� */

   dat-ref = dat-ref-old.
   dat-t = dat-t-old.

   FIND FIRST acct WHERE RECID(acct) = rid NO-LOCK NO-ERROR.

   { findcom1.i
           &rcom=in-comm
           &comm-rate=comm-rate
           &dir=first
           &rsum=0
           &since1=" > dat-ref-old + 1  and comm-rate.since <=  date-contr"
        }

   IF AVAIL comm-rate THEN dat-ref = comm-rate.since - 1.
   ELSE dat-ref = dat-t  .

   IF dat-t > dat-ref THEN dat-t = dat-ref.
   ELSE dat-ref = dat-t.

END PROCEDURE.

/* ���� ��ࢮ� ���� ��������� �⠢�� %��� � %�����
   � �롮� ⮩, �� �뫠 ࠭�� */
PROCEDURE Get-First-Comm-Rate:
   DEF INPUT PARAM rid AS RECID NO-UNDO.           /* ��뫪� �� ��� */
   DEF INPUT PARAM date-contr AS DATE NO-UNDO.     /* ��� ����砭�� ��ਮ��*/
   DEF INPUT-OUTPUT PARAM dat-ref AS DATE NO-UNDO. /* ��� �����ᨨ - 1 ���� */
   DEF INPUT-OUTPUT PARAM dat-t AS DATE NO-UNDO.   /* �������쭠� ��� */

   DEF VAR dat-ref1 AS DATE NO-UNDO.
   DEF VAR dat-ref2 AS DATE NO-UNDO.
   DEF VAR dat-t1 AS DATE NO-UNDO.
   DEF VAR dat-t2 AS DATE NO-UNDO.

   RUN Get-Date-Comm-Rate ("%���",
                            rid,
                            date-contr,
                            INPUT dat-ref,
                            INPUT dat-t,
                            OUTPUT dat-ref1,
                            OUTPUT dat-t1).

   RUN Get-Date-Comm-Rate (fGetSetting("�����।", ?, "%�����"),
                            rid,
                            date-contr,
                            INPUT dat-ref,
                            INPUT dat-t,
                            OUTPUT dat-ref2,
                            OUTPUT dat-t2).

   IF dat-ref1 < dat-ref2 THEN dat-ref = dat-ref1.
   ELSE dat-ref = dat-ref2.

   IF dat-t1 < dat-t2 THEN dat-t = dat-t1.
   ELSE dat-t = dat-t2.

END PROCEDURE.

/* ��।������  �᫮��� ���᫥��� ��業⮢ - �奬� � �⠢�� ��᫥ ���䭮� �஫����樨  */

 PROCEDURE Get_PenOpkind_Inter_Commi.
    DEF INPUT PARAM  irid AS RECID NO-UNDO .    /* RECID ������� */
    DEF INPUT-OUTPUT PARAM p-commi AS CHAR NO-UNDO.   /* ��� �����ᨨ
                                   ��� ���᫥��� ��業⮢  */
    DEF INPUT-OUTPUT PARAM p-inter AS CHAR NO-UNDO.     /* ��� �奬�
                                                      ���᫥��� */
    DEF VAR vout-op-kind AS CHAR NO-UNDO.       /*��� �࠭���樨
                              ���䭮� �஫����樨 */
    DEF VAR vin-class AS CHAR NO-UNDO .
    DEF BUFFER loan FOR loan.
    DEF BUFFER op-templ FOR op-templ .
    DEF BUFFER class FOR class .

    FIND FIRST loan WHERE RECID(loan) = irid NO-LOCK NO-ERROR .
    IF NOT AVAIL loan THEN RETURN .

    RUN get-param-const-by-kind (loan.class-code, loan.op-kind,
                                 'prol-kind-pen', output vout-op-kind).
    IF vout-op-kind  <> ?  THEN DO :
      RELEASE op-templ .
      FOR EACH op-templ WHERE op-templ.op-kind = vout-op-kind NO-LOCK :
         vin-class = op-templ.cr-class-code.
         RUN ffparent IN h_xclass  (INPUT-OUTPUT vin-class, buffer class).
         IF vin-class = 'loan' THEN LEAVE .
      END.
      /* �饬 ४������ ᭠砫� �� 蠡���� ������� */
      IF AVAIL op-templ THEN
      DO:
        ASSIGN
           p-commi = GetOpTemlXattr(op-templ.op-kind
                          + ','
                          + string(op-templ.op-template),'commission',?) .
          p-inter = GetOpTemlXattr(op-templ.op-kind
                          + ','
                          + string(op-templ.op-template),'interest',?)
.


        IF p-commi = ? THEN
           p-commi = GetXAttrInit(op-template.cr-class-code,"commission") .
        IF p-inter = ? THEN
           p-inter = GetXAttrInit(op-template.cr-class-code,"interest") .
      END. /* ��ࠡ�⪨ ���������� 蠡����� */
    END. /* ��ࠡ�⪨ �࠭���樨 ���䭮� �஫����樨 */
 END PROCEDURE .

PROCEDURE Get_Contract_Date.
   DEF INPUT PARAM rid AS RECID NO-UNDO .
   DEF INPUT PARAM in-op-date AS DATE NO-UNDO .
   DEF OUTPUT PARAM out-date AS DATE INIT ? NO-UNDO .

   DEF BUFFER loan FOR loan .
   DEF VAR dep_period   AS CHAR        NO-UNDO.
   DEF VAR fl-old-dps   AS LOGICAL     NO-UNDO.
   DEF VAR vTmpDate     AS DATE        NO-UNDO.
   DEF VAR vWorkGraf    AS CHARACTER   NO-UNDO.
   DEF VAR vIsWorkDay   AS LOGICAL     NO-UNDO.

   FIND FIRST Loan WHERE RECID(LOAN) EQ rid 
      NO-LOCK NO-ERROR.
   IF NOT AVAIL LOAN
      THEN RETURN.
   IF in-op-date gt loan.end-date THEN
      ASSIGN out-date = in-op-date.
   ELSE
   DO:
      IF new_dps(loan.open-date) > 0 THEN 
         out-date = loan.end-date + 1.
      ELSE
      DO:
         New_Dps_Prol(BUFFER loan,OUTPUT dep_period,OUTPUT fl-old-dps) .
         IF fl-old-dps THEN 
            out-date = loan.end-date.
         ELSE
         DO:
            /* ����祭�� ��䨪� ࠡ��� */
            vWorkGraf = GetWorkGraf(loan.contract + "," + loan.cont-code,
                                    loan.Class-Code).
            IF vWorkGraf NE ? THEN
            DO:
               vIsWorkDay = IsWorkDayGraf (loan.end-date,
                                           vWorkGraf).
               IF NOT vIsWorkDay THEN 
               DO:
                  vTmpDate = loan.end-date.
                  DO WHILE NOT IsWorkDayGraf (vTmpDate,
                                              vWorkGraf):
                     vTmpDate = vTmpDate + 1.
                  END.
                  out-date = vTmpDate.
               END.
               ELSE 
                  out-date = loan.end-date.
            END.
            ELSE
            DO:
               IF {holiday.i loan.end-date} THEN 
               DO:
                  vTmpDate = loan.end-date.
                  DO WHILE {holiday.i vTmpDate}:
                     vTmpDate = vTmpDate + 1.
                  END.
                  out-date = vTmpDate.
               END.
               ELSE 
                  out-date = loan.end-date.
            END.           
         END.
      END.
   END.
END PROCEDURE .

PROCEDURE chk-class-loan-period.
    DEF INPUT PARAMETER iCurrDate AS DATE  NO-UNDO. /*⥪��� ���*/
    DEF INPUT PARAMETER irec      AS RECID NO-UNDO. /* �����*/
    DEF OUTPUT PARAMETER oOk AS LOG NO-UNDO INIT FALSE. /*�������� �� ����⨥*/
    DEF BUFFER vbuf-loan FOR loan.
    DEF VAR vdate-from-ch AS CHAR NO-UNDO.
    DEF VAR vdate-to-ch   AS CHAR NO-UNDO.
    DEF VAR vdate-from AS DATE NO-UNDO.
    DEF VAR vdate-to   AS DATE NO-UNDO.

    FIND FIRST vbuf-loan WHERE RECID(vbuf-loan) = irec NO-LOCK NO-ERROR.

    vdate-from-ch = GetCacheInit(vbuf-loan.class-code,"�������") .

    vdate-from = IF vdate-from-ch = ? OR vdate-from-ch = "?"
                 THEN ?
                 ELSE DATE(vdate-from-ch) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN vdate-from = ?.
    vdate-to-ch = GetCacheInit(vbuf-loan.class-code,"��������") .
    vdate-to = IF vdate-to-ch = ? OR vdate-to-ch = "?"
               THEN ?
               ELSE DATE(vdate-to-ch) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN vdate-to = ?.
    IF vdate-to NE ? AND vdate-from NE ? THEN
        IF iCurrDate >= vdate-from AND iCurrDate <= vdate-to THEN do:
            oOk = TRUE.
            RETURN.
        END.
    IF vdate-to = ? THEN
        IF iCurrDate >= vdate-from THEN do:
            oOk = TRUE.
            RETURN.
        END.
    IF vdate-from = ? THEN
        IF iCurrDate <= vdate-to THEN do:
            oOk = TRUE.
            RETURN.
        END.
    IF vdate-to = ? AND vdate-from = ? THEN
         oOk = TRUE.
END.

/* �஢�ઠ ���������� �஫����樨 �� ��⠬ � �������� �஫����権 */
PROCEDURE Chk_Limit_Per .
   DEF INPUT PARAMETER iCurrDate AS DATE  NO-UNDO. /*⥪��� ���*/
   DEF INPUT PARAMETER rid       AS RECID NO-UNDO. /* �����*/
   DEF INPUT PARAM d_sum         AS INT64 NO-UNDO .
   DEF OUTPUT PARAM  fl-ok AS LOGICAL NO-UNDO .

   DEF VAR limitprol      AS INT64 NO-UNDO .
   DEF VAR vdate-po-prol  AS DATE NO-UNDO INIT ?.
   DEF VAR vHeritOpenDate AS DATE INIT ? NO-UNDO.  /* ��� ������ ����頭�� */
   
   DEF BUFFER loan FOR loan .
   FIND loan WHERE RECID(loan) = rid NO-LOCK NO-ERROR .
   IF NOT AVAIL loan THEN RETURN .

      vHeritOpenDate = DATE(GetXAttrValueEx("person",
                                            STRING(loan.cust-id),
                                            "herit-open-date",
                                            ?)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN vHeritOpenDate = ?.
      IF vHeritOpenDate <> ? AND vHeritOpenDate <= iCurrDate THEN
         /* ����� ��᫥��⢮ - �஫������ ����������. */
         RETURN .

   IF  New_dps(loan.open-date) lt 0
   THEN DO:
     fl-ok = YES .
     RETURN .
   END.

   /*�஢�ઠ ��࠭�祭�� ���-�� �஫����権 �१ �����䨪���
   �᫨ ��뫪� �� �����䨪��� ��� - � �஢��塞 ����� ��⥬ */
   RUN  get_limitprol_from_code (RECID(loan),
                                 iCurrDate,
                                 OUTPUT limitprol). 
   IF limitprol = ? THEN 
       /*�� 蠡���� ������ �࠭���樨 ������ ��� �� ����� ������ - ४����� limitprol*/
       RUN get-param-const-by-kind (loan.class-code, loan.op-kind,
                                    'limitprol', output limitprol).
   IF   limitprol <> ? AND limitprol < d_sum
   THEN RETURN .
   
   IF GetCacheInit (loan.class-code, '���������ਏ஫') EQ "��" THEN
      ICurrDate = ICurrDate - 1.
                  
   run chk-class-loan-period(ICurrDate,rid,OUTPUT fl-ok) .
END PROCEDURE .

/*��������� ������⢮ �஫����権 �१ �����䨪���*/
PROCEDURE get_limitprol_from_code.
  DEF INPUT PARAMETER iRec AS RECID NO-UNDO.
  DEF INPUT PARAMETER iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAMETER oNumProl AS INT64 NO-UNDO INIT ?.

  DEF VAR vPeriodInt AS INT64 NO-UNDO.
  DEF VAR vPeriodChar AS CHAR NO-UNDO.
  DEF VAR vCode AS CHAR NO-UNDO. 
  DEF VAR vi AS INT64 NO-UNDO.
  DEF VAR vj AS INT64 NO-UNDO.
  DEF VAR vDate1 AS DATE NO-UNDO.
  DEF VAR vDate2 AS DATE NO-UNDO.

  FIND FIRST loan WHERE RECID(loan) = iRec NO-LOCK NO-ERROR.
  IF NOT AVAILABLE loan THEN RETURN.

  RUN get-beg-date-prol (iRec,
                         iDate,
                         OUTPUT vDate1,
                         OUTPUT vDate2).

  RUN correct_date (iRec,
                    INPUT-OUTPUT vDate1).
  ASSIGN vPeriodint = vDate2 - vDate1.

  /*��뫪� �� �����䨪���*/
  RUN get-param-const-by-kind (loan.class-code, loan.op-kind,
                               'prol-limits-code', output vCode).

  /*��뫪� �� �����䨪��� ��� - �஫�����㥬 �� ��࠭�祭��*/
  IF vCode = ? OR vCode = "?" THEN 
      RETURN.

  /* ����� �����⨬��� ���-�� �஫����権 �� �����䨪���� */
  vi = 0.
  FOR EACH CODE WHERE CODE.class = "lim_prol" AND CODE.CODE BEGINS vCode + "@" NO-LOCK BY CODE:
     vj = INT64(ENTRY(2,CODE.CODE,"@")) NO-ERROR.

     /* �᫨ ������ � ���ࢠ� - �� */
     IF vi < vPeriodInt AND vPeriodInt <= vj THEN  DO: 
         oNumProl = INT64(CODE.val).
         RETURN.
     END.
     /* �� ������ - �த������ ���� �� �����⠭�� */
     vi = vj.
  END.
  /* �᫨ �� ��諨 ���� �����䨪��� � �� ������ �� � ���� ���ࢠ� - 0 �஫����権 */
  oNumProl =  0.
END PROCEDURE.

PROCEDURE ChangeDateProl.
   DEF INPUT PARAM  rid       AS RECID NO-UNDO .
   DEF INPUT PARAM  plan-date AS DATE  NO-UNDO .
   DEF OUTPUT PARAM fl        AS INT64   NO-UNDO .
   DEF VAR str-par           AS CHAR  NO-UNDO .

  RUN get_last_param (rid,plan-date,plan-date,'���������ਏ஫', output str-par) .
  IF str-par = ? OR str-par ='?'  THEN RETURN.
  IF str-par = '��'  THEN  fl = 1 .

END PROCEDURE .


/*��� ����砭�� ��ਮ�� ���������� ������ VTB*/
PROCEDURE end_doloan_dps.
    DEF INPUT PARAM  iRec  AS RECID NO-UNDO. /*recid ��� ���᪠ ����*/
    DEF INPUT PARAM  iDate AS DATE  NO-UNDO. /*��� ����樨*/

    DEF OUTPUT PARAM  oDate AS DATE NO-UNDO.          /*��� ��᫥�����
                                                       ��� �����ᥭ��*/
    DEF OUTPUT PARAM  oMess AS CHAR NO-UNDO INIT "".  /*����饭�� �� �訡��*/

    DEF VAR mPeriod     AS CHAR NO-UNDO. /*�த����⥫쭮��� ������*/
    DEF VAR mLim-doloan AS INT64  NO-UNDO. /*������⢮ ���� �� �������*/
    DEF VAR mLim-date   AS DATE NO-UNDO. /*��� ��᫥����� �����ᥭ��*/
    DEF VAR mCode       AS CHAR NO-UNDO. /*�����䨪���*/
    DEF VAR vBegDate    AS DATE NO-UNDO. /*��� ��砫� �஫����樨*/
    DEF VAR vEndDate    AS DATE NO-UNDO. /*��� ᫥���饩 �஫����樨*/
    DEF BUFFER loan FOR loan.

    FIND FIRST loan WHERE RECID(loan) = iRec NO-LOCK NO-ERROR.
    IF NOT AVAILABLE loan THEN DO:
        oMess = "����� �� ������".
        RETURN.
    END.

    RUN get-beg-date-prol (RECID(loan),
                           iDate,
                           OUTPUT vBegDate,
                           OUTPUT vEndDate).
    IF vEndDate EQ ? THEN DO:
       ASSIGN
          oMess = ""
          oDate = ?
       .
       RETURN.
    END.

    /*�த����⥫쭮��� ������*/
    mPeriod = GetXattrValueEx("loan",
                              loan.contract + "," + loan.cont-code,
                              "dep_period",
                              ?).
    IF mPeriod  = ? THEN
       RUN Get_Last_Param (recid(loan),
                          iDate,
                          iDate,
                          'dep-period', 
                          output mPeriod).
    IF mPeriod = ? OR mPeriod = "?" THEN 
    DO:
        ASSIGN
         oMess = "���������� ��।����� �த����⥫쭮��� ������!"
         oDate = ?.
        RETURN.
    END.

    /* �����䨪��� ����஫� �� ���������ﬨ */
    RUN Get_Last_Param (RECID(loan),
                        iDate,
                        iDate,
                        "���������", 
                        OUTPUT mCode).
    IF mCode = ? OR mCode = "" THEN do:
        ASSIGN 
         oMess = ""
         oDate = ?.
        RETURN.
    END.

    FIND FIRST CODE WHERE CODE.class   EQ "lim_dln" AND 
                          CODE.CODE    BEGINS mCode AND
                          mPeriod MATCHES ENTRY(2, CODE.CODE, "@")
       NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CODE THEN
    DO: 
        ASSIGN
        oMess = "��� ����� " + loan.class-code + "� �த����⥫쭮�� " + 
                mPeriod + 
                "~n �� ��।����� ���祭�� � �����䨪��� lim_dln"
        oDate = ?.
        RETURN.
    END.
    IF CODE.misc[1] NE "" AND CODE.misc[1] NE ? THEN
    DO:           
       oDate = get-end-date(vBegDate,CODE.misc[1]).
       RETURN.
    END.
    mLim-doloan = INT64(CODE.val).
    mLim-date = loan.end-date -  mLim-doloan + 1.
    oDate =  mLim-date.
    RETURN.

END PROCEDURE .


/* ��।������, ���� �� � 蠡����� �࠭���樨 �㭪�� ���᫐��.
** (����室��� ��� �������⥫쭮�� ��室�) */
PROCEDURE Get-Old-Ref.
   DEF INPUT  PARAM iRecOpTempl AS RECID   NO-UNDO. /* Recid op-template */
   DEF OUTPUT PARAM oOldRef     AS LOGICAL NO-UNDO. /* ����稥 ���᫐�� � 蠡���� */

   FIND FIRST op-template WHERE RECID(op-template) EQ iRecOpTempl NO-LOCK NO-ERROR.
   IF NOT AVAIL op-template THEN
      RETURN ERROR.

   IF op-template.amt-rub MATCHES "*���᫐��*" THEN DO:
      RUN SetSysConf IN h_base ("OldRef", "��").
      oOldRef = YES.
   END.
END PROCEDURE.

/* ��।������ ���� ����襭�� �������쭮�� ���⪠ */
PROCEDURE DateBreakMinOst.
   DEF INPUT  PARAM iLoanRID  AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oDate     AS DATE        NO-UNDO.

   DEF VAR vAcct        AS CHARACTER   NO-UNDO.
   DEF VAR vAcctRole    AS CHARACTER   NO-UNDO.
   DEF VAR vKodOst      AS CHARACTER   NO-UNDO.
   DEF VAR vBegDate     AS DATE        NO-UNDO.
   DEF VAR vEndDate     AS DATE        NO-UNDO.
   DEF VAR vKapDate     AS DATE        NO-UNDO.
   DEF VAR vBal         AS DECIMAL     NO-UNDO.
   DEF VAR vMinOst      AS DECIMAL     NO-UNDO.
   DEF VAR vMinOstStr   AS CHARACTER   NO-UNDO.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:

      FIND FIRST loan WHERE RECID(loan) EQ iLoanRID
         NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
         UNDO MAIN, LEAVE MAIN.

      /* ��।������ �᭮����� ��� �� ������ */          
      RUN GetBaseAcct IN h_dps (loan.contract,
                                loan.cont-code,
                                iDate,
                                OUTPUT vAcct).
      IF NOT ({assigned vAcct}) THEN
         UNDO MAIN, LEAVE MAIN.

      /* ��।����� ஫� �᭮����� ��� */                             
      RUN GetBaseAcctRole IN h_dps (iLoanRID,
                                    iDate,
                                    OUTPUT vAcctRole).
      IF NOT ({assigned vAcctRole}) THEN
         UNDO MAIN, LEAVE MAIN.

      /* ��।������ ���� ��.���⪠ */                                  
      RUN GetBaseKodOst IN h_dps (vAcctRole,
                                  OUTPUT vKodOst).
      IF NOT ({assigned vKodOst}) THEN
         UNDO MAIN, LEAVE MAIN.

      RUN get-beg-date-prol IN THIS-PROCEDURE (RECID(loan),
                                               iDate,
                                               OUTPUT vBegDate,
                                               OUTPUT vEndDate).
      vKapDate = vBegDate.
      RUN get_beg_kper IN THIS-PROCEDURE (RECID(loan),
                                          iDate,
                                          ?,
                                          INPUT-OUTPUT vKapDate).

      RUN Get_Last_Param (RECID(loan),
                          iDate,
                          iDate,
                          "������",
                          OUTPUT vMinOstStr).
      vMinOst = DECIMAL(vMinOstStr) NO-ERROR.

      RUN kau-pos.p (ENTRY(1, vAcct),
                     ENTRY(2, vAcct),
                     vKapDate,
                     vKapDate,
                     gop-status,
                     loan.contract + "," + loan.cont-code + "," + vKodOst).
      vBal = IF Loan.currency EQ "" THEN ABS(ksh-bal)
                                    ELSE ABS(ksh-val).
      IF vBal LT vMinOst THEN
      DO:
         oDate = vKapDate.
         UNDO MAIN, LEAVE MAIN.
      END.
      ELSE
      DO:
         FOR EACH kau-entry WHERE kau-entry.acct     EQ ENTRY(1, vAcct)
                              AND kau-entry.currency EQ ENTRY(2, vAcct)
                              AND kau-entry.debit 
                              AND kau-entry.op-date  GE vKapDate
                              AND kau-entry.kau      EQ loan.contract + "," + loan.cont-code + "," + vKodOst
            NO-LOCK:
            
            RUN kau-pos.p (ENTRY(1, vAcct),
                           ENTRY(2, vAcct),
                           kau-entry.contract-date,
                           kau-entry.contract-date,
                           gop-status,
                           loan.contract + "," + loan.cont-code + "," + vKodOst).
            vBal = IF Loan.currency EQ "" THEN ABS(ksh-bal)
                                          ELSE ABS(ksh-val).
            IF vBal LT vMinOst THEN
            DO:
               oDate = kau-entry.contract-date.
               UNDO MAIN, LEAVE MAIN.
            END.
         END.
      END.
   END.
END PROCEDURE.

/* ��।������ ���� ����襭�� �������쭮�� ���⪠ �� ������ �� ������
 � ��⮬ �஫����権, ᬥ�� ��⮢ � ��.
 ��� �ࠢ��쭮�� �ᯮ�짮����� �� ������ ������
 ���� ��⠭����� ���������� */

PROCEDURE DateBreakMinOstPer.
   DEF INPUT  PARAM iLoanRID    AS RECID       NO-UNDO.
   DEF INPUT  PARAM iDateStart  AS DATE        NO-UNDO.
   DEF INPUT  PARAM iDateEnd    AS DATE        NO-UNDO.
   DEF OUTPUT PARAM oDate       AS DATE        NO-UNDO.

   DEFINE VARIABLE vIn-inter      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStr-Acct      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIn-kau        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vMinOstStr     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodOst        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vI             AS INT64       NO-UNDO.
   DEFINE VARIABLE vBegDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vEndDate       AS DATE        NO-UNDO.
   DEFINE VARIABLE vSummPrichProc AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE mAmtSummIz     AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vMinOst        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vBal           AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vFlErr         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vMigrCode      AS CHARACTER   NO-UNDO. /* ����� - ����� �� ��㣮� ����/��⥬� */

   DEFINE BUFFER b-loan      FOR loan.
   DEFINE BUFFER b-acct      FOR acct.
   DEFINE BUFFER b-kau-entry FOR kau-entry.
   DEFINE BUFFER b-loan-acct FOR loan-acct.

   MAIN:
   DO ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE:
      FIND FIRST b-loan WHERE RECID (b-loan) EQ iLoanRid
         NO-LOCK NO-ERROR.
      IF NOT AVAIL b-loan THEN
         UNDO MAIN, LEAVE MAIN.

      RUN Get_Last_Param  (RECID(b-loan),
                           iDateEnd,
                           iDateEnd,
                           "����������",
                           OUTPUT vIn-inter).

      RUN GET_acct_per (RECID (b-loan),
                        iDateStart,
                        iDateEnd,
                        OUTPUT vStr-Acct). /* ���᮪ �᭮���� ��⮢ ������ */
      /* ��� �� ������ ��� */
      IF NOT {assigned vStr-acct} THEN 
      DO:
         UNDO MAIN, LEAVE MAIN.
      END.

      vMigrCode = GetXAttrValueEx("loan",
                                  SUBST("&1,&2", b-loan.contract, b-loan.cont-code),
                                  "�������",
                                  "").

      vBegDate = iDateStart.
      DO_ACCT:
      DO vI = 1 TO NUM-ENTRIES(vStr-acct) BY 2
         ON ERROR  UNDO, LEAVE
         ON ENDKEY UNDO, LEAVE:

         /* ���� ��� */
         {find-act.i
            &acct = "ENTRY(vI, vStr-acct) "
            &curr = b-loan.currency 
            &bact = b-acct 
         }

         IF NOT AVAIL b-acct THEN
            UNDO DO_ACCT, NEXT DO_ACCT.
         /* ���樠������ ��� */
         IF vI GT 1 THEN
            vBegDate = DATE(ENTRY(vI + 1,vStr-acct)).

         IF NUM-ENTRIES(vStr-acct) GE vI + 3 THEN 
            vEndDate = DATE(ENTRY(vI + 3,vStr-acct)).
         ELSE
            vEndDate = iDateEnd.   

         RUN Get_Last_Param (RECID(b-loan),
                             vEndDate,
                             vEndDate,
                             "������",
                             OUTPUT vMinOstStr).
         vMinOst = DECIMAL(vMinOstStr) NO-ERROR.

         FIND LAST b-loan-acct WHERE b-loan-acct.contract  EQ b-loan.contract 
                                 AND b-loan-acct.cont-code EQ b-loan.cont-code 
                                 AND b-loan-acct.acct      EQ b-acct.acct 
                                 AND b-loan-acct.currency  EQ b-acct.currency
                                 AND CAN-DO("loan-dps-t,loan-dps-p",b-loan-acct.acct-type) 
            NO-LOCK NO-ERROR.
         IF AVAIL b-loan-acct THEN
         DO:
            RUN GetBaseKodOst IN h_dps (b-loan-acct.acct-type,
                                        OUTPUT vKodOst).
            vIn-kau = b-loan.contract + "," + b-loan.cont-code + "," +  vKodOst.
            /* �� ������ */
            Izyat:
            FOR EACH b-kau-entry WHERE b-kau-entry.acct     EQ b-acct.acct 
                                   AND b-kau-entry.currency EQ b-acct.currency
                                   AND b-kau-entry.kau      EQ vIn-kau 
                                   AND b-kau-entry.debit 
                                   AND b-kau-entry.op-date  GE vBegDate
                                   AND b-kau-entry.op-date  LE vEndDate 
               NO-LOCK:
               IF    CAN-FIND(op-entry OF b-kau-entry WHERE op-entry.op-cod = vIn-inter)
                  OR ({assigned vMigrCode} AND b-kau-entry.op-code EQ vIn-inter) THEN
               DO:
                  RUN kau-pos.p (b-kau-entry.acct,
                                 b-kau-entry.currency,
                                 b-kau-entry.op-date,
                                 b-kau-entry.op-date,
                                 gop-status,
                                 b-loan.contract + "," + b-loan.cont-code + "," + vKodOst).
                  vBal = IF b-loan.currency EQ "" THEN ABS(ksh-bal)
                                                  ELSE ABS(ksh-val).
                  IF vBal LT vMinOst THEN
                  DO:
                     oDate = b-kau-entry.op-date.
                     LEAVE MAIN.
                  END.
               END. /*IF CAN-FIND(op-entry OF b-kau-entry*/
            END. /* FOR EACH b-kau-entry */
         END.
      END. /* DO vI = 1 TO NUM-ENTRIES(vStr-acct) BY 2 */
   END.
END PROCEDURE.


/* �㭪�� �஢������ ����室������ ᮧ����� 
** ������ �᫮��� ����� ࠧ �� �஫����樨 */
FUNCTION Dps_Cr_Cond RETURNS LOG (INPUT iRid AS RECID) :
    DEF VAR vNameProc AS CHAR NO-UNDO.
    DEF VAR vParams   AS CHAR NO-UNDO.

    FIND FIRST loan WHERE RECID(loan) EQ iRid 
       NO-LOCK NO-ERROR.
    RUN  GetClassMethod IN h_xclass (loan.class-code,
                                     'ch_loan',
                                     "","",
                                     OUTPUT vNameProc,
                                     OUTPUT vParams).
    IF vNameProc EQ ? THEN 
       RETURN NO.
    RETURN YES.
END.
/* $LINTFILE='get_date.i' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='kozv' */
/* $LINTDATE='04/07/2016 14:19:29.915+03:00' */
/*prosign3KSHAC0sHAT5RI/Iokahfg*/
/* --- get_date.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 2:02pm --- */
