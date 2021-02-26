/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: get_loans.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 01.07.2011 09:49 Daru
     Modified: 01.07.2011 09:49 Daru
     Modified: 14.10.2011 11:32 OZMI     (0149467)
*/

{globals.i}             /* Глобальные переменные сессии. */
{svarloan.def new}
{pick-val.i}
{tmprecid.def}
{tmprecid.def &PREF="copyof-" &NGSH="local" } /* для копии tmprecid */
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get ovl}

DEFINE INPUT  PARAM iParam AS CHAR  NO-UNDO. /* Содержит строку параметров в следующем формате <in-contarct> */
DEFINE OUTPUT PARAM oCount AS INT64 NO-UNDO.

DEF NEW GLOBAL SHARED VAR work-module AS CHAR NO-UNDO.
DEF VAR IsMM               AS LOG         NO-UNDO.
DEF VAR mPeresch           AS LOG         NO-UNDO.
DEF VAR mLeEndDate         AS LOG         NO-UNDO.
DEF VAR mContractDate      AS DATE        NO-UNDO.
DEF VAR mContract          AS CHARACTER   NO-UNDO.
DEF VAR mClass             AS CHARACTER   NO-UNDO.

DEFINE VARIABLE vClass     AS CHARACTER             NO-UNDO.
DEFINE VARIABLE vClassFlt  AS CHARACTER INITIAL "*" NO-UNDO.
DEFINE VARIABLE vAddFlt    AS CHARACTER INITIAL ""  NO-UNDO.
DEFINE VARIABLE vAddFltVal AS CHARACTER INITIAL ""  NO-UNDO.


IsMM = work-module EQ "mm".

mContract = iParam.

/* Запуск браузера по договорам. */
IF IsMM THEN
DO:
   RUN browseld.p("loan_mm",
                  "RidRest" + chr(1) + "iClass"  + CHR(1) + "Title"                + CHR(1) + "cont-type" + CHR(1) + "doc-num" ,
                  "YES"     + CHR(1) + "loan_mm" + CHR(1) + "МЕЖБАНКОВСКИЕ СДЕЛКИ" + CHR(1) + "Течение"   + CHR(1) + "*",
                  "",4).
END.
ELSE
DO:
   IF NUM-ENTRIES(mContract, "^") GT 1 THEN
      ASSIGN
         vClass    = ENTRY(2, mContract, "^")
         mContract = ENTRY(1, mContract, "^")
            vClassFlt = vClass
         .
   ELSE
   DO:
      IF work-module EQ "bill" OR
         work-module EQ "bm"
      THEN
         ASSIGN
            vClass = IF mContract eq "кредит" THEN "dsc-bill-asset"
                                                ELSE "own-bill-liability"
            vClassFlt  = vClass
            vAddFlt    = CHR(1) + "LoanClose"
            vAddFltVal = CHR(1) + "NO"
         .
      ELSE IF work-module EQ "Aijk"
      THEN
         ASSIGN
            vClass    = "loan_ces_pers"
            vClassFlt = vClass
         .
      ELSE
         vClass = IF mContract eq "кредит" THEN "loan_allocat"
                                             ELSE "loan_attract".
   END.

   RUN browseld.p (vClass,
                   "RidRest" + CHR(1) + "contract"  + CHR(1) + "Class-code" + vAddFlt    + CHR(1) + "FirstFrame",
                   "YES"     + CHR(1) + mContract   + CHR(1) + vClassFlt    + vAddFltVal + CHR(1) + "1",
                   ?,
                   4).
END.

RELEASE tmprecid.

/* Сохраняем выборку */
EMPTY TEMP-TABLE copyof-tmprecid .
FOR EACH tmprecid :
   CREATE copyof-tmprecid.
   BUFFER-COPY tmprecid TO copyof-tmprecid.
END.


IF NOT CAN-FIND(FIRST copyof-tmprecid) THEN
DO:
   RUN Fill-SysMes IN h_tmess ("", "", "-1", "Не выбрано ни одного объекта. Операция прерывается.").
   RETURN.
END.

{getdate.i
   &DateLabel = "ПЛАНОВАЯ ДАТА"
   &DateHelp  = "Введите плановую дату операции"
}

ASSIGN
   mContractDate = end-date
   mPeresch      = ?
   mLeEndDate    = ?
   oCount        = 0
.
SetOpDate(mContractDate).
Block_tmprecid:
FOR EACH copyof-tmprecid :
   FIND FIRST loan WHERE RECID(loan) EQ copyof-tmprecid.id  NO-LOCK NO-ERROR.
   oCount = oCount + 1.
END.
/* Сохраняем выборку */
EMPTY TEMP-TABLE tmprecid.
FOR EACH copyof-tmprecid :
   CREATE tmprecid.
   BUFFER-COPY copyof-tmprecid TO tmprecid.
   RELEASE tmprecid.
END.
EMPTY TEMP-TABLE copyof-tmprecid.
{intrface.del}
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='06/04/2015 18:04:06.684+04:00' */
/* $LINTUSER='BIS' */
/* $LINTMODE='1' */
/* $LINTFILE='get_loans.p' */
/*prosignizqljZmWg/GMhrtt82f9qQ*/