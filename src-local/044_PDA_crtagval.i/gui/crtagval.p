{globals.i}
{intrface.get tmess}

/* +++ crtagval.p was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:03pm +++ */

/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: crtagval.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 12.04.2007 15:28 fEAk    
     Modified: 18.04.2008 12:12 fEAk     <comment>
     Modified: 19.07.2007       muta  0075116  Добавлена обработка формул из loanform.p
     Modified:
*/

/* Какие парсерные функции уже готовы для работы?
   - dps_prn.p 
      - адрес
      - выдан
      - выдан_дата 
      - договор
      - инн
      - нач_дог
      - нач_ст
      - нач_ст2
      - номер_счета
      - окон_дог
      - сум_вкл2 
      - тек_ст
      - тел
      - фио 
*/

{globals.i}
{prn-doc.def &with_proc=YES}
{norm.i}

DEFINE INPUT PARAM iDate1 AS DATE NO-UNDO.
DEFINE INPUT PARAM iDate2 AS DATE NO-UNDO.
DEFINE INPUT PARAM iClass AS CHARACTER NO-UNDO.
DEFINE INPUT PARAM iRet   AS LOG       NO-UNDO. /* Зн-е реквизита МестоДляВпис. "Да" и пустое зн-е тега - заполняем 20ю символами "_" */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttnames.

DEF SHARED VAR rid_loan  AS RECID.
DEF SHARED VAR rid-p AS RECID.                  

DEF VAR vProcName       AS CHARACTER   NO-UNDO.
DEF VAR mParams         AS CHARACTER   NO-UNDO.
DEF VAR vSupportedProc  AS CHARACTER   NO-UNDO.
DEF VAR vOtherTags      AS CHARACTER   NO-UNDO.
DEF VAR vError          AS LOGICAL     NO-UNDO.
DEF VAR vTmpStr         AS CHARACTER   NO-UNDO.
DEF VAR vTmpInt         AS INT64     NO-UNDO.
DEF VAR mEnd            AS INT64     NO-UNDO.
DEF VAR vI              AS INT64     NO-UNDO.

ASSIGN 
   RetString      = YES
   vSupportedProc = "dps_prn,cardprn,loanform,dog,bankinfo,userinfo"
   PrinterWidth   = 10000
   .

/* Functions & Procedures*/
{crtagval.i}
/*-----------------------*/

IF iClass NE ? THEN
DO:
   FIND FIRST DataClass WHERE DataClass.DataClass-Id EQ iClass NO-LOCK NO-ERROR.
   {for_form.i 
      &DataClass = DataClass
      &End-Date  = iDate2}
      
       ASSIGN
         vTmpInt = INDEX(formula.formula, "(") 
         mEnd    = INDEX(formula.formula, ")")
      . 
      IF     vTmpInt NE 0 
         AND mEnd    NE 0	
      THEN
         ASSIGN 
            vProcName = SUBSTR(formula.formula, 
                               1, 
                               vTmpInt - 1)
            mParams   = SUBSTR(formula.formula, 
                               vTmpInt + 1, 
                               mEnd - vTmpInt - 1)
         .
      ELSE 
         NEXT.

      IF CAN-DO(vSupportedProc, vProcName) THEN
      DO:
         RUN Run-PrnProc(vProcName, iDate1, iDate2, mParams, OUTPUT vTmpStr).
         RUN Insert_TTName(formula.var-id, vTmpStr).
      END.
   END.
END.
ELSE
DO: 
   DO vI = 1 TO NUM-ENTRIES(vOtherTags):
      /* Вычисление формул вида "номер_прикр(<Роль прикр. счета>)" */
      IF ENTRY(vI, vOtherTags) BEGINS "номер_прикр" THEN
         RUN Run-PrnProc("dps_prn", iDate1, iDate2, ENTRY(vI, vOtherTags), OUTPUT vTmpStr).
      /* Вычисление формул вида "Параметр <Имя параметра>" */
      ELSE IF ENTRY(vI, vOtherTags) BEGINS "Параметр" THEN DO:
         run param.p (OUTPUT vTmpStr, 
                      iDate1, 
                      iDate2, 
                      ENTRY(2, ENTRY(vI, vOtherTags), "@")).     
         ASSIGN vTmpStr = printtext.
      END.
      ELSE DO:
         RUN VALUE(ENTRY(vI, vOtherTags)) (rid_loan,
                                           iDate2,
                                           OUTPUT vTmpStr).

         IF {&RETURN_VALUE} EQ "Err" THEN 
            vTmpStr = ?.     
      END.
      RUN Insert_TTName(ENTRY(vI, vOtherTags),
                           IF {&RETURN_VALUE} NE "Err" THEN vTmpStr 
                                                 ELSE ?).
   END.
END.




/* --- crtagval.p was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:03pm --- */
