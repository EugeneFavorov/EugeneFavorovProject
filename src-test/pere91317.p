/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: dpsnach30.p 
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created:
     Modified:
*/
{globals.i}

{intrface.get crd}
{intrface.get pbase} 
{intrface.get date}
{intrface.get trans}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get xclass}
{intrface.get tparam}
{intrface.get rights}
{intrface.get data}
{intrface.get db2l}
{intrface.get count}
{intrface.get strng}
{intrface.get brnch}
{intrface.get refer}
{intrface.get cust}
{intrface.get osyst}
{intrface.get print}
{intrface.get prnvd}
{intrface.get kau}
{intrface.get parsr}
{intrface.get widg}
{initstrp.i}

{form.def}
{tmprecid.def}
{g-trans.equ}
{ksh-defs.i}

/*DEFINE INPUT PARAMETER iFilial AS CHARACTER NO-UNDO.*/


DEFINE SHARED VARIABLE auto     AS LOGICAL NO-UNDO.
DEFINE VARIABLE shFilial-defore AS CHARACTER NO-UNDO.
DEFINE VARIABLE auto-defore     AS LOGICAL NO-UNDO.
DEFINE VARIABLE mOpDate         AS DATE       NO-UNDO.
DEFINE VARIABLE mTmplID         AS INT64    NO-UNDO.
DEFINE VARIABLE mProcHdl        AS HANDLE     NO-UNDO.
DEFINE VARIABLE mQTemplate      AS HANDLE     NO-UNDO.
DEFINE VARIABLE mDebugLevel     AS INT64    NO-UNDO.
DEFINE VARIABLE mOpkind         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBreak          AS INT64    NO-UNDO.

DEFINE VARIABLE mCurrentDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCurrentFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTMPFile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLogDirDay      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mLogDirTMP      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOSCommamd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFilList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTranList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt1           AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2           AS INT64     NO-UNDO.


ASSIGN
   mFilList  = "0500"
   mTranList = "Курс91317".

ASSIGN
   auto-defore     = auto
   auto            = yes
   shFilial-defore = shFilial.

shFilial = mFilList.

RUN RunTransaction2 IN THIS-PROCEDURE(mTranList) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
DO:
   MESSAGE "Ошибка выполнения транзакции " + ENTRY(mInt2,mTranList)
      VIEW-AS ALERT-BOX ERROR.
      pick-value = "Ошибка выполнения транзакции " + ENTRY(mInt2,mTranList).
END.
      
/**/
ASSIGN
   auto     = auto-defore
   shFilial = shFilial-defore.

RETURN.

/*------------------------------------------------------------------------------
  Purpose:     Запускает транзакцию с указанным кодом
  Parameters:  iOpkind  - код транзакции
  Notes:
------------------------------------------------------------------------------*/
PROCEDURE RunTransaction2:
   DEFINE INPUT  PARAMETER iOpkind AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vRetVal       AS CHARACTER  NO-UNDO.
   
   DEFINE VARIABLE vOpDay       AS date  NO-UNDO.

   FIND FIRST op-kind WHERE op-kind.op-kind = iOpkind NO-LOCK NO-ERROR.

   IF NOT AVAILABLE op-kind THEN
   DO:
      RUN Fill-SysMes("","","-1","Транзакция с кодом [" + iOpkind +
                      "] не найдена.").
      RETURN ERROR.
   END.

   IF NOT SearchPFile(op-kind.proc)  THEN
   DO:
      RUN Fill-SysMes("","","-1","Не найдена процедура запуска транзакции [" +
                      iOpkind + "]").
      RETURN ERROR.
   END.

   IF NOT GetSurrPermission("op-kind",op-kind.op-kind,"run") THEN
   DO:
      RUN Fill-SysMes("","","-1","Пользователь" + CAPS(USERID("bisquit")) +
                      " не имеет прав для запуска транзакции [" + iOpkind + "]").
      RETURN ERROR.
   END.

                        /* Запуск метода проверки возможности запуска транзакции */
   pick-value = "".
   RUN RunClassMethod (op-kind.Class-Code,
                       "BeforeRun",
                       "","",?,
                       CHR(1) +      /* Вместо CHR(1) будут подставлены 
                                        парарметры вызова из метасхемы */
                       "op-kind," + op-kind.op-kind).
                        /* Если метод возвращает непустое значение в переменной pick-value,
                        ** то запуск запрещается */
   IF {assigned pick-value} THEN
   DO:
                        /* Выводим сообщение об ошибке. */
      RUN Fill-SysMes IN h_tmess ("", "", "-1", pick-value).
      RETURN ERROR.
   END.

   mBreak = 0.
   vOpDay = TODAY.

   TR:
   DO ON ERROR UNDO TR,RETURN ERROR
      ON STOP  UNDO TR,RETURN ERROR:
      /* Commented BY KSV: Запоминаем контекст текущей транзакции */
      {&PUSH_TSTACK}

      RUN VALUE(op-kind.proc + ".p") (vOpday,RECID(op-kind)) NO-ERROR.

      vRetVal = RETURN-VALUE.
      
      /*MESSAGE vOpDay "," vRetVal
      VIEW-AS ALERT-BOX.*/
      /* Commented BY KSV: Восстанавливаем контекст текущей транзакции */
      {&POP_TSTACK}

      CASE mBreak:
         WHEN {&BREAK-ALL-UNDO} OR WHEN {&BREAK-ALL-NOUNDO} THEN
            IF VALID-HANDLE(mProcHdl) THEN
               RUN SetBreak IN mProcHdl (mBreak) NO-ERROR.
         WHEN {&BREAK-CUR-UNDO} THEN UNDO,LEAVE.
      END CASE.

      IF vRetVal = {&RET-ERROR} THEN UNDO TR,RETURN ERROR.
   END.  /* END OF TR BLOCK */

   mBreak = 0.

END PROCEDURE.
