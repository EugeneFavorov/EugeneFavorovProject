/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename:  serv-imp
      Comment:  Сервер обработки сообщений. Импорт из каталогов.
         Uses:  -
      Used by:  run-serv
      Created:  24/10/1997 Mike
     Modified:
*/

def var ok as logical no-undo.
def var in-op-date like op.op-date.
{globals.i}
{intrface.get xclass}
{intrface.get tmess}
{os-dir.pro}
{tmailusr.def}
{mimfile.i}


DEFINE STREAM FileListin.
DEFINE VAR file-name    AS CHAR              NO-UNDO.
DEFINE VAR buf          AS CHAR              NO-UNDO. /* путь к файлу         */
DEFINE VAR mExchTime    AS CHAR              NO-UNDO.
DEFINE VAR mOK          AS LOGICAL           NO-UNDO.

DEFINE VARIABLE mLockDate AS DATE        NO-UNDO.

ASSIGN
   auto       = YES.

/*----------------- Цикл по автоматичским правилам обмена кроме Клиент-Банк --*/
catalog:
FOR EACH  tt-mail-user WHERE tt-mail-user.mode EQ "imp" NO-LOCK,
    FIRST op-kind      WHERE op-kind.op-kind EQ tt-mail-user.op-kind-imp
                       NO-LOCK:
   mOk = NO.
   RUN CheckTime(INPUT  op-kind.op-kind,
                 INPUT  tt-mail-user.mail-user-num,
                 INPUT-OUTPUT mOk).
   IF NOT mOk THEN NEXT.


   {gdateimp.i &no-chk-defopdate=YES &rel-date = mLockDate}


   RUN read-dir (tt-mail-user.dir,"*","filelist.imp").

   IF SEARCH("./filelist.imp") EQ ? THEN NEXT catalog.
   IF NOT CAN-FIND( FIRST tt-file-lst
                    WHERE tt-file-lst.file-name NE ""
                      AND tt-file-lst.file-name NE ?)
          THEN NEXT catalog.

    RUN Init-SysMes ("AUTO," + op-kind.op-kind,   
                    "",               
                    "").               /* Инициализация протокола   */

                                       /* Для многофилиальной базы */
   IF shMode THEN DO:
      shFilial = tt-mail-user.filial-id.
      RUN SetEnvironment IN h_base (shFilial).
   END. 

BEGIN:
   DO ON ERROR UNDO BEGIN, LEAVE BEGIN:
      IF SearchPFile(op-kind.proc) THEN
         RUN VALUE(op-kind.proc + ".p") (INPUT in-op-date,
                                         INPUT recid(op-kind)) NO-ERROR.
   END.

   {rel-date.i &in-op-date = mLockDate}
   OS-DELETE VALUE("./filelist.imp").
   
   RUN End-SysMes.                               /* Завершение  протокола     */

END.                                             /* FOR EACH tMailUser WHERE  */

ASSIGN
  auto     = NO.
/******************************************************************************/

