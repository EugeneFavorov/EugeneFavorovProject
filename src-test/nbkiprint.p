/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ТОО "Банковские информационные системы"
     Filename: nbkiprint.p
      Comment: 
   Parameters: 
         Uses:
      Used by: 
      Created: 
     Modified: 
*/

{globals.i}
{norm.i NEW}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEFINE INPUT PARAMETER iOp     AS INT64     NO-UNDO. 
DEFINE VARIABLE        i       AS INT64     NO-UNDO. /* счетчик цикла */
DEFINE VARIABLE        mStrPar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE        tStr    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE        tPrint  AS CHARACTER NO-UNDO. /* определяет шаблон печати */ 

FOR EACH op WHERE 
         op.op   EQ iOp
     AND op-kind EQ "nbki-qry"
NO-LOCK:
  IF AVAIL op THEN
  tStr = GetXattrValueEx("op", 
                         STRING(op.op), 
                         "ЗапросНБКИ",
                         "").
  DO i = 6 TO NUM-ENTRIES(tStr, CHR(1)):
    mStrPar = ENTRY(i, tStr, CHR(1)).
    RUN Insert_TTName(i, mStrPar).
    mStrPar = "".
  END.
END.

{norm-end.i &nofil=YES &nopreview=YES}

CASE ENTRY(1, tStr, CHR(1)):
  WHEN "1" THEN
    tPrint = "zapr_ki".   /* Запрос Субъекта кредитной истории на получение кредитного отчета по своей кредитной истории */
  WHEN "2" THEN
    tPrint = "zapr_ckki".  /* Запрос субъекта кредитной истории на получение сведений из Центрального каталога кредитных историй */
  WHEN "3" THEN
    tPrint = "zapr_auto".
  WHEN "4" THEN
    tPrint = "zapr_kid". /* Запрос субъекта кредитной истории на формирование дополнительного кода/замену/аннулирование кода субъекта кредитной истории */
END CASE.  

/* Вывод данных в файл отчета */
RUN printvd.p (tPrint, INPUT TABLE ttnames).
{empty ttnames}
{intrface.del} /* Выгрузка инструментария */
