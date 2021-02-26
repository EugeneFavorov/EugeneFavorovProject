/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: str-shtr-pol-ed.p
      Comment: Редактирование полей структуры штрихкода
   Parameters:
         Uses:
      Used by:
      Created: 19.08.2009 ushd 102466
     Modified: 
*/

DEFINE INPUT PARAMETER vclass    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER in-parent AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER in-rec-id AS RECID   NO-UNDO.
DEFINE INPUT PARAMETER level     AS INT64 NO-UNDO.

{globals.i}
{intrface.get tmess}
{intrface.get xclass}

DEFINE VAR vcodeval   AS INT64 NO-UNDO.
DEFINE VAR vcodemisc1 AS INT64 NO-UNDO.
DEFINE VAR vcodemisc2 AS LOGICAL NO-UNDO.
DEFINE BUFFER xxcode FOR code.

FORM
   vcodeval
      LABEL  "Номер"
      FORMAT ">9"
      HELP   "Порядковый номер поля штрих-кода"
   vcodemisc1
      LABEL  "Знаков"
      HELP   "Количество знаков поля штрих-кода"
      FORMAT ">>9"
   vcodemisc2
      LABEL  "Контр"
      HELP   "Контрольное поле штрих-кода"
      FORMAT "Да/Нет"
   code.misc[3]
      LABEL  "Реквизит"
      HELP   "Реквизит платежа, заполняемый при считывании"
      FORMAT "x(30)"
WITH FRAME edit 1 COL 1 DOWN SIDE-LABELS TITLE COLOR BRIGHT-WHITE "".

{rec-ed.i
   &file      = code
   &surrogate = "IF AVAIL code THEN code.class + ',' + code.code ELSE ''"
   &in-title  = "ПОЛЕ СТРУКТУРЫ ШТРИХ-КОДА"
   &ef        = "str-shtr-pol-ed.uf "
   &befupd    = "str-shtr-pol-ed.bup "
   &eh        = "str-shtr-pol-ed.eh "
   &update    = "str-shtr-pol-ed.upd "
   &lookup    = "str-shtr-pol-ed.nau &class-code=vclass "
   &noxatte-ed = YES
   {&*}
}

{intrface.del}
