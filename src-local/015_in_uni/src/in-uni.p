/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: pp-uni.p
      Comment: Универсальная процедура печати инкассового поручения

      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I StrtOut3.I EndOut3.I
      Used by:
      Created: 25.10.2000 Kostik
     Modified: 29/04/2003 kraw 0016138 добавлен графический вывод
     Modified: 18/12/2003 kraw 0023749 для электронного инкассового поручения
     Modified: 12/01/2005 kraw 0040840 Модификация имени плательщика (ДР "п106н_СтатПлат")
     Modified:
*/
Form "~n@(#) pp-uni.p 1.0 Kostik 02.03.2000 Kostik 02.03.2000 Универсальные п/п" with frame sccs-id width 250.

{globals.i}                                 /* глобальные переменные         */
{chkacces.i}
&GLOB tt-op-entry yes
&SCOP TEST YES
&GLOBAL-DEFINE OFFSIGNS YES
&IF "{&TEST}" EQ "YES" &THEN
   DEF STREAM test.
   OUTPUT STREAM test TO "test.pp".
&ENDIF
&IF defined(NEW_1256) NE 0 &THEN
   &SCOP NEW_1256 YES
&ENDIF

{pp-uni.var}                                /* определение переменных        */

/* Дата отметки банка-получателя */
DEFINE VARIABLE mDateMarcRec AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrnStr-El-Doc AS CHARACTER NO-UNDO EXTENT 7 FORMAT "x(27)".
DEFINE VARIABLE mMark-El-Doc   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mSposobPoluch  AS CHARACTER NO-UNDO.
&IF DEFINED(FRM_PRN) EQ 0 &THEN
   {in-uni.frm}                                /* определение фрейма            */
&ELSE
   {{&FRM_PRN}}
&ENDIF
{pp-uni.err}                                /* сообщения об ошибках          */
{pp-uni.prg}                     /* описание стандартных процедур */

{pp-uni.chk}                                /* проверка входных данных       */

{pp-uni.run}                                /* непосредственно расчет        */
&IF DEFINED(in-el) NE 0 &THEN   /* для электронного инкассового поручения */
{{&in-el} &in-run=YES}
&ENDIF

&IF "{&TEST}" EQ "YES" &THEN
   OUTPUT STREAM test CLOSE.
&ENDIF

ASSIGN
   NameOrder  = "ИНКАССОВОЕ ПОРУЧЕНИЕ N"
   NumberForm = "0401071"
.

{nal_name.i}

{strtout3.i &cols=80 &option=Paged}  /* подготовка к выводу           */
mDateCart = DATE(GetXAttrValueEx("op", STRING(op.op), "ДатаПомещенияВКарт",?)).
mDateMarcRec = GetXAttrValueEx("op", STRING(op.op), "ДатаОтмБПОЛ", "").

mMark-El-Doc = LOGICAL(GetXAttrValueEx("op-kind",
                                       op.op-kind,
                                       "Mark-El-Doc",
                                       "нет"), "да/нет").

mSposobPoluch = GetXAttrValueEx("op",STRING(op.op),"СпособПолуч","").

IF mMark-El-Doc THEN
DO:
   mPrnStr-El-Doc[1] = REPLACE(FGetSetting("PrnStr-El-Doc", "", ""), "|", "~n").
   {wordwrap.i &s=mPrnStr-El-Doc &n=7 &l=27}
END.
{in-uni.prn}                                /* непосредственно вывод         */
&IF defined(NEW_1256) NE 0 AND defined(NO_GRAPH) EQ 0 &THEN
IF iDoc EQ 1 THEN DO:
   {lazerprn.lib &DATARETURN = 0401071.prg
              &FORM-DOC   = "71"}
END.
&ENDIF
{endout3.i  &nofooter=yes}                   /* завершение вывода             */
