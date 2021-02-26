/*                      
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: incjourtemp.p
      Comment: Отчет, созданный генератором отчетов
      Comment: Журнал учета сумок
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 21/02/11 10:31:07
     Modified:
*/
Form "~n@(#) incjour-318p.p 1.0 RGen 21/02/11 RGen 21/02/11 [ AutoReport By R-Gen ]"
     with frame sccs-id stream-io width 250.

{globals.i}
{sh-defs.i}
{chkacces.i}
/*-------------------- Входные параметры --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- Объявление переменных --------------------*/

Define Buffer buf_0_op               For op.

/*--------------- Буфера для полей БД: ---------------*/

/*--------------- Переменные для специальных полей: ---------------*/
Define Variable c-baul           As Character            No-Undo.
Define Variable c-empty          As Character            No-Undo.
Define Variable c-number         As INT64              No-Undo.
Define Variable c-totec          As Character            No-Undo.
Define Variable c-totsc          As Character            No-Undo.
Define Variable c-totst          As Character Extent   2 No-Undo.
Define Variable dnow             As Date                 No-Undo.
Define Variable dnow1            As Date                 No-Undo.
Define Variable incass           As Character            No-Undo.
Define Variable NameBank         As Character            No-Undo.
Define Variable NMarsh           As Character            No-Undo.
Define Variable sum-nac          As Decimal              No-Undo.
Define Variable sum-tot          As Decimal              No-Undo.
Define Variable SumBeg           As Character            No-Undo.
Define Variable tnow             As Character            No-Undo.
Define Variable prim             As Character            No-Undo.

/*--------------- Определение форм для циклов ---------------*/
/* Форма для цикла "NameBankCycle" */
/*Form
         NameBank format "x(71)" at 8 skip
with frame frm_-2 down no-labels no-underline no-box width 102.

Def Var FH_incjour-318p-2 as INT64 init 1 no-undo. /* frm_2: мин. строк до перехода на новую страницу */

/* Форма для цикла "baul" */
Form
         "│" at 8 c-number format ">>>>>9" at 9 " │" at 15 c-baul format "x(12)" at 19 "  │" at 31 sum-nac format "zzz,zzz,zzz,zz9.99" at 37 "     │" at 45 prim format "x(10)" "  │" at 55 skip
Header
                                        "ЖУРНАЛ" at 39 skip(1)
                        "учета принятых сумок и порожних сумок" at 23 skip(1)
                               /* "Лист ___ Листов ____" at 31 skip*/
         "┌───────┬────────────────┬──────────────────────────┬──────────────────┐" at 8 skip
         "│N п/п  │     Номера     │   Суммы по накладным к   │  Примечание      │" at 8 skip
         "│       │ принятых сумок │     сумкам (цифрами)     │                  │" at 8 skip
         "├───────┼────────────────┼──────────────────────────┼──────────────────┤" at 8 skip
with frame frm_-1 down no-labels no-underline no-box width 102.

Form
         "└───────┴────────────────┴──────────────────────────┴──────────────────┘" at 8 skip
with frame frm_1 down no-labels no-underline no-box width 102.
Def Var FH_incjour-318p-1 as INT64 init 10 no-undo.*/ /* frm_1: мин. строк до перехода на новую страницу */


/* Начальные действия */
{wordwrap.def}
{intrface.get xclass}
{inc-nac.def new}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{def-wf.i new}
{defframe.i new}
{tmprecid.def}
{ttretval.def}

DEFINE VARIABLE in-date AS CHARACTER no-UNDO.
DEFINE VARIABLE c-marsh AS CHARACTER NO-UNDO.

def var ost as char.
DEFINE VARIABLE mTimeRep AS INT64 NO-UNDO.
DEFINE VARIABLE mHr      AS INT64 NO-UNDO.
DEFINE VARIABLE mMin     AS INT64 NO-UNDO.
DEFINE VARIABLE mSec     AS INT64 NO-UNDO.

DEFINE VARIABLE i          AS INT64 NO-UNDO.
DEFINE VARIABLE mcNameBank AS CHARACTER EXTENT 5 NO-UNDO.

DEF TEMP-TABLE ttTemp NO-UNDO
	FIELD vNumM  AS CHAR
	FIELD vNumS  AS CHAR
	FIELD vSum   AS DEC
	FIELD vDateM AS DATE.
	
DEF VAR vTmpNum1 AS CHAR.
DEF VAR vTmpNum2 AS CHAR.
DEF VAR vTmpNumM AS CHAR.
DEF VAR vOpDate1 AS DATE. 
DEF VAR vTSumN   AS CHAR.
DEF VAR vTSumI   AS CHAR.

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"
&GLOB Days "первое,второе,третье,четвертое,пятое,шестое,седьмое,восьмое,девятое,десятое,одиннадцатое,двенадцатое,тринадцатое,четырнадцатое,~
пятнадцатое,шестнадцатое,семнадцатое,восемнадцатое,девятнадцатое,двадцатое,двадцать первое,двадцать второе,двадцать третье,~
двадцать четвертое,двадцать пятое,двадцать шестое,двадцать седьмое,двадцать восьмое,двадцать девятое,тридцатое,тридцать первое"

/*FIND FIRST tmprecid NO-LOCK NO-ERROR.
FIND FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK NO-ERROR.
	IF AVAIL(op) THEN vOpDate1 = op.op-date.*/
	
{getdate.i}

RUN g-prompt.p('CHAR',
               '№ маршрута',
               'x(6)',
               '?',
               'ВВЕДИТЕ № МАРШРУТА',
               30,
               '',
               '',
               ?,?,OUTPUT vTmpNumM).
			   
FOR EACH op-entry
 WHERE op-entry.filial-id EQ shFilial 
   AND op-entry.acct-db begins '20202810505001000000'
   AND op-entry.op-date GE end-date - 7
   AND op-entry.op-date LE end-date + 14 /*праздники*/
    NO-LOCK,
FIRST op OF op-entry WHERE op.op-kind BEGINS '0303i' 
					 AND   op.doc-date EQ end-date 
					 AND   op.op-status NE 'А' NO-LOCK:
	vTmpNum1 = GetXattrValue('op',string(op.op),'НомерМаршрута').
	vTmpNum2 = GetXattrValue('op',string(op.op),'Номер_сумки').
	IF vTmpNum1 EQ vTmpNumM THEN DO:
		FIND FIRST ttTemp 
			WHERE ttTemp.vNumM  EQ vTmpNum1
			AND   ttTemp.vDateM EQ op.doc-date 
			AND   ttTemp.vNumS  EQ vTmpNum2 NO-ERROR.
		IF NOT AVAIL(ttTemp) THEN DO:
			vTSumN = GetXattrValue('op',string(op.op),'СуммаНедостачи').
			vTSumI = GetXattrValue('op',string(op.op),'СуммаИзлиш').
			CREATE ttTemp.
			ASSIGN 
				ttTemp.vNumM  = vTmpNum1
				ttTemp.VNumS  = vTmpNum2
				ttTemp.vSum   = op-entry.amt-rub + (IF vTSumN NE '' THEN DEC(vTSumN) ELSE 0) - (IF vTSumN NE '' THEN DEC(vTSumI) ELSE 0)
				ttTemp.vDateM = op.doc-date
			.
		END.
		ELSE ttTemp.vSum = ttTemp.vSum + op-entry.amt-rub.
	END.
END.

mcNameBank[1] = 'Омский филиал ПАО "Плюс Банк"'.
/*FIND FIRST branch WHERE 
   branch.Branch-Type EQ "00" 
   NO-LOCK NO-ERROR.
mcNameBank[1] = (IF AVAIL branch THEN (branch.name + "~n") ELSE "") + fGetSetting("Банк","","").
FIND FIRST branch WHERE
   branch.Branch-Id EQ dept.Branch
   NO-LOCK NO-ERROR.
mcNameBank[1] = mcNameBank[1] + (IF AVAIL branch THEN ("~n" + branch.name) ELSE "").*/

{wordwrap.i
 &s = mcNameBank
 &n = 5
 &l = 71
 &centered = YES
 }

/* Установим признак того, что это не просто персон, а инкассатор */
/*RUN SetSysConf IN h_base("DRIncass","YES").

RUN browseld.p ("person",
               "sc-1" + chr(1) + "sv-1" + chr(1) + "crClass-Code" + chr(1) + "formmeth" + chr(1) + "RetRcp" + chr(1) + "RetFld" + chr(1) + "RetType",
               "incass" + chr(1) + "Да" + chr(1) + "*" + chr(1) + "U1" + chr(1) + STRING(TEMP-TABLE ttRetVal:HANDLE) + chr(1) + "person-id" + chr(1) + "Multi",
                ?,
               4).
IF KEYFUNCTION(LASTKEY) NE 'END-ERROR' THEN DO:
	FOR EACH ttRetVal:
		FIND FIRST person 
			WHERE STRING(person.person-id) EQ ttRetVal.PickValue
		NO-LOCK NO-ERROR.
		IF AVAIL(person) THEN
			incass = incass + ', ' + person.name-last + ' ' + person.first-names.
	END.
END.

/* Уберем признак того, что это не просто персон, а инкассатор */
RUN SetSysConf IN h_base ("DRIncass", "?").*/

dnow = TODAY.
in-date = STRING(DAY(dnow),"99") + " " + ENTRY(MONTH(dnow),{&Months}) + " " + STRING(YEAR(dnow)) + " г.".
mTimeRep = TIME.
ASSIGN
   mSec     = mTimeRep MOD 60
   mTimeRep = (mTimeRep - mSec) / 60
   mMin     = mTimeRep MOD 60
   mHr      = (mTimeRep - mMin) / 60 + 3
   tnow     = STRING(mHr, "99") + " час. " + STRING(mMin, "99") + " мин."
.   

/* Транзакция для предотвращения дублирования информации в случае    повторного формирования после отмены на колич-ве экземпл-в */
DO ON ERROR UNDO, RETRY:  /* END. в конечных действиях */

 /* 25.06.03 old нужна только сумма прописью - кол-во порожних сумок
 c-totec = string(accum count tt-nac.opr) + " ( " + c-totec + " )".
 */

/*-------------------- Формирование отчета --------------------*/
{strtout3.i &cols=102 &option=Paged}

put unformatted "                                                                                 ┌───────────────────┐" skip.
put unformatted "                                                                                 │     Код формы     │" skip.
put unformatted "                                                                                 │ документа по ОКУД │" skip.
put unformatted "                                                                                 ├───────────────────┤" skip.
put unformatted "                                                                                 │      0402301      │" skip.
put unformatted "                                                                                 └───────────────────┘" skip.

/* Начало цикла "NameBankCycle" */
put skip.
put unformatted "              " mcNameBAnk[1] skip.
/*do:
  i = 1.
  DO WHILE i < 5 AND mcNameBAnk[i] NE "" :
     NameBank = mcNameBAnk[i].
     i = i + 1.

  Disp
    NameBank
  with frame frm_-2.
  if Line-Count + FH_incjour-318p-2 >= Page-Size and Page-Size <> 0 then do:
    Page.
  end.
  else
    Down with frame frm_-2.
  end.
end.*/
/* Конец цикла "NameBankCycle" */

PUT UNFORMATTED
   FILL('─', 101) SKIP
   "полное фирменное (сокращенное фирменное) наименование"
   " кредитной организации или полное (сокращенное) " SKIP
   "наименование филиала, или наименование"
   " и (или) номер ВСП (при наличии) либо иные идентифицирующие " SKIP
   "            признаки ВСП (при"
   " отсутствии наименования и номера)  с указанием на его " SKIP
   "                       принадлежность кредитной"
   " организации (филиалу)" SKIP.

put skip(1).

put unformatted "                                               ЖУРНАЛ" skip.
put unformatted "                               учета принятых сумок и порожних сумок" skip.
put unformatted "             ┌───────┬────────────────┬──────────────────────────┬──────────────────┐" skip
                "             │N п/п  │     Номера     │   Суммы по накладным к   │  Примечание      │" skip
                "             │       │ принятых сумок │     сумкам (цифрами)     │                  │" skip.
/* Начало цикла "baul" */
do:
  c-number = 0.
  for each ttTemp where ttTemp.vNumM eq vTmpNumM
  break by ttTemp.vNumS: 
  
  c-baul = ttTemp.vNumS.
  sum-nac = ttTemp.vSum.
  c-number = c-number + 1 .
  prim = STRING(ttTemp.vDate,"99.99.9999").  
  
  accumulate ttTemp.vSum (total) ttTemp.vNumS (count).
  
  if last(ttTemp.vNumS) 
  then 
   do:
    sum-tot = accum total ttTemp.vSum.
  
    run amtgend.p (accum count ttTemp.vNumS, "Ж", output c-totsc, output ost).
    run amtstr2.p (sum-tot, "", output c-totst[1], output ost).
    c-totst[1] = string(sum-tot,"zzz,zzz,zz9.99") + " (" + c-totst[1] + " " + ost + "коп.)" .
  
    SumBeg = substr(c-totst[1],1,R-Index(substr(c-totst[1],1,51),' ') - 1).
    c-totst[1] = substr(c-totst[1],length(SumBeg) + 1).
    {wordwrap.i &s=c-totst &n=2 &l=83}
   end.
put unformatted "             ├───────┼────────────────┼──────────────────────────┼──────────────────┤" skip.
put unformatted "             │ " c-number Format ">>>9" "  │ " c-baul Format "x(14)" " │   " sum-nac Format "zzz,zzz,zzz,zz9.99"
                "     │    " prim Format "x(10)" "    │" skip.
  end.
end.
/* Конец цикла "baul" */

put unformatted "             ├───────┼────────────────┼──────────────────────────┼──────────────────┤" skip.
put unformatted "             │ИТОГО  │ "c-number Format ">>>9" "           │   " sum-tot Format "zzz,zzz,zzz,zz9.99"
                "     │                  │" skip.
put unformatted "             └───────┴────────────────┴──────────────────────────┴──────────────────┘" skip.
put unformatted FILL('-', 100) skip.
put unformatted "  Опломбированные сумки с наличными деньгами в количестве " c-totsc Format "x(27)"
                "" skip.
put unformatted "                                                      (прописью)" skip.
put unformatted "  штук на объявленную сумму наличных денег: " SumBeg Format "x(54)"
                "" skip.
put unformatted "  " c-totst[1] Format "x(83)"
                "" skip.
put unformatted "  " c-totst[2] Format "x(83)"
                "" skip.
put unformatted "  ________________________________________________________________________________________________" skip.
put unformatted "                                                 (цифрами и прописью)" skip.
put unformatted "  по накладным к сумкам с наличными деньгами;" skip.
put unformatted "  наличные деньги, пересчитанные из поврежденных сумок ___________________________________________" skip.
put unformatted "                                                            (цифрами и прописью)" skip.
put unformatted " " skip.
put unformatted "  ________________________________________________________________________________________________" skip.
put unformatted " " skip.
put unformatted "  по маршруту(заезду) № " vTmpNumM Format "x(10)"
                " приняты от инкассаторских работников:" skip.
put unformatted "  " /*incass*/ Format "x(70)"
                "" skip.
put unformatted "  ────────────────────────────────────────────────────────────────────────────────────────────────" skip.
put unformatted "  Кроме того, от инкассаторских работников принято порожних сумок " /*c-totec Format "x(30)"*/
                "" skip.
put unformatted "  за номерами " /*c-empty Format "x(83)"*/
                "" skip.
put unformatted " " skip.
put unformatted FILL('-', 100) skip.
put unformatted "  Опломбированные сумки с наличными деньгами в количестве __________________________________ штук." skip.
put unformatted "                                                                    (прописью)" skip.
put unformatted "  на объявленную сумму наличных денег ____________________________________________________________" skip.
put unformatted "                                                              (цифрами и прописью)" skip.
put unformatted "  ____________________________________________________ по накладным к сумкам с наличными деньгами;" skip(1).
put unformatted "  наличные деньги, пересчитанные из поврежденных сумок ___________________________________________" skip.
put unformatted "                                                                  (цифрами и прописью)" skip.
put unformatted "  ________________________________________________________________________________________________" skip(1).
put unformatted "  приняты от представителя организации ___________________________________________________________" skip.
put unformatted "                                        (наименование орг-ции, ф.и.о. представителя)" skip.
put unformatted "  ________________________________________________________________________________________________" skip(1).
put unformatted "  Количество и номера сданных инкассаторскими работниками сумок с наличными деньгами соответствует" skip.
put unformatted "  количеству и номерам по записям в явочных карточках и накладных к сумкам с наличными деньгами." skip.
/*put unformatted "  количество и номера сданных организацией сумок с наличными деньгами соответствуют количеству и" skip.
put unformatted "  и номерам по записям в накладных и квитанциях к сумкам с наличными деньгами (ненужное зачеркнуть)" skip.*/
put skip(1).
put unformatted "      Старший кассир                                         Задворнова С.А." skip.                               
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (наименование должности)       (личная подпись)        (фамилия и инициалы)" skip.
put skip(1).
put unformatted "  Сдали сумки в кредитной организации:" skip.
put skip(1).
put unformatted "      Инкассатор" skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "    (наименование должности)       (личная подпись)        (фамилия и инициалы)" skip.
put skip(1).
put unformatted "  Приняли сумки в кредитной организации:" skip.
put skip(1).
put unformatted "      Старший кассир                                         Задворнова С.А." skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (наименование должности)       (личная подпись)        (фамилия и инициалы)" skip.
put skip(1).
put unformatted "  Дата " in-date 
                "  " tnow Format "x(30)"
                "" skip.
put skip(1).
put unformatted "  Подразделение инкассации:" skip(1).
put unformatted "      Инкассатор" skip.
put unformatted "  ______________________________ ______________________  __________________________" skip.
put unformatted "     (наименование должности)       (личная подпись)        (фамилия и инициалы)" skip.
put unformatted " " skip.
put unformatted "  Дата " in-date 
                "" skip.

/* Конечные действия */
END.


{endout3.i &nofooter=yes}
/* $LINTUSER='STRE' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTDATE='22/10/2014 12:08:18.686+04:00' */
/* $LINTFILE='incjour-318p.p' */
/*prosignS4sU6FF3uBUXUq78uIXfJA*/