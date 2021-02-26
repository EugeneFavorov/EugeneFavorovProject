/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: incjour-318p.p
      Comment: Отчет, созданный генератором отчетов
      Comment: Журнал учета сумок
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 21/02/11 10:31:07
     Modified: 15/01/2014 kraw (0245890) исправления
*/
Form "~n@(#) incjour-318p.p 1.0 RGen 21/02/11 RGen 21/02/11 [ AutoReport By R-Gen ]"
     with frame sccs-id stream-io width 250.

{globals.i}
{chkacces.i}
{prn-doc.def &with_proc=YES} 

/*-------------------- Входные параметры --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- Объявление переменных --------------------*/

Define Buffer buf_0_op               For op.
Define Buffer b-work-time            For work-time.

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
Define Variable details          As Character            No-Undo.

/*--------------- Определение форм для циклов ---------------*/

/* Форма для цикла "NameBankCycle" */
Form
         NameBank format "x(71)" at 8 skip
with frame frm_-2 down no-labels no-underline no-box width 102.

Def Var FH_incjour-318p-2 as INT64 init 1 no-undo. /* frm_2: мин. строк до перехода на новую страницу */

Def Var FH_incjour-318p-1 as INT64 init 10 no-undo. /* frm_1: мин. строк до перехода на новую страницу */


/* Начальные действия */
{wordwrap.def}
{intrface.get xclass}
{inc-nac.def new}

DEFINE VARIABLE in-date AS DATE no-UNDO.
DEFINE VARIABLE c-marsh AS CHARACTER NO-UNDO.
in-date = gend-date.

def var ost as char.
DEFINE VARIABLE mTimeRep AS INT64 NO-UNDO.
DEFINE VARIABLE mHr      AS INT64 NO-UNDO.
DEFINE VARIABLE mMin     AS INT64 NO-UNDO.
DEFINE VARIABLE mSec     AS INT64 NO-UNDO.

/*DEFINE VARIABLE mItem      AS INT64 NO-UNDO.*/
DEFINE VARIABLE mcNameBank AS CHARACTER EXTENT 5 NO-UNDO.

FIND FIRST branch WHERE 
   branch.Branch-Type EQ "00" 
   NO-LOCK NO-ERROR.
mcNameBank[1] = (IF AVAIL branch THEN (branch.name + "~n") ELSE "") + fGetSetting("Банк","","").
FIND FIRST branch WHERE
   branch.Branch-Id EQ dept.Branch
   NO-LOCK NO-ERROR.

{agr-beg.def
   &TypeDoc   = '"book"'
   &NameTitle = "---"
}

mCuBrchID = dept.Branch.

run getroute.p (gend-date,3).

if keyfunc(lastkey) = "end-error" then do:
     return.
end.

n_marsh = INT64(pick-value).

find first work-time where work-time.table-no eq n_marsh
    and work-time.tab-no eq inc_work-time NO-LOCK NO-ERROR.

c-marsh = work-time.user-id.
/*dnow = date(GetXAttrValue("work-time", string(work-time.table-no), "ent-date")).*/
dnow = work-time.beg-date.
in-date = dnow.
/*tnow = GetXAttrValue("work-time", string(work-time.table-no), "ent-time").*/

mTimeRep = TIME.

FIND FIRST code 
     WHERE code.class  EQ "ЧасПояс" AND 
           code.parent EQ "ЧасПояс" AND
           code.code   EQ mCuBrchID NO-LOCK NO-ERROR.

ASSIGN
   mSec     = mTimeRep MOD 60
   mTimeRep = (mTimeRep - mSec) / 60
   mMin     = (mTimeRep MOD 60) 
   mHr      = ((mTimeRep - mMin) / 60) + IF avail code THEN (IF code.val EQ "+" THEN 
                                                               (+ INT64(code.name)) ELSE 
                                                               (- INT64(code.name))
                                                               )
                                         ELSE ""
   tnow     = STRING(mHr, "99") + " час. " + STRING(mMin, "99") + " мин."
.
 

/*-------- old 02.09.03
def buffer xxcode for code.

find first xxcode where xxcode.code eq GetXAttrValue("work-time", string(work-time.table-no), "incas1") and
     xxcode.class eq classif-inkas.
incass = xxcode.name.
find first xxcode where xxcode.code eq GetXAttrValue("work-time", string(work-time.table-no), "incas2") and
     xxcode.class eq classif-inkas.
incass = incass + ", " + xxcode.name.
----------------*/

incass =  get-inc-name(INT64(GetXAttrValue("work-time", string(work-time.table-no), "incas1"))) + ", " + get-inc-name(INT64( GetXAttrValue("work-time", string(work-time.table-no), "incas2"))).

/* Транзакция для предотвращения дублирования информации в случае    повторного формирования после отмены на колич-ве экземпл-в */
DO ON ERROR UNDO, RETRY:  /* END. в конечных действиях */
 /* инициализация */
 {empty tt-nac}  /* сумки          */
 c-empty = "".   /* порожние сумки */

 for each op no-lock where  
                 op.doc-type eq "ИнкН" 
             and op.op-date eq gend-date:
                    {tt-nac.cr}
 end.

 for each tt-nac where tt-nac.sum_nac eq 0
      and tt-nac.n_marsh eq n_marsh:
     accumulate tt-nac.opr (count).
     c-empty = c-empty + (if c-empty eq "" then "" else ", ") + tt-nac.n_baul.
 end.

 run amtgend.p (accum count tt-nac.opr, "Ж", output c-totec, output ost).
 /* 25.06.03 old нужна только сумма прописью - кол-во порожних сумок
 c-totec = string(accum count tt-nac.opr) + " ( " + c-totec + " )".
 */

/*-----------------------------------------
   Проверка наличия записи главной таблицы,
   на которую указывает Input Param RID
-------------------------------------------*/
Find op Where RecID(op) = RID no-lock no-error.
If Not Avail(op) then do:
  message "Нет записи <op>".
  Return.
end.

/*------------------------------------------------
   Выставить buffers на записи, найденные
   в соответствии с заданными в отчете правилами
------------------------------------------------*/
/* Т.к. не задано правило для выборки записей из главной таблицы,
   просто выставим его buffer на input RecID                    */
find buf_0_op where RecID(buf_0_op) = RecID(op) no-lock NO-ERROR.

/*------------------------------------------------
   Вычислить значения специальных полей
   в соответствии с заданными в отчете правилами
------------------------------------------------*/
/* Вычисление значения специального поля c-baul */
/**/

/* Вычисление значения специального поля c-empty */
/**/

/* Вычисление значения специального поля c-number */
/**/

/* Вычисление значения специального поля c-totec */
/**/

/* Вычисление значения специального поля c-totsc */
/**/

/* Вычисление значения специального поля c-totst */
/**/

/* Вычисление значения специального поля dnow */
/**/

/* Вычисление значения специального поля dnow1 */
dnow1 = dnow.

/* Вычисление значения специального поля incass */
/**/

/* Вычисление значения специального поля NameBank */
/* */

/* Вычисление значения специального поля NMarsh */
/*Добавлен НП ИспМаршИнк. Если его значение Да,то брать для номера маршрута значение из классификатора МаршИнк*/

NMarsh = IF FGetSetting("ИспМаршИнк", "","") EQ "YES" THEN GetCode("МаршИнк",MarshId(pick-value)) ELSE STRING(n_marsh).

/* Вычисление значения специального поля sum-nac */
/**/

/* Вычисление значения специального поля sum-tot */
/**/

/* Вычисление значения специального поля SumBeg */
/**/

/* Вычисление значения специального поля tnow */
/**/

/*-------------------- Формирование отчета --------------------*/

/* Начало цикла "NameBankCycle" */
put skip(2).
/*do:
  mItem = 1.
  DO WHILE mItem < 5 AND mcNameBAnk[mItem] NE "" :
     NameBank = mcNameBAnk[mItem].
     mItem = mItem + 1.

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

{orgname318p.i
   &xxOtstup        = 20
   &CurBranchName = mBranchName
}

RUN INSERT_TTName("mcNameBank",mcNameBank[1]).

RUN BeginCircle_TTName("treb").

/* Начало цикла "baul" */
do:
  c-number = 0.
  display with frame frm_-1.
  for each tt-nac where tt-nac.n_marsh eq n_marsh
      and tt-nac.sum_nac ne 0,
      first op no-lock where RECID(op) EQ tt-nac.opr
      break by op.op: 
  
  find first b-work-time where b-work-time.table-no eq tt-nac.n_marsh
         and b-work-time.tab-no = -1
  no-lock no-error.

  c-baul = tt-nac.n_baul.
  sum-nac = tt-nac.sum_nac.
  details = IF AVAIL b-work-time THEN STRING(b-work-time.beg-date, "99.99.9999") ELSE "".
  /*find first op no-lock where recid(op) eq opr.*/
  c-number = c-number + 1 . /* op.doc-num. 25.06.03 */
  
  
  accumulate tt-nac.sum_nac (total) op.op (count).
  
  if last(op.op) 
  then 
  do:
    /* Подписи  */
    RUN GetRepFioByRef("incjour-318p_new",mCuBrchID,op.op,mCuDprID).
    RUN INSERT_TTName("user-name", mFioInRep[1] ).
    RUN INSERT_TTName("user-dolj", mPostInRep[1]).
    RUN INSERT_TTName("user-name2",mFioInRep[2] ).
    RUN INSERT_TTName("user-dolj2",mPostInRep[2]).
    RUN INSERT_TTName("user-name3",mFioInRep[3] ).
    RUN INSERT_TTName("user-dolj3",mPostInRep[3]).
    RUN INSERT_TTName("user-name4",mFioInRep[4] ).
    RUN INSERT_TTName("user-dolj4",mPostInRep[4]).

    sum-tot = accum total tt-nac.sum_nac.
  
    run amtgend.p (accum count op.op, "Ж", output c-totsc, output ost).
    /* 25.06.03 только прописью колич-во сумок
    c-totsc = string(accum count op.op) + " ( " + c-totsc + " )" .
    */
    run amtstr2.p (sum-tot, "", output c-totst[1], output ost).
    c-totst[1] = string(sum-tot) + " (" + c-totst[1] + " " + ost + "коп.)" .
  
    SumBeg = substr(c-totst[1],1,R-Index(substr(c-totst[1],1,51),' ') - 1).
    c-totst[1] = substr(c-totst[1],length(SumBeg) + 1).
    {wordwrap.i &s=c-totst &n=2 &l=83}
  end.

   RUN INSERT_TTName("c-number[treb]",STRING(c-number,">>>>>9")).
   RUN INSERT_TTName("c-baul[treb]",c-baul).
   RUN INSERT_TTName("sum-nac[treb]",STRING(sum-nac,"zzz,zzz,zzz,zz9.99")).
   RUN INSERT_TTName("details[treb]",details).
   RUN NextCircle_TTName("treb").
  end.

end.

end.

RUN EndCircle_TTName("treb").


RUN INSERT_TTName("sum-tot",STRING(sum-tot,"zzz,zzz,zzz,zz9.99")).

RUN INSERT_TTName("c-totsc",c-totsc).
RUN INSERT_TTName("SumBeg",SumBeg).
RUN INSERT_TTName("c-totsc1",c-totst[1]).
RUN INSERT_TTName("c-totsc2",c-totst[2]).
RUN INSERT_TTName("NMarsh",NMarsh).
RUN INSERT_TTName("incass",incass).
RUN INSERT_TTName("c-totec",c-totec).
RUN INSERT_TTName("c-empty",c-empty).
RUN INSERT_TTName("dnow",
                   STRING(DAY(dnow)) + " "  +
                   ENTRY(MONTH(dnow),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") +  " " +
                   STRING(YEAR(dnow))).
RUN INSERT_TTName("tnow",tnow).
RUN INSERT_TTName("dnow1",
                   STRING(DAY(dnow1)) + " "  +
                   ENTRY(MONTH(dnow1),"января,февраля,марта,апреля,мая,июня,июля,августа,сентября,октября,ноября,декабря") +  " " +
                   STRING(YEAR(dnow1))).


RUN printvd.p("cjour318p", INPUT TABLE ttnames).  

{intrface.del}