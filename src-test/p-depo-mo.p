/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: p-depo.p
      Comment: Отчет об исполнении бухгалтерской операции
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I SetDest.I PreView.I
      Used by:
      Created: 25/12/98 19:36:48 Lera
     Modified: 19.07.2006 13:48 OZMI     (0065266)
*/
Form "~n@(#) p-depo-k.p 1.0 Lera 25/12/98 Lera 25/12/98 "
     with frame sccs-id stream-io width 250.

{globals.i}
{chkacces.i}

&GLOBAL-DEFINE FILE_sword_p YES
{pp-uni.var}
&UNDEFINE FILE_sword_p
{pp-uni.prg}
{prn-doc.def &with_proc=YES}

/*DEFINE INPUT PARAMETER iParms AS CHARACTER NO-UNDO.*/

/*-------------------- Входные параметры --------------------*/
Define Input Param RID as RecID no-undo.

/*-------------------- Объявление переменных --------------------*/
Define Buffer buf_0_op For op.

/*--------------- Буфера для полей БД: ---------------*/

/*--------------- Переменные для специальных полей: ---------------*/
Define Variable AcctCr           As Character            No-Undo.
Define Variable AcctDb           As Character            No-Undo.
/*Define Variable BankName         As Character            No-Undo.*/
Define Variable Dep              As Character            No-Undo.
Define Variable DepCr            As Character            No-Undo.
Define Variable reg-num          As Character            No-Undo.
Define Variable sum-nom          As Decimal              No-Undo.

DEFINE VARIABLE vDetails    AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE vDep        AS CHARACTER EXTENT  2 NO-UNDO.
DEFINE VARIABLE vDepCr      AS CHARACTER EXTENT  2 NO-UNDO.
DEFINE VARIABLE vDetItem    AS INT64     NO-UNDO.
DEFINE VARIABLE vDetails2   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv_1    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv_2    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtybkv3     AS CHARACTER   NO-UNDO.

Def Var FH_p-depo-1 as INT64 init 5 no-undo. /* frm_1: мин. строк до перехода на новую страницу */


/* Начальные действия */
/*{wordwrap.def}*/
{get-fmt.i &obj=D-Acct-Fmt}

/*def var AcctName as char extent 2 no-undo.*/

/*-----------------------------------------
   Проверка наличия записи главной таблицы, на которую указывает Input Param RID
-------------------------------------------*/
Find op Where RecID(op) = RID no-lock no-error.
If Not Avail(op) then do:
  message "Нет записи <op>".
  Return.
end.

/*------------------------------------------------
   Выставить buffers на записи, найденные в соответствии с заданными в отчете правилами
------------------------------------------------*/
/* Т.к. не задано правило для выборки записей из главной таблицы, просто выставим его buffer на input RecID  */
find buf_0_op where RecID(buf_0_op) = RecID(op) no-lock.

/* Вычисление значения специального поля BankName */
/*BankName = dept.name-bank.*/

/* Вычисление значения специального поля Dep */
find first op-entry of op no-lock.

find first acct where acct.acct EQ op-entry.acct-db no-lock.
vDep = acct.details.
{wordwrap.i &s=vDep &l=83 &n=2}
Dep = vDep[1] + vDep[2].

find first acct where acct.acct EQ op-entry.acct-cr no-lock.
vDepCr = acct.details.
{wordwrap.i &s=vDepCr &l=83 &n=2}
DepCr = vDepCr[1] + vDepCr[2].

    assign
      AcctDb = {out-fmt.i op-entry.acct-db Fmt}
      AcctCr = {out-fmt.i op-entry.acct-cr Fmt}
    .

/* Вычисление значения специального поля reg-num */
find first sec-code where sec-code.sec-code = op-entry.currency no-lock no-error.
if avail sec-code then reg-num = sec-code.reg-num.
                  else reg-num = "".

/*-------------------- Формирование отчета --------------------*/
/*{strtout3.i &cols=106 &option=Paged}*/
find first _user where _user._userid = op-entry.user-id no-lock.
find first code where code.class = "КодОп" and code.code = op-entry.op-cod no-lock no-error.

put skip(7).
RUN Insert_TTName("DateDoc", string(op.doc-date, "99.99.9999")).
/*put unformatted "МЕМОРИАЛЬНЫЙ ОРДЕР (ДЕПО) №                                 " string(op.doc-date, "99.99.9999") " г." skip.*/
/*put skip(3).*/
RUN Insert_TTName("AcctDb", AcctDb).
/*put unformatted "Дебет - лицевой счет: " AcctDb skip.*/
RUN Insert_TTName("Dep", Dep).
put unformatted "Депонент: " dep skip.
/*put skip(2).*/
RUN Insert_TTName("AcctCr", AcctCr).
/*put unformatted "Кредит - лицевой счет: " AcctCr skip.*/
RUN Insert_TTName("DepCr", DepCr).
/*put unformatted "Депонент: " depCr skip.*/
/*put skip(2).*/
RUN amtgend.p (op-entry.qty, 'Ж', OUTPUT qtybkv_1, OUTPUT qtybkv_2).

qtybkv_1 = SUBSTR(qtybkv_1, 1, (LENGTH(qtybkv_1) - 1)).
qtybkv1 = "(".
qtybkv2 = SUBSTR(qtybkv_1, 1, 1).
qtybkv3 = SUBSTR(qtybkv_1, 2, LENGTH(qtybkv_1)).

IF can-do('1', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") штука.".
IF can-do('2,3,4', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") штуки.".
IF can-do('5,6,7,8,9,0', STRING(SUBSTR(STRING(op-entry.qty), LENGTH(STRING(op-entry.qty)), 1))) EQ TRUE THEN qtybkv3 = qtybkv3 + ") штук.".

/*RUN amtstr2.p (op-entry.qty, "", OUTPUT qtybkv_1, OUTPUT qtybkv_2).*/
RUN Insert_TTName("Qty", op-entry.qty).

RUN Insert_TTName("QtyBkv1", qtybkv1).
RUN Insert_TTName("QtyBkv2", qtybkv2).
RUN Insert_TTName("QtyBkv3", qtybkv3).

/*put unformatted "Количество ЦБ: " op-entry.qty " (" qtybkv_1 ") шт.".*/
/*put skip(2).*/
RUN Insert_TTName("Details", buf_0_op.details).
/*put unformatted "Основание совершаемой операции:" vDetails2 SKIP.*/
IF vDetails2 EQ "" THEN
DO:
    vDetails[1] = buf_0_op.details.
    {wordwrap.i &s=vDetails &l=83 &n=10}

    IF buf_0_op.details NE "" THEN
    DO vDetItem = 1 TO 10:
/*        PUT UNFORMATTED "" vDetails[vDetItem] SKIP.*/
        IF vDetails[vDetItem] EQ "" THEN
            LEAVE.
    END.
END.
/*put skip(2).*/
RUN Insert_TTName("DocNum", op.doc-num).
/*put unformatted "Номер поручения, инициировавшего операцию: " op.doc-num skip.*/
/*put unformatted "Дата поручения, инициировавшего операцию: " string(op.doc-date, "99.99.9999") " г." skip.*/
/*put skip(1).*/
/*put unformatted "Приложение: Поручение ДЕПО на __________ листах." skip.*/
/*put skip(2).*/
/*put unformatted "Подписи: " skip.*/
/*put skip(2).*/
IF AVAIL _user 
   THEN RUN Insert_TTName("UserName", _user._user-name).
   ELSE RUN Insert_TTName("UserName", "").
/*put unformatted "Составитель                                                  " (if avail _user then _user._user-name else "") skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (наименование должности)   (личная подпись)   (фамилия, инициалы)" skip.*/
/*put unformatted "Сдал" skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (наименование должности)   (личная подпись)   (фамилия, инициалы)" skip.*/
/*put unformatted "Получил" skip.*/
/*put unformatted "              ________________________   ________________   ___________________" skip.*/
/*put unformatted "              (наименование должности)   (личная подпись)   (фамилия, инициалы)" skip.*/

/*{endout3.i &nofooter=yes}*/

RUN printvd.p("p-depo-mo", INPUT TABLE ttnames).
