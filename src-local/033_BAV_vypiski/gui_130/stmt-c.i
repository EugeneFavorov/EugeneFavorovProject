
/* +++ stmt-c.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: STMT-C.I
      Comment: Инклюд печати валютной выписки
   Parameters:
         Uses:
      Used by:
      Created: 16.07.2002 Gunk
     Modified: 29.08.2002 Gunk Много всего
     Modified: 27.01.2003 17:01 DEMA     (0012939) К файлу прилиты изменения из 4.1С-DB-MAP при
                                         обновлении отчетов stmtb, stmti, stmtis в версии 4.1C
     Modified: 27.01.2003 17:26 DEMA     (0012939) Изменен порядок параметров функции FormatVipName
     Modified: 15.05.2003 15:13 DEMA     (0015663) К файлу добавлены изменения из
                                         4.1C-VTB: в выписках реализован вывод
                                         А/Я при наличии у клиента доп. реквизита
                                         АбЯщик.
     Modified: kraw (0054280) 
     Modified: 04/06/2008 kraw (0086638) + 1 входной параметр iParams
*/

{globals.i}
{stmt.i}
{stmt-var.i}
{sh-defs.i}
   
{intrface.get strng} 
{intrface.get cust}
{intrface.get swi}
{intrface.get db2l}
{stmt-acc.i {&*}}
{mf-loan.i}

&IF DEFINED(amt-format) EQ 0 &THEN
   &GLOBAL-DEFINE amt-format ->>>>>,>>>>>>,>>>>>9.99
&ENDIF
&IF DEFINED(in-format) = 0 &THEN
   &GLOBAL-DEFINE in-format ->>,>>>,>>>,>>>,>>9.99
&ENDIF
&IF DEFINED(lname) = 0 &THEN
  &GLOBAL-DEFINE lname 40
&ENDIF


DEFINE INPUT  PARAMETER i-acct-recid AS RECID      NO-UNDO.
DEFINE INPUT  PARAMETER beg          AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dob          AS DATE       NO-UNDO.

DEFINE INPUT PARAMETER iParams    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iPrintProc AS CHARACTER NO-UNDO.

DEFINE VARIABLE  iTmpDate AS DATE NO-UNDO.

FIND acct WHERE RECID(acct) = i-acct-recid NO-LOCK NO-ERROR.

DEFINE VARIABLE vDateAdd AS INT64    NO-UNDO.
/* Даты заменяются в случае окончания года */
DEFINE VARIABLE mOldZo      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mOldShBal   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE mOldShInBal AS DECIMAL    NO-UNDO.
DEFINE VARIABLE mOldShVal   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE mOldShInVal AS DECIMAL    NO-UNDO.

DEF VAR dbcur    LIKE op-entry.amt-cur  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR dbrub    LIKE op-entry.amt-rub  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR crcur    LIKE op-entry.amt-cur  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR crrub    LIKE op-entry.amt-rub  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR dcur     LIKE op-entry.amt-cur  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR drub     LIKE op-entry.amt-rub  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR ccur     LIKE op-entry.amt-cur  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR crub     LIKE op-entry.amt-rub  FORMAT "{&amt-format}"       NO-UNDO.
DEF VAR new-cur2 LIKE op-entry.currency INITIAL ""                   NO-UNDO.
DEF VAR new-cur1 LIKE op-entry.currency INITIAL ""                   NO-UNDO.
DEF VAR curdif   LIKE op-entry.amt-rub  FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR curdifa  LIKE op-entry.amt-rub                               NO-UNDO.
DEF VAR icur     LIKE currency.i-currency                            NO-UNDO.
DEF VAR namecur  LIKE currency.name-currenc                          NO-UNDO.

DEF VAR prrate    AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR incd      AS CHAR FORMAT "x(23)" NO-UNDO.
DEF VAR inck      AS CHAR FORMAT "x(23)" NO-UNDO.
DEF VAR inrd      AS CHAR FORMAT "x(23)" NO-UNDO.
DEF VAR inrk      AS CHAR FORMAT "x(23)" NO-UNDO.
DEF VAR strcurdif AS CHAR FORMAT "x(63)" NO-UNDO.

DEFINE VARIABLE nocurrdif AS LOGICAL     NO-UNDO.
DEFINE VARIABLE flag-prt  AS INT64     NO-UNDO.
DEFINE VARIABLE zo-prt    AS INT64     NO-UNDO.
DEFINE VARIABLE qopd      AS DATE        NO-UNDO.

DEFINE BUFFER qop-entry FOR op-entry.

/* Переменные для работы с Cost-центрами */
DEFINE VARIABLE vCostCntr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNazn AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRef AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCustINN AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTmpName AS CHARACTER NO-UNDO.

/* Переменные, общие для всех и вынесенные из Header, Footer or Body */
DEFINE BUFFER bLoanAcct FOR loan-acct.

function FormatVipName returns char (input Mask as char, input dblMask as char, input VipName as char, input dbl as logical):
  if not dbl then
    return subst(Mask, trim(VipName)).
  else
    return subst(dblMask, trim(VipName)).
end.

if acct.acct-cat = "d" then do:
   new-cur1 = acct.currency.
   rubentries = yes.  /* в депозитарии кол-во хранится в поле рублей */
end.
else do:
   find currency of acct no-lock.
   run currcode in h_base (currency.currency, output new-cur1).
   icur = currency.i-currency.
   namecur = currency.name-currenc.
end.
new-cur2 = new-cur1.

IF ts.engl_stmt THEN DO:
   CASE acct.cust-cat:
      WHEN "Ч" THEN DO:
         FIND FIRST person WHERE person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL(person) THEN DO:
            NAME[2] = GetXAttrValueEx("person", STRING(person.person-id), "engl-name", "").
            NAME[1] = person.inn.
         END.
      END.
      WHEN "Ю" THEN DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL(cust-corp) THEN DO:
            RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT NAME[1]).
            NAME[2] = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "engl-name", "").
            IF NOT {assigned NAME[1]} THEN
               NAME[1] = cust-corp.inn.

         END.
      END.
      WHEN "Б" THEN DO:
         FIND FIRST banks WHERE banks.bank-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL(banks) THEN DO:
            NAME[2] = GetXAttrValueEx("banks", STRING(banks.bank-id), "engl-name", "").
            NAME[1] = GetBankInn ("bank-id", STRING (banks.bank-id)).
         END.
      END.
   END CASE.
   IF NOT {assigned NAME[1]} AND GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да" then DO:
      NAME[1] = GetXAttrValueEx("branch", acct.branch-id, "ИНН", ?).

      IF NOT {assigned NAME[1]} THEN
         IF bank-inn <> "" THEN
            NAME[1] = bank-inn.
         ELSE 
            NAME[1] = "000000000000".
   END.  
   NAME[1] = NAME[1] + " " + NAME[2].

   FIND FIRST branch WHERE branch.branch-id EQ acct.branch-id NO-LOCK NO-ERROR.
   IF AVAIL(branch) THEN
      mEnglBranch = GetXAttrValueEx("branch", STRING(branch.branch-id), "engl_name", "").
END.
ELSE DO:
   IF CAN-DO(FGetSetting("ВыпискаНастройки", "ВыпНаимБанк", ""), STRING(acct.bal-acct)) THEN DO:
      &IF DEFINED(OFFinn) = 0 &THEN
         NAME[1] = FGetSetting("ИНН","","").
      &ELSE 
         NAME[1] = "".
      &ENDIF
      NAME[2] = dept.name-bank.
   END.
   ELSE IF acct.cust-cat NE "В" THEN DO:
      &IF DEFINED(OFFinn) = 0 &THEN
         {getcust.i &name=name &OFFsigns=yes &inn=vCustINN}
         vTmpName = IF name[1] BEGINS "ИНН " + vCustINN THEN
                        LEFT-TRIM(SUBSTRING(name[1],
                                            LENGTH("ИНН " + vCustINN) + 1))
                    ELSE
                        name[1].
      &ELSE
         {getcust.i &name=name &OFFsigns=yes &OFFinn=yes}
         vTmpName = name[1].
      &ENDIF
      IF acct.cust-cat = 'Ю' AND
         NOT (vTmpName = LEFT-TRIM(acct.details) AND name[2] = "")
      THEN DO:
         RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT NAME[1]).
         RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT NAME[2]).
      END.
   END.
  ELSE IF acct.cust-cat EQ "В" THEN DO:
      IF CAN-DO(FGetSetting("ВыпискаНастройки", "ВыпНаимВнутрСчет", ""), STRING(acct.bal-acct)) OR
         acct.details EQ "" THEN DO:
       NAME[1] = FGetSetting("ИНН","","").
       NAME[2] = dept.name-bank.
      END.
      ELSE 
         ASSIGN
         name[1] = acct.Details
         NAME[2] = "".
  END.
   NAME[1] = NAME[1] + " " + NAME[2].
END.
/* При переносе в стандартную версию следует включить *
 * функциональность из stmt-ab.i в getcust.i, а сам   *
 * файл stmt-ab.i удалить                             */
{stmt-ab.i &name=NAME[1]}

{wordwrap.i &s=NAME &l="{&lname}" &n=4}

{get-fmt.i &obj= '" + acct.acct-cat + ""-Acct-Fmt"" + "'}
long-acct = {out-fmt.i acct.acct fmt}.

{{&extra-acct-details}}

/* Проверка наличия проводок */
FIND FIRST stmt WHERE
           stmt.op-date GE ( IF vZoBeg EQ ? THEN beg ELSE MIN (beg, vZoBeg))
       AND stmt.op-date LE ( IF vZoEnd EQ ? THEN dob ELSE MAX (dob, vZoEnd))
       AND (ts.rubentries OR stmt.amt-cur NE 0)
   NO-ERROR.
IF NOT AVAILABLE stmt AND NOT ts.emptystmt THEN NEXT.
FirstLoop = NOT AVAIL stmt AND ts.emptystmt.

/*
 * Расчет qopd
 * Это дата, от которой считается курсовая разница = дате
 * последней валютной проводки или ДНР или beg - 1.
*/
find last op-entry where op-entry.op-date < beg
                     and op-entry.acct-db eq acct.acct
                     and op-entry.currency eq acct.currency
                     and op-entry.op-status >= gop-status
                     and op-entry.amt-cur ne 0
                     use-index entry-db no-lock no-error.
find last qop-entry where qop-entry.op-date < beg
                      and qop-entry.acct-cr eq acct.acct
                      and qop-entry.currency eq acct.currency
                      and qop-entry.op-status >= gop-status
                      and qop-entry.amt-cur ne 0
                      use-index entry-cr no-lock no-error.

IF ts.ruost THEN DO:
   FIND LAST op-date WHERE op-date.op-date < beg NO-LOCK NO-ERROR.
   IF AVAIL op-date THEN
      ASSIGN
         nocurrdif = YES
         qopd      = op-date.op-date
      .
END.
ELSE DO:
   nocurrdif = NO.
   qopd      = IF AVAIL op-entry THEN op-entry.op-date ELSE qopd.
   IF AVAIL qop-entry THEN
      qopd = IF qopd EQ ? THEN qop-entry.op-date
                          ELSE MAX(qop-entry.op-date, qopd).

   IF qopd = ? THEN DO:  /* если есть остаток по начальному решению, но нет проводок */
      FIND LAST acct-cur OF acct WHERE acct-cur.since < beg NO-LOCK NO-ERROR.
      IF AVAIL acct-cur THEN qopd = acct-cur.since.
      IF qopd = ? THEN qopd = beg - 1.
   END.
END.

/*
 * Расчет исходящего остатка за qopd, входящих и исходящих курсов
*/
IF acct.acct-cat NE "d" THEN
   RUN pb_acct-pos.p (acct.acct, acct.currency, qopd, qopd, "ФБО").
ELSE
DO:
   RUN acct-qty IN h_base (acct.acct, acct.currency, qopd, qopd, ?).
   ASSIGN
      sh-in-bal = sh-in-qty
      sh-bal    = sh-qty
   .
END.

if acct.acct-cat <> "d" then do:
   find last instr-rate where instr-rate.instr-code eq currency.currency
                          and instr-rate.instr-cat eq "currency"
                          and instr-rate.rate-type eq "УЧЕТНЫЙ"
                          and instr-rate.since le dob no-lock no-error.
   find last xinstr-rate where xinstr-rate.instr-code eq currency.currency
                           and xinstr-rate.instr-cat eq "currency"
                           and xinstr-rate.rate-type eq "УЧЕТНЫЙ"
                           and xinstr-rate.since <= qopd no-lock no-error.
end.

assign
   strate    = if avail instr-rate then
&IF DEFINED(stmtgw) EQ 0 &THEN
                 trim(string(instr-rate.rate-instr,">>,>>9.99<<<<")) +
&ELSE
                 trim(string(instr-rate.rate-instr,">>,>>9.9999<<")) +
&ENDIF
                 ( IF ts.engl_stmt THEN " of " ELSE " за ") + trim(string(instr-rate.per)) else ""
&IF DEFINED(stmtgw) EQ 0 &THEN
   prrate    = if avail xinstr-rate then
                 trim(string(xinstr-rate.rate-instr,">>,>>9.99<<<<")) +
                 ( IF ts.engl_stmt THEN " of " ELSE " за ") + trim(string(xinstr-rate.per)) else ""
&ELSE
   prrate    = if avail instr-rate then
                 trim(string(instr-rate.rate-instr,">>,>>9.9999<<")) +
                 ( IF ts.engl_stmt THEN " of " ELSE " за ") + trim(string(instr-rate.per)) else ""
&ENDIF
   prevop    = lastcurr
   prevop    = lastmove WHEN acct.acct-cat EQ "d"
.


/*
 * Расчет курсовой разницы с qopd + 1 по dob.
 * Расчет входящего остатка.
*/
&IF DEFINED(stmtgw) EQ 0 &THEN
for each op-entry where op-entry.op-date <= ( IF rubentries then beg - 1 else dob) and
         op-entry.op-date > qopd and
&ELSE
for each op-entry where op-entry.op-date <= dob and
         op-entry.op-date >= beg and
&ENDIF
         op-entry.acct-db eq acct.acct and
         op-entry.op-status >= gop-status and
         op-entry.currency eq acct.currency  and
         op-entry.amt-cur eq 0
         use-index entry-db no-lock:
    curdifa = curdifa + op-entry.amt-rub.
end.
&IF DEFINED(stmtgw) EQ 0 &THEN
for each op-entry where op-entry.op-date <= ( IF rubentries then beg - 1 else dob) and
         op-entry.op-date > qopd and
&ELSE
for each op-entry where op-entry.op-date <= dob and
         op-entry.op-date >= beg and
&ENDIF
         op-entry.acct-cr eq acct.acct and
         op-entry.op-status >= gop-status and
         op-entry.currency eq acct.currency and
         op-entry.amt-cur eq 0
         use-index entry-cr no-lock:
    curdifa = curdifa - op-entry.amt-rub.
end.

&IF DEFINED(stmtgw) &THEN
   IF rubentries THEN curdifa = 0.
&ENDIF

curdif = ABS(curdifa).
IF (curdif <> 0 OR NOT rubentries) AND NOT nocurrdif then
   strcurdif = ( IF ts.engl_stmt THEN
                   "Exchange difference of " + term2strEng((qopd + 1),( IF rubentries THEN beg - 1 ELSE dob))
                ELSE 
                   "Курсовая разница за " + {term2str "(qopd + 1)" "(if rubentries then beg - 1 else dob)" yes}
                )
             + ": " + trim(string(curdif,"{&in-format}"))
             + ( IF curdifa < 0 then ( IF ts.engl_stmt THEN "C" ELSE "К") 
                else if curdifa > 0 then ( IF ts.engl_stmt THEN "D" ELSE "Д")
                else "").
ELSE
   strcurdif = "".
/*
 * Расчет исходящего остатков и оборотов с beg по dob.
*/
&IF DEFINED(stmtgw) EQ 0 &THEN
IF acct.acct-cat NE "d" THEN
   RUN pb_acct-pos.p (acct.acct, acct.currency, qopd + 1, dob, "ФБО").
ELSE
DO:
   RUN acct-qty IN h_base (acct.acct, acct.currency, qopd + 1, dob, ?).
   ASSIGN
      sh-in-bal = sh-in-qty
      sh-bal    = sh-qty
   .
END.
&ELSE
IF acct.acct-cat NE "d" THEN
   RUN pb_acct-pos.p (acct.acct, acct.currency,beg,dob, "ФБО").
ELSE
DO:
   RUN acct-qty IN h_base (acct.acct, acct.currency,beg,dob, ?).
   ASSIGN
      sh-in-bal = sh-in-qty
      sh-bal    = sh-qty
   .
END.
&ENDIF
/* Учет последующих ЗО */
IF NOT vEndOfYear                                         AND
   FGetSetting ("Выписки","ОстатокЗО","Текущие") EQ "Все" AND
   NOT Flag-ZO                                            THEN
DO:

   IF Dob LT End-Of-ZO THEN
   DO:
      /* 3. Получаем входящий остаток с учетом ВСЕХ ЗО */
      ASSIGN
         Flag-ZO     = ? /* ТОЛЬКО ЗО */
         mOldShInBal = sh-in-bal
         mOldShBal   = sh-bal
         mOldShInVal = sh-in-val
         mOldShVal   = sh-val
      .
      IF acct.acct-cat NE "d" THEN
         RUN pb_acct-pos.p (acct.acct,acct.curr, Dob + 1, End-Of-Zo, "ФБО").
      ELSE
      DO:
         RUN acct-qty IN h_base (acct.acct,acct.curr, Dob + 1, End-Of-Zo, ?).
         ASSIGN
            sh-in-bal = sh-in-qty
            sh-bal    = sh-qty
         .
      END.
      
      /* Получаем движение по ЗО.
         Во входящих остатках - уже учтенные ранее проводки
         В оборотах - все проводки по ЗО */
      ASSIGN
         mOldShInBal = mOldShInBal + sh-bal - sh-in-bal
         mOldShBal   = mOldShBal   + sh-bal - sh-in-bal
         mOldShInVal = mOldShInVal + sh-Val - sh-in-Val
         mOldShVal   = mOldShVal   + sh-Val - sh-in-Val
         sh-in-bal   = mOldShInBal
         sh-bal      = mOldShBal
         sh-in-Val   = mOldShInVal
         sh-Val      = mOldShVal
         Flag-ZO     = NO
      .
   END.
END.
/* 236510 */
mOstS = (-1) * sh-in-val. /*для пассивных счетов вкладов*/
/* Номер и дата договора */
FIND FIRST bLoanAcct WHERE bLoanAcct.acct = acct.acct NO-LOCK NO-ERROR.
IF AVAIL(bLoanAcct) THEN
DO:
   FIND FIRST loan WHERE 
         loan.contract  EQ bLoanAcct.contract
   AND   loan.cont-code EQ bLoanAcct.cont-code NO-LOCK NO-ERROR.
   IF AVAIL(loan) THEN
   ASSIGN
      mNLoan = delFilFromLoan(loan.cont-code)
      mODate = STRING(loan.open-date,"99/99/9999").
END.
/* Исполнитель */
mOIuserid = USERID("bisquit").
FIND FIRST _user WHERE 
   _user._userid EQ mOIuserid 
NO-LOCK NO-ERROR.
IF AVAILABLE _user THEN
   mOIName = _User-Name.
/* Адрес */
FOR LAST cust-ident WHERE 
         cust-ident.cust-cat       EQ acct.cust-cat
   AND   cust-ident.cust-id        EQ acct.cust-id
   AND   cust-ident.cust-code-type EQ "АдрПроп"
   AND   cust-ident.class-code     EQ "p-cust-adr"
   AND   cust-ident.close-date     EQ ?
   NO-LOCK BY cust-ident.open-date:
      mAddress = TRIM(Replace(cust-ident.issue,"~n","")).
      LEAVE.
END.
/* Паспорт */
FOR EACH cust-ident WHERE 
         cust-ident.cust-cat       EQ acct.cust-cat
   AND   cust-ident.cust-id        EQ acct.cust-id
   AND   cust-ident.cust-code-type EQ "Паспорт"
   AND   cust-ident.class-code     EQ "p-cust-ident"
   AND   cust-ident.close-date     EQ ?
   NO-LOCK:
      ASSIGN
         mPodr      = GetXattrValueEx("cust-ident",
                      GetSurrogateBuffer("cust-ident",
                      (BUFFER cust-ident:HANDLE)),
                      "Подразд","")
         mPassport1 = SUBSTITUTE("&1 ВЫДАН: &2",
                      TRIM(cust-ident.cust-code),
                      TRIM(Replace(cust-ident.issue,"~n",""))) 
         mPassport2 = SUBSTITUTE("ДАТА ВЫДАЧИ: &1 КП: &2",
                      cust-ident.open-date,
                      mPodr).
      LEAVE.
END.
/* */

assign
   prev-cur = if sh-in-val >= 0 then string(  sh-in-val, "{&in-format}" + ( IF ts.engl_stmt THEN "D   " ELSE "Д   "))
                                 else string(- sh-in-val, "{&in-format}" + ( IF ts.engl_stmt THEN "C   " ELSE "К   "))
   prev-rub = if sh-in-bal >= 0 then string(  sh-in-bal, "{&in-format}" + ( IF ts.engl_stmt THEN "D   " ELSE "Д   "))
                                 else string(- sh-in-bal, "{&in-format}" + ( IF ts.engl_stmt THEN "C   " ELSE "К   "))
.

if sh-in-val >= 0 then
   incd = string(  sh-in-val, "{&in-format}").
else if sh-in-val < 0 then
   inck = string(- sh-in-val, "{&in-format}").
if sh-in-bal >= 0 then
   inrd = string(  sh-in-bal, "{&in-format}").
else if sh-in-bal < 0 then
   inrk = string(- sh-in-bal, "{&in-format}").

&IF DEFINED(EquivalentInBracket) &THEN
   IF inrd > "" THEN
      inrd = formstr("",
                     "(" + TRIM(inrd),
                     LENGTH(inrd)
                    ) + ")".
   ELSE IF inrk > "" THEN
      inrk = formstr("",
                     "(" + TRIM(inrk),
                     LENGTH(inrk)
                    ) + ")".
&ENDIF

ASSIGN
   cnt = 0{&cnt}
.
{{&initvars}{&*}}

&IF DEFINED(noacctdet) &THEN
   ts.print-accs = NO.
&ENDIF

&IF DEFINED(no2col) &THEN
   ts.dwidth = NO.
&ENDIF

REPEAT j = ( IF ts.pgd AND NOT ts.dwidth then 1 else 2)
         TO ( IF ts.cmode eq 3 AND acct.cust-cat ne "В" then 3 else 2)
   WITH FRAME bsc DOWN NO-BOX WIDTH 255:
   /* проверяем связь счета со сделкой */
   FIND FIRST bLoanAcct WHERE bLoanAcct.acct = acct.acct NO-LOCK NO-ERROR.
   IF NOT AVAIL bLoanAcct
     THEN vRef = "".
     ELSE vRef = GetXAttrValue("loan", STRING(bLoanAcct.contract) + "," + STRING(bLoanAcct.cont-code), "TicketNumber").

IF iPrintProc NE "" THEN DO:
   /*Если сортировка по дням*/
   IF ts.splitmode THEN DO:
      IF ts.emptystmt THEN DO:
         DO iTmpDate = beg TO dob:
            CREATE stmt.
            ASSIGN
               stmt.acct = acct.acct
               stmt.op-date = iTmpDate
            .
         END.
      END.
      
      FOR EACH stmt BREAK BY stmt.op-date:
         IF stmt.op-date EQ dob AND LAST-OF(stmt.op-date) THEN DO:
            {stmtotal.i &stmtval="YES" &new="YES"}
         END.
      END.   
   END.
   /*Сортировка по счетам*/
   ELSE DO:
      IF ts.emptystmt THEN DO:
         CREATE stmt.
         ASSIGN
            stmt.acct = acct.acct
         .
      END.
      FOR EACH stmt BREAK BY stmt.acct:
         IF stmt.acct EQ acct.acct AND LAST-OF(stmt.acct) THEN DO:
            {stmtotal.i &stmtval="YES" &new="YES"}
         END.
      END.
   END.
END.
IF iPrintProc EQ "" THEN DO:
   /* Заголовок */
   {{&HEADER} {&*}}
END.
   assign
      num-db = 0
      num-cr = 0
      sh-db = 0
      sh-vdb = 0
      sh-cr = 0
      sh-vcr = 0
      flag-prt = 0
   .
IF iPrintProc EQ "" THEN DO:
   IF j NE 1 THEN DO:
      IF ts.pgd AND LINE-COUNTER + cnt > PAGE-SIZE + 1 THEN PAGE.
      IF firstloop THEN DO:
         firstloop = NO.
         IF NOT ts.emptystmt THEN VIEW FRAME bsc.
         ELSE PUT SKIP(1).
      END.
   END.
END.

   FOR EACH op-date WHERE
            op-date.op-date GE Beg
        AND op-date.op-date LE Dob NO-LOCK:
         {stmt-vic.i &this=db &corr=cr &beg=op-date.op-date &zo=NO {&*}}
         {stmt-vic.i &this=cr &corr=db &beg=op-date.op-date &zo=NO {&*}}
   END.
   IF CAN-FIND ( FIRST stmt WHERE stmt.prev-year
                   and stmt.op-date ge vZoBeg
                   and stmt.op-date le vZoEnd ) THEN DO:
      zo-prt = 0.
      FOR EACH op-date WHERE
               op-date.op-date GE vZoBeg
           AND op-date.op-date LE vZoEnd NO-LOCK:
         {stmt-vic.i &this=db &corr=cr &beg=op-date.op-date &zo=YES {&*}}
         {stmt-vic.i &this=cr &corr=db &beg=op-date.op-date &zo=YES {&*}}
      end.
   END.

IF iPrintProc EQ "" THEN DO:
   IF j NE 1 AND ((flag-prt = 1 and not ts.emptystmt) or ts.emptystmt) THEN DO:
      IF flag-prt NE 1 and ts.emptystmt then DISPLAY "" @ stmt.doc-num.
      DO WITH FRAME bscf NO-LABEL DOWN NO-BOX WIDTH 255:
         {{&footer} {&*}}
         if ts.pgdevery then do:
            PUT SKIP (1).
            page.
         END.
         else {chkpage 7} else put skip(6).
      END.
   END.
END.

   &IF DEFINED(stmtgw) NE 0 &THEN
   IF ts.dwidth AND ts.dwidth THEN
      LEAVE.
   &ENDIF

END.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='15/12/2015 12:14:51.858+04:00' */
/* $LINTUSER='anba' */
/* $LINTMODE='1' */
/* $LINTFILE='stmt-c.i' */
/*prosigndcax7TY0awzBM9+07G5++w*/
/* --- stmt-c.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:30am --- */
