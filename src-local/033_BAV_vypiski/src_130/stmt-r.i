/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: stmt-r.i
      Comment: Инклюд для печати рублевых выписок
   Parameters:
         Uses:
      Used by:
      Created: 15.04.2002 Gunk
     Modified: 03.09.2002 Gunk
     Modified: 27.01.2003 17:00 DEMA     (0012939) К файлу прилиты изменения из 4.1С-DB-MAP при
                                         обновлении отчетов stmtb, stmti, stmtis в версии 4.1C
     Modified: 27.01.2003 17:26 DEMA     (0012939) Изменен порядок параметров функции FormatVipName
     Modified: 15.05.2003 15:13 DEMA     (0015663) К файлу добавлены изменения из
                                         4.1C-VTB: в выписках реализован вывод
                                         А/Я при наличии у клиента доп. реквизита
                                         АбЯщик.
     Modified: kraw (0054280) 
     Modified: 
     Modified: 04/06/2008 kraw (0086638) + 1 входной параметр iParams
*/
&if defined(in-format) = 0 &then
  &global-define in-format ->>,>>>,>>>,>>>,>>9.99
&endif
&if defined(lname) = 0 &then
  &global-define lname 40
&endif
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

def input param acctid as recid no-undo.
def input param beg like op-entry.op-date no-undo.
def input param dob like op-entry.op-date no-undo.

DEFINE INPUT PARAMETER iParams    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iPrintProc AS CHARACTER NO-UNDO.

DEFINE VARIABLE flag-prt    AS INT64    NO-UNDO.
DEFINE VARIABLE zo-prt      AS INT64    NO-UNDO.
DEFINE VARIABLE vDateAdd    AS INT64    NO-UNDO.
/* Даты заменяются в случае окончания года */
DEFINE VARIABLE mOldZo      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mOldShBal   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE mOldShInBal AS DECIMAL    NO-UNDO.
/* Переменные для работы с Cost-центрами */
DEFINE VARIABLE vCostCntr AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNazn AS CHARACTER NO-UNDO.
DEFINE VARIABLE vRef AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCustINN AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTmpName AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTmpINN AS CHARACTER NO-UNDO.

DEFINE VARIABLE iTmpDate AS DATE NO-UNDO.
/* Переменные, общие для всех и вынесенные из Header, Footer or Body */
DEFINE BUFFER bLoanAcct FOR loan-acct.
DEFINE BUFFER person    FOR person.
DEFINE BUFFER branch    FOR branch.


function FormatVipName returns char (input Mask as char, input dblMask as char, input VipName as char, input dbl as logical):
  if not dbl then
    return subst(Mask, trim(VipName)).
  else
    return subst(dblMask, trim(VipName)).
end.

find acct where recid(acct) = acctid no-lock.

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
         IF AVAIL(cust-corp) THEN 
         DO:
            RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT NAME[1]).
            NAME[2] = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "engl-name", "").
            IF NOT {assigned NAME[1]} THEN
               NAME[1] = cust-corp.inn.
            vTmpINN = ClientTempXattrVal(acct.cust-cat,acct.cust-id,"inn",dob).
            IF vTmpINN NE "" AND vTmpINN NE NAME[1]
               THEN NAME[1] = vTmpINN.
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
   IF CAN-DO(FGetSetting("ВыпискаНастройки", "ВыпНаимБанк", ""), STRING(acct.bal-acct))THEN DO:
      NAME[1] = FGetSetting("ИНН","","").
      NAME[2] = dept.name-bank.
   END.
   ELSE IF acct.cust-cat NE "В" THEN DO:
      {getcust.i &name=name &OFFsigns=yes &inn=vCustINN}
      vTmpName = IF name[1] BEGINS "ИНН " + vCustINN THEN
                     LEFT-TRIM(SUBSTRING(name[1],
                                         LENGTH("ИНН " + vCustINN) + 1))
                 ELSE
                     name[1].
      IF acct.cust-cat = 'Ю' AND
         NOT (vTmpName = acct.details AND name[2] = "")
      THEN DO:
         RUN GetCustInfo2 (13, acct.acct, acct.currency, OUTPUT NAME[1]).
         RUN GetCustNameFormatted (acct.cust-cat, acct.cust-id, OUTPUT NAME[2]).
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF AVAIL(cust-corp) THEN 
         DO:
            vTmpINN = ClientTempXattrVal(acct.cust-cat,acct.cust-id,"inn",dob).
            IF vTmpINN NE "" AND vTmpINN NE NAME[1]
               THEN NAME[1] = vTmpINN.
         END.
      END.
   END.
   ELSE IF acct.cust-cat EQ "В" THEN DO:
      IF CAN-DO(FGetSetting("ВыпискаНастройки", "ВыпНаимВнутрСчет", ""), STRING(acct.bal-acct)) OR 
       acct.details EQ ""   THEN DO:
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
           stmt.op-date GE (IF vZoBeg EQ ? THEN beg ELSE MIN (beg, vZoBeg))
       AND stmt.op-date LE (IF vZoEnd EQ ? THEN dob ELSE MAX (dob, vZoEnd))
   NO-ERROR.

FirstLoop = NOT AVAIL stmt AND ts.emptystmt.
/* 1. Получаем дату предыдущей выписки */
IF acct.acct-cat NE "d" THEN
   RUN pb_acct-pos.p (acct.acct, acct.currency, beg - 1, beg - 1, "ФБО").
ELSE
DO:
   RUN acct-qty IN h_base (acct.acct, acct.currency, beg - 1, beg - 1, ?).
   ASSIGN
      sh-in-bal = sh-in-qty
      sh-bal    = sh-qty
   .
END.

if lastmove = ? then do:  /* если есть остаток по начальному решению, но нет проводок */
  find last acct-pos of acct where acct-pos.since < beg no-lock no-error.
  if avail acct-pos then lastmove = acct-pos.since.
end.
/* Дата предыдущей выписки */
prevop = lastmove.

/* 2. Получаем входящий остаток */
IF acct.acct-cat NE "d" THEN
   RUN pb_acct-pos.p (acct.acct, acct.currency, beg, dob, "ФБО").
ELSE
DO:
   RUN acct-qty IN h_base (acct.acct, acct.currency, beg, dob, ?).
   ASSIGN
      sh-in-bal = sh-in-qty
      sh-bal    = sh-qty
   .
END.

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
         sh-in-bal   = mOldShInBal
         sh-bal      = mOldShBal
         Flag-ZO     = NO
      .
   END.
END.
/*236510 */
mOstS = (-1) * sh-in-bal. /*для пассивных счетов вкладов*/
/*Номер и дата договора*/
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
/*Исполнитель */
mOIuserid = USERID("bisquit").
FIND FIRST _user WHERE 
   _user._userid EQ mOIuserid 
NO-LOCK NO-ERROR.
IF AVAILABLE _user THEN
   mOIName = _User-Name.
/*Адрес */
FOR EACH cust-ident WHERE 
         cust-ident.cust-cat       EQ acct.cust-cat
   AND   cust-ident.cust-id        EQ acct.cust-id
   AND   cust-ident.cust-code-type EQ "АдрФакт"
   AND   cust-ident.class-code     EQ "p-cust-adr"
   AND   cust-ident.close-date     EQ ?
   NO-LOCK BY cust-ident.open-date:
      mAddress = TRIM(Replace(cust-ident.issue,"~n","")).
      LEAVE.
END.
/*Паспорт */
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
ASSIGN 
    prev-db = IF sh-in-bal GE 0 THEN STRING (  sh-in-bal, "{&in-format}") ELSE ""
    prev-cr = IF sh-in-bal LT 0 THEN STRING (- sh-in-bal, "{&in-format}") ELSE ""
    cnt     = 0{&cnt}
.

&IF DEFINED(noacctdet) &THEN
   ts.print-accs = NO.
&ENDIF

&IF DEFINED(no2col) &THEN
   ts.dwidth = NO.
&ENDIF

REPEAT i =  (IF ts.pgd AND NOT ts.dwidth THEN 1 ELSE 2)
         TO (IF ts.cmode EQ 3 AND acct.cust-cat NE "В" THEN 3 ELSE 2)
       with frame bsr down no-box width 255 :
    ASSIGN
       num-db  = 0
       num-cr  = 0
       sh-db   = 0
       sh-cr   = 0
    .
    /* проверяем связь счета со сделкой */
    FIND FIRST bLoanAcct WHERE bLoanAcct.acct = acct.acct NO-LOCK NO-ERROR.
    IF NOT AVAIL bLoanAcct
      THEN vRef = "".
      ELSE vRef = GetXAttrValue("loan", STRING(bLoanAcct.contract) + "," + STRING(bLoanAcct.cont-code), "TicketNumber"). 
    

/* проверка печати выписки в другом формате */
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
            {stmtotal.i &stmtrub="YES" &new="YES"}
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
      RELEASE stmt.
      FOR EACH stmt BREAK BY stmt.acct:
         IF stmt.acct EQ acct.acct AND LAST-OF(stmt.acct) THEN DO:
            {stmtotal.i &stmtrub="YES" &new="YES"}
         END.
      END.
   END.
END.

IF iPrintProc EQ "" THEN DO:
    /* Заголовок */
    {{&header} {&*}}
    IF i NE 1 THEN DO:
       if ts.pgd and line-counter + cnt > page-size + 1 then page.
       if firstloop then do:
          firstloop = no.
          IF NOT ts.emptystmt THEN view frame bsr.
          ELSE PUT SKIP(1).
       end.
    END.
    flag-prt = 0.
END.

&IF DEFINED(stmtmgx) NE 0 &THEN 
    IF ts.pgdevery AND cur-page-str EQ "" THEN
    cur-page-str = "Стр. 1".
&ENDIF

    FOR EACH op-date WHERE
             op-date.op-date GE Beg
         AND op-date.op-date LE Dob NO-LOCK:             
       &IF DEFINED(dbcr) EQ 0 &THEN
          {stmt-vir.i &corr=cr &this=db &beg=op-date.op-date &zo = NO {&*}}
          {stmt-vir.i &corr=db &this=cr &beg=op-date.op-date &zo = NO {&*}}
       &ELSE     
          {stmt-vir.i &corr=db &this=cr &beg=op-date.op-date &zo = NO {&*}}             
          {stmt-vir.i &corr=cr &this=db &beg=op-date.op-date &zo = NO {&*}}
       &ENDIF
    END.
    /* Выписки после строки ЗО */
    IF CAN-FIND ( FIRST stmt WHERE stmt.prev-year
                    AND stmt.op-date GE vZoBeg
                    AND stmt.op-date LE vZoEnd ) THEN DO:
       zo-prt = 0.
       FOR EACH op-date WHERE
             op-date.op-date GE vZoBeg
         AND op-date.op-date LE vZoEnd NO-LOCK:             
         &IF DEFINED(dbcr) EQ 0 &THEN             
            {stmt-vir.i &corr=cr &this=db &beg=op-date.op-date &zo = YES {&*}} 
            {stmt-vir.i &corr=db &this=cr &beg=op-date.op-date &zo = YES {&*}}
         &ELSE
            {stmt-vir.i &corr=db &this=cr &beg=op-date.op-date &zo = YES {&*}}
            {stmt-vir.i &corr=cr &this=db &beg=op-date.op-date &zo = YES {&*}}
         &ENDIF
       END.
    END.
IF iPrintProc EQ "" THEN DO:
    IF i NE 1 AND ((flag-prt = 1 AND NOT ts.emptystmt) OR ts.emptystmt) THEN DO:
       IF flag-prt EQ 0 AND ts.emptystmt THEN DISPLAY "" @ stmt.doc-num. 
       DO WITH FRAME bsrf NO-LABEL DOWN NO-BOX WIDTH 255:
          {{&footer} {&*}} 
          if ts.pgdevery then do:
            PUT SKIP(1).
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
/* $LINTFILE='stmt-r.i' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='krok' */
/* $LINTDATE='10/10/2016 11:38:13.606+03:00' */
/*prosignjHBDG7N8FGX4837N3KzMlA*/