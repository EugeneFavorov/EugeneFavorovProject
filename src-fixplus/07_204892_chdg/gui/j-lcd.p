{globals.i}
{intrface.get tmess}

/* +++ j-lcd.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2014 ЗАО "Банковские информационные системы"
     Filename: j-lcd.p
      Comment: Метод join  условий договоров
   Parameters: нет
         Uses: l-con(l).p
      Used by:
      Created: Илюха
     Modified:
     Modified: 19/05/2005 mitr Заявка 0046810 : Ошибка вызова метода "JOIN"
     Modified: 25/12/2005 mitr 0056115: Ошибка вызова метода "JOIN"
     Modified: 11/04/2006 GORM (58474) Переведен на динамику.
     Modified: 19/05/2009 Jadv (0077883)
*/
{joinpar.i}             /* Параметры вызова для метода join */

DEF VAR mIskl      AS LOG  NO-UNDO.
DEF VAR mSurr      AS CHAR NO-UNDO.

   /* Проверочки на наличие текущего условия и договора */
FIND FIRST loan-cond WHERE
     ROWID(loan-cond) EQ TO-ROWID(iRowId)
NO-LOCK NO-ERROR.
IF NOT AVAIL loan-cond THEN
   RETURN "-1".
FIND FIRST loan WHERE
           loan.contract  EQ loan-cond.contract
   AND     loan.cont-code EQ loan-cond.cont-code
NO-LOCK NO-ERROR.
IF NOT AVAIL loan THEN
   RETURN "-1".

ASSIGN
   mSurr = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
   mIskl = GetXattrValueEx("loan-cond", mSurr, "ИсклМес", "NO") EQ "YES"
.

RUN CreateJoinLd ("Плановый остаток",
                  "browseld",
                  "term-obl-sum",
                  "contract"         + CHR(1) + "cont-code",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code,
                  "",
                  level + 1,
                  YES).

IF CAN-DO("loan_deb_scf,loan_agr_scf,loan_agr_factor", loan.Class-Code) THEN
   RUN CreateJoin   ("Процентные ставки",
                     "lrat-fct`" +
                     loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                     YES).
ELSE
   RUN CreateJoin   ("Процентные ставки",
                     "l-rat(c)`" +
                     loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                     YES).

RUN CreateJoinLd ("Плановое погашение ссуды",
                  "browseld",
                  "term-obl-debt",
                  "contract"         + CHR(1) + "cont-code"         + CHR(1) + "since",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code + CHR(1) + STRING(loan-cond.since),
                  "",
                  level + 1,
                  YES).

RUN CreateJoinLd ("Плановые платежи %%",
                  "browseld",
                  "term-obl-per",
                  "contract"         + CHR(1) + "cont-code"         + CHR(1) + "since",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code + CHR(1) + STRING(loan-cond.since),
                  "",
                  level + 1,
                  YES).

 RUN CreateJoin  ("Плановые платежи",
                  "ps_brw`" +
                  loan-cond.contract + "," +
                  loan-cond.cont-code  + "," +
                  STRING(loan-cond.since) + ","  + "0" + "," +
                  STRING(level + 1),
                  YES).

RUN CreateJoinLd("График комиссий",
                 "browseld",
                 "term-obl-comm",
                 "contract"  + CHR(1) + "cont-code",
                 loan-cond.contract + CHR(1) + loan-cond.cont-code,
                 "",
                 level + 1,
                 YES).

RUN CreateJoin   ("Исключения из графика погашения",
                  "l-trmexc`" +
                  loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                  mIskl).


RUN CreateJoinLd("Заявки на досрочное погашение", 
                 "browseld",
                 "presched",
                 "contract"  + CHR(1) + "cont-code",
                 loan-cond.contract + CHR(1) + loan-cond.cont-code,
                 "",
                 STRING(level + 1), 
                 YES).

RUN CreateJoin   ("Дополнительные реквизиты",
                  "l_con(s)`" +
                  loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                  YES).

RUN CreateJoin("История графиков",
               "histbrow`" +
               loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
               YES).

   /* Добавим пункт меню, если по классу loan есть ДР domain-code = "term-obl" */
FIND FIRST xattr WHERE
           xattr.class-code  EQ loan.class-code
   AND     xattr.domain-code EQ "term-obl"
NO-LOCK NO-ERROR.
IF AVAIL xattr THEN
   RUN CreateJoin ("Прочие",
                   "l-termch`" +
                   loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                   YES).

IF     FGetSetting("РедГрПлГаш","","Нет") EQ "Да"
   AND loan.contract                      EQ "Кредит"
   AND LOGICAL (GetXattrInit(loan-cond.Class-Code,"СхемаПлат"),
                "Аннуитетная/Дифференцированная") EQ NO THEN
   RUN CreateJoin("Ред. графика план.гашений ссуды",
                  "to-ttdebt-brw`" +
                  loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(level + 1),
                  YES).

   /* Инклюд формирования Join меню */
{procjoin.i
    &Prefix     = "loan-cond"
    &frametitle = "'[ ДОП. СОГЛ. ]'"
    &parms      = "(loan-cond.contract,loan-cond.cont-code,loan-cond.since,level + 1)"
}

RETURN "0".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:45:48.634+04:00' */
/* $LINTFILE='j-lcd.p' */
/*prosignbavq6dOgMjQFyVv+QQaicQ*/
/* --- j-lcd.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am --- */
