/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1998 ТОО "Банковские информационные системы"
     Filename: showacc1.p
      Comment: процедура выбора В счетов для списания выбираются все счета с заданным шаблоном КАУ
      + Л. счета, у которых счета 2-го порядка имеют заданный ШаблКау. Должен быть в наличии
      балансовый счет
   Parameters: in-code-value - код шаблона КАУ для выборки счетов с данным значением
   (карт-ка2 и т.д )
         Uses:
      Used by:
      Created:
     Modified:  25/05/2003 koag tt-15699 Броузер переведен на navigate.cqr
*/

{globals.i}
{sh-defs.i}
{chkacces.i}
{intrface.get xclass}
{intrface.get acct}
{intrface.get crd}   /* Библиотека инструментов работы с картотекой */
{intrface.get blkob}
{intrface.get cust}
{intrface.get trans}
{intrface.get pbase}

FUNCTION GetBlockPositionAll RETURNS DECIMAL(INPUT iAcct     AS CHARACTER,
                                             INPUT iCurrency AS CHARACTER,
                                             INPUT iDate     AS DATE
                                            ) IN h_blkob.
   

DEFINE INPUT PARAMETER in-code-value LIKE code.code NO-UNDO.
DEF VAR in-bal-acct LIKE acct.bal-acct INIT ? NO-UNDO.
DEF VAR in-cust-cat LIKE acct.cust-cat INIT ? NO-UNDO.
DEF VAR in-cust-id  LIKE acct.cust-id  INIT ? NO-UNDO.
DEF VAR in-acct-cat LIKE acct.acct-cat        NO-UNDO.

DEF VAR fkau               AS CHAR FORMAT "x"              NO-UNDO.
DEF VAR name               AS CHAR FORMAT "x(39)" extent 2 NO-UNDO.
DEF VAR name-cli           AS CHAR FORMAT "x(47)"          NO-UNDO.
DEF VAR long-acct          AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR acct-long          AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR bal-acct           AS CHAR                         NO-UNDO.
DEF VAR vBal-acct          AS CHAR FORMAT "x(22)"          NO-UNDO.
DEF VAR mColor-vbal-acct   AS CHAR                         NO-UNDO.
DEF VAR str-rec-soot       AS CHAR                         NO-UNDO.
DEF VAR ind-soot           AS INT64                          NO-UNDO.
DEF VAR users              AS CHAR                         NO-UNDO.
DEF VAR mOnlyUser          AS LOG                          NO-UNDO.
DEF VAR ff-card            AS CHAR                         NO-UNDO.
DEF VAR blk-type           AS CHARACTER                    NO-UNDO.
DEF VAR blk-amt            AS decimal                      NO-UNDO.
DEF VAR blk-cust           AS CHARACTER                    NO-UNDO.


DEF VAR mAcctFltN  AS INT64 INIT 1 NO-UNDO.
DEF VAR mUsrBranch AS CHAR         NO-UNDO.

DEF VAR vAcctFlt AS CHARACTER FORMAT "X(23)" INITIAL "По счетам ОИ"
        VIEW-AS COMBO-BOX INNER-LINES 3
        LIST-ITEMS "По счетам ОИ","По счетам отделения ОИ", "По всем счетам" 
        DROP-DOWN-LIST
        NO-UNDO.



FORM
   vAcctFlt   LABEL  "Фильтр по счетам " 
              FORMAT "x(23)"
              HELP   "Фильтр по счетам картотеки блокированных счетов"
WITH FRAME frAcct OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE "Постановка на картотеку".

ON GO OF FRAME frAcct DO:

   CASE vAcctFlt:SCREEN-VALUE IN FRAME frAcct:
      WHEN "По счетам ОИ" THEN
      DO:
         mAcctFltN = 1.
      END.
      WHEN "По счетам отделения ОИ" THEN
      DO:
         mUsrBranch = GetUserBranchID(userid("bisquit")).
         mAcctFltN = 2.
      END.
      WHEN "По всем счетам" THEN
      DO:
         mAcctFltN = 3.
      END.
   END CASE.
END.




IF NUM-ENTRIES(in-code-value,"#") GT 1 THEN
DO:
   IF ENTRY(2,in-code-value,"#") EQ "ShowForm" THEN
   DO:
      UPDATE vAcctFlt
      WITH FRAME frAcct.
   END.
   ELSE 
      mAcctFltN = 0.
 
   ASSIGN
      mOnlyUser = TRUE
      in-code-value = ENTRY(1,in-code-value,"#")
   .
END.
ELSE 
   mAcctFltN = 0.


&GLOBAL-DEFINE Noacctread  YES
&GLOBAL-DEFINE user-rights YES
{kautools.lib}

ff-card    = FGetSetting("СтандТр", "findcard2", "Нет").

/* Переменные для поиска по номеру счета без черточек */
DEF VAR long-acc           AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR acct-lon           AS CHAR FORMAT "x(24)"          NO-UNDO.

DEF BUFFER xacct    FOR acct.

DEF SHARED VAR hist-rec-acct AS RECID INIT ? NO-UNDO.

{showacct.frm}

ASSIGN
   in-acct-cat = GetCodeMisc("ШаблКау",in-code-value,8)
   users = userid('bisquit') + "," + getslaves()
.

IF in-acct-cat EQ ?
THEN DO:
   {intrface.del}          /* Выгрузка инструментария. */ 
   RETURN.
END.

{acctread.i
   &bufacct=acct
   &class-code= acct.class-code
}

{showacct.qry
   &where  = "WHERE  ( (    mOnlyUser                                 ~
                        AND LOOKUP(acct.user-id,users) GT 0           ~
                        AND mAcctFltN EQ 0)                           ~
                    OR (    NOT mOnlyUser)                            ~
                    OR (    acct.user-id   EQ USERID('bisquit')       ~
                        AND mAcctFltN      EQ 1)                      ~
                    OR (    acct.branch-id EQ mUsrBranch              ~
                        AND mAcctFltN      EQ 2)                      ~
                    OR (    mAcctFltN      EQ 3)                      ~
                     )                                                ~
                AND acct.close-date EQ ?                              ~
                AND {&user-rights} "

}

{navigate.cqr
   &file          = acct
   &avfile        = "bal-acct acct"
   
   &qry           = "qry0 qry1 "
      &defquery   = "DEFINE QUERY qry0 FOR bal-acct, acct SCROLLING. ~
                     DEFINE QUERY qry1 FOR acct SCROLLING. "
      &maxoq      = 2

   &nodel         = "/*"

   &look          = "showacct.nav "
   &lookup        = "acct.nau "

   &maxfrm        = 4
   &oh2           = "│F3 Форма"
      &oth2="frames.cqr "

   &oh7           = "│F7 Поиск"
      &oth7       = "findsp.cqr
                        &POSTFIND_PROC = YES "
         &find1   = "searchsp.cqr
                        &sfld       = long-acc
                        &file-name  = NO
                        &LAB-VAR    = ""Внебалансовый Счет""
                        &metmatch   = YES
                        &metod      = MATCHES "
         &find2   = "searchsp.cqr
                        &sfld       = acct-lon
                        &file-name  = NO
                        &LAB-VAR    = ""Балансовый Счет""
                        &metmatch   = YES
                        &metod      = MATCHES "
         &find3   = "searchsp.cqr
                        &sfld       = name-cli
                        &file-name  = no
                        &lab-var    = Наименование
                        &metod      = MATCHES
                        &metmatch   = yes "

   &startup       = "shwacct1.st "

   &bff1          = "showacct.lf "
   &bff2          = "showacct.lf2 "
   &bff3          = "showacct.lf3 "
   &bff4          = "showacct.lf4 "   
   &postfind      = "shwacct1.fnd "

   &return        = "shwacct1.ret "


}

PROCEDURE Select-Query.
   ASSIGN 
      n-qry  = IF in-code-value EQ "Карт-ка1" THEN 1 ELSE 0
      n-oqry = IF in-code-value EQ "Карт-ка1" THEN 1 ELSE 0
   .
   RETURN.
END PROCEDURE.

{intrface.del}          /* Выгрузка инструментария. */ 
RETURN.