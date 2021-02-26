/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2013 ТОО "Банковские информационные системы"
     Filename: showaccm.p
      Comment: процедура выбора В счетов для списания выбираются все счета с заданным шаблоном КАУ
               + Л. счета, у которых счета 2-го порядка имеют заданный ШаблКау. Должен быть в наличии
               балансовый счет
   Parameters: in-code-value - код шаблона КАУ для выборки счетов с данным значением
               (карт-ка2 и т.д )
         Uses:
      Used by:
      Created:
*/

{globals.i}
{sh-defs.i}
{chkacces.i}
{intrface.get xclass}
{intrface.get acct}
{intrface.get blkob}
{intrface.get crd}   
{flt-file.i}

DEFINE INPUT PARAMETER in-code-value LIKE code.code NO-UNDO.
DEF VAR in-bal-acct LIKE acct.bal-acct INIT ? NO-UNDO.
DEF VAR in-cust-cat LIKE acct.cust-cat INIT ? NO-UNDO.
DEF VAR in-cust-id  LIKE acct.cust-id  INIT ? NO-UNDO.
DEF VAR in-acct-cat LIKE acct.acct-cat        NO-UNDO.

DEF VAR name               AS CHAR FORMAT "x(39)" extent 2 NO-UNDO.
DEF VAR name-cli           AS CHAR FORMAT "x(47)"          NO-UNDO.
DEF VAR long-acct          AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR acct-long          AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR bal-acct           AS CHAR                         NO-UNDO.
DEF VAR vBal-acct          AS CHAR                         NO-UNDO.
DEF VAR blk-acct           AS CHAR                         NO-UNDO.
DEF VAR mColor-vbal-acct   AS CHAR                         NO-UNDO. 
DEF VAR mAcctColors        AS CHAR                         NO-UNDO. 
DEF VAR mAcctStat          AS CHAR                         NO-UNDO. 
DEF VAR vBlockAcctType     AS CHAR                         NO-UNDO. 
DEF VAR users              AS CHAR                         NO-UNDO.
DEF VAR mOnlyUser          AS LOG                          NO-UNDO.
DEF VAR ff-card            AS CHAR                         NO-UNDO.
DEF VAR view_mode          AS INT64                        NO-UNDO.
DEF VAR mBrwFltMode        AS INT64                        NO-UNDO. 
DEF VAR vBlkRecID          AS RECID                        NO-UNDO.
DEF VAR vBlkSum            AS DECIMAL                      NO-UNDO.
DEF VAR ii                 AS INT64                        NO-UNDO.

IF NUM-ENTRIES(in-code-value,"#") GT 1 THEN
   ASSIGN
      mOnlyUser = TRUE
      in-code-value = ENTRY(1,in-code-value,"#")
      view_mode = 0
   .
&GLOBAL-DEFINE Noacctread  YES
&GLOBAL-DEFINE user-rights YES
{kautools.lib}

ff-card = FGetSetting("СтандТр", "findcard2", "Нет").
mBrwFltMode = 0.
mAcctColors  = fGetSetting ("AcctPosColors","BAs", "white") + "," + 
               fGetSetting ("AcctPosColors","BAa", "white").
DO ii = 1 TO NUM-ENTRIES(mAcctColors):
   IF TRIM(ENTRY(ii,mAcctColors)) = "" 
      THEN ENTRY(ii,mAcctColors) = "white".
END.

/* Переменные для поиска по номеру счета без черточек */
DEF VAR long-acc           AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR acct-lon           AS CHAR FORMAT "x(24)"          NO-UNDO.

DEF BUFFER xacct     FOR acct.

DEF SHARED VAR hist-rec-acct AS RECID INIT ? NO-UNDO.

{showacct.frm &ENERGO-FRM-OFF=YES}
{shwacctm.frm} /* Новая форма */

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
   &where  = "WHERE (  (    mOnlyUser                        ~
                        AND LOOKUP(acct.user-id,users) GT 0) ~
                    OR (    NOT mOnlyUser                    ~
                        AND CAN-DO(users,acct.user-id)))     ~
               AND acct.close-date EQ ? ~
               AND {&user-rights} "
}


{navigate.cqr
   &file          = acct
   &postfind      = "shwacctm.fnd "

   &avfile        = "bal-acct acct "
   
   &qry           = "qry0 qry1 "
      &defquery   = "DEFINE QUERY qry0 FOR bal-acct, acct SCROLLING. ~
                     DEFINE QUERY qry1 FOR acct SCROLLING. "
      &maxoq      = 2

   &nodel         = "/*"

   &look          = "shwacctm.nav "
   &lookup        = "acct.nau " 

   &maxfrm        = 4
   &first-frm     = 4

   &oh6            = "│F6 фильтр"
   &oth6           = "shwacctm.f6 "

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
   &bff4          = "showacct.lfm "
   &bf4           = "Bal-Acct vBal-Acct blk-acct " 
   &cf4           = "acct-long blk-acct " 
   
   &return        = "shwacct1.ret "
   &open-query    = "pOpenQuery"
   &NavJoinFile   = "xacct"
}

PROCEDURE Select-Query.
   ASSIGN 
      n-qry  = IF in-code-value EQ "Карт-ка1" THEN 1 ELSE 0
      n-oqry = IF in-code-value EQ "Карт-ка1" THEN 1 ELSE 0
   .
   RETURN.
END PROCEDURE.

PROCEDURE pOpenQuery.
   IF n-oqry = 0 THEN DO:
     RELEASE bal-acct.
     {&oqry0}
     h_query = query qry0:handle.
   END.
   ELSE DO:
     {&oqry1}
     h_query = query qry1:handle.
   END.
   RELEASE acct.
   IF mBrwFltMode = 1 THEN DO:
      {for_dqry.i h_query}
         RUN fdbacct( BUFFER acct, ff-card,in-code-value).
         FIND FIRST ttKau NO-LOCK NO-ERROR.
         FIND FIRST xacct WHERE RECID(xacct) EQ ttKau.fRecId NO-LOCK NO-ERROR.
         IF AVAIL xacct THEN DO:
            FIND LAST acct-pos OF xacct WHERE acct-pos.since LE gend-date 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL acct-pos OR acct-pos.balance = 0 THEN 
               h_query:DELETE-RESULT-LIST-ENTRY ().
         END.
         ELSE 
            h_query:DELETE-RESULT-LIST-ENTRY ().
      END. 
   END.
   IF mBrwFltMode = 2 THEN DO:
      {for_dqry.i h_query}
         FIND LAST acct-pos OF acct WHERE acct-pos.since LE gend-date 
         NO-LOCK NO-ERROR.
         IF NOT AVAIL acct-pos OR acct-pos.balance = 0 THEN 
            h_query:DELETE-RESULT-LIST-ENTRY ().
      END. 
   END.
END PROCEDURE.

{intrface.del}          /* Выгрузка инструментария. */ 
RETURN.
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='14/11/2014 13:55:03.989+04:00' */
/* $LINTFILE='shwacctm.p' */
/*prosignh+hjZcFb3sUhfrenVYx+CQ*/