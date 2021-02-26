/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename: ACCT().P
      Comment: процедура просмотра л/с
   Parameters:
         Uses:
      Used BY:
      Created: ??? ??/??/????
     Modified: Om 21/10/2004
     Modified: grab 41465 - JOIN вызывается из метасхемы
     Modified: Om 14/05/2005 Доработка.
                        Для счетов юридических лиц отображать в наименовании
                        счета краткое название юридического лица.
     Modified: 21.01.2010 19:19 ksv      (0121399) + &qbisxtrafields
*/

{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i}            /* Определение структуры динамического фильтра. */
{navigate.def}          /* Переменные для navigate.cqr. */
{xsignrat.def}          /* Переменные для альтернативной формы просмотра ДР. */
{chkacces.i}            /* Проверка доступа к клиентам (getcust.i). */
{sh-defs.i}             /* Переменные для формирования остака по счетам. */
{ttretval.def}

{intrface.get acct}     /* Библиотека для работы со счетами. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */
{intrface.get separate} /* Библиотека для работы с категориями. */
{intrface.get widg}     /* Библиотека для работы с виджетами. */
{intrface.get dynqr}    /* Библиотека для работы с динамическими запросами. */
{intrface.get op}
{intrface.get navst}

DEF VAR name        AS CHAR   NO-UNDO EXTENT 2. /* Для наименования счета. */
DEF VAR proc-name   AS CHAR   NO-UNDO.          /* Для процедуры метода. */
DEF VAR mRecId      AS RECID  NO-UNDO.          /* 4 conversion string to recid. */
DEF VAR sort-type   AS INT64  NO-UNDO.          /* Для сортировки по F5. */
DEF VAR closedate   AS DATE   NO-UNDO.          /* Дата закрытия категории */
DEF VAR mAcctHndl   AS HANDLE NO-UNDO.          /* Для изменения реквизитов поля. */
DEF VAR mAcctRead   AS CHAR   NO-UNDO.          /* Условия для проверки прав доступа. */
DEF VAR mAcctRid    AS ROWID  NO-UNDO.          /* Позиционирование при переключение форм. */
DEF VAR mEndDate    AS DATE   NO-UNDO.          /* Для поиска остатков на дату. */
DEF VAR mAcct       AS CHAR   NO-UNDO.          /* Для форматированного вывода на экран. */
DEF VAR mProcName   AS CHAR   NO-UNDO.          /* Имя процедуры. */
DEF VAR mProcPrms   AS CHAR   NO-UNDO.          /* Параметры процедуры. */
DEF VAR mNameAcct   AS LOG    NO-UNDO.          /* Флаг формирования названия счета. */
DEF VAR mFrameExc   AS CHAR   NO-UNDO.          /* Список неотображаемых фреймов. */
DEF VAR mSec-code   AS CHAR   NO-UNDO.
DEF VAR mPosBal     AS DEC    NO-UNDO.          /* Остаток счета в рулях (форма 5, 6). */
DEF VAR mPosBalScr  AS DEC    NO-UNDO.          /* Остаток счета в рулях (форма 5, 6). */
DEF VAR mPosVal     AS DEC    NO-UNDO.          /* Остаток счета в валюте счета (форма 5). */
DEF VAR mPosValScr  AS DEC    NO-UNDO.          /* Остаток счета в валюте счета (форма 5). */
DEF VAR mPosQty     AS DEC    NO-UNDO.          /* Остаток счета в штуках (форма 6). */
DEF VAR mPosQtyScr  AS DEC    NO-UNDO.          /* Остаток счета в штуках (форма 6). */
DEF VAR mAvBal      AS DEC    NO-UNDO.          /* Остаток счета в рулях (форма 8). */
DEF VAR mAvVal      AS DEC    NO-UNDO.          /* Остаток счета в валюте счета (форма 8). */
/* переменные для фильтрации по остаткам и оборотам по счетам */
DEF VAR SldType     AS CHAR   NO-UNDO.          /* Тип сальдо (деб,кред,*) - для фильтра */
DEF VAR mSldDate    AS DATE   NO-UNDO.          /* Дата расчета остатков по счетам */
DEF VAR mTurnBeg    AS DATE   NO-UNDO.          /* дата нач. интерв. расчета оборотов по счетам */
DEF VAR mTurnEnd    AS DATE   NO-UNDO.          /* дата оконч. интерв. расчета оборотов по счетам */
DEF VAR sh-b        AS DEC    NO-UNDO.          /* обороты по счету в нац.валюте */
DEF VAR sh-v        AS DEC    NO-UNDO.          /* обороты по счету в ин.валюте */
DEF VAR mTurnType   AS CHAR   NO-UNDO.          /* тип оборотов (деб,кред,*) */
DEF VAR vFiltTurn   AS LOG    NO-UNDO.          /* осуществлять ли фильтрацию по оборотам */
DEF VAR vFiltSld    AS LOG    NO-UNDO.          /* осуществлять ли фильтрацию по остаткам */
DEF VAR mColCloAc   AS LOG    NO-UNDO.          /* Цвет закрытого счета */
DEF VAR vDate       AS DATE   NO-UNDO.          /* Дата за которую вычисляются остатки. */
DEF VAR currency    AS CHAR   NO-UNDO.
DEF VAR mNumFrm     AS INT64  NO-UNDO.
DEF VAR mAvGrList   AS CHAR   NO-UNDO.         /* доступные текущему пользователю группы клиентов */
DEF VAR mPersGr     AS CHAR   NO-UNDO.         /* Вид клиента (группа, ДР ВидСотр), для фильтрации по вычисл. полю */
DEF VAR mAccessMask AS CHAR   NO-UNDO.
DEF VAR mAccessContAcct AS CHARACTER NO-UNDO.
DEF VAR mOK         AS LOG    NO-UNDO.
DEF VAR mLinkSurr   AS CHAR   NO-UNDO.
DEF VAR mGrpFltLst  AS CHAR   NO-UNDO.          /* значение поля фильтра "Группы" - список групп по которым осуществляется фильтрация */
DEF VAR mFltGrpType AS CHAR   NO-UNDO.          /* значение поля фильтра "Отбирать счета принадлежащие" */
DEF VAR GroupList   AS CHAR   NO-UNDO.          /* переменная для фильтрации по вычисляемым полям */
DEF VAR mAcctGroups AS CHAR   NO-UNDO.          /* список групп очередного счета */
DEF VAR mGroupCnt   AS INT64  NO-UNDO.          /* счетчик для перебора списка групп */
DEF VAR mAcctGrOn   AS LOG    NO-UNDO.          /* Включено ли ограничение по группам доступа */
DEF VAR mBrwRole    AS CHAR   NO-UNDO.
DEF VAR mK2         AS LOG    NO-UNDO.          /* учитывать докуметы,стоящие в к2, при расчете остатка (форма 8)*/
DEF VAR mCrTurnVal  AS DEC    NO-UNDO.          /* кредитовый оборот счета в вал    (форма 9). */
DEF VAR mCrTurn     AS DEC    NO-UNDO.          /* кредитовый оборот счета в рублях (форма 9). */
DEF VAR mDbTurnVal  AS DEC    NO-UNDO.          /* дебетовый  оборот счета в вал    (форма 9). */
DEF VAR mDbTurn     AS DEC    NO-UNDO.          /* дебетовый  оборот счета в рублях (форма 9). */
DEF VAR mAccessStatus AS CHAR  NO-UNDO.         /*значение НП "AccessStatus"*/ 
DEFINE VARIABLE mFrmLabelLst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mMyFrmLst AS CHARACTER   NO-UNDO.

FIND FIRST user-config NO-LOCK 
   WHERE user-config.USER-ID = userid('bisquit')
     AND user-config.proc-name EQ "UserFrames_acctbrw"
NO-ERROR.
IF AVAIL user-config THEN mMyFrmLst = REPLACE(user-config.DESCR," ",",") .
ELSE mMyFrmLst = "" .

DEF BUFFER gacct FOR acct.

{tmpobj.def &pref=Local}

/* Производим подмену для категории, "" трансформируем в "*". */
IF GetFltVal ("acct-cat") EQ ""
THEN DO:
   RUN SetFltFieldList ("acct-cat", "*"). /* Записываем в строку. */
   RUN SetFltField     ("acct-cat", "*"). /* Записываем структуру. */
END.

IF       GetFltValEx ("acct-cat", "") NE ""
   AND   GetFltVal   ("acct-cat") NE "*"
THEN closedate = Get_Date_Cat (GetFltVal ("acct-cat"),gend-date).

/* Если категория неопределена,
** то ищем последний закрытый ОД по обязательным категориям. */
IF closedate EQ ?
   THEN closedate = FGetLastClsDate(gend-date, Get_StatClose_Cats(gend-date)). 
IF closedate EQ ?
   THEN closedate = gend-date.
                        /* Определяем метод формирования наименования счета. */
RUN GetClassMethod IN h_xclass (
   GetFltVal ("iClassCode"),  /* Класс объекта. */
   "BrwVal",                  /* Код метода. */
   "Details",                 /* Код реквизита. */
   "",                
   OUTPUT mProcName,          /* Название метода. */
   OUTPUT mProcPrms           /* Параметры метода. */
).
ASSIGN
                        /* Устанавливаем флаг для формирвоания наименования счета. */
   mFrameExc   = fGetSetting ("AcctPosColors","CA", ?)
   mColCloAc   = {assigned mFrameExc}
   mNameAcct   = mProcName EQ ?
                        /* Отключаем формы в зависимости от режима просмотра. */
   mFrameExc   = IF shModeMulty THEN "1" ELSE "4"
                        /* устанавливаем дату расчета остатков по счетам из настроек фильтра
                        ** по умолчанию - глобальная дата gend-date */
   mSldDate    = DATE(GetFltVal("SldDate"))
   
   mAvGrList   = GetRightPersGroup()
   mBrwRole    = GetFltValEx ("BrwRole", "")
.
      /* Устанавливаем фильтр - доступные текущему пользователю группы клиентов */
RUN SetFltField("mPersGr",mAvGrList).

IF GetFltValEx("view-type", "0") EQ "5" THEN
   mFrameExc = mFrameExc + ",2,5".

mAccessMask = FGetSetting("СтандТр", "AccessAcct", "").
mAccessContAcct  = FGetSetting("СтандТр", "AccessContAcct", "").

{acctbrw.frm}           /* Формы браузера лицевых счетов. */
{acct.qry}              /* Запросы для браузера. */

/* &oh6        = "│F6 фильтр" */
/*  */

/* Commented by SOAV: если не загружен фильтр, то сортировка по-умолчанию */
IF NUM-ENTRIES (user-config-info) LT 3 OR ENTRY (3, user-config-info) EQ "?" THEN
DO:
   sort-type = INT64(FGetSetting("СортСч", "", "1")).
   RUN SetFltField      ("sort-type", STRING (sort-type)).
   RUN SetFltFieldList  ("sort-type", STRING (sort-type)).
END.

{navigate.cqr
   &file       = "acct"
   &postfind   = "acctbrw.fnd "
   &repaint    = "acctbrw.rpt "

   &access-read      = "R"
   &access-cond      = " mAcctGrOn AND NOT GetAcctByGroupPermissionUser (acct.acct + ',' + acct.currency,'r',USERID('bisquit')) "

   &nav-permission-off = YES

   &autofind         = "!mPosVal*,!mPosBal*,!mPosQty*,*"
   &autofast         = "!mPosVal*,!mPosBal*,!mPosQty*,*"
   &autofind-no-file = "name[1] mPosVal mPosBal mPosVal mPosBalScr mPosBal mPosBalScr mPosQty mPosQtyScr"
   &autofast-no-file = "name[1] mPosVal mPosBal mPosVal mPosValScr mPosBal mPosBalScr mPosQty mPosQtyScr"
   &manualWhere      = "AcctManual"

   &CalcFld = "mPosVal mPosBal mPosQty SldType sh-v sh-b mPersGr GroupList"
   &CalcVar = "acctbrw.cv "

   &SrchFld       = "mAcct|currency|mPosVal|mPosBal|mPosQty"
   &SrchFile      = "acct|acct|||"
   &vSrchAltFld   = "(IF shMode THEN 'acct.number' ELSE 'acct.acct') + '|acct.currency|mPosVal,mPosValScr|mPosBal,mPosBalScr|mPosQty,mPosQtyScr'"
   &SrchFormat    = "GetAcctFmt (GetFltVal('acct-cat')) + '||||' + GetNullStr(GetFmtQty('', 'acct', 21, 7) + ' Д')"

   &AutoSigns  = YES
   
   &look       = "b-acct.nav "

   &oh1        = "│F2 опер"
   &oth1       = "acct().mnu "

   &oh3        = "│F3 форма│CTRL-F3 Избр" 
   &oth3       = "frames.cqr "
      &user-frames_cqr  = "RUN SetFltField      ('view-type', STRING (n-frm - 1)).
                           RUN SetFltFieldList  ('view-type', STRING (n-frm - 1))." 
      &exclfrm          = mFrameExc
      &myfrm            = YES
   
   &filt       = YES
   &oth6       = "flt-file.f6 &runt-set-filtr=YES"
   
   &oh8        = "│F8 закр"
   &oth8       = "acct.cl "

   &maxfrm     = 9
   &bf1        = "currency acct.contract name[1] code.val"
   &cf1        = "mAcct currency acct.contract name[1] code.val"
   &bf2        = "currency"
   &cf2        = "mAcct currency"
   &bf3        = "currency signs_value"
   &cf3        = "mAcct currency signs_value"
   &bf4        = "currency acct.filial-id acct.contract name[1] str-recid code.val"
   &cf4        = "mAcct currency acct.filial-id acct.contract name[1] str-recid code.val"
   &bf5        = "currency"
   &cf5        = "mAcct currency mPosVal mPosBal"
   &bf6        = "currency" 
   &cf6        = "mAcct currency mPosQty mPosBal"
   &bf7        = "mAcct currency name[1]"
   &bf8        = "currency" 
   &cf8        = "mAcct currency mAvVal mAvBal"
   &bf9        = "currency" 
   &cf9        = "mAcct currency mDbTurnVal mCrTurnVal mDbTurn mCrTurn "    
   
   &edit       = "b-acct.ef "
   &delete     = "acct.del "
   
   &startup = "xsignrat.st &runt-set-filtr='YES'"
   &oth4    = "xsigns.rat "
      &rat_file      = "acct"
      &rat_class     = "acct.class-code"
      &rat_upclass   = "GetFltVal ('iClassCode')"
      &rat_surr      = "acct.acct + ',' + acct.currency "
      &rat_num       = "3"
      &rat_key       = "F2"
   
   &oh5     = "│F5 сорт"
   &oth5    = "b-acct.srt "
         
   &total      = "acctbrw.cal "
   &print      = "b-acct.prt "
   &return     = "b-acct.ret "
   
   &mess-del   = "'Удалить счет № ' + STRING(dbuf.number) + (IF dbuf.currency <> '' THEN '/' + string(dbuf.currency) ELSE '') + ' ?'"

   &tmprecid      = YES   
   &need-tmprecid = YES
   &local-recid   = YES
   &local-rest    = YES
   &local-keep    = YES
   &AFTER-FILTER  = "RUN AcctAfterFilter. "
   &qbisxtrafields = "acctbrw.qbis "
}

{acct().pro}            /* Процедура для установления атрибутов формы. */
{intrface.del}          /* Выгрузка инструментария. */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='22/04/2015 17:12:50.496+04:00' */
/* $LINTUSER='komi' */
/* $LINTMODE='1' */
/* $LINTFILE='acctbrw.p' */
/*prosignC9Jo1920ywEWgQiApeafWA*/