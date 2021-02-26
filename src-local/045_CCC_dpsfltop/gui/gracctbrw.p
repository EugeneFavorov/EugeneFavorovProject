/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: GRACCTBRW.P
      Comment: Браузер групп лицевых счетов
   Parameters:
         Uses:
      Used by:
      Created: 26.05.2011 14:28 ariz    
     Modified: 26.05.2011 14:28 ariz    
*/

{globals.i}             /* Глобальные переменные сессии. */
{flt-file.i}            /* Определение структуры динамического фильтра. */
{ttretval.def}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

DEF VAR in-class         AS CHAR NO-UNDO. /*  класс данных (код классификатора) */
DEF VAR in-parent        AS CHAR NO-UNDO. /*  родитель классификатора           */

DEF VAR mAcctGroupLinkID AS INT64  NO-UNDO.
DEF VAR mAcctSurrogate   AS CHAR   NO-UNDO.
DEF VAR mOK              AS LOG    NO-UNDO.
DEF VAR mLinkSurr        AS CHAR   NO-UNDO.

DEF BUFFER dcode FOR code.

MAIN_BLOCK:
DO:
/*   IF NOT IsUserAdm (USERID ("bisquit")) THEN                                                  */
/*   DO:                                                                                         */
/*      RUN Fill-SysMes IN h_tmess ("", "", "1", "Вы не имеете пpава доступа к этой инфоpмации").*/
/*      LEAVE MAIN_BLOCK.                                                                        */
/*   END.                                                                                        */

   ASSIGN
      in-class    = GetFltVal ("class")
      in-parent   = GetFltVal ("parent")
   .
                        /* Определяем значение поля фильтра acct-surrogate.
                        ** Заполненное поле acct-surrogate является признаком
                        ** отображения групп доступа, назначенных счету,
                        ** суррогат которого указан в поле фильтра. */
   mAcctSurrogate = GetFltVal ("acct-surrogate").
   /* mAcctSurrogate = "10201810100020010043     @002,". */
   IF {assigned mAcctSurrogate} THEN
   DO:
      mAcctGroupLinkID = GetXLinkID ("acct","acct-group").
      IF mAcctGroupLinkID EQ ? THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ошибка настройки метасхемы. Не найдена связь 'acct-group' между классами 'acct' и 'acct-group'.").
         LEAVE MAIN_BLOCK.
      END.

      RUN SetFltField ("title", "СЧЕТ  " + mAcctSurrogate + ": ГРУППЫ ДОСТУПА").
   END.
   
   {gracctbrw.frm}           /* Формы браузера. */
   {gracctbrw.qry}           /* Запросы для браузера. */
   
   
   {navigate.cqr
      &file          = "code"
      &filt          = YES
      &AutoSigns     = YES
      &tmprecid      = YES 
      &local-recid   = YES

      &access-class  = "'code'"
      &access-surr   = "code.class + ',' + code.code"
      &access-read   = r

      &bf1           = "code.code code.name"

      &edit          = "gracctbrw.edt "
      &look          = "bis-tty.nav "
         &BEFORE-RUN-METHOD = "codvlbrw.bfe "
         &AFTER-RUN-METHOD  = "gracctbrw.aft "
         &class_upper       = "'acct-group'"
         &class_avail       = "'acct-group'"
         
      &oth6          = "flt-file.f6 "
      
      &delete        = "gracctbrw.del "
      &befdel        = "gracctbrw.bdl "
      
      &return        = "gracctbrw.ret "
   }

END.

{intrface.del}          /* Выгрузка инструментария. */
