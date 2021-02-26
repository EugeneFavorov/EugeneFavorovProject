/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: pp-grd.p
      Comment: Библиотека процедур обмена с платежной системой Город
   Parameters: нет
         Uses:
      Used BY:
      Created: 24.07.2015 KSBIS TT:0235518 Миграция. Прием платежей по системе "Город"
     Modified: 
*/

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "grd"
   &LIBNAME       = "Онлайн обмен с Город"
   &DESCRIPTION   = "Методы онлайн обмена"
}

{globals.i}
{exchange.equ}
{g-trans.equ}

{intrface.get kau}
{intrface.get card}
{intrface.get exch}
{intrface.get pack}
{intrface.get xrkc}
{intrface.get blkob}
{intrface.get count}
{intrface.get pbase}
{intrface.get refer}
{intrface.get rfrnc}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}
{intrface.get filex}

DEFINE VARIABLE mBicMFO          AS CHAR        NO-UNDO. 
DEFINE VARIABLE mNumStr          AS INT64       NO-UNDO. 
DEFINE VARIABLE mSumm            AS DEC         NO-UNDO.
DEFINE VARIABLE vFileID          AS INT64       NO-UNDO.	
DEFINE VARIABLE vFileName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFileNameNew     AS CHAR        NO-UNDO.

mBicMFO = GetXAttrValueEx("branch", shFilial, "БанкМФО", "").
/* Процедура удаления документов */

{pck-op-del.pro}


/*======================================================================================================================*/
/*=== Проверка - Если счет получателя (ben-acct) подходит под маски из НП bal-nalog, ===================================*/
/*=== а налоговые реквизиты документа не заполнены (ошибка должна срабатывать, если хотя бы один реквизит не заполнен): */
/*=== ПокДД, ПокНД, ПокНП, ПокОП, ПокСт, ПокТП, КБК, ОКАТО-НАЛОГ, Kpp-rec,  Kpp-send====================================*/

{pfuncdef 
   &DEFPROC="ChkBalNalog"
   &DESCRIPTION="Проверка налоговых реквизитов"
   &PARAMETERS="['YLKM"
   &RESULT="Результат операции"
}

PROCEDURE ChkBalNalog:

   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.


MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

IF {assigned iWOP::rec-acct} AND
   CAN-DO(FGetSetting("ГНИ","bal-nalog",""),iWOP::rec-acct) AND  
   NOT ({assigned iWOP::pokdd$} AND   
        {assigned iWOP::poknd$} AND   
        {assigned iWOP::poknp$} AND   
        {assigned iWOP::pokop$} AND   
        {assigned iWOP::pokst$} AND   
        {assigned iWOP::poktp$} AND   
        {assigned iWOP::kbk-rec} AND   
        {assigned iWOP::okato-rec} AND   
        {assigned iWOP::kpp-rec} AND   
        {assigned iWOP::kpp-send})

THEN
DO:
   RUN AddErrorFormat IN h_exch (iWOP,iList) .
END. 

vFlagSet = YES.

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. 



/*===============================================================================================*/
/*=== Проверка на совпадение БИК плательщика и БИК филилала======================================*/

{pfuncdef 
   &DEFPROC="ChkBIK"
   &DESCRIPTION="Проверка на совпадение с БИК филиала"
   &PARAMETERS="БИК плательщика"
   &RESULT="Результат операции"
}

PROCEDURE ChkBIK:

   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.


MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

IF mBicMFO NE iWOP::bank-bik-send THEN
DO:
   RUN AddErrorFormat IN h_exch (iWOP,iList) .
END. 

vFlagSet = YES.

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. 


{pfuncdef 
   &DEFPROC="GorodReeImp"
   &DESCRIPTION="Импорт строк реестра из системы Город"
   &PARAMETERS="Имя класса,указатель на объект"
   &RESULT="Результат операции"
}

PROCEDURE GorodReeImp:



   DEF INPUT PARAM iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.


DEFINE VARIABLE vPacketID        AS INT64       NO-UNDO.
DEF VAR vBicMask  AS  CHAR  NO-UNDO.  /* Маска допустимых БИК                                    */
DEF VAR vCorrAcct  AS  CHAR  NO-UNDO.  /* Маска допустимых БИК                                    */
DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vFlagPub   AS  LOG     NO-UNDO INIT NO.
DEF VAR vFilialId AS  CHAR  NO-UNDO.  /* Код подразделения получателя платежа                    */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.  /* Код транзакции для зачисления                           */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.
DEF VAR vBuffer    AS  CHAR    NO-UNDO. /* Обрабатываемая строка из файла                       */


DEF BUFFER bAcct     FOR acct.
DEF BUFFER bCode     FOR Code.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}


   /* Получение обрабатываемой строки: */
   /* Получим загруженную строку и сохраним ее на пакете */
   PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).
   IF vFlagPub EQ YES THEN 
      RUN PacketTextSave IN h_pack(iExch::PacketID, vBuffer + "~n").

   /* Проверка на соответсвие формату */
   IF GetSysConf("ErrorString") NE "NoError" THEN
   DO:
      PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).


         RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("Строка не соответствует формату: '&1'",
                                                       vBuffer)).

      vFlagSet = YES.
      LEAVE MAIN.
   END.



   /* Проверка настройки классификатора EXCH-MSG */
   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN
      UNDO MAIN, LEAVE MAIN.



   RUN InstanceCreate IN h_exch(?, iExch).


   /*=== Инициализация переменных ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::doc-num       = TRIM(iExch::doc-num2)
      iExch::acct-rec      = TRIM(iExch::rec-acct)
      iExch::bank-bik-send = TRIM(iExch::bank-bik-send)
      iExch::bank-code-send = TRIM(iExch::bank-bik-send)
      iExch::bank-code-rec  = TRIM(iExch::bank-bik-rec)
      iExch::ben-acct      = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::rec-acct)
      iExch::Amt-rub       = ABS(INT64(iExch::Amt-rub) / 100)

      /* Теперь acct-cr задается на шаблоне */
      /* Если БИКИ совпадают, то внутренний платеж, иначе межбанковский */
/*      iExch::acct-cr       = IF mBicMFO EQ iExch::bank-bik-rec THEN TRIM(iExch::rec-acct) ELSE iExch::acct-cr*/

      iExch::name-ben      = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::name-rec) 
      iExch::inn           = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::inn-rec) 
      iExch::details       = TRIM(iExch::details)
      iExch::kpp-rec       = TRIM(iExch::kpp-rec)
      iExch::kbk$           = TRIM(iExch::kbk-rec)
      iExch::okato-nalog$   = TRIM(iExch::okato-rec)
      iExch::order-pay     = TRIM(iExch::order-pay)

      /* Маска допустимых БИК банков получателей */
      vBicMask             = TRNSettingValue("","BICMask","!*")

   NO-ERROR. {&NO-ERROR}


/* Теперь acct-cr задается на шаблоне */
/*
   vCorrAcct = GetRefVal("МФР", gend-date, SUBST("&1,&2", "ГОРОД" , mBicMFO)).

   /* БИК совпадает по маске и есть в МФР, то платеж межфил.*/
   IF CAN-DO(vBicMask, iExch::bank-bik-rec) AND {assigned iExch::bank-bik-rec} AND {assigned vCorrAcct}  THEN
   DO:
      iExch::acct-cr = vCorrAcct.
   END.
*/
/*
   /* Нумерация документов */
   iExch::doc-num            = SetCounterValue(TRIM(iExch::CounterName), ?, gend-date).

   IF NOT {assigned iExch::doc-num} THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1","Ошибка получения значения для счетчика документов").
      LEAVE MAIN.

   END. /* IF NOT {assigned iExch::doc-num} THEN */
*/

   /*=== Маршрутизация документа ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* Совпадает с текущей                     */
      OR ERROR-STATUS:ERROR          /* Возникли ошибки в функции маршрутизации */
      OR NOT {assigned vOpKind}      /* Код транзакции пуст                     */
   THEN
   DO:
      vTmpStr = "Не определена транзакция создания документа".
      RUN Fill-SysMes IN h_tmess("","","-1",vTmpStr).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== Запускаем транзакцию зачисления ===*/

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   /*=== Проверка создания документа ===*/
   IF iExch::op EQ "0" THEN
   DO:

      vTmpStr = "Ошибка создания документа с номером '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::doc-num)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */


   vFlagSet = YES.

   mNumStr = mNumStr + 1.
   mSumm   = mSumm   + iExch::Amt-rub.



/*----------------------------------------------------------- Уборка мусора --*/
   RUN InstanceJunk (iExch,    0).

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */



{doreturn.i vFlagSet}



END PROCEDURE. 



PROCEDURE GorodReeImpFF:
   DEF INPUT PARAM iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.

   RUN Fill-SysMes IN h_tmess("","","",SUBST("Всего загружено платежей &1 на сумму: &2",STRING(mNumStr),STRING(mSumm," -zzz,zzz,zzz,zz9.99"))).

   mNumStr = 0.
   mSumm   = 0.

END PROCEDURE. 


PROCEDURE  GorodReeImpFH:

   DEFINE INPUT  PARAMETER iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.

   vFileID     = INT64(iExch::FileExchID).
   vFileName   = GetPureName(FileGetPath(vFileID)).

   IF vFileNameNew NE vFileName THEN
   DO:
      RUN Fill-SysMes("","","", SUBST("Обрабатывается файл: &1",vFileName)).
      vFileNameNew = vFileName.
   END.
END PROCEDURE.