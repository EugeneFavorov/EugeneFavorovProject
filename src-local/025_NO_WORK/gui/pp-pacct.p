{globals.i}

/* +++ pp-pacct.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: PP-PACCT.P
      Comment: Библиотека функций парсера для счетов
   Parameters: Нет
         Uses:
      Used by:
      Created: 10.02.2004 14:30 laav    
     Modified: 28.04.2004 12:56 KSV      (0029414) Добавлены функции
                                         ОСТАТОК_РУБ и ОСТАТОК_ВАЛ.
     Modified: 12.05.2004 20:08 KSV      (0030054) Добавлена функция СЧЕТ.
     Modified: 14.05.2004 15:07 KSV      (0029314) Добавлена функция
                                         ПАРНЫЙ_СЧЕТ.
     Modified: 21.03.2005 17:26 KSV      (0039833) Добавлены функции
                                         ОБОРОТР/ОБОРОТВ
     Modified: 31.05.2005 17:39 KSV      (0047392) Изменено описание парсерной
                                         функции ОБОРОТВ
     Modified: 16.06.2005 18:52 KSV      (0039620) Исправлены даты передаваемые
                                         в процедуру acct-pos в ф-циях
                                         ОСТАТОК_РУБ/ОСТАТОК_ВАЛ.
     Modified: 17.06.2005 12:08 KSV      (0039620) В ф-ции
                                         ОСТАТОК_РУБ/ОСТАТОК_ВАЛ добавлена
                                         возможность возврата неакцептованного
                                         остатка.
     Modified: 04.08.2007 15:41 OZMI     (0075109)
     Modified: 24.10.2007 18:06 MUTA     0082119 Добавлена функция "ОСТАТОК_КОЛ" 
     Modified: 08.04.2008 MUTA 0090931 Доработка инструмента acct-qty.
     Modified: 07.07.2011 VPA 0150535
*/

&GLOBAL-DEFINE GET-ACCT DEFINE BUFFER acct FOR acct. ~
                        FIND FIRST acct WHERE ~
                           acct.acct     = AddFilToAcct(iAcct, shFilial)   AND ~
                           acct.currency = iCurr   NO-LOCK NO-ERROR. ~
                        IF NOT AVAILABLE acct THEN ~
                        DO: ~
                           is-ok = -1. ~
                           RUN Fill-SysMes IN h_tmess("","","-1", ~
                                                      "Не найден счет с номером " + iAcct ~
                                                    + " и кодом валюты " + iCurr). ~
                           RETURN . ~
                        END.

{globals.i}
{sh-defs.i}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get rsrv}
{intrface.get blkob}
{intrface.get op}
{intrface.get refer}
{intrface.get trans}
{intrface.get db2l}

DEFINE VARIABLE h_pvok AS HANDLE      NO-UNDO.
RUN pp-pvok.p PERSISTENT SET h_pvok.

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PACCT"
   &LIBNAME       = "Библиотека функций парсера для лицевых и счетов 2-го порядка."
   &DESCRIPTION   = "Содержит функциии парсера для счетов: реквизиты счета, остатки"
   }

/* Определение валюты бюджета */
{pfuncdef
   &NAME          = "АКТИВ_ПАССИВ"
   &DESCRIPTION   = "Определяет для счета, активный он или пассивный"
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА СЧЕТА"
   &RESULT        = "'А' - для активного счета | 'П' - для пассивного"
   &SAMPLE        = "АКТИВ_ПАССИВ(@acct,@currency) = 'А' "
   }

   DEFINE INPUT  PARAMETER iAcct      AS CHAR NO-UNDO. /* номер счета*/
   DEFINE INPUT  PARAMETER iCurr      AS CHAR NO-UNDO. /* валюта счета*/
   DEFINE OUTPUT PARAMETER oUt_Result AS CHAR NO-UNDO. /* вых. пар-р: А либо П */
   DEFINE OUTPUT PARAMETER is-ok      AS INT64  NO-UNDO. 

   {pchkpar iAcct}

   IF iCurr <> "" THEN
   DO:
      {pchkpar iCurr}
   END.

   {&GET-ACCT}

   ASSIGN
      oUt_Result = acct.side
      is-ok = 0.
END PROCEDURE.

{pfuncdef
   &NAME          = "ОСТАТОК_РУБ"
   &DESCRIPTION   = "Возвращает рублевый акцептованный или неакцептованный остаток по счету на дату"
   &PARAMETERS    = "НОМЕР СЧЕТА, ВАЛЮТА СЧЕТА[,ДАТА = ДАТА ОПЕРДНЯ[,СТАТУС = ~~373]]"
   &RESULT        = "ОСТАТОК"
   &SAMPLE        = "ОСТАТОК_РУБ('10201810000020010028','',DATE('01/01/2004')) - ~
получение акцептованного остатка на 01/01/2004~~n~
ОСТАТОК_РУБ('10201810000020010028','') - получение акцептованного остатка на дату опердня~~n~
ОСТАТОК_РУБ('10201810000020010028','',ДАТА(),'П') - получение неакцептованного остатка на ~
дату опердня"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","Не найден счет с номером " + iAcct 
                                          + " и кодом валюты " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-pos IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-bal.
END PROCEDURE.

{pfuncdef
   &NAME          = "ОСТАТОК_КОЛ"
   &DESCRIPTION   = "Возвращает количество ценных бумаг, с учетом числа в акцептованных или ~
неакцептованных проводках по счету на дату"
   &PARAMETERS    = "НОМЕР СЧЕТА, ВАЛЮТА СЧЕТА[,ДАТА = ДАТА ОПЕРДНЯ[,СТАТУС = ~~373]]"
   &RESULT        = "ОСТАТОК"
   &SAMPLE        = "ОСТАТОК_КОЛ('10201810000020010028','',DATE('01/01/2004')) - получение ~
количества ценных бумаг с учетом числа в акцептированных проводках на 01/01/2004~~n~
ОСТАТОК_КОЛ('10201810000020010028','') - получение количества ценных бумаг с учетом числа в ~
акцептированных проводкахна дату опердня~~n~
ОСТАТОК_КОЛ('10201810000020010028','',ДАТА(),'П') - получение количества ценных бумаг с ~
учетом числа в неакцептированных проводках  на дату опердня"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","Не найден счет с номером " + iAcct 
                                          + " и кодом валюты " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-qty IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-qty.
END PROCEDURE.

{pfuncdef
   &NAME          = "КОЛИЧ_ПО_ВЫП"
   &DESCRIPTION   = "Расчет суммарного остатка по депо счетам"
   &PARAMETERS    = "ВЫПУСК[,СТОРОНА[,ДАТА[,СТАТУС]]]"
   &RESULT        = "КОЛИЧЕСТВО ЦБ"
   &SAMPLE        = "КОЛИЧ_ПО_ВЫП('001', 'А', ДАТА(), CHR(251)) = 1234.56789"
}

   DEFINE INPUT  PARAMETER iSecCode    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iSide       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   {pchkpar iSecCode}

   IF NOT {assigned iSide} THEN
      iSide = "А".

   IF iDate EQ ? THEN
      iDate = GetBaseOpDate().

   IF NOT {assigned iStatus} THEN
      iStatus = CHR(251).

   FOR EACH acct WHERE acct.currency EQ iSecCode
                   AND acct.acct-cat EQ "d"
                   AND acct.side     EQ iSide
   NO-LOCK:

      RUN acct-qty IN h_base (acct.acct,
                              acct.currency,
                              iDate,
                              iDate,
                              iStatus).

      oUt_Result = oUt_Result + sh-qty.
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "ОСТАТОК_ВАЛ"
   &DESCRIPTION   = "Возвращает валютный акцептованный или неакцептованный остаток по счету на дату"
   &PARAMETERS    = "НОМЕР СЧЕТА, ВАЛЮТА СЧЕТА[,ДАТА = ДАТА ОПЕРДНЯ[,СТАТУС = ~~373]]"
   &RESULT        = "ОСТАТОК"
   &SAMPLE        = "ОСТАТОК_ВАЛ('20203840300020000000','840',DATE('01/01/2004')) - ~
получение акцептованного остатка на 01/01/2004~~n~
ОСТАТОК_ВАЛ('20203840300020000000','840') - получение акцептованного остатка на дату опердня~~n~
ОСТАТОК_ВАЛ('20203840300020000000','840',ДАТА(),'П') - получение неакцептованного остатка на ~
дату опердня"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iStatus     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   DEFINE BUFFER bacct FOR acct. 
    
   FIND FIRST bacct WHERE bacct.acct EQ iAcct NO-LOCK NO-ERROR.
   IF NOT AVAIL bacct THEN DO:
      is-ok = -1. 
      RUN Fill-SysMes IN h_tmess("","","-1","Не найден счет с номером " + iAcct 
                                          + " и кодом валюты " + iCurrency). 
      RETURN . 
   END.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   IF iStatus = ? THEN iStatus = CHR(251).
   
   RUN acct-pos IN h_base (iAcct,iCurrency,iDate,iDate,iStatus).

   oUt_Result = sh-val.
END PROCEDURE.

{pfuncdef
   &NAME          = "СЧЕТ"
   &DESCRIPTION   = "Проверяет номер счета на корректность, рассчитывает ключ ~
лицевого счета и предотвращает его повторное использование. Для определения ~
метода расчета ключа используется либо класс, переданный в качестве параметра, ~
либо используется класс текущего шаблона, либо, если текущий шаблон не определен, ~
используется класс ACCTB"
   &PARAMETERS    = "НОМЕР СЧЕТА[, БИК = НАШ БИК[, КЛАСС СЧЕТА]]"
   &RESULT        = "НОМЕР СЧЕТА"
   &SAMPLE        = "СЧЕТ('10201011x00000000055') = '10201011600000000055' - ~
рассчитывает ключ для счета нашего банка и резервирует счет,~~n~
СЧЕТ('40702810x00000000001','044585109','acctbi') = '40702810100000000001' - ~
рассчитывает ключ для счета другого банка"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iBIK        AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iClass      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vClass  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmplID AS INT64    NO-UNDO.
   DEFINE VARIABLE vProg   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vKey    AS INT64    NO-UNDO.
   DEFINE VARIABLE vMask   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vPos    AS INT64    NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vTokidx AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vToklen AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bTmpl FOR op-kind-tmpl.
   DEFINE BUFFER bAcct FOR acct.

      
   {pchkpar iAcct}

   IF iBIK = FGetSetting("БанкМФО","","") THEN iBIK = ?.

   /* Класс не задан - берем из текущего шаблона */
   IF iClass = ? THEN
   DO:
      /* Commented by KSV: Определяем класс счета через шаблон транзакции */
      vTmplID = GetBaseTemplate().

      FIND FIRST bTmpl WHERE 
         bTmpl.tmpl-id = vTmplID NO-LOCK NO-ERROR.

      vClass = IF NOT AVAILABLE bTmpl 
               THEN "acctb" ELSE bTmpl.work-class-code.
   END.
   ELSE
      vClass = iClass.

   /* Commented by KSV: Определяем программу расчета ключа */
   vprog = GET-CLASS-METHOD(vClass, "U1").
   IF vprog = ? THEN vprog = "key-tst".
   vProg = vProg + ".p".

   RUN VALUE(vProg) (iAcct,iBIK,OUTPUT vKey).

   IF vKey = ? THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","acct01","","%s=" + iAcct).
      is-ok = -1.
      RETURN .
   END.

   /* Commented by KSV: Определяем позицию ключа в номере счета */
   vMask = GetXattrInit(vClass,"acct").
   /* если шаблон в неявном виде [1-5]бвввкпппппппп, то ошибка с определением позиции */
   RUN GetAcctMask IN h_acct(vMask, OUTPUT vMask, OUTPUT vTokidx, OUTPUT vToklen).

   IF {assigned vMask} THEN
      vPos = INDEX(vMask,"к").

   IF vPos = 0 THEN vPos = 9.

   SUBSTR(iAcct,vPos,1) = STRING(vKey) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","acct01","","%s=" + iAcct).
      is-ok = -1.
      RETURN .
   END.
      
   IF iBIK = ? THEN DO:

      /* Commented by KSV: Резервируем счет */
      RUN AcctKeep IN h_acct(iAcct,OUTPUT vOk).
      IF vOk <> YES THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","acct02","","%s=" + iAcct).
         is-ok = -1.
         RETURN .
      END.
      
      /* Commented by KSV: Проверяем номер счета на уникальность */
      {find-act.i
         &bact = bAcct
         &acct = iAcct
      }
      IF AVAIL bAcct THEN
      DO:
         RUN Fill-SysMes IN h_tmess("","acct03","","%s=" + iAcct).
         is-ok = -1.
         RETURN .
      END.

   END. /* iBIK <> ? */

   oUt_Result = iAcct.

END PROCEDURE.

{pfuncdef
   &NAME          = "ПАРНЫЙ_СЧЕТ"
   &DESCRIPTION   = "Возвращает парный счет для лицевого счета"
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА СЧЕТА,[С ОСТАТКОМ[,П или А]]"
   &RESULT        = "НОМЕР СЧЕТА"
   &SAMPLE        = "ПАРНЫЙ_СЧЕТ('10201810000170010004','','П') = '20202810400000031001'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iRemainder  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPass       AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.
   
   DEF VAR vIs     AS LOG  NO-UNDO .
   DEF VAR vResult AS CHAR NO-UNDO .
   DEF VAR vOst1   AS DEC  NO-UNDO.
   DEF VAR vOst2   AS DEC  NO-UNDO.

   {pchkpar iAcct}

   IF iCurr <> "" THEN
   DO:
      {pchkpar iCurr}
   END.

   iAcct = DelFilFromAcct(iAcct).
   {&GET-ACCT}

   IF iRemainder = "Да" THEN 
   DO:
      RUN acct-pos IN h_base (acct.acct,
                              acct.currency,
                              GetBaseOpDate(),
                              GetBaseOpDate(),
                              CHR(251)).

      vOst1 = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
      IF vOst1 NE 0 
      THEN oUt_Result = acct.acct.

      IF {assigned acct.contr-acct} THEN DO:
         RUN acct-pos IN h_base (acct.contr-acct,
                                 acct.currency,
                                 GetBaseOpDate(),
                                 GetBaseOpDate(),
                                 CHR(251)).
         vOst2 = IF acct.currency EQ "" THEN sh-bal ELSE sh-val.
         IF vOst1 = 0 AND vOst2 = 0 THEN DO:
            /*Определить счет, когда оба остатка нулевые */
            oUt_Result = acct.acct.
            RUN GetAcctForAP IN THIS-PROCEDURE(acct.acct,
                                               acct.side,
                                               acct.contr-acct,
                                               iPass, 
                                               OUTPUT vIs, 
                                               OUTPUT vResult ) .
            IF vIs THEN
               oUt_Result = vResult.
         END.
         ELSE IF ABS(vOst2) > ABS(vOst1) THEN 
            oUt_Result = acct.contr-acct.
      END.
   END.
   ELSE
      oUt_Result = acct.contr-acct.

END PROCEDURE.

PROCEDURE GetAccTForAP PRIVATE :
  DEF INPUT  PARAM iAcc1   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iSide   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iAcc2   AS CHAR NO-UNDO .
  DEF INPUT  PARAM iPasAct AS CHAR NO-UNDO .
  DEF OUTPUT PARAM oIs     AS LOG  NO-UNDO .
  DEF OUTPUT PARAM oResAcc AS CHAR NO-UNDO .
  /* проверка параметров */
  IF NOT (iPasAct = "П"  OR
          iPasAct = "AП" OR
          iPasAct = "A" )  OR
          iPasAct = ? OR
          iPasAct = "" THEN 
  DO:
      oIs = FALSE .
      RETURN .
  END.
  /* Определение какой счет выбрать */
  IF Iside = iPasAct THEN
     ASSIGN
        oIs = TRUE
        oResAcc = iacc1 .
  ELSE
     ASSIGN
        oIs = TRUE
        oResAcc = iacc2.
END PROCEDURE. /* GetAccTForAP  */

{pfuncdef
   &NAME          = "ВАЛЮТЫ_СЧЕТА"
   &DESCRIPTION   = "Возвращает список валют по имени счета.~~n~
Обычно одному имени счета соответствует одна валюта,~~n~
но теоретически возможна ситуация, когда существует~~n~
несколько счетов с одинаковыми именами, но разными валютами.~~n~
Если не найдено ни одного счета с указанным именем - возвращается ошибка."
   &PARAMETERS    = "НОМЕР СЧЕТА"
   &RESULT        = "ВАЛЮТА СЧЕТА,ВАЛЮТА СЧЕТА,..."
   &SAMPLE        = "ВАЛЮТЫ_СЧЕТА('10201810000170010004') = ''~~n~
ВАЛЮТЫ_СЧЕТА('42301276000020000003') = ',276,840'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   DEFINE BUFFER acct FOR acct.
   
   is-ok = 0.
   oUt_Result = "".
   FOR EACH acct WHERE acct.acct = iAcct NO-LOCK:
     is-ok = is-ok + 1.
     oUt_Result = RIGHT-TRIM(acct.currency + "," + oUt_Result, ",").
   END.
   IF is-ok = 0 THEN DO:
      RUN Fill-SysMes IN h_tmess("","","-1","Не найдено ни одного счета с именем [" + 
                                            ( IF iAcct = ? THEN "" ELSE iAcct) + "]").
      is-ok = -1.
      RETURN .
   END.
END PROCEDURE.

/* VPA 0150535 добавил параметр iUsrlst */
&GLOBAL-DEFINE OBOROT_PARAMS  TREAT_PARAM: ~
                              DO: ~
                                 IF iDateFrom <> ?  THEN ~
                                    vParStr = vParStr + "," + STRING(iDateFrom). ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iDateTo <> ?  THEN ~
                                    vParStr = vParStr + "," + STRING(iDateTo). ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iCurrency <> ?  THEN ~
                                    vParStr = vParStr + "," + iCurrency. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iOpStatus <> ?  THEN ~
                                    vParStr = vParStr + "," + iOpStatus. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iCorrAcctMask <> ?  THEN ~
                                    vParStr = vParStr + "," + iCorrAcctMask. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                                 IF iUsrlst<> ?  THEN ~
                                    vParStr = vParStr + "," + iUsrlst. ~
                                 ELSE ~
                                    LEAVE TREAT_PARAM. ~
                              END.

{pfuncdef
   &NAME          = "ОБОРОТР"
   &DESCRIPTION   = "Возвращает оборот по счету в рублях"
   &PARAMETERS    = "НОМЕР СЧЕТА,ТИП ОБОРОТА[,ДАТА НАЧАЛА = ДАТА ОПЕРДНЯ[,ДАТА ОКОНЧАНИЯ = ДАТА ~
ОПЕРДНЯ[,ВАЛЮТА[,СТАТУС ДОКУМЕНТОВ = ~~373[,МАСКА КОРРЕСПОНДИРУЮЩЕГО СЧЕТА = *[,МАСКА ~
ПОЛЬЗОВАТЕЛЕЙ = *]]]]]]"
   &RESULT        = "ОБОРОТ ПО СЧЕТУ"
   &SAMPLE        = "ОБОРОТР('47402810200020000055', 'Д') = 10000 - дебетовый рублевый оборот по ~
счету за опердень ~~n~
ОБОРОТР('47402810200020000055', 'К', DATE('01~/01~/' + YEAR(ДАТА())), ДАТА(), '', ~
373, '30102810*') = 200000 - кредитовый рублевый оборот с начала года в корреспонденции со ~
счетами 30102810 ~~n~"
   }
   DEFINE INPUT  PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iTurnType      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDateFrom      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iDateTo        AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iOpStatus      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCorrAcctMask  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUsrlst        AS CHARACTER  NO-UNDO. /* VPA 0150535 */
   DEFINE OUTPUT PARAMETER oUt_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64    NO-UNDO.

   DEFINE VARIABLE vParStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.

   iCorrAcctMask = REPLACE(iCorrAcctMask, ",", ";").
   /* VPA 0150535 */
   iUsrlst = REPLACE(iUsrlst, ",", ";"). 
   {find-act.i &acct=iAcct }
   IF AVAIL acct THEN iAcct=acct.acct.   /* VPA 0150535 полезно для многофилиальной базы */
   /* ----------- */
   IF CAN-DO("Д,К",iTurnType) <> YES THEN iTurnType = ?.

   {pchkpar iAcct iTurnType}

   vParStr = iAcct + "," + iTurnType.

   {&OBOROT_PARAMS}

   RUN oborot.p("Р",vParStr,GetBaseOpDate(),OUTPUT vOk,OUTPUT oUt_Result).

   IF vOk <> YES THEN is-ok = -1.

END PROCEDURE.

{pfuncdef
   &NAME          = "ОБОРОТВ"
   &DESCRIPTION   = "Возвращает оборот по счету в валюте"
   &PARAMETERS    = "НОМЕР СЧЕТА,ТИП ОБОРОТА[,ДАТА НАЧАЛА = ДАТА ОПЕРДНЯ[,ДАТА ОКОНЧАНИЯ = ДАТА ~
ОПЕРДНЯ[,ВАЛЮТА[,СТАТУС ДОКУМЕНТОВ = ~~373[,МАСКА КОРРЕСПОНДИРУЮЩЕГО СЧЕТА = *[,МАСКА ~
ПОЛЬЗОВАТЕЛЕЙ = *]]]]]]"
   &RESULT        = "ОБОРОТ ПО СЧЕТУ"
   &SAMPLE        = "ОБОРОТВ('47402840500020000055', 'Д') = 10000 - ~
дебетовый валютный оборот по счету за опердень ~~n~
ОБОРОТВ('47402840500020000055', 'К', DATE('01~/01~/' + YEAR(ДАТА())), ДАТА(), '840', ~
373, '30102840*') = 200000 - кредитовый валютный оборот с начала года в корреспонденции со ~
счетами 30102840 ~~n~"
   }
   DEFINE INPUT  PARAMETER iAcct          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iTurnType      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDateFrom      AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iDateTo        AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iOpStatus      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCorrAcctMask  AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUsrlst        AS CHARACTER  NO-UNDO.  /* VPA 0150535 */
   DEFINE OUTPUT PARAMETER oUt_Result     AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok          AS INT64    NO-UNDO.

   DEFINE VARIABLE vParStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOk     AS LOGICAL    NO-UNDO.

   iCorrAcctMask = REPLACE(iCorrAcctMask, ",", ";").
   /* VPA 0150535 */
   iUsrlst = REPLACE(iUsrlst, ",", ";"). 
   {find-act.i &acct=iAcct }
   IF AVAIL acct THEN iAcct=acct.acct.     /* VPA 0150535 полезно для многофилиальной базы */
   /* ----------- */
   IF CAN-DO("Д,К",iTurnType) <> YES THEN iTurnType = ?.

   {pchkpar iAcct iTurnType}

   vParStr = iAcct + "," + iTurnType.

   {&OBOROT_PARAMS}

   RUN oborot.p("В",vParStr,GetBaseOpDate(),OUTPUT vOk,OUTPUT oUt_Result).

   IF vOk <> YES THEN is-ok = -1.

END PROCEDURE.

{pfuncdef
   &NAME          = "НАИМЕНОВАНИЕ_СЧЕТА"
   &DESCRIPTION   = "Возвращает наименование счета по его номеру, коду валюты и коду филиала."
   &PARAMETERS    = "НОМЕР СЧЕТА [,ВАЛЮТА] [,ФИЛИАЛ]"
   &RESULT        = "НАИМЕНОВАНИЕ СЧЕТА"
   &SAMPLE        = "НАИМЕНОВАНИЕ_СЧЕТА('30122978100090000055','978')='ТОО Абсолют'~~n~
НАИМЕНОВАНИЕ_СЧЕТА('10201810300020010029')='Уставный фонд ТОО Фантазия'"
}
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iFilial     AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vName1        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vName2        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustINN      AS CHARACTER NO-UNDO.

   IF iFilial EQ ? THEN
      iFilial = ShFilial.
   IF iCurr EQ ? THEN
      {find-act.i
          &bacct  = acct
          &acct   = iAcct
          &filial = iFilial
      }
   ELSE
      {find-act.i 
          &bacct  = acct
          &acct   = iAcct
          &filial = iFilial
          &curr   = iCurr
      }
   IF AVAILABLE (acct) THEN DO:
      RUN GetCust IN h_base (BUFFER acct,
                             NO, NO,
                             OUTPUT vName1, 
                             OUTPUT vName2, 
                             OUTPUT vCustINN).
      oUt_Result = TRIM (vName1 + " " + vName2).
      is-OK = 0.
   END.
   ELSE DO:
      oUt_Result = "ОШИБКА".
      is-OK = 0.
   END.

END PROCEDURE.

{pfuncdef
   &NAME          = "ДОГОТКРЛС"
   &DESCRIPTION   = "Возвращает значение ДР ДогОткрЛС в нужном формате:~~n~
1, то значение имеет вид: N <ХХХХХХХХХ> от <ДД/ММ/ГГГГ>~~n~
2, то значение имеет вид: <ДД/ММ/ГГГГ>~~n~
3, то значение имеет вид: <ХХХХХХХХХ>~~n~
по умолчанию 1."
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА[,ФОРМАТ]"
   &RESULT        = "ЗНАЧЕНИЕ ДР ДогОткрЛС В НУЖНОМ ФОРМАТЕ"
   &SAMPLE        = "ДОГОТКРЛС('30122978100090000055','978',2)"
}
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iFormat     AS INT64    NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vTmp AS CHARACTER   NO-UNDO.

   {find-act.i &acct=iAcct
               &curr=iCurr}

    IF AVAIL(acct) THEN
       vTmp = GetXattrValue("acct",Acct.acct + "," + acct.currency,"ДогОткрЛС").

   IF vTmp NE "" THEN
      CASE iFormat:
         WHEN 1 THEN
            oUt_Result = "N " + ( IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                    ENTRY(2,vTmp)
                                 ELSE "") + 
                         " от " + ENTRY(1,vTmp).
         WHEN 2 THEN 
            oUt_Result = ENTRY(1,vTmp).
         WHEN 3 THEN 
            oUt_Result = IF NUM-ENTRIES(vTmp) EQ 2 THEN
                            ENTRY(2,vTmp)
                         ELSE "". 
         OTHERWISE
             oUt_Result = "N " + ( IF NUM-ENTRIES(vTmp) EQ 2 THEN
                                     ENTRY(2,vTmp)
                                  ELSE "") + 
                          " от " + ENTRY(1,vTmp).
      END CASE.
   ELSE 
      oUt_Result = "".

   is-OK = 0.

END PROCEDURE.

{pfuncdef
   &NAME          = "СЧЕТ_ПО_ПОДР"
   &DESCRIPTION   = "Определяет счет по заданному балансовому счету 2-го порядка, ~
подразделению и категории счета"
   &PARAMETERS    = "БАЛАНСОВЫЙ СЧЕТ 2-ГО ПОРЯДКА,ПОДРАЗДЕЛЕНИЕ[,КАТЕГОРИЯ СЧЕТА]"
   &RESULT        = "Номер счета"
   &SAMPLE        = "СЧЕТ_ПО_ПОДР("30102","002") = '30102-011-3-0002-0000000'"
   }

   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER   NO-UNDO. /* счет 2-го порядка */
   DEFINE INPUT  PARAMETER iBranch     AS CHARACTER   NO-UNDO. /* подразделение */
   DEFINE INPUT  PARAMETER iAcctType   AS CHARACTER   NO-UNDO. /* категория счета */
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   
   DEFINE BUFFER acct FOR acct.

   {pchkpar iBalAcct iBranch}

   FIND FIRST acct WHERE acct.bal-acct  EQ INT64(iBalAcct)
                     AND acct.branch-id EQ iBranch
                     AND acct.close-date EQ ?
                     AND acct.acct-cat  EQ IF NOT {assigned iAcctType} 
                                           THEN "b"
                                           ELSE iAcctType NO-LOCK NO-ERROR.
   IF AVAIL acct THEN
      ASSIGN
         oUt_Result = acct.acct
         is-ok      = 0
      .
END PROCEDURE.


{pfuncdef
   &NAME          = "СЧЕТ_ПО_НАЗНАЧ"
   &DESCRIPTION   = "Определяет счет по заданному назначению, клиентскому счету, ~
валюте и счету 2-го порядка"
   &PARAMETERS    = "НАЗНАЧЕНИЕ,КЛИЕНТСКИЙ СЧЕТ,ВАЛЮТА,БАЛАНСОВЫЙ СЧЕТ 2-ГО ПОРЯДКА"
   &RESULT        = "Номер счета"
   &SAMPLE        = "СЧЕТ_ПО_НАЗНАЧ('КдтКнв','40817840000010000091','840','47407') ~
= '47407840500001000202'"
   }

   DEFINE INPUT  PARAMETER iContract   AS CHARACTER   NO-UNDO. /* назначение */
   DEFINE INPUT  PARAMETER iClAcct     AS CHARACTER   NO-UNDO. /* клиентский счет */
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER   NO-UNDO. /* счет 2-го порядка */
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
   
   {pchkpar iContract iClAcct iCurr iBalAcct }

   DEFINE BUFFER acct  FOR acct.
   DEFINE BUFFER cAcct FOR acct.

   oUt_Result = "".

   {find-act.i
      &bact = cAcct
      &acct = iClAcct}
      
   IF AVAIL cAcct THEN DO:
      FIND FIRST acct WHERE acct.contract EQ iContract
                        AND acct.cust-cat EQ cAcct.cust-cat
                        AND acct.cust-id  EQ cAcct.cust-id
                        AND acct.bal-acct EQ INT64 (iBalAcct)
                        AND acct.currency EQ iCurr
         NO-LOCK NO-ERROR.
      IF AVAIL acct THEN
         oUt_Result = acct.acct.
   END.

   is-ok = 0.

END PROCEDURE.


{pfuncdef
   &NAME          = "СчкСрочСч"
   &DESCRIPTION   = "Возвращает первый свободный номер счета"
   &PARAMETERS    = "СЧЕТ_2_ПОРЯДКА, КОД_ВАЛЮТЫ, 7_ПОСЛЕДНИХ_ЦИФР_УНК_КЛИЕНТА,
                     КОД_ТИПА_СДЕЛКИ_ФОРЕКС[, ШАГ[, НАЧ_ЗНАЧ[,КАТЕГОРИЯ]]]"
   &RESULT        = "МАКС_ЗНАЧ_НОМЕРА_СЧЕТА + ШАГ"
   &SAMPLE        = "СчкСрочСч(БББББ,ВВВ,ННННЛЛЛ,СС[,ШАГ[,НАЧ_ЗНАЧ[,КАТЕГОРИЯ]]]) 
                     = МАКС(ХХ)[+ ШАГ]"
   }
   DEFINE INPUT  PARAMETER iBalAcct    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSecCod     AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iUnc        AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iForex      AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iStep       AS INT64    NO-UNDO.
   DEFINE INPUT  PARAMETER iInitCnt    AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iAcctCat    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER INIT "" NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   DEFINE VARIABLE vCnt AS CHARACTER INIT "" NO-UNDO.

   DEFINE BUFFER acct FOR acct.
   
   {pchkpar iBalAcct iSecCod iUnc iForex}
   
   vCnt = fGetSetting( "Вал", "НомСчкСрСч", "").
   is-ok = 0.
   IF vCnt NE "" THEN
   DO:
      oUt_Result = vCnt.
      RETURN.
   END.

   IF iInitCnt EQ ? THEN
       iInitCnt = "01".

   IF iStep EQ 0 OR iStep EQ ? THEN
       iStep = 1.

   IF iAcctCat EQ ? THEN
       iAcctCat = "f".

   FOR EACH acct WHERE acct.bal-acct             EQ INT64(iBalAcct)
                   AND acct.currency             EQ iSecCod
                   AND acct.acct-cat             EQ iAcctCat
                   AND SUBSTRING(acct.acct,10,7) EQ iUnc
                   AND SUBSTRING(acct.acct,19,2) EQ iForex
                   /* USE-INDEX acct-curr */ NO-LOCK:
       IF SUBSTRING(acct.acct,17,2) GT vCnt THEN
           vCnt = SUBSTRING(acct.acct,17,2).
   END.

   IF vCnt NE "" THEN
   DO:
       IF INT64(vCnt) + iStep LT 10 THEN 
           oUt_Result = "0".
       oUt_Result = oUt_Result + STRING(INT64(vCnt) + iStep).
   END.                                           

   ELSE
       oUt_Result = iInitCnt.
END PROCEDURE.

   /* Для определения значения доп.реквизита СПОДПрибУбыт */
   /* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "Уст_СПОДПРИБУБ"
   &DESCRIPTION   = "Для определения значения доп.реквизита СПОДПрибУбыт"
   &PARAMETERS    = "НОМЕР СЧЕТА"
   &RESULT        = "Значение ДР для установки"
   &SAMPLE        = "Уст_СПОДПРИБУБ(@acct)"
   }
   DEF INPUT  PARAM iAcct      AS CHAR NO-UNDO.   /* Номер счета */
   DEF OUTPUT PARAM oUt_Result AS CHAR NO-UNDO.   /* Результат   */
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO. 

   DEF VAR vResult   AS CHAR NO-UNDO.
   
   {pchkpar iAcct}

   RUN GetCodeSpodPU IN h_acct (iAcct,
                                GetBaseOpDate(),
                                OUTPUT vResult).
   ASSIGN
      oUt_Result = vResult
      is-ok      = 0
   .
END PROCEDURE.

{pfuncdef
   &NAME          = "ФИЗЛИЦО"
   &DESCRIPTION   = "Определяет данные физлица по счету, аналог ф-ии ФизЛицо ~
в стандартных транзакциях.~~n~
Параметр ТИП ДАННЫХ может иметь значения:~~n~
1  - Фамилия~~n~
2  - Имя, Отчетство~~n~
3  - Код страны~~n~
4  - Адрес~~n~
5  - тип документа~~n~
6  - код(номер) документа~~n~
7  - кем выдан (с к/п) + дата выдачи документа~~n~
8  - дата выдачи документа~~n~
9  - кем выдан (с к/п)~~n~
10 - кем выдан (без к/п)~~n~
11 - код подразделения~~n~
13 - ИНН"
   &PARAMETERS    = "ТИП ДАННЫХ,СЧЕТ[,ВАЛЮТА]"
   &RESULT        = "ДАННЫЕ"
   &SAMPLE        = "ФИЗЛИЦО("11","40817810000020000025") = '520102'"
   }
   DEFINE INPUT  PARAMETER iType      AS INT64 NO-UNDO. /* тип информации */
   DEFINE INPUT  PARAMETER iAcct      AS CHAR    NO-UNDO. /* счет */
   DEFINE INPUT  PARAMETER iCurrency  AS CHAR    NO-UNDO. /* валюта */
   DEFINE OUTPUT PARAMETER oUt_Result AS CHAR    NO-UNDO INIT ?.
   DEFINE OUTPUT PARAMETER is-ok      AS INT64 INIT -1.

   DEFINE VAR vResult AS CHAR NO-UNDO.
   DEFINE BUFFER xacct FOR acct.
   
   {pchkpar iType iAcct}

   IF iCurrency <> ? THEN DO:
      iCurrency = STRING(INT64(iCurrency),"999").
      {find-act.i
          &bact = xacct
          &acct = iAcct
          &curr = iCurrency
      }
   END.
   ELSE DO:
      {find-act.i
          &bact = xacct
          &acct = iAcct
      }
   END.
   IF AVAIL xacct THEN DO:
      IF xacct.cust-cat = "Ч"
      THEN RUN GetCustInfo2 IN h_cust (iType,
                                       xacct.acct,
                                       xacct.currency,
                                       OUTPUT vResult).
      ELSE vResult = ?.
      ASSIGN
         oUt_Result = vResult
         is-ok      = 0
      .
   END.
END PROCEDURE.

/* Проверка счетов на парность и связывание этих счетов, если они не парные */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "СДЕЛАТЬ_ПАРНЫМИ"
   &DESCRIPTION   = "Проверка счетов на парность и связывание этих счетов, если они не парные"
   &PARAMETERS    = "НОМЕР СЧЕТА1,НОМЕР СЧЕТА2"
   &RESULT        = " "
   &SAMPLE        = "СДЕЛАТЬ_ПАРНЫМИ('40817840000010000091','42309840000000930091')"
   }
   DEF INPUT  PARAM iAcct1     AS CHAR   NO-UNDO.   /* Номер счета1 */
   DEF INPUT  PARAM iAcct2     AS CHAR   NO-UNDO.   /* Номер счета2 */
   DEF OUTPUT PARAM oUt_Result AS CHAR   NO-UNDO.   /* Результат   */
   DEF OUTPUT PARAM is-ok      AS INT64  NO-UNDO. 

   DEF BUFFER bAcct FOR acct.
   DEF BUFFER xAcct FOR acct.
   
   {pchkpar iAcct1}
   {pchkpar iAcct2}
   {find-act.i
         &bact = bAcct
         &acct = iAcct1
         &lockac = "EXCLUSIVE-LOCK"         
   }
   {find-act.i
         &bact   = xAcct
         &acct   = iAcct2
         &lockac = "EXCLUSIVE-LOCK"
   } 
   IF     {assigned bAcct.contr-acct} 
      AND bAcct.contr-acct NE xAcct.contr-acct THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","У счета " + iAcct1 + " уже есть другой парный счет." +
                                            " Невозможно сделать счет " + iAcct2 + " парным").
      RETURN. 
   END.
   IF     {assigned xAcct.contr-acct} 
      AND xAcct.contr-acct ne bAcct.contr-acct THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","У счета " + iAcct2 + " уже есть другой парный счет" +
                                            " Невозможно сделать счет " + iAcct1 + " парным").
      RETURN. 
   END.
   IF NOT {assigned xAcct.contr-acct} THEN 
   DO:
      ASSIGN
         xAcct.contr-acct = bAcct.acct
         bAcct.contr-acct = xAcct.acct
      NO-ERROR. 
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.   
      VALIDATE xAcct NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.      
      VALIDATE bAcct NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
      END.
   END.
   
   ASSIGN
      oUt_Result = ""
      is-ok      = 0
   . 
END PROCEDURE.

/* Проверка счетов на парность и связывание этих счетов, если они не парные */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "СОЗД_СЧЕТ"
   &DESCRIPTION   = "Создает счет, если указаны роль, тип догвоора и номер договора, ~
то привязывать к договору"
   &PARAMETERS    = "МАСКА СЧЕТА,ВАЛЮТА[,РОЛЬ СЧЕТА,ТИП ДОГОВОРА,НОМЕР ДОГОВОРА]"
   &RESULT        = "НОМЕР СЧЕТА"
   &SAMPLE        = "СОЗД_СЧЕТ('40817810кффффссссссс','','Кредит','Кредит','12131@002')"
   }
   DEF INPUT  PARAM iMask      AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCurrency  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iAcctRole  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iContract  AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iContcode  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oUt_Result AS CHAR  NO-UNDO.   /* Результат   */
   DEF OUTPUT PARAM is-ok      AS INT64 NO-UNDO. 
   
   DEF VAR vPos0  AS INT64 NO-UNDO. /* Позиция в шаблоне */
   DEF VAR vPos1  AS INT64 NO-UNDO. /* Позиция в номере  */ 
   DEF VAR vNumb  AS CHAR  NO-UNDO.
   DEF VAR vOtdel AS CHAR  NO-UNDO.

   DEFINE BUFFER bal-acct FOR bal-acct.
   DEFINE BUFFER loan FOR loan.
   DEFINE BUFFER acct FOR acct.
   DEFINE BUFFER loan-acct FOR loan-acct.

   FIND FIRST bal-acct WHERE
              bal-acct.bal-acct = INT64(SUBSTRING(iMask,1,5))
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bal-acct THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","Не найден балансовый счет второго порядка " + 
                                            SUBSTRING(iMask,1,5)).
   END.
   
   IF {assigned iContract} THEN
   DO:
      FIND FIRST loan WHERE loan.contract  eq iContract
                        AND loan.cont-code EQ iContcode
      NO-LOCK NO-ERROR.
      IF NOT AVAIL loan THEN
      DO:          
         is-ok = -1.
         RUN Fill-SysMes IN h_tmess("","","-1","Не найден договор тип " + 
                                               iContract + " номер " + iContcode).
      END.
      IF INDEX(iMask, "g") > 0 THEN
      DO:
          ASSIGN
             vPos0 = LENGTH(iMask)
             vNumb = FILL("0", vPos0) + STRING(YEAR(loan.open-date))
             vPos1 = LENGTH(vNumb).
          DO WHILE TRUE:
             vPos0 = R-INDEX(iMask, "g" , vPos0).
             IF vPos0 <= 0 THEN LEAVE.
             ASSIGN
                SUBSTR(iMask, vPos0) = SUBSTR(vNumb, vPos1 , 1)
                vPos1 = vPos1 - 1
                vPos0 = vPos0 - 1.
          END.
      END. 
      vOtdel = loan.filial-id.
   END.
   IF vOtdel EQ "" THEN
      vOtdel = TRIM(GetUserBranchId(userid("bisquit"))).
   RELEASE acct NO-ERROR.
   /* создаем счет */
   RUN Cm_acct_cr IN h_acct (
          "acct" + bal-acct.acct-cat,  /* iClass                  */
          bal-acct.bal-acct,           /* iBal                    */
          iCurrency,                   /* iCurr                   */
          '',                          /* iCustCat                */
          '',                          /* iCustID                 */
          gend-date,                   /* iOpenDate               */
          OUTPUT oUt_Result,           /* oAcct                   */
          BUFFER acct,                 /* BUFFER iacct FOR acct . */
          iMask,                       /* iAcctMask               */
          '',                          /* iDetails                */
          '',                          /* iKauId                  */
          '',                          /* iContract               */
          USERID ('bisquit'),          /* iUserId                 */
          vOtdel,                      /* iBranchId               */
          YES                          /* iCopyBalXattr           */
   ) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN 
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",ERROR-STATUS:GET-MESSAGE(1)). 
   END.
   RUN BalToAcct_Xattr IN h_acct(RECID(acct),"*",YES,YES).
   IF {&RETURN_VALUE} EQ "ERROR" THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 "Ошибка при инициализация доп.реквизитов со счета " +
                                 "2-го порядка и из классификатора [МаскиНаслед]").
   END.
   /* Если передан договор и роль счета, то привязываем счет */
   IF     {assigned iAcctRole}
      AND {assigned iContcode} THEN
   DO:
      CREATE loan-acct.
      loan-acct.cont-code = loan.cont-code.
      {lacc.ini
          &loan-acct = loan-acct
          &contract  = loan.contract
          &acct      = acct.acct
          &currency  = acct.currency
          &acct-type = iAcctRole
          &c-since   = gend-date
      }
      RELEASE loan-acct.
   END.
   RELEASE acct.
END PROCEDURE.

{pfuncdef
   &NAME          = "СУММА_РЕЗ"
   &DESCRIPTION   = "Возвращает сумму резерва на дату"
   &PARAMETERS    = "НОМЕР СЧЕТА, ВАЛЮТА СЧЕТА[,ДАТА = ДАТА ОПЕРДНЯ]"
   &RESULT        = "СУММА"
   &SAMPLE        = "СУММА_РЕЗ('10201810000020010028','',DATE('01/01/2012'))~ 
- получение суммы резерва на 01/01/2012"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate       AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL    NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   IF iCurrency <> "" THEN
   DO:
      {pchkpar iCurrency}
   END.

   IF iDate = ? THEN iDate = GetBaseOpDate().
   oUt_Result  = DesReserveAmount(iAcct,
                                  iCurrency,
                                  iDate).
   IF oUt_Result = ? THEN oUt_Result = 0.

END PROCEDURE.

{pfuncdef
   &NAME          = "БЛОК_СЧ"
   &DESCRIPTION   = "Возвращает список блокировок счета"
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА СЧЕТА"
   &RESULT        = "СПИСОК БЛОКИРОВОК"
   &SAMPLE        = "БЛОК_СЧ('10201810000170010004,') = 'БлокДб'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct}

   oUt_Result = BlockAcct(iAcct + "," + iCurrency,DATETIME(gend-date,MTIME)).

END PROCEDURE.

{pfuncdef
   &NAME          = "ДОСТ_ОСТАТОК"
      &DESCRIPTION   = "Расчитывает остаток на счете клиента по формуле Сальдо на ~~n~
закрытый день + документы зачисления в 'крыже' - ~~n~
документы списания - документы в К2"
   &PARAMETERS    = "Номер счета,валюта счета,дата начала периода,дата конца периода, ~~n~
статус из НП 'СтандТр/AccessStatus', статус для не совпадающих с ~~n~
маской AccessAcct счетов, процедура рассчета остатка(cli-pos), ~~n~
Учитывать документы в К2,Очередность платежа"
   &RESULT        = "Остаток на счете"
   &SAMPLE        = "ДОСТ_ОСТАТОК(@acct(10),@currency(10),дата(),дата(),~~n~
настройка('СтандТр','AccessStatus','П'),'П','cli-pos',НЕТ,*)"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iCurr       AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iDate       AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iDate2      AS DATE        NO-UNDO.
   DEFINE INPUT  PARAMETER iStat       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iStat2      AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iProc       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iCrd        AS LOGICAL     NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq        AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER iBlk        AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INTEGER     NO-UNDO.

   DEFINE VARIABLE         vBal        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE         vVal        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE         vBlk        AS LOGICAL     NO-UNDO.

   {pchkpar iAcct iCurr iDate iDate2 iStat iStat2 iProc iCrd iSeq iBlk}

   /* запускаем процедуру рассчета остатка и радуемся */
   IF NOT {assigned iSeq} THEN iSeq = "*".
   IF NOT {assigned iBlk} THEN vBlk = no.
                          ELSE vBlk = CAN-DO("YES,Yes,yes,ДА,Да,да",iBlk).

   RUN CalcAvailPos IN h_op(iAcct,iCurr,iDate,iDate2,iStat,iStat2,iProc,iCrd,iSeq,vBlk,
                            OUTPUT vBal,OUTPUT vVal).

   IF iCurr EQ ""
      THEN ASSIGN
              oUt_Result = vBal
              is-ok = 1
           .
      ELSE ASSIGN
              oUt_Result = vVal
              is-ok = 1
           .

END PROCEDURE.

/* Осуществляет копирование доп. реквизитов со счета 2-го порядка
   на лицевой счет. Осуществляет инициализацию доп. реквизитов,
   указанных в класссификаторе МаскиНаслед на лицевом счете.  */
/* ===================================--=-=-=-=-= */
{pfuncdef
   &NAME          = "ДР_С_БАЛСЧ"
   &DESCRIPTION   = "Копирует ДР со счета 2-го порядка на лицевой счет."
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА СЧЕТА"
   &RESULT        = "ДР_С_БАЛСЧ"
   &SAMPLE        = "ДР_С_БАЛСЧ('40817810200215441532','')"
   }
   DEF INPUT  PARAM iAcct      AS CHAR  NO-UNDO.
   DEF INPUT  PARAM iCurrency  AS CHAR  NO-UNDO.
   DEF OUTPUT PARAM oUt_Result AS CHAR  NO-UNDO.   /* Результат   */
   DEF OUTPUT PARAM is-ok      AS INT64 NO-UNDO. 
   
   DEF BUFFER acct FOR acct.

   FIND FIRST acct WHERE
              acct.acct     EQ iAcct
          AND acct.currency EQ iCurrency
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE acct THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1","Не найден счет " + iAcct + " в валюте " + iCurrency).
   END.
         
   RUN BalToAcct_Xattr IN h_acct(RECID(acct),"*",YES,YES).
   IF {&RETURN_VALUE} EQ "ERROR" THEN
   DO:
      is-ok = -1.
      RUN Fill-SysMes IN h_tmess("","","-1",
                                 "Ошибка при инициализация доп.реквизитов со " +
                                 "счета 2-го порядка и из классификатора [МаскиНаслед]").
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "БЛОК_СЧ_ОП"
   &DESCRIPTION   = "Возвращает список действующих блокировок счета для ~~n~
указанной очередности платежа"
   &PARAMETERS    = "НОМЕР СЧЕТА,ВАЛЮТА СЧЕТА,ОЧЕРЕДНОСТЬ ПЛАТЕЖА"
   &RESULT        = "СПИСОК БЛОКИРОВОК"
   &SAMPLE        = "БЛОК_СЧ_ОП('10201810000170010004','','6') = 'БлокДб'"
   }
   DEFINE INPUT  PARAMETER iAcct       AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iSeq        AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oUt_Result  AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64    NO-UNDO.

   {pchkpar iAcct iCurrency iSeq}

   IF NOT {assigned iSeq} THEN iSeq = "*".

   oUt_Result = BlckAcctOrdPay(iAcct + "," + iCurrency,DATETIME(gend-date,MTIME),iSeq).

END PROCEDURE.

{pfuncdef
   &NAME          = "СЧ_БАНКРОТ"
   &DESCRIPTION   = "Определение сведений о банкротстве клиента счёта"
   &PARAMETERS    = "Номер счёта,Валюта счёта[,Дата=Дата опердня]"
   &RESULT        = "Список из 4 элементов с разделителем CHR(1),~
 любой элемент которого кроме 1-го может быть пустым. Элементы списка:~
   - является ли клиент банкротом (Да/Нет);~
   - стадия банкротства клиента (code.code из классификатора НБКИ_ТипБанкротства);~
   - номер дела о банкротстве (строка произвольного формата);~
   - дата решения о банкротстве клиента (в формате 99.99.99)."
   &SAMPLE        = "@TmpStr = СЧ_БАНКРОТ(@acct(10), @currency(10), @OpDate)"
   }
   DEFINE INPUT  PARAMETER iAcct     LIKE acct.acct     NO-UNDO.
   DEFINE INPUT  PARAMETER iCurrency LIKE acct.currency NO-UNDO.
   DEFINE INPUT  PARAMETER iDate     AS   DATE          NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult   AS   CHARACTER     NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok     AS   INT64         NO-UNDO.

   DEFINE BUFFER acct FOR acct.

   DEFINE VARIABLE vIsBankrupt AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vStage      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDocNum     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEndDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE vS          AS CHARACTER NO-UNDO.

   {pchkpar iAcct iCurrency}

   {find-act.i &acct = iAcct
               &curr = iCurrency}
   IF AVAIL(acct) AND CAN-DO(FGetSetting("БанкрКонтр","НазнСпецСч",""),
                             acct.contract)
   THEN vIsBankrupt = YES.
   ELSE                                          
   RUN GetBankrupcyInfo IN h_acct (iAcct,
                                   iCurrency,
                                   iDate,
                                   OUTPUT vIsBankrupt,
                                   OUTPUT vStage,
                                   OUTPUT vDocNum,
                                   OUTPUT vEndDate).
   IF vIsBankrupt = ? THEN
      is-ok = -1.
   ELSE DO:
      vS = IF vIsBankrupt THEN "Да" ELSE "Нет".
      {additem3.i oResult vS 1}
      vS = IF {assigned vStage} THEN vStage ELSE "".
      {additem3.i oResult vS 1}
      vS = IF {assigned vDocNum} THEN vDocNum ELSE "".
      {additem3.i oResult vS 1}
      vS = IF vEndDate = ? THEN "" ELSE STRING(vEndDate, "99.99.99").
      {additem3.i oResult vS 1}
   END.
END PROCEDURE.

{pfuncdef
   &NAME          = "ПОИСК_СЧЕТА"
   &DESCRIPTION   = "Возвращает номер счета по коду и параметрам из  ~~n~
буфера транзакции"
   &PARAMETERS    = "КОД СЧЕТА"
   &RESULT        = "НОМЕР СЧЕТА"
   &SAMPLE        = "ПОИСК_СЧЕТА('СейфСчет')"
   }

   DEFINE INPUT  PARAMETER iCode   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iPref   AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER iDate   AS DATE       NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok   AS INT64      NO-UNDO.

   DEFINE BUFFER DataBlock FOR DataBlock.
   DEFINE BUFFER DataAttr  FOR DataAttr.

   DEFINE VARIABLE vDataClass-Id AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDateP446     AS DATE        NO-UNDO.
   DEFINE VARIABLE vDebug        AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vValue        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCodeAttr     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vValueLst     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vValueLst2    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOldPars      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOldParams    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCurOldParam  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vParams       AS CHARACTER   NO-UNDO EXTENT 10.
   DEFINE VARIABLE vCounter      AS INT64       NO-UNDO.
   DEFINE VARIABLE vCustId       AS INT64       NO-UNDO.

   IF iDate EQ ? THEN
      iDate = gend-date.

   IF NOT {assigned iPref} THEN
      iPref = "".

   vDebug = GetAttrValue2("",0,"FADebug") NE "??_ERROR_!!".
   /* Для ряда типов поиска отличается алгоритм до и после П446 */
   IF CAN-DO("СчДРВО,СчДВО",iCode) THEN
   DO:
      vDateP446 = DATE(fGetSetting("ЦБ446П","Дата446П","01/01/2016")).
      IF vDebug THEN
         RUN Fill-SysMes IN h_tmess("", "", "",
         SUBSTITUTE("Отладка поиска счета: ~nКод справочника: &1~nДата поиска: &2~nДата 446П: &3",
                             iCode,STRING(iDate),STRING(vDateP446))).
      
      IF iDate <= vDateP446 THEN
      DO:
         ASSIGN
            vOldPars   = GetAttrValue2("",0,iPref + "OldPars")
            vOldParams = GetAttrValue2("",0,iPref + "OldParams")
            .
         DO vCounter = 1 TO NUM-ENTRIES(vOldParams):
            vCurOldParam = ENTRY(vCounter,vOldParams).
            IF vCurOldParam BEGINS "#" THEN
            DO:
               vCurOldParam = ENTRY(2,vCurOldParam,"#").
               vParams[vCounter] = GetAttrValue2("",
                                                 0,
                                                 iPref + vCurOldParam).
               IF vParams[vCounter] = "??_ERROR_!!" THEN
               DO:
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("Не заполнена переменная ~"&1&2~" необходимая для ~
                             поиска счета ~"&3~".",
                             iPref,vCurOldParam,iCode)).
                  is-ok = -1.
               END.
            END.
            ELSE
            DO:
               vParams[vCounter] = vCurOldParam.
            END.
         END.
         CASE vOldPars:
            WHEN "НОМЕР_СЧЕТА_ВС" THEN
            DO:
               RUN PARSFUNC-НОМЕР_СЧЕТА_ВС IN h_pvok (vParams[1], 
                                                      vParams[2], 
                                                      vParams[3], 
                                                      vParams[4],
                                                      vParams[5],
                                                      vParams[6],
                                                      OUTPUT oResult, 
                                                      OUTPUT is-ok).
               IF vDebug THEN
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("Отладка поиска счета: ~nПоиск по алгоритму до П446: ~
                             &1~nПараметры: &2~nНайденный счет: &3",
                             vOldPars,vOldParams,oResult)).
            END.
            WHEN "СПРАВОЧНИКЗН" THEN
            DO:
               iCode = vParams[1].
            END.
         END CASE.
      END.
   END.

   IF NOT {assigned oResult} THEN
      /*
   CASE iCode:
      WHEN "СчДВО"    THEN
      OTHERWISE
      */
      DO:
         /* Находим последнее описание универсального справочника */
         FIND LAST DataBlock WHERE 
                   DataBlock.DataClass-Id EQ iCode
               AND DataBlock.beg-date     <= iDate
            NO-LOCK NO-ERROR.
         IF AVAIL DataBlock THEN
         DO:
            vDataClass-Id = DataBlock.DataClass-Id + "|" + STRING(DataBlock.Data-Id).
            /* Для каждого критерия находим значение в буфере транзакции */
            FOR EACH DataAttr WHERE 
                     DataAttr.DataClass-Id EQ vDataClass-Id
             AND NOT DataAttr.IsView
             NO-LOCK BY DataAttr.order:
               ASSIGN
                  vCodeAttr = iPref + REPLACE(GetMangledName(DataAttr.DataAttr-Id)," ","_")
                  vValue    = GetAttrValue2("",0,vCodeAttr)
                  .
               IF vValue = "??_ERROR_!!" THEN
               DO:
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("Не заполнена переменная ~"&1~" необходимая для поиска счета ~"&2~".",
                             vCodeAttr,iCode)).
                  is-ok = -1.
               END.
               IF     DataAttr.DataAttr-Id EQ "currency" 
                  AND vValue               EQ ""         THEN
                  vValue = "810".

               IF    DataAttr.DataAttr-Id EQ "cust-cat" THEN
               DO:
                  vCustId = INT64(GetAttrValue2("",0,iPref + "cust-id")) NO-ERROR.
                  IF ClientXAttrVal(vValue,vCustId,"Предпр") BEGINS "Пред" THEN
                     vValue = "П".
                  ELSE
                  DO:
                     IF vValue EQ "Ю" AND
                        CAN-DO(FGetSetting("СтандТР", "СтатусФЛЧП", ""),
                               GetValueAttr("cust-corp", STRING(vCustId), "cust-stat"))
                     THEN
                        vValue = "П".
                  END.
               END.
               {additem.i vValueLst vValue}
                  /*
               IF DataAttr.DataAttr-Id EQ "currency" THEN
               DO:
                  {additem.i vValueLst2 vValue}
               END.
               ELSE
               DO:
                  {additem.i vValueLst2 "*"}
               END. */
            END.
            IF is-ok >= 0 THEN
            DO:
               oResult = GetRefVal(iCode,iDate,vValueLst).
               IF NOT {assigned oResult} THEN
                  oResult = GetRefVal(iCode,iDate,vValueLst2).
               IF vDebug THEN
                  RUN Fill-SysMes IN h_tmess("", "", "",
                  SUBSTITUTE("&1&2~nЗначения критериев: &3~nНайденный счет: &4",
                             "Отладка поиска счета: ~nКод справочника: ",
                             iCode,
                             vValueLst,
                             oResult)).
            END.
         END.
      END.
  /* END CASE. */

END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/12/2015 06:42:01.259+04:00' */
/* $LINTUSER='elus' */
/* $LINTMODE='1' */
/* $LINTFILE='pp-pacct.p' */
/*prosignjI6579U5xvUj93bkqplxUg*/
/* --- pp-pacct.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am --- */
