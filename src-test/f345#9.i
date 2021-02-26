/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: f345#9.i
      Comment: Расчет данных о ежедневных остатках подлежащих страхованию
               денежных средств физических лиц, размещенных во вклады
   Parameters:
         Uses:
      Used BY:
      Created: 13/10/05 YUSS  (52439)
     Modified: 22/12/06 SALN  (0054859) Добавлен столбец Курс валюты
     Modified: 28/03/07 SALN  (0075488) Исправлено ошибка формирования протокола
     Modified: 24/07/07 SALN  (0076674) При расчете класса исключаются лицевые счета,
                                        если они удовлетворяют условиям маски, приведенной
                                        в формуле f345_BalNot (класс f345)
*/

{intrface.get db2l}
{intrface.get date}
{intrface.get parsq}
{makettbal.i}
{sh-defs.i}

DEFINE VARIABLE mForID               AS INT64 NO-UNDO.
DEFINE VARIABLE mTotal               AS INT64   NO-UNDO . /* термометр - всего        */

DEFINE VARIABLE vCountColInt         AS INT64   NO-UNDO . /* кол-во элементов списка масок */
DEFINE VARIABLE vCountAcctInt        AS INT64   NO-UNDO . /* кол-во ЛС                */
DEFINE VARIABLE vAllCountAcctInt     AS INT64   NO-UNDO . /* кол-во ЛС всего          */
DEFINE VARIABLE vCountNo0AcctInt     AS INT64   NO-UNDO . /* кол-во не нулевых ЛС     */
DEFINE VARIABLE vCountAcctGT100Int   AS INT64   NO-UNDO . /* кол-во ЛС > 100000 р.    */
DEFINE VARIABLE vSumAcctLE100Dec     AS DECIMAL   NO-UNDO . /* Сумма ЛС <= 100000 р. по элементу маски  */
DEFINE VARIABLE vSumBalAcctDec       AS DECIMAL   NO-UNDO.  /* сумма ЛС вкл. в БС */
DEFINE VARIABLE vSumBadBalAcctDec    AS DECIMAL   NO-UNDO.  /* сумма ЛС искл. из расчета вкл. в БС */

DEFINE VARIABLE vBalExcludeAcctMaskChar AS CHARACTER NO-UNDO.  /* раб. перемен. - маска исключаемых БС */
DEFINE VARIABLE vBalAcctMaskChar     AS CHARACTER NO-UNDO.  /* раб. перемен. - маска БС */
DEFINE VARIABLE vBalAcctChar         AS CHARACTER NO-UNDO.  /* раб. перемен. - БС */
DEFINE VARIABLE vTmpChar             AS CHARACTER NO-UNDO.  /* раб. перемен. - символьная */
DEFINE VARIABLE vSym1Char            AS CHARACTER NO-UNDO.  /* раб. перемен. - символьная */
DEFINE VARIABLE vSumValDec           AS DECIMAL   NO-UNDO EXTENT 5.  /* раб. перемен. */

DEFINE VARIABLE vMaskCntInt          AS INT64   NO-UNDO.  /* раб. перемен. - цикла    */
DEFINE VARIABLE vMaskCntNo0Int       AS INT64   NO-UNDO.  /* раб. перемен. - цикла    */
DEFINE VARIABLE vMaskSumBalAcctDec   AS DECIMAL   NO-UNDO.  /* раб. перемен. */
DEFINE VARIABLE vMaskCntAcctGT100Int AS INT64   NO-UNDO.  /* раб. перемен. - цикла    */
DEFINE VARIABLE vMaskSumAcctLE100Dec AS DECIMAL   NO-UNDO.  /* раб. перемен. */
DEFINE VARIABLE mListf345excBalTmp   AS CHARACTER NO-UNDO.

/* данные для справочной информации */
DEFINE VARIABLE vSprCntInt       AS INT64 NO-UNDO.  /* count ЛС                         */
DEFINE VARIABLE vSprCntNo0Int    AS INT64 NO-UNDO.  /* count не нулевых ЛС              */
DEFINE VARIABLE vSprCntGT100Int  AS INT64 NO-UNDO.  /* count ЛС, если сумма ЛС превышающих 100000 р.*/
DEFINE VARIABLE vSprItgShBalDec  AS DECIMAL NO-UNDO.  /* суммы ЛС по маске БС             */
DEFINE VARIABLE vSprItgLE100Dec  AS DECIMAL NO-UNDO.  /* суммы ЛС по маске БС, если сумма ЛС не более 100000 р.*/

DEFINE VARIABLE hTypQ            AS HANDLE     NO-UNDO.
DEFINE VARIABLE mWhere           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mPrevMonth       AS INT64    NO-UNDO INIT ?.
DEFINE VARIABLE mBegFor          AS DATE       NO-UNDO.
DEFINE VARIABLE mEndFor          AS DATE       NO-UNDO.

DEFINE VARIABLE mBrList          AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mForListPredpr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mForList         AS CHARACTER  NO-UNDO.

&IF DEFINED(Instr3198-U)  &THEN
PROCEDURE pGetCustSigns:
   DEFINE INPUT  PARAMETER iAcctSurr   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCustCat    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCustId     AS INT64     NO-UNDO.
   DEFINE OUTPUT PARAMETER oIsCustCorp AS LOGICAL   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPredpr     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oSubject    AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vCustCat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustId  AS CHARACTER NO-UNDO.

   IF CAN-DO(mListAcctNoControl,SUBSTRING(iAcctSurr,1,5)) THEN
      RETURN.
   
   IF iCustCat = "В" THEN DO:
      vCustCat = GetXAttrValue("acct", iAcctSurr, "ТипКл").
      IF vCustCat > "" THEN
         vCustId = GetXAttrValue("acct", iAcctSurr, "IDCust").
   END.
   ELSE
      ASSIGN
         vCustCat = iCustCat
         vCustId  = STRING(iCustId)
         .
   IF vCustCat <> "Ю" THEN RETURN.
   /* Только по юр.лицам */
   oIsCustCorp = YES.
   oPredpr  = GetXAttrValueEx("cust-corp", vCustId, "Предпр", "").
   oSubject = GetXAttrValueEx("cust-corp", vCustId, "Субъект", "").
END PROCEDURE.
&ELSE
DEFINE VARIABLE mListMaskBACustCorpChar AS CHARACTER  NO-UNDO.

/* Искл. счета клиентов из cust-corp не ИП */
FUNCTION fIPCustCorp RETURNS LOGICAL
   (INPUT iAcct       AS CHAR,
    INPUT iCurr       AS CHAR,
    INPUT iBalAcct    AS INT64,
    INPUT iCust-cat   AS CHAR,
    INPUT iCust-id    AS INT64):

   DEFINE VARIABLE mRetLog    AS LOGICAL   NO-UNDO INIT NO.
   DEFINE VARIABLE mKlnt-Cat  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mBalAcct   AS CHARACTER NO-UNDO.

   mKlnt-Cat = iCust-cat.
   mBalAcct  = STRING(iBalAcct,"99999").
   IF mKlnt-Cat =  "В" THEN
      mKlnt-Cat = GetXAttrValueEx("acct", iAcct + "," + iCurr,"ТипКл","").

   IF mKlnt-Cat =  "Ю" THEN
      mRetLog = CAN-DO(mListMaskBACustCorpChar,mBalAcct).

   RETURN mRetLog.

END FUNCTION.
&ENDIF

/* Список подразделений для отбора счетов и проводок */
{getbrlst.i dataBlock.Branch-ID mBrList}

ASSIGN
   vListDopChkAcctChar = FGetSetting ( "Форма345","f345_Predpr","" )
   mListf345excBalTmp  = FGetSetting ( "Форма345","f345_excBalTmp","" )
   &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
   mFromFor            = (Param-Calc =  "ФОР")
   &ENDIF
   vCountColInt        = NUM-ENTRIES(vListIncludeAcctChar)
   mTotal              = vCountColInt
   vLastDateSym1       = fDate2Sym(DataBlock.End-date)
   mWhere              = fGetSetting ("Форма345","НП_ДатаСправочно","")
   &IF DEFINED(Instr3198-U) = 0 &THEN
   mListMaskBACustCorpChar = fGetSetting ("Форма345","ИсклСчетаЮр_НеИП","")
   &ENDIF
.

DEFINE TEMP-TABLE ttP342 NO-UNDO
   FIELD acct     AS CHARACTER
   FIELD curr     AS CHARACTER
   FIELD p255_not AS LOGICAL
   FIELD f345_exc AS LOGICAL
   &IF DEFINED(Instr3198-U) &THEN
   FIELD isCustCorp AS LOGICAL
   FIELD Predpr     AS CHARACTER
   FIELD Subject    AS CHARACTER
   &ELSE
   FIELD IPCustCorp AS LOGICAL
   &ENDIF
   FIELD bal-acct AS INT64   /* БС */
   FIELD Val      AS DECIMAL EXTENT 2
   INDEX Idx bal-acct acct
.
DEFINE VARIABLE hQuery   AS HANDLE     NO-UNDO.
DEFINE VARIABLE hAcct    AS HANDLE     NO-UNDO.
DEFINE VARIABLE vHAcctBuf AS HANDLE      NO-UNDO.
DEFINE VARIABLE mValDec  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vRwd     AS ROWID      NO-UNDO.
DEFINE VARIABLE mCustCat AS CHARACTER  NO-UNDO.

&IF DEFINED(DCLASS_F345SPRR) &THEN
IF mClcSprav THEN DO:
&ENDIF
   &IF DEFINED(Instr3198-U) = 0 &THEN
   IF mFromFor THEN
   DO:
      RUN SetSysConf IN h_Base ("ReturnAcctRowid","YES").
      RUN ParsqInitTempTable IN h_Parsq ("acct").
   
      /* Перечень лицевых счетов */
      RUN find_formula IN h_olap ("h255m",
                                  "p255_f",              /* код формулы */
                                  DataBlock.End-Date,    /* дата */
                                  YES,
                                  BUFFER formula).
   
      IF AVAIL formula THEN
      DO:

         &IF DEFINED (JoinPrint) &THEN
            {norm-beg.i &defs=yes &nodate=yes &nofil=yes }
            RUN SetLevel IN h_Debug (-1).
         &ENDIF
   
         RUN makeTTbal (vListIncludeAcctChar).  /* список масок БС */
   
         ASSIGN
            mForList       = ""
            mForListPredpr = ""
         .
   
         FOR EACH ttBal WHERE CAN-DO((IF mJoinPrintLog THEN ENTRY(2,mJoinPrintChar) ELSE "*"),STRING(ttBal.Bal-acct)):
            IF NOT CAN-DO(vListExcludeAcctChar,STRING(ttBal.Bal-acct)) THEN
            DO:
               vTmpChar = STRING(ttBal.Bal-acct).
               IF CAN-DO(vListDopChkAcctChar, STRING(ttBal.Bal-acct)) THEN   /* счета из f345_Predpr */
               DO:
                  {additem.i mForListPredpr vTmpChar}
               END.
               ELSE
               DO:
                  {additem.i mForList vTmpChar}
               END.
            END.
         END.
   
         RUN normpars.p (REPLACE(formula.formula,"~{list_bal~}",REPLACE(mForListPredpr,",",";")),
                         DataBlock.beg-date,
                         DataBlock.end-date,
                  OUTPUT mValDec).
   
         &IF DEFINED (JoinPrint) &THEN
            {norm-end.i &nofil=yes }
         &ENDIF
   
         fGetTempHandle ("acct", OUTPUT hAcct).
         RUN ParsqGetQuery IN h_Parsq (       "acct",
                                       OUTPUT hQuery).
         vHAcctBuf = hQuery:GET-BUFFER-HANDLE(2).
   
         {for_dqry.i hQuery}

            IF NOT CAN-DO(vListExcludeAcctChar,STRING(vHAcctBuf::bal-acct)) AND
               CAN-DO(vListDopChkAcctChar, STRING(vHAcctBuf::bal-acct)) AND
               NOT CAN-FIND(FIRST ttP342 WHERE 
                                  ttP342.bal-acct = vHAcctBuf::bal-acct
                              AND ttP342.acct     = vHAcctBuf::acct) THEN
            DO:
               CREATE ttP342.
               ASSIGN
                  ttP342.acct     = vHAcctBuf::acct
                  ttP342.curr     = vHAcctBuf::currency
                  ttP342.bal-acct = vHAcctBuf::bal-acct
               .
               ASSIGN
                  ttP342.p255_not = (GetXattrValueEx("acct",
                                                     vHAcctBuf::acct + "," + vHAcctBuf::currency,
                                                     "p255_not",
                                                     "") >  "")
                  ttP342.f345_exc = (GetXattrValueEx("acct",
                                                     vHAcctBuf::acct + "," + vHAcctBuf::currency,
                                                     "f345_exc",
                                                     "") >  "")
                  ttP342.IPCustCorp = fIPCustCorp(vHAcctBuf::acct,
                                                  vHAcctBuf::currency,
                                                  vHAcctBuf::bal-acct,
                                                  vHAcctBuf::cust-cat,
                                                  vHAcctBuf::cust-id).
            END.
         END.
   
         RUN SetSysConf IN h_Base ("ReturnAcctRowid",?).
   
      END.
      ELSE
      DO:
         PUBLISH "normDbg" (0,"ОШИБКА","В классе данных 'h255m' отсутствует формула : 'p255_f'").
      END.
   
      /* ФОР счета из НП ИсклСчетаЮр_НеИП  */
      RUN makeTTbal (mListMaskBACustCorpChar).  /* список масок БС */
      FOR EACH ttBal
         WHERE CAN-DO((IF mJoinPrintLog THEN ENTRY(2,mJoinPrintChar) ELSE "*"),STRING(ttBal.Bal-acct)),
         FIRST bal-acct OF ttBal
         NO-LOCK:
         FOR EACH acct WHERE
                  acct.bal-acct =  bal-acct.bal-acct
              AND CAN-DO(mBrList, acct.branch-id)
              AND (acct.cust-cat = "Ю" OR
                   acct.cust-cat = "В")
              AND (acct.close-date = ? OR
                   acct.close-date >= vBegDate)
              AND  acct.open-date  <= vEndDate
              {&and-acct}
            NO-LOCK:

            IF fIPCustCorp(acct.acct,acct.currency,acct.bal-acct,acct.cust-cat,cust-id) THEN
            DO:
               /* Формирования таблицы отобранных ЛС */
               FIND FIRST ttP342 WHERE ttP342.bal-acct =  acct.bal-acct
                                   AND ttP342.acct     =  acct.acct     NO-ERROR.
               IF NOT AVAIL ttP342 THEN
               DO:
                  CREATE ttP342.
                  ASSIGN
                     ttP342.acct     = acct.acct
                     ttP342.curr     = acct.currency
                     ttP342.bal-acct = acct.bal-acct
                  .
                  IF mFromFor THEN
                     ASSIGN
                        ttP342.p255_not = (GetXattrValueEx("acct",acct.acct + "," + acct.currency,"p255_not","") >  "")
                        ttP342.f345_exc = (GetXattrValueEx("acct",acct.acct + "," + acct.currency,"f345_exc","") >  "")
                        ttP342.IPCustCorp = YES.
               END.
            END.
         END.
      END.
   
      RUN makeTTbal (mForList).  /* список масок БС */
   
   END.
   ELSE
   &ENDIF /* &IF DEFINED(Instr3198-U) = 0 */
   DO:
      RUN makeTTbal (vListIncludeAcctChar).  /* список масок БС */
   END.
   
   /* ФОР счета вне f345_Predpr, не ФОР - счета из f345_BalAcct  */
   FOR EACH ttBal
      WHERE NOT CAN-DO(vListExcludeAcctChar,STRING(ttBal.Bal-acct)) AND
            CAN-DO((IF mJoinPrintLog THEN ENTRY(2,mJoinPrintChar) ELSE "*"),STRING(ttBal.Bal-acct)),
      FIRST bal-acct OF ttBal
      NO-LOCK:
      FOR EACH acct WHERE
               acct.bal-acct =  bal-acct.bal-acct
           AND CAN-DO(mBrList, acct.branch-id)
           AND (acct.cust-cat = "Ч" OR
                acct.cust-cat = "В"
                &IF DEFINED(Instr3198-U) &THEN
                OR acct.cust-cat = "Ю"
                OR (CAN-DO(mListAcctNoControl,STRING(acct.bal-acct)) AND
                    acct.cust-cat = "Б")
                &ENDIF
                )
           AND (acct.close-date = ? OR
                acct.close-date >= vBegDate)
           AND  acct.open-date  <= vEndDate
           {&and-acct}
         NO-LOCK:
         IF acct.cust-cat = "В" THEN DO:
            mCustCat = GetXAttrValue("acct", acct.acct + "," + acct.currency, "ТипКл").
            IF mCustCat = "Б" 
               &IF DEFINED(Instr3198-U) &THEN
               AND NOT CAN-DO(mListAcctNoControl,STRING(acct.bal-acct))
               &ENDIF
               THEN NEXT.
         END.
   
         /* Формирования таблицы отобранных ЛС */
         FIND FIRST ttP342 WHERE ttP342.bal-acct =  acct.bal-acct
                             AND ttP342.acct     =  acct.acct     NO-ERROR.
         IF NOT AVAIL ttP342 THEN
         DO:
            CREATE ttP342.
            ASSIGN
               ttP342.acct     = acct.acct
               ttP342.curr     = acct.currency
               ttP342.bal-acct = acct.bal-acct
            .
            IF mFromFor THEN
            DO:
               ttP342.p255_not = (GetXattrValueEx("acct",acct.acct + "," + acct.currency,"p255_not","") >  "").
               ttP342.f345_exc = (GetXattrValueEx("acct",acct.acct + "," + acct.currency,"f345_exc","") >  "").
               &IF DEFINED(Instr3198-U) &THEN
               RUN pGetCustSigns(acct.acct + "," + acct.currency,
                                 acct.cust-cat,
                                 acct.cust-id,
                                 OUTPUT ttP342.isCustCorp,
                                 OUTPUT ttP342.Predpr,
                                 OUTPUT ttP342.Subject).
               &ELSE
               ttP342.IPCustCorp = fIPCustCorp(acct.acct,acct.currency,acct.bal-acct,acct.cust-cat,cust-id).
               &ENDIF
            END.
         END.
      END.
   END.
   
   /* Формирование таблицы типов курсов по датам */
   RUN pTypeKurs (vBegDate,vEndDate).
   
   CREATE QUERY hTypQ.
   hTypQ:SET-BUFFERS (BUFFER ttTypeKurs:HANDLE).
   hTypQ:FORWARD-ONLY = TRUE.
   {bar-beg2.i &BarTotal=vCountColInt}
   /* по 1 - му элементу из списка масок БС */
   DO vIInt = 1 TO vCountColInt :
      /*  Определение данных для расчета элемент списка */
      ASSIGN
         vBalAcctMaskChar = ENTRY(vIInt,vListIncludeAcctChar)  /* Элемент списка масок БС */
      .
      RUN makeTTbal (vBalAcctMaskChar).
   
      IF NOT gRemote THEN
         PUT SCREEN ROW SCREEN-LINES + MESSAGE-LINES COL 14
            STRING(vBalAcctMaskChar
                 + " (" + STRING(vIInt) + ") "
                 + " (" + STRING(vCountColInt) + ") "
                 ,"x(50)").
   
      FOR EACH ttBal
         WHERE NOT CAN-DO(vListExcludeAcctChar,STRING(ttBal.Bal-acct)) AND
               CAN-DO((IF mJoinPrintLog THEN ENTRY(2,mJoinPrintChar) ELSE "*"),STRING(ttBal.Bal-acct)),
         FIRST bal-acct OF ttBal
         NO-LOCK:
         &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
         IF mFromFor /* Расчет из ФОР */
         THEN
         DO:
            FOR EACH ttTypeKurs
               BY ttTypeKurs.sym1:
   
               /* Если новый месяц - пересчитываем ФОР */
               IF MONTH (ttTypeKurs.symDate) <> mPrevMonth THEN
               DO:
                  /* Первый день лежит не в том месяце, к которому относится */
                  ASSIGN
                     mBegFor = firstMonDate(MAX (ttTypeKurs.symDate,DataBlock.Beg-Date))
                     mEndFor = lastMonDate (MAX (ttTypeKurs.symDate,DataBlock.Beg-Date))
                  .
                  /* Тысячный месячный ФОР */
                  RUN sv-get.p (       "p255bmt",
                                       DataBlock.Branch-id,
                                       mBegFor,
                                       mEndFor,
                                OUTPUT mForID).
                  mPrevMonth = MONTH (ttTypeKurs.symDate).
               END.
               RUN pCalcFor (ttTypeKurs.sym1).
            END.
   
            /*  Формирование данных ЛС на дату, у которых есть ДР p255_not и ДР f345_exc и НП ИсклСчетаЮр_НеИП */
            FOR EACH ttP342 WHERE ttP342.bal-acct = bal-acct.bal-acct
                              &IF DEFINED(Instr3198-U) = 0 &THEN
                              AND (ttP342.p255_not OR ttP342.f345_exc OR ttP342.IPCustCorp)
                              &ENDIF
                              NO-LOCK,
               FIRST acct WHERE acct.acct       =  ttP342.acct
                            AND acct.currency   =  ttP342.curr
               NO-LOCK :
               RUN pCalcAcct (vBegDate,vEndDate,NO).
            END.
            /*  Формирование СПРАВОЧНО , у которых ДР f345_exc - отсутсвует */
            IF mClcSprav THEN
            DO:
               FOR EACH ttP342 WHERE ttP342.bal-acct = bal-acct.bal-acct
                                 AND NOT ttP342.f345_exc
                                 &IF DEFINED(Instr3198-U) = 0 &THEN
                                 AND NOT ttP342.IPCustCorp
                                 &ENDIF
                  NO-LOCK,
                  FIRST acct WHERE acct.acct       =  ttP342.acct
                               AND acct.currency   =  ttP342.curr
                  NO-LOCK :
                  RUN pCalcAcct (vEndDate,vEndDate,YES).
               END.
            END.
         END.
         ELSE
         &ENDIF /* &IF DEFINED(DCLASS_F345SPRR) = 0 */
         DO:
            /* Расчет по лицевым счетам */
            FOR EACH ttP342 WHERE ttP342.bal-acct = bal-acct.bal-acct NO-LOCK,
               FIRST acct WHERE acct.acct          =  ttP342.acct
                            AND acct.currency      =  ttP342.curr
               NO-LOCK :
               
               /*  Формирование данных ЛС на дату */
               &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
               &IF DEFINED(Instr4212-U) = 0 OR DEFINED(JoinPrint) > 0 &THEN
               IF vBegDate =  vEndDate THEN RUN pCalcAcct (vBegDate,vEndDate,NO). /* вот здесь была ошибка по заявке 0202148 :  */
               ELSE &ENDIF 
               RUN pCalcAcct (vBegDate,vEndDate - 1,NO). 
               &ENDIF
               RUN pCalcAcct (vEndDate,vEndDate,YES).
               
            END.
         END.
      END.
      {bar2.i &BarPointer=vIInt}
   END.
   /* Сохранение врем. таблицы */
   RUN saveData.
   
   {del-bar.i}
   DELETE OBJECT hTypQ.
&IF DEFINED(DCLASS_F345SPRR) &THEN
END. /* IF mClcSprav THEN */
&ENDIF

/* Процедура расчета 1 ЛС за период. */
/* Для каждого дня периода и для предыдущего дня */
PROCEDURE pCalcAcct:
   DEFINE INPUT  PARAMETER vBegDate  AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER vEndDate  AS DATE       NO-UNDO.
   DEFINE INPUT  PARAMETER vSpravLog AS LOGICAL    NO-UNDO.

   DEFINE VARIABLE mSym1    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mSym2    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mVal1Dec AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE mVal2Dec AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE mDay     AS DATE       NO-UNDO.
   DEFINE VARIABLE xDate    AS DATE       NO-UNDO.
   DEFINE VARIABLE vTmpTypeKursChar AS CHARACTER NO-UNDO. /* Учетный,Фор            */
   DEFINE VARIABLE vAcctShBalDec    AS DECIMAL NO-UNDO.   /* сумма ЛС               */

   DEFINE VARIABLE vRemChar        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vMsgBadAcctChar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpChar        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vKursChar       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCntLog         AS LOGICAL   NO-UNDO INIT YES.
   DEFINE VARIABLE vSign           AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vMsgBadAcctCharf345exc AS CHARACTER       NO-UNDO.    /*для счетов из НП f345_excBalTmp*/
   DEFINE VARIABLE vEachDayF345exc        AS LOGICAL INIT NO NO-UNDO.
   DEFINE VARIABLE vF345_exc              AS LOGICAL         NO-UNDO.
   DEFINE VARIABLE vTmpLog                AS LOGICAL INIT NO NO-UNDO. 
   DEFINE BUFFER ttTypeKurs FOR ttTypeKurs.

   ASSIGN
      vBalAcctChar    = STRING(bal-acct.bal-acct,"99999")
      &IF DEFINED(DCLASS_F345SPRR) = 0 AND DEFINED(Instr3198-U) > 0 &THEN
      vEachDayF345exc = CAN-DO(mListf345excBalTmp,vBalAcctChar)
      &ENDIF
   .
   &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
   IF mFromFor THEN DO:
      ASSIGN
         vRemChar = (IF ttP342.p255_not THEN "есть д.р. p255_not" ELSE "")
         &IF DEFINED(Instr3198-U) = 0 &THEN
         vRemChar = vRemChar
                  + (IF ttP342.IPCustCorp THEN (IF vRemChar >  "" THEN ", " ELSE "") + "клиент 'Ю' и счет 2-го порядка удовл. НП 'Форма345'->'ИсклСчетаЮр_НеИП' " ELSE "")
         &ENDIF
      .
      IF NOT vEachDayF345exc THEN
         ASSIGN 
         vF345_exc = ttP342.f345_exc
         vRemChar  = vRemChar
                     + (IF ttP342.f345_exc THEN (IF vRemChar >  "" THEN ", " ELSE "") + "есть д.р. f345_exc" ELSE "").

      &IF DEFINED(Instr3198-U) &THEN
      vSign = 1.
      IF ttP342.p255_not AND (NOT ttP342.isCustCorp OR ttP342.Predpr > "" OR ttP342.Subject = "ФЛП") THEN
         vSign = 1.
      ELSE IF ttP342.isCustCorp AND (ttP342.Predpr = "" AND ttP342.Subject <> "ФЛП") AND NOT ttP342.p255_not THEN
         vSign = -1.
      ELSE IF (IF NOT vEachDayF345exc THEN ttP342.f345_exc ELSE TRUE) THEN
      DO:
         IF NOT vEachDayF345exc THEN vSign = -1.
         ELSE vTmpLog = YES.
      END.
      ELSE
         RETURN.
      &ENDIF
   END.
   ELSE
   &ENDIF /* &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN */
      ASSIGN
         vMsgBadAcctChar = fBadAcct345 (acct.acct + "," + acct.currency,
                                        acct.cust-cat,
                                        acct.cust-id,
                                        vListDopChkAcctChar,
                                        NOT vEachDayF345exc,  /*проверка ДР F345_exc*/
                                        YES,                  /*остальные проверки*/
                                        DataBlock.End-Date)
         vCntLog         = vCntLog AND (vMsgBadAcctChar =  "")
      .

   DO mDay = vEndDate TO vBegDate BY -1:
      
      /* Кроме остатка, определяем и дату его изменения */
      RUN acct-pos IN h_base
         (acct.acct,
          acct.currency,
          mDay,
          mDay,
          CHR (251)).
      
      IF LastMove = ? THEN
         LastMove = vBegDate.

      /* По всем дням, в которых остаток на счете не менялся */
      BlockTypeKurs:
      FOR EACH ttTypeKurs WHERE
               ttTypeKurs.symDate <= mDay
           AND ttTypeKurs.symDate >= MAX (LastMove,vBegDate)
            BY ttTypeKurs.symDate DESC:

         &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
         IF vEachDayF345exc THEN 
         DO:
            IF mFromFor THEN 
            DO:
               vF345_exc = (fBadAcct345 (acct.acct + "," + acct.currency,
                                               acct.cust-cat,
                                               acct.cust-id,
                                               vListDopChkAcctChar,
                                               vEachDayF345exc,     /*проверка ДР F345_exc*/
                                               NO,                  /*остальные проверки*/
                                               ttTypeKurs.symDate) >  "").
               IF vTmpLog THEN
               DO:
                  &IF DEFINED(Instr3198-U) &THEN
                  ASSIGN 
                     vSign     = (IF vF345_exc THEN -1 ELSE 1)
                  .
                  &ENDIF
                  IF vF345_exc =  NO THEN NEXT BlockTypeKurs. 
               END.
            END.
            ELSE 
               ASSIGN
                  vMsgBadAcctCharf345exc = fBadAcct345 (acct.acct + "," + acct.currency,
                                                        acct.cust-cat,
                                                        acct.cust-id,
                                                        vListDopChkAcctChar,
                                                        vEachDayF345exc,     /*проверка ДР F345_exc*/
                                                        NO,                  /*остальные проверки*/
                                                        ttTypeKurs.symDate).
         END.
         &ENDIF
         
         ASSIGN
            xDate              = ttTypeKurs.symDate
            mSym1              = fDate2Sym (xDate)
            vTmpTypeKursChar   = ttTypeKurs.TypeKurs
            vAcctShBalDec      = IF acct.currency >  ""
                                 THEN CurToBase (vTmpTypeKursChar,
                                                 acct.curr,
                                                 xDate,
                                                 ABSOLUTE (sh-val))
                                 ELSE ABSOLUTE (sh-bal)
            vKursChar = IF acct.curr <> ""
                           THEN STRING(FindRate(vTmpTypeKursChar, acct.curr,xDate)) ELSE ""
         .
         
         CREATE ttAcct.
         ASSIGN
            ttAcct.Msg           = ttTypeKurs.Msg                   /* Заголовок */
            ttAcct.GGGGMMDD      = mSym1                            /* дата */
            ttAcct.MaskBalAcct   = vBalAcctMaskChar                 /* маска БС */
            ttAcct.BalAcct       = vBalAcctChar                     /* БС */
            ttAcct.Acct          = acct.acct + "," + acct.currency  /* ЛС */
            ttAcct.Rate          = vKursChar                        /* Курс */
            ttAcct.Val           = (
                                   &IF DEFINED(DCLASS_F345SPRR) = 0 &THEN
                                   IF mFromFor AND NOT vSpravLog
                                    THEN
                                       (IF (ttP342.p255_not AND vF345_exc) 
                                            &IF DEFINED(Instr3198-U) = 0 &THEN
                                            OR
                                           (ttP342.p255_not AND ttP342.IPCustCorp)
                                           &ENDIF
                                                               THEN 0
                                                               ELSE
                                                                  &IF DEFINED(Instr3198-U) &THEN
                                                                  vSign * vAcctShBalDec)
                                                                  &ELSE
                                                                  IF ttP342.f345_exc OR ttP342.IPCustCorp
                                                                  THEN 0 - vAcctShBalDec
                                                                  ELSE vAcctShBalDec)         /* Остаток на дату по ЛС */
                                                                  &ENDIF
                                    ELSE
                                    &ENDIF
                                       vAcctShBalDec)
            ttAcct.Cnt           = vCntLog                     AND
                                   (vMsgBadAcctCharf345exc =  "") AND
                                   ((acct.close-date = ? OR         /* Count */
                                     acct.close-date >= xDate) AND
                                     acct.open-date  <= xDate)
            ttAcct.AcctRemBad    = TRIM(vMsgBadAcctCharf345exc + " " + vMsgBadAcctChar)   /* Remark */
            ttAcct.sprav         = vSpravLog
            ttAcct.remark        = vRemChar 
                                   + (IF vF345_exc AND vEachDayF345exc 
                                      THEN (IF vRemChar >  "" THEN ", " ELSE "") + "есть д.р. f345_exc" 
                                      ELSE "")
         .
                                           
         &IF DEFINED(DCLASS_F345SPRR) > 0 AND DEFINED(Instr3198-U) > 0 &THEN
         RUN FillTTAcctSpr(acct.acct + "," + acct.currency, acct.cust-cat, acct.cust-id).
         &ENDIF
         IF ttAcct.AcctRemBad = "" THEN
         DO:
            FIND FIRST ttBalAcct WHERE
                       ttBalAcct.GGGGMMDD    = ttAcct.GGGGMMDD
                   AND ttBalAcct.MaskBalAcct = ttAcct.MaskBalAcct
                   AND ttBalAcct.BalAcct     = ttAcct.BalAcct
               NO-LOCK NO-ERROR.

            IF NOT AVAIL ttBalAcct THEN
            DO:
               CREATE ttBalAcct.
               ASSIGN
                  ttBalAcct.GGGGMMDD    = ttAcct.GGGGMMDD    /* дата */
                  ttBalAcct.MaskBalAcct = ttAcct.MaskBalAcct /* маска БС */
                  ttBalAcct.BalAcct     = ttAcct.BalAcct     /* БС */
               .
            END.
         END.
      END. /* FOR EACH ttTypeKurs WHERE */
      /* Пропускаем все учтенные дни */
      mDay = MAX (LastMove,vBegDate).
   END.
END PROCEDURE.

PROCEDURE pCalcFOR:
   DEFINE INPUT  PARAMETER iDateStr AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vBalAcctChar AS CHARACTER  NO-UNDO.

   DEFINE BUFFER ForLine FOR DataLine.

   vBalAcctChar = STRING(bal-acct.bal-acct,"99999").

   FIND FIRST ttBalAcct WHERE
              ttBalAcct.GGGGMMDD    = iDateStr
          AND ttBalAcct.MaskBalAcct = vBalAcctMaskChar
          AND ttBalAcct.BalAcct     = vBalAcctChar
      NO-LOCK NO-ERROR.

   IF NOT AVAIL ttBalAcct THEN
   DO:
      CREATE ttBalAcct.
      ASSIGN
         ttBalAcct.GGGGMMDD     = iDateStr         /* дата */
         ttBalAcct.MaskBalAcct  = vBalAcctMaskChar /* маска БС */
         ttBalAcct.BalAcct      = vBalAcctChar     /* БС */
      .
   END.

   FOR EACH ForLine WHERE
            ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s2_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        &IF DEFINED(Instr4212-U) &THEN
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s11_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s12_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s31_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s32_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        &ELSEIF DEFINED(Instr3198-U) &THEN
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s1_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        OR  ForLine.Data-ID =  mForID
        AND ForLine.Sym1 BEGINS "p2s3_" + vBalAcctChar
        AND ForLine.Sym2 =      iDateStr
        &ENDIF
      NO-LOCK QUERY-TUNING(DEBUG SQL NO-INDEX-HINT):

      /* Суммируем */
      CREATE ttAcct.
      ASSIGN
         ttAcct.GGGGMMDD      = iDateStr                         /* дата */
         ttAcct.MaskBalAcct   = vBalAcctMaskChar                 /* маска БС */
         ttAcct.BalAcct       = vBalAcctChar                     /* БС */
         ttAcct.Acct          = ForLine.Sym1                     /* ЛС */
         ttAcct.Val           = (ForLine.Val[1] + ForLine.Val[2]) * 1000   /* Остаток на дату по ЛС */
         ttAcct.Cnt           = NO                              /* Count */
         ttAcct.AcctRemBad    = ""                               /* Remark */
         ttAcct.sprav         = NO
      .
   END.
END PROCEDURE.
/* $LINTFILE='f345#9.i' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='tsl' */
/* $LINTDATE='07/06/2017 12:35:40.739+03:00' */
/*prosignbsquTuM2os0buyf0hC3ttA*/