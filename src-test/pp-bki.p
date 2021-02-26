/* Вставка комментария Плюс банк
  ayv поправлены функции GetFirstDueDate и BKICredLimitAmt(смотри FOR EACH)
  из-за OR CreditRegistry выгружалось в ~10 раз медленнее
Конец комментария Плюс банк */
/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: pp-bki.p
      Comment: Инструмент для предоставления информации по кредитам для НБКИ
   Parameters: нет
         Uses:
      Used BY:
      Created: 27/04/2006 Gorm (59661)
     Modified: 07.11.2006 16:00 Daru     <comment>
     Modified: 09/12/2006 NIK Профилирование
     Modified: 23.05.2008 MUTA 0093248 При расчете частоты выплат ОД/процентов
               если условия на дату выгрузки не найдены - ищутся первые условия.
     Modified: 25.07.2008 MUTA 0095193 Если НБКИ_СостДата не заполнен или больше даты выгрузки, то НБКИ_Состояние не учитывается

F BKIAccType            Тип счета (4)
F BKIAccRelation        Отношение к счету (5)
F BKIDateOFLstPay       Дата последней выплаты (7)
P BKIAccRating          Состояние счета (8) и Дата Состояния счета (9)
F BKICredLimitAmt       Лимит кредита/ исходная сумма кредита (11)
F BKICredBalance        Баланс (12)
F BKIPastDue            Просрочка (13)
F BKINextPayment        Следующий платеж (14)
F BKICredPayFreq        Частота выплат (15)
F BKIMOP                Своевременность платежей (16)
F BKICollatCode         Код залога  (18)
F BKIDatePayDue         Дата финального платежа (20)
F BKIDateInterPayDue    Дата финальной выплаты процентов (21)
F BKIInterPayFreq       Частота выплат процентов (22)
F BKIAmountOutst        Текущая задолженность (25)
*/
{globals.i}
{intrface.get xclass}
{intrface.get lv}
{intrface.get loan}
{intrface.get refer}
{intrface.get i254}
{intrface.get date}
{intrface.get db2l}
{intrface.get pogcr}
{intrface.get trans}
{lshpr.pro}
{debug.equ}
{profile.def}
{pick-var.i}

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "bki"
   &LIBNAME       = "Библиотека функций для предоставления информации по кредитам для НБКИ."
   &DESCRIPTION   = "Функции для предоставления информации по кредитам для НБКИ."
   }

DEFINE TEMP-TABLE ttLoanType NO-UNDO 
   FIELD fProd AS CHARACTER 
   FIELD fPos  AS CHARACTER 
   FIELD fProv AS CHARACTER 
   FIELD fTerm AS DECIMAL  
   FIELD fSumm AS DECIMAL 
   FIELD fVal  AS INT64
   FIELD fCode AS CHARACTER  
INDEX prim fTerm fSumm.

/*----------------------------------------------------------------------------*/
/* Поиск записи в классификаторе в поле misc по вхождению                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetCodeByMisc RETURN CHAR
    (iClass AS CHAR,   /* Класс классификатора */
     iCount AS INT64,    /* В каком поле misc смотреть */
     iVal   AS CHAR):  /* Значение */

   DEF VAR vAccType AS CHAR NO-UNDO.
   DEF BUFFER bcode FOR CODE.
   {profile BK601}
      /* ищем запись в классификаторе в соотв. поле misc по вхождению,
      ** кроме "*" */
   FIND FIRST bcode WHERE
              bcode.class =  iClass
          AND CAN-DO(bcode.misc[iCount],iVal)
          AND bcode.misc[iCount] <> "*"
             NO-LOCK NO-ERROR.

      /* если не нашли, то ищем "*" */
   IF NOT AVAIL bcode THEN
      FIND FIRST bcode WHERE
                 bcode.class        =  iClass
             AND bcode.misc[iCount] =  "*"
                 NO-LOCK NO-ERROR.

    /* если нашли, то берем код из классификатора */
   IF AVAIL bcode THEN vAccType = bcode.code.
   {profile BK610}
   RETURN vAccType.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Поиск последнего параметра по одному договору                              */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDateParam:
   DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT  PARAM iOper      AS INT64  NO-UNDO.    /* Код параметра */
   DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* Дата отчета */
   DEF OUTPUT PARAM oLstDate   AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.

   {profile BK151}
   oLstDate = ?.

lint1:
   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND bloan-int.id-k      = iOper
        AND bloan-int.mdate    <= iDate
            NO-LOCK
       BY bloan-int.mdate DESC:

       oLstDate = bloan-int.mdate.

       LEAVE lint1.
   END.

lint2:
   FOR EACH bloan-int WHERE
            bloan-int.contract  = iContract
        AND bloan-int.cont-code = iContCode
        AND bloan-int.id-d      = iOper
        AND bloan-int.mdate    <= iDate
            NO-LOCK
         BY bloan-int.mdate DESC:

      IF oLstDate =  ?               OR
         oLstDate =  bloan-int.mdate THEN
         oLstDate = bloan-int.mdate.

      LEAVE lint2.
   END.

   {profile BK160}
END.
/*----------------------------------------------------------------------------*/
/* Поиск последней операции                                                   */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDateOper:
    DEF INPUT  PARAM iContract  AS CHAR NO-UNDO.
    DEF INPUT  PARAM iContCode  AS CHAR NO-UNDO.
    DEF INPUT  PARAM iOper      AS INT64  NO-UNDO.    /* Код операции */
    DEF INPUT  PARAM iDate      AS DATE NO-UNDO.    /* Дата отчета */
    DEF OUTPUT PARAM oLstDate   AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.
   DEFINE BUFFER b_chowhe FOR chowhe.

   oLstDate = ?.
   {profile BK161}

   FIND FIRST b_chowhe WHERE
              b_chowhe.id-op =  iOper
              NO-LOCK NO-ERROR.
   IF AVAILABLE(b_chowhe) THEN DO:
      FIND LAST bloan-int WHERE
                bloan-int.contract  =  iContract
            AND bloan-int.cont-code =  iContCode
            AND bloan-int.id-k      =  b_chowhe.id-k
            AND bloan-int.id-d      =  b_chowhe.id-d
            AND bloan-int.mdate     <= iDate
                NO-LOCK NO-ERROR.
      IF AVAILABLE(bloan-int) THEN oLstDate = bloan-int.mdate.
   END.
   {profile BK170}
END.
/*----------------------------------------------------------------------------*/
/* Возвращение оборота операции по ID операции                                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummOper.
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iOper      AS INT64  NO-UNDO.   /* Вид операции */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.   /* Дата отчета */
   DEF OUTPUT PARAM oAmtPrm   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DEC  NO-UNDO.   /* Затычки */
   DEF OUTPUT PARAM oAmtCr    AS DEC  NO-UNDO.   /* Затычки */

   DEFINE BUFFER b_chowhe FOR chowhe.
   DEFINE BUFFER b_lint   FOR loan-int.
   {profile BK171}
   ASSIGN
      oAmtPrm = 0.00
      oAmtDb  = 0.00
      oAmtCr  = 0.00
   .

   FIND FIRST b_chowhe WHERE
              b_chowhe.id-op =  iOper
              NO-LOCK NO-ERROR.

   IF AVAIL b_chowhe THEN
      FOR EACH b_lint WHERE
               b_lint.contract  =  iContract     AND
               b_lint.cont-code =  iContCode     AND
               b_lint.id-d      =  b_chowhe.id-d AND
               b_lint.id-k      =  b_chowhe.id-k AND
               b_lint.mdate     <= iDate
               NO-LOCK:
         oAmtPrm = oAmtPrm + b_lint.amt-rub.
      END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetSummOper","iContract = " + iContract
                                 + " iContCode = " + iContCode
                                 + " iOper = " + string(iOper)
                                 + " iDate = " +
                            (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                 + " oAmtPrm = " + STRING(oAmtPrm)).

   &ENDIF
   {profile BK180}
END.
/*----------------------------------------------------------------------------*/
/* Дата первого очередного выноса на просрочку                                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetPastDate:
   DEF INPUT PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate     AS DATE NO-UNDO.    /* дата отчета               */
   DEF INPUT PARAM iDatePrev AS DATE NO-UNDO.    /* предыдущая дата выгрузки  */
   DEF OUTPUT PARAM oPstDate AS DATE NO-UNDO.

   DEF BUFFER bloan-int FOR loan-int.
   DEF BUFFER bop       FOR op.

   oPstDate = ?.
   {profile BK181}
lint:
   FOR EACH bloan-int WHERE
            bloan-int.contract  =  iContract
        AND bloan-int.cont-code =  iContCode
        AND bloan-int.id-d      =  7
        AND (bloan-int.id-k     =  0 OR
             bloan-int.id-k     =  13)
        AND bloan-int.mdate     <= iDate
        AND bloan-int.mdate     >  iDatePrev
            NO-LOCK
         BY bloan-int.mdate:

       FIND FIRST bop WHERE
                  bop.op =  bloan-int.op
                  NO-LOCK NO-ERROR.
       IF AVAIL bop THEN DO:
          oPstDate = bop.op-date.
          LEAVE lint.
       END.
   END.
   {profile BK190}
END.

/*----------------------------------------------------------------------------*/
/* вычисление суммы операций по списку параметров                             */
/*----------------------------------------------------------------------------*/

{pfuncdef
   &DEFPROC="ListGetSummByIntVar"
   &DESCRIPTION="вычисление суммы операций по списку параметров"
   &PARAMETERS="буфер договора,дата,код параметров"
   &RESULT=" "
   &SAMPLE="ListGetSummByIntVar(buffer loan,iDate,"ТекОД")"}
FUNCTION ListGetSummByIntVar RETURNS DECIMAL
    (BUFFER vLoan FOR loan,  /* кредитный договор */
     iDate     AS DATE,     /* дата выгрузки */
     iListOper AS CHAR):    /* список параметров классификатора НБКИ_КодОпер */

   DEF VAR vAmtCur   AS DEC  NO-UNDO. /* сумма по одной операции */
   DEF VAR vAmt      AS DEC  NO-UNDO. /* общая сумма */
   DEF VAR vI        AS INT64  NO-UNDO. /* счетчик */

   {profile BK241}
   ASSIGN vAmt      = 0.00.

   DO vI = 1 TO NUM-ENTRIES(iListOper):
       vAmtCur = 0.
       RUN GetSummByIntVar IN THIS-PROCEDURE (BUFFER vLoan,
                           iDate,
                           ENTRY(vI,iListOper),
                           OUTPUT vAmtCur).
      vAmt = vAmt + vAmtCur.
   END.
   {profile BK250}
   RETURN vAmt.
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* Вычисление суммы по операциям и остаткам по параметрам                     */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummByIntVar:
   DEF PARAM  BUFFER loan FOR loan.               /* Кредитный договор        */
   DEF INPUT  PARAM  iDate AS DATE      NO-UNDO.  /* Дата отчета              */
   DEF INPUT  PARAM  iCode AS CHARACTER NO-UNDO.  /* Код - что вычисляем      */
   DEF OUTPUT PARAM  oAmt  AS DECIMAL   NO-UNDO.  /* Сумма по операциям и параметрам */

   DEF VAR vI       AS INT64    NO-UNDO.
   DEF VAR vAmtCur  AS DECIMAL    NO-UNDO.
   DEF VAR vAmtDb   AS DECIMAL    NO-UNDO.
   DEF VAR vAmtCr   AS DECIMAL    NO-UNDO.
   DEF VAR vContract AS CHARACTER NO-UNDO.
   DEF VAR vContCode AS CHARACTER NO-UNDO.

   DEF BUFFER bcode FOR CODE.
   DEF BUFFER bloan FOR loan.

   {profile BK191}
   IF NOT AVAIL loan THEN RETURN ERROR "Ошибка поиска договора.".

   IF NOT GetCodeBuff("НБКИ_КодОпер",iCode, BUFFER bcode)
       THEN RETURN ERROR "В классификаторе НБКИ_КодОпер нет параметра " + iCode.

   ASSIGN
      oAmt      = 0.00
      vContract = loan.contract
      vContCode = loan.cont-code
   .

   /* список операций, которые нужно сложить */
   IF bcode.misc[1] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[1])),
                     iDate,
                     "GetSummOper",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt + vAmtCur.
   END.

  /* список операций, которые нужно вычесть */
   IF bcode.misc[2] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[2]):
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[2])),
                     iDate,
                     "GetSummOper",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt - vAmtCur.
   END.

  /* список параметров, остатки по которым нужно сложить */
   IF bcode.misc[3] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
      vAmtCur = 0.00.
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[3])),
                     iDate,
                     "GetSummPar",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt + vAmtCur.
   END.

  /* список параметров, остатки по которым нужно вычесть */
   IF bcode.misc[4] > "" THEN
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[4]):
      vAmtCur = 0.00.
      RUN GetSummUnv(vContract,
                     vContCode,
                     INT64(ENTRY(vI,bcode.misc[4])),
                     iDate,
                     "GetSummPar",
                     OUTPUT vAmtCur,
                     OUTPUT vAmtDb,
                     OUTPUT vAmtCr).
      oAmt = oAmt - vAmtCur.
   END.

  /* суммируем проценты из Interest */
   IF bcode.misc[5] =  "Да" THEN
   DO:
      DO vI = 1 TO EXTENT(pick-var):
         vAmtCur = GetLoanProcent(BUFFER loan, vI).
         oAmt = oAmt + vAmtCur.
         /* по всем траншам */
         FOR EACH bloan WHERE bloan.contract  =  loan.contract
                          AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ") + " "
                          AND NUM-ENTRIES(bloan.cont-code," ") >  1
         NO-LOCK:            
            vAmtCur = GetLoanProcent(BUFFER bloan, vI).
            oAmt = oAmt + vAmtCur.
         END.
      END.
      /* вычитаем пени, учтенные в Interest */
      vAmtCur = ListGetSummByIntVar(BUFFER loan,
                                   iDate,
                                   "ПеняОД,ПеняПР").
      oAmt = oAmt - vAmtCur.
   END.
   {profile BK200}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Вычисление даты по операциям и остаткам по параметрам                      */
/*----------------------------------------------------------------------------*/
PROCEDURE GetDateByIntVar:
   DEF INPUT PARAM  iContract AS CHAR NO-UNDO.
   DEF INPUT PARAM  iContCode AS CHAR NO-UNDO.
   DEF INPUT PARAM  iDate     AS DATE NO-UNDO.
   DEF INPUT PARAM  iCode     AS CHAR NO-UNDO.   /* код - что вычисляем       */
   DEF OUTPUT PARAM oDate     AS DATE NO-UNDO.

   DEF VAR vI       AS INT64    NO-UNDO.
   DEF VAR vDateF   AS DATE   NO-UNDO.           /* дата для текущей операции */
   DEF VAR vDateM   AS DATE   NO-UNDO.           /* максимальная дата         */

   DEF BUFFER bcode FOR CODE.

   {profile BK201}
   IF NOT GetCodeBuff("НБКИ_КодОпер",iCode, BUFFER bcode) THEN
      RETURN ERROR "В классификаторе НБКИ_КодОпер нет параметра " + iCode.

   ASSIGN
      vDateM = ?.

   /* список операций, которые нужно сложить */
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[1]):
      RUN GetLastDateUnv(iContract,
                         iContCode,
                         INT64(ENTRY(vI,bcode.misc[1])),
                         iDate,
                         "GetLastDateOper",
                          OUTPUT vDateF).

      IF vDateF <> ?     AND
         vDateM <> ?     AND
         vDateM < vDateF THEN
         vDateM = vDateF.
      ELSE IF vDateM = ? THEN
         vDateM = vDateF.
   END.

  /* список параметров, которые нужно сложить */
   DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
      RUN GetLastDateUnv(iContract,
                         iContCode,
                         INT64(ENTRY(vI,bcode.misc[3])),
                         iDate,
                         "GetLastDateParam",
                         OUTPUT vDateF).

      IF vDateF <> ?     AND
         vDateM <> ?     AND
         vDateM < vDateF THEN
         vDateM = vDateF.
       ELSE IF vDateM = ? THEN
         vDateM = vDateF.
   END.

   oDate = vDateM.
   {profile BK210}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Оплата задолжности за счет обеспечения                                     */
/*----------------------------------------------------------------------------*/
PROCEDURE PayByOB:
    DEF INPUT PARAM  iContract AS CHAR NO-UNDO.
    DEF INPUT PARAM  iContCode AS CHAR NO-UNDO.
    DEF INPUT PARAM  iDate     AS DATE NO-UNDO.
    DEF INPUT PARAM  iDatePrev AS DATE NO-UNDO.            /* Затычка         */
    DEF OUTPUT PARAM oDateOD   AS DATE NO-UNDO.

    DEF VAR vXattrOb AS CHAR NO-UNDO.
    DEF BUFFER bloan-int FOR loan-int.
    DEF BUFFER bop FOR op.

   {profile BK211}

   oDateOD = ?.

    /* по всем операциям основного долга */
bl:
   FOR EACH   bloan-int WHERE
              bloan-int.contract  =  iContract
        AND   bloan-int.cont-code =  iContCode
        AND ((bloan-int.id-d     =  1
        AND   bloan-int.id-k     =  2)
          OR (bloan-int.id-d     =  5
        AND   bloan-int.id-k     =  7)) 
        AND   bloan-int.mdate     <= iDate
            NO-LOCK:

        /* анализируем документ оплаты */
      vXattrOb  = GetXattrValueEx("op",
                                  STRING(bloan-int.op),
                                  "вСчетОбесп",
                                  "").

      IF vXattrOb = "Да" THEN DO:
         FIND FIRST bop WHERE
                    bop.op = bloan-int.op
                    NO-LOCK NO-ERROR.
         IF AVAIL bop THEN
            ASSIGN oDateOD = bop.op-date.
         LEAVE bl.
      END.
   END.
   {profile BK220}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Определяет, был ли спор до определенной даты и возвращает дату его фиксации*/
/*----------------------------------------------------------------------------*/
PROCEDURE GetDisput:
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.  /* дата выгрузки              */
   DEF INPUT PARAM iDatePrev  AS DATE NO-UNDO.  /* предыдущая дата выгрузки   */
   DEF OUTPUT PARAM oDateDisp AS DATE NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.
   {profile BK221}
    /* ищем последний спор */
   FIND LAST bterm-obl WHERE
             bterm-obl.contract  = iContract
         AND bterm-obl.cont-code = iContCode
         AND bterm-obl.idnt      = 7
         AND bterm-obl.end-date <= iDate
             NO-LOCK NO-ERROR.

    /* если нашли спор, проверяем что он возник позже последней даты выгрузки */
   oDateDisp = IF AVAIL bterm-obl AND bterm-obl.end-date > iDatePrev
                  THEN bterm-obl.end-date
                  ELSE ?.
   {profile BK230}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Определяет последнюю дату выноса на просрочку ОД/процентов                 */
/*----------------------------------------------------------------------------*/
PROCEDURE GetLastDueDate:
   DEF INPUT PARAM  iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM  iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM  iDate      AS DATE NO-UNDO. /* дата выгрузки */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO. /* последняя дата выноса на просрочку */

   DEF VAR vListOper AS CHAR NO-UNDO.
   DEF VAR vI        AS INT64  NO-UNDO.
   DEF VAR vDateF    AS DATE NO-UNDO.
   DEF VAR vDateM    AS DATE NO-UNDO.

   ASSIGN
      vDateM = ?
      vListOper = "ЗадолжОД,ЗадолжПр,ВыкупПр,ВыкупПен".

   {profile BK231}
   DO vI = 1 TO NUM-ENTRIES(vListOper):

      RUN GetDateByIntVar(iContract,
                          iContCode,
                          iDate,
                          ENTRY(vI,vListOper),
                          OUTPUT vDateF).
      IF vDateF <> ? AND
         vDateM <> ? AND
         vDateM < vDateF
         THEN vDateM = vDAteF.
      ELSE IF vDateM = ? THEN vDateM = vDateF.
   END.

   oDate = vDateM.
   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetLastDueDate","iContract = " + iContract       +
                                  " iContCode = " + iContCode       +
                                      " iDate = " + STRING(iDate)   +
                                     " vDateM = " + STRING(vDateM)).
   &ENDIF
   {profile BK240}
END.
/*----------------------------------------------------------------------------*/
/* Определяет дату возникновения неоплаченной просрочки ОД/процентов          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetFirstDueDate:
   DEF INPUT PARAM  iContract  AS CHAR NO-UNDO. /* Назначение договора */
   DEF INPUT PARAM  iContCode  AS CHAR NO-UNDO. /* Номер договора */
   DEF INPUT PARAM  iDate      AS DATE NO-UNDO. /* Дата выгрузки */
   DEF OUTPUT PARAM oDate      AS DATE NO-UNDO. /* Дата выноса на просрочку */

   DEF VAR vListOper  AS CHAR NO-UNDO.          /* Список операций выноса на просрочку */
   DEF VAR vI         AS INT64  NO-UNDO.          /* Счетчик */
   DEF VAR vJ         AS INT64  NO-UNDO.          /* Счетчик */
   DEF VAR vDateF     AS DATE NO-UNDO.          /* Промежуточные результаты */
   DEF VAR vDateM     AS DATE NO-UNDO.          /* Промежуточные результаты */
   DEF VAR vAmnt      AS DEC  NO-UNDO.          /* Сальдо */

   DEF BUFFER bcode     FOR code.
   DEF BUFFER bloan-int FOR loan-int.

   ASSIGN
      vDateM    = ?
      vListOper = "ЗадолжОД,ЗадолжПр,ТекВыкПн,ТекВыкПр"
   .

   {profile BK231}
   DO vJ = 1 TO NUM-ENTRIES(vListOper):

      IF NOT GetCodeBuff("НБКИ_КодОпер", ENTRY(vJ, vListOper), BUFFER bcode) THEN
         RETURN ERROR "В классификаторе НБКИ_КодОпер нет параметра " + ENTRY(vJ, vListOper).

         /* По операциям нет возможности определить остаток,
         ** Определяем список параметров, по которым нужно найти дату */
      DO vI = 1 TO NUM-ENTRIES(bcode.misc[3]):
         ASSIGN
            vAmnt  = 0
            vDateF = ?
         .
            /* По договору и траншам */
         FOR EACH bloan-int WHERE
                 (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code =  iContCode
            AND   bloan-int.mdate     <= iDate)
/* Удалено Плюс банк
              OR (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code BEGINS iContCode + " "
            AND   bloan-int.mdate     <= iDate)
Конец удаленного фрагмента Плюс банк */
         NO-LOCK BY bloan-int.mdate:
            IF    bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3]))
               OR bloan-int.id-d =  INT64(ENTRY(vI, bcode.misc[3])) THEN
            DO:
               IF bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3])) THEN
                  vAmnt  = vAmnt - bloan-int.amt-rub.
               ELSE
                  vAmnt  = vAmnt + bloan-int.amt-rub.
               IF vDateF =  ? THEN
                  vDateF = bloan-int.mdate.
               IF vAmnt =  0 THEN
                  vDateF = ?.
            END.
         END.
         IF vDateF <> ? THEN
            IF vDateM =  ? THEN
               vDateM = vDateF.
            ELSE IF vDateM > vDateF THEN
               vDateM = vDateF.
/* Вставка Плюс банк */
         ASSIGN
            vAmnt  = 0
            vDateF = ?
         .
         FOR EACH bloan-int WHERE
                 /* (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code =  iContCode
            AND   bloan-int.mdate     <= iDate)
              OR */ (bloan-int.contract  =  iContract
            AND   bloan-int.cont-code BEGINS iContCode + " "
            AND   bloan-int.mdate     <= iDate)
         NO-LOCK BY bloan-int.mdate:
            IF    bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3]))
               OR bloan-int.id-d =  INT64(ENTRY(vI, bcode.misc[3])) THEN
            DO:
               IF bloan-int.id-k =  INT64(ENTRY(vI, bcode.misc[3])) THEN
                  vAmnt  = vAmnt - bloan-int.amt-rub.
               ELSE
                  vAmnt  = vAmnt + bloan-int.amt-rub.
               IF vDateF =  ? THEN
                  vDateF = bloan-int.mdate.
               IF vAmnt =  0 THEN
                  vDateF = ?.
            END.
         END.
         IF vDateF <> ? THEN
            IF vDateM =  ? THEN
               vDateM = vDateF.
            ELSE IF vDateM > vDateF THEN
               vDateM = vDateF.
/* Конец вставки Плюс банк */
      END.
   END.
   oDate = vDateM.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetFirstDueDate", "iContract = " + iContract      +
                                     "iContCode = " + iContCode      +
                                     "iDate     = " + STRING(iDate)  +
                                     "vDateM    = " + STRING(vDateM)).
   &ENDIF
   {profile BK240}
END.
/*----------------------------------------------------------------------------*/
/* 4. Тип счета                                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIAccType RETURNS CHAR
   (BUFFER loan FOR loan): /* кредитный договор */

   DEF VAR vAccType AS CHAR NO-UNDO.
   DEF VAR vI AS INT64 NO-UNDO.

   DEF BUFFER bcode FOR CODE.
   {profile BK251}
   IF NOT AVAIL loan THEN RETURN "ERROR".

   vAccType = GetXattrValueEx("loan",
                              loan.contract + "," + loan.cont-code,
                              "НБКИ_ТипСчета",
                              "").
   /* если доп. реквизит не задан, то вычисляем его */
   IF vAccType = "" THEN DO:
      /* Для банков фиксированный код в классификаторе */
      IF loan.cust-cat = "Б" THEN vAccType = "15".

      /* для юридических и физических лиц - вычисляем */
      ELSE DO:
/* Вставка Плюс банк */
         DEF VAR vLnType  AS CHAR NO-UNDO.
         DEF BUFFER bloan FOR loan.

         vLnType = loan.cont-type.
         IF (vLnType EQ "Течение")
         THEN DO:
            FIND LAST bloan
                WHERE (bloan.contract   EQ loan.contract)
                  AND (bloan.cont-code  BEGINS loan.cont-code)
                  AND (NUM-ENTRIES(bloan.cont-code, " ") EQ 2)
                NO-LOCK NO-ERROR.
            IF (AVAIL bloan)
            THEN vLnType = bloan.cont-type.
            ELSE vLnType = GetXattrValue("loan", loan.contract + "," + loan.cont-code, "ТипДогСогл").
         END.
         IF (vLnType EQ "") THEN RETURN (IF (loan.cust-cat = "Ч") THEN "9" ELSE (IF (loan.cust-cat = "Ю") THEN "10" ELSE "99")).
/* Конец вставки Плюс банк */
         IF      loan.cust-cat = "Ч" THEN vI = 1.
         ELSE IF loan.cust-cat = "Ю" THEN vI = 2.
         ELSE RETURN "".

          vAccType = GetCodeByMisc("НБКИ_ТипСчета",
                                   vI,
/* Замена Плюс банк
                                   loan.cont-type).
*/                                 vLnType).
/* Конец замены Плюс банк */
      END.
   END.
   {profile BK260}
/* Замена Плюс банк
   RETURN vAccType.
*/ RETURN IF (vAccType = "") THEN (IF (loan.cust-cat = "Ч") THEN "9" ELSE (IF (loan.cust-cat = "Ю") THEN "10" ELSE "99")) ELSE vAccType.
/* Конец замены Плюс банк */
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 5. Отношение к счету                                                       */
/*----------------------------------------------------------------------------*/
FUNCTION BKIAccRelation RETURNS CHAR
   (BUFFER loan     FOR loan,           /* кредитный договор */
    BUFFER term-obl FOR term-obl,       /* договор обеспечения */
    iCustSurr       AS CHAR, 
    iCorp           AS LOGICAL ):       /* является ли ПБОЮЛ*/

   DEF VAR vAccRel     AS CHAR NO-UNDO.
   DEF VAR vFlPricipal AS LOG  NO-UNDO.
   DEF VAR vVersion    AS DEC  NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   IF AVAIL term-obl THEN
      RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                  + " loan.Cont-Code = " + loan.Cont-Code
                                  + " Договор обеспечения ")).
   ELSE IF AVAIL loan THEN
      RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                  + " loan.Cont-Code = " + loan.Cont-Code
                                        + " cust-cat = " + loan.cust-cat)).
   ELSE RUN dbgprint.p("BKIAccRelation","loan.Contract = " + loan.Contract
                                    + " loan.Cont-Code = " + loan.Cont-Code
                                      + " Договор не найден ")).
   &ENDIF

   {profile BK261}
   IF NOT AVAIL loan THEN RETURN ?.

   ASSIGN
      vFlPricipal = FGetSetting("НБКИ","ПринципалСОбесп","Да") =  "Да"
      vVersion    = DEC(GetAttrValue("",0,"NBKIVersion")) NO-ERROR.

   /* если принципала берем с договоров гарантий, выданных банком */
   IF     NOT vFlPricipal
      AND CAN-DO(GetXclassAllChildsEx("loan-guarantee"),loan.class-code)
      AND NOT AVAIL term-obl 
      AND vVersion >= 3.00 THEN
      RETURN "6".
   /* если передали договор обеспечения и принципал с обеспечения */
   ELSE IF AVAIL term-obl THEN 
   DO:
      /*определяем, принципал или поручитель */
      IF iCustSurr =  term-obl.symbol + "," + STRING(term-obl.fop) THEN 
         RETURN "5".
      ELSE
         RETURN "6".
   END.

   IF loan.cust-cat = "Ю" OR loan.cust-cat = "Б" OR iCorp THEN vAccRel = "9".
   ELSE vAccRel = "1".

   IF FGetSetting("НБКИ","Созаемщик","") =  "Да"
      AND NOT AVAIL term-obl 
      AND iCustSurr <> loan.cust-cat + "," + STRING(loan.cust-id)
   THEN vAccRel = "4".

   {profile BK270}
   RETURN vAccRel.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 7. Дата последней выплаты                                                  */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDateOFLstPay RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,            /* кредитный договор */
    iDate AS DATE):               /* дата выгрузки */

   DEF VAR vDateM AS DATE NO-UNDO. /* дата последней выгрузки */
   {profile BK271}

   ASSIGN
      vDateM = ?
      .

   RUN GetDateByIntVar(iContract,
                       iContCode,
                       iDate,
                       "ПогашОбщ",
                       OUTPUT vDateM).
   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("BKIDateOFLstPay","iContract = " + iContract
                                       + " iContCode = " + iContCode
                                       + " vDateM = " + STRING(vDateM)).

   &ENDIF
   {profile BK280}
   RETURN vDateM.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 13. Просрочка                                                              */
/*----------------------------------------------------------------------------*/
FUNCTION BKIPastDue RETURNS DECIMAL
    (BUFFER loan     FOR loan,                   /* кредитный договор         */
            iDate    AS DATE,                    /* дата выгрузки             */
            iDatePay AS DATE):                   /* Дата последней выплаты    */

   DEF VAR mAmt AS DECIMAL NO-UNDO.

   {profile BK281}
   IF NOT AVAIL loan THEN RETURN ?.
   mAmt = ListGetSummByIntVar(BUFFER loan,
                              iDate,
                              "ЗадолжОД,ЗадолжПр,ПеняОД,ПеняПР,ТекВыкПн,ТекВыкПр").
   {profile BK290}
   RETURN mAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 8. Состояние счета, 9. Дата Состояния счета                                */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIAccRating:
   DEF PARAM BUFFER loan FOR loan.                 /* кредитный договор */
   DEF INPUT PARAM iDate        AS DATE NO-UNDO.   /* дата отчета       */
   DEF INPUT PARAM iDatePrev    AS DATE NO-UNDO.   /* дата предыдущей выгрузки */
   DEF INPUT PARAM iDatePay     AS DATE NO-UNDO.   /* дата последней выплаты */
   DEF OUTPUT PARAM oAccRat     AS CHAR NO-UNDO.   /* состояние счета */
   DEF OUTPUT PARAM oDateAccRat AS DATE NO-UNDO.   /* дата состояния счета */

   DEF VAR vAccRat   AS CHAR NO-UNDO.
   DEF VAR vAmtOst   AS DEC  NO-UNDO.
   DEF VAR vAmtCur   AS DEC  NO-UNDO.
   DEF VAR vOD       AS LOG  NO-UNDO.
   DEF VAR vListOper AS CHAR NO-UNDO.
   DEF VAR vDatePay  AS DATE NO-UNDO.
   DEF VAR vI        AS INT64  NO-UNDO.
   DEF VAR vDateOD   AS DATE NO-UNDO.
   DEF VAR vDateDisp AS DATE NO-UNDO.
   DEF VAR vDateSost  AS DATE NO-UNDO.

   DEF BUFFER signs FOR signs.
   DEF BUFFER bloan FOR loan.

   {profile BK291}

   IF NOT AVAIL loan THEN RETURN.
   vAmtOst      = 0.00.

   /* если дата предыдущей выгрузки не задана (первая выгрузка),
   ** то берем дату открытия договора */
   IF iDatePrev = ? THEN iDatePrev = loan.open-date.

   vAccRat = GetXattrValueEx("loan",
                              loan.contract + "," + loan.cont-code,
                              "НБКИ_Состояние",
                              "").

   vDateSost = DATE(GetXattrValue("loan",
                                   loan.contract + "," + loan.cont-code,
                                   "НБКИ_СостДата")) NO-ERROR.

   IF vDateSost =  ? OR vDateSost > iDate THEN vAccRat = "".

   /* определяем дату спора */
   RUN GetDisput(loan.contract,
                 loan.cont-code,
                 iDate,
                 iDatePrev,
                 OUTPUT vDateDisp).

   /* если доп. реквизит не задан, то вычисляем его */
   IF vAccRat = "" THEN DO:

      /* вычисляем сумму задолжности */
      RUN GetSummByIntVar(BUFFER loan,
                          iDate,
                          "ЗадолжОД",
                          OUTPUT vAmtOst).

      /* если задолженность нулевая по ОД, то ищем документ
      ** с пометкой "вСчетОбесп" */
      IF vAmtOst = 0.00
         THEN RUN GetFirstDateUnv(loan.contract,
                                  loan.cont-code,
                                  iDatePay,
                                  ?,
                                  "PayByOB",
                                  OUTPUT vDateOD).

      /* просрочен */
      IF ListGetSummByIntVar(BUFFER loan,
                             iDate,
                             "ЗадолжОД,ЗадолжПр,ТекВыкПн,ТекВыкПр") > 0.00
          THEN DO:
          oAccRat = "52".
          RUN GetFirstDueDate(loan.contract,
                              loan.cont-code,
                              iDate,
                              OUTPUT oDateAccRat).
      END.

      /* передан на обслуживание в другую организацию */
      ELSE IF CAN-FIND (FIRST signs WHERE signs.file-name  =  "loan"
                                      AND signs.surrogate  BEGINS "КредРО,"
                                      AND signs.code       =  "РДог"
                                      AND signs.code-value =  loan.cont-code) THEN
      DO:
         FOR FIRST signs WHERE signs.file-name  =  "loan"
                           AND signs.surrogate  BEGINS "КредРО,"
                           AND signs.code       =  "РДог"
                           AND signs.code-value =  loan.cont-code
             NO-LOCK,
             FIRST bloan WHERE bloan.contract  =  ENTRY(1,signs.surrogate)
                           AND bloan.cont-code =  ENTRY(2,signs.surrogate)
             NO-LOCK:
             ASSIGN
                oAccRat     = "14"  
                oDateAccRat = bloan.open-date.  
         END.   
      END.

      /* есть спор */
      ELSE IF vDateDisp <> ?
              THEN ASSIGN vAccRat = "21"
                          oDateAccRat = vDateDisp.
      /* закрыт */
      ELSE IF loan.close-date <= iDate
              THEN ASSIGN oAccRat = "13"
                          oDateAccRat = loan.close-date.
      /* оплачен в счет обеспечения */
      ELSE IF vAmtOst =  0.00 AND
              vDateOD <> ?
          THEN ASSIGN oAccRat = "12"
                      oDateAccRat = vDateOD.
      ELSE ASSIGN oAccRat = "00"
                  oDateAccRat = iDate.
   END.
   /* если задан доп. реквизит, то вычисляем только дату */
   ELSE DO:
      oAccRat = vAccRat.

      CASE vAccRat:
         WHEN "12" THEN DO:
            RUN GetFirstDateUnv(loan.contract,
                                loan.cont-code,
                                iDatePay,
                                ?,
                                "PayByOB",
                                OUTPUT oDateAccRat).
         END.
         WHEN "13" THEN oDateAccRat = loan.close-date.
         WHEN "14" OR
         WHEN "70" OR
         WHEN "85" OR
         WHEN "90" OR
         WHEN "95" THEN
               oDateAccRat = DATE(GetXattrValue("loan",
                                                         loan.contract + ","
                                                         + loan.cont-code,
                                                         "НБКИ_СостДата")).
         WHEN "21" THEN oDateAccRat = vDateDisp.

         /* дата просрочки (выноса) */
         WHEN "52" THEN DO:
            RUN GetFirstDueDate(loan.contract,
                                loan.cont-code,
                                iDate,
                                OUTPUT oDateAccRat).
         END.
         /* первый очередной вынос */
         WHEN "61" THEN RUN GetFirstDateUnv(loan.contract,
                                            loan.cont-code,
                                            iDate,
                                            iDatePrev,
                                            "GetPastDate",
                                            OUTPUT oDateAccRat).
         WHEN "00" THEN oDateAccRat = iDate.

      END CASE.
   END.
   {profile BK300}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 11. Лимит кредита/ исходная сумма кредита                                  */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredLimitAmt RETURNS DECIMAL
   (BUFFER loan FOR loan):   /* кредитный договор */

   DEF VAR vAmt     AS DEC NO-UNDO.
   DEF VAR vAmt1    AS DEC NO-UNDO.

   DEF BUFFER bloan      FOR loan.

   {profile BK301}
   IF NOT AVAIL loan THEN RETURN ?.

   /*проверяем, транш ли это*/
   IF INDEX(loan.doc-ref," ") = 0 THEN
   DO:
      
      RUN MaxLim IN THIS-PROCEDURE (loan.contract,
                                    loan.cont-code,
                                    TODAY,
                                    OUTPUT vAmt).

   END.
   ELSE DO:
      /* ищем основной договор */
      FIND FIRST bloan WHERE
                 bloan.filial-id = loan.filial-id
             AND bloan.contract  = loan.contract
             AND bloan.doc-ref   = ENTRY(1,loan.doc-ref," ")
          NO-LOCK NO-ERROR.

      IF AVAIL bloan THEN
      DO:

         RUN MaxLim IN THIS-PROCEDURE (bloan.contract,
                                       bloan.cont-code,
                                       TODAY,
                                       OUTPUT vAmt).

      END.
      /* если не нашли такого договора, то это наверно не течение,
      ** возвращаем его сумму */
      ELSE
      DO:
         
         RUN MaxLim IN THIS-PROCEDURE (loan.contract,
                                       loan.cont-code,
                                       TODAY,
                                       OUTPUT vAmt).
      END.
   END.
   /* добавляем выкумленную задолженность (для цессий) */
   FOR EACH bloan WHERE (bloan.contract  =  loan.contract 
                     AND bloan.cont-code =  ENTRY(1,loan.cont-code," "))
/* Удалено Плюс банк
                     OR (bloan.contract  =  loan.contract 
                     AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ")
                     AND NUM-ENTRIES(bloan.cont-code, " ") >  1)
Конец удаленного фрагмента Плюс банк */
       NO-LOCK:
       vAmt1 = ListGetSummByIntVar(BUFFER bloan,
                                   bloan.open-date,
                                   "ВыкупПр,ВыкупПен").
       vAmt = vAmt + vAmt1.    
   END.
/* Вставка Плюс банк */
   FOR EACH bloan WHERE /* (bloan.contract  =  loan.contract 
                     AND bloan.cont-code =  ENTRY(1,loan.cont-code," "))
                     OR */ (bloan.contract  =  loan.contract 
                     AND bloan.cont-code BEGINS ENTRY(1,loan.cont-code," ")
                     AND NUM-ENTRIES(bloan.cont-code, " ") >  1)
       NO-LOCK:
       vAmt1 = ListGetSummByIntVar(BUFFER bloan,
                                   bloan.open-date,
                                   "ВыкупПр,ВыкупПен").
       vAmt = vAmt + vAmt1.    
   END.
/* Конец вставки Плюс банк */
   {profile BK310}
   RETURN vAmt.
END FUNCTION.

PROCEDURE MaxLim PRIVATE:

   DEFINE INPUT  PARAM iContract  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iCont_code AS CHAR NO-UNDO.
   DEFINE INPUT  PARAM iDate      AS DATE NO-UNDO.
   DEFINE OUTPUT PARAM oAmt       AS DEC  NO-UNDO.

   DEFINE BUFFER loan      FOR loan.
   DEFINE BUFFER loan-cond FOR loan-cond.

   DEF VAR vAmtCond AS DEC NO-UNDO.

   FOR EACH loan-cond WHERE
            loan-cond.contract  =  iContract
        AND loan-cond.cont-code =  iCont_code
        AND loan-cond.since     <= iDate
   NO-LOCK:
 
       RUN RE_PLAN_SUMM_BY_LOAN IN h_loan (iContract,
                                           iCont_code,
                                           loan-cond.since,
                                           OUTPUT vAmtCond).

       oAmt = MAX(vAmtCond, oAmt).


   END.

   IF oAmt =  0 THEN
   DO:
      FIND FIRST loan WHERE
                 loan.contract  =  iContract
             AND loan.cont-code =  iCont_code
      NO-LOCK NO-ERROR.

      IF AVAIL loan THEN
         RUN RE_PLAN_SUMM_BY_LOAN IN h_loan (loan.contract, 
                                             loan.cont-code,
                                             loan.open-date,
                                             OUTPUT oAmt).
   END.


END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* 12. Баланс                                                                 */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredBalance RETURN DECIMAL
   (BUFFER loan FOR loan,     /* кредитный договор */
    iDate AS DATE,            /* дата выгрузки */
    iDatePay AS DATE):        /* дата последней выплаты
                               - результат работы функции BKIDateOFLstPay (7) */

   DEF VAR vAmt      AS DEC  NO-UNDO. /* общая сумма */

   {profile BK311}
   IF NOT AVAIL loan THEN RETURN ?.

   ASSIGN
      vAmt      = 0.00
      .

   IF iDatePay = ?
       THEN iDatePay = BKIDateOFLstPay(loan.contract,
                                       loan.cont-code,
                                       iDate).

   /* Общая выплаченная сумма, включая проценты и пени */
   RUN GetSummByIntVar(BUFFER loan,
                       IF iDatePay = ? THEN iDate ELSE iDatePay,
                       "ПогашОбщ",
                       OUTPUT vAmt).

   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("BKICredBalance","loan.contract = " + loan.contract
                                       + " loan.cont-code = " + loan.cont-code
                                       + " iDatePay = " + STRING(iDatePay)
                                       + " vAmt = " + STRING(vAmt)).

   &ENDIF

   {profile BK320}
   RETURN vAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 25. Текущая задолженость                                                   */
/*----------------------------------------------------------------------------*/

FUNCTION BKIAmountOutst RETURNS DECIMAL
    (BUFFER loan FOR loan,
     iDate AS DATE,                /* дата выгрузки */
     iFlNext AS LOG):              /* следующий платеж или остаток */

  DEF VAR vAmount     AS DECIMAL NO-UNDO.
  DEF VAR vDt1        AS DATE    NO-UNDO.
  DEF VAR vDt2        AS DATE    NO-UNDO.
  DEF VAR vFlMinOD    AS LOG     NO-UNDO.
  DEF VAR vAmt        AS DECIMAL NO-UNDO.

  DEF BUFFER term-obl  FOR term-obl.
  DEF BUFFER loan-cond FOR loan-cond.
  DEF BUFFER comm-rate FOR comm-rate.
  DEF BUFFER bloan     FOR loan.

  IF NOT AVAIL loan THEN RETURN ?.

  /* берем текущую задолженность */
  vAmount = ListGetSummByIntVar(BUFFER loan, 
                                iDate,
                                "ТекПроц,ТекОД").
  /* если рассчитываем следующий платеж, то учитываем %МинОД и просрочку */
  IF iFlNext THEN
  DO:
     /* для оверов проверяем %МинОД и можем учесть просрочку */
     vFlMinOD = CAN-DO(GetXclassAllChildsEx("l_all_over_agreement"),loan.class-code).
     IF NOT vFlMinOD THEN
        vFlMinOD = CAN-DO(GetXclassAllChildsEx("loan_trans_ov"),loan.class-code).

     /* если нет ни срочной, ни %МинОД, то берем просроченную задолженность */
     IF   (vAmount =  0
        OR vAmount =  ?) 
        AND vFlMinOD THEN     
           vAmount = ListGetSummByIntVar(BUFFER loan,
                                         iDate,
                                         "ЗадолжОД,ЗадолжПр").
     ELSE IF vFlMinOD THEN
     DO:  
        FIND LAST comm-rate WHERE comm-rate.commission =  "%МинОд"
                              AND comm-rate.kau        =  loan.contract + "," + loan.cont-code
                              AND comm-rate.since      <= iDate
        NO-LOCK NO-ERROR.
        IF     AVAIL comm-rate
           AND comm-rate.rate-comm >  0 THEN
        DO:
           vAmt = ListGetSummByIntVar(BUFFER loan,
                                      iDate,
                                      "ТекОД").
           IF vAmt >  0 THEN
              vAmount = ROUND(vAmt * comm-rate.rate-comm / 100,2).
        END.
     END.
  END.
  RETURN vAmount.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 14. Следующий платеж                                                       */
/*----------------------------------------------------------------------------*/
FUNCTION BKINextPayment RETURN DECIMAL
    (iContract AS CHAR,
     iContCode AS CHAR,              /* кредитный договор */
     iDate AS DATE):                 /* дата выгрузки */

   DEFINE VARIABLE vAmt   AS DEC         NO-UNDO.
   DEFINE VARIABLE vAmt1  AS DEC         NO-UNDO.
   DEFINE VARIABLE vAmt2  AS DEC         NO-UNDO.
   DEFINE VARIABLE vDate1 AS DATE        NO-UNDO.
   DEFINE VARIABLE vDate2 AS DATE        NO-UNDO.

   DEFINE BUFFER bLoan    FOR loan.
   DEFINE BUFFER term-obl FOR term-obl.

   {profile BK321}

   FIND FIRST bloan WHERE 
              bloan.contract  =  iContract 
         AND  bloan.cont-code =  iContCode
   NO-LOCK NO-ERROR.

   IF FGetSetting("НБКИ","НБКИСППроц","") =  "ДА" THEN
   DO:

      /* плановая дата и сумма оплаты основного долга */
      RUN GetFirstAmtUnv(iContract,
                         iContCode,
                         3,
                         iDate,
                         "BKINextPayment1",
                         OUTPUT vAmt1,
                         OUTPUT vDate1).

      /* плановая дата и сумма оплаты проценты */
      RUN GetFirstAmtUnv(iContract,
                         iContCode,
                         1,
                         iDate,
                         "BKINextPayment1",
                         OUTPUT vAmt2,
                         OUTPUT vDate2).

      FIND FIRST term-obl WHERE term-obl.contract  =  iContract
                            AND term-obl.cont-code =  iContCode
                            AND term-obl.idnt      =  3
                            AND term-obl.end-date  =  vDate1
      NO-LOCK NO-ERROR.
      IF AVAIL term-obl THEN 
         RUN summ-t.p (OUTPUT vAmt1,
                       bloan.Contract,
                       bloan.Cont-Code,
                       RECID(term-obl),
                       vDate1).
      ELSE
         vAmt1 = 0.
      vAmt2 = 0.
      FOR EACH term-obl WHERE term-obl.contract  =  iContract
                          AND term-obl.cont-code =  iContCode
                          AND term-obl.idnt      =  1
                          AND term-obl.dsc-beg-date =  vDate2
      NO-LOCK:
         RUN summ-t1.p (OUTPUT vAmt,
                        RECID(term-obl),
                        RECID(bloan)).
         vAmt2 = vAmt2 + vAmt.
      END.

      vAmt = 0.
      IF FGetSetting("НБКИ","TR14RAZD","") =  "ДА" THEN
      DO:
         IF vDate1 =  vDate2 THEN
            vAmt = vAmt1 + vAmt2.
         ELSE 
         DO:
            IF vDate1 < vDate2 AND vAmt1 <> 0 THEN 
               vAmt = vAmt1.
            ELSE 
               vAmt = vAmt2.
         END.
      END.
      ELSE
         vAmt = vAmt1 + vAmt2.
   END.
   ELSE
   /* Определяем сумму с учетом траншей */
   RUN GetFirstAmtUnv(iContract,
                      iContCode,
                      3,
                      iDate,
                      "BKINextPayment1",
                      OUTPUT vAmt,
                      OUTPUT vDate2).

   IF FGetSetting("НБКИ","TR14","") =  "Нет" THEN 
   DO:

      vAmt = BKIAmountOutst(BUFFER bLoan,
                            INPUT  vDate2,
                            TRUE).

   END.

   {profile BK330}
   RETURN vAmt.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Следующий платеж                                                           */
/*----------------------------------------------------------------------------*/
PROCEDURE BKINextPayment1:
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64       NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oAmt      AS DECIMAL   NO-UNDO.

   DEF VAR vAmt AS DEC NO-UNDO.
   DEF BUFFER bterm-obl  FOR term-obl.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEFINE VARIABLE vLgotPer AS INT64 NO-UNDO INIT 0.
   DEFINE VARIABLE vFirstPayDate AS DATE NO-UNDO .
   {profile BK331}

   IF FGetSetting("НБКИ","СледПлатеж","") =  "Да" THEN DO:
      FIND FIRST  bloan-cond WHERE
                  bloan-cond.contract   = iContract
            AND bloan-cond.cont-code  = iContCode
         NO-LOCK NO-ERROR.
      IF AVAILABLE bloan-cond THEN DO:
         vLgotPer = INT64(GetXAttrValueEx ( "loan-cond",
         iContract + "," + iContCode + "," + STRING(bloan-cond.since),
         "КолЛьгтПер","0")).
      END.
   END.

   IF vLgotPer > 0
   THEN DO:  /*  с учетом льготных периодов */
      /*Льготные периоды определяем  на основе графика ОД  -  их там нет */
      FIND FIRST bterm-obl WHERE
               bterm-obl.contract  = iContract
           AND bterm-obl.cont-code = iContCode
           AND bterm-obl.idnt      = 3
           NO-LOCK NO-ERROR.
          vFirstPayDate = bterm-obl.end-date .  /* Дата первого платежа по договору */
      IF   vFirstPayDate >= iDate
      THEN DO:   /* попадает в льготный период */
         ASSIGN
            oAmt = 0.00
            oDateTr = ?.
         /* суммируем все начисленные оплаты в льготном периоде - это и будет первый платеж */
         FOR EACH  bterm-obl WHERE
                   bterm-obl.contract  = iContract
               AND bterm-obl.cont-code = iContCode
               AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
               AND bterm-obl.end-date <= vFirstPayDate
               NO-LOCK :
                  oAmt    = oAmt + bterm-obl.amt-rub .
                  oDateTr = bterm-obl.end-date.
                  &IF DEFINED(IS-DEBUG) &THEN
                        RUN dbgprint.p("BKINextPayment1",
                                       "Льготный период = " +  string (vLgotPer)
                                       + " iContract = " + iContract
                                       + " iContCode = " + iContCode
                                       + " term-obl.end-date = "
                                       + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                                       + STRING(bterm-obl.amt-rub)).
                  &ENDIF
         END.
      END.
      ELSE DO: /* больше льготного периода и работает как обычно по графику */
         FIND FIRST bterm-obl WHERE
                  bterm-obl.contract  = iContract
               AND bterm-obl.cont-code = iContCode
               AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
               AND bterm-obl.end-date > iDate
            NO-LOCK NO-ERROR.
         IF AVAIL bterm-obl
            THEN ASSIGN
                     oAmt    = bterm-obl.amt-rub
                     oDateTr = bterm-obl.end-date.
         ELSE ASSIGN
               oAmt = 0.00
               oDateTr = ?.

         &IF DEFINED(IS-DEBUG) &THEN
            IF AVAIL bterm-obl THEN
               RUN dbgprint.p("BKINextPayment1",
                              "iContract = " + iContract
                              + " iContCode = " + iContCode
                              + " term-obl.end-date = "
                              + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                              + STRING(bterm-obl.amt-rub)).
            ELSE RUN dbgprint.p("BKINextPayment","iContract = " + iContract
                                                + " iContCode = " + iContCode
                                                + " - нет платежей").
         &ENDIF
      END.
   END.
   ELSE DO: /*  без учета льготных периодов */
      FIND FIRST bterm-obl WHERE
               bterm-obl.contract  = iContract
            AND bterm-obl.cont-code = iContCode
            AND bterm-obl.idnt      = iOper        /* iOper = 3 or 1 */
            AND bterm-obl.end-date > iDate
         NO-LOCK NO-ERROR.
      IF AVAIL bterm-obl
         THEN ASSIGN
                  oAmt    = bterm-obl.amt-rub
                  oDateTr = bterm-obl.end-date.
      ELSE ASSIGN
            oAmt = 0.00
            oDateTr = ?.

      &IF DEFINED(IS-DEBUG) &THEN
         IF AVAIL bterm-obl THEN
            RUN dbgprint.p("BKINextPayment1",
                           "iContract = " + iContract
                           + " iContCode = " + iContCode
                           + " term-obl.end-date = "
                           + STRING(bterm-obl.end-date) + " term-obl.amt-rub = "
                           + STRING(bterm-obl.amt-rub)).
         ELSE RUN dbgprint.p("BKINextPayment","iContract = " + iContract
                                             + " iContCode = " + iContCode
                                             + " - нет платежей").
      &ENDIF
   END.
   {profile BK340}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 15.  Частота выплат                                                        */
/*----------------------------------------------------------------------------*/
FUNCTION BKICredPayFreq RETURNS CHAR
    (iContract AS CHAR,
     iContCode AS CHAR,              /* кредитный договор */
     iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vAcctType AS CHAR NO-UNDO.
   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bLoan      FOR loan.

   {profile BK341}
   /* Ищем действующее условие на дату отчета */
   RUN RE_L_COND(iContract,
                 iContCode,
                 iDate,
                 BUFFER bloan-cond).

   IF NOT AVAIL bloan-cond THEN DO:

      FIND FIRST bLoan WHERE bLoan.contract  =  iContract
                         AND bLoan.cont-code =  iContCode NO-LOCK NO-ERROR.

      IF AVAIL bLoan THEN
         RUN RE_L_COND_FRST IN h_Loan(iContract,
                                      iContCode,
                                      bLoan.open-date,
                                      BUFFER bloan-cond).
   END.

   IF AVAIL bloan-cond THEN
      vAcctType = GetCodeByMisc("НБКИ_Частота",
                                1,
                                bloan-cond.cred-period).
   {profile BK350}
   RETURN vAcctType.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 16. Своевременность платежей                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIMOP RETURNS CHAR
    (iContract AS CHAR,               /* кредитный договор */
     iContCode AS CHAR,
     iDate AS DATE):                  /* дата выгрузки */

   DEF VAR vAcctMOP   AS CHAR    NO-UNDO. /* доп. реквизит на договоре */
   DEF VAR vDueDate   AS DATE    NO-UNDO. /* дата выноса на просрочку */
   DEF VAR vOpDate    AS DATE    NO-UNDO. /* дата операции погашения ОД в счет обеспечения */
   DEF VAR vPeriod    AS INT64   NO-UNDO.
   DEF VAR vVersion   AS DECIMAL NO-UNDO.
   DEF VAR vNBKISvMes AS LOG     NO-UNDO. /* НП НБКИ_Св_Мес */

   vVersion = DECIMAL(FGetSetting("НБКИ","Версия","")) NO-ERROR.
   vNBKISvMes = FGetSetting("НБКИ_Св_Мес","","Нет") =  "Да".

   {profile BK351}
   vAcctMOP = GetXattrValueEx("loan",
                              iContract + "," + iContCode,
                              "НБКИ_Своевременн",
                              "").

   /* если доп. реквизит не задан, то вычисляем его */
   IF vAcctMOP = "" THEN DO:
      /* проверяем, была ли оплата в счет обепечения */
      RUN GetFirstDateUnv(iContract,
                          iContCode,
                          iDate,
                          ?,
                          "PayByOB",
                          OUTPUT vOpDate).
      IF vOpDate <> ?  THEN vAcctMOP = "8".
      ELSE DO:
            /* Определяем дату неоплаченного выноса на просрочку */
          RUN GetFirstDueDate(iContract,
                              iContCode,
                              iDate,
                              OUTPUT vDueDate).
          vPeriod = iDate - vDueDate.
             /* Если задана настройка, то опрелеяем период по-другому */
          IF vNBKISvMes THEN
             IF vDueDate =  ? THEN
                vPeriod = 0.
             ELSE
                vPeriod =  iDate - MAX(vDueDate, FirstMonDate(iDate)) + 1.

          IF     NOT vNBKISvMes 
             AND vDueDate = ? THEN DO:
              /* определяем дату последней выплаты - если нет, то новый */
              IF BKIDateOFLstPay(iContract,
                                 iContCode,
                                 iDate) = ?
                 THEN vAcctMOP = "0".      /* новый */
              ELSE vAcctMOP = "1".         /* оплата без просрочек */
          END.
          ELSE IF vPeriod < 6
                  THEN vAcctMOP = IF vVersion >= 2.12 THEN "A" ELSE "7".     /* регулярные платежи */
          ELSE IF vPeriod >= 6 AND
                  vPeriod < 30
                  THEN vAcctMOP = "A".     /* просрочка от 6 до 29 дней */
          ELSE IF vPeriod >= 30 AND
                  vPeriod < 60
                  THEN vAcctMOP = "2".     /* просрочка от 30 до 59 дней */
          ELSE IF vPeriod >= 60 AND
                  vPeriod < 90
                  THEN vAcctMOP = "3".     /* просрочка от 60 до 89 дней */
          ELSE IF vPeriod >= 90 AND
                  vPeriod < 120
                  THEN vAcctMOP = "4".     /* просрочка от 90 до 119 дней */
          ELSE IF vPeriod >= 120
                  THEN vAcctMOP = "5".     /* просрочка более 120 дней */

      END.

   END.
   {profile BK360}
   RETURN vAcctMOP.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* 18. Код залога                                                             */
/*----------------------------------------------------------------------------*/
FUNCTION BKICollatCode RETURNS CHAR
    (BUFFER bterm-obl FOR term-obl,
     iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vAcctCode AS CHAR NO-UNDO.
   DEF VAR vTypeOb   AS CHAR NO-UNDO.
   DEF VAR vTR18     AS LOG  NO-UNDO.
   DEF VAR vTR18PCl  AS LOG  NO-UNDO.
   DEF VAR vKndOb    AS CHAR NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.

   IF NOT AVAIL bterm-obl THEN RETURN "".

   /* Заполнять только по залогам */
   vTR18 = FGetSetting("НБКИ","TR18","Нет") =  "Да".

   /* Обеспечение пропускать, если поручитель - не клиент по договору */
   vTR18PCl = FGetSetting("НБКИ","TR18PCl","Нет") =  "Да".

   FIND FIRST loan WHERE
              loan.contract  =  bterm-obl.contract
          AND loan.cont-code =  bterm-obl.cont-code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN "".

      FOR EACH term-obl WHERE term-obl.contract  =  bterm-obl.contract
                          and term-obl.cont-code =  bterm-obl.cont-code
                          and term-obl.idnt      =  5
          no-lock:
      /* Контроль - клиент по договору равен поручителю */
      IF     vTR18PCl
        AND (term-obl.symbol <> loan.cust-cat
        OR   term-obl.fop    <> loan.cust-id)  THEN 
         NEXT.
          vKndOb = GetXattrValueEx("term-obl",
                                   term-obl.contract + "," +
                                   term-obl.cont-code + "," +
                                   STRING(term-obl.idnt) + "," +
                                   STRING(term-obl.end-date) + "," +
                                   STRING(term-obl.nn),
                                   "ВидДогОб",
                                   "").
      /* Если надо заполнять только по залогам */
      IF    vTR18 
        AND NOT CAN-DO("КредОб,КредЦБум",vKndOb) THEN 
             next.
          vTypeOb = GetXattrValueEx("term-obl",
                                    term-obl.contract + "," +
                                    term-obl.cont-code + "," +
                                    STRING(term-obl.idnt) + "," +
                                    STRING(term-obl.end-date) + "," +
                                    STRING(term-obl.nn),
                                    "ВидОб",
                                    "").
      vAcctCode = IF vTypeOb =  "Гарантия" AND term-obl.symbol <> "Б" THEN
                     GetCodeByMisc("НБКИ_КодЗалога",
                                   1,
                                   "Поручит")
                  ELSE
                     GetCodeByMisc("НБКИ_КодЗалога",
                                    1,
                                    vTypeOb).
      LEAVE.
      end.
   RETURN vAcctCode.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Код дополнительных залогов для сегмента CL                                 */
/*----------------------------------------------------------------------------*/
{pfuncdef 
   &DEFPROC="BKICollatCodeCL"
   &DESCRIPTION="Код дополнительных залогов для сегмента CL"
   &PARAMETERS="БУФЕР term-obl,ДАТА"
   &RESULT="КОД ЗАЛОГА"
   &SAMPLE="BKICollatCodeCL(BUFFER term-obl,01/01/2015)"}
FUNCTION BKICollatCodeCL RETURNS CHAR
    (BUFFER vTerm-obl FOR term-obl,
     iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vAcctCode AS CHAR NO-UNDO.
   DEF VAR vTypeOb   AS CHAR NO-UNDO.
   DEF VAR vTR18     AS LOG  NO-UNDO.
   DEF VAR vTR18PCl  AS LOG  NO-UNDO.
   DEF VAR vKndOb    AS CHAR NO-UNDO.

   DEF BUFFER term-obl FOR term-obl.
   DEF BUFFER loan     FOR loan.

   IF NOT AVAIL vTerm-obl THEN RETURN "".

   /* Заполнять только по залогам */
   vTR18 = FGetSetting("НБКИ","TR18","Нет") =  "Да".

   /* Обеспечение пропускать, если поручитель - не клиент по договору */
   vTR18PCl = FGetSetting("НБКИ","TR18PCl","Нет") =  "Да".

   FIND FIRST loan WHERE
              loan.contract  =  vTerm-obl.contract
          AND loan.cont-code =  vTerm-obl.cont-code
   NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN RETURN "".

   /* Контроль - клиент по договору равен поручителю */
   IF     vTR18PCl
     AND (vTerm-obl.symbol <> loan.cust-cat
     OR   vTerm-obl.fop    <> loan.cust-id)  THEN 
     RETURN "".

   vKndOb = GetXattrValueEx("term-obl",
                             vTerm-obl.contract + "," +
                             vTerm-obl.cont-code + "," +
                             STRING(vTerm-obl.idnt) + "," +
                             STRING(vTerm-obl.end-date) + "," +
                             STRING(vTerm-obl.nn),
                             "ВидДогОб",
                             "").
   /* Если надо заполнять только по залогам */
   IF    vTR18 
     AND NOT CAN-DO("КредОб,КредЦБум",vKndOb) THEN 
     RETURN "".
      vTypeOb = GetXattrValueEx("term-obl",
                              vTerm-obl.contract + "," +
                              vTerm-obl.cont-code + "," +
                              STRING(vTerm-obl.idnt) + "," +
                              STRING(vTerm-obl.end-date) + "," +
                              STRING(vTerm-obl.nn),
                                "ВидОб",
                                "").
   vAcctCode = IF vTypeOb =  "Гарантия" AND vTerm-obl.symbol <> "Б" THEN
                  GetCodeByMisc("НБКИ_КодЗалога",
                                1,
                                "Поручит")
               ELSE
                  GetCodeByMisc("НБКИ_КодЗалога",
                                   1,
                                 vTypeOb).

   RETURN vAcctCode.
END FUNCTION.


/*----------------------------------------------------------------------------*/
/* 20.  Дата финального платежа                                               */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDatePayDue RETURNS DATE
   (iContract AS CHAR,
    iContCode AS CHAR,              /* кредитный договор */
    iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vDate AS DATE NO-UNDO.
   DEF BUFFER bloan FOR loan.

   /* вызываем процедуру, которая учтет транши */
   RUN GetLastDateUnv(iContract,
                      iContCode,
                      3,
                      iDate,
                      "BKIDatePayDue1",
                      OUTPUT vDate).
   /* Если не определили по графику последнюю дату,
   ** берем последнюю дату по договору */
   IF vDate = ? THEN DO:
      /* Ищем договор */
      RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).
      IF AVAIL bloan THEN vDate = bloan.end-date.
   END.

   RETURN vDate.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Дата финального платежа                                                    */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIDatePayDue1.
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.

   /* находим последний ненулевой платеж ОД */
   btobl:
   FOR EACH bterm-obl WHERE
            bterm-obl.contract  =  iContract
        AND bterm-obl.cont-code =  iContCode
        AND bterm-obl.idnt      =  iOper       /* iOper = 3 */
   NO-LOCK
   BY bterm-obl.end-date DESC:

       IF bterm-obl.amt-rub > 0.00 THEN DO:
           oDateTr = bterm-obl.end-date.
           LEAVE btobl.
       END.

   END.

END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* 21.  Дата финальной ненулевой выплаты процентов                            */
/*----------------------------------------------------------------------------*/
FUNCTION BKIDateInterPayDue RETURNS DATE
    (iContract AS CHAR,
     iContCode AS CHAR,              /* кредитный договор */
     iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vDate AS DATE NO-UNDO.
   DEF BUFFER bloan FOR loan.

   /* вызываем процедуру, которая учтет транши */
   RUN GetLastDateUnv(iContract,
                      iContCode,
                      1,
                      iDate,
                      "BKIDateInterPayDue1",
                      OUTPUT vDate).
   /* Если не определили по графику последнюю дату,
   ** берем последнюю дату по договору */
   IF vDate = ? THEN DO:
      /* Ищем договор */
      RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).
      IF AVAIL bloan THEN vDate = bloan.end-date.
   END.

   RETURN vDate.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Дата финальной ненулевой выплаты процентов                                 */
/*----------------------------------------------------------------------------*/
PROCEDURE BKIDateInterPayDue1.
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bterm-obl FOR term-obl.

   /* находим последний ненулевой платеж процентов */
   btobl:
   FOR EACH bterm-obl WHERE
            bterm-obl.contract  =  iContract
        AND bterm-obl.cont-code =  iContCode
        AND bterm-obl.idnt      =  iOper     /* iOper = 1 */
   NO-LOCK
   BY bterm-obl.end-date DESC:

       IF bterm-obl.amt-rub > 0.00 THEN DO:
           oDateTr = bterm-obl.dsc-beg-date.
           LEAVE btobl.
       END.

   END.

END.
/*----------------------------------------------------------------------------*/
/* 22.  Частота выплат процентов                                              */
/*----------------------------------------------------------------------------*/
FUNCTION BKIInterPayFreq RETURNS CHAR
    (iContract AS CHAR,
     iContCode AS CHAR,              /* кредитный договор */
     iDate AS DATE):                 /* дата выгрузки */

   DEF VAR vAcctType AS CHAR NO-UNDO.

   DEF BUFFER bloan-cond FOR loan-cond.
   DEF BUFFER bLoan      FOR loan.

   RUN RE_L_COND(iContract,
                 iContCode,
                 iDate,
                 BUFFER bloan-cond).

   IF NOT AVAIL bloan-cond THEN DO:

      FIND FIRST bLoan WHERE bLoan.contract  =  iContract
                         AND bLoan.cont-code =  iContCode NO-LOCK NO-ERROR.

      IF AVAIL bLoan THEN
         RUN RE_L_COND_FRST IN h_Loan(iContract,
                                      iContCode,
                                      bLoan.open-date,
                                      BUFFER bloan-cond).
   END.

   IF NOT AVAIL bloan-cond THEN RETURN ?.

   vAcctType = GetCodeByMisc("НБКИ_Частота",
                             1,
                             bloan-cond.int-period).
   RETURN vAcctType.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/* Поиск первой даты с учетом течений                                         */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetFirstDateUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iDatePrev AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vCurDate AS DATE NO-UNDO.

   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iDate,
                        iDatePrev,
                        OUTPUT oDateTr).

   /* Ищем договор */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* Если это охватывающий договор-течение, то ищем все его транши */
   IF AVAIL bloan AND
            bloan.cont-type = "Течение" THEN
   DO:
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
          NO-LOCK:
          RUN VALUE(iNameProc)(bloan.contract,
                               bloan.cont-code,
                               iDate,
                               iDatePrev,
                               OUTPUT vCurDate).
          IF vCurDate <> ? AND
             vCurDate < oDateTr
             THEN oDateTr = vCurDate.
      END.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Поиск последней даты с учетом течений                                      */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetLastDateUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vCurDate AS DATE NO-UNDO.

   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iOper,
                        iDate,
                        OUTPUT oDateTr).

   /* Ищем договор */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* Если это охватывающий договор-течение, то ищем все его транши */
   IF AVAIL bloan AND
            bloan.cont-type = "Течение" THEN
   DO:
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
          NO-LOCK:
          RUN VALUE(iNameProc)(bloan.contract,
                               bloan.cont-code,
                               iOper,
                               iDate,
                               OUTPUT vCurDate).
          IF     oDateTr =  ?
             OR (    vCurDate <> ? 
                 AND vCurDate > oDateTr)
             THEN oDateTr = vCurDate.
      END.
   END.
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Возвращает сумму по параметру или операции с учетом течений                */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummUnv:
   DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iOper     AS INT64   NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
   DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
   DEF OUTPUT PARAM oAmtRes   AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DECIMAL   NO-UNDO.
   DEF OUTPUT PARAM oAmtCr    AS DECIMAL   NO-UNDO.

   DEF BUFFER bloan FOR loan.
   DEF VAR vAmtCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtDbCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtCrCur AS DECIMAL NO-UNDO.

   {profile BK501}
   RUN VALUE(iNameProc)(iContract,
                        iContCode,
                        iOper,
                        iDate,
                        OUTPUT oAmtRes,
                        OUTPUT oAmtDb,
                        OUTPUT oAmtCr).
   {profile BK502}
   /* Ищем договор */
   FIND FIRST bLoan WHERE
              bLoan.contract  =  iContract
          AND bLoan.cont-code =  iContCode
              NO-LOCK NO-ERROR.
   {profile BK503}
   ASSIGN
      vAmtCur   = 0.00
      vAmtDbCur = 0.00
      vAmtCrCur = 0.00
   .

/*----------- Если это охватывающий договор-течение, то ищем все его транши --*/
   IF AVAIL bloan                  AND
      bloan.cont-type =  "Течение" THEN
      FOR EACH bloan WHERE
               bloan.contract  = iContract
           AND bloan.cont-code BEGINS iContCode + " "
           AND bloan.cont-code <> iContCode
               NO-LOCK:
         {profile BK504}
         RUN VALUE(iNameProc)(bloan.contract,
                              bloan.cont-code,
                              iOper,
                              iDate,
                              OUTPUT vAmtCur,
                              OUTPUT vAmtDbCur,
                              OUTPUT vAmtCrCur).
         ASSIGN
            oAmtRes = oAmtRes + vAmtCur
            oAmtDb  = oAmtDb  + vAmtDbCur
            oAmtCr  = oAmtCr  + vAmtCrCur
         .
         {profile BK505}
      END.

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p("GetSummUnv","iContract = " + iContract
                                    + " iContCode = " + iContCode
                                    + " iOper = " + string(iOper)
                                    + " iDate = " +
                               (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                    + " oAmtRes = " + STRING(oAmtRes)).

      &ENDIF
   {profile BK510}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/* Поиск первой суммы с учетом течений                                        */
/*----------------------------------------------------------------------------*/
PROCEDURE  GetFirstAmtUnv.
    DEF INPUT  PARAM iContract AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iContCode AS CHARACTER NO-UNDO.
    DEF INPUT  PARAM iOper     AS INT64     NO-UNDO. /* 1 - %%, 3 - ОД */
    DEF INPUT  PARAM iDate     AS DATE      NO-UNDO.
    DEF INPUT  PARAM iNameProc AS CHARACTER NO-UNDO.
    DEF OUTPUT PARAM oAmt      AS DECIMAL   NO-UNDO.
    DEF OUTPUT PARAM oDateTr   AS DATE      NO-UNDO.

    DEF BUFFER bloan FOR loan.

    DEF VAR vCurDate AS DATE    NO-UNDO.
    DEF VAR vCurAmt  AS DECIMAL NO-UNDO.

   /* Ищем договор */
   RUN RE_B_LOAN (iContract,iContCode,BUFFER bloan).

   /* Если это охватывающий договор-течение, то ищем все его транши */
    IF AVAIL bloan AND
             bloan.cont-type = "Течение"
        AND CAN-DO(FGetSetting("НБКИ", IF (iOper = 1) THEN "ЛинииПрТранш" ELSE "ЛинииОДТранш", ""), bloan.class-code)   /* ведется на траншах */
    THEN DO:
        FOR EACH bloan
            WHERE  bloan.contract   = iContract
              AND  bloan.cont-code  BEGINS iContCode + " "
              AND  bloan.cont-code  <> iContCode
              AND  bloan.open-date  <= iDate  /* Транши, еще не выданные на дату расчета, не учитываем */
              AND (bloan.close-date =  ?
                OR bloan.close-date >  iDate)
            NO-LOCK:

            RUN VALUE(iNameProc)(bloan.contract,
                                 bloan.cont-code,
                                 iOper,
                                 iDate,
                                 OUTPUT vCurDate,
                                 OUTPUT vCurAmt).
            /* Платежи на одну дату суммируем */
            IF vCurDate <> ? AND
               vCurDate = oDateTr
            THEN oAmt = oAmt + vCurAmt.

            IF (vCurDate <> ?) AND (vCurAmt NE 0) AND
               (oDateTr = ? OR vCurDate < oDateTr)
            THEN ASSIGN
                    oDateTr = vCurDate
                    oAmt    = vCurAmt.
        END.
    END.
    ELSE    /* договор без траншей */
        RUN VALUE(iNameProc)(iContract,
                             iContCode,
                             iOper,
                             iDate,
                             OUTPUT oDateTr,
                             OUTPUT oAmt).

    IF oAmt =  ? THEN oAmt = 0.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* Возвращение оборота по параметру                                          */
/*----------------------------------------------------------------------------*/
PROCEDURE GetSummPar.
   DEF INPUT PARAM iContract  AS CHAR NO-UNDO.
   DEF INPUT PARAM iContCode  AS CHAR NO-UNDO.
   DEF INPUT PARAM iOper      AS INT64  NO-UNDO.   /* Вид операции */
   DEF INPUT PARAM iDate      AS DATE NO-UNDO.   /* Дата отчета */
   DEF OUTPUT PARAM oAmtPrm   AS DEC  NO-UNDO.
   DEF OUTPUT PARAM oAmtDb    AS DEC  NO-UNDO.   /* Затычки */
   DEF OUTPUT PARAM oAmtCr    AS DEC  NO-UNDO.   /* Затычки */

   DEF BUFFER bloan FOR loan.
   DEF VAR vAmtCur   AS DECIMAL NO-UNDO.
   DEF VAR vAmt      AS DECIMAL NO-UNDO.
   DEF VAR vAmtDbCur AS DECIMAL NO-UNDO.
   DEF VAR vAmtCrCur AS DECIMAL NO-UNDO.

   RUN STNDRT_PARAM(iContract,
                    iContCode,
                    iOper,
                    iDate,
                    OUTPUT vAmtCur,
                    OUTPUT vAmtDbCur,
                    OUTPUT vAmtCrCur).

   FIND FIRST bLoan WHERE bloan.contract =  iContract AND bloan.cont-code =  iContCode NO-LOCK NO-ERROR.

   IF AVAIL(bloan) THEN
   RUN inter_current  (buffer bloan, iOper, OUTPUT vAmt).

   ASSIGN
      oAmtPrm =  vAmt + vAmtCur.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p("GetSummPar","iContract = " + iContract
                                 + " iContCode = " + iContCode
                                 + " iOper = " + string(iOper)
                                 + " vAmt = " + string(vAmt)
                                 + " iDate = " +
                            (IF iDate <> ? THEN STRING(iDate) ELSE "    ")
                                 + " oAmtPrm = " + STRING(oAmtPrm)).
   &ENDIF

END.

{pfuncdef 
   &DEFPROC="GetBuyerInfo"
   &DESCRIPTION="получение данных о преобретателе прав по ссуде"
   &PARAMETERS="НАЗНАЧЕНИЕ ДОГОВОРА,ИДЕНТИФИКАТОР ДОГОВОРА,ДАТА"
   &RESULT="данные о преобретателе прав по ссуде"
   &SAMPLE="GetBuyerInfo('Кредит','123',01/01/2015,OUTPUT oBuyerName,OUTPUT oBuyerID, ~
OUTPUT oBuyerINN,OUTPUT oBuyerSNILS)"}

PROCEDURE GetBuyerInfo:
   DEF INPUT  PARAM iContract   AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode   AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate       AS DATE NO-UNDO.   /* Дата отчета */ 
   DEF OUTPUT PARAM oBuyerName  AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerID    AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerINN   AS CHAR NO-UNDO.
   DEF OUTPUT PARAM oBuyerSNILS AS CHAR NO-UNDO.
   
   DEF VAR vPBirth   AS CHAR  NO-UNDO.
   DEF VAR vDtIss    AS DATE  NO-UNDO.
   DEF VAR vCode     AS CHAR  NO-UNDO.
   DEF VAR vCharAdd  AS CHAR  NO-UNDO.
   DEF VAR vCorpName AS CHAR  NO-UNDO.
   DEF VAR vCust     AS CHAR  NO-UNDO.
   DEF VAR vCustCat  AS CHAR  NO-UNDO.
   DEF VAR vCustId   AS INT64 NO-UNDO. 
   DEF VAR vFlCes    AS LOG   NO-UNDO. 
   
   DEF BUFFER signs      FOR signs.
   DEF BUFFER loan       FOR loan.
   DEF BUFFER bloan      FOR loan.
   DEF BUFFER person     FOR person.
   DEF BUFFER cust-ident FOR cust-ident.
   DEF BUFFER cust-corp  FOR cust-corp.
   DEF BUFFER banks-code FOR banks-code.
   
   main:
   DO:       
       FIND FIRST loan WHERE loan.contract  =  iContract
                         AND loan.cont-code =  iContCode
       NO-LOCK NO-ERROR.
       IF NOT AVAIL loan THEN
          LEAVE MAIN.
       /* Цессия? */
       vFlCes = CAN-DO(GetXclassAllChildsEx("loan_ces"),loan.class-code).
       IF vFlCes THEN
       DO:
          ASSIGN
             oBuyerName  = FGetSetting("БанкПН","","") 
             oBuyerID    = FGetSetting("БанкМФО","","").
          FIND FIRST banks-code WHERE banks-code.bank-code      =  oBuyerID
                                  AND banks-code.bank-code-type =  'МФО-9' 
          NO-LOCK NO-ERROR.
          IF AVAIL banks-code THEN
          DO:
             FIND FIRST cust-ident WHERE cust-ident.cust-cat  =  "Б" 
                                     AND cust-ident.cust-id   =  banks-code.bank-id        
                                     AND cust-ident.cust-code-type =  "ИНН"
             NO-LOCK NO-ERROR.
             IF AVAIL cust-ident THEN
                oBuyerINN = cust-ident.cust-code.
          END.
       END.
       ELSE
       DO:
           vCust = GetXAttrValueEx("loan",iContract + "," + iContCode,"Контрагент","").
           IF NOT {assigned vCust} THEN
           DO:
              /* определяем, проданы ли обязательства */
              FIND FIRST signs WHERE signs.file-name  =  "loan"
                                 AND signs.surrogate  BEGINS "КредРО,"
                                 AND signs.code       =  "РДог"
                                 AND signs.code-value =  iContCode
              NO-LOCK NO-ERROR.
              IF AVAIL signs THEN
              DO:
                 FIND FIRST bloan WHERE bloan.contract  =  ENTRY(1,signs.surrogate)
                                    AND bloan.cont-code =  ENTRY(2,signs.surrogate)
                 NO-LOCK NO-ERROR.
                 IF AVAIL bloan THEN
                 DO:
                    vCode = GetXAttrValueEx("loan",
                                            bloan.contract + "," + bloan.cont-code,
                                            "РКонтрДог",
                                            "").
                    IF NOT {assigned vCode} THEN
                       LEAVE main.
                    FIND FIRST loan WHERE loan.contract  =  "aijk"
                                      AND loan.cont-code =  vCode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL loan THEN
                       LEAVE main.
                    ASSIGN
                       vCustCat = loan.cust-cat
                       vCustId  = loan.cust-id
                    NO-ERROR.
                 END.
              END.
           END.
           ELSE
              ASSIGN
                 vCustCat = ENTRY(1,vCust)
                 vCustId  = INT64(ENTRY(2,vCust))
              NO-ERROR.
          
           IF     {assigned vCustCat}
              AND vCustId <> 0 THEN
           DO:
              IF vCustCat =  "Ч" THEN
              DO:
                 FIND FIRST person WHERE person.person-id =  vCustId NO-LOCK NO-ERROR.
                 IF NOT AVAIL person THEN
                    LEAVE main.
                 ASSIGN
                    vDtIss      = DATE(GetXAttrValueEx("person",
                                                       string(person.person-id),
                                                       "Document4Date_vid",
                                                       ?))
                    vPBirth     = GetXAttrValue("person",
                                                 string(person.person-id),
                                                "BirthPlace")
                    oBuyerName  = subst("&1 &2 &3 &4",
                                        person.name-last,
                                        person.first-names,
                                        person.BirthDay,
                                        vPBirth)
                    oBuyerID    = subst("&1 &2 &3",person.Document,vDtIss,person.Issue)
                    oBuyerINN   = person.inn
                 no-error.
                 IF ERROR-STATUS:ERROR THEN
                    LEAVE main.
                 FIND LAST cust-ident WHERE  cust-ident.cust-cat   =  'Ч' 
                                        AND  cust-ident.cust-id    =  person.person-id
                                        AND  cust-ident.cust-code-type =  person.document-id 
                                        AND  cust-ident.class-code =  'p-cust-ident'
                                        AND (cust-ident.close-date =  ? 
                                         OR  cust-ident.close-date >  end-date)
                 NO-LOCK NO-ERROR.
                 IF AVAIL cust-ident THEN
                 DO:
                    ASSIGN
                       vCode       = GetXAttrValueEx("cust-ident",
                                                 cust-ident.cust-code-type + "," + 
                                                 STRING(cust-ident.cust-code) + "," +
                                                 STRING(cust-ident.cust-type-num),
                                                 "Подразд",
                                                 "")
                       oBuyerID    = subst("&1 &2",oBuyerID,vCode).
                 END.
                 FIND LAST cust-ident WHERE  cust-ident.cust-cat   =  'Ч' 
                                        AND  cust-ident.cust-id    =  person.person-id
                                        AND  cust-ident.cust-code-type =  "СНИЛС" 
                                        AND  cust-ident.class-code =  'p-cust-ident'
                                        AND (cust-ident.close-date =  ? 
                                         OR  cust-ident.close-date >  end-date)
                 NO-LOCK NO-ERROR.
                 IF AVAIL cust-ident THEN
                    oBuyerSNILS = cust-ident.cust-code.
              END. 
              ELSE
              DO:
                 FIND FIRST cust-corp WHERE cust-corp.cust-id =  vCustId NO-LOCK NO-ERROR.
                 IF NOT AVAIL cust-corp THEN
                    NEXT.
                 ASSIGN
                    vCharAdd  = FGetSetting("НБКИ","НБКИЮрНаимС","")
                    vCorpName = vCharAdd + cust-corp.Name-Corp + vCharAdd.
                 IF FGetSetting("НБКИ","НБКИОргФ","") =  "ДА" THEN
                    vCorpName = GetCodeName("КодПредп",
                                            GetCodeVal("КодПредп",cust-corp.cust-stat)) +
                                            " " + vCorpName. 
                 ASSIGN 
                    oBuyerName = subst("&1, &2", vCorpName, cust-corp.name-short)
                    oBuyerID   = GetXAttrValueEx("cust-corp",string(cust-corp.cust-id),"ОГРН","")
                    oBuyerINN  = cust-corp.inn.
              END. 
           END.
       END.
   END.
   
END PROCEDURE.

{pfuncdef 
   &DEFPROC="BKIInitLoanType"
   &DESCRIPTION="Загрузка классификатора NBKILoanType для работы BKIGetLoanType"
   &PARAMETERS="Загрузить/Очистить"
   &RESULT=" "
   &SAMPLE="BKIInitLoanType(YES)"}

PROCEDURE BKIInitLoanType:
   DEFINE INPUT PARAMETER iInit AS LOGICAL NO-UNDO.
    
   DEFINE BUFFER bcode  FOR code. 
   IF iInit THEN 
      FOR EACH bcode WHERE bcode.class  = 'NBKILoanType'
                       AND bcode.parent = 'NBKILoanType' NO-LOCK:
         CREATE ttLoanType.
         ASSIGN
            ttLoanType.fProd = bcode.misc[1]
            ttLoanType.fPos  = bcode.misc[2]
            ttLoanType.fProv = bcode.misc[3]
            ttLoanType.fTerm = DECIMAL(bcode.misc[4])
            ttLoanType.fSumm = DECIMAL(bcode.misc[5])
            ttLoanType.fVal  = INT64(bcode.misc[6])
            ttLoanType.fCode = bcode.val
         .
      END.
   ELSE DO:
     {empty ttLoanType}
   END.
END PROCEDURE.
   
{pfuncdef 
   &DEFPROC="BKIGetLoanType"
   &DESCRIPTION="Определение типа договора для БКИ"
   &PARAMETERS="BUFFER boan,ДАТА"
   &RESULT="тип договора из кл-ра NBKILoanType"
   &SAMPLE="BKIGetLoanType(BUFFER vLoan, end-date)"}
FUNCTION BKIGetLoanType RETURNS INT64 
   (BUFFER vLoan     FOR loan,
    INPUT iDate     AS DATE):

   DEFINE VARIABLE vProd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSumm AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vTerm AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE vPos  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSVal AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER NO-UNDO INIT ?.
   DEFINE VARIABLE vND   AS INT64     NO-UNDO.
   DEFINE VARIABLE vNM   AS INT64     NO-UNDO.
   DEFINE VARIABLE vNY   AS INT64     NO-UNDO.
   DEFINE VARIABLE vI    AS INT64     NO-UNDO.
   DEFINE VARIABLE vProv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vVal  AS DECIMAL   NO-UNDO.
          
   DEFINE BUFFER ttIndicate FOR ttIndicate.
   DEFINE BUFFER term-obl FOR term-obl.
   DEFINE BUFFER bcode FOR code. 

   IF AVAILABLE vLoan THEN 
   LOOP:
   DO: 
      ASSIGN 
         vProd = IF vLoan.class-code =  "precrd_loan" 
                 THEN GetXattrValueEx("loan",
                                       vLoan.contract + "," + 
                                       vLoan.cont-code,
                                       "ТипДоговора",
                                       "")
                 ELSE vLoan.cont-type
         vPos  = LnInBagOnDate (vLoan.contract, vLoan.cont-code, iDate)
         vPos = IF {assigned vPos} THEN "Д" ELSE "Н"
      . 
      RUN DMY_In_Per IN h_date (vLoan.open-date,
                                vLoan.end-date,
                                OUTPUT vND, 
                                OUTPUT vNM, 
                                OUTPUT vNY). 
  /* формула приблизительная чтоб понять больше целого количества лет или нет */ 
      vTerm = vNY + vNM / 12 + vND / 360.
      
      FIND FIRST term-obl WHERE term-obl.contract  =  vLoan.contract
                            AND term-obl.cont-code =  vLoan.cont-code
                            AND term-obl.idnt      =  2
      NO-LOCK NO-ERROR.                            
      IF AVAILABLE term-obl THEN 
         vSumm = IF vLoan.currency =  "" 
                 THEN term-obl.amt-rub
                 ELSE CurToBase ("УЧЕТНЫЙ", 
                                 vLoan.currency, 
                                 iDate, 
                                 term-obl.amt-rub).
      ELSE vSumm = 0.
      
      ASSIGN 
         vVal = 0
         vProv = ""
      .
      FOR EACH term-obl WHERE term-obl.contract  =  vLoan.contract
                          AND term-obl.cont-code =  vLoan.cont-code
                          AND term-obl.idnt      =  5
      NO-LOCK:
         vSVal = GetXAttrValueEx("term-obl",
                                 GetSurrogateBuffer("term-obl",
                                                    BUFFER term-obl:HANDLE),
                                 "Авто",
                                 "").
         IF vSVal MATCHES "*пробег*" THEN 
         DO vI = 1 TO NUM-ENTRIES(vSVal,"|"):
            IF ENTRY(1, ENTRY(vI,vSVal,"|"), "=") =  "пробег" THEN 
              vVal = INT64(ENTRY(2, ENTRY(vI,vSVal,"|"), "=")) NO-ERROR. 
         END.                       
         vProv = vProv + "," + GetXAttrValueEx("term-obl",
                                               GetSurrogateBuffer("term-obl",
                                               BUFFER term-obl:HANDLE),
                                               "ВидОб",
                                               "").
   
      END.
      vProv = TRIM(vProv,",").
      
      CASE vProd:
   
         WHEN "Авто" THEN
         DO:
            /* SORT-ACCESS code необходим для корректного поиска */
            FOR FIRST ttLoanType WHERE ttLoanType.fProd =  vProd  
                                   AND ttLoanType.fVal  >= vVal 
            NO-LOCK:
               vCode = ttLoanType.fCode.               
            END.
         END.
         OTHERWISE 
            LOOP1:
            DO:
               IF vProv <> "" 
               THEN 
                  DO vI = 1 TO NUM-ENTRIES(vProv):
                     FOR FIRST ttLoanType WHERE CAN-DO(ttLoanType.fProd,vProd)
                                            AND CAN-DO(ttLoanType.fPos ,vPos) 
                                            AND CAN-DO(ttLoanType.fProv,
                                                       ENTRY(vI,vProv))
                                            AND ttLoanType.fTerm >= vTerm
                                            AND ttLoanType.fSumm >= vSumm   
                     NO-LOCK:
                        vCode = ttLoanType.fCode.
                        LEAVE LOOP1.               
                     END.
                  END.
               ELSE
                  FOR FIRST ttLoanType WHERE CAN-DO(ttLoanType.fProd,vProd)
                                         AND CAN-DO(ttLoanType.fPos ,vPos) 
                                         AND ttLoanType.fProv =  vProv
                                         AND ttLoanType.fTerm >= vTerm
                                         AND ttLoanType.fSumm >= vSumm   
                  NO-LOCK:
                     vCode = ttLoanType.fCode.
                     LEAVE LOOP1.               
                  END.
            END.
      END CASE. 
   END.
   IF vCode =  ? THEN vCode = "999".
   IF NUM-ENTRIES(vCode,"_") >  1 THEN
      vCode = ENTRY(1,vCode,"_").
   RETURN INT64(vCode).
END FUNCTION.


{pfuncdef 
   &DEFPROC="BKIGetShipWayApp"
   &DESCRIPTION="Определение cпособа оформления заявления для БКИ"
   &PARAMETERS="BUFFER boan"
   &RESULT="код cпособа оформления заявления"
   &SAMPLE="BKIGetShipWayApp(BUFFER vLoan)"}
FUNCTION BKIGetShipWayApp RETURNS CHARACTER 
   (BUFFER vLoan FOR loan):

   DEFINE VARIABLE vSpPol AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDopOf AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode  AS CHARACTER NO-UNDO.
          
   DEFINE BUFFER cust-role FOR cust-role.

   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   LOOP:
   DO: 
      ASSIGN 
         vSpPol = GetXattrValueEx("loan",
                                  vLoan.contract + "," + 
                                  vLoan.cont-code,
                                  "СпособПолуч",
                                  "")
         vDopOf = GetXattrValueEx("loan",
                                  vLoan.contract + "," + 
                                  vLoan.cont-code,
                                  "ДопОфис",
                                  "")
      .

      IF {assigned vSpPol} AND
         {assigned vDopOf} THEN
      FOR FIRST cust-role NO-LOCK
          WHERE cust-role.Class-Code =  "ImaginRO"
            AND cust-role.RegNum     =  vDopOf : 
          vCode = GetXAttrValueEx("cust-role",
                                   GetSurrogateBuffer("cust-role", 
                                                      BUFFER cust-role:HANDLE),
                                  "НБКИ_СпособПред",
                                  "").
      END.
   END.
   IF NOT {assigned vCode } THEN vCode = "1".
   RETURN vCode.
END FUNCTION.

{pfuncdef 
   &DEFPROC="BKIGetApprovalFlag"
   &DESCRIPTION="Определение наличия решения об одобрении заявки для БКИ"
   &PARAMETERS="BUFFER boan,ДАТА,ВРЕМЯ"
   &RESULT="флаг наличия решения об одобрении заявки"
   &SAMPLE="BKIGetApprovalFlag(BUFFER vLoan, end-date, TIME)"}
FUNCTION BKIGetApprovalFlag RETURNS CHARACTER 
   (BUFFER vLoan     FOR loan,
    INPUT iDatePrev  AS DATE,
    INPUT iTimePrev  AS INT64):

   DEFINE VARIABLE vCode    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFinStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult  AS CHARACTER NO-UNDO.

   DEFINE BUFFER term-obl FOR term-obl.
       
   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   DO: 
       LOOP:
       /* SORT-ACCESS term-obl */
       FOR EACH term-obl NO-LOCK 
          WHERE term-obl.contract  =  "протокол"
            AND ENTRY(1,term-obl.cont-code,"-") =  DelFilFromLoan(vLoan.cont-code) 
            AND term-obl.idnt      =  1000000 
          BY term-obl.end-date DESCENDING 
          BY term-obl.fop      DESCENDING :
 
          IF iDatePrev         =  ?
             OR
             term-obl.end-date >  iDatePrev
             OR
             term-obl.end-date =  iDatePrev AND
             term-obl.fop      >  iTimePrev THEN
          ASSIGN  
             vBegStat = term-obl.Cont-type
             vFinStat = term-obl.Symbol
             vCode    = vLoan.cont-cli + "," + 
                        vBegStat       + "_" +
                        vFinStat 
             vResult  = IF GetCode("КРЕД_СТАТ_НБКИ", vCode) =  "одобрена"
                        THEN "Y"
                        ELSE ""
          .

          IF vResult =  "Y" THEN
          LEAVE LOOP.
       END.

       IF NOT {assigned vResult} THEN
       DO:
          FIND LAST term-obl OF vLoan 
              WHERE term-obl.idnt =  1000000 NO-LOCK NO-ERROR.

          IF AVAIL term-obl
             AND ( iDatePrev         =  ?
                   OR
                   term-obl.end-date >  iDatePrev 
                   OR
                   term-obl.end-date =  iDatePrev AND
                   term-obl.fop      >  iTimePrev
                 ) THEN
          ASSIGN  
             vBegStat = term-obl.Cont-type
             vFinStat = term-obl.Symbol
             vCode    = vLoan.cont-cli + "," + 
                        vBegStat       + "_" +
                        vFinStat 
             vResult  = IF GetCode("КРЕД_СТАТ_НБКИ", vCode) =  "одобрена"
                        THEN "Y"
                        ELSE ""
          .
       END.
   END.
   RETURN vResult.
END FUNCTION.


{pfuncdef 
   &DEFPROC="BKIGetRejAmount"
   &DESCRIPTION="Определение параметров отклоненной заявки для БКИ"
   &PARAMETERS="BUFFER boan,ДАТА,ВРЕМЯ"
   &RESULT="YES/NO"
   &SAMPLE="BKIGetRejAmount(BUFFER vLoan, end-date, TIME, 
                            OUTPUT vAmt, OUTPUT vAmCurr, OUTPUT vRejDate, OUTPUT vRejReason)"}
FUNCTION BKIGetRejAmount RETURNS LOGICAL 
   (BUFFER vLoan     FOR loan,
     INPUT iDatePrev  AS DATE,
     INPUT iTimePrev  AS INT64,
    OUTPUT oAmt       AS DECIMAL,
    OUTPUT oAmCurr    AS CHARACTER,
    OUTPUT oRejDate   AS DATE,
    OUTPUT oRejReason AS CHARACTER):

   DEFINE VARIABLE vContCli AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBegStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFinStat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult  AS LOGICAL   NO-UNDO.

   DEFINE BUFFER  term-obl FOR term-obl.
   DEFINE BUFFER xterm-obl FOR term-obl.
   DEFINE BUFFER      code FOR code.
          
   IF AVAILABLE vLoan AND 
      vLoan.class-code =  "precrd_loan" THEN 
   DO: 
       loopcode:
       FOR EACH code NO-LOCK
          WHERE code.class =  "КРЕД_СТАТ_НБКИ" 
            AND code.val   =  "отказ" :

          vContCli = ENTRY(1,code.code).
          IF NUM-ENTRIES(code.code) >= 2 THEN
          vBegStat = ENTRY(1,ENTRY(2,code.code),"_").
          IF NUM-ENTRIES(code.code) >= 2 AND
             NUM-ENTRIES(ENTRY(2,code.code),"_") >= 2 THEN
          vFinStat = ENTRY(2,ENTRY(2,code.code),"_").

          IF vLoan.cont-cli =  vContCli THEN
          DO:
             FIND LAST term-obl OF vLoan 
                 WHERE term-obl.idnt      =  1000000 
                   AND term-obl.Cont-type =  vBegStat
                   AND term-obl.Symbol    =  vFinStat NO-LOCK NO-ERROR.
             IF AVAIL term-obl
                AND ( iDatePrev         =  ?
                      OR
                      term-obl.end-date >  iDatePrev 
                      OR
                      term-obl.end-date =  iDatePrev AND
                      term-obl.fop      >  iTimePrev
                    ) THEN
             DO:
                 FIND FIRST xterm-obl
                      WHERE xterm-obl.contract  =  vLoan.contract
                        AND xterm-obl.cont-code =  vLoan.cont-code
                        AND xterm-obl.end-date  =  vLoan.open-date 
                        AND xterm-obl.idnt      =  2 
                        AND xterm-obl.nn        =  1 NO-LOCK NO-ERROR.
                 
                 ASSIGN 
                    vResult    = YES
                    oAmt       = IF AVAIL xterm-obl THEN xterm-obl.amt-rub
                                                    ELSE 0
                    oAmCurr    = IF vLoan.currency =  "" THEN "810" 
                                                         ELSE vLoan.currency
                    oRejDate   = term-obl.end-date
                    oRejReason = GetXattrValueEx("loan",
                                                 vLoan.contract + "," + 
                                                 vLoan.cont-code,
                                                 "НБКИ_ПричОтк",
                                                 "5")
                 .     
                 LEAVE loopcode.
             END.
          END.
       END.
   END.
   RETURN vResult.
END FUNCTION.


/* Определение признака дефолта */
{pfuncdef 
   &DEFPROC="IsDefaultPayment"
   &DESCRIPTION="Определение признака дефолта "
   &PARAMETERS="BUFFER boan,ДАТА,колич.дней,к-во пропусков"
   &RESULT="YES/NO"
   &SAMPLE="IsDefaultPayment(BUFFER bLoan, end-date, 120, 2)"}
FUNCTION IsDefaultPayment RETURNS LOGICAL
   (BUFFER vLoan     FOR loan,
    INPUT iDate     AS DATE,
    INPUT iTermDays AS INT64,
    INPUT iCnt      AS INT64):
       
   DEFINE BUFFER term-obl FOR term-obl.
   
   DEFINE VARIABLE vRes   AS LOGICAL NO-UNDO INIT ?.
   DEFINE VARIABLE vSumma AS DECIMAL NO-UNDO.
   DEFINE VARIABLE vCnt   AS INT64   NO-UNDO.
   
   IF AVAILABLE vLoan THEN 
   LOOP:
   DO: 
      vRes = YES.
      vCnt = 0.
      FOR EACH term-obl WHERE
               term-obl.contract  =  vLoan.contract
           AND term-obl.cont-code =  vLoan.cont-code
           AND term-obl.idnt      =   3
           AND term-obl.end-date  >= iDate - iTermDays 
           AND term-obl.end-date  <= iDate
      NO-LOCK:
         RUN summ-t.p (OUTPUT vSumma,
                       vLoan.contract,
                       vLoan.cont-code,
                       RECID(term-obl),
                       iDate).
         IF vSumma >  0 THEN vCnt = vCnt + 1.
         IF vCnt >  iCnt THEN LEAVE LOOP.
      END.
      vCnt = 0.
      FOR EACH term-obl WHERE
               term-obl.contract  =  vLoan.contract
           AND term-obl.cont-code =  vLoan.cont-code
           AND term-obl.idnt      =  1
           AND term-obl.end-date  >= iDate - iTermDays
           AND term-obl.end-date  <= iDate
      NO-LOCK:
         RUN summ-t1.p (OUTPUT vSumma,RECID(term-obl),RECID(vLoan)).
         IF vSumma >  0 THEN vCnt = vCnt + 1.
         IF vCnt >  iCnt THEN LEAVE LOOP.
      END.
      vRes = NO.
   END.
   RETURN vRes.
END FUNCTION.

/* Возврат реквизита по коду из классификатора НБКИ_Реквизит 
   или описание своих функций */
{pfuncdef 
   &DEFPROC="BKIGetXAttrByCode"
   &DESCRIPTION="Возврат реквизита по коду из классификатора НБКИ_Реквизит~
или описание своих функций"
   &PARAMETERS="BUFFER boan,код кл-ра"
   &RESULT="Значение реквизита"
   &SAMPLE="BKIGetXAttrByCode(BUFFER vLoan,"1")"}
FUNCTION BKIGetXAttrByCode RETURNS CHARACTER  
   (BUFFER vLoan     FOR loan,
    INPUT  iCode     AS  CHARACTER):
   
   DEFINE VARIABLE vRes  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCode AS CHARACTER NO-UNDO.
   
   CASE iCode:
      WHEN "" THEN
      DO: /* Заглушка для последующей обработки */
         vRes = "".
      END.         
      OTHERWISE 
      DO: /* Значение реквизита Договора, по коду классификатора НБКИ_Реквизит
             выбирается имя реквизита */
         vCode = GetCode("НБКИ_Реквизит",iCode).
         vRes = GetXAttrValueEx("loan",
                                vLoan.contract + "," + vLoan.cont-code,
                                vCode,
                                GetValue(BUFFER vLoan:HANDLE,vCode)
                               ).
      END.
   END CASE.
       
   RETURN vRes.
END FUNCTION.
 
{pfuncdef 
   &DEFPROC="GetLoanEPS"
   &DESCRIPTION="получение ЭПС по договору"
   &PARAMETERS="НАЗНАЧЕНИЕ ДОГОВОРА,ИДЕНТИФИКАТОР ДОГОВОРА,ДАТА"
   &RESULT="ЭПС"
   &SAMPLE="GetLoanEPS('Кредит','123',01/01/2015,OUTPUT oEps)"}
PROCEDURE GetLoanEps:
   DEF INPUT  PARAM iContract AS CHAR NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHAR NO-UNDO.
   DEF INPUT  PARAM iDate     AS DATE NO-UNDO.
   DEF OUTPUT PARAM oEps      AS DEC  NO-UNDO.
   
   DEF VAR vAddPay   AS LOG  NO-UNDO.
   
   /* сначала пытаемся подучить ЭПС с ДР на договре */
   oEps = DEC(GetXAttrValueEx("loan",iContract + "," + iContCode,"ЭПС","0")).
   IF oEps =  0 THEN
   DO:
      RUN pGetEpsLoan IN h_loan (iContract, 
                                 iContCode, 
                                 iDate,
                                 OUTPUT oEps,
                                 OUTPUT vAddPay).  
      oEps = oEps * 100.
   END.
END PROCEDURE.

{pfuncdef 
   &DEFPROC="GetFinalDate"
   &DESCRIPTION="получение даты фак.исполнения обязательств по договору"
   &PARAMETERS="BUFFER loan,ДАТА"
   &RESULT="Дата фактического исполнения обязательств"
   &SAMPLE="GetLoanEPS(BUFFER Loan,01/01/2015,OUTPUT oFinDate)"}
PROCEDURE GetFinalDate:
   DEF PARAM  BUFFER loan FOR loan.               /* Кредитный договор        */
   DEF INPUT  PARAM  iDate    AS DATE NO-UNDO.  /* Дата отчета           */
   DEF OUTPUT PARAM  oFinDate AS DATE NO-UNDO.  /* Дата фактического исполнения обязательств */

   DEF VAR vListCode AS CHAR  NO-UNDO 
       INIT "ТекПроц,ТекОД,ЗадолжОД,ЗадолжПр,ТекВыкПр,ТекВыкПн,ПеняОД,ПеняПр".
   DEF VAR vCnt      AS INT64 NO-UNDO.
   DEF VAR vCnt1     AS INT64 NO-UNDO.

   DEF BUFFER code     FOR code.
   DEF BUFFER loan-int FOR loan-int.
   
   /* находим дату последнего списания по параметрам из НБКИ_КодОпер */
   DO vCnt = 1 TO NUM-ENTRIES(vListCode):
      FOR EACH code WHERE code.class =  "НБКИ_КодОпер"
                      AND code.code  =  ENTRY(vCnt,vListCode)
      NO-LOCK:
         DO vCnt1 = 1 TO NUM-ENTRIES(code.misc[3]):
            FIND LAST loan-int WHERE loan-int.contract  =  loan.contract
                                 AND loan-int.cont-code =  loan.cont-code
                                 AND loan-int.id-d      =  INT64(ENTRY(vCnt1,code.misc[3]))
            NO-LOCk NO-ERROR.
            IF AVAIL loan-int THEN
            DO:
               IF oFinDate =  ? THEN
                  oFinDate = loan-int.mdate.
               ELSE 
                  oFinDate = MAX(oFinDate,loan-int.mdate).
            END.
         END.
      END.
   END.
   
END PROCEDURE.
/******************************************************************************/
/* $LINTFILE='pp-bki.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:16:13.518+03:00' */
/*prosignOOq5JdgNSEbc7yj+ocs0sg*/