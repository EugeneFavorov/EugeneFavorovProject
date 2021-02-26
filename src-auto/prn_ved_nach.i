/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: PRN_VED_NACH.I
      Comment: Инклюд для печати отчетов групповых процедур, поддерживающих
               "быстрое" начисление.
   Parameters:
         Uses:
      Used by:
      Created: 17.11.2006 16:05 fEAk    
     Modified: 11.02.2008 14:05 fEAk     <comment>
*/

/* Таблица ошибок при работе групповых транзакций */                         
DEF TEMP-TABLE err NO-UNDO
   FIELD err AS CHAR
   FIELD num AS INT64
   INDEX err num.

/* Таблица для пошаблонной печати договоров процедур dps-tr и dps-tr1 */
DEF TEMP-TABLE print-op NO-UNDO
   FIELD cont-code AS CHARACTER
   FIELD curr      AS CHARACTER 
   FIELD amt-rub   AS DECIMAL 
   FIELD amt-cur   AS DECIMAL 
   FIELD op-templ  AS INT64 
   FIELD details   AS CHARACTER 
   INDEX op-templ op-templ.

/* Таблица для хранения сумм по комиссиям для ведомости начисленных процентов */
DEFINE TEMP-TABLE NchSumm
      FIELD ContCode AS CHARACTER 
      FIELD CommSumm AS DECIMAL
      FIELD CommCode AS CHARACTER 
   INDEX CCCC ContCode CommCode.

&IF DEFINED(ved_prolong) NE 0 &THEN
DEFINE TEMP-TABLE ttProlong
   FIELD OpDate   AS DATE
   FIELD ContCode AS CHARACTER
   FIELD Curr     AS CHARACTER 
INDEX ind1 OpDate ContCode.
&ENDIF

DEF VAR vErrNum AS INT64 NO-UNDO. /* Номер ошибки */

&IF DEFINED(ved_prolong) NE 0 &THEN
FORM
   op-entry.op-date
   loan.cont-code
   loan.currency
WITH FRAME info-fr.
&ENDIF

/* Создание записи об ощибке */
PROCEDURE CreateErr.
   DEF INPUT PARAM iMess AS CHAR NO-UNDO.

   CREATE err.
   ASSIGN 
      err.num = vErrNum
      err.err = iMess
      .
   RELEASE err.

   vErrNum = vErrNum + 1.

END PROCEDURE.


PROCEDURE prep_cr_data.
   DEF INPUT PARAM iCred  AS DATE NO-UNDO.
   DEF INPUT PARAM iOtdel AS CHAR NO-UNDO.
   DEF INPUT PARAM iDCID  AS CHAR NO-UNDO.
   
   DEF VAR vDataID   AS CHAR    NO-UNDO.
   DEF VAR vTmpLevel AS INT64     NO-UNDO.

   /*Без этого не создастся блок данных*/
   RUN SetSysConf IN h_base ("op-contract-date", string(iCred)).
   RUN SetSysConf IN h_base ("op-id","0").
   
   RUN GetLevel IN h_debug (OUTPUT vTmpLevel).
   RUN setlevel IN h_debug (-1).
   RUN sv-get.p (Input  iDCID,
                 Input  iOtdel,
                 Input  iCred,
                 Input  iCred,
                 Output vDataID).
   IF vDataID EQ ? THEN DO:
      MESSAGE "Информацию о %% сохранить невозможно. Продолжить?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE vPr AS LOG.
      IF NOT vPr  THEN do:
         RUN setlevel IN h_debug (vTmpLevel).
         RETURN.
      END.
   END.
   ELSE 
      RUN SetSysConf IN h_base ("mData-ID",string(vDataID)).
   RUN DeleteOldDataProtocol IN h_base ("op-contract-date").
   RUN DeleteOldDataProtocol IN h_base ("op-id").
END PROCEDURE.


&IF DEFINED(no_nachkin) EQ 0 &THEN
PROCEDURE ved_nach.
   DEF INPUT PARAM iOpDate LIKE op.op-date NO-UNDO.
   DEF INPUT PARAM iCred   AS DATE NO-UNDO.

   DEF VAR vDelNchk  AS LOGICAL   NO-UNDO.
   DEF VAR vPrevNchk AS CHARACTER NO-UNDO.
   DEF VAR vTotSumm  AS DECIMAL   NO-UNDO.

   DEFINE BUFFER Nchk FOR {&Nachkin-tt}.   
   DEF VAR vCommSumm AS DEC NO-UNDO.

   FORM 
      {&Nachkin-tt}.ContCode
      {&Nachkin-tt}.acct
      {&Nachkin-tt}.BegDate  
      {&Nachkin-tt}.EndDate  
      {&Nachkin-tt}.Days     
      {&Nachkin-tt}.SummOst
      {&Nachkin-tt}.CommCode
      {&Nachkin-tt}.SummProc 
      vCommSumm FORMAT "->>>,>>>,>>>,>>9.99"
       
   WITH DOWN WIDTH 200 NO-LABELS FRAME ved.

   PUT stream err UNFORMATTED "Ведомость начисленных процентов на " +
      STRING(iOpDate) AT 5 SKIP
   "Плановая дата " + STRING(iCred) AT 5 SKIP(1).
   
   PUT stream err  UNFORMATTED
          'НОМЕР ВКЛАДА'      AT 1
          'СЧЕТ'              AT 17
          'С'                 AT 43
          'ПО'                AT 52
          'КОЛ-ВО ДНЕЙ'       AT 61
          'ОСТАТОК'           AT 84
          'СТАВКА'            AT 106
          'НАЧИСЛЕНО'         AT 123
          'ИТОГО'             AT 147
          SKIP.
   
   PUT stream err UNFORMATTED
      '-------------------------------------------------------------------------------------------------------------------------------------------------------'
      SKIP.


   FOR EACH {&Nachkin-tt} BREAK BY {&Nachkin-tt}.curr
                                BY {&Nachkin-tt}.Contcode
                                BY {&Nachkin-tt}.opCode 
                                BY {&Nachkin-tt}.pars-type
                                BY {&Nachkin-tt}.CommCode
                                BY {&Nachkin-tt}.BegDate
                                /*BY {&Nachkin-tt}.EndDate*/
                                :

      IF FIRST-OF ({&Nachkin-tt}.curr) THEN DO:
            IF ({&Nachkin-tt}.SummProc NE 0) OR ({&Nachkin-tt}.SummProc EQ 0 AND CAN-FIND (FIRST Nchk WHERE Nchk.curr EQ {&Nachkin-tt}.curr
                                       AND Nchk.SummProc GT 0)) THEN 
            IF {&Nachkin-tt}.curr EQ "" THEN
               PUT stream err  UNFORMATTED 'Рублевые договора' SKIP.
            ELSE
               PUT stream err  UNFORMATTED 'Валюта ' + {&Nachkin-tt}.curr SKIP.
      END.
      /* Первая запись по вкладу */
      IF FIRST-OF ({&Nachkin-tt}.Contcode) THEN DO:

         IF ({&Nachkin-tt}.SummProc NE 0) OR
            ({&Nachkin-tt}.SummProc EQ 0 AND 
                CAN-FIND (FIRST Nchk WHERE Nchk.Contcode EQ {&Nachkin-tt}.Contcode
                                       AND Nchk.SummProc GT 0))  THEN 
            DISPLAY STREAM err
               {&Nachkin-tt}.Contcode FORMAT "x(20)"
               {&Nachkin-tt}.Acct     FORMAT "x(20)"
            WITH /*WIDTH 200 NO-LABELS*/ FRAME ved.
      END.
      /* Вывод основной части */
      IF {&Nachkin-tt}.SummProc GT 0 THEN DO:
      
          DISP STREAM  err            
                   {&Nachkin-tt}.BegDate
                   {&Nachkin-tt}.EndDate
                   {&Nachkin-tt}.Days
                   {&Nachkin-tt}.SummOst FORMAT "->>>,>>>,>>>,>>9.99"
                   FILL(" ", (20 - LENGTH(STRING({&Nachkin-tt}.CommCode,"x(9)") + " " + STRING({&Nachkin-tt}.Comm, ">>9.99999")))) + (STRING({&Nachkin-tt}.CommCode,"x(9)") + " " + STRING({&Nachkin-tt}.Comm, ">>9.99999")) FORMAT  "x(20)" @ {&Nachkin-tt}.CommCode
                   {&Nachkin-tt}.SummProc FORMAT "->>>,>>>,>>>,>>9.99"

                  WITH  FRAME ved.
       END.
       
      ASSIGN
         vCommSumm = vCommSumm + {&Nachkin-tt}.SummProc
         vTotSumm  = vTotSumm + {&Nachkin-tt}.SummProc
         .

      IF LAST-OF ({&Nachkin-tt}.CommCode) THEN 
      DO:
         IF vCommSumm NE 0 THEN 
         DO:
            DISPLAY STREAM err
                vCommSumm
                WITH FRAME ved.
            PUT STREAM err UNFORMATTED SKIP(1).
         END.
         vCommSumm = 0.
      END.
      DOWN STREAM err WITH FRAME ved.

      IF     LAST-OF ({&Nachkin-tt}.curr) 
         AND vTotSumm NE 0 THEN 
      DO:         
         PUT STREAM err UNFORMATTED 
            FILL(" ", 115) + "ВСЕГО ПО ВАЛЮТЕ: " + STRING(vTotSumm, "->>>,>>>,>>>,>>9.99") SKIP(2).
         vTotSumm = 0.
      END.
   END.
   
   FIND _user WHERE _user._userid EQ userid('bisquit') NO-LOCK NO-ERROR.
   DISPLAY STREAM err SKIP(2) "Исполнитель:"
      "_________________________" AT 20
      _user._user-name FORMAT "x(30)"  NO-LABEL AT 48 SKIP(2)
      "Контролер:"
      "_________________________" AT 20
      "_____________________________" AT 48 SKIP(2).

END PROCEDURE.
&ENDIF


PROCEDURE ved_err.
   DEF INPUT PARAM iTitle AS CHARACTER NO-UNDO.

   PUT STREAM err-ved  UNFORMATTED iTitle + " ~n ~n".
   FOR EACH err:
      PUT STREAM err-ved  UNFORMATTED err.err + "~n".
   END.
END PROCEDURE.


&IF DEFINED(no-ved_op) EQ 0 &THEN
PROCEDURE ved_op.
   DEF INPUT PARAM iSetDest   AS INT64         NO-UNDO.
   DEF INPUT PARAM iLstTmplOp AS CHARACTER       NO-UNDO.
   DEF INPUT PARAM iOpKindRec AS RECID           NO-UNDO.
   DEF INPUT PARAM iRIDOp     AS CHAR            NO-UNDO. 

   DEF VAR vI        AS INT64            NO-UNDO.
   DEF VAR vDetails  AS CHARACTER   NO-UNDO.

   DO vI = iSetDest TO NUM-ENTRIES(iLstTmplOp) TRANSACTION:
      FIND FIRST op-kind WHERE RECID(op-kind) EQ iOpKindRec NO-LOCK NO-ERROR.
      FIND FIRST op-template OF op-kind WHERE op-template.op-template EQ INT64(ENTRY(vI,
                                                                                   iLstTmplOp))
                                                                            NO-LOCK NO-ERROR.

      IF NOT AVAIL op-template THEN NEXT.
      
      FIND op WHERE RECID(op) EQ INT64(ENTRY(op-template.op-template, iRIDOp))
                                              NO-LOCK NO-ERROR.
      IF NOT AVAIL op THEN NEXT.
      FIND FIRST op-entry OF op  NO-LOCK NO-ERROR.

      IF NOT AVAIL op-entry THEN DO:
         FIND op WHERE RECID(op) EQ INT64(ENTRY(op-template.op-template, iRIDOp))
                                          EXCLUSIVE-LOCK NO-ERROR.
         DELETE op.
      END.
      ELSE DO:      
         vDetails = GetXattrValueEx("op-template",op-template.op-kind + "," + STRING (op-template.op-template), "Заголовок", "").
         IF NOT {assigned vDetails} THEN
            IF AVAIL op THEN
               vDetails = op.details.
            ELSE
               vDetails = "Рассчитанные проценты".

         put stream err1 unformatted 'Плановая дата ' + string(op.contract-date) skip  .
         put stream err1 unformatted 'Дата операционного дня ' + string(op.op-date) skip .
         put stream err1 unformatted  vDetails skip(1).

         PUT STREAM err1 UNFORMATTED 
            "НОМЕР ДОГОВОРА         ВАЛ      СУММА ИНВАЛЮТЫ     СУММА НАЦ. ВАЛ." SKIP
            "────────────────────── ─── ─────────────────── ───────────────────" SKIP.       
        
         FOR EACH op-entry OF op NO-LOCK:
         
             PUT STREAM err1 UNFORMATTED
                 STRING(SUBSTRING (GetXAttrValue ("op-entry",STRING (op-entry.op) + "," + STRING (op-entry.op-entry),'cont-code'), 5), "x(22)")
                 STRING(op-entry.currency, "x(3)") + " " + 
                 STRING(op-entry.amt-cur, "->>>,>>>,>>>,>>9.99") + " " + 
                 STRING(op-entry.amt-rub, "->>>,>>>,>>>,>>9.99") SKIP(1).
                 UpdateSigns ("op-entry",STRING (op-entry.op) + "," + STRING (op-entry.op-entry),'cont-code',?,?).
         END.
         PUT STREAM err1 UNFORMATTED SKIP(2).
         PAGE STREAM err1.
     end.
   END.
END PROCEDURE.
&ENDIF


PROCEDURE Create-Op-Dpstr.   
   DEF INPUT PARAM iDetails AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vDetails AS CHARACTER   NO-UNDO.

   CREATE print-op.
   ASSIGN
      print-op.op-templ  = op-templ.op-templ
      print-op.cont-code = loan.cont-code
      print-op.curr      = op-entry.currency
      print-op.amt-cur   = op-entry.amt-cur
      print-op.amt-rub   = op-entry.amt-rub
      .   
   vDetails = GetXattrValueEx("op-template",op-template.op-kind + "," + STRING (op-template.op-template), "Заголовок", "").
   IF {assigned vDetails} THEN
      print-op.details = vDetails.
   ELSE
   DO:
      IF {assigned iDetails} THEN
         print-op.details = iDetails.
      ELSE
      DO:
      IF AVAIL op THEN
         print-op.details = op.details.
      ELSE
         print-op.details = "Рассчитанные проценты".
      END.
   END.
END PROCEDURE.


PROCEDURE PrintVed.
   DEF INPUT PARAM iDate  AS DATE NO-UNDO.
   DEF INPUT PARAM iFname AS CHAR NO-UNDO.

   DEF VAR vPutStr AS CHARACTER NO-UNDO.
   DEF VAR fname   AS CHARACTER NO-UNDO.

   FOR EACH print-op BREAK BY print-op.op-templ
                           BY print-op.cont-code:
      IF FIRST-OF (print-op.op-templ) THEN
      DO:
         Fname = REPLACE(iFname,"<&>",STRING(print-op.op-templ)).
         ASSIGN
            vPutStr                = ""
            SUBSTRING(vPutStr,5,1) = print-op.details + " на "
                                     + STRING(iDate)
            .
         RUN PutStrToFile(fname,
                          vPutStr,
                          1).
         ASSIGN
            vPutStr                 = ""
            SUBSTRING(vPutStr,5,1)  = 'Номер вклада'
            SUBSTRING(vPutStr,30,1) = 'Валюта'
            SUBSTRING(vPutStr,40,1) = 'Сумма в валюте'
            SUBSTRING(vPutStr,60,1) = 'Сумма в рублях'
            .
         RUN PutStrToFile(fname,
                          vPutStr,
                          0).
         vPutStr = FILL ('-', 24) + ' ' +
                   FILL ('-', 9)  + ' ' +
                   FILL ('-', 19) + ' ' +
                   FILL ('-',23)  + ' '.
         RUN PutStrToFile(fname,
                          vPutStr,
                          0).
      END.
      ASSIGN
         vPutStr                 = ""
         SUBSTRING(vPutStr,5,1)  = DelFilFromLoan(print-op.cont-code)
         SUBSTRING(vPutStr,30,1) = print-op.curr
         SUBSTRING(vPutStr,40,1) = IF print-op.curr NE "" THEN
                                      STRING(print-op.amt-cur,
                                             '>>,>>>,>>>,>>9.99')
                                   ELSE
                                      ""
         SUBSTRING(vPutStr,60,1) = STRING(print-op.amt-rub,
                                          '>>,>>>,>>>,>>9.99')
         .   
   RUN PutStrToFile(fname,
                    vPutStr,
                    0).
   END.   

   RUN PrintToFile.
END PROCEDURE.


&IF DEFINED(ved_prolong) NE 0 &THEN
/* Формирование ttProlong */
PROCEDURE CreatettProlong.
   DEF INPUT  PARAM iDate     AS DATE        NO-UNDO.
   DEF INPUT  PARAM iContCode AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAM iCurr     AS CHARACTER   NO-UNDO.
   
   CREATE ttProlong.
   ASSIGN 
      ttProlong.OpDate     = iDate
      ttProlong.ContCode   = iContCode
      ttProlong.Curr       = iCurr
      .
END PROCEDURE.


/* Вывод отчета о пролонгированных вкладах */
PROCEDURE DisplayttProlong.
   FORM
      op-entry.op-date
      loan.cont-code
      loan.currency
   WITH FRAME info-fr DOWN.

   PUT STREAM _prolong_ UNFORMATTED "Пролонгированные договоры" SKIP(2).

   DOWN STREAM _prolong_  1 WITH FRAME info-fr.


   FOR EACH ttProlong BREAK BY ttProlong.Curr
                            BY ttProlong.OpDate
                            BY ttProlong.ContCode:
      ACCUM ttProlong.ContCode (COUNT BY ttProlong.Curr).                            
      ACCUM ttProlong.ContCode (COUNT).
      
      IF FIRST-OF(ttProlong.Curr) THEN
      DO:
         PUT STREAM _prolong_ UNFORMATTED
            "По валюте "
            (IF ttProlong.Curr EQ "" THEN FGetSetting("КодНацВал",?,"") ELSE ttProlong.Curr)
            ":"
            SKIP. 
      END.                      
                           
      DISPLAY STREAM _prolong_
         ttProlong.OpDate     @ op-entry.op-date
         ttProlong.ContCode   @ loan.cont-code   
         ttProlong.Curr       @ loan.currency
         WITH FRAME info-fr DOWN.
      DOWN STREAM _prolong_ WITH FRAME info-fr.

      IF LAST-OF(ttProlong.Curr) THEN
      DO:
         PUT STREAM _prolong_ UNFORMATTED
            "Итого: " (ACCUM COUNT BY ttProlong.Curr ttProlong.ContCode).
         IF LAST(ttProlong.Curr) THEN
            PUT STREAM _prolong_ UNFORMATTED SKIP
               "Oбщий ИТОГ: " (ACCUM COUNT ttProlong.ContCode).
         PUT STREAM _prolong_ SKIP(1).
      END.   

   END.
END PROCEDURE.
&ENDIF
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='17/04/2015 15:13:37.358+04:00' */
/* $LINTUSER='kozv' */
/* $LINTMODE='1' */
/* $LINTFILE='prn_ved_nach.i' */
/*prosignzQ6Ol1vDBCXJny5qdE/6nQ*/