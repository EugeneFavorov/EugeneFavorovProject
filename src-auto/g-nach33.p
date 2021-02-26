/*
               Банковская интегрированная система БИСквит
    copyright: (c) 1992-2020 ЗАО "Банковские информационные системы"
     filename: g-nach33.p
      comment: Групповая операция начисления % по вкладам
   parameters: in-op-date, oprid
         uses:
      used by:
      Created: xx/xx/xxxx
     Modified: 08/05/2003 Илюха - Форматирование
                                  + вставлен инклюдник g-psigns.i с обработкой
                                  реквизитов создаваемых парсером
                                  + мессяги выдаваемые в logg.p перенесены в
                                  dps-logs.i
     Modified: 03.02.2004 15:48 FEAK      
     Modified: 12.01.2005 12:23 SAP       
     Modified: 12.01.2005 12:35 SAP       
     Modified: 12.01.2005 15:27 SAP       
     Modified: 31.08.2005 21:32 serge    0051030 Оптимизация использования временных таблиц
     Modified: 29.10.2005 17:44 SAP       
     Modified:

*/

DEF INPUT PARAM in-op-date LIKE op.op-date NO-UNDO.
DEF INPUT PARAM oprid      AS RECID        NO-UNDO.

/*Отключение триггеров для лучшей прозводительности*/
DISABLE TRIGGERS FOR LOAD OF kau-entry.
DISABLE TRIGGERS FOR LOAD OF kau.
DISABLE TRIGGERS FOR DUMP OF kau-entry.
DISABLE TRIGGERS FOR DUMP OF kau.

DEF NEW GLOBAL SHARED STREAM err.
DEF STREAM err1.
DEF STREAM doc.
DEF STREAM err-ved.


&SCOP Ofsrch     0
&SCOP DoContract 0
&SCOP Col-lab    'ПЛАНОВАЯ ДАТА НАЧИСЛЕНИЯ'
&SCOP BYrole     YES
&Glob type-nach 'Резервирование'
&SCOPED-DEFINE Nachkin-tt Nchk-tt
{globals.i}
{intrface.get brnch}
{g-defs.i}
{dpsproc.def}

DEFINE VAR str-kau      AS CHAR     NO-UNDO.
DEFINE VAR cod-ost      AS CHAR     NO-UNDO.
DEF VAR type-of-nach AS CHAR NO-UNDO.

{chktempl.i}    /* Поиск транзакции и шаблонов */

/* Инициализация процесса протоколирования. */
RUN Init-SysMes IN h_tmess (op-kind.op-kind, "", "").

cod-ost =  get-ost(op-kind.op-kind).
IF cod-ost EQ "НачПр" THEN 
DO:
   /*для парсера, чтобы запускал "быструю схему начисления" */
   RUN SetSysConf IN h_base ("TypeNach", "Резервирование").
   type-of-nach =  {&type-nach}.
END.

{def-wf.i new}
{chk-stat.i &NOT_CLOSE=YES}

{defframe.i new}
{intrface.get nachd}
{intrface.get xclass}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{prn-ved.def
    &new-nach = new-nach
    &tt = {&Nachkin-tt}
}
{dps-logs.i
   &DpsLogsQuoter = "'"}
{cr-nach.i
       no-svget="/*"
}
{prn_ved_nach.i 
   &Nachkin-tt = Nchk-tt}

DEFINE VAR tt-num       AS INT64.
DEFINE VAR tmp-recid    AS CHAR.
DEFINE VAR b-date       AS DATE     NO-UNDO.
DEFINE VAR cred         AS DATE     NO-UNDO.
DEFINE VAR i            AS INT64      NO-UNDO.
DEFINE VAR i-num        AS INT64      NO-UNDO.
DEFINE VAR str-num      AS CHAR     NO-UNDO.
DEFINE VAR loan_h       AS HANDLE   NO-UNDO.
DEFINE VAR let-nach     AS LOGICAL  NO-UNDO.
DEFINE VAR k            AS INT64      NO-UNDO.
DEFINE VAR ntempl       AS INT64      NO-UNDO.
DEFINE VAR name-klient  AS CHAR     NO-UNDO.
DEFINE VAR fler         AS LOGICAL  NO-UNDO.
DEFINE VAR prn-tit      AS CHAR FORMAT 'X(50)'  NO-UNDO.
DEFINE VAR c-type       AS CHAR INIT '*'        NO-UNDO.
DEFINE VAR rid1         AS RECID EXTENT 30      NO-UNDO.
DEFINE VAR curr1        LIKE loan.currency          NO-UNDO.
DEFINE VAR in-contract  LIKE loan.contract          NO-UNDO.
DEFINE VAR dval         LIKE op-entry.value-date    NO-UNDO.
DEFINE VAR in-cont-code LIKE loan.cont-code         NO-UNDO.
DEFINE VAR s-rub        LIKE op-entry.amt-rub EXTENT 30  NO-UNDO.
DEFINE VAR s-cur        LIKE op-entry.amt-cur EXTENT 30 NO-UNDO.
DEFINE VAR vChkDate     AS LOG                      NO-UNDO.
DEF    VAR mAfterProc   AS CHARACTER                NO-UNDO. 
DEF    VAR mErrText     AS CHAR                     NO-UNDO. /* Иногда необходимо сохранить текст возможной ошибки */  
DEF    VAR mHPQuery     AS HANDLE         NO-UNDO.
DEF    VAR mRowId       AS ROWID          NO-UNDO.
DEF VAR mRek AS CHAR   NO-UNDO.

DEF VAR vTmp-Date AS DATE NO-UNDO.
DEFINE VAR t-type       AS CHAR NO-UNDO.
DEF VAR vi AS INT64 NO-UNDO.
DEF VAR vTotal AS DEC NO-UNDO.
DEF VAR vSave%  AS LOG NO-UNDO.
DEF VAR fname   AS CHARACTER                NO-UNDO. 


DEFINE VARIABLE mUser   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMenuId AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipPotok AS INT64 NO-UNDO.
DEFINE VARIABLE mDelim  AS INT64 NO-UNDO.

IF NUM-ENTRIES(session:parameter) GE 8 THEN
ASSIGN
   ipPotok = INT64(ENTRY(3,session:parameter))
   mDelim  = INT64(ENTRY(4,session:parameter))
   mUser   =  ENTRY(7,session:parameter) 
   in-op-date = IF ENTRY(8,session:parameter) NE "*" THEN DATE(ENTRY(8,session:parameter))  ELSE  today - 1.
ELSE
  RETURN.

DEFINE BUFFER xwop       FOR wop.
DEFINE BUFFER xop-entry  FOR op-entry.
DEFINE BUFFER xxop-entry FOR op-entry.
DEFINE BUFFER xop-templ  FOR op-templ.

let-nach = (TRIM(FGetSetting('НачислЕжедн',?,?)) = "Да").

{g-currv1.i 
   &OFbase = "/*"
   &ByRole = YES
}

FUNCTION Set_type   RETURNS LOGICAL (INPUT l-type  AS CHAR) IN loan_h.
FUNCTION Set_ost    RETURNS LOGICAL (INPUT cod-ost AS CHAR) IN loan_h.
FUNCTION Set_period RETURNS LOGICAL (INPUT dat1    AS DATE,
                                     INPUT dat2    AS DATE) IN loan_h.

{details.def}

DebugParser = INT64(GetXAttrValueEx('op-kind',
                                  op-kind.op-kind,
                                  'debugparser',
                                  '0')).

{befexpr.i &befopkind = op-kind.op-kind}

RUN "l-type.p" PERSISTENT SET loan_h.

ASSIGN
   cur-op-date = in-op-date
   ntempl      = op-templ.op-templ
   .


{g-frame3.i
   &op      = t-op
   &DoTable = yes
}
{g-frame3.i
   &DoFrame = YES
   &row     = 10
   &op      = t-op
}
/*чистка зомби*/
run GarbageCollect in h_base.
/*отключение процедуры ZombyProc*/
run  NoGarbageCollect in h_base.


{plibinit.i}
{currency.def}
{prn-ved.i
   &EmptyTempTable     = "Очищаем табличку"
   &DefPrintProcedures = "Объявляем процедуры печати"
   &Stream             = " STREAM err"
   &FileName           = _spool1.tmp
   &PutPlanDate        = cred
}


MAIN:
DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   FOR EACH op-template OF op-kind
      NO-LOCK
         BREAK
         BY op-template.op-template
         ON ERROR  UNDO, RETURN
         ON ENDKEY UNDO, RETURN:
   
      CREATE t-op.
      ASSIGN
         t-op.op             = op-templ.op-templ
         t-op.doc-type       = op-templ.doc-type
         t-op.op-date        = in-op-date
         t-op.contract-date  = IF op-templ.op-templ EQ 1 THEN
                                  in-op-date 
                               ELSE
                                  cred
         t-op.doc-date       = in-op-date
         t-op.details        = op-templ.details
         .
/*   
      {g-frame3.i
         &DoBefore = YES
         &op       = t-op
      }
      {g-frame3.i
         &DoDisp   = YES
         &op       = t-op
      }

      &IF DEFINED(SESSION-REMOTE) &THEN
      {g-frame3.i
         &DoSet    = YES
         &op       = t-op
         &DoDocNum = YES
      }
      &ELSE
      {g-frame3.i
         &DoSet    = YES
         &op       = t-op
      }
      &ENDIF
*/   
      IF op-templ.op-templ EQ 1 THEN
         cred = t-op.contract-date.
   
      IF op-templ.op-templ EQ 1 AND
      t-op.doc-num NE '' AND
      t-op.doc-num NE ?
      THEN
      DO:
         tt-num =  INT64(t-op.doc-num) NO-ERROR.
         str-num = IF ERROR-STATUS:ERROR
            THEN t-op.doc-num
            ELSE ''.
      END.
   END.
   IF tt-num EQ 0 THEN tt-num = 1.
   FIND FIRST op-template OF op-kind
      NO-LOCK NO-ERROR.
   curr1 = IF op-template.currency NE ? THEN GetCurr(op-template.currency)
                                        ELSE op-template.currency.
   
   IF GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?) <> ? THEN
      c-type = GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?).
   
   
   FOR EACH CODE WHERE CODE.class = "cont-type"
         AND CODE.PARENT = "cont-type" NO-LOCK:
         IF  CAN-DO(c-type, CODE.CODE)  THEN DO:
            {additem.i t-type CODE.CODE}
         END.
   END.

   vSave% = (FGetSetting('СохрПроц',?,?) = "Да").
   IF vSave%  THEN DO:
      RUN prep_cr_data (cred,
                        GetThisUserOtdel(),
                        mDataClass-ID). 
   END.

   RUN qrybrwld.p ("dep_person",THIS-PROCEDURE:HANDLE,OUTPUT mHPQuery) NO-ERROR.

   /* В переменную mHBrwQuery (bstty.def) кладется хэндл загруженной DS-компоненты */
   IF NOT VALID-HANDLE (mHPQuery) THEN 
   DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "Ошибка загрузки процедуры формирования запроса для класса ~"dep_person~"").
      UNDO MAIN, LEAVE MAIN.
   END.
   
   DO vi = 1 TO NUM-ENTRIES(t-type):
   
      RUN SetDSContext IN mHPQuery ("icontr~001close-date1~001close-date2~001cont-type" + (IF curr1 NE ? THEN "~001currency"
                                                                                                         ELSE "") + "~001since2~001FieldOper" ,
                                    "dps~001?~001?~001" + ENTRY(vi, t-type) + (IF curr1 NE ? THEN ("~001" + curr1)
                                                                                             ELSE "") + "~001" + STRING(in-op-date) + "~001since2,LE",
                                    "",
                                    "") NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         UNDO MAIN, LEAVE MAIN.
         
      /* Открытие запроса */
      RUN Open-Query IN mHPQuery.
                            
      /* обработка ошибки */
      IF ERROR-STATUS:ERROR THEN 
         UNDO MAIN, LEAVE MAIN.
     
      tr:
      DO WHILE TRUE
         TRANSACTION 
         ON ERROR  UNDO tr, LEAVE tr
         ON ENDKEY UNDO tr, LEAVE tr
         WITH FRAME prn:
          IF RETRY THEN .
         RUN Get_Query_Record IN mHPQuery ("NEXT",
                                           "loan-cond",
                                           OUTPUT mRowId).      

         IF mRowId EQ ?  /* or i-num GE 5 */  THEN
            LEAVE tr.

         FIND FIRST loan-cond WHERE ROWID(loan-cond) EQ mRowId
            NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            FIND FIRST loan WHERE loan.contract  EQ loan-cond.contract
                              AND loan.cont-code EQ loan-cond.cont-code
               NO-LOCK NO-ERROR.
         IF     AVAIL loan-cond
            AND AVAIL loan THEN         
         DO:      
         IF INT64(RECID(loan)) MODULO mDelim NE ipPotok THEN NEXT tr.                  

            FIND FIRST person WHERE
                 person.person-id EQ loan.cust-id
            NO-LOCK NO-ERROR.
      
            ASSIGN  
               name-klient = IF AVAILABLE person THEN person.name-last + " " + person.first-name
                                                 ELSE ""
               mErrText = {&ErrorAcctOperLogg}                           
               .
               
            IF loan.loan-status NE "ф" THEN
            DO:
               RUN CreateErr({&ErrorDpsStatus}).
               UNDO tr, RETRY tr.
            END.
      
            RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).
            ASSIGN
               in-contract  = loan.contract
               in-cont-code = loan.cont-code
               .
      
            doc:
            FOR EACH op-template OF op-kind
            NO-LOCK
                  ON ERROR  UNDO doc, LEAVE doc
                  ON ENDKEY UNDO doc, LEAVE doc
            WITH FRAME prn:
               FIND t-op WHERE
                    t-op.op EQ op-template.op-template
               NO-LOCK NO-ERROR.
      
               IF NOT AVAILABLE t-op THEN
                  NEXT.
      
               /*Если это дата капитализации процентов, то в этот день не резервируем*/
      
               IF op-templ.op-templ EQ 1
               THEN
               DO:
               vChkDate = Chk_Date(RECID(loan-cond), t-op.contract-date).
                  /*Если это не капитализация и не последний день месяца,
                   и нельзя начислять кажый день то нам тут делать нечего.*/
                  IF NOT vChkDate AND /*не капитализация*/
                     NOT (MONTH(t-op.contract-date) NE MONTH(t-op.contract-date + 1)) AND /*не последний день месяца*/
                     NOT let-nach
                  THEN
                  DO:
                      RUN CreateErr({&ErrorDpsStatus}). 
                      UNDO tr, RETRY tr.
                  END.
               END.
               /************************************************************************/

              /* Проверка на шаблон без создания объектов */
              IF GetXAttrValueEx("op-template",
                             op-template.op-kind + "," + STRING(op-template.op-template),
                             "PrsnTmpl",
                             "Нет") EQ "Нет" THEN
              DO:
          
               i-num = i-num  + 1.

               CREATE op.
               {op(sess).cr}
               {g-op.ass}

               mRek = GetXattrValue ("op-template",
                                     op-template.op-kind + "," + STRING (op-template.op-template),
                                     "ДокНомер"). 
               IF NOT {assigned mRek} THEN
               DO:
                   ASSIGN
                       op.doc-num       = IF str-num EQ '' THEN
                                         STRING(tt-num)
                                         ELSE
                                            str-num + STRING(tt-num)
                   .
               END.
               ASSIGN
                  op.doc-type      = t-op.doc-type
                  op.details       = t-op.details + IF fGetSetting("СодержОперВкл",?,'ДА') EQ 'ДА'  THEN ' Вклад N ' + loan.doc-ref + ' Клиент ' + name-klient
                                                    ELSE ""

                  op.contract-date = t-op.contract-date
                  tt-num           = tt-num + 1 .
               IF type-of-nach = {&type-nach}
               THEN DO :
                  /*Резервирование!*/
                  FIND LAST loan-acct OF loan WHERE
                            loan-acct.acct-type EQ "loan-dps-int"
                        AND loan-acct.since     LE op.contract-date
                  NO-LOCK NO-ERROR.
      
                  IF NOT AVAILABLE loan-acct THEN
                  DO:
                     RUN CreateErr({&NotLoanDpsInt}).                
                     UNDO doc, LEAVE doc.
                  END.
                  /*пытаемся резервироваить в день капитализации или не в последний день месяца*/
                  IF (MONTH(op.contract-date) EQ MONTH (op.contract-date + 1) OR vChkDate )
                      AND NOT let-nach
                  THEN
                  DO:
                     RUN CreateErr({&NotNachDateLogg}).
                     UNDO doc, LEAVE doc.
                  END.
               END.
               /*не резервирование*/
               ELSE /* ne "НачПр"  */
                  IF (NOT vChkDate AND NOT let-nach) THEN 
                     cod-ost = ?.
                  ELSE
                     IF vChkDate AND cod-ost = ? THEN
                        cod-ost = IF loan.end-date EQ ?  THEN
                                     'НачПрВ'
                                  ELSE
                                     'НачПрС1'.
      
               Set_type(loan.cont-code).
               IF cod-ost <> ? THEN
                  Set_ost(cod-ost).
      
               ASSIGN
                  str-kau = "dps" + "," + loan.cont-code + "," +  cod-ost.
      
               /*проверка на будущие начисления*/
               RUN get-Beg-date-all in h_dpspc (RECID(loan),
                                                01/01/3000,
                                                OUTPUT vTmp-Date).          
               IF vTmp-Date >= op.contract-date THEN DO:
                   RUN CreateErr({&NachHaveDone}).
                   UNDO doc ,LEAVE doc.
               END.
               FORM
                  HEADER
                  CAPS(name-bank)
                  FORMAT "x(55)"
                  AT 5
                  PAGE-NUMBER
                  FORMAT "Листzz9" TO 82
                  SKIP
                  WITH FRAME prn WIDTH-CHARS 160.
      
               FIND LAST xxop-entry WHERE
                         xxop-entry.op EQ op.op
               NO-LOCK NO-ERROR.
      
               k = IF AVAILABLE xxop-entry THEN
                      xxop-entry.op-entry + 1
                   ELSE
                      1.
      
               CREATE op-entry.
            &IF DEFINED(oracle) &THEN
                VALIDATE op NO-ERROR.
            &ENDIF
                {g-en.ass &ind=k}
            IF NUM-ENTRIES(session:parameter) GE 7 THEN
                UpdateSigns(op.Class-Code, STRING(op.op), "СоздалПоток",
                         ENTRY(7,session:parameter) + "|" + ENTRY(3,session:parameter),?).
                ASSIGN
                   op-entry.value-date = in-op-date
                   op-entry.currency   = IF op-templ.currency NE ? THEN
                                            GetCurr(op-templ.currency)
                                         ELSE
                                            loan.currency
                   tcur                = op-entry.currency
                   .
      
                FOR EACH wop WHERE
                         wop.op-templ GE op-templ.op-templ:
                   DELETE wop.
                END.
      
                CREATE wop.
      
                {asswop.i}
      
                ASSIGN
                   wop.currency = op-entry.currency
                   dval         = op-entry.value-date
                   wop.op-kind  = op-kind.op-kind
                   wop.con-date = op.contract-date
                   wop.op-templ = op-template.op-template
                   wop.op-recid = RECID(op-entry)
                   wop.con-date = op.contract-date
                   cred         = op.contract-date
                   .
      
                {g-acctv1.i
                   &OFbase = YES
                   &BYrole = YES
                      &vacct  = tacct
                }
                IF tacct-db EQ ? OR tacct-cr EQ ? THEN
                DO:
      
                   IF tacct-cr = ? THEN
                      RUN CreateErr({&NotFindCrAcctLogg}).             
                   IF tacct-db = ? THEN 
                      RUN CreateErr({&NotFindDbAcctLogg}).             
                   UNDO tr, RETRY tr .
                END.
      
                ASSIGN
                   wop.acct-db  = tacct-db
                   wop.acct-cr  = tacct-cr
                   .
                set_type(loan.cont-code).
                RUN ProcessDetails (RECID(wop), INPUT-OUTPUT op.detail).
                IF {assigned cod-ost} THEN
                   Set_ost(cod-ost).
      
                IF (op-templ.prep-amt-rub    NE ? AND
                    op-templ.prep-amt-rub    NE "") OR
                   (op-templ.prep-amt-natcur NE ? AND
                    op-templ.prep-amt-natcur NE "") THEN
                DO:
                   RUN parssen.p (RECID(wop), in-op-date, OUTPUT
                      fler).
                   IF fler THEN
                   DO:
                      RUN CreateErr({&ErrorParsenLogg}).
                      UNDO tr, LEAVE tr.
                   END.
                   ASSIGN
                      op-entry.amt-rub = wop.amt-rub
                      op-entry.amt-cur = IF op-entry.currency NE "" THEN wop.amt-cur
                                                                    ELSE op-entry.amt-cur
                      op-entry.user-id = USERID('bisquit')
                      op-entry.acct-cr = tacct-cr
                      op-entry.acct-db = tacct-db
                      .
                   IF op-entry.amt-rub NE 0 OR
                      op-entry.amt-cur NE 0 THEN
                   DO:
                      {op-entry.upd
                         &871       = YES
                         &Ofnext    = "/*"
                         &open-date = "DO:
                                          UNDO tr, RETRY tr.
                                       END "
                         &open-undo = "DO:
                                          RUN CreateErr(mErrText).
                                          UNDO tr, RETRY tr.
                                       END "
                      }
                       /* Создание ДР типа PARSEN_<КодРеквизита>*/
                      {g-psigns.i}
                      {crs.i}
                   END.
                   ELSE
                   DO:
                      RUN CreateErr({&NullSummProc} +  " (шаблон " + STRING(op-template.op-template) + ")").                 
                      UNDO doc, LEAVE doc.
                   END.
                   ASSIGN
                      s-rub[op-templ.op-templ] = s-rub[op-templ.op-templ] +
                                                 op-entry.amt-rub
                      s-cur[op-templ.op-templ] = s-cur[op-templ.op-templ] +
                                                 op-entry.amt-cur
                         .
                END. /*=0 */
               END. /*prsntmpl = no*/
               ELSE
               DO:
               Set_type(loan.cont-code).
               {wop-cr.i
                &Err-ParsAcct   = "IF tacct-db EQ ? OR tacct-cr EQ ? THEN     ~
                                   DO:                                        ~
                                      IF tacct-cr = ? THEN                    ~
                                         RUN CreateErr({&NotFindCrAcctLogg}). ~
                                      IF tacct-db = ? THEN                    ~
                                         RUN CreateErr({&NotFindDbAcctLogg}). ~
                                      UNDO tr, NEXT tr.                       ~
                                   END. "
                &Err-ParsSumm   = "IF fler THEN                               ~
                                   DO:                                        ~
                                      RUN CreateErr({&ErrorParsenLogg}).      ~
                                      UNDO tr, LEAVE tr.                      ~
                                   END. "
                &NoParsDetails  = YES
                }
                END. /* PrsnTmpl=yes */

             END. /*doc: FOR EACH op-templ*/
          END.  /*avail loan loan-cond*/
       END. /*tr*/
       
       RUN Close-Query IN mHPQuery (NO).
   END. /*cont-type*/
   
   RUN Close-Query IN mHPQuery (YES).

   OUTPUT STREAM err CLOSE.
   
   IF vSave%  THEN 
   DO:
      RUN setlevel IN h_debug (mTmpLevel).
   END.
   
   /*Печать ведомостей*/
   mAfterProc = op-kind.after.
   {prn-ved.i
     &RunReportPrint  = "Печатаем ведомость"}

   IF NOT {assigned mAfterProc} THEN
   DO:
      IF mReportProcName EQ ? THEN DO:   

         output stream err CLOSE.
         OUTPUT stream err TO VALUE ( ISO-DATE(in-op-date) + "_" + shFilial + "_" + op-kind.op-kind + "_nach" + ".log" + STRING(ipPotok)) APPEND.

         /* Ведомость начисленных процентов */
         RUN ved_nach (in-op-date,
                       cred).   
      END.
   END.
   IF CAN-FIND (FIRST err) THEN 
   DO:
      output stream err-ved CLOSE.
      OUTPUT stream err-ved TO VALUE ( ISO-DATE(in-op-date) + "_" + shFilial + "_" + op-kind.op-kind + "_error" + ".log" + STRING(ipPotok)) APPEND.

      RUN ved_err ("").
   END.
   
   OUTPUT STREAM err-ved CLOSE.
   OUTPUT STREAM err CLOSE.
 
  
END.

RUN DeleteOldDataProtocol IN h_base ("TypeNach").
RUN DeleteOldDataProtocol IN h_base ("mData-ID").
RUN DeleteOldDataProtocol IN h_base ("OldRef").
EMPTY TEMP-TABLE t-op.
HIDE FRAME opreq NO-PAUSE.
/*чистка зомби*/
run GarbageCollect in h_base.
 
DELETE PROCEDURE loan_h.
{plibdel.i}

/* Завершение процесса протоколирования. */
RUN End-SysMes IN h_tmess.

{intrface.del}

/* Удаление SysConf, созданного в Get-Old-Ref */
RUN DeleteOldDataProtocol IN h_base("OldRef").

/* Команда ОС после выполнения транзакции */
{cmd-exec.i
   &cmd = "'Postcmd'"
   }

RETURN.


