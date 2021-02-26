/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/* НЕОБХОДИМО ИМЕТЬ OP-TEMPLATE */
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
DEF VAR vInn     AS CHAR NO-UNDO.
DEF VAR vTextErr AS CHAR NO-UNDO.
DEF VAR vLockErr AS CHAR NO-UNDO.

DEFINE BUFFER u-op FOR op.
DEFINE BUFFER u-oe FOR op-entry.

FIND op-impexp OF yop-entry NO-LOCK NO-ERROR.
{debug.i "avail op-impexp" "avail op-impexp"}
IF AVAIL op-impexp THEN
   IF op-impexp.exp-batch NE "" THEN NEXT loop.
FIND op-bank OF yop NO-LOCK NO-ERROR.
rid = recid(yop-entry).
{debug.i "rid" rid}

ASSIGN
 ndd     = ndd + 1
 buf-sum = buf-sum + yop-entry.amt-rub.

{debug.i "doc-num" yop.doc-num}
find first yop-entry of yop WHERE
           yop-entry.op-entry EQ op-entry.op-entry and
           yop-entry.acct-cr NE ? no-lock no-error.
FIND FIRST kacct WHERE kacct.acct EQ yop-entry.acct-cr NO-LOCK NO-ERROR.
&IF DEFINED(dbcr) &THEN
   find first yop-entry of yop WHERE
              yop-entry.op-entry EQ op-entry.op-entry and
              yop-entry.acct-db NE ? no-lock no-error.
   FIND FIRST kacct WHERE kacct.acct EQ yop-entry.acct-db NO-LOCK NO-ERROR.
&ENDIF

IF AVAIL kacct THEN DO:
   {debug.i "Cчет" kacct.acct}
   FIND FIRST mail-user WHERE
      mail-user.cust-cat EQ kacct.cust-cat AND
      mail-user.cust-id  EQ kacct.cust-id  AND
      mail-user.mail-format EQ "{&format-message}" AND
      mail-user.op-kind-exp EQ op-kind.op-kind AND
      mail-user.filial-id   EQ shFilial AND
      CAN-DO(mail-user.acct,kacct.acct)
   NO-LOCK NO-ERROR.
   IF AVAIL mail-user THEN
   DO:
      {debug.i "Абонент" mail-user.mail-user-num}
      in-user-num = mail-user.mail-user-num.

      CREATE w-op.
      CREATE w-op-entry.
      {op-entry.i w-op-entry yop-entry}
      {op.i  &buf1=w-op &buf2=yop}

      ASSIGN
         w-op.op-kind  = op-template.op-kind + "," +
                         string(op-template.op-template)
         w-op.op-error = "".

      IF GetXAttrValue("op-kind",op-template.op-kind,"ImportDocType") NE
         "Doc-Type"
         THEN
            RUN GetDocTypeDigital IN h_op (w-op.doc-type,
                                           ?,
                                           OUTPUT w-op.doc-type).

      FIND op-bank OF yop NO-LOCK NO-ERROR.

      IF AMBIGUOUS op-bank THEN
      DO: /*Транзитный */
         flag-go = 3.
         {debug.i "Транз"}

         FIND op-bank OF yop WHERE
            op-bank.op-bank-type EQ "send" NO-LOCK NO-ERROR.
         FIND banks-code WHERE
            banks-code.bank-code EQ op-bank.bank-code AND
            banks-code.bank-code-type EQ "МФО-9"
         NO-LOCK NO-ERROR.
         IF AVAIL banks-code
            THEN id-bank = banks-code.bank-id.
         ELSE id-bank = 0.
         vInn = GetBankInn ("bank-id", STRING(id-bank)).
         ASSIGN
            w-op.bank-code-send      = IF AVAIL op-bank
                                          THEN op-bank.bank-code
                                       ELSE "0"
            w-op.bank-corr-acct-send = IF AVAIL op-bank
                                          THEN op-bank.corr-acct
                                       ELSE "0"
            w-op.inn-bank-send       = IF {assigned vInn}
                                          THEN vInn
                                       ELSE "000000000000".

         IF AVAILABLE(op-bank) THEN
         DO: /* включить фиктивную запись */
            {e-swift.crb ""send""}
         END.

         FIND op-bank OF yop WHERE
            op-bank.op-bank-type EQ "rec" NO-LOCK NO-ERROR.
         FIND banks-code WHERE
            banks-code.bank-code      EQ op-bank.bank-code AND
            banks-code.bank-code-type EQ "МФО-9"
         NO-LOCK NO-ERROR.
         IF AVAIL banks-code
            THEN id-bank = banks-code.bank-id.
         ELSE id-bank = 0.
         vInn = GetBankInn ("bank-id", STRING (id-bank)).
         ASSIGN
            w-op.bank-code-rec      = IF AVAIL op-bank
                                         THEN op-bank.bank-code
                                      ELSE "0"
            w-op.bank-corr-acct-rec = IF AVAIL op-bank
                                         THEN op-bank.corr-acct
                                      ELSE "0"
            w-op.inn-bank-rec       = IF {assigned vInn}
                                         THEN vInn
                                      ELSE "00000000000".

         IF AVAILABLE(op-bank) THEN
         DO: /* включить фиктивную запись */
            {e-swift.crb ""rec""}
         END.
      END.
      ELSE DO: /*Обычный */
         {debug.i "Обычн"}
         ASSIGN
            name-pl = ""
            flag-go = 1
            inn-pl  = "".

         FIND bal-acct OF kacct NO-LOCK NO-ERROR.
         IF AVAIL bal-acct AND
            CAN-DO(str-acct,string(bal-acct.bal-acct))
            /* список балансовых счетов,по которым в поле "ИНН"
               подставляется инн банка, а не клиента */
            THEN ASSIGN
               inn-pl  = bank-inn
               name-pl = dept.name-bank.

         ELSE DO:
            {getcust.i &pref=k &name=name &inn=temp-inn &OFFInn="/*"}
            ASSIGN
               name-pl = name[1] + " " + name[2]
               inn-pl  = IF temp-inn NE ""
                         THEN string(temp-inn,"999999999999")
                         ELSE "000000000000".
         END.

         IF kacct.cust-cat EQ "В" THEN
         ASSIGN
             inn-pl  = bank-inn
             name-pl = dept.name-bank.

         FIND banks-code WHERE
            banks-code.bank-code EQ op-bank.bank-code AND
            banks-code.bank-code-type EQ "МФО-9"
         NO-LOCK NO-ERROR.
         IF AVAIL banks-code
            THEN id-bank = banks-code.bank-id.
         ELSE id-bank = 0.
         vInn = GetBankInn ("bank-id", STRING (id-bank)).
         IF yop-entry.acct-db EQ ? THEN
            FIND FIRST xop-entry OF yop WHERE
               xop-entry.acct-db NE ? NO-LOCK NO-ERROR.
         ASSIGN
            w-op.bank-code-send      = bank-mfo-9
            w-op.bank-corr-acct-send = bank-acct
            w-op.inn-bank-send       = bank-inn
            w-op.inn-send            = inn-pl
            w-op.acct-send           = IF yop-entry.acct-db NE ?
                                       THEN yop-entry.acct-db
                                       ELSE IF AVAIL xop-entry
                                       THEN xop-entry.acct-db
                                       ELSE yop-entry.acct-db
            w-op.name-send           = name-pl

            w-op.acct-rec            = yop.ben-acct
            w-op.name-rec            = yop.name-ben
            w-op.bank-code-rec       = IF AVAIL op-bank
                                       THEN op-bank.bank-code
                                       ELSE "0"
            w-op.bank-corr-acct-rec  = IF AVAIL op-bank AND
                                          op-bank.corr-acct NE ?
                                       THEN op-bank.corr-acct
                                       ELSE "0"
            w-op.inn-bank-rec        = IF {assigned vInn}
                                       THEN vInn
                                       ELSE "000000000000"
            w-op.inn-rec             = yop.inn.

         IF AVAILABLE(op-bank) THEN
         DO:   /* включить фиктивную запись */
            {e-swift.crb ""rec""}
         END.
      END.

      IF yop-entry.acct-db EQ ? THEN
      DO:
         RELEASE acct.
         FIND FIRST xop-entry OF yop WHERE
            xop-entry.acct-db NE ? NO-LOCK NO-ERROR.
         IF AVAIL xop-entry THEN
            FIND FIRST acct WHERE
               acct.acct EQ xop-entry.acct-db NO-LOCK NO-ERROR.
      END. /* банк заказчика */
      ELSE
         FIND FIRST acct WHERE
            acct.acct EQ yop-entry.acct-db NO-LOCK NO-ERROR.
      IF AVAIL acct AND
         acct.cust-cat EQ "Б"
         THEN flag-go = 3.

      {empty w-signs}

      FOR EACH signs WHERE signs.file EQ "op" AND
                           signs.surr EQ STRING(yop.op) NO-LOCK:
         CREATE w-signs.
         BUFFER-COPY signs TO w-signs NO-ERROR.
      END.

      FOR EACH op-bank OF yop NO-LOCK:
         IF NOT CAN-FIND(FIRST w-op-bank WHERE
            w-op-bank.op EQ op-bank.op AND
            w-op-bank.op-bank-type EQ op-bank.op-bank-type)
         THEN
         DO:
            CREATE w-op-bank.
            BUFFER-COPY op-bank TO w-op-bank NO-ERROR.
         END.
      END.
      ASSIGN
         w-op.inn-rec   = GetXattrValueEx("op",string(op.op),"inn-rec",
                                          w-op.inn-rec)
         w-op.inn-send  = GetXattrValueEx("op",string(op.op),"inn-send",
                                          w-op.inn-send)
         w-op.name-rec  = GetXattrValueEx("op",string(op.op),"name-rec",
                                          w-op.name-rec)
/* Замена Плюс банк
         w-op.name-send = GetXattrValueEx("op",string(op.op),"name-send",
                                          w-op.name-send)
*/       vInn           = GetXattrValue  ("op",string(op.op),"swift-50f-4") /* телефон ФЛ */
         w-op.name-send = GetXattrValueEx("op",string(op.op),"sw-name-send",
                                          w-op.name-send)
                        + (IF (vInn = "") THEN "" ELSE (", " + vInn))
         w-op.details   = GetXattrValueEx("op",string(op.op),"sw-details",
                                          w-op.details)
/* Конец замены Плюс банк */
         w-op.acct-rec  = GetXattrValueEx("op",string(op.op),"acct-rec",
                                          w-op.acct-rec)
         w-op.acct-send = GetXattrValueEx("op",string(op.op),"acct-send",
                                          w-op.acct-send)
      .
      &IF DEFINED(TEST) &THEN
      RUN SetSysConf IN h_base ("format-message", "{&format-message}").
      
      RUN i-sw-upd.p (flag-go,YES).
      
      RUN SetSysConf IN h_base ("format-message", "").
      
      {empty w-op-bank} /* удалить фиктивные записи */

      CREATE exp-temp-table.
      ASSIGN
         exp-temp-table.order    = (IF INT64(yop.order-pay) EQ 0 THEN 600
                                    ELSE IF INT64(yop.order-pay) >= 10
                                    THEN INT64(yop.order-pay)
                                    ELSE INT64(yop.order-pay) * 100)
         exp-temp-table.op       = yop-entry.op
         exp-temp-table.N-templ  = op-template.op-template
         exp-temp-table.op-templ = recid(op-template)
         exp-temp-table.arbit    = mail-user.file-exp
         exp-temp-table.op-entry = yop-entry.op-entry.

      FIND FIRST w-op NO-LOCK NO-ERROR.
      FIND FIRST w-op-entry NO-LOCK NO-ERROR.

      IF w-op.op-error NE "" THEN
      DO:
         ASSIGN
           flag-err                 = YES
           exp-temp-table.op-status = "".

         DEF VAR err-class AS CHAR INITIAL "mess-error" NO-UNDO.
         {sw-error.fnd
            &pre-op-kind   = "entry(1,w-op.op-kind)"
            &pre-err-class = err-class
            &pre-TmpStr    = w-op.op-error
            &pre-in        = "{e-swcr.i &tt=y}"
            &pre-i         = j
            &pre-frame     = "with FRAME exp-error"
         }
      END.

   vTextErr = "Документ изменяет другой пользователь. "  +
              "Проверьте соответствие данных сообщению. " +
              "Статус сообщения не изменен.". 


   FIND FIRST u-op WHERE recid(u-op) EQ recid(op) EXCLUSIVE-LOCK NO-ERROR no-wait.
   IF NOT AVAILABLE(u-op) THEN 
   DO:
      WhoLocks2(recid(op),"op",INPUT-OUTPUT vLockErr).
      IF {assigned vLockErr} THEN DO:
         MESSAGE "Документ" Op.doc-num ".~n" vLockErr
                     VIEW-AS ALERT-BOX.

         FIND FIRST exp-temp-table WHERE
                    exp-temp-table.op EQ op.op NO-ERROR.
         IF AVAIL exp-temp-table THEN
            DELETE exp-temp-table.
      END.
   END.
   ELSE
   FOR EACH yop-entry OF op NO-LOCK:
      FIND FIRST u-oe WHERE recid(u-oe) EQ recid(yop-entry) EXCLUSIVE-LOCK NO-ERROR no-wait. 
      IF NOT AVAILABLE(u-oe) THEN 
      DO:
         WhoLocks2(recid(u-oe),"op-entry",INPUT-OUTPUT vLockErr).
         IF {assigned vLockErr} THEN DO:
         MESSAGE "Документ" Op.doc-num ".~n" vLockErr
                     VIEW-AS ALERT-BOX.

         FIND FIRST exp-temp-table WHERE
                    exp-temp-table.op EQ yop-entry.op NO-ERROR.
         IF AVAIL exp-temp-table THEN
            DELETE exp-temp-table.
         END.
      END.
   END.
      &ENDIF
   END. /* avil mail-user */
END.  /* AVAIL kacct */
