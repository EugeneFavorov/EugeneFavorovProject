/*
Глухов. Модернизация op-copy.p
*/

{globals.i}
{intrface.get instrum}
{intrface.get xclass}
{intrface.get rights}

DEF INPUT  PARAM oldrid     AS RECID NO-UNDO. /* ID исходного документа. */
DEF OUTPUT PARAM newrid     AS RECID
                            INIT 0   NO-UNDO. /* ID нового документа. */
DEF INPUT  PARAM in-op-date AS DATE  NO-UNDO. /* Дата ОД копии. */
DEF INPUT  PARAM iOpStatus  AS CHAR  NO-UNDO. 
DEF INPUT  PARAM iOpClass   AS CHAR  NO-UNDO. 

DEF VAR cur-op-date AS DATE   NO-UNDO.
DEF VAR rid         AS RECID
                    EXTENT 50 NO-UNDO.
DEF VAR j           AS INT64    NO-UNDO.
DEF VAR flager      AS INT64    NO-UNDO.
DEF VAR flag        AS INT64    NO-UNDO.
DEF VAR cddif       AS LOG    NO-UNDO.
DEF VAR mExcDR      AS CHAR   NO-UNDO.

{defoptr.i new}
{sh-defs.i}

DEF BUFFER cop        FOR op.       /* Буфер исходного документа. */
DEF BUFFER cop-en     FOR op-entry.
DEF BUFFER cop-bank   FOR op-bank.
DEF BUFFER cop-impexp FOR op-impexp.
DEF BUFFER ccust-role FOR cust-role.
DEF BUFFER dacct      FOR acct.
DEF BUFFER cacct      FOR acct.
DEF BUFFER xop        FOR op.
DEF BUFFER csigns     FOR signs.

PROCEDURE CopyOpDates PRIVATE.
    DEFINE PARAMETER BUFFER dst-op FOR op.
    DEFINE PARAMETER BUFFER src-op FOR op.
    ASSIGN
    dst-op.doc-date      = in-op-date
    dst-op.due-date      = in-op-date
    dst-op.ins-date      = in-op-date
    dst-op.op-value-date = in-op-date
    dst-op.contract-date = in-op-date.
    
END PROCEDURE.

tt:
DO TRANS
ON ENDKEY UNDO tt, RETURN
ON ERROR  UNDO tt, RETURN:

   FIND cop WHERE
      RECID(cop) EQ oldrid
   SHARE-LOCK NO-ERROR NO-WAIT.

   IF NOT AVAIL cop THEN DO:
      RETURN "Документ удален или кем-то редактируется.".
   END.

   cur-op-date = in-op-date.

   CREATE op.

   {op(sess).cr &op-status=iOpStatus}

   op.class = iOpClass.

   {g-op.ass &ob=cop}

   ASSIGN
      op.doc-num       = cop.doc-num
      op.op-status     = iOpStatus
   .

   RUN CopyOpDates(BUFFER op, BUFFER cop).

   FIND FIRST op-entry OF cop WHERE
      op-entry.acct-db EQ ?
   NO-LOCK NO-ERROR.

   IF AVAIL op-entry THEN
      cddif = YES.
   ELSE
   DO:

      FIND FIRST op-entry OF cop WHERE
         op-entry.acct-cr EQ ?
      NO-LOCK NO-ERROR.

      IF AVAIL op-entry THEN cddif = YES.
   END.

   pick-value = "no".

   RUN RunClassMethod IN h_xclass (op.class-code,
                                   "chkupd",
                                   "","",
                                   ?,
                                   string(recid(op)) + ",copy").

   IF NOT CAN-DO ("no-method, no-proc", RETURN-VALUE) AND
      pick-value NE "yes"
      THEN UNDO tt, RETURN.

   FOR EACH cop-en OF cop:
      CREATE op-entry.

      {g-en.ass &eb=cop-en}                      /* Формирование проводки.    */

      ASSIGN
         op-entry.qty        = cop-en.qty
         op-entry.value-date = cur-op-date
      .
      
      IF NOT cddif                        AND
         cur-op-date    NE cop-en.op-date AND
         cop-en.amt-cur NE 0              AND
         cop-en.curr    GT ""             THEN
         op-entry.amt-rub = CurToBase ("УЧЕТНЫЙ",
                                       cop-en.currency,
                                       cur-op-date,
                                       cop-en.amt-cur).
      {op-entry.upd
         &open-undo = "undo tt, return"
         &kau-undo  = "undo tt, return"
         &chkupd    = "copy"
         &Offopupd  = "/*"
      }
   END.

   FOR EACH cop-bank OF cop
   NO-LOCK:
      CREATE op-bank.
      BUFFER-COPY cop-bank EXCEPT op TO op-bank
         ASSIGN op-bank.op = op.op NO-ERROR.
      FOR EACH csigns WHERE csigns.file-name EQ "op-bank"
                        AND csigns.surr EQ STRING(cop-bank.op) + "," +
                                          cop-bank.op-bank-type
      NO-LOCK:
          CREATE signs.
          BUFFER-COPY csigns EXCEPT surr TO signs
             ASSIGN signs.surr = STRING(op.op) + "," +
                                 op-bank.op-bank-type NO-ERROR.
      END.
   END.

   mExcDR = FGetSetting("ИсклДр", "", "") .

   {additem.i  " mExcDR " 'num-rkc' " }
   {additem.i  " mExcDR " 'Право_ввода' " }
   {additem.i  " mExcDR " 'Право_второй_подписи' " }
   {additem.i  " mExcDR " 'Право_первой_подписи' " }
   
   FOR EACH csigns WHERE csigns.file-name EQ "op"
                     AND csigns.surrogate EQ string(cop.op)
                     AND NOT CAN-DO(mExcDR,csigns.code)    
   USE-INDEX surrogate NO-LOCK:

      &IF DEFINED(oracle) &THEN
      IF CAN-FIND (FIRST signs WHERE signs.file-name EQ csigns.file-name AND
                                     signs.surrogate EQ STRING(op.op)    AND
                                     signs.code      EQ csigns.code
                               NO-LOCK) THEN
         NEXT.
      &ENDIF

      CREATE signs.
      BUFFER-COPY csigns EXCEPT surrogate TO signs
         ASSIGN signs.surrogate = STRING(op.op) NO-ERROR.

      /* Не выводить возможное сообщение об ошибке уникальности индекса.
      ** (некоторые ДР могут быть созданы в op-entry.upd) */
      IF ERROR-STATUS:ERROR THEN
         UNDO, NEXT.
   END.
   
   FOR EACH ccust-role WHERE 
            ccust-role.file-name EQ "op"      
        AND ccust-role.surrogate EQ STRING(cop.op) 
   NO-LOCK:
      CREATE cust-role.
      BUFFER-COPY ccust-role EXCEPT surrogate cust-role-id TO cust-role.
      ASSIGN 
         cust-role.surrogate = STRING(op.op) NO-ERROR.
      FOR EACH csigns WHERE 
               csigns.file-name EQ "cust-role"
           AND csigns.surr      EQ STRING(ccust-role.cust-role-id)
      NO-LOCK:
          CREATE signs.
          BUFFER-COPY csigns EXCEPT surr TO signs
             ASSIGN signs.surr = STRING(cust-role.cust-role-id) NO-ERROR.
      END.
   END.

   newrid = RECID (op).

   {op.upd
         &undo      = "undo tt, return"
         &chkupd    = "copy"
   }

   /* не копируются code-relation */
END.

{intrface.del }
