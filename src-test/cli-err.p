DEF INPUT PARAM in-rec AS recid NO-UNDO.

DEF VAR buf        AS CHAR NO-UNDO FORMAT "x(570)".
DEF VAR curr-iso   AS CHAR NO-UNDO.
DEF VAR det        AS CHAR NO-UNDO EXTENT 3.
DEF VAR filename   AS CHAR NO-UNDO.
DEF VAR i          AS INT64  NO-UNDO INIT 0.
DEF VAR j          AS INT64  NO-UNDO INIT 0.
DEF VAR msg        AS CHAR NO-UNDO FORMAT "x(70)".
DEF VAR mError     AS CHAR NO-UNDO.
DEF VAR mStrFormat AS CHAR NO-UNDO /*список форматов*/.
DEF VAR mFormat    AS CHAR NO-UNDO /*формат для финотказа*/.
DEF VAR mStrNum    AS CHAR NO-UNDO. /*строка номеров правил для финотказа*/
DEF VAR name       AS CHAR NO-UNDO EXTENT 2.
DEF VAR origin     AS CHAR NO-UNDO.
DEF VAR reject-cnt AS INT64  NO-UNDO.

DEFINE VARIABLE mOK     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mResMsg AS CHARACTER   NO-UNDO.
DEFINE VAR fl_DBO_opkind AS LOGICAL     NO-UNDO.
DEF BUFFER xop FOR op.

{globals.i}
{intrface.get op}
{intrface.get widg}
{pick-val.i}
{wordwrap.def}
{chkacces.i}

{topkind.def}

IF OPSYS EQ "unix" THEN
   origin = OS-GETENV("ORIGIN") + "/".
ELSE
   origin = OS-GETENV("ORIGIN") + "~\".

PAUSE 0 before-hide.

RUN CheckOpRight IN h_base(in-rec,?,"Upd") NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   RETURN.
END.

FIND FIRST op        WHERE recid(op) EQ in-rec NO-LOCK NO-ERROR.
FIND FIRST op-impexp OF op                     NO-LOCK NO-ERROR.
FIND FIRST op-entry  OF op                     NO-LOCK NO-ERROR.
FIND FIRST op-bank   OF op                     NO-LOCK NO-ERROR.

det[1] = op.details.
{wordwrap.i &s=det &l=60 &n=3}


fl_DBO_opkind = CAN-DO(FGetSettingEx("Документы","КодыТДБО","",NO),op.op-kind).

FIND FIRST op-impexp OF    op NO-LOCK NO-ERROR.
FIND FIRST acct      WHERE acct.acct     EQ op-entry.acct-db
                       AND acct.currency EQ op-entry.currency NO-LOCK NO-ERROR.

                       
FOR EACH mail-user WHERE
   CAN-DO(mail-user.acct,acct.acct)
NO-LOCK:
   IF NOT CAN-DO(mStrFormat,mail-user.mail-format) THEN
   DO:
      {additem.i mStrFormat mail-user.mail-format}
      {additem.i mStrNum string(mail-user.mail-user-num)}
   END.
END.

FOR EACH AbntRule WHERE AbntRule.file-name EQ "acct" AND
                        AbntRule.Surrogate EQ acct.acct
   NO-LOCK:
   IF NOT CAN-DO (mStrFormat, AbntRule.FormatVers) THEN
   DO:
      {additem.i mStrFormat AbntRule.FormatVers}
      {additem.i mStrNum string(AbntRule.mail-user-num)}
   END.
END.

IF NUM-ENTRIES(mStrFormat) > 1 THEN
   RUN SLMenu IN h_widg
      (mStrFormat,"выберите формат по которому выполнять финотказ",
       OUTPUT mFormat).
ELSE mFormat = mStrFormat.

FIND FIRST mail-user WHERE
   mail-user.mail-user-num EQ INT64(ENTRY(LOOKUP(mFormat,mStrFormat),mStrNum))
NO-LOCK NO-ERROR.

IF (mFormat EQ ? OR
   mStrFormat EQ "" OR
   NOT AVAIL mail-user) AND  
   NOT fl_DBO_opkind 
THEN
DO:
   MESSAGE "Абонент по счету " acct.acct SKIP
          " не найден !" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF AVAIL mail-user AND mail-user.mail-format EQ "CLI-BANK" THEN DO:
   FIND FIRST person-ident WHERE
      person-ident.acct       EQ acct.acct AND
      person-ident.currency   EQ acct.currency AND
      person-ident.ident-type EQ "PGP" NO-LOCK NO-ERROR.
   IF NOT AVAIL person-ident THEN DO:
      MESSAGE "У абонента нет ни одного счета !".
      RETURN.
   END.
END.
ELSE IF AVAIL mail-user AND mail-user.mail-format BEGINS "SWIFT" THEN DO:
   RUN e-swo195.p
      (op.op,mail-user.mail-user-num,op-entry.acct-db,op-entry.currency).
   RETURN.
END.
ELSE IF AVAIL mail-user AND mail-user.mail-format BEGINS "TELEX" THEN DO:
   RUN e-tel195.p
      (op.op,mail-user.mail-user-num,op-entry.acct-db,op-entry.currency).
   RETURN.
END.

IF NOT AVAIL(mail-user) THEN
DO:
   MESSAGE "Отказ не сформирован. Вероятно клиент не подключен к банк-клиенту"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

DO ON ENDKEY UNDO, LEAVE
   ON ERROR UNDO, LEAVE WITH FRAME aa:

   ON F1 anywhere DO: END.

   UPDATE buf VIEW-AS EDITOR size 69 BY 10
      WITH FRAME aa ROW 10 CENTERED OVERLAY no-labels
      TITLE COLOR bright-white "[ ВВЕДИТЕ ТЕКСТ CООБЩЕНИЯ ]"

   EDITING:
      READKEY.
      IF LASTKEY EQ KEYCODE("F1") THEN DO:
         RUN pclass
            ("mess-error","mail", "ФИНАНСОВЫЕ ОТКАЗЫ И СООБЩЕНИЯ ОБ ОШИБКАХ",4).
         IF (LASTKEY EQ 13 OR LASTKEY EQ 10) AND
            pick-value NE ? THEN
         DO:
            FIND FIRST code WHERE
               code.parent EQ "mail" AND
               code.class  EQ "mess-error" AND
               code.code   EQ ENTRY(1, pick-value)
            NO-LOCK NO-ERROR.
            buf = INPUT buf + (IF INPUT buf NE "" THEN "~n"  ELSE "") +
                  code.name.
            DISPLAY  buf.
         END.
      END.
      ELSE
         APPLY LASTKEY.
   END.
END.

HIDE FRAME aa.
PAUSE 0 before-hide.

{message &text="|Отправить финансовый отказ клиенту ? |Вы уверены ?"
         &alert-box=question
         &buttons=yes-no
}.

IF pick-value EQ "no" OR
   LASTKEY EQ 27
   THEN RETURN.
   ELSE IF pick-value EQ "yes" THEN
   DO:
      RUN UpdStatusAfterFin IN h_op (in-rec, op.op-status, OUTPUT mError).
      IF mError NE "" THEN DO:
         MESSAGE
            " Не удалось изменить статус." SKIP
            mError
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
      END.
   END.
filename = {f-name.i}

IF AVAIL mail-user AND mail-user.mail-format EQ "CLI-BANK" THEN
DO:
   {cli-err.i
      &msg-type   = "'text'"
      &msg-cat    = "'op'"
      &object-id  = "string(op.op)"
      &msg-header = "'Финансовый отказ'"}

   IF mail-user.file-exp NE "" THEN
   DO:
      filename = filename  + ".txt".
      msg = "".
      OUTPUT TO VALUE(mail-user.file-exp + "/" + filename).
      export "960".
      export op-impexp.op-referenc.
      DO i = 1 TO NUM-ENTRIES(buf,chr(10)):
         export ENTRY(i,buf,chr(10)).
      END.
      export "-----END-----".
      OUTPUT CLOSE.

      OUTPUT TO VALUE(mail-user.file-exp + "/bishead.log" ) append.
      PUT UNFORMATTED
      filename + ",4," + string(today,"99/99/9999") SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF mail-user.e-mail-addr NE "" THEN
   DO:
      msg = "".
      DO:
         filename = origin + {f-name.i}
         OUTPUT TO VALUE(filename ).
         export "960".
         export op-impexp.op-referenc.
         DO i = 1 TO NUM-ENTRIES(buf,"~n"):
            export ENTRY(i,buf).
         END.
         export "-----END-----".
         OUTPUT CLOSE.
      END.
      RUN exp-err.p (filename,acct.acct,acct.currency).
   END.
END.
ELSE IF AVAIL mail-user AND mail-user.mail-format MATCHES "*ARPO*" THEN
DO:
   {cli-err.i
      &msg-type   = "'e-swo195'"
      &msg-cat    = "'op'"
      &object-id  = "string(op.op)"
      &msg-header = "'Финансовый отказ'"}
   {getcust.i &name=name &OffInn=/*}
   IF AVAIL op AND
      AVAIL mail-user THEN
   DO:
      fileName = {f-name.i}                      /* Сообщение об ошибке                          */
      filename = filename + ".txt".
      OUTPUT TO VALUE(mail-user.file-exp + "/" + filename ).

      {curr-iso.i &curr="op-entry.currency"}

      PUT UNFORMATTED                                SKIP
         "ARPO 00"                                   SKIP
         "FROM:    " name-bank                       SKIP
         "TO:      " name[1] + " " + name[2]         SKIP
         "DATE:    " {date-swf.i &date="op.op-date"} SKIP
         "SUBJECT: ФИНАНСОВЫЙ ОТКАЗ"                 SKIP
         "TYPE:    Z905"                             SKIP(1)
         ":13: " {date-swf.i &date="op.op-date"} ";"
            string(time,"hh:mm:ss")                  SKIP
         ":20: ФИНАНСОВЫЙ ОТКАЗ"                     SKIP
         ":21: " op-impexp.op-reference              SKIP
         ":25: " op-entry.acct-db ";" curr-iso       SKIP
         ":73: "                                     SKIP.
      DO i = 1 TO NUM-ENTRIES(buf,chr(10)):
         PUT UNFORMATTED  "     " ENTRY(i,buf,chr(10)) SKIP.
      END.
   END.
   PUT UNFORMATTED ":ZZ:" SKIP "ARPO END" SKIP(1).
   OUTPUT CLOSE.

   IF OPSYS EQ "unix" AND
      SEARCH(mail-user.file-exp + "/" + filename) NE ? THEN
      unix silent chmod 666 VALUE(mail-user.file-exp + "/" + filename).
END.
ELSE IF AVAIL mail-user AND mail-user.mail-format EQ "DIASOFT" THEN
DO:
   IF mail-user.file-exp NE "" THEN
   DO:

      FIND FIRST _user WHERE
         _user._userid EQ userid('bisquit') NO-LOCK NO-ERROR.
      buf = "Документ от " + string(op.op-date,"99/99/9999")                              +
            " номер "      + op.doc-num                                                   +
            " на сумму "   +
               left-trim(string(op-entry.amt-rub,">>>,>>>,>>>,>>>,>>9.99")) +
            " удален. "    +
            "Причина удаления: " + buf.

      DO reject-cnt = 1 TO 9999:
         filename = TRIM(mail-user.file-exp) + string(reject-cnt,"9999").
         file-info:FILE-NAME = filename.
         IF file-info:file-type EQ ?
            THEN LEAVE.
      END.

      OUTPUT TO VALUE(filename).
      PUT UNFORMATTED "#NEW MESSAGE" SKIP.
      RUN e-dias-t.p (":ID      :", string(op.op, "9999999999")                  ).
      RUN e-dias-t.p (":1       :", string(today, "99/99/9999")                  ).
      RUN e-dias-t.p (":2       :", string(time,  "HH:MM:SS")                    ).
      RUN e-dias-t.p (":3       :", "Удаление документа#" +
                                     string(op-impexp.op-reference,"9999999999") ).
      RUN e-dias-t.p (":4       :", _user._user-name                             ).
      RUN e-dias-t.p (":Note1   :", SUBSTRING(buf,  1,200)                       ).
      RUN e-dias-t.p (":Note2   :", SUBSTRING(buf,201,200)                       ).
      RUN e-dias-t.p (":Note3   :", SUBSTRING(buf,401,200)                       ).
      PUT UNFORMATTED ":END     :" SKIP.
      OUTPUT CLOSE.
   END.
   ELSE
      MESSAGE "Не задан каталог для экспорта файлов" VIEW-AS ALERT-BOX.

END.
ELSE IF  (AVAIL mail-user AND (mail-user.mail-format EQ "XMLBSS_R"    OR 
        mail-user.mail-format EQ "XMLDboBSS_R")) OR 
        fl_DBO_opkind
THEN
DO:
   FIND FIRST op-entry OF op NO-LOCK NO-ERROR.
   IF NOT AVAIL op-entry THEN
      RETURN.

   DEFINE VARIABLE vres AS LOGICAL     NO-UNDO.

   TDAddParam("in-op-entry", STRING(op-entry.op) + "," + STRING (op-entry.op-entry)).
   TDAddParam("in-msg", buf).
   TDAddParam("in-op-tr", STRING (op.op-transaction)).
   RUN ex-trans.p (IF fl_DBO_opkind THEN "_findbod" ELSE "_findeny",
                   gEnd-Date,
                   TABLE tOpKindParams,
                   OUTPUT mOK,
                   OUTPUT mResMsg) NO-ERROR.
END.
ELSE DO:
  IF mail-user.file-exp NE "" THEN
  DO:
     filename = filename  + ".txt".
     msg = "".
     OUTPUT TO VALUE(mail-user.file-exp + "/" + filename).
     PUT UNFORMATTED "                     ФИНАНСОВЫЙ ОТКАЗ." SKIP (1).
     PUT UNFORMATTED
        "Документ N  : " op.doc-num "               Банк получателя : "
           op-bank.bank-code SKIP
        "Документ от : " op.doc-date "           Кор.счет        : "
           op-bank.corr-acct SKIP
        "Сумма       : " op-entry.amt-rub FORMAT ">>>,>>>,>>>,>>9.99" SKIP
        "Назнач.плат.: " det[1] SKIP
        "              " det[2] SKIP
        "              " det[3] SKIP(1).

     DO i = 1 TO NUM-ENTRIES(buf,chr(10)):
        PUT UNFORMATTED ENTRY(i,buf,chr(10)) SKIP.
     END.
     OUTPUT CLOSE.
   END.
END.
/*************************************************************************************************/

{intrface.del}          /* Выгрузка инструментария. */ 

