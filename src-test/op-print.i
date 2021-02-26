/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1997 ТОО "Банковские информационные системы"
     Filename: OP-PRINT.I
      Comment:
   Parameters:
         Uses:
      Used by: op-print.p
      Created: 06/07/1997 Serge
     Modified: 06/07/1997 Serge
     Modified: 31/10/2001 NIK
     Modified: 28/01/2010 kraw (0123155) Совместная печать "графики" и "текста"
     Modified: 31/03/2011 kraw (0145601) user-proc-id от процедуры печати документа
     Modified: 04/04/2011 kraw (0144622) PAGE-SIZE 10000 --> 10000000
     Modified: 14/02/2014 kraw (0215709)  Возможность печати без прогона страницы
*/

DEFINE BUFFER b-op FOR op.
DEFINE VARIABLE mOpBal AS CHARACTER NO-UNDO.

detail[1] = op.detail.
{wordwrap.i &s=detail &l=45 &n=4}

CASE set-target:
   WHEN 1 THEN
   DO:
      DOWN.
      DISPLAY
         op.op-date
         op.op-status
         op.doc-type
         op.doc-num
         op.doc-date
         detail[1]     LABEL "СОДЕРЖАНИЕ ОПЕРАЦИИ"
         op.user-id
         op.user-inspector WITH stream-io.

      DO idet = 2 TO 4 while detail[idet] NE "":
         DOWN.
         DISPLAY detail[idet] @ detail[1].
      END.
   END.
   WHEN 2 THEN
   DO:

      IF {assigned mProfile} THEN
      DO:
         FIND FIRST tt-doctypes WHERE tt-doctypes.code EQ op.doc-type
            NO-ERROR.

         IF AVAILABLE tt-doctypes THEN
         DO:
            IF     SEARCH(tt-doctypes.PROC + ".r") EQ ?
               AND SEARCH(tt-doctypes.PROC + ".p") EQ ? THEN
               MESSAGE COLOR MESSAGE "Процедура" tt-doctypes.PROC + ".p" SKIP
                                     "не обнаружена."
                  VIEW-AS ALERT-BOX.
            ELSE
            DO:

               DO mItem = 1 TO tt-doctypes.quan:

                  RUN CleanTT IN h_print(?).

                  IF isRollPrinter("") THEN
                     OUTPUT TO _txt__spool.tmp PAGE-SIZE 0.
                  ELSE
                     OUTPUT TO _txt__spool.tmp PAGED PAGE-SIZE 10000000.

                  mStrTMP = GetSysConf("user-proc-id").

                  FIND FIRST user-proc WHERE user-proc.PROCEDURE EQ tt-doctypes.PROC NO-LOCK NO-ERROR.
                  IF AVAILABLE user-proc THEN
                     RUN SetSysConf IN h_base("user-proc-id", STRING(RECID(user-proc))).
                  RUN VALUE(tt-doctypes.PROC + ".p") (RECID(op)) NO-ERROR.
                  RUN SetSysConf IN h_base("user-proc-id", mStrTMP).
                  OUTPUT CLOSE.

                  IF ERROR-STATUS:ERROR THEN
                     MESSAGE "В процедуре печати" tt-doctypes.PROC + ".p" SKIP
                             "произошла ошибка."
                         VIEW-AS ALERT-BOX.
                  RUN op_print_print.
               END.
            END.
         END.
      END.
      ELSE
      DO:
      
         &IF DEFINED(print-proc-field) = 0 &THEN
            &SCOPED-DEFINE print-proc-field printout
         &ENDIF
         
         FIND FIRST doc-type OF op WHERE doc-type.{&print-proc-field} NE ""
            NO-LOCK NO-ERROR.

         IF AVAILABLE doc-type THEN
         DO:
            IF     SEARCH(doc-type.{&print-proc-field} + ".r") EQ ?
               AND SEARCH(doc-type.{&print-proc-field} + ".p") EQ ? THEN
               MESSAGE COLOR MESSAGE                             SKIP
                            "Процедура" doc-type.{&print-proc-field} + ".p" SKIP
                            "не обнаружена."
                  VIEW-AS ALERT-BOX.
            ELSE
            DO:
               RUN CleanTT IN h_print(?).

               IF isRollPrinter("") THEN
                  OUTPUT TO _txt__spool.tmp PAGE-SIZE 0.
               ELSE
                  OUTPUT TO _txt__spool.tmp PAGED PAGE-SIZE 10000000.

               mStrTMP = GetSysConf("user-proc-id").

/*               FIND FIRST user-proc WHERE user-proc.PROCEDURE EQ doc-type.{&print-proc-field} NO-LOCK NO-ERROR.*/
               
               FIND FIRST user-proc WHERE user-proc.PROCEDURE EQ "bankord_ops" NO-LOCK NO-ERROR.
               
               IF AVAILABLE user-proc THEN
                  RUN SetSysConf IN h_base("user-proc-id", STRING(RECID(user-proc))).
                
               &IF DEFINED(print-op-bal) NE 0 &THEN
                  IF AVAIL(op) THEN
                  DO:
                     mOpBal = GetXAttrValueEx("op",STRING(op.op), "op-bal", "").
                     IF mOpBal NE "" THEN
                        FIND FIRST b-op WHERE b-op.op EQ INT64(mOpBal) NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE(b-op) THEN
                        FIND FIRST b-op WHERE b-op.op-transaction EQ op.op-transaction
                                          AND b-op.acct-cat       EQ "b"
                                          AND b-op.op-status      BEGINS  "А"
                        NO-LOCK NO-ERROR.
               
                     IF AVAILABLE(b-op) THEN
                        RUN VALUE(doc-type.{&print-proc-field} + ".p") (RECID(b-op)) NO-ERROR.
                     ELSE
                        RUN VALUE(doc-type.{&print-proc-field} + ".p") (RECID(op)) NO-ERROR.
                  END.
               &ELSE
                  RUN VALUE(doc-type.{&print-proc-field} + ".p") (RECID(op)) NO-ERROR.
               &ENDIF

               RUN SetSysConf IN h_base("user-proc-id", mStrTMP).
               OUTPUT CLOSE.

               IF ERROR-STATUS:ERROR THEN
                  MESSAGE "В процедуре печати" doc-type.{&print-proc-field} + ".p" SKIP
                          "произошла ошибка."
                      VIEW-AS ALERT-BOX.
               RUN op_print_print.
            END.
         END.
      END.
   END.
END.

