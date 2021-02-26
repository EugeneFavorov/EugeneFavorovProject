/*
                ญชฎขแช ๏ จญโฅฃเจเฎข ญญ ๏ แจแโฅฌ  ‘ชขจโ
    Copyright: (C) 1992-2002 ’ " ญชฎขแชจฅ จญไฎเฌ ๆจฎญญ๋ฅ แจแโฅฌ๋"
     Filename: CRD2DOC.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: ????
     Modified: 06.06.2006 15:38 ILVI     <comment>
     Modified: 16/10/2002 kraw (0008664) - ฏฅเฅขฅคฅญฎ ญ  คจญ ฌจ็ฅแชจฅ ไจซ์โเ๋
*/


{globals.i}
{sh-defs.i}
{tmprecid.def}
{intrface.get crd}   /* จกซจฎโฅช  จญแโเใฌฅญโฎข เ กฎโ๋ แ ช เโฎโฅชฎฉ */

DEFINE INPUT PARAMETER in-kau-id AS CHARACTER NO-UNDO.

DEFINE VAR summ-post    LIKE op-entry.amt-rub FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR summ-ost     LIKE op-entry.amt-rub FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR in-op-date   AS DATE      NO-UNDO.
DEFINE VAR tmp_char     AS CHARACTER NO-UNDO.
DEFINE VAR zero-val     AS LOGICAL INIT YES VIEW-AS TOGGLE-BOX   NO-UNDO.

DEFINE VAR frm-doc-num  AS CHARACTER FORMAT "x(7)"               NO-UNDO.
DEFINE VAR frm-doc-date AS DATE      FORMAT "99/99/9999"         NO-UNDO.
DEFINE VAR frm-acct     AS CHARACTER FORMAT "x(20)"              NO-UNDO.
DEFINE VAR frm-amt      AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR frm-balance  AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VAR frm-bank     AS CHARACTER FORMAT "x(11)"              NO-UNDO.

DEFINE BUFFER xop        FOR op.
DEFINE BUFFER xop-bank   FOR op-bank.
DEFINE STREAM out-doc.

DEFINE TEMP-TABLE ttFrm NO-UNDO
    FIELD f-doc-num  AS CHARACTER
    FIELD f-doc-date AS DATE  
    FIELD f-acct     AS CHARACTER
    FIELD f-amt      AS DECIMAL   
    FIELD f-balance  AS DECIMAL 
    FIELD f-bank     AS CHARACTER 
. 

PAUSE 0.
in-op-date = gend-date.
UPDATE zero-val   LABEL "‘—’ “…… ‘’€’"
       in-op-date LABEL "€’€ €‘—…’€"
WITH FRAME enter-cond
     WIDTH 40
     SIDE-LABELS
     CENTERED
     ROW 10
     TITLE "[  ็ ซ์ญ๋ฅ ใแซฎขจ๏ ]"
     OVERLAY
     EDITING:
       READKEY.
       IF LASTKEY = 301 THEN DO:
          RUN calend.p.
          IF (LASTKEY = 13 OR LASTKEY = 10) AND pick-value <> ? THEN FRAME-VALUE = pick-value.
       END.
       ELSE
          APPLY LASTKEY.
END.
HIDE FRAME enter-cond.

{setdest.i &stream="STREAM out-doc" &cols=92}
IF AVAIL dept
   THEN PUT STREAM out-doc UNFORMATTED CAPS(dept.name-bank) SKIP(2).

FIND FIRST bal-acct WHERE bal-acct.kau-id EQ in-kau-id NO-LOCK NO-ERROR.

IF AVAIL bal-acct 
THEN 
DO TRANSACTION:
   RUN browseld.p ("acct",
                   "bal-acct"                + CHR(1) + "acct-cat" + CHR(1) + "ridrest",
                   STRING(bal-acct.bal-acct) + CHR(1) + "o"        + CHR(1) + "yes",
                   "bal-acct"                + CHR(1) + "acct-cat",
                   4).
   IF NOT CAN-FIND(FIRST tmprecid) THEN DO:
      IF  pick-value NE ""
      AND pick-value NE ? THEN DO:
         FIND acct WHERE acct.acct     EQ ENTRY(1,pick-value)
                     AND acct.currency EQ ENTRY(2,pick-value) NO-LOCK NO-ERROR.
         CREATE tmprecid.
         tmprecid.id = recid(acct).
      END.
   END.
END.

ELSE
IF CAN-FIND ( FIRST acct NO-LOCK
              WHERE acct.kau-id   EQ in-kau-id 
                AND acct.acct-cat EQ "o" ) THEN
DO TRANSACTION:
   RUN browseld.p ("acct",
                   "kau-id"   + CHR(1) + "acct-cat" + CHR(1) + "ridrest",
                   in-kau-id  + CHR(1) + "o"        + CHR(1) + "yes",
                   "bal-acct" + CHR(1) + "acct-cat",
                   4). 
   IF NOT CAN-FIND(FIRST tmprecid) THEN DO:
      IF  pick-value NE ""
      AND pick-value NE ? THEN DO:
         FIND acct WHERE acct.acct     EQ ENTRY(1,pick-value)
                     AND acct.currency EQ ENTRY(2,pick-value) NO-LOCK NO-ERROR.
         CREATE tmprecid.
         tmprecid.id = recid(acct).
      END.
   END.
END.

ELSE RETURN.

FOR EACH tmprecid,
    FIRST acct NO-LOCK
    WHERE RECID(acct) EQ tmprecid.id 
    BY acct.acct :

   ASSIGN
      summ-post = 0
      summ-ost  = 0
      frm-amt   = 0
   .

   {empty ttFrm}
   FOR EACH kau WHERE kau.acct     EQ acct.acct
                  AND kau.currency EQ acct.currency NO-LOCK,
      FIRST op-entry WHERE op-entry.op       EQ INT64(ENTRY(1,kau.kau))
                       AND op-entry.op-entry EQ INT64(ENTRY(2,kau.kau))
                       AND op-entry.op-date  LE in-op-date
      NO-LOCK BREAK BY kau.currency BY kau.sort:
      IF (    kau.currency EQ ""
          AND kau.balance  EQ 0
          AND zero-val)
      OR (    kau.currency NE ""
          AND kau.curr-bal EQ 0
          AND zero-val)
         THEN NEXT.
      FIND op OF op-entry NO-LOCK NO-ERROR.
      tmp_char = GetXattrValue("op",STRING(op-entry.op),"op-bal").
      IF tmp_char = ? THEN DO:
         FIND FIRST xop WHERE xop.op-transaction EQ op.op-transaction
                          AND xop.acct-cat       EQ "b"
         NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST xop WHERE xop.op EQ INT64(tmp_char)
            NO-LOCK NO-ERROR.
      END.
      IF AVAIL xop
         THEN FIND FIRST xop-bank OF xop NO-LOCK NO-ERROR.
      ASSIGN
         summ-post = summ-post + IF kau.currency EQ "" THEN op-entry.amt-rub
                                                       ELSE op-entry.amt-cur
         summ-ost  = summ-ost  + IF kau.currency EQ "" THEN kau.balance
                                                       ELSE kau.curr-bal
      .
      RUN COUNT-KAU(RECID(kau), in-op-date, OUTPUT frm-balance).
      IF AVAIL xop
         THEN frm-acct = GetXAttrValueEx("op",STRING(xop.op),"acctbal", "").
         ELSE frm-acct = "".
      ASSIGN
         frm-doc-num  = IF AVAIL xop THEN xop.doc-num
                                     ELSE (IF AVAIL op THEN op.doc-num
                                                       ELSE "")
         frm-doc-date = IF AVAIL op THEN op.op-date
                                    ELSE ?
         frm-amt      = IF acct.currency EQ "" THEN op-entry.amt-rub
                                               ELSE op-entry.amt-cur
         frm-bank     = IF AVAIL xop-bank THEN xop-bank.bank-code
                                          ELSE "-"
      .

      CREATE ttFrm.
      ASSIGN ttFrm.f-doc-num  = frm-doc-num
             ttFrm.f-doc-date = frm-doc-date
             ttFrm.f-acct     = frm-acct
             ttFrm.f-amt      = frm-amt
             ttFrm.f-balance  = frm-balance
             ttFrm.f-bank     = frm-bank
      .
   END.

   IF CAN-FIND(FIRST ttFrm) THEN
   DO:
       PUT STREAM out-doc UNFORMATTED "~n" "~n"
       SPACE(36) "ฅเฅ็ฅญ์ คฎชใฌฅญโฎข"                             SKIP
       SPACE(24) "ฏฎ ขญฅก ซ ญแฎขฎฌใ แ็ฅโใ " ENTRY(1,acct.acct,"@") SKIP(2)
       SPACE(36) {strdate.i in-op-date}                            SKIP
       .

       RUN PRINT-HEADER.
       FOR EACH ttFrm :
           ASSIGN frm-doc-num  = ttFrm.f-doc-num   
                  frm-doc-date = ttFrm.f-doc-date  
                  frm-acct     = ttFrm.f-acct      
                  frm-amt      = ttFrm.f-amt       
                  frm-balance  = ttFrm.f-balance   
                  frm-bank     = ttFrm.f-bank 
           .
           RUN PRINT-BODY.
       END.
       RUN PRINT-FOOTER.
   END.
END.

{signatur.i &department = branch &stream="STREAM out-doc"
            &user-only  ="YES"
}

{preview.i  &stream="stream out-doc"}
{intrface.del}          /* ๋ฃเใงช  จญแโเใฌฅญโ เจ๏. */ 

FOR EACH tmprecid:
  DELETE tmprecid.
END.

PROCEDURE PRINT-HEADER:
   PUT STREAM out-doc UNFORMATTED
      "ีอออออออัออออออออออัออออออออออออออออออออัออออออออออออออออออัออออออออออออออออออัอออออออออออธ" SKIP
      "ณ ฎฌฅเ ณ    โ    ณ       จๆฅขฎฉ      ณ       ‘ใฌฌ       ณ      แโ โฎช     ณ ฎซใ็ โฅซ์ณ" SKIP
      "ณ คฎชใฌ ณ คฎชใฌฅญโ ณ                    ณ                  ณ                  ณ           ณ" SKIP
      "ฦอออออออุออออออออออุออออออออออออออออออออุออออออออออออออออออุออออออออออออออออออุอออออออออออต" SKIP
   .
END PROCEDURE.
PROCEDURE PRINT-BODY:
   PUT STREAM out-doc
       "ณ" frm-doc-num
       "ณ" frm-doc-date
       "ณ" frm-acct
       "ณ" frm-amt
       "ณ" frm-balance
       "ณ" frm-bank     "ณ" SKIP
   .
END PROCEDURE.
PROCEDURE PRINT-FOOTER:
   PUT STREAM out-doc
      "ฦอออออออฯออออออออออฯออออออออออออออออออออฯออออออออออออออออออุออออออออออออออออออุอออออออออออต" SKIP
      "ณ                                  ’:" summ-post      "ณ" summ-ost       "ณ           ณ" SKIP
      "ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฯออออออออออออออออออฯอออออออออออพ" SKIP
   .
END PROCEDURE.
PROCEDURE COUNT-KAU:
DEF INPUT  PARAMETER in-recid   AS RECID     NO-UNDO.
DEF INPUT  PARAMETER in-op-date AS DATE      NO-UNDO.
DEF OUTPUT PARAMETER xResult    AS DECIMAL   NO-UNDO.
DEF VAR    in-op-status         AS CHARACTER NO-UNDO.
DEF BUFFER buf-kau       FOR kau.
DEF BUFFER buf-code      FOR code.
DEF BUFFER buf1-op-entry FOR op-entry.
DEF BUFFER buf2-op-entry FOR op-entry.

   FIND buf-kau WHERE RECID(buf-kau) EQ in-recid NO-LOCK NO-ERROR.
   FIND buf-code WHERE buf-code.class EQ " กซ ใ"
                   AND buf-code.code  EQ buf-kau.kau-id NO-LOCK NO-ERROR.
   in-op-status = IF AVAIL buf-code AND NUM-ENTRIES(buf-code.misc[4]) > 1
                  THEN ENTRY(2,buf-code.misc[4])
                  ELSE gop-status.
   FIND buf1-op-entry WHERE buf1-op-entry.op       EQ INT64(ENTRY(1,buf-kau.kau))
                        AND buf1-op-entry.op-entry EQ INT64(ENTRY(2,buf-kau.kau))
                                                               NO-LOCK NO-ERROR.
   xResult = IF buf-kau.currency EQ "" THEN buf1-op-entry.amt-rub
                                       ELSE buf1-op-entry.amt-cur.
   FOR EACH buf2-op-entry WHERE buf2-op-entry.acct-cr   EQ buf-kau.acct
                            AND buf2-op-entry.op-date   GE buf1-op-entry.op-date
                            AND (IF in-op-date NE ? THEN buf2-op-entry.op-date LE in-op-date
                                                    ELSE TRUE)
                            AND buf2-op-entry.op-status GE in-op-status
                            AND buf2-op-entry.currency  EQ buf-kau.currency
                            AND buf2-op-entry.kau-cr    EQ buf1-op-entry.kau-db
                                                                        NO-LOCK:
      xResult = xResult - IF buf-kau.currency EQ "" THEN buf2-op-entry.amt-rub
                                                    ELSE buf2-op-entry.amt-cur.
   END.
END PROCEDURE.
