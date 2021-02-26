{globals.i}
{intrface.get pbase}
{parsin.def}
{sh-defs.i}

{tmprecid.def}

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.

ETIME(YES).

{setdest.i &file-name = "111.log"}

mInt = 0.
FOR EACH acct WHERE
       acct.filial-id  EQ shFilial
   AND acct.currency   NE ""
   AND acct.close-date EQ ?
   AND acct.rate-type  EQ ""
   NO-LOCK:

   mInt = mInt + 1.
      
   RUN acct-pos IN h_base (acct.acct,
                           acct.currency,
                           TODAY,
                           TODAY,
                           "П").
                           
   IF sh-val NE 0 THEN
      PUT UNFORMATTED mInt ";" acct.acct ";" sh-val "; Не заполнен Тип курса"  SKIP.
END.

IF mInt = 0 THEN
   MESSAGE "Филиал " + shFilial + " - всё OK."
   VIEW-AS ALERT-BOX.
ELSE
DO:
   {preview.i &file-name = "111.log"}
END.
{intrface.del}

RETURN.
