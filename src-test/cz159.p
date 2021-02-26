{globals.i}
{intrface.get pbase}
{intrface.get instrum}

DEFINE VARIABLE mBlVal    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mBlSum    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mBlSumRub AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMailRec  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCommand AS CHARACTER NO-UNDO.

DEFINE BUFFER acct        FOR acct.
DEFINE BUFFER BlockObject FOR BlockObject.

DEFINE TEMP-TABLE tt-rid NO-UNDO
   FIELD rid AS INT64
   FIELD val AS CHARACTER
   FIELD amt AS DECIMAL
   FIELD act AS CHARACTER.

mFileName = "ratblock-" + 
   STRING(YEAR(TODAY),"9999") + 
   STRING(MONTH(TODAY),"99") + 
   STRING(DAY(TODAY),"99") + "-" +
   TRIM(STRING(TIME,"hh:mm:ss")) + ".txt".
mFileName = REPLACE(mFileName,":","").
mMailRec  = "s.strahov@bankom.omsk.su y.tsisnevich@bankom.omsk.su v.mukhina@bankom.omsk.su n.narhova@bankom.omsk.su o.nosova@bankom.omsk.su s.sharukha@bankom.omsk.su O.Livinetc@plus-bank.ru ny.vasilyeva@plus-bank.ru".
/*mMailRec  = "s.strahov@bankom.omsk.su".*/

OUTPUT TO VALUE(mFileName).

PUT UNFORMATTED CODEPAGE-CONVERT("Начало    переоценки блокировок:","1251") + CHR(13) SKIP.

/*PUT UNFORMATTED "Начало    переоценки блокировок:" + CHR(13) SKIP.*/

FOR EACH BlockObject WHERE
       BlockObject.class-code   EQ "BlockAcct"
   AND BlockObject.file-name    EQ "acct"
   AND BlockObject.end-datetime EQ ?
   AND BlockObject.txt[1]       EQ ""
   AND BlockObject.txt[3]      BEGINS "Арест"
/*     OR BlockObject.txt[3]      BEGINS "Взыскание")*/
   AND BlockObject.beg-datetime LE NOW
   NO-LOCK,
   EACH acct WHERE
       acct.acct       EQ ENTRY(1,BlockObject.surrogate)
   AND acct.currency   EQ ENTRY(2,BlockObject.surrogate)
   AND acct.close-date EQ ?
   NO-LOCK:      

   IF acct.currency NE "" THEN
   DO:

      IF DEC(BlockObject.val[4]) NE 0 THEN 
      DO:
                  
         CREATE tt-rid.
         ASSIGN 
            tt-rid.rid = RECID(BlockObject)
            tt-rid.val = acct.currency
            tt-rid.amt = ROUND(DEC(BlockObject.val[4]) / FindRate("УЧЕТНЫЙ",acct.currency,TODAY),2)
            tt-rid.act = acct.acct.
      END.
   END.
   ELSE
   DO:
	   mBlSum    = DEC(GetXAttrValueEx("BlockObject",STRING(BlockObject.BlockObjectID),"BlSum","")).
	   mBlVal    = GetXAttrValueEx("BlockObject",STRING(BlockObject.BlockObjectID),"BlVal","").      
	   mBlSumRub = DEC(GetXAttrValueEx("BlockObject",STRING(BlockObject.BlockObjectID),"BlSumRub","")).
	   IF mBlSum NE 0 THEN
      DO:
         CREATE tt-rid.
         ASSIGN
            tt-rid.rid = RECID(BlockObject)
            tt-rid.val = ""
            tt-rid.amt = - 1 * ROUND(mBlSum * FindRate("УЧЕТНЫЙ",mBlVal,TODAY) + mBlSumRub,2)
            tt-rid.act = acct.acct.
      END.
   END.
END.

FOR EACH tt-rid NO-LOCK:
   FIND FIRST BlockObject WHERE
      RECID(BlockObject) EQ tt-rid.rid
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL(BlockObject) THEN
   DO:
      IF BlockObject.val[3] NE tt-rid.amt THEN
      DO:
         PUT UNFORMATTED CODEPAGE-CONVERT(" Переоценка Блокировок" +
                        "; RECID(BlockObject) = " + STRING(tt-rid.rid) +
                        "; acct = " + tt-rid.act +
                        "; было = " + STRING(BlockObject.val[3]) +
                        "; стало = " + STRING(tt-rid.amt),"1251") + CHR(13) SKIP.

/*         PUT UNFORMATTED " Переоценка Блокировок" +                      */
/*                        "; RECID(BlockObject) = " + STRING(tt-rid.rid) + */
/*                        "; acct = " + tt-rid.act +                       */
/*                        "; было = " + STRING(BlockObject.val[3]) +       */
/*                        "; стало = " + STRING(tt-rid.amt) + CHR(13) SKIP.*/

         IF tt-rid.val EQ "" THEN
            ASSIGN
               BlockObject.val[3] = tt-rid.amt
               BlockObject.val[4] = tt-rid.amt.
         ELSE
            ASSIGN
               BlockObject.val[3] = tt-rid.amt.
      END.
      ELSE
      DO:
         PUT UNFORMATTED CODEPAGE-CONVERT(" Переоценка Блокировок" +
                        "; RECID(BlockObject) = " + STRING(tt-rid.rid) +
                        "; acct = " + tt-rid.act +
                        "; было = " + STRING(BlockObject.val[3]) +
                        "; стало = " + STRING(tt-rid.amt),"1251") + CHR(13) SKIP.
         
      END.
   END.
END.

PUT UNFORMATTED CODEPAGE-CONVERT("Окончание переоценки блокировок.","1251") + CHR(13) SKIP.

OUTPUT CLOSE.

mCommand = './ratblock-mail ' + SEARCH(mFileName) + ' "' + mMailRec + '"'.

OS-COMMAND SILENT VALUE(mCommand).

/*RUN pb_mail.p (mMailRec,"Переоценка блокировок","",SEARCH(mFileName)).                          */
/*RUN pb_mail.p (mMailRec,CODEPAGE-CONVERT("Переоценка блокировок","1251"),"",SEARCH(mFileName)). */
/*RUN pb_mail.p (mMailRec,CODEPAGE-CONVERT("Переоценка блокировок","UTF-8"),"",SEARCH(mFileName)).*/

{intrface.del}

RETURN.

