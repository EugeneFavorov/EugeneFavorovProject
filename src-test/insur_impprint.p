/*
pda

*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get xclass}
{prn-doc.def &with_proc=YES}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan1 FOR loan.

DEFINE TEMP-TABLE otch
   FIELD cont_code  AS CHAR    /* номер КД */
   FIELD fio        AS CHAR    /* ФИО заемщика */
   FIELD open_date  AS DATE    /* дата открытия договора */
   FIELD strahbik   AS CHAR    /*  */
   FIELD strah      AS CHAR    /*  */
   FIELD ind        AS CHAR    /* индекс заемщика */
   FIELD adrstrah   AS CHAR    /* адрес страховой организации */
   FIELD dogstrah   AS CHAR    /* номер договора страхования */
   INDEX cont_code cont_code   
.

{empty otch}

def var mDogStrah as char no-undo.
def var nameCl    as char no-undo.
def var telCl     as char no-undo.
def var mStrah    as char no-undo.
def var adrStrah  as char no-undo.
def var adr_strah as char no-undo.
def var vRekv     as char no-undo.
DEF VAR indStrah  AS CHAR NO-UNDO.
def var open_date as date no-undo.
DEF VAR mK        AS INT64 INIT 0 NO-UNDO. /* счетчик договоров */
DEF VAR mI        AS INT64        NO-UNDO. /* перебор индекса */
DEF VAR mDecimal  AS DECIMAL      NO-UNDO.

DEF VAR fname   AS CHAR NO-UNDO.
DEF VAR mString AS CHAR NO-UNDO.
DEF VAR fstr    AS CHAR INIT '' NO-UNDO.
DEF NEW SHARED STREAM vvs.
 
 /* {getdate.i} */

INPUT FROM VALUE("SKsoyuz.csv") CONVERT  TARGET "IBM866"  SOURCE "1251".
REPEAT:
   IMPORT UNFORMATTED mString.
   IF mString NE "end" THEN 
   DO:
      CREATE otch.
      ASSIGN
         otch.fio       = TRIM(ENTRY(4,mString,";"))
         otch.cont_code = TRIM(ENTRY(5,mString,";"))
         otch.strahbik  = TRIM(ENTRY(6,mString,";"))
         otch.strah     = REPLACE(TRIM(ENTRY(7,mString,";")),'""','"')
         otch.ind       = TRIM(ENTRY(8 ,mString,";"))
         otch.open_date = ?
         otch.adrstrah  = TRIM(ENTRY(15,mString,";"))
         /* otch.dogstrah  = TRIM(ENTRY(18,mString,";")) */
      .
   END.
END.
INPUT CLOSE.
/* leave. */
    
/* {setdest.i &file-name = "111.log"}
FOR EACH otch NO-LOCK
BREAK BY otch.adrstrah:
  PUT UNFORMATTED otch.cont_code '; ' otch.fio '; ' otch.strah '; ' otch.adrstrah SKIP.
END.
{preview.i &file-name = "111.log"} */


RUN BeginCircle_TTName("client").
FOR EACH otch NO-LOCK 
BREAK BY otch.adrstrah
      BY otch.fio:
   mK = mK + 1.
   IF FIRST-OF(otch.adrstrah) THEN 
   DO:
      RUN Insert_TTName("strah",REPLACE(otch.strah,'""','"')).
      RUN Insert_TTName("indadr",otch.ind + ', ' + otch.adrstrah).
   END.
      
   RUN Insert_TTName("num[client]",mK).
   RUN Insert_TTName("fio[client]",otch.fio).
   RUN Insert_TTName("rekv[client]",otch.cont_code).
   RUN NextCircle_TTName("client").

   IF LAST-OF(otch.adrstrah) THEN 
   DO:
      RUN EndCircle_TTName("client").
      RUN printvd.p ("outinsur", INPUT TABLE ttnames).
      mK = 0.
      {empty ttCircles}
      {empty ttNames}
      PAUSE(2).
      /* MESSAGE  getSysConf("printvd_file") VIEW-AS ALERT-BOX. */
   END.

END.