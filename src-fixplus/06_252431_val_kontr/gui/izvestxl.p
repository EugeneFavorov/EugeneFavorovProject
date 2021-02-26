{globals.i}
{intrface.get tmess}

/* +++ izvestxl.p was humbly modified by (c)blodd converter v.1.09 on 3/19/2015 11:54am +++ */

/*
     Filename: izvestxl.p
      Comment: Отчет по поступлениям на транзитные счета и поступлениям рублей от нерезидентов 
   Parameters:
         Uses:
      Used by:
      Created: kuds 10/03/15

*/
{globals.i}

{intrface.get cust}      /* Библиотека для работы с клиентами. */
{intrface.get count}     /* Библиотека для работы с клиентами. */
{intrface.get xclass}

{prn-doc.def  &with_proc=YES}

DEF VAR mName     AS CHAR    EXTENT 3    NO-UNDO. /* Наименование клиента            */

{getdates.i}

{justasec}

RUN Insert_TTName("enddate","'" + STRING(end-date,"99.99.9999")).
RUN Insert_TTName("begdate","'" + STRING(beg-date,"99.99.9999")).

RUN BeginCircle_TTName("izvest").
FOR EACH op-entry WHERE op-entry.op-date   GE beg-date
                    AND op-entry.op-date   LE end-date
                  NO-LOCK,
   FIRST op OF op-entry NO-LOCK
   BREAK BY op.op:

   IF GetXAttrValueEx("op",STRING(op.op),"izvest","") EQ "" THEN
      NEXT.

   FIND FIRST acct WHERE acct.acct EQ  op-entry.acct-cr NO-LOCK NO-ERROR.
   IF AVAIL acct THEN 
   DO:
      RUN GetCustName IN h_base(acct.cust-cat,
                                acct.cust-id,
                                ?,
                                OUTPUT mName[1],
                                OUTPUT mName[2],
                                INPUT-OUTPUT mName[3]).
      FIND FIRST currency
           WHERE currency.currency EQ acct.currency NO-LOCK NO-ERROR.
   END.

   RUN Insert_TTName("dateuved[izvest]     " , GetXAttrValueEx("op",STRING(op.op),"ДатаУвед","")).
   RUN Insert_TTName("dateopentry[izvest]  " , "'" + STRING(Op-entry.op-date,"99.99.9999") ).
   RUN Insert_TTName("docnum[izvest]       " , op.doc-num ).
   RUN Insert_TTName("currency[izvest]     " , IF AVAIL currency THEN currency.i-currency ELSE acct.currency ).
   RUN Insert_TTName("sum[izvest]          " , TRIM(STRING(IF op-entry.currency EQ "" THEN op-entry.amt-rub ELSE op-entry.amt-cur,"->>,>>>,>>>,>>>,>>9.99")) ).
   RUN Insert_TTName("acct[izvest]         " , "'" + DelFilFromAcct(op-entry.acct-cr) ).
   RUN Insert_TTName("name[izvest]         " , TRIM(TRIM(mName[1]) + " " + TRIM(mName[2])) ).
   RUN Insert_TTName("details[izvest]      " , REPLACE(Op.details,CHR(10),"") ).

   RUN NextCircle_TTName("izvest").
END. 

RUN EndCircle_TTName("izvest").

RUN printvd.p("izvestxl",INPUT TABLE ttnames). 

{intrface.del} 
/* --- izvestxl.p was humbly modified by (c)blodd converter v.1.09 on 3/19/2015 11:54am --- */
