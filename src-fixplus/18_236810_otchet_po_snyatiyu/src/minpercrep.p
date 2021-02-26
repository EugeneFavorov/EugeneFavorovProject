/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: minpercrep.p
      Comment: minpercrep.p
   Parameters:
         Uses:
      Used by:
      Created: 10.08.2015 11:10 KSBIS TT:0236810 Миграция. отчет по проценту наличных снятий от общего оборота ЮЛ
     Modified: 
*/

{globals.i}
{intrface.get strng}
{intrface.get xclass}
{intrface.get cust}     /* Библиотека для работы с клиентами. */
{intrface.get tmess}   /* Служба системных сообщений */
{sh-defs.i}
{prn-doc.def &with_proc=YES}

{tmprecid.def}
{empty tmprecid}

DEFINE INPUT PARAMETER iParams AS CHARACTER.


DEFINE TEMP-TABLE tt-rep NO-UNDO
   FIELD cust-cat  LIKE cust-role.cust-cat
   FIELD cust-id   LIKE cust-corp.cust-id
   FIELD cust-name LIKE cust-role.cust-name
   FIELD dates     AS CHARACTER
   FIELD numct     AS INT64                 /* кол-во снятий */
   FIELD numdb     AS INT64                 /* кол-во поступлений */
   FIELD sumct     LIKE op-entry.amt-rub
   FIELD sumdb     LIKE op-entry.amt-rub
   FIELD minper    AS DECIMAL
.

DEFINE VARIABLE mPickRole AS CHARACTER   INIT "*"  
                             LABEL "Клиенты" FORMAT "x(50)" NO-UNDO.  
DEFINE VARIABLE mSumSort  AS CHARACTER   
                             FORMAT "x(20)" 
                             INIT "сумме снятий"
                             VIEW-AS COMBO-BOX  LIST-ITEMS 'сумме снятий','сумме поступлений' 
                             LABEL 'Сортировать по' NO-UNDO.
DEFINE VARIABLE mMinProc  AS DEC
                             FORMAT "999.99" 
                             LABEL 'Мин. % снятия' NO-UNDO.

DEFINE VARIABLE mTmpStr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mCount    AS INT64       NO-UNDO.
DEFINE VARIABLE mItogct   AS INT64       NO-UNDO.
DEFINE VARIABLE mItogdb   AS INT64       NO-UNDO.
DEFINE VARIABLE mItogsumct   AS DEC       NO-UNDO.
DEFINE VARIABLE mItogsumdb   AS DEC       NO-UNDO.
DEFINE VARIABLE mItogper   AS DEC       NO-UNDO.
DEFINE VARIABLE vQry      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vHQry     AS HANDLE      NO-UNDO.

DEFINE BUFFER b-tt-rep FOR tt-rep.

{getdates.i
   &TitleLabel     = "Данные для отчета"
   &dispBeforeDate = "mPickRole
                         HELP 'F1 - Выбор клиентов. * - По всем клиентам'
                     "
   &UpdBeforeDate  = "mPickRole"
   &AddLookUp      = "IF {&LAST_KEY} EQ KEY-CODE ('F1') AND {&FRAME_FIELD} EQ 'mPickRole' THEN
                         DO :
                            mPickRole = ''.
                            RUN browseld.p ('cust-corp','','','',3).
                        
                            FOR EACH tmprecid,
                               FIRST cust-corp WHERE RECID(cust-corp) = tmprecid.id NO-LOCK:
                               CREATE tt-rep.
                               ASSIGN 
                                  tt-rep.cust-cat = 'Ю'
                                  tt-rep.cust-id  = cust-corp.cust-id 
                                  tt-rep.cust-name = cust-corp.name-corp
                                  mPickRole = mPickRole + (IF mPickRole EQ '' THEN '' ELSE ',') + STRING(cust-corp.cust-id ).
                               .

                            END. /* FOR EACH tmprecid */
                        
                            RUN browseld.p ('person',
                                            'sc-1'        + CHR(1) +
                                            'sv-1',
                                            'Предпр'      + CHR(1) +
                                            'Предпр',
                                             '',
                                             3).

                            FOR EACH tmprecid,
                               FIRST person WHERE RECID(person) = tmprecid.id NO-LOCK:
                               CREATE tt-rep.
                               ASSIGN 
                                  tt-rep.cust-cat = 'Ч'
                                  tt-rep.cust-id  = person.person-id 
                                  tt-rep.cust-name = person.name-last + ' ' + person.first-names
                                  mPickRole = mPickRole + (IF mPickRole EQ '' THEN '' ELSE  ',') + STRING(person.person-id).
                               .

                            END. /* FOR EACH tmprecid */
                        

                            DISPLAY mPickRole @ mPickRole.

                         END. 
                         ELSE
                        "
   &BegLabel       = "Дата начала"
   &EndLabel       = "Дата окончания"
   &DispAfterDate  = "mSumSort mMinProc"
   &UpdAfterDate   = "mSumSort mMinProc"


}


IF AMBIGUOUS tt-rep THEN
   LEAVE.

{justasec}

RUN Clear_TTName.

FIND FIRST tt-rep.
RUN Insert_TTName("custcode",IF NUM-ENTRIES(mPickRole) EQ 1 THEN STRING(tt-rep.cust-id) ELSE "-").

/* Заполнение временной таблицы  */
IF NUM-ENTRIES(mPickRole) EQ 1 THEN
DO:

   FOR EACH acct WHERE acct.cust-cat  EQ tt-rep.cust-cat
                   AND acct.cust-id   EQ tt-rep.cust-id 
                   AND CAN-DO(iParams,acct.acct) 
                   AND acct.open-date LE end-date
                   AND (acct.close-date GT end-date 
                   OR   acct.close-date EQ ?
                        )
   NO-LOCK BREAK BY acct.acct :

     {minpercrep.i}
      
      mTmpStr = IF acct.open-date GT beg-date THEN STRING(acct.open-date,"99.99.99") ELSE STRING(beg-date,"99.99.9999").
      
      mTmpStr = mTmpStr + "-" + IF acct.close-date LT end-date THEN STRING(acct.close-date,"99.99.99") ELSE STRING(end-date,"99.99.99").
     
      ASSIGN
         tt-rep.cust-name = "'" + DelFilFromAcct(acct.acct)
         tt-rep.dates     = mTmpStr
         tt-rep.minper = IF tt-rep.sumdb GT 0 THEN ROUND((tt-rep.sumct / tt-rep.sumdb) * 100,2) ELSE 0
      .
      CREATE tt-rep.

   END.  /* FOR EACH acct */

   DELETE tt-rep.

END.
ELSE

FOR EACH tt-rep:

   FOR EACH acct WHERE acct.cust-cat  EQ tt-rep.cust-cat
                   AND acct.cust-id   EQ tt-rep.cust-id 
                   AND CAN-DO(iParams,acct.acct) 
                   AND acct.open-date LE end-date
                   AND (acct.close-date GT end-date 
                   OR   acct.close-date EQ ?
                        )
   NO-LOCK BREAK BY acct.acct :

       {minpercrep.i}


   END. /*FOR EACH acct*/
   ASSIGN
      tt-rep.minper = IF tt-rep.sumdb GT 0 THEN ROUND((tt-rep.sumct / tt-rep.sumdb) * 100,2) ELSE 0
      tt-rep.dates  = STRING(beg-date,"99.99.99") + "-" + STRING(end-date,"99.99.99")
   .

END.   /*FOR EACH tt-rep*/


RUN Insert_TTName("beg-date","'" + STRING(beg-date,"99.99.9999")).
RUN Insert_TTName("end-date","'" + STRING(end-date,"99.99.9999")).

RUN Insert_TTName("Procent",STRING(mMinProc)).

RUN BeginCircle_TTName("rep").

ASSIGN
   vQry = "~
   FOR EACH tt-rep WHERE "
   vQry = vQry + IF NUM-ENTRIES(mPickRole) GT 1 AND mMinProc GT 0 
                 THEN 
                      "tt-rep.minper GE " + QUOTER(mMinProc)
                 ELSE " "
   vQry = vQry + IF NUM-ENTRIES(mPickRole) GT 1 AND mSumSort EQ "сумме снятий"  THEN 
                 " BY tt-rep.sumct"
                  ELSE 
                 (
                  IF NUM-ENTRIES(mPickRole) GT 1 AND mSumSort EQ "сумме поступлений" THEN 
                  " BY tt-rep.sumdb"
                  ELSE
                  " BY tt-rep.cust-name"
                 )
.
CREATE QUERY vHQry.
vHQry:SET-BUFFERS(BUFFER tt-rep:HANDLE).
vHQry:QUERY-PREPARE(vQry).
vHQry:QUERY-OPEN().
BLCK:
REPEAT:
   vHQry:GET-NEXT().
   IF vHQry:QUERY-OFF-END THEN LEAVE.


   mItogct      =  mItogct     + tt-rep.numct.
   mItogdb      =  mItogdb     + tt-rep.numdb.
   mItogsumct   =  mItogsumct  + tt-rep.sumct.
   mItogsumdb   =  mItogsumdb  + tt-rep.sumdb.
   mItogper     =  ROUND((mItogsumct / mItogsumdb) * 100,2).



   mCount = mCount + 1.
   RUN Insert_TTName("NumRec[rep]",STRING(mCount)).
   RUN Insert_TTName("Custid[rep]",STRING(tt-rep.cust-id)).
   RUN Insert_TTName("custname[rep]",TRIM(tt-rep.cust-name)).
   RUN Insert_TTName("dates[rep]",tt-rep.dates).
   RUN Insert_TTName("numdb[rep]",STRING(tt-rep.numdb)).
   RUN Insert_TTName("numct[rep]",STRING(tt-rep.numct)).
   RUN Insert_TTName("sumdb[rep]",STRING(tt-rep.sumdb,"-zzz,zzz,zzz,zz9.99")).
   RUN Insert_TTName("sumct[rep]",STRING(tt-rep.sumct,"-zzz,zzz,zzz,zz9.99")).
   RUN Insert_TTName("minper[rep]",STRING(tt-rep.minper,"999.99")).

   RUN NextCircle_TTName("rep").


END. /* REPEAT BLCK */            
vHQry:QUERY-CLOSE().
DELETE OBJECT vHQry.

RUN EndCircle_TTName("rep").


RUN Insert_TTName("Itogct   " , STRING(mItogct)   ).
RUN Insert_TTName("Itogdb   " , STRING(mItogdb)   ).
RUN Insert_TTName("Itogsumct" , STRING(mItogsumct,"-zzz,zzz,zzz,zz9.99")).
RUN Insert_TTName("Itogsumdb" , STRING(mItogsumdb,"-zzz,zzz,zzz,zz9.99")).
RUN Insert_TTName("Itogper  " , STRING(mItogper,"999.99")).



RUN printvd.p("snyatie",INPUT TABLE ttnames). 

{intrface.del}