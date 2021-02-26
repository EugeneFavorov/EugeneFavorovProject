/*Глухов*/
/*
ФИО/Наименование орг-ии КД Остаток КредРасч Остаток КредРасч1 Сумма просрочки Тип Блокировки Сумма блокировки Остаток на 458

1. Отчет должен выгружать только те КД, у которых есть остатки на счетах просрочки (КредПр, КредПр%, КредПр%В, КредПр%ОВ, КредШт%ОВ)
2. Графа <Сумма просрочки> равна сумме остатков по всем счетам просрочки
3. Графа Остаток КредРасч равна остатку на сч 408/407 с ролью КредРасч
4. Графа Остаток КредРасч1- равна остатку на сч 408/407 с ролью КредРасч1
5. Графа Тип Блокировки- если есть блокировка на счете КредРасч -название блокировки
6. Графа Сумма блокировки- сумма последней по дате блокировки
7. Графа Остаток на 458- равна остатку на счете с ролью КредПр
*/

{globals.i}
{client.i}
{sh-defs.i}
{mf-loan.i}
{intrface.get blkob}

DEFINE VARIABLE mDateS    AS DATE INITIAL TODAY NO-UNDO.
DEFINE VARIABLE mFilial   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKD       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFname    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst      AS DECIMAL NO-UNDO.
DEFINE BUFFER bloan-acct FOR loan-acct.
DEFINE STREAM kag.
def var acct408 as char no-undo.
def buffer bacct for acct.
def var vFilialId as char no-undo.
def var vOldFilialId as char no-undo.
def var listFilial as char no-undo.
def var ii as int no-undo.
def var tmptmpstr as char no-undo.

listFilial = '0000,0300,0500'.

DEFINE TEMP-TABLE ttRep NO-UNDO 
   FIELD name      AS CHARACTER 
   FIELD kd        AS CHARACTER 
   FIELD sumrs     AS DECIMAL
   FIELD sumrs1    AS DECIMAL
   FIELD ost40     AS DECIMAL
   FIELD ost40signsD AS Decimal
   FIELD ost40signsC AS CHAR
   FIELD sumpros   AS DECIMAL
   FIELD block     AS CHARACTER 
   FIELD sumblock  AS DECIMAL
   FIELD sum458    AS DECIMAL
   FIELD priznak   AS CHARACTER 
   INDEX i1 name kd.

ASSIGN
   mDateS  = gend-date
   mFilial = shFilial.

DEFINE FRAME iParam 
   mDateS    LABEL "За дату"  FORMAT "99.99.9999"
   mFilial   LABEL "Филиал" FORMAT "x(4)"
   WITH CENTERED SIDE-LABELS ROW 10 1 COL OVERLAY TITLE COLOR bright-white "[ ОТЧЕТ О КД С ПРОСРОЧКОЙ]".

ON F1 OF mDateS
   DO:
      RUN calend.p.
      IF (LASTKEY EQ 13 OR
         LASTKEY EQ 10) AND
         pick-value NE ?
         THEN mDateS:SCREEN-VALUE = pick-value.
   END.

ON F1 OF mFilial
   DO:
      DO TRANSACTION:
         RUN browseld.p("branch",
            "branch-type" ,
            "10,11",
            "",
            4).
         mFilial:SCREEN-VALUE = pick-value.
      END. 
   END.

PAUSE 0.
UPDATE 
   mDateS
   mFilial
   WITH FRAME iParam. 

IF LAST-EVENT:FUNCTION  EQ "END-ERROR"
   THEN 
DO:
   HIDE FRAME iParam NO-PAUSE.
   RETURN.
END.


FOR EACH loan WHERE loan.filial     EQ mFilial
                AND loan.contract   EQ "Кредит"
                AND loan.close-date EQ ?
NO-LOCK,
EACH loan-acct WHERE loan-acct.contract  EQ loan.contract
                 AND loan-acct.cont-code EQ loan.cont-code
                 AND CAN-DO("КредПр,КредПр%,КредПр%В,КредПр%ОВ,КредШт%ОВ",loan-acct.acct-type)
NO-LOCK,
FIRST acct WHERE 
                 acct.acct       EQ loan-acct.acct
             AND acct.currency   EQ loan-acct.currency
             AND acct.close-date EQ ? 
   NO-LOCK QUERY-TUNING(DEBUG SQL):

   RUN acct-pos IN h_base (
      loan-acct.acct,
      loan-acct.currency,
      mDateS,
      mDateS, 
      ?).

   most = ABSOLUTE(IF loan-acct.currency = "" THEN sh-bal ELSE sh-val ) .

   IF most EQ 0 THEN NEXT.
   /*
message loan-acct.acct + ' ' + string(most) view-as alert-box.
*/
   FIND FIRST ttRep WHERE ttRep.kd EQ loan.cont-code EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE ttRep THEN
   DO:
      RUN RE_CLIENT (loan.cust-cat, loan.cust-id, INPUT-OUTPUT mCustName).

      CREATE ttRep.
      ASSIGN 
         ttRep.name = mCustName
         ttRep.kd = loan.cont-code
         .
      acct408 = ''.
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ loan-acct.contract
                             AND bloan-acct.cont-code EQ loan-acct.cont-code
                             AND bloan-acct.acct-type  EQ "КредРасч"
      NO-LOCK NO-ERROR.

      IF AVAILABLE bloan-acct THEN
      DO:
         acct408 = bloan-acct.acct.
         RUN acct-pos IN h_base (
            bloan-acct.acct,
            bloan-acct.currency,
            mDateS,
            mDateS, 
            ?).
             		
         ASSIGN
            ttRep.sumrs    = ABSOLUTE(IF loan-acct.currency = "" THEN sh-bal ELSE sh-val)
            ttRep.sumblock = ABSOLUTE(GetBlockPositionAll(bloan-acct.acct,bloan-acct.currency,mDateS))
            ttRep.block    = BlockAcct(bloan-acct.acct + "," + bloan-acct.currency ,DATETIME(mDateS + 1) - 1)
         .



      END.
      FIND LAST bloan-acct WHERE bloan-acct.contract  EQ loan-acct.contract
                             AND bloan-acct.cont-code EQ loan-acct.cont-code
                             AND loan-acct.acct-type  EQ "КредРасч1"
         NO-LOCK NO-ERROR.
      IF AVAILABLE bloan-acct THEN
      DO:
         RUN acct-pos IN h_base (
            bloan-acct.acct,
            loan-acct.currency,
            mDateS,
            mDateS, 
            ?).

         ttRep.sumrs1 = ABSOLUTE(IF loan-acct.currency = "" THEN sh-bal ELSE sh-val).

      END.
      for each bacct where bacct.cust-id = loan.cust-id and bacct.cust-cat = loan.cust-cat 
            and (bacct.acct begins '407' or bacct.acct begins '408') 
            and bacct.close-date EQ ? 
            and bacct.acct <> acct408 no-lock:

            RUN acct-pos IN h_base (
            bacct.acct,
            bacct.currency,
            mDateS,
            mDateS, 
            ?).

            ttRep.ost40 = ttRep.ost40 + ABSOLUTE(sh-bal).

      end.
      ttRep.ost40signsD = 0.
      find first signs where signs.file-name = 'loan'
        and signs.code = 'acctpost'
        and signs.surrogate = loan.contract + ',' + loan.cont-code no-lock no-error.
      if avail signs then do:
          find first bacct where bacct.acct = signs.code-value no-lock no-error.
          if avail bacct then do:
               RUN acct-pos IN h_base (
                bacct.acct,
                bacct.currency,
                mDateS,
                mDateS, 
                ?).
                find first person where person.person-id = acct.cust-id no-lock no-error.
                tmptmpstr = ''.
                if avail person then tmptmpstr = person.name-last + ' ' + person.first-names.
                ttRep.ost40signsD = ABSOLUTE(sh-bal).
                ttRep.ost40signsC = bacct.acct + '  ' + tmptmpstr.         
          end.
      end.
   END.
   
   


   IF loan-acct.acct-type EQ "КредПр" THEN
      ASSIGN
         ttRep.sumpros = ttRep.sumpros + most
         ttRep.sum458  = ttRep.sum458 + most.
   ELSE
      ttRep.sumpros = ttRep.sumpros + most.
   ttRep.priznak = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"priznak","").
/*   ttRep.ost40 = 0. */
   
   
  

   
   
END.


mFname = "./rpt_prosr_" + replace(string(TODAY,"99.99.9999"),".","_") + "_" + mFilial +  "_" + USERID('bisquit') + ".xml".   

OUTPUT STREAM kag TO VALUE (mFname)
   UNBUFFERED  CONVERT  TARGET "UTF-8"  SOURCE "IBM866".    


def var tmpstr as char no-undo.

tmpstr =  STRING(ttRep.sumrs1) + ' ' + STRING(ttRep.ost40).
      
      
PUT STREAM kag UNFORMATTED {rpt_prosr.i1} SKIP.


/*
1 ФИО/Наименование орг-ии 
2 КД 
3 Остаток КредРасч 
4 Остаток КредРасч1 
5 Сумма просрочки 
6 Тип Блокировки 
7 Сумма блокировки 
8 Остаток на 458
*/
FOR EACH ttrep WHERE ttRep.sumrs GT 0 OR ttRep.sumrs1 GT 0  OR ttRep.ost40 GT 0 OR ttRep.ost40signsD GT 0 NO-LOCK:

   PUT STREAM kag UNFORMATTED
   '<Row>
    <Cell><Data ss:Type="String">' + string(ttRep.name) + '</Data></Cell>' +
    '<Cell ss:StyleID="s68"><Data ss:Type="String">' + string(DelFilFromAcct(ttRep.kd)) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.sumrs) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.sumrs1) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.ost40) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.sumpros) + '</Data></Cell>' +
    '<Cell><Data ss:Type="String">' + string(ttRep.block) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.sumblock) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.sum458) + '</Data></Cell>' +
    '<Cell><Data ss:Type="String">' + string(ttRep.priznak) + '</Data></Cell>' +
    '<Cell><Data ss:Type="Number">' + string(ttRep.ost40signsD) + '</Data></Cell>' +
    '<Cell><Data ss:Type="String">' + string(ttRep.ost40signsC) + '</Data></Cell>
    </Row>' SKIP.
    
    /*
   PUT STREAM kag UNFORMATTED
      SUBSTITUTE('
   <Row>
    <Cell><Data ss:Type="String">&1</Data></Cell>
    <Cell ss:StyleID="s68"><Data ss:Type="String">&2</Data></Cell>
    <Cell><Data ss:Type="Number">&3</Data></Cell>
    <Cell><Data ss:Type="Number">&4</Data></Cell>
    <Cell><Data ss:Type="Number">&5</Data></Cell>
    <Cell><Data ss:Type="Number">&6</Data></Cell>
    
    <Cell><Data ss:Type="String">&7</Data></Cell>
    
    <Cell><Data ss:Type="Number">&8</Data></Cell>
    <Cell><Data ss:Type="Number">&9</Data></Cell>
    <Cell><Data ss:Type="String">&10</Data></Cell>
   </Row>',
      ttRep.name,
      DelFilFromAcct(ttRep.kd),
      ttRep.sumrs,
      ttRep.sumrs1,
      ttRep.ost40,
      ttRep.sumpros,
      ttRep.block,
      ttRep.sumblock,
      ttRep.sum458,
      ttRep.priznak) SKIP.
    */
END.

PUT STREAM kag UNFORMATTED {rpt_prosr.i2} SKIP.

RUN sndbispc ("file=" + mfname + ";class=bq").


