{globals.i}
{prn-doc.def &with_proc=YES}
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}
{justasec}
{intrface.get refer}    /* Библиотека службы справочников. */
{intrface.get date}

{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */


/*
DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
*/

DEFINE VARIABLE iStr	     AS CHARACTER 			NO-UNDO. /* Ю Ч*/


DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mInn	     AS CHARACTER 			NO-UNDO. /* Ю Ч*/
DEFINE VARIABLE mAdrReg      AS CHARACTER 			NO-UNDO. /*АдрЮр*/
DEFINE VARIABLE mAdrFact     AS CHARACTER 			NO-UNDO. /*АдрФакт*/
DEFINE VARIABLE mCustName    AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mValName     AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress1    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAddress2    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vAmtStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE vDecStr      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mError       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mSex         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mWord        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE mText        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDocV        AS CHARACTER          NO-UNDO.
DEFINE VARIABLE phone        AS CHARACTER          NO-UNDO INIT ''.
DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mDatTMP      AS DATE               NO-UNDO.
DEFINE VARIABLE mList_Acct   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mCurrency    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE shfilial_    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE ost          AS decimal            NO-UNDO.
DEFINE VARIABLE mBegDate     AS date               NO-UNDO.   
DEFINE VARIABLE mEndDate     AS date               NO-UNDO.   
DEFINE VARIABLE n            AS int                NO-UNDO.   
DEFINE VARIABLE i            AS int                NO-UNDO.   
DEFINE VARIABLE j            AS int                NO-UNDO.   
DEFINE VARIABLE i1           AS int                NO-UNDO.   
DEFINE VARIABLE j2           AS int                NO-UNDO.   
DEFINE VARIABLE mSh-Db       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-Cr       AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vDb      AS dec                NO-UNDO.   
DEFINE VARIABLE mSh-vCr      AS dec                NO-UNDO.   
DEFINE VARIABLE mList_cont   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mBalance     AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK1   AS dec                NO-UNDO.   
DEFINE VARIABLE mBalanceK2   AS dec                NO-UNDO.   
DEFINE VARIABLE StrBalanceK1 AS char               NO-UNDO.   
DEFINE VARIABLE StrBalanceK2 AS char               NO-UNDO.   
DEFINE VARIABLE namestat     AS char               NO-UNDO.   
DEFINE VARIABLE mtoday       AS date               NO-UNDO.   
DEFINE VARIABLE mSumma       AS dec                NO-UNDO.   
DEFINE VARIABLE iDate        AS date               NO-UNDO.   
DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
DEFINE VARIABLE vWork        AS LOGICAL            NO-UNDO.
DEFINE VARIABLE List-bal     AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-bal-2   AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-bal-3   AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE List-bal-4   AS CHARACTER          NO-UNDO.    /* Список балансовых счетов */
DEFINE VARIABLE bal_         AS CHARACTER          NO-UNDO.
DEFINE VARIABLE file-name    AS CHARACTER          NO-UNDO.
DEFINE VARIABLE kold         AS int                NO-UNDO.    /* сколько дней перебирать для поиска рабочих дней */
DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* количество рабочих дней */

DEFINE VARIABLE var-NameOrg   AS char               NO-UNDO.   
DEFINE VARIABLE var-NameShort AS char               NO-UNDO.   

DEFINE VARIABLE beg-dt1      AS date               NO-UNDO.   
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.


{getdates.i}


DEF BUFFER bacct for acct.

DEFINE TEMP-TABLE ttDoc
   FIELD branch       AS CHARACTER
   FIELD op-date      AS DATE
   FIELD rec-name  AS CHARACTER
   FIELD account      AS CHARACTER
   FIELD num-doc      AS CHARACTER
   FIELD currency     AS CHARACTER
   FIELD amt          AS decimal
   FIELD date-mess    AS DATE
   FIELD date-ans     AS CHARACTER
   FIELD op           AS CHARACTER
   FIELD type         AS CHARACTER
   INDEX ind1 branch op-date .


PAUSE 0.

kold = 45.
koldo = 15.



/*
Зачисление средств на счета ЮЛ и ИП (407*,406*, 40802*) в рублях со счетов нерезидентов:
из российского банка  в т.ч. внутренние операции со счетов клиентов нерезидентов ПАО Плюс Банк)  со счетов 40807*, 40820*, 426*.
*/

list-bal   = "406*,407*,40802*".
list-bal-2 = "40807*,40820*,426*".
/*
list-bal-3 = "30111*".
*/
list-bal-3 = "30111*,30302*".
list-bal-4 = "40702,40802".

for each op-entry where  op-entry.op-date  >= beg-date
                    and  op-entry.op-date  <= end-date
                    and  op-entry.op-status >= chr(251)
                    and  CAN-DO(list-bal, op-entry.acct-cr)
                    no-lock.
   find first op of op-entry.
   if  CAN-DO(list-bal-2, op.ben-acct) and  op-entry.currency = "" and CAN-DO(list-bal, op-entry.acct-cr) then do:
       RUN create_ttDoc(op.op,"Извещение").
   end.
   if  CAN-DO(list-bal-3, op.ben-acct) and  op-entry.currency = "" and CAN-DO(list-bal, op-entry.acct-cr) and substr(trim(op.details),2,2) EQ "VO"  then do:
       RUN create_ttDoc(op.op,"Извещение").
   end.
   if  CAN-DO(list-bal-2, op-entry.acct-db)  and  op-entry.currency = "" and  CAN-DO(list-bal, op-entry.acct-cr) then do:
         RUN create_ttDoc(op.op,"Извещение").
   end.
   if  CAN-DO(list-bal, op-entry.acct-cr) and op-entry.amt-cur <> 0
   then do:
       find first acct where acct.acct = op-entry.acct-cr no-lock no-error.
       if avail acct and acct.contract begins "Тран" then do:
          RUN create_ttDoc(op.op,"Уведомление").
      end.
   end.
end.
/*
output to "ttDoc.txt".
for each ttDoc.
   export ttDoc.
end.
output close.
*/

FOR each branch WHERE                                                
   lookup("СВОД",branch.parent-id,";") GT 0                          
   AND (branch.close-date EQ ? OR branch.close-date >= today)     
   AND branch.isbank EQ yes                                          
   NO-LOCK                                                           
   BY branch.order                                                   
   BY branch.branch-id 
   .

   cFl = "./qm.xml".
   
   OUTPUT TO VALUE(cFl).
   
   PUT UNFORMATTED XLHead("tmp", "CCCCCCNCCCC", "70,90,300,215,100,100,90,125,110,100,120").
   
   cXL = XLCellHead("Филиал",0,0,0)
       + XLCellHead("Дата зачисления",0,0,0)
       + XLCellHead("Наименование клиента",0,0,0)
       + XLCellHead("Расчётный счёт",0,0,0)
       + XLCellHead("Номер уведомления / извещения",0,0,0)
       + XLCellHead("Валюта поступления",0,0,0)
       + XLCellHead("Сумма поступления",0,0,0)
       + XLCellHead("Срок предоставления обосновывающих документов",0,0,0)
       + XLCellHead("Клиент отчитался (дата)",0,0,0)
       + XLCellHead("Уведомление/Извещение",0,0,0)
       + XLCellHead("Идентификатор документа",0,0,0)
       .
   
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   
   FOR EACH ttDoc where ttDoc.branch = branch.branch-id NO-LOCK.
      cXL = XLCell(string(ttDoc.branch))                 
          + XLCell("`" + STRING(ttDoc.op-date     ))          
          + XLCell(STRING(ttDoc.rec-name ))          
          + XLCell(STRING(entry(1,ttDoc.account,"@") ))          
          + XLCell(STRING(ttDoc.num-doc ))            
          + XLCell(STRING(ttDoc.currency))                                   
          + XLNumCell(ttDoc.amt)                                      
          + XLCell("`" + STRING(ttDoc.date-mess ))                                   
          + XLCell("`" + STRING(ttDoc.date-ans  ))                                        
          + XLCell(STRING(ttDoc.type))                         
          + XLCell(STRING(ttDoc.op))                         
          .
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.
   
   
   PUT UNFORMATTED XLEnd().
   OUTPUT CLOSE.
   
      RUN pb_mail.p ("v.ignatchenko,e.polovinkina,o.shakin,A.Korobova,M.Vasileva", "QM Report " + branch.branch-id , "QM Report " + branch.branch-id, cFl).

end.
return.
   
PROCEDURE create_ttDOc:
   DEF INPUT  PARAM pop      AS DEC   NO-UNDO.
   DEF INPUT  PARAM type     AS Char  NO-UNDO.

   DEF BUFFER bop for op.
   DEF BUFFER bop-entry for op-entry.



   DEFINE VARIABLE buf-str        AS char   NO-UNDO.   
   DEFINE VARIABLE mCust-Cat      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE ost          AS decimal            NO-UNDO.
   DEFINE VARIABLE SummaStr     AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE VARIABLE mSignsVal    AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE i            AS int                NO-UNDO.   
   DEFINE VARIABLE j            AS int                NO-UNDO.   
   DEFINE VARIABLE iDate        AS date               NO-UNDO.   
   DEFINE VARIABLE datebeg_     AS date               NO-UNDO.   
   DEFINE VARIABLE kold         AS int                NO-UNDO.    /* сколько дней перебирать для поиска рабочих дней */
   DEFINE VARIABLE koldo        AS int                NO-UNDO.    /* количество рабочих дней */

   DEFINE VARIABLE var-sum        AS char   NO-UNDO.   
   DEFINE VARIABLE var-srok       AS char   NO-UNDO.   
   DEFINE VARIABLE var-stat       AS char  NO-UNDO.   
   DEFINE VARIABLE var-inn        AS char   NO-UNDO.   
   DEFINE VARIABLE var-date-ans  AS char   NO-UNDO.   


/*
Номер п/п /Дата зачисления на расчетный счет ДД.ММ.ГГГГ
номер счета
Наименование орг-ции, ИНН
AddressUr - Юридический адрес
(Омском филиале ПАО Плюс Банк) (в зависимости от филиала)
Var_01 Привет (указывается дата зачисления на р/счет
(номер п/п) с которого происходит формирование документа
Сумма цифрами:
Сумма прописью:
в срок (настроить автоматически 15 раб. дней с даты, следующей за датой зачисления) по ДД.ММ.ГГГГ

*/
   find first bop where bop.op = pop no-lock no-error.
   find first bop-entry of bop no-error.
   find first acct where acct.acct = bop-entry.acct-cr no-lock no-error.
   datebeg_ = bop-entry.op-date.
   j = 0.
   kold = 45.
   koldo = 15.
   do i= 1 to kold:
      iDate = datebeg_ + i.
      vWork = NOT holidayru(iDate).
      if vWork then do:
         j = j + 1.
      end.
      if j >= koldo then do:
         leave.       
      end.
   end.
   var-srok      = string(iDate,"99.99.9999").
   ost           =  op-entry.amt-rub.
   var-sum       = string(ost).
   var-date-ans = GetXattrValueEx("op",STRING(Op.op),"ВК-ДатаДокум",?). 

   IF acct.cust-cat = "Ю" THEN
   DO:
 /* Данные клиента */      
         FIND FIRST cust-corp WHERE
         		   cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN RETURN.
     
         var-NameOrg = cust-corp.name-corp.
         var-NameShort = cust-corp.name-short.
   END. /*Юридическое лицо*/   
   IF acct.cust-cat = "Ч" THEN
   DO:
 /* Данные клиента */      
         FIND FIRST person WHERE
         		   person.person-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL person THEN RETURN.
         var-NameOrg = "ИП " + person.name-last + " " + person.first-names.
         var-NameShort = var-NameOrg.
   END. /* Физическое лицо */   
   create ttDOc.
   assign
      ttDOc.branch       =  bop.filial-id
      ttDOc.op-date      =  bop.op-date
      ttDOc.rec-name     =  var-NameShort
      ttDOc.account      =  acct.acct
      ttDOc.num-doc      =  bop.doc-num
      ttDOc.currency     =  if bop-entry.currency = "" then "643" else bop-entry.currency
      ttDOc.amt          =  if bop-entry.currency = "" then bop-entry.amt-rub else bop-entry.amt-cur
      ttDOc.date-mess    =  date(var-srok)
      ttDOc.date-ans     =  if var-date-ans <> ? then  var-date-ans  else ""
      ttDOc.type         =  type
      ttDOc.op           =  string(bop.op)
   .
END PROCEDURE.




