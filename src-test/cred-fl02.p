/*
Прошу Вас сделать выгрузку информации  из БИСКВИТА по ФЛ для предоставления информации  руководству  в виде трех разных отчетов следующего содержания:

1.	Все ФИЗИЧЕСКИЕ ЛИЦА со статусом VIP имеющие ссудные счета 455* и 457* если признак VIP на клиенте кто-нибудь проставлял

2.	Все кредиты ФИЗИЧЕСКИХ ЛИЦ , номер кредитного договора которых содержит  <К>, <КП>, КЛ>. -  т.е. буквенная часть  номера договора содержит только К, КП, КЛ (только охватывающий договор), КС

3.	Все кредиты ФИЗИЧЕСКИХ ЛИЦ, номер кредитного договора которых содержит <КС>

Отчеты должны содержать следующую информацию:

1.	Порядковый номер
2.	Идентификационный номер в программе (БИС) - идентификационный номер клиента?
3.	ФИО заемщика
4.	Номер кредитного договора
5.	Дата кредитного договора
6.	Сумма выданного кредита
7.	Процентная ставка по кредиту текущую  ставку или на дату выдачи?
8.	Сумма задолженности на текущую дату (отдельно срочная *455 и 457* и просроченная *45815 и 45817*)
9.	Срок окончания кредита
10.	Обеспечение (счет, номер договора залога, описание, сумма залога) - может быть, счет не обязателен? А то ссудный счет не показываем, а счет залога показываем.
11.	Коэффициент резервирования - на какую дату ?
12.	Сумма резерва, сформированного на текущую дату



С уважением

Садовик А. Н.
тел.:10210

*/

{globals.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */
{getdate.i}
{sh-defs.i new}

output to "cred-fl.txt".

def var tNp              AS int64 no-undo.
def var i                AS int64 no-undo.
def var j                AS int64 no-undo.
def var k                AS int64 no-undo.
def var cFl              as char  no-undo.
def var cXL              as char  no-undo.
def var mdebt455         as dec   no-undo.
def var mdebt458         as dec   no-undo.
def var mterm-cont-code  as char  no-undo.
def var mterm-opisanie$  as char  no-undo.
def var mterm-amt-rub    as dec   no-undo.
def var vTrmSurChar      as char  no-undo.
def var mTerm-acct       as char  no-undo.
def var mres-amt-rub     as dec   no-undo.
def var mres-coef        as dec   no-undo.
def var mamt-rub         as dec   no-undo.
def var mrate-comm       as dec   no-undo.
def var mres-coef-c      as dec   no-undo.
def var mres-coef-t      as dec   no-undo.



DEFINE TEMP-TABLE ttCred NO-UNDO
   FIELD np           AS int64
   FIELD uinkg        AS int64
   FIELD fio          AS CHARACTER
   FIELD cont-code    AS CHARACTER
   FIELD open-date    AS date
   FIELD amt-rub      AS dec
   FIELD rate-comm    AS dec
   FIELD debt455      AS dec
   FIELD debt458      AS dec
   FIELD end-date     AS date
   FIELD term-acct    AS CHARACTER
   FIELD term-cont-code    AS CHARACTER
   FIELD term-opisanie$    AS CHARACTER
   FIELD term-amt-rub      AS dec
   FIELD res-coef         AS dec
   FIELD res-amt-rub      AS dec
.

DEF BUFFER bloan for loan.
DEF BUFFER bcomm-rate for comm-rate.

tNp = 1.
/*
2.	Все кредиты ФИЗИЧЕСКИХ ЛИЦ , номер кредитного договора которых содержит  <К>, <КП>, КЛ>. -  т.е. буквенная часть  номера договора содержит только К, КП, КЛ (только охватывающий договор), КС
*/

for each loan where loan.contract   = "Кредит"
                and loan.close-date = ?
                and (   entry(1,loan.cont-code,"@")  MATCHES "*-К"  
                     or entry(1,loan.cont-code,"@")  MATCHES "*-КП"
                     or entry(1,loan.cont-code,"@")  MATCHES "*-КЛ"  
                     or entry(1,loan.cont-code,"@")  MATCHES "*-КС" 
                    ) 
                and loan.cust-cat   = "Ч"
                and loan.filial-id <> "0400"
                no-lock.

   mdebt455        = 0.
   mdebt458        = 0.
   mterm-cont-code = "".
   mterm-opisanie$ = "".
   mterm-amt-rub   = 0.
   vTrmSurChar     = "".
   mTerm-acct      = "". 
   mres-amt-rub    = 0. 
   mamt-rub        = 0.
   mrate-comm      = 0.
   mres-coef-t = 0.
   mres-coef-c = 0.


   find last loan-acct where loan-acct.contract  = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "Кредит"
                         and loan-acct.since    <= end-date
                         no-lock no-error.

   if avail loan-acct then do:
       run acct-pos in h_base (loan-acct.acct,loan-acct.currency,end-date,end-date,?).
       mdebt455 = sh-bal.
   end.
   find last loan-acct where loan-acct.contract  = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "КредПр"
                         no-lock no-error.

   if avail loan-acct then do:
       run acct-pos in h_base (loan-acct.acct,loan-acct.currency,end-date,end-date,?).
       mdebt458 = sh-bal.
   end.

   find last loan-acct where loan-acct.contract  = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "КредРез"
                         no-lock no-error.

   if avail loan-acct then do:
       run acct-pos in h_base (loan-acct.acct,loan-acct.currency,end-date,end-date,?).
       mres-amt-rub = - sh-bal.
   end.
   find last loan-acct where loan-acct.contract  = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type = "КредРез1"
                         no-lock no-error.

   if avail loan-acct then do:
       run acct-pos in h_base (loan-acct.acct,loan-acct.currency,end-date,end-date,?).
       mres-amt-rub = mres-amt-rub + ( - sh-bal).
   end.

   find last loan-acct where loan-acct.contract  = loan.contract
                         and loan-acct.cont-code = loan.cont-code
                         and loan-acct.acct-type begins "КредОб"
                         no-lock no-error.
   if avail loan-acct then do:
      mTerm-acct = entry(1,loan-acct.acct,"@").
   end.


   find first term-obl WHERE  term-obl.contract  EQ loan.contract
                         AND  term-obl.cont-code EQ loan.cont-code
                         AND term-obl.idnt EQ 5                         
                         NO-LOCK no-error.
   if avail term-obl then do:
      vTrmSurChar = term-obl.contract + "," + term-obl.cont-code  + "," +
                    STRING (term-obl.idnt)     + "," +
                    STRING (term-obl.end-date) + "," +
                    STRING (term-obl.nn).


      mterm-cont-code = GetXAttrValueEx("term-obl",
                                      vTrmSurChar,
                                      "НомДогОб",
                                      "").
      mterm-opisanie$ = GetXAttrValueEx("term-obl",
                                      vTrmSurChar,
                                      "Описание",
                                      "").

      mterm-amt-rub = term-obl.amt-rub.
   end.

   find first person where person.person-id = loan.cust-id no-lock no-error.
   find first loan-cond where loan-cond.contract  = loan.contract
                         and loan-cond.cont-code  = loan.cont-code
                         no-lock no-error.
   if not avail loan-cond then next.

   i = 0.

   find last  term-obl where term-obl.contract    = loan-cond.contract
                         and term-obl.cont-code   = loan-cond.cont-code
                         and term-obl.idnt        = 128
                         and term-obl.sop-date    = ? 
                         and term-obl.end-date   <= end-date
                         use-index primary 
                         no-lock no-error.
   find last comm-rate where comm-rate.kau eq loan.contract + ',' + loan.cont-code  
                         and comm-rate.commission = "%Рез" 
                         and comm-rate.since    <= end-date
                         use-index kau
                         no-lock no-error.
   if avail term-obl then do:
      find last bcomm-rate where bcomm-rate.kau eq term-obl.lnk-contract + ',' + term-obl.lnk-cont-code  
                            and bcomm-rate.commission = "%Рез" 
                            and bcomm-rate.since    <= end-date
                            use-index kau
                            no-lock no-error.
      if avail bcomm-rate then do:
         mres-coef-t = bcomm-rate.rate-comm.
         mres-coef   = bcomm-rate.rate-comm.
      end.
   end.
   if avail comm-rate then do:
      mres-coef-c = comm-rate.rate-comm.
      mres-coef = comm-rate.rate-comm.
   end.
   if avail comm-rate and avail term-obl then do:
      if comm-rate.since >= term-obl.end-date  then do:
         mres-coef = mres-coef-c.
      end.
      else do:
         mres-coef = mres-coef-t.
      end.
   end.
       
   find first   term-obl where term-obl.contract  = loan-cond.contract
                         and term-obl.cont-code   = loan-cond.cont-code
                         and term-obl.end-date   <= loan-cond.since
                         and term-obl.idnt        = 2
                         no-lock no-error.
   if avail term-obl then do:
      mamt-rub = term-obl.amt-rub.
   end.

   find last comm-rate WHERE comm-rate.kau eq loan.contract + ',' + loan.cont-code
                          AND comm-rate.commission eq '%Кред'
                          and comm-rate.since <= end-date
                          no-lock no-error.
   if avail comm-rate then do:
      mrate-comm = comm-rate.rate-comm.
   end.
   
   create ttCred.
   ASSIGN 
     ttCred.np               = tNp
     ttCred.uinkg            = loan.cust-id
     ttCred.fio              = person.name-last + " " + person.first-names
     ttCred.cont-code        = entry(1,loan.cont-code,"@")
     ttCred.open-date        = loan.open-date
     ttCred.amt-rub          = mamt-rub
     ttCred.rate-comm        = mrate-comm
     ttCred.debt455          = mdebt455
     ttCred.debt458          = mdebt458
     ttCred.end-date         = loan.end-date
     ttCred.term-acct        = mterm-acct 
     ttCred.term-cont-code   = mterm-cont-code 
     ttCred.term-opisanie$   = mterm-opisanie$ 
     ttCred.term-amt-rub     = mterm-amt-rub 
     ttCred.res-coef         = mres-coef 
     ttCred.res-amt-rub      = mres-amt-rub
   .
   tNp = tNp + 1.
end.
output close.

cFl = "./cred-fl02.xml".

OUTPUT TO VALUE(cFl).

PUT UNFORMATTED XLHead("tmp", "CCCCCNNNNCCCNNNNC", "90,90,90,110,90,90,90,90,90,90,150,110,110,110,90,90,90").
PUT UNFORMATTED XLRow(0) XLCellHat("Все кредиты ФИЗИЧЕСКИХ ЛИЦ , номер кредитного договора которых содержит  <К>, <КП>, КЛ> за " + STRING(end-date, "99.99.9999"), 13) XLRowEnd().



cXL = XLCellHead("Порядковый номер                                  ",0,0,0)
    + XLCellHead("Идентификационный номер в программе (БИС)         ",0,0,0)
    + XLCellHead("ФИО заемщика                                      ",0,0,0)
    + XLCellHead("Номер кредитного договора                         ",0,0,0)
    + XLCellHead("Дата кредитного договора                          ",0,0,0)
    + XLCellHead("Сумма выданного кредита                           ",0,0,0)
    + XLCellHead("Процентная ставка по кредиту                      ",0,0,0)
    + XLCellHead("Сумма задолженности на текущую дату (срочная)     ",0,0,0)
    + XLCellHead("Сумма задолженности на текущую дату (просроченная)",0,0,0)
    + XLCellHead("Срок окончания кредита                            ",0,0,0)
    + XLCellHead("Обеспечение : счет                                ",0,0,0)
    + XLCellHead("Обеспечение : номер договора залога               ",0,0,0)
    + XLCellHead("Обеспечение : описание                            ",0,0,0)
    + XLCellHead("Обеспечение : сумма залога                        ",0,0,0)
    + XLCellHead("Коэффициент резервирования                        ",0,0,0)
    + XLCellHead("Сумма резерва, сформированного на текущую дату    ",0,0,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


cXL = XLCellHead("1",0,0,0)
    + XLCellHead("2",0,0,0)
    + XLCellHead("3",0,0,0)
    + XLCellHead("4",0,0,0)
    + XLCellHead("5",0,0,0)
    + XLCellHead("6",0,0,0)
    + XLCellHead("7",0,0,0)
    + XLCellHead("8",0,0,0)
    + XLCellHead("9",0,0,0)
    + XLCellHead("10",0,0,0)
    + XLCellHead("11",0,0,0)
    + XLCellHead("12",0,0,0)
    + XLCellHead("13",0,0,0)
    + XLCellHead("14",0,0,0)
    + XLCellHead("15",0,0,0)
    + XLCellHead("16",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH ttCred  NO-LOCK.
   cXL = XLCell(STRING(       ttCred.np              ))                       
       + XLCell(STRING(       ttCred.uinkg           ))                       
       + XLCell(STRING(       ttCred.fio             ))                 
       + XLCell(STRING(       ttCred.cont-code       ))                    
       + XLCell(STRING(       ttCred.open-date       ))                    
       + XLNumCell(           ttCred.amt-rub         )                         
       + XLNumCell(           ttCred.rate-comm       )                     
       + XLNumCell(           ttCred.debt455         )                 
       + XLNumCell(           ttCred.debt458         )                        
       + XLCell(STRING(       ttCred.end-date        ))                    
       + XLCell(STRING(       ttCred.term-acct       ))              
       + XLCell(STRING(       ttCred.term-cont-code  ))    
       + XLCell(trim(STRING(  ttCred.term-opisanie$  )))    
       + XLNumCell(           ttCred.term-amt-rub    )           
       + XLNumCell(           ttCred.res-coef        )           
       + XLNumCell(           ttCred.res-amt-rub     )           
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

RUN sndbispc ("file=" + cFl + ";class=bq").














