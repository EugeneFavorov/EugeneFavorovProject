   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {intrface.get date}

   DEF VAR vRecLoan      AS RECID   NO-UNDO.
   DEF VAR vRecCond      AS RECID   NO-UNDO.
   DEF VAR vtt-term-amt  AS DEC     NO-UNDO.
   DEF VAR iMode         AS INT64   NO-UNDO.
   DEF VAR vChangeSumm   AS LOG     NO-UNDO.
   DEF VAR vChangePr     AS LOG     NO-UNDO.
   DEF VAR vChangeDate   AS LOG     NO-UNDO.
   DEF VAR vChangePer    AS LOG     NO-UNDO.
   DEF VAR vCondCount    AS INT64   NO-UNDO.

   def var stracct       as char    no-undo .
   def var strmess       as char    no-undo.
   def var i             as INT64   no-undo .
   def var j             as INT64   no-undo .
   def var fl            as INT64   no-undo .
   def var lcount        as INT64   no-undo .
   def var count-total   as INT64   no-undo .
   def var bacct-rec     as char    no-undo.
                                   
   def var b-end-date    as date    no-undo.
   def var datebeg_      as date    no-undo.
   def var dateend_      as date    no-undo.
   def var iDate         as date    no-undo.
                                    
   def var bloan         as char    no-undo.
   def var binn          as char    no-undo.
   def var bInnDoc       as char    no-undo.
   def var bSrok         as char    no-undo.
   def var bSurr         as char    no-undo.
   def var bDataSogl     as char    no-undo.
   DEF VAR vWork         AS LOG     NO-UNDO.
   def var iBranch       as char    no-undo.
   DEF var user_         AS CHAR    NO-UNDO.
   def var bInnSend      as char    no-undo.
                                  
   def buffer bsigns for signs.

   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
 
   pick-value = "".


   find first op where op.op = int(in-str) no-lock no-error.
   if not avail op then do:
     message "Не найден документ "  in-str  view-as alert-box.
     return .
   end.
   find first op-entry where op-entry.op = int(in-str) no-lock no-error.
   if not avail op then do:
     message "Не найдена проводка документа "  in-str  view-as alert-box.
     return .
   end.
   bacct-rec = GetXattrValueEx("op",STRING(Op.op),"acct-rec",?).
   if bacct-rec = ? then do:
     message "Не найден ДР acct-rec для документа "  Op.op  view-as alert-box.
     return .
   end.   
   bInnDoc = GetXattrValueEx("op",STRING(Op.op),"inn-rec",?).
   if bInnDoc = ? then do:
     message "Не найден ДР inn-rec для документа "  Op.op  view-as alert-box.
     return .
   end.   
   bInnSend = op.inn.
   if bInnDoc <> bInnSend then do:
     message "ИНН плательщика " bInnSend " и ИНН получателя " bInnDoc " разные для документа " op.op  view-as alert-box.
     return .
   end.   
   find first acct where acct.acct begins bacct-rec no-lock no-error.
   if not avail acct then do:
     message "Счет " acct.acct " не открыт "  view-as alert-box.
     return .
   end.   
   find last signs where signs.file = "loan"
                      and signs.code = "acct-dep"
                      and signs.code-val begins bacct-rec
                      no-lock no-error.
   if not avail signs then do:
     message "Не найден  ДР acct-rec, в котором был бы указан счёт " bacct-rec " из ДР acct-rec документа "  op.op ". Найти депозит невозможно."  view-as alert-box.
     return .
   end.   
   bSurr = signs.surr.
   find first loan where   loan.close-date EQ ?     
                     AND loan.contract EQ 'Депоз' 
                     and loan.cont-code = entry(2,signs.surr)
                     AND loan.filial-id EQ loan.filial-id
                     EXCLUSIVE-LOCK no-error.
   if not avail loan then do:
      message "Не найден депозит " entry(2,signs.surr)  view-as alert-box.
      return .
   end.   
   FIND FIRST loan-cond
   	WHERE loan-cond.contract EQ loan.contract
          AND loan-cond.cont-code EQ loan.Cont-Code
          EXCLUSIVE-LOCK NO-ERROR.
   if not avail loan-cond then do:
      message "Для договора "  loan.Cont-Code " не найдены условия." view-as alert-box.
      return .    
   end.

/* 
   Проверить условия депозита.
   1. Инн документа совпадает и Инн клиента
*/
   binn = "".
   if loan.cust-cat = "Ю" then do:
      find first cust-corp where cust-corp.cust-id = loan.cust-id no-lock no-error.
      if avail cust-corp then do:
         binn = cust-corp.inn.
      end.
   end.
   if loan.cust-cat = "Ч" then do:
      find first person where person.person-id = loan.cust-id no-lock no-error.
      if avail person then do:
         binn = person.inn.
      end.
   end.
   if binn <> bInnDoc then do:
      message "Для документа номер "  op.doc-num " ИНН документа " bInnDoc " не совпадает с ИНН клиента "  binn view-as alert-box.
      return .    
   end.

/*
   Проверить условия депозита.
   2. в справочникезн отсрочку
*/

/* Отсрочка  есть на договоре */

   bSrok = GetXattrValueEx("loan",bSurr,"КнтСрокПост",?).
   if bSrok = ? then do:
     message "Не найден ДР КнтСрокПост для договора " loan.Cont-Code  view-as alert-box.
     return .
   end.

/*
   Проверить условия депозита.
   3. проверить что взнос ещё возможен, т.е. open-date + отсрочка >= дате документа.
*/

   bDataSogl = GetXattrValueEx("loan",bSurr,"ДатаСогл",?).
   if bDataSogl = ? then do:
     message "Не найден ДР ДатаСогл для договора " loan.Cont-Code  view-as alert-box.
     return .
   end.

/*
   Определение количества рабочих дней.
*/

   datebeg_ = date(bDataSogl).
   j = 0.
   do i= 1 to op.op-date - date(bDataSogl) :
      iDate = datebeg_ + i.
      ASSIGN
        vWork = IsWorkDayBranch (iDate, "0100")
        vWork = IF  vWork EQ ? THEN NOT holidayru(iDate) ELSE vWork
      .
      if vWork then do:
         j = j + 1.
      end.
      if j > int(bSrok) then do:
         message "Срок договора истёк. Дата заключения " bDataSogl " отсрочка по продукту " bSrok " дата документа " op.op-date  view-as alert-box.
         return .       
      end.
   end.
/*
   find last  comm-rate where  comm-rate.kau        = "Депоз," + loan.Cont-Code
                          and  comm-rate.commission = "НеснОст"
                     no-lock no-error.
   if not avail comm-rate  then do:
     message "Не найдена комиссия НеснОст для договора " loan.Cont-Code  view-as alert-box.
     return .
   end.
   if  op-entry.amt-rub < comm-rate.rate-comm then do:
     message "Сумма "  op-entry.amt-rub " документа  " op.op  " меньше неснижаемого остатка НеснОст "  comm-rate.rate-comm   view-as alert-box.
     return .
   end.
*/

   find first term-obl where term-obl.contract  = loan.contract
                         and term-obl.idnt      = 2
                         and term-obl.cont-code = loan.cont-code 
                         and term-obl.end-date  = loan.open-date   
                         EXCLUSIVE-LOCK no-error.
   if not avail term-obl then do:
      message "Для договора "  loan.Cont-Code " не найдена сумма договора." view-as alert-box.
      return .    
   end.

   disable triggers for load of loan.
   disable triggers for load of loan-cond.
   disable triggers for load of term-obl.
   for each term-obl where term-obl.contract  = loan.contract
                       and term-obl.cont-code = loan.cont-code 
                       and term-obl.end-date  = loan.open-date   
                       EXCLUSIVE-LOCK .
      ASSIGN
         term-obl.fop-date  = op.op-date   
         term-obl.end-date  = op.op-date  
         term-obl.amt-rub   = op-entry.amt-rub  
         term-obl.dsc-beg-date = op.op-date
      .
   end.
   ASSIGN
      loan-cond.since = op.op-date
   .
   b-end-date = loan.end-date + (op.op-date - loan.open-date).
   ASSIGN
      loan.end-date    = b-end-date  
      loan.open-date   = op.op-date                	
      loan.since       = op.op-date                	
      loan.l-int-date  = op.op-date                	
   .
   for each bsigns where bsigns.file = "loan-cond"
                     and bsigns.surr begins loan.contract + "," + loan.cont-code 
                   .
       find first signs where recid(bsigns) eq recid(signs) no-error.
       if avail signs and  date(entry(3,signs.surr)) <> loan.open-date then do:
          signs.surr = loan.contract + "," + loan.cont-code  + "," + string(op.op-date).
       end.
   end.
   vRecLoan       = recid(loan).     
   vRecCond       = recid(loan-cond).   
   vtt-term-amt   = op-entry.amt-rub.
   iMode          = 2       .   
   vChangeSumm    = yes     .   
   vChangePr      = yes     .   
   vChangeDate    = no      .   
   vChangePer     = yes     .   
   vCondCount     = 1       .
   RUN mm-to.p(vRecLoan,
               vRecCond,
               vtt-term-amt,
               iMode,
               vChangeSumm,
               vChangePr  ,
               vChangeDate,
               vChangePer,
               ?,
               vCondCount) NO-ERROR.

   pick-value = bSurr.
   disable triggers for load of comm-rate.
   for each comm-rate where comm-rate.kau begins "Депоз," + loan.Cont-Code
                        and comm-rate.since = date(bDataSogl)
                      .
      comm-rate.since = op.op-date.
   end.
return. 
   
   
   
   
   
   