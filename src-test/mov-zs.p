   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {intrface.get date}
   {tmprecid.def}
   {prn-doc.def &with_proc=YES}
   {lshpr.pro} 
   {t-otch.i NEW}
   
   {svarloan.def NEW}
   {loan.pro}
   {intrface.get comm}
   {intrface.get instrum}
   {intrface.get card}
   
   {intrface.get cust}
   {intrface.get tmess}
   {sh-defs.i new}
   {ksh-defs.i NEW}
   {intrface.get terr}
   
   DEF VAR vOK      AS LOGICAL NO-UNDO.
   DEF VAR oName    AS CHAR    no-undo.
   def var ListIskl as char    no-undo.
   def var mNameIn   as char    no-undo.
   def var mNameBase as char    no-undo.
   def var mNameBaseShort as char    no-undo.
   def var KolList   as int     no-undo.
   def var i         as int     no-undo.
   def var mPercLen  as int     no-undo.  
   def var mPercWord as int     no-undo.  


   def var mtoday          as date    no-undo.
   def var iBranch         as char    no-undo.
   DEF var user_           AS CHAR    NO-UNDO.
   def var mList-cr        as char    no-undo.
   def var mList-acct-cr   as char    no-undo.
   def var mList-d         as char    no-undo.
   def var mList-v         as char    no-undo.
   def var bacct-rec       as char    no-undo.
   def var bInnDoc         as char    no-undo.
   def var bInnSend        as char    no-undo.
   def var bSurr           as char    no-undo.
   def var binn            as char    no-undo.
   def var bmail           as char    no-undo.
   def var bmailfull       as char    no-undo.
   def var Str-err         as char    no-undo.
   def var Str-time-stamp  as char    no-undo.

   mPercLen  = 90.
   mPercWord = 80.
   listIskl = "индивидуальный,предприниматель".
   bmail = "v.ignatchenko,I.Kanygina".
/*
   bmail = "v.ignatchenko".
*/
   mList-cr = "47416".                               
   mList-acct-cr = "420,421,422,523".                               
   mList-v       = "523".                               
   mList-d       = "420,421,422".                               
   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   mtoday = date(entry(1,in-str,"|")).
   bmailfull  = entry(2,in-str,"|").
   bmail = bmailfull.
   output to "/data/home/vignatchenko/exp/protocol.txt" append.

   export  string(time,"hh:mm:ss PM") mtoday bmail .

   FOR EACH op  WHERE op.op-date   eq mtoday
                  and op.op-status eq "В"  ,
                  FIRST op-entry WHERE op-entry.op EQ op.op 
                                   AND CAN-DO(mList-cr,substr(op-entry.acct-cr,1,5)) 
      .
      bacct-rec = GetXattrValueEx("op",STRING(Op.op),"acct-rec",?). 
      if bacct-rec = ? then do:
        Str-err = string(time,"hh:mm:ss PM") + " Не найден ДР acct-rec для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id.
        Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
        if Str-time-stamp eq ? then do:
           RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
           UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
        end.
        next .
      end.   
      if CAN-DO(mList-acct-cr,substr(bacct-rec,1,3)) then do:
         if CAN-DO(mList-v,substr(bacct-rec,1,3)) then do:
            ASSIGN
               op.op-status          = "ЗСВ"
               op-entry.op-status    = "ЗСВ"
           .
         end.
         if CAN-DO(mList-d,substr(bacct-rec,1,3)) then do:
            bInnDoc = GetXattrValueEx("op",STRING(Op.op),"inn-rec",?).
            if bInnDoc = ? then do:
              Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
              if Str-time-stamp eq ? then do:
                 Str-err = string(time,"hh:mm:ss PM") + " Не найден ДР inn-rec для документа "  + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id.
                 RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                 UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
              end.
              next .
            end.   
            bInnSend = op.inn.
            find first acct where acct.acct begins bacct-rec no-lock no-error.
            if avail acct then do:
              Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
              if Str-time-stamp eq ? then do:
                 Str-err =  string(time,"hh:mm:ss PM") + " Счет " + acct.acct + " уже открыт " + string(acct.open-date) + " для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id. 
                 RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                 UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
              end.
              next .
            end.   
            find first signs where signs.file = "loan"
                               and signs.code = "acct-dep"
                               and signs.code-val begins bacct-rec
                               no-lock no-error.
            if not avail signs then do:
              Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
              if Str-time-stamp eq ? then do:
                 Str-err =  string(time,"hh:mm:ss PM") + "Найти депозит невозможно. Не найден  ДР acct-rec, в котором был бы указан счёт "  + bacct-rec + 
                            " из ДР acct-rec "  + " для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id.
                 RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                 UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
              end.
              next .
            end.   
            bSurr = signs.surr.
            find first loan where   loan.close-date EQ ?     
                              AND loan.contract EQ 'Депоз' 
                              and loan.cont-code = entry(2,signs.surr)
                              AND loan.filial-id EQ loan.filial-id
                              EXCLUSIVE-LOCK no-error.
            if not avail loan then do:
               Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
               if Str-time-stamp eq ? then do:
                  Str-err = string(time,"hh:mm:ss PM") + " Не найден депозит " + entry(2,signs.surr) + " для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id. 
                  RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                  UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
               end.
               next .
            end.   

            binn = "".
            if loan.cust-cat = "Ю" then do:
               find first cust-corp where cust-corp.cust-id = loan.cust-id no-lock no-error.
               if avail cust-corp then do:
                  binn = cust-corp.inn.
                  mNameBase      = cust-corp.name-corp.
                  mNameBaseShort = cust-corp.name-short.
               end.
            end.
            if loan.cust-cat = "Ч" then do:
               find first person where person.person-id = loan.cust-id no-lock no-error.
               if avail person then do:
                  binn = person.inn.
                  mNameBase      = person.name-last + " " + person.first-names.
                  mNameBaseShort = mNameBase.
               end.
            end.
            if binn <> bInnDoc then do:
               Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
               if Str-time-stamp eq ? then do:
                  Str-err = string(time,"hh:mm:ss PM") + " ИНН документа " + bInnDoc + 
                            " не совпадает с ИНН клиента " + binn  + " для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id.
                  RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                  UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
               end.
               next.    
            end.

/* Сравнение наименований */

            mNameIn = GetXattrValueEx("op",STRING(Op.op),"name-rec","").

            kolList = num-entries(listIskl).
            do i = 1 to kolList.
               mNameIn = replace(mNameIn,entry(i,listIskl),"").
            end.

            vOK = CompareNameFast (INPUT  mNameBase,
                                   INPUT  mNameIn,
                                   INPUT  mPercLen ,
                                   INPUT  mPercWord   
                                   ).
            
            IF vOK NE YES THEN vOK = CompareNameSlow (INPUT  mNameBase,
                                   INPUT  mNameIn,
                                   INPUT  mPercLen,
                                   INPUT  mPercWord   
                                   ).
            IF vOK NE YES THEN vOK = CompareNameFast (INPUT  mNameBaseShort,
                                   INPUT  mNameIn,
                                   INPUT  mPercLen ,
                                   INPUT  mPercWord   
                                   ).
            
            IF vOK NE YES THEN vOK = CompareNameSlow (INPUT  mNameBaseShort,
                                   INPUT  mNameIn,
                                   INPUT  mPercLen,
                                   INPUT  mPercWord   
                                   ).
            
            if vOK NE YES then do:
               Str-time-stamp = GetXattrValueEx("op",STRING(Op.op),"time-stamp",?). 
               if Str-time-stamp eq ? then do:
                  Str-err = string(time,"hh:mm:ss PM") + " Есть ошибки в наименовании Клиента"  + " для документа " + string( Op.op ) + " ( " + op.doc-num  + " ) "  + " филиала " + op.filial-id.
                  RUN pb_mail.p (bmail, "Error-status for deposit", Str-err,"").
                  UpdateSigns(op.class-code, STRING(op.op), "time-stamp", string(time,"hh:mm:ss PM") , ?). 
               end.
               next.    
            end.
            ASSIGN
               op.op-status          = "ЗСД"
               op-entry.op-status    = "ЗСД"
/*               loan.loan-status      = "ПРД" */
            .
            if bmail <> "" then do:
               RUN pb_mail.p (bmailfull, "Новый документ " + op.doc-num + " ЗСД! Подразделение: " + op.filial-id , "Посмотрите документ по счёту " + bacct-rec + " по договору "  + loan.cont-code  + " филиала " + op.filial-id,"").
            end. 
/*
*/
         end.
      end. 
   end.
   output close.
   return. 
   