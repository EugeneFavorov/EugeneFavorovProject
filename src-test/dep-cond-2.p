   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {intrface.get date}


   DEF var user_         AS CHAR    NO-UNDO.
   def var iBranch       as char    no-undo.
   def var bSurr         as char    no-undo.
   def var bSrok         as int     no-undo.
   def var iDate         as date    no-undo.
   def var iContract     as char    no-undo.
   def var iCont-Code    as char    no-undo.
   def var iProdSt       as char    no-undo.
   def var iProdStBase   as char    no-undo.

   DEF BUFFER bcomm-rate   FOR comm-rate.
   DEF BUFFER b2comm-rate   FOR comm-rate.
   DEF BUFFER bloan        FOR loan.
   DEF BUFFER bloan-cond   FOR loan-cond.
   DEF BUFFER b2loan-cond  FOR loan-cond.
   DEF BUFFER bsigns       FOR signs.
   DEF BUFFER b2signs      FOR signs.

   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
   iContract    = entry(1,in-str,"|").
   iCont-Code   = entry(2,in-str,"|").
   iProdSt      = entry(3,in-str,"|").
   iProdStBase  = "".
   if num-entries(in-str,"|") > 3 then do:
      iProdStBase  = entry(4,in-str,"|").
   end.
   pick-value = "".
   find first bloan where   bloan.close-date EQ ?     
                     AND bloan.contract  EQ iContract 
                     and bloan.cont-code =  iCont-Code
                     No-LOCK no-error.
   if not avail bloan then do:
      message "Не найден депозит " iCont-Code  view-as alert-box.
      return .
   end.   
   FIND FIRST bloan-cond
   	WHERE bloan-cond.contract  EQ bloan.contract
          AND bloan-cond.cont-code EQ bloan.Cont-Code
          NO-LOCK NO-ERROR.
   if not avail bloan-cond then do:
      message "Для договора "  bloan.Cont-Code " не найдены условия." view-as alert-box.
      return .    
   end.


   FOR each bcomm-rate WHERE bcomm-rate.filial-id eq bloan.filial-id
                        AND bcomm-rate.since LE bloan-cond.since
                        AND bcomm-rate.kau EQ "ПродТрф," + iProdStBase
                        AND bcomm-rate.commission EQ "ПополНеПоздней" 
                        USE-INDEX kau
                        NO-LOCK.
       bSrok = bloan.end-date - bloan.open-date.
       bSrok = int(bSrok * bcomm-rate.rate-comm).
       if bSrok = 0 then next.
       iDate = bloan.open-date + bSrok.
       create loan-cond.
       BUFFER-COPY bloan-cond  EXCEPT since TO loan-cond.
       loan-cond.since = iDate.
       /* Нужно скопировать кучу ДР на условии */
       FOR each b2signs WHERE b2signs.file-name = "loan-cond"
                          and b2signs.surrogate eq bloan-cond.contract + "," + bloan-cond.cont-code  + "," + string(bloan-cond.since)
                              NO-LOCK.
           create signs.
           BUFFER-COPY b2signs  EXCEPT surrogate  TO signs. 
           signs.surrogate = bloan-cond.contract + "," + bloan-cond.cont-code  + "," + string(iDate).
       end.
       find first term-obl where term-obl.contract    = bloan.contract
                             and term-obl.cont-code   = bloan.cont-code
                             and term-obl.end-date    = bloan.open-date
                             and term-obl.idnt        = 2
                             no-lock no-error .
       if avail term-obl then do:
          FIND first b2signs WHERE b2signs.file-name = "loan-cond"
                               and b2signs.surrogate eq bloan.contract + "," + bloan.cont-code  + "," + string(iDate)
                               and b2signs.code = "PaySum"
                               no-error.
          if avail b2signs then do:
              b2signs.dec-value   = term-obl.amt-rub.
              b2signs.code-value  = string(term-obl.amt-rub).
              b2signs.xattr-value = string(term-obl.amt-rub).
          end.
          else do:
             create b2signs.
             assign
                b2signs.file-name = "loan-cond"
                b2signs.code = "PaySum"
                b2signs.surrogate = bloan.contract + "," + bloan.cont-code  + "," + string(iDate) 
                b2signs.dec-value   = term-obl.amt-rub.
                b2signs.code-value  = string(term-obl.amt-rub).
                b2signs.xattr-value = string(term-obl.amt-rub).
             .
          end.
          FIND first b2signs WHERE b2signs.file-name = "loan-cond"
                               and b2signs.surrogate eq bloan.contract + "," + bloan.cont-code  + "," + string(bloan.open-date)
                               and b2signs.code = "PaySum"
                               no-error.
          if avail b2signs then do:
              b2signs.dec-value   = term-obl.amt-rub.
              b2signs.code-value  = string(term-obl.amt-rub).
              b2signs.xattr-value = string(term-obl.amt-rub).
          end.
          else do:
             create b2signs.
             assign
                b2signs.file-name = "loan-cond"
                b2signs.code = "PaySum"
                b2signs.surrogate = bloan.contract + "," + bloan.cont-code  + "," + string(bloan.open-date) 
                b2signs.dec-value   = term-obl.amt-rub.
                b2signs.code-value  = string(term-obl.amt-rub).
                b2signs.xattr-value = string(term-obl.amt-rub).
             .
          end.
       end.
       FIND first b2signs WHERE b2signs.file-name = "loan-cond"
                            and b2signs.surrogate eq bloan.contract + "," + bloan.cont-code  + "," + string(iDate)
                            and b2signs.code = "PayType"
                            no-error.
       if avail b2signs then do:
           b2signs.code-value    = "Остаток".
           b2signs.xattr-value   = "Остаток".
       end.
       else do:
          create b2signs.
          assign
             b2signs.file-name = "loan-cond"
             b2signs.code = "PayType"
             b2signs.surrogate = bloan.contract + "," + bloan.cont-code  + "," + string(iDate) 
             b2signs.code-value    = "Остаток".
             b2signs.xattr-value   = "Остаток".
          .
       end.



       /* Нужно скопировать ставки на условии */
       FOR each b2comm-rate WHERE b2comm-rate.filial-id eq bloan.filial-id
                              AND b2comm-rate.since     EQ bloan-cond.since
                              AND b2comm-rate.kau       EQ bloan.contract + "," + bloan.cont-code
                              NO-LOCK.
           create comm-rate.
           BUFFER-COPY b2comm-rate  EXCEPT since comm-rate-id TO comm-rate.
           comm-rate.since = iDate.

       end.
       FIND first b2comm-rate WHERE b2comm-rate.filial-id  eq bloan.filial-id
                               AND b2comm-rate.since      EQ iDate
                               AND b2comm-rate.kau        EQ bloan.contract + "," + bloan.cont-code
                               and b2comm-rate.commission EQ "МинДопВ"
                               no-error.
       if avail b2comm-rate then do:
           b2comm-rate.rate-comm  = 999999999999.99.
       end.
   end.

   pick-value = iCont-Code.

return. 
   
   
   
   
   
   