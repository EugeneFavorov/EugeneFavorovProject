def output parameter ndd as INT64 no-undo.
def output parameter buf-sum as decimal no-undo.

/*def var vsgn as char no-undo.*/
def var i as INT64 no-undo.
def var vstr as char initial "link-op-sum" no-undo.
def var zop as char no-undo.
def buffer yop for op.
def buffer yop-entry for op-entry.

loop:
    for each op-entry where op-entry.op-date eq cur-op-date and
                            can-do((if op-templ.op-status ne "" and 
                                       op-templ.op-status ne ? then 
                                      op-templ.op-status 
                                    else "*"), op-entry.op-status) and 
                            can-do(in-type, op-entry.type) and
                            (if op-templ.acct-db ne ? then
                             can-do(op-templ.acct-db, op-entry.acct-db)
                             else yes) and 
                            (if op-templ.acct-cr ne ? then
                             can-do(op-templ.acct-cr, op-entry.acct-cr)
                             else yes) 
                            no-lock,
        first op of op-entry where 
          (if op-templ.doc-type eq "" then 
            yes 
           else can-do(op-templ.doc-type, op.doc-type)) and
          (if op-templ.cr-class-code eq "" then yes
           else op-templ.cr-class-code eq op.class-code) no-lock
        by op.order-pay:
        
        {debug.i "swdbcr" op.doc-num}


        zop = GetXAttrValue("op",string(op.op),vstr).
        {debug.i "zop" zop}
        if zop ne "" then do:
           i = num-link("op",vstr,string(op.op)).
           
           {debug.i "i" i}
           if i ne 1 then next loop.
           else do:
              release yop-entry.
              find first yop where yop.op eq INT64(zop) no-lock no-error.
              if avail yop then find first yop-entry of yop no-lock no-error.
              {debug.i "avail yop" "avail yop"}
              if not avail yop-entry then next loop.
              {debug.i "avail yopen" "avail yop-entry"}
           end.
        end.
        else do:
              {debug.i "else"}
              find first yop where yop.op eq op.op no-lock no-error.
              find first yop-entry of yop no-lock no-error.
        end.

/* Замена Плюс банк
        {e-swift.cr &test=yes {&*} }
*/      {pbe-swift.cr &test=yes {&*} }
/* Конец замены Плюс банк */
        
        find first w-op no-lock no-error.
        if avail w-op then delete w-op.
        find first w-op-entry no-lock no-error.
        if avail w-op-entry then delete w-op-entry.
  /* Создание exp-temp-table запоминание документов подлежащих экспорту   */
  /* Создание временных таблиц и проверка на ошибки                       */
  /* Создание протокола ошибок                                            */

end. /* Окончание loop */

/*ndd   =  accum count op-entry.amt-rub.
buf-sum =  accum total op-entry.amt-rub.*/
