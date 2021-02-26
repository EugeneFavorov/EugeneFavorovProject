{globals.i} 
{tmprecid.def}

{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */


DEF VAR nn AS INT64 NO-UNDO.
def var kaucrstr as char no-undo.
def var kaudbstr as char no-undo.
def var idd as INT64 no-undo.
def var idk as INT64 no-undo.
def buffer bloan-int for loan-int.


FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

   find last loan-acct of loan where loan-acct.acct-type = 'КредРезВб' no-lock no-error.
   if avail loan-acct then do:
      for each op-entry where (op-entry.acct-db = loan-acct.acct or op-entry.acct-cr = loan-acct.acct)
		        and op-entry.op-date <> ?
              AND op-entry.op-status   >= "√"
              and trim(op-entry.kau-cr) = '' and trim(op-entry.kau-db) = '' exclusive-lock:
         nn = 1.
         kaucrstr = ' '.
         kaudbstr = ' '.
         if op-entry.acct-db = loan-acct.acct then do:
            kaudbstr = 'Кредит,' + loan.cont-code + ',321'.
            idd = 88.
            idk = 22.
         end.
         if op-entry.acct-cr = loan-acct.acct then do:
            kaucrstr = 'Кредит,' + loan.cont-code + ',320'.
            idd = 22.
            idk = 88.
         end.   

         op-entry.kau-cr = kaucrstr.
         op-entry.kau-db = kaudbstr.
         FIND LAST bloan-int WHERE bloan-int.contract = loan.contract
            AND bloan-int.cont-code = loan.cont-code 
            AND bloan-int.mdate <> ?
            AND bloan-int.mdate = op-entry.op-date NO-LOCK NO-ERROR.


         IF AVAIL bloan-int THEN nn = bloan-int.nn + 1.
         create loan-int.
            ASSIGN
            loan-int.op-date   = op-entry.op-date
            loan-int.mdate     = op-entry.op-date
            loan-int.id-k      = idk
            loan-int.id-d      = idd
            loan-int.op        = op-entry.op
            loan-int.op-entry  = op-entry.op-entry
            loan-int.amt-rub   = op-entry.amt-rub
            loan-int.nn        = DECIMAL(nn)
            loan-int.avt       = FALSE
             loan-int.contract = loan.contract
            loan-int.user-id   = 'SERVSOUZ'
            loan-int.cont-code = loan.cont-code
        .

      end.
   end.


find last loan-acct of loan where loan-acct.acct-type = 'КредВГар' no-lock no-error.
   if avail loan-acct then do:
      for each op-entry where (op-entry.acct-db = loan-acct.acct or op-entry.acct-cr = loan-acct.acct)
              and op-entry.op-date <> ?
              AND op-entry.op-status   >= "√"
              and trim(op-entry.kau-cr) = '' and trim(op-entry.kau-db) = '' exclusive-lock:
         nn = 1.
         kaucrstr = ' '.
         kaudbstr = ' '.
         if op-entry.acct-db = loan-acct.acct then do:
            kaudbstr = 'Кредит,' + loan.cont-code + ',139'.
            idd = ?.
            idk = 47.
         end.
         if op-entry.acct-cr = loan-acct.acct then do:
            kaucrstr = 'Кредит,' + loan.cont-code + ',138'.
            idd = 47.
            idk = ?.
         end.   

         op-entry.kau-cr = kaucrstr.
         op-entry.kau-db = kaudbstr.
         FIND LAST bloan-int WHERE bloan-int.contract = loan.contract
            AND bloan-int.cont-code = loan.cont-code 
            AND bloan-int.mdate <> ?
            AND bloan-int.mdate = op-entry.op-date NO-LOCK NO-ERROR.


         IF AVAIL bloan-int THEN nn = bloan-int.nn + 1.
         create loan-int.
            ASSIGN
            loan-int.op-date   = op-entry.op-date
            loan-int.mdate     = op-entry.op-date
            loan-int.id-k      = idk
            loan-int.id-d      = idd
            loan-int.op        = op-entry.op
            loan-int.op-entry  = op-entry.op-entry
            loan-int.amt-rub   = op-entry.amt-rub
            loan-int.nn        = DECIMAL(nn)
            loan-int.avt       = FALSE
             loan-int.contract = loan.contract
            loan-int.user-id   = 'SERVSOUZ'
            loan-int.cont-code = loan.cont-code
        .

      end.
   end.

end.
   message "создали, все что смогли" view-as alert-box.
