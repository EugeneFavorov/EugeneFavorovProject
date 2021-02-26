/*
   Назначение  : Отчет о сообщения об открытии/закрытии счетов за период
   Параметры   :

  Используемые 
  инклюд-файлы :

  Используется
  в процедурах :

   Создание    :

   Исправление :
*/

def input param exmask as char no-undo.

def var f-name as char no-undo.
def var f-nameImp as char no-undo.
/* список согласован с Мухиной 29.06.2015 */
def var acctmask as char init "30109*,30111*,405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*" no-undo.
pause 0.
/* find first printer no-lock no-error.  не знаю зачем - иначе ругается */

{globals.i}

{sh-defs.i new}

procedure mesacct.
   def parameter buffer acct for acct.
   def input parameter f-name as char.
   def input parameter IsOpened as log.

   def var f-nameImp as char no-undo.
   def var f-nameImpE as char no-undo.
   DEF BUFFER referenceF FOR reference.
   DEF BUFFER packettextF FOR packettext.
   DEF BUFFER referenceP FOR reference.
   DEF BUFFER packettextP FOR packettext.
   DEF BUFFER referenceR FOR reference.
   DEF BUFFER packettextR FOR packettext.

  f-nameImp = CAPS(f-name).
  f-nameImpE = CAPS(f-name).
  if f-nameImp <> "" then substring(f-nameImp,3,1) = "F".
  if f-nameImpE <> "" then substring(f-nameImpE,3,1) = "E".
  if f-nameImp <> "" then do:

    /*    for each PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID no-lock,
       
       BY Packet.PackDate DESC:*/

    find last referenceF where referenceF.class-code = "RTaxImp" and (CAPS(referenceF.RefValue) = f-nameImp OR CAPS(referenceF.RefValue) = f-nameImpE) no-lock no-error.
    if avail referenceF then
     find first packettextF where packettextF.PacketID = referenceF.PacketID no-lock no-error.
     else release packettextF.
  end.
  if f-nameImp <> "" then substring(f-nameImp,3,1) = "P".
  if f-nameImp <> "" then do:
    find last referenceP where referenceP.class-code = "RTaxImp" and referenceP.RefValue = f-nameImp no-lock no-error.
    if avail referenceP then
     find first packettextP where packettextP.PacketID = referenceP.PacketID no-lock no-error.
     else release packettextP.
  end.
  if f-nameImp <> "" then substring(f-nameImp,3,1) = "R".
  if f-nameImp <> "" then do:
    find last referenceR where referenceR.class-code = "RTaxImp" and referenceR.RefValue = f-nameImp no-lock no-error.
    if avail referenceR then
     find first packettextR where packettextR.PacketID = referenceR.PacketID no-lock no-error.
     else release packettextR.
  end.

  put unformatted
    acct.number " "
    (if IsOpened then string( acct.open-date, "99/99/99") else "        ") " "
    (if not IsOpened then string( acct.close-date, "99/99/99") else "        ") " "
    f-name format "x(55)" " "
    (if not avail referenceF
       then "?????"
       else (if avail packettextF
               then (if can-do("*╩юф╬°шсъш=~"000~"*",packettextF.Contents) OR can-do("*╩юф╬сЁ=~"1~"*",packettextF.Contents) OR can-do("*╩юф╬сЁ=~"1~"*",packettextF.Contents) OR can-do("*КодОшибки:000*",packettextF.Contents)
	               then CHR(251)
		       else "Ошибка") 
	       else "нет данных")) format "x(10)"
    (if not avail referenceP
       then "?????" 
       else (if avail packettextP
               then (if can-do("*╩юф╬°шсъш=~"000~"*",packettextP.Contents) OR can-do("*╩юф╬сЁ=~"1~"*",packettextP.Contents) OR can-do("*КодОшибки:000*",packettextF.Contents)
	               then CHR(251)
		       else "Ошибка") 
	       else "нет данных")) format "x(10)"
    (if not avail referenceR
       then "?????" 
       else (if avail packettextR
               then (if can-do("*╩юф╬°шсъш=~"000~"*",packettextR.Contents) OR can-do("*╩юф╬сЁ=~"1~"*",packettextR.Contents) OR can-do("*КодОшибки:000*",packettextF.Contents)
	               then CHR(251)
		       else "Ошибка") 
	       else "нет данных")) format "x(10)"
    skip.
end.

{getdates.i}

{setdest.i}

  put unformatted
    "          Ведомость сообщений об открытии/закрытии счетов за период с " beg-date " по " end-date skip
    "              ( " + acctmask + " исключая транзитные)" skip
    "              с фильтром " + exmask skip(1)
    .
  put unformatted
    "Счет" format "x(21)"
    "д.откр" format "x(9)"
    "д.закр" format "x(9)"
    "файл"   format "x(56)"
    "фнс" format "x(10)"
    "пфр" format "x(10)"
    "фсс" format "x(10)"
    skip.
  put unformatted
    "--------------------" format "x(21)"
    "--------" format "x(9)"
    "--------" format "x(9)"
    "------------------------------------------------------"   format "x(56)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    skip.
DEF VAR icnt AS INT NO-UNDO.
icnt = 0.
for each acct 
  where can-do(acctmask,acct.acct) and
       ((acct.open-date >= beg-date and acct.open-date <= end-date)
     or (acct.close-date >= beg-date and acct.close-date <= end-date))
     and (can-do( exmask, acct.acct))
     and acct.filial-id EQ shFilial
      no-lock
  by acct.open-date by acct.acct:
  if acct.contract EQ 'транз1' AND can-do("40701*,40702*,40703*,40802*,420*,421*,422*", acct.acct) THEN NEXT.
  if acct.contract EQ 'накоп'  AND can-do("40701*,40702*,40703*,40802*,420*,421*,422*", acct.acct) THEN NEXT.
/*    find last Packet where Packet.State = "ОТПР" by Packet.PacketDate no-error.*/

  if acct.open-date >= beg-date and acct.open-date <= end-date then do:
    f-name = ''.
    
    /*
    if acct.cust-cat EQ 'Ч' then
    for each PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID
       and can-do( "XFNSAcctOpen", Packet.mail-format) no-lock
       BY Packet.PackDate DESC:
      find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
      f-name = (if avail reference then reference.RefValue else "" ).
      LEAVE.
    end.
/*if acct.acct EQ "40817810004440001688     @0400"
then message "f-name=" + f-name view-as alert-box.*/
    if f-name EQ '' then
    for last PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct
       and can-do( "!ConfirmExpTaxClose,ConfirmExp*", PackObject.Kind)
        no-lock /*,
       first packet where Packet.PacketID EQ PackObject.PacketID
        no-lock*/ :
      find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
      f-name = (if avail reference then reference.RefValue else "" ).
    end.
    */

    for each PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID no-lock
       BY Packet.PackDate DESC:
	IF can-do( "XFNSAcctOpen", Packet.mail-format) /* XML format */
	  OR can-do( "!ConfirmExpTaxClose,ConfirmExp*", PackObject.Kind) /* old TXT format */
	  THEN DO:
    	    find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
	    f-name = (if avail reference then reference.RefValue else "" ).
	    LEAVE.
	END.
    END.
    run mesacct(buffer acct, f-name, True).
    icnt = icnt + 1.
  end.

  if acct.close-date >= beg-date and acct.close-date <= end-date then do:
    f-name = ''.
    /*
    if acct.cust-cat EQ 'Ч' then
    for each PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID
       and can-do( "XFNSAcctClose", Packet.mail-format) no-lock
       BY Packet.PackDate DESC:
      find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
      f-name = (if avail reference then reference.RefValue else "" ).
      LEAVE.
    end.
    if f-name EQ '' then
    for last PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct
       and can-do( "ConfirmExpTaxClose", PackObject.Kind)
        no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID
        no-lock:
      find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
      f-name = (if avail reference then reference.RefValue else "" ).
    end.
    */
    for each PackObject
     where file-name = "acct"
       and PackObject.Surrogate begins acct.acct and PackObject.file-name = 'acct' no-lock,
       first packet where Packet.PacketID EQ PackObject.PacketID no-lock
       BY Packet.PackDate DESC BY Packet.PackTime DESC:
	IF can-do( "XFNSAcctClose", Packet.mail-format) /* XML format */
	  OR can-do( "ConfirmExpTaxClose", PackObject.Kind) /* old TXT format */
	  THEN DO:
    	    find reference  where reference.PacketID = PackObject.PacketID and reference.class-code = "RTaxEXP" no-lock no-error.
	    f-name = (if avail reference then reference.RefValue else "" ).
	    LEAVE.
	END.
    END.
    run mesacct(buffer acct, f-name, False).
    icnt = icnt + 1.
  end.

end.
  put unformatted
    "--------------------" format "x(21)"
    "--------" format "x(9)"
    "--------" format "x(9)"
    "------------------------------------------------------"   format "x(56)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    skip.
/*
сообщения об изменении реквизитов счета
FOR EACH Packet 
 WHERE Packet.class-code BEGINS 'PTAX' AND Packet.Kind BEGINS 'TAX' AND

*/

  put unformatted
    "  итого " string(icnt) skip.

{preview.i}
