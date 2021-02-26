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

DEFINE INPUT PARAMETER exmask as char no-undo.

DEFINE VARIABLE f-name as char no-undo.
DEFINE VARIABLE f-nameImp as char no-undo.
DEFINE VARIABLE icnt AS INT NO-UNDO.
/* список согласован с Мухиной 29.06.2015 */
DEFINE VARIABLE acctmask as char init "30109*,405*,406*,407*,40802*,40807*,40821*,40817*,40820*,420*,421*,422*,4230*,4250*,4260*" no-undo.
pause 0.
/* find first printer no-lock no-error.  не знаю зачем - иначе ругается */

{globals.i}

{sh-defs.i new}

{getdates.i}

{setdest.i}

  put unformatted
    "          Ведомость сообщений об изменениях реквизитов счетов за период с " beg-date " по " end-date skip
/*    "              ( " + acctmask + " исключая транзитные)" skip
    "              с фильтром " + exmask skip(1) */
    .
icnt = 0.

/* Сообщения о изменении реквизитов счета*/    
  put unformatted
    "Счет" format "x(21)"
    "д.откр" format "x(9)"
    "д.сообщ" format "x(9)"
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

  FOR EACH PackObject
     where 
      PackObject.file-name EQ "acct",
      FIRST packet 
      where 
      packet.filial-id EQ shFilial
      AND Packet.PacketID EQ PackObject.PacketID 
      AND Packet.PackDate GE beg-date
      AND Packet.PackDate LE end-date
      AND Packet.mail-format EQ "XFNSAcctCh" NO-LOCK
      BY Packet.PackDate DESC BY Packet.PackTime DESC:
            FIND FIRST acct
              WHERE acct.filial-id EQ shFilial
              AND acct.acct EQ ENTRY(1, PackObject.Surrogate)
              AND acct.currency EQ ENTRY(2, PackObject.Surrogate)
              NO-LOCK NO-ERROR.

            FIND reference 
              WHERE reference.PacketID EQ PackObject.PacketID 
              AND reference.class-code EQ "RTaxEXP" 
              NO-LOCK NO-ERROR.
              f-name = (if avail reference then reference.RefValue else "" ).
    RUN mesacct(BUFFER acct, f-name, ?).
    icnt = icnt + 1.

 END. 
  put unformatted
    "--------------------" format "x(21)"
    "--------" format "x(9)"
    "--------" format "x(9)"
    "------------------------------------------------------"   format "x(56)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    "---------- " format "x(10)"
    skip.

  put unformatted
    "  итого " string(icnt) skip.

{preview.i}

procedure mesacct.
   DEFINE PARAMETER buffer acct for acct.
   DEFINE INPUT PARAMETER f-name as char.
   DEFINE INPUT PARAMETER IsOpened as log.

   DEFINE VARIABLE f-nameImp as char no-undo.
   DEFINE VARIABLE f-nameImpE as char no-undo.
   DEFINE BUFFER referenceF FOR reference.
   DEFINE BUFFER packettextF FOR packettext.
   DEFINE BUFFER referenceP FOR reference.
   DEFINE BUFFER packettextP FOR packettext.
   DEFINE BUFFER referenceR FOR reference.
   DEFINE BUFFER packettextR FOR packettext.

  f-nameImp = CAPS(f-name).
  f-nameImpE = CAPS(f-name).
  if f-nameImp <> "" then substring(f-nameImp,3,1) = "F".
  if f-nameImpE <> "" then substring(f-nameImpE,3,1) = "E".
  if f-nameImp <> "" then do:

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
    (IF IsOpened EQ ? THEN 
      string( acct.open-date, "99/99/99") + " " +
      string( packet.PackDate, "99/99/99") + " "
    ELSE
    (if IsOpened then string( acct.open-date, "99/99/99") else "        ") + " " +
    (if not IsOpened then string( acct.close-date, "99/99/99") else "        ") + " "
    )
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
