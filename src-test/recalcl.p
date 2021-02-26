/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:
         Uses:
      Used by:
      Created:
     Modified:
*/

{globals.i}
{intrface.get cdrep}
{intrface.get pint}

def input param datepereschet as date no-undo.
define input parameter ipPotok as INT64 no-undo.
define input parameter ipFilial as CHARACTER no-undo.
define input parameter ipDelim as INT64 no-undo.

put unformatted "recalc - " STRING(TIME,"HH:MM:SS") " " ipFilial skip.


for each loan
 where loan.contract EQ "кредит"
  and loan.close-date = ?
  and loan.filial-id EQ ipFilial
  and INT64(RECID(loan)) MODULO ipDelim EQ ipPotok  NO-LOCK: /*1*/

    FIND FIRST loan-int OF loan NO-LOCK NO-ERROR.
    IF loan.since <> datepereschet AND
     AVAIL loan-int then 
    DO: /*2*/
/*
put unformatted loan.doc-ref skip.
*/

      RUN l-calc2.p ("Кредит",       /* Назначение договора. */
             loan.cont-code,      /* Номер договора. */
             date(datepereschet),   /* Окончание договора + день для выполнения автом. */
             FALSE,   /* включать/не включать пересчет течений договора */
             TRUE).   /* выводить/ не выводить протокол на экран */


/*      put unformatted loan.cont-code ' ' STRING(TIME,"HH:MM:SS") " " ipFilial skip. */
    END. /*2*/
END.


