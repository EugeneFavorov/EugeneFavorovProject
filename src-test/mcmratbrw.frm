/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: mcmratbrw.frm
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 04.03.2010 11:38 Chumv   
     Modified: 04.03.2010 11:38 Chumv   
*/

FORM
   comm-rate.commission
      FORMAT 'x(15)'
   mZnak
      FORMAT 'x'
      COLUMN-LABEL "ЗНАК"
      HELP "Операция изменения ставки базового продукта (+/-)"
   comm-rate.rate-comm 
      FORMAT "->>>,>>>,>>>,>>>,>>9.99999"
   comm-rate.since
WITH FRAME browse1 TITLE COLOR bright-white "[ МОДИФИКАТОРЫ ДЛЯ " + CAPS(mKauModif) + " ]".
                            
FORM
   comm-rate.commission
      FORMAT 'x(15)'
   mZnak
      FORMAT 'x'
      LABEL "ЗНАК"
      HELP "Операция изменения ставки базового продукта (+/-)"
   comm-rate.rate-comm 
      FORMAT "->>>>>>9.99999"
   comm-rate.since
WITH FRAME edit WIDTH 50 SIDE-LABEL.
