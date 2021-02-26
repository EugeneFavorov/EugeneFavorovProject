FORM
   long-acct
      LABEL "СЧЕТ (ВНЕБАЛАНС)"
      HELP  "Номер лицевого счета (внебаланс)."
   acct.currency
   acct-long
      LABEL "СЧЕТ (БАЛАНС)"
      HELP  "Соответствующий лицевой счет из балансовой категории."
   vBal-Acct
      FORMAT "x(24)"
      LABEL "ОСТАТОК (ВНЕБАЛАНС)"
WITH FRAME browse1 TITLE COLOR bright-white "[ ЛИЦЕВЫЕ СЧЕТА ]".

FORM
   long-acct
      LABEL "СЧЕТ (ВНЕБАЛАНС)"
      HELP  "Номер лицевого счета (внебаланс)."
   acct.currency
   vBal-Acct
      FORMAT "x(24)"
      LABEL "ОСТАТОК (ВНЕБАЛАНС)"
   Bal-Acct
      FORMAT "x(24)"
      LABEL "ОСТАТОК (БАЛАНС)"
WITH FRAME browse2 TITLE COLOR bright-white "[ ЛИЦЕВЫЕ СЧЕТА ]".

FORM
   long-acct
      LABEL "СЧЕТ (ВНЕБАЛАНС)"
      HELP  "Номер лицевого счета (внебаланс)."
   acct.currency
   name-cli
      LABEL "НАИМЕНОВАНИЕ ВЛАДЕЛЬЦА СЧЕТА"
      HELP  "Наименование владельца счета."
WITH FRAME browse3 TITLE COLOR bright-white "[ ЛИЦЕВЫЕ СЧЕТА ]".
&IF DEFINED(ENERGO-FRM-OFF) EQ 0 &THEN
FORM
   acct-long
      LABEL "СЧЕТ (БАЛАНС)"
      HELP  "Соответствующий лицевой счет из балансовой категории."
   Bal-Acct
      FORMAT "x(24)"
      LABEL "ОСТАТОК (БАЛАНС)"
   blk-type 
      FORMAT "x(15)"
      COLUMN-LABEL "Тип!блокировки"
      HELP "Тип блокировки балансового счета"
   blk-amt FORMAT "->>>,>>>,>>9.99" 
      COLUMN-LABEL "Сумма!блокировки"
      HELP "Сумма блокировки балансового счета"
   blk-cust FORMAT "x(7)"
      COLUMN-LABEL "Блок.!клиента"
      HELP "Наличие блокировки клиента"
WITH FRAME BROWSE4 TITLE COLOR BRIGHT-WHITE "[ ЛИЦЕВЫЕ СЧЕТА ]" WIDTH 135.
&ENDIF
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='14/11/2014 13:55:18.160+04:00' */
/* $LINTFILE='showacct.frm' */
/*prosignzSRAmU1gpvS9EmLbOjVa6A*/