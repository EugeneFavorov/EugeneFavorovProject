
/* kam */
/* снимаем блокировку БлокВозвр, если есть */

{globals.i}
{sh-defs.i}

Define Input Parameter acctblock As CHAR. /* Номер счета с которого снимаем блокировку */
Define Output Parameter mSum As DECIMAL.

mSum = 0.

FOR EACH  BlockObject WHERE
          BlockObject.class-code   EQ 'BlockAcct'
      AND BlockObject.FILE-NAME    EQ 'acct'
      AND BlockObject.block-type   EQ 'БлокВозвр'
      AND BlockObject.surrogate    EQ acctblock + ','
      AND BlockObject.end-datetime EQ ?
:

ASSIGN
    BlockObject.end-datetime = BlockObject.beg-datetime. 

END.


