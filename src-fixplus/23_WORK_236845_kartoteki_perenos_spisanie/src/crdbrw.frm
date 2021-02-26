FORM
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL "ДОК. ОТ"
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      COLUMN-LABEL "                     "
WITH FRAME browse1 TITLE COLOR BRIGHT-WHITE
   "[ СЧЕТ " + STRING(mAcct) +  ":"  +
   " ДОКУМЕНТЫ НА КАРТОТЕКЕ ]"
   WIDTH 120.

FORM
   str-recid
     HELP "Пробел - поставить/убрать отметку"
   opb.Order-Pay
      COLUMN-LABEL "ОП"
      FORMAT "x(2)"
      HELP "Очередность платежа балансового документа"
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL "ДОК. ОТ"
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      FORMAT ">>>,>>>,>>>,>>9.99" 
      COLUMN-LABEL "                  "
   mAmountForWriteOff
      FORMAT ">>>,>>>,>>>,>>9.99"
      COLUMN-LABEL "Доступно!для списания"
   mBlockType
      COLUMN-LABEL "Блокировка"
WITH FRAME browse2 TITLE COLOR BRIGHT-WHITE
   "[ СЧЕТ " + STRING(mAcct) +  ":"  +
   " ДОКУМЕНТЫ НА КАРТОТЕКЕ ]"
   WIDTH 120.
