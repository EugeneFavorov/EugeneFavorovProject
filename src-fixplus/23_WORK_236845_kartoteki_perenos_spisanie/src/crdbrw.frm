FORM
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL ". "
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      COLUMN-LABEL "                     "
WITH FRAME browse1 TITLE COLOR BRIGHT-WHITE
   "[  " + STRING(mAcct) +  ":"  +
   "    ]"
   WIDTH 120.

FORM
   str-recid
     HELP "àźĄ„« - Żźáâ ąšâì/ăĄà âì źâŹ„âȘă"
   opb.Order-Pay
      COLUMN-LABEL ""
      FORMAT "x(2)"
      HELP "ç„à„€­źáâì Ż« â„Š  Ą « ­áźąźŁź €źȘăŹ„­â "
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL ". "
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      FORMAT ">>>,>>>,>>>,>>9.99" 
      COLUMN-LABEL "                  "
   mAmountForWriteOff
      FORMAT ">>>,>>>,>>>,>>9.99"
      COLUMN-LABEL "źáâăŻ­ź!€«ï áŻšá ­šï"
   mBlockType
      COLUMN-LABEL "«źȘšàźąȘ "
WITH FRAME browse2 TITLE COLOR BRIGHT-WHITE
   "[  " + STRING(mAcct) +  ":"  +
   "    ]"
   WIDTH 120.
