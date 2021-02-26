FORM
   DataLine.Sym3
      FORMAT       "x(12)"
      COLUMN-LABEL "ИНН"
      HELP         "ИНН поставщика"
   DataLine.Sym4
      FORMAT       "x(9)"
      COLUMN-LABEL "КПП"
      HELP         "КПП поставщика"
   Txt[3]
      FORMAT       "x(20)"
      COLUMN-LABEL "Счёт"
      HELP         "Счёт поставщика"
   Sym2
      FORMAT       "x(160)"
      COLUMN-LABEL "Наименование"
      HELP         "Наименование поставщика"
      VIEW-AS FILL-IN SIZE 21 BY 1
   Txt[2]
      FORMAT       "x(9)"
      COLUMN-LABEL "БИК"
      HELP         "БИК банка поставщика"
WITH FRAME browse1 TITLE COLOR bright-white " " WIDTH 77.
/******************************************************************************/
/* $LINTFILE='postjkh.frm' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.878+03:00' */
/*prosignPE22Z9dZZZGu4lMy3bIQEg*/