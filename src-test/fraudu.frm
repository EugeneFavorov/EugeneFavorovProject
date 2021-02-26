FORM
   DataLine.Sym1
      FORMAT       "x(12)"
      COLUMN-LABEL "ИНН"
      HELP         "ИНН получателя"
   DataLine.Sym2
      FORMAT       "x(9)"
      COLUMN-LABEL "БИК"
      HELP         "БИК банка получателя"
   DataLine.Sym3
      FORMAT       "x(20)"
      COLUMN-LABEL "Счёт"
      HELP         "Счёт получателя"
   DataLine.Sym4
      FORMAT       "x(15)"
      COLUMN-LABEL "Наименование"
      HELP         "Наименование получателя"
   Txt[1]
      FORMAT       "x(15)"
      COLUMN-LABEL "Назначение"
      HELP         "Назначение"
WITH FRAME browse1 TITLE COLOR bright-white " " WIDTH 78.

