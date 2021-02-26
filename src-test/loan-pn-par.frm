FORM
   mCustINN
      FORMAT       "x(13)"
      COLUMN-LABEL "ИНН"
      HELP         "ИНН получателя"
   mAcct
      FORMAT       "x(20)"
      COLUMN-LABEL "Счет"
      HELP         "Счет получателя"
   mReceiver
      FORMAT       "x(20)"
      COLUMN-LABEL "Получатель"
      HELP         "Получатель платежа"
   mSymbol
      FORMAT       "x(2)"
      COLUMN-LABEL "Символ"
      HELP         "Символ кассового плана"
   mPNTypeCode
      FORMAT       "x(13)"
      COLUMN-LABEL "Наим. вида"
      HELP         "Наименование вида платежа"

WITH FRAME browse1 WIDTH 80 TITLE COLOR BRIGHT-WHITE "[ ПАРАМЕТРЫ ПЛАТЕЖЕЙ ]".

FORM
   loan.cont-cli
      FORMAT       "x(15)"
      COLUMN-LABEL "Наим. парам"
      HELP         "Наименование параметра платежа"
   mReceiver
      FORMAT       "x(18)"
      COLUMN-LABEL "Получатель"
      HELP         "Получатель платежа"
   mAcct
      FORMAT       "x(20)"
      COLUMN-LABEL "Счет"
      HELP         "Счет получателя"
   mUseVedKvit
      FORMAT       "Да/Нет"
      COLUMN-LABEL "Квит!анц."
      HELP         "Признак ""ведомственная квитанция"""
   mPNTypePay
      FORMAT       "x(13)"
      COLUMN-LABEL "Вид платежа"
      HELP         "Вид платежа"
WITH FRAME browse2 WIDTH 80 TITLE COLOR BRIGHT-WHITE "[ ПАРАМЕТРЫ ПЛАТЕЖЕЙ ]".

FORM
   loan.cont-cli
      FORMAT       "x(15)"
      COLUMN-LABEL "Наим. парам"
      HELP         "Наименование параметра платежа"
   mReceiver
      FORMAT       "x(15)"
      COLUMN-LABEL "Получатель"
      HELP         "Получатель платежа"
   signs_value 
      FORMAT       "x(300)" 
      COLUMN-LABEL "signs" VIEW-AS FILL-IN SIZE 26 BY 1
WITH FRAME browse3 WIDTH 78 TITLE COLOR BRIGHT-WHITE "[ ПАРАМЕТРЫ ПЛАТЕЖЕЙ ]".
