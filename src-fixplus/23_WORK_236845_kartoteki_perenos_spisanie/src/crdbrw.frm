FORM
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL "���. ��"
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      COLUMN-LABEL "                     "
WITH FRAME browse1 TITLE COLOR BRIGHT-WHITE
   "[ ���� " + STRING(mAcct) +  ":"  +
   " ��������� �� ��������� ]"
   WIDTH 120.

FORM
   str-recid
     HELP "�஡�� - ���⠢���/���� �⬥��"
   opb.Order-Pay
      COLUMN-LABEL "��"
      FORMAT "x(2)"
      HELP "��।����� ���⥦� �����ᮢ��� ���㬥��"
   kau1
      COLUMN-LABEL " "
   kau2
      COLUMN-LABEL " "
   kau3
      COLUMN-LABEL " "
   mDocDate
      FORMAT       "99/99/99"
      COLUMN-LABEL "���. ��"
   mDocNum
      FORMAT        "x(9)"
   kau.balance
      FORMAT ">>>,>>>,>>>,>>9.99" 
      COLUMN-LABEL "                  "
   mAmountForWriteOff
      FORMAT ">>>,>>>,>>>,>>9.99"
      COLUMN-LABEL "����㯭�!��� ᯨᠭ��"
   mBlockType
      COLUMN-LABEL "�����஢��"
WITH FRAME browse2 TITLE COLOR BRIGHT-WHITE
   "[ ���� " + STRING(mAcct) +  ":"  +
   " ��������� �� ��������� ]"
   WIDTH 120.
