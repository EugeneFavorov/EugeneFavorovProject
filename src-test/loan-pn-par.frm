FORM
   mCustINN
      FORMAT       "x(13)"
      COLUMN-LABEL "���"
      HELP         "��� �����⥫�"
   mAcct
      FORMAT       "x(20)"
      COLUMN-LABEL "���"
      HELP         "��� �����⥫�"
   mReceiver
      FORMAT       "x(20)"
      COLUMN-LABEL "�����⥫�"
      HELP         "�����⥫� ���⥦�"
   mSymbol
      FORMAT       "x(2)"
      COLUMN-LABEL "������"
      HELP         "������ ���ᮢ��� �����"
   mPNTypeCode
      FORMAT       "x(13)"
      COLUMN-LABEL "����. ����"
      HELP         "������������ ���� ���⥦�"

WITH FRAME browse1 WIDTH 80 TITLE COLOR BRIGHT-WHITE "[ ��������� �������� ]".

FORM
   loan.cont-cli
      FORMAT       "x(15)"
      COLUMN-LABEL "����. ��ࠬ"
      HELP         "������������ ��ࠬ��� ���⥦�"
   mReceiver
      FORMAT       "x(18)"
      COLUMN-LABEL "�����⥫�"
      HELP         "�����⥫� ���⥦�"
   mAcct
      FORMAT       "x(20)"
      COLUMN-LABEL "���"
      HELP         "��� �����⥫�"
   mUseVedKvit
      FORMAT       "��/���"
      COLUMN-LABEL "����!���."
      HELP         "�ਧ��� ""������⢥���� ���⠭��"""
   mPNTypePay
      FORMAT       "x(13)"
      COLUMN-LABEL "��� ���⥦�"
      HELP         "��� ���⥦�"
WITH FRAME browse2 WIDTH 80 TITLE COLOR BRIGHT-WHITE "[ ��������� �������� ]".

FORM
   loan.cont-cli
      FORMAT       "x(15)"
      COLUMN-LABEL "����. ��ࠬ"
      HELP         "������������ ��ࠬ��� ���⥦�"
   mReceiver
      FORMAT       "x(15)"
      COLUMN-LABEL "�����⥫�"
      HELP         "�����⥫� ���⥦�"
   signs_value 
      FORMAT       "x(300)" 
      COLUMN-LABEL "signs" VIEW-AS FILL-IN SIZE 26 BY 1
WITH FRAME browse3 WIDTH 78 TITLE COLOR BRIGHT-WHITE "[ ��������� �������� ]".
