FORM
   long-acct
      LABEL "���� (���������)"
      HELP  "����� ��楢��� ��� (���������)."
   acct.currency
   acct-long
      LABEL "���� (������)"
      HELP  "���⢥�����騩 ��楢�� ��� �� �����ᮢ�� ��⥣�ਨ."
   vBal-Acct
      FORMAT "x(24)"
      LABEL "������� (���������)"
WITH FRAME browse1 TITLE COLOR bright-white "[ ������� ����� ]".

FORM
   long-acct
      LABEL "���� (���������)"
      HELP  "����� ��楢��� ��� (���������)."
   acct.currency
   vBal-Acct
      FORMAT "x(24)"
      LABEL "������� (���������)"
   Bal-Acct
      FORMAT "x(24)"
      LABEL "������� (������)"
WITH FRAME browse2 TITLE COLOR bright-white "[ ������� ����� ]".

FORM
   long-acct
      LABEL "���� (���������)"
      HELP  "����� ��楢��� ��� (���������)."
   acct.currency
   name-cli
      LABEL "������������ ��������� �����"
      HELP  "������������ �������� ���."
WITH FRAME browse3 TITLE COLOR bright-white "[ ������� ����� ]".
&IF DEFINED(ENERGO-FRM-OFF) EQ 0 &THEN
FORM
   acct-long
      LABEL "���� (������)"
      HELP  "���⢥�����騩 ��楢�� ��� �� �����ᮢ�� ��⥣�ਨ."
   Bal-Acct
      FORMAT "x(24)"
      LABEL "������� (������)"
   blk-type 
      FORMAT "x(15)"
      COLUMN-LABEL "���!�����஢��"
      HELP "��� �����஢�� �����ᮢ��� ���"
   blk-amt FORMAT "->>>,>>>,>>9.99" 
      COLUMN-LABEL "�㬬�!�����஢��"
      HELP "�㬬� �����஢�� �����ᮢ��� ���"
   blk-cust FORMAT "x(7)"
      COLUMN-LABEL "����.!������"
      HELP "����稥 �����஢�� ������"
WITH FRAME BROWSE4 TITLE COLOR BRIGHT-WHITE "[ ������� ����� ]" WIDTH 135.
&ENDIF
/* $LINTUSER='BIS' */
/* $LINTENV ='energo' */
/* $LINTVSS ='$/ws3-dpl/energo/bq/' */
/* $LINTDATE='14/11/2014 13:55:18.160+04:00' */
/* $LINTFILE='showacct.frm' */
/*prosignzSRAmU1gpvS9EmLbOjVa6A*/