   /* === ��䨪 ���⥦�� �� ��業⠬ === idnt=301 */ 
   /* �⠭���⭠� */
FORM
   term-obl.end-date 
      COLUMN-LABEL "����"
      HELP         "�������� ��� ������ ��業⮢"
   term-obl.dsc-beg-date
      COLUMN-LABEL "������"
      HELP         "��� ����砭�� ���⥦���� ��ਮ��"
      FORMAT       "99.99.9999"
   term-obl.amt
      COLUMN-LABEL "�����"
   mSumm-t 
      COLUMN-LABEL "������������ �������"
      HELP         "������襭�� ���⮪ �� ��業⠬"
      FORMAT ">>>,>>>,>>9.99"
   term-obl.sop-date 
      COLUMN-LABEL "���� ������"
      HELP         "��� 䠪��᪮� ������ ��業⮢"
WITH FRAME browse4 WIDTH 80 CENTERED
   TITLE COLOR bright-white "[ ������ �������� �� ��������� ]".

   /* === ������� ���⮪ === idnt=302 */
   /* �⠭���⭠� */
FORM 
   term-obl.end-date
      COLUMN-LABEL "����. ����"
      HELP         "�������� ��� �뤠� �।��"
   term-obl.amt
      COLUMN-LABEL "�����"
WITH FRAME browse2 WIDTH 40 CENTERED
   TITLE COLOR bright-white "[ �������� ������� ]".

   /* === �������� ����襭�� ���� === idnt=303 */
   /* �⠭���⭠� */
FORM
   term-obl.fop-date
      FORMAT "99.99.99"
   term-obl.end-date
      COLUMN-LABEL "���� �"
      FORMAT "99.99.99"
   term-obl.dsc-beg-date
      COLUMN-LABEL "��"
      FORMAT "99.99.99"
      HELP "��� ����砭�� ���⥦���� ��ਮ��"
   term-obl.amt-rub
      COLUMN-LABEL "�������� �����"
   mSumm-t
      FORMAT       ">>,>>>,>>>,>>9.99"
      COLUMN-LABEL "�������. �������"
      HELP         "������襭�� ���⮪ �� ��筮�� ��易⥫����"
   term-obl.sop-date
      FORMAT "99.99.99"
   mProl
      FORMAT       "�/"
      COLUMN-LABEL "����"
      HELP         "�ਧ��� �஫����樨 ��筮�� ��易⥫��⢠"
WITH FRAME browse3 
   /* WIDTH 80 CENTERED */
   TITLE COLOR bright-white "[ �������� ��������� ����� ]".

FORM
   term-obl.end-date
      COLUMN-LABEL "����"
      HELP "�������� ��� ������ �����ᨨ"
      FORMAT "99.99.9999"
   term-obl.dsc-beg-date
      COLUMN-LABEL "������"
      HELP "��� ����砭�� ���⥦���� ��ਮ��"
      FORMAT "99.99.9999"
   term-obl.nn
      COLUMN-LABEL "���"
      FORMAT ">>>9"
      HELP "��� ��ࠬ��� �����ᨨ"
   mParName
      COLUMN-LABEL "�������� ���������"
      FORMAT "x(19)"
      HELP "������������ ���� ��ࠬ��� �����ᨨ"
   term-obl.amt-rub
      COLUMN-LABEL "�����"
      FORMAT ">>>,>>>,>>9.99"
      HELP "�㬬� ��易⥫��� �� ����� �����ᨨ"
   mSumm-t
      COLUMN-LABEL "�������. �������"
      FORMAT ">>>,>>>,>>9.99"
      HELP "������襭�� ���⮪ �� �����ᨨ"
WITH FRAME browse1 WIDTH 80
TITLE COLOR bright-white "[ ������ �������� �� ��������� ]".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='10/12/2014 15:11:25.113+04:00' */
/* $LINTFILE='brw-toblcm.frm' */
/*prosignDiI3rhcYtXv11A7hyhrqUQ*/