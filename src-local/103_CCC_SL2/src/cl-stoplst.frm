
FORM
   str-recid            
      COLUMN-LABEL "�"
      HELP         "�"
   &IF DEFINED(LOOK-STOPLIST) <> 0 &THEN
   tt-look-sl.matchstr   
      COLUMN-LABEL  "�᪮��� ���祭��" 
      FORMAT "x(30)"
      HELP   "�᪮��� ���祭��"   
   &ENDIF
   code.description[1]   
      COLUMN-LABEL  "� �/�" 
      FORMAT "x(7)"
      HELP   "�����"
   code.misc[1]   
      COLUMN-LABEL  "���" 
      FORMAT "x(1)"   
      HELP "��� ������"
   code.misc[2]   
      COLUMN-LABEL  "���" 
      FORMAT "x(7)" 
      HELP "����७��� ����� ��� �����⮢ �����"
  code.misc[3]  
      COLUMN-LABEL  "���~/ ������������" 
      FORMAT "x(30)"
      HELP "���/������������"
   code.misc[4]   
      COLUMN-LABEL  "��࠭�"
      FORMAT "x(3)"
      HELP "�������� ��� ��࠭�"
   code.misc[5]   
      COLUMN-LABEL  "���"
      FORMAT "x(12)"
      HELP "���"
   code.misc[6]
      COLUMN-LABEL  "��ᯮ��" 
      FORMAT "x(10)"
      HELP "���� � ����� ���㬥�� 㤮�⮢����饣� ��筮���"
   mBirthDay
      COLUMN-LABEL  "��� ஦�����"
      FORMAT "99/99/9999"
      HELP "��� ஦�����"
   mBirthPlace
      COLUMN-LABEL  "���� ஦�����"
      FORMAT "x(30)"
      HELP "���� ஦�����"
    code.misc[7]
      COLUMN-LABEL  "�ਬ�砭��" 
      FORMAT "x(20)" 
      HELP "�ਬ�砭��"
   code.name
      COLUMN-LABEL  "�⮯-����" 
      FORMAT "x(20)"  
      HELP "��� �⮯-����"
      VIEW-AS FILL-IN SIZE 20 BY 1
   code.val
      COLUMN-LABEL  "����� १���樨" 
      FORMAT "x(20)"
      HELP "����� १���樨"
   mDataRez
      COLUMN-LABEL  "��� १���樨"
     FORMAT "99/99/9999"    
      HELP "��� १���樨"
   mDataAdd
      COLUMN-LABEL  "��� ����������"
      FORMAT "99/99/9999"
      HELP "��� ���������� ��ꥪ� � �⮯-����"
  mDataCls
      COLUMN-LABEL  "��� �������"
      FORMAT "99/99/9999"
      HELP "��� ������� �⮯-����"
  code.misc[8]
      COLUMN-LABEL  "���짮��⥫�"
      FORMAT "x(12)"    
      HELP "���짮��⥫�, �������訩/�������訩 ������ � �⮯-����"

WITH FRAME browse1 WIDTH 270 TITLE COLOR bright-white "[  ]".

FORM
   code.description[1]     
      LABEL  "� �/�" 
      FORMAT "x(7)" 
      HELP   "�����"
      AT 12 SKIP
   code.misc[1]    
      LABEL  "���" 
      VIEW-AS COMBO-BOX LIST-ITEMS "�", "�", "�"
      FORMAT "x(1)"  
      HELP "��� ������"
      AT 14 SKIP
   code.misc[2]   
      LABEL  "���" 
      FORMAT "x(7)"  
      HELP "��� ������"
      AT 14 SKIP
   code.misc[3]    
      LABEL  "���~/������������" 
      FORMAT "x(4000)"
      VIEW-AS EDITOR SIZE 50 BY 2  
      HELP  "������������"
      SKIP
   code.misc[4]   
      LABEL  "��࠭�"
      FORMAT "x(3)" 
      HELP "�������� ��� ��࠭�"
      AT 11 SKIP  
   code.misc[5]   
      LABEL  "���"
      FORMAT "x(12)"   
      HELP "���"
      AT  14   SKIP 
   code.misc[6]
      LABEL  "��ᯮ��" 
      FORMAT "x(50)"
      HELP "���� � ����� ���㬥�� 㤮�⮢����饣� ��筮���"
      AT  10   SKIP
   mBirthDay
      LABEL  "��� ஦�����"
      FORMAT "99/99/9999"    
      HELP "��� ஦�����"
      AT 4
   mBirthPlace
      LABEL  "���� ஦�����" 
      FORMAT "x(150)" 
      VIEW-AS FILL-IN SIZE 50 BY 1
      HELP "���� ஦�����"
      AT 3
   code.misc[7] 
      LABEL  "�ਬ�砭��" 
      FORMAT "x(4000)"
      VIEW-AS EDITOR SIZE 50 BY 2 
      HELP "�ਬ�砭��"
      AT  7   SKIP
   code.name
      LABEL  "�⮯-����"   
      FORMAT "x(20)"    
      HELP "��� �⮯-����"
      AT  8   SKIP
   code.val
      LABEL  "����� १���樨"  
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 50 BY 1
      HELP "����� १���樨"  
      AT 2   SKIP
   mDataRez 
      LABEL  "��� १���樨"  
      FORMAT "99/99/9999"  
      HELP  "��� १���樨"  
      AT  3  SKIP
   mDataAdd  
      LABEL  "��� ����������"    
      FORMAT "99/99/9999"
      HELP   "��� ���������� ��ꥪ� � �⮯-����"
      AT  2   SKIP
   mDataCls  
      LABEL  "��� �������"    
      FORMAT "99/99/9999"
      HELP   "��� ������� �⮯-����"
      AT  4   SKIP      
   code.misc[8]
      LABEL  "���짮��⥫�"   
      FORMAT "x(12)"    
      HELP "���짮��⥫�, �������訩/�������訩 ������ � �⮯-����"
      AT  5   SKIP 
WITH FRAME edit SIDE-LABELS CENTERED TITLE COLOR BRIGHT-WHITE "[  ]" .
/* $LINTFILE='cl-stoplst.frm' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.378+03:00' */
/*prosignhoi9vaXqjMwjpUYUiixjEw*/