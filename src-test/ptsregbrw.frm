/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ptsregbrw.FRM
      Comment: ��������(ॣ������) ���㬥�⮢ - ���
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:    
*/

FORM
   DateValue
      COLUMN-LABEL "�ப(���)"
      FORMAT       "99/99/9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "�ப(���)"
   NameEvent
      COLUMN-LABEL "����⨥"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "����⨥"
   Branch_
      COLUMN-LABEL "���"
      FORMAT       "x(4)"
      VIEW-AS FILL-IN SIZE 4 BY 1
      HELP         "���"
   Details
      COLUMN-LABEL "�������ਨ"
      FORMAT       "x(40)"
      VIEW-AS FILL-IN SIZE 40 BY 1
      HELP         "�������ਨ"
   Descriptions
      COLUMN-LABEL "�ਭ�"
      FORMAT       "x(40)"
      VIEW-AS FILL-IN SIZE 20 BY 1
      HELP         "�ਭ�"      
   DateChange
      COLUMN-LABEL "��� ���������"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "��� ���������"
   User_Id
      COLUMN-LABEL "���짮�."
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "���짮�."
      
            
WITH FRAME browse1 TITLE COLOR bright-white "[ ��� ��� ]" WIDTH 120.


FORM
   DateValue
      LABEL "�ப(���)"
      FORMAT       "99/99/9999"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "�ப(���)"
   NameEvent
      LABEL "����⨥"
      FORMAT       "x(10)"
      VIEW-AS FILL-IN SIZE 10 BY 1
      HELP         "����⨥"
   Branch_
      LABEL "���"
      FORMAT       "x(4)"
      VIEW-AS FILL-IN SIZE 4 BY 1
      HELP         "���"
   Details
      LABEL "�������ਨ"
      FORMAT       "x(200)"
      VIEW-AS EDITOR SIZE 54 BY 3
      HELP         "�������ਨ"
   PrinalFIO
      LABEL "�ਭ� ���"
      FORMAT       "x(200)"
      VIEW-AS TEXT SIZE 54 BY 1
      HELP         "�ਭ� ���"  
   Descriptions
      LABEL "�ਭ�"
      FORMAT       "x(30)"
      VIEW-AS FILL-IN SIZE 15 BY 1
   DateChange
      LABEL "��� ���������"
      FORMAT       "x(19)"
      VIEW-AS TEXT SIZE 19 BY 1
      HELP         "��� ���������"
   User_Id
      LABEL "���짮�."
      FORMAT       "x(10)"
      VIEW-AS TEXT SIZE 10 BY 1 
      HELP         "���짮�."
   User_FIO
      LABEL "��� ���짮�."
      FORMAT       "x(40)"
      VIEW-AS TEXT SIZE 40 BY 1 
      HELP         "��� ���짮�."
WITH FRAME edit CENTERED TITLE COLOR bright-white IF LASTKEY EQ KEYCODE("INS") THEN "[ �������� ]" ELSE "[ ������஢���� ]".


