/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: mcmratbrw.frm
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 04.03.2010 11:38 Chumv   
     Modified: 04.03.2010 11:38 Chumv   
*/

FORM
   comm-rate.commission
      FORMAT 'x(15)'
   mZnak
      FORMAT 'x'
      COLUMN-LABEL "����"
      HELP "������ ��������� �⠢�� �������� �த�� (+/-)"
   comm-rate.rate-comm 
      FORMAT "->>>,>>>,>>>,>>>,>>9.99999"
   comm-rate.since
WITH FRAME browse1 TITLE COLOR bright-white "[ ������������ ��� " + CAPS(mKauModif) + " ]".
                            
FORM
   comm-rate.commission
      FORMAT 'x(15)'
   mZnak
      FORMAT 'x'
      LABEL "����"
      HELP "������ ��������� �⠢�� �������� �த�� (+/-)"
   comm-rate.rate-comm 
      FORMAT "->>>>>>9.99999"
   comm-rate.since
WITH FRAME edit WIDTH 50 SIDE-LABEL.
