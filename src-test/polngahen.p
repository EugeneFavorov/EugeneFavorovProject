/*
               ������᪠� ��⥣�஢����� ��⥬� �������pDP
    Copyright: 
     Filename: polngahen.p
      Comment: ����� ᮣ��襭�� �� ����㯭��
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 13/04/2018 zss ���饭�� ����騪� ������ ��襭��
     Modified: 
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE gdTplName  "obr_za_gah"     /*��� 蠡���� ���*/
&GLOBAL-DEFINE gdSignProc ""      /*��� �����䨪��� signat*/




{getdate.i
   &DateLabel="������ ����"
   &DateHelp="�롥�� ���� ����� �㤥� ���⠢������ � ����"
   &AddPostUpd=" 
      
               "


}
RUN Insert_TTName("Date_oth", STRING(end-date, '99.99.9999')).
{precrdprint.p {&*}}
