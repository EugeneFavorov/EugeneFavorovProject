/*
               ������᪠� ��⥣�஢����� ��⥬� �������pDP
    Copyright: 
     Filename: sogl_otst.p
      Comment: ����� ᮣ��襭�� �� ����㯭��
      Comment:
   Parameters: 
         Uses: 
      Used by:
      Created: 13/04/2018 zss ���饭�� ����騪� � ��� ��
     Modified: ZSS ���������� �롮� ����
*/
{globals.i}

DEFINE INPUT PARAMETER iPar AS CHARACTER NO-UNDO.
DEFINE VARIABLE myStartDate AS DATE NO-UNDO.

myStartDate = TODAY.

&GLOBAL-DEFINE gdTplName  "obr_zaem"     /*��� 蠡���� ���*/
&GLOBAL-DEFINE gdSignProc ""      /*��� �����䨪��� signat*/

{getdate.i
   &DateLabel="������ ����"
   &DateHelp="�롥�� ���� ����� �㤥� ���⠢������ � ����"
   &AddPostUpd=" 
      /* IF end-date > TODAY THEN
                DO:
                   MESSAGE '��� �� ����� ����� ⥪�饩'
                   VIEW-AS ALERT-BOX.
                   

                   UNDO, RETRY.
                END.*/
               "


}
RUN Insert_TTName("Date_oth", STRING(end-date, '99.99.9999')).


{precrdprint.p {&*}}

