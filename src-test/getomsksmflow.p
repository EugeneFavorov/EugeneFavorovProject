/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: 
     Filename: getomsksmflow.p
      Comment: ����㦠�� ����⮢�� ᬠ��䫮�
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

DEF INPUT PARAM iForm AS CHAR.

DEF VAR retHandle AS CHAR NO-UNDO.

IF iForm = 'YES' THEN DO:    

    IF NOT CONNECTED("bank")
     THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").
     IF NOT CONNECTED("bank") THEn message "��� ᮥ�������" view-as alert-box.
END.
        RUN getomsksmflow2.p(INPUT iForm, OUTPUT retHandle).

	IF CONNECTED("bank") THEN DISCONNECT bank.
	IF CONNECTED("bismfr") THEN DISCONNECT bismfr.

RETURN retHandle.
