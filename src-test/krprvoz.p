/*
               ��� "���� ����"
    Copyright: 
     Filename: kredprodtranz.p
      Comment: ��� �㬬� ������ ᫥��� ᯨ��� � ������ �� ��������
   Parameters: 
         Uses:
      Used by:
      Created: kau
     Modified: 
*/


DEFINE INPUT  PARAMETER iDate    AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER iOprid   AS RECID       NO-UNDO.

/*DEF INPUT PARAM iParam AS DATE NO-UNDO.

DEF VAR iDate AS DATE NO-UNDO.*/


/*iDate = 04/24/2014.*/



/*iDate = iParam.*/

/*MESSAGE iDate VIEW-AS ALERT-BOX.*/




/*���砫� ������塞 ⠡��� �� ��� ��� ��⮬ ��ᯮ��㥬�� �� ��� �⡮� ���㬥�⮢ � ��।���묨 ��⠬�*/
DO TRANSACTION:
RUN kredprodtranz.p (iDate).
END.







DO TRANSACTION:


IF NOT CONNECTED("qbiswork") THEN DO:
    CONNECT  -pf VALUE("/home2/bis/quit41d/conf/bisreal.pf") NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "�� 㤠���� ᮥ�������� � ����� qbiswork" VIEW-AS ALERT-BOX.
        RETURN.



    END.
END.



RUN krprvoz_.p (iDate).

finally:
IF CONNECTED("qbiswork") THEN
    DISCONNECT qbiswork.
IF CONNECTED("bisreal") THEN
    DISCONNECT bisreal.
end.    
    
end.


RUN g-trans.p (iDate,iOprid).






