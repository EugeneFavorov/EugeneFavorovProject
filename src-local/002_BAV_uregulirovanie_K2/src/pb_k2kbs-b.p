/**
����᪨� �ࠢ� �ਭ�������: ��� "���� ����"
�� ������:     ��楤�� �ॣ㫨஢���� �2 - ��� �� ����祭��� �/�
��� ࠡ�⠥�:   
                ������ ����⢮���� ��⠫��: /home2/bis/quit41d/log/k2-kbs/
���� ����᪠:  ���᮪ ��⮢ - Ctrl-G
������:         31.03.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get filex}
{intrface.get netw}     /* ��ࠢ�� � bispc */
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

{pb_k2init.i}

FOR EACH tmprecid 
    NO-LOCK,
FIRST acct
    WHERE (RECID(acct)  EQ tmprecid.id)
    NO-LOCK:

    RUN pb_k2kbs-1.p(acct.acct, acct.currency, "��", "", cLog, cPrt, INPUT-OUTPUT lPrtFirst).
END.

{pb_k2prot.i}
{intrface.del}
