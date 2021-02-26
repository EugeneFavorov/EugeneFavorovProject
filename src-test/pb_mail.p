/**
����᪨� �ࠢ� �ਭ�������: ��吝�� ���� (���)
�� ������:     ��ࠢ�� ���஭��� ����� �� ��᪢��
��� ࠡ�⠥�:   �맮� unix-������� mail
��ࠬ����:      1. ���᮪ �/� �१ "," � ��� �ࢥ� (�� @)
                2. ���� ���쬠
                3. ����� ���쬠
                4. ���᮪ ��������� 䠩���
�ਬ��:         RUN pb_mail.p ("a.borisov,s.strahov", "�訡�� � ...", cMess, SEARCH("sysmess.log") + "," + SEARCH("_spoolm.tmp")).
������:         13.05.2016 ���ᮢ �.�.
*/

DEF INPUT PARAM iMail   AS CHARACTER NO-UNDO.   /* ���᮪ �/� */
DEF INPUT PARAM iSubj   AS CHARACTER NO-UNDO.   /* ���� */
DEF INPUT PARAM iMess   AS CHARACTER NO-UNDO.   /* ����� ���쬠 */
DEF INPUT PARAM iAdd    AS CHARACTER NO-UNDO.   /* ���᮪ ��������� 䠩��� */

DEFINE VARIABLE cTmp    AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE I       AS INTEGER   NO-UNDO.

iMail = replace(iMail,"^",",").
DO I = 1 TO NUM-ENTRIES(iAdd):
    cTmp = cTmp + ' -a ' + ENTRY(I, iAdd).
END.

cTmp = 'echo "' + iMess + '" | mail -s "' + iSubj + '" -r bis@plus-bank.ru' + cTmp
     + ' ' + REPLACE(iMail, ",", "@bankom.omsk.su ") + "@bankom.omsk.su".
OS-COMMAND SILENT VALUE(cTmp).
