/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�� ������:     �஢����, �� ����� �ࢥ� ����饭� �ணࠬ��
��� ࠡ�⠥�:   ����᪠�� ������� Linux: uname -a
��ࠬ����:      �����頥� ��� �ࢥ�
������:         15.02.2018 ���ᮢ �.�.
*/

DEFINE VARIABLE cText       AS CHARACTER    NO-UNDO.
DEFINE STREAM sUname.

OS-COMMAND SILENT uname -a > /tmp/uname.txt.
INPUT  STREAM sUname FROM /tmp/uname.txt.
IMPORT STREAM sUname UNFORMATTED cText.
INPUT  STREAM sUname CLOSE.
OS-DELETE /tmp/uname.txt.

RETURN ENTRY(2, cText, " ").
