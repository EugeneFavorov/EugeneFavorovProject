/* **************************************************************************** */
/* ��ࠢ�� ��⮪��� �� ����.���짮��⥫� �� BisPC                             */
/* **************************************************************************** */

IF NOT lPrtFirst
THEN DO:
    /* �����蠥� ��⮪�� */
    OUTPUT TO VALUE(cPrt) APPEND.
    PUT UNFORMATTED XLEnd().
    OUTPUT CLOSE.

    /* ��। ��ࠢ��� ��⮪��� �஢�ਬ, ����饭 �� bispc */
    DEFINE VARIABLE mRet        AS CHARACTER    NO-UNDO INIT "".
    DO WHILE (mRet EQ ""):
        RUN IsUserServReady IN h_netw ("", OUTPUT mRet).
        IF mRet EQ "" THEN
           MESSAGE 
              "������� �ணࠬ�� bispc � ������ ��"
           VIEW-AS ALERT-BOX.
    END.

    /* ��ࠢ�塞 ��⮪�� */
    RUN sndbispc.p ("file=" + cPrt + ";class=bq").

    OS-COPY VALUE(cPrt) "".
END.

MESSAGE "�����祭� �ॣ㫨஢���� �2 - ���."
    IF lPrtFirst THEN "~n�� ���� ��� �� ��ࠡ�⠭."
                 ELSE "~n���� ��⮪���  " + cPrtF + " ��ନ஢��." /* "~n��室���� � ��⠫���  C:/BisPC/." */
    VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
