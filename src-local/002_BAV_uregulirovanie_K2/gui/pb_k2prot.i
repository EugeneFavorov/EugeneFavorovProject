
/* +++ pb_k2prot.i was humbly modified by (c)blodd converter v.1.09 on 8/23/2016 11:28am +++ */

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
           RUN Fill-AlertSysMes IN h_tmess("","",1,"������� �ணࠬ�� bispc � ������ ��").

    END.

    /* ��ࠢ�塞 ��⮪�� */
    RUN sndbispc.p ("file=" + cPrt + ";class=bq").

    OS-COPY VALUE ( {&RELATIVE_2_ABSOLUTE}( cPrt ) ) "".
END.

RUN Fill-AlertSysMes IN h_tmess("","",1,"�����祭� �ॣ㫨஢���� �2 - ���." + CHR(32) + IF lPrtFirst THEN "~n�� ���� ��� �� ��ࠡ�⠭." ELSE "~n���� ��⮪��� " + STRING(cPrtF) + " ��ନ஢��." /* "~n��室���� � ��⠫��� C:/BisPC/." */).


/* --- pb_k2prot.i was humbly modified by (c)blodd converter v.1.09 on 8/23/2016 11:28am --- */
