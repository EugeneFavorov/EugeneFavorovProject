
/* +++ pb_k2init.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:55pm +++ */

/* **************************************************************************** */
/* ��ନ�㥬 ����� ��⮪��� � ����                                             */
/* **************************************************************************** */

DEFINE VARIABLE cDir        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cLog        AS CHARACTER    NO-UNDO.    /* ��� ��� �ணࠬ���� */
DEFINE VARIABLE cPrt        AS CHARACTER    NO-UNDO.    /* ��⮪�� ��� �ᯮ���⥫� - XL */
DEFINE VARIABLE cPrtF       AS CHARACTER    NO-UNDO.    /* ���쪮 ��� 䠩�� ��⮪���    */
DEFINE VARIABLE lPrtFirst   AS LOGICAL      NO-UNDO INIT YES.   /* ���� ��� � ��⮪��� => ���� �뢥�� 蠯�� */

cDir  = "/home2/bis/quit41d/log/k2-kbs/".
cPrtF = "k2kbs-" + USERID("bisquit") + "-"
      + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99")
      + "-" + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "").
cLog  = cDir  + cPrtF + ".log".
cPrt  = cDir  + cPrtF + ".xml".

/* --- pb_k2init.i was humbly modified by (c)blodd converter v.1.09 on 10/5/2016 1:55pm --- */
