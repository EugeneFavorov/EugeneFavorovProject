{globals.i}
{intrface.get tmess}

/* +++ pb_getdmy.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am +++ */

/* ������ ����, ����楢 � ��� � ��ਮ�� */

DEFINE INPUT PARAMETER iDate1   AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iDate2   AS CHAR   NO-UNDO.

{intrface.get date}
DEFINE VARIABLE mNDays      AS INT64      NO-UNDO.
DEFINE VARIABLE mNMonth     AS INT64      NO-UNDO.
DEFINE VARIABLE mNYear      AS INT64      NO-UNDO.

RUN DMY_In_Per(iDate1, iDate2, OUTPUT mNDays, OUTPUT mNMonth, OUTPUT mNYear).
RETURN STRING(mNDays) + "," + STRING(mNMonth) + "," + STRING(mNYear).
{intrface.del}

/* --- pb_getdmy.p was humbly modified by (c)blodd converter v.1.09 on 12/16/2016 7:07am --- */
