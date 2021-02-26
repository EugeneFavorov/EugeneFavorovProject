/* Подсчет дней, месяцев и лет в периоде */

DEFINE INPUT PARAMETER iDate1   AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER iDate2   AS CHAR   NO-UNDO.

{intrface.get date}
DEFINE VARIABLE mNDays      AS INT64      NO-UNDO.
DEFINE VARIABLE mNMonth     AS INT64      NO-UNDO.
DEFINE VARIABLE mNYear      AS INT64      NO-UNDO.

RUN DMY_In_Per(iDate1, iDate2, OUTPUT mNDays, OUTPUT mNMonth, OUTPUT mNYear).
RETURN STRING(mNDays) + "," + STRING(mNMonth) + "," + STRING(mNYear).
{intrface.del}
