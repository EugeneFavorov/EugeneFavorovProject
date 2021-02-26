{globals.i}
{intrface.get xclass}

DEFINE INPUT PARAMETER iClass AS character NO-UNDO.
DEFINE INPUT PARAMETER iSurr  AS character NO-UNDO.
DEFINE INPUT PARAMETER iVal   AS character NO-UNDO.

DEFINE VARIABLE mErrMess      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mLinkSurr     AS CHARACTER NO-UNDO.

RUN CreateLinksRetSurr(
    "acct",
    "acct-group",
    iSurr,
    iVal,
    DATE(Today),
    ?,
    "",
    OUTPUT mLinkSurr) NO-ERROR.

RETURN mErrMess.
