{globals.i}
DEFINE INPUT PARAMETER iClass AS character NO-UNDO.
DEFINE INPUT PARAMETER iSurr  AS character NO-UNDO.
DEFINE INPUT PARAMETER iVal   AS character NO-UNDO.

DEFINE VAR i       AS INT64     NO-UNDO.
DEFINE VAR mCnt    AS INT64     NO-UNDO.
DEFINE VAR vLen    AS INT64     NO-UNDO.
DEFINE VAR isAlpha AS LOGICAL   NO-UNDO.
DEFINE VAR mKPP    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mErrMess     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk          AS LOGICAL   NO-UNDO.

DO mCnt = 1 TO NUM-ENTRIES(iVal): 
   mKPP = ENTRY(mCnt,iVal).
   IF mKPP NE "" THEN
   DO:
      vLen = LENGTH(mKPP).
      DO i = 1 TO vLen:
         IF ASC(SUBSTRING(mKPP,i,1)) LT 48 OR 
            ASC(SUBSTRING(mKPP,i,1)) GT 57 
         THEN isAlpha = YES.
         ELSE isAlpha = NO.
         IF isAlpha THEN LEAVE.
      END.
      IF isAlpha EQ YES THEN mErrMess = 'В КПП должны быть только цифры.'.
      IF vLen NE 9      THEN mErrMess = 'В КПП должно быть 9 цифр, введено ' + TRIM(STRING(vLen)) + '.'. 
   END.
END.

RETURN mErrMess.
