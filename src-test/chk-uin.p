/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2017 АО "Банковские информационные системы"
     Filename: chk-uin.p
      Comment: Контроль реквизита УИН для неналоговых платежей
   Parameters: iOp - Внутренний номер документа
      Created: 09/09/2014
*/

{globals.i}             /* Глобальные переменные сессии. */
{intrface.get re}
{intrface.get strng}
{intrface.get xclass}
{chk-uin.pro}


DEF INPUT  PARAM iOp       AS INT64 NO-UNDO. /* Внутренний номер документа. */
DEF 
&IF DEFINED(uin-xattr-value) = 0 &THEN
    OUTPUT PARAM
&ELSE
    VAR
&ENDIF
    oResult   AS LOG   NO-UNDO INIT NO. /* Результат проверки. */

DEF VAR mAcctRec AS CHARACTER NO-UNDO.
DEF VAR mUIN     AS CHARACTER NO-UNDO.
DEF VAR mPokST   AS CHARACTER NO-UNDO.
DEF VAR mRes     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mHousing AS LOGICAL NO-UNDO.

FIND FIRST op WHERE op.op = iOp NO-LOCK NO-ERROR.

IF AVAILABLE op THEN
   RUN gkh-signs-ar.p(op.op-kind,
                      RECID(op),
                      OUTPUT mHousing).
                      
IF mHousing THEN DO:
   mUIN = GetXAttrValue("op", STRING(iOp), "УИН").
   mPokST = GetXAttrValue("op", STRING(iOp), "ПокСТ").
   /* Если у нас уже есть налоговые реквизиты (ПокСТ),
      то УИН для ГИС ЖКХ можно не проверять */
   IF NOT {assigned mPokST} OR 
      mPokST =  "0" THEN DO:
     RUN Validate-UIN-Housing IN THIS-PROCEDURE (mUIN,
                                                 OUTPUT mRes).
     oResult = NOT mRes.
   END.
END.
ELSE DO:
   RUN ChkOpUIN IN THIS-PROCEDURE (iOp,
                                   OUTPUT mRes,
                                   OUTPUT mAcctRec,
                                   OUTPUT mUIN).
   IF mRes THEN DO:
/*      RUN Validate-UIN-regexp IN THIS-PROCEDURE (mUIN, OUTPUT mRes). /*mRes = no*/*/
      mRes = YES.
      oResult = (mRes <> YES).
      IF oResult THEN
         RETURN RETURN-VALUE.
   END.
   IF mRes AND {assigned mAcctRec} THEN DO:
      RUN ChkKey IN THIS-PROCEDURE (mAcctRec, mUIN, OUTPUT oResult).
      oResult = NOT oResult.
      IF oResult THEN
         RETURN "Неверное значение ключевого разряда УИН".
   END.
END.

{intrface.del}

/* $LINTFILE='chk-uin.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:36.213+03:00' */
/*prosign6B61oyfTOpUi6uldNK9S0g*/