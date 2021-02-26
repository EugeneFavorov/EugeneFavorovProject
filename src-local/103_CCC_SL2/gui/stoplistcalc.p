/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 2000-2016 АО "Банковские информационные системы"
     Filename: stoplistcalc.p
      Comment: Расчет класса
   Parameters: Нет
         Uses:
      Used by:
      Created: 08.11.2013  ANBA    
     Modified: 08.11.2013  ANBA     
*/

{globals.i}
{sv-temp.i}
{norm.i}
{wordwrap.def}
{intrface.get xclass}  
DEFINE INPUT PARAMETER iDataID AS INT64 NO-UNDO.

DEFINE BUFFER bCode FOR code.

DEFINE VAR mCntAll   AS INT64 INIT 0 NO-UNDO.
DEFINE VAR mCntWrk   AS INT64 INIT 0 NO-UNDO.
DEFINE VAR mFlagErr  AS LOGICAL        NO-UNDO.
DEFINE VAR mCnt      AS INT64        NO-UNDO.
DEFINE VAR mBirthDay     AS DATE      NO-UNDO.
DEFINE VAR mBirthPlace   AS CHARACTER NO-UNDO.
DEFINE VAR mDataCls      AS DATE      NO-UNDO.

FORM
   "      Подготовка данных для контроля         " SKIP
   "террористических и экстремистских организаций" SKIP
   WITH FRAME frLoad OVERLAY CENTERED ROW 19 COLOR MESSAGE.

/*{intrface.get terr}*/
{stoplist.fun}
{stoplist.pro &DATA-LINE = TDataLine}
/*============================================================================*/
{fexp-chk.i &DataID = iDataID}

FIND FIRST bCode WHERE
           bCode.class =  ""
       AND bCode.code =  "StopList"
           NO-LOCK NO-ERROR.

DISPLAY WITH FRAME frLoad. PAUSE 0.

SELECT count(*) into mCntAll
  FROM code
 WHERE code.class = "StopList".

{bar-beg2.i &BarTotal = mCntAll}
FOR EACH code WHERE                              /* Нормализация списка       */
         code.class =  "StopList" NO-LOCK:

   ASSIGN
      mBirthDay   = DATE(GetXattrValueEx("code","StopList," + code.code,"birthday",""))
      mBirthPlace =      GetXattrValueEx("code","StopList," + code.code,"birthplace","").
   /* формирование DataLine */
   {stoplistln.i &DataID=iDataID &BirthDay = mBirthDay &BirthPlace = mBirthPlace} 

   mCntWrk = mCntWrk + 1.
   IF mCntWrk modulo 10 =  0       OR
      mCntWrk           =  mCntAll THEN DO:
      {bar2.i &BarPointer = mCntWrk}
   END.

END.
{bar2.i &BarPointer = mCntAll}


mFlagErr = YES.                        /* Создание ссылки в классификаторе    */
REPEAT TRANSACTION ON ERROR UNDO, LEAVE:
   FIND current bCode EXCLUSIVE-LOCK no-wait NO-ERROR.
   IF NOT AVAILABLE(bCode) THEN DO:
      RUN normdbg IN h_debug (0,"Ошибка","Классификатор блокирован другим пользователем.").
      LEAVE.
   END.

   ASSIGN bCode.Misc[8] = string(iDataID).

   mFlagErr = NO.
   LEAVE.
END.

RELEASE bCode.
HIDE FRAME frLoad.

IF mFlagErr THEN DO:
   RUN normdbg IN h_debug (0,"Ошибка","Невозможно выполнить расчет блока данных.").
   {empty TDataLine}
   RETURN "error".
END.
ELSE
   RETURN "".
/******************************************************************************/
/* $LINTFILE='stoplistcalc.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:45.649+03:00' */
/*prosignowVVkorBpzx+ab+VujjSeQ*/