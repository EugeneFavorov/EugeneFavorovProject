/**/
FUNCTION ReplaceQuotes2 CHAR (INPUT iText AS CHAR):
   DEFINE VAR vBadList  AS CHAR     NO-UNDO INIT '"'.
   DEFINE VAR vLen      AS INT64    NO-UNDO.
   DEFINE VAR vItm      AS INT64    NO-UNDO.
   DEFINE VAR vName     AS CHAR     NO-UNDO.
   DEFINE VAR vOne      AS CHAR     NO-UNDO.
   vLen = LENGTH(vBadList).
   DO vItm = 1 TO vLen:
      iText = REPLACE(iText,SUBSTRING(vBadList,vItm,1),"").
   END.
   RETURN TRIM(iText).
END FUNCTION.
/**/
FUNCTION ParsStringSL RETURN CHAR 
   (INPUT iString AS CHARACTER).

   DEFINE VARIABLE oStr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iStr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE oString AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNum   AS INT64 NO-UNDO.
   DEFINE VARIABLE vNum1  AS INT64 NO-UNDO.
   DEFINE VARIABLE vNumBg AS INT64 NO-UNDO.
   DEFINE VARIABLE str    AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE VARIABLE Str1   AS CHARACTER EXTENT 2 NO-UNDO.
   
   iString = ReplaceSpecSl(iString).
   DO vNumBg = 1 TO MAX(1,NUM-ENTRIES(iString,"/")):
      oStr = ENTRY(vNumBg,iString,'/').
      oStr = REPLACE(oStr,".","").
      DO WHILE INDEX(oStr,"  ") >  0:
         oStr = REPLACE(oStr,"  "," ").
      END.
      {additem2.i oString oStr ;}
   END.
   RETURN oString.
END FUNCTION.
/* -------------------------------------------------------------------------- */
/* Если текст в кавычках то заменяем "," на chr(1)                            */
/* -------------------------------------------------------------------------- */
FUNCTION ChkComma RETURN CHAR (INPUT iStr AS CHAR):
   DEFINE VARIABLE vTmpStr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOneSym    AS CHAR  NO-UNDO.
   DEFINE VARIABLE vFlCom     AS LOG   NO-UNDO.
   DEFINE VARIABLE vI         AS INT64 NO-UNDO.

   vTmpStr = iStr.
   /*проверяем кавычки*/
   DO vI = 1 TO LENGTH(vTmpStr):
      vOneSym =  SUBSTRING(vTmpStr,vI,1).
      IF  vOneSym EQ '"'    THEN 
        vFlCom = NOT vFlCom.
      ELSE 
      IF    vFLCom 
        AND vOneSym EQ "," 
      THEN
         vTmpStr = SUBSTRING(vTmpStr,1,vI - 1) + CHR(1) + SUBSTRING(vTmpStr,vI + 1, LENGTH(vTmpStr)).
   END.
   RETURN vTmpStr.
END FUNCTION.
/**/
PROCEDURE CreateStopList :
   DEFINE INPUT  PARAMETER iRawLine AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vJ           AS INT64     NO-UNDO.
   DEFINE VARIABLE vJ1          AS INT64     NO-UNDO.
   DEFINE VARIABLE vI           AS INT64     NO-UNDO.
   DEFINE VARIABLE vTmp         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vInd1        AS INT64     NO-UNDO.
   DEFINE VARIABLE vInd2        AS INT64     NO-UNDO.
   DEFINE VARIABLE vTmpStr      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOneFldInd   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBirthDay    AS DATE      NO-UNDO.
   DEFINE VARIABLE vBirthPlace  AS CHARACTER NO-UNDO.
   
   IF NOT {assigned iRawLine} THEN
      RETURN.
   CR_STOPLST:
   DO TRANSACTION:
      CREATE CODE.
      ASSIGN                                            
         code.class          = 'StopList'
         code.parent         = 'StopList'
         code.code           = STRING(ROWID(CODE))
         code.misc[8]        = USERID("bisquit")              /*ползователь, создавший запись*/  
         code.val            = mNumRez                        /*номер резолюции*/
         .
         UpdateSignsEx(code.class, code.class + "," + code.code,"RezolutionData",STRING(mDataRez)).  /*дата резолюции*/
         UpdateSignsEx(code.class, code.class + "," + code.code,"AddData",STRING(TODAY)).            /*дата добавления*/

      /*Тип клиента может быть задан в НП*/
      vInd1 =  INT64(mFldIndex[1]) NO-ERROR.
      IF vInd1 EQ 0  THEN
         code.misc[1]  =  IF {assigned mCLType} THEN mCLType ELSE "".
      ELSE DO: 
         vTmp = GetEntries(vInd1,iRawLine,mDelimeter, "") NO-ERROR.
         code.misc[1]  = IF CAN-DO("Ч,Ю,Б",vTmp) THEN vTmp ELSE "".
      END.
      code.misc[2] = STRING(mCLNum).

      vTmpStr = ChkComma(iRawLine).
      DO vJ = 2 TO 10:
         IF NOT {assigned mFldIndex[vJ]} THEN
            NEXT.
         DO vJ1 = 1 TO NUM-ENTRIES(mFldIndex[vJ]):
            vOneFldInd = ENTRY(vJ1, mFldIndex[vJ]).
            vTmp = "".
            IF INDEX(vOneFldInd,"-") GT 0 THEN  
            DO:
               ASSIGN 
                  vInd1 = INT64(GetEntries(1,vOneFldInd, "-", "0")) 
                  vInd2 = INT64(GetEntries(2,vOneFldInd, "-", "0")) NO-ERROR.
               IF    vInd1 NE 0 
                 AND vInd2 NE 0 
               THEN
                  DO vI = vInd1 TO vInd2:
                     vTmp = REPLACE(TRIM(GetEntries(vI,vTmpStr,mDelimeter, "")), CHR(1),",").
                     IF {assigned vTmp} THEN
                     DO:
                        vTmp = IF trim(vTmp) EQ "-0-" THEN    "0" ELSE vTmp.
                        IF    SUBSTRING(vTmp, 1,1) EQ '"' 
                          AND SUBSTRING(vTmp,LENGTH(vTmp),1) EQ '"'   
                        THEN
                           vTmp = TRIM(vTmp, '"').
                        code.misc[vJ] =  (code.misc[vJ]  + IF  code.misc[vJ] NE "" THEN "||" ELSE "") + vTmp NO-ERROR.
                     END.
                 END.
            END.
            ELSE  DO:
               vInd1 = INT64(vOneFldInd) NO-ERROR.
               IF vInd1 NE 0 THEN
               DO: 
                  vTmp = REPLACE(TRIM(GetEntries(vInd1,vTmpStr,mDelimeter, "")), CHR(1),",").
                 
                  IF {assigned vTmp} THEN
                  DO:
                     vTmp = IF trim(vTmp) EQ "-0-" THEN "0" ELSE vTmp.
                     IF    SUBSTRING(vTmp, 1,1) EQ '"' 
                       AND SUBSTRING(vTmp,LENGTH(vTmp),1) EQ '"'   
                     THEN
                        vTmp = TRIM(vTmp, '"') NO-ERROR.
                     IF vJ LT 9 THEN
                     DO:
                        code.misc[vJ] = (code.misc[vJ]  + IF code.misc[vJ] NE "" THEN "||" ELSE "") + vTmp NO-ERROR.
                     END.
                     IF vJ EQ 9 THEN
                     DO:
                        vBirthDay = DATE(vTmp).

                        UpdateSignsEx(code.class,code.class + "," + code.code,
                                     "birthday",STRING(vBirthDay,"99/99/9999")).  /*дата рождения*/
                     END.
                     IF vJ EQ 10 THEN
                     DO:
                        vBirthPlace = TRIM(vTmp).
                        UpdateSignsEx(code.class,code.class + "," + code.code,
                                     "birthplace",vBirthPlace).  /*место рождения*/
                     END.
                  END.
               END.
            END.
         END.
      END.

      /*Добавляем символы к названию стоп-листа в зависимости от значения поля*/
      FOR EACH  tt-chkfldval:
         vTmp = TRIM((GetEntries( tt-chkfldval.fldnum , vTmpStr, mDelimeter, ""))). 
         IF {assigned vTmp} AND
            CAN-DO( tt-chkfldval.fld-val,  vTmp) 
          THEN
             code.name  =  mSLName  + GetEntries (LOOKUP(vTmp,tt-chkfldval.fld-val), tt-chkfldval.fld-add, ",", "") .                 /*вид стоп-листа*/
      END.
      IF NOT {assigned code.name} THEN
         code.name = mSLName.
      code.description[1]    =  STRING(CalcCODENum(code.name)). 
      RUN  CrDataLine(mDataId, code.code, "ADD").
   END.
END PROCEDURE.
/**/
PROCEDURE CrDataLine:
   DEFINE INPUT PARAM iDataId  AS INT64  NO-UNDO.
   DEFINE INPUT PARAM iCode    AS CHAR   NO-UNDO.
   DEFINE INPUT PARAM iMode    AS CHAR   NO-UNDO.

   DEFINE BUFFER DataBlock FOR DataBlock.
   DEFINE BUFFER DataLine  FOR DataLine.
   
   IF iDataID NE 0 AND
      iDataID NE ? THEN DO:

      FIND FIRST DataBlock WHERE DataBlock.Data-ID EQ iDataID
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAILABLE(DataBlock) THEN
         MESSAGE COLOR MESSAGE iMode SKIP
                 "Класс быстрого поиска блокирован другим пользователем."     SKIP
                 "Изменения классификатора не отражены."                      SKIP
                 "Выполните расчет класса StopList для исправления ошибки."  SKIP
              VIEW-AS ALERT-BOX ERROR.
      ELSE DO:                            /* Удалить текущую расшифровку         */
         FOR EACH DataLine OF DataBlock WHERE
                  DataLine.Sym1 EQ icode
         EXCLUSIVE-LOCK:
            DELETE DataLine.
         END.
                                          /* Создать новую расшифровку           */
                                                   
         IF iMode NE "DEL" THEN
         DO:
            ASSIGN
               mBirthDay   = DATE(GetXattrValueEx("code","StopList," + iCode,"birthday",""))
               mBirthPlace =      GetXattrValueEx("code","StopList," + iCode,"birthplace","").
            /* формирование DataLine */
/*            {stoplistln.i &DataID=iDataID &BirthDay = mBirthDay &BirthPlace = mBirthPlace}*/
            {stoplistln.i &DataID=mDataID &BirthDay = mBirthDay &BirthPlace = mBirthPlace &DataCls = mDataCls} 
         END.
      END.
      RELEASE DataBlock.
   END.
   ELSE
      MESSAGE COLOR MESSAGE
              "Не существует ссылка на класс быстрого поиска."             SKIP
              "Изменения в классификаторе не отражены."                    SKIP
              "Выполните расчет класса TerrBlack для исправления ошибки."  SKIP
              VIEW-AS ALERT-BOX ERROR.      
END PROCEDURE.
/**/

PROCEDURE CreateStopListDL:
   DEFINE INPUT         PARAMETER iDataID       AS INT64    NO-UNDO.
   DEFINE INPUT         PARAMETER iCode         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iName         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iStopList     AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iInn          AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iPasp         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iBirthDate    AS DATE     NO-UNDO.
   DEFINE INPUT         PARAMETER iBirthPlace   AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iDataCls      AS CHAR     NO-UNDO.
   
   DEFINE INPUT-OUTPUT  PARAMETER pCnt          AS INT64    NO-UNDO.

   DEFINE VAR vCnt   AS INT64 NO-UNDO.
   DEFINE VAR vCnt1  AS INT64 NO-UNDO.
   DEFINE VAR vItm   AS INT64 NO-UNDO.
   DEFINE VAR vItm1  AS INT64 NO-UNDO.
   DEFINE VAR vAll   AS CHAR  NO-UNDO.
   DEFINE VAR vStr   AS CHAR  NO-UNDO.
   DEFINE VAR vCnt2     AS INT64 NO-UNDO.
   DEFINE VAR vDataCls  AS DATE  NO-UNDO.
   
   ASSIGN
      vAll = ParsStringSl(iName)
      vCnt = NUM-ENTRIES(vAll,";")
   .
   
   DO vItm =  1 TO max(vCnt,1):
     ASSIGN 
         vStr  = ENTRY(vItm,vAll,";").
    vCnt2 = vCnt2 + 1.

   /* убираем кавычки из названия */
     vStr  = ReplaceQuotes(vStr).

     vDataCls = DATE(iDataCls).
     
     DO TRANSACTION:
      CREATE DataLine.
      ASSIGN DataLine.Data-ID = iDataID
             DataLine.Sym1    = TRIM(REPLACE(ParsStringSl(iPasp), " ", "")) + CHR(1) + 
                                    (IF iBirthDate <> ? 
                                     THEN STRING(iBirthDate,"99/99/9999")   
                                     ELSE "")                                   + CHR(1) +
                                    TRIM(iBirthPlace)                           + CHR(1) +
                                    (IF vDataCls <> ? 
                                     THEN STRING(vDataCls,"99/99/9999")   
                                     ELSE "")
             DataLine.Sym2    = TRIM(vStr) 
             DataLine.Sym3    = TRIM(iInn)
             DataLine.Sym4    = iCode
             DataLine.Txt     = TRIM(iStopList)
      NO-ERROR.
       END.
   END.

END PROCEDURE.

/*
PROCEDURE CreateStopListDL:
   DEFINE INPUT         PARAMETER iDataID       AS INT64    NO-UNDO.
   DEFINE INPUT         PARAMETER iCode         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iName         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iStopList     AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iInn          AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iPasp         AS CHAR     NO-UNDO.
   DEFINE INPUT         PARAMETER iBirthDate    AS DATE     NO-UNDO.
   DEFINE INPUT         PARAMETER iBirthPlace   AS CHAR     NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER pCnt          AS INT64    NO-UNDO.

   DEFINE VAR vCnt   AS INT64 NO-UNDO.
   DEFINE VAR vCnt1  AS INT64 NO-UNDO.
   DEFINE VAR vItm   AS INT64 NO-UNDO.
   DEFINE VAR vItm1  AS INT64 NO-UNDO.
   DEFINE VAR vAll   AS CHAR  NO-UNDO.
   DEFINE VAR vStr   AS CHAR  NO-UNDO.
   DEFINE VAR vCnt2  AS INT64 NO-UNDO.
   
   ASSIGN
      vAll = ParsStringSl(iName)
      vCnt = NUM-ENTRIES(vAll,";")
   .
   
   DO vItm =  1 TO max(vCnt,1):
     ASSIGN 
         vStr  = ENTRY(vItm,vAll,";").
    vCnt2 = vCnt2 + 1.

     /* убираем кавычки из названия */
     vStr  = ReplaceQuotes2(vStr).

     DO TRANSACTION:
      CREATE DataLine.
      ASSIGN DataLine.Data-ID = iDataID
             DataLine.Sym1    = TRIM(REPLACE(ParsStringSl(iPasp), " ", "")) + CHR(1) + 
                                    (IF iBirthDate <> ? THEN STRING(iBirthDate,"99/99/9999") ELSE "") + CHR(1) + TRIM(iBirthPlace)
             DataLine.Sym2    = TRIM(vStr) 
             DataLine.Sym3    = TRIM(iInn)
             DataLine.Sym4    = iCode
             DataLine.Txt     = TRIM(iStopList)
      NO-ERROR.
       END.
   END.
END PROCEDURE.
*/
