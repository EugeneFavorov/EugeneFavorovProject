&IF DEFINED(TERRCALC-PRO-DEF) =  0 &THEN
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
      CREATE {&DATA-LINE}.
      ASSIGN {&DATA-LINE}.Data-ID = iDataID
             {&DATA-LINE}.Sym1    = TRIM(REPLACE(ParsStringSl(iPasp), " ", "")) + CHR(1) + 
                                    (IF iBirthDate <> ? 
                                     THEN STRING(iBirthDate,"99/99/9999")   
                                     ELSE "")                                   + CHR(1) +
                                    TRIM(iBirthPlace)                           + CHR(1) +
                                    (IF vDataCls <> ? 
                                     THEN STRING(vDataCls,"99/99/9999")   
                                     ELSE "")
             {&DATA-LINE}.Sym2    = TRIM(vStr) 
             {&DATA-LINE}.Sym3    = TRIM(iInn)
             {&DATA-LINE}.Sym4    = iCode
             {&DATA-LINE}.Txt     = TRIM(iStopList)
      NO-ERROR.
       END.
   END.

END PROCEDURE.
&ENDIF
/* $LINTFILE='stoplist.pro' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='shoi' */
/* $LINTDATE='18/05/2017 09:19:19.580+03:00' */
/*prosign7+dpeXVKPxWuJAq8nvrruA*/