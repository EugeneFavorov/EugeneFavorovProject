/*  
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: lg7001cl.p
      Comment: Контроль на принадлежность террористическим организациям
   Parameters: Нет
         Uses:
      Used BY:
      Created: 26.05.2004 abko 0027013
     Modified: 22.11.2004 17:52 KSV      (0038814) Переведена на системные
                                         сообщения.
     Modified:
*/
&IF DEFINED(nodefpesr) = 0
&THEN
   {intrface.get terr}
   {intrface.get xclass}
   {intrface.get tmess}
   DEFINE TEMP-TABLE t-obj NO-UNDO
            FIELD rec AS recid.
   {tmprecid.def}
&ENDIF

DEFINE VAR mLG7001Name        AS CHAR    NO-UNDO.
DEFINE VAR mLG7001List        AS CHAR    NO-UNDO.
DEFINE VAR mLG7001Table AS CHAR    NO-UNDO.
DEFINE VAR mLG7001ClWh  AS LOGICAL NO-UNDO.
DEFINE BUFFER xlg7001acct FOR acct.
DEFINE BUFFER xlg7001loan FOR loan.

DEFINE VARIABLE mNumPerTerr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDatePerTerr AS DATE      NO-UNDO.
DEFINE BUFFER bhistory FOR history.  

&IF DEFINED(rez_prov) NE 0 &THEN
   ASSIGN
       mNumPerTerr = GetTempXAttrValue("code",",TerrBlack","ПереченьНомер").
    FIND LAST bhistory WHERE bhistory.file-name EQ "code"
                         AND bhistory.field-ref EQ ",TerrBlack"
                         AND bhistory.modify    EQ "C" 
    NO-LOCK NO-ERROR.
    IF AVAILABLE bhistory THEN
    DO:
        mStrFile = ENTRY(NUM-ENTRIES(bhistory.field-value,"/"),bhistory.field-value,"/").   
        mDatePerTerr = DATE(SUBSTRING(mStrFile,1,10)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           mDatePerTerr = DATE(SUBSTRING(mStrFile,1,8)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           mDatePerTerr = DATE(SUBSTRING(mStrFile,1,6)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           mDatePerTerr = bhistory.modif-date.              
    END.
    
    UpdateTempSignsEx({&in-class},{&surrogate},"ДатаПровТерр",gend-date,STRING(gend-date,"99/99/9999"),?).
    UpdateTempSignsEx({&in-class},{&surrogate},"ПричастТерр",gend-date,"не причастен",?). 
    UpdateTempSignsEx({&in-class},{&surrogate},"НомПеречТерр",gend-date,"",?).
    UpdateTempSignsEx({&in-class},{&surrogate},"ДатаПеречТерр",gend-date,"",?).   
&ENDIF 


mLG7001List = GetXAttrValueEx({&in-class},{&surrogate},"LegTerr","").
IF mLG7001List = "" 
OR NOT CAN-FIND(FIRST code WHERE code.class = "TerrBlack"
                             AND CAN-DO(mLG7001List,code.code))
THEN DO:

   mLG7001ClWh = NO.
   mLG7001Table = GetXclassProgress({&in-class}).
   CASE mLG7001Table:
      WHEN "person"    THEN mLG7001ClWh = CheckClientWhite("Ч",INT64({&surrogate})).
      WHEN "cust-corp" THEN mLG7001ClWh = CheckClientWhite("Ю",INT64({&surrogate})).
      WHEN "banks"     THEN mLG7001ClWh = CheckClientWhite("Б",INT64({&surrogate})).
      WHEN "acct"      THEN DO:
         RUN GetBufferBySurrNoLock ((BUFFER xlg7001acct:HANDLE),{&surrogate}).
         IF AVAIL xlg7001acct THEN DO:
            IF xlg7001acct.cust-cat = "Ч"
            OR xlg7001acct.cust-cat = "Ю"
            OR xlg7001acct.cust-cat = "Б"
            THEN mLG7001ClWh = CheckClientWhite(xlg7001acct.cust-cat,xlg7001acct.cust-id).
         END.
      END.
      WHEN "loan"      THEN DO:
         RUN GetBufferBySurrNoLock ((BUFFER xlg7001loan:HANDLE),{&surrogate}).
         IF AVAIL xlg7001loan THEN DO:
            IF xlg7001loan.cust-cat = "Ч"
            OR xlg7001loan.cust-cat = "Ю"
            OR xlg7001loan.cust-cat = "Б"
            THEN mLG7001ClWh = CheckClientWhite(xlg7001loan.cust-cat,xlg7001loan.cust-id).
         END.
      END.
   END CASE.

   IF mLG7001ClWh <> YES THEN DO:
      mLG7001Name = {&cl_name1}.
      
      {empty t-obj}
      RUN CompareFast IN h_terr (mLG7001Name,'plat', INPUT-OUTPUT TABLE t-obj).
      &IF DEFINED(cl_name2) NE 0 &THEN
         mLG7001Name = {&cl_name2}.
         RUN CompareFast IN h_terr (mLG7001Name,'plat', INPUT-OUTPUT TABLE t-obj).
      &ENDIF
      
      IF CAN-FIND(FIRST t-obj) THEN DO:
         SEL-TERR:
         DO WHILE YES ON ERROR  UNDO, LEAVE
                      ON ENDKEY UNDO, LEAVE:
      
            RUN terrslct.p (INPUT TABLE t-obj,
                            mLG7001Name + CHR(1) + "" + CHR(1) + "",
                            4).
            IF KEYFUNCTION(LASTKEY) NE "END-ERROR" THEN DO:
               mLG7001List = "".
               FOR EACH tmprecid,
                  FIRST code WHERE recid(code) EQ tmprecid.id
                        NO-LOCK:
                  IF NUM-ENTRIES(mLG7001List) LE 30 THEN
                  DO:
                     {additem.i mLG7001List code.code}
                  END.
                  DELETE tmprecid.
               END.
               UpdateSigns({&in-class},{&surrogate},"LegTerr", mLG7001List, NO ).
               &IF DEFINED(rez_prov) NE 0 &THEN
                  IF {assigned mLG7001List} THEN
                  DO:
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "ПричастТерр",
                                       gend-date,
                                       "причастен",
                                       ?).
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "ДатаПеречТерр",
                                       gend-date,
                                       STRING(mDatePerTerr),
                                       ?). 
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "НомПеречТерр",
                                       gend-date,
                                       mNumPerTerr,
                                       ?).  
                  END.     
               &ENDIF
            END.
            LEAVE.
         END. /* REPEAT                                        */
      END. /* IF CAN-FIND(FIRST t-obj)                      */
  end.    
      DO:
                                
                     UpdateTempSignsEx({&in-class},
                                         {&surrogate},
                                         "ДатаПровТерр",
                                         gend-date,
                                         STRING(gend-date,"99/99/9999")
                                         ,?).
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "ПричастТерр",
                                       gend-date,
                                       "не причастен",
                                       ?).
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "ДатаПеречТерр",
                                       gend-date,
                                       STRING(mDatePerTerr),
                                       ?). 
                     UpdateTempSignsEx({&in-class},
                                       {&surrogate},
                                       "НомПеречТерр",
                                       gend-date,
                                       mNumPerTerr,
                                       ?).  
                  END.     

END.
ELSE DO:
   RUN Fill-SysMes("","","0",
                   {&cl_name1} + " имеет отношение к террористам!").
END.
/* $LINTFILE='lg7001cl.i' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='paus' */
/* $LINTDATE='08/04/2016 12:02:27.336+04:00' */
/*prosignQp2Xtsl02oizye75Pl5bZA*/