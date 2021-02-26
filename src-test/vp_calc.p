/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: vp_calc.P
      Comment: отчет "Выявление выгодоприобретателей", процедура расчета по F5
   Parameters: tmprecid
         Uses:
      Used by:
      Created: 18.03.2013    
     Modified: 18.03.2013 
*/

{globals.i}
{vp_otdoc.def NEW}
{tmprecid.def}
{intrface.get xclass}

DEFINE VARIABLE vOpDate1      AS DATE     NO-UNDO. /* дата отбираемых документов */
DEFINE VARIABLE vOpDate2      AS DATE     NO-UNDO. /* дата отбираемых документов */
DEFINE VARIABLE mIsAcctDb     AS LOGICAL  NO-UNDO.
DEFINE VARIABLE mIsAcctCr     AS LOGICAL  NO-UNDO.
DEFINE VARIABLE IsDetailNoReg AS LOGICAL  NO-UNDO.
DEFINE VARIABLE IsDetailReg   AS LOGICAL  NO-UNDO.
DEFINE VAR vMaskFlt AS CHAR            NO-UNDO.

DEFINE VARIABLE IntCount AS INT64 NO-UNDO.       /*количество вхождений c учетом регистра*/
DEFINE VARIABLE IntIndex   AS INT64   NO-UNDO. 
DEFINE VARIABLE IntCount2 AS INT64 NO-UNDO.      /*количество исключений с учетом регистра*/
DEFINE VARIABLE IntIndex2   AS INT64   NO-UNDO. 
DEFINE VARIABLE IntCountNoR AS INT64 NO-UNDO.       /*количество вхождений без учета регистра*/
DEFINE VARIABLE IntIndexNoR   AS INT64   NO-UNDO. 
DEFINE VARIABLE IntCountNoR2 AS INT64 NO-UNDO.      /*количество исключений без учета регистра*/
DEFINE VARIABLE IntIndexNoR2   AS INT64   NO-UNDO. 
DEFINE VARIABLE IsSum          AS LOGICAL  NO-UNDO.
DEFINE VARIABLE vSum          AS DECIMAL NO-UNDO.

DEFINE BUFFER   op       FOR op.
DEFINE BUFFER   op-entry FOR op-entry.
DEFINE BUFFER   acct     FOR acct.

FUNCTION CheckDetails      LOGICAL (INPUT iFlDet  AS CHAR,
                                    INPUT iOpDet  AS CHAR,
                                    INPUT iSense  AS LOG) FORWARD.
ASSIGN
   vOpDate1 = DATE(GetSysConf("op-date1"))
   vOpDate2 = DATE(GetSysConf("op-date2")).

RUN vp_otdoc.p ("ВП_ОтборДок", "ВП_ОтборДок", "Выберите код для отбора документов", 4).
IF KEYFUNC (LASTKEY) EQ "end-error" THEN RETURN.

FIND FIRST tmprecid NO-LOCK NO-ERROR.
IF AVAIL tmprecid THEN
   FIND FIRST Code WHERE RECID (Code) EQ tmprecid.id NO-LOCK NO-ERROR.
   IF AVAIL CODE THEN 
      RUN DecodeFiltrFromString (CODE.val).

FOR EACH op WHERE 
         op.op-date  GE vOpDate1 
     AND op.op-date  LE vOpDate2 
     AND (   shModeMulty 
          OR op.filial-id EQ shFilial)
   NO-LOCK:

   /*Проверяем счета в проводках*/
   FOR EACH op-entry OF op NO-LOCK:

      IF vBalDb NE "" THEN
      DO:
         {find-act.i &acct=op-entry.acct-db}
         IF AVAIL acct
         THEN
            mIsAcctDb = CAN-DO(vBalDb,STRING(acct.bal-acct)).
      END.
      ELSE
         mIsAcctDb = YES.

      IF vBalCr NE "" THEN
      DO:
         {find-act.i &acct=op-entry.acct-cr}
         IF AVAIL acct
         THEN
            mIsAcctCr = CAN-DO(vBalCr,STRING(acct.bal-acct)).
      END.
      ELSE
         mIsAcctCr = YES.

      IF vTypRun THEN
      DO: /* И */

         IF NOT (    mIsAcctDb
                 AND mIsAcctCr ) THEN
            NEXT .    
      END.
      ELSE
      DO: /* ИЛИ */

         IF NOT (   mIsAcctDb
                 OR mIsAcctCr ) THEN
            NEXT .
      END.

  /*Суммы**/

     IF {assigned vSumOt} and {assigned vSumDo} THEN  
     do:
        IF  (op-entry.amt-rub GE DEC(vSumOt)) and (op-entry.amt-rub LE DEC(vSumDo)) THEN 
         IsSum = Yes.
         else 
           IsSum = no.
     end.
     else 
        if NOT {assigned vSumOt} and NOT {assigned vSumDo} THEN  
         IsSum = Yes.
       /*IF {assigned vSumOt} or {assigned vSumDo} THEN*/
        else
            IF ({assigned vSumOt} and (op-entry.amt-rub >= DEC(vSumOt))) or ({assigned vSumDo} and (op-entry.amt-rub <= DEC(vSumDo))) THEN 
       
        /* IF ( (op-entry.amt-rub GE DEC(vSumOt))) and (op-entry.amt-rub LE DEC(vSumDo)) THEN */
         IsSum = Yes. 
     else  
       IsSum = no.     
 
         IF NOT IsSum THEN
            NEXT. 

      /*Если по маскам счет подходит, то проверяем содержание операции*/
    IntCount2 = 0.
     IntIndex = 0. 
     IntIndex2 = 0.
     IntCountNoR = 0.
     IntIndexNoR = 0.
     IntCountNoR2 = 0.
     IntIndexNoR2 = 0.
      IF  {assigned vDetailNoReg}  OR {assigned vDetailReg} THEN
      DO:       IntCount = 0. 
    
            IF {assigned vDetailReg} THEN
            DO:
            IF NUM-ENTRIES(vDetailReg,"|") GE 2 THEN
              do:                  
                vMaskFlt    = ENTRY(2,vDetailReg,"|").
                vDetailReg  = ENTRY(1,vDetailReg,"|").                         
              end.            
        
        RUN CountIndex(INPUT op.details, INPUT trim(vDetailReg), YES, OUTPUT IntCount,OUTPUT IntIndex).
        RUN CountIndex(INPUT op.details, INPUT trim(vMaskFlt), YES, OUTPUT IntCount2,OUTPUT IntIndex2).       
     end.
         
     IF {assigned vDetailNoReg} THEN
     DO:
         
        IF NUM-ENTRIES(vDetailNoReg,"|") GE 2 THEN
            do: 
                vMaskFlt        = ENTRY(2,vDetailNoReg,"|").    
                vDetailNoReg    = ENTRY(1,vDetailNoReg,"|").  
              end.                 
        RUN CountIndex(INPUT op.details, INPUT trim(vDetailNoReg), No, OUTPUT IntCountNoR,OUTPUT IntIndexNoR).
        
        RUN CountIndex(INPUT op.details, INPUT trim(vMaskFlt), No, OUTPUT IntCountNoR2,OUTPUT IntIndexNoR2).        
       /* if IntCountNoR2>0 then
       MESSAGE '44' IntCount IntIndex IntCountNoR2 IntIndexNoR2 op.details VIEW-AS ALERT-BOX.
     */
        
        END.            
        
        /*If ((IntCount2 > 0) and (IntCount EQ 0)) or (IntCount EQ 0)  THEN  NEXT.
        
        IF ((IntCount eq 1) and IntCount2 eq 1) And (IntIndex eq IntIndex2 ) THEN NEXT.
        
               
        If ((IntCountNoR2 > 0) and (IntCountNoR EQ 0)) or (IntCountNoR EQ 0) THEN  NEXT.
        
        IF ((IntCountNoR eq 1) and IntCount2NoR eq 1) And (IntIndexNoR eq IntIndex2NoR ) THEN NEXT. */
        
         If (IntCount = 0) and (IntCountNoR = 0) then 
            next.
         else                   
         do:
          /*  MESSAGE "123" IntCount IntIndex IntCountNoR2 IntIndexNoR2 op.details VIEW-AS ALERT-BOX.*/
         IF ((IntCount eq 1) and IntCount2 eq 1) And (IntIndex eq IntIndex2 ) and (IntCountNoR eq 0) THEN NEXT.  /*!!так!!**/   
          If (IntCount eq IntCountNoR2) and (IntIndex eq IntIndexNoR2) and  (IntCountNoR eq 0) then   NEXT.
             If (IntCount eq IntCountNoR2) and (IntCountNoR2 > 1) and  (IntCountNoR eq 0) then   NEXT.
              
         /*If ((IntCount eq 1) and (IntCountNoR eq 1)) and  (IntIndex eq IntIndexNoR2) then   NEXT.
         If (op.doc-num ='32') or (op.doc-num ='271') then  */
     /*    MESSAGE "123" op.details  vMaskFlt  IntCountNoR2  IntIndexNoR2 VIEW-AS ALERT-BOX.  */
        /*   MESSAGE IntCount IntCountNoR2 IntIndex IntIndexNoR2 op.details op.doc-num VIEW-AS ALERT-BOX. */            
            IntCount = 0. 
     IntCount2 = 0.
     IntIndex = 0. 
     IntIndex2 = 0.
     IntCountNoR = 0.
     IntIndexNoR = 0.
     IntCountNoR2 = 0.
     IntIndexNoR2 = 0.
           END. 
        END.
      
      /*Документ подозрительный*/
      UpdateSignsEx (op.class-code, STRING(op.op), "ВПДокИдент", "Подозрение"). 
        
   END.
END.

/*Функция проверяет является ли символ любой заглавной буквой*/
FUNCTION IsUpCaseAlpha RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN (ASC(iChr) >= ASC("A") AND ASC(iChr) <= ASC("Z")) OR
          (ASC(iChr) >= ASC("А") AND ASC(iChr) <= ASC("Я")) OR
           ASC(iChr)  = ASC("Ё").

END FUNCTION.

FUNCTION Two_str LOGICAL (INPUT OneStr          AS CHAR ,
    INPUT TwoStr     AS CHAR                                
    ):                                   
    DEFINE VAR vItm AS INT64 NO-UNDO.                
    DO vItm = 1 TO length(OneStr):       
       IF ASC(SUBSTRING(OneStr, vItm, 1)) <> ASC(SUBSTRING(TwoStr, vItm, 1)) THEN 
        do: 
      /*       MESSAGE  'разные2' vItm OneStr TwoStr ASC(SUBSTRING(OneStr, vItm, 1))  ASC(SUBSTRING(TwoStr, vItm, 1))  VIEW-AS ALERT-BOX.*/ 
            RETURN False.   
           LEAVE.             
        end.            
        /*MESSAGE  'разные' vItm OneStr TwoStr ASC(SUBSTRING(OneStr, vItm, 1))  ASC(SUBSTRING(TwoStr, vItm, 1))  VIEW-AS ALERT-BOX.        */ 
    end.
    RETURN true.      
END FUNCTION.

PROCEDURE IndexStr:
   DEF INPUT PARAM vStr AS CHAR NO-UNDO.                        /*содержание документа*/
   DEF INPUT PARAM vStrMask AS CHAR NO-UNDO.                    /**/   
   DEF INPUT PARAM iSense AS LOGICAL NO-UNDO.                   /*проверять на регистр следующую букву?*/
   DEFINE OUTPUT PARAMETER vCount AS INT64 INITIAL 0 NO-UNDO.             /*количество вхождений*/
   DEFINE OUTPUT PARAMETER vIndex   AS INT64   NO-UNDO.         /*индекс последнего вхождения*/

   DEFINE VAR vUpChr   AS CHAR            NO-UNDO. 
   DEFINE VAR vStr2   AS CHAR            NO-UNDO.  
        
         
     DO WHILE  (INDEX(vStr, vStrMask) > 0)  and Two_str(vStrMask,SUBSTRING(vStr ,INDEX(vStr, vStrMask), length(vStrMask))):   
         
            
          If iSense  Then 
             Do:
              vUpChr = SUBSTRING(vStr, INDEX(vStr, vStrMask) + LENGTH(vStrMask) ,1).
           /*     MESSAGE  vStrMask vStr vUpChr  VIEW-AS ALERT-BOX.*/
                
                
                if IsUpCaseAlpha (vUpChr)  THEN  
               do:  
                     vCount = vCount + 1.
                      vIndex = INDEX(vStr, vStrMask). 
                                    
                end.
               /*  vCount = vCount + 1.
                 vIndex = INDEX(vStr, vStrMask). 
                vStr2 = SUBSTRING(vStr2 ,INDEX(vStr, vStrMask), length(vStrMask)). */
                  
              end.  
            else  
             do:          
            vCount = vCount + 1.
            vIndex = INDEX(vStr, vStrMask). 
            end.
            
            
            vStr = SUBSTRING(vStr,INDEX(vStr, vStrMask) + LENGTH (vStrMask),LENGTH (vStr)).
                     /*
                     
             do:
               
       /*         vIndex = INDEX(vStr, vStrMask) + LENGTH(vStrMask).*/
             
              
                end.
              vStr =SUBSTRING(vStr,INDEX(vStr, vStrMask) + LENGTH (vStrMask) ,LENGTH (vStr)).
                
              /*  SUBSTRING(vDetail,
                                      INDEX(vDetail,vOneSens) + vInd2 , 
                                      LENGTH (vDetail) ).
       SUBSTRING(vStr,1,INDEX(vStr, vStrMask)).  */  
         */
   end.
END PROCEDURE.

PROCEDURE CountIndex:
   DEF INPUT PARAM pStr AS CHAR NO-UNDO.                        /*содержание документа*/
   DEF INPUT PARAM pStrMask AS CHAR NO-UNDO.                    /*не исп*/   
   DEF INPUT PARAM iSense AS LOGICAL NO-UNDO.                   /*учитывать ли регистр*/
   DEFINE OUTPUT PARAMETER vCount AS INT64 INITIAL 0 NO-UNDO.             /*количество вхождений*/
   DEFINE OUTPUT PARAMETER vIndex   AS INT64   NO-UNDO.         /*индекс последнего вхождения*/
   DEFINE VAR vOneSens AS CHAR     CASE-SENSITIVE  NO-UNDO.
   DEFINE VAR vItm     AS INT64           NO-UNDO.
   DEFINE VAR vCou2    AS INT64           NO-UNDO.
   DEFINE VAR vInd2    AS INT64           NO-UNDO.
       /* DEFINE VAR vOne     AS CHAR            NO-UNDO.
  
   DEFINE VAR vUpChr   AS CHAR            NO-UNDO.
    
   DEFINE VAR vDetail  AS CHAR            NO-UNDO.
   DEFINE VAR vMaskFlt AS CHAR            NO-UNDO.
   DEFINE VAR vCount   AS INT64           INITIAL 0 NO-UNDO.*/
   
   DEFINE VAR vFlag    AS LOGICAL INIT NO NO-UNDO.

    If NOT iSense THEN
    DO:
        pStr = LC(pStr).
        pStrMask = LC(pStrMask).
    END.             
    DO vItm = 1 TO NUM-ENTRIES(pStrMask, ";"):
        vOneSens = trim(ENTRY(vItm,pStrMask,";")) NO-ERROR. 
        IF (INDEX(vOneSens, "$#") > 0) then  
        do:
            vOneSens = REPLACE(vOneSens,"$#", "").
            RUN IndexStr(INPUT pStr, INPUT vOneSens, YES, OUTPUT vCou2, OUTPUT vInd2 ).
        end.     
        else     
            RUN IndexStr(INPUT pStr, INPUT vOneSens, no, OUTPUT vCou2, OUTPUT vInd2 ).
        /*do:
         IsUpCaseAlpha (SUBSTRING(pStr,vInd2 + LENGTH (vOneSens) - 1  ,1)) THEN
         */              
     /*                do: 
                vCount = vCount + vCou2.
                vIndex = vIndex + vInd2.
            end.  
        end.
        else */
            vCount = vCount + vCou2.            
            vIndex = vIndex + vInd2.
                    
          /* MESSAGE  vCount vIndex  vInd2 "|" vOneSens VIEW-AS ALERT-BOX.
        
             IF INDEX(vOneSens, "$#") Ne 0 THEN
            Do:               
                vOneSens = REPLACE(vOneSens,"$#", ""). 
                RUN IndexStr(INPUT pStr, INPUT vOneSens, YES, OUTPUT vCou2, OUTPUT vInd2 ).      
            end.  
            ELSE                 
                RUN IndexStr(INPUT pStr, INPUT vOneSens, NO, OUTPUT vCou2, OUTPUT vInd2 ).                   
                            
                 vCount = vCount + vCou2.
                 vIndex = vIndex + vInd2.                 
                                 If NOT iSense THEN
                
                   vInd2    = INDEX(vOneSens,"$#").*/
                /*запустить процедуру поиска количества вхождений в строке*/ /*должна выводить последний индекс*/
     /*проверить является следующая буква заглавной*/  
    end.
END PROCEDURE.
/*
FUNCTION CheckDetails LOGICAL (INPUT iFlDet    AS CHAR ,
                               INPUT iOpDet    AS CHAR ,
                               INPUT iSense    AS LOG
                               ):
   DEFINE VAR vItm     AS INT64           NO-UNDO.
   DEFINE VAR vOne     AS CHAR            NO-UNDO.
   DEFINE VAR vOneSens AS CHAR     CASE-SENSITIVE  NO-UNDO.
   DEFINE VAR vUpChr   AS CHAR            NO-UNDO.
   DEFINE VAR vFlag    AS LOGICAL INIT NO NO-UNDO.
   DEFINE VAR vInd2    AS INT64           NO-UNDO.
   DEFINE VAR vDetail  AS CHAR            NO-UNDO.
   DEFINE VAR vMaskFlt AS CHAR            NO-UNDO.
   DEFINE VAR vCount   AS INT64           INITIAL 0 NO-UNDO.


      MESSAGE vMaskFlt iFlDet VIEW-AS ALERT-BOX.

   /*не регистрочувствительная проверка*/
   IF NOT iSense THEN
  
      DO vItm = 1 TO NUM-ENTRIES(iFlDet, ";"):
         vOne = ENTRY(vItm,iFlDet,";").
         IF INDEX(iOpDet,vOne) GT 0 THEN
         DO:
       /*     IF CAN-DO(vMaskFlt,iOpDet) THEN*/
            DO:
               vFlag = YES.
               vCount = vCount + 1.
               LEAVE.
            END.
         END.
   END.
   ELSE
   /*регистрочувствительная проверка*/
   DO vItm = 1 TO NUM-ENTRIES(iFlDet, ";"):
      vOneSens = ENTRY(vItm,iFlDet,";") NO-ERROR.
      IF INDEX(vOneSens, "$#") EQ 0 THEN
      DO:
         IF INDEX(iOpDet,vOneSens) GT 0 THEN
         DO:
          /*  IF CAN-DO(vMaskFlt,iOpDet) THEN              пока просто посчитаем кол во вххождений*/ 
            DO:
               vFlag = YES.
              vCount = vCount + 1.
               LEAVE.
            END.
         END.
      END.
      ELSE
      DO:
         /*Проверяем являетчся ли $# - заглавной буквой*/
         ASSIGN  
            vDetail  = iOpDet
            vInd2    = INDEX(vOneSens,"$#") 
            vOneSens = " " +  TRIM(REPLACE(vOneSens," $#", "")) + " ".

         IF  INDEX(vDetail,vOneSens) GT 0  THEN 
            DO WHILE NOT vFlag AND vDetail <> "":
               vUpChr   = SUBSTRING(vDetail,
                                    INDEX(vDetail,vOneSens) + vInd2 ,
                                    1).
               IF    INDEX(vDetail,vOneSens) GT 0 
                 AND IsUpCaseAlpha (vUpChr) 
               THEN DO:
                 /* IF CAN-DO(vMaskFlt,vDetail) THEN*/
                   vCount = vCount + 1.
                    MESSAGE  "Ваил" vCount VIEW-AS ALERT-BOX. 
                     vFlag = YES.
                     end.
                  ELSE DO:
                     vDetail = SUBSTRING(vDetail,
                                         INDEX(vDetail,vOneSens) + vInd2 , 
                                         LENGTH (vDetail) ).
                     IF INDEX(vDetail,vOneSens) EQ 0  THEN 
                        vDetail = "".
                  END.
               END.
               ELSE DO:
                  vDetail = SUBSTRING(vDetail,
                                      INDEX(vDetail,vOneSens) + vInd2 , 
                                      LENGTH (vDetail) ).
                  IF INDEX(vDetail,vOneSens) EQ 0  THEN 
                     vDetail = "".
               END.
            END.
      END.  
  /* END.*/
   
RETURN vFlag.
END FUNCTION.

*/
/* $LINTFILE='vp_calc.p' */
/* $LINTMODE='1' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='nikk' */
/* $LINTDATE='02/08/2016 11:30:42.406+03:00' */
/*prosignJMxAKtP3c4WGApm+T/QaZA*/