{globals.i}  
{intrface.get xclass} 
{intrface.get tmess}
{intrface.get terr}
/*{debug.equ}*/

DEFINE VARIABLE mDbgPrint2 AS LOGICAL NO-UNDO.
mDbgPrint2 = NO.

DEFINE  STREAM rep.

DEFINE TEMP-TABLE ttFindSL NO-UNDO
            FIELD SLId     AS CHAR
            FIELD Name     AS CHAR
            FIELD Sym2     AS CHAR
            FIELD sym1     AS CHAR
            INDEX SLId SLId
            INDEX Name   Sym2 SLId
.
DEFINE TEMP-TABLE tt-view-sl NO-UNDO
            FIELD fld1  AS CHAR
            FIELD fld2  AS CHAR
            FIELD fld3  AS CHAR
            FIELD fld4  AS CHAR
            FIELD fld5  AS CHAR
            FIELD fld6  AS CHAR
            FIELD fld7  AS CHAR
            FIELD fld8  AS CHAR
            FIELD fld9  AS CHAR
            FIELD fld10 AS CHAR
            FIELD fld11 AS CHAR
            FIELD fld12 AS CHAR
            FIELD fld13 AS CHAR
            FIELD fld14 AS CHAR
.  
DEFINE TEMP-TABLE t-obj-sl NO-UNDO
            FIELD rec AS ROWID
            FIELD dop AS LOGICAL.
            
DEFINE TEMP-TABLE ttSLAction NO-UNDO
            FIELD StopListName   AS CHAR
            FIELD CreateAction   AS CHAR
            FIELD EditAction     AS CHAR
.

DEF VAR mDelimList AS CHAR     NO-UNDO.
ASSIGN 
   mDelimList =  fGetSetting("�⮯-�����","DelimSymbol","^")
   mDelimList =  IF NOT {assigned mDelimList} THEN "^" ELSE mDelimList.

FUNCTION ReplaceBadSL CHAR (INPUT iText AS CHAR):
   DEFINE VAR vBadList  AS CHAR     NO-UNDO INIT ",.;:".
   DEFINE VAR vLen      AS INT64    NO-UNDO.
   DEFINE VAR vItm      AS INT64    NO-UNDO.
   DEFINE VAR vName     AS CHAR     NO-UNDO.
   DEFINE VAR vOne      AS CHAR     NO-UNDO.

   vLen = LENGTH(vBadList).
   DO vItm = 1 TO vLen:
      iText = REPLACE(iText,SUBSTRING(vBadList,vItm,1),"").
   END.

   vName = "".
   DO vItm = 1 TO NUM-ENTRIES(iText," "):
      vOne = ENTRY(vItm,iText," ").
      IF vOne NE "" THEN vName = vName + " " + vOne.
   END.

   RETURN TRIM(vName).
END FUNCTION.


FUNCTION ReplaceQuotes CHAR (INPUT iText AS CHAR):
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


FUNCTION ReplaceSpecSL CHAR (INPUT iText AS CHAR):
   DEFINE VAR vBadList  AS CHAR   NO-UNDO INIT ",.".
   DEFINE VAR vLen      AS INT64  NO-UNDO.
   DEFINE VAR vItm      AS INT64  NO-UNDO.
   DEFINE VAR vOne      AS CHAR   NO-UNDO.

   DO vItm = 1 TO 19:
      vBadList = vBadList + CHR(vItm).
   END.

   vLen = length(vBadList).
   DO vItm = 1 TO vLen:
      iText = replace(iText,SUBSTRING(vBadList,vItm,1)," ").
   END.

   RETURN TRIM(iText).
END FUNCTION.

FUNCTION ReplaceSpec2SL CHAR (INPUT iText AS CHAR):
   DEFINE VAR vBadList  AS CHAR   NO-UNDO INIT " ,.;:-".
   DEFINE VAR vLen      AS INT64  NO-UNDO.
   DEFINE VAR vItm      AS INT64  NO-UNDO.
   DEFINE VAR vOne      AS CHAR   NO-UNDO.

   DO vItm = 1 TO 19:
      vBadList = vBadList + CHR(vItm).
   END.

   vLen = LENGTH(vBadList).
   DO vItm = 1 TO vLen:
      iText = REPLACE(iText,SUBSTRING(vBadList,vItm,1),"").
   END.

   RETURN TRIM(iText).
END FUNCTION.

PROCEDURE ChkStopListByName: 
   DEFINE INPUT PARAMETER iName    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.
      
   DEFINE VARIABLE vI       AS INT64       NO-UNDO.
   DEFINE VARIABLE vName1   AS CHARACTER   NO-UNDO.
   DEFINE BUFFER   DataLine FOR DataLine. 

   {empty ttFindSL}

   IF NOT {assigned iName} THEN
      RETURN.

   DO vI = 1  TO NUM-ENTRIES( iName, CHR(1)):
      vName1 = TRIM(ENTRY( vI,iName, CHR(1))).
      IF {assigned vName1} THEN
         FOR EACH DataLine WHERE
                  DataLine.Data-ID EQ INT64(iDataID)
              AND DataLine.Sym2    EQ vName1
              AND CAN-DO(iSLType,DataLine.txt)
         NO-LOCK:
            
            {stoplist.funi}   /*�᫨ ������ - NEXT*/
            
            IF NOT CAN-FIND(FIRST ttFindSL WHERE
                                  ttFindSL.SLId EQ DataLine.sym4) 
            THEN DO:
               CREATE ttFindSL.
               ASSIGN ttFindSL.SLId = DataLine.sym4
                      ttFindSL.Name = DataLine.txt.
            END.
         END.
   END.
   
END.

PROCEDURE ChkStopListByNamePerc: 
   DEFINE INPUT PARAMETER iName    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.
      
   DEFINE VARIABLE vI         AS INT64       NO-UNDO.
   DEFINE VARIABLE vName1     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mPercWord  AS INT64       NO-UNDO.
   DEFINE VARIABLE mPercLent  AS INT64       NO-UNDO.
   DEFINE VARIABLE vOK        AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER   DataLine FOR DataLine. 

   {empty ttFindSL}

   IF NOT {assigned iName} THEN
      RETURN.

   ASSIGN
      mPercLent = INT64(FGetSetting("�����","��愫���","60"))
      mPercWord = INT64(FGetSetting("�����","��摫��", "60")).
   FOR EACH DataLine WHERE
            DataLine.Data-ID EQ INT64(iDataID)
        
      NO-LOCK:
      
      
      
      IF NOT CAN-DO(iSLType,DataLine.txt) THEN NEXT.
        
      ASSIGN
         iName  = ReplaceBad(REPLACE(REPLACE(iName,        "�","�"),"�","�"))
         vName1 = ReplaceBad(REPLACE(REPLACE(DataLine.Sym2,"�","�"),"�","�")).

      vOK = CompareNameFast (
                             INPUT  vName1,
                             INPUT  iName,
                             INPUT  mPercLent,
                             INPUT  mPercWord).

      IF NOT vOK  THEN
         vOK = CompareNameSlow (
                                INPUT  vName1,
                                INPUT  iName,
                                INPUT  mPercLent,
                                INPUT  mPercWord).
      IF vOK  THEN
      DO:
         
         {stoplist.funi}   /*�᫨ ������ - NEXT*/
                  
         IF NOT CAN-FIND(FIRST ttFindSL WHERE
            ttFindSL.SLId EQ DataLine.Sym4) 
         THEN
         DO:
            CREATE ttFindSL.
            ASSIGN ttFindSL.SLId = DataLine.Sym4
                   ttFindSL.Name = DataLine.txt.
         END.
      END.
   END.
END.
PROCEDURE ChkStopListByOpRec: 
   DEFINE INPUT PARAMETER iName    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.

   DEFINE VARIABLE vI       AS INT64      NO-UNDO.
   DEFINE VARIABLE vName1   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDelim   AS CHAR       NO-UNDO.
   DEFINE VARIABLE vInd1    AS INT64      NO-UNDO.
   DEFINE VARIABLE vInd2    AS INT64      NO-UNDO.
   DEFINE VARIABLE vSubStr  AS CHAR       NO-UNDO.
   DEFINE VARIABLE vTmpStr  AS CHAR       NO-UNDO.

   DEFINE BUFFER   DataLine FOR DataLine. 
   {empty ttFindSL}

   DO vInd1 = 1 TO NUM-ENTRIES(mDelimList, "^"):
      vDelim = ENTRY(vInd1,mDelimList, "^").
      IF NOT {assigned vDelim} THEN 
         vDelim = CHR(2).
      ASSIGN   
         vTmpStr = REPLACE(iName,vDelim,chr(2))
         vDelim  = CHR(2).
         
      DO vInd2 = 1 TO NUM-ENTRIES(vTmpStr, vDelim):
         vSubStr = ENTRY(vInd2,vTmpStr,vDelim).

         DO vI = 1  TO NUM-ENTRIES( vSubStr, CHR(1)):
            vName1 = ENTRY( vI,vSubStr, CHR(1)).
            FOR EACH DataLine WHERE
                     DataLine.Data-ID EQ INT64(iDataID)
                 AND DataLine.Sym2    EQ  vName1
            NO-LOCK:
               
               {stoplist.funi}   /*�᫨ ������ - NEXT*/
               
               IF    (NOT {assigned DataLine.Sym3 }
                  OR TRIM(DataLine.Sym3,"0") EQ "")
                 AND CAN-DO(iSLType,DataLine.txt)
               THEN DO:
                  IF NOT CAN-FIND(FIRST ttFindSL WHERE
                                        ttFindSL.SLId EQ DataLine.sym4) 
                  THEN DO:
                     CREATE ttFindSL.
                     ASSIGN ttFindSL.SLId = DataLine.sym4
                            ttFindSL.Name = DataLine.txt.
                  END.
               END.
            END.
         END.
      END. 
   END.
END.

PROCEDURE ChkStopListByInn: 
   DEFINE INPUT PARAMETER iInn     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.
   
   iInn = TRIM(iInn).
   
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~niInn    = " + iInn + 
      "~niSLType = " + iSLType +
      "~niDataID = " + iDataID).
   
   {empty ttFindSL}
   FOR EACH DataLine WHERE
            DataLine.Data-ID EQ INT64(iDataID)
        AND DataLine.Sym3    EQ iInn
        AND CAN-DO(iSLType,DataLine.txt)
        NO-LOCK:
       
       
       
       IF TRIM(DataLine.Sym3,"0") EQ "" THEN
          NEXT.
       IF {assigned iInn} THEN DO:
          
          FIND FIRST ttFindSL WHERE
                     ttFindSL.SLId EQ DataLine.sym4
          NO-LOCK NO-ERROR.
                      
          IF NOT AVAIL  ttFindSL THEN
          DO:
             {stoplist.funi}   /*�᫨ ������ - NEXT*/
             CREATE ttFindSL.
             ASSIGN ttFindSL.SLId = DataLine.sym4
                    ttFindSL.Name = DataLine.txt.
             IF mDbgPrint2 EQ YES THEN
             RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
                "~nDataLine.sym3 = " + DataLine.sym3 + 
                "~nDataLine.sym4 = " + DataLine.sym4 +
                "~nDataLine.txt  = " + DataLine.txt).
          END.
      END.
   END.
END.

PROCEDURE ChkStopListByPasp: 
   DEFINE INPUT PARAMETER iPasp    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.

   {empty ttFindSL}
   
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~niPasp   = " + iPasp + 
      "~niSLType = " + iSLType +
      "~niDataID = " + iDataID).
   
   FOR EACH DataLine WHERE
            DataLine.Data-ID EQ INT64(iDataID)
      AND DataLine.Sym1    BEGINS  iPasp
      AND CAN-DO(iSLType,DataLine.txt)
      NO-LOCK:
         
      {stoplist.funi}   /*�᫨ ������ - NEXT*/
      
      IF  ENTRY(1,DataLine.Sym1,CHR(1)) NE  iPasp THEN
          NEXT.
       FIND FIRST ttFindSL WHERE
                  ttFindSL.SLId EQ DataLine.sym4
       NO-LOCK NO-ERROR.
                       
      IF NOT AVAIL  ttFindSL THEN
      DO:
         CREATE ttFindSL.
         ASSIGN ttFindSL.SLId = DataLine.sym4
                ttFindSL.Name = DataLine.txt.
         IF mDbgPrint2 EQ YES THEN
         RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
            "~nDataLine.sym1 = " + DataLine.sym1 + 
            "~nDataLine.sym4 = " + DataLine.sym4 +
            "~nDataLine.txt  = " + DataLine.txt).
      END.
   END.
  
END.

PROCEDURE ChkStopListByFIO-INN: 
   DEFINE INPUT PARAMETER iName     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iDataID  AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE vI       AS INT64       NO-UNDO.
   DEFINE VARIABLE vName1   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vINN     AS CHARACTER   NO-UNDO.
   DEFINE BUFFER   DataLine FOR DataLine. 

   {empty ttFindSL}

/*   DO vI = 1  TO NUM-ENTRIES( iName, CHR(1)):*/
      vName1 = ENTRY(1,iName,CHR(1)).
      IF NUM-ENTRIES(iName,CHR(1)) GT 1 THEN
         vINN   = ENTRY(2,iName,CHR(1)).
   
      IF mDbgPrint2 EQ YES THEN
      RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
         "~niName   = " + iName +
         "~nvINN    = " + vINN + 
         "~niSLType = " + iSLType +
         "~niDataID = " + iDataID).   
      
      FOR EACH DataLine WHERE
               DataLine.Data-ID EQ INT64(iDataID)
           AND DataLine.Sym2 EQ vName1
           AND DataLine.Sym3 EQ vINN
           AND CAN-DO(iSLType,DataLine.txt)
         NO-LOCK:
         
         {stoplist.funi}   /*�᫨ ������ - NEXT*/
         
         IF TRUE     
/*            (NOT {assigned DataLine.Sym3}          */
/*                  OR TRIM(DataLine.Sym3,"0") EQ "")
            AND ENTRY(1,DataLine.Sym1,CHR(1)) EQ ""*/
         THEN
         DO:
            IF NOT CAN-FIND(FIRST ttFindSL WHERE
                                  ttFindSL.SLId EQ DataLine.sym4) 
            THEN DO:
              CREATE ttFindSL.
              ASSIGN ttFindSL.SLId = DataLine.sym4
                     ttFindSL.Name = DataLine.txt.
              IF mDbgPrint2 EQ YES THEN
              RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
                 "~nDataLine.sym2 = " + DataLine.sym2 + 
                 "~nDataLine.sym4 = " + DataLine.sym4 +
                 "~nDataLine.txt  = " + DataLine.txt).
            END.
         END.
      END.
/*   END.*/
END.

PROCEDURE ChkStopListByBirthDay: 
   DEFINE INPUT  PARAMETER iBDay    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDataID  AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER isym4    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oKeep    AS LOGICAL   NO-UNDO.
     
   IF NOT {assigned iBDay} THEN
   DO:
      oKeep = YES.
      RETURN.
   END.
      
   FOR EACH DataLine WHERE
            DataLine.Data-ID EQ iDataID
      AND   DataLine.sym4    EQ isym4
      AND CAN-DO(iSLType,DataLine.txt)
      NO-LOCK:
         
      {stoplist.funi}   /*�᫨ ������ - NEXT*/
      
      IF {assigned ENTRY(2,DataLine.sym1,CHR(1))} THEN
      DO:
         IF iBDay EQ ENTRY(2,DataLine.sym1,CHR(1)) 
         THEN oKeep = YES.
         ELSE oKeep = NO.
      END.
      ELSE oKeep = YES.
   END.
END.

PROCEDURE ChkStopListByBirthPlace: 
   DEFINE INPUT  PARAMETER iBPlace  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iSLType  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDataID  AS INT64     NO-UNDO.
   DEFINE INPUT  PARAMETER isym4    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oKeep    AS LOGICAL   NO-UNDO.   
   
   IF NOT {assigned iBPlace} THEN
   DO:
      oKeep = YES.
      RETURN.
   END.
   FOR EACH DataLine WHERE
            DataLine.Data-ID EQ INT64(iDataID)
      AND   DataLine.sym4    EQ isym4
      AND CAN-DO(iSLType,DataLine.txt)
      NO-LOCK:
      
      {stoplist.funi}   /*�᫨ ������ - NEXT*/
      
      IF {assigned ENTRY(3,DataLine.sym1,CHR(1))} THEN
      DO:
         IF ReplaceSpec2SL(iBPlace) EQ 
            ReplaceSpec2SL(ENTRY(3,DataLine.sym1,CHR(1))) 
         THEN oKeep = YES.
         ELSE oKeep = NO.
      END.
      ELSE oKeep = YES.
   END.
END.

PROCEDURE Fill-ttSL:
   DEFINE INPUT PARAMETER iChkType   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iChsStr    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSLType    AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR t-obj-sl.
   
   DEFINE VAR vStr      AS CHAR     NO-UNDO.
   DEFINE VAR vDataID   AS INT64    NO-UNDO.
   DEFINE BUFFER code FOR code.

   ASSIGN
      vStr    = IF iChkType EQ "���_��"  
                THEN iChsStr
                ELSE ReplaceBadSL(iChsStr)  
      vDataID = INT64(GetCodeMisc("","StopList",8))
   NO-ERROR.
 
   IF vDataID EQ 0 OR
      vDataID EQ ? THEN DO:
      RUN Fill-SysMes("","","-1",
                      "�� ������� ��뫪� �� �����䨪��� StopList"  + "~n" +
                      "�� ����� ����ண� ���᪠. ����஫� ����������."   + "~n" +
                      "�믮���� ���� ����� StopList ��� ��ࠢ����� �訡��.").
      RETURN.
   END.
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~niChkType = " + iChkType).
   CASE iChkType:
      WHEN "���" THEN
         RUN ChkStopListByName (vStr,iSLType,vDataID).
      WHEN "���_��" THEN
         RUN ChkStopListByOpRec(vStr,iSLType,vDataID).
      WHEN "���" THEN
         RUN ChkStopListByPasp (REPLACE(vStr, " ",""),iSLType,vDataID).
      WHEN "���" THEN
         RUN ChkStopListByInn  (vStr,iSLType, vDataID).
      WHEN "���-���" THEN
         RUN ChkStopListByFIO-INN(vStr,iSLType, vDataID).
/*      WHEN "��" THEN                                        */
/*         RUN ChkStopListByBirthDay(vStr,iSLType, vDataID).  */
/*      WHEN "��" THEN                                        */
/*         RUN ChkStopListByBirthPlace(vStr,iSLType, vDataID).*/
      WHEN "������" THEN
         RUN ChkStopListByNamePerc(vStr,iSLType,vDataID).
   END CASE.

   FOR EACH ttFindSL:
      FIND FIRST code WHERE
                 code.class EQ "StopList"
             AND code.code  EQ ttFindSL.SLId
      NO-LOCK NO-ERROR.
      IF AVAILABLE(code) THEN
      DO:
         IF mDbgPrint2 EQ YES THEN
         RUN dbgprint.p ("chkstoplist","Add " + GetSysConf("SLSubjectID") +
                                          ": " + iChkType + " " + vStr).
         CREATE t-obj-sl.
         ASSIGN t-obj-sl.rec = ROWID(code).
         IF    iChkType EQ "��"
            OR iChkType EQ "��" 
            OR iChkType EQ "������" THEN
            ASSIGN t-obj-sl.dop = YES.
         IF mDbgPrint2 EQ YES THEN
         RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
            "~nt-obj-sl.rec = " + STRING(t-obj-sl.rec) +
            "~nt-obj-sl.dop = " + STRING(t-obj-sl.dop)).
      END.
   END.
END PROCEDURE.

PROCEDURE Cre-tt-sl:
   DEFINE INPUT PARAMETER  iPar1 AS CHAR.
   DEFINE INPUT PARAMETER  iPar2 AS CHAR.
   DEFINE INPUT PARAMETER  iPar3 AS CHAR.
   DEFINE INPUT PARAMETER  iPar4 AS CHAR.
   DEFINE INPUT PARAMETER  iPar5 AS CHAR.
   FOR EACH t-obj-sl NO-LOCK,
       FIRST code WHERE ROWID(code) EQ t-obj-sl.rec
                  NO-LOCK BREAK BY t-obj-sl.rec:
      IF FIRST-OF(t-obj-sl.rec) THEN
      DO:
         CREATE tt-view-sl.
   
         &IF DEFINED  (iChkCl) NE 0 &THEN
         CASE iPar1:
            WHEN "person"    THEN tt-view-sl.fld1 = "�".
            WHEN "cust-corp" THEN tt-view-sl.fld1 = "�".
            WHEN "banks"     THEN tt-view-sl.fld1 = "�".
         END CASE.
   
         ASSIGN
            tt-view-sl.fld2  = iPar2                                           /*��� ������ */
            tt-view-sl.fld3  = iPar3                                           /*������������ ������*/
            tt-view-sl.fld4  = IF {assigned iPar4 }  THEN  iPar4 ELSE " "      /*���*/
            tt-view-sl.fld5  = IF {assigned iPar5 }  THEN  iPar5 ELSE " "      /*��ᯮ��*/
   
         
         &ELSE
         ASSIGN
            tt-view-sl.fld1 = iPar1     /*����� ���㬥��*/
            tt-view-sl.fld2 = iPar2     /*��� ���㬥��*/
            tt-view-sl.fld3 = iPar3     /*���਩*/
            tt-view-sl.fld4 = iPar4     /*���祭�� �����*/
            tt-view-sl.fld5 = " "       /*�����誠*/
               
         &ENDIF
         
            tt-view-sl.fld6   = code.description[1]   
            tt-view-sl.fld7   = code.name
            tt-view-sl.fld8   = REPLACE(code.misc[3], "~n", "")
            tt-view-sl.fld9   = code.misc[5]
            tt-view-sl.fld10  = code.val
            tt-view-sl.fld11  = GetXattrValueEx("code",code.class + "," + code.code,"RezolutionData","")
            tt-view-sl.fld12  = GetXattrValueEx("code",code.class + "," + code.code,"birthday","")
            tt-view-sl.fld13  = GetXattrValueEx("code",code.class + "," + code.code,"birthplace","")
            tt-view-sl.fld14  = code.misc[7].
         
         IF mDbgPrint2 EQ YES THEN
         RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
            "~ntt-view-sl.fld1 = " + STRING(tt-view-sl.fld1) +
            "~ntt-view-sl.fld2 = " + STRING(tt-view-sl.fld2) + 
            "~ntt-view-sl.fld3 = " + STRING(tt-view-sl.fld3) + 
            "~ntt-view-sl.fld4 = " + STRING(tt-view-sl.fld4)).
      END.
   END.
   FOR EACH t-obj-sl EXCLUSIVE-LOCK:
      DELETE t-obj-sl.
   END.
END PROCEDURE.
 
PROCEDURE Cre-tt-sl-clnt:
   DEFINE INPUT PARAMETER  iPar1 AS CHAR.
   DEFINE INPUT PARAMETER  iPar2 AS CHAR.
   DEFINE INPUT PARAMETER  iPar3 AS CHAR.
   DEFINE INPUT PARAMETER  iPar4 AS CHAR.
   DEFINE INPUT PARAMETER  iPar5 AS CHAR.
   FOR EACH t-obj-sl NO-LOCK,
       FIRST code WHERE ROWID(code) EQ t-obj-sl.rec
                  NO-LOCK BREAK BY t-obj-sl.rec:

      IF FIRST-OF(t-obj-sl.rec) THEN
      DO:
         CREATE tt-view-sl.
   
         CASE iPar1:
            WHEN "person"    THEN tt-view-sl.fld1 = "�".
            WHEN "cust-corp" THEN tt-view-sl.fld1 = "�".
            WHEN "banks"     THEN tt-view-sl.fld1 = "�".
         END CASE.
   
         ASSIGN
            tt-view-sl.fld2   = iPar2                                           /*��� ������ */
            tt-view-sl.fld3   = iPar3                                           /*������������ ������*/
            tt-view-sl.fld4   = IF {assigned iPar4 }  THEN  iPar4 ELSE " "      /*���*/
            tt-view-sl.fld5   = IF {assigned iPar5 }  THEN  iPar5 ELSE " "      /*��ᯮ��*/
            tt-view-sl.fld6   = code.description[1]   
            tt-view-sl.fld7   = code.name 
            tt-view-sl.fld8   = REPLACE(code.misc[3], "~n", "")
            tt-view-sl.fld9   = code.misc[5]
            tt-view-sl.fld10  = code.val
            tt-view-sl.fld11  = GetXattrValueEx("code",code.class + "," + code.code,"RezolutionData","")
            tt-view-sl.fld12  = GetXattrValueEx("code",code.class + "," + code.code,"birthday","")
            tt-view-sl.fld13  = GetXattrValueEx("code",code.class + "," + code.code,"birthplace","")
            tt-view-sl.fld14  = code.misc[7].
      END.
   END.
   FOR EACH t-obj-sl EXCLUSIVE-LOCK:
      DELETE t-obj-sl.
   END.
END PROCEDURE.

FUNCTION GetKrVal RETURN CHAR (iCust-cat  AS CHAR,
                                iCust-id   AS  INT64):
DEFINE VARIABLE vKval AS CHARACTER NO-UNDO.
CASE iCust-cat:

WHEN "�" THEN
   DO:
      FIND FIRST cust-corp WHERE cust-corp.cust-id EQ iCust-id NO-LOCK NO-ERROR.
      IF AVAIL cust-corp THEN
         vKval = cust-corp.name-corp.
   END.
   WHEN "�" THEN
   DO:
      FIND FIRST person WHERE person.person-id EQ  iCust-id NO-LOCK NO-ERROR.
      IF AVAIL person THEN
         vKval =  person.name-last + ' ' + person.first-name .
   END. 
   WHEN "�" THEN
   DO:
      FIND FIRST banks WHERE banks.bank-id EQ   iCust-id NO-LOCK NO-ERROR.
      IF AVAIL banks THEN
         vKval = banks.short-name.
   END.
   END CASE.
   RETURN vKval.
END FUNCTION.


PROCEDURE ChkStopListOp:
   DEF INPUT  PARAMETER iOp     AS INT64 NO-UNDO.
   DEF INPUT  PARAMETER iSLType AS CHAR  NO-UNDO.
   DEF OUTPUT PARAMETER oRezult AS LOG   NO-UNDO.
   
   DEF VAR mClType   AS CHAR     NO-UNDO.  /*⨯ �����⮢ ��� �஢�ન*/
   DEF VAR mSLtype   AS CHAR     NO-UNDO.  /*���� �⮯-���⮢, �� ����� �஢����� �����⮢*/
   DEF VAR mChkType  AS CHAR     NO-UNDO.  /*��� �஢�ન*/
   DEF VAR mCustDoc  AS CHAR     NO-UNDO.
   DEF VAR mKName    AS CHAR     NO-UNDO.
   DEF VAR mKVal     AS CHAR     NO-UNDO.
   DEF VAR mInnSend  AS CHAR     NO-UNDO.
   DEF VAR mInnRec   AS CHAR     NO-UNDO.
   DEF VAR mNameSend AS CHAR     NO-UNDO.
   DEF VAR mNameRec  AS CHAR     NO-UNDO.
   DEF VAR mBankInn  AS CHAR     NO-UNDO.
   DEF VAR mAcctSend AS CHAR     NO-UNDO.
   DEF VAR mAcctRec  AS CHAR     NO-UNDO.
   DEF VAR mRez      AS LOG      NO-UNDO.

   DEF BUFFER op FOR {&ttw}op.
   DEF BUFFER op-entry FOR {&ttw}op-entry.
   mSLtype = iSLType.
   mChkType = "*".
   FIND FIRST op WHERE op.op EQ iOp NO-LOCK NO-ERROR.
   IF AVAIL op THEN 
   DO: 
      {empty ttFindSL}
      {empty t-obj-sl}

      /*�஢��塞 ��� �� �஢����*/
      FOR EACH op-entry WHERE op-entry.op EQ op.op NO-LOCK:
         /*�஢��塞 ��� �� �।���*/
         IF {assigned op-entry.acct-cr } THEN
         DO:
            {find-act.i &acct=op-entry.acct-cr}
            IF AVAIL acct   THEN DO:
               mKName = "�� " + DelFilFromAcct(STRING(op-entry.acct-cr)).
               IF mDbgPrint2 EQ YES THEN
               RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
                  "~nmKName = " + mKName).
               RUN ChkByStopList(acct.cust-cat, acct.cust-id, iSLType, "OP", OUTPUT mRez).
               IF mRez THEN
                  RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),mKname, GetKrVal(acct.cust-cat, acct.cust-id), " " ).
            END.                    
         END.
         /*�஢��塞 ��� �� ������*/
         IF {assigned op-entry.acct-db } THEN
         DO:
             mKName = "�� " + DelFilFromAcct(STRING(op-entry.acct-db)).
             IF mDbgPrint2 EQ YES THEN
             RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
               "~nmKName = " + mKName).
            {find-act.i &acct=op-entry.acct-db}
            IF AVAIL acct   THEN DO:
               RUN ChkByStopList(acct.cust-cat, acct.cust-id, iSLType, "OP", OUTPUT mRez).
               IF mRez THEN
                  RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),mKname,GetKrVal(acct.cust-cat, acct.cust-id), " " ).
            END.                    
         END.
      END.
      /*�஢��塞 ��� �� �� acct-send acct-rec*/
      ASSIGN 
         mAcctSend = GetXattrValueEx("op", STRING(op.op), "acct-send","")
         mAcctRec  = GetXattrValueEx("op", STRING(op.op), "acct-rec", "").

      IF {assigned mAcctSend } THEN
      DO:
         {find-act.i &acct=mAcctSend}
         IF AVAIL acct   THEN DO:
            mKName = "acct-send " + mAcctSend.
            RUN ChkByStopList(acct.cust-cat, acct.cust-id, iSLType, "OP", OUTPUT mRez).
            IF mRez THEN
               RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),mKname,GetKrVal(acct.cust-cat, acct.cust-id), " " ).
         END.                    
      END.      
      IF {assigned mAcctRec} THEN
      DO:
          mKName = "acct-rec " + STRING(mAcctRec).
         {find-act.i &acct=mAcctRec}
         IF AVAIL acct   THEN DO:
            RUN ChkByStopList(acct.cust-cat, acct.cust-id, iSLType, "OP", OUTPUT mRez).
            IF mRez THEN
               RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),mKname,GetKrVal(acct.cust-cat, acct.cust-id), " " ).
         END.                    
      END.
      
      /*�஢��塞 ४������ ���㬥��*/

      ASSIGN 
         mInnSend    = GetXattrValueEx("op",STRING(op.op), "inn-send" ,"")
         mInnRec     = GetXattrValueEx("op",STRING(op.op), "inn-rec  ","")
         mNameSend   = GetXattrValueEx("op",STRING(op.op), "name-send","")
         mNameRec    = GetXattrValueEx("op",STRING(op.op), "name-rec" ,"")
      .

      IF {assigned op.inn} THEN
      DO:
         RUN Fill-ttSL("���", op.inn, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"inn",op.inn, " " ).
      END.
      
      IF {assigned mInnSend} THEN
      DO:
         RUN Fill-ttSL("���", mInnSend, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"inn-send",mInnSend, " " ).
      END.
      
      IF {assigned mInnRec} THEN
      DO:
         RUN Fill-ttSL("���", mInnRec, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"inn-rec",mInnRec, " " ).
      END.

      IF {assigned op.name-ben} THEN
      DO:
         RUN Fill-ttSL("���_��", op.name-ben, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"name-ben",op.name-ben, " " ).
      END.
      
      IF {assigned op.details} THEN
      DO:
         RUN Fill-ttSL("���_��", op.details, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"details",op.details, " " ).
      END.
      
      IF    {assigned mNameSend} THEN 
      DO:
         RUN Fill-ttSL("���_��", mNameSend, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"name-send",mNameSend, " " ).
      END.
      
      IF    {assigned mNameRec}  THEN
      DO:
         RUN Fill-ttSL("���_��", mNameRec, iSLType, INPUT-OUTPUT TABLE t-obj-sl).
         IF CAN-FIND(FIRST t-obj-sl) THEN
            RUN Cre-tt-sl (STRING(op.doc-num), STRING(op.doc-date),"name-rec",mNameRec, " " ).
      END.
   END.
   FIND FIRST tt-view-sl NO-LOCK NO-ERROR.
   oRezult = AVAIL tt-view-sl.
END PROCEDURE.
          
PROCEDURE PrintOpSLRep.
   DEF VAR vNum        AS INT64         NO-UNDO.
   DEF VAR vI          AS INT64         NO-UNDO.
   DEF VAR vKritval    AS CHAR EXTENT 5 NO-UNDO.
   DEF VAR vSL         AS CHAR EXTENT 5 NO-UNDO.
   DEF VAR vNameFromSL AS CHAR EXTENT 5 NO-UNDO.
   DEF VAR vResolution AS CHAR EXTENT 5 NO-UNDO.
   
   FIND FIRST tt-view-sl NO-ERROR.
   IF AVAIL tt-view-sl THEN
   DO:
                 
      /*�뢮� ����*/
      
      PUT STREAM rep " ���㬥��� � 䨣�࠭⠬� �ࠢ�筨�� '�⮯-�����'"  SKIP .
      PUT STREAM rep SKIP(1).
      PUT STREAM rep "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
      PUT STREAM rep "�� �/� � ��� ���㬥�� � � ���㬥��  �          �ਬ�砭��          �   ���਩                  �    ������������ �����     �  � �/� �� �      ���/������������        �      ���        �   ����� १���樨  �    ���    �" SKIP.
      PUT STREAM rep "�      �                �              �                              �                             �                              � �⮯-���⠳       �� �⮯-����          �  �� �⮯-����  �                    �  १���樨 �" SKIP.
      FOR EACH tt-view-sl NO-LOCK 
                          BREAK BY tt-view-sl.fld3 
                                BY tt-view-sl.fld2 
                                BY tt-view-sl.fld1 
                                BY tt-view-sl.fld7:
         IF       FIRST-OF(tt-view-sl.fld3)
            AND   FIRST-OF(tt-view-sl.fld2)
            AND   FIRST-OF(tt-view-sl.fld1)
            AND   FIRST-OF(tt-view-sl.fld7)
         THEN
         DO:
                 
         PUT STREAM rep "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.   
         ASSIGN
            vNum           =  vNum + 1
            vKritval[1]    =  tt-view-sl.fld4
            vSL[1]         =  tt-view-sl.fld14
            vNameFromSL[1] =  tt-view-sl.fld8
            vResolution[1] =  tt-view-sl.fld10.
            
         {wordwrap.i &s=vKritval    &n=5 &l=30 }
         {wordwrap.i &s=vSL         &n=5 &l=30 }
         {wordwrap.i &s=vNameFromSL &n=5 &l=30 }
         {wordwrap.i &s=vResolution &n=5 &l=20 }
      
         PUT STREAM rep  "�" STRING(vNum)        FORMAT "X(6)"   +                                       /*�����*/
                         "�" tt-view-sl.fld2     FORMAT "X(16)"  +                                       /*��� ���㬥��*/
                         "�" tt-view-sl.fld1     FORMAT "X(14)"  +                                       /*����� ���㬥��*/
                         "�" IF vSL[1]           NE "" THEN vSL[1]           ELSE " " FORMAT "X(30)"  +  /*��� �⮯ ����*/
                         "�" tt-view-sl.fld3     FORMAT "X(29)"  +                                       /*���਩*/
                         "�" vKritval[1]         FORMAT "X(30)"  +                                       /*���祭�� �����*/
                         "�" tt-view-sl.fld6     FORMAT "X(11)"  +                                       /*����� �⮯ ���*/
                         "�" IF vNameFromSL[1]   NE "" THEN vNameFromSL[1]   ELSE " " FORMAT "X(30)"  +  /*������������ �� �⮯ ����*/
                         "�" IF tt-view-sl.fld9  NE "" THEN tt-view-sl.fld9  ELSE " " FORMAT "X(17)"  +  /*��� �� �⮯ ����*/
                         "�" IF vResolution[1]   NE "" THEN vResolution[1]   ELSE " " FORMAT "X(20)"  +  /*����� १���樨 */
                         "�" IF tt-view-sl.fld11 NE "" THEN tt-view-sl.fld11 ELSE " " FORMAT "X(12)"  +  /*��� १���樨 */
                         "�"
         SKIP. 
        
         DO vI = 2 TO 5:
            IF    vKritval[vI]    NE ""
               OR vSl[vI]         NE ""
               OR vNameFromSL[vI] NE ""
               OR vResolution[vI] NE ""
            THEN
               PUT STREAM rep  "�" " "       FORMAT "X(6)"   + 
                               "�" " "       FORMAT "X(16)"  + 
                               "�" " "       FORMAT "X(14)"  +
                               "�" IF vSL[vI]           NE "" THEN vSL[vI]         ELSE " " FORMAT "X(30)"  + 
                               "�" " "       FORMAT "X(29)"  + 
                               "�" IF vKritval[vI]      NE "" THEN vKritval[vI]    ELSE " " FORMAT "X(30)"  + 
                               "�" " "       FORMAT "X(11)"  + 
                               "�" IF vNameFromSL[vI]   NE "" THEN vNameFromSL[vI] ELSE " " FORMAT "X(30)"  + 
                               "�" " "       FORMAT "X(17)"  + 
                               "�" IF vResolution[vI]   NE "" THEN vResolution[vI] ELSE " " FORMAT "X(20)"  + 
                               "�" " "       FORMAT "X(12)"  + 
                               "�"
               SKIP.
         END.
         END.
         
      END.
      PUT STREAM rep "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP.
   END.
END PROCEDURE.    

PROCEDURE PrintClientSLRep.

   DEFINE VARIABLE vName        AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vNameFromSL  AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vSL          AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vResolution  AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vBirthPlace  AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vNum         AS INT64              NO-UNDO.
   DEFINE VARIABLE vI           AS INT64              NO-UNDO.

   /*�뢮� ����*/

   PUT STREAM rep " �������, ᮢ���訥 � 䨣�࠭⠬� �ࠢ�筨�� '�⮯-�����'"  SKIP .
   PUT STREAM rep SKIP(1).
   PUT STREAM rep "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
   PUT STREAM rep "�� �/� � ��� � ���  �     ���/������������         �          �ਬ�砭��          �    ���      � ���� � ����� � � �/� ��  �      ���/������������        �      ���        � ��� ஦����� �    ���� ஦�����            �  ����� १���樨   �    ���    �" SKIP.
   PUT STREAM rep "�      �     �      �                              �                              �             �   ��ᯮ��    ��⮯-���� �       �� �⮯-����          �  �� �⮯-����  � �� �⮯-���� �    �� �⮯-����             �                    �  १���樨 �" SKIP.

   FOR EACH tt-view-sl BY tt-view-sl.fld7 BY tt-view-sl.fld1 BY tt-view-sl.fld3:
      ASSIGN
         vNum           =  vNum + 1
         vName[1]       =  tt-view-sl.fld3
         vSL[1]        =  tt-view-sl.fld14
         vNameFromSL[1] =  tt-view-sl.fld8
         vResolution[1] =  tt-view-sl.fld10
         vBirthPlace[1] =  tt-view-sl.fld13.
      
      {wordwrap.i &s=vName       &n=5 &l=30 }
      {wordwrap.i &s=vSL         &n=5 &l=30 }
      {wordwrap.i &s=vNameFromSL &n=5 &l=30 }
      {wordwrap.i &s=vResolution &n=5 &l=20 }
      {wordwrap.i &s=vBirthPlace &n=5 &l=30 }
      PUT STREAM rep  "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
      PUT STREAM rep  "�" STRING(vNum)                                                 FORMAT "X(6)"        +
                      "�" tt-view-sl.fld1                                              FORMAT "X(5)"        +  /*⨯ ������*/
                      "�" tt-view-sl.fld2                                              FORMAT "X(6)"        +  /*����� ������*/
                      "�" vName[1]                                                     FORMAT "X(30)"       +  /*������������ ������*/
                      "�" IF vSL[1]           NE ""     THEN vSL[1]           ELSE " " FORMAT "X(30)"       +  /*��� �⮯ ����*/
                      "�" tt-view-sl.fld4                                              FORMAT "X(13)"       +  /*���*/
                      "�" tt-view-sl.fld5                                              FORMAT "X(15)"       +  /*��ᯮ��*/
                      "�" tt-view-sl.fld6                                              FORMAT "X(11)"       +  /*����� �⮯-����*/
                      "�" IF vNameFromSL[1]   NE ""     THEN vNameFromSL[1]   ELSE " " FORMAT "X(30)"       +  /*������������ �� �⮯-����*/
                      "�" IF {assigned tt-view-sl.fld9} THEN tt-view-sl.fld9  ELSE " " FORMAT "X(17)"       +  /*��� �� �⮯-����*/
                      "�  " IF tt-view-sl.fld12 NE ""   THEN tt-view-sl.fld12 ELSE " " FORMAT "X(13)"       +  /*��� ஦����� */
                      "�" IF vBirthPlace[1]   NE ""     THEN vBirthPlace[1]   ELSE " " FORMAT "X(30)"       +  /*���� ஦����� */       
                      "�" IF vResolution[1]   NE ""     THEN vResolution[1]   ELSE " " FORMAT "X(20)"       +  /*����� १���樨 */
                      "�" IF tt-view-sl.fld11 NE ""     THEN tt-view-sl.fld11 ELSE " " FORMAT "X(12)"       +  /*��� १���樨 */
                      "�"
      SKIP. 
      DO vI = 2 TO 5:
         IF    vName[vI]       NE ""
            OR vSl[vI]         NE ""
            OR vNameFromSL[vI] NE ""
            OR vResolution[vI] NE ""
         THEN
            PUT STREAM rep  "�" " "       FORMAT "X(6)"   + 
                            "�" " "       FORMAT "X(5)"   + 
                            "�" " "       FORMAT "X(6)"   + 
                            "�" IF vName[vI]        NE "" THEN vName[vI]       ELSE " " FORMAT "X(30)"  + 
                            "�" IF vSL[vI]          NE "" THEN vSL[vI]         ELSE " " FORMAT "X(11)"  +
                            "�" " "       FORMAT "X(13)"  + 
                            "�" " "       FORMAT "X(15)"  + 
                            "�" " "       FORMAT "X(30)"  + 
                            "�" IF vNameFromSL[vI]  NE "" THEN vNameFromSL[vI] ELSE " " FORMAT "X(30)"  + 
                            "�" " "       FORMAT "X(17)"  + 
                            "�" " "       FORMAT "X(15)"  +
                            "�" IF vBirthPlace[vI]  NE "" THEN vBirthPlace[vI] ELSE " " FORMAT "X(30)" +
                            "�" IF vResolution[vI]  NE "" THEN vResolution[vI] ELSE " " FORMAT "X(20)"  + 
                            "�" " "       FORMAT "X(12)"  + 
                            "�"
            SKIP.
      END.
   END.
   PUT STREAM rep "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������"SKIP.

END PROCEDURE.
                  
FUNCTION CalcCodeNum RETURN INT64 (iName AS CHAR):

   DEFINE BUFFER   bfcode        FOR code. 
   DEFINE VARIABLE vCodeNum      AS INT64  NO-UNDO.
   VALIDATE CODE.
   FIND FIRST    bfcode WHERE 
             bfcode.class  EQ "StopList"
         AND bfcode.parent EQ "StopList" 
         AND bfcode.name   EQ iName 
   NO-LOCK NO-ERROR.
   IF NOT AVAIL bfcode THEN
      RETURN 1.

   FOR EACH  bfcode WHERE 
             bfcode.class  EQ "StopList"
         AND bfcode.name   EQ iName 
   NO-LOCK  BY bfcode.name BY INT64(bfcode.description[1]) DESC :  
      IF {assigned bfcode.DESCRIPTION[1]} THEN
      DO:
          vCodeNum =  INT64(bfcode.DESCRIPTION[1]) NO-ERROR.
         LEAVE.
      END.
   END.
      vCodeNum = vCodeNum + 1.
   RETURN vCodeNum.      
END FUNCTION.                                                        

FUNCTION ChkClientByStopList RETURN LOGICAL
  (iClass      AS CHAR,     
   iCust-id    AS CHAR, 
   iName       AS CHAR, 
   iShortName  AS CHAR, 
   iInn        AS CHAR, 
   iDocument   AS CHAR,
   iBirthDay   AS DATE,
   iBirthPlace AS CHAR,
   iSLType     AS CHAR,
   iMode       AS CHAR):

   DEFINE VARIABLE vNameForRep         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustName           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSLtype             AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vControlInsEditClnt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vNPContInsEditClnt  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDopControl         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDopCriteria        AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vDopBirthDay        AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vDopBirthPlace      AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vDopFIO             AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vTmpStr1            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpStr2            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vI                  AS INT64     NO-UNDO.
   DEFINE VARIABLE vRezult             AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vDataID             AS INT64     NO-UNDO.
   DEFINE VARIABLE vKeepDay            AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vKeepPlace          AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vChkType            AS CHARACTER NO-UNDO.

   DEFINE BUFFER code FOR code.

   IF  iMode EQ "CLNT"  THEN DO:
      {empty ttFindSL}
      {empty tt-view-sl}
   END.
   
   /*��।��塞 ���� �⮯-���⮢ �� ����� �஢����� � ����⢨� � ��砨 ᮢ�������*/
   
   iInn = IF iInn EQ ? THEN "" ELSE iInn.
   
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~niClass      = " + iClass + 
      "~niCust-id    = " + iCust-id +
      "~niName       = " + iName +
      "~niShortName  = " + iShortName +
      "~niInn        = " + (IF iInn EQ ? THEN "?" ELSE iInn) +
      "~niDocument   = " + iDocument +
      "~niBirthDay   = " + (IF iBirthDay EQ ? THEN "?" ELSE STRING(iBirthDay,"99/99/9999")) +
      "~niBirthPlace = " + iBirthPlace +
      "~niSLType     = " + iSLType +
      "~niMode       = " + iMode).

   IF iMode EQ "OP" THEN
      vSLtype = iSLType.
   ELSE 
   DO: 
      vNPContInsEditClnt =   fGetSetting("�⮯-�����",iSLType,"").
      IF  NOT (    ENTRY(1,vNPContInsEditClnt, ";")     EQ "��" 
               AND NUM-ENTRIES(vNPContInsEditClnt, ";") GE 2) 
      THEN
         RETURN NO.

      vControlInsEditClnt = ENTRY(2,vNPContInsEditClnt, ";").
      
      IF iSLType EQ "������������_��" THEN
      DO:    
         vChkType = ENTRY(3,vNPContInsEditClnt, ";") NO-ERROR.
         IF  INDEX(vControlInsEditClnt, "=") GT 0 THEN
         DO vI = 1 TO NUM-ENTRIES(vControlInsEditClnt):
            CREATE ttSLAction.
            ASSIGN 
               vTmpStr1                 = ENTRY(vI,vControlInsEditClnt)
               vTmpStr2                 = ENTRY(2,vTmpStr1, "=")
               ttSLAction.StopListName  = TRIM(ENTRY(1,vTmpStr1, "="))
               ttSLAction.CreateAction  = ENTRY(1,vTmpStr2,"|") 
               ttSLAction.EditAction    = ENTRY(2,vTmpStr2,"|") 
            NO-ERROR.
            {additem.i vSLtype ttSLAction.StopListName}
         END.
         ELSE 
            vSLtype = vControlInsEditClnt.
      END.
      ELSE 
         vSLtype = vControlInsEditClnt.
   END.
   
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~nvSLtype  = " + vSLtype + 
      "~nvChkType = " + vChkType).
   
   /*0248377*/
   vDopControl = fGetSetting("�⮯-�����","������஢","").
   vDopFIO = NO.
   DO vI = 1 TO NUM-ENTRIES(vDopControl,";"):
      vDopCriteria[vI] = ENTRY(vI,vDopControl,";").
      IF vDopCriteria[vI] EQ "��" THEN vDopBirthDay = YES.
      IF vDopCriteria[vI] EQ "��" THEN vDopBirthPlace = YES.
      IF vDopCriteria[vI] EQ "������" THEN
      DO:
         vDopFIO = YES.
         RUN Fill-ttSL("������",iName,vSLtype,INPUT-OUTPUT TABLE t-obj-sl).
      END.
   END.
   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~nvDopControl    = " + STRING(vDopControl) + 
      "~nvDopBirthDay   = " + STRING(vDopBirthDay) +
      "~nvDopBirthPlace = " + STRING(vDopBirthPlace) +
      "~nvDopFIO        = " + STRING(vDopFIO)).
   /*0248377*/

   IF NOT {assigned vChkType} THEN
      vChkType = "*".

   IF iClass EQ "person"  THEN
   DO:
      CASE vChkType: 
         WHEN "���_���" THEN
            RUN Fill-ttSL ("���", 
                           iDocument, 
                           vSLtype,
                           INPUT-OUTPUT TABLE t-obj-sl).     
         WHEN "���" THEN
            RUN Fill-ttSL ("���", 
                           iName , 
                           vSLtype,
                           INPUT-OUTPUT TABLE t-obj-sl).
         OTHERWISE  DO:
            IF {assigned iDocument} THEN
               RUN Fill-ttSL ("���", iDocument, vSLtype,INPUT-OUTPUT TABLE t-obj-sl).     /*c��砫� �஢��塞 ����� ��ᯮ�� ��� 䨧. ���*/
            ELSE
               RUN Fill-ttSL ("���", iName , vSLtype,INPUT-OUTPUT TABLE t-obj-sl).
         END. 
      END CASE.  

      ASSIGN 
         vNameForRep = iName
         vCustName   = iName.
   END.

   ELSE
   DO:
      ASSIGN
         vNameForRep = IF iClass EQ "cust-corp" THEN iName
                                                ELSE iShortName
         vCustName   = IF iClass EQ "cust-corp" THEN ""
                                                ELSE   iName + CHR(1) + iShortName
      NO-ERROR.
      IF iClass EQ "cust-corp" THEN  
      DO:
         ASSIGN 
            vTmpStr1 = IF ENTRY(1,iName,      " ") NE "" THEN TRIM(REPLACE(iName,      ENTRY(1,iName,      " "), "")) ELSE iName
            vTmpStr2 = IF ENTRY(1,iShortName, " ") NE "" THEN TRIM(REPLACE(iShortName, ENTRY(1,iShortName, " "), "")) ELSE iShortName.

         vCustName = (IF {assigned  vTmpStr1}  THEN vTmpStr1 + CHR(1) ELSE "") + (IF {assigned  vTmpStr2}  THEN vTmpStr2 + CHR(1) ELSE "").
       
         IF    ENTRY(1,iName, " ") NE ""
           AND  {assigned  vTmpStr1}
         THEN
            vCustName =  vCustName + CHR(1) + ENTRY(1,iName,      " ") + " "  +  vTmpStr1 .
         IF  ENTRY(1,iShortName, " ") NE "" 
           AND {assigned  vTmpStr2}
         THEN
            vCustName =  vCustName + CHR(1) + ENTRY(1,iShortName, " ") + " "  +  vTmpStr2 .

      END.

      CASE vChkType: 
         WHEN "���_���" THEN
            RUN  Fill-ttSL ("���", iInn, vSLtype,INPUT-OUTPUT TABLE t-obj-sl).         /*��� ��� ��. ��� � ������*/
         WHEN "���" THEN
            RUN Fill-ttSL ("���", vCustName , vSLtype,INPUT-OUTPUT TABLE t-obj-sl).
         OTHERWISE DO:
            IF {assigned iInn} THEN
               RUN  Fill-ttSL ("���", iInn, vSLtype,INPUT-OUTPUT TABLE t-obj-sl).         /*��� ��� ��. ��� � ������*/
            ELSE
               RUN Fill-ttSL ("���", vCustName , vSLtype,INPUT-OUTPUT TABLE t-obj-sl).
         END.
      END CASE.                                                                 
   END.

   IF vChkType EQ "*"  THEN
      RUN  Fill-ttSL ("���-���",vCustName + CHR(1) + iInn,vSLtype,INPUT-OUTPUT TABLE t-obj-sl).
   
   FOR EACH t-obj-sl EXCLUSIVE-LOCK:
    
      IF vDopBirthDay EQ YES THEN
      DO:
         ASSIGN
            vDataID = INT64(GetCodeMisc("","StopList",8))
         NO-ERROR.
         IF     vDataID NE 0 
            AND vDataID NE ? THEN
         DO:
            RUN ChkStopListByBirthDay(
                   INPUT STRING(iBirthDay,"99/99/9999"),
                   INPUT vSLType,
                   INPUT vDataID,
                   INPUT STRING(t-obj-sl.rec),
                   OUTPUT vKeepDay
                   ).
         END.
      END.
      ELSE vKeepDay = YES.
      IF vDopBirthPlace EQ YES THEN
      DO:
         ASSIGN
            vDataID = INT64(GetCodeMisc("","StopList",8))
         NO-ERROR.
         IF     vDataID NE 0 
            AND vDataID NE ? THEN
         DO:
            RUN ChkStopListByBirthPlace(
                   INPUT iBirthPlace,
                   INPUT vSLType,
                   INPUT vDataID,
                   INPUT STRING(t-obj-sl.rec),
                   OUTPUT vKeepPlace
                   ).
         END.
      END.
      ELSE vKeepPlace = YES.
      IF     vKeepDay   EQ NO
         OR  vKeepPlace EQ NO THEN
         DELETE t-obj-sl.
   END.

   FIND FIRST t-obj-sl NO-LOCK NO-ERROR.
   
   vRezult = AVAIL t-obj-sl.
   IF    AVAIL t-obj-sl  
     AND iMode EQ "CLNT" 
   THEN
      RUN Cre-tt-sl-clnt (iClass,
                          STRING(iCust-id),
                          iName,
                          iInn,
                          iDocument).
   RETURN  vRezult.
END FUNCTION.

PROCEDURE ChkByStopList.
   DEF INPUT  PARAMETER iCust-cat   AS CHAR  NO-UNDO.    
   DEF INPUT  PARAMETER iCust-id    AS INT64 NO-UNDO.
   DEF INPUT  PARAMETER iSlType     AS CHAR  NO-UNDO.
   DEF INPUT  PARAMETER iMode       AS CHAR  NO-UNDO.
   DEF OUTPUT PARAMETER oRezult     AS LOG   NO-UNDO.

   DEF VAR vName       AS CHAR NO-UNDO.
   DEF VAR vShortName  AS CHAR NO-UNDO.
   DEF VAR vInn        AS CHAR NO-UNDO.
   DEF VAR vDocument   AS CHAR NO-UNDO.
   DEF VAR vBirthDay   AS DATE NO-UNDO.
   DEF VAR vBirthPlace AS CHAR NO-UNDO.
   DEF VAR vClass      AS CHAR NO-UNDO.
   
   IF  iMode EQ "CLNT"  THEN DO:
      {empty ttFindSL}
      {empty tt-view-sl}
   END.
   CASE iCust-cat:
      WHEN "�" THEN
      DO:
         FIND FIRST person WHERE person.person-id EQ icust-id NO-LOCK NO-ERROR.
         IF AVAIL person THEN 
            ASSIGN 
               vClass      = "person"
               vName       = person.name-last + ' ' + person.first-name
               vShortName  = ""
               vInn        = person.inn
               vDocument   = person.document
               vBirthDay   = person.birthday
               vBirthPlace = GetXattrValueEx("person",
                                             STRING(person.person-id),
                                             "birthplace",
                                             "").
      END.
      WHEN "�" THEN
      DO:
         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ icust-id NO-LOCK NO-ERROR.
         IF AVAIL cust-corp THEN 
            ASSIGN 
               vClass      = "cust-corp"
               vName       = TRIM(cust-corp.cust-stat) + " " + TRIM(cust-corp.name-corp)
               vShortName  = TRIM(cust-corp.cust-stat) + " " + TRIM(cust-corp.name-short)
               vInn        = cust-corp.inn
               vDocument   = ""
               vBirthDay   = DATE(GetXattrValueEx("cust-corp",
                                             STRING(cust-corp.cust-id),
                                             "birthday",
                                             ?))
               vBirthPlace = GetXattrValueEx("cust-corp",
                                             STRING(cust-corp.cust-id),
                                             "birthplace",
                                             "").

      END.
      WHEN "�" THEN
      DO:
         FIND FIRST banks WHERE banks.bank-id EQ icust-id NO-LOCK NO-ERROR.
         IF AVAIL banks THEN 
         DO:
            ASSIGN 
               vClass      = "banks"
               vName       = banks.NAME
               vShortName  = banks.short-name
               vDocument   = "".
   
            FIND FIRST cust-ident   WHERE 
                       cust-ident.cust-cat       EQ "�"
                   AND cust-ident.cust-id        EQ banks.bank-id        
                   AND cust-ident.cust-code-type EQ "���"
            NO-LOCK  NO-ERROR.
            IF AVAIL cust-ident AND  {assigned cust-ident.cust-code} THEN
               vInn = cust-ident.cust-code.
         END.
      END.
   END CASE.

   IF mDbgPrint2 EQ YES THEN
   RUN dbgprint.p (PROGRAM-NAME(1) + " stoplist.fun" + " line = {&line-number}",
      "~nvClass      = " + vClass +
      "~niCust-id    = " + STRING(icust-id) +
      "~nvName       = " + vName +
      "~nvShortName  = " + vShortName +
      "~nvInn        = " + (IF vInn EQ ? THEN "?" ELSE vInn) +
      "~nvDocument   = " + (IF vDocument EQ ? THEN "?" ELSE vDocument) +
      "~nvBirthDay   = " + (IF vBirthDay EQ ? THEN "?" ELSE STRING(vBirthDay,"99/99/9999")) +
      "~nvBirthPlace = " + vBirthPlace +
      "~niSLType     = " + iSLType +
      "~niMode       = " + iMode).

   oRezult =  ChkClientByStopList (vclass ,
                                   STRING(icust-id),
                                   vName,
                                   vShortName,
                                   vInn,
                                   vDocument,
                                   vBirthDay,
                                   vBirthPlace,
                                   iSlType,
                                   iMode).
   
 
END PROCEDURE.
   
FUNCTION StopListAction    RETURN LOGICAL
   (iClass      AS CHAR,     
    iCust-id    AS CHAR,
    iMode       AS CHAR):

   DEFINE VARIABLE vFindSl AS LOGICAL           NO-UNDO.
   DEFINE VARIABLE vStr    AS CHARACTER         NO-UNDO.
   DEFINE VARIABLE vReturn AS LOGICAL INIT YES  NO-UNDO.

   IF NOT CAN-FIND( FIRST ttSLAction )THEN
      RETURN TRUE.

   IF iMode EQ "CREATE" THEN
   DO:   
      FOR EACH tt-view-sl NO-LOCK,
         FIRST ttSLAction WHERE 
               CAN-DO( ttSLAction.StopListName, tt-view-sl.fld7) 
           AND ttSLAction.CreateAction EQ  "������"
      NO-LOCK:
         RETURN NO.
      END. 
      
      FOR EACH tt-view-sl NO-LOCK,
       EACH ttSLAction WHERE 
               CAN-DO( ttSLAction.StopListName, tt-view-sl.fld7) 
           AND ttSLAction.CreateAction EQ  "�����"
      NO-LOCK : 
         {additem.i vStr tt-view-sl.fld7}
      END.
      IF {assigned vStr} THEN
      DO:    
         pick-value = "NO".
         RUN Fill-SysMes IN h_tmess ("", "", "4",  "��������! ��� ᮢ������ � 䨣�࠭⠬� �ࠢ�筨�� �⮯-����� -  " + QUOTER(vStr)  + "!  ~n ���࠭��� ������ ������?").
         IF pick-value EQ  "NO" THEN 
            RETURN NO.
      END.
   END.

   FOR EACH tt-view-sl NO-LOCK,
       FIRST ttSLAction WHERE 
             CAN-DO( ttSLAction.StopListName, tt-view-sl.fld7) 
         AND ttSLAction.EditAction EQ  "����"
   NO-LOCK:
      UpdateSignsEx(iClass,iCust-id,"����", "��").
      RETURN vReturn.
   END.

   vStr = "".
   FOR EACH tt-view-sl NO-LOCK,
    EACH ttSLAction WHERE 
            CAN-DO( ttSLAction.StopListName, tt-view-sl.fld7) 
        AND ttSLAction.EditAction EQ  "�����"
   NO-LOCK:
         vFindSl = TRUE.
         {additem.i vStr tt-view-sl.fld7}
   END.
   IF   vStr NE "" 
     OR vFindSl THEN
   DO:    
      pick-value = "NO".
      RUN Fill-SysMes IN h_tmess ("", "", "4", "��������! ������ ᮢ������ � 䨣�࠭⮬ �ࠢ�筨�� �⮯-����� -  " + QUOTER(vStr)  + "!  ~n  �����஢��� ��� ������ (��⠭����� �� ����=��)?").
      IF pick-value EQ "YES" THEN
         UpdateSignsEx(iClass,iCust-id,"����", "��").
   END.
   RETURN vReturn.

END FUNCTION.


FUNCTION ChkStopListImpExp    RETURN LOGICAL (iOp AS INT64):

   DEFINE VARIABLE vSLType   AS CHAR  NO-UNDO.  
   DEFINE VARIABLE vRez      AS LOG  NO-UNDO.  

   vSLType =  FGetSetting("�⮯-�����","��ᯈ��_���","").
   
   IF ENTRY(1,vSLType, ";") EQ "���" THEN
      RETURN NO.
   
   vSLType = ENTRY(2,vSLType, ";") NO-ERROR.   
   vSLType =  IF  {assigned  TRIM(vSLType) } THEN  vSLType ELSE "*".

   {empty ttFindSL}
   {empty tt-view-sl}

   RUN ChkStopListOp(iOp, vSLType, OUTPUT vRez).
   
   RETURN  vREz.

END FUNCTION.

&IF DEFINED(hWOPCHECK) &THEN
PROCEDURE ChkStopListWOp:
   DEFINE INPUT  PARAMETER hWOP AS handle   NO-UNDO.
   DEFINE OUTPUT PARAMETER oRezult AS LOG   NO-UNDO.

   DEF VAR vSLType   AS CHAR     NO-UNDO.
   DEF VAR mClType   AS CHAR     NO-UNDO.  /*⨯ �����⮢ ��� �஢�ન*/
   DEF VAR mSLtype   AS CHAR     NO-UNDO.  /*���� �⮯-���⮢, �� ����� �஢����� �����⮢*/
   DEF VAR mChkType  AS CHAR     NO-UNDO.  /*��� �஢�ન*/
   DEF VAR mCustDoc  AS CHAR     NO-UNDO.
   DEF VAR mKName    AS CHAR     NO-UNDO.
   DEF VAR mKVal     AS CHAR     NO-UNDO.
   DEF VAR mInnSend  AS CHAR     NO-UNDO.
   DEF VAR mInnRec   AS CHAR     NO-UNDO.
   DEF VAR mNameSend AS CHAR     NO-UNDO.
   DEF VAR mNameRec  AS CHAR     NO-UNDO.
   DEF VAR mBankInn  AS CHAR     NO-UNDO.
   DEF VAR mAcctSend AS CHAR     NO-UNDO.
   DEF VAR mAcctRec  AS CHAR     NO-UNDO.
   DEF VAR mRez      AS LOG      NO-UNDO.

   vSLType =  FGetSetting("�⮯-�����","��ᯈ��_���","").
   
   IF ENTRY(1,vSLType, ";") EQ "���" 
   THEN DO:
      oRezult = NO.
      RETURN.
   END.

   ASSIGN
      vSLType = ENTRY(2,vSLType, ";")    
      vSLType = IF {assigned TRIM(vSLType)} THEN vSLType ELSE "*"
   NO-ERROR.

   {empty ttFindSL}
   {empty tt-view-sl}
   {empty t-obj-sl}

   /*�஢��塞 ��� �� �।���*/
   IF AVAILABLE(mAcctCR) THEN  
   DO:
      mKName = "�� " + DelFilFromAcct(STRING(mAcctCR.acct)).
      RUN ChkByStopList(mAcctCR.cust-cat, mAcctCR.cust-id, vSLType, "OP", OUTPUT mRez).
      IF mRez THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),mKname, GetKrVal(mAcctCR.cust-cat, mAcctCR.cust-id), " " ).
   END.
   /*�஢��塞 ��� �� ������*/
   IF AVAILABLE(mAcctDB) THEN  
   DO:
      mKName = "�� " + DelFilFromAcct(STRING(mAcctDB.acct)).
      RUN ChkByStopList(mAcctDB.cust-cat, mAcctDB.cust-id, vSLType, "OP", OUTPUT mRez).
      IF mRez THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),mKname,GetKrVal(mAcctDB.cust-cat, mAcctDB.cust-id), " " ).
   END.
      
   /*�஢��塞 ��� �� �� acct-send acct-rec*/
   IF AVAILABLE(mAcctSend) THEN  
   DO:
      mKName = "acct-send " + mAcctSend.acct.
      RUN ChkByStopList(mAcctSend.cust-cat, mAcctSend.cust-id, vSLType, "OP", OUTPUT mRez).
      IF mRez THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),mKname,GetKrVal(mAcctSend.cust-cat, mAcctSend.cust-id), " " ).
   END.      

   IF AVAILABLE(mAcctRecv) THEN
   DO:
       mKName = "acct-rec " + STRING(mAcctRecv.acct).
       RUN ChkByStopList(mAcctRecv.cust-cat, mAcctRecv.cust-id, vSLType, "OP", OUTPUT mRez).
       IF mRez THEN
          RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),mKname,GetKrVal(mAcctRecv.cust-cat, mAcctRecv.cust-id), " " ).
   END.
   
   /*�஢��塞 ४������ ���㬥��*/
   ASSIGN 
      mInnSend    =  hWOP::inn-send
      mInnRec     =  hWOP::inn-rec 
      mNameSend   =  hWOP::name-send
      mNameRec    =  hWOP::name-rec
   .
   {empty t-obj-sl}
   IF {assigned hWOP::inn} THEN
   DO:
      RUN Fill-ttSL("���", hWOP::inn, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"inn",hWOP::inn, " " ).
   END.
   
   IF {assigned mInnSend} THEN
   DO:
      RUN Fill-ttSL("���", mInnSend, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"inn-send",mInnSend, " " ).
   END.
   
  IF {assigned mInnRec} THEN
   DO:
      RUN Fill-ttSL("���", mInnRec, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"inn-rec",mInnRec, " " ).
   END.
  
   IF {assigned hWOP::name-ben} THEN
   DO:
      RUN Fill-ttSL("���_��", hWOP::name-ben, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"name-ben",op.name-ben, " " ).
   END.
   
   IF {assigned hWOP::details} THEN
   DO:
      RUN Fill-ttSL("���_��", hWOP::details, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(op.doc-num), STRING(hWOP::doc-date),"details",hWOP::details, " " ).
   END.
   
   IF {assigned mNameSend} 
     AND NOT {assigned mInnSend} THEN
   DO:
      RUN Fill-ttSL("���_��", mNameSend, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"name-send",mNameSend, " " ).
   END.
   
   IF    {assigned mNameRec} 
     AND NOT {assigned mInnRec} THEN
   DO:
      RUN Fill-ttSL("���_��", mNameRec, vSLType, INPUT-OUTPUT TABLE t-obj-sl).
      IF CAN-FIND(FIRST t-obj-sl) THEN
         RUN Cre-tt-sl (STRING(hWOP::doc-num), STRING(hWOP::doc-date),"name-rec",mNameRec, " " ).
   END.

   FIND FIRST tt-view-sl NO-LOCK NO-ERROR.
   oRezult = AVAIL tt-view-sl.

END PROCEDURE.
&ENDIF

PROCEDURE  ChkStopLines:
   DEFINE INPUT PARAMETER iString    AS CHARACTER NO-UNDO.
    
   /*���砫� �஢��塞 ����� ��ᯮ��*/
   RUN Fill-ttSL ("���", iString, "*",INPUT-OUTPUT TABLE t-obj-sl).        

   /*�᫨ ��祣� �� ��諨 � �஢��塞 ���*/
   IF NOT  CAN-FIND(FIRST t-obj-sl) THEN     
      RUN  Fill-ttSL ("���", iString, "*",INPUT-OUTPUT TABLE t-obj-sl).  

   /*�᫨ ����� ��祣� �� ��諨 � �஢��塞 ���*/
   IF NOT  CAN-FIND(FIRST t-obj-sl) THEN                                    
      RUN Fill-ttSL ("���", iString, "*",INPUT-OUTPUT TABLE t-obj-sl).


END PROCEDURE.
/* $LINTFILE='stoplist.fun' */
/* $LINTMODE='1,0,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='08/07/2016 10:13:37.151+03:00' */
/*prosignJybLYwI4ydJUAIFj7q9Zjg*/