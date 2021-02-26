/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 2000-2018 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LOADSTOPLST.P
      Comment: ����㧪� ������ � �����䨪��� "�⮯ �����"
   Parameters: iPar
         Uses:
      Used by:
      Created: 08.11.2013  ANBA    
     Modified: 08.11.2013  ANBA     
*/

{globals.i}     
{intrface.get xclass}
{intrface.get strng} 
{intrface.get count}
{intrface.get strng}
{wordwrap.def}
{stoplist.fun }
{sv-temp.i NEW}
{norm.i NEW} 

DEFINE INPUT PARAMETER iPar   AS CHAR  NO-UNDO.

DEFINE VARIABLE mAddSym       AS CHAR  NO-UNDO.
DEFINE VARIABLE mCLType       AS CHAR  NO-UNDO.
DEFINE VARIABLE mCnt          AS INT64 NO-UNDO.
DEFINE VARIABLE mDataID       AS INT64 NO-UNDO.
DEFINE VARIABLE mDataRez      AS DATE  NO-UNDO.
DEFINE VARIABLE mExchRule     AS CHAR  NO-UNDO.
DEFINE VARIABLE mExchParam    AS CHAR  NO-UNDO.
DEFINE VARIABLE mFileName     AS CHAR  NO-UNDO.
DEFINE VARIABLE mNum          AS INT64 NO-UNDO.
DEFINE VARIABLE mNeedDel      AS LOG   NO-UNDO.
DEFINE VARIABLE mNeedIskl     AS LOG   NO-UNDO.
DEFINE VARIABLE mNumRez       AS CHAR  NO-UNDO.
DEFINE VARIABLE mRet          AS INT64 NO-UNDO.
DEFINE VARIABLE mScript       AS CHAR  NO-UNDO.
DEFINE VARIABLE mSLName       AS CHAR  NO-UNDO.
DEFINE VARIABLE mTxtFileName  AS CHAR  NO-UNDO.
DEFINE VARIABLE mTmpStr       AS CHAR  NO-UNDO.
DEFINE VARIABLE mTmpStr1      AS CHAR  NO-UNDO.
DEFINE VARIABLE mFldNames     AS CHAR  NO-UNDO 
   INIT "���,���,���,��࠭�,���,��ᯮ��,�ਬ�砭��,,��,��".
DEFINE VARIABLE mFldIndex     AS CHAR  NO-UNDO EXTENT 10.
DEFINE VARIABLE mI            AS INT64 NO-UNDO.
DEFINE VARIABLE mInd          AS INT64 NO-UNDO.
DEFINE VARIABLE mStartStr     AS INT64 NO-UNDO.
DEFINE VARIABLE mDelimeter    AS CHAR  NO-UNDO.
DEFINE VARIABLE mFileExtent   AS CHAR  NO-UNDO.
DEFINE VARIABLE mNPName       AS CHAR  NO-UNDO.
DEFINE VARIABLE mRid          AS INT64 NO-UNDO.
DEFINE VARIABLE mBirthDay     AS DATE NO-UNDO.
DEFINE VARIABLE mBirthPlace   AS CHARACTER NO-UNDO.

DEFINE STREAM   sStream.

DEFINE TEMP-TABLE tt-chkfldval NO-UNDO
   FIELD fldnum  AS INT64 
   FIELD fld-val AS CHAR
   FIELD fld-add AS CHAR
INDEX fldnum fldnum.

DEFINE TEMP-TABLE tt-FldVal NO-UNDO
   FIELD val AS CHAR.

DEFINE TEMP-TABLE tt-FindDelStop NO-UNDO
   FIELD fldtype   AS CHARACTER  /*⨯*/
   FIELD fldcode   AS CHARACTER  /*���*/
   FIELD fldname   AS CHARACTER  /*���*/
   FIELD fldcoun   AS CHARACTER  /*��࠭�*/
   FIELD fldinn    AS CHARACTER  /*���*/
   FIELD fldpas    AS CHARACTER  /*��ᯮ��*/
   FIELD fldbday   AS DATE       /*��� ஦�����*/
   FIELD fldbplace AS CHARACTER  /*���� ஦�����*/
   FIELD fldprim   AS CHARACTER  /*�ਬ�砭��*/
   FIELD fldstlst  AS CHARACTER  /*�⮯-����*/
   FIELD fldnumrez AS CHARACTER  /*����� १���樨*/
   FIELD flddatrez AS DATE       /*��� १���樨*/
   FIELD flddatadd AS DATE       /*��� ����������*/
   FIELD flduser   AS CHARACTER  /*���짮��⥫�*/
.

ASSIGN 
   mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.
{stoplist.pro &DATA-LINE = DataLine}

/*�����ࠥ� ��ࠬ���� ������*/
&IF DEFINED (LoadTxt) NE 0 &THEN
ASSIGN 
   mNPName    = GetEntries (1,iPar,";", "")
   mExchParam = FGetSetting("�⮯-�����", ENTRY(1,iPar,";"), "")
   mDelimeter = REPLACE(iPar,GetEntries (1,iPar,";", ""),"").
   mDelimeter = SUBSTRING(mDelimeter,INDEX(mDelimeter, "_") + 1, 1) 
   NO-ERROR.
&ELSE
ASSIGN
   mNPName    = iPar
   mExchParam = FGetSetting("�⮯-�����", iPar, "")
   mDelimeter = ";"
   .
&ENDIF
IF NUM-ENTRIES(mExchParam,";") LT 5  THEN
DO:
   MESSAGE  "�� �ࠢ��쭮 �������� ����஥�� ��ࠬ��� " + QUOTER(mNPName)     VIEW-AS ALERT-BOX.
   RETURN.
END.
ASSIGN
   mSLName   = ENTRY(1,mExchParam,";")           /*��� �⮯-����*/
   mStartStr = INT64(ENTRY(2,mExchParam,";"))    /*� ����� ��ப� ��稭��� ����㧪�*/
   mExchRule = ENTRY(3,mExchParam,";")           /*�ࠢ��� ������*/
   mAddSym   = ENTRY(4,mExchParam,";")           /*�������⥫�� ᨬ���� � ����*/
   mNeedDel  = ENTRY(5,mExchParam,";") NE "���"  /*��������/㤠����*/
   mNeedIskl = ENTRY(5,mExchParam,";") BEGINS "�᪫"
   mCLType   = ENTRY(6,mExchParam,";")           /*⨯ ������*/
   mCLType   = IF CAN-DO("�,�,�", mCLType) THEN mCLType ELSE  ""
NO-ERROR.

FORM 
   mFileName   
      FORMAT "x(300)"         
      VIEW-AS FILL-IN SIZE 30 BY 1 LABEL "��� 䠩��" 
      HELP "F1 - ᯨ᮪" SKIP
   mSLName
      LABEL  "�⮯-����"   
      FORMAT "x(20)"    
      HELP "��� �⮯-����"
     SKIP
   mNumRez
      LABEL  "����� १���樨"  
      FORMAT "x(100)"
      VIEW-AS FILL-IN SIZE 50 BY 1
      HELP "����� १���樨"  
       SKIP
   mDataRez
      LABEL  "��� १���樨"
      FORMAT "99/99/9999"    
      HELP "��� १���樨" SKIP
WITH FRAME fImp TITLE "������ �� Excel" CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

mRid = INT64(GETSYSCONF("user-proc-id")).
FIND FIRST user-proc WHERE RECID(user-proc) = mRid NO-LOCK NO-ERROR.
IF AVAIL user-proc THEN DO:
   IF GetXAttrValueEx("user-proc",STRING(user-proc.public-number),"���",?) <> ? THEN
   mFileName = FgetSetting("��_���","������",""). 
END.
ON "F1" OF mFileName IN FRAME fImp DO:
   mFileName = mFileName:SCREEN-VALUE.
   &IF DEFINED (LoadTxt) NE 0 &THEN
   RUN ch-file2.p("*", "", INPUT-OUTPUT mFileName, YES).
   &ELSE
   RUN ch-file2.p("*.xls", "", INPUT-OUTPUT mFileName, YES).
   &ENDIF
   mFileName:SCREEN-VALUE = mFileName.
   mFileExtent = "." + ENTRY(2,mFileName, ".") NO-ERROR.
   IF {assigned mFileName} THEN
   ASSIGN
      mNum                   = NUM-ENTRIES(mFileName,"/")
      mNumRez:SCREEN-VALUE   = REPLACE(GetEntries (mNum,mFileName,"/",""), mFileExtent, "")
      mDataRez:SCREEN-VALUE  = SUBSTRING(mNumRez:SCREEN-VALUE,LENGTH(mNumRez:SCREEN-VALUE) - 5) NO-ERROR.
END.
ON 'GO' OF FRAME fImp
DO:
   IF {assigned mFileName:SCREEN-VALUE}  THEN
   DO:  
      ASSIGN
         mNum         = NUM-ENTRIES(mFileName,"/")
         mNumRez      = REPLACE(GetEntries (mNum,mFileName,"/",""),mFileExtent, "")
         mTxtFileName = mFileName NO-ERROR.
      
     &IF DEFINED (LoadTxt) EQ 0 &THEN
     mDataRez     = DATE(SUBSTRING(mNumRez,LENGTH(mNumRez) - 5)) NO-ERROR.
     ASSIGN
         mTxtFileName = REPLACE(mFileName, ".xls" , ".txt" )
         mScript = "bq_launch_java.sh xls " +  mFileName + " " + mTxtFileName  + " ';'"  NO-ERROR.
     &ENDIF
   END.
   ELSE 
      RETURN.
END. 

ON 'END-ERROR' OF FRAME fImp
DO:
   HIDE FRAME fimp.
END.

PAUSE 0.
UPDATE
   mFileName
   mSLName
   mNumRez
   mDataRez
WITH FRAME fImp.
HIDE FRAME fimp.

&IF DEFINED (LoadTxt) NE 0 &THEN
mTxtFileName = mFileName.
&ENDIF

IF {assigned mFileName } THEN 
DO:
   {justamin} 
   &IF DEFINED (LoadTxt) EQ 0 &THEN
   IF {assigned  mScript } THEN 
   DO:   
      RUN unixcmd.p(mScript , "" , "" , NO , OUTPUT mRet).
      IF mRet NE 0 THEN
      DO:
         MESSAGE  "�訡�� �� �������樨 �� xls � txt! (" + STRING(mRet) + ")" VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   &ENDIF
   
   /*�����ࠥ� �ࠢ��� ������*/
   DO mI = 1 TO NUM-ENTRIES(mExchRule):
     mTmpStr = GetEntries (mI,mExchRule,",","").
     IF NUM-ENTRIES(mTmpStr, "|") EQ 2 THEN
     DO:
        mInd = LOOKUP(GetEntries (2,mTmpStr, "|",""),mFldNames).
        IF mInd GT 0 AND 
           mInd LE 10 
        THEN
        DO:
           mFldIndex[mInd] = mFldIndex[mInd] +  IF mFldIndex[mInd] EQ  ""  THEN GetEntries (1,mTmpStr, "|","")  ELSE ("," + GetEntries (1,mTmpStr, "|","")) NO-ERROR.  /*??*/
        END.
     END.                                           
   END.
   
   /*�������⥫�� ᨬ���� � ��������*/
   IF {assigned mAddSym } AND
      NUM-ENTRIES(mAddSym, "�") GE 2 THEN
   {empty tt-chkfldval}
   DO mI = 1 TO NUM-ENTRIES(mAddSym, "�") - 1:
   
      mTmpStr = GetEntries (mI + 1,mAddSym, "�", "").
      
      CR_TTCHKFLDVAL:
      DO TRANSACTION:
         CREATE tt-chkfldval.
         ASSIGN 
            tt-chkfldval.fldnum = INT64(GetEntries (1,mTmpStr, "=", "") )
            mTmpStr = GetEntries (2,mTmpStr, "=", "") .
   
         DO mInd = 1 TO NUM-ENTRIES(mTmpStr) :
            ASSIGN
               mTmpStr1 = GetEntries (mInd,mTmpStr, ",", "")
               tt-chkfldval.fld-val =  tt-chkfldval.fld-val   + (IF tt-chkfldval.fld-val NE "" THEN  "," ELSE "") +  STRING(GetEntries (1,mTmpStr1,  "|", "") ) 
               tt-chkfldval.fld-add =  tt-chkfldval.fld-add   + (IF tt-chkfldval.fld-add NE "" THEN  "," ELSE "") +  STRING(GetEntries (2,mTmpStr1,  "|","")) NO-ERROR.  
         END.
      END.   
   END.
   /*�饬 ���� ������*/
   RUN PrepareDataBlock (mDataId).
   mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.
   /*�㦭� �� �᪠�� 㤠�塞�� ��ப�*/
   IF mNeedIskl THEN
   DO:
      RUN DelStr(mTxtFileName,
                 mSlName,
                 TRIM(ENTRY(2,ENTRY(5,mExchParam,";"),"("),")")).
      IF CAN-FIND(FIRST tt-FindDelStop) THEN
      DO:
         {setdest.i &stream = "stream rep" &filename = "'DelStopList.log'"}
         RUN PrintDelStr.
         {preview.i &stream = "stream rep" &filename = "'DelStopList.log'"}
      END.
      ELSE 
         RUN Fill-AlertSysMes IN h_tmess("","","2","��� ᮢ������� ��� ��⮪���!").
   END.
   ELSE
   DO:
      /*�㦭� �� 㤠���� �����*/
      IF mNeedDel THEN
         RUN CrDelLst (mTxtFileName,mSlName).
      /*����㦠�� ���� �����*/
      RUN ImportStopList(mTxtFileName).
      MESSAGE  "����㧪� ������ �����襭�!" VIEW-AS ALERT-BOX.
   END.
END.    

RETURN.

/* -------------------------------------------------------------------------- */
/* �᫨ ⥪�� � ����窠� � �����塞 "," �� chr(1)                            */
/* -------------------------------------------------------------------------- */
FUNCTION ChkComma RETURN CHAR (INPUT iStr AS CHAR):
   DEFINE VARIABLE vTmpStr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vOneSym    AS CHAR  NO-UNDO.
   DEFINE VARIABLE vFlCom     AS LOG   NO-UNDO.
   DEFINE VARIABLE vI         AS INT64 NO-UNDO.

   vTmpStr = iStr.
   /*�஢��塞 ����窨*/
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


/* -------------------------------------------------------------------------- */
/* �����頥� ᯨ᮪ ������������ �⮯-���⮢                                 */
/* ��ࠬ����:                                                                 */
/* iFName - ��� 䠩�� ������                                                 */
/* iSLName - ������������ �⮯-���� �� ����. ����                            */                      
/* -------------------------------------------------------------------------- */
FUNCTION GetSLName RETURN CHAR (INPUT iFName AS CHAR,INPUT iSLName AS CHAR):
   DEFINE VARIABLE vTmpStr    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmp       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmp1      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSlName    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStart     AS LOG   NO-UNDO.

   /*��।��塞 ������������ �⮯-����*/
   INPUT STREAM sStream FROM VALUE (iFName).
   REPEAT:
      IMPORT STREAM sStream UNFORMATTED vTmpStr.
      IF TRIM(vTmpStr) BEGINS "1" THEN
         vStart = YES.
      IF vStart THEN
      DO:
         vTmpStr = ChkComma(vTmpStr).
         IF  {assigned vTmpStr} THEN
         DO:
            /*������塞 ᨬ���� � �������� �⮯-���� � ����ᨬ��� �� ���祭�� ����*/
            FOR EACH  tt-chkfldval:
               vTmp = REPLACE(TRIM(ENTRY(tt-chkfldval.fldnum,vTmpStr,mDelimeter)), CHR(1), "") NO-ERROR.
               IF     {assigned vTmpStr} 
                  AND CAN-DO(tt-chkfldval.fld-val,vTmp) THEN 
               DO: 
                  vTmp1  =  iSLName  + ENTRY (LOOKUP(vTmp,tt-chkfldval.fld-val),tt-chkfldval.fld-add, ",") . 
                  IF LOOKUP(vTmp1,vSlName) EQ 0 THEN
                     vSlName = IF vSlName EQ "" THEN vTmp1 ELSE ( vSlName + "," + vTmp1).
               END.
            END.            
         END.
      END.
   END.
   vSlName = IF vSlName  EQ "" THEN iSLName ELSE vSlName.
   
   INPUT STREAM sStream CLOSE.
   RETURN vSlName.
END FUNCTION.

/* -------------------------------------------------------------------------- */
/* ������ ��ଠ樨                                                           */
/* -------------------------------------------------------------------------- */
PROCEDURE ImportStopList:
   DEFINE INPUT   PARAMETER iFname    AS CHARACTER  NO-UNDO.
   
   DEFINE VARIABLE vTxtLine AS CHAR         NO-UNDO.
   DEFINE VARIABLE vStart   AS LOG INIT NO  NO-UNDO.
   DEFINE VARIABLE vNumStr  AS INT64        NO-UNDO.
   
   INPUT STREAM sStream FROM VALUE (iFName).
   REPEAT:
      vTxtLine = "".
      IMPORT STREAM sStream UNFORMATTED vTxtLine .
      vNumStr = vNumStr + 1.
      IF vNumStr EQ mStartStr THEN
         vStart = YES.
      IF vStart THEN
      DO:
         RUN CreateStopList (vTxtLine).
      END.
   END.
   INPUT STREAM sStream CLOSE.
END PROCEDURE.

/* -------------------------------------------------------------------------- */
/* ��ࠡ�⪠ ������஢����� ��ப�                                           */
/* -------------------------------------------------------------------------- */
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
         code.misc[8]        = USERID("bisquit")              /*�������⥫�, ᮧ���訩 ������*/  
         code.val            = mNumRez                        /*����� १���樨*/
         .
         UpdateSignsEx(code.class, code.class + "," + code.code,"RezolutionData",STRING(mDataRez)).  /*��� १���樨*/
         UpdateSignsEx(code.class, code.class + "," + code.code,"AddData",STRING(TODAY)).            /*��� ����������*/

      /*��� ������ ����� ���� ����� � ��*/
      vInd1 =  INT64(mFldIndex[1]) NO-ERROR.
      IF vInd1 EQ 0  THEN
         code.misc[1]  =  IF {assigned mCLType} THEN mCLType ELSE "".
      ELSE DO: 
         vTmp = GetEntries(vInd1,iRawLine,mDelimeter, "") NO-ERROR.
         code.misc[1]  = IF CAN-DO("�,�,�",vTmp) THEN vTmp ELSE "".
      END.   

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
                        code.misc[vJ] =   (code.misc[vJ]  + IF  code.misc[vJ] NE "" THEN "||" ELSE "") +   vTmp NO-ERROR.
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
                        code.misc[vJ ] = (code.misc[vJ]  + IF code.misc[vJ] NE "" THEN "||" ELSE "") + vTmp NO-ERROR.
                     IF vJ EQ 9 THEN
                     DO:
                        vBirthDay = DATE(vTmp).

                        UpdateSignsEx(code.class,code.class + "," + code.code,
                                     "birthday",STRING(vBirthDay,"99/99/9999")).  /*��� ஦�����*/
                     END.
                     IF vJ EQ 10 THEN
                     DO:
                        vBirthPlace = TRIM(vTmp).
                        UpdateSignsEx(code.class,code.class + "," + code.code,
                                     "birthplace",vBirthPlace).  /*���� ஦�����*/
                     END.
                  END.
               END.
            END.
         END.
      END.

      /*�������塞 ᨬ���� � �������� �⮯-���� � ����ᨬ��� �� ���祭�� ����*/
      FOR EACH  tt-chkfldval:
         vTmp = TRIM((GetEntries( tt-chkfldval.fldnum , vTmpStr, mDelimeter, ""))). 
         IF {assigned vTmp} AND
            CAN-DO( tt-chkfldval.fld-val,  vTmp) 
          THEN
             code.name  =  mSLName  + GetEntries (LOOKUP(vTmp,tt-chkfldval.fld-val), tt-chkfldval.fld-add, ",", "") .                 /*��� �⮯-����*/
      END.
      IF NOT {assigned code.name} THEN
         code.name = mSLName.
      code.description[1]    =  STRING(CalcCODENum(code.name)). 
      RUN  CrDataLine(mDataId, code.code, "ADD").
   END.
END PROCEDURE.

/* -------------------------------------------------------------------------- */
/* ����塞 ����� ��। ����������� �����                                     */
/* -------------------------------------------------------------------------- */
PROCEDURE CrDelLst  :
   DEFINE INPUT PARAM iFname  AS CHAR  NO-UNDO.
   DEFINE INPUT PARAM iSLName AS CHAR  NO-UNDO.

   DEFINE VARIABLE vDelSl     AS CHAR  NO-UNDO.

   DEFINE BUFFER delcode FOR code.
   
   /*��।��塞 ������������ �⮯-����*/
   vDelSl = GetSlName(iFName,iSlName).

   DElSTOPLST:
   FOR EACH delcode WHERE 
            delcode.parent EQ "StopList"
       AND  delcode.class  EQ "StopList"
       AND  CAN-DO(vDelSl,delcode.name)
   EXCLUSIVE-LOCK:
      RUN  CrDataLine(mDataId, delcode.code, "DEL").
      DELETE delcode.
   END.
END PROCEDURE.

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
                 "����� ����ண� ���᪠ �����஢�� ��㣨� ���짮��⥫��."     SKIP
                 "��������� �����䨪��� �� ��ࠦ���."                      SKIP
                 "�믮���� ���� ����� StopList ��� ��ࠢ����� �訡��."  SKIP
              VIEW-AS ALERT-BOX ERROR.
      ELSE DO:                            /* ������� ⥪���� ����஢��         */
         FOR EACH DataLine OF DataBlock WHERE
                  DataLine.Sym1 EQ icode
         EXCLUSIVE-LOCK:
            DELETE DataLine.
         END.
                                          /* ������� ����� ����஢��           */
                                                   
         IF iMode NE "DEL" THEN
         DO:
            ASSIGN
               mBirthDay   = DATE(GetXattrValueEx("code","StopList," + iCode,"birthday",""))
               mBirthPlace =      GetXattrValueEx("code","StopList," + iCode,"birthplace","").
            /* �ନ஢���� DataLine */
            {stoplistln.i &DataID=iDataID &BirthDay = mBirthDay &BirthPlace = mBirthPlace} 
         END.
      END.
      
      RELEASE DataBlock.
   END.
   ELSE
      MESSAGE COLOR MESSAGE
              "�� ������� ��뫪� �� ����� ����ண� ���᪠."             SKIP
              "��������� � �����䨪��� �� ��ࠦ���."                    SKIP
              "�믮���� ���� ����� TerrBlack ��� ��ࠢ����� �訡��."  SKIP
              VIEW-AS ALERT-BOX ERROR.      
END PROCEDURE.

/* ------------------------------------------------- */
/*       㤠����� ��������� ��ப                   */
/* ------------------------------------------------- */
PROCEDURE DelStr:
   DEFINE INPUT PARAMETER iFName   AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER iSlName  AS CHARACTER  NO-UNDO. 
   DEFINE INPUT PARAMETER iFindFld AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vI          AS INT64     NO-UNDO.
   DEFINE VARIABLE vInd1       AS INT64     NO-UNDO.
   DEFINE VARIABLE vNum        AS INT64     NO-UNDO.
   DEFINE VARIABLE vFindFldNum AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmp        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmps       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vTmpStr     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vDelSl      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStart      AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vPrintInd   AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE vNumStr     AS INT64     NO-UNDO.
   DEFINE BUFFER delcode FOR code.
   /*��।��塞 ������������ �⮯-����*/
   vDelSl = GetSlName(iFName,iSlName).
   INPUT STREAM sStream FROM VALUE (iFName).
   REPEAT:
      vTmpStr = "".
      IMPORT STREAM sStream UNFORMATTED vTmpStr.
      vNumStr = vNumStr + 1.
      IF vNumStr EQ mStartStr THEN
         vStart = YES.
      IF vStart THEN
      DO:
         vTmpStr = ChkComma(vTmpStr).
         IF  {assigned vTmpStr} THEN
         DO:
            DO vI = 1 TO NUM-ENTRIES(iFindFld):
               vInd1 = INT64(ENTRY(vI,iFindFld)) NO-ERROR.
               IF vInd1 NE 0 THEN
               DO: 
                  vTmp = REPLACE(TRIM(GetEntries(vInd1,vTmpStr,mDelimeter, "")), CHR(1),",").
                  IF {assigned vTmp} THEN
                     ASSIGN
                        vTmp  = IF trim(vTmp) EQ "-0-" THEN "0" ELSE vTmp
                        vTmps = vTmps + (IF {assigned vTmps} THEN "," ELSE "") + TRIM(vTmp, '"') 
                     NO-ERROR.  
               END.
            END.
            IF {assigned vTmps} THEN
            DO:
               /* �����뢠�� � tt-FldVal, �⮡� ᮪���� �᫮ ���饭�� � delcode */
               CREATE tt-FldVal.
               tt-FldVal.val = vTmps.
               vTmps = "".
            END.
         END.
      END.
   END.
   INPUT STREAM sStream CLOSE.
   IF CAN-FIND(FIRST tt-FldVal) THEN
   DO:
      /* ����塞 �������� ��ப� */
      FOR EACH delcode WHERE delcode.class  EQ "StopList"
                         AND delcode.parent EQ "StopList"
                         AND CAN-DO(vDelSl,delcode.name) EXCLUSIVE-LOCK,
         FIRST tt-FldVal WHERE CAN-DO(tt-FldVal.val,delcode.misc[5])
                            OR CAN-DO(tt-FldVal.val,delcode.misc[6]) NO-LOCK:
         RUN CrDataLine(mDataId, delcode.code, "DEL").
         CREATE tt-FindDelStop.
         ASSIGN
            tt-FindDelStop.fldtype   = delcode.misc[1]
            tt-FindDelStop.fldcode   = delcode.misc[2]
            tt-FindDelStop.fldname   = delcode.misc[3]
            tt-FindDelStop.fldcoun   = delcode.misc[4]
            tt-FindDelStop.fldinn    = delcode.misc[5]
            tt-FindDelStop.fldpas    = delcode.misc[6]
            tt-FindDelStop.fldbday   = DATE(GetXattrValueEx(
                                            "code",
                                            delcode.class + 
                                            "," +
                                            delcode.code,
                                            "birthday",
                                            ""))
            tt-FindDelStop.fldbplace = TRIM(GetXattrValueEx(
                                            "code",
                                            delcode.class + 
                                            "," +
                                            delcode.code,
                                            "birthplace",
                                            ""))                                            
            tt-FindDelStop.fldprim   = delcode.misc[7]
            tt-FindDelStop.fldstlst  = delcode.NAME
            tt-FindDelStop.fldnumrez = delcode.val
            tt-FindDelStop.flddatrez = mDataRez
            tt-FindDelStop.flddatadd = DATE(GetXattrValueEx(
                                            "code",
                                            delcode.class + 
                                            "," +
                                            delcode.code,
                                            "AddData",
                                            ""))
            tt-FindDelStop.flduser   = delcode.misc[8]
            .
         DELETE delcode.
      END.
      
      /* �㬥�㥬 ��ப� �⮯-���⮢ */
      FOR EACH delcode WHERE 
               delcode.parent EQ "StopList"
          AND  delcode.class  EQ "StopList"
          AND  CAN-DO(vDelSl,delcode.name) EXCLUSIVE-LOCK:
         ASSIGN
            vNum = vNum + 1
            delcode.description[1] = STRING(vNum) NO-ERROR.
      END.
   END.
END PROCEDURE.

/* ---------------------------------------------------------------- */
/*                ����� ��⮪��� �� �᪫���� ��ப��           */
/* ---------------------------------------------------------------- */
PROCEDURE PrintDelStr.
   DEFINE VARIABLE vName        AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vStop-List   AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vResolution  AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vBirthPlase  AS CHARACTER EXTENT 5 NO-UNDO.
   DEFINE VARIABLE vNum         AS INT64              NO-UNDO.
   DEFINE VARIABLE vI           AS INT64              NO-UNDO.
   
   /*�뢮� ��⮪���*/
   PUT STREAM rep "��ப�, 㤠��� �� �ࠢ�筨�� �⮯-�����." AT 70 SKIP.
   PUT STREAM rep "��� 㤠����� ��ப:" AT 75 TODAY FORMAT "99/99/9999" SKIP(1).
   PUT STREAM rep "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
   PUT STREAM rep "�� �/� � ��� � ���  �     ���/������������         � ��࠭� �    ���      �    ��ᯮ��    �   ���     �       ���� ஦�����         �       �ਬ�砭��   � �⮯-����                    �   ����� १���樨  �    ���    �    ���    � ���짮��⥫� �" SKIP.
   PUT STREAM rep "�      �     �      �                              �        �             �               �  ஦�����  �                              �                    �                              �                    �  १���樨 � ���������� �              �" SKIP.
   FOR EACH tt-FindDelStop NO-LOCK BY tt-FindDelStop.fldstlst
                                   BY tt-FindDelStop.fldtype
                                   BY tt-FindDelStop.fldname:
      ASSIGN
         vNum           =  vNum + 1
         vName[1]       =  tt-FindDelStop.fldname
         vStop-List[1]  =  tt-FindDelStop.fldstlst
         vResolution[1] =  tt-FindDelStop.fldnumrez
         vBirthPlase[1] =  tt-FindDelStop.fldbplace
      .
      {wordwrap.i &s=vName       &n=5 &l=30 }
      {wordwrap.i &s=vStop-List  &n=5 &l=30 }
      {wordwrap.i &s=vResolution &n=5 &l=20 }
      {wordwrap.i &s=vBirthPlase &n=5 &l=20 }
      PUT STREAM rep "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
      PUT STREAM rep
         "�" STRING(vNum)                     FORMAT "x(6)"  +
         "�" tt-FindDelStop.fldtype           FORMAT "x(5)"  +
         "�" IF {assigned tt-FindDelStop.fldcode} THEN tt-FindDelStop.fldcode ELSE " " FORMAT "x(6)"  +
         "�" vName[1]                         FORMAT "x(30)" +
         "�" IF {assigned tt-FindDelStop.fldcoun} THEN tt-FindDelStop.fldcoun ELSE " " FORMAT "x(8)"  +
         "�" tt-FindDelStop.fldinn            FORMAT "x(13)" +
         "�" tt-FindDelStop.fldpas            FORMAT "x(15)" +
         "�" STRING(tt-FindDelStop.fldbday,"99/99/9999") FORMAT "x(12)" +
         "�" vBirthPlase[1]                   FORMAT "x(30)" +
         "�" tt-FindDelStop.fldprim           FORMAT "x(20)" +
         "�" vStop-List[1]                    FORMAT "x(30)" +
         "�" vResolution[1]                   FORMAT "x(20)" +
         "�" STRING(tt-FindDelStop.flddatrez) FORMAT "x(12)" +
         "�" STRING(tt-FindDelStop.flddatadd) FORMAT "x(12)" +
         "�" tt-FindDelStop.flduser           FORMAT "x(14)" +
         "�"
         SKIP.
      DO vI = 2 TO 5:
         IF    vName[vI]       NE ""
            OR vStop-List[vI]  NE ""
            OR vResolution[vI] NE ""
            OR vBirthPlase[vI] NE ""
         THEN
            PUT STREAM rep
               "�" " "           FORMAT "x(6)"  +
               "�" " "           FORMAT "x(5)"  +
               "�" " "           FORMAT "x(6)"  +
               "�" IF vName[vI]        NE "" THEN vName[vI]      ELSE " " FORMAT "x(30)" +
               "�" " "           FORMAT "x(8)"  +
               "�" " "           FORMAT "x(13)" +
               "�" " "           FORMAT "x(15)" +
               "�" " "           FORMAT "x(12)" +
               "�" IF vBirthPlase[vI]  NE "" THEN vBirthPlase[vI]  ELSE " " FORMAT "x(30)" +
               "�" " "           FORMAT "x(20)" +
               "�" IF vStop-List[vI]  NE "" THEN vStop-List[vI]  ELSE " " FORMAT "x(30)" +
               "�" IF vResolution[vI] NE "" THEN vResolution[vI] ELSE " " FORMAT "x(20)" +
               "�" " "           FORMAT "x(12)" +
               "�" " "           FORMAT "x(12)" +
               "�" " "           FORMAT "x(14)"
               "�"
               SKIP.
      END.
   END.
   PUT STREAM rep "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP.
END PROCEDURE.

PROCEDURE PrepareDataBlock:
   DEFINE INPUT PARAM iDataId  AS INT64  NO-UNDO.
   DEFINE BUFFER bcode     FOR code.
   DEFINE BUFFER DataBlock FOR DataBlock.
   DEFINE BUFFER DataClass FOR DataClass.

   /*�஢��塞 ���� �� ����� ����� ���� ������*/
   FIND FIRST DataBlock WHERE 
              DataBlock.Branch-Id       EQ shFilial 
          AND DataBlock.DataClass-Id    EQ 'StopList' 
          AND DataBlock.Data-ID         EQ iDataId
   NO-LOCK  NO-ERROR.

   /*������뢠�� �������騩 ����*/
   IF NOT AVAIL DataBlock THEN
   /*ᮧ���� ���� � ��⮩ ࠢ��� ��� ����㧪�*/
   DO:
      {sv-add().i "99" TODAY TODAY shFilial "'stoplist'"}

      IF NOT AVAIL DataBlock THEN
      DO:
         MESSAGE "�� �� ����� �ࠢ� ᮧ������ ����� ������ �⮣� �����." VIEW-AS ALERT-BOX.
         RETURN "ERROR".
      END.
      FIND FIRST DataClass WHERE DataClass.DataClass-id EQ ENTRY(1,DataBlock.DataClass-id,'@') NO-LOCK NO-ERROR.
      RUN sv-clc.p (DataClass.DataClass-ID,
                    DataBlock.Branch-ID,
                    DataBlock.Beg-Date,
                    DataBlock.End-Date).
      
      FIND FIRST bCode WHERE
                 bCode.class EQ ""
             AND bCode.code  EQ "StopList"
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE(bCode) THEN DO:
         RUN normdbg IN h_debug (0,"�訡��","�����䨪��� �����஢�� ��㣨� ���짮��⥫��.").
         LEAVE.
      END.
      ASSIGN bCode.Misc[8] = STRING(DataBlock.Data-ID  ).
   END.
END PROCEDURE.
/* $LINTFILE='loadstoplst.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='anba' */
/* $LINTDATE='07/06/2016 08:40:31.849+03:00' */
/*prosignF/eTSZJHf68Y4wdxG9YKgA*/