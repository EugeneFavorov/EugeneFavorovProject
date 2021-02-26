/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: 
     Filename: clientsomsk.fun
      Comment: �㭪樨, ��楤�ન...
   Parameters: 
         Uses:
      Used by:
      Created: kam 
     Modified:    
*/

/* �஢����, ���� �� ��ப� - �᫮� */
FUNCTION IsMyNumber LOGICAL (iStr AS CHAR):
   DEF VAR vRes AS INT64 NO-UNDO.
   vRes = INT64(iStr + '.0') NO-ERROR. /* �⮡� �㣠���� �� Decimal */
   RETURN ERROR-STATUS:ERROR.
END FUNCTION.

/* ॣ����:  55 = 00052 */
FUNCTION ConvertRegion RETURN CHAR(INPUT iRegion AS CHARACTER):
    DEFINE VAR strResult AS CHAR NO-UNDO.
    DEF VAR GNI2OKATO AS CHAR NO-UNDO.
    DEF VAR ii AS INT NO-UNDO.
    
    GNI2OKATO =
        		"00079,00080,00081,00084,00082,00026,00083,00085,00091,00086," + /*  1 - 10 */
        		"00087,00088,00089,00098,00090,00092,00093,00094,00095,00096," + /* 11 - 20 */
        		"00097,00001,00003,00004,00005,00007,00008,00010,00011,00012," + /* 21 - 30 */
        		"00014,00015,00017,00018,00019,00020,00024,00025,00027,00029," + /* 31 - 40 */
        		"00030,00032,00033,00034,00037,00038,00041,00042,00044,00046," + /* 41 - 50 */
        		"00047,00022,00049,00050,00052,00053,00054,00056,00057,00058," + /* 51 - 60 */
        		"00060,00061,00036,00063,00064,00065,00066,00068,00028,00069," + /* 61 - 70 */
        		"00070,00071,00073,00075,,00078,00045,00040,00099,00076," + /* 71 - 80 */
        		"00087,,,,00025,,00077,,," + /* 81 - 90 */
        		",,,,,,,,,".
    ii = INTEGER( iRegion) NO-ERROR.
    strResult = (IF ii NE ? AND ii > 0 AND ii < 100 THEN ENTRY( ii, GNI2OKATO ) ELSE "").
    /* ��⮢�� ⠡��� *
        	DO ii = 1 TO 99:
        	    PUT UNFORMATTED
        		GetCodeName( "���������", (IF ii < 10 THEN "0" ELSE "" ) + STRING( ii)) " "
        		GetCodeName( "������", ENTRY( ii,GNI2OKATO))
        		SKIP.
        	END. */
    /*  IF iRegion = "55" THEN 
    CASE iRegion:
        WHEN "55" THEN strResult = "00052".
        OTHERWISE DO:
            strResult = "".
        END.
    END CASE.*/
    RETURN strResult.
END FUNCTION.

/* �஡㥬 �ਢ��� ���� � �㦭� �ଠ� */
FUNCTION ConvertAdress2BIS RETURN CHAR(INPUT iAdr AS CHARACTER):
    DEFINE VAR returnAdr AS CHAR NO-UNDO.
    DEFINE VAR tmpAdr AS CHAR NO-UNDO.
    DEFINE VAR iNums AS INT64 NO-UNDO.
    DEFINE VAR tmpKodReg AS CHAR NO-UNDO.
    DEFINE VAR strStatus AS CHAR NO-UNDO.
    DEF VAR tmpStr AS CHAR NO-UNDO.
    /*
     ��ଠ� ���� ���������:
 1 ���⮢� ������ 6                --- 644109
 2 ��࠭� 40                        --- ����� 
 3 ������ 40                        --- ��᪠� ��� 
 4 ������� 40                       --- NULL
 5 ��த 40                         --- ��� �
 6 �������� ��ᥫ������ �㭪� 40   --- NULL
 7 �������� 㫨�� 40                --- ������ ���
 8 ����� ���� 40                    --- 6
 9 ����� 40                        --- NULL
 10 ������ 40                     --- NULL
 
    ,644019,55,,���,,22 ��५�,15,,27

    */
    
    strStatus = "bad".
    tmpKodReg = "".
    IF iAdr <> ? THEN DO ON ERROR UNDO, THROW:
        tmpAdr = TRIM(iAdr).
        tmpStr = ENTRY( 1, tmpAdr).
        IF tmpStr NE ''
          AND LENGTH( tmpStr) = 6
          AND ( NOT IsMyNumber( tmpStr))
         THEN DO ON ERROR UNDO, THROW:
            /* � ���� �����ᮬ �� ���� � �ଠ� ��� */
            returnAdr = TRIM( tmpAdr).
            tmpKodReg = ''.
        END. ELSE DO ON ERROR UNDO, THROW:
        IF NUM-ENTRIES(tmpAdr) > 1 THEN
            returnAdr = TRIM(ENTRY(2,tmpAdr)). /* 1 ������ */
        ELSE 
            returnAdr = tmpAdr.
        IF NUM-ENTRIES(tmpAdr) > 2 THEN
            tmpKodReg = /* ConvertRegion*/ (TRIM(ENTRY(3,tmpAdr))). /* ॣ��� */      
        IF NUM-ENTRIES(tmpAdr) > 3 THEN
            returnAdr = returnAdr + ',' + TRIM(ENTRY(4,tmpAdr)). /* 2 ࠩ�� */
        IF NUM-ENTRIES(tmpAdr) > 4 THEN
            returnAdr = returnAdr + ',' + TRIM(ENTRY(5,tmpAdr)). /* 3 ��த */        
        IF NUM-ENTRIES(tmpAdr) > 5 THEN
            returnAdr = returnAdr + ',' + TRIM(ENTRY(6,tmpAdr)). /* 4 ��ᥫ���� �㭪� */ 
        IF NUM-ENTRIES(tmpAdr) > 6 THEN
            returnAdr = returnAdr + ',' + TRIM(ENTRY(7,tmpAdr)). /* 5 㫨� */ 
        IF NUM-ENTRIES(tmpAdr) > 7 THEN
            returnAdr = returnAdr + ',' + TRIM(ENTRY(8,tmpAdr)). /* 6 ����� ���� */ 
        IF NUM-ENTRIES(tmpAdr) > 8 THEN
            returnAdr = returnAdr + ',' + ENTRY( 1, TRIM(ENTRY( 9, tmpAdr)), "/"). /* 7 ����� */
        returnAdr = returnAdr + ',' +
    	     ( IF NUM-ENTRIES(tmpAdr) > 9 THEN TRIM( ENTRY( 10, tmpAdr)) ELSE ""). /* 8 ������ */
        returnAdr = returnAdr + ',' +
    	     ( IF NUM-ENTRIES(tmpAdr) > 8 AND NUM-ENTRIES( TRIM( ENTRY( 9, tmpAdr)), '/') > 1
    	    	 THEN ENTRY( 2,TRIM( ENTRY( 9, tmpAdr)), "/") ELSE ""). /* 9 ��஥��� */
	END.
/* PUT UNFORMATTED "##" tmpAdr SKIP "##" tmpKodReg SKIP. */
    END.
    ELSE returnAdr = ",".
    strStatus = "ok". /* ��㧨� �ᥣ�� */    
    iNums = NUM-ENTRIES(returnAdr).
    DO WHILE iNums < 10 ON ERROR UNDO, THROW:
        returnAdr = returnAdr + (IF iNums < 9 THEN ',' ELSE "").
        iNums = iNums + 1.
    END.
    /**/
    IF tmpKodReg NE "" THEN
    DO:
       IF tmpKodReg EQ "77" 
       AND ENTRY(3,returnAdr) EQ ""   
       THEN
          ENTRY(3,returnAdr) = "��᪢� �". 
       IF tmpKodReg EQ "78" 
       AND ENTRY(3,returnAdr) EQ ""
       THEN
          ENTRY(3,returnAdr) = "�����-������ �".
    END.
    /**/
    returnAdr = strStatus + "|" + tmpKodReg + "|" + returnAdr.
    RETURN returnAdr.
END FUNCTION.

FUNCTION Convert2Gender RETURN CHAR(INPUT vFName AS CHARACTER, INPUT vGender AS CHARACTER):
    DEF VAR femaleNames AS CHAR NO-UNDO INIT "�������,�����,����,���,��⠫��,�����⨭�,����ﭠ,��죠,��࣠��,����ਭ�,����,����,���⫠��,�����,�ਭ�,������,��ਭ�,����,�����,������,�����,��஭���,�����,��ᠭ�,�����,����,����,�������,����ᠭ��,����,������,���,�������,����,�����,��졨��,��������,����,������,����,���,����,���,����,�����,�ᥭ��,������,�����,���".
    DEF VAR maleNames AS CHAR NO-UNDO INIT "����਩,��ࣥ�,����⠭⨭,�������,��堨�,����ᥩ,����ᠭ��,���३,����,�����,�����,��������,�������,������,����⠪,��⥬,�����,�⥯��,��ᨫ��,��⮭,����,���⮫��,����,���ᨬ,�ਣ�਩,�����,�����,����,��������,�਩,��⠫��,����,�����᫠�,���᫠�,����,�����,��⥬,�⠭�᫠�,����਩,����᫠�,�����,��줠�,�����,��ਫ�,������,����,�����".
    IF vGender = "M" THEN RETURN "�".
    IF vGender = "�" OR vGender = "�" THEN RETURN vGender.
    IF NUM-ENTRIES( vFName, " ") > 1 THEN DO ON ERROR UNDO, THROW:
	IF LOOKUP( ENTRY(2, vFName, " "), femaleNames) > 0 THEN RETURN "�".
	IF LOOKUP( ENTRY(2, vFName, " "), maleNames) > 0 THEN RETURN "�".
    END.
    RETURN "�" /*?*/ .
END.

/* �஡㥬 �ਢ��� ���� � �㦭� �ଠ� */
FUNCTION ConvertAdress2ABS RETURN CHAR(INPUT iKodRegGni AS CHARACTER, INPUT iAdr AS CHARACTER):
    DEFINE VAR returnAdr AS CHAR NO-UNDO.
    DEFINE VAR tmpAdr AS CHAR NO-UNDO.
    DEFINE VAR iNums AS INT64 NO-UNDO.
    DEFINE VAR tmpKodReg AS CHAR NO-UNDO.
    DEFINE VAR strStatus AS CHAR NO-UNDO.
    /*
     ��ଠ� ���� ���������:
 1 ���⮢� ������ 6                --- 644109
 2 ��࠭� 40                        --- ����� 
 3 ������ 40                        --- ��᪠� ��� 
 4 ������� 40                       --- NULL
 5 ��த 40                         --- ��� �
 6 �������� ��ᥫ������ �㭪� 40   --- NULL
 7 �������� 㫨�� 40                --- ������ ���
 8 ����� ���� 40                    --- 6
 9 ����� 40                        --- NULL
 10 ������ 40                     --- NULL
 
    ,644019,55,,���,,22 ��५�,15,,27

    */
    IF NUM-ENTRIES( iAdr) < 9
	THEN UNDO, THROW NEW Progress.Lang.AppError( "���� ᮤ�ন� ����� 9 ����⮢").
	/* RETURN ERROR "���� ᮤ�ন� ����� 9 ����⮢".*/

    ReturnAdr = "," + /* 1 */
		ENTRY( 1, iAdr) + "," + /* 2 ������ */
		iKodRegGni      + "," + /* 3 ॣ��� ��� */
		ENTRY( 2, iAdr) + "," + /* 4 ࠩ�� */
		ENTRY( 3, iAdr) + "," + /* 5 ��த */
		ENTRY( 4, iAdr) + "," + /* 6 ���.�㭪� */
		ENTRY( 5, iAdr) + "," + /* 7 㫨� */
		ENTRY( 6, iAdr) + "," + /* 8 ��� */
		ENTRY( 7, iAdr) + (IF ENTRY( 9, iAdr) <> "" THEN "/" + ENTRY( 9, iAdr) ELSE "") + "," + /* 9 ��� / ��஥��� */
		ENTRY( 8, iAdr) + ","   /* 10 ������ */
		.
    RETURN returnAdr.
END FUNCTION.


/* �஡㥬 ���������� � ���� */
/* �室��� ��ப� � �ଠ� 11.11.11  ��� 11.11.2011 */
FUNCTION ConvertDate RETURN DATE(INPUT iDate AS CHARACTER):
    DEFINE VAR returnDate AS DATE NO-UNDO INIT ?.
    DEFINE VAR mTmpStr AS CHAR NO-UNDO.
    DEFINE VAR mTmpDay AS INT64 NO-UNDO.
    DEFINE VAR mTmpMonth AS INT64 NO-UNDO.
    DEFINE VAR mTmpYear AS INT64 NO-UNDO.
    DEFINE VAR mTmpDate AS DATE NO-UNDO.

    
    ERROR-STATUS:ERROR = NO.
    mTmpStr = REPLACE(iDate,"/",".").
        IF LENGTH(mTmpStr) = 8 OR LENGTH(mTmpStr) = 10 THEN DO:
            mTmpDay = INT(SUBSTRING(mTmpStr,1,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpMonth = INT(SUBSTRING(mTmpStr,4,2)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpYear = INT(SUBSTRING(mTmpStr,7)) NO-ERROR.
            IF ERROR-STATUS:ERROR = YES THEN RETURN.
            ERROR-STATUS:ERROR = NO.
            mTmpDate = DATE (mTmpMonth,mTmpDay,mTmpYear) NO-ERROR.
            IF ERROR-STATUS:ERROR = NO THEN DO:
                returnDate = mTmpDate.
            END.
        END.
    RETURN returnDate.
END FUNCTION.

/* ����������� �訡�� */
FUNCTION ADDERROR RETURN CHAR(INPUT iAllErr AS CHAR, INPUT iErr AS CHAR):
    DEFINE VAR returnErr AS CHAR NO-UNDO.
    returnErr = iAllErr.
    IF LENGTH(iAllErr) > 0 THEN returnErr = returnErr + ';'.
    returnErr = returnErr + iErr.
    RETURN returnErr.
END.


/* �஢�ઠ ����� ���㬥�� �� ॣ.��ࠦ����, �������� � �ࠢ�筨�� ������� */
FUNCTION IsCorrectNumberDoc LOGICAL(INPUT iTypeDoc AS CHAR, INPUT iNumberDoc AS CHAR):
    DEFINE VARIABLE vRegExpr AS CHAR NO-UNDO.
    DEFINE VARIABLE vResult AS CHAR NO-UNDO.
    DEFINE VARIABLE vErrMes AS CHAR NO-UNDO.
    vRegExpr = GetCodeMisc("�������", iTypeDoc, 3).
    RETURN NOT DYNAMIC-FUNCTION("ereg":U, vRegExpr, iNumberDoc, OUTPUT vResult, INPUT-OUTPUT vErrMes).
END.

/*----------------------------------------------------------------------------*/
/* �����頥� ᢮����� ID ��� ����� ����� cust-corp                         */
/*----------------------------------------------------------------------------*/
FUNCTION Get-New-CustCorp-Id RETURNS INT64:
   { getnewid.i
     &file           = cust-corp
     &id-field       = cust-id
     &use-index-name = corp-cust
   }
   RETURN vCust-Id-tmp.
END FUNCTION.

/* ������ �訡�� � ��ப� ������ */
PROCEDURE SET_ERROR_CLIENT.
    DEFINE INPUT PARAM iError AS INT64 NO-UNDO. /* ��� �訡�� */
    DEFINE INPUT PARAM strError AS CHAR NO-UNDO. /* ⥪�� �訡��  */ 
    IF AVAIL bank.clients-mfr THEN DO:
        IF bank.clients-mfr.id <> ? THEN
            strError = "ID = " + STRING(bank.clients-mfr.id) + ', '  + strError.
        bank.clients-mfr.stat = iError.
        bank.clients-mfr.errortext = TRIM(strError).
    END.
    ELSE DO:
        PUT UNFORMATTED "�� ������� ������ � �����⮬!!!" SKIP.
        /* MESSAGE "�� ������� ������ � �����⮬!!!" VIEW-AS ALERT-BOX. */
    END.    
END PROCEDURE.

FUNCTION sUpdateSigns RETURN LOG(
   INPUT iTable AS CHARACTER,
   INPUT iSurr  AS CHARACTER,
   INPUT iCode  AS CHARACTER,
   INPUT iVal   AS CHARACTER,
   INPUT iIndex AS LOGICAL
   ):
   DEF VAR vOk AS LOGICAL NO-UNDO.

   TR:
   DO
      ON ERROR UNDO TR, RETURN ERROR
      ON STOP  UNDO TR, RETURN ERROR:
      IF GetXattrValueEx( iTable, iSurr, iCode, "") <> iVal
       THEN vOk = UpdateSigns(iTable, iSurr, iCode, iVal, iIndex).
       ELSE vOk = TRUE.
   END.  /* End of TR BLOCK */
   RETURN vOk.
END FUNCTION.

