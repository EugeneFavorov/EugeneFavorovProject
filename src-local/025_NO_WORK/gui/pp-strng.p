{globals.i}
{intrface.get tmess}

/* +++ pp-strng.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-strng.p
      Comment: �᭮��� �㭪樨 � ��楤��� ��� ࠡ��� � ��ப��� � ᯨ᪠��.
               ���饭�� � ��楤�ࠬ �१ handle h_str
   Parameters: ���
      Created: 10/09/2003   fedm
     Modified: 14/08/2003 Om ��ࠡ�⪠.
                             ��ꥤ����� ������⥪� �� ࠡ�� � ��ப���.
     Modified: 19.09.2005 19:20 KSV      (0046989) ��������� �㭪��                             
                                         SortDelimList, ��� ���஢�� ᯨ᪮�
                                         ��ப.
     Modified: 29.09.2005 19:23 KSV      (0050131) ��������� �㭪樨 BQEncode,
                                         BQDecode � GetUniqueFileName.
     Modified: 06.05.2006 15:48 VASOV    (0061624) ��������� �㭪��
                                         DelDoubleChars

   F/P  Name                   Comment
   ���  ���������������������  ������������������������������������������
    F   FStrEmpty              ��ப� �����?

    F   FStrNVL                ��ப� ����� (१���� - ��ப�)?

    F   FStrCat                �����⥭��� ��ப asSrc || asCat
                               � ࠧ����⥫�� asDelim
    F   FStrPad                ���������� ��ப� asStr �஡�����
                               �� ����� anLen �ࠢ� ��� ᫥��
    F   FStrPadC               ���������� ��ப� asStr ᨬ������
                               �� ����� anLen �ࠢ� ��� ᫥��
    F   FStrCenter             �����஢���� ��ப� asStr
                               �� �ਭ� anLen ᨬ�����
    F   GetEntries             �������� ����祭�� ����� ᯨ᪠

    F   SetEntries             �������� ��᢮���� ���祭�� ��� ����� ᯨ᪠
    
    F   PADR                   ���������� �஡����� �ࠢ�

    F   PADL                   ���������� �஡����� ᫥��

    F   PAD�                   �������� ��ப�

    P   PutScreen              �뢮� ���ଠ樨 �� �࠭


    F   ConvMatch2Beg          ������� BEGINS-蠡��� �� MATCHES-蠡����.

    P   SetLstParam            ��⠭����� ���祭�� ��ࠬ��� � 㪠����� �����.

    P   DelLstParam            ������� ��ࠬ���� � 㪠����묨 ������.

    F   GetLstParam            ������� ���祭�� ��ࠬ��� � 㪠����� �����.

    F   GetLstParamS           �� ᯨ�� ��ࠬ��஢ �����頥� ᯨ᮪ ���祭��.

    F   GetEqParamList         ������� ᯨ᮪ ��ࠬ��஢ � 㪠����� ���祭���.

    F   DelDoubleChars         ������� �� ��ப� ������ ��������� ᨬ����

    F   GetValueRange          ����祭�� ��������� ��������� �� ��ப�,
                               ��室 �� �� �।��� � �����-���� ��஭�
                               �������� १���� ᯥ樠��� ᨬ�����

    P   ParseIntRange          ���⠪��᪨� ࠧ��� ��ࠦ����, �����饣� 
                               楫��᫥��� ���ࢠ�
    P   CompareAsInt           �ࠢ����� ���� ��ப ��� 楫�� �ᥫ
*/

{pfuncdef
   &DefLib="strng"
   &Description="������⥪� ࠡ��� � ��ப��� � ᯨ᪠��"}

&GLOBAL-DEFINE pp-str

/* Commented by KSV: ������⥪� �㭪権 ��� ��ࠡ�⪨ ॣ����� ��ࠦ����
** !!! ������祭� ��� �����, �.�. 䠩� ����� �ᯮ�짮������ �� ⮫쪮 �
** � ��������  */
{pp-re.p}

/* Commented by KSV: ������⥪� �㭪権 ��� ��ࠡ�⪨ ��ப */
{strings.fun}

/* ��楤�� ࠧ��� ४����⮢ person.document � opb.���� */
{docum-sn.pro}

/*----------------------------------------------------------------------------*/
/* �� 蠡���� ��� MATCHES �����頥� 蠡��� ��� BEGINS                        */
/*----------------------------------------------------------------------------*/
FUNCTION ConvMatch2Beg  RETURNS CHAR
   (iMatch AS CHAR):

   &SCOPED-DEFINE SpecChar "*,."

   /* ����稪 */
   DEF VAR vCnt  AS INT64  NO-UNDO.
   /* ������ � ��ப� */
   DEF VAR vPos  AS INT64  NO-UNDO.

   DO vCnt = 1 TO NUM-ENTRIES({&SpecChar}):
      vPos = INDEX(iMatch, ENTRY(vCnt, {&SpecChar})).

      IF vPos > 0 THEN
        iMatch = SUBSTR(iMatch, 1, vPos - 1).
   END.

   RETURN iMatch.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* ��⠭�������� ���祭�� ��ࠬ��� ᯨ᪠ � 㪠����� �����                  */
/*----------------------------------------------------------------------------*/
PROCEDURE SetLstParam:
   DEF INPUT        PARAMETER iCod      AS CHAR  NO-UNDO.  /* ��� ��ࠬ���   */
   DEF INPUT        PARAMETER iVal      AS CHAR  NO-UNDO.  /* ���祭�� ���-� */
   DEF INPUT-OUTPUT PARAMETER ioCodLst  AS CHAR  NO-UNDO.  /* ���᮪ �����    */
   DEF INPUT-OUTPUT PARAMETER ioValLst  AS CHAR  NO-UNDO.  /* ���᮪ ���祭�� */
   DEF INPUT        PARAMETER iDlm      AS CHAR  NO-UNDO.  /* �������⥫�     */

   /* ����稪 */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.

   vNum = NUM-ENTRIES(ioCodLst, iDlm).

   DO vCnt = 1 TO vNum:
      IF ENTRY(vCnt, ioCodLst, iDlm) = iCod THEN
      DO:
         ENTRY(vCnt, ioValLst, iDlm) = iVal.
         RETURN.
      END.
   END.

   IF ioCodLst = "" THEN
      ASSIGN
         ioCodLst = iCod
         ioValLst = iVal.
   ELSE
      ASSIGN
         ioCodLst = iCod + iDlm + ioCodLst
         ioValLst = iVal + iDlm + ioValLst.

   RETURN.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* ������� ��ࠬ���� � 㪠����묨 ������                                      */
/* (���� ��ࠬ��� - ᯨ᮪ 㤠�塞�� ����� �१ �������)                   */
/*----------------------------------------------------------------------------*/
PROCEDURE DelLstParam:
   DEF INPUT        PARAMETER iDelCodes AS CHAR  NO-UNDO.  /* ����塞� ����  */
   DEF INPUT-OUTPUT PARAMETER ioCodLst  AS CHAR  NO-UNDO.  /* ���᮪ �����    */
   DEF INPUT-OUTPUT PARAMETER ioValLst  AS CHAR  NO-UNDO.  /* ���᮪ ���祭�� */
   DEF INPUT        PARAMETER iDlm      AS CHAR  NO-UNDO.  /* �������⥫�     */

   /* ����稪 */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.
   /* ���� ᯨ᮪ ����� */
   DEF VAR vCodLst AS CHAR NO-UNDO.
   /* ���� ᯨ᮪ ���祭�� */
   DEF VAR vValLst AS CHAR NO-UNDO.

   vNum = NUM-ENTRIES(ioCodLst, iDlm).
   DO vCnt = 1 TO vNum:
      IF LOOKUP(ENTRY(vCnt, ioCodLst, iDlm), iDelCodes) = 0 THEN
         ASSIGN
            vCodLst = vCodLst + iDlm + ENTRY(vCnt, ioCodLst, iDlm)
            vValLst = vValLst + iDlm + ENTRY(vCnt, ioValLst, iDlm).
   END.

   ASSIGN
      ioCodLst = SUBSTR(vCodLst, LENGTH(iDlm) + 1)
      ioValLst = SUBSTR(vValLst, LENGTH(iDlm) + 1).

   RETURN.

END PROCEDURE.

/*----------------------------------------------------------------------------*/
/* �����頥� ���祭�� ��ࠬ��� ᯨ᪠ � 㪠����� �����                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetLstParam     RETURNS CHAR
   (iCod     AS CHAR, /* ��� ��ࠬ���      */
    iCodLst  AS CHAR, /* ���᮪ �����       */
    iValLst  AS CHAR, /* ���᮪ ���祭��    */
    iDlm     AS CHAR  /* �������⥫�        */
   ):

   /* ����稪 */
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.

   vNum = NUM-ENTRIES(iCodLst, iDlm).

   DO vCnt = 1 TO vNum:
      IF ENTRY(vCnt, iCodLst, iDlm) = iCod THEN
         RETURN ENTRY(vCnt, iValLst, iDlm).
   END.

   RETURN ?.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �� ᯨ�� ��ࠬ��஢ �����頥� ᯨ᮪ �� ���祭��                         */
/* (���� ��ࠬ��� - ᯨ᮪ ����� �१ �������)                             */
/*----------------------------------------------------------------------------*/
FUNCTION GetLstParamS  RETURNS CHAR
   (iCod     AS CHAR, /* ���� ��ࠬ��஢    */
    iCodLst  AS CHAR, /* ���᮪ �����       */
    iValLst  AS CHAR, /* ���᮪ ���祭��    */
    iDlm     AS CHAR  /* �������⥫�        */
   ):

   /* ����稪 */
   DEF VAR vCnt  AS INT64   NO-UNDO.
   /* �����頥�� ᯨ᮪ ���祭�� */
   DEF VAR vLst  AS CHAR  NO-UNDO.

   vLst = REPLACE(iCod, ",", iDlm).

   DO vCnt = 1 TO NUM-ENTRIES(iCod):
      ENTRY(vCnt, vLst, iDlm) = GetLstParam(ENTRY(vCnt, iCod),
                                            iCodLst,
                                            iValLst,
                                            iDlm
                                           ) NO-ERROR.
   END.

   RETURN vLst.

END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �����頥� ᯨ᮪ ����� ��ࠬ��஢ (�१ �������),                        */
/* ������ 㪠������ ���祭��                                                 */
/*----------------------------------------------------------------------------*/
FUNCTION GetEqParamList     RETURNS CHAR
   (iVal     AS CHAR, /* ���祭�� ��ࠬ��� */
    iCodLst  AS CHAR, /* ���᮪ �����       */
    iValLst  AS CHAR, /* ���᮪ ���祭��    */
    iDlm     AS CHAR  /* �������⥫�        */
   ):

   /* ����稪                  */
   DEF VAR vCnt           AS INT64   NO-UNDO.
   /* ��� ����� ᯨ᪠      */
   DEF VAR vCod           AS CHAR  NO-UNDO.
   /* ���祭�� ����� ᯨ᪠ */
   DEF VAR vVal           AS CHAR  NO-UNDO.
   /* �����頥��� ���祭��    */
   DEF VAR vRetVal        AS CHAR  NO-UNDO.

   /* ����� ��ॡ�� ����� ��易⥫��! */
   DO vCnt = NUM-ENTRIES(iCodLst, iDlm) TO 1 BY -1:

      ASSIGN
         vCod = ENTRY(vCnt, iCodLst, iDlm)
         vVal = ENTRY(vCnt, iValLst, iDlm).

      IF vVal = iVal THEN
      DO:
         {additem.i vRetVal vCod}
      END.
   END.

   RETURN vRetVal.

END FUNCTION.

/* ������� �宦����� ᯥ�ᨬ����� � ��ப� �� �� ���� ��� ���४⭮�� 
** ࠡ��� prepare  ��ࠦ���� � �������᪨� query. */
FUNCTION RepSpecSym RETURNS CHAR (
   INPUT iStr  AS CHAR, /* ��㬥�� ��� WHERE ��ࠦ����. */
   INPUT iOper AS CHAR  /* ������. */
): 
   DEF VARI vSymList AS CHAR   NO-UNDO.
   DEF VARI vNum     AS INT64    NO-UNDO.
   DEF VARI vInd     AS INT64    NO-UNDO.
   DEF VAR  vOperLst AS CHAR   NO-UNDO. /* ���᮪ ����権. */

   ASSIGN
      /* " - 34, ' - 39, { - 123 , } - 125 , \ - 92 */
      vSymList = "126,34,39,123,125,92"
      vOperLst = "EQ,GE,GT,LE,LT,BEGINS,=,>=,>,<=,<"
   .
   DO vNum = 1 TO NUM-ENTRIES (vSymList):
      vInd = INDEX (iStr, CHR (INT64 (ENTRY (vNum, vSymList)))).

      IF vInd NE 0 THEN   /* �᫨ ᯥ�ᨬ��� ��������� �����塞 �� ~ᯥ�ᨬ��� */
         iStr = REPLACE(iStr,
                        CHR(INT64(ENTRY(vNum,vSymList))),
                        /* �㡫� ����室�� ⮫쪮 ��� ����権
                        ** �� �室��� � ᯨ᮪ vOperLst. */
                        IF       ENTRY  (vNum,vSymList)     EQ "92"
                           AND   LOOKUP (iOper, vOperLst)   EQ 0
                           THEN CHR(126) + CHR(INT64(ENTRY(vNum,vSymList))) + 
                                CHR(126) + CHR(INT64(ENTRY(vNum,vSymList)))
                           ELSE CHR(126) + CHR(INT64(ENTRY(vNum,vSymList)))
                        ).
   END.
   RETURN iStr.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* �஢���� ���४⭮��� �ଠ� ��� 㪠������� ⨯� ������                  */
/*----------------------------------------------------------------------------*/
FUNCTION IsValidFormat RETURNS LOGICAL 
   (INPUT iDataType AS CHAR,
    INPUT iFormat   AS CHAR):
   
   DEFINE VARIABLE vRetValue AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE vTestChar AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTestDec  AS DECIMAL    NO-UNDO.
   
   /* ��� ������� �ଠ� �����筮 3 �㪢� */
   CASE SUBSTR (iDataType,1,3):
      /* �����᪨� */
      WHEN "LOG" THEN vRetValue = (NUM-ENTRIES (iFormat,"/") NE 2).
      /* ��⠫�� ������� */
      WHEN "DEC" OR
      WHEN "INT" THEN
      DO:
         ASSIGN vTestChar = STRING (0,iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR. 
      END.
      WHEN "CHA" THEN
      DO:
         ASSIGN vTestChar = STRING (FILL ("9",500),iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR.
      END.
      WHEN "DAT" THEN
      DO:
         ASSIGN vTestChar = STRING (TODAY, iFormat) NO-ERROR.
         vRetValue = ERROR-STAT:ERROR.
      END.
      /* ��⠫�� ��������� */
      OTHERWISE vRetValue = NO.
   END CASE.
   /* ������� ࠡ��� */
   RETURN NOT (vRetValue).
END FUNCTION.



/*----------------------------------------------------------------------------*/
/* �������� ������� ��ப� �� �����ப�                                       */
/*----------------------------------------------------------------------------*/
FUNCTION SplitStr RETURNS CHAR
   (iVal   AS CHAR, /**/
    iLen   AS INT64,  /*����� ��ப�*/
    iSep   AS CHAR  /*ᨬ���-ࠧ����⥫�*/
   ):

   {wordwrap.def}

   /* ����稪 */
   DEF VAR vCnt  AS INT64   NO-UNDO.
   /* ������������ ���⠢騪� */
   DEF VAR vStr  AS CHAR  NO-UNDO EXTENT 20.

   ASSIGN
      vStr[1] = iVal
      iVal = "".

   {wordwrap.i
      &s = vStr
      &n = EXTENT(vStr)
      &l = iLen
   }

   DO vCnt = 1 TO EXTENT(vStr) WHILE vStr[vCnt] <> "":
      iVal = iVal
           + ( IF iVal = "" THEN "" ELSE iSep)
           + STRING(vStr[vCnt], "x(" + STRING(iLen) + ")").
   END.
  
   RETURN iVal.
  
END FUNCTION.

DEFINE TEMP-TABLE tSort NO-UNDO
   FIELD fItem AS CHARACTER
   INDEX iItem fItem
   .
/*------------------------------------------------------------------------------
  Purpose:     �����頥� �����஢���� ᯨ᮪
  Parameters:  iList - ᯨ᮪ ��� ���஢��
               iDlm  - ࠧ����⥫� ᯨ᪠
               iDsc  - YES - ���஢�� �� �����⠭��
  Notes:       
------------------------------------------------------------------------------*/
FUNCTION SortDelimList RETURNS CHARACTER (iList   AS CHARACTER,
                                          iDlm    AS CHARACTER,
                                          iDsc    AS LOGICAL):
   DEFINE VARIABLE vNum AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vStr AS CHARACTER  NO-UNDO.
   
   IF NOT {assigned iDlm} THEN iDlm = ",".

   EMPTY TEMP-TABLE tSort.

   vNum = NUM-ENTRIES(iList,iDlm).
   DO vCnt = 1 TO vNum:
      CREATE tSort.
      tSort.fItem = ENTRY(vCnt,iList,iDlm).
   END.
   
   IF iDsc THEN
      FOR EACH tSort BY tSort.fItem DESC:
         vStr = vStr + ( IF vStr = "" THEN "" ELSE iDlm) + tSort.fItem.
      END. /* FOR EACH */
   ELSE
      FOR EACH tSort BY tSort.fItem:
         vStr = vStr + ( IF vStr = "" THEN "" ELSE iDlm) + tSort.fItem.
      END. /* FOR EACH */

   RETURN vStr.

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ������� ��ப�
  Parameters:  iString - ��ப�
  Notes:       �㭪�� ������ �ᯮ�짮������ ��� ����祭�� ���-��ப�, 
               ����� �� ᮤ�ন� ������� ᨬ�����, �஬� ������, �� 
               ��������  �� �ᯮ�짮���� � �� �����, ��� �������⨬� ���
               ������� ᨬ����, ���ਬ�� XML
------------------------------------------------------------------------------*/
FUNCTION BQEncode RETURNS CHARACTER (iString AS CHARACTER):
   DEFINE VARIABLE vRaw AS RAW      NO-UNDO.
   PUT-STRING(vRaw,1,LENGTH(iString)) = iString.
   RETURN STRING(vRaw).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �����頥� 㭨���쭮� ��� 䠩��, ���஥ ����� �ᯮ�짮���� ���
               �६����� 䠩���, ����� ������ ������ ���� 㭨����묨.
               ��������� ���ᬮ�७� � PSC 䠩� adecomm/_tmpfile.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FUNCTION GetUniqueFileName RETURNS CHARACTER (iPrefix    AS CHARACTER,
                                              iExtension AS CHARACTER):
   DEFINE VARIABLE vFName AS CHARACTER  NO-UNDO.
   DO WHILE YES:
      vFname = iPrefix + STRING(( TIME * 1000 + ETIME ) MODULO 100000,"99999") +
               "." + iExtension.
      IF SEARCH(vFName) = ? THEN RETURN vFName.
   END.
   RETURN vFName.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ��������� ��ப�, ������஢����� �१ BQEncode
  Parameters:  iString - ������஢����� ��ப�
               oString - �᪮��஢����� ��ப�
  Notes:       ������஢���� �믮����� � �ᯮ�짮����� INPUT/OUTPUT ���⮬�
               ��ଫ��� � ���� ��楤���. �᫨ ���� ��㣮� ᯮᮡ ॠ����樨
               ���졠 �� �⮬ ᮮ����.
------------------------------------------------------------------------------*/
PROCEDURE BQDecode:
   DEFINE INPUT  PARAMETER iString AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oString AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vRaw    AS RAW         NO-UNDO.
   DEFINE VARIABLE vFName  AS CHARACTER   NO-UNDO.
   
   /* Commented by KSV: ����塞 㭨���쭮� ��� 䠩�� */
   vFName = GetUniqueFileName("bq","tmp").
   /* Commented by KSV: ��襬 � 䠩� ������஢����� ��ப� */
   OUTPUT TO VALUE(vFName).
   PUT UNFORMATTED iString SKIP.
   OUTPUT CLOSE.
   /* Commented by KSV: ��⠥� ��ப� �� 䠩�� � raw */
   INPUT FROM VALUE(vFName).
   IMPORT vRaw.
   INPUT CLOSE.
   /* Commented by KSV: �������㥬 ��ப� */
   oString = GET-STRING(vRaw,1).
   /* Commented by KSV: ����塞 䠩� */
   OS-DELETE VALUE ( {&RELATIVE_2_ABSOLUTE}( vFName ) ).
END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     㤠��� �� ��ப� ������ ��������� ᨬ����
  Parameters:  iString - ��ப�,
               iChar   - 㤠�塞� ᨬ���
  Notes:       �㭪�� ����� �ᯮ�짮������ ��� �ଠ�஢���� ���ᮢ
------------------------------------------------------------------------------*/
FUNCTION DelDoubleChars RETURNS CHARACTER (iString AS CHARACTER, iChar AS CHARACTER):
   DEFINE VARIABLE vStr AS CHARACTER NO-UNDO.
   vStr = iString.
   IF iChar = "," THEN
      DO WHILE INDEX (vStr, ", ") > 0 :
         vStr = REPLACE (vStr, ", ", ",").
      END.
   DO WHILE INDEX (vStr, iChar + iChar) > 0 :
      vStr = REPLACE (vStr, iChar + iChar, iChar).
   END.
   vStr = TRIM (vStr, iChar).
   vStr = TRIM (vStr).
   RETURN vStr.
END FUNCTION.

/*----------------------------------------------------------------------------
   �஢���� ��ப� �� ����稥 �������⨬�� ᨬ�����. 

   iStr           �஢��塞�� ��ப�
   iParam         ���ਨ �஢�ન:
                  1 - ��ਫ��
                  2 - ��⨭��
                  3 - �����
                  4 - ��⨭�� ���孨� ॣ����
                  ���� ����������� 㪠�뢠�� ᯨ᮪: 1,2 1,2,3 
   iOthersSymbol  ���祭� ���� �����⨬�� ᨬ�����
   oResult        १���� �஢�ન
                  �� �᫨ � ��ப� ⮫쪮 �����⨬� ᨬ����
----------------------------------------------------------------------------*/
PROCEDURE Check-Ascii-Set.

   DEFINE INPUT  PARAMETER iStr          AS CHARACTER   NO-UNDO. /* �஢��塞�� ��ப� */
   DEFINE INPUT  PARAMETER iParam        AS CHARACTER   NO-UNDO. /* ���ਨ �஢�ન 1,2,3 */
   DEFINE INPUT  PARAMETER iOthersSymbol AS CHARACTER   NO-UNDO. /* ���祭� ���� �����⨬�� ᨬ����� */
   DEFINE OUTPUT PARAMETER oResult       AS LOGICAL     NO-UNDO INIT NO.

   DEFINE VARIABLE vCorrectlyStr AS CHARACTER   NO-UNDO. /* ���᮪ ����� �����⨬�� ���祭�� */
   DEFINE VARIABLE vTmpChar      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE i             AS INT64     NO-UNDO.
   DEFINE VARIABLE j             AS INT64     NO-UNDO.

   iOthersSymbol = iOthersSymbol + " ".

   /* ibm866 */
   DO j = 1 TO NUM-ENTRIES(iParam):
      CASE ENTRY(j,iParam):
         WHEN "1" THEN /* 1. �஢�ઠ ��ப� �� ��ਫ����. */
         DO: 
            DO i = 128 TO 175:
               {additem.i vCorrectlyStr STRING(i)}
            END.
            DO i = 224 TO 241:
               {additem.i vCorrectlyStr STRING(i)}
            END.
         END.
         WHEN "2" THEN /* 2. �஢�ઠ ��ப� �� ��⨭���. */
         DO:
            DO i = 65 TO 90:
               {additem.i vCorrectlyStr STRING(i)}
            END.
            DO i = 97 TO 122:
               {additem.i vCorrectlyStr STRING(i)}
            END.
         END.
         WHEN "3" THEN /* 3. �஢�ઠ �� ����. */
         DO i = 48 TO 57:
            {additem.i vCorrectlyStr STRING(i)}
         END.
         WHEN "4" THEN /* 4. ��⨭�� ���孨� ॣ���� */
         DO i = 65 TO 90:
            {additem.i vCorrectlyStr STRING(i)}
         END.
      END CASE.
   END.
   /* + ��稥 �����⨬� ᨬ���� */
   DO i = 1 TO LENGTH(iOthersSymbol):
      vTmpChar = STRING(ASC(SUBSTRING(iOthersSymbol,i,1))).
      {additem.i vCorrectlyStr vTmpChar}
   END.
   /* ���� �஢�ઠ */
   DO i = 1 TO LENGTH(iStr):
      IF CAN-DO(vCorrectlyStr,STRING(ASC(SUBSTRING(iStr,i,1)))) THEN
         NEXT.
      ELSE DO:                   /* �������⨬� ᨬ����  */
         oResult = YES.
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE Check-Cyrillic-Roman.

   DEFINE INPUT  PARAMETER iStr          AS CHARACTER   NO-UNDO. /* �஢��塞�� ��ப� */
   DEFINE OUTPUT PARAMETER oCyrillic     AS LOGICAL     NO-UNDO INIT NO.
   DEFINE OUTPUT PARAMETER oRoman        AS LOGICAL     NO-UNDO INIT NO.

   DEFINE VARIABLE vCurAscii AS INT64     NO-UNDO.

   DEFINE VARIABLE i             AS INT64     NO-UNDO.

   /* ���� �஢�ઠ */
   DO i = 1 TO LENGTH(iStr):
      vCurAscii = ASC(SUBSTRING(iStr,i,1)).
      IF    (    vCurAscii >= 128 
             AND vCurAscii <= 175)
         OR (    vCurAscii >= 224 
             AND vCurAscii <= 241) THEN
         oCyrillic = YES.
      IF    (    vCurAscii >= 65 
             AND vCurAscii <= 90) 
         OR (    vCurAscii >= 97 
             AND vCurAscii <= 122) THEN
         oRoman = YES.
      IF oCyrillic AND oRoman THEN
         LEAVE.
   END.
END PROCEDURE.

/*----------------------------------------------------------------------------
   ��⠥� � ��ப� ��।������� ������⢮ ���� �� 䠩��, 㤠��� �������⨬� 
   ᨬ���� � ������ 

   iFile          ��� 䠩��
   iSize          ������⢮ ᨬ�����
   oStr           ����������� ��ப�
----------------------------------------------------------------------------*/


PROCEDURE Read-Str-FromFile.
   DEFINE INPUT  PARAMETER iFile   AS CHARACTER   NO-UNDO. 
   DEFINE INPUT  PARAMETER iSize   AS INT64       NO-UNDO. 
   DEFINE OUTPUT PARAMETER oStr    AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vLen  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE vI    AS INT64     NO-UNDO.
   DEFINE VARIABLE vJ    AS INT64     NO-UNDO.
   DEFINE VARIABLE vStr  AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE vWord AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE vBrk  AS LONGCHAR NO-UNDO.

   vBrk = "� | � � � � � � � � � � � � � � �  � � � � � � � � � ~n ; : = ~*~  + % @ _ . , - / ~\ ! ? ( ) [ ] < > ~"~ ~'~ 0 1 2 3 4 5 6 7 8 9".
   FILE-INFORMATION:FILE-NAME = iFile.
   vLen = FILE-INFORMATION:FILE-SIZE.

   DO ON ERROR UNDO, LEAVE:
      COPY-LOB FILE iFile FOR vLen TO vStr NO-CONVERT.
   END.
   DO vI = 1 TO NUM-ENTRIES(vStr," "):
      IF ENTRY(vI,vStr," ") NE "" AND ENTRY(vI,vStr," ") NE "~n" THEN DO:
         vWord = TRIM(ENTRY(vI,vStr," "),STRING(vBrk)).
         oStr = REPLACE(oStr," " + vWord + " ", " ") + " " + vWord.      
      END.
   END.

   IF iSize < LENGTH(oStr) THEN DO:
      DO vJ = iSize TO LENGTH(oStr):
         IF INDEX(" ",SUBSTRING(oStr,vJ,1)) > 0 THEN LEAVE.
      END.
      oStr = TRIM(SUBSTRING(oStr,1,vJ)).
   END.
   ELSE
      oStr = TRIM(SUBSTRING(oStr,1,iSize)).
END PROCEDURE.
{pfuncdef
   &DEFPROC     = "CompareAsInt"
   &DESCRIPTION = "�ࠢ����� ���� ��ப ��� 楫�� �ᥫ"
   &PARAMETERS  = "iStr1, iStr2, iOp, OUTPUT oResult"
   &RESULT      = "��� ��ࠬ���, ᮤ�ঠ饣� �訡��� �����, ��� ����"
   &SAMPLE      = "CompareAsInt IN h_strng ("-11", "22", "GE", OUTPUT vResult)"}
/*------------------------------------------------------------------------------
  Purpose:     �ࠢ����� ���� ��ப ��� 楫�� �ᥫ.
  Parameters:  iStr1   - ��ࢠ� ��ப�.
               iStr2   - ���� ��ப�.
               iOp     - ������ �ࠢ���� � ࠬ��� ᨭ⠪�� Progress ABL.
               oResult - ��室��� ��ࠬ���, ��⨭����� ��ࠦ����
                         (INT64(iStr1) iOp INT64(iStr2)).
                         �� ���� �஡����� �ਢ������ �室��� ��ப
                         � ⨯� INT64 ��� 㪠����� �������⨬�� ����樨
                         �㤥� ᮤ�ঠ�� ����।��񭭮� ���祭��, ⠪��
                         � �⮬ ��砥 ��᫥ �맮�� ��楤��� �㤥�
                         ��������� RETURN-VALUE.
  Notes:       RETURN-VALUE = "iStr1" - ��ࢠ� ��ப� �� ���� 楫� �᫮�.
               RETURN-VALUE = "iStr2" - ���� ��ப� �� ���� 楫� �᫮�.
               RETURN-VALUE = "iOp"   - 㪠���� �������⨬�� ������ �ࠢ�����.
               RETURN-VALUE = ""      - � ��⠫��� �����.
------------------------------------------------------------------------------*/
PROCEDURE CompareAsInt:
   DEFINE INPUT  PARAMETER iStr1   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iStr2   AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iOp     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oResult AS LOGICAL   NO-UNDO INITIAL ?.

   DEFINE VARIABLE vRetVal AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vInt1   AS INT64     NO-UNDO.
   DEFINE VARIABLE vInt2   AS INT64     NO-UNDO.

   vInt1 = INT64(iStr1) NO-ERROR.
   IF NOT {assigned iStr1} OR ERROR-STATUS:ERROR OR vInt1 = ? THEN
      vRetVal = "iStr1".
   ELSE DO:
      vInt2 = INT64(iStr2) NO-ERROR.
      IF NOT {assigned iStr2} OR ERROR-STATUS:ERROR OR vInt2 = ? THEN
         vRetVal = "iStr2".
      ELSE
         CASE iOp:
            WHEN "="  OR WHEN "EQ" THEN oResult = (vInt1 =  vInt2).
            WHEN "<>" OR WHEN "NE" THEN oResult = (vInt1 <> vInt2).
            WHEN "<"  OR WHEN "LT" THEN oResult = (vInt1 <  vInt2).
            WHEN ">"  OR WHEN "GT" THEN oResult = (vInt1 >  vInt2).
            WHEN "<=" OR WHEN "LE" THEN oResult = (vInt1 <= vInt2).
            WHEN ">=" OR WHEN "GE" THEN oResult = (vInt1 >= vInt2).
            OTHERWISE                   vRetVal = "iOp".
         END CASE.
   END.
   IF vRetVal <> "" THEN
      RETURN vRetVal.
END PROCEDURE.
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTDATE='12/11/2014 10:21:40.871+04:00' */
/* $LINTFILE='pp-strng.p' */
/*prosignTTUFhbkf0tDGNumP+051Fg*/
/* --- pp-strng.p was humbly modified by (c)blodd converter v.1.09 on 9/19/2016 7:56am --- */
