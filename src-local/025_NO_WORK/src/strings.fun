/*-----------------------------------------------------------------------------
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: STRINGS.FUN
      Comment: �㭪樨 ࠡ��� � ��ப���
         Uses:
      Used BY:
      Created: 11/05/2001 yakv
     Modified: 16/05/2001 yakv - ��������� FStrPad � FStrCenter
     Modified: 30/05/2001 yakv - ��������� FStrPadC
     Modified: 30/05/2002 NIK  - ��������� GetEntries
     Modified: 05.09.2003 15:10 KSV      (0019702) ��������� �㭪樨
                                         GetMangledName � GetOriginalName,
                                         �����᢫��騥 ������� �८�ࠧ������
                                         ��ப �� ���᪮� �몥. ���������
                                         �㭪�� RemoveDoubleChars, 㤠�����
                                         �� ��ப� ������騥�� ᨬ����.
     Modified: 30.09.2003 19:07 KSV      (0019702) ��������� �㭪樨
                                         ��।������ ���� �� ᨬ���:
                                         IsAlpha - �㪢��, IsDigit - ��ன �
                                         IsAlphaNum - �㪢�� ��� ��ன.
     Modified: 01.11.2003 18:13 KSV      (0020852) �������� ࠡ�� �㭪権
                                         GetMangledName � GetOriginalName �
                                         ��⮬ ⮣�, �� � � ४������ �����
                                         ��������� ࠧ����⥫� '_' � '-'.
     Modified: 01.12.2003 19:04 KSV      (0020852) ��ࠢ���� ���������
                                         CheckIDName.
     Modified: 10.12.2003 14:36 KSV      (0020852) ��ࠢ���� �㭪��
                                         GetOriginalName, �⮡� ��� �筮 �뫠
                                         ���⭮� �� �⭮襭�� GetMangledName.
     Modified: 19.01.2004 20:47 KSV      (0019947) � �㭪樨 GetMangledName �
                                         GetOriginalName ��������� ��ࠡ�⪠
                                         ���祢�� ᫮� 4GL.
     Modified: 30.01.2004 18:37 KSV      (0019947) ��������� �㭪�� PairTrim,
                                         ����� ���������� ������ ���祭���
                                         墮�⮢�� � ��������� ᨬ�����.
     Modified: 31.03.2004 18:37 NIK      ��������� �㭪樨 GetNullStr
                                                           GetNullNum
                                                           GetNullDat
     Modified: 08.05.2004 19:00 KSV      (0019947) ��ࠢ���� �㭪��
                                         GetMangledName, �।�������
                                         ������஢���� ᬥ蠭���
                                         �����䨪��஢.
     Modified: 11.05.2004 09:30 KSV      (0019947) ��ࠢ���� GetMangledName.
     Modified: 12.11.2004 13:09 Om       
     Modified: 19.01.2005 09:30 NIK      �᪮७� GetMangledName.
     Modified: 11.05.2005 15:33 REVV     ��������� �㭪�� SetEntries
     Modified: 14.05.2005 18:00 ��RO     (0034340) �������� ࠡ�� �㭪権
                                         GetMangledName � GetOriginalName �
                                         ��⮬ ⮣�, �� � ४������ �����
                                         ��������� ��ਫ�� � ��⨭�� ���६����.
                                         �।��騥 �㭪樨 GetMangledName � GetOriginalName
                                         ��२�������� � GetMangledNameS � GetOriginalNameS
     Modified: 25.11.2005 18:37 NIK      ��������� �㭪�� GetNullInt
     Modified: 30.03.2010 16:39 ariz     ��������� �㭪�� ListsCrossing - ��室��
                                         ����祭�� ����⮢ ���� ᯨ᪮�
-------------------------------------------------------------------------------
���祭� �㭪権/��楤��:
    FStrEmpty
    FStrNVL
    FStrCat
    FStrPad
    FStrPadC
    FStrCenter
    GetEntries
    SetEntries
-----------------------------------------------------------------------------*/
&IF DEFINED( FILE_strings_fun ) = 0 &THEN &GLOBAL-DEFINE FILE_strings_fun

&IF DEFINED(pp-str) = 0 &THEN
   &MESSAGE ** ��������� - �맢�� STRINGS.FUN, �ᯮ���� intrface.get strng
&ENDIF
/* �ਧ��� �� ������஢���� ����� Instance � pp-data.p �ந�������� � ��⮬ ���������� ��।�� �१ XML */
DEF NEW GLOBAL SHARED VAR gInstanceXml AS LOGICAL NO-UNDO. 

DEFINE VAR mMangled AS CHAR EXTENT 255 NO-UNDO.
DEFINE TEMP-TABLE ttMangled NO-UNDO
   FIELD i_name AS CHAR
   FIELD o_name AS CHAR
INDEX i_name i_name
.

RUN PrepareMangledName.

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION GetPartStr  CHAR (INPUT iText AS CHAR,
                           INPUT iLen  AS INT64,
                           INPUT iNum  AS INT64):

   {wordwrap.def}
   DEFINE VAR vText AS CHAR      EXTENT 2 NO-UNDO.
   DEFINE VAR vCnt  AS INT64   INIT   0 NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetPartStr","iText:" + iText +
                                " iNum:" + string(iNum)).
   &ENDIF

   vText[1] = iText.
SPLIT:
   DO WHILE TRUE:
      vCnt = vCnt + 1.
      {wordwrap.i &s = vText &l = iLen &n = 2}

      &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgprint.p ("GetPartStr","vCnt:" + string(vCnt) +
                              " vText[1]:" + vText[1]     +
                              " vText[2]:" + vText[2]).
      &ENDIF

      IF vCnt EQ iNum THEN LEAVE SPLIT.
      vText[1] = vText[2].
   END.

   RETURN vText[1].
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION FStrEmpty returns LOGICAL (INPUT asValue AS CHAR).
    RETURN (asValue = "" OR asValue = ?).
END.

/*---------------------------------------------------------------------------*/
FUNCTION FStrNVL RETURN CHAR (
    INPUT  asValTrue    AS CHAR,
    INPUT  asValFalse   AS CHAR
).
/*---------------------------------------------------------------------------*/
    RETURN IF( FStrEmpty( asValTrue ) ) THEN asValFalse ELSE asValTrue.
END.

/*----------------------------------------------------------------------------*/
/*  �����⥭��� ��ப "asSrc" || "asCat" � ࠧ����⥫�� "asDelim"            */
/*----------------------------------------------------------------------------*/
FUNCTION FStrCat returns CHAR (
    INPUT-OUTPUT asSrc    AS CHAR,
    INPUT        asCat    AS CHAR,
    INPUT        asDelim  AS CHAR
).
    IF( asSrc <> "" ) THEN ASSIGN asSrc = asSrc + asDelim.
    ASSIGN asSrc = asSrc + asCat.
    RETURN asSrc.
END.

/*----------------------------------------------------------------------------*/
/*  ���������� ��ப� "asStr" �஡����� �� ����� "anLen" �ࠢ� ��� ᫥��     */
/*----------------------------------------------------------------------------*/
FUNCTION FStrPad returns CHAR (
    INPUT  asStr    AS CHAR,
    INPUT  anLen    AS INT64,
    INPUT  abRight  AS LOGICAL
).
    DEF VAR sSpaces AS CHAR INIT "" NO-UNDO.
    ASSIGN sSpaces = IF( abRight ) THEN "" ELSE FILL( " ", anLen - length( asStr ) ).
    RETURN string( sSpaces + asStr, "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/* ���������� ��ப�  ᨬ������ �� ����� "anLen" �ࠢ� ��� ᫥��             */
/*----------------------------------------------------------------------------*/
FUNCTION FStrPadC returns CHAR (
    INPUT  asStr    AS CHAR,
    INPUT  anLen    AS INT64,
    INPUT  abRight  AS LOGICAL,
    INPUT  asFill   AS CHAR
).
    DEF VAR sFills AS CHAR INIT "" NO-UNDO.
    ASSIGN sFills = FILL( asFill, anLen - length( asStr ) ).
    RETURN string( IF( abRight ) THEN (asStr + sFills) ELSE (sFills + asStr), "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/*      �����஢���� ��ப� "asStr" �� �ਭ� "anLen" ᨬ�����               */
/*----------------------------------------------------------------------------*/
FUNCTION FStrCenter returns CHAR (INPUT asStr AS CHAR, INPUT anLen AS INT64).
    DEF VAR nSpaces AS INT64  NO-UNDO.
    ASSIGN nSpaces = ( anLen - length( asStr ) ) / 2.
    RETURN string( FILL( " ", nSpaces ) + asStr, "x(" + string( anLen ) + ")" ).
END.

/*----------------------------------------------------------------------------*/
/*          �������� ����祭�� ����� ᯨ᪠                                */
/*----------------------------------------------------------------------------*/
FUNCTION GetEntries RETURN CHAR (INPUT ipItem      AS INT64,
                                 INPUT ipText      AS CHAR,
                                 INPUT ipSplit     AS CHAR,
                                 INPUT ipDefault   AS CHAR):
   IF NUM-ENTRIES(ipText,ipSplit) GE ipItem
      THEN RETURN ENTRY(ipItem,ipText,ipSplit).
      ELSE RETURN ipDefault.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*          �������� ��᢮���� ���祭�� ��� ����� ᯨ᪠                  */
/*----------------------------------------------------------------------------*/
FUNCTION SetEntries RETURN CHAR (INPUT ipItem      AS INT64, /* ������     */
                                 INPUT ipText      AS CHAR,    /* ᯨ᮪      */
                                 INPUT ipSplit     AS CHAR,    /* ࠧ����⥫� */
                                 INPUT ipValue     AS CHAR):   /* ���祭��    */
   DEFINE VAR vI AS INT64 NO-UNDO.

   DO vI = NUM-ENTRIES(ipText,ipSplit) TO ipItem - 1:
      ipText =  ipText + ipSplit.
   END.
   ENTRY(ipItem,ipText,ipSplit) = ipValue.

   RETURN ipText.

END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION PADR RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   RETURN TRIM(SUBSTRING(ipText,1,ipLen)) +
          FILL(" ",max(ipLen - length(ipText),0)).
END FUNCTION.

FUNCTION PADL RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   RETURN FILL(" ",max(ipLen - length(ipText),0)) +
          TRIM(SUBSTRING(ipText,1,ipLen)).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                �������� ��ப�                                           */
/*----------------------------------------------------------------------------*/
FUNCTION PADC RETURN CHAR (INPUT ipText AS CHAR,
                           INPUT ipLen  AS INT64):
   DEFINE VAR vLen  AS INT64 NO-UNDO.
   DEFINE VAR vLeft AS INT64 NO-UNDO.
   vLen  = length(TRIM(ipText)).
   vLeft = max(0,truncate((ipLen - vLen) / 2,0)).
   RETURN FILL(" ",vLeft)                                +
          TRIM(SUBSTRING(ipText,1,ipLen))                +
          FILL(" ",max(ipLen - length(ipText) - vLeft,0)).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* �����頥� ��ப� � ��⮬ ���⮣� ���祭��                                */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullStr CHAR (INPUT iVal AS CHAR):
   RETURN IF iVal EQ ? THEN "?" ELSE iVal.
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* �����頥� �᫮ (��� ��ப�) � ��⮬ ���⮣� ���祭��                    */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullNum CHAR (INPUT iVal AS DECIMAL):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/* �����頥� �᫮ (��� ��ப�) � ��⮬ ���⮣� ���祭��                    */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullInt CHAR (INPUT iVal AS INT64):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.

/*----------------------------------------------------------------------------*/
/* �����頥� ���� (��� ��ப�) � ��⮬ ���⮣� ���祭��                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetNullDat CHAR (INPUT iVal AS DATE):
   RETURN IF iVal EQ ? THEN "?" ELSE string(iVal).
END FUNCTION.
/*----------------------------------------------------------------------------*/
/*                �뢮� ���ଠ樨 �� �࠭                                   */
/*----------------------------------------------------------------------------*/
PROCEDURE PutScreen:
   DEFINE INPUT PARAMETER ipCol  AS INT64  NO-UNDO.
   DEFINE INPUT PARAMETER ipText AS CHAR     NO-UNDO.

   PUT screen ROW screen-lines + 1 COL ipCol ipText.

END PROCEDURE.

/*******************************************************************************
 * 4.1D SECTION                                                              * *
 ******************************************************************************/
/*------------------------------------------------------------------------------
  Purpose:     ������� � ��ப� ��������� ᨬ���� �� �������.
  Parameters:  iStr  - ��ப�
               iChr  - ��������� ᨬ���
  Notes:       �������, ���ਬ��, ��� 㤠����� ������ ����⮢ ᯨ᪠ ��᫥
               �믮������ ������ ENTRY(<list>) = "", ���ਬ��,
               a,,,b, -> a,b
------------------------------------------------------------------------------*/
FUNCTION RemoveDoubleChars RETURNS CHARACTER (iStr AS CHARACTER,
                                              iChr AS CHARACTER):
   DEFINE VARIABLE vStr AS CHARACTER  NO-UNDO.
   vStr = REPLACE(iStr,iChr + iChr,iChr).
   DO WHILE iStr <> vStr:
      iStr = vStr.
      vStr = REPLACE(iStr,iChr + iChr,iChr).
   END.
   IF iStr = iChr THEN iStr = "".
   RETURN TRIM(iStr,iChr).
END FUNCTION.


/*------------------------------------------------------------------------------
  Purpose:     ��।��� ���� �� ᨬ��� �㪢��
  Parameters:  iChr  - ᨬ���
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsAlpha RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN (ASC(iChr) >= ASC("A") AND ASC(iChr) <= ASC("Z")) OR
          (ASC(iChr) >= ASC("a") AND ASC(iChr) <= ASC("z")) OR
          (ASC(iChr) >= ASC("�") AND ASC(iChr) <= ASC("�")) OR
          (ASC(iChr) >= ASC("�") AND ASC(iChr) <= ASC("�")) OR
          (ASC(iChr) >= ASC("�") AND ASC(iChr) <= ASC("�")) OR
           ASC(iChr)  = ASC("�") OR  ASC(iChr)  = ASC("�").

END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ��।��� ���� �� ᨬ��� ��ன
  Parameters:  iChr  - ᨬ���
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsDigit RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN ASC(iChr) >= ASC("0") AND ASC(iChr) <= ASC("9").
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ��।��� ���� �� ᨬ��� �㪢�� ��� ��ன
  Parameters:  iChr  - ᨬ���
  Notes:
------------------------------------------------------------------------------*/
FUNCTION IsAlphaNum RETURNS LOGICAL (iChr AS CHARACTER):
   RETURN IsAlpha(iChr) OR IsDigit(iChr).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �஢���� �ࠢ��쭮��� ����� ��ꥪ�
  Parameters:  pName - ��� ��ꥪ� (Class,Xattr,Setting,Op-Kind)
  Notes:
------------------------------------------------------------------------------*/
FUNCTION CheckIdName RETURN LOGICAL (pName AS CHAR):
   DEFINE VARIABLE vRes AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vErrMess AS CHARACTER NO-UNDO.
   DEFINE VARIABLE IsOnlyLat AS LOG NO-UNDO.
   DEFINE VARIABLE IsOnlyRus AS LOG NO-UNDO.

   IsOnlyLat = DYNAMIC-FUNCTION("ereg":U,"^[0-9_~\-A-Za-z]+$",
                    pName,
                    OUTPUT vRes,
                    INPUT-OUTPUT vErrMess) EQ YES.
   IsOnlyRus = DYNAMIC-FUNCTION("ereg":U,"^[0-9_~\-�-��-�]+$",
                    pName,
                    OUTPUT vRes,
                    INPUT-OUTPUT vErrMess) EQ YES.
   IF pName EQ "" OR pName EQ "?" THEN
      RETURN NO.
   IF NOT IsAlpha(SUBSTR(pName,1,1)) OR
      (NOT IsOnlyLat AND NOT IsOnlyRus) THEN
      RETURN NO.
   RETURN YES.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ������ ����୮ �������騥 � 墮�⮢� ᨬ���� �� ��ப�.
  Parameters:  iStr   - ��ப�
               iChrs  - ᨬ���� ��� 㤠�����.
  Notes:       ���ਬ��: PairTrim("'xyz''") = "xyz'". �᭮���� �ᯮ�짮�����
               � dopars.p.
------------------------------------------------------------------------------*/
FUNCTION PairTrim RETURNS CHARACTER (iStr    AS CHARACTER,
                                     iChrs   AS CHARACTER):
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vTmp AS CHARACTER  NO-UNDO.

   vLen = LENGTH(iChrs).

   DO WHILE YES:
      vTmp = iStr.
      DO vCnt = 1 TO vLen:
         vChr = SUBSTR(iChrs,vCnt,1).
         /* Commented BY KSV: �᫨ ������⢮ ᨬ����� � ��ப� ����� 2-� �
         ** ���� ᨬ��� ࠢ�� ��᫥�����, � �ᥪ��� ��ப� */
         IF LENGTH(iStr) >= 2  AND
            iStr BEGINS  vChr AND
            iStr MATCHES "*" + vChr THEN iStr = SUBSTR(iStr,2,LENGTH(iStr) - 2).
      END.
      /* Commented BY KSV: ������ ���� ��ப� �ᥪ����� */
      IF vTmp = iStr THEN RETURN iStr.
   END.
END FUNCTION.



&GLOBAL-DEFINE MANGLED_CHR "$"

PROCEDURE PrepareMangledName:

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PrepareMangledName","START").
   &ENDIF

   ASSIGN
      mMangled[asc("�")] = "a"
      mMangled[asc("�")] = "a"
      mMangled[asc("�")] = "b"
      mMangled[asc("�")] = "b"
      mMangled[asc("�")] = "v"
      mMangled[asc("�")] = "v"
      mMangled[asc("�")] = "g"
      mMangled[asc("�")] = "g"
      mMangled[asc("�")] = "d"
      mMangled[asc("�")] = "d"
      mMangled[asc("�")] = "e"
      mMangled[asc("�")] = "e"
      mMangled[asc("�")] = "wo"
      mMangled[asc("�")] = "wo"
      mMangled[asc("�")] = "wz"
      mMangled[asc("�")] = "wz"
      mMangled[asc("�")] = "z"
      mMangled[asc("�")] = "z"
      mMangled[asc("�")] = "i"
      mMangled[asc("�")] = "i"
      mMangled[asc("�")] = "wi"
      mMangled[asc("�")] = "wi"
      mMangled[asc("�")] = "k"
      mMangled[asc("�")] = "k"
      mMangled[asc("�")] = "l"
      mMangled[asc("�")] = "l"
      mMangled[asc("�")] = "m"
      mMangled[asc("�")] = "m"
      mMangled[asc("�")] = "n"
      mMangled[asc("�")] = "n"
      mMangled[asc("�")] = "o"
      mMangled[asc("�")] = "o"
      mMangled[asc("�")] = "p"
      mMangled[asc("�")] = "p"
      mMangled[asc("�")] = "r"
      mMangled[asc("�")] = "r"
      mMangled[asc("�")] = "s"
      mMangled[asc("�")] = "s"
      mMangled[asc("�")] = "t"
      mMangled[asc("�")] = "t"
      mMangled[asc("�")] = "u"
      mMangled[asc("�")] = "u"
      mMangled[asc("�")] = "f"
      mMangled[asc("�")] = "f"
      mMangled[asc("�")] = "h"
      mMangled[asc("�")] = "h"
      mMangled[asc("�")] = "c"
      mMangled[asc("�")] = "c"
      mMangled[asc("�")] = "wc"
      mMangled[asc("�")] = "wc"
      mMangled[asc("�")] = "ws"
      mMangled[asc("�")] = "ws"
      mMangled[asc("�")] = "wt"
      mMangled[asc("�")] = "wt"
      mMangled[asc("�")] = "w%"
      mMangled[asc("�")] = "w%"
      mMangled[asc("�")] = "w#"
      mMangled[asc("�")] = "w#"
      mMangled[asc("�")] = "y"
      mMangled[asc("�")] = "y"
      mMangled[asc("�")] = "we"
      mMangled[asc("�")] = "we"
      mMangled[asc("�")] = "wu"
      mMangled[asc("�")] = "wu"
      mMangled[asc("�")] = "wa"
      mMangled[asc("�")] = "wa"
   .

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("PrepareMangledName","FINIS").
   &ENDIF
END PROCEDURE.

/*------------------------------------------------------------------------------
  Purpose:     �㭪�� �८�ࠧ��뢠�� ��ப�,ᮤ�ঠ��� ��ਫ��� � ��⨭���.
               � ����� ���������� ��ப�  ���������� ᨬ��� $.
               ���ਬ��, ��� -> ABV$. ����� �㭪�� �ᯮ����� ���祢� ᫮��
               � �������� � ��ப� ��� ᨬ���� $.
  Parameters:  iStr - ��ப� ��� �८�ࠧ������
  Notes:       ���ਬ��, ���� �६����� ⠡���� �� ����� ᮤ�ঠ�� ��ਫ����
               � �����䪠��.
------------------------------------------------------------------------------*/
FUNCTION GetMangledNameS RETURNS CHARACTER (iStr AS CHAR):
   DEFINE VARIABLE vCnt AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen AS INT64    NO-UNDO.
   DEFINE VARIABLE vDst AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMng AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vCn2 AS INT64    NO-UNDO.

   vLen = LENGTH(iStr).

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","iStr:" + GetNullStr(iStr)).
   &ENDIF

   DO vCnt = 1 TO vLen:

      vChr = SUBSTR(iStr,vCnt,1).
      vMng = mMangled[asc(vChr)].

      IF NOT {assigned vMng} THEN DO:            /* �� ���᪠� �㪢�          */
         vMng = vChr.

         /* Commented BY KSV: �᫨ ��ࢠ� �㪢� �� ���᪠�, � �� �믮��塞
         ** ������஢����                                                 */
         IF vCnt = 1 THEN DO:
            /* Commented BY KSV: �᫨ � ����⢥ ��ப� ��।��� ���祢��
            ** ᫮��, �������㥬 ��� ������ ᨬ����� ������஢���� */
            IF KEYWORD(iStr) = ? THEN DO:
               /* �஢�ઠ �� ������ �㪢�, ��稭�� � ��ன  */
               IF vLen > 1 THEN
               DO vCn2 = 2 TO vLen:
                  vChr = SUBSTR(iStr,vCn2,1).
                  vMng = mMangled[asc(vChr)].
                  IF {assigned vMng} THEN
                     RETURN ?.
               END.
               RETURN iStr.
            END.
            ELSE
               RETURN iStr + {&MANGLED_CHR} + {&MANGLED_CHR}.
         END.
         ELSE DO:
            IF IsAlpha(vMng) THEN RETURN ?.
         END.
      END.

      vDst = vDst + vMng.
   END.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","vDst:" + GetNullStr(vDst)).
   &ENDIF

   RETURN vDst + {&MANGLED_CHR}.
END FUNCTION.


/*------------------------------------------------------------------------------
  Purpose:     �����頥� �ਣ������� ��ப� �� "���������" ��ப�. �믮����
               ���⭮� �८�ࠧ������ ���祭�� �㭪樨 GetMangledName.
  Parameters:  iMangledStr - "���������" ��ப�.
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetOriginalNameS RETURNS CHARACTER (iMangledStr AS CHAR):
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDst AS CHARACTER  NO-UNDO.

   IF NOT iMangledStr MATCHES "*" + {&MANGLED_CHR} THEN
      RETURN iMangledStr.

   /* Commented BY KSV: �᫨ ��ப� �����蠥��� ������ ᨬ����� ������஢����,
   ** � �� ���祢�� ᫮��, ���������㥬 ���, �⪨�뢠� ���� ᨬ���
   ** ������஢���� */
   IF iMangledStr MATCHES "*" + {&MANGLED_CHR} + {&MANGLED_CHR} THEN
      RETURN TRIM(iMangledStr,{&MANGLED_CHR}).

   vLen = LENGTH(iMangledStr) - 1.
   DO vCnt = 1 TO vLen:
      vChr = SUBSTR(iMangledStr,vCnt,1).
      IF vChr = "w" THEN
      DO:
         vChr = vChr + SUBSTR(iMangledStr,vCnt + 1,1).
         vCnt = vCnt + 1.
      END.
      CASE vChr:
         WHEN "a"   THEN vChr = "�".
         WHEN "b"   THEN vChr = "�".
         WHEN "v"   THEN vChr = "�".
         WHEN "g"   THEN vChr = "�".
         WHEN "d"   THEN vChr = "�".
         WHEN "e"   THEN vChr = "�".
         WHEN "wo"  THEN vChr = "�".
         WHEN "wz"  THEN vChr = "�".
         WHEN "z"   THEN vChr = "�".
         WHEN "i"   THEN vChr = "�".
         WHEN "wi"  THEN vChr = "�".
         WHEN "k"   THEN vChr = "�".
         WHEN "l"   THEN vChr = "�".
         WHEN "m"   THEN vChr = "�".
         WHEN "n"   THEN vChr = "�".
         WHEN "o"   THEN vChr = "�".
         WHEN "p"   THEN vChr = "�".
         WHEN "r"   THEN vChr = "�".
         WHEN "s"   THEN vChr = "�".
         WHEN "t"   THEN vChr = "�".
         WHEN "u"   THEN vChr = "�".
         WHEN "f"   THEN vChr = "�".
         WHEN "h"   THEN vChr = "�".
         WHEN "c"   THEN vChr = "�".
         WHEN "wc"  THEN vChr = "�".
         WHEN "ws"  THEN vChr = "�".
         WHEN "wt"  THEN vChr = "�".
         WHEN "w%"  THEN vChr = "�".
         WHEN "w#"  THEN vChr = "�".
         WHEN "y"   THEN vChr = "�".
         WHEN "we"  THEN vChr = "�".
         WHEN "wu"  THEN vChr = "�".
         WHEN "wa"  THEN vChr = "�".
      END CASE.
      vDst = vDst + vChr.
   END.
   RETURN vDst.
END FUNCTION.

FUNCTION GetNameForXml RETURN CHARACTER
   (INPUT iFieldName AS CHAR):

   IF NOT gInstanceXml THEN RETURN iFieldName.

   IF SUBSTR(iFieldName,1,1) >= "0" AND
      SUBSTR(iFieldName,1,1) <= "9" THEN
      iFieldName = "_dig_" + iFieldName.
   /* [ - _quadr_ ] - _backquadr_ */
   iFieldName = REPLACE(REPLACE(iFieldName,"[","_quadr_"),"]","_backquadr_").
   /* #  - _reschetka_ */
   iFieldName = REPLACE(iFieldName,"#","_reschetka_").
   /* $  - _rusmangl_ */
   iFieldName = REPLACE(iFieldName,"$","_rusmangl_").
   /* %  - _perc_ */
   iFieldName = REPLACE(iFieldName,"%","_perc_").

   RETURN iFieldName.
END FUNCTION.

FUNCTION GetNameForProgress RETURN CHARACTER
   (INPUT iFieldName AS CHAR):

   IF NOT gInstanceXml THEN RETURN iFieldName.

   iFieldName = REPLACE(iFieldName,"_dig_","").
   /* [ - _quadr_ ] - _backquadr_ */
   iFieldName = REPLACE(REPLACE(iFieldName,"_quadr_","["),"_backquadr_","]").
   /* #  - _reschetka_ */
   iFieldName = REPLACE(iFieldName,"_reschetka_","#").
   /* $  - _rusmangl_ */
   iFieldName = REPLACE(iFieldName,"_rusmangl_","$").
   /* %  - _perc_ */
   iFieldName = REPLACE(iFieldName,"_perc_","%").

   RETURN iFieldName.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �㭪�� �८�ࠧ��뢠�� ��ப�,ᮤ�ঠ��� ��ਫ��� � ��⨭���.
               � ����� ���������� ��ப�  ���������� ᨬ��� $.
               ���ਬ��, ��� -> ABV$. ����� �㭪�� �ᯮ����� ���祢� ᫮��
               � �������� � ��ப� ��� ᨬ���� $.
               �᫨ ��ਫ��� � ��⨭�� ���� ���६����, ᨬ��� $ ����������
               � ���� ��㯯� ᨬ���� ��ਫ��� ��� ��⨭���.
               ���ਬ��, ���RT�� -> ABV$RTAB$. �᫨ ��ப� �����稢�����
               ��⨭�楩 ᨬ��� �� $ ����������.
  Parameters:  iStr - ��ப� ��� �८�ࠧ������
  Notes:       ���ਬ��, ���� �६����� ⠡���� �� ����� ᮤ�ঠ�� ��ਫ����
               � �����䪠��.
------------------------------------------------------------------------------*/
FUNCTION GetMangledName RETURNS CHARACTER (iStr AS CHAR):
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vDst  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vMng  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vRus  AS LOGICAL    NO-UNDO. /* �ਧ��� ��ਫ��� */
   DEFINE VARIABLE vMngl AS CHARACTER  NO-UNDO.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","iStr:" + GetNullStr(iStr)).
   &ENDIF
   FIND FIRST ttMangled WHERE 
              ttMangled.i_name EQ iStr 
      NO-LOCK NO-ERROR.
   IF AVAILABLE ttMangled THEN RETURN ttMangled.o_name.
   
   /* �맮� ����ன �㭪樨 ������஢����
   ** � ����設�⢥ ��砥� � ��ப� ��� ᬥ� ��⨭��� � ��ਫ��� */
   vMngl = GetMangledNameS (iStr).

   /* �᫨ ��ப� �⬠����஢��� ��� �訡��, � ������ */
   IF vMngl NE ? THEN
   DO:
      CREATE ttMangled.
      ASSIGN
         ttMangled.o_name = GetNameForXml(vMngl)
         ttMangled.i_name = iStr
      .
      RETURN ttMangled.o_name.
   END.

   /* �� �室��� ��ப� ᬥ�� ��⨭��� � ��ਫ���
   ** �믮���� ᮮ⢥�����饥 �८�ࠧ������ */
   vLen = LENGTH(iStr).

   /* Commented BY KSV: �᫨ � ����⢥ ��ப� ��।��� ���祢��
   ** ᫮��, �������㥬 ��� ������ ᨬ����� ������஢���� */
   IF KEYWORD(iStr) <> ? THEN
   DO:
      CREATE ttMangled.
      ASSIGN
         ttMangled.o_name = GetNameForXml(iStr + {&MANGLED_CHR} + {&MANGLED_CHR})
         ttMangled.i_name = iStr
      .
      RETURN ttMangled.o_name.
   END.

   DO vCnt = 1 TO vLen:

      vChr = SUBSTR(iStr,vCnt,1).
      /* �᫨ ᨬ��� �� ���� �㪢��,
      ** ���� ������塞 ��� � ��室��� ��ப� */
      IF NOT IsAlpha(vChr) THEN DO:
         vDst = vDst + vChr.
         NEXT.
      END.

      vMng = mMangled[asc(vChr)].

      /* ������塞 ᨬ����� ������஢���� � ����� ��㯯�
      ** ��ਫ��� ��� ��⨭��� */
      IF NOT {assigned vMng} THEN DO:   /* �� ��ਫ�� */
         vMng = vChr.
         IF vCnt > 1 AND
            vRus     THEN
            vMng = {&MANGLED_CHR} + vMng.
         vRus = NO.
      END.
      ELSE DO:
         IF vCnt > 1 AND NOT
            vRus     THEN
            vMng = {&MANGLED_CHR} + vMng.
         vRus = YES.
      END.

      vDst = vDst + vMng.
   END.

   /* �������㥬 ��ப� ����稢������� ��ਫ�楩 */
   IF vRus THEN
      vDst = vDst + {&MANGLED_CHR}.

   &IF DEFINED(IS-DEBUG) &THEN
   RUN dbgprint.p ("GetMangledName","vDst:" + GetNullStr(vDst)).
   &ENDIF

   CREATE ttMangled.
   ASSIGN
      ttMangled.o_name = GetNameForXml(vDst)
      ttMangled.i_name = iStr
   .
   RETURN ttMangled.o_name.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �����頥� �ਣ������� ��ப� �� "���������" ��ப�. �믮����
               ���⭮� �८�ࠧ������ ���祭�� �㭪樨 GetMangledName.
  Parameters:  iMangledStr - "���������" ��ப�.
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetOriginalName RETURNS CHARACTER (iMangledStr AS CHAR):
   DEFINE VARIABLE vLen  AS INT64    NO-UNDO.
   DEFINE VARIABLE vCnt  AS INT64    NO-UNDO.
   DEFINE VARIABLE vChr  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vDst  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vRus  AS LOGICAL    NO-UNDO. /* �ਧ��� ��ਫ��� */
   DEFINE VARIABLE vMngl AS CHARACTER  NO-UNDO.

   iMangledStr = GetNameForProgress(iMangledStr).
   IF NOT iMangledStr MATCHES "*" + {&MANGLED_CHR} + "*" THEN
      RETURN iMangledStr.

   /* Commented BY KSV: �᫨ ��ப� �����蠥��� ������ ᨬ����� ������஢����,
   ** � �� ���祢�� ᫮��, ���������㥬 ���, �⪨�뢠� ������� ᨬ���
   ** ������஢���� */
   IF iMangledStr MATCHES "*" + {&MANGLED_CHR} + {&MANGLED_CHR} THEN
      RETURN TRIM(iMangledStr,{&MANGLED_CHR}).

   /* �맮� ����ன �㭪樨 ��������஢����
   ** � ����設�⢥ ��砥� � ��ப� ��� ᬥ� ��⨭��� � ��ਫ��� */
   vMngl = GetOriginalNameS (iMangledStr).

   /* �஢�ਬ ��������஢���� ��ப�
   ** � ��� �� ������ ������� ᨬ����� ������஢���� */
   IF NOT vMngl MATCHES "*" + {&MANGLED_CHR} + "*" THEN
      RETURN vMngl.

   /* �� �室��� ��ப� ᬥ�� ��⨭��� � ��ਫ���
   ** �믮���� ᮮ⢥�����饥 �८�ࠧ������ */
   vLen = LENGTH(iMangledStr).
   /* ��ॡ�� �室��� ��ப� �ࠢ� ������ */
   DO vCnt = vLen TO 1 BY -1:
      vChr = SUBSTR(iMangledStr,vCnt,1).

      /* ����뢠�� ᨬ��� ������஢���� */
      IF vChr = {&MANGLED_CHR} THEN
      DO:
         vRus = NOT vRus.
         NEXT.
      END.

      /* �᫨ ᨬ��� �� ��ਫ��
      ** ���� ������塞 ��� � ��室��� ��ப� */
      IF NOT vRus          OR
         NOT IsAlpha(vChr) THEN DO:
         vDst = vChr + vDst.
         NEXT.
      END.
      /* �८�ࠧ������ ��ਫ��� */
      IF vCnt                           >  1  AND
         SUBSTR(iMangledStr,vCnt - 1,1) = "w" THEN
      DO:
         vChr = SUBSTR(iMangledStr,vCnt - 1,1) + vChr.
         vCnt = vCnt - 1.
      END.
      CASE vChr:
         WHEN "a"   THEN vChr = "�".
         WHEN "b"   THEN vChr = "�".
         WHEN "v"   THEN vChr = "�".
         WHEN "g"   THEN vChr = "�".
         WHEN "d"   THEN vChr = "�".
         WHEN "e"   THEN vChr = "�".
         WHEN "wo"  THEN vChr = "�".
         WHEN "wz"  THEN vChr = "�".
         WHEN "z"   THEN vChr = "�".
         WHEN "i"   THEN vChr = "�".
         WHEN "wi"  THEN vChr = "�".
         WHEN "k"   THEN vChr = "�".
         WHEN "l"   THEN vChr = "�".
         WHEN "m"   THEN vChr = "�".
         WHEN "n"   THEN vChr = "�".
         WHEN "o"   THEN vChr = "�".
         WHEN "p"   THEN vChr = "�".
         WHEN "r"   THEN vChr = "�".
         WHEN "s"   THEN vChr = "�".
         WHEN "t"   THEN vChr = "�".
         WHEN "u"   THEN vChr = "�".
         WHEN "f"   THEN vChr = "�".
         WHEN "h"   THEN vChr = "�".
         WHEN "c"   THEN vChr = "�".
         WHEN "wc"  THEN vChr = "�".
         WHEN "ws"  THEN vChr = "�".
         WHEN "wt"  THEN vChr = "�".
         WHEN "w%"  THEN vChr = "�".
         WHEN "w#"  THEN vChr = "�".
         WHEN "y"   THEN vChr = "�".
         WHEN "we"  THEN vChr = "�".
         WHEN "wu"  THEN vChr = "�".
         WHEN "wa"  THEN vChr = "�".
      END CASE.
      vDst = vChr + vDst.
   END.
   RETURN vDst.
END FUNCTION.

/* �� ������ ����� � ᯨ᪥ ��।���� ������
** � ���ன ���᭥��� ����� �����.*/
FUNCTION GetPosEntry RETURN INT64 (
   INPUT iStr   AS CHAR, /* ��ப�  */
   INPUT iEntry AS INT64,  /* �������, ������ ���ண� ����室��� ��।�����. */
   INPUT iDel   AS CHAR  /* �������⥫� ����⮢ � ��ப�. */
):
   DEF VAR vCnt AS INT64 NO-UNDO. /* ���稪. */
   DEF VAR vPos AS INT64 NO-UNDO. /* ������ � ���ன ��稭����� ����� � ᯨ᪥. */

   DO vCnt = 1 TO NUM-ENTRIES (iStr, iDel):
      IF vCnt EQ iEntry
         THEN RETURN vPos + 1.
      vPos = INDEX (iStr, iDel, vPos + 1).
   END.
   RETURN 0.
END FUNCTION.

&ENDIF

/* ��室�� ����祭�� ���� ᯨ᪮� */
FUNCTION ListsCrossing RETURN CHAR (
   INPUT iList1 AS CHARACTER,
   INPUT iList2 AS CHARACTER,
   INPUT iDelim AS CHARACTER
):
   DEF VAR vi        AS INT64    NO-UNDO.
   DEF VAR vResult   AS CHAR   NO-UNDO.
   DEF VAR vEntryVal AS CHAR   NO-UNDO.
   DEF VAR vTmpList AS CHAR   NO-UNDO.

   IF iDelim EQ ? THEN iDelim = ",".   /* ࠧ����⥫� ��-㬮�砭�� - "," */

         /* ��室�� ����� ���⪨� ᯨ᮪ */
   IF NUM-ENTRIES(iList2,iDelim) LT NUM-ENTRIES(iList1,iDelim) THEN
   ASSIGN
      vTmpList = iList1
      iList1   = iList2
      iList2   = vTmpList
   .
         /* ��ॡ�ࠥ� ����� ���⪨� ᯨ᮪ */
   DO vi = 1 TO NUM-ENTRIES(iList1,iDelim):
      vEntryVal = ENTRY(vi,iList1,iDelim).
            /* ��室�� �宦����� ����� ��ࢮ�� ᯨ᪠ �� ��ன */
      IF LOOKUP(vEntryVal,iList2) GT 0
            /* �᫨ �宦����� �������, �������� ����� � १������騩 ���᮪ */
      THEN {additem.i vResult vEntryVal}
   END.
   RETURN vResult.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ����祭�� ��������� ��������� �� ��ப�, ��室 �� �� �।���
               � �����-���� ��஭� �������� १���� ᯥ樠��� ᨬ�����.
  Parameters:  iValue   - ��室��� ��ப�.
               iStart   - ��砫� ���������, ����।��񭭮� ���祭�� ᮮ⢥����-
                          �� ��砫� ��ப�.
               iEnd     - ����� ���������, ����।��񭭮� ���祭�� ᮮ⢥�����
                          ����� ��ப�.
               iDefault - ᨬ���, ����� ���������� १���� �� ��室� 
                          ����襭���� ��������� �� �࠭��� ��ப�
  Notes:       �ਬ���:
               GetValueRange("abcdefgh", 3, 7, "*") = "cdefg"
               GetValueRange("abcdefgh", -4, 5, "*") = "*****abcde"
               GetValueRange("abcdefgh", ?, 10, "*") = "abcdefgh**"
               GetValueRange("abcdefgh", 70, 73, "*") = "****"
------------------------------------------------------------------------------*/
FUNCTION GetValueRange RETURN CHARACTER (INPUT iValue   AS CHARACTER,
                                         INPUT iStart   AS INT64,
                                         INPUT iEnd     AS INT64,
                                         INPUT iDefault AS CHARACTER):
   DEFINE VARIABLE vN AS INT64 NO-UNDO.
   DEFINE VARIABLE vM AS INT64 NO-UNDO.
   DEFINE VARIABLE vL AS INT64 NO-UNDO.
   DEFINE VARIABLE vR AS INT64 NO-UNDO.

   IF iValue = ? THEN
      RETURN ?.
   ASSIGN
      vN = LENGTH(iValue)
      vM = LENGTH(iDefault)
   .
   ASSIGN
      iStart = 1  WHEN iStart = ?
      iEnd   = vN WHEN iEnd   = ?
   .
   IF iStart > iEnd THEN
      RETURN ?.
   IF (iStart < 1 OR iEnd > vN) AND (iDefault = ? OR vM > 1) THEN
      RETURN ?.
   IF iStart > vN OR iEnd < 1 THEN
      RETURN FILL(iDefault, 1 + ABSOLUTE(iEnd - iStart)).
   ASSIGN
      vL = 1 - iStart WHEN iStart < 1
      vR = iEnd - vN  WHEN iEnd > vN
   .
   RETURN FILL(iDefault, vL) +
          SUBSTRING(iValue,
                    MAXIMUM(1, iStart),
                    MINIMUM(vN, iEnd - iStart + (vR - vL) * vM + 1)) +
          FILL(iDefault, vR).
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     ���⠪��᪨� ࠧ��� ��ࠦ����, ����뢠�饣� 楫��᫥���
               ���ࢠ�. ��ࠦ���� ������ ����� ���
               "SXSaS[ZSbS]YS",
               ��� S - �� ������⢮ ᨬ�����-ࠧ����⥫��, a � b - 楫�
               �᫠, X � Y - ���뢠����� � ����뢠����� ᪮��� ᮮ�., 
               Z - ࠧ����⥫� ���������, ���� � �������� ᪮���� �����
               ������⢮����.
  Parameters:  iStrRange - ��ࠦ����.
               iABC      - ��ப� �� ���� ᨢ����, 1-� � 2-� - ���뢠���� �
                           ����뢠����� ᪮��� ᮮ⢥��⢥���, 3-� - ࠧ��-
                           ��⥫� ���������.
               oStart    - ��砫� ���������, �� �訡��� ࠧ��� ��ࠦ���� 
                           �㤥� �����饭� ����।��񭭮� ���祭��.
               oEnd      - ����� ���������, ����।��񭭮� ���祭�� ����砥�, 
                           �� ����� ��������� �� ����� (�� �訡��).
  Notes:       ��������� ��� "᪮���" �� �����ন������.
               �������⥫� �� ������ ᮢ������ � "-" � �� �����, ����� 
               ��砫� ��� ����� ����� ���� ����⥫�묨.
------------------------------------------------------------------------------*/
PROCEDURE ParseIntRange.
   DEFINE INPUT  PARAMETER iStrRange AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iABC      AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oStart    AS INT64     NO-UNDO INITIAL -1.
   DEFINE OUTPUT PARAMETER oEnd      AS INT64     NO-UNDO INITIAL -1.

   DEFINE VARIABLE vStart AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vEnd   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSep   AS CHARACTER NO-UNDO.

   IF iStrRange                        =  ? OR
      LENGTH(iABC)                     <> 3 OR
      LENGTH(TRIM(iABC, "0123456789")) <  3
   THEN
      RETURN.
   ASSIGN
      vStart = SUBSTRING(iABC, 1, 1)
      vEnd   = SUBSTRING(iABC, 2, 1)
      vSep   = SUBSTRING(iABC, 3, 1)
   .
   IF vStart = vEnd OR
      vStart = vSep OR
      vEnd   = vSep
   THEN
      RETURN.
   iStrRange = TRIM(iStrRange).
   IF INDEX(iStrRange, vStart) = 1 AND
      INDEX(iStrRange, vEnd)   = LENGTH(iStrRange)
   THEN DO:
      iStrRange = SUBSTRING(iStrRange, 2, LENGTH(iStrRange) - 2).
      IF NUM-ENTRIES(iStrRange, vSep) < 3 THEN DO:
         ASSIGN
            vStart = ?
            vEnd   = ?
         .
         ASSIGN
            vStart = ENTRY(1, iStrRange, vSep)
            vEnd   = ENTRY(2, iStrRange, vSep)
                     WHEN NUM-ENTRIES(iStrRange, vSep) = 2
         .
         IF vStart <> ? THEN DO:
            ASSIGN
               oStart = INT64(TRIM(vStart))
               oEnd   = INT64(TRIM(vEnd))
                        WHEN vEnd <> ?
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN.
         END.
      END.
   END.
END PROCEDURE.

FUNCTION toLogical RETURN LOGICAL (INPUT iStr AS CHARACTER):
   DEFINE VARIABLE vResult AS LOGICAL NO-UNDO.

   IF CAN-DO("YES,��", iStr) THEN
      vResult = YES.
   ELSE IF CAN-DO("NO,���", iStr) THEN
      vResult = NO.
   ELSE
      vResult = ?.
   RETURN vResult.
END FUNCTION.
/* $LINTUSER='BIS' */
/* $LINTENV ='dvp' */
/* $LINTVSS ='$/ws1-dvp/bq/' */
/* $LINTDATE='12/11/2014 10:09:45.058+04:00' */
/* $LINTFILE='strings.fun' */
/*prosign1hFaDQCexw+hhbdz/s62ew*/