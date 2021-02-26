/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SIGNS.FUN
      Comment: ������, ᮤ�ঠ騩 �孮�����᪨� ����७���
               �㭪樨 � ��楤��� ࠡ��� � signs � code
   Parameters:
         Uses:
      Used BY:
      Created: 25/04/2000 Olenka
     Modified: 19/07/2000 Om      ��७�� �㭪樨 GetCodeName.
     Modified: 30/01/2001 Kostik  ������� �஢��� �� ����୮� ������� �㭪権.
                                  �� ���ॡ������� � transit.i
     Modified: 10/04/2001 serge   ��⨬�����
     Modified: 18/04/2001 yakv    Create/UpdateSigns: ��ࠬ��� "inindexed"
     Modified: 21/02/2002 serge   ����� 祣�
     Modified: 19/03/2002 GunK    ��楤�� ���᪠ �� code-val
     Modified: 26/01/2003 Om      ��������� �㭪�� DelSignsCodeVal.
     Modified: 28/04/2003 koag    ��������� �㭪�� GetCodeEx.
     Modified: 08/05/2003 Om      �� xclass.pro ��७�ᥭ� ��楤�� GetXAttr.
     Modified: 05/04/2004 NIK     ��������� �㭪�� GetCodeBuff.
     Modified: 22/08/2005 NIK     � ����-� �祭� �ਢ� �㪨...
     Modified: 03.09.2005 17:39 serge    0051030 ��⨬����� �ᯮ�짮����� �६����� ⠡���
                                         � ����஥��� ��ࠬ��஢. ����⠭���� ᨣ������
                                         �맮�� UpdateSigns, ���� ��ࠬ��� ������ �ᥣ��
                                         ᮤ�ঠ�� file-name, �᫨ isIndexed <> ?
     Modified: 23.09.2008 11:58 KSV      (0097155) �뤥���� ��楤��
                                         CopySignsEx
     Modified: 13.10.2008 11:36 ariz     ��ࠢ���� �訡��: �� 㤠�﫨�� ���祭��
                                  ⥬���஢����� ���४����⮢

   �㭪樨:
      AvailCode     - �஢���� ���� �� ������ � �����䨪���
      AvailXattr    - �஢���� ���� �� ��।������� �ਢ離� � ��ꥪ�
      AvailAnySigns     - �஢���� ���� �� ��।������� �ਢ離� �� �ᥬ ��ꥪ⠬
      GetSigns      - �����頥� ���祭�� ���४�����
      GetCode       - �����頥� ���祭�� �����䨪��� (code.val)
      GetCodeEx     - �����頥� ���祭�� �����䨪��� (code.val), ����
                        㪠������ 3-� ��ࠬ��஬ ���祭�� (� ��砥
                        ������⢨� �����䨪���)
      GetCodeName   - �����頥� �������� �����䨪��� (code.name)
      GetCodeMisc   - �����頥� �������⥫쭮� ���祭�� (code.misc[i])
      CreateSigns   - ᮧ���� ���� ���४����� (�ᯮ�짮���� ⮫쪮 ��
                          ᮧ����� ������ ��ꥪ�)
      UpdateSigns   - ����䨪��� ���४�����, �᫨ ������� �������㥬����
                      � ��� ⠡����
      UpdateSignsEx   - ����䨪��� ���४����� �� ����� �����
      UpdateTempSignsEx - ��������/।���஢���� ���������������� �� �� ��।������� ����
      UpdateTempSigns   - ��������/।���஢���� ���������������� �� �� ��������� ����
      isXAttrIndexed    - �஢����, ������஢���� �� ४�����
      AvailSignsCode    - �஢���� ���� �� ���४����� �� ��ࠬ���� �����䨪���
      AvailSignsAnyCode - �஢���� ���� �� ���४����� �� �����䨪����
      GetXclassFile - �� ������ �����頥� �ண��c��� ⠡����, � ���ன
                          �ਢ�뢠���� ���४������ (��� signs.file-name)
      GetXAttrValueEx   - ������� ���祭�� ���४����� � ��⮬ ���祭��
                          �� 㬮�砭��
      GetXAttrInit      - ������� ��砫쭮� ���祭�� ४����� �����

   ��楤���:
      DelSigns          - 㤠��� �� ���४������ ��ꥪ�
      DelSignsCode      - 㤠��� �� ���४������ �� ��ࠬ���� �����䨪���
      DelSignsAllCode   - 㤠��� �� ���४������ �� �����䨪����
      FindSignsByVal    - �����⢫�� ���� �� code-val,code � ᯨ��
                          ��������� 䠩���. �����頥� ���ண�� ����� �
                          ����� ���������� signs � ᯨ᪥ 䠩���.
      DelSignsCodeVal   - ������ � ��� ��ꥪ⮢ ��࠭��� ⠡����
                          �������� ���祭�� �������⥫쭮�� ४�����
      CopySigns         - ����஢���� ���.४����⮢ ������ ��ꥪ� �� ��㣮�
      GetXAttr          - �����頥� ����� � 㪠����� ४����⮬ �����
*/

{read-only.fun}

&IF DEFINED (PP-XCLAS) EQ 0
&THEN
   &MESSAGE ** ��������� - �맢�� signs.fun, �ᯮ���� INTRFACE.GET xclass
&ENDIF

&IF DEFINED(MethodSignsAlredyDefined) EQ 0 &THEN

{xattrtmp.def}          /* ������ ᮤ�ন� ���祭� ⥬��ࠫ��� ��. */
 
/* ����ࠥ� ᯨ᮪ ⥬��ࠫ��� ��. */
PROCEDURE GetAllXattrTmp PRIVATE.
   DEF BUFFER class  FOR   class.   /* ���������� ����. */
   DEF BUFFER xattr  FOR   xattr.   /* ���������� ����. */
                        /* ��ॡ�ࠥ� �� ������ ����奬� ��� �ନ஢����,
                        ** ⥬��ࠫ쭮�� ᯨ᪠. */
   EMPTY TEMP-TABLE ttXattrTemp.

   BLCK_CLASS:
   FOR EACH Xattr WHERE xattr.temporal
                    AND xattr.sign-inherit EQ "�"
                    AND xattr.DATA-TYPE NE "class"
   NO-LOCK,
   FIRST CLASS WHERE CLASS.Class-Code     EQ xattr.Class-Code
                 AND class.Progress-Table
   NO-LOCK:
                        /* �������� ����� ���� ⥬��ࠫ�� � �१� ⠡����. */
      FIND FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ class.Progress-Code
                               AND ttXattrTemp.fXattr   EQ xattr.xattr-code
      NO-ERROR.
      IF AVAIL ttXattrTemp
         THEN NEXT BLCK_CLASS.
      CREATE ttXattrTemp.
      ASSIGN
         ttXattrTemp.fTable   =  class.Progress-Code
         ttXattrTemp.fXattr   =  xattr.xattr-code
         ttXattrTemp.fBasic   =  xattr.Progress-Field
         ttXattrTemp.fIndexed =  xattr.Indexed
      .
   END.
   RETURN.
END PROCEDURE.

/* "���㦠��" �६����� ⠡���� � ������ ⥬���஢���묨 ४����⠬� */
PROCEDURE GetXattrTmp.
   DEFINE OUTPUT PARAMETER TABLE FOR ttXattrTemp.
   RETURN.
END PROCEDURE.

/* ��।���� ⥬���஢���� �� ४����� */
FUNCTION IsTemporal RETURN LOGICAL (
   INPUT iTable AS CHARACTER,
   INPUT iXAttr AS CHARACTER
):
   RETURN CAN-FIND(FIRST ttXattrTemp WHERE ttXattrTemp.fTable EQ iTable
                                       AND ttXattrTemp.fXAttr EQ iXAttr
               NO-LOCK).
END FUNCTION.

/* �� ������ �����頥� �ண��c��� ⠡����, � ���ன
   �ਢ�뢠���� ���४������ (��� signs.file-name)
*/
FUNCTION GetXclassFile RETURN CHAR
        (INPUT io-class AS CHAR):

   FIND class WHERE class.class-code EQ io-class NO-LOCK NO-ERROR.
   RETURN IF AVAIL class THEN class.progress-code ELSE ?.

END.

/* ����祭�� ����� ��ꥪ�. */
FUNCTION GetClassObj RETURN CHAR (
   INPUT iBH AS HANDLE  /* �����⥫� �� ����� ��ꥪ�. */
):
   DEF VAR mClassCode AS CHAR NO-UNDO. /* ����� ��ꥪ�. */

   DEF BUFFER xattr FOR xattr. /* ���������� ����. */
                        /* �᪫�祭�� �� �ࠢ��. */
   IF CAN-DO (
      "class,xattr,class-method,xlink,xstatus,xtrans-link,xtrans-status,xlink-valid",
      iBH:TABLE)
      THEN RETURN iBH:TABLE.
                        /* �᫨ ���� ���� "class-code", � ��६ �� ����. */
   mClassCode = GetValue (iBH, "class-code").
                        /* �᫨ ⠪��� ���� ���,� ����� ��ꥪ�
                        ** ���� ⠡���, ���� ���祭�� �� "class-code". */
   IF NOT  {assigned mClassCode} 
   THEN DO:
                        /* �� 㬮�砭��, ��� ⠡���� � ���� �����. */
      mClassCode = iBH:TABLE.
                        /* ����砥� ���ᠭ�� ��. */
      RUN GetXattr (mClassCode, "class-code", BUFFER xattr).
                        /* �᫨ 㪠��� �� "class-code",
                        ** � ��� ���祭�� � ���� ����� ��ꥪ�. */

                        /* ��६ ���祭�� �� "class-code" � ��ꥪ�.
                        ** �᫨ ���筨� ४����� �� ��।�����,
                        ** � �� 㬮�砭�� ����� - ��� ⠡����.*/
      IF     AVAIL xattr              
         AND NOT xattr.progress-field
      THEN ASSIGN
                        /* ����砥� ���祭� ����� �� �� ��ꥪ�. */
         mClassCode = GetXAttrValueEx (
                        iBH:TABLE,
                        GetSurrogateBuffer(iBH:TABLE, iBH),
                        "class-code",
                        mClassCode)
                        /* �᫨ ����祭��� ���祭�� �⫨砥��� �� ����� ⠡����,
                        ** � �஢��塞 ����稥 ����� � ����奬�. */
         mClassCode = IF      iBH:TABLE NE mClassCode
                        AND   CAN-FIND (FIRST class WHERE
                                          class.class-code EQ mClassCode
                                        NO-LOCK)
                        /* �᫨ ���ᠭ�� �������, � mClassCode - �����,
                        ** ���� ��� ⠡���� � ���� ����� ��ꥪ�. */
                        THEN mClassCode
                        ELSE iBH:TABLE
      .
   END.
   RETURN mClassCode.
END FUNCTION.

FUNCTION AvailXattr RETURN LOGICAL
        (INPUT infile AS CHAR,
         INPUT insurr AS CHAR,
         INPUT inclass-code AS CHAR):

   IF inclass-code = ?
   THEN DO:
               /* ���� ���祭�� ⥬���஢������ �� �� ������� ��ࠬ��ࠬ �� ⥪���� ���� */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* ���� ����� �� ��� */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name EQ infile
                                       AND signs.surrogate EQ insurr)
                     /* ���� ⥬���஢���� �� */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value     NE ""
                   OR  tmpsigns.xattr-value    NE "")).
   END.
   ELSE IF NUM-ENTRIES(inclass-code) GT 1
   THEN DO:
               /* ���� ���祭�� ⥬���஢������ �� �� ������� ��ࠬ��ࠬ �� ⥪���� ���� */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.code       EQ ENTRY(1,inclass-code)
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* ���� ����� �� ��� */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name  EQ infile
                                       AND signs.surrogate  EQ insurr
                                       AND signs.code       EQ ENTRY(1,inclass-code)
                                       AND signs.code-value EQ ENTRY(2,inclass-code))
                     /* ���� ⥬���஢���� �� */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value   EQ ENTRY(2,inclass-code)
                   OR  tmpsigns.xattr-value  EQ ENTRY(2,inclass-code))).
   END.
   ELSE DO:
               /* ���� ���祭�� ⥬���஢������ �� �� ������� ��ࠬ��ࠬ �� ⥪���� ���� */
      FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ infile
                           AND tmpsigns.code       EQ inclass-code
                           AND tmpsigns.surrogate  EQ insurr
                           AND tmpsigns.since      LE gend-date
      NO-LOCK NO-ERROR.
                     /* ���� ����� �� ��� */
      RETURN    CAN-FIND(FIRST signs WHERE signs.file-name  EQ infile
                                       AND signs.surrogate  EQ insurr
                                       AND signs.code       EQ inclass-code)
                     /* ���� ⥬���஢���� �� */
             OR       (AVAIL tmpsigns
                  AND (tmpsigns.code-value     NE ""
                   OR  tmpsigns.xattr-value    NE "")).
   END.

END.

FUNCTION AvailAnySigns RETURN LOGICAL
        (INPUT infile AS CHAR,
         INPUT inclass-code AS CHAR):

   RETURN    CAN-FIND(FIRST signs WHERE signs.file-name EQ infile
                                 AND signs.code EQ inclass-code)
          OR CAN-FIND(LAST  tmpsigns WHERE tmpsigns.file-name  EQ infile
                                       AND tmpsigns.code       EQ inclass-code
                                       AND tmpsigns.since      LE gend-date
                                       AND tmpsigns.code-value NE "").
END.

/* �஢���� ���� �� ������ � �����䨪��� */
FUNCTION AvailCode RETURN LOGICAL (INPUT ipClass AS CHAR, INPUT ipCode AS CHAR):
   RETURN CAN-FIND(FIRST code WHERE code.class EQ ipClass
                                AND code.code EQ ipCode NO-LOCK).
END.


/* �஢����, ������஢���� �� �������⥫�� ४����� (��).
** �����頥�� ���祭��:
** ?     - �� �� ������.
** YES   - ��   �������㥬�.
** NO    - �� ���������㥬�.
** */
FUNCTION IsXAttrIndexed RETURNS LOG (
   INPUT cname AS CHAR,    /* ����� ��ꥪ�. */
   INPUT ccode AS CHAR     /* ��� �������⥫쭮�� ४�����. */
):

   DEFINE BUFFER xattr FOR xattr.   /* ���������� ����. */

   /* ���� ���� ��. */
   RUN GetXAttr (cname,
                 ccode,
                 BUFFER xattr).

   RETURN IF AVAIL xattr
             THEN xattr.indexed
             ELSE ?.
END.

FUNCTION GetCode RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ inclass AND
                         code.code EQ incode NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN ? ELSE code.val.
END.

FUNCTION GetCodeEx RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR,
         INPUT inReturn AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ inclass AND
                         code.code EQ incode NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN inReturn ELSE code.val.
END.

FUNCTION GetCodeNameEx RETURN CHAR
        (INPUT iClass   AS CHAR,  /* ����� �����䨪���. */
         INPUT iCode    AS CHAR,  /* ��� �����䨪���. */  
         INPUT iReturn  AS CHAR): /* ���祭�� �� 㬮�砭�� */

   DEFINE BUFFER code FOR code.

   FIND FIRST code WHERE 
              code.class EQ iClass 
          AND code.code  EQ iCode 
      NO-LOCK NO-ERROR.

   RETURN IF NOT AVAIL code THEN iReturn ELSE code.name.
END.

FUNCTION GetCodeName RETURN CHAR
        (INPUT inclass AS CHAR,
         INPUT incode AS CHAR):
   RETURN GetCodeNameEx(inclass,incode,?).
END.

/* �����頥� �������⥫쭮� ���祭�� �����䨪���
** code.misc[i] */
FUNCTION GetCodeMiscEx RETURN CHARACTER
        (INPUT iClass  AS CHARACTER, /* ����� �����䨪���. */
         INPUT iCode   AS CHARACTER, /* ��� �����䨪���. */
         INPUT iPos    AS INT64,
         INPUT iReturn AS CHARACTER): /* ���浪��� ����� ���७��. */

   DEFINE BUFFER code FOR code. /* ���������� ����. */

   FIND FIRST code WHERE
              code.class EQ iClass
          AND code.code  EQ iCode
      NO-LOCK NO-ERROR.

   RETURN IF NOT AVAIL code THEN iReturn ELSE code.misc[iPos].
END.

/* �����頥� �������⥫쭮� ���祭�� �����䨪���
** code.misc[i] */
FUNCTION GetCodeMisc RETURN CHAR
        (INPUT ipClassChar AS CHAR, /* ����� �����䨪���. */
         INPUT ipCodeChar  AS CHAR, /* ��� �����䨪���. */
         INPUT ipPosInt    AS INT64): /* ���浪��� ����� ���७��. */

   RETURN GetCodeMiscEx(ipClassChar,ipCodeChar,ipPosInt,?).
END.

/* �����頥� ���ᠭ�� �����䨪���
** code.description[i] */
FUNCTION GetCodeDesc RETURN CHAR
        (INPUT ipClassChar AS CHAR, /* ����� �����䨪���. */
         INPUT ipCodeChar  AS CHAR, /* ��� �����䨪���. */
         INPUT ipPosInt    AS INT64,  /* ���浪��� ����� ���७��. */
         INPUT inReturn    AS CHAR):

   DEFINE BUFFER code FOR code. /* ���������� ����. */

   FIND FIRST code WHERE
              code.class EQ ipClassChar
          AND code.code  EQ ipCodeChar
              NO-LOCK NO-ERROR.

   IF AVAILABLE(code)
      THEN RETURN code.description[ipPosInt].
      ELSE RETURN inReturn.
END.

/*----------------------------------------------------------------------------*/
/* �����頥� ���� SIGNS                                                     */
/*----------------------------------------------------------------------------*/
FUNCTION GetCodeBuff LOGICAL (INPUT  iClass AS  CHAR,
                              INPUT  iCode  AS  CHAR,
                              BUFFER bCode  FOR Code):
   FIND FIRST bCode WHERE
              bCode.Class EQ iClass
          AND bCode.Code  EQ iCode
              NO-LOCK NO-ERROR.
   RETURN AVAILABLE(bCode).
END FUNCTION.

/* �����।�⢥��� ��࠭���� ���祭�� ���������������� ��
** ��� �஢�ન ⥬���஢������ ���४�����,
** ��� ���᫥��� ����� ⠡����, �ਧ���� �������㥬��� � �.�.
** � infile ������ ��।������� ��� ⠡����, � �� �����. */
PROCEDURE UpdateTempSignsExDirect.
   DEF INPUT  PARAM infile AS CHAR   NO-UNDO.   /* ������������ ⠡���� (��� �����). */         
   DEF INPUT  PARAM insurr AS CHAR   NO-UNDO.   /* �����䨪��� (���ண��) ����� � ⠡���. */ 
   DEF INPUT  PARAM incode AS CHAR   NO-UNDO.   /* ��� ��. */                                    
   DEF INPUT  PARAM indate AS DATE   NO-UNDO.   /* ��� �� */                                    
   DEF INPUT  PARAM inval  AS CHAR   NO-UNDO.   /* ���祭�� ��. */                               
   DEF INPUT  PARAM inindx AS LOG    NO-UNDO.   /* �ਧ��� �������㥬��� */
   DEF OUTPUT PARAM oFlag  AS LOG    NO-UNDO.   /* ���� �ᯥ譮�� ᮧ����� �� */

   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .

   vListCS = GetHistoryFieldsCS ( infile ). 

   DEF BUFFER class        FOR class.
   DEF BUFFER tmpsigns     FOR tmpsigns.
   DEF BUFFER b-tmpsigns   FOR tmpsigns.

   /* ���� ���ண�� "commission,acct,currency,kau,min-value,period,since" ���塞 �� ���� "comm-rate-id" */
   DEF BUFFER comm-rate FOR comm-rate.
   IF GetXclassFile (infile) EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
   DO:
      IF ENTRY(4,insurr) <> "" THEN
         FIND FIRST comm-rate WHERE
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
         NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST comm-rate WHERE
                    comm-rate.filial-id = shfilial AND
                    comm-rate.branch-id = "" AND
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
            insurr = STRING(comm-rate.comm-rate-id).
   END.

   BLCK:
   DO
   ON ERROR  UNDO BLCK, LEAVE BLCK
   ON ENDKEY UNDO BLCK, LEAVE BLCK:
                        /* ���� ���祭�� ⥬���஢������ ���४����� �� ���� */
      FIND LAST tmpsigns  WHERE tmpsigns.file-name   EQ infile
                            AND tmpsigns.code        EQ incode
                            AND tmpsigns.surrogate   EQ insurr
                            AND tmpsigns.since       LE indate
      NO-LOCK NO-ERROR.
                        /* �᫨ ��।��� ?, � ᮧ���� ������ � ����� ���祭��� �� */
      IF inval EQ ? THEN inval = "".
                        /* ��।����� ���祭�� ࠢ�� �।��饬� �������饬� -
                        ** �� ᮧ���� ����� ������ */
      IF CAN-DO(vListCS,incode) THEN
      DO:
         vStrCS = inval .
         /* �ॡ���� �஢�ઠ � ��⮬ ॣ���� */
         IF     AVAIL tmpsigns
            AND ((    inindx
                  AND tmpsigns.code-value EQ vStrCS)
                 OR 
                 (    NOT inindx
                  AND tmpsigns.xattr-value EQ vStrCS))
         THEN DO:
            RELEASE tmpsigns.
            oFlag = YES.
            LEAVE BLCK.
         END.
      END.
      ELSE 
      DO:
         IF     AVAIL tmpsigns
            AND ((    inindx
                  AND tmpsigns.code-value EQ inval)
                 OR 
                 (    NOT inindx
                  AND tmpsigns.xattr-value EQ inval))
         THEN DO:
            RELEASE tmpsigns.
            oFlag = YES.
            LEAVE BLCK.
         END.
      END.
                        /* �᫨ ��������� ���祭�� �� �� �ॡ㥬�� ���� - ��� ��� ����� �� ����������� */
      IF AVAIL tmpsigns AND tmpsigns.since NE indate THEN
         RELEASE tmpsigns.

      TR:
      DO TRANSACTION
      ON ERROR  UNDO TR, LEAVE TR
      ON ENDKEY UNDO TR, LEAVE TR:
                     /* �᫨ �� ������� �������饥 ���祭�� ⥬���஢������
                     ** �� �� �������� ����, ᮧ���� �����, ���� ।����㥬. */
         IF NOT AVAIL tmpsigns THEN
         DO:
            CREATE tmpsigns.
            ASSIGN
               tmpsigns.file-name   = infile
               tmpsigns.code        = incode
               tmpsigns.surrogate   = insurr
               tmpsigns.since       = indate
            .
         END.
         ELSE
            FIND CURRENT tmpsigns EXCLUSIVE-LOCK NO-ERROR.
                     /* ��������� ���祭�� ⥬���஢������ �� */
         ASSIGN
            tmpsigns.code-value  = IF inindx THEN inval ELSE ""
            tmpsigns.xattr-value = IF inindx THEN ""    ELSE inval
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            UNDO TR, LEAVE TR.
         tmpsigns.dec-value   = IF NOT {assigned inval} THEN ? ELSE DEC(inval) NO-ERROR.
         &IF DEFINED (ORACLE) &THEN
         IF tmpsigns.dec-value > 999999999999999.99 THEN tmpsigns.dec-value = ?. /* ��࠭�祭�� �ଠ� ���� ORACLE */
         &ENDIF
         tmpsigns.date-value  = DATE(inval) NO-ERROR.
         RELEASE tmpsigns.
         oFlag =  YES.
      END.
   END.
END PROCEDURE.

/* ��������/।���஢���� ���������������� �� �� ��।������� ����
** �㭪�� �����頥�:
**    YES - �� �ᯥ譮 ᮧ���
**    NO  - �� �� ��ࠡ�⠭
**    ?   - �� �� ⥬���஢���� */
FUNCTION UpdateTempSignsEx RETURN LOG (
   INPUT infile       AS CHAR,   /* ������������ ⠡���� (��� �����). */
   INPUT insurr       AS CHAR,   /* �����䨪��� (���ண��) ����� � ⠡���. */
   INPUT incode       AS CHAR,   /* ��� ��. */
   INPUT indate       AS DATE,   /* ��� �� */
   INPUT inval        AS CHAR,   /* ���祭�� ��. */
   INPUT inindexed    AS LOG    /* �ਧ��� �������㥬��� */
):
   DEF VAR vFlag     AS LOG    NO-UNDO. /* ������� ��ࠡ�⪨ ��. */
   DEF VAR vTable    AS CHAR   NO-UNDO. /* ������ �����. */

   DEF BUFFER comm-rate FOR comm-rate.

   MAIN_BLOCK:
   DO:
                        /* ��।������ ����� ⠡����. */
      vTable = GetXclassFile (infile).
      IF vTable EQ ?
      THEN DO:
         vFlag = ?.
         LEAVE MAIN_BLOCK.
      END.
                        /* ���� ���ண�� "commission,acct,currency,kau,min-value,period,since" 
                        ** ���塞 �� ���� "comm-rate-id" */
      IF vTable EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
      DO:
         IF ENTRY(4,insurr) <> "" THEN
            FIND FIRST comm-rate WHERE
                       comm-rate.commission = ENTRY(1,insurr) AND
                       comm-rate.acct = ENTRY(2,insurr) AND
                       comm-rate.currency = ENTRY(3,insurr) AND
                       comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                       comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                       comm-rate.period = INT64(ENTRY(6,insurr)) AND
                       comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         ELSE
            FIND FIRST comm-rate WHERE
                       comm-rate.filial-id = shfilial AND
                       comm-rate.branch-id = "" AND
                       comm-rate.commission = ENTRY(1,insurr) AND
                       comm-rate.acct = ENTRY(2,insurr) AND
                       comm-rate.currency = ENTRY(3,insurr) AND
                       comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                       comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                       comm-rate.period = INT64(ENTRY(6,insurr)) AND
                       comm-rate.since = DATE(ENTRY(7,insurr)) 
               NO-LOCK NO-ERROR.
            IF AVAIL comm-rate THEN
               insurr = STRING(comm-rate.comm-rate-id).
      END.

                        /* ��।���塞, ���� �� �� ⥬���஢����. */
      IF CAN-FIND (FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ vTable
                                       AND ttXattrTemp.fXattr   EQ inCode)
      THEN DO:          /* �� - ⥬���஢����.*/
         IF inindexed EQ ?
         THEN DO:
                        /* ��।������ �������㥬��� ��. */
            inindexed = IsXAttrIndexed(infile,   /* ����� ��ꥪ�. */
                                       incode).  /* ��� ��. */
            IF inindexed EQ ? THEN LEAVE MAIN_BLOCK.    /* �� � ⠪�� ����� �� ������. */
         END.

         RUN UpdateTempSignsExDirect(vTable,insurr,incode,indate,inval,inindexed,OUTPUT vFlag).
      END.
      ELSE vFlag = ?.   /* �� �� ⥬���஢���� */
   END.
   RETURN vFlag.
END.


/* ��������/।���஢���� ���������������� �� �� ��������� ����
** �㭪�� �����頥�:
**    YES - �� �ᯥ譮 ᮧ���
**    NO  - �� �� ��ࠡ�⠭
**    ?   - �� �� ⥬���஢���� */
FUNCTION UpdateTempSigns RETURN LOG (
   INPUT infile       AS CHAR,   /* ������������ ⠡���� (��� �����). */
   INPUT insurr       AS CHAR,   /* �����䨪��� (���ண��) ����� � ⠡���. */
   INPUT incode       AS CHAR,   /* ��� ��. */
   INPUT inval        AS CHAR,  /* ���祭�� ��. */
   INPUT inindexed    AS LOG    /* �ਧ��� �������㥬��� */
):
   RETURN UpdateTempSignsEx (infile,insurr,incode,(IF gend-date = ? THEN gend-hdate ELSE gend-date),inval,inindexed).
END.


/* �������� / ।���஢���� / 㤠����� ���祭�� �������⥫쭮�� ४����� (��).
**
** �������� ��� ��������������:
** ���祭�� �� ������ �⫨����� ��:
** "" (����� ��ப�) ���
** ? (����।������� ���祭��).
**
** ��������
** ���祭�� �� ������ ����:
** ���� "" (����� ��ப�) ���
** ���� ? (����।������� ���祭��).
**
** �᫨ �� �� ����� �� ���譥� ��楤�� ��।����� �������㥬���� ��, �
** ��।��� � ��ࠬ��� ININDEXED ? (����।������� ���祭��), �
** INFILE ������ ᮤ�ঠ�� ����� �������.
** ���� INFILE ������ ᮤ�ঠ�� ��� ⠡����!
**
** ������� �� ��� �᪮७��, � ��஬��� ���졠 ��࠭�� ��।����� �������㥬����
** � ��� ⠡����, �᫨ ������ �㭪�� ������ ��뢠���� � 横�� ����� ࠧ
**
** �����頥�� ���祭��:
** YES   - �� ��ࠡ�⠭ �ᯥ譮.
** NO    - �� �� ��ࠡ�⠭.
** */
FUNCTION UpdateSigns RETURN LOG (
   INPUT infile       AS CHAR,   /* ������������ ⠡���� (��� �����). */
   INPUT insurr       AS CHAR,   /* �����䨪��� (���ண��) ����� � ⠡���. */
   INPUT incode       AS CHAR,   /* ��� ��. */
   INPUT inval        AS CHAR,   /* ���祭�� ��. */
   INPUT inindexed    AS LOGICAL /* �ਧ���   �������㥬� �� (YES)
                                 **         ���������㥬� �� (NO). */
):
   DEF VAR vFlag     AS LOG    NO-UNDO. /* ������� ��ࠡ�⪨ ��. */
   DEF VAR vTempFlag AS LOG    NO-UNDO. /* ���� ᮧ����� ⥬���஢������ �� */
   DEF VAR vClassLst AS CHAR   NO-UNDO. /* ���᮪ ����ᮢ */
   DEF VAR vi        AS INT64  NO-UNDO. /* ���稪 */
   DEF VAR vClass    AS CHAR   NO-UNDO. /* ��� �����, �᫨ ��।�� �����, � �� ��� 䠩�� (inindexed = ?) */
   DEF VAR vTmpDate  AS DATE   NO-UNDO. /* ��� ᮧ����� ���祭�� ⥬���஢������ �� */
   DEF VAR vDecVal   AS DEC    NO-UNDO INIT ?. /* ���祭�� ⨯� INTEGER,INT64,DECIMAL */
   DEF VAR vDateVal  AS DATE   NO-UNDO INIT ?. /* ���祭�� ⨯� DATE */
   DEF VAR vMandatory AS LOG   NO-UNDO.

   DEF BUFFER signs     FOR signs.     /* ���������� ����. */
   DEF BUFFER tmpsigns  FOR tmpsigns.  /* ���������� ����. */
   DEF BUFFER xattr     FOR xattr.     /* ���������� ����. */
   DEF BUFFER class     FOR class.     /* ���������� ����. */
      
   DEFINE VARIABLE vStrCS AS CHARACTER   NO-UNDO CASE-SENSITIVE .
   DEFINE VARIABLE vListCS AS CHARACTER   NO-UNDO.

   IF NOT DataBaseReadOnly() THEN
   DO:
   vListCS = GetHistoryFieldsCS ( infile ). 

   ASSIGN vDecVal  = DEC (inval) WHEN {assigned inval} NO-ERROR.  /* ���⮥ ���祭�� �� ��⠥� ���室�騬 ���祭�� ��� �᫮��� ⨯�� */
   &IF DEFINED (ORACLE) &THEN
   IF vDecVal > 999999999999999.99 THEN vDecVal = ?. /* ��࠭�祭�� �ଠ� ���� ORACLE */
   &ENDIF
   ASSIGN vDateVal = DATE (inval) NO-ERROR.
                        /* ���� ���ᠭ�� ���.४�����. �� ����� ������ ����室���
                        ** ��� ��।������ �������㥬���. ���⮬�
                        ** ᫥���騩 ���� �믮��塞 ⮫쪮, �᫨ �������㥬���� ��
                        ** ��।�����. � ��⠫���
                        ** ����� ���ᠭ�� ���.४����� �� �ॡ����. */
   IF    inindexed EQ ?
   THEN DO:
                        /* �᫨ �������㥬���� �� �� ��।����� (inindexed = ?), � � infile
                        ** ������ ��।������� ��� �����, ���⮬� �饬 �� � �⮬ ����� */
      BLK:
      FOR FIRST class WHERE class.Class-code EQ infile NO-LOCK,
      FIRST xattr WHERE xattr.Class-Code EQ class.Class-Code
                    AND xattr.Xattr-Code EQ incode
      NO-LOCK:
         ASSIGN
                        /* ��।������ ����� ⠡����. */
            infile    = CLASS.Progress-Code
                        /* ��।������ �������㥬��� ��. */
            inindexed = xattr.Indexed.
         LEAVE BLK.
      END.
                     /* �᫨ �� ������� ���ᠭ�� ����� ��� ४����� - ��室 � �訡��� */
      IF NOT AVAIL Xattr THEN RETURN NO.
   END.

   /* ���� ���ண�� "commission,acct,currency,kau,min-value,period,since" ���塞 �� ���� "comm-rate-id" */
   DEF BUFFER comm-rate FOR comm-rate.
   IF infile EQ "comm-rate" AND NUM-ENTRIES(insurr) > 3 THEN
   DO:
      IF ENTRY(4,insurr) <> "" THEN
         FIND FIRST comm-rate WHERE
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
         NO-LOCK NO-ERROR.
      ELSE
         FIND FIRST comm-rate WHERE
                    comm-rate.filial-id = shfilial AND
                    comm-rate.branch-id = "" AND
                    comm-rate.commission = ENTRY(1,insurr) AND
                    comm-rate.acct = ENTRY(2,insurr) AND
                    comm-rate.currency = ENTRY(3,insurr) AND
                    comm-rate.kau = PopSurr(ENTRY(4,insurr)) AND
                    comm-rate.min-value = DEC(ENTRY(5,insurr)) AND
                    comm-rate.period = INT64(ENTRY(6,insurr)) AND
                    comm-rate.since = DATE(ENTRY(7,insurr)) 
            NO-LOCK NO-ERROR.
         IF AVAIL comm-rate THEN
            insurr = STRING(comm-rate.comm-rate-id).
   END.

                        /* ��।���塞, ���� �� �� ⥬���஢����. */
   IF CAN-FIND (FIRST ttXattrTemp WHERE ttXattrTemp.fTable   EQ infile
                                    AND ttXattrTemp.fXattr   EQ inCode)
   THEN                 /* I. �� - ⥬���஢����.*/
      UPDSIG:
      DO:
                        /* ��।�塞 ⥪���� ���� */
      vTmpDate = IF gend-date EQ ? THEN gend-hdate ELSE gend-date.
                        /* ���� ���祭�� ⥬���஢������ ���४����� �� ���� */
      FIND LAST tmpsigns  WHERE tmpsigns.file-name   EQ infile
                            AND tmpsigns.code        EQ incode
                            AND tmpsigns.surrogate   EQ insurr
                            AND tmpsigns.since       LE vTmpDate
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED tmpsigns THEN DO:
         RUN wholocks2.p (RECID(tmpsigns), "tmpsigns", "������ � tmpsigns �������஢����").
         RETURN ERROR. 
      END.

                        /* ��।����� ���祭�� ࠢ�� �।��饬� �������饬� -
                        ** �� ᮧ���� ����� ������ */
      IF CAN-DO(vListCS,incode) THEN
      DO:
         vStrCS = inval .
         /* �ॡ���� �஢�ઠ � ��⮬ ॣ���� */
         IF     AVAIL tmpsigns
            AND ((    inindexed
                  AND tmpsigns.code-value EQ vStrCS)
                 OR 
                 (    NOT inindexed
                  AND tmpsigns.xattr-value EQ vStrCS))
         THEN DO:
            RELEASE tmpsigns.
            vFlag = YES.
            LEAVE UPDSIG.
         END.
      END.
      ELSE 
      DO:
         IF     AVAIL tmpsigns
            AND ((    inindexed
                  AND tmpsigns.code-value EQ inval)
                 OR 
                 (    NOT inindexed
                  AND tmpsigns.xattr-value EQ inval))
         THEN DO:
            RELEASE tmpsigns.
            vFlag = YES.
            LEAVE UPDSIG.
         END.
      END.
                        /* �᫨ ��������� ���祭�� �� �� �ॡ㥬�� ���� - ��� ��� ����� �� ����������� */
      IF AVAIL tmpsigns AND tmpsigns.since NE vTmpDate THEN RELEASE tmpsigns.
                        /* �᫨ ��।��� ? , � ᮧ���� ������ � ����� ���祭��� �� */
      IF inval EQ ? THEN inval = "".
      IF     vMandatory
         AND inval EQ ""
      THEN DO:
         RELEASE tmpsigns.
         vFlag = YES.
         LEAVE UPDSIG.
      END.
                        /* �᫨ �� ������� �������饥 ���祭�� ⥬���஢������
                        ** �� �� �������� ����, ᮧ���� �����, ���� ।����㥬. */
      IF NOT AVAIL tmpsigns
      THEN DO:
         CREATE tmpsigns.
         ASSIGN
            tmpsigns.file-name   = infile
            tmpsigns.code        = incode
            tmpsigns.surrogate   = insurr
            tmpsigns.since       = vTmpDate
            tmpsigns.code-value  = IF inindexed THEN inval ELSE ""
            tmpsigns.xattr-value = IF inindexed THEN ""    ELSE inval
            tmpsigns.dec-value   = vDecVal
            tmpsigns.date-value  = vDateVal
         NO-ERROR.
      END.
      ELSE DO:
      /* ��������� ���祭�� ⥬���஢������ �� */
      ASSIGN
         tmpsigns.code-value  = IF inindexed THEN inval ELSE ""
         tmpsigns.xattr-value = IF inindexed THEN ""    ELSE inval
         tmpsigns.dec-value   = vDecVal
         tmpsigns.date-value  = vDateVal
      NO-ERROR.
         END.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
         RELEASE tmpsigns NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
      vFlag =  YES.
   END.
   ELSE                 /* II. �� �� ⥬���஢���� */
   UPDSIG:
   DO:
                        /* ���� ���祭�� ��. */
      FIND FIRST signs WHERE
               signs.file-name   EQ infile
         AND   signs.surrogate   EQ insurr
         AND   signs.code        EQ incode
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED signs THEN DO:
         RUN wholocks2.p (RECID(signs), "signs", "������ � signs �������஢����").
         RETURN ERROR. 
      END.
                        /* �������� ���祭�� ��. */
      IF    inval EQ ""
         OR inval EQ ? THEN
      DO:
         IF AVAIL signs THEN 
            DELETE signs NO-ERROR.
      END.
   
      /* �������� / ।���஢���� ���祭�� ��. */
      ELSE
      DO:
         /* �᫨ ���४����� �� ��ꥪ� �� ������, ᮧ���� ��� */
         IF NOT AVAIL signs
         THEN DO:
            CREATE signs.
            ASSIGN
               signs.file-name   = infile
               signs.surrogate   = insurr
               signs.code        = incode
                  signs.code-value  = IF inindexed THEN inval  ELSE ""
                  signs.xattr-value = IF inindexed THEN ""     ELSE inval
                  signs.dec-value   = vDecVal
                  signs.date-value  = vDateVal
               NO-ERROR.
         END.
         ELSE DO:
            /* �᫨ ���祭�� �� ����������, ��室��, �.�. ��१����� �����
            ** �ਢ��� � ������ ���㦭�� ���ਨ ��������� */
            IF     (inindexed 
               AND signs.code-value  EQ inval)
                OR (NOT inindexed
               AND signs.xattr-value EQ inval)
            THEN DO:
               RELEASE signs.
               vFlag = YES.
               LEAVE UPDSIG.
            END.

         /* �����뢠�� ���祭�� ��� ᮧ������� ��� ।����㥬��� ���४�����
         ** � �㡫��㥬 ���祭�� � ����� dec-value, date-value */
         ASSIGN
            signs.code-value  = IF inindexed THEN inval  ELSE ""
            signs.xattr-value = IF inindexed THEN ""     ELSE inval
            signs.dec-value   = vDecVal
            signs.date-value  = vDateVal
         NO-ERROR.
         END.
         IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
            RELEASE signs NO-ERROR.
            IF ERROR-STATUS:ERROR THEN LEAVE UPDSIG.
      END.
      vFlag =  YES.
   END.
   END.

   RETURN vFlag.
END.

/* �������� / ।���஢���� / 㤠����� ���祭�� �������⥫쭮�� ४����� (��).
**
** �������� ��� ��������������:
** ���祭�� �� ������ �⫨����� ��:
** "" (����� ��ப�) ���
** ? (����।������� ���祭��).
**
** ��������
** ���祭�� �� ������ ����:
** ���� "" (����� ��ப�) ���
** ���� ? (����।������� ���祭��).
**
** ������ �㭪�� �����⢫�� �� �஢�ન, � ���⮬� �믮������ �������� �
** �� ����� ��뢠�� �������⭮ �� ������ � ⮬� �� ४������!
** �᫨ ��� �㤥� ��뢠���� � 横��, � ����室��� ᭠砫� ��।����� ���
** ⠡���� � �������㥬����, � ��⥬ � �⨬� ��ࠬ��ࠬ� ��뢠�� UpdateSigns
**
** �����頥�� ���祭��:
** YES   - �� ��ࠡ�⠭ �ᯥ譮.
** NO    - �� �� ��ࠡ�⠭.
** */
FUNCTION UpdateSignsEx RETURN LOG (
   INPUT infile       AS CHAR,   /* ��� ����� */
   INPUT insurr       AS CHAR,   /* �����䨪��� (���ண��) ����� � ⠡���. */
   INPUT incode       AS CHAR,   /* ��� ��. */
   INPUT inval        AS CHAR    /* ���祭�� ��. */
):

   RETURN UpdateSigns(infile,insurr,incode,inval,?).
END.


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
FUNCTION CreateSigns RETURN log
        (INPUT infile       AS CHAR,      /* ������������ ⠡���� (��� �����). */
         INPUT insurr       AS CHAR,      /* �����䨪��� (���ண��) ����� � ⠡���. */
         INPUT incode       AS CHAR,      /* ��� ��. */
         INPUT inval        AS CHAR,      /* ���祭�� ��. */
         INPUT inindexed    AS LOGICAL):

   RETURN UpdateSigns (infile,   
                       insurr,   
                       incode,   
                       inval,   
                       inindexed).
END.


/* 㤠��� �� ���४������ ��ꥪ� */
PROCEDURE DelSigns.
   DEF INPUT PARAM infile       AS CHAR NO-UNDO.
   DEF INPUT PARAM insurr       AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* ���������� ����. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* ���������� ����. */

   IF DataBaseReadOnly() THEN
      LEAVE.
   /* 㤠����� ��⥬���஢����� ���祭�� ���४����⮢ */
   FOR EACH signs WHERE signs.file-name EQ infile
                    AND signs.surrogate EQ insurr
                  EXCLUSIVE-LOCK:
      DELETE signs.
   END.
   /* 㤠����� ⥬���஢����� ���祭�� ���४����⮢ */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                       AND tmpsigns.surrogate   EQ insurr
   EXCLUSIVE-LOCK:
      DELETE tmpsigns.
   END.
END.

/* 㤠��� �� ���४������ �� ��ࠬ���� �����䨪��� */
PROCEDURE DelSignsCode.
   DEF INPUT PARAM incode       AS CHAR NO-UNDO.
   DEF INPUT PARAM inval        AS CHAR NO-UNDO.
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* ���������� ����. */
   DEFINE BUFFER xattr     FOR xattr.     /* ���������� ����. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* ���������� ����. */

   FOR EACH xattr WHERE xattr.domain-code = incode
                    AND xattr.indexed NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ?
      THEN DO:
         /* 㤠����� ���祭�� ��⥬���஢����� ���४����⮢ */
         FOR EACH signs WHERE signs.file-name   EQ infile
                          AND signs.code        EQ incode
                          AND signs.code-value  EQ inval
         EXCLUSIVE-LOCK:
            DELETE signs.
         END.
         /* 㤠����� ���祭�� ⥬���஢����� ���४����⮢ */
         FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                             AND tmpsigns.code        EQ incode
                             AND tmpsigns.code-value  EQ inval
         EXCLUSIVE-LOCK:
            DELETE tmpsigns.
         END.
      END.
   END.
END.

/* 㤠��� �� ���४������ �� �����䨪���� */
PROCEDURE DelSignsAllCode.
   DEF INPUT PARAM incode       AS CHAR NO-UNDO.
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER signs     FOR signs.     /* ���������� ����. */
   DEFINE BUFFER xattr     FOR xattr.     /* ���������� ����. */
   DEFINE BUFFER tmpsigns  FOR tmpsigns.  /* ���������� ����. */

   FOR EACH xattr WHERE xattr.domain-code = incode NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ?
      THEN DO:
         /* 㤠����� ���祭�� ��⥬���஢����� ���४����⮢ */
         FOR EACH signs WHERE signs.file-name EQ infile AND
                              signs.code      EQ incode
         EXCLUSIVE-LOCK:
            DELETE signs.
         END.
         /* 㤠����� ���祭�� ⥬���஢����� ���४����⮢ */
         FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ infile
                             AND tmpsigns.code        EQ incode
         EXCLUSIVE-LOCK:
            DELETE tmpsigns.
         END.
      END.
   END.
END.

/* �஢���� ���� �� ���४����� �� �����䨪���� */
FUNCTION AvailSignsAnyCode RETURN LOGICAL
   (INPUT incode AS CHAR):
   DEF VAR infile AS CHAR NO-UNDO.

   DEFINE BUFFER xattr FOR xattr. /* ���������� ����. */

   FOR EACH xattr WHERE xattr.domain-code = incode NO-LOCK:
      infile = GetXclassFile (xattr.class-code).
      IF infile <> ? AND AvailAnySigns(infile, incode) THEN
         RETURN YES.
   END.
   RETURN NO.
END.

/* �஢���� ���� �� ���४����� �� ��ࠬ���� �����䨪��� */
FUNCTION AvailSignsCode RETURN LOGICAL (
   INPUT incode   AS CHAR, /* ��� �����䨪���*/
   INPUT inval    AS CHAR  /* ���祭�� ��ࠬ��� �����䨪��� */
):
   DEF VAR infile AS CHAR NO-UNDO. /* ��� 䠩�� (⠡����),
                                     ���ன �ਭ������� ����� �����.*/

   DEFINE BUFFER xattr FOR xattr.  /* ���������� ���� */

   FOR EACH xattr WHERE
      xattr.domain-code EQ incode
   NO-LOCK: 
      infile = GetXclassFile (xattr.class-code).  
      IF     infile <> ?
         AND (CAN-FIND(FIRST signs WHERE signs.file-name   EQ infile
                                     AND signs.code        EQ xattr.Xattr-Code
                                     AND signs.code-value  EQ inval)
         OR   CAN-FIND(LAST  tmpsigns WHERE tmpsigns.file-name    EQ infile
                                        AND tmpsigns.code         EQ xattr.Xattr-Code
                                        AND tmpsigns.since        LE gend-date
                                        AND tmpsigns.code-value   EQ inval))
      THEN RETURN YES.
   END.
   RETURN NO.
END.

FUNCTION DelAllSigns RETURN log
        (INPUT infile       AS CHAR,
         INPUT insurr       AS CHAR
         ):
  RUN DelSigns(infile,insurr).
  FIND FIRST signs WHERE signs.file-name EQ infile AND
                         signs.surrogate EQ insurr NO-LOCK NO-ERROR.
  IF NOT AVAIL signs THEN RETURN YES.
  ELSE RETURN NO .

END FUNCTION .

PROCEDURE FindSignsByVal.
  DEFINE INPUT PARAM i-file-list   AS CHAR NO-UNDO. /* ���᮪ ��������� 䠩��� */
  DEFINE INPUT PARAM i-code        AS CHAR NO-UNDO.
  DEFINE INPUT PARAM i-code-val    AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAM o-file-order AS INT64 NO-UNDO INIT 0. /* ���浪��� ����� � ᯨ᪥ */
  DEFINE OUTPUT PARAM o-surrogate  AS CHAR NO-UNDO INIT "".

  DEFINE VARIABLE i AS INT64 NO-UNDO.

  block-1:
  DO i = 1 TO NUM-ENTRIES(i-file-list):
     FOR FIRST signs
         WHERE signs.file-name = ENTRY(i,i-file-list)
           AND signs.code      = i-code
           AND signs.code-val  = i-code-val NO-LOCK:
              ASSIGN o-file-order = i
                     o-surrogate  = signs.surrogate.
              LEAVE block-1.
     END.

     FOR LAST tmpsigns WHERE tmpsigns.file-name    EQ ENTRY(i,i-file-list)
                         AND tmpsigns.code         EQ i-code
                         AND tmpsigns.since        LE gend-date
                         AND tmpsigns.code-value   EQ i-code-val
     NO-LOCK:
        ASSIGN o-file-order = i
               o-surrogate  = tmpsigns.surrogate.
        LEAVE block-1.
     END.
  END.

END PROCEDURE.

/* ������ � ��� ��ꥪ⮢ ��࠭��� ⠡���� (ipTableChar)
** �������� ���祭�� (ipValueChar) �������⥫쭮�� ४����� (ipXattrChar). */
PROCEDURE DelSignsCodeVal.

   DEF INPUT PARAM ipTableChar AS CHAR NO-UNDO. /* ����������� ⠡����. */
   DEF INPUT PARAM ipXattrChar AS CHAR NO-UNDO. /* ��� ��. */
   DEF INPUT PARAM ipValueChar AS CHAR NO-UNDO. /* ���祭�� ��. */

   DEF BUFFER signs     FOR signs.     /* ���������� ����. */
   DEF BUFFER tmpsigns  FOR tmpsigns.  /* ���������� ����. */

   /* ��ॡ�� ��� ���祭�� ��⥬���஢����� �� �� ⠡���. */
   FOR EACH signs WHERE
      signs.file-name  EQ ipTableChar AND
      signs.code       EQ ipXattrChar
   EXCLUSIVE-LOCK:
      /* �஢�ઠ ���祭��. */
      IF ipValueChar EQ (IF signs.code-val NE ''
                         THEN signs.code-val
                         ELSE signs.xattr-val)
      THEN DELETE signs.
   END.
   /* 㤠����� ⥬���஢����� ���祭�� ���४����⮢ */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ ipTableChar
                       AND tmpsigns.code        EQ ipXattrChar
   EXCLUSIVE-LOCK:
      /* �஢�ઠ ���祭��. */
      IF ipValueChar EQ (IF tmpsigns.code-value NE ''
                         THEN tmpsigns.code-value
                         ELSE tmpsigns.xattr-value)
      THEN DELETE tmpsigns.
   END.

   RETURN.

END PROCEDURE.

PROCEDURE CopySigns:
   /*  ����� � ���ண�� ��ꥪ�-���筨�� */
   DEF INPUT PARAMETER iSrcClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iSrcSurr  AS CHAR NO-UNDO.
   /*  ����� � ���ண�� ��ꥪ�-�ਥ����� */
   DEF INPUT PARAMETER iTrgClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iTrgSurr  AS CHAR NO-UNDO.
   
   RUN CopySignsEx(iSrcClass,iSrcSurr,iTrgClass,iTrgSurr,"*","!*").
END PROCEDURE.

/* ����஢���� ���.४����⮢ � ������ ��ꥪ� �� ��㣮�.
** ���祭�� 㦥 ��������� ४����⮢ �����������!
** ���� ����饭� ����஢��� ४������ �� ��ꥪ� ��㣮�� �����!
*/
PROCEDURE CopySignsEx:
   /*  ����� � ���ண�� ��ꥪ�-���筨�� */
   DEF INPUT PARAMETER iSrcClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iSrcSurr  AS CHAR NO-UNDO.
   /*  ����� � ���ண�� ��ꥪ�-�ਥ����� */
   DEF INPUT PARAMETER iTrgClass AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iTrgSurr  AS CHAR NO-UNDO.
   /* Commented by KSV: ��᪠ ४����⮢, ���祭�� ������ ����஢��� */
   DEFINE INPUT  PARAMETER iInclList AS CHARACTER  NO-UNDO.
   /* Commented by KSV: ��᪠ ४����⮢, ���祭�� ������ �� ����஢��� */
   DEFINE INPUT  PARAMETER iExclList AS CHARACTER  NO-UNDO.

   /*  ������ ��ꥪ�-���筨�� */
   DEF VAR   vSrcFileName   AS CHAR      NO-UNDO.
   /*  ������ ��ꥪ�-�ਥ����� */
   DEF VAR   vTrgFileName   AS CHAR      NO-UNDO.
   /*  �������� ������஢����? */
   DEF VAR   vIndexed       AS LOGICAL   NO-UNDO.

   DEF BUFFER signs     FOR signs.
   DEF BUFFER tmpsigns  FOR tmpsigns.
   DEF BUFFER btmpsigns FOR tmpsigns.

   ASSIGN
      vSrcFileName = GetXclassFile (iSrcClass)
      vTrgFileName = GetXclassFile (iTrgClass).

   /* �᫨ �� 㪠������� ������ �� 㤠���� ��।����� ��� ⠡����,
   ** � ��室�� ��� �訡�� (��������, ����� �� ���ᠭ � ����奬� �,
   ** ᫥����⥫쭮, �� ������ ����� ���.४����⮢).
   */
   IF vSrcFileName = ? OR
      vTrgFileName = ? THEN
      RETURN.

   /* ����஢��� ४������ �� ��ꥪ�� ��㣮� ⠡���� �����! */
   IF vSrcFileName <> vTrgFileName THEN
      RETURN ERROR.

   FOR EACH signs WHERE
            signs.file-name = vSrcFileName
        AND signs.surrogate = iSrcSurr
      NO-LOCK:

      IF NOT CAN-DO(iInclList,signs.code) OR
             CAN-DO(iExclList,signs.code) THEN NEXT.
      
      vIndexed = isXAttrIndexed(iTrgClass, signs.code).
      /* �᫨ isXAttrIndexed ���㫠 ?,
      ** ����� ⠪��� ४����� � ����� ���.
      */
      IF vIndexed <> ? THEN
         UpdateSigns (vTrgFileName,
                      iTrgSurr,
                      signs.code,
                      (IF isXAttrIndexed(iSrcClass, signs.code)
                       THEN signs.code-value
                       ELSE signs.xattr-value),
                      vIndexed
                     ).
   END.

   /* ����஢���� ⥬���஢����� �� */
   FOR EACH tmpsigns WHERE tmpsigns.file-name   EQ vSrcFileName
                       AND tmpsigns.surrogate   EQ iSrcSurr
   NO-LOCK,
   LAST btmpsigns WHERE btmpsigns.file-name  EQ vSrcFileName
                    AND btmpsigns.code       EQ tmpsigns.code
                    AND btmpsigns.surrogate  EQ iSrcSurr
                    AND btmpsigns.since      LE gend-date
                    AND (btmpsigns.code-value  NE ""
                     OR  btmpsigns.xattr-value NE "")
   NO-LOCK,
   FIRST symbol WHERE ROWID(btmpsigns) EQ ROWID(tmpsigns)
   NO-LOCK:
      /* 䨫����� �� ᯨ�� �����㥬�� � �᪫�砥��� �� */
      IF NOT CAN-DO(iInclList,btmpsigns.code) OR
             CAN-DO(iExclList,btmpsigns.code) THEN NEXT.
      /* ��।������ �ਧ���� ������஢������ */
      vIndexed = isXAttrIndexed(iTrgClass, btmpsigns.code).
      /* �᫨ isXAttrIndexed ���㫠 ?,
      ** ����� ⠪��� ४����� � ����� ���. */
      IF vIndexed <> ? THEN
         UpdateSigns (vTrgFileName,
                      iTrgSurr,
                      btmpsigns.code,
                      (IF isXAttrIndexed(iSrcClass, btmpsigns.code)
                       THEN btmpsigns.code-value
                       ELSE btmpsigns.xattr-value),
                      vIndexed).
   END.

   RETURN.
END PROCEDURE.

PROCEDURE GetXAttr.
/* �����頥� ����� � 㪠����� ४����⮬ �����, */

   DEF INPUT PARAM io-class LIKE class.class-code NO-UNDO.
   DEF INPUT PARAM io-xattr-code LIKE xattr.xattr-code NO-UNDO.
   DEF PARAM BUFFER xattr FOR xattr.

   FIND FIRST xattr WHERE
              xattr.class-code = io-class AND
              xattr.xattr-code = io-xattr-code
              NO-LOCK NO-ERROR.

END.

/*------------------------------------------------------------------------------
  Purpose:     �����頥� ���� �����䨪��� � ���� ᯨ᪠
  Parameters:  iClass   - ����� �����䨪���
               iParent  - ��뫪� �� ��� த�⥫�
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeList RETURNS CHARACTER (iClass  AS CHARACTER,
                                        iParent AS CHARACTER):
   DEFINE VARIABLE vList AS CHARACTER  NO-UNDO.

   DEFINE BUFFER bCode FOR CODE.

   IF {assigned iParent} THEN
      FOR EACH bCode WHERE
               bCode.class    EQ iClass
           AND bCode.parent   EQ iParent
               NO-LOCK:
         {additem.i vList bCode.code}
      END.                                                      /* END OF FOR */
   ELSE
      FOR EACH bCode WHERE
               bCode.class    EQ iClass
               NO-LOCK:
         {additem.i vList bCode.code}
      END.                                                      /* END OF FOR */

   RETURN vList.
END FUNCTION.

/*------------------------------------------------------------------------------
  Purpose:     �����頥� ��� �����䨪��� �� ���祭��
  Parameters:  iClass   - ����� �����䨪���
               iVal  - ���祭��
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeVal RETURN CHAR
        (INPUT iClass AS CHAR,
         INPUT iVal AS CHAR):

   DEF BUFFER code FOR code.

   FIND FIRST code WHERE code.class EQ iClass AND
                         code.val EQ iVal NO-LOCK NO-ERROR.
   RETURN IF NOT AVAIL code THEN ? ELSE code.CODE.
END.

/*------------------------------------------------------------------------------
  Purpose:     �����頥� ���� �����䨪��� �� ���祭�� � ���� ᯨ᪠
  Parameters:  iClass   - ����� �����䨪���
               iVal  - ���祭��
  Notes:
------------------------------------------------------------------------------*/
FUNCTION GetCodeValList RETURN CHAR
        (INPUT iClass AS CHAR,
         INPUT iVal AS CHAR):

   DEFINE VARIABLE vList AS CHARACTER  NO-UNDO.
   
   DEFINE BUFFER bCode FOR CODE.
   
   FOR EACH bcode WHERE 
            bcode.class EQ iClass AND
            bcode.val EQ iVal 
      NO-LOCK:
      {additem.i vList bCode.code}
   END.

   RETURN vList.
END.

/*�㭪�� ��� ���᪠ ���ᨬ��쭮�� ���祭�� ��� ४�����,
��������� �� ��᪮�쪨� ������*/
FUNCTION GetMaxSigns RETURNS CHARACTER (ipFileLstChar AS CHARACTER,  /*���᮪ ����ᮢ*/
                                        ipCodeChar    AS CHARACTER,  /*��� ४�����*/
                                        ipBegValStr   AS CHARACTER   /*��砫쭮� ���祭�� ��� ���᪠*/
                                        ):

   DEFINE VAR vIndInt            AS INT64   NO-UNDO.
   DEFINE VAR vMaxStr            AS CHARACTER NO-UNDO.
   DEFINE BUFFER buf_signs    FOR signs.
   DEFINE BUFFER tmpsigns     FOR tmpsigns.
   DO vIndInt = 1 TO NUM-ENTRIES(ipFileLstChar):
      /* ��।���塞, ���� �� �� ⥬���஢����. */
      FIND FIRST ttXattrTemp WHERE
               ttXattrTemp.fTable   EQ ENTRY(vIndInt,ipFileLstChar)
         AND   ttXattrTemp.fXattr   EQ ipCodeChar
      NO-ERROR.
      /* �� - �� ⥬���஢����.*/
      IF NOT AVAIL ttXattrTemp
      THEN DO:
         FIND LAST buf_signs WHERE buf_signs.file-name EQ     ENTRY(vIndInt,ipFileLstChar)
                               AND buf_signs.code      EQ     ipCodeChar
                               AND buf_signs.code-val  BEGINS ipBegValStr
                               AND buf_signs.code-val  GE     vMaxStr
                                                                 NO-LOCK NO-ERROR.
         IF AVAIL buf_signs THEN vMaxStr = buf_signs.code-val.
      END.
      ELSE DO:
         FIND LAST tmpsigns WHERE tmpsigns.file-name  EQ     ENTRY(vIndInt,ipFileLstChar)
                              AND tmpsigns.code       EQ     ipCodeChar
                              AND tmpsigns.since      LE     gend-date
                              AND tmpsigns.code-value BEGINS ipBegValStr
                              AND tmpsigns.code-value GE     vMaxStr
         NO-LOCK NO-ERROR.
         IF AVAIL tmpsigns THEN vMaxStr = tmpsigns.code-value.
      END.
   END.
   RETURN vMaxStr.
END FUNCTION.

/* ������� �஢��� ����娨 ��ࠬ��� �����䨪��� */
FUNCTION GetCodeLevel RETURNS INT64
   (iClass  AS CHAR,
    iCode   AS CHAR
   ):

   DEF BUFFER code FOR code.

   DEF VAR vLevel  AS INT64   NO-UNDO.
   DEF VAR vParent AS CHAR  NO-UNDO.

   FIND FIRST code    WHERE
              code.class EQ iClass
          AND code.code  EQ iCode
   NO-LOCK NO-ERROR.

   DO WHILE AVAILABLE code:
      ASSIGN
         vParent = code.parent
         vLevel  = vLevel + 1
      .

      IF code.parent EQ iClass THEN
         RELEASE code.
      ELSE
         FIND FIRST code WHERE
                    code.class  EQ iClass
                AND code.code   EQ vParent
         NO-LOCK NO-ERROR.
   END.

   RETURN vLevel.

END FUNCTION.

/* ���᮪ ��� �����ࠬ��஢ 㪠������� ��ࠬ��� �����䨪��� */
FUNCTION GetCodeChildren RETURNS CHAR
   (iClass AS CHAR,
    iCode  AS CHAR
   ):

   DEF VAR vList  AS CHAR  NO-UNDO.

   DEF BUFFER code  FOR code.

   IF GetCode(iClass, iCode) NE ? THEN
   DO:
      vList = iCode.

      FOR EACH code     WHERE
               code.class  EQ iClass
           AND code.parent EQ iCode
         NO-LOCK:

         vList = vList + "," + GetCodeChildren(code.class,
                                               code.code
                                              ).
      END.
   END.

   RETURN vList.
END FUNCTION.

/* ���᮪ ��� �����ࠬ��஢ �� 㪠������� ᯨ�� ��ࠬ��஢ �����䨪��� */
FUNCTION List-Item-Of-Code RETURNS CHAR
   (iClass AS CHAR,
    iCodes AS CHAR
   ):

   DEF VAR vItem     AS CHAR  NO-UNDO.
   DEF VAR vList     AS CHAR  NO-UNDO.
   DEF VAR vRetList  AS CHAR  NO-UNDO.

   DEF BUFFER code  FOR code.

   DEF VAR vI     AS INT64   NO-UNDO.
   DEF VAR vJ     AS INT64   NO-UNDO.

   DO vI = 1 TO NUM-ENTRIES(iCodes):

      vList = GetCodeChildren (iClass,
                               ENTRY(vI, iCodes)
                              ).
      DO vJ = 1 TO NUM-ENTRIES(vList):

         vItem = ENTRY(vJ, vList).

         IF NOT CAN-DO(vRetList, vItem) THEN
            {additem.i vRetList vItem}
      END.
   END.

   RETURN vRetList.
END FUNCTION.


&GLOB MethodSignsAlredyDefined YES
&ENDIF

/*----------------------------------------------------------------------------*/
/* �����頥� ��� ��⮬��� ��� ��।�������� ���� �����䨪��� ��� த�⥫�*/
/*----------------------------------------------------------------------------*/

FUNCTION GetOnlyChildCode RETURNS CHARACTER ( INPUT iClass AS CHARACTER,
                                              INPUT iCode  AS CHARACTER):
   DEFINE VARIABLE mI            AS INT64     NO-UNDO.
   DEFINE VARIABLE mParentChild  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mChild        AS CHARACTER   NO-UNDO.

   mParentChild = GetCodeChildren(iClass,iCode).
   IF NUM-ENTRIES(mParentChild) > 1 THEN
   DO:
      mChild = SUBSTRING(mParentChild,INDEX(mParentChild,",") + 1).
   END.
   RETURN mChild.
END FUNCTION.


/* ���㠫����� ���祭�� ⥬���஢������ ४����� */
PROCEDURE SetValueTempAttr.
   DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iSurr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iCode AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iDate AS DATE      NO-UNDO.

   DEFINE VARIABLE vClassObj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vValue    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBuffer   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE vXattrSgn AS LOGICAL   NO-UNDO.
   
   IF iDate EQ ?  THEN
      iDate = gend-date.

   FIND FIRST ttXattrTemp WHERE
              ttXattrTemp.fTable   EQ iFile
         AND  ttXattrTemp.fXattr   EQ iCode NO-LOCK NO-ERROR.

   IF AVAIL ttXattrTemp THEN DO:
      FIND FIRST Xattr WHERE
                 xattr.Class-Code EQ iFile
             AND xattr.Xattr-Code EQ iCode NO-LOCK NO-ERROR.
      IF    NOT AVAIL Xattr
         OR NOT Xattr.Progress-Field THEN
         RETURN.
      
      CREATE BUFFER vBuffer FOR TABLE iFile NO-ERROR.
      IF NOT VALID-HANDLE(vBuffer) THEN RETURN ERROR "�訡�� ᮧ����� ���� ��ꥪ�!".
      vBuffer:FIND-FIRST("WHERE " + GetWhereSurr(iFile,iSurr),NO-LOCK) NO-ERROR.
      IF NOT vBuffer:AVAIL THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR "�訡�� ���᪠ �����!".
      END.
      
      vClassObj = GetClassObj(vBuffer).
      IF NOT {assigned vClassObj} THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR "�� ������ ����� ��ꥪ�!".
      END.

      FIND LAST tmpsigns WHERE 
                tmpsigns.file-name  EQ iFile
            AND tmpsigns.code       EQ iCode
            AND tmpsigns.surrogate  EQ iSurr
            AND tmpsigns.since      LE iDate
      NO-LOCK NO-ERROR.

      vValue = IF AVAIL tmpsigns 
               THEN (IF tmpsigns.code-value NE "" 
                     THEN tmpsigns.code-value 
                     ELSE tmpsigns.xattr-value)
               ELSE "".

      RUN CheckFullFieldValue (vClassObj,
                               iCode,
                               iSurr, 
                               vValue       
                               ).
      IF RETURN-VALUE NE "" THEN DO:
         DELETE OBJECT vBuffer.
         RETURN ERROR RETURN-VALUE.
      END.

      TR_BL:
      DO TRANS ON ERROR UNDO TR_BL,LEAVE TR_BL:
         IF ENTRY(1,PutValueByQuery(iFile,
                                    iCode,
                                    vValue,
                                    "ROWID(" + iFile + ") EQ TO-ROWID('" +
                                     STRING(vBuffer:ROWID) + "')"
                                    )) NE '1' THEN DO:
            DELETE OBJECT vBuffer.
            RETURN ERROR RETURN-VALUE.
         END.
      END.
   END.

END PROCEDURE.

/* �����頥� "��" �᫨ ��� �� ���� �� ४����⮢ iAttrList �� ������ **
** � ��ਮ� � iBegDate �� iEndDate �����⥫쭮                          */

FUNCTION ChkChangedAttrs RETURNS LOGICAL (INPUT iFileName  AS CHARACTER,
                                          INPUT iSurrogate AS CHARACTER,
                                          INPUT iBegDate   AS DATE, 
                                          INPUT iEndDate   AS DATE, 
                                          INPUT iAttrList  AS CHARACTER):

   DEFINE VARIABLE i       AS INT64     NO-UNDO.
   DEFINE VARIABLE vAttr   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vReturn AS LOGICAL     NO-UNDO.

   DO i = 1 TO NUM-ENTRIES(iAttrList):
      vAttr = ENTRY(i,iAttrList).
      IF CAN-FIND(FIRST tmpsigns WHERE
                        tmpsigns.file-name  EQ iFileName
                    AND tmpsigns.surrogate  EQ iSurrogate
                    AND tmpsigns.code       EQ vAttr
                    AND (    tmpsigns.since >= iBegDate
                         AND tmpsigns.since <= iEndDate)) THEN
      DO:
         vReturn = YES.
         LEAVE.
      END.
   END.
   RETURN vReturn.

END FUNCTION.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/07/2015 18:40:48.249+04:00' */
/* $LINTUSER='ariz' */
/* $LINTMODE='1' */
/* $LINTFILE='signs.fun' */
/*prosign+iOhWi3X+7PGwbZBlJYbhA*/