/* 
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: tax-parse-tag.p
      Comment: ��� 5.10. ������ ��᫥����� �ᯥ譮�� ᮮ�饭�� �ᯮ��
               � ࠧ��饭�� ��� � �࠭ᯮ�⭮� �ଥ
   Parameters: ���
         Uses:
      Used by:
      Created: 08.09.2014 zhua
     Modified:
*/

DEFINE INPUT PARAMETER iTxt AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttAttr NO-UNDO
   FIELD class-code   AS CHAR
   FIELD xattr-code   AS CHAR
   FIELD xattr-label  AS CHAR
   FIELD xattr-clabel AS CHAR
   FIELD HIER         AS CHAR
.
DEFINE VARIABLE mKind       AS CHAR    NO-UNDO.
DEFINE VARIABLE mBaseOpKind AS CHAR    NO-UNDO.
DEFINE VARIABLE mBaseTmpl   AS INT64   NO-UNDO.
DEFINE VARIABLE mTaxExp     AS HANDLE  NO-UNDO.
DEFINE VARIABLE mIndMidd    AS INT64   NO-UNDO.
DEFINE VARIABLE mIndRBrd    AS INT64   NO-UNDO.
DEFINE VARIABLE mIndLBrd    AS INT64   NO-UNDO.
DEFINE VARIABLE mTag        AS CHAR    NO-UNDO.
DEFINE VARIABLE mVal        AS CHAR    NO-UNDO.
DEFINE VARIABLE mMailFormat AS CHAR    NO-UNDO.
DEFINE VARIABLE mTagSubStr  AS CHAR    NO-UNDO.
DEFINE VARIABLE mHier       AS CHAR    NO-UNDO INIT "".
{globals.i}
{exchange.equ}
{intrface.get xclass}
{intrface.get pbase}
{intrface.get exch}
{intrface.get trans}
{intrface.get strng}

/******************************************************************************/
/*�����ᨢ��� ��楤��, �ନ����� ᮮ⢥��⢨� ���� ��ਡ�⮢ �            */
/*�ଠ� ������ � �࠭ᯮ�⭮� �ଥ.� ttAttr.HIER - ���� � ��ਡ��� �� ⥣��*/
/******************************************************************************/

PROCEDURE ttAttrFill:

   DEFINE INPUT PARAMETER iClass    AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER iHier     AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER iPrevDesc AS CHAR NO-UNDO.
   DEFINE BUFFER xattr FOR xattr.
   DEFINE VARIABLE mXattr-code   AS CHAR NO-UNDO.

   FOR EACH xattr WHERE xattr.class-code EQ iClass NO-LOCK:

      IF Xattr.Description EQ "ATTRLIST" OR 
         Xattr.Description EQ "OBJECT"   THEN
      DO:
         mXattr-code = IF xattr.xattr-code EQ "header__" THEN xattr.initial
                                                         ELSE xattr.xattr-code.
         /*�ய�᪠�� ����� OBJECT -> ATTRLIST � ��������묨 �������ﬨ*/
         IF iPrevDesc         EQ "OBJECT"   AND
            xattr.Description EQ "ATTRLIST" AND
            xattr.xattr-code  EQ "header__" THEN.
         ELSE
            iHier = iHier + CHR(1) + mXattr-code.

         RUN ttAttrFill(xattr.xattr-label, iHier, Xattr.Description).
         /*�ய�᪠�� ����� OBJECT -> ATTRLIST � ��������묨 �������ﬨ*/
         IF iPrevDesc         EQ "OBJECT"   AND
            Xattr.Description EQ "ATTRLIST" AND
            xattr.xattr-code  EQ "header__" THEN.
         ELSE
            iHier = SUBSTRING(iHier,1,R-INDEX(iHier,CHR(1)) - 1).

      END.
      IF Xattr.Description EQ "ATTR" THEN
      DO:
         CREATE ttAttr.
         ASSIGN
            ttAttr.class-code   = xattr.class-code
            ttAttr.xattr-code   = xattr.xattr-code
            ttAttr.xattr-label  = xattr.xattr-label
            ttAttr.xattr-clabel = xattr.xattr-clabel
            ttAttr.HIER         = iHier
         .
      END.
   END.
END PROCEDURE.

/******************************************************************************/

   ASSIGN
      mBaseOpKind = GetBaseOpKind()
      mBaseTmpl   = GetBaseTemplate()
      mKind       = GetAttrValue2(mBaseOpKind,mBaseTmpl,"ExchMain")
      mMailFormat = GetAttrValue2(mBaseOpKind,mBaseTmpl,"mail-format")
      mTaxExp     = GetTransObject(mKind)
      mTaxExp     = mTaxExp:default-buffer-handle
   NO-ERROR.

   RUN ttAttrFill (mMailFormat, "", "").
                                     /*�����頥� ������*/
   ASSIGN iTxt = TRIM(REPLACE(iTxt,CHR(2),CHR(44))) NO-ERROR.
   DO WHILE INDEX(iTxt,'<') NE 0:
      ASSIGN
         mTagSubStr = SUBSTRING(iTxt,1                 ,INDEX(iTxt,'>'))
         iTxt =       SUBSTRING(iTxt,INDEX(iTxt,'>')+ 2,LENGTH(iTxt)   )
      .
      IF INDEX(mTagSubStr,"</") EQ 0 THEN /*�᫨ �� ����뢠�騩 ⥣*/
          /*� mHier ᮡ�ࠥ� ����� ⥣�� ��� ⥪�饣� ��ਡ��.
            �� ����室��� ��� ������������ ��।������ ����� ��ਡ��, �.�.
            � �ଠ� ������ ����� �� 㭨����� �� �⭮襭�� � �࠭ᯮ�⭮�
            �ଥ
            ��� �������筮�� ᮮ⢥��⢨� �ࠧ�������� ttAttr.HIER � mHier
            �ਬ�� ᮤ�ন����: ���㬥���������������
          */
      mHier = mHier + CHR(1) +
                      SUBSTRING(mTagSubStr,
                            INDEX(mTagSubStr,"<") + 1,
                           (INDEX(mTagSubStr," ") - INDEX(mTagSubStr,"<")) - 1).
      ELSE
                     /*�᫨ ⥣ ����뢠�騩, � 㤠�塞 ��᫥���� ⥣ �� mHier*/
         mHier = SUBSTRING(mHier,1,R-INDEX(mHier,CHR(1)) - 1).

      DO WHILE INDEX(mTagSubStr,'="') NE 0:
         ASSIGN
            mIndMidd = INDEX(mTagSubStr,'="')
            mIndRBrd =   INDEX(mTagSubStr,'"',mIndMidd + 2)
            mIndLBrd = R-INDEX(mTagSubStr,' ',mIndMidd) + 1
            mTag = SUBSTRING(mTagSubStr,mIndLBrd,    mIndMidd - mIndLBrd      )
            mVal = SUBSTRING(mTagSubStr,mIndMidd + 2,mIndRBrd - (mIndMidd + 2))
            mTagSubStr = SUBSTRING(mTagSubStr,mIndRBrd + 1,LENGTH(mTagSubStr) )
         NO-ERROR.
       
         IF mTag EQ "�������" THEN
            mVal = SUBSTRING(mVal,1,1) + "77".

         IF mTag EQ "��⠑���" THEN
            mVal = STRING(TODAY,"99.99.9999").
            
         IF mTag EQ "���ᔮ�" THEN
            mVal = "5.11".
            
         FIND FIRST ttAttr WHERE ttAttr.xattr-code EQ mTag  AND
                                 ttAttr.HIER       EQ mHier NO-LOCK NO-ERROR.
         IF AVAIL ttAttr THEN
         RUN SetValue in h_exch (mTaxExp,
                                    GetMangledName(ttAttr.xattr-clabel),mVal, 0)
         NO-ERROR.
      END.
                     /*�᫨ ⥣ ����뢠����, � 㤠�塞 ��᫥���� ⥣ �� mHier*/
      IF INDEX(mTagSubStr,"/>") NE 0 THEN
         mHier = SUBSTRING(mHier,1,R-INDEX(mHier,CHR(1)) - 1).
   END.

   RUN SetValue in h_exch (mTaxExp,"mail-format",mMailFormat, 0).

RETURN.

{intrface.del}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='03/04/2015 15:18:53.652+04:00' */
/* $LINTUSER='zhua' */
/* $LINTMODE='1' */
/* $LINTFILE='tax-parse-tag.p' */
/*prosignMA0pTAfbAsz48JDm5rdi3g*/