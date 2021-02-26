/*
{globals.i}           / ** �������� ��।������ * /
{intrface.get xclass} / * �㭪樨 ��� ࠡ��� � ����奬�� * /
*/
/* ========================================================================= */
FUNCTION RegGNI RETURNS CHARACTER
   (INPUT cR AS CHARACTER):

   IF    (cR EQ ?)
      OR (cR EQ "")
      OR (cR EQ "77")
      OR (cR EQ "78")
      OR (cR EQ "0")
      OR (cR EQ "00000")
      OR (cR EQ "00040")
      OR (cR EQ "00045")
   THEN
      RETURN "".
   ELSE
      CASE LENGTH(cR):
         WHEN 2 THEN
            RETURN REPLACE(REPLACE(GetCodeName ("���������", cR), "�������", "���."), "��⮭���� ����", "��") + ",".
         WHEN 5 THEN
            RETURN REPLACE(REPLACE(GetCodeName ("������",    cR), "�������", "���."), "��⮭���� ����", "��") + ",".
         OTHERWISE
            RETURN "".
      END /* CASE */.
END.

/* ========================================================================= */
/* �८�ࠧ������ ���� � �ଠ� ����� � 㤮���⠥���� ����                   */
/* �ଠ� �����: ������,ࠩ��,��த,���.�㭪�,㫨�,���,�����,������,��஥��� */
/* ��祬 ���.2-5 ᮯ஢�������� ���������ﬨ �,�-�,� � �.�., � ���.6-9 ����������� ������ ��ࠬ� */
FUNCTION Kladr RETURNS CHARACTER
   (INPUT cReg AS CHARACTER, /* Country,GNI */
    INPUT cAdr AS CHARACTER):

   DEFINE VARIABLE cAdrPart AS CHARACTER EXTENT 9 INITIAL "".
   DEFINE VARIABLE cAdrKl   AS CHARACTER.
   DEFINE VARIABLE iI       AS INTEGER.
   DEFINE VARIABLE iNzpt    AS INTEGER.

   iNzpt = MINIMUM(NUM-ENTRIES(cAdr), 9).

   DO iI = 1 TO iNzpt:
      cAdrPart[iI] = ENTRY(iI, cAdr).
   END.

   IF (ENTRY(1, cReg) NE "RUS")
   THEN DO:
      DO iI = 5 TO MINIMUM(iNzpt, 9) :
         IF (cAdrPart[iI] NE "") THEN cAdrKl = TRIM(cAdrKl + "," + cAdrPart[iI], ",").
      END.

      DO iI = 4 TO 2 BY -1 :
         IF (cAdrPart[iI] NE "") THEN cAdrKl = TRIM(cAdrKl + "," + cAdrPart[iI], ",").
      END.

      FIND FIRST country
         WHERE (country.country-id EQ ENTRY(1, cReg))
         NO-LOCK NO-ERROR.
      IF (AVAIL country) THEN cAdrKl = TRIM(cAdrKl + "," + TRIM(country.country-name), ",").

      IF     (cAdrPart[1] NE "")
         AND (cAdrPart[1] NE "000000")
      THEN
         cAdrKl = TRIM(cAdrKl + "," + cAdrPart[1], ",").
   END.
   ELSE DO:
      IF (cAdrPart[5] NE "") THEN cAdrKl = cAdrPart[5].
      IF     (cAdrPart[6] NE "")
         AND (iNzpt GE 6)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[6], 1, 1)) NO-ERROR.
         cAdrKl = TRIM(cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",�.")
                + cAdrPart[6], ",").
      END.

      IF     (cAdrPart[9] NE "")
         AND (iNzpt EQ 9)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[9], 1, 1)) NO-ERROR.
         cAdrKl = TRIM(cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",���.")
                + cAdrPart[9], ",").
      END.

      IF     (cAdrPart[7] NE "")
         AND (iNzpt GE 7)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[7], 1, 1)) NO-ERROR.
         cAdrKl = TRIM(cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",���.")
                + cAdrPart[7], ",").
      END.

      IF     (cAdrPart[8] NE "")
         AND (iNzpt GE 8)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[8], 1, 1)) NO-ERROR.
         cAdrKl = TRIM(cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",��.")
                + cAdrPart[8], ",").
      END.

      DO iI = 4 TO 2 BY -1 :
         IF (cAdrPart[iI] NE "") THEN cAdrKl = TRIM(cAdrKl + "," + cAdrPart[iI], ",").
      END.

      cAdrKl = TRIM(cAdrKl + "," + RegGNI(ENTRY(2, cReg))
                  + (IF ((cAdrPart[1] NE "") AND (cAdrPart[1] NE "000000"))
                     THEN ("," + cAdrPart[1])
                     ELSE ""), ",").
   END.

   RETURN TRIM(cAdrKl, ",").
END.

/* ========================================================================= */
FUNCTION KlientAdr     RETURNS CHARACTER
   (INPUT  iCistCat AS CHARACTER,
    INPUT  iCistId  AS INTEGER,
    INPUT  iTypAdr  AS CHARACTER ):     /* ��� ����: � - �ਤ��᪨�, � - 䠪��᪨�, � - ���⮢� */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    FIND LAST cust-ident
        WHERE (cust-ident.cust-cat       EQ iCistCat)
          AND (cust-ident.cust-id        EQ iCistId)
          AND (cust-ident.cust-code-type EQ (IF (iTypAdr EQ "�") THEN (IF (iCistCat EQ "�") THEN "�������" ELSE "�������") ELSE 
                                            (IF (iTypAdr EQ "�") THEN (IF (iCistCat EQ "�") THEN "����ய" ELSE "�����")   ELSE "�������")))
          AND (cust-ident.class-code     EQ "p-cust-adr")
          AND (cust-ident.close-date     EQ ?)
        NO-ERROR.
    IF (AVAIL cust-ident)
    THEN DO:
        cTmp = cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num).
        cTmp = GetXAttrValue("cust-ident", cTmp, "country-id") + ","
             + GetXAttrValue("cust-ident", cTmp, "���������").
        RETURN Kladr(cTmp, cust-ident.issue).
    END.
    ELSE RETURN "".
END FUNCTION.
