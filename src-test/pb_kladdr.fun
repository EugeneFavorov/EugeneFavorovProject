/*
{globals.i}           / ** Глобальные определения * /
{intrface.get xclass} / * Функции для работы с метасхемой * /
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
            RETURN REPLACE(REPLACE(GetCodeName ("КодРегГНИ", cR), "область", "обл."), "автономный округ", "АО") + ",".
         WHEN 5 THEN
            RETURN REPLACE(REPLACE(GetCodeName ("КодРег",    cR), "область", "обл."), "автономный округ", "АО") + ",".
         OTHERWISE
            RETURN "".
      END /* CASE */.
END.

/* ========================================================================= */
/* Преобразование адреса в формате КЛАДР к удобочитаемому виду                   */
/* формат КЛАДР: индекс,район,город,нас.пункт,улица,дом,корпус,квартира,строение */
/* причем поз.2-5 сопровождаются дополнениями г,р-н,ул и т.д., а поз.6-9 заполняются одними цифрами */
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
      FIND FIRST country
         WHERE (country.country-id EQ ENTRY(1, cReg))
         NO-LOCK NO-ERROR.
      cAdrKl = IF (AVAIL country) THEN TRIM(country.country-name) ELSE "".

      IF     (cAdrPart[1] NE "")
         AND (cAdrPart[1] NE "000000")
      THEN
         cAdrKl = TRIM(cAdrKl + "," + cAdrPart[1], ",").

      DO iI = 2 TO MINIMUM(iNzpt, 9) :
         IF (cAdrPart[iI] NE "") THEN cAdrKl = cAdrKl + "," + cAdrPart[iI].
      END.
   END.
   ELSE DO:
      cAdrKl = TRIM((IF ((cAdrPart[1] NE "") AND (cAdrPart[1] NE "000000"))
                     THEN (cAdrPart[1] + ",")
                     ELSE "")
                   + RegGNI(ENTRY(2, cReg)), ",").
      DO iI = 2 TO MINIMUM(iNzpt, 5) :
         IF (cAdrPart[iI] NE "") THEN cAdrKl = cAdrKl + "," + cAdrPart[iI].
      END.

      IF     (cAdrPart[6] NE "")
         AND (iNzpt GE 6)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[6], 1, 1)) NO-ERROR.
         cAdrKl = cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",д.")
                + cAdrPart[6].
      END.

      IF     (cAdrPart[9] NE "")
         AND (iNzpt EQ 9)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[9], 1, 1)) NO-ERROR.
         cAdrKl = cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",стр.")
                + cAdrPart[9].
      END.

      IF     (cAdrPart[7] NE "")
         AND (iNzpt GE 7)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[7], 1, 1)) NO-ERROR.
         cAdrKl = cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",корп.")
                + cAdrPart[7].
      END.

      IF     (cAdrPart[8] NE "")
         AND (iNzpt GE 8)
      THEN DO:
         iI = INTEGER(SUBSTRING(cAdrPart[8], 1, 1)) NO-ERROR.
         cAdrKl = cAdrKl
                + (IF (ERROR-STATUS:ERROR) THEN "," ELSE ",кв.")
                + cAdrPart[8].
      END.
   END.

   RETURN TRIM(cAdrKl, ",").
END.

/* ========================================================================= */
FUNCTION KlientAdr     RETURNS CHARACTER
   (INPUT  iCistCat AS CHARACTER,
    INPUT  iCistId  AS INTEGER,
    INPUT  iTypAdr  AS CHARACTER ):     /* Тип адреса: Ю - юридический, Ф - фактический, П - почтовый */

    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

    FIND LAST cust-ident
        WHERE (cust-ident.cust-cat       EQ iCistCat)
          AND (cust-ident.cust-id        EQ iCistId)
          AND (cust-ident.cust-code-type EQ (IF (iTypAdr EQ "П") THEN (IF (iCistCat EQ "Ч") THEN "АдрУвед" ELSE "АдрПочт") ELSE 
                                            (IF (iTypAdr EQ "Ю") THEN (IF (iCistCat EQ "Ч") THEN "АдрПроп" ELSE "АдрЮр")   ELSE "АдрФакт")))
          AND (cust-ident.class-code     EQ "p-cust-adr")
          AND (cust-ident.close-date     EQ ?)
        NO-ERROR.
    IF (AVAIL cust-ident)
    THEN DO:
        cTmp = cust-ident.cust-code-type + ',' + cust-ident.cust-code + ',' + STRING(cust-ident.cust-type-num).
        cTmp = GetXAttrValue("cust-ident", cTmp, "country-id") + ","
             + GetXAttrValue("cust-ident", cTmp, "КодРегГНИ").
        RETURN Kladr(cTmp, cust-ident.issue).
    END.
    ELSE RETURN "".
END FUNCTION.
