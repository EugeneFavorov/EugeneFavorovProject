/*in-uni.p*/
/* Инкассовое поручение */
&IF "{&OFFSET}" EQ "NO" &THEN
      &SCOP OFFSET_VAL 0
&ELSE
      &SCOP OFFSET_VAL 1703
&ENDIF
&IF DEFINED(SET_DATA) &THEN
   DEFINE VAR lzr-Cond-Pay       AS CHARACTER
                                    EXTENT      3 NO-UNDO.
   DEFINE VAR lzr-Amt-Rub        AS CHARACTER
                                    EXTENT      3 NO-UNDO.
   DEFINE VAR lzr-Plat-Name      AS CHARACTER
                                    EXTENT      6 NO-UNDO.
   DEFINE VAR lzr-Plat-Bank      AS CHARACTER
                                    EXTENT      3 NO-UNDO.
   DEFINE VAR lzr-Pol-Name       AS CHARACTER
                                    EXTENT      6 NO-UNDO.
   DEFINE VAR lzr-Pol-Bank       AS CHARACTER
                                    EXTENT      3 NO-UNDO.
   DEFINE VAR lzr-Details        AS CHARACTER
                                    EXTENT      8 NO-UNDO.
   DEFINE VAR vInnPl             AS CHARACTER     NO-UNDO.
   DEFINE VAR vInnPo             AS CHARACTER     NO-UNDO.
   DEFINE VAR vKppPl             AS CHARACTER     NO-UNDO.
   DEFINE VAR vKppPo             AS CHARACTER     NO-UNDO.
   DEFINE VAR vI                 AS INT64       NO-UNDO.

   DEFINE VARIABLE vIsLongPolName AS LOGICAL NO-UNDO.
   DEFINE VARIABLE vIsLongPlaName AS LOGICAL NO-UNDO.

   vIsLongPolName = NO.
   vIsLongPlaName = NO.

   ASSIGN
      lzr-Amt-Rub[1]   = AmtStr[1] + " " +
                         AmtStr[2] + " " +
                         AmtStr[3].
      lzr-Plat-Name[1] = PlName[1] + " " +
                         PlName[2] + " " +
                         PlName[3] + " " +
                         PlName[4] + " " +
                         PlName[5].
      lzr-Plat-Bank[1] = PlRKC[1] + " " +
                         PlRKC[2].
      lzr-Pol-Bank[1]  = PoRKC[1] + " " +
                         PoRKC[2].
      lzr-Pol-Name[1]  = PoName[1] + " " +
                         PoName[2] + " " +
                         PoName[3] + " " +
                         PoName[4] + " " +
                         PoName[5].
      lzr-Details[1]   = Detail[1] + "|" +
                         Detail[2] + "|" +
                         Detail[3] + "|" +
                         Detail[4] + "|" +
                         Detail[5]
   .

   {wordwrap.i &s=lzr-Amt-Rub     &n=2 &l=63}
   {wordwrap.i &s=lzr-Plat-Bank   &n=2 &l=39}
   {wordwrap.i &s=lzr-Pol-Bank    &n=2 &l=39}

   IF FGetSetting("ИсклИНН","","Да") EQ "Да" AND lzr-Plat-Name[1] BEGINS "ИНН " THEN
   DO:
      vI = 4.
      
      DO WHILE SUBSTRING(lzr-Plat-Name[1],vI,1) EQ " ":
         vI = vI + 1.
      END.
      
      DO WHILE LOOKUP(SUBSTRING(lzr-Plat-Name[1],vI,1),"0,1,2,3,4,5,6,7,8,9") > 0 :
         vI = vI + 1.
      END.

      ASSIGN
         vInnPl           = trim(substring(lzr-Plat-Name[1],5,vI - 5))
         lzr-Plat-Name[1] = substring(lzr-Plat-Name[1],vI)
      . 
   END.

   IF FGetSetting("ИсклИНН","","Да") EQ "Да" AND lzr-Pol-Name[1] BEGINS "ИНН " THEN
   DO:
      vI = 4.
      
      DO WHILE SUBSTRING(lzr-Pol-Name[1],vI,1) EQ " " :
         vI = vI + 1.
      END.
      
      DO WHILE LOOKUP(SUBSTRING(lzr-Pol-Name[1],vI,1),"0,1,2,3,4,5,6,7,8,9") > 0 :
         vI = vI + 1.
      END.

      ASSIGN
         vInnPo           = trim(substring(lzr-Pol-Name[1],5,vI - 5))
         lzr-Pol-Name[1] = substring(lzr-Pol-Name[1],vI)
      . 
   END.

   IF lzr-Plat-Name[1] BEGINS "КПП " THEN
   DO:
      vI = 4.
      
      DO WHILE SUBSTRING(lzr-Plat-Name[1],vI,1) EQ " ":
         vI = vI + 1.
      END.
      
      DO WHILE LOOKUP(SUBSTRING(lzr-Plat-Name[1],vI,1),"0,1,2,3,4,5,6,7,8,9") > 0 :
         vI = vI + 1.
      END.

      ASSIGN
         vKppPl           = trim(substring(lzr-Plat-Name[1],5,vI - 5))
         lzr-Plat-Name[1] = substring(lzr-Plat-Name[1],vI)
      . 
   END.

   IF lzr-Pol-Name[1] BEGINS "КПП " THEN
   DO:
      vI = 4.
      
      DO WHILE SUBSTRING(lzr-Pol-Name[1],vI,1) EQ " " :
         vI = vI + 1.
      END.
      
      DO WHILE LOOKUP(SUBSTRING(lzr-Pol-Name[1],vI,1),"0,1,2,3,4,5,6,7,8,9") > 0 :
         vI = vI + 1.
      END.

      ASSIGN
         vKppPo           = trim(substring(lzr-Pol-Name[1],5,vI - 5))
         lzr-Pol-Name[1] = substring(lzr-Pol-Name[1],vI)
      . 
   END.
   {wordwrap.i &s=lzr-Plat-Name   &n=6 &l=39}
   {wordwrap.i &s=lzr-Pol-Name    &n=6 &l=39}
   {wordwrap.i &s=lzr-Details     &n=8 &l=70}
   ASSIGN
      lzr-Plat-Bank[2] = substring(lzr-Plat-Bank[2], 1,39)
      lzr-Pol-Bank[2]  = substring(lzr-Pol-Bank[2],  1,39)
   .

   IF LENGTH(lzr-Plat-Name[4]) GT 0 THEN
   DO:
      vIsLongPlaName = YES.

      lzr-Plat-Name[1] = lzr-Plat-Name[1] + " " +
                         lzr-Plat-Name[2] + " " +
                         lzr-Plat-Name[3] + " " +
                         lzr-Plat-Name[4] + " " +
                         lzr-Plat-Name[5] + " " +
                         lzr-Plat-Name[6].

      {wordwrap.i &s=lzr-Plat-Name   &n=6 &l=65}
   END.

   IF LENGTH(lzr-Pol-Name[4]) GT 0 THEN
   DO:
      vIsLongPolName = YES.

      lzr-Pol-Name[1] = lzr-Pol-Name[1] + " " +
                        lzr-Pol-Name[2] + " " +
                        lzr-Pol-Name[3] + " " +
                        lzr-Pol-Name[4] + " " +
                        lzr-Pol-Name[5] + " " +
                        lzr-Pol-Name[6].

      {wordwrap.i &s=lzr-Pol-Name   &n=6 &l=65}
   END.
&ENDIF
&IF DEFINED(PRINT) &THEN
/*Указание 1256*/   
   DEFINE VARIABLE vII AS INT64 NO-UNDO.

   RUN PUT_PCL_STR(187, 20 + {&OFFSET_VAL},
                   (IF op.ins-date EQ ? THEN ""
                                        ELSE string(op.ins-date, "99.99.9999")
                   ),
                                                                INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(620,  20 +   {&OFFSET_VAL}, mSpisPl,         INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(850,  200 +  {&OFFSET_VAL}, trim(op.doc-num),INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1141, 197 +  {&OFFSET_VAL}, trim(theDate),   INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(380,  310 +  {&OFFSET_VAL}, lzr-Amt-Rub[1],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(380,  360 +  {&OFFSET_VAL}, lzr-Amt-Rub[2],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1700, 197 +  {&OFFSET_VAL}, trim(paytype),   INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 485 +  {&OFFSET_VAL}, trim(rub),       INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(245,  485 +  {&OFFSET_VAL}, PlINN,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(825,  485 +  {&OFFSET_VAL}, PlKPP,           INPUT-OUTPUT buf-str).

   IF vIsLongPlaName THEN
      RUN PRINT_COMPRESSED(                                   INPUT-OUTPUT buf-str).

   RUN PUT_PCL_STR(145,  555 +  {&OFFSET_VAL}, lzr-Plat-Name[1],INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  600 +  {&OFFSET_VAL}, lzr-Plat-Name[2],INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  645 +  {&OFFSET_VAL}, lzr-Plat-Name[3],INPUT-OUTPUT buf-str).

   IF vIsLongPlaName THEN
      RUN PRINT_PITCH(                                        INPUT-OUTPUT buf-str).

   RUN PUT_PCL_STR(145,  775  + {&OFFSET_VAL}, lzr-Plat-Bank[1],INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  820  + {&OFFSET_VAL}, lzr-Plat-Bank[2],INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 662 +  {&OFFSET_VAL}, SUBSTRING(pllacct,1,25),
                                                                INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 783 +  {&OFFSET_VAL}, plmfo,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 840 +  {&OFFSET_VAL}, SUBSTRING(plCAcct,1,25),
                                                                INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 958  + {&OFFSET_VAL}, pomfo,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 1013 + {&OFFSET_VAL}, SUBSTRING(pocacct,1,25),
                                                                INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 1131 + {&OFFSET_VAL}, SUBSTRING(poacct,1,25),
                                                                INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(245,  1133 + {&OFFSET_VAL}, PoINN,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(825,  1133 + {&OFFSET_VAL}, PoKPP,           INPUT-OUTPUT buf-str).

   IF vIsLongPolName THEN
      RUN PRINT_COMPRESSED(                                   INPUT-OUTPUT buf-str).

   RUN PUT_PCL_STR(145,  1200 + {&OFFSET_VAL}, lzr-Pol-Name[1], INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1245 + {&OFFSET_VAL}, lzr-Pol-Name[2], INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1290 + {&OFFSET_VAL}, lzr-Pol-Name[3], INPUT-OUTPUT buf-str).

   IF vIsLongPolName THEN
      RUN PRINT_PITCH(                                        INPUT-OUTPUT buf-str).

   RUN PUT_PCL_STR(145,  965  + {&OFFSET_VAL}, lzr-Pol-Bank[1], INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145, 1000  + {&OFFSET_VAL}, lzr-Pol-Bank[2], INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505, 1255 + {&OFFSET_VAL},
                   (IF avail(doc-type) THEN doc-type.digital
                                       ELSE "er"),              INPUT-OUTPUT buf-str).

   IF INT64(op.order-pay) GT 0 THEN
      RUN PUT_PCL_STR(1975, 1283 + {&OFFSET_VAL}, STRING(INT64(op.order-pay)),
                                                                INPUT-OUTPUT buf-str).

   IF LENGTH(mUIN1) GT 7 THEN
      RUN PRINT_ELITE(                                             INPUT-OUTPUT buf-str).

   RUN PUT_PCL_STR(1505,  1370 +  {&OFFSET_VAL}, mUIN1,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505,  1420 +  {&OFFSET_VAL}, mUIN2,           INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1505,  1470 +  {&OFFSET_VAL}, mUIN3,           INPUT-OUTPUT buf-str).

   IF LENGTH(mUIN1) GT 7 THEN
      RUN PRINT_PITCH(                                        INPUT-OUTPUT buf-str).

   RUN PRINT_ELITE(                                             INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1585 + {&OFFSET_VAL}, Detail[1],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1635 + {&OFFSET_VAL}, Detail[2],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1685 + {&OFFSET_VAL}, Detail[3],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1735 + {&OFFSET_VAL}, Detail[4],  INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(145,  1785 + {&OFFSET_VAL}, Detail[5],  INPUT-OUTPUT buf-str).
   RUN PRINT_PITCH(                                             INPUT-OUTPUT buf-str).

   /*Новые поля*/
   RUN PUT_PCL_STR(2185, 197  + {&OFFSET_VAL}, mPokST,          INPUT-OUTPUT buf-str).
   RUN PRINT_ELITE(                                             INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(150,  1525 + {&OFFSET_VAL}, mKBK,            INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(690,  1525 + {&OFFSET_VAL}, mOKATO,          INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1030, 1525 + {&OFFSET_VAL}, mPokOp,          INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1157, 1525 + {&OFFSET_VAL}, mPokNP,          INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1440, 1525 + {&OFFSET_VAL}, mPokND,          INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1855, 1525 + {&OFFSET_VAL}, mPokDD,          INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(2170, 1525 + {&OFFSET_VAL}, mPokTP,          INPUT-OUTPUT buf-str).
   
   
   RUN PRINT_PITCH(                                             INPUT-OUTPUT buf-str).
/*   
   RUN PRINT_ELITE(                                             INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(2016, 1888 + {&OFFSET_VAL}, "получателя",    INPUT-OUTPUT buf-str).
   RUN PRINT_PITCH(                                             INPUT-OUTPUT buf-str).
*/
   RUN PUT_PCL_STR(1855, 2061 + {&OFFSET_VAL}, mDateMarcRec,    INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(1855, 2466 + {&OFFSET_VAL}, TRIM(STRING(mDateCart,"99.99.9999")),   INPUT-OUTPUT buf-str).
   RUN PUT_PCL_STR(900, 2100 + {&OFFSET_VAL}, mSposobPoluch,   INPUT-OUTPUT buf-str).
   RUN PRINT_COMPRESSED(INPUT-OUTPUT buf-str).
   DO vII = 1 TO EXTENT(mPrnStr-El-Doc):
       RUN PUT_PCL_STR(1750,
                       2650 + 50 * (vII - 1) + {&OFFSET_VAL},
                       TRIM(mPrnStr-El-Doc[vII]),
                       INPUT-OUTPUT buf-str).
   END.
   RUN PRINT_PITCH(INPUT-OUTPUT buf-str).
&ENDIF
