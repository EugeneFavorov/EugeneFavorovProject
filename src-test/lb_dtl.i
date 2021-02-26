
FUNCTION vf_tabldtl RETURN LOGICAL (INPUT iCat AS CHARACTER, INPUT iName AS CHARACTER):

DEFINE VARIABLE mIsNotEmpty AS LOGICAL NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.

mIsNotEmpty = NO.


FIND FIRST  ttDetail WHERE ttDetail.fAcctCat EQ iCat NO-ERROR.

IF AVAILABLE ttDetail THEN
DO:

   PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
   PUT UNFORMATTED "�     " + iName + "                                                     �" SKIP.   
   PUT UNFORMATTED "쳐컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
   mItog = 0.

   FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat:
      PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                   + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                   + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                   + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
      mIsNotEmpty = YES.
      mItog = mItog + ttDetail.fAmt.
   END.

   PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
   PUT UNFORMATTED "� 댿�．                                                    � " 
                 + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
      PUT UNFORMATTED "�     뒥遜��瑜 ㄾゃД�瞬                                                          �" SKIP.   
      PUT UNFORMATTED "쳐컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
      mItog = 0.

      FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas:
         PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                      + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                      + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                      + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
         mIsNotEmpty = YES.
         mItog = mItog + ttDetail.fAmt.
      END.

      PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
      PUT UNFORMATTED "� 댿�．                                                    � " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                         AND (NOT ttDetail.fDocRub) 
                         AND (NOT ttDetail.fDocDrg)
      NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
      PUT UNFORMATTED "�     룼 ����졿⑨� � Þ쥯ヮ獸�                                                    �" SKIP.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "쳐컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 곥鶯젷收褻え�                                                      �" SKIP.   
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (NOT ttDetail.fDocDrg) 
                             AND (NOT ttDetail.fDocKas):
            PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 댿�．                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 뒥遜��瑜                                                           �" SKIP.   
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (NOT ttDetail.fDocDrg) 
                             AND (ttDetail.fDocKas):
            PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 댿�．                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
      END.
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocDrg NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:
      PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
      PUT UNFORMATTED "�     룼 ����졿⑨� � ㅰ젫. Д�젷쳽Ж                                              �" SKIP.   

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "쳐컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 곥鶯젷收褻え�                                                      �" SKIP.   
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (ttDetail.fDocDrg) 
                             AND (NOT ttDetail.fDocKas):
            PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 댿�．                                       � " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 뒥遜��瑜                                                           �" SKIP.   
         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         mItog = 0.

         FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat
                             AND (NOT ttDetail.fDocRub) 
                             AND (ttDetail.fDocDrg) 
                             AND (ttDetail.fDocKas):
            PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                         + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                         + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                         + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
            mIsNotEmpty = YES.
            mItog = mItog + ttDetail.fAmt.
         END.

         PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
         PUT UNFORMATTED "�            � 댿�．                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.
      END.
/*      PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.*/
   END.
END.


END FUNCTION.







IF mIsDetail THEN
DO:
PAGE.
PUT UNFORMATTED SKIP(2) SPACE(16) "맆訟ⓧ昔˚� ▲鶯젷收褻え� ㄾゃД�獸� 쭬 " end-date Format "99/99/9999" SKIP(1).

PUT UNFORMATTED "旼컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
PUT UNFORMATTED "�   뜮Д�    �                뜮Д�� 淞β��                �     묆К� (說�.)     �" SKIP.   
PUT UNFORMATTED "� ㄾゃД���  쳐컴컴컴컴컴컴컴컴컴컴컫컴컴컴컴컴컴컴컴컴컴컴�                      �" SKIP. 
PUT UNFORMATTED "�            �      룼 ㄵ‥栒       �      룼 むⅳⓥ�      �                      �" SKIP.   
PUT UNFORMATTED "쳐컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴탠컴컴컴컴컴컴컴컴컴컴컵컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
PUT UNFORMATTED "�     1      �           2          �           3          �          4           �" SKIP.   
/*PUT UNFORMATTED "쳐컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.*/

vf_tabldtl("b", "룼 줎쳽�貰�臾 淞β젹   ").
vf_tabldtl("o", "룼 ˛ⅰ젷젺貰�臾 淞β젹").


PUT UNFORMATTED "읕컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컨컴컴컴컴컴컴컴컴컴컴컴�" SKIP.
END.


