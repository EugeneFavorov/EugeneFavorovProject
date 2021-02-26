
FUNCTION vf_tabldtl RETURN LOGICAL (INPUT iCat AS CHARACTER, INPUT iName AS CHARACTER):

DEFINE VARIABLE mIsNotEmpty AS LOGICAL NO-UNDO.
DEFINE VARIABLE mItog       AS DECIMAL NO-UNDO.

mIsNotEmpty = NO.


FIND FIRST  ttDetail WHERE ttDetail.fAcctCat EQ iCat NO-ERROR.

IF AVAILABLE ttDetail THEN
DO:

   PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
   PUT UNFORMATTED "�     " + iName + "                                                     �" SKIP.   
   PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
   mItog = 0.

   FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat:
      PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                   + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                   + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                   + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
      mIsNotEmpty = YES.
      mItog = mItog + ttDetail.fAmt.
   END.

   PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
   PUT UNFORMATTED "� �⮣�                                                    � " 
                 + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
      PUT UNFORMATTED "�     ���ᮢ� ���㬥���                                                          �" SKIP.   
      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
      mItog = 0.

      FOR EACH ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocKas:
         PUT UNFORMATTED "� " + STRING(ttDetail.fDocNum, "x(10)") 
                      + " � " + STRING(ttDetail.fAcctDb, "x(20)") 
                      + " � " + STRING(ttDetail.fAcctCr, "x(20)") 
                      + " � " + STRING(ttDetail.fAmt, ">,>>>,>>>,>>>,>>9.99") + " �" SKIP.   
         mIsNotEmpty = YES.
         mItog = mItog + ttDetail.fAmt.
      END.

      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
      PUT UNFORMATTED "� �⮣�                                                    � " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                         AND (NOT ttDetail.fDocRub) 
                         AND (NOT ttDetail.fDocDrg)
      NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:

      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
      PUT UNFORMATTED "�     �� ������ � ������⮩                                                    �" SKIP.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � �⮣�                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (NOT ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � �⮣�                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.   
      END.
   END.

   FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat AND ttDetail.fDocDrg NO-ERROR.

   IF AVAILABLE ttDetail THEN
   DO:
      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
      PUT UNFORMATTED "�     �� ������ � �ࠣ. ��⠫����                                              �" SKIP.   

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (NOT ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � ��壠���᪨�                                                      �" SKIP.   
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � �⮣�                                       � " 
                    + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.
      END.

      FIND FIRST ttDetail WHERE ttDetail.fAcctCat EQ iCat 
                            AND (NOT ttDetail.fDocRub) 
                            AND (ttDetail.fDocDrg)
                            AND (ttDetail.fDocKas)
         NO-ERROR.

      IF AVAILABLE ttDetail THEN
      DO:
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � ���ᮢ�                                                           �" SKIP.   
         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
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

         PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
         PUT UNFORMATTED "�            � �⮣�                                       � " 
                       + STRING(mItog, ">,>>>,>>>,>>>,>>9.99") +  " �" SKIP.
      END.
/*      PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.*/
   END.
END.


END FUNCTION.







IF mIsDetail THEN
DO:
PAGE.
PUT UNFORMATTED SKIP(2) SPACE(16) "�����஢�� ��壠���᪨� ���㬥�⮢ �� " end-date Format "99/99/9999" SKIP(1).

PUT UNFORMATTED "���������������������������������������������������������������������������������Ŀ" SKIP.
PUT UNFORMATTED "�   �����    �                ����� ��⮢                �     �㬬� (��.)     �" SKIP.   
PUT UNFORMATTED "� ���㬥��  ���������������������������������������������Ĵ                      �" SKIP. 
PUT UNFORMATTED "�            �      �� ������       �      �� �।���      �                      �" SKIP.   
PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.
PUT UNFORMATTED "�     1      �           2          �           3          �          4           �" SKIP.   
/*PUT UNFORMATTED "���������������������������������������������������������������������������������Ĵ" SKIP.*/

vf_tabldtl("b", "�� �����ᮢ� ��⠬   ").
vf_tabldtl("o", "�� ��������ᮢ� ��⠬").


PUT UNFORMATTED "�����������������������������������������������������������������������������������" SKIP.
END.


