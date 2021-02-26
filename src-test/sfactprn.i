/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2006 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: SFACTPRN.I
      Comment: ����⢥��� ����� ��⮢-䠪���
   Parameters:
         Uses:
      Used by: SFACTPRN.I
      Created: 06.05.2006 ZIAL
     Modified: 18/05/2006 ZIAL (0059334) ���. �������� ��� �� �믮������ ࠡ��/��� 
               �� ����-䠪���
     Modified: 24/07/2006 ZIAL (0059334) ���. �������� ��� �� �믮������ ࠡ��/��� 
               �� ����-䠪���
     Modified: 20.03.2007 17:38 OZMI     (0070614)
*/

mIspolnit = "��, ���������ᠢ訥��, " + (IF mSFSellerFIO NE "" THEN mSFSellerFIO
                                                               ELSE mSFSeller) +
            " �।�⠢�⥫� �����������, � ����� ��஭�".

IF LENGTH(mIspolnit) GT 100 THEN
   mIspolnit = SplitStr(mIspolnit,
                        100,
                        "~n").

mContrAgent = IF mSFBuyerFIO NE "" THEN mSFBuyerFIO
                                   ELSE mSFBuyer.
mContrAgent = mContrAgent + " �।�⠢�⥫� ��������� � ��㣮� ��஭�, ��⠢��� �����騩 ���, ~
� ⮬, �� ����������� �믮����, � �������� �ਭ� ᫥���騥 ࠡ���: ".

IF LENGTH(mContrAgent) GT 100 THEN
   mContrAgent = SplitStr(mContrAgent,
                          100,
                          "~n").

PUT STREAM sfact UNFORMATTED
/*   SKIP(5) */
   SPACE(20) "��� �" + STRING(mSFNum,"x(14)") + 
   "�� " + 
   (IF mSfDate NE ? THEN 
       STRING(DAY(mSFDate),"99") + " " + 
       STRING(ENTRY(MONTH(mSFDate),Monthes)) + " " + 
       STRING(YEAR(mSFDate)) + "�." 
    ELSE "") 
   SKIP 
   SPACE(25) "�� �믮������ ࠡ��-���." SKIP(1)
   SPACE(5) "�᭮�����  : " + 

   IF mAxdNum EQ ? OR mAxdNum EQ "" THEN 
      ("���-䠪��� �" + 
      mSFNum +
      "  �� " + 
      (IF mSFDate NE ? THEN 
          STRING(mSFDate)
       ELSE ""))
   ELSE 
      ("������� �" + 
      mAxdNum + 

      "  �� " + 
      (IF mAxdDate NE ? THEN 
          STRING(mAxdDate)
       ELSE ""))

   SKIP

   SPACE(5) mIspolnit SKIP
   "� " + mContrAgent SKIP.


IF CAN-FIND(FIRST ttServ NO-LOCK) 
THEN DO:

   PUT STREAM sfact UNFORMATTED

      "����������������������������������������������������������������������������������������������������������Ŀ" SKIP
      "�   �                                        �                  �             �         �                  �" SKIP
      "�  ��              ������������              �       ����       �   ���.-��   � ��.���. �       �㬬�      �" SKIP
      "�   �                                        �                  �             �         �                  �" SKIP
      "����������������������������������������������������������������������������������������������������������Ĵ" SKIP
   .

   ASSIGN
      mNalogSumm = 0
      mTotalSumm = 0

      mSummOut = 0
      mN = 0.
   .
   FOR EACH ttServ 
      NO-LOCK
      BREAK BY ttServ.NameServ:
      ASSIGN   
         mTotalSumm = mTotalSumm + ttServ.TotalSumm
         mSummOut = mSummOut + ttServ.SummOut
         mN = mN + 1
         mSummNalogSumm = mSummNalogSumm + ttServ.NalogSumm.
      .
      mNameStr[1] = TRIM(REPLACE(ttServ.NameServ, "~n", " ")).

      {wordwrap.i &s = mNameStr &l = 40 &n = 10}

      /* ���⠥� ����� �� ��㣥 */
      PUT STREAM sfact UNFORMATTED
             "�" + STRING(mN,">>9") +
             "�" + STRING(mNameStr[1], "x(40)") +
             "�" + STRING(ttServ.Price,">>>,>>>,>>>,>>9.99") +
             "�" + STRING(ttServ.Quant,">>>>>>>>>9.99") +
             "�" + STRING(ttServ.Edin, "x(9)") +
             "�" + STRING(ttServ.SummOut,">>>,>>>,>>>,>>9.99") +
             "�" SKIP.

      /* ���⠥� ������������ ��㣨 �� ᫥����� ���窠�, 
      ** �᫨ ��� ��������� ������ */
      DO mI = 2 TO 10:
         IF NOT {assigned mNameStr[mI]} THEN LEAVE.

         PUT STREAM sfact UNFORMATTED
            "�" + FILL(" ",3)   +
            "�" + STRING(mNameStr[mI], "x(40)") +
            "�" + FILL(" ",18)  +
            "�" + FILL(" ",13)  +
            "�" + FILL(" ",9)   +
            "�" + FILL(" ",18)  +
            "�" SKIP
         .
      END.

      IF NOT LAST(ttServ.NameServ) 
      THEN DO:
         PUT STREAM sfact UNFORMATTED 
      "����������������������������������������������������������������������������������������������������������Ĵ" SKIP
         .

      END.
      ELSE DO:

         RUN x-amtstr.p (
                         INPUT mTotalSumm,
                         INPUT "",
                         INPUT NO,
                         INPUT NO,
                         OUTPUT mSummOutRub,
                         OUTPUT mSummOutCop
                        ).


         PUT STREAM sfact UNFORMATTED 
      "����������������������������������������������������������������������������������������������������������Ĵ" SKIP
      "                                                                                  �⮣�:�" + STRING(mSummOut,">>>,>>>,>>>,>>9.99") + "�" SKIP
      "                                                                                        ������������������Ĵ" SKIP
      "                                                                              �⮣� ���:�" + STRING(mSummNalogSumm,">>>,>>>,>>>,>>9.99") + "�" SKIP
      "                                                                                        ������������������Ĵ" SKIP
      "                                                                   �ᥣ� (� ��⮬ ���):�" + STRING(mTotalSumm,">>>,>>>,>>>,>>9.99") + "�" SKIP
      "                                                                                        ��������������������" SKIP
      "�⮣���� �㬬� � �����: " + mSummOutRub + "�㡫��  " + mSummOutCop + "������ , � ⮬ �᫥, " SKIP
         .
      END.
   END.

   mSummNalogSumm = 0.

   FOR EACH ttServ 
      NO-LOCK
      BREAK BY ttServ.Nlog:
      mSummNalogSumm = mSummNalogSumm + ttServ.NalogSumm.
      IF LAST-OF(ttServ.Nlog) THEN
      DO:
         PUT STREAM sfact UNFORMATTED
            "��� " + STRING(ttServ.Nlog) + "% - " + 
            TRIM(REPLACE(STRING(mSummNalogSumm,"->>>>>>>>9.99"),"~."," �� ")) + 
            " ���" + (IF NOT LAST(ttServ.Nlog) THEN ", " ELSE ".") .
         ASSIGN 
            mNalogSumm = 0
            mSummNalogSumm = 0
         .

      END.

   END.

   PUT STREAM sfact UNFORMATTED
      SKIP
      "������ �믮����� � ������ ��ꥬ�, � ��⠭������� �ப� � � �������騬 ����⢮�. ��஭�" SKIP
      "��⥭��� ��� � ���� �� �����.".

   DISPLAY  STREAM sfact
      mSFSeller VIEW-AS EDITOR SIZE 30 BY 2 LABEL "�ᯮ���⥫�"  SPACE(10)
      mSFBuyer  VIEW-AS EDITOR SIZE 30 BY 2 LABEL "�����稪" SKIP
   WITH FRAME a WIDTH 110 SIDE-LABELS
   .

   DISPLAY  STREAM sfact
      mSFSellerAddr LABEL "����" VIEW-AS EDITOR SIZE 35 BY 2 SPACE(11)
      mSFBuyerAddr LABEL "����" VIEW-AS EDITOR SIZE 35 BY 2 SKIP
   WITH FRAME a WIDTH 110 SIDE-LABELS.

   PUT STREAM sfact UNFORMATTED
      "���: " + mSFSellerINN  SPACE(30)
      "        ���: " STRING(mSFBuyerINN,"x(22)") 
    /*  "       ���: " STRING(mSFBuyerKPP,"x(22)") SKIP*/

    /*  "        ���: " STRING(mSFSellerKPP,"x(22)")*/ 
      SKIP(2)
   .

   PUT STREAM sfact UNFORMATTED
      "       ___________________" mSFSellerFIO SPACE(34)
      "___________________" mSFBuyerFIO
      SKIP(1)
      .
   /*
   DISPLAY  STREAM sfact
      "       ___________________" mSFSellerFIO VIEW-AS EDITOR SIZE 22 BY 2 NO-LABELS SPACE(8)
      "___________________" mSFBuyerFIO VIEW-AS EDITOR SIZE 22 BY 2 NO-LABELS 
   WITH FRAME b WIDTH 110 SIDE-LABELS.
   */

END.
