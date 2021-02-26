PUT STREAM sfact UNFORMATTED
    PADL("�ਫ������ � 1",mLeng) SKIP
    PADL("� ���⠭������� �ࠢ�⥫��⢠",mLeng) SKIP
    PADL("���ᨩ᪮� �����樨",mLeng) SKIP
    PADL("�� 26 ������� 2011 �. � 1137",mLeng) SKIP
    PADL("(� ।. ���⠭������� �ࠢ�⥫��⢠ �� �� 19.08.2017 � 981)",mLeng)
    SKIP.

PUT STREAM sfact UNFORMATTED
   SKIP
   SPACE(mLengBody + mLs + 19) "����-������� � " + STRING(mSFNum,"x(15)") +  " �� <<" + (IF mSfDate <> ? THEN STRING(DAY(mSFDate),"99") ELSE "--") + ">>" + (IF mSfDate <> ? THEN (STRING(ENTRY(MONTH(mSFDate),mMonthes)) + "  " + STRING(YEAR(mSFDate)) + "�.") ELSE "     -") SKIP
   SPACE(mLengBody + mLs + 19) "               ���������������      ��  ����������������" SKIP 
.

IF mSFFixInfo =  "" THEN
DO:
   PUT STREAM sfact UNFORMATTED
      SKIP
      SPACE(mLengBody + mLs + 19) "����������� �  " + PADC("-", 15) +  " �� <<-->>" + "    -" SKIP
      SPACE(mLengBody + mLs + 19) "               ���������������      ��  ����������������" SKIP(1) 
   .
END.      
ELSE
DO mI = 1 TO NUM-ENTRIES(mSFFixInfo,";"):
   ASSIGN
      mSFFixNum  = TRIM(ENTRY(1,ENTRY(mI,mSFFixInfo,";")))  
      mSFFixDate = DATE(ENTRY(2,ENTRY(mI,mSFFixInfo,";")))
   NO-ERROR.
   PUT STREAM sfact UNFORMATTED
      SKIP
      SPACE(mLengBody + mLs + 19) "����������� �  " + STRING(mSFFixNum,"x(15)") +  " �� <<" + (IF mSFFixDate <> ? THEN STRING(DAY(mSFFixDate),"99") ELSE "--") + ">>" + (IF mSFFixDate <> ? THEN (STRING(ENTRY(MONTH(mSFFixDate),mMonthes)) + "  " + STRING(YEAR(mSFFixDate)) + "�.") ELSE "-") SKIP
      SPACE(mLengBody + mLs + 19) "               ���������������      ��  ����������������" SKIP(1) 
   .   
END.

{sf-print-wrap.i &w = "'�த����'" &v = mStrSeller     &und = YES &d = 80}
{sf-print-wrap.i &w = "'����'"    &v = mStrSellerAddr &und = YES &d = 80}
 
PUT STREAM sfact UNFORMATTED
  
   SPACE(mLengBody + mLs) "���/��� �த��� " + mSFSellerINN + "/" + mSFSellerKPP
   SKIP
   SPACE(mLengBody + mLs) "                ������������������������������������������������
   ����������������" SKIP
.

{sf-print-wrap.i &w = "'��㧮��ࠢ�⥫� � ��� ����'" &v = mStrOtprav &und = YES &d = 80}
{sf-print-wrap.i &w = "'��㧮�����⥫� � ��� ����'"  &v = mStrPoluch &und = YES &d = 80}

PUT STREAM sfact UNFORMATTED
   SPACE(mLengBody + mLs) {&STR1} + " " + mDocNumDate SKIP 
   SPACE(mLengBody + mLs) mDocNumLine SKIP
.

{sf-print-wrap.i &w = "'���㯠⥫�'"  &v = mStrBuyer     &und = YES &d = 80}
{sf-print-wrap.i &w = "'����'"       &v = mStrBuyerAddr &und = YES &d = 80}

PUT STREAM sfact UNFORMATTED

   SPACE(mLengBody + mLs) "���/��� ���㯠⥫� " + mSFBuyerINN +  if mSFBuyerKPP = '0' then "" else "/" + mSFBuyerKPP SKIP
   SPACE(mLengBody + mLs) "                  ��������������������������������������������������������������" SKIP
.

PUT STREAM sfact UNFORMATTED
   SPACE(mLengBody + mLs) "�����:" mSFCurrInfo SKIP
   SPACE(mLengBody + mLs) mSFCurrLine SKIP
.
PUT STREAM sfact UNFORMATTED
   SPACE(mLengBody + mLs) "�����䨪��� ���㤠��⢥����� ����ࠪ�, ������� (ᮣ��襭��) (�� ����稨):" SKIP
   SPACE(mLengBody + mLs) mIgk SKIP
   SPACE(mLengBody + mLs) FILL("�", 80) SKIP
.
IF CAN-FIND(FIRST ttServ NO-LOCK) 
THEN DO:
   PUT STREAM sfact UNFORMATTED
      /* 蠯�� ⠡���� */
      SPACE(mLengBody) "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
      SPACE(mLengBody) "������������� ⮢�� �      �      ������      �  ������⢮  �     ���� (���)    ��⮨����� ⮢�஢ �  � ⮬   ����������   �㬬� ������,  ��⮨����� ⮢�஢ �       ��࠭�       �   �������樮���     �" SKIP
      SPACE(mLengBody) "�(���ᠭ�� �믮����- � ���  �     ����७��     �   (��ꥬ)    �      �� �������     � (ࠡ��, ���),  �  �᫥   � �⠢��  �  �।�塞��   � (ࠡ��, ���),  �    �ந�宦�����   �        �����          �" SKIP
      SPACE(mLengBody) "���� ࠡ��, ��������� ���� �                   �              �      ����७��      �  �����⢥����   �  �㬬�   �         �    ���㯠⥫�    �  �����⢥����   �       ⮢��       �      ⠬�������       �" SKIP
      SPACE(mLengBody) "����), �����⢥�- �⮢���������������������Ĵ              �                     �    �ࠢ ���      �  ��樧�  �         �                  �      �ࠢ,       ��������������������Ĵ      ������樨       �" SKIP
      SPACE(mLengBody) "����� �ࠢ�          �      ���� �    �᫮����  �              �                     �  ������ - �ᥣ�  �          �         �                  �   � ������� -    ����    ��⪮�    �                       �" SKIP
      SPACE(mLengBody) "�                    �      �    �  ������祭�� �              �                     �                  �          �         �                  �      �ᥣ�       ������  ������������ �                       �" SKIP
      SPACE(mLengBody) "�                    �      �    �(��樮���쭮�)�              �                     �                  �          �         �                  �                  ���� �               �                       �" SKIP
      SPACE(mLengBody) "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
      SPACE(mLengBody) "�         1          �  1�  �  2 �     2�       �      3       �          4          �        5         �     6    �    7    �        8         �        9         � 10 �      10�      �          11           �" SKIP
      SPACE(mLengBody) "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
      .                                                                 
   ASSIGN
      mPriceSumm = 0
      mNalogSumm = 0
      mTotalSumm = 0
   .
   FOR EACH ttServ 
      NO-LOCK
      BREAK BY ttServ.NameServ:
      ASSIGN   
         mPriceSumm = mPriceSumm + ttServ.SummOut
         mNalogSumm = mNalogSumm + ttServ.NalogSumm
         mTotalSumm = mTotalSumm + ttServ.TotalSumm
         mStrNlog   = ""
         .
      IF ttServ.Nlog =  0 THEN
         mStrNlog = SplitStr("��� ������ (���)", LENGTH(mVATformat), '~n').
      ELSE DO:
         IF ttServ.Nlog    <  1 AND
            ttServ.Nlog    >  0 OR
            loan.cont-type =  "�/�"
         THEN DO:
            IF loan.cont-type =  "�/�" THEN
               mStrNlog = PADL("18/118", LENGTH(mVATformat)).
            ELSE
            DO:
               mX = (ttServ.Nlog * 100) / (1 - ttServ.Nlog).
               mX = ROUND(mX, 0).
               mStrNlog = PADL(STRING(mX, ">9") + "/" + STRING(mX + 100, ">>9"), LENGTH(mVATformat)).
            END.
         END.
         ELSE
            mStrNlog = STRING(ttServ.Nlog, mVATformat).
      END.

      PUT STREAM sfact UNFORMATTED
             SPACE(mLengBody) 
             "�" + STRING(ENTRY(1,ttServ.NameServ,'~n'),"x(20)") + 
             "�" + (IF ttServ.KodTov <> "" THEN STRING(ttServ.KodTov, "x(6)")
                                           ELSE "  -   ") + 
             "�" + (IF    ttServ.Edin    <> "" 
                      AND loan.cont-type <> "�/�" THEN STRING(ENTRY(1,ttServ.Edin,'~n'), "x(4)")
                                                  ELSE "  - ") +
             "�" + (IF    ttServ.Edin    <> "" 
                      AND loan.cont-type <> "�/�" THEN STRING(ttServ.EdinName, "x(14)")
                                                  ELSE "      -       ") +
             "�" + (IF    ttServ.Quant   <> 0 
                      AND loan.cont-type <> "�/�" THEN STRING(ttServ.Quant,">>>,>>>,>>9.99")
                                                  ELSE "      -       ") + 
             "�" + (IF    ttServ.Price <> 0 
                      AND loan.cont-type <> "�/�" THEN STRING(ttServ.Price,
                                                              ">>>,>>>,>>>,>>9.99" + 
                                                                (IF ttServ.rate-fixed =  '%'
                                                                 THEN "99%" ELSE "   "))
                                                  ELSE "          -          ")  +
             "�" + (IF loan.cont-type <> "�/�" THEN STRING(ttServ.SummOut,">>>,>>>,>>>,>>9.99") 
                                               ELSE "        -         ")   +
             "�" + (IF loan.cont-type =  "�/�" THEN "     -    " ELSE (IF ttServ.Akciz <> 0 THEN STRING(ttServ.Akciz,">>>>>>9.99")
                                         ELSE "��� ��樧�")) +
             "�" + STRING(ENTRY(1, mStrNlog, '~n'), "x(" + STRING(LENGTH(mVATformat)) + ")")  +
             "�" + (IF ttServ.Nlog <> 0 THEN STRING(ttServ.NalogSumm,">>>,>>>,>>>,>>9.99")
                                        ELSE " ��� ������(���)  ") +
             "�" + STRING(ttServ.TotalSumm,">>>,>>>,>>>,>>9.99") +
             "�" + (IF    ttServ.Contry  <> "" AND
                    (NOT CAN-DO(mDispReq, loan.cont-type)) THEN STRING(ttServ.Contry,"x(4)")
                                                  ELSE "  - ") +
             "�" + (IF    ttServ.ContryName <> "" AND
                    (NOT CAN-DO(mDispReq, loan.cont-type)) THEN STRING(ENTRY(1,ttServ.ContryName,"~n"),"x(15)")
                                                  ELSE "      -        ") +
             "�" + (IF    ttServ.GTDNum  <> "" AND
                    (NOT CAN-DO(mDispReq, loan.cont-type)) THEN STRING(ttServ.GTDNum,"x(23)") 
                                                  ELSE "          -            ") +
             "�" SKIP
          .
   
      /* ���⠥� ������������ ��㣨 �� ᫥����� ���窠�, 
      ** �᫨ ��� ��������� ������ */
      IF    NUM-ENTRIES(ttServ.NameServ,   "~n") >= 2
         OR NUM-ENTRIES(ttServ.contryName, "~n") >= 2
         OR NUM-ENTRIES(ttServ.Edin,       "~n") >= 2
         OR NUM-ENTRIES(mStrNlog,          "~n") >= 2
         THEN
      DO:
         mMaxRow = MAX(NUM-ENTRIES(ttServ.NameServ,"~n"),NUM-ENTRIES(ttServ.contry,"~n"),NUM-ENTRIES(ttServ.Edin,"~n"),NUM-ENTRIES(ttServ.ContryName,"~n"),NUM-ENTRIES(mStrNlog,"~n")).
         DO mI = 2 TO mMaxRow:

            PUT STREAM sfact UNFORMATTED
                 SPACE(mLengBody) 
                 "�" + ( IF NUM-ENTRIES(ttServ.NameServ,"~n") >= mI THEN STRING(ENTRY(mI,ttServ.NameServ,'~n'),"x(20)")
                                                                    ELSE FILL(" ",20) ) +
                 "�" + FILL(" ",6)  +
                 "�" + FILL(" ",4)  +                                                    
                 "�" + FILL(" ",14) +
                 "�" + FILL(" ",14) +
                 "�" + FILL(" ",21) +
                 "�" + FILL(" ",18) +
                 "�" + FILL(" ",10) +
                 "�" + ( IF NUM-ENTRIES(mStrNlog,"~n") >= mI THEN STRING(ENTRY(mI,mStrNlog,'~n'),"x(9)")
                                                             ELSE FILL(" ",9) ) +
                 "�" + FILL(" ",18) +
                 "�" + FILL(" ",18) +
                 "�" + FILL(" ",4)  +
                 "�" + ( IF NUM-ENTRIES(ttServ.ContryName,"~n") >= mI AND (NOT CAN-DO(mDispReq, loan.cont-type)) THEN STRING(ENTRY(mI,ttServ.ContryName,'~n'),"x(15)")
                                                                                                  ELSE FILL(" ",15) ) +
                 "�" + FILL(" ",23) +
                 "�" SKIP
            .
         END.
      END.
      IF NOT LAST(ttServ.NameServ) 
      THEN DO:
         PUT STREAM sfact UNFORMATTED
            SPACE(mLengBody)    "���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
            .
      END.
      ELSE DO:
            PUT STREAM sfact UNFORMATTED
               SPACE(mLengBody) "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP
               SPACE(mLengBody) "��ᥣ� � �����                                                                      �" + (IF loan.cont-type <> "�/�" THEN STRING(mPriceSumm,">>>,>>>,>>>,>>9.99") ELSE FILL(" ", 18)) + "�                    �" + STRING(mNalogSumm,">>>,>>>,>>>,>>9.99") + "�" + STRING(mTotalSumm,">>>,>>>,>>>,>>9.99") + "�" SKIP
               SPACE(mLengBody) "��������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP
               .
      END.
   END.
END.

{sf-print-sgnt.i}

/*   Filename: SF-PRINT_2N.I  --  E n d */
/* $LINTFILE='sf-print_2n.i' */
/* $LINTMODE='1,2,6,3' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq/' */
/* $LINTUSER='ozmi' */
/* $LINTDATE='19/09/2017 11:38:19.778+03:00' */
/*prosignN/bx/cO65DK7kGCepqZqug*/