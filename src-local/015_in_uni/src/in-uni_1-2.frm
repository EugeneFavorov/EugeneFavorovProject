/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1998 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-new.frm
      Comment: ��ଠ ���⥦���� ����祭��
   Parameters:
         Uses:
      Used by: pp-new.p pp-new1.p
      Created: 09.11.1999 Kostik
     Modified: 10.02.2000 Kostik �����㪨� �� �� 691-�
     Modified: 18/12/2003 kraw 0023749 ��� ���஭���� �����ᮢ��� ����祭��
     Modified: 17/11/2010 kraa 0132710 ��ࠢ����� ����ୠ� �ਭ� ��� 
     Modified: 27/05/2014 kraw (0227236) ��� ����� ���� ����� 20 ᨬ�����
*/
form
&IF DEFINED(in-el) NE 0 &THEN 
/* ��� ���஭���� �����ᮢ��� ����祭�� */
{{&in-el} &in-frame=YES}
&ENDIF
           op.ins-date FORMAT "99.99.9999" AT 5 mSpisPl FORMAT "x(10)" AT 27      "���������Ŀ" at 75  skip
     "��������������������" AT 1        "��������������������"                    "�" at 75 NumberForm format "x(7)" "�"     skip
     "�����. � ���� ����." AT 1        "C��ᠭ� � ��. ����."                    "�����������" AT 75 skip(2)

                                                                                                                 "��Ŀ" AT 75
     NameOrder Format "x(22)" op.doc-num Format "x(7)" theDate Format "x(10)" AT 35 PayType Format "x(16)" AT 52 "�" AT 75 SPACE(0) mPokST FORMAT "x(2)" SPACE(0) "�"
     "                               ����������������    �����������������"                                       "����" AT 75
     "                                     ���             ��� ���⥦� "             SKIP
     "�㬬�    �" AmtStr[1] Format "x(71)"  skip
     "�ய���� �" AmtStr[2] Format "x(71)"  skip
     "         �" AmtStr[3] Format "x(71)"  skip
     "         �"                           skip
     "��������������������������������������������������������������������������������������" skip
     "���" PlINN FORMAT "x(18)" "����" plKPP FORMAT "x(18)"    "�      �" skip
     "����������������������������������������������Ĵ      �"                               SKIP
        PlName[1] Format "x(46)"                    "��㬬� �" Rub Format "x(15)" "" skip
        PlName[2] Format "x(46)"                    "���������������������������������������" skip
        PlName[3] Format "x(46)"                    "���.N  �" PlLAcct Format "x(25)" "" skip
        PlName[4] Format "x(46)"                    "�      �" skip
     "���⥫�騪                                     �      �" skip
     "�����������������������������������������������������Ĵ" skip
        PlRKC[1] Format "x(46)"          "����   �" PlMFO Format "x(25)" "" skip
        PlRKC[2] Format "x(46)"          "������Ĵ" skip
     "���� ���⥫�騪�                               ���.N  �" PlCAcct Format "x(25)" "" skip
     "��������������������������������������������������������������������������������������" skip
        PoRKC[1] Format "x(46)"          "����   �" PoMFO Format "x(25)" "" skip
        PoRKC[2] Format "x(46)"          "������Ĵ" skip
     "���� �����⥫�                                ���.N  �" PoCAcct Format "x(25)" "" skip
     "�����������������������������������������������������Ĵ" skip
     "���" PoINN FORMAT "x(18)" "����" poKPP FORMAT "x(18)"    "�      �" skip
     "����������������������������������������������Ĵ      �"                                SKIP
      PoName[1] Format "x(46)"                      "���.N  �" PoAcct Format "x(25)" "" skip
      PoName[2] Format "x(46)"                      "���������������������������������������" skip
      PoName[3] Format "x(46)"                      "���� ���" op.doc-type Format "x(2)" "       ����.����" op.order-pay Format "x(2)" "" skip
     "                                               ������Ĵ           �         �" SKIP
      PoName[4] Format "x(46)"                      "����.���           ���������Ĵ" SKIP
     "                                               ������Ĵ           �         �" SKIP
     "                                               �      �" mUIN1 format "x(9)" "�         �" skip
     "                                               ����   �" mUIN2 format "x(9)" "����.���� �" skip
     "�����⥫�                                     �      �" mUIN3 format "x(9)" "�         �" skip
     "��������������������������������������������������������������������������������������" skip
     mKBK FORMAT "x(20)�" mOKATO FORMAT "x(11)" "�" mPokOp FORMAT "x(2)" "�"  mPokNP FORMAT "x(10)"  "�" SPACE(0) mPokND FORMAT "x(15)" SPACE(0) "�" SPACE(0) mPokDD FORMAT "x(10)" SPACE(0) "�" SPACE(0) mPokTP FORMAT "x(2)"
     "��������������������������������������������������������������������������������������"

        Detail[1] Format "x(80)" "" skip
        Detail[2] Format "x(80)" "" skip
        Detail[3] Format "x(80)" "" skip
        Detail[4] Format "x(80)" "" skip
        Detail[5] Format "x(80)" "" skip
     "�����祭�� ���⥦�" skip
     "��������������������������������������������������������������������������������������" skip
     "                        ������                          �⬥⪨ �����" 
/*&IF DEFINED(in-el) NE 0 &THEN */
"�����⥫�" skip
mDateMarcRec FORMAT "x(10)" AT 65
/*&ELSE
skip(1)
&ENDIF*/
skip
     mSposobPoluch FORMAT "x(20)" AT 27 SKIP
     "                 �����������������������������" skip
     "      �.�." skip(1)
     "                 �����������������������������" skip
     with width 90 no-labels frame out-doc.

FORM
  "����������������������������������������������������������    ��� ����饭��" at 1 skip
  "N �.�N ���Ⳅ��      ��㬬� ����-��㬬� ���⪠��������      � ����⥪�" at 1 skip
  "���ⳮथ࠳����.     �����         ����⥦�      �" at 1 skip
  "    �      ��थ�    ����⥦�      �             �             " AT 1 mDateCart FORMAT "99.99.9999" SKIP
  "����������������������������������������������������������" at 1 skip
  "    �      �          �             �             �        �⬥⪨ ����� ���⥫�騪�:" at 1 skip
  "    �      �          �             �             �" at 1 skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[1] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[2] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[3] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[4] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[5] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[6] skip
  "    �      �          �             �             �       " at 1 mPrnStr-El-Doc[7] skip
with frame plattreb-end down no-labels no-underline no-box width 86.

/*prosignMrjAJJT+6RY+KoI6xsO1lA*/