
/* +++ pt-uni_1.frm was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1998 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: plattreb.frm
      Comment: ��।������ �� ��� 横��� ���� plattreb
      Comment:
   Parameters:
         Uses:
      Used by:
      Created: 15/05/98 13:03:44 RepGen
     Modified:
*/
&IF DEFINED(a107h)  &THEN 
   {pt-uni_1-2.frm {&*}}
&ELSE 
/*--------------- ��।������ ��६����� ---------------*/
/*--------------- ��।������ �� ��� 横��� ---------------*/
/* ��ଠ ��� 横�� "op-entry" */
Form
&IF DEFINED(pt-el) NE 0 &THEN 
/* ��� ���஭���� ���⥦���� �ॡ������ */
{{&pt-el} &pt-frame=YES}
&ENDIF
           op.ins-date FORMAT "99.99.9999" AT 5  op.Due-date FORMAT "99.99.9999" AT 31 mSpisPl FORMAT "x(10)" AT 52  "���������Ŀ" at 75  skip
     "��������������������   ��������������������   ��������������������"                             "�" at 75 NumberForm format "x(7)" "�"     skip
     "�����. � ���� ����.   �����. �ப� ��楯�   C��ᠭ� � ��. ����."                             "�����������" AT 75 skip(2)


     NameOrder Format "x(22)" op.doc-num Format "x(7)" theDate Format "x(10)" AT 35 PayType Format "x(16)" AT 52
     "                               ����������������    �����������������"
     "                                     ���             ��� ���⥦� "             SKIP
  "�������������������������������������������������������������������������������������" at 1 skip
  "�᫮���  �" at 1  Cond-Pay[1] format "x(57)" at 11                   " ��ப ���" at 71 skip
  "������   �" at 1  Cond-Pay[2] format "x(57)" at 11                   " ���楯� �" at 71 Num-Day format "x(4)" at 82 skip
           "�" at 10 Cond-Pay[3] format "x(57)" at 11                   " �        �" at 71 skip
           "�" at 10                                                    " �        �" at 71 skip
  "�������������������������������������������������������������������������������������" at 1 skip
  "�㬬�    �" at 1  AmtStr[1] format "x(75)" at 11 skip
  "�ய���� �" at 1  AmtStr[2] format "x(75)" at 11 skip
           "�" at 10 AmtStr[3] format "x(75)" at 11 skip
           "�" at 10 
  "�������������������������������������������������������������������������������������" at 1 skip
  PlName[1] format "x(46)" at 1 " ��㬬� �" at 47 Rub format "x(15)" at 57 skip
  PlName[2] format "x(46)" at 1 " �      �" at 47 skip
  PlName[3] format "x(46)" at 1 " �      �" at 47 skip
  PlName[4] format "x(46)" at 1 " ��������������������������������������" at 47 skip
  PlName[5] format "x(46)" at 1 " �      �" at 47 skip
                                " ���.N  �" at 47 PlLAcct format "x(25)" at 57 skip
  "���⥫�騪                                     �      �" at 1 skip
  "�����������������������������������������������������Ĵ" at 1 skip
  PlRKC[1] format "x(46)" at 1        " ����   �" at 47 PlMFO format "x(25)" at 57 skip
  PlRKC[2] format "x(46)" at 1        " ������Ĵ" at 47 skip
  "���� ���⥫�騪�                               ���.N  �" at 1 PlCAcct format "x(25)" at 57 skip
  "�������������������������������������������������������������������������������������" at 1 skip
  PoRKC[1] format "x(46)" at 1 " ����   �" at 47 PoMFO format "x(25)" at 57 skip
  PoRKC[2] format "x(46)" at 1 " ������Ĵ" at 47 skip
  "���� �����⥫�                                ���.N  �" at 1 PoCAcct format "x(25)" at 57 skip
  "�����������������������������������������������������Ĵ" at 1 skip
  PoName[1] format "x(46)" at 1                 " ���.N  �" at 47 PoAcct format "x(25)" at 57 skip
  PoName[2] format "x(46)" at 1                 " �      �" at 47  skip
  PoName[3] format "x(46)" at 1                 " ��������������������������������������" at 47 skip
  PoName[4] format "x(46)" at 1                 " ���� ���" at 47 op.doc-type format "x(2)" at 57 "����.����.�" at 64 op.Order-Pay format "x(2)" at 77 skip
  PoName[5] format "x(46)" at 1                 " ����.���        ����������Ĵ" at 47  skip
                                                " ����   �        ����.����  �" at 47 skip
  "�����⥫�                                     �      �        �          �" at 1 skip
  "�������������������������������������������������������������������������������������" at 1 skip
  "�����祭�� ���⥦�" AT 1 SKIP
  Detail[1] format "x(80)" at 1 skip
  Detail[2] format "x(80)" at 1 skip
  Detail[3] format "x(80)" at 1 skip
  Detail[4] format "x(80)" at 1 skip
  Detail[5] format "x(80)" at 1 skip(3)
  "��� ���뫪� (���祭��) ���⥫�騪� �।�ᬮ�७��� ������஬ ���㬥�⮢"
  "�������������������������������������������������������������������������������������" at 1 skip
                          "������                         �⬥⪨ ����� �����⥫�" at 25 skip(1)
                   mDataMarkDBank FORMAT "x(10)" at 72 skip
                   mSposobPoluch  FORMAT "x(20)" AT 27 SKIP
                   "�����������������������" at 25 skip
        "�.�." at 7 skip(3)
                   "�����������������������" at 25 skip
with frame plattreb no-labels no-underline no-box width 90.

FORM
  "����������������������������������������������������������    ��� ����饭��" at 1 skip
  "N �.�N ���Ⳅ��      ��㬬� ����-��㬬� ���⪠��������    � ����⥪�" at 1 skip
  "���ⳮथ࠳����.     �����         ����⥦�      �" at 1 skip
  "    �      ��थ�    ����⥦�      �             �          " at 1 mDataCartIn FORMAT "x(10)" skip
  "����������������������������������������������������������" at 1 skip
  "    �      �          �             �             �        �⬥⪨ ����� ���⥫�騪�" at 1 skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[1] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[2] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[3] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[4] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[5] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[6] skip
  "    �      �          �             �             �       " at 1 mPrnStr-PT-UFEBS[7] skip
  "    �      �          �             �             �" at 1 skip
with frame plattreb-end down no-labels no-underline no-box width 86.
&ENDIF
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='20/11/2014 16:44:06.262+04:00' */
/* $LINTFILE='pt-uni_1.frm' */
/*prosignfVqJBjddOwBGO7n2cQJ4AQ*/
/* --- pt-uni_1.frm was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:28am --- */
