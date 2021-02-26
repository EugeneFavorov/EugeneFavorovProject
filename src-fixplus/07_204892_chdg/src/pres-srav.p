/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: pres-srav.p
      Comment: ��楤�� �ࠢ����� �।����. � ������.��䨪��
   Parameters: none
         Uses:
      Used by:
      Created: 17/10/2014 kuds (0204892)
     Modified: 
*/
DEF INPUT PARAM iStatusTO AS CHAR NO-UNDO.
DEF INPUT PARAM iPreschTo AS CHAR NO-UNDO.
DEF PARAM BUFFER term-obl FOR term-obl.
DEF OUTPUT PARAM oRefresh AS LOGICAL NO-UNDO.

{globals.i}
{intrface.get xclass} /* ����㧪� �����㬥���� ����奬�  */
{intrface.get tmess}
{intrface.get loan}
{intrface.get strng}
{loan.pro}
{setdest.i}
{obl-pr.i}

DEFINE VARIABLE mIdnt-cls   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCount      AS INT64     NO-UNDO.

DEFINE BUFFER b-term-obl    FOR term-obl.
DEFINE BUFFER b-term-obl-pr FOR term-obl.
DEFINE BUFFER b-tt-term-obl FOR tt-term-obl.


/*������塞 �६����� ⠡���� ��� ��䨪�� � �������묨 ������묨 ��⠬� */
mIdnt-cls = "301,302,303,310".


FOR EACH b-term-obl-pr 
   WHERE b-term-obl-pr.cont-code EQ term-obl.cont-code
     AND b-term-obl-pr.contract  EQ term-obl.contract
     AND CAN-DO(mIdnt-cls,STRING(b-term-obl-pr.idnt))
   NO-LOCK:

      CREATE tt-term-obl.
      ASSIGN 
         tt-term-obl.end-date-pr     = b-term-obl-pr.end-date
         tt-term-obl.dsc-beg-date-pr = b-term-obl-pr.dsc-beg-date
         tt-term-obl.amt-rub-pr      = b-term-obl-pr.amt-rub
         tt-term-obl.idnt            = b-term-obl-pr.idnt
      .

      FIND FIRST b-term-obl 
         WHERE b-term-obl.cont-code  EQ b-term-obl-pr.cont-code
           AND b-term-obl.contract   EQ b-term-obl-pr.contract
           AND b-term-obl.end-date   EQ b-term-obl-pr.end-date
           AND b-term-obl.idnt       EQ b-term-obl-pr.idnt - 300  
         NO-LOCK NO-ERROR.

      IF AVAIL b-term-obl THEN
         ASSIGN 
            tt-term-obl.end-date        = b-term-obl.end-date
            tt-term-obl.dsc-beg-date    = b-term-obl.dsc-beg-date
            tt-term-obl.amt-rub         = b-term-obl.amt-rub
      .
END.  

/*������ ����� �� �����⥫쭮�� ��䨪�, ������ ��� � �।���⥫쭮� ��䨪�*/
mIdnt-cls = "1,2,3,10".
FOR EACH b-term-obl 
   WHERE b-term-obl.cont-code EQ term-obl.cont-code
     AND b-term-obl.contract  EQ term-obl.contract
     AND CAN-DO(mIdnt-cls,STRING(b-term-obl.idnt))
   NO-LOCK
   BY b-term-obl.idnt BY b-term-obl.end-date:

   FIND FIRST tt-term-obl 
      WHERE tt-term-obl.end-date EQ b-term-obl.end-date
        AND tt-term-obl.idnt     EQ 300 + b-term-obl.idnt
      NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-term-obl THEN 
   DO:
      CREATE tt-term-obl.
      ASSIGN 
         tt-term-obl.idnt            = b-term-obl.idnt + 300 
         tt-term-obl.end-date        = b-term-obl.end-date
         tt-term-obl.dsc-beg-date    = b-term-obl.dsc-beg-date
         tt-term-obl.amt-rub         = b-term-obl.amt-rub
      .
   END.
END.

/*�뢮� ���� �� �६����� ⠡����, � ������ ���� �⫨�� � ���� ��� �㬬�*/
                                                       
FOR EACH tt-term-obl
   WHERE tt-term-obl.end-date-pr     NE tt-term-obl.end-date
      OR tt-term-obl.dsc-beg-date-pr NE tt-term-obl.dsc-beg-date
      OR tt-term-obl.amt-rub-pr      NE tt-term-obl.amt-rub
   NO-LOCK 
   BY tt-term-obl.end-date-pr BY tt-term-obl.end-date BY tt-term-obl.idnt :
   mCount = mCount + 1.
   IF mCount EQ 1 THEN DO:
      PUT UNFORMATTED 
      "��������! �� �������� " GetContCode(term-obl.contract,term-obl.cont-code) 
      " �।���⥫�� (�� �ᯮ������ ��� �� " STRING(term-obl.end-date,"99/99/99") ")" SKIP 
      "� �������騩 ��䨪� ࠧ�������."  SKIP
      "�������������������������������������������������������������������������������������Ŀ" 
   SKIP
      "�   �������� ���   � ��� ����砭�� �� �                 �㬬�         �             �" 
   SKIP
      "�����������������������������������������������������������������������Ĵ    ���      �" 
   SKIP
      "� �।�.  � �����. � �।�.  � �����. ��।�.��䨪   � �����.��䨪 ������������⨳" 
   SKIP
      "� ��䨪  � ��䨪  � ��䨪  � ��䨪  �               �               �             �" 
   SKIP.

   END.

   PUT UNFORMATTED 
      "�������������������������������������������������������������������������������������Ĵ" SKIP
      "�" fStrNvl(STRING(tt-term-obl.end-date-pr,"99/99/99"),"")     FORMAT "x(9)"
      "�" fStrNvl(STRING(tt-term-obl.end-date,"99/99/99"),"")        FORMAT "x(9)"
      "�" fStrNvl(STRING(tt-term-obl.dsc-beg-date-pr,"99/99/99"),"") FORMAT "x(9)"  
      "�" fStrNvl(STRING(tt-term-obl.dsc-beg-date,"99/99/99"),"")    FORMAT "x(9)" 
      "�" STRING(tt-term-obl.amt-rub-pr,"->>>,>>>,>>9.99")       FORMAT "x(15)" 
      "�" STRING(tt-term-obl.amt-rub,"->>>,>>>,>>9.99")          FORMAT "x(15)" 
      "�" GetIdntType(tt-term-obl.idnt)                              FORMAT "x(13)" 
      "�" SKIP.

END.

IF mCount GT 0 THEN
   PUT UNFORMATTED  
   "���������������������������������������������������������������������������������������" SKIP.

/*�᫨ �⫨稩 ���*/
IF mCount EQ 0 THEN
   PUT UNFORMATTED "�� �������� " GetContCode(term-obl.contract,term-obl.cont-code) + 
      " �� ������� ��宦����� � �।���⥫쭮� ��䨪�" SKIP 
      "(�� �ᯮ������ ��� �� " STRING(term-obl.end-date,"99/99/99") ") � �������饬 ��䨪�."
      SKIP.

{preview.i}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:14:07.526+04:00' */
/* $LINTFILE='pres-srav.p' */
/*prosignBjQOVbPgSlRaI+1r0hU7wA*/