/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2009 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: l-gar-joi.p
      Comment: Join ���� ��� ����� term-obl-gar
   Parameters:
         Uses:
      Used by:
      Created: 21.06.2009 15:50 Jadv    
     Modified:
*/
{joinpar.i}
{globals.i}
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */

DEF VAR mSurr     AS CHAR NO-UNDO.

FIND FIRST term-obl WHERE 
     ROWID(term-obl) EQ TO-ROWID(iRowId) 
NO-LOCK NO-ERROR.
IF NOT AVAIL term-obl THEN 
   RETURN "-1".
FIND FIRST loan WHERE 
           loan.contract  EQ term-obl.contract
   AND     loan.cont-code EQ term-obl.cont-code 
NO-LOCK NO-ERROR.
IF NOT AVAIL loan 
   THEN RETURN "-1".

mSurr = term-obl.contract         + "," +
        term-obl.cont-code        + "," +
        STRING(term-obl.idnt)     + "," +
        STRING(term-obl.end-date) + "," + 
        STRING(term-obl.nn).

RUN CreateJoin("�����樥�� ����筮� �⮨����", 
               "ob-rate`" + 
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).


IF term-obl.fop-offbal EQ 2 THEN
   RUN CreateJoin("���஢�� 䨭��ᮢ��� �����㬥��", 
                  "ob-curs`" + 
                  STRING(RECID(term-obl)) + "," + STRING(level + 1), 
                  YES). 
ELSE
   RUN CreateJoin("�������㠫쭠� �⮨�����", 
                  "ob-price`" + 
                  STRING(RECID(term-obl)) + "," + STRING(level + 1),
                  YES).

RUN CreateJoinLd("��⥣��� ����⢠",
                 "browseld",
                 "��玡�ᯥ�",
                 "contract"  + CHR(1) + 
                 "cont-code" + CHR(1) + 
                 "idnt"      + CHR(1) + 
                 "end-date"  + CHR(1) + 
                 "nn",
                 term-obl.contract         + CHR(1) + 
                 term-obl.cont-code        + CHR(1) + 
                 STRING(term-obl.idnt)     + CHR(1) + 
                 STRING(term-obl.end-date) + CHR(1) + 
                 STRING(term-obl.nn),
                 "",
                 STRING(level + 1), 
                 YES).

RUN CreateJoin("�������⥫�� ४������", 
               "toblsign`" +
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).

RUN CreateJoin("��� ���", 
               "ptsregbrw`" +
               STRING(RECID(term-obl))+ "," + STRING(level + 1),
               YES).               

RUN CreateJoin("����饭�� �ᯮ��-������",
               "ob-pck`" + 
               STRING(RECID(term-obl)) + "," + STRING(level + 1),
               YES).

   /* ����� �ନ஢���� Join ���� */
{procjoin.i
    &Prefix     = "term-obl"
    &frametitle = "'[ ����������� �������� ]'" 
    &parms      = "(RECID(term-obl),level + 1)"
}

IF RETURN-VALUE NE "" THEN 
   RETURN RETURN-VALUE.
ELSE
   RETURN "0".
