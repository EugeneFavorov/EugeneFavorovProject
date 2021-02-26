/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: t-obl-joi.p
      Comment: Join ���� ��� ����ᮢ term-obl 
   Parameters: ���
         Uses:
      Used by:
      Created: 19.05.2009 11:11 Jadv    
     Modified: 
*/
{joinpar.i}
{globals.i}

{flt-file.i} /* ��।������ �������� �������᪮�� 䨫��� */

DEF VAR mTitle AS CHAR NO-UNDO INIT "[ �������� ]".   /* ��������� ���� */

FIND FIRST term-obl WHERE 
     ROWID(term-obl) EQ TO-ROWID(iRowId) 
NO-LOCK NO-ERROR.

FIND FIRST loan-cond 
     WHERE loan-cond.contract EQ term-obl.contract
       AND loan-cond.cont-code EQ term-obl.cont-code
NO-LOCK NO-ERROR.

IF NOT AVAIL term-obl THEN 
   RETURN "-1".

IF iClass EQ "term-obl-debt" THEN
   RUN CreateJoin ("�஫������",
                   "p-obl(l)`" + 
                   STRING(RECID(term-obl)) + "," + STRING(level + 1),
                   YES).

IF iClass EQ "term-obl-agrm" THEN
DO:
   mTitle = "[ ����������� ����������� ]".
   RUN CreateJoinLd ("�����饭�� �ᯮ��/������", 
                     "browseld",
                     "PackObject",
                     "file-name" + CHR(1) + "surrogate",
                     "term-obl" + CHR(1) + term-obl.contract         + CHR(2) +
                                           term-obl.cont-code        + CHR(2) +
                                           STRING(term-obl.idnt)     + CHR(2) +
                                           STRING(term-obl.end-date) + CHR(2) +
                                           STRING(term-obl.nn),
                     "file-name" + CHR(1) + "surrogate",
                     level + 1,
                     YES).
END.

IF iClass EQ "term-obl-dspt" THEN
DO:
   mTitle = "[ ����� ]".
   RUN CreateJoinLd ("�����饭�� �ᯮ��/������", 
                     "browseld",
                     "PackObject",
                     "file-name" + CHR(1) + "surrogate",
                     "term-obl" + CHR(1) + term-obl.contract         + CHR(2) +
                                           term-obl.cont-code        + CHR(2) +
                                           STRING(term-obl.idnt)     + CHR(2) +
                                           STRING(term-obl.end-date) + CHR(2) +
                                           STRING(term-obl.nn),
                     "file-name" + CHR(1) + "surrogate",
                     level + 1,
                     YES).
END.

IF iClass EQ "presched" THEN
DO:
   RUN CreateJoin ("�������⥫�� ४������", 
                   "t-obl-sgn`" + 
                   STRING(RECID(term-obl)) + "," + STRING(level + 1),
                   YES).
   RUN CreateJoinLd ("�।���⥫�� ������� ���⪨", 
                     "browseld",
                     "term-obl-sum-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "302",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("�।���⥫�� ������� ����襭�� ����", 
                     "browseld",
                     "term-obl-debt-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "303",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("�।���⥫�� ������� ���⥦� %%", 
                     "browseld",
                     "term-obl-per-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "301",
                     "",
                     STRING(level + 1),
                     YES).
   RUN CreateJoinLd ("�।���⥫�� ��䨪 �����ᨩ", 
                     "browseld",
                     "term-obl-comm-pr",
                     "contract"  + CHR(1) + "cont-code" + CHR(1) + "idnt",
                     term-obl.contract + CHR(1) + term-obl.cont-code + CHR(1) + "310",
                     "",
                     STRING(level + 1),
                     YES).

END.

   /* Join-���� ���뢠�� ⮫쪮 ��� 㪠������ ����ᮢ */
IF CAN-DO("term-obl-debt,term-obl-agrm,term-obl-dspt,presched", iClass) THEN
DO:
      /* ����� �ନ஢���� Join ���� */
   {procjoin.i
       &Prefix     = "term-obl"
       &frametitle = mTitle
       &parms      = "(recid(term-obl),level + 1)"
       &showsingle = "YES"
   }
END.
RETURN "0".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:56:56.035+04:00' */
/* $LINTFILE='t-obl-joi.p' */
/*prosignoEYKHL8hlv2rktEuzJYetQ*/