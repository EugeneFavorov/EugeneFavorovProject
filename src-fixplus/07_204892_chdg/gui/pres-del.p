{globals.i}
{intrface.get tmess}

/* +++ pres-del.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: pres-del.p
      Comment: ��楤�� 㤠����� �।���⥫쭮�� ��䨪�
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
{obl-pr.i}

/*㤠�塞 ����� �� ����ᮢ term-obl-debt-pr
                            term-obl-per-pr 
                            term-obl-sum-pr 
                            term-obl-comm-pr 
*/
RUN DeleteObl-pr IN THIS-PROCEDURE (BUFFER term-obl) .

{intrface.del}
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:13:44.605+04:00' */
/* $LINTFILE='pres-del.p' */
/*prosignIyBaqBUQ6HGRlTya85uGtA*/
/* --- pres-del.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am --- */
