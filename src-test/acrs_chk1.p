/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2006 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACRS_CHK.P
      Comment: �᪫�祭�� �� �६����� ⠡���� �� ��⮢, �� �����
               ��� ��������� 䠪�஢, ������� �� ࠧ��� १�ࢠ.
   Parameters:
         Uses:
      Used by:
      Created: 16.03.2006 17:46 ILVI    
     Modified: 
*/
{globals.i}
{intrface.get rsrv}
{intrface.get i254}

DEFINE INPUT PARAMETER iDate    AS DATE NO-UNDO.
DEFINE INPUT PARAMETER iBefDate AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tProv.

DEF BUFFER btProv FOR tProv. /* ���������� ����. */

DEFINE VARIABLE vFlag AS LOGICAL NO-UNDO.


/* ����᫮��� 㤠�塞 ���, � ������ �� �뫮 ���������  */
FOR EACH tProv WHERE
         tProv.acct-reserve EQ "" :
   IF NOT AcctRegNeed(tProv.acct-risk,tProv.currency,iBefDate,iDate) THEN
      DELETE tProv.
END. 

/* ���, ����騥 ��������� ��� १�ࢠ 㤠������ ��㯯��� */
FOR EACH tProv WHERE
         tProv.acct-reserve NE "" BREAK BY tProv.acct-reserve:
   IF FIRST-OF(tProv.acct-reserve) THEN
      vFlag = YES.

   vFlag = vFlag AND NOT AcctRegNeed(tProv.acct-risk,tProv.currency,iBefDate,iDate).

   IF tProv.cont-code NE "" AND tProv.acct-risk-type NE "" 
       AND CAN-DO(FGetSetting("���犐",?,""),tProv.acct-risk-type)
       AND LnRsrvRate (tprov.contract,tProv.cont-code,iDate) NE LnRsrvRate (tprov.contract,tProv.cont-code,iBefDate)
       THEN vFlag = NO.

   IF     LAST-OF(tProv.acct-reserve)
      AND vFlag THEN DO:
      FOR EACH btProv WHERE
               btProv.acct-reserve EQ tProv.acct-reserve:
         DELETE btProv.
      END.
   END.
END. 
 
{intrface.del}          /* ���㧪� �����㬥����. */ 

