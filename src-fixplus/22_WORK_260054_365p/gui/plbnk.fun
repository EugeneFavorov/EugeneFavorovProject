
/* +++ plbnk.fun was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: plbnk.fun
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      �㭪樨 �������⥪� pp-plbnk.p
     Modified: 
*/
/*===============================================================================================*/
/*=== �믮����� ����� �� ���譥� ��⥬� �� ���� ===============================================*/
FUNCTION RunExtQuery RETURNS LOG (INPUT iDate AS DATE):

DEF VAR vRes  AS  LOG   NO-UNDO INIT NO.

   IF     iDate       NE ?            /* �室��� ��� �� ����                                  */
      AND mDateNR365p NE ?            /* ��� ��砫쭮�� �襭�� ��।�����                      */
      AND iDate       LE mDateNR365p  /* ��室��� ��� ����� ���� ��砫쭮�� �襭��           */
      AND mZaprosNO   EQ YES          /* ������� �ਧ��� "����� ��������� �ࠣ���"              */
   THEN
      vRes = YES.

   RETURN vRes.

END FUNCTION. /* IsLessDateNR */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �஢�ઠ �� ����⢮����� ��� �� ���譥� ��⥬� ========================================*/
FUNCTION IsAcctEists RETURNS LOG (INPUT  iAcct AS  CHAR,
                                  OUTPUT oCur  AS  CHAR):

DEF BUFFER bExtAcct FOR ttExtAcct.

   /* ����᪠�� ���� ��� �� ���譥� ⠡��� */
   RUN FindAcct IN THIS-PROCEDURE(iAcct, 
                                  OUTPUT TABLE bExtAcct) 
                                 NO-ERROR.

   FIND FIRST bExtAcct NO-LOCK NO-ERROR.
   oCur = IF AVAIL(bExtAcct) THEN fStrNvl(bExtAcct.currency, "")
                             ELSE "".

   RETURN AVAIL(bExtAcct).

END FUNCTION. /* IsAcctEists */

/*===============================================================================================*/

/*===============================================================================================*/
/*=== �८�ࠧ������ ���� � ��ப� ==============================================================*/
FUNCTION date2str RETURNS CHAR PRIVATE (INPUT iDate AS DATE):

   /* ����� date2str �� core365p.pro */
   RETURN ( IF iDate EQ ? THEN "" 
                         ELSE STRING(iDate, {&DATE-FORMAT-365P})).

END FUNCTION. /* date2str */

/*===============================================================================================*/

/* --- plbnk.fun was humbly modified by (c)blodd converter v.1.09 on 10/16/2015 7:12am --- */
