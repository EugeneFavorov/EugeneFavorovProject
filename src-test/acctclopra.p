/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACCTCLOPR.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 15.04.2009 18:25 BIS     
     Modified: 15.04.2009 18:25 BIS     
*/

{globals.i}
{intrface.get xclass}
{intrface.get tmess}
{sh-defs.i}
{dps-a-cl.tmp SHARED}
                                                   
DEF INPUT  PARAM in-cont-code LIKE loan.cont-code   NO-UNDO.
DEF INPUT  PARAM in-op-date   LIKE op-date.op-date  NO-UNDO.
DEF OUTPUT PARAM oResult      AS INT64            NO-UNDO.

DEF VAR i                     AS INT64           NO-UNDO.
DEF VAR mess                  AS CHAR EXTENT 100   NO-UNDO.
DEF VAR flagerr               AS LOGICAL           NO-UNDO.
DEF VAR vQuestLog             AS LOGICAL           NO-UNDO. /*��६����� ��� ᮮ�饭�� � �롮஬*/
DEF VAR vPosSinceDate         AS DATE              NO-UNDO.
DEF VAR gLogMessage           AS LOG               NO-UNDO.
DEF VAR list_type             AS CHAR              NO-UNDO.
DEF VAR vMainAcct             AS LOGICAL           NO-UNDO.
DEF VAR vClosedMainAcct       AS LOGICAL INIT YES  NO-UNDO.

DEF BUFFER bb-loan-acct FOR loan-acct.
DEF BUFFER bb-loan      FOR loan.
DEF BUFFER b-loan-acct  FOR loan-acct.
DEF BUFFER bAcct        FOR acct.
DEF BUFFER xloan-acct   FOR loan-acct.

MAIN:
DO ON ERROR  UNDO MAIN, LEAVE MAIN
   ON ENDKEY UNDO MAIN, LEAVE MAIN:

   IF GetSysConf("gLogMessage") = "��" THEN 
      gLogMessage = TRUE.
   
   mess[1]  = "��� �� ����ॡ������ �� ������.".
   mess[2]  = "���� ��� �� ������.".
   mess[3]  = "��� ���᫥���� ��業⮢ �� ������" .
   mess[4]  = "��� ���᫥���� ��業⮢ '����⠫' �� ������" .
   mess[5]  = "��� ���������� �� ������.".
   mess[6]  = "��� ���������� '����⠫' �� ������.".
   mess[7]  = "��� ����窨 �� ������.".
   mess[8]  = "��� ���⮢ �� ������� �� ������.".
   mess[9]  = "��� '���室� ����� ��ਮ���' �� ������.".
   
   mess[10] = "���� ����楯⮢���� ���㬥��� �� ���� �� ����ॡ������.".
   mess[11] = "���� ����楯⮢���� ���㬥��� �� ��筮�� ����.".
   mess[12] = "���� ����楯⮢���� ���㬥��� �� ���� ���᫥���� ��業⮢.".
   mess[13] = "���� ����楯⮢���� ���㬥��� �� ���� ���᫥���� ��業⮢ '����⠫'.".
   mess[14] = "���� ����楯⮢���� ���㬥��� �� ���� ����������.".
   mess[15] = "���� ����楯⮢���� ���㬥��� �� ���� ���������� '����⠫'.".
   mess[16] = "���� ����楯⮢���� ���㬥��� �� ���� ����窨.".
   mess[17] = "���� ����楯⮢���� ���㬥��� �� ���� ���⮢ �� �������.".
   mess[18] = "���� ����楯⮢���� ���㬥��� �� ���� '���室� ����� ��ਮ���'.".
   
   mess[19] = "��� �� ����ॡ������ �ਢ易� � ��㣮�� ������.".
   mess[20] = "���� ��� �ਢ易� � ��㣮�� ������.".
   mess[21] = "��� ���᫥���� ��業⮢ �ਢ易� � ��㣮�� ������.".
   mess[22] = "��� ���᫥���� ��業⮢ '����⠫' �ਢ易� � ��㣮�� ������.".
   mess[23] = "C�� ���������� �ਢ易� � ��㣮�� ������.".
   mess[24] = "C�� ���������� '����⠫' �ਢ易� � ��㣮�� ������.".
   mess[25] = "C�� ����窨 �ਢ易� � ��㣮�� ������.".
   mess[26] = "��� ���⮢ �� ������� �ਢ易� � ��㣮�� ������.".
   mess[27] = "��� '���室� ����� ��ਮ���' �ਢ易� � ��㣮�� ������.".
   
   mess[28] = "���㫥��� ���⮪ �� ��� �� ����ॡ������.".
   mess[29] = "���㫥��� ���⮪ �� ��筮� ���.".
   mess[30] = "���㫥��� ���⮪ �� ��� ���᫥���� ��業⮢.".
   mess[31] = "���㫥��� ���⮪ �� ��� ���᫥���� ��業⮢ '����⠫'.".
   mess[32] = "���㫥��� ���⮪ �� ��� ����������.".
   mess[33] = "���㫥��� ���⮪ �� ��� ���������� '����⠫'.".
   mess[34] = "���㫥��� ���⮪ �� ��� ����窨.".
   mess[35] = "���㫥��� ���⮪ �� ��� ���⮢ �� �������.".
   mess[36] = "���㫥��� ���⮪ �� ��� '���室� ����� ��ਮ���'".
   
   mess[37] = "�� ��� �� ����ॡ������ ���� ���㬥��� ��᫥ ���� �������.".
   mess[38] = "�� ��筮� ��� ���� ���㬥��� ��᫥ ���� �������.".
   mess[39] = "�� ��� ���᫥���� ��業⮢ ���� ���㬥��� ��᫥ ���� �������.".
   mess[40] = "�� ��� ���᫥���� ��業⮢ '����⠫' ���� ���㬥��� ��᫥ ���� �������.".
   mess[41] = "�� ��� ���������� ���� ���㬥��� ��᫥ ���� �������.".
   mess[42] = "�� ��� ���������� '����⠫' ���� ���㬥��� ��᫥ ���� �������.".
   mess[43] = "�� ��� ����窨 ���� ���㬥��� ��᫥ ���� �������.".
   mess[44] = "�� ��� ���⮢ �� ������� ���� ���㬥��� ��᫥ ���� �������.".
   mess[45] = "�� ��� '���室� ����� ��ਮ���' ���� ���㬥��� ��᫥ ���� �������.".
   
   mess[46] = "C�� �� ����ॡ������ ����� ��᫥ ���� �������. ������� ���?".
   mess[47] = "���� ��� ����� ��᫥ ���� �������. ������� ���?".
   mess[48] = "��� ���᫥���� ��業⮢ ����� ��᫥ ���� �������. ������� ���?".
   mess[49] = "��� ���᫥���� ��業⮢ '����⠫' ����� ��᫥ ���� �������. ������� ���?".
   mess[50] = "��� ���������� ����� ��᫥ ���� �������. ������� ���?".
   mess[51] = "��� ���������� '����⠫' ����� ��᫥ ���� �������. ������� ���?".
   mess[52] = "C�� ����窨 ����� ��᫥ ���� �������. ������� ���?".
   mess[53] = "��� ���⮢ �� ������� ����� ��᫥ ���� �������. ������� ���?".
   mess[54] = "��� '���室� ����� ��ਮ���' ����� ��᫥ ���� �������. ������� ���?".
   
   mess[55] = "C�� �� ����ॡ������ ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[56] = "���� ��� ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[57] = "��� ���᫥���� ��業⮢ ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[58] = "��� ���᫥���� ��業⮢ '����⠫' ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[59] = "��� ���������� ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[60] = "��� ���������� '����⠫' ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[61] = "C�� ����窨 ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[62] = "��� ���⮢ �� ������� ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".
   mess[63] = "��� '���室� ����� ��ਮ���' ���-� ।�������� � �㤥� ��⠢��� ��� ���������.".

   FIND loan WHERE loan.contract  EQ 'DPS'
               AND loan.cont-code EQ in-cont-code 
      EXCLUSIVE-LOCK NO-ERROR.   
   IF NOT AVAIL loan THEN DO:
      IF LOCKED loan THEN
         IF NOT gLogMessage  THEN
            RUN Fill-SysMes IN h_tmess ("", "", "0", "� �⨬ ������� ࠡ����.").
         ELSE 
            RUN LogMess("� �������" + in-cont-code + "ࠡ����.").
      ELSE
         IF NOT gLogMessage  THEN
            RUN Fill-SysMes IN h_tmess ("", "", "0", "����� �� ������.").         
         ELSE
            RUN LogMess("�����" + in-cont-code +  "�� ������.").
      UNDO MAIN, LEAVE MAIN.
   END.

   TMP_ACCT:
   FOR EACH tmp-acct-cl WHERE tmp-acct-cl.acct-type NE ""
      NO-LOCK,
       FIRST loan-acct WHERE loan-acct.contract  EQ 'DPS'
                         AND loan-acct.cont-code EQ in-cont-code
                         AND loan-acct.acct-type EQ tmp-acct-cl.acct-type
                         AND loan-acct.acct      EQ tmp-acct-cl.acct
                         AND loan-acct.since     EQ tmp-acct-cl.since
      NO-LOCK:

         i = tmp-acct-cl.num.

         FIND FIRST acct WHERE acct.acct     EQ loan-acct.acct
                           AND acct.currency EQ loan-acct.currency
            NO-LOCK NO-ERROR.
         IF NOT AVAIL acct THEN DO:
            IF NOT gLogMessage  THEN
               RUN Fill-SysMes IN h_tmess ("", "", "0", mess[ i ]).            
            ELSE 
               RUN LogMess(mess[i]).
            NEXT TMP_ACCT.
         END.      

      {ac-cloa.i
         &main_cycle = "TMP_ACCT "
         &total      = "tmp-acct-cl.TotalNum "
      }
   END.

   IF NOT vClosedMainAcct THEN
      oResult = -1.
END.

{intrface.del}

PROCEDURE LogMess.
   DEF INPUT PARAMETER iLogMess AS CHAR NO-UNDO.

   DEF VAR vLogMess AS CHAR NO-UNDO.

   vLogMess = GetSysConf("LogMessage").
   IF vLogMess NE ? THEN 
      iLogMess = vLogMess + '~n' + iLogMess.
   RUN SetSysConf IN h_base ("LogMessage",iLogMess).
END PROCEDURE.
