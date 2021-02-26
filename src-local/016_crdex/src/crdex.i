/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: crdex.i
      Comment: �����⨪� ��⮢ �஢���� ���㬥��
   Parameters: 
         Uses:
      Used BY: crdex.p
      Created: ??
     Modified: 29.12.2004 abko (0038926) - �訡�� �� ᬥ�� ����� ����⭮��
                               ���㬥�� � ��⠬�, ����騬� �����⨪�
     Modified: 30.04.2009 kraw (0103092) ��� ����⥪� �����ᨩ
*/

&IF "{&dbcr}" = "db" &THEN
   &SCOP FORDB -db
   &SCOP FORCR -cr
&ELSEIF "{&dbcr}" = "cr" &THEN
   &SCOP FORDB -cr
   &SCOP FORCR -db
&ENDIF

&IF DEFINED(CARTNAME) EQ 0 &THEN
&SCOPED-DEFINE CARTNAME �����᭥���� ����㯫����
&ENDIF

pick-value = "no".
FIND FIRST op-entry WHERE RECID(op-entry) EQ rid EXCLUSIVE-LOCK.
IF     op-entry.currency NE ""
   AND op-entry.amt-cur  EQ 0 THEN
DO:
   pick-value = "yes" .
   RETURN.
END.
IF    op-entry.kau{&FORDB} EQ ?
   OR op-entry.kau{&FORDB} EQ "" THEN
DO:
   IF NOT type-balance
      AND type-curracct THEN
      FIND FIRST acct WHERE acct.acct     EQ op-entry.acct{&FORDB}
                        AND acct.currency EQ op-entry.currency NO-LOCK.
   ELSE
      {find-act.i
         &acct = op-entry.acct{&FORDB}
         &curr = op-entry.currency
      }
   IF NOT AVAIL acct THEN
      RETURN.
   IF     acct.kau-id NE ?
      AND acct.kau-id NE "" THEN
      FIND FIRST code WHERE code.class EQ "�������"
                        AND code.code  EQ acct.kau-id NO-LOCK NO-ERROR.
   ELSE DO:
      FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
      IF NOT AVAIL bal-acct THEN
         RETURN.

      IF     bal-acct.kau-id NE ?
         AND bal-acct.kau-id NE "" THEN
      FIND FIRST code WHERE code.class EQ "�������"
                        AND code.code EQ bal-acct.kau-id NO-LOCK NO-ERROR.
   END.
   PAUSE 0.
   mCurrentKau = GetSysConf("CurrentKau").
   IF {assigned mCurrentKau} THEN
   DO: 
      kau-rid = INT64(mCurrentKau).
      FIND FIRST kau WHERE RECID(kau) EQ kau-rid EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      RUN SetSysConf IN h_base("CurrentKau","").
   END.
   ELSE
   DO:
      IF kau-rid NE 0 THEN 
         FIND FIRST kau WHERE RECID(kau) EQ kau-rid EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      ELSE DO:
         FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
         RUN getKauDocSysConf(op.op-kind,
                              BUFFER op-entry,
                              BUFFER kau).
         IF AVAIL(kau) THEN
            FIND CURRENT kau EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      END.
   END.
   IF AVAIL kau THEN
      ASSIGN
         ret-value = STRING(RECID(kau))
         pick-value = "no"
      .
   IF NOT AVAIL kau THEN
   DO:
      IF AVAIL code THEN
         IF code.misc[5] EQ "" THEN
            RUN anal-v.p (op-entry.acct{&FORDB},
                          acct.currency,6).
         ELSE IF   SEARCH(code.misc[5] + ".p") NE ?
                OR SEARCH(code.misc[5] + ".r") NE ? THEN
                 RUN VALUE(code.misc[5] + ".p") (op-entry.acct{&FORDB},
                                                 acct.currency,6).
         ELSE DO:
           {message &text="�� ������� ��楤�� ��ᬮ�� ""
                          + code.misc[5] + "".p""
                          + "" � 蠡���� ��� !"
                    &alert-box=error
           }
        END.
/*   IF AVAIL code THEN RUN VALUE(code.misc[5]) (op-entry.acct{&FORDB},
                                               op-entry.currency, 6).  */
      IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
      DO:
         IF FGetSetting("�������",?,"��") EQ "���" THEN
         DO:
            pick-value = "no".
            RETURN.
         END.
         ELSE DO:
            MESSAGE COLOR WHITE/BLACK "�� �� ��� �஢����� �஢���� ��
  ����⥪�(��) ��� ���� �⪠�뢠����(���) ?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
            IF choice EQ yes THEN
               pick-value = "yes".
            ELSE
               pick-value = "no".
            RETURN.
         END.
      END.
      ASSIGN
         ret-value = pick-value
         pick-value = "no".
      FIND FIRST kau WHERE RECID(kau) EQ INT64(ret-value) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   END.
   ASSIGN
       op-entry.kau{&FORDB} = kau.kau.
END.
ELSE DO:
   {kau(op).fnd &op-entry=op-entry &db-cr={&FORDB}}
END.
{kau(off).cal &op-entry=op-entry &ssum="- op-entry.amt-rub"
              &inc=1 &scur="- op-entry.amt-cur"}
IF    (    op-entry.currency NE ""
       AND kau.curr-bal < 0)
   OR (    op-entry.currency EQ ""
       AND kau.balance  < 0) THEN
DO:
   {op(kau).fnd}
   {message
      &text="����⥪� {&CARTNAME}: ���㬥�� N ""
            + (IF op.doc-num EQ ? THEN ""?"" ELSE STRING(op.doc-num))
            + "" �� "" +
          (IF op-entry.op-date EQ ? THEN ""?"" ELSE STRING(op-entry.op-date)) +
          "" �� ���� "" + STRING(kau.acct) +
          (IF op-entry.currency NE """" THEN (""/"" + op-entry.currency) ELSE """") +
          "" - ��室 ����� ��室�!"
      &alert-box=error
      &color=MESSAGES
   }
   pick-value = "no".
END.
ELSE
   pick-value = "yes".
