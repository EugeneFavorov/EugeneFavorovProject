/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: X:\TMP\LACCT-CL.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 20.12.2011 17:21 gva     
     Modified: 21.12.2011 09:17 gva      
     Modified: 21.12.2011 09:59 gva      
     Modified: 
*/

{globals.i}
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */
{sh-defs.i new}

DEFINE INPUT  PARAMETER ipAcctRole AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.


MESSAGE "���⢥न� ������� ������� ��� " SKIP 
          VIEW-AS ALERT-BOX WARNING 
          BUTTONS YES-NO 
          SET yes_ AS LOG.
IF yes_ EQ no OR yes_ EQ ? 
THEN RETURN.


{tmprecid.def}

{getdates.i
  &BegLabel = "��� �롮ન" 
  &EndLabel = "��� �������"
  &BegHelp  = "������ ����, �� ������ ����� ��� ��� ������� (F1 - ���������)" 
  &EndHelp  = "������ ����, ���ன �㦭� ������� ��� (F1 - ���������)" 
}

def stream out_.
{setdest2.i &stream="stream out_" &filename="lacct-cl1.log" &cols=160}
put stream out_ unformatted  "��� �롮ન " beg-date skip.
put stream out_ unformatted  "��� ������� " end-date skip.

FOR EACH tmprecid:
    FIND FIRST loan WHERE RECID(loan) EQ tmprecid.id
         NO-LOCK NO-ERROR.
    IF AVAILABLE(loan) THEN
    DO i = 1 TO NUM-ENTRIES(ipAcctRole):
        for each loan-acct  WHERE loan-acct.contract  eq loan.contract
                              and LOAN-acct.cont-code eq loan.cont-code
                              AND loan-acct.acct-type EQ ENTRY(i, ipAcctRole)
                              AND loan-acct.since LT beg-date
                              NO-LOCK.
            put stream out_ unformatted  "���� "  loan-acct.acct  " � ஫�� " loan-acct.acct-type  " ��ࠡ��뢠���� " skip.

            FIND FIRST acct 
                WHERE acct.acct EQ loan-acct.acct 
                NO-ERROR.
            IF AVAILABLE(acct) AND GetPermission (acct.class-code, acct.acct + "," + acct.currency, "w")
            THEN DO:
               if acct.close-date NE ? then do:
                  put stream out_ unformatted  "���� "  loan-acct.acct  " � ஫�� " loan-acct.acct-type " 㦥 ������ "  string(acct.close-date)  skip.
                  next.
               end.
               run acct-pos in h_base (loan-acct.acct,loan-acct.currency,end-date,end-date,?).
               if abs(sh-bal) > 0 then do:
                  put stream out_ unformatted  "���� "  loan-acct.acct  " � ஫�� " loan-acct.acct-type " ����� ���㫥��� ���⮪ "  abs(sh-bal)  skip.
               end.
               else do:
                  acct.close-date = end-date.
                  put stream out_ unformatted  "���� "  loan-acct.acct  " � ஫�� " loan-acct.acct-type " ������ "  string(end-date)  skip.
               end.
            END.
        end.
    END.
END.
{preview2.i &stream="stream out_" &filename=lacct-cl1.log }
