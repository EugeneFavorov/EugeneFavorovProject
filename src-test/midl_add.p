/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1999 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: midl_add.p
      Comment: ���� %-�� �⠢��, ��室� �� �।���� ���⪠ �� ��ਮ�.
      Comment: ���᫥��� % �� ��������� ��業⮭�� �⠢��
      Comment: �� �������騩�� ���⮪.
      Comment: ������ fost ������� � infiltr.p, ���㫥��� �ந��������
      Comment: �� ������ �롮� ������ ���.
   Parameters:
      Created: Om 30/08/99
     Modified: Om 01/09/99
     Modified: Om 02/11/99 "�室�騩/��室�騩" ���⮪ �१ ���४.
     Modified: ���४�஢�� ���᫥��� �।���� ���⪠.
     Modified: Om 18/11/99
     Modified: Om 09/11/00 ������祭�� �����㬥�� ���᪠ %% �⠢��.
     Modified: Om 06/12/00 ��ࠡ�⪠: ��ॢ�� �� �����㬥���� �஢���.
     Modified: Om 05/02/01 ��ࠡ�⪠: ���४�஢�� �室��� ��ࠬ��஢.
     Modified: Om 06/04/01 ��ࠡ�⪠: ��ࠡ�⪠ �訡�� ���᪠ �����ᨨ.
*/

Form "~n@(#) midl_add.p 1.0 Om  30/08/99 Om 01/09/99 Om 06/12/00"
     with frame sccs-id stream-io width 250.

{globals.i}
{def_work.i} /* ��।������ ⠡���� fost */
{prn-ved.i &DefTempTable = "���塞 �६����� ⠡����"}


def input param rid1         as recid                   no-undo. /* �� ��� %% */
def input param in-commi     like commission.commission no-undo. /* �������  */
def input param rid          as recid                   no-undo. /* ��� when acct */
def input param in_kau       like kau.kau               no-undo. /* ��� when avail kau */
def input param curr_beg     as date                    no-undo. /* ��� ��砫� ��ਮ�� */
def input param curr_end     as date                    no-undo. /* ��� ����砭��  ��ਮ�� */
define var midl_val as decimal no-undo. /* �।��� ���祭�� �� ��ਮ� */
define var nach_h   as handle  no-undo.
DEF var  ioXResult   AS DEC   NO-UNDO.
DEF var  ioDate      AS DATE  NO-UNDO.
DEF var  oXResultRef AS DEC   NO-UNDO. /* �ᥣ�� 0 */
DEF  STREAM err.




/* ����㧪� �����㬥���� */
run load_nachtool (No, output nach_h).



/* ���� �����ᨨ */
run get_sch_line_by_rid in nach_h (rid1, buffer interest-sch-line).

/* ���� ��� */
run GET_ACCT_BY_RID in nach_h (rid, buffer acct).
if not avail acct
    then return "��� �� ������".

/* �������� �������᪨� ���⪮� �� ���� */
run CREATE_REAL_FOST in nach_h (rid, curr_beg, curr_end).

/* ���� �।���� ���⪠ �� ��ਮ�. */
run FIND_MIDL_REM in nach_h (curr_beg, 
                             curr_end, 
                             interest-sch-line.interest-month, 
                             output midl_val).

/* ������� */

run CREATE_RATE_MIN_AMT in nach_h (in-commi, rid, ?, midl_val, in_kau, curr_end).

if return-value ne ""
then do:
    /* ���㧪� �����㬥���� */
    run remove_nachtool (No, nach_h).
    return return-value.
end.

/* ����� ���᫥��� � �ନ஢���� ���� */
run NACH_AND_REPORT in nach_h (interest-sch-line.interest-sch, 
                               acct.acct,
                               acct.currency, 
                               in_kau, 
                               curr_beg, 
                               curr_end,
                               interest-sch-line.interest-month, 
                               interest-sch-line.basis-time).

/* ���㧪� �����㬥���� */
   FOR EACH nach_rep:
      ioXResult = ioXResult + nach_rep.acct_val_per.
   END.
   RUN MakeNachkinTTT.
run remove_nachtool (No, nach_h).

return "".



PROCEDURE MakeNachkinTTT:
   DEF VAR vOpID           AS CHAR NO-UNDO.
   DEF VAR vOpContractDate AS CHAR NO-UNDO.
   DEF VAR vSumRpoc        AS DEC  NO-UNDO.
   DEF VAR vFlagError      AS INT64  NO-UNDO.
   DEF VAR iFileNameDest  AS CHAR NO-UNDO.
   output stream err to "spooln.tmp" append.
   LOOP:
   FOR EACH nach_rep where nach_rep.acct_rem <> ?  BREAK BY nach_rep.intrvl_beg :
      CREATE nachkin-tt.
      ASSIGN                  
         nachkin-tt.BegDate   = nach_rep.intrvl_beg
         nachkin-tt.Days      = nach_rep.day_p_int
         nachkin-tt.EndDate   = nach_rep.intrvl_end
         nachkin-tt.Comm      = nach_rep.rate
         nachkin-tt.CommCode  = In-commi
         nachkin-tt.SummOst   = nach_rep.acct_rem
         nachkin-tt.SummProc  = nach_rep.acct_val_per
         nachkin-tt.Acct      = acct.acct
         nachkin-tt.Curr      = acct.currency
         nachkin-tt.BalAcct   = acct.bal-acct
      .
      PUT STREAM err 
         IF FIRST(nach_rep.intrvl_beg) THEN nachkin-tt.Acct ELSE "" FORMAT "x(25)"
         " "
         nach_rep.intrvl_beg
         " "
         nach_rep.intrvl_end
         " "
         nach_rep.day_p_int
         " "
         nach_rep.acct_rem  FORMAT ">>>,>>>,>>>,>>9.99Cr"
         " "
         IF LAST(nach_rep.intrvl_beg) THEN STRING (midl_val , ">>,>>>,>>>,>>9.99Cr") ELSE "" FORMAT "x(19)"
         FILL(" ", 14 - LENGTH(STRING(nach_rep.rate, ">>9.99999"))) + STRING(nach_rep.rate, ">>9.99999") FORMAT "x(14)"
         nach_rep.acct_val_per FORMAT ">>>,>>>,>>>,>>9.99Cr"
         " "
         IF LAST(nach_rep.intrvl_beg) THEN STRING (ioXResult , ">>,>>>,>>>,>>9.99Cr") ELSE "" FORMAT "x(19)"
         SKIP
      .
   END.
   output stream err  close.
END PROCEDURE.
