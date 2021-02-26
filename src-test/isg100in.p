/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1999 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: is100.p
      Comment: ���᫥��� ��業⮢ �� ॠ��� ���⮪ �� ���.
               ���᫥��� �㤥� �ந�������� ⮫쪮 � ⮬ ��砥,
               �᫨ ���⮪ �� ��� �� ᭨����� ����� �������쭮��.
   Parameters:
      Created: Om 25/12/2003
     Modified:
*/

DEFINE INPUT PARAM in_par   AS char      NO-UNDO. /*  */
{globals.i}
{sh-defs.i new}

DEFINE var acct     AS char      NO-UNDO. /* ��� when acct */
DEFINE var curr_beg AS DATE      NO-UNDO. /* ��� ��砫� ��ਮ�� */
DEFINE var curr_end AS DATE      NO-UNDO. /* ��� ����砭��  ��ਮ�� */
DEFINE var curr_cur AS DATE      NO-UNDO. /* ��� ����砭��  ��ਮ�� */

DEFINE var i        as INT64     no-undo .
DEFINE var j        as INT64     no-undo .
DEFINE VAR vWork    AS LOG       NO-UNDO.
DEFINE var iDate    as date      no-undo.

DEFINE var rid        AS RECID     NO-UNDO. /* ��� when acct */
define var midl_val    as decimal no-undo. 
define var nach_h      as handle  no-undo.
DEFINE var ioXResult   AS DEC     NO-UNDO.
DEFINE var ioDate      AS DATE    NO-UNDO.
DEFINE var oXResultRef AS DEC     NO-UNDO. 
DEFINE var vPeriod     AS INT64     NO-UNDO. 
DEFINE var vStavka     AS DEC     NO-UNDO. 
DEFINE var vActive     AS char     NO-UNDO. 

DEFINE var DB_         AS DATE    NO-UNDO.   /* ��� ��砫� ������� */                      
DEFINE var DE_         AS DATE    NO-UNDO.   /* ��� ����砭�� �������*/                    
DEFINE var PB_         AS DATE    NO-UNDO.   /* ��� ��砫� ��ਮ�� ���� ��業⮢ */     
DEFINE var PE_         AS DATE    NO-UNDO.   /* ��� ��砫� ����砭�� ���� ��業⮢ */   
DEFINE var RB_         AS DATE    NO-UNDO.   /* ��砫� ���ࢠ�� ���� */                  
DEFINE var RE_         AS DATE    NO-UNDO.   /* ����砭�� ���ࢠ�� ���� */               
DEFINE var SD_         AS int64   NO-UNDO.   /* ����� �� ���� ࠡ�稩 ���� */               

DEFINE var DateNO_     AS DATE    NO-UNDO.   /* ��� ����襭�� ���⪠ */
DEFINE var SummaNO_    AS DEC     NO-UNDO.   /* �㬬� ����襭�� ���⪠ */
DEFINE var ost-t       as dec     NO-UNDO.


DEF  STREAM err.
output stream err to "spooln.tmp" append.

DEFINE  VARIABLE vMinlValDec AS DECIMAL NO-UNDO. /* ��������� ���⮪. */


IF num-entries(in_par,"|") <> 3 then do:
   RETURN "�� ��୮� �᫮ ��ࠬ��஢".
end.       
acct     = entry(1,in_par,"|").
curr_beg = date(entry(2,in_par,"|")).
curr_end = date(entry(3,in_par,"|")).

find first acct where acct.acct  = acct  no-lock no-error.
IF NOT AVAILABLE acct
THEN DO:
   PUT stream err UNFORMATTED acct " ��� �� ������" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   RETURN "��� �� ������".
END.
rid = recid(acct).
find loan-acct where loan-acct.acct      = acct.acct 
                 and loan-acct.curr      = acct.curr
                 and loan-acct.contract  = "�����"
                 and loan-acct.acct-type = "�����" 
                 no-lock no-error.
if not avail loan-acct then do:
   PUT stream err UNFORMATTED acct.acct " ��� ��� ������� �� ����� ��� ����� ������" SKIP.
   PUT stream err UNFORMATTED " " SKIP.
   RETURN RETURN-VALUE.
end.
for each loan-cond where loan-cond.contract  = loan-acct.contract
                     and loan-cond.cont-code = loan-acct.cont-code
                     no-lock.
   vActive     = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "��⨢",?).     
   if vActive <> "��" then next.

   vMinlValDec = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "�㬬����",?)).     
   vStavka     = dec(GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "����⠢��",?)).     
 
   run lwdayu.p(curr_end,output SD_).        
   DB_ = loan-cond.since + 1.                            /* ��� ��砫� ������� */
   DE_ = loan-cond.since + loan-cond.int-date + SD_.     /* ��� ����砭�� �������*/
   PB_ = curr_beg.                                       /* ��� ��砫� ��ਮ�� ���� ��業⮢ */
   PE_ = curr_end.                                       /* ��� ��砫� ����砭�� ���� ��業⮢ */
   RB_ = max(DB_,PB_).                                   /* ��砫� ���ࢠ�� ���� */
   RE_ = min(DE_,PE_).                                   /* ����砭�� ���ࢠ�� ���� */
   if (DB_ >= PB_ and DB_ <= PE_) or (DE_ >= PB_ and DE_ <= PE_) or (DB_ < PB_ and DE_ > PE_) then do:

      /* ����祭�� �������쭮�� ���⪠, 㪠�뢠���� � �� "�㬬����" �� �᫮��� �������. */

      IF vMinlValDec EQ 0
      THEN DO:
          /* ���㧪� �����㬥���� */
         PUT stream err UNFORMATTED acct.number " ��������� ���⮪ �� ������" SKIP.
         PUT stream err UNFORMATTED " " SKIP.
         RETURN RETURN-VALUE.
      END.        	
      /* �஢�ઠ ����襭�� �������쭮�� ���⪠ �� ��� */
      j = curr_end - curr_beg .
      do i = 0 to j.
          run acct-pos in h_base (acct.acct,acct.currency,curr_beg + i, curr_beg + i,?).
          ost-t = - sh-in-bal.
          if ost-t < vMinlValDec then do:
             PUT stream err UNFORMATTED
             '���.����.'         AT 1
             '���� ���.'         AT 21
             '�����.���'         AT 35
             '����'              AT 49
             '������'            AT 55
             SKIP.         
             PUT stream err UNFORMATTED
             loan-cond.cont-code  format "x(20)"
             loan-cond.since      format "99.99.9999"
             vMinlValDec format ">>,>>>,>>>,>>9.99Cr" 
             " "
             loan-cond.int-date
             " "
             vStavka format ">>9.99999"
             SKIP.         
             SummaNO_ = ost-t.
             DateNO_  = curr_beg + i.
             PUT stream err UNFORMATTED acct.number " ����襭 ��������� ���⮪. ��� ����襭�� ���⪠  "   DateNO_ " �㬬� ���⪠ " SummaNO_ SKIP.
             PUT stream err UNFORMATTED " " SKIP.
             pick-value =  string(DateNO_) + "|" + string(SummaNO_).
             RETURN pick-value.
          end. 
      end.
   END.
   RETURN "".
end.
return "".



    
