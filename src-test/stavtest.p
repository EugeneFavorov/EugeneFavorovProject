/*
	��� "���� ����"		
     Filename: stavtest.p
      Comment: 
      Created: 
     Modified: 25/03/14 KAU

*/

{globals.i}
{getdate.i}
{dpsproc.def}
{ksh-defs.i NEW}

{tmprecid.def}

DEF VAR list-intr AS CHAR INIT "nachbal,nach2nch,nachscl" NO-UNDO. /*ᯨ᮪ �奬 ���᫥���, � ������ ��業⭠� �⠢�� ������ �� ��砫쭮�� �����, 
                                                                     ��� ����������*/
DEF VAR list-comm AS CHAR INIT "xpl_st_0" NO-UNDO.                 /*ᯨ᮪ �奬 ���᫥��� c ������饩 ��業⭮� �⠢��� */

DEF VAR list-period AS CHAR INIT "". /*ᯨ᮪ �奬 ���᫥���, � ������ ��業⭠� �⠢�� �� (!!!) ������ �� ��ਮ�� ���⥫쭮�� 
                                       ������ */
DEF VAR in-interest AS CHAR NO-UNDO.
DEF VAR tmp_date AS DATE NO-UNDO.
DEF VAR vop-date AS DATE NO-UNDO.
DEF VAR vop-date1 AS DATE NO-UNDO.
DEF VAR cl-date  AS DATE NO-UNDO.
DEF VAR in-kau   AS CHAR NO-UNDO.
DEF VAR ost      LIKE acct-pos.balance NO-UNDO.
DEF VAR RESULT AS CHAR NO-UNDO .
DEF VAR no-RESULT AS CHAR NO-UNDO INIT "?".
DEF VAR str-acct AS CHAR NO-UNDO.
DEF STREAM rep.
def var vPeriodInt   as INT64            no-undo. /* ����쭠� �த����⥫쭮��� ������� */
DEF VAR in-commi AS CHAR NO-UNDO.
DEF VAR end-commi AS DATE NO-UNDO .
DEF VAR tmp_dec1 AS DEC NO-UNDO. /*�८�।������ %% �⠢�� �� 1/2*/

DEF VAR vComm      AS CHAR NO-UNDO.
DEF VAR vComm-rate AS DEC  NO-UNDO.

{prn-ved.i &DefTempTable = "���塞 �६����� ⠡���� nachkin-tt"}

DEF VAR vInterest     AS CHAR  NO-UNDO. 
DEF VAR vCommission   AS CHAR  NO-UNDO.  /* ��� �����ᨨ*/
DEF VAR vResult    AS DEC  NO-UNDO.
DEF VAR vResult1   AS DEC  NO-UNDO.
DEF VAR vFlag      AS INT64  NO-UNDO.
DEF VAR vValOst    AS DEC   NO-UNDO.
DEF VAR vValPeriod AS DEC   NO-UNDO.
DEF VAR vRole      AS CHAR  NO-UNDO.

DEF VAR val1 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val2 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val3 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val5 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val6 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val9 LIKE comm-rate.acct  NO-UNDO.      
DEF VAR val8 LIKE comm-rate.acct  NO-UNDO.      



{setdest2.i &stream="stream rep" &filename=spoolrep.tmp &cols=80}

beg-date = end-date.
   PUT STREAM rep UNFORMATTED SPACE(10) "����� �� ������������� ������� �� " beg-date SKIP (1).

   PUT STREAM rep UNFORMATTED string("������� �","x(20)") + " " + "�⠢��" + " " 
                                                             + string("��� �⠢��", "x(12)") + " "
                                                             + string("��� ������", "x(15)") + " "
                                                             + string("��� ������", "x(20)") SKIP.
   FOR EACH tmprecid NO-LOCK,
   FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

   IF loan.close-date LE beg-date OR loan.open-date GE beg-date THEN NEXT. /*�� ���४⭮
   ࠡ�⠥� �᫨ ��⠢��� � ���孨� FIRST loan, ��祬� - �� �����, �� � ⠪ ������*/
   ASSIGN in-interest = ? 
          in-commi = ?.
   /*��।������ ����㠫��� ���� � ��砫� ������*/
   run get-beg-date-prol in h_dpspc (recid(loan), 
                                          beg-date,
                                          output vop-date,
                                          output cl-date).
   /* ��।������ ���� �奬� ���᫥���*/
   run Get_Last_Inter in h_dpspc (recid(loan),beg-date,beg-date,output in-interest).
   IF in-interest = ? OR in-interest = '?'  THEN do:
      RESULT = no-RESULT.
      PUT STREAM rep UNFORMATTED loan.doc-ref FORMAT "x(20)"  + " " + RESULT SKIP.
      NEXT.
   END.

   /* ��।������ ���� �⠢�� */
   run Get_Last_Commi in h_dpspc (recid(loan),vop-date,vop-date,output in-commi).
   if in-commi = ? or in-commi = '?' then do :
      RESULT = no-RESULT.
      PUT STREAM rep UNFORMATTED loan.doc-ref FORMAT "x(20)"  + " " + RESULT SKIP.
      NEXT.
   end.

   /*��।������ ��������� ���*/

   vRole = IF cl-date EQ ? 
        THEN "loan-dps-p" 
        ELSE "loan-dps-t".

    FIND LAST loan-acct WHERE loan-acct.contract  = loan.contract
          AND loan-acct.cont-code = loan.cont-code
          AND loan-acct.since <= beg-date 
          AND (IF vRole = "loan-dps-t" THEN  loan-acct.acct-type BEGINS vRole
               ELSE loan-acct.acct-type = vRole)
    NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE loan-acct THEN DO:
       RESULT = no-RESULT.
       PUT STREAM rep UNFORMATTED loan.doc-ref FORMAT "x(20)"  + " " + RESULT SKIP.
       NEXT.
    END.
    
    
    FIND FIRST acct OF loan-acct NO-LOCK NO-ERROR.
    IF NOT AVAILABLE acct THEN DO:
       RESULT = no-RESULT.
       PUT STREAM rep UNFORMATTED loan.doc-ref FORMAT "x(20)"  + " " + RESULT SKIP.
       NEXT.
    END.



    in-kau  = loan.contract + "," + 
             loan.cont-code + "," + 
             IF cl-date = ? 
               THEN "��₪��" 
               ELSE "��₪��".

     /*ॠ�쭠� �த����⥫쭮��� ������*/
     IF cl-date = ?  THEN vValPeriod = 0.
     ELSE run depos-dep-period in h_dpspc (recid(loan), beg-date,output vValPeriod) .
     IF vValPeriod < 0 THEN vValPeriod = 0.



   release comm-rate .

   /*���� �奬� ���᫥���*/
      { findsch.i
        &dir    = last
        &sch    = in-interest
        &since1 = " <= beg-date"}
        
   if NOT  AVAILABLE interest-sch-line then DO:
       RESULT = no-RESULT.
       PUT STREAM rep UNFORMATTED loan.doc-ref FORMAT "x(25)"  + " " + RESULT SKIP.
       NEXT.
   END.

   SUBSCRIBE "publish-comm"  ANYWHERE RUN-PROCEDURE "GetComm".
   
   RUN nachki(RECID(interest-sch-line),
                    in-commi,
                    RECID(acct),
                    beg-date,
                    in-kau,
                    NO,
                    OUTPUT vResult,
                    OUTPUT vResult1,
                    INPUT-OUTPUT vop-date,
                    OUTPUT vFlag) NO-ERROR.

   IF NOT AVAILABLE comm-rate OR error-status:ERROR THEN DO: 
   ASSIGN
      RESULT = string(no-result,"x(6)")
      val1 =  string(no-result,"x(20)")
      val3 =  string(no-result,"x(6)")
      val5 =  string(no-result,"x(18)")
      val6 =  string(no-result,"x(6)")
      val9 =  string(no-result,"x(10)").
   END.

   ELSE DO:
      tmp_dec1 = vComm-rate.
      FIND FIRST  commission OF comm-rate NO-LOCK NO-ERROR.
      IF AVAILABLE commission THEN 
         val8 = commission.name-comm[1].
      ELSE val8 = string(no-result,"x(11)").
      ASSIGN RESULT = string(tmp_dec1, "zz9.99")
             val1 = string(comm-rate.acct, "x(20)") 
             val3 = STRING(comm-rate.currency, "x(6)") 
             val5 = STRING(comm-rate.MIN-VALUE, "zzz,zzz,zzz,zz9.99") 
             val6 = STRING(comm-rate.period, "zzzzz9" ) 
             val9 = STRING(comm-rate.since, "99/99/9999").
   END.

IF RESULT EQ no-RESULT or DEC(RESULT) < 0.1 THEN
   PUT STREAM rep UNFORMATTED string(loan.doc-ref,"x(20)") + " " + RESULT + " " 
                                                             + string(in-commi, "x(12)") + " "
                                                             + string(loan.cont-type, "x(15)")
							     + string(loan-acct.acct, "x(20)") SKIP.


END.

{preview2.i &stream="stream rep" &filename=spoolrep.tmp }

{intrface.del}

/*��।������ �⠢�� �१ ����� �奬� ���᫥���*/
PROCEDURE nachki:
  &GLOB use-tt use-tt
  {nachkin.p}
END.

/*��।������ "��ࠬ��஢ ���᫥���" */
PROCEDURE GetComm.
   DEFINE INPUT PARAMETER iRecComm   AS RECID NO-UNDO. /*recid ����� %% �⠢��*/
   DEFINE INPUT PARAMETER iValComm   AS DEC   NO-UNDO. /*���祭�� %% �⠢��*/
   FIND FIRST comm-rate WHERE RECID(comm-rate) = iRecComm NO-LOCK NO-ERROR.
   ASSIGN
    vComm-rate = iValComm.
END.

