/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: RPTEPS_XL.P
      Comment: ���� "����������� � ������ �⮨���� �।�� (�ଠ� MS EXCEL)"
   Parameters: ���
         Uses:
      Used by:
      Created: 09.08.2010 18:57 BOES    
     Modified: 09.08.2010 18:57 BOES    
*/

&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{lshpr.pro} 
{t-otch.i NEW}

{svarloan.def NEW}
{loan.pro}
{intrface.get comm}
{intrface.get instrum}
{intrface.get card}

{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{ksh-defs.i NEW}
{justasec}
{intrface.get refer}    /* ������⥪� �㦡� �ࠢ�筨���. */
{intrface.get date}


DEF VAR vNameCl AS CHARACTER NO-UNDO.
DEF VAR vCurStr AS CHARACTER NO-UNDO.
DEF VAR mBalance  AS DECIMAL NO-UNDO.
DEF VAR mStrTable AS CHAR    NO-UNDO.
DEF VAR mSt       AS DATE    NO-UNDO.
DEF VAR mSrok     AS DECIMAL NO-UNDO.
DEF VAR num-cond  AS CHAR    NO-UNDO.
DEF VAR cl-date   AS char    NO-UNDO.
DEF VAR Summa_    AS char    NO-UNDO.
DEF VAR st-proc   AS char    NO-UNDO.
DEF VAR sno       AS char    NO-UNDO.
DEF VAR dno       AS char    NO-UNDO.
DEF VAR name_cl   AS char    NO-UNDO.
DEF VAR mID_cl    AS char    NO-UNDO.

DEFINE TEMP-TABLE tloan FIELD tcont-code  AS CHAR  
                        field tacct       as char
                        field tname_cl    as char
                        field tID_cl    as char
                        field tNum-cond   as char  
                        field tsince      as char 
                        field tint-date   as char  
                        field tSumma_     as char 
                        FIELD tst-proc    AS CHAR   
                        field tmst        as date 
                        field tcl-date    as char 
                        field tsno        as char 
                        field tdno        as char 
                        field filial-id   as char 
                        Index x tmst.

/*�㭪�� ����祭�� �⠢�� �� ����*/
{getdates.i}

/* {getdate.i} /*⥯��� ���� �� �������� ����*/
 */
{empty ttnames}

RUN Insert_TTName("AddComm", "����  �� ���� �� " + string(end-date,"99.99.9999") ).

RUN Insert_TTName ("info", ""). 

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'info'
NO-LOCK NO-ERROR.
mStrTable = "".
for each loan where loan.contract = "�����"
/*                and loan.filial-id = shfilial */ 
/*2) �� ����᪥ ������� ���� (��� � � ���) � ����� 䨫����, ����䥫� ���㦠��� �� �ᥬ 䨫�����.
�� �祭� 㤮��� � ������ 䨫���� ����᪠�� ��� � �ନ஢��� ��騩 �� �ᥬ 䨫�����.(����� �((((()
 */
                NO-LOCK.
   find last  loan-acct where loan-acct.contract = loan.contract
                          and loan-acct.cont-code = loan.cont-code
                          and loan-acct.acct-type = "�����" no-lock no-error.
   find  FIRST acct where acct.acct = loan-acct.acct NO-LOCK no-error.

   if acct.cust-cat = "�" then do:
      find first cust-corp where cust-corp.cust-id = acct.cust-id no-lock no-error.
      if avail cust-corp then do:
         name_cl = cust-corp.name-short.   
         mID_cl  = string(cust-corp.cust-id).
      end.
   end.



   for each loan-cond where loan-cond.contract = loan.contract
                        and loan-cond.cont-code = loan.cont-code
/*
����� ����.
��������� �� ����� �⪮�४�஢��� ����� ���� (���ࢠ� ��� �� ��� �������)
� ���������,
��᪮⨭� ���� ���॥���

                        and loan-cond.since >= beg-date
                        and loan-cond.since <= end-date
*/
                        no-lock.

      cl-date  = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"close-date","").

      if cl-date <> "" then do :
         if date(cl-date) < beg-date or date(cl-date) > end-date then do:
            next.
         end.  
      end.
      Num-cond = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"���������","").
      st-proc  = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"����⠢��","").
      Summa_   = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"�㬬����","").
      sno      = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"��⍍�","").
      dno      = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since,"99/99/99"),"��⠍��","").
      if cl-date = ? then cl-date = "".
      mSt = loan-cond.since + loan-cond.int-date.
      create tloan.
      assign
         tloan.tcont-code  =  loan.cont-code
         tloan.tacct       =  loan-acct.acct
         tloan.tname_cl    =  name_cl
         tloan.tNum-cond   =  Num-cond
         tloan.tsince      =  string(loan-cond.since)
         tloan.tint-date   =  STRING(loan-cond.int-date)
         tloan.tSumma_     =  STRING(Summa_)
         tloan.tst-proc    =  STRING(st-proc)
         tloan.tmst        =  mSt
         tloan.tcl-date    =  cl-date
         tloan.tsno        =  sno
         tloan.tdno        =  dno
         tloan.tID_cl      =  mID_cl
         tloan.filial-id   = loan.filial-id
      .

   end.
END.

for each tloan.

         mStrTable = mStrTable + (IF {assigned mStrTable} THEN "~n"
                                                           ELSE "") +
         TRIM(STRING(tloan.tID_cl)) + "~n" +  
         TRIM(tloan.tcont-code) + "~n" +  
         TRIM(tloan.tacct) + "~n" +  
         TRIM(tloan.tname_cl) + "~n" +  
         TRIM(tloan.tNum-cond) + "~n" +  
         replace(TRIM(tloan.tsince),"/",".") + "~n" +  
         TRIM(tloan.tint-date) + "~n" +  
         TRIM(tloan.tSumma_) + "~n" +  
         TRIM(tloan.tst-proc) + "~n" +  
         replace(TRIM(string(tloan.tmst)),"/",".") + "~n" +  
         replace(TRIM(string(tloan.tcl-date)),"/",".")  + "~n" + 
         tloan.tsno  + "~n" + 
         replace(TRIM(string(tloan.tdno)),"/",".")  + "~n" + 
         "`" + TRIM(tloan.filial-id)   
         .
end.



RUN INSERT_TTNAME ("Info", "").

FIND FIRST ttNames WHERE
           ttnames.tname EQ 'Info'
NO-LOCK NO-ERROR.
ttnames.tvalue = mStrTable.

RUN printvd.p ("rko_xli",INPUT TABLE ttnames).


{intrface.del} 
