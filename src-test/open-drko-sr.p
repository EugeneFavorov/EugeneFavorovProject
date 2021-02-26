 /*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: open-lrko.p
      Comment: ����⨥ ������஢ ��� 
   Parameters:
         Uses:
      Used by:
      Created: 
*/

{globals.i}
{prn-doc.def &with_proc=YES}
{tmprecid.def}
{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{dpsproc.def}
{ksh-defs.i NEW}
{wordwrap.def}
{intrface.get xclass}


   DEFINE INPUT PARAMETER iStr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mName	     AS CHARACTER EXTENT 2 NO-UNDO.
   DEF BUFFER bacct for acct.
   
   DEFINE VARIABLE mUser     AS CHAR  NO-UNDO.
   DEFINE VARIABLE iBranch   AS CHAR  NO-UNDO.
   DEFINE VARIABLE tmpdate   as date  no-UNDO.  
   DEFINE VARIABLE Summa     as dec   no-UNDO.  
   DEFINE VARIABLE Srok      as int   no-UNDO.  
   DEFINE VARIABLE tarif     as dec   no-UNDO.  
   DEFINE VARIABLE NCode     as Char  no-UNDO.  
   DEFINE VARIABLE NCodeCur  as Char  no-UNDO.  
   DEFINE VARIABLE vOk       as logic no-UNDO.  
   DEFINE VARIABLE counter_  as int   no-UNDO.  
   DEFINE VARIABLE date-beg  as date  no-UNDO.  
   DEFINE VARIABLE mKodTar   as char  no-UNDO.  

   if iStr <> "" and iStr <> ? then do:
      mKodTar = iStr.
   end.
   else do:
      mKodTar = "��".
   end.
   mUser =  USERID("bisquit").   

   find first tmprecid NO-LOCK no-error.
   if not avail tmprecid then message " �� �⬥⨫� ����." view-as alert-box. 

   find FIRST loan WHERE RECID(loan) EQ tmprecid.id NO-LOCK no-error.
   if not avail loan then do:
      message " �� ��諨 �������." view-as alert-box.
      return.
   end.
   find first loan-acct where loan-acct.contract  eq loan.contract
                          and loan-acct.cont-code eq loan.cont-code 
                          and loan-acct.acct-type eq "�����"
                          no-lock no-error.
   if not avail loan-acct then do:
      message " �� ��諨 ���� � ஫�� ����� � ����⥪� ��⮢ �������." view-as alert-box.
      return.
   end.
   find FIRST acct WHERE acct.acct EQ loan-acct.acct  NO-LOCK no-error.
   if not avail acct then do:
      message " �� ��諨 ����." view-as alert-box.
      return.
   end.
   counter_ = 1.

   for each loan-cond  where loan-cond.contract   eq loan-acct.contract
                         and loan-cond.cont-code  eq loan-acct.cont-code
                         and loan-cond.class-code eq "loanr-cond"
                         no-lock.
       counter_ = counter_ + 1.
   end.


/*
   find first loan-cond  where loan-cond.contract   eq loan-acct.contract
                           and loan-cond.cont-code  eq loan-acct.cont-code
                           and loan-cond.class-code eq "loanr-cond"
                           and loan-cond.since      eq today
                           no-lock no-error.
   if avail loan-cond then do:
        message " � ࠧ�� ����� ���뢠�� ��� �������⥫��� ᮣ��襭�� � ���� ���� ?" view-as alert-box.
        return.
   end.
*/
   
   tmpdate = today.
   beg-date = tmpdate.
   end-date = today.
   
   
   pause 0.

   define FRAME frame_date_codes 
	 Srok     label "�ப (� ����) " format ">>9"	
         beg-date label "��� ��砫�   " format "99.99.9999"
         Summa    label "�㬬� ��� ���" format ">>>,>>>,>>9.99"
	 tarif    label "��業�       " format ">>9.9999"	
	 with 1  COL 1 down 
         width 50 CENTERED OVERLAY ROW 11 TITLE "����� ��� ���.ᮣ��襭�� <�।�������� ���⮪>".

   do ON ERROR  UNDO, LEAVE
      ON ENDKEY UNDO, LEAVE  :
  
      UPDATE
	Srok  
        beg-date
	Summa 
 	tarif 
  
      WITH FRAME frame_date_codes
      EDITING:
         READKEY.
         if LASTKEY EQ KEYCODE("ESC") THEN
            return.
        if LASTKEY EQ KEYCODE("F1")
 	THEN do:
            CASE FRAME-FIELD:
           end case.
 	end.
        if LASTKEY EQ KEYCODE("ENTER")
 	THEN do:
            CASE FRAME-FIELD:
               WHEN "Summa" THEN
                  DO:
/*                    run find-tarif.p(Summa:SCREEN-VALUE,Srok:SCREEN-VALUE). 
*/
                    if (lastkey eq 13 or lastkey eq 10) and pick-value ne ?
                    then tarif:SCREEN-VALUE =  pick-value.
  
                  END.
               WHEN "Srok" THEN
                  DO:
/*                    run find-tarif.p(Summa:SCREEN-VALUE,Srok:SCREEN-VALUE). 
*/
                    if (lastkey eq 13 or lastkey eq 10) and pick-value ne ?
                    then tarif:SCREEN-VALUE =  pick-value.
                  END.
               WHEN "beg-date" THEN
                  DO:
/*                    if int(Srok:SCREEN-VALUE) <> 0 then do:
                       for each loan-cond where loan-cond.contract   = loan-acct.contract
                                            and loan-cond.cont-code  = loan-acct.cont-code
                                            and loan-cond.class-code = "loanr-cond"
                                            no-lock.
                         if     GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "���䏫��",?) = mKodTar 
                            and GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "close-date",?) = ? 
                         then do:
                            if date(beg-date:SCREEN-VALUE) < date(loan-cond.since + loan-cond.int-date) then do:
                               NCodeCur = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "���������",?). 
                               message "��� ������ ������ � ��ਮ� ����⢨� ��㣮�� ᮣ��襭��. (�����襭�� N " NCodeCur " �� " string(loan-cond.since) 
                                       ". ��� ��� ����砭�� " loan-cond.since + loan-cond.int-date  view-as alert-box.
                            end.
                         end. 
                      end.
                    end.
*/
                  END.
            end case.
           APPLY LASTKEY.
	end.
 	ELSE APPLY LASTKEY.
    
      end. /* EDITING: */
   end.  /* do on */	

   end-date = beg-date + Srok.
   for each loan-cond where loan-cond.contract   = loan-acct.contract
                        and loan-cond.cont-code  = loan-acct.cont-code
                        and loan-cond.class-code = "loanr-cond"
                        no-lock.
      if     GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "���䏫��",?) = mKodTar 
         and GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "close-date",?) = ? 
      then do:
         if beg-date < loan-cond.since + loan-cond.int-date then do:
            NCodeCur = GetXattrValueEx("loan-cond",loan-cond.contract + "," + loan-cond.cont-code + "," + string(loan-cond.since), "���������",?). 
            message "��� ������ ������ � ��ਮ� ����⢨� ��㣮�� ᮣ��襭��. (�����襭�� N " NCodeCur " �� " string(loan-cond.since) 
                    ". ��� ��� ����砭�� " loan-cond.since + loan-cond.int-date  view-as alert-box.
            return.
         end.
      end. 
   end.

   CREATE loan-cond.
   ASSIGN
      loan-cond.contract   = loan-acct.contract
      loan-cond.cont-code  = loan-acct.cont-code
      loan-cond.since      = beg-date
      loan-cond.int-date   = Srok
      loan-cond.class-code = "loanr-cond"
   .
   NCode = string(counter_).

   vOk =  UpdateSigns("loan-cond","�����" + "," + loan-cond.cont-code + "," + string(loan-cond.since),"���䏫��",mKodTar,no).
   vOk =  UpdateSigns("loan-cond","�����" + "," + loan-cond.cont-code + "," + string(loan-cond.since),"���������",NCode,no).
   vOk =  UpdateSigns("loan-cond","�����" + "," + loan-cond.cont-code + "," + string(loan-cond.since),"����⠢��",string(tarif),no).
   vOk =  UpdateSigns("loan-cond","�����" + "," + loan-cond.cont-code + "," + string(loan-cond.since),"�㬬����",string(Summa),no).

message "�������⥫쭮� ᮣ��襭�� �����" view-as alert-box.

/*
   output to "loanqq.txt".
   for each loan where loan.class-code = "loanr".
      export loan.
   end.
   output close.
*/
return.



