/* ��饭�� ����� 
9
12
82
509
519
  */

{globals.i}

{intrface.get loan}
{intrface.get cdrep}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get lv}
{intrface.get chwch}
{tmprecid.def}

end-date = today.
{getdate.i
   &DateLabel = "��� ��饭��"
} 

define var summParam as decimal no-undo.
DEF VAR vCurr         AS CHAR NO-UNDO. /* ����� ��ࠬ��� */
DEF VAR vAmtRub       AS DEC  NO-UNDO. /* �㬬� ��ࠬ��� � �㡫�� */
def var lProsh AS LOGICAL NO-UNDO.


def var myStartDate as date FORMAT "99/99/9999" view-as FILL-IN init today.
def var myEndDate as date FORMAT "99/99/9999" view-as FILL-IN init today.

define temp-table tt-loans
    FIELD cont-code like loan.cont-code
	INDEX idx cont-code
    .

for each tt-loans:
	delete tt-loans.
end.



for each tmprecid,
    first loan where recid(loan) eq tmprecid.id no-lock:
	lProsh = FALSE.
	/* ���� ������� �� ���� */
			/* ���祭�� ��ࠬ��� 9 �� ���⠭��� ���� ������� */
			RUN ALL_PARAM IN h_Loan ("�।��",                  /* ��� ������� */
	                         loan.cont-code,                 /* ����� ������� */
        	                 9, /* ��� ��ࠬ��� */
				 end-date,	
                	         OUTPUT summParam,                     /* �㬬� ��ࠬ��� */
                        	 OUTPUT vCurr,                    /* ����� ��ࠬ��� */
	                         OUTPUT vAmtRub).                 /* �㬬� ��ࠬ��� � �㡫�� */
			if summParam > 0 then do:
				RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
                        	5,           /* ���᫥��  */
	                        9,           /* ᯨᠭ�  */
        	                NO,                   /* ���. ������   */
                	        ?).                    /* handle �஢���� */ 
				lProsh = true.
			end. 

			/* ���祭�� ��ࠬ��� 12 �� ���⠭��� ���� ������� */
			RUN ALL_PARAM IN h_Loan ("�।��",                  /* ��� ������� */
	                         loan.cont-code,                 /* ����� ������� */
        	                 12, /* ��� ��ࠬ��� */
				 end-date,
                	         OUTPUT summParam,                     /* �㬬� ��ࠬ��� */
                        	 OUTPUT vCurr,                    /* ����� ��ࠬ��� */
	                         OUTPUT vAmtRub).                 /* �㬬� ��ࠬ��� � �㡫�� */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        5,           /* ���᫥��  */
        	                12,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
				lProsh = true.
			end. 

			/* ���祭�� ��ࠬ��� 12 �� ���⠭��� ���� ������� */
			RUN ALL_PARAM IN h_Loan ("�।��",                  /* ��� ������� */
	                         loan.cont-code,                 /* ����� ������� */
        	                 82, /* ��� ��ࠬ��� */
				 end-date,
                	         OUTPUT summParam,                     /* �㬬� ��ࠬ��� */
                        	 OUTPUT vCurr,                    /* ����� ��ࠬ��� */
	                         OUTPUT vAmtRub).                 /* �㬬� ��ࠬ��� � �㡫�� */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        5,           /* ���᫥��  */
        	                82,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
				lProsh = true.
			end. 

			/* ���祭�� ��ࠬ��� 12 �� ���⠭��� ���� ������� */
			RUN ALL_PARAM IN h_Loan ("�।��",                  /* ��� ������� */
	                         loan.cont-code,                 /* ����� ������� */
        	                 509, /* ��� ��ࠬ��� */
				 end-date,
                	         OUTPUT summParam,                     /* �㬬� ��ࠬ��� */
                        	 OUTPUT vCurr,                    /* ����� ��ࠬ��� */
	                         OUTPUT vAmtRub).                 /* �㬬� ��ࠬ��� � �㡫�� */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                loan.since,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        5,           /* ���᫥��  */
        	                516,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        516,           /* ���᫥��  */
        	                509,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
				lProsh = true.
			end. 

			/* ���祭�� ��ࠬ��� 12 �� ���⠭��� ���� ������� */
			RUN ALL_PARAM IN h_Loan ("�।��",                  /* ��� ������� */
	                         loan.cont-code,                 /* ����� ������� */
        	                 519, /* ��� ��ࠬ��� */
				 end-date,
                	         OUTPUT summParam,                     /* �㬬� ��ࠬ��� */
                        	 OUTPUT vCurr,                    /* ����� ��ࠬ��� */
	                         OUTPUT vAmtRub).                 /* �㬬� ��ࠬ��� � �㡫�� */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        519,           /* ���᫥��  */
        	                5,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
		 		RUN Cr_LoanInt IN h_lv ("�।��",             /* �����䨪���   */
	                        loan.cont-code,             /* �������        */
        	                end-date,            /* ��� ����樨   */
                	        summParam,              /* �㬬� ����樨  */
	                        5,           /* ���᫥��  */
        	                519,           /* ᯨᠭ�  */
                	        NO,                   /* ���. ������   */
	                        ?).                    /* handle �஢���� */ 
				lProsh = true.
			end. 
	IF lProsh THEN DO:
	    create tt-loans.
		tt-loans.cont-code = loan.cont-code.
	    release tt-loans.
	end.

end.


for each tt-loans no-lock:
	RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
               tt-loans.cont-code,      /* ����� �������. */
               date(today),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
               FALSE,
               TRUE).
	RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
               tt-loans.cont-code,      /* ����� �������. */
               date(today - 10),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
               FALSE,
               TRUE).
	RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
               tt-loans.cont-code,      /* ����� �������. */
               date(today),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
               FALSE,
               TRUE).
end.

/*
 {setdest.i} 

for each tt-loans no-lock:
	if (tt-loans.nopereschet) then
			put unformatted "�訡�� ������ ������� " + tt-loans.cont-code + " �� ���� " + string(tt-loans.dateGash) skip.
	put unformatted tt-loans.cont-code + ' ' + string(tt-loans.dateGash) + ' ' + string(tt-loans.sumProshen9) + ' ' + string(tt-loans.sumProshen12) skip. 
end.

 {preview.i} 
*/

{intrface.del}

   
