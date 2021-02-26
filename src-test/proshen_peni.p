/* Прощение Пеней 
9
12
82
509
519
  */

{globals.i}

{intrface.get loan}
{intrface.get cdrep}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get lv}
{intrface.get chwch}
{tmprecid.def}

end-date = today.
{getdate.i
   &DateLabel = "Дата прощения"
} 

define var summParam as decimal no-undo.
DEF VAR vCurr         AS CHAR NO-UNDO. /* Валюта параметра */
DEF VAR vAmtRub       AS DEC  NO-UNDO. /* Сумма параметра в рублях */
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
	/* расчет договора на дату */
			/* значение параметра 9 на расчитанную дату договора */
			RUN ALL_PARAM IN h_Loan ("Кредит",                  /* Тип договора */
	                         loan.cont-code,                 /* Номер договора */
        	                 9, /* Код параметра */
				 end-date,	
                	         OUTPUT summParam,                     /* Сумма параметра */
                        	 OUTPUT vCurr,                    /* Валюта параметра */
	                         OUTPUT vAmtRub).                 /* Сумма параметра в рублях */
			if summParam > 0 then do:
				RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
                        	5,           /* начислено  */
	                        9,           /* списано  */
        	                NO,                   /* Авт. операция   */
                	        ?).                    /* handle проводки */ 
				lProsh = true.
			end. 

			/* значение параметра 12 на расчитанную дату договора */
			RUN ALL_PARAM IN h_Loan ("Кредит",                  /* Тип договора */
	                         loan.cont-code,                 /* Номер договора */
        	                 12, /* Код параметра */
				 end-date,
                	         OUTPUT summParam,                     /* Сумма параметра */
                        	 OUTPUT vCurr,                    /* Валюта параметра */
	                         OUTPUT vAmtRub).                 /* Сумма параметра в рублях */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        5,           /* начислено  */
        	                12,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
				lProsh = true.
			end. 

			/* значение параметра 12 на расчитанную дату договора */
			RUN ALL_PARAM IN h_Loan ("Кредит",                  /* Тип договора */
	                         loan.cont-code,                 /* Номер договора */
        	                 82, /* Код параметра */
				 end-date,
                	         OUTPUT summParam,                     /* Сумма параметра */
                        	 OUTPUT vCurr,                    /* Валюта параметра */
	                         OUTPUT vAmtRub).                 /* Сумма параметра в рублях */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        5,           /* начислено  */
        	                82,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
				lProsh = true.
			end. 

			/* значение параметра 12 на расчитанную дату договора */
			RUN ALL_PARAM IN h_Loan ("Кредит",                  /* Тип договора */
	                         loan.cont-code,                 /* Номер договора */
        	                 509, /* Код параметра */
				 end-date,
                	         OUTPUT summParam,                     /* Сумма параметра */
                        	 OUTPUT vCurr,                    /* Валюта параметра */
	                         OUTPUT vAmtRub).                 /* Сумма параметра в рублях */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                loan.since,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        5,           /* начислено  */
        	                516,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        516,           /* начислено  */
        	                509,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
				lProsh = true.
			end. 

			/* значение параметра 12 на расчитанную дату договора */
			RUN ALL_PARAM IN h_Loan ("Кредит",                  /* Тип договора */
	                         loan.cont-code,                 /* Номер договора */
        	                 519, /* Код параметра */
				 end-date,
                	         OUTPUT summParam,                     /* Сумма параметра */
                        	 OUTPUT vCurr,                    /* Валюта параметра */
	                         OUTPUT vAmtRub).                 /* Сумма параметра в рублях */
			if summParam > 0 then do:
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        519,           /* начислено  */
        	                5,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
		 		RUN Cr_LoanInt IN h_lv ("Кредит",             /* Идентификатор   */
	                        loan.cont-code,             /* договора        */
        	                end-date,            /* Дата операции   */
                	        summParam,              /* Сумма операции  */
	                        5,           /* начислено  */
        	                519,           /* списано  */
                	        NO,                   /* Авт. операция   */
	                        ?).                    /* handle проводки */ 
				lProsh = true.
			end. 
	IF lProsh THEN DO:
	    create tt-loans.
		tt-loans.cont-code = loan.cont-code.
	    release tt-loans.
	end.

end.


for each tt-loans no-lock:
	RUN l-calc2.p ("Кредит",       /* Назначение договора. */
               tt-loans.cont-code,      /* Номер договора. */
               date(today),   /* Окончание договора + день для выполнения автом. */
               FALSE,
               TRUE).
	RUN l-calc2.p ("Кредит",       /* Назначение договора. */
               tt-loans.cont-code,      /* Номер договора. */
               date(today - 10),   /* Окончание договора + день для выполнения автом. */
               FALSE,
               TRUE).
	RUN l-calc2.p ("Кредит",       /* Назначение договора. */
               tt-loans.cont-code,      /* Номер договора. */
               date(today),   /* Окончание договора + день для выполнения автом. */
               FALSE,
               TRUE).
end.

/*
 {setdest.i} 

for each tt-loans no-lock:
	if (tt-loans.nopereschet) then
			put unformatted "Ошибка пересчета договора " + tt-loans.cont-code + " на дату " + string(tt-loans.dateGash) skip.
	put unformatted tt-loans.cont-code + ' ' + string(tt-loans.dateGash) + ' ' + string(tt-loans.sumProshen9) + ' ' + string(tt-loans.sumProshen12) skip. 
end.

 {preview.i} 
*/

{intrface.del}

   
