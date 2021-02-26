/*
 Отчет по дебиторской задолженности.
*/

{globals.i}
{sh-defs.i}
{intrface.get xclass}
{intrface.get comm}

DEFINE VARIABLE mAcctRez AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAmount AS DECIMAL NO-UNDO.
DEFINE VARIABLE mAcctDetails AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE mSrokClose AS CHARACTER NO-UNDO.


DEFINE BUFFER acct FOR acct.
DEFINE BUFFER acct-rez FOR acct.
/*DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
*/

DEFINE TEMP-TABLE ttDbt
	FIELD acct AS CHARACTER
	FIELD acct-rez AS CHARACTER
	FIELD CliName AS CHARACTER
	FIELD AmountPos AS CHARACTER
	FIELD AmountPr AS CHARACTER
	FIELD AmountDoh AS CHARACTER
	FIELD Srok AS CHARACTER
	FIELD PrcRez AS CHARACTER
	FIELD AmountPosRez AS CHARACTER
	FIELD Kurator AS CHARACTER
	FIELD Docum AS CHARACTER
	FIELD Summ AS CHARACTER
	FIELD Prim AS CHARACTER
.


{getdate.i}
/*
message (end-date - 1) view-as alert-box.
*/
FOR EACH acct
	WHERE acct.filial-id EQ shFilial
	AND CAN-DO("60311*,60312*,60314*", acct.acct)
	NO-LOCK:
/* проверяем остаток на дату расчета */
	RUN acct-pos IN h_base (
		acct.acct,
        acct.currency,
        end-date,
        end-date, "√"
        ).
	IF acct.currency EQ "" THEN
		mAmount = ABSOLUTE(sh-bal).
	ELSE
		mAmount = ABSOLUTE(sh-val).

	IF mAmount GT 0 THEN
	DO:
/* Если остаток ненулевой, создаем запись */	
		CREATE ttDbt.
		ASSIGN
			ttDbt.acct = SUBSTRING(acct.acct, 1, 20)

			ttDbt.acct-rez = GetLinks(acct.class-code,
							acct.acct + "," + acct.currency,
							?,
				            "acct-reserve",
				            "|",
				            end-date)

			ttDbt.AmountPos = STRING(mAmount, ">>>>>>>>>>>9.99")
			ttDbt.AmountPr = " "
			ttDbt.AmountDoh = " "
			ttDbt.Docum = " "
			ttDbt.Summ = " "
		.
/* Счет резерва */		
		IF ttDbt.acct-rez GT "" THEN
		DO:
			FIND FIRST acct-rez
				WHERE acct-rez.acct EQ ENTRY(1, ENTRY(1, ttDbt.acct-rez, "|"))
				AND acct-rez.currency EQ ENTRY(2, ENTRY(1, ttDbt.acct-rez, "|"))
			NO-LOCK NO-ERROR.
			IF AVAILABLE(acct-rez) THEN
			DO:
				ttDbt.acct-rez = SUBSTRING(acct-rez.acct, 1, 20).
				RUN acct-pos IN h_base (
					acct-rez.acct,
			        acct-rez.currency,
			        end-date,
			        end-date, "√"
			        ).
				IF acct-rez.currency EQ "" THEN
					ttDbt.AmountPosRez = STRING(ABSOLUTE(sh-bal), ">>>>>>>>>>>9.99").
				ELSE
					ttDbt.AmountPosRez = STRING(ABSOLUTE(sh-val), ">>>>>>>>>>>9.99").
			END.
			ELSE
				ttDbt.acct-rez = " ".
		END.
		ELSE
			ttDbt.acct-rez = " ".
/* Наиемнование клиента */
        IF acct.cust-cat NE "В" THEN
        DO:
	        RUN GetCustName IN h_base (acct.cust-cat,
	                                   acct.cust-id,
	                                   ?,
	                                   OUTPUT mAcctDetails[1],
	                                   OUTPUT mAcctDetails[2],
	                                   INPUT-OUTPUT mAcctDetails[3]).
	        mAcctDetails[1] = mAcctDetails[1] + " " + mAcctDetails[2].
            ttDbt.CliName = mAcctDetails[1]
            .
        END.
        ELSE
            ttDbt.CliName = REPLACE(acct.details, "Расчеты с ", "").
/* срол закрытия */           
		mSrokClose = GetXAttrValueEx("acct",
			acct.acct + "," + acct.currency,
			"Срокзакрытия",
			"NO"
			).
		IF mSrokClose EQ "NO" THEN
			ASSIGN
			ttDbt.Srok = " "
			ttDbt.Kurator = " "
			ttDbt.Prim = " "
			.
		ELSE
			ASSIGN
			ttDbt.Srok = ENTRY(1, mSrokClose, ";")
			ttDbt.Kurator = IF NUM-ENTRIES(mSrokClose, ";") GT 1 THEN ENTRY(2, mSrokClose, ";") ELSE " "
			ttDbt.Prim = (IF NUM-ENTRIES(mSrokClose, ";") GT 2 THEN  "'" + ENTRY(3, mSrokClose, ";") ELSE " ") +
						(IF NUM-ENTRIES(mSrokClose, ";") GT 3 THEN  ENTRY(4, mSrokClose, ";") ELSE " ")
			.

		ttDbt.PrcRez = STRING(GET_COMM ("%Рез",  /* Код комиссии. */
								RECID(acct), /* Идентификатор счета. */
								acct.currency,  /* Код валюты. */
								"",  /* Код КАУ ("" - по умолчанию). */
								0.0,   /* Миним. остаток (0 - поумолчанию). */
								0,   /* Период/срок (0 - поумолчанию). */
								end-date  /* Дата поиска. */
								)
						).
		IF ttDbt.PrcRez EQ ? THEN
			ttDbt.PrcRez = " ".

	END.

END.	

OUTPUT to VALUE("dz2.csv") CONVERT TARGET "1251".

	PUT UNFORMATTED
	" "
	";"
	" "
	";"
	"ОТЧЕТ ПО ДЕБИТОРСКОЙ ЗАДОЛЖЕННОСТИ  на "
	";"
	STRING(end-date, "99.99.9999")
	";"
	" г."
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	";"
	" "
	SKIP
	"Расчеты с поставщиками,подрядчиками и покупателями(60311,60312,60314)"
	";"
	"Резервы на возможные потери (60324)"
	";"
	"Наименование Контрагента"
	";"
	"Сумма текущей ДЗ"
	";"
	"Сумма просроченной ДЗ"
	";"
	"Сумма  будущих доходов"
	";"
	"Срок"
	";"
	"% резерва на ВП"
	";"
	"Остаток на счете Резерв на ВП"
	";"
	"Куратор"
	";"
	"Документ"
	";"
	"Сумма"
	";"
	"Примечание(назначение платежа)"
	SKIP
	.



FOR EACH ttDbt:
	PUT UNFORMATTED
	"'"
	ttDbt.acct
	";'"
	ttDbt.acct-rez
	";"
	ttDbt.CliName
	";"
	ttDbt.AmountPos
	";"
	ttDbt.AmountPr
	";"
	ttDbt.AmountDoh
	";"
	ttDbt.Srok
	";"
	ttDbt.PrcRez
	";"
	ttDbt.AmountPosRez
	";"
	ttDbt.Kurator
	";"
	ttDbt.Docum
	";"
	ttDbt.Summ
	";"
	ttDbt.Prim
	SKIP.


END.

OUTPUT CLOSE.

RUN sndbispc ("file=" + "dz2.csv" + ";class=bq").

{intrface.del}

