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
DEFINE VARIABLE mNaznPlat  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFIO       AS CHARACTER NO-UNDO.

DEFINE BUFFER acct FOR acct.
DEFINE BUFFER acct-rez FOR acct.
/*DEFINE BUFFER cust-corp FOR cust-corp.
DEFINE BUFFER person FOR person.
*/

DEFINE TEMP-TABLE ttDbt
	FIELD acct         AS CHARACTER
	FIELD acct-rez     AS CHARACTER
	FIELD CliName      AS CHARACTER
	FIELD AmountPos    AS CHARACTER
	FIELD AmountPr     AS CHARACTER
	FIELD AmountDoh    AS CHARACTER
	FIELD Srok         AS CHARACTER
	FIELD PrcRez       AS CHARACTER
	FIELD AmountPosRez AS CHARACTER
	FIELD Kurator      AS CHARACTER
   FIELD NaznPlat     AS CHARACTER
	FIELD Docum        AS CHARACTER
	FIELD Summ         AS CHARACTER
	FIELD Prim         AS CHARACTER
	FIELD FIO          AS CHARACTER
.

{getdate.i}

FOR EACH acct
	WHERE acct.filial-id EQ shFilial
	AND CAN-DO("60311*,60312*,60314*",acct.acct)
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
        /*Наименование клиента */
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
      /* срок закрытия */           
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
			ttDbt.Kurator = IF NUM-ENTRIES(mSrokClose, ";") GT 1 THEN TRIM(ENTRY(2, mSrokClose, ";"),"-") ELSE " "
			ttDbt.Prim = (IF NUM-ENTRIES(mSrokClose, ";") GT 2 THEN  "'" + ENTRY(3, mSrokClose, ";") ELSE " ") +
						(IF NUM-ENTRIES(mSrokClose, ";") GT 3 THEN  ENTRY(4, mSrokClose, ";") ELSE " ").

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
      /* ФИО */
      mFIO = "".
      FOR EACH op-entry WHERE TRUE
         AND op-entry.op-date GT DATE("01/01/1900")
         AND (op-entry.acct-db EQ acct.acct OR op-entry.acct-cr EQ acct.acct)
         NO-LOCK:
         mFIO = op-entry.user-id.
      END.
      FIND FIRST _user WHERE _user._userid EQ mFIO NO-LOCK NO-ERROR.
      IF AVAIL(_user) THEN 
         mFIO = mFIO + "/" + TRIM(_user._user-name).
      IF mFIO = "" THEN mFIO = acct.user-id.
      ttDbt.FIO = mFIO.
	END.
END.	

OUTPUT to VALUE("dz2.csv") CONVERT TARGET "1251".

PUT UNFORMATTED
";;ОТЧЕТ ПО ДЕБИТОРСКОЙ ЗАДОЛЖЕННОСТИ на счетах 60311 на " + STRING(end-date, "99.99.9999") + " г."
SKIP.
PUT UNFORMATTED 
   "ФИО Исполнителя" ";" 
   "Номер счета" ";" 
   "Наименование Контрагента" ";" 
   "Общая сумма задолженности" ";" 
   "Сумма текущей задолженности" ";" 
   "Сумма просроченной задолженности" ";"
   "Срок закрытия" ";"
   "Куратор" ";"
   "Назначение платежа" ";"
   "% резерва" ";"
   "Остаток на счете резерва" ";"
   "Номер счета резерва" ";"
   "Примечание"
SKIP.

FOR EACH ttDbt WHERE
   ttDbt.acct BEGINS "60311"
   NO-LOCK:
	PUT UNFORMATTED
	   ttDbt.FIO ";"
	   "'" ttDbt.acct      ";"
	   ttDbt.CliName      ";"
      ";"
      ttDbt.AmountPos    ";"
	   ttDbt.AmountPr     ";"
	   ttDbt.Srok         ";"
	   ttDbt.Kurator      ";"
	   ttDbt.Prim         ";"
	   ttDbt.PrcRez       ";"
	   ttDbt.AmountPosRez ";"
	   "'" ttDbt.acct-rez ";"
	   ttDbt.NaznPlat     ";"
	SKIP.
END.

PUT UNFORMATTED "-----------------" SKIP.
PUT UNFORMATTED
";;ОТЧЕТ ПО ДЕБИТОРСКОЙ ЗАДОЛЖЕННОСТИ на счетах 60312 на " + STRING(end-date, "99.99.9999") + " г."
SKIP.
PUT UNFORMATTED 
   "ФИО Исполнителя" ";" 
   "Номер счета" ";" 
   "Наименование Контрагента" ";" 
   "Общая сумма задолженности" ";" 
   "Сумма текущей задолженности" ";" 
   "Сумма просроченной задолженности" ";"
   "Срок закрытия" ";"
   "Куратор" ";"
   "Назначение платежа" ";"
   "% резерва" ";"
   "Остаток на счете резерва" ";"
   "Номер счета резерва" ";"
   "Примечание"
SKIP.

FOR EACH ttDbt WHERE
   ttDbt.acct BEGINS "60312"
   NO-LOCK:
	PUT UNFORMATTED
	   ttDbt.FIO ";"
	   "'" ttDbt.acct      ";"
	   ttDbt.CliName      ";"
      ";"
      ttDbt.AmountPos    ";"
	   ttDbt.AmountPr     ";"
	   ttDbt.Srok         ";"
	   ttDbt.Kurator      ";"
	   ttDbt.Prim         ";"
	   ttDbt.PrcRez       ";"
	   ttDbt.AmountPosRez ";"
	   "'" ttDbt.acct-rez ";"
	   ttDbt.NaznPlat     ";"
	SKIP.
END.

PUT UNFORMATTED "-----------------" SKIP.
PUT UNFORMATTED
";;ОТЧЕТ ПО ДЕБИТОРСКОЙ ЗАДОЛЖЕННОСТИ на счетах 60314 на " + STRING(end-date, "99.99.9999") + " г."
SKIP.
PUT UNFORMATTED 
   "ФИО Исполнителя" ";" 
   "Номер счета" ";" 
   "Наименование Контрагента" ";" 
   "Общая сумма задолженности" ";" 
   "Сумма текущей задолженности" ";" 
   "Сумма просроченной задолженности" ";"
   "Срок закрытия" ";"
   "Куратор" ";"
   "Назначение платежа" ";"
   "% резерва" ";"
   "Остаток на счете резерва" ";"
   "Номер счета резерва" ";"
   "Примечание"
SKIP.
	
FOR EACH ttDbt WHERE
   ttDbt.acct BEGINS "60314"
   NO-LOCK:
	PUT UNFORMATTED
	   ttDbt.FIO ";"
	   "'" ttDbt.acct      ";"
	   ttDbt.CliName      ";"
      ";"
      ttDbt.AmountPos    ";"
	   ttDbt.AmountPr     ";"
	   ttDbt.Srok         ";"
	   ttDbt.Kurator      ";"
	   ttDbt.Prim         ";"
	   ttDbt.PrcRez       ";"
	   ttDbt.AmountPosRez ";"
	   "'" ttDbt.acct-rez ";"
	   ttDbt.NaznPlat     ";"
	SKIP.
END.

OUTPUT CLOSE.

RUN sndbispc ("file=" + "dz2.csv" + ";class=bq").

{intrface.del}

