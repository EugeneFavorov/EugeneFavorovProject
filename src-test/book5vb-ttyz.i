/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: book5vb-tty.i
      Comment: расчет книги регистрации лицевых счетов
               (переделан из book5v-tty.i)
   Parameters:
         Uses:
      Used by: book5vb-tty.p
      Created: 01.03.2012 kraa
     Modified: 
*/
DEF VAR fname AS CHAR NO-UNDO. 
DEF STREAM ws.
def var eol as char format "x(2)" no-undo.
def var col7 as char no-undo.
eol = chr(13) + chr(10).

fname = "./_a.txt".  
OUTPUT STREAM ws TO VALUE (fname)
                 CONVERT TARGET "1251" SOURCE "IBM866".

PUT STREAM ws UNFORMATTED "												КНИГА РЕГИСТРАЦИИ ЛИЦЕВЫХ СЧЕТОВ C "+ STRING(beg-date, "99.99.9999") + " ПО " + STRING(end-date, "99.99.9999") + eol.
      
PUT STREAM ws UNFORMATTED    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" + eol.
PUT STREAM ws UNFORMATTED    "|   Дата   |         Дата и номер договора,         |                     |                                                                                                    |       Вид банковского счета,        | Порядок и периодичность |  Дата сообщения  |   Дата   |                 |   Дата сообщения   |" + eol.
PUT STREAM ws UNFORMATTED    "| открытия |         на основании которого          |     Номер счета     |                                       Фамилия, имя, отчество                                       |     счета по вкладу (депозиту)      |     выдачи выписок      |налоговым органам | закрытия |   Примечание    | налоговым органам  |" + eol.
PUT STREAM ws UNFORMATTED    "|  счета   |           открывается счет             |                     |                                       или наименование клиента                                     |    или наименование (цель) счета    |        из счета         |   об открытии    |  счета   |                 |     о закрытии     |" + eol.
PUT STREAM ws UNFORMATTED    "|          |                                        |                     |                                                                                                    |                                     |                         |банковского счета |          |                 |  банковского счета |" + eol.
PUT STREAM ws UNFORMATTED    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" + eol.
PUT STREAM ws UNFORMATTED    "|    1     |                   2                    |          3          |                                                   4                                                |                 5                   |            6            |        7         |     8    |        9        |          10        |" + eol.
PUT STREAM ws UNFORMATTED    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" + eol.
	  

FOR EACH bal-acct WHERE CAN-DO (mAcctCat, bal-acct.acct-cat)
    NO-LOCK,
    EACH acct OF bal-acct
         WHERE ((acct.open-date  GE beg-date AND acct.open-date LE end-date)
             OR (acct.close-date GE beg-date AND acct.close-date LE end-date) 
             OR (acct.open-date  LE beg-date AND (acct.close-date EQ ?
                                                  OR
                                                  acct.close-date GT beg-date)))
            AND acct.cust-cat BEGINS mSewMode /* юр,физ,банк,все */
AND acct.filial-id = '0400'
			/*AND acct.bal-acct > 80002*/
            /*AND acct.user-id BEGINS access
            AND CAN-DO(list-id,acct.user-id)
            AND (   shMode <> TRUE
                 OR (    shMode         = TRUE
                     AND acct.filial-id = shFilial))*/
   NO-LOCK
   BREAK BY bal-acct.bal-acct
/*         BY acct.currency 
         BY SUBSTR(acct.acct,10,11) */
         BY acct.open-date
         BY SUBSTRING(acct.acct,1,8) + SUBSTRING(acct.acct,10):
	
	
   mCustCat2 = GetXAttrValueEx("bal-acct",STRING(acct.bal-acct),"ПрКл","Клиентский").

   IF mCustCat2 EQ "Внутренний" THEN 
      name[1] = FGetSetting("Банк","","") + " " + FGetSetting("БанкГород","","").
   ELSE DO:
      RUN GetCustName IN h_base(acct.cust-cat,
                                acct.cust-id,
                                ?,
                                OUTPUT in-name[1],
                                OUTPUT in-name[2],
                                INPUT-OUTPUT in-name[3]).
      IF     acct.cust-cat EQ "В"  
      THEN
         ASSIGN 
            in-name[1] = FGetSetting("Банк","","") + " " + FGetSetting("БанкГород","","")
            in-name[2] =""
         .
      ELSE
         IF {assigned acct.details} 
         THEN
            ASSIGN
               in-name[1] = acct.details
               in-name[2] = ""
            .
       
      ASSIGN
         name[1]    = in-name[1] + " " + in-name[2]
         in-name[2] = ""
      .
   END.

   FORM
      acct.open-date AT 2  FORMAT "99.99.9999" 
      col1[1]              FORMAT "x(40)"   
	  acct.acct 
      name[1]              FORMAT "x(50)"   
      col5[1]              FORMAT "x(37)"                               
      col2[1]              FORMAT "x(25)"   
      col3                                  
      col7			       FORMAT "99.99.9999"
	  
      col6                 FORMAT "x(16)"  
      col4[1]                               
    
      WITH NO-LABELS NO-BOX WIDTH 240.

   acct-surr = acct.acct + "," + acct.currency.

   col1[1] = GetXAttrValueEx("acct",acct-surr,"ДогОткрЛС","").

   IF col1[1] EQ "" THEN

      FOR EACH loan-acct OF acct WHERE CAN-DO("Кредит,Депоз,dps,Расчет", loan-acct.contract)
                                   AND CAN-DO(mRuleAcct, loan-acct.acct-type)
                                   AND loan-acct.since LE end-date
         NO-LOCK,
         FIRST loan OF loan-acct NO-LOCK BY loan-acct.since DESCENDING:
         mDataSogl = date(GetXAttrValueEx("loan",
                                          loan.contract + "," + loan.cont-code,
                                          "датасогл",
                                          ?)).
         col1[1] = "Договор " + loan-acct.cont-code + " от " + STRING(IF mDataSogl = ? THEN loan.open-date ELSE mDataSogl ,"99.99.9999").
         LEAVE.
      END.
   ELSE
       ASSIGN
           col1[1] = "Договор " + ENTRY (2, col1[1]) + " от " + ENTRY (1, col1[1])
           NO-ERROR.


   col2[1] = GetXAttrValueEx("acct",acct-surr,"ПорВыдВыпис","").
   
   IF col2[1] NE "" THEN DO:
      FIND FIRST CODE WHERE code.class EQ "ПорВыдВыпис"
                        AND code.code  EQ col2[1] NO-LOCK NO-ERROR.
      col2[1] = IF AVAIL code THEN code.val ELSE "не требуется".
      RELEASE code.
   END.
   ELSE
      col2[1] = "не требуется".

   col3 = GetXAttrValue("acct",acct-surr,"ДатаСообщЛС").

   col6 = (IF   acct.close-date NE ? AND acct.close-date LE end-date
           THEN GetXAttrValueEx("acct",acct-surr,"ДатаСообщЗак","")
           ELSE "").
		   
   col7 = (IF acct.close-date NE ? AND acct.close-date LE end-date THEN STRING(acct.close-date, "99.99.9999") ELSE "          ").
   
   IF col3 = "" THEN
      col3 = "не требуется".
   col4[1] = GetXAttrValue("acct",acct-surr,"ПримЛС").
/* -0148908-- Если заполнен доп реквизит <ЦельСч>, то печатается конструкция
    <ЦельСч> + <;> + acct.details. Если нет, то печатается acct.contract. */
   IF GetXAttrValueEx("acct",acct-surr,"ЦельСч","") NE "" THEN
	  col5[1] = GetXAttrValueEx("acct",acct-surr,"ЦельСч","") + ";" + IF acct.details NE ? THEN acct.details ELSE "".
   ELSE DO:
	/*col5[1] = acct.contract.*/
	FIND FIRST contract 
	WHERE contract.contract = acct.contract
	NO-LOCK NO-ERROR.
		IF AVAIL contract THEN
			col5[1] = contract.name-contract.
   END.   
    
	
   /* разбиваем длинные наименования на несколько строк */
   /*
   {wordwrap.i &s=name &l=50 &n=10}
   {wordwrap.i &s=col1 &l=40 &n=10}
   {wordwrap.i &s=col2 &l=25 &n=10}
   {wordwrap.i &s=col4 &l=20 &n=10}
   {wordwrap.i &s=col5 &l=37 &n=10}
   */
   /*
   DISPLAY
      acct.open-date
      col1[1]
	  acct.acct
      col5[1]
      col2[1]
      col3
      acct.close-date WHEN acct.close-date NE ? AND acct.close-date LE end-date
      name[1]
	  col4[1]       NO-ERROR.
   */
   PUT STREAM ws UNFORMATTED    "|" + STRING(acct.open-date, "99.99.9999") + 
								"|" + STRING(col1[1], "x(40)")+ 
								"|" + STRING(SUBSTRING(acct.acct, 1, 20), "x(21)") +  
								"|" + STRING(name[1], "x(100)") +
								"|" + STRING(col5[1], "x(37)") +   
								"|" + STRING(col2[1], "x(25)") +    
								"|" + STRING(col3, "x(18)") +  
								"|" + col7 +
								"|" + STRING(col4[1], "x(17)") + 
								"|" + STRING(col6, "x(20)") + 
								"|" + eol.
   /* Длинные наименования переносим на несколько строк */
   /*
   DO i = 2 TO 10:
      IF    name[i] <> "" 
         OR col1[i] <> "" 
         OR col2[i] <> "" 
         OR col4[i] <> "" 
         OR col5[i] <> "" 
      THEN DO:

         DOWN.
         DISPLAY 
            col1[i] @ col1[1]
            col5[i] @ col5[1]
            col2[i] @ col2[1]
            col4[i] @ col4[1]
            name[i] @ name[1].

      END.
      ELSE
         LEAVE. 
   END.
   */
END.

PUT STREAM ws UNFORMATTED    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" + eol.

OUTPUT STREAM ws CLOSE.
RUN sndbispc ("file=" + fname + ";class=bq").

/*
{signatur.i}
PAGE.
*/