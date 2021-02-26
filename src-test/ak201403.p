{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{sh-defs.i}
{ksh-defs.i NEW}
{intrface.get comm}     /* Инструменты для работы с комиссиями. */
{intrface.get loan}     /* Инструменты для работы с кредитами.  */
{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */
{intrface.get acct}     /* Библиотека для работы со счетами.    */
{intrface.get tmess}    /* Инструменты обработки сообщений.     */
{intrface.get date}
{intrface.get db2l}
{intrface.get cust}
{intrface.get i254}
{intrface.get crole}
{intrface.get tmcod}
{intrface.get refer} 

DEF INPUT PARAM cont-code AS CHAR NO-UNDO.

&GLOB Months "января,февраля,марта,апреля,мая,июня,июля,августа,сентября,~
октября,ноября,декабря"

DEF VAR vTmpStr AS CHAR NO-UNDO.
DEF VAR vCopStr AS CHAR NO-UNDO.
DEF VAR vDecStr AS DEC  NO-UNDO.
DEF VAR vDatStr AS DATE NO-UNDO.

FIND loan WHERE
	 loan.cont-code = cont-code AND
	 loan.contract = 'dps'
NO-LOCK NO-ERROR.
	
	IF AVAIL loan THEN DO:
		RUN Insert_TTName ("vklnum",loan.doc-ref).
		RUN Insert_TTName ("vklday",STRING(DAY(loan.open-date))).
		vTmpStr = ENTRY(MONTH(loan.open-date),{&Months}) + " " + STRING(YEAR(loan.open-date)).
		RUN Insert_TTName ("vkldate",vTmpStr).

		FIND person WHERE
			 person.person-id = loan.cust-id
		NO-LOCK NO-ERROR.
		
			IF AVAIL person THEN DO:
				RUN Insert_TTName ("perfio",person.name-last + " " + person.first-names).
				IF person.document-id = "Паспорт" THEN DO:
					RUN Insert_TTName ("perser",ENTRY(1,person.document," ") + " " + ENTRY(2,person.document," ")).
					RUN Insert_TTName ("pernum",ENTRY(3,person.document," ")).
				END.
				ELSE DO:
					RUN Insert_TTName ("perser","НЕРЕЗ").
					RUN Insert_TTName ("pernum",person.document).
				END.
				vTmpStr = GetXAttrValueEx("person",STRING(person.person-id),"Document4Date_vid",?).
				vDatStr = DATE(vTmpStr).
				vTmpStr = STRING(DAY(vDatStr)) + " " + ENTRY(MONTH(vDatStr),{&Months}) + " " + STRING(YEAR(vDatStr)) + " г.". 
				RUN Insert_TTName ("perdate",vTmpStr).
				RUN Insert_TTName ("perkem",person.issue).
			END.
			
		RUN RetAdr.p(loan.cust-id,  "Ч", "АдрПроп", ?, OUTPUT vTmpStr).
		RUN Insert_TTName ("peradr",vTmpStr).		
		
		FIND loan-acct WHERE
			 loan-acct.cont-code = cont-code AND
			 loan-acct.acct-type = 'loan-dps-t'
		NO-LOCK NO-ERROR.

			IF AVAIL loan-acct THEN DO:
				RUN Insert_TTName ("vklacct",STRING(delFilFromAcct(loan-acct.acct))).
				RUN acct-pos IN h_base (loan-acct.acct, loan-acct.currency, TODAY, TODAY, ?).
				RUN Insert_TTName ("vklpos",STRING(abs(sh-bal),">>>,>>>,>>>,>>>,>>9.99")).
				RUN x-amtstr.p (abs(sh-bal),'',YES,YES,OUTPUT vTmpStr,OUTPUT vCopStr).
				RUN Insert_TTName ("vklpospr",vTmpStr + " " + vCopStr).
			END.

		FIND _User WHERE 
			 _User._Userid EQ USERID("bisquit")  
		NO-LOCK NO-ERROR.

			IF AVAIL _User THEN 
				RUN Insert_TTName ("sotrfio",STRING(_user._User-Name)).

		RUN printvd.p ("ak201403",INPUT TABLE ttnames).

	END.


		
PROCEDURE RetAdr:
DEF INPUT  PARAM iAdr AS CHAR NO-UNDO.
DEF INPUT  PARAM cusc AS CHAR NO-UNDO.
DEF OUTPUT PARAM oAdr AS CHAR NO-UNDO.

DEF VAR vOldAdr   AS LOG   NO-UNDO.
DEF VAR vPrefAdr  AS CHAR  NO-UNDO.
DEF VAR vSepAdr   AS CHAR  NO-UNDO.
DEF VAR vFlDot    AS LOG   NO-UNDO INIT FALSE. /* Использовалась ли точка в адресе */
DEF VAR vCnt      AS INT64 NO-UNDO.
DEF VAR vTmp      AS CHAR  NO-UNDO.
DEF VAR vLeft	  AS LOG   NO-UNDO.

DEF VAR np AS CHAR INITIAL "дп,кп,пгт,рп,сп,аал,аул,д,нп,п,с,сл,снт,ст,ст-ца,х,остров,ул,пер,пр-кт".
DEF VAR nd AS CHAR INITIAL "алл,аул,ст-ца,остров,пр-кт".

vOldAdr = FGetSetting("ВарПечатАдр","",?) NE "Новый".
IF NOT vOldAdr THEN
DO:
   DO vCnt = 1 TO NUM-ENTRIES(iAdr):
      CASE vCnt:
		 WHEN 3 THEN DO:
			vTmp = ENTRY(vCnt,iAdr,",").
			vPrefAdr = "г.".
			vLeft = TRUE.
		 END.
		 WHEN 4 OR WHEN 5 THEN DO:
			vTmp = ENTRY(vCnt,iAdr,",").
			IF vTmp NE "" THEN DO:
				vPrefAdr = ENTRY(NUM-ENTRIES(vTmp," "),vTmp," ").
				IF LOOKUP(vPrefAdr,np) = 0
				THEN
					vLeft = FALSE.				
				ELSE 
					vLeft = TRUE.
				IF LOOKUP(vPrefAdr,nd) = 0 
				THEN
					vPrefAdr = vPrefAdr + ".".
				ELSE
					vPrefAdr = vPrefAdr + " ".
			END.
		 END.
		 WHEN 6 THEN
            vPrefAdr = "д.".
/* sku поменял местами строение и квартиру */
         WHEN 9 THEN
            vPrefAdr = "стр.".
         WHEN 7 THEN
            vPrefAdr = "корп.".
         WHEN 8 THEN
             IF cusc = "Ю" THEN vPrefAdr = "офис.".
			 ELSE vPrefAdr = "кв.".
         OTHERWISE
            vPrefAdr = "".
      END CASE.
      /* sku убираем лишний префикс */
      IF LENGTH(vPrefAdr)>0 AND ENTRY(vCnt,iAdr,",") BEGINS vPrefAdr
        THEN vPrefAdr = "".
      IF     NOT vFlDot
         AND vCnt GE 6 THEN
         ASSIGN
            vSepAdr = " "
            vFlDot  = TRUE.
      ELSE
         vSepAdr = ", ".
      IF ENTRY(vCnt,iAdr,",") NE "" THEN
      CASE vCnt:
         WHEN 1 THEN
            oAdr = ENTRY(vCnt,iAdr,",").
		 WHEN 4 OR
		 WHEN 3 OR
		 WHEN 5 THEN
			IF vLeft THEN
				oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp) - LENGTH(vPrefAdr),"CHARACTER").
			ELSE
				oAdr = oAdr + vSepAdr + ENTRY(vCnt,iAdr,",").
         OTHERWISE
            oAdr = oAdr + vSepAdr + vPrefAdr + ENTRY(vCnt,iAdr,",").
      END CASE.
   END.
END.
else
   oAdr = iAdr.
END PROCEDURE.