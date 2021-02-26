/* ayv
RetAdr.p - процедура парсинга адресов
Входящие параметры:
	- vCust-id - id 
	- vCust-cat - Ч или Ю
	- vCode-type - АдрПроп, АдрФакт, АдрЮр
	- vDate - не раньше какой даты необходимо искать адрес
*/
{globals.i}

FUNCTION TitleCase RETURNS CHARACTER
  (pcText AS CHARACTER) :

  DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWord       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iLast       AS INTEGER     NO-UNDO.

  DEFINE VARIABLE cSmallWords AS CHARACTER   NO-UNDO
     INITIAL "на,область,край,автономная,г".

  pcText = REPLACE(LC(pcText),"-"," - ").
  iLast = NUM-ENTRIES(pcText, " ").
  DO i = 1 TO iLast:
    cWord = ENTRY(i, pcText, " ").
    IF LENGTH(cWord) > 0 THEN
      IF LOOKUP(cWord, "АО") = 0 THEN
	    IF LOOKUP(cWord, cSmallWords) = 0 THEN
			ENTRY(i, pcText, " ") = CAPS(SUBSTRING(cWord, 1, 1)) + LC(SUBSTRING(cWord, 2)).
		ELSE ENTRY(i, pcText, " ") = LC(cWord).
	/*pda исправлена ошибка в названии региона "NameCity г"*/
	IF i = iLast AND ENTRY(i, pcText, " ") = "г" THEN DO:
  	  pcText = REPLACE(pcText, " г", "").
  	  pcText = "г. " + pcText.
  	END.
  END.
  IF ENTRY(1, pcText, " ") = "г" THEN
	pcText = "г." + SUBSTRING(pcText,2).
  RETURN REPLACE(pcText," - ","-").   

END FUNCTION.

DEF INPUT  PARAM vCust-id   AS INT64 NO-UNDO.
DEF INPUT  PARAM vCust-cat  AS CHAR  NO-UNDO.
DEF INPUT  PARAM vCode-type AS CHAR  NO-UNDO.
DEF INPUT  PARAM vDate      AS DATE  NO-UNDO.
DEF OUTPUT PARAM oAdr       AS CHAR  NO-UNDO.
DEF OUTPUT PARAM oCity      AS CHAR  NO-UNDO.

/* DEF VAR oReg      AS CHAR  NO-UNDO. */
DEF VAR iAdr      AS CHAR  NO-UNDO.
DEF VAR VCodeReg  AS CHAR  NO-UNDO EXTENT 3. /* КодРег, его значение и город-центр*/
DEF VAR vOldAdr   AS LOG   NO-UNDO.
DEF VAR vPrefAdr  AS CHAR  NO-UNDO.
DEF VAR vSepAdr   AS CHAR  NO-UNDO.
DEF VAR vFlDot    AS LOG   NO-UNDO INIT FALSE. /* Использовалась ли точка в адресе */
DEF VAR vCityC	  AS LOG   NO-UNDO INIT FALSE. /* Является ли город центром региона */
DEF VAR vCityF    AS LOG   NO-UNDO INIT FALSE. /* Является ли городом федерального значения*/
DEF VAR vCnt      AS INT64 NO-UNDO.
DEF VAR vTmp      AS CHAR  NO-UNDO.
DEF VAR vLeft	  AS LOG   NO-UNDO. /* С какой стороны ставить сокращение*/

DEF VAR np AS CHAR INITIAL "дп,кп,пгт,рп,сп,аал,аул,д,нп,п,с,сл,снт,ст,ст-ца,х,остров,ул,пер,пр-кт,ш".
DEF VAR nd AS CHAR INITIAL "алл,аул,ст-ца,остров,пр-кт".


IF vDate EQ ? THEN 
	vDate = TODAY.

FIND LAST cust-ident WHERE cust-ident.class-code     EQ "p-cust-adr"
					 AND   cust-ident.cust-code-type EQ vCode-type
					 AND   cust-ident.cust-id        EQ vCust-id
					 AND   cust-ident.cust-cat       EQ vCust-cat
					 AND   cust-ident.open-date      LE vDate
	NO-LOCK NO-ERROR.
	
IF AVAILABLE cust-ident THEN DO:  

	vCodeReg[1] = GetXAttrValueEx("cust-ident",
								  cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num),
								  "КодРег","").
								  
	FIND FIRST code WHERE code.class EQ "КодРег"
					AND   code.code  EQ vCodeReg[1]
		NO-LOCK NO-ERROR.
		
	IF AVAIL code THEN DO:
		oCity = code.name.
		vCodeReg[2] = TitleCase(code.val).  /* Город  */
		vCodeReg[3] = TitleCase(code.name). /* Регион */
	END.
		/* MESSAGE oReg VIEW-AS ALERT-BOX. */
	
	vOldAdr = FGetSetting("ВарПечатАдр","",?) NE "Новый".
	iAdr = cust-ident.issue.
	
	IF NOT vOldAdr THEN
	DO:
		
		IF TRIM(ENTRY(3,iAdr,",")) NE "" THEN DO:
			IF SUBSTRING(ENTRY(3,iAdr,","),LENGTH(ENTRY(3,iAdr,",")) - 1) EQ " г" THEN DO:
				IF TRIM(LC(SUBSTRING(ENTRY(3,iAdr,","),1,LENGTH(ENTRY(3,iAdr,",")) - 2))) EQ TRIM(LC(SUBSTRING(vCodeReg[2],3))) THEN 
					vCityC = TRUE.
				IF TRIM(LC(SUBSTRING(ENTRY(3,iAdr,","),1,LENGTH(ENTRY(3,iAdr,",")) - 2))) EQ TRIM(LC(SUBSTRING(vCodeReg[3],3))) THEN 
					vCityF = TRUE.
			END.
			ELSE DO:
				IF TRIM(LC(ENTRY(3,iAdr,","))) EQ TRIM(LC(SUBSTRING(vCodeReg[2],3))) THEN 
					vCityC = TRUE.
				IF TRIM(LC(ENTRY(3,iAdr,","))) EQ TRIM(LC(SUBSTRING(vCodeReg[3],3))) THEN 
					vCityF = TRUE.
			END.
		END.
		ELSE
			vCityF = TRUE.
		
	   DO vCnt = 1 TO NUM-ENTRIES(iAdr):
	   
		  vTmp = TRIM(ENTRY(vCnt,iAdr,",")).
		  
		  CASE vCnt:
			 WHEN 3 THEN DO:
				IF vTmp NE "" THEN
				DO:
					vPrefAdr = "г. ".
					vLeft = TRUE.
				END.
			 END.
			 WHEN 4 OR WHEN 5 THEN DO:
				IF vTmp NE "" THEN DO:
				vPrefAdr = ENTRY(NUM-ENTRIES(vTmp," "),vTmp," ").
				IF LOOKUP(vPrefAdr,np) = 0
					THEN
						vLeft = FALSE.				
					ELSE 
						vLeft = TRUE.
				IF LOOKUP(vPrefAdr,nd) = 0 
					THEN 
						vPrefAdr = vPrefAdr + ". ".
					ELSE
						vPrefAdr = vPrefAdr + " ".
				END.
			 END.
			 WHEN 6 THEN
				vPrefAdr = "д. ".
			 WHEN 7 THEN
				vPrefAdr = "корп. ".
			 WHEN 8 THEN
				IF vCust-cat = "Ю" 
				THEN 
					vPrefAdr = "офис ".
				ELSE 
					vPrefAdr = "кв. ".
			 WHEN 9 THEN
				vPrefAdr = "стр. ".
			 OTHERWISE
				vPrefAdr = "".
		  END CASE.
		  
		  IF LENGTH(vPrefAdr)>0 AND ENTRY(vCnt,iAdr,",") BEGINS vPrefAdr 
			THEN vPrefAdr = "".
			
		  IF NOT vFlDot AND vCnt GE 6 THEN
			 ASSIGN
				vSepAdr = " "
				vFlDot  = TRUE.
		  ELSE
			 vSepAdr = ", ".
			 
		  IF vTmp NE "" THEN
			CASE vCnt:
				WHEN 1 THEN DO:
					oAdr = vTmp.
					IF vCityF OR NOT vCityC THEN
						oAdr = oAdr + vSepAdr + vCodeReg[3].
				END.
				WHEN 3 THEN
					IF NOT vCityF THEN
						IF SUBSTRING(vTmp,LENGTH(vTmp) - 1) EQ " г" THEN
							oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp) - 2).
						/*pda исправлено задвоение сокращения "г. г"*/
						ELSE IF vTmp BEGINS "г "	
							THEN oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,3).
						ELSE
							oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp)). 
						
				WHEN 4 OR
				WHEN 5 THEN
					IF vLeft THEN
						oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp) - LENGTH(vPrefAdr) + 1).
					ELSE
						oAdr = oAdr + vSepAdr + vTmp.
				/*pda исправлено задвоение сокращения "д. д"  и "корп. к"*/
				WHEN 6 THEN DO:
				    IF vTmp BEGINS "д "
				    	THEN oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,3).
				    ELSE
				    	oAdr = oAdr + vSepAdr + vPrefAdr + vTmp.
				    END.
				WHEN 7 THEN DO:
				    IF vTmp BEGINS "к "
				    	THEN oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,3).
				    ELSE
				    	oAdr = oAdr + vSepAdr + vPrefAdr + vTmp.
				    END.
				OTHERWISE
					oAdr = oAdr + vSepAdr + vPrefAdr + vTmp.
			END CASE.
			
		END.
		
	END.
	ELSE
	   oAdr = TRIM(iAdr).
	   
END.
ELSE 
	oAdr = "".