/* ayv
RetAdr.p - ��楤�� ���ᨭ�� ���ᮢ
�室�騥 ��ࠬ����:
	- vCust-id - id 
	- vCust-cat - � ��� �
	- vCode-type - ����ய, �������, �����
	- vDate - �� ࠭�� ����� ���� ����室��� �᪠�� ����
*/
{globals.i}

FUNCTION TitleCase RETURNS CHARACTER
  (pcText AS CHARACTER) :

  DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWord       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iLast       AS INTEGER     NO-UNDO.

  DEFINE VARIABLE cSmallWords AS CHARACTER   NO-UNDO
     INITIAL "��,�������,�ࠩ,��⮭�����,�".

  pcText = REPLACE(LC(pcText),"-"," - ").
  iLast = NUM-ENTRIES(pcText, " ").
  DO i = 1 TO iLast:
    cWord = ENTRY(i, pcText, " ").
    IF LENGTH(cWord) > 0 THEN
      IF LOOKUP(cWord, "��") = 0 THEN
	    IF LOOKUP(cWord, cSmallWords) = 0 THEN
			ENTRY(i, pcText, " ") = CAPS(SUBSTRING(cWord, 1, 1)) + LC(SUBSTRING(cWord, 2)).
		ELSE ENTRY(i, pcText, " ") = LC(cWord).
	/*pda ��ࠢ���� �訡�� � �������� ॣ���� "NameCity �"*/
	IF i = iLast AND ENTRY(i, pcText, " ") = "�" THEN DO:
  	  pcText = REPLACE(pcText, " �", "").
  	  pcText = "�. " + pcText.
  	END.
  END.
  IF ENTRY(1, pcText, " ") = "�" THEN
	pcText = "�." + SUBSTRING(pcText,2).
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
DEF VAR VCodeReg  AS CHAR  NO-UNDO EXTENT 3. /* ������, ��� ���祭�� � ��த-業��*/
DEF VAR vOldAdr   AS LOG   NO-UNDO.
DEF VAR vPrefAdr  AS CHAR  NO-UNDO.
DEF VAR vSepAdr   AS CHAR  NO-UNDO.
DEF VAR vFlDot    AS LOG   NO-UNDO INIT FALSE. /* �ᯮ�짮������ �� �窠 � ���� */
DEF VAR vCityC	  AS LOG   NO-UNDO INIT FALSE. /* ������� �� ��த 業�஬ ॣ���� */
DEF VAR vCityF    AS LOG   NO-UNDO INIT FALSE. /* ������� �� ��த�� 䥤�ࠫ쭮�� ���祭��*/
DEF VAR vCnt      AS INT64 NO-UNDO.
DEF VAR vTmp      AS CHAR  NO-UNDO.
DEF VAR vLeft	  AS LOG   NO-UNDO. /* � ����� ��஭� �⠢��� ᮪�饭��*/

DEF VAR np AS CHAR INITIAL "��,��,���,�,�,���,��,�,��,�,�,�,��,��,��-�,�,���஢,�,���,��-��,�".
DEF VAR nd AS CHAR INITIAL "���,��,��-�,���஢,��-��".


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
								  "������","").
								  
	FIND FIRST code WHERE code.class EQ "������"
					AND   code.code  EQ vCodeReg[1]
		NO-LOCK NO-ERROR.
		
	IF AVAIL code THEN DO:
		oCity = code.name.
		vCodeReg[2] = TitleCase(code.val).  /* ��த  */
		vCodeReg[3] = TitleCase(code.name). /* ������ */
	END.
		/* MESSAGE oReg VIEW-AS ALERT-BOX. */
	
	vOldAdr = FGetSetting("������․�","",?) NE "����".
	iAdr = cust-ident.issue.
	
	IF NOT vOldAdr THEN
	DO:
		
		IF TRIM(ENTRY(3,iAdr,",")) NE "" THEN DO:
			IF SUBSTRING(ENTRY(3,iAdr,","),LENGTH(ENTRY(3,iAdr,",")) - 1) EQ " �" THEN DO:
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
					vPrefAdr = "�. ".
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
				vPrefAdr = "�. ".
			 WHEN 7 THEN
				vPrefAdr = "���. ".
			 WHEN 8 THEN
				IF vCust-cat = "�" 
				THEN 
					vPrefAdr = "��� ".
				ELSE 
					vPrefAdr = "��. ".
			 WHEN 9 THEN
				vPrefAdr = "���. ".
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
						IF SUBSTRING(vTmp,LENGTH(vTmp) - 1) EQ " �" THEN
							oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp) - 2).
						/*pda ��ࠢ���� ��������� ᮪�饭�� "�. �"*/
						ELSE IF vTmp BEGINS "� "	
							THEN oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,3).
						ELSE
							oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp)). 
						
				WHEN 4 OR
				WHEN 5 THEN
					IF vLeft THEN
						oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,1,LENGTH(vTmp) - LENGTH(vPrefAdr) + 1).
					ELSE
						oAdr = oAdr + vSepAdr + vTmp.
				/*pda ��ࠢ���� ��������� ᮪�饭�� "�. �"  � "���. �"*/
				WHEN 6 THEN DO:
				    IF vTmp BEGINS "� "
				    	THEN oAdr = oAdr + vSepAdr + vPrefAdr + SUBSTRING(vTmp,3).
				    ELSE
				    	oAdr = oAdr + vSepAdr + vPrefAdr + vTmp.
				    END.
				WHEN 7 THEN DO:
				    IF vTmp BEGINS "� "
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