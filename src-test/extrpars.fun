/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: EXTRPARS.FUN
      Comment: �������⥫�� �㭪樨 �����, ����ᠭ�� � ������
               ���� �������㠫�� ��� ������ ���ᨨ �����
               
               ��� �ணࠬ���⮢ ���: ���� ������ ������ !!!!!!!!!!!!!!!
               
               ��� �ணࠬ���� �����: �� �㭪樨, ���������� � ����� �����
               ⮫쪮 � (� �� � g-pfunc.def parsacct.def details.fun).
               �᫨ �㭪�� �㤥� ���, � ��� �������� �㤥� ��७�ᥭ� ���
               �ᯮ�짮����� � ��㣨� �����
               
               ��� �����!!!!!
               ��஬��� ���졠:
               1. ��ࠩ��� ���� ����� �㭪樨 ������ �᫨ ��� ��� ᮢᥬ ��祣� ����������
               2. ��ࠩ��� 㬥����� ��� �� �����㬠
               3. ����⥫쭮 ����� �ᯮ�짮���� �㭪樨 � ���� ���譨� ��楤�� � �㭪権 � ������� ������⥪��!
               
   Parameters: &PARSER-DETAILS-P  - ����砥� �� �㭪�� �㤥� �ᯮ�짮������ ⮫쪮
                                    � ����� �� ���� "ᮤ�ঠ���"
               &PARSER-PARSACCT-P - ����砥� �� �㭪�� �㤥� �ᯮ�짮������ ⮫쪮
                                    � ����� �� ��⠬
               &PARSER-PARSSEN-P  - ����砥� �� �㭪�� �㤥� �ᯮ�짮������ ⮫쪮
                                    � ����� �� ���� �㬬
                                
               ��� �ᯮ�짮���� ? ����:
               &IF DEFINED (PARSER-DETAILS-P) &THEN
                  /* �� �㭪�� �㤥� �ᯮ�짮������ � ����� �� ���� "ᮤ�ঠ���" */
               &ENDIF
               
               �᫨ ��� ������� 㪠��⥫�� - �㭪�� �㤥� ������ �����
               
               �� ���뢠�� ����� ������ਨ, �� �㭪�� ������!!!!!!!!!!!!!!!!!!!!!!!!!
         Uses:
      Used by:
      Created: 25.06.2002 15:11 SEMA    ��� �⤥����� �㭪権 ����� �� �㭪権 ���
     Modified: 25.06.2002 15:54 SEMA     �� ��� 0003868 ᮧ����� 䠩��
     Modified: 16.10.2014 KAU ����ୠ� �㭪�� ��� �맮�� ��楤��
     Modified: 



   �ਬ�� �㭪樨 :

/*
  �� ������: ���ᠭ�� �㭪樨
  ���⠪�� : �㭪��ਬ��1 ( ��ࠬ���1, ��ࠬ���2 )
  ����     :
*/

PROCEDURE �㭪��ਬ��1:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
    IF NOT Pars-ValidParam(2) THEN RETURN.
/* --- */
    DEFINE VARIABLE vFuncParam1 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vFuncParam2 AS INT64    NO-UNDO.
    DEFINE VARIABLE vResult     AS CHARACTER  NO-UNDO.

    ASSIGN
        vFuncParam1 = Pars-GetString ( 0 )
        vFuncParam2 = Pars-GetInt ( 1 )
        .

    RUN ��楤�ࠎ�ࠡ��뢠���⮒� (vFuncParam1, vFuncParam2, OUTPUT vResult).

    RUN Pars-SetCHARResult ( vResult ).

/* --- */
    is-ok = TRUE.
END PROCEDURE.


&IF DEFINED (PARSER-DETAILS-P) &THEN
    /*
      �� ������: ���ᠭ�� �㭪樨
      ���⠪�� : �㭪��ਬ��2 ( ��ࠬ���1 [, ��ࠬ���2 ] )
      ����     :
    */
    
    PROCEDURE �㭪��ਬ��2:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    /* --- */
        IF NOT (Pars-ValidParam(1) OR Pars-ValidParam(2)) THEN RETURN.
    /* --- */
        DEFINE VARIABLE vFuncParam1 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE vFuncParam2 AS DECIMAL    NO-UNDO.
        DEFINE VARIABLE vResult     AS DECIMAL    NO-UNDO.
    
        ASSIGN
            vFuncParam1 = Pars-GetString ( 0 )
            vFuncParam2 = (IF pn > 0 THEN Pars-GetDec ( 1 ) ELSE 0)
            .
    
        RUN ��楤�ࠎ�ࠡ��뢠���饗⮒� (vFuncParam1, vFuncParam2, OUTPUT vResult).
    
        RUN Pars-SetResult ( vResult ).
    
    /* --- */
        is-ok = TRUE.
    END PROCEDURE.
&ENDIF

*/

/* ************************************ ����� ��� ****************************************** */

PROCEDURE �᫨���:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
    IF NOT Pars-ValidParam(2) THEN RETURN.
/* --- */
    DEFINE VARIABLE vMaskParam AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vStrParam  AS CHARACTER  NO-UNDO.

    ASSIGN
        vMaskParam = Pars-GetString ( 0 )
        vStrParam = Pars-GetString ( 1 )
        .
    RUN Pars-SetResult ( IF CAN-DO( vMaskParam, vStrParam) THEN 1 ELSE 0 ).

/* --- */
    is-ok = TRUE.
END PROCEDURE.

/*
&IF DEFINED (PARSER-PARSACCT-P) &THEN
*/
/*--------------------------------------------------------------------------
    kam
    * �� ������: �ந������ ४����� � ������ �� ��� ����
                                                       
    * �ਬ��    : �������⊫����2('��ꥪ�','40817544664646446     @0000')
  --------------------------------------------------------------------------*/
PROCEDURE �������⊫����2:


   DEF OUTPUT PARAM  is-ok AS LOGICAL   NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEF VAR vAttr        AS CHAR   NO-UNDO.   
   DEF VAR vAcct        AS CHAR   NO-UNDO.    
   DEF VAR vResult      AS CHAR   NO-UNDO.  
   DEF VAR vTable       AS CHAR   NO-UNDO. 
 

   /* ������ ���� 2 ��ࠬ��� */
   is-ok = FALSE.
   IF Pars-ValidParam(2) THEN DO:

      vAttr = Pars-GetString(0).
      vAcct = Pars-GetString(1).
      IF length(vAcct) > 20 THEN vAcct = SUBSTRING(vAcct,1,20).

      FOR FIRST acct WHERE
          acct.number EQ vAcct
          AND acct.filial-id = shFilial
      NO-LOCK:
          vTable = if (acct.cust-cat EQ "�") then "person" else
              if (acct.cust-cat EQ "�") then "cust-corp" else
              if (acct.cust-cat EQ "�") then  "banks" else "person".
   
          vResult = GetXAttrValueEx(vTable,STRING(acct.cust-id),vAttr,"").   
   END.
     RUN Pars-SetCharResult (vResult).
     is-ok = yes.
  END.

END PROCEDURE.

/*
&ENDIF
*/
&IF DEFINED (PARSER-DETAILS-P) &THEN

/* 
���� ���� ������ ��᫥����� ����⮣� ��� ������ 90909 
*/
PROCEDURE date90909:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� */
   DEFINE VARIABLE in-date	AS DATE NO-UNDO.
   DEFINE VARIABLE h_templ      AS HANDLE NO-UNDO. 
   DEFINE VARIABLE date9	AS DATE NO-UNDO.
   IF NOT (Pars-ValidParam(1)) THEN RETURN.

   in-date = DATE(Pars-GetString(0)).   
   is-ok = YES.
   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
      IF NOT VALID-HANDLE(h_templ) THEN RETURN.
      FIND FIRST loan WHERE loan.contract  EQ "dps"
                        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
         NO-LOCK NO-ERROR.      
      IF NOT AVAIL loan THEN RETURN.

   FOR EACH acct WHERE  acct.close-date = ? 	      AND
			acct.cust-cat = loan.cust-cat AND 
		       	acct.cust-id  = loan.cust-id  AND
			acct.bal-acct = 90909	      AND
		        acct.details BEGINS loan.doc-ref AND
		       	acct.open-date <= in-date  NO-LOCK BY acct.open-date DESC:
	date9 = acct.open-date.
	LEAVE.
   END.
   RUN Pars-SetCHARResult(STRING(date9)).

END PROCEDURE.

&ENDIF

/*
�����頥� ४����� ���㬥�� �� ������ 蠡���� (ࠡ�⠥� � op_flt)
*/
PROCEDURE �������:
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� */
   DEFINE VARIABLE vNom    AS INT NO-UNDO.
   DEFINE VARIABLE vAttr   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vResult AS CHARACTER NO-UNDO.
   DEFINE BUFFER xop FOR op.

   is-ok = FALSE.
   IF Pars-ValidParam(2) THEN DO:

      vNom = Pars-GetInt(0).
      vAttr = Pars-GetString(1).
      FIND FIRST xwop WHERE xwop.op-templ EQ vNom NO-LOCK NO-ERROR.
      IF AVAIL xwop THEN DO:
         FIND FIRST xop WHERE RECID(xop) = xwop.op-recid NO-LOCK NO-ERROR.
         IF AVAIL xop THEN DO:
            vResult = BUFFER xop:BUFFER-FIELD(vAttr):BUFFER-VALUE NO-ERROR.
            RUN Pars-SetCharResult (IF vResult = ? THEN "" ELSE vResult).
            is-ok = YES.
         END.
      END.

   END.
   
END PROCEDURE.

&IF DEFINED (PARSER-DETAILS-P) &THEN
    
    PROCEDURE ��������:
    
    DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE vDate     AS DATE NO-UNDO.
    DEFINE VARIABLE vNumQuart AS INT NO-UNDO.
    DEFINE VARIABLE vNumMonth AS INT NO-UNDO.
    
    is-ok = FALSE.

    IF pn <> 1 THEN
    DO:
       MESSAGE "���ࠢ��쭮� ������⢮ ��ࠬ��஢" 
          ENTRY(1, PROGRAM-NAME(1), " ") "!" VIEW-AS ALERT-BOX ERROR.
       RETURN "�訡��".
    END.

   IF NOT Pars-ValidParam(1) THEN 
      RETURN.

   ASSIGN
      vDate = DATE(Pars-GetString(0)).
   
   vNumMonth = MONTH(vDate).
   IF vNumMonth MODULO 3 NE 0 THEN
      vNumQuart = TRUNCATE(vNumMonth / 3, 0) + 1.
   ELSE
      vNumQuart = TRUNCATE(vNumMonth / 3, 0).
   
   RUN Pars-SetResult(vNumQuart).
   is-ok = TRUE.
   
   END PROCEDURE.
&ENDIF

&IF DEFINED (PARSER-DETAILS-P) &THEN
/*
    * �� ������:
    * ���⠪�� : ��⠍�焑(�����ᮢ�_���[,�����_���])
    * ����     :
    * �ਬ��    : ��⠍�焑(��(1)[,���(1)]) ��� ��⠍�焑(��(1)[,���(1)]) 
*/
   PROCEDURE ��⠍�焑:

   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO.

   DEFINE VARIABLE vDate     AS DATE            NO-UNDO.
   DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
   DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.
    
   DEFINE BUFFER bacct FOR acct.

   is-ok = FALSE.

   IF pn NE 2 THEN
   DO:
      MESSAGE "���ࠢ��쭮� ������⢮ ��ࠬ��஢" 
         ENTRY(1, PROGRAM-NAME(1), " ") "!" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.

   IF NOT (Pars-ValidParam(1)
       OR  Pars-ValidParam(2)) THEN RETURN.
    
   ASSIGN
       vAcct     = Pars-GetString(0)
       vCurrency = Pars-GetString(1)
   .
     
   IF NOT {assigned vAcct} OR
      NOT {assigned vCurrency} THEN
      RETURN.
   
   IF vCurrency EQ "810" THEN ASSIGN vCurrency = "".
    
   {find-act.i 
         &bact = bacct
         &acct = vAcct
         &curr = vCurrency
   }

   IF NOT AVAILABLE bacct THEN DO:
      MESSAGE "���" """" + vAcct +  """" "�� ������!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.
   ELSE DO:
      FIND FIRST signs WHERE signs.file-name EQ "acct"
                         AND signs.surrogate EQ  bacct.acct + "," + bacct.currency
                         AND signs .code     EQ "��_���_�"
                         NO-LOCK NO-ERROR.
      IF AVAIL signs THEN DO:
         vDate = DATE(signs.xattr-value).
         RUN Pars-SetCHARResult(STRING(vDate)).
         is-ok = TRUE.
      END.
   END.
   END PROCEDURE.
&ENDIF

   PROCEDURE MonthDays:
  
      DEFINE INPUT  PARAMETER iBegDate   AS DATE.
      DEFINE INPUT  PARAMETER iEndDate   AS DATE.
      DEFINE INPUT  PARAMETER iMonthNum  AS INT.
      DEFINE OUTPUT PARAMETER oMonthDays AS INT.
      
      DEFINE VARIABLE vDate AS DATE.
      
      REPEAT vDate = iBegDate TO iEndDate:
         IF INT(SUBSTRING(STRING(vDate),4,2)) EQ iMonthNum THEN
            oMonthDays = oMonthDays + 1.
      END.
   
   END PROCEDURE.
   
   PROCEDURE YearDays:
     
      DEFINE INPUT  PARAMETER vDate     AS DATE NO-UNDO.
      DEFINE OUTPUT PARAMETER vYearDays AS INT  NO-UNDO.
      
      DEFINE VARIABLE vYear AS INT NO-UNDO.

      vYear = YEAR(vDate).

      IF vYear MODULO 4 NE 0 THEN 
         vYearDays = 365. 
      ELSE 
         vYearDays = 366. 
   
   END PROCEDURE.
   
   PROCEDURE NumQuart:
   
      DEFINE INPUT  PARAMETER vDate     AS DATE NO-UNDO.
      DEFINE OUTPUT PARAMETER vNumQuart AS INT  NO-UNDO.

      DEFINE VARIABLE vNumMonth AS INT NO-UNDO.

      vNumMonth = MONTH(vDate).
   
      IF vNumMonth MODULO 3 NE 0 THEN
         vNumQuart = TRUNCATE(vNumMonth / 3, 0) + 1.
      ELSE
         vNumQuart = TRUNCATE(vNumMonth / 3, 0).
   
   END PROCEDURE.
   
   PROCEDURE MinSumm:
      
      DEFINE INPUT  PARAMETER iAcct LIKE acct.acct NO-UNDO.
      
      DEFINE INPUT  PARAMETER iBegDate AS DATE NO-UNDO.
      DEFINE INPUT  PARAMETER iEndDate AS DATE NO-UNDO.
      DEFINE OUTPUT PARAMETER oMinSumm AS INT  NO-UNDO INIT 999999999.
      
      DEFINE VARIABLE vDate AS DATE NO-UNDO.
      
      iEndDate = iEndDate + 1.
   
      FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct NO-LOCK NO-ERROR.
      IF AVAIL acct-pos THEN
      DO:
         oMinSumm = 999999999.
         REPEAT vDate = iBegDate TO iEndDate:
      
            FIND FIRST acct-pos WHERE acct-pos.acct  EQ iAcct
                                  AND acct-pos.since EQ vDate NO-LOCK NO-ERROR.
            IF AVAIL acct-pos THEN 
            DO:
               IF ABS(acct-pos.balance) < oMinSumm THEN 
                  oMinSumm = ABS(acct-pos.balance).
            END.
            ELSE DO:
               FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct
                                    AND acct-pos.since < vDate NO-LOCK NO-ERROR.
               IF AVAIL acct-pos THEN 
               DO:
                  IF ABS(acct-pos.balance) < oMinSumm THEN 
                     oMinSumm = ABS(acct-pos.balance).
               END.
            END.
         END. 
      END.
      ELSE
         oMinSumm = 0.
      
   END PROCEDURE.

   PROCEDURE TotalMinSummPerc:
      
      DEFINE INPUT PARAMETER iAcct LIKE acct.acct NO-UNDO.
      
      DEFINE INPUT  PARAMETER iPerc          AS INT  NO-UNDO.
      DEFINE INPUT  PARAMETER iBegDate       AS DATE NO-UNDO.
      DEFINE INPUT  PARAMETER iEndDate       AS DATE NO-UNDO.
      DEFINE INPUT  PARAMETER iMinSumm       AS INT  NO-UNDO.
      DEFINE OUTPUT PARAMETER oTotalMinSummP AS INT  NO-UNDO.
      
      DEFINE VARIABLE vDate         AS DATE NO-UNDO.
      DEFINE VARIABLE vYearDays     AS INT  NO-UNDO.
      DEFINE VARIABLE vCurrYearDays AS INT  NO-UNDO.
      DEFINE VARIABLE vMonthDays    AS INT  NO-UNDO.
      DEFINE VARIABLE vPercDays     AS INT  NO-UNDO.
      DEFINE VARIABLE vMinSummP     AS INT  NO-UNDO.
      DEFINE VARIABLE vMonthNum     AS INT  NO-UNDO.
      DEFINE VARIABLE vCurrMonthNum AS INT  NO-UNDO.
      
      iEndDate = iEndDate + 1. 
   
      FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct no-lock no-error.
      IF AVAIL acct-pos THEN
      DO:
      
         vCurrMonthNum = MONTH(iBegDate).
         RUN YearDays (INPUT iBegDate, OUTPUT vCurrYearDays).
      
         REPEAT vDate = iBegDate TO iEndDate:
         
            FIND FIRST acct-pos WHERE acct-pos.acct  EQ iAcct
                                  AND acct-pos.since EQ vDate NO-LOCK NO-ERROR.
            IF AVAIL acct-pos THEN 
            DO:
               IF vCurrMonthNum <> MONTH(vDate) THEN
               DO:
               
                  RUN MonthDays(INPUT iBegDate, INPUT iEndDate, INPUT MONTH(vDate - 1), OUTPUT vMonthDays).
                  RUN YearDays (INPUT vDate - 1, OUTPUT vYearDays). 
               
                  IF vCurrYearDays <> vYearDays THEN 
                  DO:
                  
                     IF iMinSumm < 30000000 THEN
                        vMinSummP = (iMinSumm * iPerc * vPercDays) / (vYearDays * 100).
                     ELSE
                        vMinSummP = (30000000 * iPerc * vPercDays) / (vYearDays * 100).
                  
                     oTotalMinSummP = oTotalMinSummP + vMinSummP.
                     
                     vCurrYearDays = vYearDays.
                     vPercDays     = 0.
                  END.
               
                  vPercDays = vPercDays + vMonthDays.
                  vCurrMonthNum = MONTH(vDate).
               END.
            END.
            ELSE 
            DO:
               FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct
                                    AND acct-pos.since < vDate NO-LOCK NO-ERROR.
               IF AVAIL acct-pos THEN
               DO:
                  IF vCurrMonthNum <> MONTH(vDate) THEN
                  DO:
                  
                     RUN MonthDays(INPUT iBegDate, INPUT iEndDate, INPUT MONTH(vDate - 1), OUTPUT vMonthDays).
                     RUN YearDays (INPUT vDate - 1, OUTPUT vYearDays).
                  
                     IF vCurrYearDays <> vYearDays THEN 
                     DO:
                     
                        IF iMinSumm < 30000000 THEN
                           vMinSummP = (iMinSumm * iPerc * vPercDays) / (vYearDays * 100).
                        ELSE
                           vMinSummP = (30000000 * iPerc * vPercDays) / (vYearDays * 100).
                     
                        oTotalMinSummP = oTotalMinSummP + vMinSummP.
                     
                        vCurrYearDays = vYearDays.
                        vPercDays     = 0.
                     END.
                  
                     vPercDays = vPercDays + vMonthDays. 
                     vCurrMonthNum = MONTH(vDate).
                  END.
               END.
            END.
         END.
       
         IF iMinSumm < 30000000 THEN
            vMinSummP = (iMinSumm * iPerc * vPercDays) / (vYearDays * 100).
         ELSE
            vMinSummP = (30000000 * iPerc * vPercDays) / (vYearDays * 100).
      
         oTotalMinSummP = oTotalMinSummP + vMinSummP.
      
      END.
      ELSE
         oTotalMinSummP = 0.

   END PROCEDURE.
   
   PROCEDURE TotalSubMinSummPerc:
      
      DEFINE INPUT PARAMETER iAcct LIKE acct.acct NO-UNDO.
      
      DEFINE INPUT  PARAMETER iPerc          AS INT  NO-UNDO.
      DEFINE INPUT  PARAMETER iBegDate       AS DATE NO-UNDO.
      DEFINE INPUT  PARAMETER iEndDate       AS DATE NO-UNDO.
      DEFINE OUTPUT PARAMETER oTotalMinSummP AS INT  NO-UNDO.
      
      DEFINE VARIABLE vDate         AS DATE NO-UNDO.
      DEFINE VARIABLE vYearDays     AS INT  NO-UNDO.
      DEFINE VARIABLE vMonthDays    AS INT  NO-UNDO.
      DEFINE VARIABLE vMonthNum     AS INT  NO-UNDO.
      DEFINE VARIABLE vCurrMonthNum AS INT  NO-UNDO.
      DEFINE VARIABLE vMinSummP     AS INT  NO-UNDO. 
      DEFINE VARIABLE vMinSumm      AS INT  NO-UNDO INIT 999999999.
      
      iEndDate = iEndDate + 1. 
   
      FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct no-lock no-error.
      IF AVAIL acct-pos THEN
      DO:
      
         vMinSumm = 999999999.
         vCurrMonthNum = MONTH(iBegDate).
      
         REPEAT vDate = iBegDate TO iEndDate:
         
            FIND FIRST acct-pos WHERE acct-pos.acct  EQ iAcct
                                  AND acct-pos.since EQ vDate NO-LOCK NO-ERROR.
            IF AVAIL acct-pos THEN 
            DO:
               IF vCurrMonthNum <> MONTH(vDate) THEN
               DO:
               
                  RUN MonthDays(INPUT iBegDate, INPUT iEndDate, INPUT MONTH(vDate - 1), OUTPUT vMonthDays).
                  RUN YearDays (INPUT vDate - 1, OUTPUT vYearDays).
               
                  IF vMinSumm < 30000000 THEN
                     vMinSummP = (vMinSumm * iPerc * vMonthDays) / (vYearDays * 100).
                  ELSE
                     vMinSummP = (30000000 * iPerc * vMonthDays) / (vYearDays * 100).
                  
                  oTotalMinSummP = oTotalMinSummP + vMinSummP.
                  
                  vCurrMonthNum = MONTH(vDate).
                  vMinSumm      = ABS(acct-pos.balance).
            
               END.
               ELSE DO:
                 IF ABS(acct-pos.balance) < vMinSumm THEN vMinSumm = ABS(acct-pos.balance).
               END.
            END.
            ELSE 
            DO:
               FIND LAST acct-pos WHERE acct-pos.acct EQ iAcct
                                    AND acct-pos.since < vDate NO-LOCK NO-ERROR.
               IF AVAIL acct-pos THEN
               DO:
                  IF vCurrMonthNum <> MONTH(vDate) THEN
                  DO:
                  
                     RUN MonthDays(INPUT iBegDate, INPUT iEndDate, INPUT MONTH(vDate - 1), OUTPUT vMonthDays).
                     RUN YearDays (INPUT vDate - 1, OUTPUT vYearDays).
                  
                     IF vMinSumm < 30000000 THEN
                        vMinSummP = (vMinSumm * iPerc * vMonthDays) / (vYearDays * 100).
                     ELSE
                        vMinSummP = (30000000 * iPerc * vMonthDays) / (vYearDays * 100).
                     
                     oTotalMinSummP = oTotalMinSummP + vMinSummP.
                     
                     vCurrMonthNum = MONTH(vDate).
                     vMinSumm      = ABS(acct-pos.balance).
               
                  END.
                  ELSE DO:
                     IF ABS(acct-pos.balance) < vMinSumm THEN vMinSumm = ABS(acct-pos.balance).
                  END.
               END.
            END.
         END.
      END.
      ELSE
         oTotalMinSummP = 0.
              
END PROCEDURE. 

&IF DEFINED (PARSER-PARSSEN-P) &THEN

   PROCEDURE ��猨����:
   
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO.
   
   DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
   DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.
   
   DEFINE VARIABLE vBegDate       AS DATE NO-UNDO.
   DEFINE VARIABLE vEndDate       AS DATE NO-UNDO.
   DEFINE VARIABLE vBegDate1      AS DATE NO-UNDO.
   DEFINE VARIABLE vEndDate1      AS DATE NO-UNDO.
   DEFINE VARIABLE vPerc          AS INT  NO-UNDO.
   DEFINE VARIABLE vNumQuart      AS INT  NO-UNDO.
   DEFINE VARIABLE vNumQuart1     AS INT  NO-UNDO.
   DEFINE VARIABLE vOpenDate      AS DATE NO-UNDO.
   DEFINE VARIABLE vCloseDate     AS DATE NO-UNDO.
   DEFINE VARIABLE vMinSumm       AS INT  NO-UNDO.
   DEFINE VARIABLE vTotalMinSummP AS INT  NO-UNDO.
   
   DEFINE BUFFER bacct FOR acct.

   is-ok = FALSE.

   IF pn NE 5 THEN
   DO:
      MESSAGE "���ࠢ��쭮� ������⢮ ��ࠬ��஢"
         ENTRY(1, PROGRAM-NAME(1), " ") "!" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   
   IF NOT (Pars-ValidParam(1)
       OR  Pars-ValidParam(2)
       OR  Pars-ValidParam(3)
       OR  Pars-ValidParam(4)
       OR  Pars-ValidParam(5)) THEN
       RETURN.
   
   ASSIGN
       vAcct     = Pars-GetString(0)
       vCurrency = Pars-GetString(1)
       vBegDate  = DATE(Pars-GetString(2))
       vEndDate  = DATE(Pars-GetString(3))
       vPerc     = INT(Pars-GetString(4))
   .
  
   IF NOT {assigned vAcct} OR NOT {assigned vCurrency} THEN
      RETURN.
      
   IF vCurrency EQ "810" THEN ASSIGN vCurrency = "".
   
   {find-act.i
       &bact = bacct
       &acct = vAcct
       &curr = vCurrency
   }

   IF NOT AVAILABLE bacct THEN
   DO:
      MESSAGE "��� """" + bacct.acct +  """"�� ������!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.
   
   FIND FIRST signs WHERE signs.file-name EQ "acct"
                      AND signs.surrogate EQ bacct.acct + "," + "" 
                      AND signs.code      EQ "��_���_�" NO-LOCK NO-ERROR.
   IF AVAIL signs THEN
   DO:
      vOpenDate = DATE(signs.xattr-value).
   END.

   FIND FIRST signs WHERE signs.file-name EQ "acct"
                      AND signs.surrogate EQ bacct.acct + "," + ""
                      AND signs.code      EQ "��_���_�" NO-LOCK NO-ERROR.
   IF AVAIL signs THEN
   DO:
      vCloseDate = DATE(signs.xattr-value).
   END.
       
   RUN NumQuart(INPUT vOpenDate, OUTPUT vNumQuart).
   RUN NumQuart(INPUT vBegDate, OUTPUT vNumQuart1).  
         
   IF vNumQuart NE vNumQuart1 THEN
   DO:
      IF vNumQuart  - vNumQuart1 EQ 3 OR vNumQuart1 - vNumQuart  EQ 1 THEN 
      DO:
         IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
         DO:
            vBegDate1 = vOpenDate + 1.
            vEndDate1 = vEndDate.
                
            RUN MinSumm(INPUT bacct.acct, INPUT vBegDate1, INPUT vEndDate1, OUTPUT vMinSumm).
            RUN TotalMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1, INPUT vMinSumm, OUTPUT vTotalMinSummP).
               
            RUN Pars-SetResult(vTotalMinSummP).
            vTotalMinSummP = 0.
              
            is-ok = TRUE.
         END.
      END.
      ELSE 
      DO:
         IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
         DO:
         
            vBegDate1 = vBegDate.
            vEndDate1 = vEndDate.
               
            RUN MinSumm(INPUT bacct.acct, INPUT vBegDate1, INPUT vEndDate1, OUTPUT vMinSumm).
            RUN TotalMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1, INPUT vMinSumm, OUTPUT vTotalMinSummP).
               
            RUN Pars-SetResult(vTotalMinSummP).
            vTotalMinSummP = 0.
               
            is-ok = TRUE.
         END.
      END. 
   END.
   ELSE
   DO:
      IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
      DO: 
      
         vBegDate1 = vBegDate.
         vEndDate1 = IF 
                     vEndDate >= DATE(vCloseDate) 
                     THEN 
                     DATE(vCloseDate) 
                     ELSE 
                     vEndDate.
         
         RUN MinSumm(INPUT bacct.acct, INPUT vBegDate1, INPUT vEndDate1, OUTPUT vMinSumm).
         RUN TotalMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1, INPUT vMinSumm, OUTPUT vTotalMinSummP).
        
         RUN Pars-SetResult(vTotalMinSummP).
         vTotalMinSummP = 0.
         
         is-ok = TRUE.
      END.
   END.
END PROCEDURE.
&ENDIF

&IF DEFINED (PARSER-PARSSEN-P) &THEN

   PROCEDURE ��猨���⌥�:
   
   DEFINE OUTPUT PARAMETER is-ok AS LOGICAL NO-UNDO.
   
   DEFINE VARIABLE vAcct     LIKE acct.acct     NO-UNDO.
   DEFINE VARIABLE vCurrency LIKE acct.currency NO-UNDO.
   
   DEFINE VARIABLE vBegDate       AS DATE NO-UNDO.
   DEFINE VARIABLE vEndDate       AS DATE NO-UNDO.
   DEFINE VARIABLE vBegDate1      AS DATE NO-UNDO.
   DEFINE VARIABLE vEndDate1      AS DATE NO-UNDO.
   DEFINE VARIABLE vPerc          AS INT  NO-UNDO.
   DEFINE VARIABLE vNumQuart      AS INT  NO-UNDO.
   DEFINE VARIABLE vNumQuart1     AS INT  NO-UNDO.
   DEFINE VARIABLE vOpenDate      AS DATE NO-UNDO.
   DEFINE VARIABLE vCloseDate     AS DATE NO-UNDO.
   DEFINE VARIABLE vMinSumm       AS INT  NO-UNDO.
   DEFINE VARIABLE vTotalMinSummP AS INT  NO-UNDO.
   
   DEFINE BUFFER bacct FOR acct.

   is-ok = FALSE.

   IF pn NE 5 THEN
   DO:
      MESSAGE "���ࠢ��쭮� ������⢮ ��ࠬ��஢"
         ENTRY(1, PROGRAM-NAME(1), " ") "!" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   
   IF NOT (Pars-ValidParam(1)
       OR  Pars-ValidParam(2)
       OR  Pars-ValidParam(3)
       OR  Pars-ValidParam(4)
       OR  Pars-ValidParam(5)) THEN
       RETURN.
   
   ASSIGN
       vAcct     = Pars-GetString(0)
       vCurrency = Pars-GetString(1)
       vBegDate  = DATE(Pars-GetString(2))
       vEndDate  = DATE(Pars-GetString(3))
       vPerc     = INT(Pars-GetString(4))
   .
  
   IF NOT {assigned vAcct} OR NOT {assigned vCurrency} THEN
      RETURN.
      
   IF vCurrency EQ "810" THEN ASSIGN vCurrency = "".
   
   {find-act.i
       &bact = bacct
       &acct = vAcct
       &curr = vCurrency
   }

   IF NOT AVAILABLE bacct THEN
   DO:
      MESSAGE "��� """" + bacct.acct +  """"�� ������!" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN ERROR.
   END.
   
   FIND FIRST signs WHERE signs.file-name EQ "acct"
                      AND signs.surrogate EQ bacct.acct + "," + "" 
                      AND signs.code      EQ "��_���_�" NO-LOCK NO-ERROR.
   IF AVAIL signs THEN
   DO:
      vOpenDate = DATE(signs.xattr-value).
   END.

   FIND FIRST signs WHERE signs.file-name EQ "acct"
                      AND signs.surrogate EQ bacct.acct + "," + ""
                      AND signs.code      EQ "��_���_�" NO-LOCK NO-ERROR.
   IF AVAIL signs THEN
   DO:
      vCloseDate = DATE(signs.xattr-value).
   END.
       
  
   RUN NumQuart(INPUT vOpenDate, OUTPUT vNumQuart).
   RUN NumQuart(INPUT vBegDate, OUTPUT vNumQuart1).  
         
   IF vNumQuart NE vNumQuart1 THEN
   DO:
      IF vNumQuart  - vNumQuart1 EQ 3 OR vNumQuart1 - vNumQuart  EQ 1 THEN 
      DO:
         IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
         DO: 
            vBegDate1 = vOpenDate + 1.
            vEndDate1 = vEndDate.
                
            RUN TotalSubMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1, OUTPUT vTotalMinSummP). 
            RUN Pars-SetResult(vTotalMinSummP).
               
            vTotalMinSummP = 0.
            is-ok = TRUE.
         END.
      END.
      ELSE 
      DO:
         IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
         DO:
         
            vBegDate1 = vBegDate.
            vEndDate1 = vEndDate.
               
            RUN TotalSubMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1, OUTPUT vTotalMinSummP).
            RUN Pars-SetResult(vTotalMinSummP).
               
            vTotalMinSummP = 0.
            is-ok = TRUE.
         END.
      END.
   END.
   ELSE 
   DO:
      IF vOpenDate < beg-date AND vCloseDate >= end-date THEN 
      DO: 
         vBegDate1 = vBegDate.
         vEndDate1 = IF 
                     vEndDate >= DATE(vCloseDate) 
                     THEN 
                     DATE(vCloseDate) 
                     ELSE 
                     vEndDate.
         
         RUN TotalSubMinSummPerc(INPUT bacct.acct, INPUT vPerc, INPUT vBegDate1, INPUT vEndDate1,  OUTPUT vTotalMinSummP).
         RUN Pars-SetResult(vTotalMinSummP).
         
         vTotalMinSummP = 0.
         is-ok = TRUE.
      END.
   END.
END PROCEDURE.
&ENDIF

/* ��⠥� ���⪨ �� ��ࠬ���� �㪠�� */
/* kam -- ����� ��� ��� */
PROCEDURE FifoGetParam:
	DEFINE INPUT PARAM fContCode AS CHAR NO-UNDO.
	DEFINE INPUT PARAM fParam AS DECIMAL NO-UNDO.
	DEFINE INPUT PARAM fDate AS DATE NO-UNDO.
	DEFINE OUTPUT PARAM fOutSum AS DECIMAL NO-UNDO.

	DEFINE VAR fFirstDate AS DATE NO-UNDO.
	DEFINE VAR fLastDate AS DATE NO-UNDO.

        fOutSum = 0.
	fFirstDate = DATE(01, 01, 1900).
	fLastDate = fFirstDate.
	FIND FIRST loan-var WHERE loan-var.contract = '�।��'
	  AND loan-var.cont-code = fContCode
	  AND loan-var.amt-id = fParam
	  AND loan-var.since <= fDate NO-LOCK NO-ERROR.
	IF AVAIL loan-var THEN DO:
		fOutSum = loan-var.balance. 
	END.
	FOR EACH loan-int WHERE loan-int.contract = '������'
	  AND loan-int.cont-code = loan.cont-code
	  AND (
               loan-int.id-k = fParam
	       OR loan-int.id-d = fParam
  	      )
	  AND loan-int.mdate > fFirstDate
	  AND loan-int.mdate <= fDate NO-LOCK BY loan-int.mdate:
		IF loan-int.id-d = fParam THEN fOutSum = fOutSum + loan-int.amt-rub.
		ELSE fOutSum = fOutSum - loan-int.amt-rub.
		fLastDate = loan-int.mdate.
	END.
	IF fOutSum > 0 THEN DO:
	 fParam = fParam - 3000.
	 FOR EACH loan-int WHERE loan-int.contract = '������'
	  AND loan-int.cont-code = loan.cont-code
	  AND (
               loan-int.id-k = fParam
	       OR loan-int.id-d = fParam
  	      )
	  AND loan-int.op <> -2 
	  AND loan-int.mdate > fLastDate
	  AND loan-int.mdate <= fDate NO-LOCK:
		IF loan-int.id-d = fParam THEN fOutSum = fOutSum + loan-int.amt-rub.
		ELSE fOutSum = fOutSum - loan-int.amt-rub.
 	 END.
	END.

END PROCEDURE.


&IF DEFINED (PARSER-PARSSEN-P) &THEN 
    /*
      �� ������: ��������� �㬬� ��襭�� �� �� �� ��ࠬ��ࠬ ��� ���ᯥ祭�� ���⮢ �� ��⮤� FIFO
      ���⠪�� : �㭪��ਬ��2 ( ��ࠬ���1 [, ��ࠬ���2 ] )
	  �᫨ ��騩 ����, �:
		1 ���-� - ����� �������, 2 ���-� - �㬬� ��襭��, 
		3 ���-� - ⥪�騩 ��ࠬ���, 4 ���-� - ��᫥����⥫쭮��� ��ࠬ��஢.
	  �᫨ �� �����⭮�� ��ࠬ����, � 1 ���-� - ⥪�騩 ��ࠬ���.
      ����     : MNG-SKU-KAM
    */

	DEFINE NEW GLOBAL SHARED temp-table all-sum-par no-undo    /* ����樨 �� ��ࠬ��ࠬ �� ���� ������������� ����窨 */
		field par as int64
		field summ as de
		field date as date
	.
	DEFINE TEMP-TABLE each-par no-undo    /* ���⪨ �� ��ࠬ���� */
		field par as int64
		field total as de 
	.
              /* kam -- ����� ��� ��� */
	DEFINE TEMP-TABLE temploan-int NO-UNDO LIKE loan-int.

    PROCEDURE FifoSumT:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    /* --- */
    /*    DEFINE VARIABLE vFuncParam1 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE vFuncParam2 AS DECIMAL    NO-UNDO.*/
        DEFINE VARIABLE fiforesult     AS DECIMAL    NO-UNDO.
			
		DEF VAR pp-seq as char no-undo.
		DEF VAR pp-k as int64 no-undo.
		DEF VAR pp-par as int64 no-undo.
		DEF VAR pp-sum as de no-undo.	
		DEF VAR pp-sum1 as de no-undo extent 2.
		DEF VAR pp-contcode as char no-undo.
		DEF VAR pp-total as de no-undo.
              /* kam -- ����� ��� ��� */
		DEF VAR pp-mainsum as de no-undo init 0.
		DEF VAR pp-idd as de no-undo.
.
		
		
        IF NOT (Pars-ValidParam(4)) THEN RETURN.
/* message " 444-1-!!! 1=" Pars-GetString(1) " -2=" Pars-GetString(2) " -3=" Pars-GetString(3) " -4=" Pars-GetString(4) view-as alert-box.*/

				{empty all-sum-par}
				{empty each-par}
				pp-contcode = AddFilToLoan(Pars-GetString(0), shFilial).
				pp-seq  = Pars-GetString(3).
				pp-total = Pars-GetDec(1). 
 message " nnn " pp-contcode " - " pp-seq " - " Pars-GetString(3) view-as alert-box.				
				do pp-k = 1 to num-entries(pp-seq,","):     /* ���⪨ �� ��ࠬ��ࠬ */
					/* kam -- ����� ��� ��� */ 
					IF DECIMAL(ENTRY(pp-k,pp-seq,",")) > 3000 THEN DO:
					RUN FifoGetParam(pp-contcode,DEC(ENTRY(pp-k,pp-seq,",")),wop.con-date,OUTPUT pp-sum).
					END.
					ELSE DO:
					RUN STNDRT_PARAM in h_Loan 	("�।��",          /* �����祭�� ������� */
								pp-contcode,						/* ����� ������� */
								ENTRY(pp-k,pp-seq,","), 			/* ��� ��ࠬ���  */
								wop.con-date,                       /* �� ���� (��.���⮪) */
								OUTPUT pp-sum,
								OUTPUT pp-sum1[1], OUTPUT pp-sum1[2]).
					END.
/*  message ENTRY(pp-k,pp-seq,",") + ' ' +  string(pp-sum) view-as alert-box.  */
					if pp-sum > 0 then do:
						create each-par.
						assign 
							each-par.par = int(entry (pp-k,pp-seq,","))
							each-par.total = pp-sum
						.
					end.
				end.
	
			/* kam -- ����� ��� ��� */ 
				{empty temploan-int}
				for each loan-int where loan-int.cont-code = pp-contcode 
                                	and lookup(string(loan-int.id-d),pp-seq) > 0 no-lock:
					find first each-par where each-par.par = loan-int.id-d no-lock no-error.
					if avail each-par then do:
						create temploan-int.
						buffer-copy loan-int to temploan-int.
						if loan-int.id-d = 10 or loan-int.id-d = 7 or loan-int.id-d = 48 then do:
 		                                        find first each-par where each-par.par = (loan-int.id-d + 3000) no-lock no-error.
							if avail each-par then do:
							    if temploan-int.op = -2 and temploan-int.op-entry = -1 
								then delete temploan-int.
							    else temploan-int.id-d = temploan-int.id-d + 3000.
							end.
						end.
					end.
				end.
				for each temploan-int 
					where temploan-int.cont-code = pp-contcode 
					and lookup(string(temploan-int.id-d),pp-seq) > 0
					break by temploan-int.mdate DESCENDING:
						find first each-par where each-par.par = temploan-int.id-d no-lock no-error.
						if avail each-par and (each-par.total > 0) then do:
							create all-sum-par.
							assign
								all-sum-par.par = temploan-int.id-d
								all-sum-par.date = temploan-int.mdate
								all-sum-par.summ = minimum (temploan-int.amt-rub, each-par.total)
							.								
							assign each-par.total = each-par.total - all-sum-par.summ.							
						end.						
				end.

				for each all-sum-par /* �롮� ����権 ��� ��襭�� */
					break by all-sum-par.date by lookup(string(all-sum-par.par),pp-seq):
						if all-sum-par.summ <= pp-total 
							then pp-total = pp-total - all-sum-par.summ.
							else do:
								assign all-sum-par.summ = pp-total.
								pp-total = 0.	
							end.					
				end.

				pp-mainsum = 0.
				pp-par = Pars-GetInt(2).
				find first all-sum-par  no-lock no-error. 
				if avail all-sum-par then do: 
/* run instview.p(TEMP-TABLE all-sum-par:HANDLE).		 */
					for each all-sum-par where all-sum-par.par = pp-par:						
						pp-mainsum = pp-mainsum + all-sum-par.summ.		
					end.			
				end.
			
/*message " 444 pp-mainsum=" string(pp-mainsum)  view-as alert-box.*/
		RUN Pars-SetResult ( pp-mainsum ).
		/*ASSIGN result_l[pj-pn] = pp-mainsum  mvar[pj-pn] = "" no-error.*/
    
    /* --- */
        is-ok = TRUE.
    END PROCEDURE.

    PROCEDURE FifoSumMT:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    /* --- */
			
		DEF VAR pp-par as int64 no-undo.
		DEF VAR pp-mainsum as de no-undo init 0.
		
        IF NOT (Pars-ValidParam(1)) THEN RETURN.
/*message " 444-1-!!! 1=" Pars-GetString(1) view-as alert-box.*/
				pp-mainsum = 0.
				pp-par = Pars-GetInt(0).
				find first all-sum-par no-lock no-error. 
				if avail all-sum-par then do: 
					for each all-sum-par where all-sum-par.par = pp-par:
						pp-mainsum = pp-mainsum + all-sum-par.summ.		
					end.	
				end.
						
/*message " 444 pp-mainsum=" string(pp-mainsum)  view-as alert-box.*/
		RUN Pars-SetResult ( pp-mainsum ).
		/*ASSIGN result_l[pj - pn] = pp-mainsum  mvar[pj - pn] = "" no-error.*/
		/*ASSIGN result_l[pj - pn] = pp-mainsum mvar[pj - pn] = (IF pp-mainsum = 0 THEN "0" ELSE "").*/
    
    /* --- */
        is-ok = TRUE.
    END PROCEDURE.


/* ������� ����樨 ������ 3000+ */
/* kam -- ����� ��� ��� */ 
    PROCEDURE FifoSumCT:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
	DEF VAR pp-tumen AS DEC NO-UNDO.
	DEF VAR pp3000 AS DEC NO-UNDO.
	DEF VAR iOp AS DEC NO-UNDO.
	DEF BUFFER bLI FOR loan-int.
	DEF BUFFER bloan-int FOR loan-int.
	DEF VAR pp-contcode as char no-undo.

        IF NOT (Pars-ValidParam(2)) THEN RETURN.

	pp-contcode = AddFilToLoan(Pars-GetString(0), shFilial).
        iOp = Pars-GetDec(1).

		pp-tumen = 0.
		pp3000 = 0.
		for each all-sum-par where all-sum-par.par = iOp no-lock:
			pp-tumen = pp-tumen + all-sum-par.summ.		
		end.
		for each loan-int where loan-int.cont-code = pp-contcode 
		    and loan-int.mdate <= wop.con-date
		    and (loan-int.id-d = iOp or loan-int.id-k = iOp) no-lock:
		    if loan-int.id-d = iOp then pp3000 = pp3000 + loan-int.amt-rub.
		       else pp3000 = pp3000 - loan-int.amt-rub.
		end.
		if pp3000 > 0 and pp-tumen > 0 then do:
		    if pp3000 > pp-tumen then pp3000 = pp-tumen.
		end.

/*
message string(iOp) + ' ' + string(pp3000) + ' ' + string(wop.con-date) view-as alert-box.
  */

		for each bloan-int where bloan-int.cont-code = pp-contcode 
			and bloan-int.id-k = iOp              
			and bloan-int.id-d = 5
			and bloan-int.mdate = wop.con-date:
				delete bloan-int.
		end.
		if pp3000 <> 0 then do:
			FIND LAST bLI WHERE bLI.cont-code = pp-contcode 
                	AND bLI.mdate = wop.con-date 
        	        NO-LOCK NO-ERROR. 
			CREATE loan-int.
			ASSIGN
			loan-int.op-date = wop.con-date 
			loan-int.mdate = wop.con-date 
			loan-int.nn = (IF AVAIL bLI 
                                       THEN bLI.nn + 1
                                       ELSE 1)
			loan-int.contract = '�।��'
			loan-int.user-id = USERID("bisquit")
			loan-int.cont-code = pp-contcode
			loan-int.op = -2
			loan-int.op-entry = -1
			loan-int.id-k = iOp
			loan-int.id-d = 5
			loan-int.amt-rub = pp3000
        	        .
	              	RELEASE loan-int.

		end.
        RUN Pars-SetResult ( 0 ).  
        is-ok = TRUE.
    END PROCEDURE.

&ENDIF

&IF DEFINED (PARSER-PARSSEN-P) &THEN 
    /*
      �� ������: ��������� �㬬� ��襭�� �� �� �� ��ࠬ��ࠬ ��� ���ᯥ祭�� ���⮢ �� ��⮤� FIFO
      ���⠪�� : �㭪��ਬ��2 ( ��ࠬ���1 [, ��ࠬ���2 ] )
	  �᫨ ��騩 ����, �:
		1 ���-� - ����� �������, 2 ���-� - �㬬� ��襭��, 
		3 ���-� - ⥪�騩 ��ࠬ���, 4 ���-� - ��᫥����⥫쭮��� ��ࠬ��஢.
	  �᫨ �� �����⭮�� ��ࠬ����, � 1 ���-� - ⥪�騩 ��ࠬ���.
      ����     : MNG-SKU
    */
        /*
	DEFINE NEW GLOBAL SHARED temp-table all-sum-par no-undo    /* ����樨 �� ��ࠬ��ࠬ �� ���� ������������� ����窨 */
		field par as int64
		field summ as de
		field date as date
	.
	
	DEFINE TEMP-TABLE each-par no-undo    /* ���⪨ �� ��ࠬ���� */
		field par as int64
		field total as de 
	.
	*/
    PROCEDURE FifoSum:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    /* --- */
    /*    DEFINE VARIABLE vFuncParam1 AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE vFuncParam2 AS DECIMAL    NO-UNDO.*/
        DEFINE VARIABLE fiforesult     AS DECIMAL    NO-UNDO.
			
		DEF VAR pp-seq as char no-undo.
		DEF VAR pp-k as int64 no-undo.
		DEF VAR pp-par as int64 no-undo.
		DEF VAR pp-sum as de no-undo.	
		DEF VAR pp-sum1 as de no-undo extent 2.
		DEF VAR pp-contcode as char no-undo.
		DEF VAR pp-total as de no-undo.
		DEF VAR pp-mainsum as de no-undo init 0.
		
        IF NOT (Pars-ValidParam(4)) THEN RETURN.
/*message " 444-1-!!! 1=" Pars-GetString(1) " -2=" Pars-GetString(2) " -3=" Pars-GetString(3) " -4=" Pars-GetString(4) view-as alert-box.*/

				{empty all-sum-par}
				{empty each-par}
				pp-contcode = AddFilToLoan(Pars-GetString(0), shFilial).
				pp-seq  = Pars-GetString(3).
				pp-total = Pars-GetDec(1). 
/*message " nnn " pp-contcode " - " pp-seq " - " Pars-GetString(3) view-as alert-box.		*/		
				do pp-k = 1 to num-entries(pp-seq,","):     /* ���⪨ �� ��ࠬ��ࠬ */
					RUN STNDRT_PARAM in h_Loan 	("�।��",          /* �����祭�� ������� */
								pp-contcode,						/* ����� ������� */
								ENTRY(pp-k,pp-seq,","), 			/* ��� ��ࠬ���  */
								wop.con-date,                       /* �� ���� (��.���⮪) */
								OUTPUT pp-sum,
								OUTPUT pp-sum1[1], OUTPUT pp-sum1[2]).
					if pp-sum > 0 then do:
						create each-par.
						assign 
							each-par.par = int(entry (pp-k,pp-seq,","))
							each-par.total = pp-sum
						.
					end.
				end.
				for each loan-int 
					where loan-int.cont-code = pp-contcode and lookup(string(loan-int.id-d),pp-seq) > 0
					break by loan-int.mdate DESCENDING:
						find first each-par where each-par.par = loan-int.id-d no-lock no-error.
						if avail each-par and (each-par.total > 0) then do:
							create all-sum-par.
							assign
								all-sum-par.par = loan-int.id-d
								all-sum-par.date = loan-int.mdate
								all-sum-par.summ = minimum (loan-int.amt-rub, each-par.total)
							.								
							assign each-par.total = each-par.total - all-sum-par.summ.							
						end.						
				end.

				for each all-sum-par /* �롮� ����権 ��� ��襭�� */
					break by all-sum-par.date by lookup(string(all-sum-par.par),pp-seq):
						if all-sum-par.summ <= pp-total 
							then pp-total = pp-total - all-sum-par.summ.
							else do:
								assign all-sum-par.summ = pp-total.
								pp-total = 0.	
							end.					
				end.

				pp-mainsum = 0.
				pp-par = Pars-GetInt(2).
				find first all-sum-par  no-lock no-error. 
				if avail all-sum-par then do: 
/*run instview.p(TEMP-TABLE all-sum-par:HANDLE).		*/
					for each all-sum-par where all-sum-par.par = pp-par:						
						pp-mainsum = pp-mainsum + all-sum-par.summ.		
					end.			
				end.
			
/*message " 444 pp-mainsum=" string(pp-mainsum)  view-as alert-box.*/
		RUN Pars-SetResult ( pp-mainsum ).
		/*ASSIGN result_l[pj-pn] = pp-mainsum  mvar[pj-pn] = "" no-error.*/
    
    /* --- */
        is-ok = TRUE.
    END PROCEDURE.

    PROCEDURE FifoSumM:
        DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    /* --- */
			
		DEF VAR pp-par as int64 no-undo.
		DEF VAR pp-mainsum as de no-undo init 0.
		
        IF NOT (Pars-ValidParam(1)) THEN RETURN.
/*message " 444-1-!!! 1=" Pars-GetString(1) view-as alert-box.*/
				pp-mainsum = 0.
				pp-par = Pars-GetInt(0).
				find first all-sum-par no-lock no-error. 
				if avail all-sum-par then do: 
					for each all-sum-par where all-sum-par.par = pp-par:
						pp-mainsum = pp-mainsum + all-sum-par.summ.		
					end.			
				end.
						
/*message " 444 pp-mainsum=" string(pp-mainsum)  view-as alert-box.*/
		RUN Pars-SetResult ( pp-mainsum ).
		/*ASSIGN result_l[pj - pn] = pp-mainsum  mvar[pj - pn] = "" no-error.*/
		/*ASSIGN result_l[pj - pn] = pp-mainsum mvar[pj - pn] = (IF pp-mainsum = 0 THEN "0" ELSE "").*/
    
    /* --- */
        is-ok = TRUE.
    END PROCEDURE.
&ENDIF


&IF DEFINED (PARSER-PARSSEN-P) &THEN

/*  �� ������: ��⠥� ����譥 ���᫥�� ��業�� �� ⥪�騩 ��� �㪠��....
**  kam 
**  ���⠪�� : ��搠��(vPer,vOpDate,vNachDate)
**  �����頥� ࠧ���� ����. ��業⮢ �� ⥪�騩 ��� 
*/

PROCEDURE ��搠��:
    
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
      
   DEFINE VARIABLE vResult      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
   
   DEFINE VARIABLE vPer         AS CHAR       NO-UNDO.
   DEFINE VARIABLE vOpDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE vNachDate    AS DATE       NO-UNDO.   
   DEFINE VARIABLE prevSumm     AS DECIMAL    INIT 0    NO-UNDO.
   DEFINE VARIABLE currSumm     AS DECIMAL    INIT 0    NO-UNDO.
   DEFINE VARIABLE fullSumm     AS DECIMAL    INIT 0    NO-UNDO.
   
   is-ok = FALSE.
   

   IF NOT (Pars-ValidParam(3)) THEN RETURN.
      
   vPer = Pars-GetString(0).
   vOpDate = DATE(Pars-GetString(1)).
   vNachDate = DATE(Pars-GetString(2)).
   vResult = 0.
   
   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
      IF NOT VALID-HANDLE(h_templ) THEN RETURN.
      FIND FIRST loan WHERE loan.contract  EQ "dps"
                        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
         NO-LOCK NO-ERROR.      
      IF NOT AVAIL loan THEN RETURN.
      
   RUN ExcessProc.p(input loan.cont-code, input vOpDate, input vNachDate, output prevSumm, output currSumm).
   fullSumm = currSumm + prevSumm.
   if currSumm < 0 then currSumm = 0.
   if fullSumm < 0 then fullSumm = 0.

   if vPer = '�' then 
    RUN Pars-SetResult ( currSumm ).
   else if vPer = '*' then 
    RUN Pars-SetResult ( fullSumm ).
/*  message string(vPer) + ' ' + string(fullSumm) view-as alert-box.  */
   is-ok = TRUE.
END PROCEDURE.


&ENDIF


&IF DEFINED (PARSER-PARSSEN-P) &THEN

/*  �� ������: �����頥� ���祭�� ���४����� �����������
**  kam 
**  ���⠪�� : ���४���("�����������")
**  
*/

PROCEDURE ���४���:
   
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   is-ok = FALSE.
   DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE cResult      AS CHAR       NO-UNDO.
   DEFINE VARIABLE cCode        AS CHAR       NO-UNDO.

   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
   IF NOT VALID-HANDLE(h_templ) THEN RETURN.
   FIND FIRST loan WHERE loan.contract  EQ "dps"
        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
   IF NOT AVAIL loan THEN RETURN.

   IF NOT (Pars-ValidParam(1)) THEN RETURN.
   cCode = Pars-GetString(0).
      
   cResult = GetXattrValueEx("loan",
                          loan.contract + ',' + loan.cont-code,
                          cCode,
                          "").
                          
   RUN Pars-SetCharResult ( cResult ). 

   is-ok = TRUE.
END PROCEDURE.

&ENDIF



&IF DEFINED (PARSER-PARSACCT-P) &THEN

/*  �� ������: ���뢠�� ���, ��� �� ������
**  kam 01/2015
**  ���⠪�� : AcctNalDps("Date("������"))
**  
*/
   
PROCEDURE AcctNalDps:
    
    DEFINE OUTPUT PARAM is-ok AS LOG NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
    DEFINE VARIABLE vOpDate      AS DATE       NO-UNDO.
    DEFINE VARIABLE myacct       AS CHAR       NO-UNDO.
    
    myacct = ''.
    RUN SetSysConf IN h_base ("mycustcat",'').       
    RUN SetSysConf IN h_base ("mycustid",'').
    RUN SetSysConf IN h_base ("myopendate",'').   
    
    IF NOT (Pars-ValidParam(1)) THEN RETURN.
    vOpDate = DATE(Pars-GetString(0)).
    
    RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
    IF NOT VALID-HANDLE(h_templ) THEN RETURN.
    FIND FIRST Loan WHERE Loan.contract  EQ "dps"
        AND Loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
    IF NOT AVAIL loan THEN RETURN.
   
    FOR EACH acct WHERE acct.cust-cat = loan.cust-cat
        AND acct.cust-id = loan.cust-id
        AND acct.bal-acct = 60323 
        AND acct.close-date = ? NO-LOCK,
        FIRST signs WHERE signs.file-name = 'acct' 
        AND signs.surrogate = acct.acct + ',' 
        AND signs.code = '������'
        AND signs.code-value = '����'
        NO-LOCK:
            myacct = acct.acct.
        LEAVE.
   END.
    IF myacct = '' THEN DO:
        RUN SetSysConf IN h_base ("mycustcat",STRING(loan.cust-cat)).       
        RUN SetSysConf IN h_base ("mycustid",STRING(loan.cust-id)).
        RUN SetSysConf IN h_base ("myopendate",STRING(vOpDate)).
    
        FIND FIRST op-kind WHERE op-kind.op-kind = 'Acct60323' NO-LOCK NO-ERROR.
        IF NOT AVAIL op-kind THEN RETURN. 

        RUN g-trans.p (vOpDate, RECID(op-kind)) NO-ERROR. /* �࠭����� ������ ��� */

        IF RETURN-VALUE EQ {&RET-ERROR} OR
            ERROR-STATUS:ERROR THEN RETURN.

        myacct = GetSysConf("myacct").
    
        IF myacct = '' OR myacct = ? THEN RETURN.
    END.
    
    RUN Pars-SetCHARResult (myacct).
    is-ok = TRUE.
END PROCEDURE.

&ENDIF



&IF DEFINED (PARSER-PARSSEN-P) &THEN

/*  �� ������: �����頥� �㬬� ����᫥���� �������
**  kam 01/2015
**  ���⠪�� : SummNalDps("Date("������"))
**  
*/
   
PROCEDURE SumNalDps:
     
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   is-ok = FALSE.
   DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE dResult      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE vOpDate      AS DATE       NO-UNDO.
   DEFINE VARIABLE sAcctInt     AS CHAR       NO-UNDO INIT ''.
   DEFINE VARIABLE sAcctNal     AS CHAR       NO-UNDO INIT ''.
   DEFINE VARIABLE bShtraf      AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE sContCode    AS CHAR       NO-UNDO.
   DEFINE VARIABLE sDoc_Ref     AS CHAR       NO-UNDO.
   DEF VAR vBegDate        AS DATE NO-UNDO.
   DEF VAR vEndDate        AS DATE NO-UNDO.
   dResult = 0.
   
/*   DEFINE VAR h_dpspc AS 
   {intrface.get dpspc}    */

   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
   IF NOT VALID-HANDLE(h_templ) THEN RETURN.
   FIND FIRST Loan WHERE Loan.contract  EQ "dps"
        AND Loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
   IF NOT AVAIL loan THEN RETURN.
   sContCode = loan.cont-code.
   sDoc_Ref = loan.doc-ref.
   IF NOT (Pars-ValidParam(1)) THEN RETURN.
   vOpDate = DATE(Pars-GetString(0)).


   vBegDate = vOpDate.
   FOR EACH loan-cond WHERE loan-cond.cont-code = loan.cont-code NO-LOCK BY loan-cond.since DESC:
                                                                                                	
	vBegDate = loan-cond.since.
	LEAVE.
   END.


  /* 
   FIND FIRST acct WHERE acct.cust-cat = loan.cust-cat
        AND acct.cust-id = loan.cust-id
        AND acct.bal-acct = 90909
        AND acct.details BEGINS loan.doc-ref 
        AND acct.close-date = ? NO-LOCK NO-ERROR.
   IF aVAIL acct THEN  */ DO: 
        RUN dps_fshtr.p(INPUT RECID(Loan),
                   INPUT vOpDate,
                   OUTPUT bShtraf).
                   
   END.
   
   IF bShtraf THEN DO:
        FIND FIRST loan-acct WHERE loan-acct.contract = 'dps'
            and loan-acct.cont-code = sContCode
            and loan-acct.acct-type = 'loan-dps-int' NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN sAcctInt = loan-acct.acct.
        FIND FIRST loan-acct WHERE loan-acct.contract = 'dps'
            and loan-acct.cont-code = sContCode
            and loan-acct.acct-type = 'loan-nal' NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN sAcctNal = loan-acct.acct.
        IF sAcctInt <> '' AND sAcctNal <> '' THEN DO:
            FOR EACH op-entry WHERE op-entry.acct-db = sAcctInt 
                AND op-entry.acct-cr = sAcctNal
                AND op-entry.op-status BEGINS "�"
                AND op-entry.op-date <= vOpDate 
		AND op-entry.op-date > vBegDate NO-LOCK:
                dResult = dResult + op-entry.amt-rub.
            END.
        END.
   END. 
   RUN Pars-SetResult ( dResult ).
   is-ok = TRUE.
END PROCEDURE.

&ENDIF


&IF DEFINED (PARSER-PARSSEN-P) &THEN

/*  �� ������: ��⠥� ������ ����� ��� ���
**  ���뢠�� ���ઢ���� ��犮����, ⨯ ������, ������⢮ ����� ��⮢ (����� ������ �� ��⠥�) 
**  kam 
**  ���⠪�� : �㦥�����炥�()
**  
*/
PROCEDURE �㦥�����炥�:
    
   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   is-ok = FALSE.
   DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
   DEFINE VARIABLE cResult      AS CHAR       NO-UNDO.
   DEFINE VARIABLE cCode        AS CHAR       NO-UNDO.
   DEFINE VARIABLE countdps_t   AS INT64      NO-UNDO.
   DEFINE VARIABLE vResult      AS INT64      INIT 0 NO-UNDO.
   cResult = '���'.
   IF NOT (Pars-ValidParam(1)) THEN RETURN.
 main:
 do:
   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
   IF NOT VALID-HANDLE(h_templ) THEN leave main.
   FIND FIRST loan WHERE loan.contract  EQ "dps"
        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
   IF NOT AVAIL loan THEN leave main.
  
   select count(*) into countdps_t from loan-acct where loan-acct.cont-code = loan.cont-code and loan-acct.acct-type = 'loan-dps-t'.
   if countdps_t > 1 then leave main.
    
   if loan.cont-type = 'gold' or loan.cont-type = '���ᨮ���' then do:
       cResult = '��'.
       leave main.
   end.

   if loan.cont-type = '�������' then do:
       cResult = '��'.

       leave main.
   end.

   cCode = GetXattrValueEx("loan",
                          loan.contract + ',' + loan.cont-code,
                          "��犮����",
                          "").
 end. /* main: */
 if cResult = '��' then vResult = 1.
 RUN Pars-SetResult ( vResult ). 
 is-ok = TRUE.

END PROCEDURE.
&ENDIF


&IF DEFINED (PARSER-PARSACCT-P) &THEN
/*
** �� ������: �����頥� ���祭�� �� ������ ���祭�� �����. (���砫� �饬 � SysConf)
** ���⠪�� : ��ࠢ�筨���Sys ()
** ����     : Om 20/04/2005  kam 12/03/2014
**
** ��㬥���:
**    ��� ����� ���ਥ� �� �ࠢ�筨��.
**    ���祭�� �ࠢ�筨��, ࠧ������� �����묨 ("01/01/01,256.2,128,��").
**    ���, �� ������ ����室��� �맢��� �ࠢ�筨�.
**    �ॡ���� �� �⫠��� (��/��� �� 㬮�砭�� ���).
*/
DEFINE NEW GLOBAL SHARED temp-table sprznachsys no-undo    /* ����樨 �� ��ࠬ��ࠬ �� ���� ������������� ����窨 */
        field vid as char
        field zhach as char
        INDEX idx vid
    .
    
PROCEDURE ��ࠢ�筨���Sys:

   DEF OUTPUT PARAM oIsOk AS LOG NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
   DEFINE VARIABLE vRefCode     AS CHAR   NO-UNDO. /* ��� �ࠢ�筨��. */
   DEFINE VARIABLE vCrValue     AS CHAR   NO-UNDO. /* ���祭�� �ࠢ�筨��. */
   DEFINE VARIABLE vRefDate     AS DATE   NO-UNDO. /* ���, ���祭��. */
   DEFINE VARIABLE vDebug       AS LOG    NO-UNDO. /* �㦭� �� �⫠���. */
   DEFINE VARIABLE vResult      AS CHAR   NO-UNDO. /* �������. */
   DEFINE VARIABLE vId          AS CHAR   NO-UNDO.
   
    
                        /* �஢�ઠ ������⢠ �室��� ��ࠬ��஢. */
   IF NOT ( Pars-ValidParam (4) OR Pars-ValidParam (3)) THEN RETURN. 
   ASSIGN
                        /* ���� �訡�� ��� �宣� ��室�. */
      oIsOk       = TRUE
                        /* ��ନ஢���� ��ࠬ��஢. */
      vRefCode    = Pars-GetString (0)
      vCrValue    = Pars-GetString (1)
      vRefDate    = DATE (Pars-GetString (2))   WHEN pn GE 2
      vDebug      = Pars-GetString (3) EQ "��"  WHEN pn GE 3
   .
   vId = vRefCode + vCrValue + STRING(vRefDate). 

    /*
   vResult = GetSysConf(vId).
   */
   FIND FIRST sprznachsys WHERE sprznachsys.vid = vId NO-LOCK NO-ERROR.
   IF AVAIL sprznachsys THEN DO:
   /* IF {assigned vResult} THEN DO: */
        vResult = sprznachsys.zhach.
        RUN Pars-SetCHARResult (vResult).
        RETURN.
   END.

   /* ����祭�� ���祭�� �� �ࠢ�筨��. */
   vResult = GetRefVal  (vRefCode, vRefDate, vCrValue).
                        /* ��ନ஢���� १����. */
   
   RUN Pars-SetCHARResult (vResult).
   CREATE sprznachsys.
   ASSIGN
        sprznachsys.vid = vId
        sprznachsys.zhach = vResult
   .

   /*
   RUN SetSysConf IN h_base (vId, vResult).
   */
   
   RETURN.
END PROCEDURE.

&ENDIF


/*&IF DEFINED (PARSER-PARSSEN-P) &THEN*/

/*  �� ������: �롮� �����ᨨ � ��⮬ �ᯮ�짮������� ����� ���
**  kau
**  ���⠪�� : ��������� (vKom, vAcct, vDate)
**  �����頥� ������� �᫨ � ��� ��� ����� ��� ��� �᫨ 㦥 �����稫��
	���� �����頥� 0
*/

PROCEDURE ���������:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
    DEF VAR vKom        AS CHAR NO-UNDO. /*�室�騩 ��ࠬ��� �������*/
    DEF VAR vAcct       AS CHAR NO-UNDO. /*�室�騩 ��ࠬ��� ��� ������*/
    DEF VAR vDate       AS DATE NO-UNDO. /*�室�騩 ��ࠬ��� ��� ���भ�*/
    DEF VAR vSumKS      AS DEC  NO-UNDO. /*�室�騩 ��ࠬ��� �㬬� ���ᮢ�� ����樨*/
    DEF VAR oKom        AS CHAR NO-UNDO. /*��室�騩 ��ࠬ��� �������*/
   
   /*�᫨ �������筮 ��ࠬ��஢ ��।��� �訡��*/
    IF NOT ( Pars-ValidParam (4) OR Pars-ValidParam (3)) THEN RETURN.
   
    vKom = Pars-GetString(0).
    vAcct = Pars-GetString(1).
    vDate = DATE(Pars-GetString(2)).
    IF pn >= 3 
    THEN vSumKS = DEC(Pars-GetString (3)).
    ELSE vSumKS = 0. /* WHEN pn GE 3.*/
    

    RUN pack-usl-kom.p (INPUT vAcct,
			INPUT vDate,
			INPUT vSumKS,
			INPUT-OUTPUT vKom).
    is-ok = TRUE.
    RUN Pars-SetCHARResult ( vKom ).
END PROCEDURE.

/*&ENDIF*/

/*  �� ������: ����᪠�� ��楤��� 㪠������ � ��ࠬ���. ����� �������� ���祭�� ��� �� ��������.
**  kau
**  ���⠪�� : RunProcParam (vNameProc, vIsOutRezult, vParamProc)
*/


PROCEDURE RunProcParam:
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
    IF NOT Pars-ValidParam(3) AND NOT Pars-ValidParam(2) THEN RETURN.
/* --- */
    DEFINE VARIABLE vNameProc		AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vParamProc	 	AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE vIsOutRezult	AS INT64      NO-UNDO.
    DEFINE VARIABLE vResult     	AS CHARACTER  NO-UNDO.

    ASSIGN
        vNameProc	= Pars-GetString ( 0 )
	vIsOutRezult	= Pars-GetInt	 ( 1 )
	.
    IF pn > 1 THEN vParamProc 	= Pars-GetString ( 2 ).

    IF vIsOutRezult EQ 1
    THEN DO:
	IF pn > 1
	THEN RUN VALUE (vNameProc) ( vParamProc , OUTPUT vResult).
	ELSE RUN VALUE (vNameProc) ( OUTPUT vResult).
	RUN Pars-SetCHARResult ( vResult ).

    END.
    ELSE DO:
	IF pn > 1
	THEN RUN VALUE (vNameProc) (vParamProc).
	ELSE RUN VALUE (vNameProc).
    END.

/* --- */
    is-ok = TRUE.

END PROCEDURE.


/*
** �� ������: ��楤�� �����頥� ���⮪ �� �᭮����� ���� ������ � ����訢��� �㬬� ������ � ���쭥�訬 �����⮬ �㬬�
**
** ���⠪�� : �뤠砄�(�㬬�).
**
** �ਬ��    :  �뤠砄�(5000) ���  �뤠砄�(�㬬�(1))
*/

PROCEDURE �뤠砄�:

DEFINE OUTPUT PARAMETER result  AS INT64 NO-UNDO.

DEFINE VARIABLE rrid AS RECID NO-UNDO.
DEFINE VARIABLE v_nal LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_bnal LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_per LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_ost LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE inc-mess  AS CHAR   NO-UNDO.
DEFINE VARIABLE inc-init  AS DEC    NO-UNDO.
DEFINE BUFFER bop-entry for op-entry.


   IF NOT (Pars-ValidParam(1)) THEN RETURN.
   
   ASSIGN
      inc-init  = Pars-GetDec (0)

   .

pause 0.
/*FIND FIRST bop-entry WHERE RECID(bop-entry) EQ rec_opentr.*/
ASSIGN
    v_ost = inc-init.
    v_bnal = 0.
    v_per = 0.
    v_nal = v_ost.
    
DEFINE FRAME ftune
  v_ost  LABEL "���⮪ �� ���"
  SKIP(1)
  v_bnal LABEL "�/� ����᫥���"
  SKIP
  v_per  LABEL "��⠢��� �� ���"
  SKIP(1)
  v_nal  LABEL "�१ �����"
  with centered row 10 overlay side-labels 1 col
  title "[ ᯮᮡ �뤠� �������� �।�� ]".
Do on error undo, leave on endkey undo, leave with frame ftune:
    DISPLAY v_ost v_bnal v_per v_nal.
    enable v_bnal v_per with frame ftune.

    ON VALUE-CHANGED OF v_bnal,v_per IN FRAME ftune DO:
        assign v_bnal v_per.
        v_nal = v_ost - v_bnal - v_per.
        DISPLAY v_nal WITH FRAME ftune.
    END.
    ON LEAVE OF v_bnal IN FRAME ftune DO:
	IF v_bnal + v_per > v_ost
        THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "1", "�㬬� ����� ���⪠").
         RETURN NO-APPLY {&RET-ERROR}.
        END.
    END.

    WAIT-FOR GO OF FRAME ftune.
End.

if LASTKEY EQ KEYCODE("ESC") THEN
	result = -1.
ELSE
result = 1.

RUN SetSysConf IN h_base ("�뤠砭��",STRING(v_nal)).
RUN SetSysConf IN h_base ("�뤠砮��",STRING(v_per)).
RUN SetSysConf IN h_base ("�뤠砡�����",STRING(v_bnal)).
END PROCEDURE.


/*
** �� ������: ��楤�� ����訢��� ���� �㬬� ����樨 � �࠭������ ������ / �����ᥭ�� �� �����
**
** ���⠪�� : �㬬���(<����� ���᪠���>, <��砫쭮� ���祭�� �⢥�>).
**
** �ਬ��    :  �㬬���("������ᥭ��",0) 
*/

PROCEDURE �㬬���:

   DEFINE OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

   DEFINE VARIABLE inc-mess0  AS CHAR   NO-UNDO.
   DEFINE VARIABLE inc-mess1  AS CHAR   NO-UNDO.
   DEFINE VARIABLE inc-mess2  AS CHAR   NO-UNDO.
   DEFINE VARIABLE inc-init1  AS DEC    NO-UNDO.


   IF NOT (Pars-ValidParam(4)) THEN RETURN.
   
   ASSIGN
      inc-mess1 = Pars-GetString (0) + ":"
      inc-mess2 = Pars-GetString (1)
      inc-mess0  = Pars-GetString (2) + ":"
      inc-init1  = Pars-GetDec (3)
   .

   FORM
      inc-mess1 
         VIEW-AS TEXT SIZE 30 BY 1 
         FORMAT "x(30)"
      inc-mess2 
         VIEW-AS TEXT SIZE 10 BY 1 AT ROW 1 COL 18 
         FORMAT "x(10)"
      inc-mess0 
         VIEW-AS TEXT SIZE 20 BY 1 
         FORMAT "x(20)"
      inc-init1
         VIEW-AS FILL-IN SIZE 30 BY 1 AT ROW 2 COL 18
         FORMAT "->>>>,>>>,>>>,>>9.99"
         HELP ""
       

   WITH FRAME fQstn CENTERED ROW 10 OVERLAY NO-LABELS 1 COL
   COLOR MESSAGES TITLE "������� ����� �������".

   IF LASTKEY NE 10 AND LASTKEY NE 13 THEN RETURN.

   B_INPUT:
   DO WITH FRAME fQstn
   ON ERROR    UNDO B_INPUT, LEAVE B_INPUT
   ON ENDKEY   UNDO B_INPUT, LEAVE B_INPUT:
      PAUSE 0.
      DISPLAY  inc-init1 inc-mess0 inc-mess1 inc-mess2 WITH FRAME fQstn.
      UPDATE inc-init1.
      RUN Pars-SetResult(inc-init1).
   END.

   HIDE FRAME fQstn NO-PAUSE.

   is-ok = TRUE.

END PROCEDURE.






/*
** �� ������: ��楤�� �����頥� ���⮪ �� �᭮����� ���� ������ � ����訢��� �㬬� ������ � ���쭥�訬 �����⮬ �㬬�
**
** ���⠪�� : �뤄�(�㬬�).
**
** �ਬ��    :  �뤄�(5000) ���  �뤄�(�㬬�(1))
*/
PROCEDURE �뤄�:

DEFINE OUTPUT PARAMETER result  AS INT64 NO-UNDO.

DEFINE VARIABLE rrid1 AS RECID NO-UNDO.
DEFINE VARIABLE v_nal1 LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_bnal1 LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_per1 LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE v_ost1 LIKE op-entry.amt-cur NO-UNDO.
DEFINE VARIABLE inc-mess1  AS CHAR   NO-UNDO.
DEFINE VARIABLE inc-init1  AS DEC    NO-UNDO.
DEFINE BUFFER bop-entry1 for op-entry.


   IF NOT (Pars-ValidParam(1)) THEN RETURN.
   
   ASSIGN
      inc-init1  = Pars-GetDec (0)

   .

pause 0.
/*FIND FIRST bop-entry WHERE RECID(bop-entry) EQ rec_opentr.*/
ASSIGN
    v_ost1 = inc-init1.
    v_bnal1 = 0.
    v_nal1 = v_ost1.
    
DEFINE FRAME ftune
  v_ost1  LABEL "���⮪ �� ���"
  SKIP(1)
  v_bnal1 LABEL "�㬬� ������"
  SKIP(1)
  v_nal1  LABEL "���⮪"
  with centered row 10 overlay side-labels 1 col
  title "[ ������ �㬬� ������ ]".
Do on error undo, leave on endkey undo, leave with frame ftune:
    DISPLAY v_ost1 v_bnal1 v_nal1.
    enable v_bnal1  with frame ftune.

    ON VALUE-CHANGED OF v_bnal1 IN FRAME ftune DO:
        assign v_bnal1.
        v_nal1 = v_ost1 - v_bnal1.
        DISPLAY v_nal1 WITH FRAME ftune.
    END.
    ON LEAVE OF v_bnal1 IN FRAME ftune DO:
	IF v_bnal1 > v_ost1
        THEN DO:
         RUN Fill-SysMes IN h_tmess ("", "", "1", "�㬬� ����� ���⪠").
         RETURN NO-APPLY {&RET-ERROR}.
        END.
    END.

    WAIT-FOR GO OF FRAME ftune.
End.

if LASTKEY EQ KEYCODE("ESC") THEN
	result = -1.
ELSE
result = 1.

RUN SetSysConf IN h_base ("�뤠砭����묨",STRING(v_bnal1)).
END PROCEDURE.






/*
  kam
  �� ������: �饬 ��� �� ����
  ���⠪�� : ������(���_���,"=,<,<=")
*/
PROCEDURE ������:
  DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */
/* --- */
  is-ok = FALSE.
  IF NOT Pars-ValidParam(3) THEN RETURN.
/* --- */

  DEFINE VARIABLE h_templ      AS HANDLE     NO-UNDO. 
  DEFINE VARIABLE l1 as char no-undo.
  DEFINE VARIABLE l2 as char no-undo. /* <, =, <= */
  DEF VAR vDate       AS DATE NO-UNDO. /*�室�騩 ��ࠬ��� ��� ���भ�*/
  DEF BUFFER bloan-acct FOR loan-acct.

    l1 = Pars-GetString(0).
    l2 = Pars-GetString(1).
    vDate = DATE(Pars-GetString(2)).


main:
do:
   RUN DPS_VALID_HANDLE (INPUT-OUTPUT h_templ).   
   IF NOT VALID-HANDLE(h_templ) THEN leave main.
   FIND FIRST loan WHERE loan.contract  EQ "dps"
        AND loan.cont-code EQ ENTRY(1, SUBSTRING(h_templ:PRIVATE-DATA, 6))
        NO-LOCK NO-ERROR.      
   IF not AVAIL loan THEN leave main.



  CASE l2 :
    WHEN '<'  THEN DO:
        	FIND LAST bloan-acct  of loan WHERE 
                                         bloan-acct.acct-type = l1
                                         AND bloan-acct.since     <= vDate   NO-LOCK NO-ERROR.
		IF AVAIL bloan-acct THEN do: vDate = bloan-acct.since. 
end.
					
		FIND LAST loan-acct  of loan WHERE 
                                         loan-acct.acct-type = l1
                                         AND loan-acct.since     < vDate   NO-LOCK NO-ERROR.
    END.
  END CASE.
  IF AVAIL loan-acct THEN do:
	    RUN Pars-SetCHARResult ( loan-acct.acct ).

                             end.
else if avail bloan-acct then 	    RUN Pars-SetCHARResult ( bloan-acct.acct ).
  ELSE RETURN.
END. /* main */

/* --- */
  is-ok = TRUE.
END PROCEDURE. 






PROCEDURE ����3:

   DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO.

   DEF VAR vSumm  AS DECIMAL    NO-UNDO.
   DEF VAR vCurr1 AS CHARACTER  NO-UNDO.
   DEF VAR vCurr2 AS CHARACTER  NO-UNDO.
   DEF VAR vKurs  AS CHARACTER  NO-UNDO.
   DEF VAR vPodr  AS CHARACTER  NO-UNDO.

   IF NOT (Pars-ValidParam(2) OR
           Pars-ValidParam(3) OR
           Pars-ValidParam(4) OR
           Pars-ValidParam(5))
   THEN RETURN.

   assign
      vSumm  = Pars-GetDec ( 0 )
      vCurr1 = Pars-GetStringFormatted ( 1, "999" )
      vCurr2 = if pn < 2 THEN tcur      ELSE Pars-GetStringFormatted ( 2, "999" )
      vKurs  = if pn < 3 THEN '�������' ELSE Pars-GetString ( 3 )
      vPodr  = Pars-GetStringFormatted ( 4, "9999" )
      .

   FIND FIRST code WHERE code.class = "����� ���" AND code.code = vKurs NO-LOCK NO-ERROR.

   IF NOT AVAIL code
   THEN DO:
            MESSAGE "� 蠡���� �஢���� N " + STRING(wop.op-templ) + " ��뫪� �� ���������騩 ���� '" + vKurs + "'" VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.

   /*��室�� ����� ������*/
   IF     vCurr1 NE "{&in-NC-Code}"
      AND vCurr1 NE ""
   THEN DO:
            FIND LAST irate-time WHERE irate-time.instr-cat  = "currency"
                                   AND irate-time.rate-type  = vKurs
                                   AND irate-time.instr-code = vCurr1
                                   AND irate-time.branch-id  = vPodr
                                   AND date(irate-time.iratedatetime) <= date(in-op-date)
                 SHARE-LOCK NO-WAIT NO-ERROR.
                 /*message string(irate-time.rate-instr) view-as alert-box.*/
                 IF NOT AVAIL irate-time
                 THEN DO:
                          MESSAGE "������ �" wop.op-templ ": ����" '"' + vKurs + '"' "������"  vCurr1 "�� ��⠭�����!" VIEW-AS ALERT-BOX ERROR.
                          RETURN.
                      END.
                 ASSIGN vSumm = vSumm * irate-time.rate-instr / irate-time.per.
                 /*message string(vSumm) view-as alert-box.*/
        END.

   /*��室�� ����� ������*/
   IF     vCurr2 NE "{&in-NC-Code}"
      AND vCurr2 NE ""
   THEN DO:
            FIND LAST irate-time WHERE irate-time.instr-cat  = "currency"
                                   AND irate-time.rate-type  = vKurs
                                   AND irate-time.instr-code = vCurr2
                                   AND irate-time.branch-id  = vPodr
                                   AND date(irate-time.iratedatetime) <= date(in-op-date)
                 SHARE-LOCK no-wait NO-ERROR.
                 /*message string(irate-time.rate-instr) view-as alert-box.*/
                 IF NOT AVAIL irate-time
                 THEN DO:
                          MESSAGE "������ �" wop.op-templ ": ����" '"' + vKurs + '"' "������"  vCurr2 "�� ��⠭�����!" VIEW-AS ALERT-BOX ERROR.
                          RETURN.
                      END.
                 ASSIGN vSumm = (vSumm / irate-time.rate-instr) * irate-time.per.
        END.

   RUN Pars-SetResult ( vSumm ).

   is-ok = TRUE.

END PROCEDURE.





/*�� ������: �����頥� �� office �� ���짮��⥫�*/
/*���⠪�� : Office�����(���_���짮��⥫�) ��� Office�����() - �� ⥪�饬�*/

PROCEDURE Office�����:
/* --- */
DEFINE VARIABLE l-user AS CHAR NO-UNDO.
DEFINE VARIABLE out-office as char no-undo.
DEFINE VARIABLE vResult as char no-undo.
DEF OUTPUT PARAM  is-ok AS LOGICAL   NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

is-ok = FALSE.
{&type-er} = "".
IF pn ne 0 and pn ne 1
THEN DO: {&type-er} = {&EGMBadParamCount}. RETURN. END.
ELSE pn = pn - 1.

l-user = if pn eq 0 then Pars-GetString ( 0 ) ELSE userid('bisquit').

find first signs where signs.file-name eq "_user"
                   and signs.code      eq "office"
                   and signs.surrogate eq l-user
           no-lock no-error.

if avail signs then out-office = IF signs.xattr-value NE "" THEN signs.xattr-value ELSE substr(signs.code-value,1,4).

if out-office EQ "0598"
THEN vResult = "0598".
ELSE vResult = "����".

RUN Pars-SetCHARResult ( vResult ).

mvar[pj - pn] = """" + mvar[pj - pn] + """". /* ����頥� � ����窨 */

is-ok = TRUE.

END PROCEDURE.






PROCEDURE BlockAcctPlus:

    DEF VAR vAcct          AS CHAR NO-UNDO. /*���� �室�騩 ��ࠬ��� - ��� ������*/
    DEF VAR vBlock         AS CHAR NO-UNDO. /*��ன �室�騩 ��ࠬ��� - �����஢��*/
    DEF VAR vAllBlock      AS CHAR NO-UNDO.
    DEF VAR vResult        AS CHAR NO-UNDO.
    DEF OUTPUT PARAM is-ok AS LOGICAL NO-UNDO. /* �ᯥ譮 �� �믮����� ? */

    /*�᫨ �������筮 ��ࠬ��஢ ��।��� �訡��*/
    IF NOT (Pars-ValidParam( 2 )) THEN RETURN.

    vAcct  = Pars-GetString( 0 ).
    vBlock = Pars-GetString( 1 ).

    vAllBlock = ''.
    vResult   = 'NO'.

    FOR EACH blockobject WHERE blockobject.class-code   = 'blockacct'
                           AND blockobject.file-name    = 'acct'
                           AND blockobject.end-datetime = ?
                           AND blockobject.surrogate BEGINS vAcct
    NO-LOCK:
             vAllBlock = vAllBlock + ',' + blockobject.block-type.
    END.

    MESSAGE vAllBlock view-as alert-box.

    IF CAN-DO(vAllBlock, ENTRY(1, vBlock)) THEN DO: vResult = 'YES'. END.
    IF CAN-DO(vAllBlock, ENTRY(2, vBlock)) THEN DO: vResult = 'YES'. END.

    MESSAGE vResult view-as alert-box.

    is-ok = TRUE.

    RUN Pars-SetCHARResult ( vResult ).

END PROCEDURE.
