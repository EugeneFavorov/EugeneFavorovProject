
/* +++ crtagval.i was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:05pm +++ */

/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: CRTAGVAL.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 18.04.2008 12:11 fEAk    
     Modified: 18.04.2008 12:11 fEAk     <comment>
*/
{intrface.get xclass}

/* ���᮪ �㭪権, ����� ���� ��ࠡ��뢠���� ������ ��楤�ன */                                                       
vOtherTags = "docnumber,docvid,birthdate,face,sotr_fio,numdover,datadover,postemp,postfio,���_���," + 
             "�����_�ਪ�(loan-dps-dv)," + 
             "��ࠬ���@����,��ࠬ���@����_��,��ࠬ���@���,��ࠬ���@�������,��ࠬ���@�����," +
             "dolgIP,dolgRP,bankID,cityName,nameGB,adrGb,adrkor,nameFil,adrFil,innkpp,bik,korsch,ogrn,telfax,nameOO,adrOO,acctper,dateper".

/* Support functions */

PROCEDURE Run-PrnProc.
    DEFINE INPUT  PARAMETER iProc   AS CHARACTER.
    DEFINE INPUT  PARAMETER iDate1  AS DATE.
    DEFINE INPUT  PARAMETER iDate2  AS DATE.
    DEFINE INPUT  PARAMETER iStrPar AS CHARACTER.
    DEFINE OUTPUT PARAMETER oResult AS CHARACTER.

    RUN VALUE(LC(iProc) + ".p") (OUTPUT oResult, iDate1, iDate2, iStrPar).
    IF {assigned RETURN-VALUE} THEN
        oResult = {&RETURN_VALUE}.
    ELSE
        ASSIGN
            oResult   = printtext
            printtext = ""
        .
    ASSIGN
        oResult = FILL("_", 20) WHEN iRet AND NOT {assigned oResult}
        oResult = TRIM(oResult)
    .
END PROCEDURE.

FUNCTION date2text RETURNS CHAR (INPUT iDate AS DATE, INPUT iAdd AS LOGICAL):
   DEF VAR mont_h  AS CHAR
INIT "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,������,�����,�������" NO-UNDO.

   IF iAdd THEN
      RETURN '"' + STRING(DAY(iDate)) + '" ' + 
                 ENTRY(MONTH(iDate), mont_h) +
                 STRING(YEAR(iDate), " 9999 �.").
   ELSE
      RETURN STRING(DAY(iDate)) + " " + 
                 ENTRY(MONTH(iDate), mont_h) +
                 STRING(YEAR(iDate), " 9999 �.").

END FUNCTION.

/* Main procedures */

PROCEDURE docnumber:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".

   ASSIGN oRes = person.document.
                
END PROCEDURE.

PROCEDURE acctper:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".
   REStmp = ENTRY(1,GETXATTRVALUEEX('loan',loan.contract + ',' + loan.cont-code,'acct_trans',' '),' ').


   ASSIGN oRes = REStmp.
                
END PROCEDURE.

PROCEDURE dateper:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".
   REStmp = date2text(DATE(ENTRY(1,GETXATTRVALUEEX('loan',loan.contract + ',' + loan.cont-code,'date_trans',' '),' ')),NO).

   ASSIGN oRes = REStmp.
                
END PROCEDURE.

PROCEDURE docvid:                                              
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".
   ASSIGN 
    oRes = GetCodeNameEx("�������", person.document-id, person.document-id)
   .
                
END PROCEDURE.

PROCEDURE birthdate:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   FIND FIRST loan WHERE RECID(loan) EQ iRid NO-LOCK NO-ERROR.
   IF NOT AVAIL loan THEN
      RETURN "Err".

   FIND FIRST person WHERE person.person EQ loan.cust-id NO-LOCK NO-ERROR.             
   IF NOT AVAIL person THEN
      RETURN "Err".

   ASSIGN oRes = date2text(person.birthday, NO).
                
END PROCEDURE.

PROCEDURE face:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   ASSIGN oRes = FGetSetting("�������",?,?) + " " + FGetSetting("�����",?,?).             
                
END PROCEDURE.

PROCEDURE sotr_fio:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.


   FIND FIRST cust-role WHERE cust-role.file-name  EQ "_user"
                                AND cust-role.Class-Code EQ "���짮��⥫�"
                                AND cust-role.surrogate  EQ USERID('bisquit')
         NO-LOCK NO-ERROR.
   ASSIGN oRes = ( IF AVAIL cust-role THEN cust-role.cust-name ELSE "").
   IF NOT AVAIL cust-role THEN DO:
     RUN Fill-AlertSysMes IN h_tmess("","",1,"� ⥪�饣� ���짮��⥫� �� ����஥�� �ਢ離� � 䨧.����.").

     FIND first _user where _user._userid eq user('bisquit') no-lock no-error.
     IF avail _user then
	oRes = _user._user-name.
   END.

END PROCEDURE.

PROCEDURE numdover:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'�⤥�����','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'����᭍����','�� ��।�����').
  
   if GetXAttrValue('_user',USERID('bisquit'),'����᭍����') = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'����᭍����').
                
END PROCEDURE.

PROCEDURE datadover:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'�⤥�����','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'����᭄��','�� ��।�����').
  
   if GetXAttrValue('_user',USERID('bisquit'),'����᭄��') = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'����᭄��').
                
END PROCEDURE.

PROCEDURE postemp:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   DEF VAR REStmp1 AS CHARACTER NO-UNDO.
   DEF VAR REStmp2 AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'�⤥�����','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   REStmp = GETXATTRVALUEEX('_user',nachOperu,'��������쐏','�� ��।�����').
   REStmp1 = GETXATTRVALUEEX('_user',user('bisquit'),'����᭄��','').
   REStmp2 = GETXATTRVALUEEX('_user',user('bisquit'),'����᭍����','').

   if REStmp1 = '' OR REStmp2 = '' THEN
   oRes = REStmp.
   ELSE
   oRes = GetXAttrValue('_user',USERID('bisquit'),'��������쐏').
                
END PROCEDURE.

PROCEDURE postfio:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
   DEF VAR OTDEL AS CHARACTER NO-UNDO.
   DEF VAR NACHOPERU AS CHARACTER NO-UNDO.
   DEF VAR REStmp AS CHARACTER NO-UNDO.
   DEF VAR REStmp1 AS CHARACTER NO-UNDO.
   DEF VAR REStmp2 AS CHARACTER NO-UNDO.
   OTDEL = GETXATTRVALUEEX('_user',USERID('bisquit'),'�⤥�����','0400').
   NACHOPERU = GETXATTRVALUEEX('branch',otdel,'NachOperu','O0400GUE').
   find first _user where _user._userid eq user('bisquit') no-lock no-error.
	if avail _user then
		REStmp = _user._user-name.	
   REStmp1 = GETXATTRVALUEEX('_user',user('bisquit'),'����᭄��','').
   REStmp2 = GETXATTRVALUEEX('_user',user('bisquit'),'����᭍����','').

   if REStmp1 = '' OR REStmp2 = '' THEN DO:
   find first _user where _user._userid eq NACHOPERU no-lock no-error.
	if avail _user then
		oRes = _user._user-name.

	END.                
	ELSE 
   oRes = REStmp.


END PROCEDURE.

PROCEDURE ���_���:
   DEF INPUT PARAM iRid  AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   ASSIGN oRes = Date2Text(iDate, YES).             
                
END PROCEDURE.

/*��������� ���짮��⥫� � ��*/
PROCEDURE dolgIP:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  
    oRes = GetXAttrValueEx("_user",USERID("bisquit"),"���������","").
    
END PROCEDURE.

/*��������� ���짮��⥫� � ��*/
PROCEDURE dolgRP:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  
    oRes = GetXAttrValueEx("_user",USERID("bisquit"),"��������쐏","").
    
END PROCEDURE.

PROCEDURE bankID:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
        oRes = GetXAttrValueEx("branch",
                               STRING(branch.branch-id),
                               "InternalID",
                               "").
        ELSE oRes = GetXAttrValueEx("branch",
                                    STRING(branch.parent-id),
                                    "InternalID",
                                    "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

PROCEDURE cityName:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.
  DEF VAR tmpVar        AS CHARACTER NO-UNDO.
  DEF VAR tmp           AS CHARACTER NO-UNDO.
  DEF VAR i             AS INTEGER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        tmpVar = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "��瀤�����",
                                 "").
        DO i = 1 TO NUM-ENTRIES(tmpVar):
          IF TRIM(ENTRY(i, tmpVar)) BEGINS "�." THEN  /*�饬 ����� ᯨ᪠ ᮤ�ঠ騩 "�."*/
          DO:
            tmp = ENTRY(i, tmpVar).
          END.
        END.
        oRes = LEFT-TRIM(tmp, "�. ").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*��� ��*/
PROCEDURE nameGB:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   oRes = GetXAttrValueEx("branch",
                          "0000",
                          "dps_bank",
                          "").
END PROCEDURE.

/*���� ��*/
PROCEDURE adrGB:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   oRes = '���� ��宦�����: ' + GetXAttrValueEx("branch",
                                                 "0000",
                                                 "��瀤�����",
                                                 "").
END PROCEDURE.

/*���� ���ࠢ����� ����ᯮ����樨*/
PROCEDURE adrkor:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

    oRes = '���� ���ࠢ����� ����ᯮ����樨: ' + GetXAttrValueEx("branch",
                                                                  "0500",
                                                                  "��瀤����",
                                                                  "").
END PROCEDURE.

/*������������ 䨫����*/
PROCEDURE nameFil:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("11", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "dps_bank",
                                 "").
        ELSE oRes = GetXAttrValueEx("branch",
                                    STRING(branch.parent-id),
                                    "dps_bank",
                                    "").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*���� 䨫����*/
PROCEDURE adrFil:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("11", branch.branch-type) THEN
        oRes = '���� ��宦�����: ' + GetXAttrValueEx("branch",
                                                      STRING(branch.branch-id),
                                                      "��瀤�����",
                                                      "").
        ELSE oRes = '���� ��宦�����: ' + GetXAttrValueEx("branch",
                                                           STRING(branch.parent-id),
                                                           "��瀤�����",
                                                           "").
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*��� � ���*/
PROCEDURE innkpp:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:
            oRes = '���/��� ' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "���",
                                                "").
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "���",
                                                "").
          END.   
        ELSE 
          DO:
            oRes = '���/��� ' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "���",
                                                "").
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "���",
                                                "").
          END.        
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*���*/
PROCEDURE bik:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
        oRes = '��� ' + GetXAttrValueEx("branch",
                                        STRING(branch.branch-id),
                                        "�������",
                                        "").
        ELSE oRes = '��� ' + GetXAttrValueEx("branch",
                                             STRING(branch.parent-id),
                                             "�������",
                                             "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

/*������ + ��� ����� ������*/
PROCEDURE korsch:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:  
            oRes = '�/� � ' + GetXAttrValueEx("branch",
                                              STRING(branch.branch-id),
                                              "�����",
                                              "").
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "��爧�������",
                                                "").
          END. 
        ELSE 
          DO:
            oRes = '�/� � ' + GetXAttrValueEx("branch",
                                              STRING(branch.parent-id),
                                              "�����",
                                              "").
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "��爧�������",
                                                "").
          END.
      END.  
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*���� ��� ��*/
PROCEDURE ogrn:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10", branch.branch-type) THEN
        oRes = '���� ' + GetXAttrValueEx("branch",
                                         STRING(branch.branch-id),
                                         "����",
                                         "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

/*⥫�䮭 � 䠪�*/
PROCEDURE telfax:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("10,11", branch.branch-type) THEN
          DO:
            IF GetXAttrValueEx("branch", STRING(branch.branch-id), "����䮭", "") NE "" THEN
            oRes = '���/䠪� ' + GetXAttrValueEx("branch",
                                                 STRING(branch.branch-id),
                                                 "����䮭",
                                                 "").
            IF GetXAttrValueEx("branch", STRING(branch.branch-id), "����", "") NE "" THEN
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                 STRING(branch.branch-id),
                                                 "����",
                                                 "").
          END.   
        ELSE 
          DO:
            IF GetXAttrValueEx("branch", STRING(branch.parent-id), "����䮭", "") NE "" THEN
            oRes = '���/䠪� ' + GetXAttrValueEx("branch",
                                                 STRING(branch.parent-id),
                                                 "����䮭",
                                                 "").
            IF GetXAttrValueEx("branch", STRING(branch.parent-id), "����", "") NE "" THEN
            oRes = oRes + '/' + GetXAttrValueEx("branch",
                                                 STRING(branch.parent-id),
                                                 "����",
                                                 "").
          END.
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*������������ ����樮����� ���*/
PROCEDURE nameOO:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("23", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "dps_bank",
                                 ""). 
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*���� ����.���*/
PROCEDURE adrOO:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
    NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      DO:
        IF CAN-DO("23", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "��瀤�����",
                                 "").
          oRes = IF oRes EQ "0518" THEN "0500" ELSE "".
        IF oRes NE "" THEN
          oRes = "���� ��宦�����: " + oRes.
      END.
    ELSE
      LEAVE.
  END.
END PROCEDURE.

/*PROCEDURE <���>:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   <⥫�>             
                
END PROCEDURE.*/

/* --- crtagval.i was humbly modified by (c)blodd converter v.1.11 on 6/9/2017 12:05pm --- */
