{globals.i}
{intrface.get tmess}
{intrface.get xclass}
&GLOBAL-DEFINE CustIdntSurr    cust-ident.cust-code-type + ',' + ~
                               cust-ident.cust-code      + ',' + ~
                               STRING(cust-ident.cust-type-num)

DEFINE TEMP-TABLE ttDoc NO-UNDO
   FIELD code AS CHARACTER
   FIELD val  AS CHARACTER
.   
                               
DEFINE VAR mUpdate AS LOGICAL INIT NO NO-UNDO.                               
DEFINE VAR mAvtoSrok AS CHARACTER NO-UNDO.
DEFINE VAR mVarMess  AS CHARACTER NO-UNDO.
DEFINE VAR mEndDate  AS DATE NO-UNDO.
DEFINE VAR mNewDate  AS DATE NO-UNDO.
DEFINE VAR mFlOk     AS LOGICAL NO-UNDO.
DEFINE VAR mIdStr    AS INTEGER NO-UNDO.


FUNCTION CalcSrokForDocument RETURNS DATE
      (iFormula   AS CHARACTER,
       iDateDoc   AS DATE,  /* ��� �뤠� ���㬥��*/
       iBirthDate AS DATE  /* ��� ஦�����*/
      ):
   DEFINE VAR vFirstSrok  AS INT64 NO-UNDO.
   DEFINE VAR vSecondSrok AS INT64 NO-UNDO.
   DEFINE VAR vLastSrok   AS INT64 NO-UNDO.
   DEFINE VAR i AS INTEGER NO-UNDO.
   DEFINE VAR oDate       AS DATE  NO-UNDO.
   DEFINE VAR oDate1       AS DATE  NO-UNDO.
   DEFINE VAR oDate2      AS DATE  NO-UNDO.
   DEFINE VAR oDate3       AS DATE  NO-UNDO.   
   
   IF iFormula   BEGINS "�" THEN DO:
      vFirstSrok  = INT64(REPLACE(ENTRY(1,iFormula),"�","")).
      vSecondSrok = IF NUM-ENTRIES(iFormula) GE 2 
                    THEN INT64(REPLACE(ENTRY(2,iFormula),"�",""))
                    ELSE 0.
      oDate = DATE(IF MONTH(iDateDoc) + vSecondSrok GT 12 
                   THEN (MONTH(iDateDoc) + vSecondSrok - 12)
                   ELSE (MONTH(iDateDoc) + vSecondSrok),
                   1,
                   YEAR(iDateDoc) + vFirstSrok + IF MONTH(iDateDoc) + vSecondSrok GT 12 THEN 1 ELSE 0).
      oDate = DATE(MONTH(oDate),DAY(iDateDoc),YEAR(oDate)) NO-ERROR.
      i = 1.
      DO WHILE ERROR-STATUS:ERROR:
         oDate = DATE(MONTH(oDate),DAY(iDateDoc) - i,YEAR(oDate)) NO-ERROR.
         i = i + 1.
      END.
   END.
   ELSE IF iFormula BEGINS "�" THEN DO:
      vFirstSrok =  INT64(REPLACE(ENTRY(1,iFormula,";"),"�","")).
      vSecondSrok = IF NUM-ENTRIES(iFormula,";") GE 2 
                    THEN INT64(REPLACE(ENTRY(2,iFormula,";"),"�",""))
                    ELSE 0.
      vLastSrok   = 100.              
/*    vAgeClient = YEAR(iDate) - YEAR(iBirthDate) - 
                   (IF (MONTH(iDate) EQ MONTH(iBirthDate) AND
                       DAY(iDate)   GT DAY(iBirthDate))  OR 
                       MONTH(iDate) GT MONTH(iBirthDate) THEN 0
                    ELSE 1).*/
      oDate1 = DATE(MONTH(iBirthDate),DAY(iBirthDate),YEAR(iBirthDate) + vFirstSrok) NO-ERROR.
      i = 1.
      DO WHILE ERROR-STATUS:ERROR:
         oDate1 = DATE(MONTH(oDate1),DAY(iBirthDate) - i,YEAR(oDate1)) NO-ERROR.
         i = i + 1.
      END.
      
      oDate2 = DATE(MONTH(iBirthDate),DAY(iBirthDate),YEAR(iBirthDate) + vSecondSrok) NO-ERROR.
      i = 1.
      DO WHILE ERROR-STATUS:ERROR:
         oDate2 = DATE(MONTH(oDate1),DAY(iBirthDate) - i,YEAR(oDate2)) NO-ERROR.
         i = i + 1.
      END.
      
      oDate3 = DATE(MONTH(iBirthDate),DAY(iBirthDate),YEAR(iBirthDate) + vLastSrok) NO-ERROR.
      i = 1.
      DO WHILE ERROR-STATUS:ERROR:
         oDate3 = DATE(MONTH(oDate3),DAY(iBirthDate) - i,YEAR(oDate3)) NO-ERROR.
         i = i + 1.
      END.

      IF iDateDoc <= oDate1 THEN
      oDate = oDate1.
      ELSE IF iDateDoc <= oDate2 THEN
      oDate = oDate2.
      ELSE IF iDateDoc <= oDate3 THEN
      oDate = oDate3.
      
   END.   
   RETURN oDate.   
END FUNCTION.







RUN Init-SysMes IN h_tmess ("op-kind,��������", "��������� 㤮�⮢������ ���㬥�⮢ 䨧. ���", "").
      RUN Fill-SysMes IN h_tmess ("","","1",      
      "N �/�     "        + " " +
      "��� ������"       + " " +
      "��� ���㬥��  "   + " " +
      "��� ���㬥��"    + " " +
      "����饭��                                                      |").      




mIdStr = 1.      
FOR EACH code WHERE code.class  EQ "�������"
                AND code.parent EQ "�������"
NO-LOCK:
   
   mAvtoSrok = GetXattrValueEx("code",code.class + "," + code.code,"��⮑ப","").
   IF NOT {assigned mAvtoSrok} THEN NEXT.
   CREATE ttDoc.
   ASSIGN 
      ttDoc.code = code.code
      ttDoc.val  = mAvtoSrok
   .
END.   
   
FOR EACH person   
NO-LOCK:
   FOR EACH ttDoc NO-LOCK:
      FOR EACH cust-ident USE-INDEX cust
                       WHERE cust-ident.cust-cat        EQ "�"
                         AND cust-ident.cust-id         EQ person.person-id
                         AND cust-ident.class-code      EQ "p-cust-ident"
                         AND cust-ident.cust-code-type  EQ ttDoc.code
                         AND cust-ident.close-date      EQ ?
      NO-LOCK BY cust-ident.open-date DESCENDING:
         LEAVE.
      END.   
      mAvtoSrok = ttDoc.val.
      
      IF NOT AVAIL cust-ident THEN NEXT.
      
      /* � ������ �� ��⠭������ ��� ஦����� */
      IF mAvtoSrok BEGINS "�" AND person.birthday EQ ? THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","1",
         STRING(mIdStr,">>>>>>>>>9")            + " " +
         STRING(person.person-id,">>>>>>>>>>9") + " " +      
         STRING(ttDoc.code,"x(15)")              + " " +
         "**�訡��**"                           + "    " + 
         "�� ��⠭������ ��� ஦�����").            
         NEXT.
      END.

      mEndDate = DATE(GetXattrValueEx("cust-ident",{&CustIdntSurr},"end-date","")).
      mNewDate = CalcSrokForDocument(mAvtoSrok,cust-ident.open-date,person.birthday).

      IF mNewDate EQ ? THEN DO:
         RUN Fill-SysMes IN h_tmess ("","","1",
         STRING(mIdStr,">>>>>>>>>9")            + " " +
         STRING(person.person-id,">>>>>>>>>>9") + " " +      
         STRING(ttDoc.code,"x(15)")              + " " +
         "**�訡��**"                           + "    " + 
         "�訡�� ���� ���� ����砭�� ���㬥��").            
         NEXT.
      END.
            
      IF mEndDate NE ? AND mNewDate EQ mEndDate THEN NEXT. 
      
      IF mEndDate NE ? AND mNewDate NE mEndDate AND mUpdate THEN DO:
         /* ��� ����砭�� ���㬥�� ᪮�४�஢��� */
         mFlOk = UpdateSigns("cust-ident", 
                             {&CustIdntSurr},
                             "end-date",
                             STRING(mNewDate),
                             yes).
         mVarMess = "᪮�४�஢��".
      END.
      ELSE IF mEndDate NE ? AND mNewDate NE mEndDate AND NOT mUpdate THEN DO:
         /* ��� ����砭�� ���㬥�� �ॡ�� ���४�஢�� */
         
         mVarMess = "�ॡ�� ���४�஢��".
      END.
      ELSE IF mEndDate EQ ? THEN DO:
         mFlOk = UpdateSigns("cust-ident", 
                             {&CustIdntSurr},
                             "end-date",
                             STRING(mNewDate),
                             yes).
         mVarMess = "��⠭������ ��� ����砭��". 
         IF mNewDate <= TODAY THEN 
         mVarMess = mVarMess + ". ���㬥�� ����祭 ".          
      END.

      RUN Fill-SysMes IN h_tmess ("","","1",
      STRING(mIdStr,">>>>>>>>>9")            + " " +
      STRING(person.person-id,">>>>>>>>>>9")  + " " +      
      STRING(ttDoc.code,"x(15)")                      + " " +
      STRING(mNewDate,"99/99/9999")          + "    " +
      mVarMess).      
      mIdStr = mIdStr + 1.
   END.
END.

/* �����襭�� ����� ��⮪���஢����. */
RUN End-SysMes IN h_tmess.


