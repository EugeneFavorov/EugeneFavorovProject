/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: crttransz.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 18.04.2008 12:11 fEAk    
     Modified: 18.04.2008 12:11 fEAk     <comment>
*/

/* ���᮪ �㭪権, ����� ���� ��ࠡ��뢠���� ������ ��楤�ன */                                                       
vOtherTags = "docnumber,face,acctdb,acctcr,client_f,client_f_cr,client_f_inn,client_f_inn_cr,client_f_birthday,client_f_doc,client_f_docdate,client_f_docnum,client_f_docissue,client_f_dockp,client_f_adr,client_f_tel".
vOtherTags = vOtherTags + "," + "amt-rub,amt-rub-str,bik,op-details,op-inn,kpp-rec,name-ben,ben-acct,corr-acct,bank-name,bank-town,op-date".
vOtherTags = vOtherTags + "," + "acct-db,amt-db,amt-db-str,acct-cr,amt-cr,amt-cr-str,client_cr,conv_bank,conv_bik,conv_ks".
vOtherTags = vOtherTags + "," + "kbk,okato-nalog,osn_plat,nalog_per,num_doc,date_doc,type_pay,uin".
vOtherTags = vOtherTags + "," + "client_f_BOS,client_f_birthday_BOS,client_f_doc_BOS,client_f_docnum_BOS,client_f_docissue_BOS,client_f_docdate_BOS,client_f_adr_BOS".

/* Support functions */


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
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   find first op where op.op eq iRid no-lock no-error.
	if avail op then
   ASSIGN oRes = op.doc-num.               

END PROCEDURE.

PROCEDURE face:
   DEF INPUT PARAM iRid AS RECID NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   ASSIGN oRes = FGetSetting("�������",?,?) + " " + FGetSetting("�����",?,?).             
               
END PROCEDURE.

PROCEDURE acctdb:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock:
	   ASSIGN oRes = String(op-entry.acct-db,"x(20)").
   end.                                                   
END PROCEDURE.

PROCEDURE acctcr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock:
	   ASSIGN oRes = IF op-entry.acct-cr BEGINS "3" THEN "" 
                      ELSE String(op-entry.acct-cr,"x(20)").
   end.                                                   
END PROCEDURE.

PROCEDURE client_f:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
	   ASSIGN oRes = person.name-last + ' ' + person.first-names.
   end.

END PROCEDURE.

PROCEDURE client_f_cr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-cr no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
	   ASSIGN oRes = person.name-last + ' ' + person.first-names.
   end.

END PROCEDURE.

PROCEDURE client_f_birthday:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db and acct.cust-cat eq '�' no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
     ASSIGN oRes = string(person.birthday,"99.99.9999").
   end.
   IF oRes EQ "" THEN
       oRes = REPLACE(GetXAttrValueEx("op",STRING(op.op), "Birthday", ""), "/", ".").
   IF oRes NE "" THEN
       oRes = oRes + ' �.'. 
   
END PROCEDURE.

PROCEDURE client_f_inn:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
     ASSIGN oRes = person.inn.
   end.

END PROCEDURE.

PROCEDURE client_f_inn_cr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-cr no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
     ASSIGN oRes = person.inn.
   end.

END PROCEDURE.

PROCEDURE client_f_doc:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first cust-ident where cust-ident.class-code eq 'p-cust-ident' 
         and cust-ident.cust-id eq acct.cust-id
         and cust-ident.close-date eq ? 
         and cust-ident.cust-cat eq '�' no-lock,
       first code where code.class eq '�������' and code.code eq cust-ident.cust-code-type no-lock:
     ASSIGN oRes = code.name.
   end.
     
END PROCEDURE.

PROCEDURE client_f_docnum:
  DEF VAR tStr AS CHAR NO-UNDO.
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first cust-ident where cust-ident.class-code eq 'p-cust-ident' 
         and cust-ident.cust-id eq acct.cust-id 
         and cust-ident.close-date eq ? 
         and cust-ident.cust-cat eq '�' no-lock:

     ASSIGN oRes = cust-ident.cust-code.
   end.
      
END PROCEDURE.

PROCEDURE client_f_docissue:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first cust-ident where cust-ident.class-code eq 'p-cust-ident' 
         and cust-ident.cust-id eq acct.cust-id 
         and cust-ident.close-date eq ? 
         and cust-ident.cust-cat eq '�' no-lock:
     ASSIGN oRes = cust-ident.issue + ', ' + STRING(cust-ident.open-date, "99.99.9999") + ' �.'.

   end.

END PROCEDURE.

/* ⥣� ��� ������ ��� ������ ��� */
PROCEDURE client_f_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   FIND FIRST op WHERE op.op-transaction EQ tStr 
                 NO-LOCK.
   ASSIGN oRes = op.name-ben.
END PROCEDURE.

PROCEDURE client_f_birthday_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
   NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   FIND FIRST op WHERE op.op-transaction EQ tStr 
   NO-LOCK.
   ASSIGN 
      oRes = REPLACE(GetXAttrValueEx("op",STRING(op.op), "Birthday", ""), "/", ".")
   . 
   IF oRes NE "" THEN
      oRes = oRes + ' �.'.
END PROCEDURE.

PROCEDURE client_f_doc_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   FIND FIRST op WHERE op.op-transaction EQ tStr 
                 NO-LOCK.
   ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "document-id", "").
     
END PROCEDURE.

PROCEDURE client_f_docnum_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF VAR    tRes        AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   
   FIND FIRST op WHERE op.op-transaction EQ tStr 
                 NO-LOCK.
   ASSIGN tRes = GetXAttrValueEx ("op",STRING(op.op), "����", "").
   
   IF tRes NE "" THEN
   DO:
     IF CAN-DO ("*뤠�*", tRes) THEN 
        ASSIGN oRes = ENTRY(1,tRes,"�뤠�").
     ELSE IF NUM-ENTRIES(tRes) > 1 THEN 
        ASSIGN oRes = ENTRY(1,tRes).
     ELSE ASSIGN oRes = SUBSTRING(tRes,1,12).
   END.
   
END PROCEDURE.

PROCEDURE client_f_docissue_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF VAR    tRes        AS CHARACTER NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   
   FIND FIRST op WHERE op.op-transaction EQ tStr 
                 NO-LOCK.
   ASSIGN tRes = GetXAttrValueEx ("op",STRING(op.op), "����", "").

   IF tRes NE "" THEN
   DO:
     tRes = CAPS(tRes).
     IF NUM-ENTRIES(tRes) = 3 THEN
        ASSIGN oRes = ENTRY(2,tRes) + ', ' + ENTRY(3,tRes).
     ELSE IF NUM-ENTRIES(tRes) = 2 THEN
        ASSIGN oRes = ENTRY(2,tRes).
      ELSE IF CAN-DO ("*�����*", tRes) THEN
         ASSIGN oRes = REPLACE(tRes,"�����",""). 
   END. 
   
   IF oRes EQ "" THEN ASSIGN oRes = LEFT-TRIM(tRes, "1234567890 ").
END PROCEDURE.

PROCEDURE client_f_docdate_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
   FIND FIRST op WHERE op.op-transaction EQ tStr 
                 NO-LOCK.
   ASSIGN 
      oRes = REPLACE(GetXAttrValueEx ("op",STRING(op.op), "Document4Date_vid", ""), "/", ".")
   .
   IF oRes NE "" THEN
      oRes = oRes + ' �.'.  
END PROCEDURE.

PROCEDURE client_f_adr_BOS:
   DEF VAR    tStr        AS INT64 NO-UNDO.
   DEF INPUT  PARAM iRid  AS INT64 NO-UNDO.
   DEF INPUT  PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes  AS CHARACTER NO-UNDO.

   FIND FIRST op WHERE op.op EQ iRid 
                 NO-LOCK.
   ASSIGN tStr = op.op-transaction.
     FIND FIRST op WHERE op.op-transaction EQ tStr 
                   NO-LOCK.
     ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "����", "").

END PROCEDURE.
/*----------------------------------------------------------------------------------*/


PROCEDURE client_f_docdate:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first cust-ident where cust-ident.class-code eq 'p-cust-ident' 
         and cust-ident.cust-id eq acct.cust-id 
		     and cust-ident.close-date eq ?
         and cust-ident.cust-cat eq '�' no-lock:

	   ASSIGN oRes = string(cust-ident.open-date,"99.99.9999").

   end.

END PROCEDURE.

PROCEDURE client_f_dockp:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first cust-ident where cust-ident.class-code eq 'p-cust-ident' 
         and cust-ident.cust-id eq acct.cust-id 
		     and cust-ident.close-date eq ?
         and cust-ident.cust-cat eq '�' no-lock,
      first signs where signs.file-name eq 'cust-ident' and signs.surrogate begins string(cust-ident.cust-code-type + ',' + cust-ident.cust-code) 
		          and signs.code eq '���ࠧ�' no-lock:

           ASSIGN oRes = signs.code-value.

   end.

END PROCEDURE.

PROCEDURE client_f_adr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
 
	   ASSIGN oRes = person.address[1] + ' ' + person.address[2]
                  oRes = REPLACE(oRes,",,",",")
                  oRes = TRIM(oRes,",").

   end.

END PROCEDURE.

PROCEDURE client_f_tel:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
 
	   ASSIGN oRes = person.phone[1] + ' ' + person.phone[2].

   end.

END PROCEDURE.

PROCEDURE amt-rub:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock:
	   ASSIGN oRes = String(op-entry.amt-rub,">>>>>>>>>>>9.99").
   end.                                                   
END PROCEDURE.

PROCEDURE amt-rub-str:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   def var tmpstr as char extent 2 no-undo.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock:
	run x-amtstr.p(op-entry.amt-rub,acct.currency,true,true,
                  output tmpstr[1], output tmpstr[2]).
	   ASSIGN oRes = tmpstr[1] + ' ' + tmpstr[2].
   end.                                                   
END PROCEDURE.

PROCEDURE acct-db:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock:
     ASSIGN oRes = DelFilFromAcct(op-entry.acct-db).
     end.                                                   
END PROCEDURE.

PROCEDURE amt-db:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock,
       first currency where currency.currency eq acct.currency no-lock:
      IF acct.currency EQ "" THEN  
         ASSIGN oRes = String(op-entry.amt-rub,">>>>>>>>>>>9.99").
      ELSE ASSIGN oRes = String(op-entry.amt-cur,">>>>>>>>>>>9.99").
      oRes = oRes + " " + currency.curr-form5.
   end.
END PROCEDURE.

PROCEDURE amt-db-str:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   def var tmpstr as char extent 2 no-undo.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-db no-lock:
   IF acct.currency EQ "" THEN
      run x-amtstr.p(op-entry.amt-rub,acct.currency,true,true,
                     output tmpstr[1], output tmpstr[2]).
   ELSE run x-amtstr.p(op-entry.amt-cur,acct.currency,true,true,
                       output tmpstr[1], output tmpstr[2]).
     ASSIGN oRes = tmpstr[1] + ' ' + tmpstr[2].
     end.                                                   
END PROCEDURE.

PROCEDURE acct-cr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock:
     ASSIGN oRes = DelFilFromAcct(op-entry.acct-cr).
     end.                                                   
END PROCEDURE.

PROCEDURE amt-cr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-cr no-lock,
       first currency where currency.currency eq acct.currency no-lock:
      IF acct.currency EQ "" THEN  
         ASSIGN oRes = String(op-entry.amt-rub,">>>>>>>>>>>9.99").
      ELSE ASSIGN oRes = String(op-entry.amt-cur,">>>>>>>>>>>9.99").
      oRes = oRes + " " + currency.curr-form5.
   end.
END PROCEDURE.

PROCEDURE amt-cr-str:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   def var tmpstr as char extent 2 no-undo.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-cr no-lock:
   IF acct.currency EQ "" THEN
      run x-amtstr.p(op-entry.amt-rub,acct.currency,true,true,
                     output tmpstr[1], output tmpstr[2]).
   ELSE run x-amtstr.p(op-entry.amt-cur,acct.currency,true,true,
                       output tmpstr[1], output tmpstr[2]).
     ASSIGN oRes = tmpstr[1] + ' ' + tmpstr[2].
     end.                                                   
END PROCEDURE.

PROCEDURE client_cr:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
       first op-entry where op-entry.op eq op.op no-lock,
       first acct where acct.acct eq op-entry.acct-cr no-lock,
       first person where person.person-id eq acct.cust-id no-lock:
      ASSIGN oRes = person.name-last + ' ' + person.first-names.
   end.

END PROCEDURE.

PROCEDURE conv_bank:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
      ASSIGN oRes = GetXAttrValueEx("branch", op.filial-id, "dps_bank", "").
   end.
END PROCEDURE.

PROCEDURE conv_bik:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
      ASSIGN oRes = GetXAttrValueEx("branch", op.filial-id, "�������", "").
   end.
END PROCEDURE.

PROCEDURE conv_ks:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
      ASSIGN oRes = GetXAttrValueEx("branch", op.filial-id, "�����", "").
   end.
END PROCEDURE.

PROCEDURE name-ben:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
	  ASSIGN oRes = op.name-ben.
   end.                                                   
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN client_f_cr(iRid,iDate, OUTPUT oRes).
END PROCEDURE.

PROCEDURE ben-acct:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
	  ASSIGN oRes = op.ben-acct.
   end.                                                   
END PROCEDURE.

PROCEDURE op-inn:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
	  ASSIGN oRes = op.inn.
   end.                                                   
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN client_f_inn_cr(iRid,iDate, OUTPUT oRes).
END PROCEDURE.


PROCEDURE op-details:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = REPLACE(op.details, CHR(10), ' ').
   end.                                                   
END PROCEDURE.

PROCEDURE bik:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
  first op-bank of op no-lock:
    ASSIGN oRes = op-bank.bank-code.
   end.                                                   
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN bik_cr(iRid,iDate, OUTPUT oRes).
END PROCEDURE.

PROCEDURE corr-acct:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
  first op-bank of op no-lock:
    ASSIGN 
      oRes = op-bank.corr-acct.
   end.                                                   
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN korsch_cr(iRid,iDate, OUTPUT oRes).
END PROCEDURE.

PROCEDURE bank-town:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
  first op-bank of op no-lock,
  first banks-code where banks-code.bank-code eq op-bank.bank-code and op-bank.bank-code-type eq banks-code.bank-code-type no-lock,
  first banks of banks-code no-lock:
    ASSIGN oRes = banks.town.
   end.
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN cityname_cr(iRid,iDate, OUTPUT oRes).
END PROCEDURE.

PROCEDURE bank-name:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock,
  first op-bank of op no-lock,
  first banks-code where banks-code.bank-code eq op-bank.bank-code and op-bank.bank-code-type eq banks-code.bank-code-type no-lock,
  first banks of banks-code no-lock:

    ASSIGN oRes = banks.name.
   end.                                                   
   IF oRes EQ ? OR TRIM(oRes) EQ "" THEN 
      RUN namefil(iRid,iDate, OUTPUT oRes).
   
END PROCEDURE.

PROCEDURE op-date:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = string(op.op-date,"99.99.9999").
   end.                            
                       
END PROCEDURE.

PROCEDURE kpp-rec:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
	  ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "Kpp-rec", "").
   end.                                                   
END PROCEDURE.

PROCEDURE kbk:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "���", "").
   end.                                                   
END PROCEDURE.

PROCEDURE okato-nalog:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����-�����", "").
   end.                                                   
END PROCEDURE.

PROCEDURE osn_plat:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����", "").
   end.                                                   
END PROCEDURE.

PROCEDURE nalog_per:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����", "").     
   end.
   oRes = REPLACE(STRING(oRes), ".", "").                                                   
END PROCEDURE.

PROCEDURE num_doc:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����", "").
   end.                                                   
END PROCEDURE.

PROCEDURE date_doc:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����", "").
   end.                                                   
END PROCEDURE.

PROCEDURE type_pay:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "�����", "").
   end.                                                   
END PROCEDURE.

PROCEDURE uin:
   DEF INPUT PARAM iRid AS INT64 NO-UNDO.
   DEF INPUT PARAM iDate AS DATE NO-UNDO.
   DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.

   for first op where op.op eq iRid no-lock:
    ASSIGN oRes = GetXAttrValueEx ("op",STRING(op.op), "���", "").
   end.                                                   
END PROCEDURE.


PROCEDURE bik_cr:
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
        oRes = '' + GetXAttrValueEx("branch",
                                        STRING(branch.branch-id),
                                        "�������",
                                        "").
        ELSE oRes = '' + GetXAttrValueEx("branch",
                                             STRING(branch.parent-id),
                                             "�������",
                                             "").
      END.
    ELSE
      LEAVE.
  END.             
END PROCEDURE.

/*������ + ��� ����� ������*/
PROCEDURE korsch_cr:
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
            oRes = '' + GetXAttrValueEx("branch",
                                              STRING(branch.branch-id),
                                              "�����",
                                              "").
/*
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.branch-id),
                                                "��爧�������",
                                                "").
*/
          END. 
        ELSE 
          DO:
            oRes = '' + GetXAttrValueEx("branch",
                                              STRING(branch.parent-id),
                                              "�����",
                                              "").
/*
            oRes = oRes + ' ' + GetXAttrValueEx("branch",
                                                STRING(branch.parent-id),
                                                "��爧�������",
                                                "").
*/
          END.
      END.  
    ELSE
      LEAVE.
  END.
END PROCEDURE.

PROCEDURE cityName_cr:
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
        IF tmpVar EQ "" THEN
          tmpVar = GetXAttrValueEx("branch",
                                   STRING(branch.parent-id),
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

PROCEDURE nameFil:
  DEF INPUT PARAM iRid  AS RECID NO-UNDO.
  DEF INPUT PARAM iDate AS DATE NO-UNDO.
  DEF OUTPUT PARAM oRes AS CHARACTER NO-UNDO.
  DEF VAR UserOtd       AS CHARACTER NO-UNDO.

  DO:
    UserOtd = GetXAttrValueEx("_user",USERID("bisquit"),"filial-id","").
    FIND FIRST branch WHERE
      branch.branch-id EQ UserOtd
      NO-LOCK NO-ERROR.
    IF AVAILABLE(branch) THEN
      oRes = branch.name.

      /* DO:
        IF CAN-DO("11", branch.branch-type) THEN
          oRes = GetXAttrValueEx("branch",
                                 STRING(branch.branch-id),
                                 "dps_bank",
                                 "").
        ELSE oRes = GetXAttrValueEx("branch",
                                    STRING(branch.parent-id),
                                    "dps_bank",
                                    "").
      END. */
    ELSE
      LEAVE.
  END.
END PROCEDURE.


/*    */
