/*
   Проверочный отчет
pda
*/

{globals.i}
{tmprecid.def}
{sh-defs.i}
{ksh-defs.i NEW}
{prn-doc.def &with_proc=YES}
{intrface.get xclass}
{intrface.get date}

DEFINE TEMP-TABLE otch
   FIELD acctdb    AS CHAR                 /* */
   FIELD acctcr    AS CHAR                 /* */
   FIELD opdetails AS CHAR                 /* */
.
{empty otch}

DEF VAR in-name   AS CHAR FORMAT "x(35)" EXTENT 3 NO-UNDO.
DEF VAR vXattrVal AS CHAR NO-UNDO.
DEF VAR vTplName  AS CHAR NO-UNDO.
DEF VAR vTmpStr   AS CHAR NO-UNDO.
DEF VAR vAmtDec   AS DEC  NO-UNDO.
DEF VAR c1  AS CHAR  NO-UNDO.
DEF VAR c2  AS CHAR  NO-UNDO.

/* Функция, прибавляет заданное количество рабочих дней к указанной дате */
FUNCTION AddWorkday RETURN DATE (INPUT vDateIn AS DATE,INPUT amtWorkDay AS INT64).
   DEF VAR vDate AS DATE  NO-UNDO.
   DEF VAR i     AS INT64 NO-UNDO.
   
   vDate = vDateIn.
   DO i = 1 TO amtWorkDay:
      vDate = vDate + 1. 
      IF HOLIDAYRU(vDate) OR CAN-DO("1,7",STRING(WEEKDAY(vDate))) THEN i = i - 1.
      /* MESSAGE i skip STRING(vDate) skip HOLIDAY(vDate) VIEW-AS ALERT-BOX. */
   END.
   RETURN vDate.
END FUNCTION.

{sign_select.i} /*Определение подписантов*/
/* {setdest.i &file-name = "111.log"} */
/* По отмеченным */
FOR EACH tmprecid NO-LOCK,
	FIRST op WHERE RECID(op) EQ tmprecid.id 
NO-LOCK:

   RUN Insert_TTName("doc-num",STRING(op.doc-num)).               /*номер документа*/
   RUN Insert_TTName("doc-date",STRING(op.op-date,"99.99.9999")). /*дата зачисления*/
   RUN Insert_TTName("doc-date15",STRING(AddWorkday(op.op-date,15),"99.99.9999")). /* +15 дней к дате зачисления*/

   FIND FIRST branch WHERE 
              branch.Branch-Id EQ op.filial-id 
   NO-LOCK NO-ERROR.
   IF AVAIL branch THEN 
   DO:
      vTmpStr = GetXattrValueEx("branch",STRING(branch.Branch-Id),"БанкНаимПП","").
      RUN Insert_TTName("bank-name-pp",vTmpStr).                     /*имя филиала в преп. падеже*/
   END.

   /*инн получателя*/
  /*  vTmpStr = TRIM(GetXattrValueEx("op",
                                  STRING(op.op),
                                  "inn-rec",
                                  "not/avail")).
   FIND FIRST cust-corp WHERE
              cust-corp.inn EQ vTmpStr
   NO-LOCK NO-ERROR.
   IF AVAIL cust-corp THEN 
   DO:
      RUN RetAdr.p(cust-corp.cust-id,"Ю","АдрЮр",?,OUTPUT vTmpStr).
      RUN Insert_TTName("AdrUr",vTmpStr). 
   END.
   ELSE DO: 
      FIND FIRST op-entry WHERE 
                 op-entry.op EQ op.op NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) THEN
      DO:
         FIND FIRST acct WHERE 
                    acct.acct EQ op-entry.acct-cr
         NO-LOCK NO-ERROR.
      
         RUN GetCustName IN h_base(acct.cust-cat,
                                   acct.cust-id,
                                   ?,
                                   OUTPUT in-name[1],
                                   OUTPUT in-name[2],
                                   INPUT-OUTPUT in-name[3]).
         MESSAGE in-name[3] VIEW-AS ALERT-BOX.
      END.
   END. */

   /*=============== Доп. реквизиты документа ================*/
   FOR EACH xattr 
      WHERE xattr.class-code EQ "op"
      AND NOT xattr.progress-field
   NO-LOCK:
      IF AVAIL xattr AND NOT CAN-DO("inn-rec,acct-rec,name-rec",xattr.xattr-code) THEN
      DO:   
         vXattrVal = GetXattrValueEx("op",
                                     STRING(op.op),
                                     xattr.xattr-code,
                                     "").
         RUN Insert_TTName(xattr.xattr-code,vXattrVal).
      END.
   END.

   FOR EACH op-entry 
      WHERE op-entry.op EQ op.op 
   NO-LOCK:
      IF AVAIL op-entry THEN
      DO:
         FIND FIRST acct WHERE 
                    acct.acct EQ op-entry.acct-cr
         NO-LOCK NO-ERROR.
         IF AVAIL(acct) THEN
         DO:
            RUN Insert_TTName("acct-rec",DelFilFromAcct(op-entry.acct-cr)). 
            
            /*имя, инн*/
            RUN GetCustName IN h_base(acct.cust-cat,
                                      acct.cust-id,
                                      ?,
                                      OUTPUT in-name[1],
                                      OUTPUT in-name[2],
                                      INPUT-OUTPUT in-name[3]).
            IF acct.cust-cat EQ "Ч" AND GetXAttrValueEx("person",STRING(acct.cust-id),"Субъект","") EQ "ФЛП" THEN
               in-name[1] = "ИП " + in-name[1]. /*только для ИП*/
            RUN Insert_TTName("name-rec",in-name[1] + " " + in-name[2]). 
            RUN Insert_TTName("inn-rec",in-name[3]). 
            
            /*заполняем адрес*/
            CASE acct.cust-cat: 
            WHEN "Ч" THEN DO:
               RUN RetAdr.p(acct.cust-id,acct.cust-cat,"АдрФакт",?,OUTPUT vTmpStr).
               IF TRIM(vTmpStr) EQ "" THEN
                  RUN RetAdr.p(acct.cust-id,acct.cust-cat,"АдрПроп",?,OUTPUT vTmpStr).
            END.
            WHEN "Ю" THEN
               RUN RetAdr.p(acct.cust-id,acct.cust-cat,"АдрЮр",?,OUTPUT vTmpStr).
            END CASE.
            
            RUN Insert_TTName("AdrUr",vTmpStr). 
         END.
         
         IF op-entry.currency EQ "" THEN vAmtDec = op-entry.amt-rub. 
            ELSE vAmtDec = op-entry.amt-cur. 
         
         /*сумма прописью*/
         RUN "x-amtstr.p" (vAmtDec,
                           op-entry.currency,
                           YES,
                           YES,
                           OUTPUT c1,OUTPUT c2).
         RUN Insert_TTName("amtstr",c1 + " " + c2).
         
         /*сумма цифрами*/
         RUN Insert_TTName("amtdec",STRING(vAmtDec,"->>>>>>>>>9.99")).
      END.
   END.
END.

/* {preview.i &file-name = "111.log"} */
vTplName = GetXAttrValueEx ("user-proc",
                            STRING(user-proc.public-number),
                            "ВидДок",
                            "").

RUN printvd.p (vTplName,INPUT TABLE ttnames).