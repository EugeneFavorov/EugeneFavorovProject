{globals.i}
{tmprecid.def}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

/*
DEFINE VARIABLE m       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE m       AS CHARACTER   NO-UNDO.
*/
DEFINE VARIABLE i       AS INT64     NO-UNDO.
DEFINE VARIABLE n       AS INT64     NO-UNDO.

DEFINE TEMP-TABLE tt-mess
   FIELD op          AS INT64
   FIELD doc-num     AS CHARACTER
   FIELD amt-rub     AS DEC
   FIELD op-acct     AS CHARACTER
   FIELD op-name     AS CHARACTER
   FIELD op-inn      AS CHARACTER
   FIELD bis-name    AS CHARACTER
   FIELD bis-name-k  AS CHARACTER
   FIELD bis-inn     AS CHARACTER
   FIELD err-mess    AS CHARACTER
.

DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.


output to "op.txt".
FOR EACH tmprecid NO-LOCK, 
   FIRST op WHERE
      RECID(op) EQ tmprecid.id 
   NO-LOCK:
   FIND FIRST op-entry of op NO-LOCK NO-ERROR.
   create tt-mess.
   assign
      tt-mess.op      = op.op
      tt-mess.doc-num = op.doc-num
      tt-mess.amt-rub = op-entry.amt-rub
      tt-mess.op-acct = GetXattrValueEx("op",STRING(Op.op),"acct-rec",?).
      tt-mess.op-name = GetXattrValueEx("op",STRING(Op.op),"name-rec",?).
      tt-mess.op-inn  = GetXattrValueEx("op",STRING(Op.op),"inn-rec",?).
   .
   FOR EACH PackObject WHERE (PackObject.file-name EQ     "op-entry" AND 
                              PackObject.Surrogate BEGINS STRING(op.op) + ",")
                          OR (PackObject.file-name EQ     "op" AND
                              PackObject.Surrogate EQ     STRING(op.op))
      NO-LOCK:
      FIND FIRST Packet OF PackObject NO-LOCK.
      n = num-entries(PackError).
      do i = 1 to n :
         find first xattr WHERE  xattr.class-code EQ ClassError 
                            and  xattr.Xattr-Code eq entry(i,PackError)
                    NO-LOCK no-error.
         if avail xattr and xattr.Initial = "Ошибка" then do:
            tt-mess.err-mess = tt-mess.err-mess + xattr.Name + "~n".
         end.                 
      end.
   END.
   find first acct where acct.acct begins tt-mess.op-acct NO-LOCK NO-ERROR. 
   if avail acct then do:
      if acct.cust-cat = "Ю" then do:
         find first cust-corp where cust-corp.cust-id = acct.cust-id NO-LOCK NO-ERROR.
         tt-mess.bis-name   = cust-corp.name-corp.
         tt-mess.bis-name-k = cust-corp.name-short.
         tt-mess.bis-inn    = cust-corp.inn.
      end.
      if acct.cust-cat = "Ч" then do:
         find first person where person.person-id = acct.cust-id NO-LOCK NO-ERROR.
         tt-mess.bis-name   = person.name-last + " " + person.first-names.
         tt-mess.bis-name-k = "".
         tt-mess.bis-inn    = person.inn.
      end.
   end.
   export tt-mess.
END.
output close.

cFl = "./tz15.xml".
OUTPUT TO VALUE(cFl).


PUT UNFORMATTED XLHead("tmp", "CNCCCCCCC", "90,102,160,190,90,190,190,90,200").
PUT UNFORMATTED XLRow(0) XLCellHat("Отчет",8) XLRowEnd().


cXL = XLCellHead("Номер",0,1,0)
    + XLCellHead("Сумма",0,1,0)
    + XLCellHead("Данные получателя из ПП",0,0,2)
    + XLCellHead("Данные клиента по номеру счета из БИСКВИТА",0,0,2)
    + XLCellHead("Сообщения об ошибках обмена",0,1,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("Счет",3,0,0)
    + XLCellHead("Наименование",0,0,0)
    + XLCellHead("ИНН",0,0,0)
    + XLCellHead("Наим-ние П",0,0,0)
    + XLCellHead("Наим-ние К",0,0,0)
    + XLCellHead("ИНН",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
cXL = XLCellHead("1",0,0,0)
    + XLCellHead("2",0,0,0)
    + XLCellHead("3",0,0,0)
    + XLCellHead("4",0,0,0)
    + XLCellHead("5",0,0,0)
    + XLCellHead("6",0,0,0)
    + XLCellHead("7",0,0,0)
    + XLCellHead("8",0,0,0)
    + XLCellHead("9",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH tt-mess NO-LOCK.
   cXL = XLCell(string(tt-mess.doc-num))
       + XLNumCell(dec(tt-mess.amt-rub))
       + XLCell(STRING(tt-mess.op-acct))
       + XLCellWrap(STRING(tt-mess.op-name))
       + XLCell(STRING(tt-mess.op-inn))
       + XLCellWrap(STRING(tt-mess.bis-name))
       + XLCellWrap(STRING(tt-mess.bis-name-k))
       + XLCell(STRING(tt-mess.bis-inn))
       + XLCellWrap(STRING(tt-mess.err-mess))
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.


PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.

/*
   RUN pb_mail.p ("v.ignatchenko", "TZ15 Report", "TZ15FM Report", cFl).
*/
   RUN sndbispc ("file=" + cFl + ";class=bq").



