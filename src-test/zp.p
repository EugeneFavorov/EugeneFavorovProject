/*
    Импорт документов из 1С.
*/

/* Проверяет, является ли строка - числом */
FUNCTION IsMyNumber LOGICAL (iStr AS CHAR):
   DEF VAR vRes AS INT64 NO-UNDO.
   vRes = INT64(iStr + '.0') NO-ERROR. /* Чтобы ругаться на Decimal */
   RETURN ERROR-STATUS:ERROR.
END FUNCTION.

/* Убираем повторяющиеся пробелы */
FUNCTION Remove32 CHAR (iStr AS CHAR):
   DEF VAR cRes AS CHAR NO-UNDO.
   cRes = TRIM(REPLACE(REPLACE(REPLACE(iStr,CHR(9),' '),'╨',''),'  ',' ')).
   DEF VAR iCount AS INT NO-UNDO.
   iCount = 4.
    DO WHILE iCount > 0:
        cRes = TRIM(REPLACE(cRes,'  ',' ')).
        iCount = iCount - 1.
    END.
   RETURN cRes.
END FUNCTION.

/* Убираем все пробелы */
FUNCTION FullRemove32 CHAR (iStr AS CHAR):
   DEF VAR cRes AS CHAR NO-UNDO.
   cRes = TRIM(REPLACE(REPLACE(iStr,' ',''),' ','')).
   DEF VAR iCount AS INT NO-UNDO.
   iCount = 4.
    DO WHILE iCount > 0:
        cRes = TRIM(REPLACE(cRes,' ','')).
        iCount = iCount - 1.
    END.
   RETURN cRes.
END FUNCTION.

{imp-mci.def}                          /* определения               */

    def var vFilial as char no-undo.
    
  /*  vFilialSob = "@" + vFilial. */
 

def var counter as INT64 no-undo.
def var first-loop as logical init yes no-undo.
def var num-esid as INT64 no-undo.
def var flager as INT64 no-undo.
def var buf-tag as char no-undo.
def var buf-det  as char extent 4 no-undo.
def var kind-pay as char no-undo.
def var old-auto as logical no-undo.
def var in-acct  as char no-undo.
def var can-modif-pay as char no-undo.
def var result   as INT64 no-undo.
DEF VAR vBufSet  AS CHAR NO-UNDO.
def var mychar as char no-undo.
def var myNumDoc as char format "x(4)" view-as fill-in no-undo.
def var myAcctDb as char format "x(20)" view-as fill-in no-undo.
def var myDetails as char format "x(40)" view-as EDITOR SIZE 54 BY 3 no-undo.
def var myFullAcctCr as char no-undo.
def var myFullAcctDb as char no-undo.
def var mySumm as decimal no-undo.
def var myNameAcct as char no-undo.
def var myFam as char no-undo.
def var myIm as char no-undo.
def var myOtch as char no-undo.
def var myFullSumm as decimal no-undo.
def var bFlag as logical no-undo.
def var myStatus as char no-undo.
def var myClassCode as char no-undo.
def var myDocType as char no-undo.
def var myAcctCr as char no-undo.
def var myBankBic as char no-undo.
def var myBankName as char no-undo.
def var myBankCorrSend as char no-undo.
def var myInnSend as char no-undo.
def var myInnRec as char no-undo.

DEFINE VARIABLE mDate AS DATE NO-UNDO.





find first op-kind where recid(op-kind) eq in-rec-kind no-lock no-error.
if not avail op-kind then return.
    
find first op-template where op-template.op-kind = op-kind.op-kind no-lock no-error.
if not avail op-template then return.

myAcctDb = substring(op-template.acct-db,1,20).
myAcctCr = op-template.acct-cr.
myDetails = op-template.details.
myStatus = op-template.op-status.
myClassCode = op-template.cr-class-code.
myDocType = op-template.doc-type.
      
find first signs where signs.file-name = 'op-template'
    and signs.surrogate = op-template.op-kind + ',' + string(op-template.op-template)
    and signs.code = 'PARSSEN_inn-rec' no-lock no-error.
if avail signs then myInnRec = signs.xattr-value.
    else myInnRec = ''.
find first signs where signs.file-name = 'op-template'
    and signs.surrogate = op-template.op-kind + ',' + string(op-template.op-template)
    and signs.code = 'PARSSEN_inn-send' no-lock no-error.
if avail signs then myInnSend = signs.xattr-value.
    else myInnSend = ''.       

if num-entries(op-template.bank-op-templ) > 3 then do:
    myBankBic       = entry(3,op-template.bank-op-templ).
    myBankCorrSend  = entry(4,op-template.bank-op-templ).
    FIND FIRST banks-code WHERE banks-code.bank-code = myBankBic AND banks-code.bank-code-type = 'МФО-9' NO-LOCK NO-ERROR.
    IF AVAIL banks-code THEN DO:
        FIND FIRST banks WHERE banks.bank-id = banks-code.bank-id NO-LOCK NO-ERROR.
        IF AVAIL banks THEN myBankName = trim(banks.name).
    END.                                            
    
end.
else do:
    myBankBic = ''.
    myBankName =''.
    myBankCorrSend = ''.
end.
           
myFullSumm = 0.
myNumDoc = '99'.
/*
myAcctDb = '60306810300900000000'.
myDetails = 'Перечисляется '.
*/

def temp-table tt no-undo
  field tag as char
  field tag-value as char.

def stream qq.

{intrface.get xclass}
{intrface.get op}
{exchange.equ}
{intrface.get filex}
{ tmprecid.def }

pause 0.

DEFINE FRAME frame_date_codes

   mDate     LABEL "Дата  документа" FORMAT "99/99/9999"
   myNumDoc  LABEL "Номер документа"
   myAcctDb  LABEL "Счет"
   myDetails LABEL "Назначение"
         
   WITH 1 COL 1 DOWN
WIDTH 78 CENTERED OVERLAY TITLE "Данные документов".

mDate = in-op-date. 

ON LEAVE OF myAcctDb IN FRAME frame_date_codes
DO:
    find first acct where acct.number = myAcctDb no-lock no-error.
    if not avail acct then do:
         MESSAGE "Не найден счет " + myAcctDb:SCREEN-VALUE SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY {&RET-ERROR}.
    END. 
    myFullAcctDb = acct.acct.
END. 


  do ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:

   UPDATE
      mDate myNumDoc myAcctDb myDetails

   WITH FRAME frame_date_codes
   EDITING:
   READKEY.


   if LASTKEY EQ KEYCODE("ESC") THEN
        return.
   if LASTKEY EQ KEYCODE("F1")
                THEN do:
                        CASE FRAME-FIELD:
                        WHEN "myAcctDb" THEN
                                DO:
                                RUN browseld.p ("acct",
                                "bal-acct",
                                "603*",
                                "",
                                4).    
                                if (lastkey eq 13 or
                                lastkey eq 10) and
                                pick-value ne ?
                                then do: 
                                    FOR EACH tmprecid,
                                    FIRST acct WHERE 
                                    RECID(acct) EQ tmprecid.id NO-LOCK:
                                        myAcctDb = acct.number.
                                        display myAcctDb with frame frame_date_codes.
                                        leave.
                                    END.
                                end.
                        END.

                        END CASE.
                        
                end.
                ELSE APPLY LASTKEY.
   end. /* EDITING: */

   end.  /* do on */

   HIDE FRAME frame_date_codes.

    find first acct where acct.number = myAcctDb no-lock no-error.
    if not avail acct then do:
         MESSAGE "Не найден счет " + myAcctDb:SCREEN-VALUE SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
    END. 
    myFullAcctDb = acct.acct.
    myNameAcct = acct.details.
    in-op-date = mDate.

/*   MESSAGE myFullAcctDb ";" myNameAcct ";" in-op-date*/
/*   VIEW-AS ALERT-BOX.                                */
    
imp-loop:
do on endkey undo, leave:
  {justasec}

/* cинтаксический контроль форматой строки */
  DEF VAR mFileMask AS CHAR INIT "zp*.TXT" NO-UNDO.
  DEF VAR mOneMask  AS CHAR INIT "" NO-UNDO.
  DEF VAR mMailUser AS INT64          NO-UNDO.
  DEF VAR mOutMask  AS CHAR INIT "" NO-UNDO.
  DEF VAR mPath     AS CHAR INIT "" NO-UNDO.
  DEF VAR mI        AS INT64          NO-UNDO.
  def var fl as char init "" no-undo.
  DEF VAR mFio      AS CHAR NO-UNDO.
  DEF VAR mAcct     AS CHAR NO-UNDO.
  def var bError as logical no-undo.

  mPath     = "./".
  mPath = '/home2/bis/quit41d/imp-exp/0000/1c/'.
  {debug.i "Чтение каталога " mPath}
  {os-dir.pro}
  os-delete VALUE("filelist.imp").
  RUN read-dir (mPath,
              mFileMask,
              "filelist.imp").
  IF SEARCH("filelist.imp") EQ ? THEN DO:
      MESSAGE "Каталог импорта"                    SKIP
              mPath                                SKIP
              "пуст ! "                            SKIP
              "Показать маску файлов ?"
              VIEW-AS ALERT-BOX QUESTION
              BUTTONS YES-NO SET choice.
   IF choice AND {assigned mFileMask} THEN
      MESSAGE "Маска файлов"                       SKIP
               REPLACE(mFileMask,",","~n")         SKIP
               VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.
/*
  run delete-op.  /* поиск и удаление документов, импортирванных ранее */
  if return-value eq "exit" then return.
*/
 input stream FileListin from "filelist.imp".
 repeat:                                /* цикл по файлам в каталоге */
  import stream FileListin File-Name.
/*message "import stroki 000 in-op-date=" string(in-op-date)  view-as alert-box.*/
  if auto = no then do:
    {setdest.i &stream="stream err"}
/*    put
     stream err unformatted
    "ВОЗНИКЛИ ПРОБЛЕМЫ СО СЛЕДУЮЩИМИ ДОКУМЕНТАМИ: файл: " + file-name + " " +
     string(in-op-date,"99/99/99") + "  " + string(time,"hh:mm:ss") skip.*/
  end.

  input stream imp-str from value(File-Name) convert source '1251'.

  do transaction:

   run do-main.
   if return-value ne "" then undo imp-loop, leave imp-loop.
  end.

  input stream imp-str close.
  
   os-delete value(File-Name). 
  IF bError THEN
    put
    stream err unformatted
    "Документы не загружены из-за ошибок" skip.
  ELSE
  put
    stream err unformatted
	"Загружено " + string(numstr,">>>>>9") + " документов на сумму " + string(myFullSumm, "->>>>>>>>>>>>9.99") skip.

/*  message "Считано " + string(numstr,">>>>>9") + " строк файла." skip
          "Выводить протокол ?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.
  if choice then do:*/
  {preview.i &stream="stream err"  {&*} }
  /*end.*/
  if debug eq 0 then do:
/*        os-delete value(File-Name).*/
        os-delete "./scan.d".
  end.
 end.    /* repeat */
end.


procedure cr-op-entry.
        j = 1.
        for each w-op-entry where w-op.op eq w-op-entry.op:
/*
message "cr-op w-op-entry.acct-cr=" w-op-entry.acct-cr view-as alert-box.
*/
           w-op-entry.op = op.op.
           create op-entry.
/* message "create op-entr" view-as alert-box. */
           {op-entry.i "op-entry" "w-op-entry"}
           /* message 'next' view-as alert-box. */
            assign
			/*  op-entry.filial-id = "0000" */
              op-entry.op         = op.op
              op-entry.op-date    = op.op-date
              op-entry.value-date = op.op-date
              op-entry.user-id    = op.user-id
              op-entry.acct-cat   = op.acct-cat
              op-entry.op-status  = op.op-status
              op-entry.op-cod     = '000000'
			  op-entry.acct-db = w-op-entry.acct-db
			  op-entry.acct-cr = w-op-entry.acct-cr
			  /*op-entry.type = op-template.type*/
              op-entry.op-entry   = j
              j                   = j  + 1.
/*message "cr-op op-entry.acct-db=" op-entry.acct-db view-as alert-box.*/

           if w-op-entry.type eq "" then do:
                op-entry.type = "НЕ".
           end.
/*   &IF DEFINED(ORACLE) &THEN
      RUN saverec.p ((BUFFER op-entry:HANDLE)).
   &ENDIF */
   /* run instview.p(TEMP-TABLE w-op-entry:HANDLE). */
/*message "op-e=6_2 j=" string(j) " op-entry.op = " string(op-entry.op) view-as alert-box.*/

           /*UpdateSigns("op",string(op.op), "inn-send", STRING(w-op.inn-send),mIsIndexINNSend).
             UpdateSigns("op",string(op.op), "inn-rec",  STRING(w-op.inn-rec), mIsIndexINNRec).
             UpdateSigns("op",string(op.op), "name-send",w-op.name-send,mIsIndexNameSend).
             UpdateSigns("op",string(op.op), "name-rec", w-op.name-rec, mIsIndexNameRec).
             UpdateSigns("op",string(op.op), "acct-send",w-op.acct-send,mIsIndexAcctSend).
             UpdateSigns("op",string(op.op), "acct-rec", w-op.acct-rec, mIsIndexAcctRec).*/
             
          /* message 'all' view-as alert-box.
           VALIDATE op NO-ERROR.
          VALIDATE op-entry NO-ERROR.
                 
              DEF VAR i AS INT64 NO-UNDO.
        DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            message 'Ошибка - ' + ERROR-STATUS:GET-MESSAGE(i) view-as alert-box.
        END.
        */  
                /*   release op-entry.   
        message '2' view-as alert-box.
        */
		end.

/*message "op-e=6_3 j=" string(j) " op-entry.op = " string(op-entry.op) view-as alert-box.*/

end procedure.

procedure delete-op.
 flag-del = no.
 for each op where op.op-date = in-op-date and
         op.op-kind eq op-kind.op-kind no-lock:
        flag-del = yes.
        leave.
 end.
 if flag-del then do:
     {message &text="|Уже есть записи, импортированные: "" + string(in-op-date) + "". ||Удалить ? "
              &alert-box=question
              &buttons=YES-NO}.
     if pick-value ne "yes" then return "exit".
 end.
end procedure.

procedure do-main.
bError = false.
DO TRANS ON ERROR   UNDO, LEAVE:
def var flagger as logical no-undo.

 main:
 do on error undo, leave /* with frame error down */ :
  numstr  = 0.

  loop:
  repeat with frame error  down:
  {imp-mci.frm}       /* form */

  import stream imp-str unformatted buf.

  buf = trim(buf).
  if buf eq "" then next loop.

    mychar = substring(buf,1,1).
    if IsMyNumber(mychar)<> FALSE then next loop. 
    buf = Remove32(buf).
    find first acct where acct.number = ENTRY(4,buf,' ') no-lock no-error.

    if not avail acct then do:
       /*         message 'Не найден счет ' + ENTRY(4,buf,' ') view-as alert-box. */
         /* display "Не найден счет " + ENTRY(4,buf,' ') skip with frame mess-str overlay centered row 14 no-label  . */
        put stream err unformatted
                "Не найден счет " + ENTRY(4,buf,' ') skip.
        bError = true.
        next loop.
    END. 
    
    FIND FIRST PERSON
    WHERE PERSON.PERSON-ID = acct.cust-id NO-LOCK NO-ERROR.
if avail person then do:
    myFam = trim(person.Name-last).
    myIm = trim(person.first-names).
    bFlag = false.
    if myFam <> ENTRY(2,buf,' ') then bFlag = true.
    if substring(myIm,1,1) <> substring(ENTRY(3,buf,' '),1,1) then bFlag = true.
    if substring(entry(2,ENTRY(3,buf,' '),'.'),1,1) <> '' 
        and substring(entry(2,myIm,' '),1,1) <> ''
        then do:
        if substring(entry(2,myIm,' '),1,1) <> substring(entry(2,ENTRY(3,buf,' '),'.'),1,1) then bFlag = true.
    end.
    if bFlag then do:    
        put stream err unformatted "Есть разница в ФИО " + ENTRY(2,buf,' ') + ' ' + ENTRY(3,buf,' ') +  ' и ' + myFam + ' ' + myIm skip.
        bError = true.
        next loop.  
    end.    
end.
else do:
     put stream err unformatted "Не найден клиент для счета " + ENTRY(4,buf,' ') skip.
     bError = true.
     next loop.
end. 
    
    myFullAcctCr = acct.acct.

def var err-class    as char initial 'mess-error' no-undo.
def var buf-status   as char no-undo.
def var mval         as char no-undo.
DEF VAR vIsIntr      AS LOGICAL NO-UNDO.
DEF VAR vFlagDB      AS LOGICAL INIT NO NO-UNDO.
DEF VAR vFlagCR      AS LOGICAL INIT NO NO-UNDO.
DEF VAR mOpStatDb    AS CHAR            NO-UNDO.
DEF VAR mOpStatCr    AS CHAR            NO-UNDO.

    vFilial = '0000'.
    shfilial = vFilial.
    shFilHead = vFilial.

    create w-op.
    create w-op-entry.
    create w-op-bank.
    w-op.bank-code-send = bank-mfo-9.

  ASSIGN w-op.op = numstr + 1
 	 w-op-entry.op            = w-op.op
         w-op-entry.op-entry      = 1.     
     run imp-tt.
     pause 0.
     if  numstr mod 100 eq 0  and numstr ne 0  then
     display  "Считано " + string(numstr,">>>>>9") + " строк файла."
       format "x(27)"
       with frame mess-str overlay centered row 14 no-label  .

   find first w-op no-error.
   find first w-op-entry where w-op-entry.op eq w-op.op no-error.
   find first w-op-bank  where w-op-bank.op  eq w-op.op no-error.
   find first op-template of op-kind no-lock.

   IF NOT AVAIL w-op then leave main.

/*message "in-op-date=" string (in-op-date) "op-templ.op-status=" op-templ.op-status view-as alert-box.*/

  assign
    numstr                   = numstr + 1
    /*w-op.op                  = 1*/
    w-op.op-date             = in-op-date
    w-op.doc-type            = w-op.doc-type
    w-op.user-id             = userid('bisquit')
    w-op.acct-cat            = "b"
    w-op.op-kind             = op-kind.op-kind
    w-op.op-status           = myStatus

    w-op-entry.op-date       = w-op.op-date
    w-op-entry.user-id       = w-op.user-id
    w-op-entry.acct-cat      = w-op.acct-cat
    w-op-entry.acct-db       = myFullAcctDb
    w-op-entry.op-status     = w-op.op-status
    /*w-op.inn                 = ""*/
    .

    flag-go = 3. flag-op-cr = yes. /* sku исходящий платеж */

     find first op-template of op-kind no-lock.
   /* message 'Начало проверки документа' view-as alert-box. */
     {debug.i "Начало проверки документа"}
     run imp-upd.p (1,no). /*  проверка загруженных документов */
     {debug.i "Проверен документ"}
     do k = 1 to num-entries(w-op.op-error):
       msg =  entry(k,w-op.op-error).
       if num-entries(msg,":") eq 2 then msg =  entry(2,msg,":").
     end.

	 w-op.op-error = "".
/*     run disp-err.*/



     {zp.mci}    /*   Создание op, op-entry, signs   */
/*message "8 op.op-date=" string (op.op-date) "op.op-status=" op.op-status view-as alert-box.*/

     find first op where op.op eq mem-op no-lock.
     find first op-entry of op /*where op-entry.op eq mem-op*/ no-lock.
     find current op no-lock.
     find current op-kind no-lock.
     find first op-entry of op exclusive-lock.
     find first op-bank of op no-lock no-error.
 /*    for each op-templ of op-kind no-lock: */
       assign 
         op-entry.type = 'НЕ'
         .


     {debug.i "Документ создан"}

/*message "9 op.op-date=" string (op.op-date) "op.op-status=" op.op-status view-as alert-box.*/

     {del-zp.i} /* удаление рабочих таблиц */

/*
/* второй документ */
    vFilial = '0500'.
    shfilial = vFilial.
    shFilHead = vFilial.

    create w-op.
    create w-op-entry.
    create w-op-bank.
    w-op.bank-code-send = bank-mfo-9.

  ASSIGN w-op.op = numstr + 1
     w-op-entry.op            = w-op.op
         w-op-entry.op-entry      = 1.      /* sku присваиваем ид. записи*/
     run imp-tt2.
/*     pause 0.
     if  numstr mod 100 eq 0  and numstr ne 0  then
     display  "Считано " + string(numstr,">>>>>9") + " строк файла."
       format "x(27)"
       with frame mess-str overlay centered row 14 no-label  .
*/
   find first w-op no-error.
   find first w-op-entry where w-op-entry.op eq w-op.op no-error.
   find first w-op-bank  where w-op-bank.op  eq w-op.op no-error.
   find first op-template of op-kind no-lock.

   IF NOT AVAIL w-op then leave main.

  assign
    /*w-op.op                  = 1*/
    w-op.op-date             = in-op-date
    w-op.doc-type            = w-op.doc-type
    w-op.user-id             = userid('bisquit')
    w-op.acct-cat            = "b"
    w-op.op-kind             = op-kind.op-kind
    w-op.op-status           = "√"

    w-op-entry.op-date       = w-op.op-date
    w-op-entry.user-id       = w-op.user-id
    w-op-entry.acct-cat      = w-op.acct-cat
    w-op-entry.acct-db       = '30302810205950010001     @0500'
    w-op-entry.acct-cr       = myFullAcctCr
    w-op-entry.op-status     = w-op.op-status
    /*w-op.inn                 = ""*/
    .

    flag-go = 3. flag-op-cr = no. /* sku исходящий платеж */

     find first op-template of op-kind no-lock.
   /* message 'Начало проверки документа' view-as alert-box. */
     {debug.i "Начало проверки документа"}
     run imp-upd.p (1,no). /*  проверка загруженных документов */
     {debug.i "Проверен документ"}
     do k = 1 to num-entries(w-op.op-error):
       msg =  entry(k,w-op.op-error).
       if num-entries(msg,":") eq 2 then msg =  entry(2,msg,":").
     end.

     w-op.op-error = "".
/*     run disp-err.*/
     {zp.mci}    /*   Создание op, op-entry, signs   */
/*message "8 op.op-date=" string (op.op-date) "op.op-status=" op.op-status view-as alert-box.*/

     find first op where op.op eq mem-op no-lock.
     find first op-entry of op /*where op-entry.op eq mem-op*/ no-lock.
     find current op no-lock.
     find current op-kind no-lock.
     find first op-entry of op exclusive-lock.
     find first op-bank of op no-lock no-error.
 /*    for each op-templ of op-kind no-lock: */
       assign 
         op-entry.type = 'НЕ'
         .

     {debug.i "Документ создан"}

     {del-zp.i} /* удаление рабочих таблиц */
     
    */
     
   end.    /* loop */
  end.     /* main */
  if bError then undo, leave.
  end. /* DO TRANS ON ERROR   UNDO, LEAVE: */
end procedure.

/*
procedure imp-tt2.
  def var nl as logical no-undo init true.
  def var ns as character no-undo init "".

/*  do while TRUE:*/
    if buf = "" then leave.

      w-op.class-code = myClassCode.
      w-op.doc-num = myNumDoc.  /* entry (1,buf," "). */
      w-op.doc-type = myDocType.
      w-op.doc-date = in-op-date.
      w-op.order-pay = '5'.
      w-op.due-date =  in-op-date.


      w-op.bank-corr-acct-send = '30101810900000000783'.

/*      w-op-entry.acct-cr = '30301810801100000004     @0000'.
      w-op-entry.acct-db = myFullAcctDb. */
      w-op.bank-code-rec = '045209783'. /*w-op-bank.bank-code = entry (9,buf,";").*/
      w-op.bank-corr-acct-rec = '30101810900000000783'. /*w-op-bank.corr-acct = entry (10,buf,";").*/
      w-op.ben-acct = myAcctDb.

      w-op-entry.currency = "" .

      w-op-entry.amt-rub = decimal(entry (5,buf," ")).
      

      w-op-bank.bank-name = 'ОАО "ПЛЮС БАНК"'.
     /* w-op.name-ben = entry (2,buf," ") + entry(3,buf,' '). */
     w-op.name-ben = ''.
      w-op.inn = '5503016736'.
      w-op.details = myDetails.

      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "inn-send"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = '550311448390'.
      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "acct-rec"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = entry (4,buf," ").
      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "name-rec"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = entry (2,buf," ") + ' ' + entry(3,buf,' ').
end procedure.
*/

procedure imp-tt.
    
  def var nl as logical no-undo init true.
  def var ns as character no-undo init "".

/*  do while TRUE:*/
    if buf = "" then leave.

      w-op.class-code = myClassCode.
      w-op.doc-num = myNumDoc. /* entry (1,buf," "). */
      w-op.doc-type = myDocType.
      w-op.doc-date = in-op-date.
      w-op.order-pay = '5'.


      w-op.bank-corr-acct-send = myBankCorrSend.
      w-op-entry.acct-cr = myAcctCr.
      w-op-entry.acct-db = myFullAcctDb.
      w-op.bank-code-rec = myBankBic. /*w-op-bank.bank-code = entry (9,buf,";").*/
      w-op.bank-corr-acct-rec = myBankCorrSend. /*w-op-bank.corr-acct = entry (10,buf,";").*/
      w-op.ben-acct = entry (4,buf," ").

      w-op-entry.currency = "" .

      w-op-entry.amt-rub = decimal(entry (5,buf," ")).
      myFullSumm = myFullSumm + w-op-entry.amt-rub.

	  w-op-bank.bank-name = myBankName.
      w-op.name-ben = myFam + ' ' + myIm.
      w-op.inn = myInnRec.
      w-op.details = myDetails.

      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "inn-send"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = myInnSend.
      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "acct-send"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = myAcctDb.
      create w-signs.
      assign
        w-signs.file-name = "op"
        w-signs.code = "name-send"
        w-signs.surrogate = string(w-op.op)
        w-signs.xattr-value = myNameAcct.


end procedure.

