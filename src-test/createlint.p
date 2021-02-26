/*   
     kam
     Filename: createlint.p
     Comment:  Создаем операцию 
     Чтобы вызвать: на шаблоне документа надо заполнить допрек ВыпПосле: Выполнить после обработки шаблона : createlint
     
*/


/* Проверяет, является ли строка - числом */
FUNCTION ConvertINT RETURN INT64 (iStr AS CHAR):
   DEF VAR vRes AS INT64 NO-UNDO.
   ERROR-STATUS:ERROR = NO.
   vRes = INT64(TRIM(iStr)) NO-ERROR.
   IF ERROR-STATUS:ERROR = YES THEN vRes = -1.
   RETURN vRes.
END FUNCTION.



DEF INPUT  PARAM in-rid  AS RECID NO-UNDO.
DEF INPUT  PARAM in-rid1 AS RECID NO-UNDO.
DEF OUTPUT PARAM fl      AS INT64   NO-UNDO INIT 0.
def buffer bloan-int for loan-int.

def var idd as INT64 no-undo.
def var idk as INT64 no-undo.
def var iOp as INT64 no-undo.
def var strOp as char no-undo.
def var strType as char no-undo.
def var strAcct as char no-undo.
def var strContCode as char no-undo.

/*
message '0 ' string(in-rid) view-as alert-box.    
*/

  if in-rid <> 0 then
     find op-templ where recid(op-templ) = in-rid no-lock no-error .
  find op-entry where recid(op-entry)  = in-rid1 no-lock no-error .
  if (in-rid <> 0 and not avail op-templ) or not avail op-entry then do :
/*    message 'Ошибка контроля проводки ' view-as alert-box error.  */
    return .
  end.

  find first op of op-entry no-error.
  if avail op then do:
    FIND FIRST op-template WHERE 
              op-template.op-kind     EQ op.op-kind 
          AND op-template.op-template EQ op.op-templ NO-LOCK NO-ERROR.
    IF NOT AVAIL op-template THEN DO:
   /*   MESSAGE "Не могу найти запись шаблона!"  
         VIEW-AS ALERT-BOX ERROR.                */
    return.
    END.
/*
message '1' view-as alert-box.    
*/
    strContCode = ?.
    strAcct = ?.
    strType = REPLACE(REPLACE(TRIM(op-template.acct-cr),'Роль("',''),'")','').
    if index(strType, "ОВ") > 0 then do:
        strAcct = op-entry.acct-cr.
    end.    
    else do:
        strType = REPLACE(REPLACE(TRIM(op-template.acct-db),'Роль("',''),'")','').
        if index(strType, "ОВ") > 0 then do:
             strAcct = op-entry.acct-cr.
        end.
    end.
/*
message '2 ' string(strAcct) view-as alert-box.    
*/
    if strAcct <> ? then find last loan-acct where loan-acct.contract = 'Кредит'
        and loan-acct.acct-type = strType
        and loan-acct.acct = strAcct 
        no-lock no-error.  
    if avail loan-acct then strContCode = loan-acct.cont-code.
/*
message '3 ' string(strContCode) view-as alert-box.    
*/
    if strContCode = ? then return.
    
    strOP = ''.  
    find first signs
         where file-name = 'op-template'
	 and surrogate = string(op-template.op-kind) + ',' + string(op-templ.op-template)
	 and code = 'op_code'
    no-lock no-error.

    if avail signs then do:
        strOp = REPLACE(REPLACE(TRIM(signs.xattr-value),'КР_',''),'ДБ_','').
        iOp = ConvertINT(strOP).
        IF iOp > 0 THEN DO:
            find first chowhe where chowhe.id-op = iOp no-lock no-error.
            if avail chowhe then do:
                idd = chowhe.id-d.
                idk = chowhe.id-k.
                find last bloan-int where bloan-int.cont-code = strContCode
                    and bloan-int.mdate = op.op-date
                    no-lock no-error.
                CREATE loan-int.
                assign
        			loan-int.op-date = op.op-date 
		    	    loan-int.mdate = op.op-date 
			        loan-int.nn = (IF AVAIL bloan-int 
                                       THEN bloan-int.nn + 1
                                       ELSE 1)
			        loan-int.contract = 'Кредит'
			        loan-int.user-id = USERID("bisquit")
			        loan-int.cont-code = strContCode
			        loan-int.op = -2
			        loan-int.op-entry = -1
			        loan-int.id-k = idk
			        loan-int.id-d = idd
			        loan-int.amt-rub = abs(op-entr.amt-rub)
        	        .
	              	RELEASE loan-int.
            end.
        END.
    end.
  end.

