/*
                    ОАО ПЛЮС-БАНК
          ***  Отдел банковских ситстем  ***

   Назначение  : Коррекция кассовых символов.
                 Запускается из Разных процедур в БМ.
   Параметры   :

  Используемые
  инклюд-файлы :

  Используется
  в процедурах :

   Создание    : Савельев К.Ю.

   Исправление : 
*/
{globals.i}

def var ii as i no-undo.
def var opdate as date no-undo.
def var docnum as c format "x(8)" no-undo.
def var oldks as c format "xx" no-undo.
def var newks as c format "xx" no-undo.
def var maskdb as c format "x(20)" no-undo.
def var maskcr as c format "x(20)" no-undo.

/*
opdate = date("25/02/2013").
maskdb = '40817810304000029111'.
maskcr = '30102810904000000129'.
oldks = "03".
*/

pause 0.
/*{getdates.i}*/
form
  opdate label "дата док" skip
  docnum label "№ док" skip
  maskdb label "счет Дб" skip
  maskcr label "счет Кр" skip
  oldks  label "Старый КС  " skip
  newks  label "Новый КС   " skip
  with centered overlay row 5 frame qq10 side-label
  title "Параметры замены КС".

repeat /*transaction */ on error undo, retry on endkey undo, leave:
	UPDATE
		opdate docnum
		maskdb maskcr oldks
		WITH FRAME qq10 /*OVERLAY ROW 10*/
		editing:
			readkey.
			if lastkey = 27 then do:
				HIDE FRAME qq10.
				LEAVE.
			end. else
			if keyfunc(lastkey) eq "end-error" then do:
			/*if keylabel(lastkey) = "F1" then do:
				assign LoadFile.
				run ch-file.p (input-output LoadFile).
				if LoadFile = ? then return.
				display LoadFile with frame dd overlay row 10.*/
			end. else
			if lastkey = keycode("F1") then do:
				if frame-field = "maskdb" then do:
					run acct.p ("b",3).
					if (lastkey = 10 or lastkey = 13) and pick-value <> ? then do:
						disp ENTRY( 1, pick-value) @ maskdb with frame qq10.
						maskdb = ENTRY( 1, pick-value).
						/*message pick-value "-" maskdb view-as alert-box.*/
					end.
				end. else
				if frame-field = "maskcr" then do:
					run acct.p ("b",4).
					if (lastkey = 10 or lastkey = 13) and pick-value <> ? then
						disp (entry(1, pick-value)) @ maskcr with frame qq10.
				end.
			end. else
			if lastkey eq keycode("ENTER") and pick-value ne ? then do:
				apply lastkey.
				assign
					opdate docnum
					maskdb maskcr oldks
					.
				ii = 0.
				for each op-entry where
					op-entry.op-date eq opdate AND
					(maskdb = '' OR op-entry.acct-db begins maskdb) AND
					(maskcr = '' OR op-entry.acct-cr begins maskcr) AND
					(oldks = '' OR op-entry.symbol eq oldks) no-lock,
					each op of op-entry where docnum = '' OR op.doc-num EQ docnum no-lock:
					ii = ii + 1.
				end.
				/*message "opa " ii "-" opdate "-" docnum view-as alert-box.*/
				if ii=1 THEN DO:
					find first op-entry where
						op-entry.op-date eq date(opdate) AND
						(maskdb = '' OR op-entry.acct-db begins maskdb) AND
						(maskcr = '' OR op-entry.acct-cr begins maskcr) AND
						(oldks = '' OR op-entry.symbol eq oldks) AND 
						CAN-FIND(first op of op-entry where docnum = '' OR op.doc-num EQ docnum no-lock) no-lock
						.
						find first op of op-entry where docnum = '' OR op.doc-num EQ docnum no-lock.
					assign
						docnum = op.doc-num
						maskdb = op-entry.acct-db
						maskcr = op-entry.acct-cr
						oldks = op-entry.symbol.
					DISPLAY
						opdate docnum
						maskdb maskcr oldks
						WITH FRAME qq10.
				END.
			end. else apply lastkey.
	end.
	if keyfunc(lastkey) eq "end-error" then leave.
	ii = 0.
	for each op-entry where
		op-entry.op-date eq opdate AND
		op-entry.acct-db begins maskdb AND
		op-entry.acct-cr begins maskcr AND
		op-entry.symbol eq oldks no-lock,
		each op of op-entry where op.doc-num EQ docnum no-lock:
	   ii = ii + 1.
	end.
	if ii<1 then DO:
		message "Документ не найден !".
		UNDO, RETRY.
	END.
	if ii>1 then DO:
		message "Найдено несколько документов с указанными реквизитами !".
		UNDO, RETRY.
	END.

	find first op-entry where
		op-entry.op-date eq opdate AND
		op-entry.acct-db begins maskdb AND
		op-entry.acct-cr begins maskcr AND
		op-entry.symbol eq oldks AND 
		CAN-FIND(first op of op-entry where op.doc-num EQ docnum no-lock).

	newks = op-entry.symbol.
	do on error undo, retry on endkey undo, leave:
			UPDATE newks WITH FRAME qq10.
			if lastkey = 27 then LEAVE.

			if oldks = newks then do:
  				message "Указаны одинаковые кассовые символы".
				UNDO, RETRY.
			end.
			assign
				op-entry.symbol = newks.
			VALIDATE op-entry NO-ERROR.
			IF ERROR-STATUS:ERROR then DO:
				message "Кассовый символ отсутствует в справочнике !".
				UNDO, RETRY.
			END.
			message "Кассовый символ заменен на" op-entry.symbol view-as alert-box.
	end.
end.

HIDE FRAME qq10 no-pause.

/*
{setdest.i}
put unformatted "Протокол замены КС за период " beg-date " - " end-date skip
  "по документам " maskdb " - " maskcr skip.
for each op-entry where op-entry.op-date >= beg-date and
  op-entry.op-date <= end-date and

  op-entry.symbol = oldks and
  can-do(maskdb,op-entry.acct-db) and
  can-do(maskcr,op-entry.acct-cr) and
  (can-do("!20209*,202..810*",op-entry.acct-db) or
  can-do("!20209*,202..810*",op-entry.acct-cr)),
  first op of op-entry no-lock:

  ii = ii + 1.
  put unformatted
    ii " "
    op.op-date " "
    op.doc-num format "x(6)" " "
    op-entry.acct-db " "
    op-entry.acct-cr " "
    op-entry.amt-rub format "->>>>>>>>>9.99"
    skip.
  op-entry.symbol = newks.
end.
{preview.i}*/

