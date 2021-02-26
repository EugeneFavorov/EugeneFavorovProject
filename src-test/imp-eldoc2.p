{globals.i}
{intrface.get store}
{intrface.get xclass}
{intrface.get db2l}

{tmprecid.def}

{setdest.i}

    DEF VAR iFName AS CHAR NO-UNDO.
    DEF VAR mPtr1 AS MEMPTR NO-UNDO.
    DEF VAR mPtr2 AS MEMPTR NO-UNDO.
    DEF VAR oResult AS CHAR NO-UNDO.
    DEF VAR vIdCust AS CHAR NO-UNDO.
    DEF VAR errorId AS INTEGER NO-UNDO.
    DEF VAR errorMsg AS CHAR NO-UNDO.
    DEF VAR iCid AS INT64 NO-UNDO.

FOR EACH tmprecid,
   FIRST person WHERE
      recid(person) EQ tmprecid.id NO-LOCK:
    vIdCust = STRING( person.person-id).
    iCid = INT64( GetXattrValue( "person", vIdCust, "CID")).

    IF iCid NE ? AND iCid > 0 THEN
    FOR EACH bank.clients_image
     where clients_image.cid = iCid and clients_image.type <> 9999 no-lock:

	iFName = LC( 'file_' + STRING(clients_image.lid) + '.' + clients_image.file_type).
	FOR EACH indocs
	 WHERE indocs.file-name = "person" AND indocs.surrogate EQ STRING( person.person-id) NO-LOCK,
	  FIRST signs WHERE signs.file-name = "indocs" AND signs.surrogate EQ STRING(indocs.indoc-id)
	    AND signs.code EQ 'DocumentFile' AND (signs.code-value EQ iFName OR signs.code-value MATCHES '*/' + iFName) NO-LOCK:
	    LEAVE.
	END.
	IF AVAIL indocs THEN NEXT.
	/* RELEASE indocs. */
	FIND FIRST bank.glossary where clients_image.type = glossary.gid NO-LOCK NO-ERROR.

        COPY-LOB clients_image.image TO FILE iFName.

	/* iFName = 'face.jpg'. */

	CREATE indocs.
	ASSIGN 
	  indocs.class-code  = "eDocument"
          indocs.contract    = "ЭД"
          indocs.filial-id   = shFilial
          indocs.branch-id   = GetXAttrValueEx("_user", 
                                           USERID("bisquit"),
                                           "Отделение", 
                                           FGetSetting("КодФил",?,""))
          indocs.user-id     = 'SYNC' /* USERID("bisquit") */
          indocs.create-date = clients_image.opentime
          indocs.exp-date    = (IF clients_image.closetime EQ ? THEN clients_image.opentime + 365 * 100 ELSE clients_image.closetime)
          indocs.doc-type    = "Файл"
          indocs.reference   = "-Нет-"
          indocs.parent-id   = 0
          indocs.file-name = "person"
          indocs.doc-team  = (IF AVAIL glossary THEN glossary.sname ELSE "")
	   .

        UpdateSigns("eDocument",STRING(indocs.indoc-id),"DocumentFile",iFName,?).

        VALIDATE indocs NO-ERROR.

        indocs.cust-cat = "Ч".
        indocs.cust-id  = person.person-id.
        indocs.surrogate  = STRING( person.person-id).

        IF NOT GetEdDescription (indocs.indoc-id, iFName, output mPtr2)
         THEN DO:
            MESSAGE "Не удалось привязать документ: xml-дескриптор не найден" VIEW-AS ALERT-BOX.
         END.
         ELSE DO: /* xml-дескриптор найден */
             /* COPY-LOB FILE iFName TO mPtr1 NO-ERROR. /* в mPtr1 ссылка на привязываемый файл... */ */
             COPY-LOB clients_image.image TO mPtr1.
             IF NOT ERROR-STATUS:ERROR THEN DO: /* ...получена без ошибок */
                 oResult = PutDocumentIntoStorage(mPtr1,mPtr2,output errorId,output errorMsg).
                 IF oResult EQ ? /* не удалось поместить документ в хранилище */
                 THEN DO:
                    MESSAGE "Не удалось поместить документ id=" STRING(indocs.cust-id)  STRING(indocs.indoc-id) "в хранилище:" errorMsg  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 END.
                 ELSE DO: /* удалось поместить документ в хранилище */
                     indocs.reference = oResult.
                     UpdateSignsEx('eDocument', STRING(indocs.indoc-id), 'DocumentFile', iFName).
                 END.
             END.
             ELSE DO:
                MESSAGE "Не удалось привязать документ: файл " iFName " не найден" VIEW-AS ALERT-BOX.
             END.
         END.
        OS-DELETE VALUE ( iFName).
    END.
END.
