{globals.i}
{intrface.get pbase}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{intrface.get instrum}
{intrface.get tmess}
{intrface.get pqres}
{dpsproc.def}
{parsin.def}
{sh-defs.i}
{tmprecid.def}

DEFINE VARIABLE mUpd  AS LOGICAL NO-UNDO.
DEFINE VARIABLE mAcct AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk   AS LOGICAL NO-UNDO.

DEFINE BUFFER signs-acct FOR signs.
DEFINE BUFFER signs-date FOR signs.

DEFINE TEMP-TABLE tt-rid
   FIELD rid-acct AS INT64
   FIELD rid-date AS INT64.

DEFINE TEMP-TABLE tt-acct
   FIELD racct AS CHARACTER
   FIELD rdate AS CHARACTER.

RUN dbgprint.p("rmvacct"," Очистка реквизитов ResAcct ResDate").

FOR EACH signs-acct WHERE
       signs-acct.file-name EQ "cust-corp"
   AND signs-acct.code      EQ "ResAcct"
   NO-LOCK,
   FIRST signs-date WHERE
       signs-date.file-name EQ "cust-corp"
   AND signs-date.code      EQ "ResDate"
   AND signs-date.surrogate EQ signs-acct.surrogate
   NO-LOCK:

   IF DATE(signs-date.code-value) LT TODAY - 31 THEN
   DO:
      CREATE tt-rid.
      ASSIGN
         tt-rid.rid-acct = RECID(signs-acct) 
         tt-rid.rid-date = RECID(signs-date).
   END.
END.

FOR EACH tt-rid NO-LOCK:
  
   FIND FIRST signs-acct WHERE
      RECID(signs-acct) EQ tt-rid.rid-acct
   NO-LOCK NO-ERROR.
   
   IF AVAIL(signs-acct) THEN
   DO:
      RUN dbgprint.p("rmvacct"," acct = " + signs-acct.xattr-value).
      mAcct = signs-acct.xattr-value.
      mUpd = UpdateSigns("cust-corp",signs-acct.surrogate,"ResAcct","",?).
      
      /* Удаляем счет из классификатора "СчетаРезерва". */
      RUN AcctFree IN h_acct (mAcct                ,OUTPUT mOk).
      RUN AcctFree IN h_acct (DelFilFromAcct(mAcct),OUTPUT mOk).
   END.
   
   FIND FIRST signs-date WHERE
      RECID(signs-date) EQ tt-rid.rid-date
   NO-LOCK NO-ERROR.
   
   IF AVAIL(signs-date) THEN
   DO:
      RUN dbgprint.p("rmvacct"," date = " + signs-date.code-value).
      mUpd = UpdateSigns("cust-corp",signs-date.surrogate,"ResDate","",?).
   END.
END.

{empty tt-acct}

FOR each code WHERE TRUE
   AND code.class  EQ "СчетаРезерва"
   AND code.parent EQ "СчетаРезерва"
   AND code.code   BEGINS "40817"
   NO-LOCK:
   IF DATE(code.val) GT TODAY - 32 THEN NEXT.
   CREATE tt-acct.
   ASSIGN
      tt-acct.racct = code.code
      tt-acct.rdate = code.val.
END.

FOR EACH tt-acct NO-LOCK:
   /* Удаляем счет из классификатора "СчетаРезерва". */
   RUN AcctFree IN h_acct (tt-acct.racct,OUTPUT mOk).
   RUN dbgprint.p("rmvacct","acct = " + tt-acct.racct + " date = " + tt-acct.rdate + ":" + STRING(mOk)).
END.

RETURN.
