DEFINE INPUT PARAMETER iTypeFil AS CHARACTER.
DEFINE INPUT PARAMETER iTypeAcc  AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER iTypeVal AS CHARACTER.
DEFINE INPUT PARAMETER iTypeID AS CHARACTER.
DEFINE VARIABLE kc  AS CHARACTER NO-UNDO.
DEFINE VARIABLE nom_z  AS CHARACTER NO-UNDO .

DEFINE VARIABLE date_z  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE int_ob  AS CHARACTER  NO-UNDO.
 
IF iTypeFil eq '1' THEN
DO:  
 
PAUSE 0.
FORM

   kc  VIEW-AS COMBO-BOX LIST-ITEMS "40", "42", "53" INNER-LINES 3 DROP-DOWN-LIST SIZE 10 BY 3 AT ROW 1 COL 2 LABEL "Кассовый символ " 
    
   nom_z FORMAT "x(8)" VIEW-AS FILL-IN SIZE 8 BY 2 AT ROW 3 COL 2 LABEL "Заявление №"
   date_z   /*LABEL  "123"*/
   FORMAT "x(10)"  VIEW-AS FILL-IN SIZE 10 BY 2 AT ROW 3 COL 23 LABEL " от "
    
   int_ob FORMAT "x(60)" VIEW-AS FILL-IN SIZE 40 BY 2 AT ROW 5 COL 2  LABEL  "Цель перевода " 
    

    
    WITH FRAME fMain CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE COLOR bright-red "[ ВЫБОР ПАРАМЕТРОВ ]"  .   

   
ON RETURN OF date_z  IN FRAME fMain 
    DO:
        date_z  = date_z:SCREEN-VALUE.  
        APPLY "TAB" TO SELF.
    END.

ON RETURN OF nom_z  IN FRAME fMain 
    DO:
        nom_z  = nom_z:SCREEN-VALUE.  
        APPLY "TAB" TO SELF.
    END.
 
ON F1 OF date_z IN FRAME fMain DO:
      DO TRANSACTION:
         RUN calend.p.
      END.

      IF (lastkey=13 OR lastkey=10) /*AND pick-value<>? */ THEN
      DO:
         ASSIGN date_z = string(today,'99.99.9999').
         DISP date_z WITH FRAME fMain.
      END.
   END.
 
ON "GO",CTRL-G OF FRAME fMain ANYWHERE 
    DO:
        APPLY "LEAVE" TO kc IN FRAME fMain.
        IF RETURN-VALUE EQ "NO-APPLY" THEN
            RETURN NO-APPLY.
   
    END.

MAIN:
DO ON ERROR  UNDO, LEAVE 
    ON ENDKEY UNDO, LEAVE WITH FRAME fMain: 
    UPDATE
        kc nom_z date_z int_ob 
        .
END. 
HIDE FRAME fMain NO-PAUSE.
IF KEYFUNC(LASTKEY) EQ "END-ERROR" THEN
    RETURN.

ASSIGN
    kc nom_z date_z int_ob .
    
    RETURN kc + ',' +  nom_z  + ',' + date_z + ',' + int_ob .
ENd.
ELSE
DO:
    
DEFINE VARIABLE hQry       AS HANDLE        NO-UNDO.

CREATE QUERY hQry.
iTypeAcc = iTypeAcc:DEFAULT-BUFFER-HANDLE.

/*hQry:SET-BUFFERS (iTypeAcc).
hQry:QUERY-PREPARE ("PRESELECT EACH " + iTypeAcc:NAME).

{for_dqry.i hQry}*/

 FIND FIRST acct WHERE acct.acct EQ STRING(iTypeAcc:BUFFER-FIELD("acct"):BUFFER-VALUE) NO-LOCK NO-ERROR.
    
   /* IF AVAILABLE acct THEN*/
   
    MESSAGE     acct.acct  STRING(iTypeAcc:BUFFER-FIELD("acct"):BUFFER-VALUE)                   VIEW-AS ALERT-BOX .
   
    
    RETURN acct.acct.
/*DEFINE VARIABLE vAcct     AS CHARACTER NO-UNDO.
{pick-val.i}
pick-value = ?.
    /*
FOR EACH ACCT WHERE ACCT.FILIAL-ID = '" + () + "'
AND ACCT.CLOSE-DATE EQ ?
AND ACCT.CURRENCY EQ '',
FIRST SIGNS WHERE SIGNS.FILE-NAME EQ 'ACCT'
AND SIGNS.SURROGATE EQ (ACCT.ACCT + ',' + ACCT.CURRENCY)
AND SIGNS.CODE EQ ''
AND SIGNS.CODE-VALUE NE '' NO-LOCK:"*/
    
    
         
           RUN browseld.p("acctb",
                  "cust-cat" + CHR(1) + "acct",
                  "Ю"        + CHR(1) + "40817*,40820*,423*,426*",
                  "",
                  4).
         
          IF LASTKEY NE KEYCODE("ESC") 
      AND pick-value NE ? THEN
 /*  ASSIGN
      vAcct              = pick-value
      vAcct:SCREEN-VALUE = pick-value.*/
      
       RETURN pick-value.
         
END.*/ end.   