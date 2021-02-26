DEFINE INPUT PARAMETER iTypeFil AS CHARACTER.
DEFINE INPUT PARAMETER iTypeAcc  AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER iTypeVal AS CHARACTER.
DEFINE INPUT PARAMETER iTypeID AS CHARACTER.

DEFINE VARIABLE nom_z  AS CHARACTER NO-UNDO .
DEFINE VARIABLE cod  AS CHARACTER NO-UNDO .
DEFINE VARIABLE bik  AS CHARACTER NO-UNDO .
DEFINE VARIABLE rc  AS CHARACTER NO-UNDO .
DEFINE VARIABLE date_z  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE client  AS CHARACTER NO-UNDO .
 
{ globals.i           }
{ intrface.get xclass }
 
/*IF iTypeFil eq '1' THEN*/
DO:  
 
PAUSE 0.
FORM
     
   nom_z  FORMAT "x(8)"   VIEW-AS FILL-IN SIZE 8 BY 2 AT ROW 1 COL 2 LABEL "Референс операции"
   date_z FORMAT "x(10)"  VIEW-AS FILL-IN SIZE 10 BY 2 AT ROW 1 COL 30 LABEL " от "
   client FORMAT "x(100)" VIEW-AS FILL-IN SIZE 40 BY 2 AT ROW 3 COL 2  LABEL  "Клиент" 
   cod    FORMAT "x(11)"  VIEW-AS FILL-IN SIZE 11 BY 2 AT ROW 4 COL 2  LABEL  "Код   "    
   bik    FORMAT "x(11)"  VIEW-AS FILL-IN SIZE 11 BY 2 AT ROW 5 COL 2  LABEL  "БИК   " 
   rc     FORMAT "x(40)"  VIEW-AS FILL-IN SIZE 40 BY 2 AT ROW 6 COL 2  LABEL  "Р/С   " 
    
    WITH FRAME fMain CENTERED ROW 10 OVERLAY SIDE-LABELS
    TITLE COLOR bright-red "[ ВЫБОР ПАРАМЕТРОВ ]"  .   

    cod = 'BIC'. 
    nom_z = '1'.
    date_z = string(today,'99.99.9999').
   
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
   
ON F1 OF bik IN FRAME fMain DO:
DO TRANSACTION:
      RUN banks.p (7).
        IF (LASTKEY = 13 OR LASTKEY = 10) AND PICK-VALUE <> ? THEN
           SELF:SCREEN-VALUE = ENTRY(2,PICK-VALUE).
        {getbank.i banks SELF:SCREEN-VALUE}
        IF NOT AVAIL BANKS THEN DO: 
           MESSAGE "Такого банка нет в справочнике !" VIEW-AS ALERT-BOX.
           APPLY "LEAVE" TO SELF.
           RETURN NO-APPLY.
        END.
        RETURN NO-APPLY.
   END.
END. 

ON F1 OF cod IN FRAME fMain DO:
DO TRANSACTION:
       RUN pclass.p ("КодБанка","КодБанка","Код",7).  
        IF (LASTKEY = 13 OR LASTKEY = 10) AND PICK-VALUE <> ? THEN
           SELF:SCREEN-VALUE = PICK-VALUE.    
        RETURN NO-APPLY.
  END.
END. 
 
/* ON LEAVE OF bik IN FRAME edit OR GO    OF FRAME fMain
DO:
    
    IF bik:SCREEN-VALUE NE "" THEN DO:
   {getbank.i "banks" "op-bank.bank-code:SCREEN-VALUE"}
   IF NOT AVAILABLE banks THEN DO:
    
end
 
 
ON LEAVE OF bik IN FRAME edit OR GO    OF FRAME fMain
DO:
    
    IF bik:SCREEN-VALUE NE "" THEN DO:
   {getbank.i "banks" "op-bank.bank-code:SCREEN-VALUE"}
   IF NOT AVAILABLE banks THEN DO:
    
end. */   
 
/*

ON F1 OF vText IN FRAME f1 DO:
   DO:
      pick-value = "".
      DO TRANSACTION:
         RUN pclass.p ("365","365","", 3).
      END.
      IF KEYFUNCTION(LASTKEY) NE "END-ERROR"
         AND {assigned pick-value} THEN 
         SELF:SCREEN-VALUE = GetCodeName("365", pick-value).
      RETURN.
   END.
END.


ON "GO",CTRL-G OF FRAME fMain ANYWHERE 
    DO:
        APPLY "LEAVE" TO kc IN FRAME fMain.
        IF RETURN-VALUE EQ "NO-APPLY" THEN
            RETURN NO-APPLY.
   
    END.
*/



MAIN:
    
DO ON ERROR  UNDO, LEAVE 
    ON ENDKEY UNDO, LEAVE WITH FRAME fMain: 
    UPDATE
        nom_z date_z client cod bik rc
        .
END. 
HIDE FRAME fMain NO-PAUSE.

IF KEYFUNC(LASTKEY) EQ "END-ERROR" THEN
    RETURN.

ASSIGN
    nom_z date_z client cod bik rc.
    
    RETURN   nom_z  + ';' + date_z + ';' + client + ';' + rc + ';'+ bik + ';' + cod .
ENd.



/*ELSE
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
         
END.*/ end.   */