/*nbki-qry.p*/

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}

{sh-defs.i}
{tmpobj.def}
{tmprecid.def}
{clnt.fun}

/* DEFINE VARIABLE mTypeQ    AS CHARACTER NO-UNDO VIEW-AS COMBO-BOX INNER-LINES 3. */
DEFINE VARIABLE mDolg     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDolgRP   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDolgTP   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtdel    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtdelRP  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtdelTP  AS CHARACTER NO-UNDO.

DEFINE VARIABLE mInt      AS INT64     NO-UNDO.
ASSIGN
   mDolg    = GetXattrValueEx("_user",USERID('bisquit'),"Должность","")
   mDolgRP  = GetXattrValueEx("_user",USERID('bisquit'),"ДолжностьРП","")
   mDolgTP  = GetXattrValueEx("_user",USERID('bisquit'),"ДолжностьТП","")
   mOtdelRP = GetXattrValueEx("_user",USERID('bisquit'),"ОтделРП","")
.

PAUSE 0.
FORM
   mDolg    LABEL  "Должность ИП" 
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Должность в иминительном падеже"
   mDolgRP  LABEL  "Должность РП"
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Должность в родительном падеже"            
   mDolgTP  LABEL  "Должность ТП" 
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Должность в творительном падеже"
   mOtdelRP LABEL  "Отдел РП" 
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Наименование отдела родительном падеже"
   /* mOtdel   LABEL  "Отдел ИП" 
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Наименование отдела иминительном падеже"
   mOtdelTP LABEL  "Отдел РП" 
            FORMAT "x(56)"
            VIEW-AS FILL-IN SIZE 56 BY 1
            HELP   "Наименование отдела в творительном падеже" */

WITH FRAME frUser OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE " Изменение Д.Р. пользователя: " + USERID('bisquit') + " " + GetXattrValueEx("_user",USERID('bisquit'),"ФИОП","").

ON LEAVE OF mDolg                             
DO:                                           
   IF mDolg:SCREEN-VALUE EQ ""              
   AND LAST-KEY NE 301                       
   AND LAST-KEY NE 501                       
   AND LAST-KEY NE 509                       
   THEN                                      
   DO:                                       
      MESSAGE "Должность должна быть заполнена."
      VIEW-AS ALERT-BOX.                     
      RETURN NO-APPLY.                       
   END.                                      
END.                                          

ON LEAVE OF mDolgRP
DO:          
   IF mDolgRP:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "Должность в РП должна быть заполнена."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.


ON LEAVE OF mOtdelRP
DO:
   IF mOtdelRP:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "Отдел в РП должен быть заполнен."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

/* ON LEAVE OF mOtdel
DO:
   
   IF mOtdel:SCREEN-VALUE EQ "" 
   AND LAST-KEY NE 501 
   AND LAST-KEY NE 509
   THEN
   DO:
      MESSAGE "Отдел должен быть заполнен."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END. */

ON "GO" OF FRAME frUser  
DO:
IF mDolg:SCREEN-VALUE EQ ""               
AND LAST-KEY NE 301                        
THEN                                       
   DO:                                        
      MESSAGE "Должность должна быть заполнена."
      VIEW-AS ALERT-BOX.                      
      APPLY "ENTRY" TO mDolg .                
      RETURN NO-APPLY.                        
   END.                                       

   IF mDolgRP:SCREEN-VALUE EQ "" 
   THEN
   DO:
      MESSAGE "Должность РП должна быть заполнена."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mDolgRP.
      RETURN NO-APPLY.
   END.
   
   IF mOtdelRP:SCREEN-VALUE EQ "" 
   AND mInt EQ 1 
   THEN
   DO:
      MESSAGE "Отдел РП должен быть заполнен."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mOtdelRP.
      RETURN NO-APPLY.
   END.
   
   /* IF mOtdel:SCREEN-VALUE EQ "" 
   AND (mInt EQ 1 OR mInt EQ 2)  
   THEN
   DO:
      MESSAGE "Отдел должен быть заполнен."
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO mOtdel.
      RETURN NO-APPLY.
   END. */
END.   

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frUser.
      RETURN ERROR.
   END.
   
   UPDATE
      mDolg 
      mDolgRP
      mDolgTP
      mOtdelRP
      /* mOtdel
      mOtdelTP */
   WITH FRAME frUser.

FIND FIRST _user WHERE 
   _user._userid EQ USERID('bisquit') 
NO-LOCK NO-ERROR.
   IF AVAIL(_user) THEN
   DO:
      UpdateSigns("_user",STRING(_user._userid),"Должность",mDolg,?).
      UpdateSigns("_user",STRING(_user._userid),"ДолжностьРП",mDolgRP,?).
      UpdateSigns("_user",STRING(_user._userid),"ДолжностьТП",mDolgTP,?).
      UpdateSigns("_user",STRING(_user._userid),"ОтделРП",mOtdelRP,?).
      /*UpdateSigns("_user",STRING(_user._userid),"DR",mOtdelRP,?).
      UpdateSigns("_user",STRING(_user._userid),"DR",mOtdelTP,?). */
   END.

END.
HIDE FRAME frUser  .

{intrface.del}