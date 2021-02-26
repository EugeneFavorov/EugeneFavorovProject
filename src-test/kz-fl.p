/*
cred-pko.p
*/

{globals.i}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get strng}
{intrface.get blkob}

{sh-defs.i}
{tmpobj.def}
{tmprecid.def}
{ttretval.def}

DEFINE VARIABLE vCustID   AS INT64     NO-UNDO.
DEFINE VARIABLE vName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCurr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSymbol   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDate     AS DATE      NO-UNDO.
DEFINE VARIABLE vAmt      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vInnSend  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vNameCl   AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE vInt      AS INT64     NO-UNDO.

FORM
   vDate    FORMAT "99/99/9999"
            LABEL  "Дата"
            HELP   "Дата "
   vAcct    FORMAT "x(20)"
            LABEL  "Номер счета"
            HELP   "F1 - выбор счета "
   vSymbol  FORMAT "x(2)"
            LABEL  "Символ"
            HELP   "Символ "
   vAmt     LABEL  "Сумма заявки" 
            FORMAT ">>>>>>>>>>>>>>>>>9.99"
            HELP   "Сумма заявки "
WITH FRAME frKZ OVERLAY CENTERED ROW 8 SIDE-LABELS 1 COL
TITLE " Данные кассовой заявки ".

ON LEAVE OF vDate
DO:
   ASSIGN vDate.
   IF vDate LE TODAY THEN 
   DO:
      MESSAGE "Дата должна быть больше сегодняшней"
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON 'F1':U OF vAcct IN FRAME frKZ
DO:
   RUN browseld.p("acctb",
                  "cust-cat" + CHR(1) + "acct",
                  "Ч"        + CHR(1) + "40817*,40820*,423*,426*",
                  "",
                  4).
   IF LASTKEY NE KEYCODE("ESC") 
      AND pick-value NE ? THEN
   ASSIGN
      vAcct              = pick-value
      vAcct:SCREEN-VALUE = pick-value.
END.

ON LEAVE OF vAcct
DO:
   ASSIGN vAcct.
   IF NOT {assigned vAcct} THEN 
   DO:
      MESSAGE "Счет должен быть заполнен"
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vSymbol
DO:
   ASSIGN vSymbol.
   IF NOT {assigned vSymbol} THEN 
   DO:
      MESSAGE "Символ должен быть заполнен"
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON LEAVE OF vAmt
DO:
   ASSIGN vAmt.
   IF vAmt EQ 0 THEN 
   DO:
      MESSAGE "Сумма не может быть равна 0.00"
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

vDate = TODAY + 1.

UPD:
DO TRANSACTION ON ERROR  UNDO UPD, RETRY UPD
               ON ENDKEY UNDO UPD, LEAVE UPD:
   IF RETRY THEN DO:
      HIDE FRAME frKZ.
      RETURN ERROR.
   END.

   UPDATE
      vDate
      vAcct
      vSymbol
      vAmt 
   WITH FRAME frKZ.

   vAcct = AddFilToAcct(vAcct,shFilial).
   
   {find-act.i
	   &acct = vAcct 
	}

   IF AVAIL(acct) THEN
   DO:
      RUN GetCustName IN h_Base
            (acct.cust-cat, 
             acct.cust-id,
             "",
             OUTPUT vNameCl[1],
             OUTPUT vNameCl[2],
             INPUT-OUTPUT vInnSend).
   END.
   
   IF vAcct NE ""
   AND vAmt GT 0 THEN
   DO:
      pick-value = "1".
      FIND FIRST code WHERE
             code.class  EQ "KassZ"
         AND code.parent EQ "KassZ"
         AND code.code   EQ STRING(vDate,"99-99-9999") + "-" + vAcct
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL(code) THEN
      DO:
         {messmenu
            &text    = "Уже есть запись по этому счету. Обновить?"
            &choices = "Да,Отмена"
            &update  = 1
         }
      END.
      IF pick-value EQ "1" THEN
      DO:
         IF NOT AVAIL(code) THEN
         DO: 
            CREATE code.
         	ASSIGN
               code.class  = "KassZ"
               code.parent = "KassZ"
               code.code   = STRING(vDate,"99-99-9999") + "-" + vAcct
               code.name   = vAcct
               code.val    = TRIM(STRING(vAmt,">>>>>>>>>>>>>>>>>9.99")) + "|" + vSymbol
               code.description[1] = vNameCl[1] + " " + vNameCl[2].
            MESSAGE "Кассовая заявка создана"
            VIEW-AS ALERT-BOX.
         END.
         ELSE
         DO:
            ASSIGN
               code.name   = vAcct
               code.val    = TRIM(STRING(vAmt,">>>>>>>>>>>>>>>>>9.99")) + "|" + vSymbol
               code.description[1] = vNameCl[1] + " " + vNameCl[2].
            MESSAGE "Кассовая заявка обновлена"
            VIEW-AS ALERT-BOX.
         END.      
      END.
      
/*    CREATE indocs.                                                                                                                */
/*	   ASSIGN                                                                                                                        */
/*	     indocs.class-code  = "eDocument"                                                                                            */
/*        indocs.contract    = "ЭД"                                                                                                 */
/*        indocs.filial-id   = shFilial                                                                                             */
/*        indocs.branch-id   = GetXAttrValueEx("_user",                                                                             */
/*                                         USERID("bisquit"),                                                                       */
/*                                         "Отделение",                                                                             */
/*                                         FGetSetting("КодФил",?,""))                                                              */
/*        indocs.user-id     = 'SYNC' /* USERID("bisquit") */                                                                       */
/*        indocs.create-date = clients_image.opentime                                                                               */
/*        indocs.exp-date    = (IF clients_image.closetime EQ ? THEN clients_image.opentime + 365 * 20 ELSE clients_image.closetime)*/
/*        indocs.doc-type    = "Файл"                                                                                               */
/*        indocs.reference   = "-Нет-"                                                                                              */
/*        indocs.parent-id   = 0                                                                                                    */
/*        indocs.file-name = "cust-corp"                                                                                            */
/*        indocs.doc-team  = (IF AVAIL glossary THEN glossary.sname ELSE "")                                                        */
/*        indocs.cust-cat = "Ю".                                                                                                    */
/*        indocs.cust-id  = cust-corp.cust-id.                                                                                      */
/*        indocs.surrogate  = STRING( cust-corp.cust-id)..                                                                          */
/*                                                                                                                                  */
/*        VALIDATE indocs NO-ERROR.                                                                                                 */

   END.
END.
HIDE FRAME frKZ.

{intrface.del}   

RETURN.

