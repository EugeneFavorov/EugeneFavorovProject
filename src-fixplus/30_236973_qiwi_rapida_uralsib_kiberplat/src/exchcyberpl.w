&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

               Банковская интегрированная система QBIS
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: exchcyberpl.w
      Comment: Прием запросов от КиберПлат
   Parameters: 
         Uses:
      Used by:
      Created: 14.05.2015 KMBIS

------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/*=== По аналогии с runsched.p ===*/

DEF NEW GLOBAL SHARED VAR h_cache AS HANDLE     NO-UNDO.
RUN pp-cache.p PERSIST SET h_cache.
{bislogin.i}

/*================================*/

{intrface.get xclass}
{intrface.get rights}

{intrface.get pbase}
{intrface.get tmess}

/* ***************************  Definitions  ************************** */


/*=== По аналогии с runsched.p ===*/

{filial.pro}

DEF VAR delay     AS INT64  NO-UNDO.
DEF VAR servpath  AS CHAR   NO-UNDO.
DEF VAR mTimeZn   AS INT64  NO-UNDO.
DEF VAR mDateZn   AS DATE   NO-UNDO.
DEF VAR mFililId  AS CHAR   NO-UNDO.

mFililId = getThisUserXAttrValue("filial-id").
RUN SetConnectLink (mFililId).
RUN SetEnvironment (mFililId).

/*================================*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.15
         WIDTH              = 60.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ************************  Main Code Block  *********************** */

/* Process the latest Web event. */

RUN process-web-request IN THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputHeader Procedure 
PROCEDURE outputHeader :
/*------------------------------------------------------------------------------
  Purpose:     Output the MIME header, and any "cookie" information needed 
               by this procedure.  
  Parameters:  <none>
  Notes:       In the event that this Web object is state-aware, this is
               a good place to set the webState and webTimeout attributes.
------------------------------------------------------------------------------*/

  /* To make this a state-aware Web object, pass in the timeout period 
   * (in minutes) before running outputContentType.  If you supply a timeout 
   * period greater than 0, the Web object becomes state-aware and the 
   * following happens:
   *
   *   - 4GL variables webState and webTimeout are set
   *   - a cookie is created for the broker to id the client on the return trip
   *   - a cookie is created to id the correct procedure on the return trip
   *
   * If you supply a timeout period less than 1, the following happens:
   *
   *   - 4GL variables webState and webTimeout are set to an empty string
   *   - a cookie is killed for the broker to id the client on the return trip
   *   - a cookie is killed to id the correct procedure on the return trip
   *
   * Example: Timeout period of 5 minutes for this Web object.
   *
   *   setWebState (5.0).
   */
    
  /* 
   * Output additional cookie information here before running outputContentType.
   *      For more information about the Netscape Cookie Specification, see
   *      http://home.netscape.com/newsref/std/cookie_spec.html  
   *   
   *      Name         - name of the cookie
   *      Value        - value of the cookie
   *      Expires date - Date to expire (optional). See TODAY function.
   *      Expires time - Time to expire (optional). See TIME function.
   *      Path         - Override default URL path (optional)
   *      Domain       - Override default domain (optional)
   *      Secure       - "secure" or unknown (optional)
   * 
   *      The following example sets cust-num=23 and expires tomorrow at (about) the 
   *      same time but only for secure (https) connections.
   *      
   *      RUN SetCookie IN web-utilities-hdl 
   *        ("custNum":U, "23":U, TODAY + 1, TIME, ?, ?, "secure":U).
   */ 
   output-http-header ("Content-Type":U, "text/xml; charset=windows-1251").
   output-http-header ("", ""). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-web-request Procedure 
PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   ETIME(yes). */

   RUN outputHeader IN THIS-PROCEDURE.

   DEF VAR vOnlineReq  AS  HANDLE NO-UNDO.
   DEF VAR vFldList    AS  CHAR   NO-UNDO.
   DEF VAR vFldName    AS  CHAR   NO-UNDO.
   DEF VAR vTmpStr     AS  CHAR   NO-UNDO.
   DEF VAR vI          AS  INT64  NO-UNDO.

   CREATE TEMP-TABLE vOnlineReq.
   /* Получаем список параметров в запросе */
   vFldList = get-value(?).

   IF {assigned vFldList} THEN
   DO:
      /* Создаем временную таблицу с полями запроса и их значениями */
      lTTCreat:
      DO vI = 1 TO NUM-ENTRIES(vFldList):

         vFldName = ENTRY(vI, vFldList).
         IF NOT {assigned vFldName} THEN
            NEXT lTTCreat.
         /* Получаем значение поля */
         vTmpStr = get-value(vFldName).
         vOnlineReq:ADD-NEW-FIELD(vFldName,                                    /* Имя поля     */
                                  "CHAR",                                      /* Тип          */
                                  0,                                           /* игнорируем   */
                                  SUBST("x(&1)", STRING(LENGTH(vTmpStr) + 1)), /* Формат поля  */
                                  vTmpStr).                                    /* Нач.значение */
      END. /* lTTCreat: DO vI = 1 TO NUM-ENTRIES(vFldList): */
   END. /* IF {assigned vFldList} THEN */

   /* Получаем сам запрос */
   vTmpStr = get-cgi("QUERY_STRING").
   IF vTmpStr EQ ? THEN
      vTmpStr = "".
   /* Сохраняем запрос в отдельную переменную */
   vOnlineReq:ADD-NEW-FIELD("OnlineReq", 
                            "CHAR", 
                            0, 
                            SUBST("x(&1)", STRING(LENGTH(vTmpStr) + 1)),
                            vTmpStr).

   /* Сохраняем изменения в нашей таблице */
   vOnlineReq:TEMP-TABLE-PREPARE("tt-OnlineReq").
   ASSIGN
      gRemote = YES
      auto    = YES
   .
   /* Вызываем процедуру обмена */
   RUN i-cyberpl.p (INPUT vOnlineReq, OUTPUT vTmpStr) NO-ERROR.
                     
   IF vTmpStr EQ ? THEN 
      vTmpStr = "".

   {&OUT} vTmpStr {&END}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
