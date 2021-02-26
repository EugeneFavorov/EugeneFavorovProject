{globals.i}
{intrface.get pbase}
{intrface.get pack}
{intrface.get exch}
{intrface.get trans}
{intrface.get xclass}
{intrface.get acct}
{intrface.get cust}
{intrface.get tmcod}
{intrface.get db2l}
{parsin.def}
{sh-defs.i}

{tmprecid.def}

DEFINE VARIABLE mCnt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt   AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2  AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3  AS INT64     NO-UNDO.
DEFINE VARIABLE mNom   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSymb  AS CHARACTER NO-UNDO.
DEFINE BUFFER b-loan FOR loan.
DEFINE VARIABLE mFIO  AS CHARACTER NO-UNDO.

ETIME(YES).

DEFINE VARIABLE mString     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mName       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPasp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOGRN       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtsenka    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskIP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskUL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSkip       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE  tt-cli
   FIELD cust-cat  AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD uid       AS INT64.

DEFINE VARIABLE mTxt       AS CHARACTER  NO-UNDO.

{setdest2.i &option="UNBUFFERED CONVERT TARGET 'UTF-8' SOURCE 'IBM866'" &filename="550p.xml" }

/*
LegCBROtk_mart.MEMBERTYPE   = Тип участника     1-ЮЛ, 2-ФЛ, 3-ИП, 4-ФЛЧП, 5-ИНБОЮЛ
LegCBROtk_mart.MEMBERSIGN   = Признак участника 1-резидент, 0-нерезидент
LegCBROtk_mart.MEMBERSTATUS = Статус участника  1-клиент,   2-контрагент
*/

{empty tt-cli}

mInt = 0.
FOR EACH LegCBROtk_mart WHERE TRUE
/*   AND LegCBROtk_mart.UID EQ 465595 /*58862*/*/
   NO-LOCK:
   /*ФЛ*/
   IF CAN-DO("2",LegCBROtk_mart.MEMBERTYPE) THEN
   DO:
      FOR EACH person WHERE TRUE
         AND person.name-last   EQ LegCBROtk_mart.SECONDNAME
         AND person.first-names EQ LegCBROtk_mart.NAME + " " + LegCBROtk_mart.MIDDLENAME
         NO-LOCK:
         IF person.birthday EQ LegCBROtk_mart.BIRTHDATE THEN
         DO:
/*            mPasp = "".                                                       */
/*            FOR EACH cust-ident WHERE TRUE                                    */
/*               AND (cust-ident.close-date EQ ?                                */
/*                 OR cust-ident.close-date GE TODAY)                           */
/*               AND cust-ident.class-code  EQ "p-cust-ident"                   */
/*               AND cust-ident.cust-cat    EQ "Ч"                              */
/*               AND cust-ident.cust-id     EQ person.person-id                 */
/*               NO-LOCK:                                                       */
/*               mPasp = cust-ident.cust-code-type + " " + cust-ident.cust-code.*/
/*            END.                                                              */
            FIND FIRST tt-cli WHERE TRUE
               AND tt-cli.cust-id  EQ person.person-id
               AND tt-cli.cust-cat EQ "Ч"
            NO-LOCK NO-ERROR.
            IF NOT AVAIL(tt-cli) THEN
            DO:
               CREATE tt-cli.
               ASSIGN
                  tt-cli.cust-id  = person.person-id
                  tt-cli.cust-cat = "Ч"
                  tt-cli.uid      = LegCBROtk_mart.UID.
            END.
         END.
      END.
   END.
	/*ИП*/   
   mInt = 0.
   IF CAN-DO("3,4,5",LegCBROtk_mart.MEMBERTYPE) THEN
   DO:
      FOR EACH person WHERE TRUE
         AND person.name-last   EQ LegCBROtk_mart.SECONDNAME
         AND person.first-names EQ LegCBROtk_mart.NAME + " " + LegCBROtk_mart.MIDDLENAME
         AND person.inn         NE ""
         NO-LOCK:
         
         IF NOT (GetXattrValueEx("person",STRING(person.person-id),"Предпр", "") EQ "Предпр") THEN NEXT.
         IF NOT (GetXattrValueEx("person",STRING(person.person-id),"Субъект","") EQ "ФЛП")    THEN NEXT. 
         
/*         PUT UNFORMATTED                                                       */
/*            LegCBROtk_mart.INN ";"                                             */
/*            person.inn ";"                                                     */
/*            GetXattrValueEx("person",STRING(person.person-id),"Предпр","") ";" */
/*            GetXattrValueEx("person",STRING(person.person-id),"Субъект","") ";"*/
/*         SKIP.                                                                 */
            
         IF person.inn EQ LegCBROtk_mart.INN THEN
         DO:
            FIND FIRST tt-cli WHERE TRUE
               AND tt-cli.cust-id  EQ person.person-id
               AND tt-cli.cust-cat EQ "Ч"
            NO-LOCK NO-ERROR.
            IF NOT AVAIL(tt-cli) THEN
            DO:
               CREATE tt-cli.
               ASSIGN
                  tt-cli.cust-id  = person.person-id
                  tt-cli.cust-cat = "Ч"
                  tt-cli.uid      = LegCBROtk_mart.UID.
            END.
         END.
      END.
   END.
	/*ЮЛ*/
   IF LegCBROtk_mart.MEMBERTYPE EQ "1" THEN
   DO:
      FOR EACH cust-corp WHERE TRUE
         AND CAN-DO("*",cust-corp.country-id)
         AND cust-corp.inn EQ LegCBROtk_mart.INN
         NO-LOCK:
         ASSIGN
            mKPP  = ""
            mOGRN = "".
      	/*FOR EACH tmpsigns WHERE TRUE                          */
      	/*   AND tmpsigns.file-name EQ "cust-corp"              */
      	/*   AND tmpsigns.surrogate EQ STRING(cust-corp.cust-id)*/
      	/*   NO-LOCK:                                           */
      	/*   IF tmpsigns.code EQ "КПП"                          */
      	/*      THEN mKPP = tmpsigns.xattr-value.               */
      	/*END.                                                  */
      	/*FOR EACH signs WHERE TRUE                             */
      	/*   AND signs.file-name EQ "cust-corp"                 */
      	/*   AND signs.surrogate EQ STRING(cust-corp.cust-id)   */
      	/*   NO-LOCK:                                           */
      	/*   IF signs.code EQ "ОГРН"                            */
      	/*      THEN mOGRN = signs.code-value.                  */
      	/*END.                                                  */
         FIND FIRST tt-cli WHERE TRUE
            AND tt-cli.cust-id  EQ cust-corp.cust-id
            AND tt-cli.cust-cat EQ "Ю"
         NO-LOCK NO-ERROR.
         IF NOT AVAIL(tt-cli) THEN
         DO:
            CREATE tt-cli.
            ASSIGN
               tt-cli.cust-id  = cust-corp.cust-id
               tt-cli.cust-cat = "Ю"
               tt-cli.uid      = LegCBROtk_mart.UID.
         END.
      END.
   END.
END.

/*
FOR EACH tt-cli WHERE TRUE
   AND tt-cli.cust-cat EQ "Ч" NO-LOCK,
   FIRST LegCBROtk_mart WHERE TRUE
   AND LegCBROtk_mart.UID EQ tt-cli.uid
   NO-LOCK,
   FIRST person WHERE TRUE
   AND person.person-id EQ tt-cli.cust-id
   NO-LOCK:
      
   PUT UNFORMATTED
      tt-cli.cust-cat    ";"
      tt-cli.cust-id     ";"
      LegCBROtk_mart.INN ";"
      person.inn         ";"
      GetXattrValueEx("person",STRING(person.person-id),"Предпр","") ";"
      GetXattrValueEx("person",STRING(person.person-id),"Субъект","") ";"
   SKIP.
END.
FOR EACH tt-cli WHERE TRUE
   AND tt-cli.cust-cat EQ "Ю" NO-LOCK,
   FIRST LegCBROtk_mart WHERE TRUE
   AND LegCBROtk_mart.UID EQ tt-cli.uid
   NO-LOCK,
   FIRST cust-corp WHERE TRUE
   AND cust-corp.cust-id EQ tt-cli.cust-id
   NO-LOCK:
   PUT UNFORMATTED
      tt-cli.cust-cat    ";"
      tt-cli.cust-id     ";"
      LegCBROtk_mart.INN ";"
      cust-corp.inn      ";"
   SKIP.
END.
*/

{find-match.hdr}

mInt = 0.
FOR EACH tt-cli 
   NO-LOCK,
   FIRST LegCBROtk_mart WHERE TRUE
   AND LegCBROtk_mart.UID EQ tt-cli.uid
   NO-LOCK:
   RELEASE acct. 
   FOR EACH acct WHERE TRUE
      AND acct.cust-cat   EQ tt-cli.cust-cat
      AND acct.cust-id    EQ tt-cli.cust-id
      AND acct.acct-cat   EQ "b"
      AND acct.close-date EQ ?
      NO-LOCK:
      LEAVE.  
   END.
   IF AVAIL(acct) THEN
   DO:
      /*LegCBROtk_mart.MEMBERTYPE   = Тип участника     1-ЮЛ, 2-ФЛ, 3-ИП, 4-ФЛЧП, 5-ИНБОЮЛ*/
      mOtsenka = "".
      mRiskIP  = "".
      mRiskFL  = "".
      mRiskUL  = "".
      mSkip    = NO.
      IF LegCBROtk_mart.MEMBERTYPE EQ "2" 
      OR LegCBROtk_mart.MEMBERTYPE EQ "3"
      OR LegCBROtk_mart.MEMBERTYPE EQ "4" THEN
      DO:
         FOR EACH tmpsigns WHERE TRUE
      	   AND tmpsigns.file-name  EQ "person"
      	   AND tmpsigns.surrogate  EQ STRING(tt-cli.cust-id)
      	   NO-LOCK BY tmpsigns.since:
            IF tmpsigns.code EQ "ОценкаРиска" THEN mOtsenka = tmpsigns.xattr-value.
            IF tmpsigns.code EQ "РискОтмывИП" THEN mRiskIP  = tmpsigns.code-value.
            IF tmpsigns.code EQ "РискОтмыв"   THEN mRiskFL  = tmpsigns.code-value.
      	END.
      	IF LegCBROtk_mart.MEMBERTYPE EQ "2" THEN
         DO:
            IF INDEX(mOtsenka,"550-П") NE 0
      	   AND mRiskFL EQ "высокий" THEN mSkip = YES.
         END.
         ELSE
         DO:
         	IF INDEX(mOtsenka,"550-П") NE 0
         	AND mRiskIP EQ "высокий" THEN mSkip = YES.
      	END. 
   	END.
      IF LegCBROtk_mart.MEMBERTYPE EQ "1" THEN
      DO:
         FOR EACH tmpsigns WHERE TRUE
      	   AND tmpsigns.file-name  EQ "cust-corp"
      	   AND tmpsigns.surrogate  EQ STRING(tt-cli.cust-id)
      	   NO-LOCK BY tmpsigns.since:
            IF tmpsigns.code EQ "ОценкаРиска" THEN mOtsenka = tmpsigns.xattr-value.
            IF tmpsigns.code EQ "РискОтмыв"   THEN mRiskUL  = tmpsigns.code-value.
      	END.
      	IF INDEX(mOtsenka,"550-П") NE 0
         	AND mRiskUL EQ "высокий" THEN mSkip = YES.
      END.
      mCustID = "".
      mINN    = "".
   	IF mSkip EQ NO THEN
      DO:
         /*№	Наименование ЮЛ /ФИО ИП,ФЛ	ИНН	Код клиента в БИС */
         mInt    = mInt + 1.
         mName   = IF tt-cli.cust-cat EQ "Ю" 
                   THEN LegCBROtk_mart.CUST-NAME 
                   ELSE /*LegCBROtk_mart.MEMBERTYPE + " " + */ LegCBROtk_mart.SECONDNAME + " " + LegCBROtk_mart.NAME + " " + LegCBROtk_mart.MIDDLENAME.
         mCustID = IF CAN-DO("2,3,4,5",LegCBROtk_mart.MEMBERTYPE) THEN STRING(tt-cli.cust-id) ELSE "".
         mINN    = IF CAN-DO("1,3,4,5",LegCBROtk_mart.MEMBERTYPE) THEN LegCBROtk_mart.INN     ELSE "".
         PUT UNFORMATTED
            '   <Row>~n' +
            '    <Cell><Data ss:Type="Number">' + STRING(mInt)  + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mName)   + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mINN)    + '</Data></Cell>~n' +
            '    <Cell><Data ss:Type="String">' + TRIM(mCustID) + '</Data></Cell>~n' +
            '   </Row>~n'.
      END.
   END. 
END.

{find-match.ftr}

/*{preview2.i &filename="550p.xml" }*/

OUTPUT CLOSE.

RUN sndbispc ("file=" + "550p.xml" + ";class=bq").

{intrface.del}

RETURN.
