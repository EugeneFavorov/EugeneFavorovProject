{globals.i}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

DEFINE TEMP-TABLE tt-names NO-UNDO
   FIELD id     AS CHARACTER
   FIELD fio    AS CHARACTER
   FIELD lname  AS CHARACTER
   FIELD fname  AS CHARACTER
   FIELD drold  AS CHARACTER
   FIELD bplace AS CHARACTER
   FIELD issue  AS CHARACTER
   FIELD addr1  AS CHARACTER
   FIELD addr2  AS CHARACTER
   FIELD drnew  AS DATE
   FIELD chdate AS DATE
   FIELD filial AS CHARACTER
   FIELD fioisp AS CHARACTER
   FIELD acct   AS LOGICAL.

DEFINE VARIABLE mFilials   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameDR    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInt       AS INT64     NO-UNDO.
DEFINE VARIABLE mNum       AS INT64     NO-UNDO.
DEFINE VARIABLE mFileName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mExistAcct AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cXL        AS CHARACTER NO-UNDO.

{getdates.i}

/*beg-date = DATE("07/12/2017").*/
/*end-date = DATE("07/12/2017").*/

MESSAGE "Формирование отчета..".

mFileName = "./" +
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(TIME,"hh:mm:ss")) + "-" + "rep-251.xml".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE(mFileName).

DEFINE VARIABLE mListDR AS CHARACTER NO-UNDO.

mListDR = "*first-names*,*name-last*,*birthday*,*birthplace*,*issue*,*address*". 

mInt = 0.
FOR EACH history WHERE TRUE 
   AND history.modify     EQ 'W'
   AND history.modif-date GE beg-date
   AND history.modif-date LE end-date
   AND history.file-name  EQ "person"
   AND CAN-DO(mListDR,history.field-value)
   /*AND history.field-ref  EQ "141692"*/
   NO-LOCK
   BY history.modif-date
   BY history.modif-time:

   FIND FIRST person
      WHERE person.person-id EQ INT64(history.field-ref)
      NO-LOCK NO-ERROR.
   IF AVAIL(person) THEN
   DO:
      mExistAcct = NO.
      FOR EACH acct
         WHERE acct.cust-cat EQ "Ч"
           AND acct.cust-id  EQ person.person-id
           AND CAN-DO("40802*,40817*,40820*,42108*-42114*,4230*,4260*",acct.acct)
         NO-LOCK:
         mExistAcct = YES.
         LEAVE.
      END.
      
/*      PUT UNFORMATTED history.field-value SKIP.*/

      IF mExistAcct THEN
      DO mNum = 1 TO NUM-ENTRIES(history.field-value) / 2:
         mNameDR = ENTRY(2 * mNum - 1,history.field-value).
         mValue  = ENTRY(2 * mNum,    history.field-value).
         
         mValue = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(mValue,CHR(1),""),CHR(2),","),CHR(3),""),CHR(4),""),CHR(5),"").

/*         PUT UNFORMATTED mNameDR ";" mValue SKIP.*/

         IF    (mNameDR EQ "*first-names"
             OR mNameDR EQ "*name-last"
             OR mNameDR EQ "birthday"
             OR mNameDR EQ "*birthplace"
             OR mNameDR EQ "issue"
             OR mNameDR EQ "address[1]"
             OR mNameDR EQ "address[2]")
            AND mValue  NE ""
         THEN DO:
            mFilials = "".
            FOR EACH cust-role
               WHERE cust-role.file-name  NE "loan"
                 AND cust-role.cust-cat   EQ "Ч"
                 AND cust-role.cust-id    EQ STRING(person.person-id)
                 AND cust-role.Class-Code EQ "ImaginClient"
               NO-LOCK:

               mFilials = mFilials + "," + cust-role.surrogate.
            END.
            mFilials = TRIM(mFilials,",").

            FIND FIRST _user
                WHERE (_user._userid = history.user-id)
                NO-LOCK NO-ERROR.

            CREATE tt-names.
            ASSIGN
               mInt            = mInt + 1
               tt-names.id     = STRING(person.person-id)
               tt-names.fio    = person.name-last + " " + person.first-names
               tt-names.chdate = history.modif-date
               tt-names.filial = mFilials
               tt-names.acct   = mExistAcct
               tt-names.drnew  = person.birthday
               tt-names.drold  = ""
               tt-names.fname  = ""
               tt-names.lname  = ""
               tt-names.fioisp = IF (AVAIL _user) THEN _user._user-name ELSE history.user-id.

            CASE mNameDR:
               WHEN "*name-last"   THEN tt-names.lname  = mValue.
               WHEN "*first-names" THEN tt-names.fname  = mValue.
               WHEN "birthday"     THEN tt-names.drold  = mValue.
               WHEN "*birthplace"  THEN tt-names.bplace = mValue.
               WHEN "issue"        THEN tt-names.issue  = mValue.
               WHEN "address[1]"   THEN tt-names.addr1  = mValue.
               WHEN "address[2]"   THEN tt-names.addr2  = mValue.
            END CASE.
         END.
      END.
   END.
END.

IF (mInt = 0)
THEN MESSAGE "Нет данных для отчета.".
ELSE DO:
   PUT UNFORMATTED XLHead("Изменения", "ICCDCCDCCCDCC", "40,55,300,71,100,160,111,250,250,250,80,100,120").
   cXL = XLCellHat('Отчет по внесению изменений в поля карточки клиента'
       + " за период с " + STRING(beg-date, "99.99.9999") + " по " + STRING(end-date, "99.99.9999"),9).
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   cXL = XLCellHead("№",0,0,0)
       + XLCellHead("ID",0,0,0)
       + XLCellHead("Фамилия Имя Отчество",0,0,0)
       + XLCellHead("Дата рождения",0,0,0)
       + XLCellHead("Фамилия~nдо внесения изменений",0,0,0)
       + XLCellHead("Имя Отчество~nдо внесения изменений",0,0,0)
       + XLCellHead("Дата рождения~nдо внесения изменений",0,0,0)
       + XLCellHead("Место рождения~nдо внесения изменений",0,0,0)
       + XLCellHead("Паспорт выдан~nдо внесения изменений",0,0,0)
       + XLCellHead("Адрес 1~nдо внесения изменений",0,0,0)
       + XLCellHead("Дата внесения изменений",0,0,0)
       + XLCellHead("Филиалы",0,0,0)
       + XLCellHead("ФИО исполнителя",0,0,0)
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

   mInt = 0.
   FOR EACH tt-names WHERE
      NO-LOCK:

      mInt = mInt + 1.
      cXL = XLNumCell(mInt)
          + XLCell(tt-names.id)
          + XLCellWrap(tt-names.fio)
          + XLDateCell(tt-names.drnew)
          + XLCell(tt-names.lname)
          + XLCell(tt-names.fname)
          + XLDateCell(DATE(tt-names.drold))
          + XLCell(tt-names.bplace)
          + XLCell(tt-names.issue)
          + XLCell(tt-names.addr1)
          + XLDateCell(tt-names.chdate)
          + XLCell(tt-names.filial)
          + XLCell(tt-names.fioisp)
          NO-ERROR.
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.

   PUT UNFORMATTED XLEnd().
   RUN sndbispc ("file=" + mFileName + ";class=bq").
END.

OUTPUT CLOSE.
MESSAGE "Отчет готов.".
