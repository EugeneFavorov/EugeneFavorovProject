{globals.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

ETIME(YES).

DEFINE TEMP-TABLE tt-names NO-UNDO
   FIELD id     AS CHARACTER
   FIELD fio    AS CHARACTER
   FIELD lname  AS CHARACTER
   FIELD fname  AS CHARACTER
   FIELD drold  AS CHARACTER
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
MESSAGE "��ନ஢���� ����..".

mFileName = "./" +
   STRING(YEAR(TODAY),"9999") + "-" +
   STRING(MONTH(TODAY),"99") + "-" +
   STRING(DAY(TODAY),"99") + "-" + TRIM(STRING(TIME,"hh:mm:ss")) + "-" + "rep-251.xml".
mFileName = REPLACE(mFileName,":","").

OUTPUT TO VALUE(mFileName).

mInt = 0.
FOR EACH history
   WHERE history.modify     EQ 'W'
     AND history.modif-date GE beg-date
     AND history.modif-date LE end-date
     AND history.file-name  EQ "person"
     AND CAN-DO('*first-names*,*name-last*,*birthday*',history.field-value)
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
         WHERE acct.cust-cat EQ "�"
           AND acct.cust-id  EQ person.person-id
           AND CAN-DO("40802*,40817*,40820*,42108*-42114*,4230*,4260*",acct.acct)
         NO-LOCK:

         mExistAcct = YES.
         LEAVE.
      END.

      IF mExistAcct THEN
      DO mNum = 1 TO NUM-ENTRIES(history.field-value) / 2:
         mNameDR = ENTRY(2 * mNum - 1,history.field-value).
         mValue  = ENTRY(2 * mNum,    history.field-value).

         IF    (mNameDR EQ "*first-names"
             OR mNameDR EQ "*name-last"
             OR mNameDR EQ "birthday")
            AND mValue  NE ""
         THEN DO:
            mFilials = "".
            FOR EACH cust-role
               WHERE cust-role.file-name  NE "loan"
                 AND cust-role.cust-cat   EQ "�"
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
               WHEN "*name-last"   THEN tt-names.lname = mValue.
               WHEN "*first-names" THEN tt-names.fname = mValue.
               WHEN "birthday"     THEN tt-names.drold = mValue.
            END CASE.
         END.
      END.
   END.
END.

IF (mInt = 0)
THEN MESSAGE "��� ������ ��� ����.".
ELSE DO:
   PUT UNFORMATTED XLHead("���������", "ICCDCCDDCC", "40,55,300,71,100,160,111,80,100,120").
   cXL = XLCellHat('���� �� ���ᥭ�� ��������� � ���� ��� � ���� ஦�����'
       + " �� ��ਮ� � " + STRING(beg-date, "99.99.9999") + " �� " + STRING(end-date, "99.99.9999"),9).
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   cXL = XLCellHead("�",0,0,0)
       + XLCellHead("ID",0,0,0)
       + XLCellHead("������� ��� ����⢮",0,0,0)
       + XLCellHead("��� ஦�����",0,0,0)
       + XLCellHead("�������~n�� ���ᥭ�� ���������",0,0,0)
       + XLCellHead("��� ����⢮~n�� ���ᥭ�� ���������",0,0,0)
       + XLCellHead("��� ஦�����~n�� ���ᥭ�� ���������",0,0,0)
       + XLCellHead("��� ���ᥭ�� ���������",0,0,0)
       + XLCellHead("�������",0,0,0)
       + XLCellHead("��� �ᯮ���⥫�",0,0,0)
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
          + XLDateCell(tt-names.chdate)
          + XLCell(tt-names.filial)
          + XLCell(tt-names.fioisp)
          NO-ERROR.
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   END.

   PUT UNFORMATTED XLEnd().
   OUTPUT CLOSE.
   MESSAGE "���� ��⮢.".
   RUN sndbispc ("file=" + mFileName + ";class=bq").
END.
