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
{intrface.get strng}
{intrface.get count}
{intrface.get tmess}

{parsin.def}
{sh-defs.i}
{wordwrap.def}
{stoplist.fun }
{sv-temp.i NEW}
{norm.i NEW}

{topkind.def}

DEFINE TEMP-TABLE tt-chkfldval NO-UNDO
   FIELD fldnum  AS INT64 
   FIELD fld-val AS CHAR
   FIELD fld-add AS CHAR
INDEX fldnum fldnum.

DEFINE TEMP-TABLE tt-FldVal NO-UNDO
   FIELD val AS CHAR.

DEFINE TEMP-TABLE tt-FindDelStop NO-UNDO
   FIELD fldtype   AS CHARACTER  /*тип*/
   FIELD fldcode   AS CHARACTER  /*код*/
   FIELD fldname   AS CHARACTER  /*ФИО*/
   FIELD fldcoun   AS CHARACTER  /*страна*/
   FIELD fldinn    AS CHARACTER  /*ИНН*/
   FIELD fldpas    AS CHARACTER  /*паспорт*/
   FIELD fldbday   AS DATE       /*дата рождения*/
   FIELD fldbplace AS CHARACTER  /*место рождения*/
   FIELD fldprim   AS CHARACTER  /*примечание*/
   FIELD fldstlst  AS CHARACTER  /*стоп-лист*/
   FIELD fldnumrez AS CHARACTER  /*номер резолюции*/
   FIELD flddatrez AS DATE       /*дата резолюции*/
   FIELD flddatadd AS DATE       /*дата добавления*/
   FIELD flduser   AS CHARACTER  /*пользователь*/
.

DEFINE TEMP-TABLE  tt-inf
   FIELD cid       AS INT64
   FIELD cust-cat  AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD number    AS CHARACTER
   FIELD inn       AS CHARACTER
   FIELD an3       AS CHARACTER
   FIELD txt       AS CHARACTER
   FIELD lname     AS CHARACTER
   FIELD linetxt   AS CHARACTER
   FIELD document  AS CHARACTER
.

DEFINE BUFFER u-signs FOR signs.
DEFINE BUFFER f-signs FOR signs.
DEFINE BUFFER b-code  FOR code.

ETIME(YES).

DEFINE VARIABLE iPar          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mInt          AS INT64      NO-UNDO.
DEFINE VARIABLE mDataID       AS INT64      NO-UNDO.
DEFINE VARIABLE mDataRez      AS DATE       NO-UNDO.
DEFINE VARIABLE mNumRez       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mTxtLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mFldNames     AS CHARACTER  NO-UNDO INIT "Тип,Код,ФИО,Страна,ИНН,Паспорт,Примечание,,ДР,МР".
DEFINE VARIABLE mFldIndex     AS CHARACTER  NO-UNDO EXTENT 10.
DEFINE VARIABLE mI            AS INT64      NO-UNDO.
DEFINE VARIABLE mInd          AS INT64      NO-UNDO.
DEFINE VARIABLE mStartStr     AS INT64      NO-UNDO.
DEFINE VARIABLE mDelimeter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mFileExtent   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCLType       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCLNum        AS INT64      NO-UNDO.
DEFINE VARIABLE mSLName       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mNPName       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mExchRule     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mExchParam    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAddSym       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCnt          AS INT64      NO-UNDO.
DEFINE VARIABLE mNum          AS INT64      NO-UNDO.
DEFINE VARIABLE mNeedDel      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mNeedIskl     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mRid          AS INT64      NO-UNDO.
DEFINE VARIABLE mBirthDay     AS DATE       NO-UNDO.
DEFINE VARIABLE mBirthPlace   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mTmpStr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mString       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAnalizOFM    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDatAnalizOFM AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mA3           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mFraza        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL    NO-UNDO.

DEFINE VARIABLE vJ           AS INT64     NO-UNDO.
DEFINE VARIABLE vJ1          AS INT64     NO-UNDO.
DEFINE VARIABLE vI           AS INT64     NO-UNDO.
DEFINE VARIABLE vTmp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vInd1        AS INT64     NO-UNDO.
DEFINE VARIABLE vInd2        AS INT64     NO-UNDO.
DEFINE VARIABLE vTmpStr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOneFldInd   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vBirthDay    AS DATE      NO-UNDO.
DEFINE VARIABLE vBirthPlace  AS CHARACTER NO-UNDO.

{chk_ufm.pro}

mFraza = "Операции с согласования УФМ".

FORM mFraza WITH FRAME frPrim.

FORM 
   mFraza   
   FORMAT "x(300)"         
   VIEW-AS FILL-IN SIZE 50 BY 1 LABEL "Введите текст" 
   SKIP
WITH FRAME frPrim TITLE " Текст примечания " 
CENTERED KEEP-TAB-ORDER OVERLAY ROW 10 SIDE-LABELS.

ON 'END-ERROR' OF FRAME frPrim
DO:
   HIDE FRAME frPrim.
END.

PAUSE 0.
UPDATE
   mFraza
WITH FRAME frPrim.
HIDE FRAME frPrim.

mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.

iPar   = "ImpXlsUFM".
mA3    = "".
      
/*Разбираем параметры обмена*/
ASSIGN
   mNPName    = iPar
   mExchParam = FGetSetting("Стоп-листы", iPar, "")
   mDelimeter = ";".

IF NUM-ENTRIES(mExchParam,";") LT 5  THEN
DO:
   MESSAGE  "Не правильно заполнен настроечный параметр " + QUOTER(mNPName)     VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   mSLName   = ENTRY(1,mExchParam,";")                 /*вид стоп-листа*/
   mStartStr = INT64(ENTRY(2,mExchParam,";"))          /*с какой строки начинать загрузку*/
   mExchRule = ENTRY(3,mExchParam,";")                 /*правило обмена*/
   mAddSym   = ENTRY(4,mExchParam,";")                 /*дополнительные символы к полям*/
   mNeedDel  = ENTRY(5,mExchParam,";") NE "Доб"        /*Добавить/удалить*/
   mNeedIskl = ENTRY(5,mExchParam,";") BEGINS "Искл"
NO-ERROR.

/*Разбираем правило обмена*/
DO mI = 1 TO NUM-ENTRIES(mExchRule):
  mTmpStr = GetEntries (mI,mExchRule,",","").
  IF NUM-ENTRIES(mTmpStr, "|") EQ 2 THEN
  DO:
     mInd = LOOKUP(GetEntries (2,mTmpStr, "|",""),mFldNames).
     IF mInd GT 0 AND 
        mInd LE 10 
     THEN
     DO:
        mFldIndex[mInd] = mFldIndex[mInd] +  IF mFldIndex[mInd] EQ  ""  THEN GetEntries (1,mTmpStr, "|","")  ELSE ("," + GetEntries (1,mTmpStr, "|","")) NO-ERROR.  /*??*/
     END.
  END.                                           
END.

DEF VAR ffname AS CHAR VIEW-AS FILL-IN NO-UNDO.
DEF VAR fstr AS CHAR INIT '' NO-UNDO.

ffname = '*.csv'.

{getfile.i &filename=ffname}

IF SEARCH (fname) = ? THEN DO:
	MESSAGE ("Файл не найден " + fname) VIEW-AS ALERT-BOX.
	RETURN.
END.

INPUT FROM VALUE(fname) CONVERT TARGET "ibm866"  SOURCE "1251".

mInt = 0.
REPEAT:
   IMPORT UNFORMATTED mString.
   IF mString NE "" THEN
   DO:
      mInt = mInt + 1.
      CREATE tt-inf.
      ASSIGN
         tt-inf.lname = REPLACE(REPLACE(LEFT-TRIM(ENTRY(1,mString,";"),'"'),'"""','"'),'""','"') 
         tt-inf.inn   = ENTRY(2,mString,";")
         tt-inf.an3   = mA3
         tt-inf.txt   = mFraza.
      /*PUT UNFORMATTED mInt ";" tt-inf.inn ";" tt-inf.lname SKIP.*/
   END.
END.
INPUT CLOSE.

{setdest.i &filename = "'chk_ufm.log'"}

mInt = 0.
FOR EACH tt-inf EXCLUSIVE-LOCK:
   
   mInt = mInt + 1.
   IF {assigned tt-inf.inn} THEN
   DO:
      IF LENGTH(tt-inf.inn) EQ 12 THEN
      DO:
         mCLType       = "Ч".
         mAnalizOFM    = "".
         mDatAnalizOFM = "".
         FOR EACH person WHERE
            person.inn = tt-inf.inn
         NO-LOCK:
         ASSIGN
            tt-inf.cust-cat = "Ч"
            tt-inf.cust-id  = person.person-id
            tt-inf.document = REPLACE(person.document," ","")
            mCLNum          = person.person-id
            mAnalizOFM      = GetXattrValueEx("person",STRING(person.person-id),"АнализОФМ","").
         END.
         IF tt-inf.an3 NE "" AND mAnalizOFM NE tt-inf.an3 THEN
         DO:
            mOk = UpdateSigns("person",
      	      STRING(tt-inf.cust-id),
      	      "ДатАнализОФМ",
      	      STRING(TODAY,"99/99/9999"),
      	      ?).
      	   mDatAnalizOFM = GetXattrValueEx("person",STRING(tt-inf.cust-id),"ДатАнализОФМ","").
      	   
         	mOk = UpdateSigns("person",
         	      STRING(tt-inf.cust-id),
         	      "АнализОФМ",
         	      tt-inf.an3,
         	      ?).
            PUT UNFORMATTED "Установлен АнализОФМ " + mAnalizOFM + " : " + mDatAnalizOFM + " " + tt-inf.inn " " tt-inf.lname  SKIP.
         END.
         ELSE
         DO:
            IF tt-inf.an3 NE "" THEN
            PUT UNFORMATTED "АнализОФМ уже " + mAnalizOFM + "        : " tt-inf.inn " " tt-inf.lname  SKIP.
         END.
         
         FOR EACH DataLine WHERE TRUE
            AND DataLine.Data-ID EQ mDataID
            AND DataLine.Sym3    EQ tt-inf.inn
            NO-LOCK:
            LEAVE.
         END.
         IF NOT AVAIL(DataLine) THEN
         DO:
            mTxtLine = TRIM(STRING(mInt)) + ";" + tt-inf.lname + ";" + tt-inf.inn + ";" + tt-inf.document + ";" + tt-inf.txt + ";".
            PUT UNFORMATTED "Добавлен в Стоп-Лист   : " tt-inf.inn " " tt-inf.lname  SKIP.
         	RUN CreateStopList(mTxtLine).
         END.
         ELSE
         DO:
            FOR EACH b-code WHERE TRUE
               AND b-code.class   EQ "StopList"
               AND b-code.parent  EQ "StopList"
               AND b-code.misc[5] EQ tt-inf.inn
               EXCLUSIVE-LOCK:
               b-code.misc[7] = b-code.misc[7] + "; " + tt-inf.txt.
            END. 
            PUT UNFORMATTED "Добавлено примечание   : " tt-inf.inn " " tt-inf.lname  SKIP.
         END.
      END. /*12*/
      IF LENGTH(tt-inf.inn) EQ 20 THEN
      DO:
         mCLType       = "Ч".
         mAnalizOFM    = "".
         mDatAnalizOFM = "".
         FIND FIRST acct WHERE TRUE
            AND acct.number EQ tt-inf.inn
         NO-LOCK NO-ERROR.
         IF AVAIL(acct) THEN
         DO:
            FOR EACH person WHERE
               person.person-id EQ acct.cust-id
            NO-LOCK:
            ASSIGN
               tt-inf.cust-cat = "Ч"
               tt-inf.cust-id  = person.person-id
               tt-inf.document = REPLACE(person.document," ","")
               mCLNum          = person.person-id
               mAnalizOFM      = GetXattrValueEx("person",STRING(person.person-id),"АнализОФМ","").
            END.
            IF tt-inf.an3 NE "" AND mAnalizOFM NE tt-inf.an3 THEN
            DO:
               mOk = UpdateSigns("person",
         	      STRING(tt-inf.cust-id),
         	      "ДатАнализОФМ",
         	      STRING(TODAY,"99/99/9999"),
         	      ?).
         	   mDatAnalizOFM = GetXattrValueEx("person",STRING(tt-inf.cust-id),"ДатАнализОФМ","").
            	
            	mOk = UpdateSigns("person",
            	      STRING(tt-inf.cust-id),
            	      "АнализОФМ",
            	      tt-inf.an3,
            	      ?).
            	mAnalizOFM = GetXattrValueEx("cust-corp",STRING(mCLNum),"АнализОФМ","").
               PUT UNFORMATTED "Установлен АнализОФМ " + mAnalizOFM + " : " + mDatAnalizOFM + " " + tt-inf.inn " " tt-inf.lname  SKIP.
            END.
            ELSE
            DO:
               IF tt-inf.an3 NE "" THEN
               PUT UNFORMATTED "АнализОФМ уже " + mAnalizOFM + "        : " tt-inf.inn " " tt-inf.lname  SKIP.
            END.
            
            FOR EACH DataLine WHERE TRUE
               AND DataLine.Data-ID EQ mDataID
               AND DataLine.Sym3    EQ tt-inf.inn
               NO-LOCK:
               LEAVE.
            END.
            IF NOT AVAIL(DataLine) THEN
            DO:
               mTxtLine = TRIM(STRING(mInt)) + ";" + tt-inf.lname + ";" + tt-inf.inn + ";" + tt-inf.document + ";" + tt-inf.txt + ";".
               PUT UNFORMATTED "Добавлен в Стоп-Лист   : " tt-inf.inn " " tt-inf.lname  SKIP.
            	RUN CreateStopList(mTxtLine).
            END.
            ELSE
            DO:
            FOR EACH b-code WHERE TRUE
               AND b-code.class   EQ "StopList"
               AND b-code.parent  EQ "StopList"
               AND b-code.misc[5] EQ tt-inf.inn
               EXCLUSIVE-LOCK:
               b-code.misc[7] = b-code.misc[7] + "; " + tt-inf.txt.
            END. 
            PUT UNFORMATTED "Добавлено примечание   : " tt-inf.inn " " tt-inf.lname  SKIP.
            END.
         END.
         ELSE
            PUT UNFORMATTED "Счет не найден         : " tt-inf.inn " " tt-inf.lname  SKIP.
      END. /*20*/
      IF LENGTH(tt-inf.inn) EQ 10 THEN
      DO:
         mCLType       = "Ю".
         mAnalizOFM    = "".
         mDatAnalizOFM = "".
         FOR EACH cust-corp WHERE
            cust-corp.inn = tt-inf.inn
         NO-LOCK:
            ASSIGN
               tt-inf.cust-cat = "Ю"
               tt-inf.cust-id  = cust-corp.cust-id
               tt-inf.document = ""
               mCLNum          = cust-corp.cust-id
               mAnalizOFM = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"АнализОФМ","").
         END.
         IF tt-inf.an3 NE "" AND mAnalizOFM NE tt-inf.an3 THEN
         DO:
            mOk = UpdateSigns("cust-corp",
         	      STRING(mCLNum),
         	      "ДатАнализОФМ",
         	      STRING(TODAY,"99/99/9999"),
         	      ?).
         	mDatAnalizOFM = GetXattrValueEx("cust-corp",STRING(mCLNum),"ДатАнализОФМ","").
         	
         	mOk = UpdateSigns("cust-corp",
         	      STRING(mCLNum),
         	      "АнализОФМ",
         	      tt-inf.an3,
         	      ?).
         	mAnalizOFM = GetXattrValueEx("cust-corp",STRING(mCLNum),"АнализОФМ","").
            PUT UNFORMATTED "Установлен АнализОФМ " + mAnalizOFM + " : " + mDatAnalizOFM + " " + tt-inf.inn "   " tt-inf.lname  SKIP.
         END.
         ELSE
         DO:
            IF tt-inf.an3 NE "" THEN            
            PUT UNFORMATTED "АнализОФМ уже " + mAnalizOFM + "        : " tt-inf.inn "   " tt-inf.lname  SKIP.
         END.
         /**/
         FOR EACH DataLine WHERE TRUE
            AND DataLine.Data-ID EQ mDataID
            AND DataLine.Sym3    EQ tt-inf.inn
            NO-LOCK:
            LEAVE.
         END.
         IF NOT AVAIL(DataLine) THEN
         DO:
            mTxtLine = TRIM(STRING(mInt)) + ";" + tt-inf.lname + ";" + tt-inf.inn + ";" + tt-inf.document + ";" + tt-inf.txt + ";".
            PUT UNFORMATTED "Добавлен в Стоп-Лист   : " tt-inf.inn "   " tt-inf.lname  SKIP.
         	RUN CreateStopList(mTxtLine).
         END.
         ELSE
         DO:
            FOR EACH b-code WHERE TRUE
               AND b-code.class   EQ "StopList"
               AND b-code.parent  EQ "StopList"
               AND b-code.misc[5] EQ tt-inf.inn
               EXCLUSIVE-LOCK:
               b-code.misc[7] = b-code.misc[7] + "; " + tt-inf.txt.
            END. 
            PUT UNFORMATTED "Добавлено примечание   : " tt-inf.inn " " tt-inf.lname  SKIP.
         END.
      END. /*10*/
   END.
END.

PUT UNFORMATTED ETIME " msec" SKIP.

{preview.i &filename = "'chk_ufm.log'"}

{intrface.del}

RETURN.
