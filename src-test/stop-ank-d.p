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

{parsin.def}
{sh-defs.i}
{wordwrap.def}
{stoplist.fun }
{sv-temp.i NEW}
{norm.i NEW}

DEF VAR mDataCls     AS DATE  NO-UNDO.

DEFINE VARIABLE mNumRez       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDataRez      AS DATE       NO-UNDO.
DEFINE VARIABLE mFldIndex     AS CHARACTER  NO-UNDO EXTENT 10.
DEFINE VARIABLE mCLType       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDelimeter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mDataID       AS INT64      NO-UNDO.
DEFINE VARIABLE iPar          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mInt          AS INT64      NO-UNDO.
DEFINE VARIABLE mFraza        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mNPName       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mExchParam    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCLNum        AS INT64      NO-UNDO.
DEFINE VARIABLE vTmpStr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mSLName       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mExchRule     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mI            AS INT64      NO-UNDO.
DEFINE VARIABLE mTmpStr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mInd          AS INT64      NO-UNDO.
DEFINE VARIABLE mFldNames     AS CHARACTER  NO-UNDO INIT "Тип,Код,ФИО,Страна,ИНН,Паспорт,Примечание,,ДР,МР".
DEFINE VARIABLE mTxtLine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mCnt          AS INT64      NO-UNDO.
DEFINE VARIABLE mBirthDay     AS DATE       NO-UNDO.
DEFINE VARIABLE mBirthPlace   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE DatObAnk    AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE  tt-inf
   FIELD cust-cat  AS CHARACTER
   FIELD cust-id   AS INT64
   FIELD number    AS CHARACTER
   FIELD inn       AS CHARACTER
   FIELD txt       AS CHARACTER
   FIELD lname     AS CHARACTER
   FIELD linetxt   AS CHARACTER
   FIELD document  AS CHARACTER
.

DEFINE TEMP-TABLE tt-chkfldval NO-UNDO
   FIELD fldnum  AS INT64 
   FIELD fld-val AS CHAR
   FIELD fld-add AS CHAR
INDEX fldnum fldnum.

{chk_ufm.pro}

{setdest.i &filename = "'chk_ufm.log'"}

mDataID = INT64(GetCodeMisc("","StopList",8)) NO-ERROR.

iPar   = "InStopOSK".
mFraza = "Не проведено обновление".
      
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
   mExchRule = ENTRY(3,mExchParam,";")                 /*правило обмена*/  
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

mInt = 0.
 
FOR each  person WHERE 
    person.date-in  <= (today - 365)    
    /*        and cust-corp.cust-id eq 10682  */           
    NO-LOCK,                              
    last tmpsigns WHERE tmpsigns.file-name EQ 'person'
    AND tmpsigns.surrogate EQ STRING(person.person-id)
    /*AND tmpsigns.code EQ 'ДатаОбнАнкеты'
       
     AND tmpsigns.since LE today*/
    NO-LOCK,
    first signs where signs.file-name = 'person'
    AND signs.code = 'Субъект'
    AND signs.surrogate = STRING(person.person-id)
    AND signs.code-value = 'ФЛП'
    NO-LOCK, 
    first  acct WHERE   
    acct.cust-cat = "Ч"
    AND acct.cust-id = person.person-id
    and acct.open-date <= (today - 365)
    and acct.close-date eq ?
    
    and can-do("407*,40802*,40807*,40821*,406*",trim(acct.acct)) 
      
    NO-LOCK :    
      
    DatObAnk =    GetTempXAttrValueEx('person',STRING(person.person-id), "ДатаОбнАнкеты",TODAY,?).
       
    if(DatObAnk eq  ?)  or (date(DatObAnk) <= (today - 365)) or (DatObAnk eq "" ) then          
    do: 
        mInt = mInt + 1.
        CREATE tt-inf.
        ASSIGN
            tt-inf.lname    = person.name-last + " " + person.first-names 
            tt-inf.inn      = person.inn
            tt-inf.txt      = mFraza
            tt-inf.cust-cat = "Ч"
            tt-inf.cust-id  = person.person-id   
            . 
    end.    
end.

 PUT UNFORMATTED "кол-во Ч " mInt SKIP. 


FOR each  cust-corp WHERE 
    cust-corp.date-in  <= (today - 365)    
    /*        and cust-corp.cust-id eq 10682  */ 
      and (cust-corp.date-out EQ ? OR cust-corp.date-out >= today)         
    NO-LOCK,                              
    last tmpsigns WHERE tmpsigns.file-name EQ 'cust-corp'
    AND tmpsigns.surrogate EQ STRING(cust-corp.cust-id)
    /*AND tmpsigns.code EQ 'ДатаОбнАнкеты'       
     AND tmpsigns.since LE today*/
    NO-LOCK,   
    first  acct WHERE   
    acct.cust-cat = 'Ю'
    AND acct.cust-id = cust-corp.cust-id
    and acct.open-date <= (today - 365)
  
       and acct.close-date eq ?
    
    and can-do("407*,40802*,40807*,40821*,406*",trim(acct.acct)) /**/
      
    NO-LOCK :    
      
    DatObAnk =    GetTempXAttrValueEx('cust-corp',STRING(cust-corp.cust-id), "ДатаОбнАнкеты",TODAY,?).
       
    if(DatObAnk eq  ?)  or (date(DatObAnk) <= (today - 365)) or (DatObAnk eq "" ) then          
    do: 
        mInt = mInt + 1.
        CREATE tt-inf.
        ASSIGN
            tt-inf.lname    = cust-corp.name-short 
            tt-inf.inn      = cust-corp.inn
            tt-inf.txt      = mFraza
            tt-inf.cust-cat = "Ю"
            tt-inf.cust-id  = cust-corp.cust-id   
            . 
    end.    
end.

mInt = 0.
FOR EACH tt-inf EXCLUSIVE-LOCK:
   
    mInt = mInt + 1.   

    FOR each code WHERE
        code.class EQ 'StopList'
        AND code.name EQ 'OSK'
        AND code.parent EQ 'StopList'
        AND code.misc[5] = tt-inf.inn 
        NO-LOCK:
        LEAVE.
    END.
    
  /*  FOR EACH DataLine WHERE TRUE
            AND DataLine.Data-ID EQ tt-inf.cust-id
            AND DataLine.Sym3    EQ tt-inf.inn
            NO-LOCK:
            
            
            PUT UNFORMATTED mDataID "/" tt-inf.inn  SKIP.  LEAVE.
         END.*/
    IF NOT AVAIL(code) THEN 
    do:
        mCLNum        = tt-inf.cust-id.
        mCLType       = tt-inf.cust-cat.
        mTxtLine = TRIM(STRING(mInt)) + ";" + tt-inf.lname + ";" + tt-inf.inn + ";" + tt-inf.document + ";" + tt-inf.txt + ";".                 
        PUT UNFORMATTED "Добавлен в Стоп-Лист  OSK : " tt-inf.inn " " tt-inf.lname  SKIP.
        RUN CreateStopList(mTxtLine).   
    end.

END.


PUT UNFORMATTED ETIME " msec" SKIP.
PUT UNFORMATTED "количество  " mInt SKIP.  

{preview.i &filename = "'chk_ufm.log'"}

{intrface.del}

RETURN.