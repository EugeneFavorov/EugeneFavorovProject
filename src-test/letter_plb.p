/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: letter_plb.p
      Comment: ���쬮 ������� ��� �।��⠢����� ������ �룮���ਮ���⥫�
   Parameters: ���
         Uses:
      Used by:
      Created: 31/10/2014 IT
     Modified: 
*/
{globals.i}

{intrface.get strng}
{intrface.get xclass}
{intrface.get cust}
{intrface.get instrum}
{intrface.get date}

{tmprecid.def}
{prn-doc.def &with_proc = YES}

DEF VAR mName     AS CHAR EXTENT 2 NO-UNDO.
DEF VAR mAddr     AS CHAR          NO-UNDO.
DEF VAR mDolRuk   AS CHAR          NO-UNDO.
DEF VAR mNameRuk  AS CHAR          NO-UNDO.
DEF VAR mNaznSch  AS CHAR          NO-UNDO.
DEF VAR mCur      AS CHAR          NO-UNDO.
DEF VAR mCurEng   AS CHAR          NO-UNDO.
DEF VAR mCurAmt   AS CHAR          NO-UNDO.
DEF VAR mCurStr   AS CHAR          NO-UNDO.
DEF VAR mNameRec  AS CHAR          NO-UNDO.
DEF VAR mDetails  AS CHAR          NO-UNDO.
DEF VAR mUsrName  AS CHAR          NO-UNDO.
DEF VAR mUsrPhone AS CHAR          NO-UNDO.
DEF VAR mTmpStr   AS CHAR          NO-UNDO.


FUNCTION prev_wk_date RETURNS DATE ( INPUT pd_date_real AS DATE, INPUT a_days AS INTEGER ): 
    DEFINE VARIABLE vIndDat AS DATE.  
    DEFINE VARIABLE mWL     AS INTEGER.
 
    mWL = a_days - 1.

    DO vIndDat = pd_date_real TO pd_date_real + mWL:

        IF  (WEEKDAY(vIndDat) = 1) OR (WEEKDAY(vIndDat) = 7) OR (Holiday(vIndDat)) THEN 
        DO:
            mWL = mWL  + 1.
        END. 
    END.

    pd_date_real = pd_date_real + mWL. 

    RETURN  pd_date_real.

END FUNCTION. 


FOR FIRST tmprecid NO-LOCK,
    FIRST op NO-LOCK
    WHERE RECID(op) EQ tmprecid.id,
    FIRST op-entry OF op NO-LOCK,
    FIRST acct NO-LOCK
    WHERE acct.acct EQ op-entry.acct-db :

    mName = "".
    IF acct.cust-cat EQ "�" 
    THEN RUN GetCustNameFormatted IN h_cust(acct.cust-cat, acct.cust-id, OUTPUT mName[1]).
    ELSE 
    DO:
        {getcust.i &name=mName }
        mName[1] = mName[1] + mName[2].
    END.
    RUN Insert_TTName("name", mName[1]).

    RUN GetCustInfo2 IN h_cust(4, acct.acct, acct.currency, OUTPUT mAddr).
    RUN Insert_TTName("addr",    mAddr).

    RUN Insert_TTName("date",    STRING(prev_wk_date(op.op-date,2), '99.99.9999')).                 
    RUN Insert_TTName("opdate",  STRING(op.op-date,    '99.99.9999')).
    RUN Insert_TTName("docdate", IF op.doc-date NE ? THEN STRING(op.doc-date,'99.99.9999') ELSE '').
    RUN Insert_TTName("prtdate", STRING(prev_wk_date(op.op-date,7),'99.99.9999')).
    
    CASE acct.cust-cat :
      WHEN "�" THEN 
        FOR FIRST person NO-LOCK
            WHERE person.person-id EQ acct.cust-id :

           IF LOOKUP(GetXAttrValueEx("person", 
                                     STRING(person.person-id), 
                                     "�।��", 
                                     ""), 
                     "�।��,�।_��") NE 0
           THEN ASSIGN
                   mDolRuk  = "��४���"
                   mNameRuk = person.name-last + ' ' + person.first-names
                .
           ELSE ASSIGN 
                   mDolRuk  = GetXAttrValueEx("person", 
                                              STRING(person.person-id), 
                                              "�����", 
                                              "")
                   mNameRuk = GetXAttrValueEx("person", 
                                              STRING(person.person-id), 
                                              "�����", 
                                              "")
                .
        END.
      WHEN "�" THEN 
        FOR FIRST cust-corp NO-LOCK
            WHERE cust-corp.cust-id EQ acct.cust-id :
            mDolRuk = IF cust-corp.cust-stat EQ "��"  
                      THEN "��४���"
                      ELSE GetXAttrValueEx("cust-corp", 
                                           STRING(cust-corp.cust-id), 
                                           "�����", 
                                           "").
            mNameRuk = IF cust-corp.cust-stat EQ "��"  
                       THEN cust-corp.name-corp 
                       ELSE GetXAttrValueEx("cust-corp", 
                                            STRING(cust-corp.cust-id), 
                                            "�����", 
                                            "").
        END. 
    END CASE.
      
    RUN Insert_TTName("dolruk",  mDolRuk).
    RUN Insert_TTName("nameruk", mNameRuk).

    mNaznSch = IF acct.contract EQ "�����"  THEN "���⭮��"  ELSE
               IF acct.contract EQ "�����"   THEN "⥪�饣�"    ELSE
               IF acct.contract BEGINS "��"  THEN "�࠭��⭮��" ELSE
               IF acct.contract BEGINS "���" THEN "������⭮��" ELSE "".
    RUN Insert_TTName("naznsch", mNaznSch).

    RUN Insert_TTName("acctnum", DelFilFromAcct(acct.acct)).

    FOR FIRST currency WHERE currency.currency EQ acct.currency
        NO-LOCK:
        mCurEng = currency.i-name-curre.
    END.
    RUN Insert_TTName("cureng",  mCurEng).
    mCur = Get_Val_Name(acct.currency, IF acct.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub).
    RUN Insert_TTName("currus",  mCur).

    RUN x-amtstr.p(IF acct.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub, 
                   acct.currency, 
                   TRUE, 
                   TRUE, 
                   OUTPUT mCurStr, OUTPUT mTmpStr).
    mCurStr = mCurStr + " " + mTmpStr.
    mCurAmt = TRIM(STRING(IF acct.currency NE "" THEN op-entry.amt-cur ELSE op-entry.amt-rub,"->>>>>>>>>>>>>>>>>>>>9.99")).
    mCurAmt = REPLACE(mCurAmt,".","-").
    RUN Insert_TTName("sum",     mCurAmt).
    RUN Insert_TTName("sumstr",  mCurStr).

    mNameRec = GetXAttrValueEx("op", STRING(op.op), "name-rec", "").
    IF NOT {assigned mNameRec} THEN 
    mNameRec = op.name-ben.
    RUN Insert_TTName("namerec", mNameRec).

    RUN Insert_TTName("docnum",  op.doc-num).
 
    mDetails = REPLACE((REPLACE(op.details,CHR(13),"")),CHR(10),"").
    RUN Insert_TTName("details", mDetails).

    FOR FIRST _user NO-LOCK
        WHERE _user._userid EQ USERID ("bisquit") :
        ASSIGN
          mUsrName  = _user._user-name
          mUsrPhone = GetXAttrValueEx("_User", _User._Userid, "����䮭", "")
        .
    END.
    RUN Insert_TTName("usrname", mUsrName).
    RUN Insert_TTName("usrtel",  mUsrPhone).

END. /* FOR FIRST op WHERE RECID(op) EQ iRecIdOp  */

RUN printvd.p ("letter_plb", INPUT TABLE ttnames).
{intrface.del }
