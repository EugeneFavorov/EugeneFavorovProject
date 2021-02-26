{globals.i}
{intrface.get pbase}
{intrface.get cust}
{intrface.get xobj}
{intrface.get db2l}
{intrface.get xclass}

{parsin.def}
{sh-defs.i}

DEFINE VARIABLE mInt       AS INT64     NO-UNDO.
DEFINE VARIABLE mInt2      AS INT64     NO-UNDO.
DEFINE VARIABLE mInt3      AS INT64     NO-UNDO.
DEFINE VARIABLE mInt4      AS INT64     NO-UNDO.
DEFINE VARIABLE mCnt       AS INT64     NO-UNDO.
DEFINE VARIABLE mString    AS CHARACTER NO-UNDO.

DEFINE VARIABLE mType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFIO       AS CHARACTER NO-UNDO.         
DEFINE VARIABLE mCustName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAddress   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mINN       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mKPP       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcct      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDolRuk    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mClassCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpenDate  AS DATE      NO-UNDO.
DEFINE VARIABLE mCloseDate AS DATE      NO-UNDO.
DEFINE VARIABLE mPrim      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk        AS LOGICAL   NO-UNDO.

DEFINE VARIABLE mOtsenk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtseUL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtseIP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOtseFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskIP     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskFL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mRiskUL     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIP         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mFraza      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk1        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mOk2        AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE  tt-inf
   FIELD cid        AS INT64 
   FIELD cust-id    AS INT64
   FIELD number     AS CHARACTER
   FIELD fio        AS CHARACTER
   FIELD country    AS CHARACTER
   FIELD inn        AS CHARACTER
   FIELD ogrn       AS CHARACTER
   FIELD prim       AS CHARACTER
   FIELD close-date AS DATE.

DEFINE BUFFER u-signs FOR signs.
DEFINE BUFFER f-signs FOR signs.

{setdest.i &filename = "'load-550p.log'"}

INPUT FROM VALUE("550.csv").
/*INPUT FROM VALUE("set-550p-16.txt").*/
REPEAT:
   IMPORT UNFORMATTED mString.
   IF mString NE "end" THEN
   DO:
      CREATE tt-inf.
      ASSIGN
         tt-inf.inn      = ENTRY(3,mString,";")
         tt-inf.cust-id  = INT64(ENTRY(4,mString,";")).  
   END.   
END.

mInt  = 0.
mInt2 = 0.
mInt3 = 0.
mInt4 = 0.

mFraza = "����稥 ���ଠ樨, ����㯨�襩 �� ����� ���ᨨ, � ����� �⪠�� � �믮������ �ᯮ�殮��� ������ � ᮢ��襭�� ����樨, �⪠�� �� �����祭�� ������� ������᪮�� ��� (������) � (���) ���থ��� ������� ������᪮�� ��� (������) � �����⮬ � ᮮ⢥��⢨� � ���������� � 550-�.".

FOR EACH tt-inf
   NO-LOCK:
   mInt = mInt + 1.
   
    if {assigned tt-inf.inn} then 
        if tt-inf.cust-id eq 0 then
            FOR EACH cust-corp WHERE TRUE
                AND cust-corp.inn EQ tt-inf.inn
                NO-LOCK:    
                        
           
                mOtseUL = "".
                mRiskUL = "".
                mOtsenk = "". 
           
                mOtseUL = GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id), "�業����᪠",today,"").
                mRiskUL = GetTempXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"��᪎��",today,"").

                mInt2 = mInt2 + 1.      

                If not CAN-DO("*" + mFraza + "*", mOtseUL)  then 
                    iF mRiskUL EQ "��᮪��" THEN mOtseUL = IF NOT {assigned mOtseIP} THEN mFraza ELSE mOtseUL + "; " + mFraza.
                    else 
                        mOtseUL = mFraza.       
        
                mOk1 = UpdateTempSignsEx("cust-corp",STRING(cust-corp.cust-id),"��᪎��",Today,"��᮪��",?).
                mOk2 = UpdateTempSignsEx("cust-corp",STRING(cust-corp.cust-id),"�業����᪠",Today,mOtseUL,?).
                mRiskFL = GetTempXAttrValueEx("person",STRING(cust-corp.cust-id),"��᪎��",today,"").
               
                PUT UNFORMATTED
                    mInt           ";"
                    mInt2          ";"
                    mInt3          ";"
                    mInt4          ";"
                    cust-corp.cust-id ";��;"
                    tt-inf.inn     ";"                
                    mRiskUL        ";"
                    mOtseUL        ";"
                    mOk1           ";"
                    mOk2           ";"
                    SKIP.
            END.      
       
        else         

            FOR EACH person WHERE TRUE
                AND person.person-id EQ tt-inf.cust-id           
                NO-LOCK:     
               
                mOtsenk = "".
                mOtseIP = "".
                mRiskIP = "".
                                      
                mOtseIP = GetTempXAttrValueEx("person",STRING(person.person-id), "�業����᪠",today,"").
                mRiskIP = GetTempXAttrValueEx("person",STRING(person.person-id),"��᪎�뢈�",today,"").            
   	
                mInt3 = mInt3 + 1.
           
                If not CAN-DO("*" + mFraza + "*", mOtseIP)  then 
                    iF mRiskIP EQ "��᮪��" THEN mOtseIP = IF NOT {assigned mOtseIP} THEN mFraza ELSE mOtseIP + "; " + mFraza.
                    else 
                        mOtseIP = mFraza.                                     
                                  
                mOk1 = UpdateTempSignsEx("person",STRING(person.person-id),"��᪎�뢈�",Today,"��᮪��",?).
                mOk2 = UpdateTempSignsEx("person",STRING(person.person-id),"�業����᪠",Today, mOtseIP,?).
                mOk1 = UpdateTempSignsEx("person",STRING(person.person-id),"��᪎��",Today,  "��᮪��",?).
                mRiskFL = GetTempXAttrValueEx("person",STRING(person.person-id),"��᪎�뢈�",today,"").  
               
                PUT UNFORMATTED
                    mInt           ";"
                    mInt2          ";"
                    mInt3          ";"
                    mInt4          ";"
                    tt-inf.cust-id ";"              
                    "��"           ";"
                    mRiskIP        ";"
                    mOtseIP        ";"
                    mOk1           ";"
                    mOk2           ";"
                    SKIP.
            END.
   

    else        
        FOR EACH person WHERE TRUE
            AND person.person-id EQ tt-inf.cust-id
            NO-LOCK:
      
            mOtsenk = "".
            mOtseIP = "".
            mOtseFL = "".
            mRiskIP = "".
            mRiskFL = "".

            /*FOR EACH tmpsigns WHERE TRUE
                AND tmpsigns.file-name  EQ "person"
                AND tmpsigns.surrogate  EQ STRING(tt-inf.cust-id)
                NO-LOCK BY tmpsigns.since:
                IF tmpsigns.code EQ "�業����᪠" THEN mOtseFL = tmpsigns.xattr-value.
                IF tmpsigns.code EQ "��᪎��"   THEN mRiskFL = tmpsigns.code-value.
            END.*/
           
            mOtseFL = GetTempXAttrValueEx("person",STRING(person.person-id), "�業����᪠",today,"").
            mRiskFL = GetTempXAttrValueEx("person",STRING(person.person-id),"��᪎��",today,"").  
   	
            mInt4 = mInt4 + 1.
      
            If not CAN-DO("*" + mFraza + "*", mOtseFL)  then 
                iF mRiskFL EQ "��᮪��" THEN mOtseFL = IF NOT {assigned mOtseIP} THEN mFraza ELSE mOtseFL + "; " + mFraza.
                else 
                    mOtseFL = mFraza.          
         
            mOk1 = UpdateTempSignsEx("person",STRING(person.person-id),"��᪎��",Today,  "��᮪��",?).
            mOk2 = UpdateTempSignsEx("person",STRING(person.person-id),"�業����᪠",Today, mOtseFL,?).
            mRiskFL = GetTempXAttrValueEx("person",STRING(person.person-id),"��᪎��",today,"").  
               
    
            PUT UNFORMATTED
                mInt           ";"
                mInt2          ";"
                mInt3          ";"
                mInt4          ";"
                tt-inf.cust-id ";"                              
                "��"           ";"
                mRiskFL        ";"
                mOtseFL        ";"
                mOk1           ";"
                mOk2           ";"
                SKIP.
        END.

END.

MESSAGE "�ᥣ�:" mInt SKIP "��:" mInt2 SKIP "��:" mInt3 SKIP "��:" mInt4
VIEW-AS ALERT-BOX.

{preview.i &filename = "'load-550p.log'"}

{intrface.del}

RETURN.

