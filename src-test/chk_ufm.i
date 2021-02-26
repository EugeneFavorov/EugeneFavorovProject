/*
  &GLOBAL-DEFINE name      'cust-corp'
        &GLOBAL-DEFINE nom     mCLNum
{chk_ufm.i}
*/


FOR EACH cust-role WHERE cust-role.file-name EQ 'cust-corp'
    AND   cust-role.surrogate EQ STRING({&nom})
                                
    AND   ((cust-role.close-date EQ ?) or (cust-role.close-date GE end-date))
    AND   cust-role.open-date LE end-date NO-LOCK  
    break by cust-id      :        
    
    IF FIRST-OF(cust-role.cust-id) THEN     
    do:           
        mCLType       = "�".
        mAnalizOFM    = "".
        mDatAnalizOFM = "".        
         
         
           
        FOR EACH person WHERE
            person.person-id EQ int64(cust-role.cust-id)
            NO-LOCK:
            ASSIGN
                FIO      = person.name-last + " " + person.first-names 
                inn        = person.inn                            
                document   = REPLACE(person.document," ","")
                mCLNum     = person.person-id
                mAnalizOFM = GetXattrValueEx("person",STRING(person.person-id),"���������","").          
                             
        
            IF mAnalizOFM NE mA3 THEN
            DO:
                mOk = UpdateSigns("person",
                    STRING(person.person-id),
                    "��‭�������",
                    STRING(TODAY,"99/99/9999"),
                    ?).
                mDatAnalizOFM = GetXattrValueEx("person",STRING(person.person-id),"��‭�������","").
                
                mOk = UpdateSigns("person",
                    STRING(person.person-id),
                    "���������",
                    mA3,
                    ?).
                mAnalizOFM = GetXattrValueEx("person",STRING(person.person-id),"���������","").
                PUT UNFORMATTED "�� ��ꥪ� �� " + STRING(person.person-id) + " ��⠭����� ��������� " + mAnalizOFM + " : " + mDatAnalizOFM + " " + inn " " FIO  SKIP.
            END.
            ELSE
            DO:
                PUT UNFORMATTED "�� ��ꥪ� �� " + STRING(person.person-id) + " ��������� 㦥 " +  mAnalizOFM + "        : " inn " " FIO  SKIP.
            END.
            
            FOR EACH code WHERE TRUE
                AND code.class   EQ "StopList"
                AND code.parent  EQ "StopList"
                AND code.misc[1] EQ mCLType
                AND code.misc[2] EQ STRING(person.person-id)
                NO-LOCK:
                LEAVE.      
            END.
      
            IF NOT AVAIL(code) then
             
        
            DO:
                mTxtLine = TRIM(STRING(mInt)) + ";" + FIO + ";" + inn + ";" + document + ";" + mFraza + ";".
                PUT UNFORMATTED "��ꥪ� �� " + STRING(person.person-id) + " �������� � �⮯-����   : " + inn " " FIO  SKIP.
                RUN CreateStopList(mTxtLine).
            END.
            ELSE
            DO:
                PUT UNFORMATTED "��ꥪ� �� " + STRING(person.person-id) + " 㦥 � �⮯-����        : " + inn " " FIO  SKIP.
            END.
       
        END.
    end.

end. 