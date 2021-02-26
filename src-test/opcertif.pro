/* èÆ§Ø®·® §Æ™„¨•≠‚† */

DEFINE TEMP-TABLE ttSignCertif NO-UNDO            /* Ç‡•¨•≠≠†Ô ‚†°´®Ê† §´Ô Â‡†≠•≠®Ô ·‚‡Æ™ ≠†®¨•≠Æ¢†≠®© ·•‡‚®‰®™†‚Æ¢ */
   FIELD DateTime as CHARACTER
   FIELD Name AS CHARACTER             
.

PROCEDURE GetSignCertif:
   DEFINE INPUT PARAMETER iClassCode AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER iSurrogate AS CHARACTER NO-UNDO.

   DEFINE VARIABLE mRole AS CHARACTER NO-UNDO.    /* êÆ´® ØÆ§Ø®·•© */
   DEFINE VARIABLE mI    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE mDate AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mBodyText AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mHeadText AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER certifdbo  FOR certif.
   DEFINE BUFFER signsdbo   FOR signs.
   DEFINE BUFFER persondbo  FOR person.
   DEFINE BUFFER loandbo    FOR loan.
   DEFINE BUFFER historydbo FOR history.

   mRole     = FGetSetting("ÑÆ™„¨•≠‚Î","êÆ´®èÆ§Ø®·Ï","").
   mHeadText = GetCodeEx("dbo-sign","opsignhead","").
   mBodyText = GetCodeEx("dbo-sign","opsignbody","").
   
   &IF DEFINED(IS-DEBUG) &THEN
      RUN dbgdboprn.p ("{&HTML} OPCERTIF.PRO","iClassCode:" + iClassCode +
                                            "; iSurrogate:" + iSurrogate +
                                            "; mRole:" + mRole +
                                            "; head:" + mHeadText +
                                            "; body:" + mBodyText).
   &ENDIF
   
   DO mI = 1 TO NUM-ENTRIES(mRole):
      FOR EACH signsdbo where
               signsdbo.file-name EQ iClassCode
           AND signsdbo.surrogate EQ iSurrogate
           AND signsdbo.code      EQ ENTRY(mI,mRole)
           NO-LOCK BREAK BY signsdbo.code descending:
         
         &IF DEFINED(IS-DEBUG) &THEN
            RUN dbgdboprn.p ("{&HTML} OPCERTIF.PRO","certif:" + (IF AVAIL certif THEN STRING(certif.certif-id) ELSE "") +
                                                  "; signsdbo.code:" + signsdbo.code +
                                                  "; signsdbo.xattr-value:" + signsdbo.xattr-value).
         &ENDIF
         
         FIND FIRST certifdbo WHERE
                    certifdbo.certif-id EQ INTEGER(ENTRY(1,signsdbo.xattr-value))
            NO-LOCK NO-ERROR.
         
         IF AVAIL certifdbo THEN DO:
         
            FIND FIRST loandbo WHERE
                       loandbo.contract  EQ entry(2,signsdbo.xattr-value)
                   AND loandbo.cont-code EQ entry(3,signsdbo.xattr-value)
            NO-LOCK NO-ERROR.
            
            IF AVAIL loandbo THEN do:
            
               FIND FIRST persondbo WHERE
                          persondbo.person-id EQ certifdbo.person-id
                  NO-LOCK NO-ERROR.
               
               IF FIRST(signsdbo.code) AND mHeadText NE "" THEN DO:
                  CREATE ttSignCertif.
                  ASSIGN
                     ttSignCertif.Name = REPLACE(mHeadText,"<ÑéÉçéåÖê>",DelFilFromLoan(loandbo.cont-code)).
                     ttSignCertif.Name = REPLACE(ttSignCertif.Name,"<ÑéÉÑÄíÄ>",STRING(loandbo.open-date, "99.99.9999")).
                  .
               END.
               
               FOR EACH historydbo WHERE
                        historydbo.file-name EQ signsdbo.file-name
                    AND historydbo.field-ref EQ signsdbo.surrogate
                    AND historydbo.modify    EQ "W"
                    AND CAN-DO(historydbo.field-value,signsdbo.code)
                  NO-LOCK BREAK BY modif-date DESCENDING BY modif-time DESCENDING:
                   &IF DEFINED(IS-DEBUG) &THEN
                      RUN dbgdboprn.p ("{&HTML} OPCERTIF.PRO","certif:" + (IF AVAIL certif THEN STRING(certif.certif-id) ELSE "") +
                                                            "; historydbo.modif-date:" + STRING(historydbo.modif-date,"99.99.9999") +
                                                            "; historydbo.modif-time:" + STRING(historydbo.modif-time,"HH:MM:SS") +
                                                            "; historydbo.field-value:" + STRING(historydbo.field-value)).
                   &ENDIF
                   IF SUBSTRING(historydbo.field-value,
                                INDEX(historydbo.field-value,signsdbo.code) + LENGTH(signsdbo.code),2) eq ",," THEN DO:
                      mDate = STRING(historydbo.modif-date,"99.99.9999") + " " + STRING(historydbo.modif-time,"HH:MM:SS").
                      LEAVE.
                   END.
               END.
   
               IF mBodyText NE "" THEN DO:
                  CREATE ttSignCertif.
                  ASSIGN
                     ttSignCertif.DateTime = mDate
                     ttSignCertif.Name = REPLACE(mBodyText,"<ÑÄíÄÇêÖåü>",mDate)
                     ttSignCertif.Name = REPLACE(ttSignCertif.Name,"<ëÖêíàîàäÄí>",certifdbo.number)
                     ttSignCertif.Name = REPLACE(ttSignCertif.Name,"<ÇãÄÑÖãÖñ>",persondbo.name-last + " " + persondbo.first-name)
                     ttSignCertif.Name = REPLACE(ttSignCertif.Name,"<ÑéÉçéåÖê>",DelFilFromLoan(loandbo.cont-code)).
                     ttSignCertif.Name = REPLACE(ttSignCertif.Name,"<ÑéÉÑÄíÄ>" ,STRING(loandbo.open-date, "99.99.9999")).
                  .
               END.
            END.
         END.
      END.
   END.
END PROCEDURE.
