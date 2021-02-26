
/* +++ e-950i.i was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:44am +++ */


&GLOB ShInBal (sh-in-bal + sr + ~
                 ( IF topen.db THEN (- op-entry.amt-rub) ~
                  ELSE op-entry.amt-rub ~
                 ) ~
               )

IF in-currency EQ "" THEN
   sr = sr - ( IF topen.db THEN
                 (- op-entry.amt-rub)
              ELSE op-entry.amt-rub).
ELSE ASSIGN
   sv  = sv - ( IF topen.db THEN
                  (- op-entry.amt-cur)
               ELSE op-entry.amt-cur)
   srv = srv - ( IF topen.db THEN
                   (- op-entry.amt-rub)
                ELSE op-entry.amt-rub).

flag-last = last-of(topen.acct).
mCurDate = IF op.op-date EQ ? 
           THEN xop-entry.op-date
           ELSE op.op-date.
IF mCurDate GT mLastDate OR mLastDate EQ ? THEN
   mLastDate = mCurDate.

IF topen.db THEN ASSIGN
   s950-db = s950-db +
             IF in-currency EQ "" THEN
                op-entry.amt-rub
             ELSE op-entry.amt-cur
   q950-db = q950-db + 1.
ELSE ASSIGN
   s950-cr = s950-cr +
             IF in-currency EQ "" THEN
                op-entry.amt-rub
             ELSE op-entry.amt-cur
   q950-cr = q950-cr + 1.


FIND FIRST code WHERE code.class EQ  "Маршруты" AND
                      code.code  EQ op-entry.type NO-LOCK NO-ERROR.
f-trans = NO.
IF "{&format-message}" EQ "SWIFT" AND
    ((AVAIL code AND
      code.descr[2] NE "М"
     ) OR
     NOT AVAIL code
    ) THEN
   ASSIGN
      f-trans = is-cyril(op.details) OR
                (NOT is-cyril(op.details) AND
                 op.details BEGINS "~{VO")
      f-trans = IF NOT f-trans THEN
                   is-cyril(op.name-ben)
                ELSE f-trans.

ASSIGN
   foreign = AVAIL code AND
             code.descr[2] EQ "М"
   VerForm = GetXattrValueEx("mail-user",string(mail-user.mail-user-num),
                             "Ver-Format","")
   VerForm = IF "{&format-message}" EQ "swift" AND
                NOT foreign THEN VerForm
             ELSE "".

IF (last-op modulo num-op-stmt EQ 1) OR
   num-op-stmt EQ 1 OR 
   FIRST-OF(topen.acct) THEN
DO:
   /* формирование шапки */
   IF FIRST-OF(topen.acct) THEN DO:
/* Замена Плюс банк
      RUN acct-pos IN h_base (topen.acct, in-currency, in-op-date, in-op-date2,
                              {&St64}).
*/    RUN pb_acct-pos.p (topen.acct, in-currency, in-op-date, in-op-date2,
                              {&St64}).     /* ФБО - Плюс банк */
/* Конец замены Плюс банк */
      ASSIGN
         mTmpShBal   = sh-bal
         mTmpShVal   = sh-val
         mTmpShInBal = sh-in-bal
         mTmpShInVal = sh-in-val
      .
/* Удалено Плюс банк
      RUN acct-pos IN h_base (topen.acct, in-currency, in-op-date, in-op-date2,
                              tmp-status).
Конец удаленного фрагмента Плюс банк */
      {curr-iso.i &curr=in-currency}
      IF NOT {assigned curr-iso} OR
         curr-iso EQ "?" THEN curr-iso = "RUR".
      {is-resid &findcust=yes}
   END.

   ASSIGN
      buf-s2 = IF in-currency EQ "" THEN
                  TRIM(string({abs "if flag-last THEN sh-bal
                                     ELSE (sh-in-bal + sr)"
                              },">>>,>>>,>>>,>>9.99"))
               ELSE TRIM(string({abs "if flag-last THEN sh-val
                                      ELSE (sh-in-val + sv)"
                                },">>>,>>>,>>>,>>9.99"))
      buf-s2 = SUBSTRING(buf-s2,1,length(buf-s2) - 3).
      buf-s2 = REPLACE(buf-s2,",",".").

   {{&hdr}}
   
   num-pach = num-pach + 1.
   IF FIRST-OF(topen.acct) THEN DO:
      ASSIGN
         num-pach = 1
         num-stmt = INT64(GetXAttrValueEx("acct", acct.acct + "," + acct.curr,
                                      "NumStmt","0")).
      RUN GetNumStmt(BUFFER signs).
   END.

   /* формирование тела сообщения */
   FIND FIRST op-impexp OF op NO-LOCK NO-ERROR.
   /* Референс операции */
   FIND FIRST acct WHERE acct.acct EQ topen.acct NO-LOCK NO-ERROR.

   IF AVAIL acct THEN
      RUN getref.p (op.op, (&IF DEFINED(FrmMsg) &THEN {&FrmMsg}
                            &ELSE "950"
                            &ENDIF),acct.acct).
   buf-work = ":20:" +
              ( IF f-trans AND
                  CAN-DO("RUR5,RUR6",VerForm) THEN "+"
               ELSE IF f-trans THEN "'"
               ELSE "") +
              ( IF {&RETURN_VALUE} NE ? THEN {&RETURN_VALUE}
               ELSE "").
   RUN cr-buf-copy-doc(buf-work).
   &IF DEFINED(FrmMsg) &THEN
   IF NOT ({&FrmMsg} EQ "940" AND
      "{&format-message}" EQ "SWIFT") THEN
   DO:
      /* Связанный Референс операции */
      FIND FIRST op-impexp OF op NO-LOCK NO-ERROR.
      IF {&FrmMsg} NE "942" OR
         ({&FrmMsg} EQ "942" AND
          ref-920 NE "") THEN
      DO:
         buf-work = ":21:" +
            ( IF ref-920 EQ "" THEN "NONREF"
             ELSE ref-920).
         RUN cr-buf-copy-doc(buf-work).
      END.
   END.
   &ENDIF
   buf-work = ":25:" + DelFilFromAcct(topen.acct).
   RUN cr-buf-copy-doc(buf-work).

   buf-work = ":28C:" + string(num-stmt mod 100000,"99999") + "/" +
                        string(num-pach mod 1000,"999").
   RUN cr-buf-copy-doc(buf-work).

   buf-s = IF in-currency EQ "" THEN  
              TRIM(string( {abs (sh-in-bal +
                                 ( IF last-op EQ 1 OR
                                     FIRST-OF(topen.acct)
                                     THEN 0
                                  ELSE (sr +
                                     ( IF topen.db THEN (- op-entry.amt-rub)
                                      ELSE op-entry.amt-rub))))},
                           "->>>>>>>>>>>9.99"))  
           ELSE TRIM(string( {abs (sh-in-val +
                                   ( IF last-op EQ 1 OR
                                       FIRST-OF(topen.acct)
                                       THEN 0
                                    ELSE (sv +
                                       ( IF topen.db THEN (- op-entry.amt-cur)
                                        ELSE op-entry.amt-cur))))},
                             "->>>>>>>>>>>9.99")).
   OVERLAY(buf-s,INDEX(buf-s,"."),1) = ",".

   &IF DEFINED(FrmMsg) &THEN
   ASSIGN   
      buf-copy-doc = ""
      tmpcode      = ""
      tmptype      = {&FrmMsg}
      tmpformat    = "{&format-message}".
   IF {&FrmMsg} EQ "942" THEN
   DO:
      buf-work = ":13:" + {date-swf.i &date=today} + {time-swf.i &time=time}.
      RUN cr-buf-copy-doc(buf-work).
   END.
   ELSE DO:
   &ENDIF
      buf-work = ":60" +
                 ( IF last-op EQ 1 OR
                     FIRST-OF(topen.acct) THEN "F:"
                  ELSE "M:") + 
                 ( IF (last-op EQ 1 AND 
                      (sh-in-bal < 0 OR 
                       (sh-in-bal = 0 AND acct.side EQ "П")
                      )
                     ) OR 
                     (last-op NE 0 AND last-op NE 1 AND 
                      (
                       {&ShInBal} < 0 OR 
                       ({&ShInBal} EQ 0 AND acct.side EQ "П")
                      )
                     ) 
                  THEN "C"
                  ELSE "D"
                 ) + {date-swf.i &date="in-op-date"} +  curr-iso + buf-s.
      RUN cr-buf-copy-doc(buf-work).
   &IF DEFINED(FrmMsg) &THEN
   END.
   &ENDIF
END.
                                      
{e-bss950.61 {&*}}

&IF DEFINED(FrmMsg) EQ 0 &THEN
IF {assigned "op.details"} THEN
   RUN cr-buf-copy-doc
      (
       ( IF f-trans AND
           "{&format-message}" EQ "swift"
           THEN substr(sw-trans(ENTRY(1,op.detail,"~n"),YES,VerForm),1,34)
        ELSE substr(TRIM(ENTRY(1,op.detail,"~n")),1,34)
       )
      ).
&ENDIF

&IF DEFINED(FrmMsg) &THEN
IF NOT ({&FrmMsg} EQ "940" AND
        "{&format-message}" EQ "SWIFT") THEN
DO:
   FIND FIRST msg-impexp WHERE
      msg-impexp.msg-cat   EQ "op"          AND
      msg-impexp.object-id EQ string(op.op) AND
      msg-impexp.msg-type BEGINS "i-" NO-LOCK NO-ERROR.
   ASSIGN
      mInType = 
         IF AVAIL msg-impexp AND
            LENGTH(msg-impexp.msg-type) >= 3 AND
            SUBSTR(msg-impexp.msg-type,LENGTH(msg-impexp.msg-type) - 2,3) EQ "103"
             THEN "103"
         ELSE "100".
   IF mInType EQ "103" THEN
   DO:
      buf-copy-doc = "".
      {e-tel100.26}
      {e-tel950.put}
   END.
   /* Клиент-отправитель */
   buf-copy-doc = "".
   RUN e-sw50 IN h_swi
      (recid(op),
       recid(op-entry),
       foreign,
       f-trans,
       VerForm + ( IF AVAIL op-templ AND
                     AVAIL mail-user
                     THEN (',' + mail-user.mail-format + "," +
                           op-kind.op-kind + "," + STRING(op-templ.op-templ))
                  ELSE ""),
       INPUT-OUTPUT buf-copy-doc,
       INPUT-OUTPUT op-err-50 ).
   {e-tel950.put}
   
   /* Банк-отправитель */
   buf-copy-doc = "".
   IF {&FrmMsg} EQ "940" THEN DO:
      {e-tel100.52 send 940}
   END.
   ELSE IF {&FrmMsg} EQ "942" THEN DO:
      {e-tel100.52 send 942}
   END.
   {e-tel950.put}

   /* Банк-посредник */
   buf-copy-doc = "".
   {e-tel100.56}
   {e-tel950.put}
   
   /* Банк-получатель */
   buf-copy-doc = "".
   IF {&FrmMsg} EQ "940" THEN DO:
      {e-tel100.52 rec 940}
   END.
   ELSE IF {&FrmMsg} EQ "942" THEN DO:
      {e-tel100.52 rec 942}
   END.
   {e-tel950.put}
   
   /* Клиент-получатель */
   buf-copy-doc = "".
   RUN e-sw59 IN h_swi
      (recid(op),
       recid(op-entry),
       foreign,
       f-trans,
       VerForm + ( IF AVAIL op-templ AND
                     AVAIL mail-user
                     THEN (',' + mail-user.mail-format + "," +
                           op-kind.op-kind + "," + STRING(op-templ.op-templ))
                  ELSE ""),
       INPUT-OUTPUT buf-copy-doc,
       INPUT-OUTPUT op-err-50 ).

   IF buf-copy-doc NE "" THEN
      {e-tel950.put}

END.
/* информация для владельца счета */
buf-copy-doc = "".
{e-swift.72 &ofett=yes}
IF buf-copy-doc NE "" THEN
   {e-tel950.put}
IF mInType EQ "103" THEN
DO:
   buf-copy-doc = "".
   {e-tel100.77b}
   {e-tel950.put}
END.
&ENDIF

IF (last-op modulo num-op-stmt EQ 0) OR
   num-op-stmt EQ 1 OR
   last-of(topen.acct) THEN
DO:
   ASSIGN
      buf-s3 = IF in-currency EQ "" THEN
                  TRIM(string( {abs ( IF flag-last THEN sh-bal
                                     ELSE (sh-in-bal + sr))},
                               ">>>,>>>,>>>,>>9.99"))
               ELSE TRIM(string( {abs ( IF flag-last THEN sh-val
                                       ELSE (sh-in-val + sv))},
                                 ">>>,>>>,>>>,>>9.99"))
      buf-s3 = SUBSTRING(buf-s3,1,length(buf-s3) - 3)
      buf-s3 = REPLACE(buf-s3,",",".")
      buf-s = IF in-currency EQ "" THEN  
                 TRIM(string( {abs ( IF flag-last THEN sh-bal
                                    ELSE (sh-in-bal + sr))},
                              "->>>>>>>>>>>9.99"))  
              ELSE (TRIM(string( {abs ( IF flag-last THEN sh-val
                                       ELSE (sh-in-val + sv))},
                                 "->>>>>>>>>>>9.99")) +
                    ( IF "{&format-message}" EQ "telex" AND
                        mCrAmtRub
                        THEN ("/" +
                              FGetSetting("SWIFT", "AltAlphCode",
                                          IF is-resident THEN "RUR"
                                          ELSE "RUB") +
                              TRIM(string( {abs ( IF flag-last THEN sh-bal
                                                 ELSE (sh-in-bal + srv))},
                                           "->>>>>>>>>>>9.99"))
                             )
                     ELSE ""
                    )
                   )
      buf-s = replace(buf-s,".",",")
      mStrSumm = IF in-currency EQ "" THEN  
                 TRIM(string( {abs ( IF flag-last THEN mTmpShBal
                                    ELSE (mTmpShInBal + sr))},
                              "->>>>>>>>>>>9.99"))  
              ELSE (TRIM(string( {abs ( IF flag-last THEN mTmpShVal
                                       ELSE (mTmpShInVal + sv))},
                                 "->>>>>>>>>>>9.99"))
                   )
      mStrSumm = replace(mStrSumm,".",",")
      .
   
   &IF DEFINED(FrmMsg) &THEN
   IF {&FrmMsg} NE "942" THEN
   DO:
   &ENDIF
      mBalance = IF acct.currency EQ "" 
                     THEN sh-bal
                     ELSE sh-val.
      mBalanceS = IF acct.currency EQ "" 
                     THEN sh-in-bal + sr
                     ELSE sh-in-val + sv.
      buf-work = ":62" +
                 ( IF flag-last THEN "F:" ELSE "M:") + 
                 ( IF (flag-last AND
                      (mBalance LT 0 OR 
                      (mBalance EQ 0 AND acct.side EQ "П") 
                      )
                     ) OR 
                     (NOT flag-last AND
                      (mBalanceS LT 0 OR 
                      (mBalanceS EQ 0 AND acct.side EQ "П") 
                       )
                      )
                  THEN "C"
                  ELSE "D"
                 ) +
                 {date-swf.i &date="in-op-date2"} + curr-iso + buf-s.
      RUN cr-buf-copy-doc(buf-work).
   &IF DEFINED(FrmMsg) &THEN
   END.
   IF {&FrmMsg} EQ "942" AND
      flag-last THEN
   DO:
      ASSIGN
         buf-s = TRIM(string( {abs s950-db} ,"->>>>>>>>>>>9.99"))
         OVERLAY(buf-s,INDEX(buf-s,"."),1) = ","
         buf-work = ":90D:" + string(q950-db) + curr-iso + buf-s.
      RUN cr-buf-copy-doc(buf-work).

      buf-s = TRIM(string( {abs s950-cr} ,"->>>>>>>>>>>9.99")).
      OVERLAY(buf-s,INDEX(buf-s,"."),1) = ",".

      buf-work = ":90C:" + string(q950-cr) + curr-iso + buf-s.
      RUN cr-buf-copy-doc(buf-work).
   END.
   ELSE
   &ENDIF
   IF flag-last THEN
   DO:
      mBalance = IF acct.currency EQ "" 
                     THEN mTmpShBal
                     ELSE mTmpShVal.
      buf-work = ":64:" +
                 ( IF mBalance LT 0 OR
                    (mBalance EQ 0 AND acct.side EQ "П")
                     THEN "C"
                  ELSE "D") +
                 {date-swf.i
                    &date= mLastDate
                 } + curr-iso + mStrSumm.
      RUN cr-buf-copy-doc(buf-work).
   END.
   buf-work = IF (last-op modulo num-op-stmt EQ 0) OR
                 num-op-stmt EQ 1 OR
                 last-of(topen.acct)
                 THEN end-file
              ELSE end-msg.
   RUN cr-buf-copy-doc(buf-work).
END.

/* --- e-950i.i was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:44am --- */
