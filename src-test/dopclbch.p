/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename:  dopclbch.p
      Comment:  проверки документов по классификатору <ДопКонтрКЛБ>
         Uses:  
      Used by:  
      Created:  06.08.2015 IT

*/

&GLOBAL-DEFINE IS-DEBUG YES

{globals.i}
{tmprecid.def}
{intrface.get flt}      /* Инструмент для сбора данных из браузера. */
{intrface.get xclass}
{intrface.get swi}
 
DEFINE INPUT PARAMETER iRecIdOp AS RECID NO-UNDO.
DEFINE INPUT PARAMETER iOpDate  AS DATE  NO-UNDO.
                              
DEFINE VARIABLE mErrClass  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE mStError   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mError     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMesError  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mElemErr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI         AS INT64     NO-UNDO.


   FIND FIRST op WHERE RECID(op) EQ iRecIdOp
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAIL op THEN
   DO:
       IF LOCKED op 
       THEN DO:
                &IF DEFINED(IS-DEBUG) &THEN
                    RUN dbgprint.p ("dopclbch.p","LOCKED op").
                &ENDIF
                RUN wholocks2.p (iRecIdOp,"op" ,"").
            END.
       ELSE DO:
                &IF DEFINED(IS-DEBUG) &THEN
                    RUN dbgprint.p ("dopclbch.p","NOT AVAIL op").
                &ENDIF
                RETURN.
            END.
   END.

   FIND FIRST op-kind 
        WHERE op-kind.op-kind EQ op.op-kind NO-LOCK NO-ERROR.
   IF NOT AVAIL op-kind THEN 
   DO:
       &IF DEFINED(IS-DEBUG) &THEN
           RUN dbgprint.p ("dopclbch.p","NOT AVAIL op-kind " + op.op-kind).
       &ENDIF
       RETURN.
   END.

   ASSIGN 
      mErrClass = GetXattrValueEx("op-kind",op-kind.op-kind,"code-err","mess-error")
      mStError  = FGetSetting("SWIFT","st-err","В").
   .

   loopcheck:
   FOR EACH code NO-LOCK
      WHERE code.class EQ "ДопКонтрКЛБ" 
        AND code.val   EQ "действуетБИС" :
          
      &IF DEFINED(IS-DEBUG) &THEN
          RUN dbgprint.p ("dopclbch.p","code           : " + code.code).
          RUN dbgprint.p ("dopclbch.p","name           : " + code.name).
          RUN dbgprint.p ("dopclbch.p","misc[1]        : " + code.misc[1]).
          RUN dbgprint.p ("dopclbch.p","misc[2]        : " + code.misc[2]).
          RUN dbgprint.p ("dopclbch.p","misc[3]        : " + code.misc[3]).
          RUN dbgprint.p ("dopclbch.p","description[1] : " + code.description[1]).
      &ENDIF

      mElemErr = mErrClass + ":" + code.misc[3].

      IF {assigned code.name }  AND
         SearchPfile(code.name) THEN 
      DO:
         RUN VALUE(code.name + ".p") (iRecIdOp, code.misc[1]).

         &IF DEFINED(IS-DEBUG) &THEN
             RUN dbgprint.p ("dopclbch.p",code.name + ".p RETURN-VALUE: " + RETURN-VALUE).
         &ENDIF

         IF RETURN-VALUE EQ "FALSE" AND
            LOOKUP(mElemErr, mError) EQ 0 THEN 
         DO:
            {additem.i mError mElemErr}
            {additem.i mMesError code.description[1]}
         END.
      END.

      ELSE
      IF {assigned code.misc[2] } THEN
      DO:
          RUN SelectFltObject ("op",
                               "UserConf~001op-date1~001op-date2",
                               code.misc[2] + CHR(1) + STRING(iOpDate) + CHR(1) + STRING(iOpDate),
                               "op-date1~001op-date2").
          loop:
          FOR EACH tmprecid,
             FIRST op NO-LOCK
             WHERE RECID(op) EQ tmprecid.id :

             IF RECID(op) EQ iRecIdOp THEN
             DO:
                IF LOOKUP(mElemErr, mError) EQ 0 THEN 
                DO:
                   {additem.i mError mElemErr}
                   {additem.i mMesError code.description[1]}
                END.

                &IF DEFINED(IS-DEBUG) &THEN
                    RUN dbgprint.p ("dopclbch.p",STRING(op.op-date) + " - " + op.doc-num ).
                &ENDIF

                LEAVE loop.
             END.
          END.
      END.
   END.

   IF {assigned mError} THEN 
   DO:
       {additem.i op.op-error mError} 
       UpdateSigns(op.class-code,
                   STRING(op.op),
                   "СообщКонтр",
                   mMesError,
                   ?).

       {sw-error.fnd
             &pre-op-kind   = op.op-kind
             &pre-err-class = mErrClass
             &pre-TmpStr    = mError
             &pre-in        = "op.op-status =
                                  IF code.val BEGINS 'Ош' THEN
                                     MINIMUM(IF code.misc[2] EQ '' THEN mStError ELSE code.misc[2],op.op-status)
                                  ELSE 
                                     MINIMUM(op.op-status,GetStWarning(op.op-kind,op.op-status)).
                              "
             &pre-where     = "AVAIL code AND CAN-DO('Ош*,Пред*',code.val)"
             &pre-i         = mI
       }
   END.

{intrface.del}          /* Выгрузка инструментария. */