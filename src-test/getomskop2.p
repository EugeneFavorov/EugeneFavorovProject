/*
������᪠� ��⥣�஢����� ��⥬� �������
Copyright:
Filename:    getomskop2.p
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{globals.i}
{intrface.get xclass}

{getopomsk.i}

DEFINE INPUT PARAMETER hNo AS INT.

/*DEFINE VARIABLE hNo AS INT NO-UNDO.*/

DEF VAR iCnt        AS INT NO-UNDO.
DEF VAR iCnt2       AS INT NO-UNDO.
DEF VAR iCnt3       AS INT NO-UNDO.
DEF VAR iCntLoaded  AS INT64 NO-UNDO.
DEF VAR iCntDeleted AS INT64 NO-UNDO.
DEF VAR iCntCreated AS INT64 NO-UNDO.
DEF VAR mCntCreated AS INT64 NO-UNDO.
DEF VAR vStartTime  AS DATETIME NO-UNDO.
DEF VAR shF         AS CHAR NO-UNDO.
DEF VAR vSurr       AS CHAR NO-UNDO.
DEF VAR curOp       AS INT64 NO-UNDO.
DEF VAR mDprID      AS CHAR  NO-UNDO.

DEFINE VARIABLE mCorrPacket AS LOGICAL NO-UNDO.

DEF BUFFER post_change_h FOR bank.post_change_h.
DEF BUFFER vpost_change  FOR bank.post_change_h.

DEF BUFFER post_over_h   FOR bank.post_over_h.
DEF BUFFER b-over        FOR bank.post_over_h.

DEF BUFFER post-banker   FOR bank.post-banker.
DEF BUFFER vpost         FOR bank.post-banker.

DEFINE TEMP-TABLE  tt-inpostno
   FIELD inpostno  AS INT64.

DEFINE TEMP-TABLE  tt-docno
   FIELD docno     AS INT64
   FIELD resultobr AS INT64
   FIELD errortext AS CHARACTER.

iCnt = 0.
iCntLoaded = 0.
iCntDeleted = 0.
iCntCreated = 0.
vStartTime = NOW.
shF = shFilial.
etime(TRUE).

DEF VAR qh AS HANDLE.

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}","Begin ��⮪ - " + STRING(hNo)).

CREATE QUERY qh.

/*hNo = 5.*/

IF hNo NE 5 THEN
DO:
   CASE hNo:
      WHEN 1 THEN DO:
         qh:SET-BUFFERS(BUFFER bank.post-h1-mfr:HANDLE).
         qh:QUERY-PREPARE("FOR EACH post-h1-mfr NO-LOCK BY post-h1-mfr.date_modify QUERY-TUNING ( NO-INDEX-HINT)").
      END.
      WHEN 2 THEN DO:
         qh:SET-BUFFERS(BUFFER bank.post-h2-mfr:HANDLE).
         qh:QUERY-PREPARE("FOR EACH post-h2-mfr NO-LOCK BY post-h2-mfr.date_modify QUERY-TUNING ( NO-INDEX-HINT)").
      END.
      WHEN 3 THEN DO:
         qh:SET-BUFFERS(BUFFER bank.post-h3-mfr:HANDLE).
         qh:QUERY-PREPARE("FOR EACH post-h3-mfr NO-LOCK BY post-h3-mfr.date_modify QUERY-TUNING ( NO-INDEX-HINT)").
      END.
   END.
   
   iCnt = 0.
   qh:QUERY-OPEN.
   REPEAT:
      qh:GET-NEXT().
      IF qh:QUERY-OFF-END THEN LEAVE.

      iCnt = iCnt + 1.
      
      FIND FIRST bank.post_change_h WHERE
         post_change_h.id EQ qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("id"):BUFFER-VALUE
         USE-INDEX IN_POST_CHANGE_H_ID
      /*EXCLUSIVE-LOCK*/ NO-WAIT NO-ERROR.
      
      IF NOT AVAIL(post_change_h) OR post_change_h.status_ NE 0 THEN NEXT.
   
      /* �᫨ ���� � bank.post_over_h �ய�᪠�� - ����� �� ��ࠡ��뢠�� */
      FIND FIRST bank.post_over_h WHERE
         bank.post_over_h.docno EQ qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("docno"):BUFFER-VALUE
      NO-LOCK NO-ERROR.

      IF AVAIL(bank.post_over_h) THEN
      DO:
         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
            " bank.post_change_h.docno = " + STRING(bank.post_change_h.docno) +
            " AVAIL(bank.post_over_h)  = " + STRING(AVAIL(bank.post_over_h))).
         NEXT.
      END.
      /* �᫨ ���� � bank.post_over_h �ய�᪠�� - ����� �� ��ࠡ��뢠�� */

      RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}"," ��ࠡ�⪠ �� ������ ���㬥�⮢").

      DO TRANSACTION ON ERROR UNDO, THROW:
         DO ON ERROR UNDO, THROW: /* ERR */
            FOR FIRST bank.post-mfr WHERE 
                  post-mfr.docno EQ post_change_h.docno
               NO-LOCK QUERY-TUNING ( NO-INDEX-HINT):
               LEAVE.
            END.
            /*   䨫��� �।�⮢����  ???????   */
            IF AVAIL bank.post-mfr THEN 
            DO:
               IF CAN-DO("0500", post-mfr.deb_filial_id) THEN
               DO:
                  /*����㧪� �஢����*/
                  RUN LoadPostMfr(BUFFER post-mfr,hNo,OUTPUT iCntCreated).
                  mCntCreated = mCntCreated + iCntCreated.
               END.
            END.
            ELSE
            DO:  /*������ �� �������, ����� ���� 㤠���� */
               FIND FIRST signs WHERE 
                  signs.dec-value     EQ post_change_h.docno
                  AND signs.FILE-NAME EQ 'op-entry'
                  AND signs.CODE      EQ 'link-docno'
               NO-LOCK NO-ERROR.
               IF AVAIL signs THEN
               DO:
                  vSurr = signs.surrogate.
                  FIND FIRST op-entry WHERE
                     op-entry.op = INTEGER(ENTRY(1,vSurr))
                     AND op-entry.op-entry EQ INTEGER(ENTRY(2,vSurr)) 
                  NO-ERROR.
                  IF AVAIL op-entry THEN 
                  DO:
                     shFilial = op-entry.filial-id.
               
                     FIND FIRST op WHERE op.op EQ INTEGER(ENTRY(1,vSurr)) NO-LOCK NO-ERROR.
                     
                     IF op.user-id NE 'SYNC' THEN UNDO, THROW NEW Progress.Lang.AppError( "���㬥�� ������ �� ���짮��⥫�� SYNC").
   
   	               /* �஢�ઠ �� ����⢮����� �痢� �� ���㬥�� */
                     FOR EACH xlink WHERE 
                        xlink.class-code EQ op.class-code 
                        NO-LOCK,
                        EACH qbis.links WHERE 
                        qbis.links.link-id = xlink.link-id
                        AND (qbis.links.source-id = STRING(op.op) OR qbis.links.target-id = STRING(op.op)) 
                        NO-LOCK:
                        UNDO, THROW NEW Progress.Lang.AppError( "���㬥�� ����� �裡 � �� ����� ���� 㤠���").
                     END.
   
                     /* �஢�ઠ �� ������� ���� */
                     IF ClDay( op-entry.filial-id, op.op-date, op-entry.acct-cat) THEN
                        UNDO, THROW NEW Progress.Lang.AppError( "���� " + (IF op-entry.op-date EQ ? THEN "?" ELSE STRING( op-entry.op-date)) + " ������ �� 䨫���� " + op-entry.filial-id + " 㤠����� ����������").
                     
                     IF op-entry.filial-id = '0000' AND op.op-date < date( 2, 10, 2015) THEN
                     UNDO, THROW NEW Progress.Lang.AppError( "���� " + STRING( post-mfr.postdate) + " ����饭. ��������� ��  �� ��� ����������.").
   
                     IF op-entry.filial-id = '0300' AND op.op-date GE date( 9, 5, 2015) THEN
                     UNDO, THROW NEW Progress.Lang.AppError( "���� " + STRING( post-mfr.postdate) + " ����饭. ��������� ��  �� ��� ����������.").
                     
                     FOR EACH signs WHERE
                        signs.FILE-NAME EQ 'op-entry'
                        AND signs.surrogate EQ STRING(vSurr)
                        EXCLUSIVE-LOCK:
                        DELETE signs. 
                     END.
                     PUT UNFORMATTED "㤠���� �஢���� docno=" 
                        bank.post_change_h.docno ";" 
                        op-entry.op              ";" 
                        op-entry.op-entry        ";"
   		               op-entry.acct-db         ";"
   		               op-entry.acct-cr         ";"
   		               op-entry.amt-cur         ";"
   		               op-entry.amt-rub         ";"
   		            SKIP.
                     DELETE op-entry.
   
                     DEF BUFFER bop-entry1 FOR op-entry.
                     FIND LAST bop-entry1 USE-INDEX op-entry WHERE 
                        bop-entry1.op = INTEGER(ENTRY(1,vSurr)) 
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL bop-entry1 THEN 
                     DO:
                        FIND FIRST op WHERE 
                           op.op EQ INTEGER(ENTRY(1,vSurr)) 
                        EXCLUSIVE-LOCK NO-ERROR.
                        IF AVAIL op THEN 
                        DO:
                           FOR EACH op-bank WHERE 
                              op-bank.op EQ INTEGER(ENTRY(1,vSurr))
                              EXCLUSIVE-LOCK:
                              DELETE op-bank.
                           END.
                           FOR EACH signs WHERE 
                              signs.FILE-NAME EQ 'op'
                              AND signs.surrogate EQ ENTRY(1,vSurr):
                              DELETE signs.
                           END.
                           DELETE op.
                        END.
                     END. /* IF NOT AVAIL bop-entry1 */
                     iCntDeleted = iCntDeleted + 1.
                  END. /* IF AVAIL op-entry */
               END. /* IF AVAIL signs  */
            END. /* IF AVAIL bank.post-mfr */
            
            /*ASSIGN
	         bank.post_change_h.status_ = 1.
	         VALIDATE bank.post_change_h.*/
       
            DELETE bank.post_change_h.
            RELEASE bank.post_change_h.
       
            CATCH eAnyError AS Progress.Lang.Error:
               /* IF INDEX(RETURN-VALUE + " " + eAnyError:GetMessage(1), 'filial') < 1 THEN */
               PUT UNFORMATTED 'docno='
               qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("docno"):BUFFER-VALUE
               /* vpost_change.docno */
               ": " + RETURN-VALUE + " " + eAnyError:GetMessage(1) SKIP.
               IF AVAIL bank.post_change_h THEN 
               DO:
                  ASSIGN
                     bank.post_change_h.status_ = -1
                     bank.post_change_h.errortext = RETURN-VALUE + " " + eAnyError:GetMessage(1).
                  VALIDATE bank.post_change_h.
                  RELEASE bank.post_change_h.
               END.
            END CATCH.
         END. /* ERR */
      END. /* DO TRANSACTION */
      
      /*IF iCnt >= 1000 THEN LEAVE.*/
      /*IF INTERVAL( NOW, vStartTime, 'seconds') > 155 THEN LEAVE.*/
   END.  /*REPEAT*/
    
   qh:QUERY-CLOSE().
   DELETE OBJECT qh.
    
   shFilial = shF.
      
   IF iCnt GT 0
   THEN
      PUT UNFORMATTED
         STRING(NOW,"99/99/9999 HH:MM:SS") + "~n" +
         '   ��ࠡ�⠭� �஢���� ' + STRING(iCnt) + "~n" +
      	'   ᮧ���� '            + STRING(mCntCreated) + "~n" +
      	'   㤠���� '            + STRING(iCntDeleted) + "~n" +
      	'   �६� '              + STRING(etime(false)) + ' �ᥪ.' SKIP.
   ELSE
      PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") SKIP.
END.
ELSE IF hNo EQ 5 THEN
DO:
   FOR EACH bank.post_over_h WHERE TRUE
      
      /*AND bank.post_over_h.date_modify GE DATE("01/11/2017")*/
      /*AND bank.post_over_h.date_modify LT DATE("02/11/2017")*/
      
      /*AND bank.post_over_h.docno EQ 193114661*/
      
      /*AND (bank.post_over_h.inpostno EQ 192274636*/
      /*OR bank.post_over_h.inpostno EQ 192544463)*/
      
      NO-LOCK QUERY-TUNING(NO-INDEX-HINT)
      BREAK BY bank.post_over_h.inpostno:
      
      IF bank.post_over_h.status_ NE 0 THEN
      DO:
         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
            "status_ NE 0 -  �ய�饭 docno = " + STRING(bank.post_over_h.docno)).
         NEXT.
      END.
      
      IF bank.post_over_h.type_change NE 1 THEN
      DO:
         RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
            "type_change NE 1 - �ய�饭 docno = " + STRING(bank.post_over_h.docno)).
         NEXT.
      END.
      
      /*RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",    */
      /*   "��ࠡ��뢠���� docno = " + STRING(bank.post_over_h.docno)).*/
         
      IF FIRST-OF(bank.post_over_h.inpostno) THEN
      DO:
         CREATE tt-inpostno.
         ASSIGN
            tt-inpostno.inpostno = bank.post_over_h.inpostno.
      END.
   END.
   INPOSTNO:
   FOR EACH tt-inpostno NO-LOCK:
      iCnt2 = 0.
      iCnt3 = 0.
      DO TRANSACTION ON ERROR UNDO, THROW:
         /* �஢�ઠ �� ����稥 �஢���� � ��᪢�� bank.post_over_h.op */
         FOR EACH bank.post_over_h WHERE TRUE
            AND bank.post_over_h.inpostno EQ tt-inpostno.inpostno
            NO-LOCK:
            IF bank.post_over_h.op NE ? AND bank.post_over_h.op GT 0 THEN
            DO:
               FIND FIRST op-entry WHERE TRUE
                  AND op-entry.op       EQ bank.post_over_h.op
                  AND op-entry.op-entry EQ bank.post_over_h.op_entry
               EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAIL(op-entry) THEN   /*����� ���, � �뫠*/   /*�஢�७�*/
               DO:
                  FOR EACH b-over WHERE
                     b-over.inpostno EQ bank.post_over_h.inpostno
                     EXCLUSIVE-LOCK:
                     ASSIGN
                        b-over.status_   = - 1
                        b-over.errordate = TODAY
                        b-over.errortext = "�� ������� �஢���� � ��᪢�� �� bank.post_over_h.op = " + STRING(bank.post_over_h.op).
                  END.
                  
                  RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                     " �� ������� �஢���� � ��᪢�� �� bank.post_over_h.op!~nbank.post_over_h = " + STRING(bank.post_over_h.docno) +
                     " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno)).
                  NEXT INPOSTNO.
               END.
               ELSE
               DO:
                  IF CAN-DO("�*",op-entry.op-status) THEN   
                  DO:
                     FOR EACH b-over WHERE
                        b-over.inpostno EQ bank.post_over_h.inpostno
                        EXCLUSIVE-LOCK:
                        ASSIGN
                           b-over.status_   = - 1
                           b-over.errordate = TODAY
                           b-over.errortext = "�஢���� �� bank.post_over_h.op � ��᪢�� ���㫨஢��� bank.post_over_h.op = " + STRING(bank.post_over_h.op).
                     END.
                     
                     RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                        " �஢���� �� bank.post_over_h.op � ��᪢�� ���㫨஢���!~nbank.post_over_h = " + STRING(bank.post_over_h.docno) +
                        " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno)).
                     NEXT INPOSTNO.
                  END. 
               END.
            END.
         END.
         /* ��� �஢������ ᭠砫� �।�⮢�� �� 408 */
         FOR EACH bank.post_over_h WHERE TRUE
            AND bank.post_over_h.inpostno EQ tt-inpostno.inpostno
            NO-LOCK:
            iCnt2 = iCnt2 + 1.

            FOR EACH bank.post-mfr WHERE TRUE
               AND bank.post-mfr.docno   EQ bank.post_over_h.docno
               AND bank.post-mfr.credacc BEGINS("408")
               NO-LOCK QUERY-TUNING (NO-INDEX-HINT):
               LEAVE.
            END.
            
            RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                     " bank.post_over_h.docno = " + STRING(bank.post_over_h.docno) + " AVAIL(bank.post-mfr) =  " + STRING(AVAIL(bank.post-mfr))).
            
               
            IF AVAIL(bank.post-mfr) THEN 
            DO:
               IF CAN-DO("0500",post-mfr.deb_filial_id) THEN
               DO:
                  /* �᫨ ���� op */
                  IF bank.post_over_h.op NE ? AND bank.post_over_h.op GT 0 THEN
                  DO:
                     RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                        " ���� bank.post_over_h.op - bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                        " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                        " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                        " bank.post-mfr.credacc = " + bank.post-mfr.credacc +
                        " bank.post-mfr.debacc  = " + STRING(bank.post_over_h.op)).
                     
                     FIND FIRST op-entry WHERE TRUE
                        AND op-entry.op       EQ bank.post_over_h.op
                        AND op-entry.op-entry EQ bank.post_over_h.op_entry
                     EXCLUSIVE-LOCK NO-ERROR.
                     
                     IF AVAIL(op-entry) THEN
                     DO:
                        /* 
                        �஢���� �᫨ ��� � ����� ���. 
                        �᫨ �� ���� � ���⠢��� ��� ��릠 � ����
                        */
                        IF CAN-DO("40817....0599*,40820....0599*",op-entry.acct-cr)
                           AND NOT CAN-DO("202*",op-entry.acct-db)
                           AND op-entry.op-status EQ "���" THEN
                        DO:
                        	ASSIGN
                        	   op-entry.op-status = CHR(251) + CHR(251).
                        	VALIDATE op-entry.
                        	
                        	FIND FIRST op WHERE
                        	   op.op EQ op-entry.op
                        	EXCLUSIVE-LOCK NO-ERROR.
                        	IF AVAIL(op) THEN
                           DO:
                        	   ASSIGN op.op-status = CHR(251) + CHR(251).
                              VALIDATE op.
                           
                              sUpdateSigns(op.class-code,STRING( op.op),"����6","�⢥ত���",?).
                           END.
                           
                           sUpdateSigns("op-entry",STRING(op-entry.op) + "," + STRING(op-entry.op-entry),"pc_over","��",?).
                           
                           RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                              " ����� ����� � ���� �� bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                              " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                              " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                              " bank.post-mfr.credacc = " + bank.post-mfr.credacc).
                           
                           CREATE tt-docno.
                           ASSIGN
                              tt-docno.docno = bank.post-mfr.docno.
                           iCnt3 = iCnt3 + 1.
                        END.
                        /* �஢���� �᫨ ��� � ����� ���. 
                        �᫨ ���� � ⮦� ��� ��릠 � ���� */
                        ELSE IF CAN-DO("40817....0599*,40820....0599*",op-entry.acct-cr)   /*�஢�७�*/
                           AND CAN-DO("202*",op-entry.acct-db)
                           AND op-entry.op-status EQ "���" THEN
                        DO:
                        	FIND FIRST op WHERE
                        	   op.op EQ op-entry.op
                        	EXCLUSIVE-LOCK NO-ERROR.
                        	IF AVAIL(op) THEN
                           DO:
                        	   ASSIGN op.op-status = CHR(251) + CHR(251).
                              VALIDATE op.
                              sUpdateSigns(op.class-code,STRING( op.op),"����6","�⢥ত���",?).
                           END.
                           
                           RUN add-an.p (INPUT op.op,OUTPUT mDprID). 
                           
                           ASSIGN 
                              op-entry.kau-db  = mDprID
                        	   op-entry.op-status = CHR(251) + CHR(251).
                        	VALIDATE op-entry.
                           
                           RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                              " op-entry.kau-db = " + op-entry.kau-db).
                           
                           sUpdateSigns("op-entry",STRING(op-entry.op) + "," + STRING(op-entry.op-entry),"pc_over","��",?).
                           
                           RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                              " ����� ����� � ���� �� bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                              " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                              " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                              " bank.post-mfr.credacc = " + bank.post-mfr.credacc).
                           
                           CREATE tt-docno.
                           ASSIGN
                              tt-docno.docno = bank.post-mfr.docno.
                           iCnt3 = iCnt3 + 1.
                        END.
                        /* �஢���� �᫨ �� ��� ��祣� �� ������. */
                        ELSE
                        DO:
                           RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                              " ������ op �� bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                              " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                              " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                              " bank.post-mfr.credacc = " + bank.post-mfr.credacc).
                           
                           sUpdateSigns("op-entry",STRING(op-entry.op) + "," + STRING(op-entry.op-entry),"pc_over","��",?).
                           
                           CREATE tt-docno.
                           ASSIGN
                              tt-docno.docno = bank.post-mfr.docno.
                           iCnt3 = iCnt3 + 1.
                        END.
                     END.
                     RELEASE op-entry.
                  END.
                  ELSE    /* �।�� 408 � ��� op*/
                  DO:    
                     /*����㧪� �஢���� � ��᪢��*/
                     RUN LoadPostMfr(BUFFER post-mfr,hNo,OUTPUT iCntCreated).
                     RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                        " �������� �� bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                        " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                        " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                        " bank.post-mfr.credacc = " + bank.post-mfr.credacc).
                     CREATE tt-docno.
                     ASSIGN
                        tt-docno.docno = bank.post-mfr.docno.
                     iCnt3 = iCnt3 + 1.
                  END.
               END.
            END.
            ELSE
            DO:
               FOR EACH b-over WHERE
                  b-over.id EQ bank.post_over_h.id
                  EXCLUSIVE-LOCK:
                  
                  RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                     "bank.post_over_h.docno " + STRING(bank.post_over_h.docno) + " �ய�饭, ��� � bank.post-mfr").                     
                  
                  ASSIGN
                     b-over.status_   = - 1
                     b-over.errordate = TODAY
                     b-over.errortext = "���㬥�� ��� � bank.post-mfr".
               END.
            END.
         END.
         /* ��⮬ �� ��⠫�� �஬� �।�⮢�� �� 408 */
         FOR EACH bank.post_over_h WHERE TRUE
            AND bank.post_over_h.inpostno EQ tt-inpostno.inpostno
            NO-LOCK:
            iCnt2 = iCnt2 + 1.
            FOR EACH bank.post-mfr WHERE TRUE
               AND bank.post-mfr.docno EQ bank.post_over_h.docno
               AND NOT (bank.post-mfr.credacc BEGINS("408"))   
               NO-LOCK QUERY-TUNING (NO-INDEX-HINT):
               LEAVE.
            END.
            IF AVAIL(bank.post-mfr) THEN 
            DO:
               IF CAN-DO("0500",post-mfr.deb_filial_id) THEN
               DO:
                  /*����㧪� �஢���� � ��᪢��*/
                  RUN LoadPostMfr(BUFFER bank.post-mfr,hNo,OUTPUT iCntCreated).
                  RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                     " �������� �� bank.post-mfr.docno = " + STRING(bank.post-mfr.docno) +
                     " bank.post_over_h.inpostno  = " + STRING(bank.post_over_h.inpostno) +
                     " bank.post-mfr.debacc  = " + bank.post-mfr.debacc +
                     " bank.post-mfr.credacc = " + bank.post-mfr.credacc).
                  CREATE tt-docno.
                  ASSIGN
                     tt-docno.docno = bank.post-mfr.docno.
                  iCnt3 = iCnt3 + 1.
               END.
            END.
            DO:
               FOR EACH b-over WHERE
                  b-over.id EQ bank.post_over_h.id
                  EXCLUSIVE-LOCK:
                  
                  RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
                     "bank.post_over_h.docno " + STRING(bank.post_over_h.docno) + " �ய�饭, ��� � bank.post-mfr").                     
                  
                  ASSIGN
                     b-over.status_   = - 1
                     b-over.errordate = TODAY
                     b-over.errortext = "���㬥�� ��� � bank.post-mfr".
               END.
            END.
         END.
      END.
   END.
   /*�㦭� 㤠���� �� bank.post_over_h � bank.post_change_h*/
   FOR EACH tt-docno NO-LOCK:
      FIND FIRST bank.post_change_h WHERE
         bank.post_change_h.docno EQ tt-docno.docno
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL(bank.post_change_h) THEN
      DO:
         DELETE bank.post_change_h.
         RELEASE bank.post_change_h.
      END.
      /**/
      FIND FIRST bank.post_over_h WHERE
         bank.post_over_h.docno EQ tt-docno.docno
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL(bank.post_over_h) THEN
      DO:
         DELETE bank.post_over_h.
         RELEASE bank.post_over_h.
      END.
      RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}"," �������� tt-docno = " + STRING(tt-docno.docno)).
   END.
END.

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}","End").

RETURN.
