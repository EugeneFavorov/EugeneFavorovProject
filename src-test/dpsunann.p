/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: DPSUNANN.P
      Comment: ��㯯���� �⪠� � ������� ���㬥�⮢
   Parameters: iType
         Uses:
      Used by:
      Created: 04.09.2003 19:13 ilvi    
     Modified: 29.06.2010 19:37 rija     0130090: �� - �訡�� �� ��� � 0108990 -
                                         㤠�����/���㫨஢���� ���㬥��
     Modified: 13.01.2016 17:44 seei     0275019  
     Modified: 18.03.2016 15:00 seei     0279884                                  
*/
{globals.i}
{intrface.get card}
{intrface.get db2l}
{intrface.get xclass}
{intrface.get tmess}
{chkopmf.i}
{card_del.i}

DEFINE INPUT PARAMETER iType AS CHARACTER NO-UNDO.

DEFINE VARIABLE lastmess     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iall         AS INT64   NO-UNDO.
DEFINE VARIABLE iok          AS INT64   NO-UNDO.
DEFINE VARIABLE ierr         AS INT64   NO-UNDO.
DEFINE VARIABLE vStr         AS CHARACTER NO-UNDO.
DEFINE VARIABLE mTrPCStr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOp          AS INT64   NO-UNDO.
DEFINE VARIABLE mAuthFlg     AS LOGICAL NO-UNDO.
DEFINE VARIABLE mAlert       AS LOGICAL   NO-UNDO. /* �����誠 ��� crt-rvrs-opint.p */

DEFINE BUFFER xlock-op FOR op.
DEFINE BUFFER buf-op       FOR op.
DEFINE BUFFER op       FOR op.

DEF STREAM fout.

{tmprecid.def}
{tmprecid.def &PREF=local &NGSH=YES}

{setdest.i 
   &cols   = 128 
   &STREAM = "stream fout"}

RUN rid-rest.p (OUTPUT TABLE localtmprecid).
_tran:                        
DO TRANSACTION ON ERROR  UNDO _tran, LEAVE _tran
               ON ENDKEY UNDO _tran, LEAVE _tran:

   FOR EACH localtmprecid NO-LOCK:
      ASSIGN
         vOp      = 0
         lastmess = ""
         iall = iall + 1
      .
      FIND FIRST xlock-op WHERE RECID(xlock-op) = localtmprecid.id EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL xlock-op THEN DO:
         FIND FIRST op WHERE RECID(op) = localtmprecid.id NO-LOCK NO-ERROR.
         DELETE localtmprecid.
         IF LOCKED(xlock-op) THEN DO:
            vStr = "������ ।�������� ��㣨� ���짮��⥫��".
            WhoLocks2(RECID(op),"op", INPUT-OUTPUT vStr).
         END.
         ELSE vStr = "������ 㤠���� ��㣨� ���짮��⥫��".
         RUN PutLog(vStr).
         IF AVAILABLE op THEN
         DO:
            ASSIGN
               vOp = op.op
            .
         END.
      END.
      ELSE
      DO:
         ASSIGN
            vOp = xlock-op.op
         .
      END.
   END.

   tt: FOR EACH localtmprecid NO-LOCK ON ERROR  UNDO tt, LEAVE _tran
                                      ON ENDKEY UNDO tt, LEAVE _tran:

      lastmess = "".
      
      FIND FIRST op WHERE
              RECID(op) EQ localtmprecid.id NO-LOCK NO-ERROR.
              
      IF CAN-DO("015,02",op.doc-type) THEN
      DO:
         DEF VAR mPrim AS CHARACTER NO-UNDO FORMAT "x(210)".
         mPrim = GetXAttrValueEx("op",GetSurrogateBuffer("op",(BUFFER op:HANDLE)),"�ਬ�砭��","").
         DEF FRAME prim_frame
            mPrim VIEW-AS FILL-IN SIZE 35 BY 1
               NO-LABEL    
               HELP  "�ਬ�砭�� ��� �� � ��"
               WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 DOWN WIDTH 37
               TITLE COLOR BRIGHT-WHITE "[ �ਬ�砭�� ��� �� � �� ]".
         
         DISP mPrim WITH FRAME prim_frame.
         PAUSE 0.
/*         DO TRANSACTION ON ERROR  UNDO, LEAVE                        */
/*                        ON ENDKEY UNDO, LEAVE WITH FRAME  prim_frame:*/
         DO WITH FRAME  prim_frame:                           
            UPDATE mPrim.
         END.
         HIDE FRAME prim_frame NO-PAUSE.
         UpdateSigns("op",STRING(op.op),"�ਬ�砭��",mPrim, NO).
      END.
         
      IF CheckSrc(op.class-code,op.op) THEN DO:
         RUN PutLog("��������! ��������/���㫨஢���� ���㬥�� ����������. ���� �易��� �室�騩 ���㬥�� � ��㣮� 䨫����.").
         UNDO, NEXT tt.      
      END.
      
      IF CheckTrg(op.class-code,op.op,mTargetId) THEN DO:
         FIND FIRST buf-op WHERE buf-op.op EQ mTargetId NO-LOCK NO-ERROR.

         RUN dpsopb.p (RECID(buf-op), 1, YES).
         IF RETURN-VALUE GT "" THEN DO:
            RUN PutLog(RETURN-VALUE).      
            UNDO, NEXT tt.
         END.
      END.
      
      RUN dpsopb.p (localtmprecid.id, iType, YES).
      IF RETURN-VALUE GT "" THEN DO:
         RUN PutLog(RETURN-VALUE).      
         UNDO, NEXT tt.
      END.
      IF iType EQ "2" THEN
      DO:
         ASSIGN
            mTrPCStr = GetXAttrValueEx("op",GetSurrogateBuffer("op",(BUFFER op:HANDLE)),"�࠭���","")
            mAuthFlg = fGetSetting("������", "���������", "���") EQ "��"
         .
         IF NOT mAuthFlg THEN DO:
         /* ��ନ�㥬 ���⭮� ����祭�� */
         mAlert = FALSE.
         RUN crt-rvrs-opint.p(op.op, INPUT-OUTPUT mAlert).
         /* �������� ����७��� ����権 */
         RUN del-op-opint.p(op.op).
      END.
      END.
      ELSE
      DO:
         ASSIGN
            mTrPCStr = ""
         .
      END.

      RUN SetSysConf IN h_base ("TransPCOperation","StA").
      RUN DelCardByOp(localtmprecid.id, mTrPCStr) NO-ERROR.
      IF RETURN-VALUE EQ "ERROR" THEN
         UNDO, NEXT tt.
      iok = iok + 1.
      DELETE localtmprecid.
   END.
END.
RUN rid-keep.p (TABLE localtmprecid).

IF iall = 1 THEN DO:
   OUTPUT STREAM fout CLOSE.
   IF lastmess <> "" AND
      lastmess <> "no" THEN
      MESSAGE lastmess VIEW-AS ALERT-BOX ERROR.
END.
ELSE DO:
   ierr = iall - iok.
   PUT STREAM fout UNFORMATTED
      SKIP(1) "        �����:" SKIP
      "���㬥�⮢ ��� ����樨            :" iall SKIP
      "�����襭� ����権 ��� ���㬥�⠬� :" iok  SKIP
      "�訡�� ����樨 ��� ���㬥�⠬�    :" ierr SKIP.
   {preview.i 
      &STREAM = "stream fout"}
END.

DEFINE FRAME ferr 
   HEADER "������ ��������� �������� ��� �����������" WITH DOWN WIDTH 128.

PROCEDURE PutLog.
   DEFINE INPUT PARAM in-mess AS CHARACTER NO-UNDO.
   
   lastmess = in-mess.
   DISPLAY STREAM fout
      iall 
         LABEL "N" 
         FORMAT ">>>9"
      op.doc-num WHEN AVAIL op 
         FORMAT "x(12)"
      "id " + STRING(localtmprecid.id) WHEN NOT AVAIL op @ op.doc-num
      op.op-status WHEN AVAIL op
      op.op-date WHEN AVAIL op
      op.user-id WHEN AVAIL op
      lastmess 
         LABEL "������" 
         FORMAT "x(80)"
   WITH FRAME ferr.
   DOWN STREAM fout 1 WITH FRAME ferr.
   ierr = ierr + 1.
END PROCEDURE.

/* 0279884 SEEI:  ����᫮���� ���㧪� ����䥩ᮢ   */
FINALLY:
{intrface.del}
END FINALLY.
/* $LINTFILE='dpsunann.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='nikk' */
/* $LINTDATE='23/09/2016 09:48:46.187+03:00' */
/*prosignv6S7dJz3wZKxjTq7Sh/kjg*/