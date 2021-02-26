/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chkpersdoc.p
      Comment: �஢�ઠ �ப� ����⢨� ���㬥�� ������
   Parameters: ���
         Uses:
      Used by:
      Created: 10.10.2012 
*/

{globals.i}
{intrface.get xclass}
{intrface.get tmess}
{intrface.get pbase}

&GLOBAL-DEFINE CustIdntSurr    cust-ident.cust-code-type + ',' + ~
                               cust-ident.cust-code      + ',' + ~
                               STRING(cust-ident.cust-type-num)
                               
DEFINE INPUT PARAMETER iOpKind   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCustCat  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iCustId   AS INT64     NO-UNDO.
DEFINE INPUT PARAMETER iOpDate   AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER iMessType AS CHARACTER NO-UNDO.
DEFINE VAR mChkPersDoc AS CHARACTER NO-UNDO.
DEFINE VAR mEndDate    AS DATE NO-UNDO.
DEFINE VAR mErr        AS CHARACTER INIT "NO"     NO-UNDO.
DEFINE VAR mPersClient AS CHARACTER NO-UNDO. /* ������ �� �����⢫��騩 ������ */
DEFINE VAR mNameClient AS CHARACTER NO-UNDO.

IF {assigned iOpKind} THEN 
   FIND FIRST op-kind WHERE op-kind.op-kind = iOpKind NO-LOCK NO-ERROR.
ELSE IF {assigned GetBaseOpkind()} THEN
   FIND FIRST op-kind WHERE op-kind.op-kind = GetBaseOpkind() NO-LOCK NO-ERROR.

RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}",
   "~nmChkPersDoc = " + STRING(mChkPersDoc) + 
   "~nAVAIL(op-kind)" + STRING(AVAIL(op-kind))).

IF AVAIL op-kind THEN
/*   mChkPersDoc = GetXattrValueEx("op-kind",op-kind.op-kind,"�ப����",FGetSetting("�⠭���","�ப����","���")).*/
   mChkPersDoc = GetXattrValueEx("op-kind",op-kind.op-kind,"�ப����",FGetSetting("�⠭���","���ப����","���")).
ELSE 
   IF GetSysConf("PROCESS_OP-EDIT") = "��" THEN 
      mChkPersDoc = FGetSetting("�⠭���","�ப����","���").
   ELSE 
      mChkPersDoc = "���".
      
RUN dbgprint.p (PROGRAM-NAME(1) + " line = {&line-number}","mChkPersDoc = " + STRING(mChkPersDoc)).
      
mPersClient = GetSysConf("�����⥫�।��").
IF iCustCat = "�" AND CAN-DO("��,��",mChkPersDoc) 
THEN DO:
   FIND FIRST cust-corp WHERE cust-corp.cust-id = iCustId NO-LOCK NO-ERROR.
   IF NOT (AVAIL cust-corp
           AND 
           (   GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "�।��", "") <> ""
            OR GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id), "��ꥪ�", "") = "���"
            OR CAN-DO(FGetSetting("�⠭���","����ᔋ��",""),cust-corp.cust-stat))
          ) 
   THEN
      mChkPersDoc = "���".
END.
      
IF CAN-DO("��,��",mChkPersDoc) THEN DO:
   FOR EACH cust-ident USE-INDEX cust
                       WHERE cust-ident.cust-cat        = iCustCat
                         AND cust-ident.cust-id         = iCustId
                         AND cust-ident.class-code      = "p-cust-ident"
                         AND cust-ident.close-date      = ?
   NO-LOCK BREAK BY cust-ident.cust-code-type BY
                    cust-ident.open-date:

      IF LAST-OF(cust-ident.cust-code-type) AND LAST-OF(cust-ident.open-date)
         AND CAN-FIND(FIRST code WHERE code.class = "�������"
                                   AND code.code  = cust-ident.cust-code-type 
                                   AND code.misc[2] = "��")
      THEN DO:
         mEndDate = DATE(GetXattrValueEx("cust-ident",{&CustIdntSurr},"end-date","")).
         
         IF mEndDate < iOpDate THEN DO:
            mNameClient = "".
            IF iCustCat = "�" THEN DO:
               FIND FIRST person WHERE person.person-id = cust-ident.cust-id NO-LOCK NO-ERROR.
               IF AVAIL person THEN
                  mNameClient = person.name-last + " " + person.first-names.
            END.
            ELSE DO:
               FIND FIRST cust-corp WHERE cust-corp.cust-id = cust-ident.cust-id NO-LOCK NO-ERROR.
               IF AVAIL cust-corp THEN
                  mNameClient = cust-corp.cust-stat + " " + cust-corp.name-corp.
            END.
            
            IF {assigned mPersClient} THEN
            DO:
               RUN Fill-SysMes IN h_tmess ("",
                                           "comm15",
                                           "1",
                                           "%s=" + mNameClient +
                                           "%s=" + cust-ident.cust-code-type + 
                                           "%s=" + STRING(mEndDate)).       
               LEAVE.
            END.
            ELSE
            DO:
               RUN Fill-SysMes IN h_tmess ("",
                                           "comm15",IF mChkPersDoc = "��" THEN "-1" ELSE iMessType,
                                           "%s=" + mNameClient +
                                           "%s=" + cust-ident.cust-code-type + 
                                           "%s=" + STRING(mEndDate) + "|�த������,�⬥����").

               IF   pick-value = "yes" 
                  OR pick-value = "2" 
                  OR pick-value = ?  
               THEN
                  mErr = "YES".
               LEAVE.
            END.
         END.
         ELSE
            NEXT.
      END.
   END.         
END.
{intrface.del}
RETURN mErr.
/* $LINTFILE='chkpersdoc.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='shoi' */
/* $LINTDATE='10/02/2017 22:40:18.275+03:00' */
/*prosignYKvk0JqkcovSCzgg/TEBwQ*/