/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: sspin-proc.p 
      Comment: �롮� ��⮢ ��� �࠭���樨 ssp-in
   Parameters: iClass iInstance
         Uses:
      Used by:
      Created: 
*/

DEF INPUT PARAM iClass    AS CHARACTER NO-UNDO. /* ����� ��� �⡮� */
DEF INPUT PARAM iInstance AS HANDLE    NO-UNDO. /* ������ ������� ����� */

{globals.i}
{intrface.get tmess}
{intrface.get trans}
{intrface.get pbase}
{intrface.get xclass}
{intrface.get date}

{tmpobj.def}
{ttretval.def}

DEF VAR mFilterTable AS HANDLE      NO-UNDO. /* ���� ⠡���� ��� 䨫��� */
DEF VAR mBuffer      AS HANDLE      NO-UNDO. /* ���� ⠡����             */

DEFINE VARIABLE mCnt          AS INT64       NO-UNDO.
DEFINE VARIABLE mOk           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mInt          AS INT64       NO-UNDO.
DEFINE VARIABLE mInt1         AS INT64       NO-UNDO.
DEFINE VARIABLE mInt2         AS INT64       NO-UNDO.

DEFINE VARIABLE mStr          AS CHARACTER   NO-UNDO.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   /* �������� �������᪮� ⠡���� */
   RUN _CreateFilterTable IN h_trans (OUTPUT mFilterTable).

   mFilterTable:ADD-NEW-FIELD ("acct",     "character").
   mFilterTable:ADD-NEW-FIELD ("number",   "character").
   mFilterTable:ADD-NEW-FIELD ("currency", "character").
   mFilterTable:ADD-NEW-FIELD ("cust-cat", "character").
   mFilterTable:ADD-NEW-FIELD ("cust-id",  "int64").
   mFilterTable:ADD-NEW-INDEX ("number").
   
   mFilterTable:ADD-INDEX-FIELD("number","number").

   /* �������� ⠡���� */
   mFilterTable:TEMP-TABLE-PREPARE("FilterTable") NO-ERROR.

   RUN Fill-ProgressErr IN h_tmess ("").
   IF    ERROR-STATUS:ERROR
      OR NOT mFilterTable:PREPARED THEN
      UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
   
   mBuffer = mFilterTable:DEFAULT-BUFFER-HANDLE.
   mBuffer:FIND-FIRST() NO-ERROR.

   /*!*/
   mCnt = 0.
   mStr = "".



/* =======================================================
"FOR EACH ACCT WHERE
(ACCT.CLOSE-DATE EQ ?)
AND
(
(ACCT.CURRENCY EQ ''
AND CAN-DO('���揀,���ப,��,�����,�����',ACCT.CONTRACT))
OR
(ACCT.CURRENCY NE ''
AND ACCT.CONTRACT EQ '�����')
)
AND CAN-DO('!409*,407*,40802*,40807*,40821*',ACCT.NUMBER)
AND ACCT.FILIAL-ID = '" + ������() + "' NO-LOCK:"
======================================================= */   



   FOR EACH acct WHERE acct.filial-id   EQ shFilial
                   AND can-do('!409*,407*,40802*,40807*,40821*', acct.acct)
                   AND acct.close-date  EQ ?
                   AND (((acct.currency EQ '') AND (can-do('���揀,���ப,��,�����,�����', acct.contract))) OR ((acct.currency NE '') AND (acct.contract EQ '�����')))
                   /*AND acct.acct        NE "40702810500400010846     @0500"*/
                   /*AND acct.acct        EQ "40702978805170010001     @0500"*/
       NO-LOCK:      
                IF GetXAttrValueEx("acct",acct.acct + "," + acct.currency, "��ொ���", "") EQ "��"
                THEN NEXT.

                /*MESSAGE acct.acct VIEW-AS ALERT-BOX.*/

                mBuffer:BUFFER-CREATE().
                ASSIGN                                
                       mCnt                                             = mCnt + 1
                       mBuffer:BUFFER-FIELD ("__filterid"):BUFFER-VALUE = mCnt
                       mBuffer:BUFFER-FIELD ("acct")      :BUFFER-VALUE = acct.acct
                       mBuffer:BUFFER-FIELD ("number")    :BUFFER-VALUE = acct.number
                       mBuffer:BUFFER-FIELD ("currency")  :BUFFER-VALUE = acct.currency
                       mBuffer:BUFFER-FIELD ("cust-cat")  :BUFFER-VALUE = acct.cust-cat
                       mBuffer:BUFFER-FIELD ("cust-id")   :BUFFER-VALUE = acct.cust-id
                .
   END.
   


   /*�࠭����� */   
   IF mCnt NE 0 THEN          
   RUN g-fltr.p (iInstance,   
                 mFilterTable,
                 "",          
                 OUTPUT mOk). 
END.



{intrface.del}

IF mOk NE YES THEN
   RETURN {&RET-ERROR}.
