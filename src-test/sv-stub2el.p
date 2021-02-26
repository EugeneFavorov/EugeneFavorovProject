/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: sv-stub2.p
      Comment: ���室��� ��� ���� ���⮢ �� 䨭.�������
   Parameters: 1 - ��� ����, 2 - ����� ������, 3 - ��� 䨫���� (�᫨ �� 㪠��� - � ⥪�騩)
         Uses:
      Used by:
      Created: Olenka
     Modified:  18/09/2003 YUSS  19105 �������� �९����� hibeg
                                    �᫨ ����� nobeg,� �� ������� hibeg
                                    ���� in-beg-date �� ��ࠦ�����
     Modified:  08/12/2003 ILVI  19493 �������� �맮� 蠡����                                     
*/

DEFINE INPUT PARAMETER s AS CHARACTER NO-UNDO.

DEFINE VARIABLE in-DataClass-Id LIKE DataClass.DataClass-Id NO-UNDO.
DEFINE VARIABLE in-branch-id    LIKE DataBlock.Branch-Id    NO-UNDO.
DEFINE VARIABLE out-Data-Id     LIKE DataBlock.Data-Id      NO-UNDO.
DEFINE VARIABLE in-beg-date     LIKE DataBlock.beg-date     NO-UNDO.
DEFINE VARIABLE in-end-date     LIKE DataBlock.end-date     NO-UNDO.
DEFINE VARIABLE in-partition    LIKE user-proc.partition    NO-UNDO.
DEFINE VARIABLE mFilialList AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNmFil AS CHARACTER NO-UNDO.

{globals.i}
{norm.i NEW}

ASSIGN
mFilialList = "0000,0300"
mNmFil = "gb,tf"
.
{getdate.i}
{setdirel.i}

DO mI = 1 TO NUM-ENTRIES(mFilialList):
ASSIGN
   in-beg-date     = end-date
   in-end-date     = end-date
   &IF DEFINED(ONLY_SPOD) EQ 0 &THEN
      in-dataClass-Id = ENTRY(2,s)
   &ENDIF
   in-branch-id    = ENTRY(mI, mFilialList)  
.
/*
{norm-beg.i 
   &nobeg     = YES 
   &hibeg     = YES 
   &title     = "'��������� ������'" 
   &is-branch = YES
   &with-zo   = YES}
*/
{justamin}

&IF DEFINED(ONLY_SPOD) EQ 0 &THEN

RUN sv-get.p (       in-dataClass-Id, 
                     in-branch-id, 
                     in-end-date, 
                     in-end-date, 
              OUTPUT out-data-id).

&ENDIF

OUTPUT STREAM fil CLOSE.

IF out-data-id = ? THEN
   RUN normdbg IN h_debug (0,"�訡��", "���������� ������� ����� " +
                           "�� ������ ~"" + in-dataclass-id + "~"!").
ELSE IF CAN-FIND(FIRST norm WHERE 
                       norm.norm EQ ENTRY(1,s)) THEN 
DO:
   in-partition = IF PROGRAM-NAME(1) EQ PROGRAM-NAME(2) 
                     THEN "" 
                     ELSE in-dataclass-id.
   RUN norm-rpt.p (in-partition,ENTRY(1, s),in-branch-id,in-end-date,in-end-date).
END.
ELSE IF SearchPFile(ENTRY(1, s)) THEN 
DO:
   RUN VALUE(ENTRY(1, s) + ".p") (out-data-id) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
      MESSAGE "�� ���� ��������" ENTRY(1, s) + ".p"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE
   MESSAGE "��楤��" ENTRY(1, s) + ".p" "�� �������!"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

{norm-end.i 
   &nopreview = YES 
   &nofil     = YES}
OS-COPY VALUE("./_spool.tmp") VALUE( mDir + mDirElec + "//balance-" + ENTRY(mI, mNmFil) + "-" + STRING(end-date, "99.99.9999") + ".txt").
END.
{chmoddir.i}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='01/09/2015 16:57:24.915+04:00' */
/* $LINTUSER='yuss' */
/* $LINTMODE='1' */
/* $LINTFILE='sv-stub2.p' */
/*prosign8vrRMamQGCCg1WOtq8f85Q*/