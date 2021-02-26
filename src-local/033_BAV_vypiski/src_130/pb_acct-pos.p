/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     ������ ���⪮� ��� �믨᮪ � ��⮬ ⠡���� ����ᮢ
��� ࠡ�⠥�:   ����⠢��� ���.�஢��� op-entry-check � �ணࠬ�� acct-pos.i (� ���㫥 acct-p0.i)
��ࠬ����:      ��।����� �� ���譥� ���㫥 :
                op-entry   - �஢��塞�� �஢����,
                in-acct    - �/�, ��� ���ண� ��⠥��� ���⮪,
                half-entry - ���� ��� ����஢����,
                lDoNext    - �ਧ��� �ய�᪠ �஢����.
���� ����᪠:  
������:         27.10.2016 ���ᮢ �.�.
*/

{globals.i}             /** �������� ��।������ */
{sh-defs.i}
{zo-defs.i}

&SCOPED-DEFINE op-entry-check  /* ���.�஢�ઠ � 横�� �� �஢����� ��� ���᫥��� ���⪠ */ ~
    StatusTst: ~
    DO: ~
        lDoNext = FALSE. ~
 ~
        FIND FIRST op OF op-entry ~
            NO-LOCK NO-ERROR. ~
        FIND FIRST acct ~
            WHERE (acct.acct    EQ in-acct) ~
            NO-LOCK NO-ERROR. ~
 ~
        IF (op-entry.acct-db EQ in-acct) ~
        THEN DO:                                    /* ����⮢�� �஢���� */ ~
            FOR EACH half-entry OF op ~
                NO-LOCK: ~
 ~
                IF (half-entry.acct-cr BEGINS "3")  /* ���譨� */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "���萠�室", "") ~
                                                               ELSE FGetSetting("�믨᪨", "�����室", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
 ~
                IF (half-entry.acct-cr BEGINS "2")  /* ���ᮢ� */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "���ᐠ�室", "") ~
                                                               ELSE FGetSetting("�믨᪨", "�����室", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
            END. ~
 ~
            /* ��⠫�� */ ~
            IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "��␠�室", "") ~
                                                       ELSE FGetSetting("�믨᪨", "����室", ""))) ~
            THEN lDoNext = TRUE. ~
        END. ~
        ELSE DO:                                    /* �।�⮢�� �஢���� */ ~
            FOR EACH half-entry OF op ~
                NO-LOCK: ~
 ~
                IF (half-entry.acct-db BEGINS "3")  /* ���譨� */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "���萠�室", "") ~
                                                               ELSE FGetSetting("�믨᪨", "�����室", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
 ~
                IF (half-entry.acct-db BEGINS "2")  /* ���ᮢ� */ ~
                THEN DO: ~
                    IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "���ᐠ�室", "") ~
                                                               ELSE FGetSetting("�믨᪨", "�����室", ""))) ~
                    THEN lDoNext = TRUE. ~
                    LEAVE StatusTst. ~
                END. ~
            END. ~
 ~
            /* ��⠫�� */ ~
            IF (op.op-status LT (IF (acct.side EQ "�") THEN FGetSetting("�믨᪨", "��␠�室", "") ~
                                                       ELSE FGetSetting("�믨᪨", "����室", ""))) ~
            THEN lDoNext = TRUE. ~
        END. ~
    END. ~
    IF lDoNext THEN NEXT.

DEFINE VARIABLE lDoNext     AS LOGICAL  NO-UNDO.
DEFINE BUFFER   half-entry  FOR op-entry.

{acct-pos.i &Type = "acct-pos"}
