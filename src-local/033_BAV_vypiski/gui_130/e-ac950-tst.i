
/* +++ e-ac950-tst.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:29am +++ */

/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     �஢����, �㦭��� �஢���� ������ � �믨᪥
��� ࠡ�⠥�:   �ࠢ������ ����� op � �� �믨᪨ / ���萠�室 - ����室
��ࠬ����:      op-entry, op, acct - ��।����� �� ���譥� ���㫥
                &corr  - db/cr
                &label - ���譨� 横� �� op-entry
�ᯮ������ �: 
������:         20.10.2016 ���ᮢ �.�.
*/

{&corr}StatusTst:
DO:
    FOR EACH half-entry OF op
        NO-LOCK:

        IF (half-entry.acct-{&corr} BEGINS "3")   /* ���譨� */
        THEN
            IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "�" &ELSE "�" &ENDIF )
                                 THEN FGetSetting("�믨᪨", "���萠�室", "")
                                 ELSE FGetSetting("�믨᪨", "�����室", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.

        IF (half-entry.acct-{&corr} BEGINS "2")   /* ���ᮢ� */
        THEN
            IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "�" &ELSE "�" &ENDIF )
                                 THEN FGetSetting("�믨᪨", "���ᐠ�室", "")
                                 ELSE FGetSetting("�믨᪨", "�����室", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.
    END.

    /* ��⠫�� */
    IF (op.op-status LT ( IF (acct.side EQ &IF "{&corr}" EQ "cr" &THEN "�" &ELSE "�" &ENDIF )
                         THEN FGetSetting("�믨᪨", "��␠�室", "")
                         ELSE FGetSetting("�믨᪨", "����室", "")))
    THEN NEXT {&label}.
END.

/* --- e-ac950-tst.i was humbly modified by (c)blodd converter v.1.09 on 11/3/2016 9:29am --- */
