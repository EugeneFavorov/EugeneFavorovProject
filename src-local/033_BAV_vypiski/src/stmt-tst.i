/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      
�� ������:     �஢����, �㦭��� �஢���� ������ � �믨᪥
��� ࠡ�⠥�:   �ࠢ������ ����� op � �� �믨᪨ / ���萠�室 - ����室
��ࠬ����:      op-entry, op, acct - ��।����� �� ���譥� ���㫥
                &corr  - db/cr
                &label - ���譨� 横� �� op-entry
�ᯮ������ �: 
������:         01.01.2012 ���ᮢ �.�.
*/

{&corr}StatusTst:
DO:
    FOR EACH half-entry OF op
        NO-LOCK:

        IF (half-entry.acct-{&corr} BEGINS "3")   /* ���譨� */
        THEN
            IF (op.op-status LT (IF (((acct.side EQ "�") AND ("{&corr}" EQ "cr"))
                                  OR ((acct.side EQ "�") AND ("{&corr}" EQ "db"))) THEN FGetSetting("�믨᪨", "���萠�室", "")
                                                                                   ELSE FGetSetting("�믨᪨", "�����室", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.

        IF (half-entry.acct-{&corr} BEGINS "2")   /* ���ᮢ� */
        THEN
            IF (op.op-status LT (IF (((acct.side EQ "�") AND ("{&corr}" EQ "cr"))
                                  OR ((acct.side EQ "�") AND ("{&corr}" EQ "db"))) THEN FGetSetting("�믨᪨", "���ᐠ�室", "")
                                                                                   ELSE FGetSetting("�믨᪨", "�����室", "")))
            THEN NEXT {&label}.
            ELSE LEAVE {&corr}StatusTst.
    END.

    /* ��⠫�� */
    IF (op.op-status LT (IF (((acct.side EQ "�") AND ("{&corr}" EQ "cr"))
                          OR ((acct.side EQ "�") AND ("{&corr}" EQ "db"))) THEN FGetSetting("�믨᪨", "��␠�室", "")
                                                                           ELSE FGetSetting("�믨᪨", "����室", "")))
    THEN NEXT {&label}.
END.
