/**
����᪨� �ࠢ� �ਭ�������: ��� ���� ����
�᭮�����:      �� �.1588
�� ������:     �뢮��� ᯨ᮪ ���㤭����
��� ࠡ�⠥�:   �⡨ࠥ� ��, � ������ ���� ������ ��� 550-� ��㯯�
��ࠬ����:      
���� ����᪠:  
������:         21.03.2017 ���ᮢ �.�.
*/

{globals.i}

DEFINE TEMP-TABLE ttFl      NO-UNDO
    FIELD persid    AS INTEGER
    FIELD pname     AS CHARACTER
    .

FOR EACH signs
    WHERE (signs.file-name  EQ 'acct')
      AND (signs.code       EQ 'groupOABS')
      AND (signs.code-value EQ '599')
    NO-LOCK,
FIRST acct
    WHERE (acct.acct        EQ ENTRY(1, signs.surrogate))
      AND (acct.currency    EQ ENTRY(2, signs.surrogate))
      AND (acct.acct    BEGINS "40817")
      AND (acct.cust-cat    EQ "�")
    NO-LOCK,
FIRST person
    WHERE (person.person-id EQ acct.cust-id)
    NO-LOCK
/*  BREAK BY person.person-id */ :

/*  IF FIRST-OF(person.person-id)
    THEN DO:
*/      CREATE ttFl.
        ASSIGN
            ttFl.persid = person.person-id
            ttFl.pname  = person.name-last + " " + person.first-names
            .
/*  END. */
END.

/* ���㠫����� ᯨ᪠ ���㤭���� ============================================ */
DEFINE VARIABLE num-line AS INT64 INITIAL 0 NO-UNDO. /* �ॡ�� navigate.cqr */
FORM
    ttFl.pname      FORMAT 'x(50)'      COLUMN-LABEL '������� ��� ����⢮' HELP "������� ��� ����⢮"
    ttFl.persid     FORMAT '>>>>>>9'    COLUMN-LABEL '���'                  HELP "���浪��� �����"
WITH FRAME browse1
TITLE COLOR BRIGHT-WHITE '����� ���������� ��������'
SCROLLABLE.

{qrdef.i
    &buff-list = "ttFl"
    &Sortby    = "'by ttFl.pname'"
}

{navigate.cqr
    &file       = "ttFl"
    &workfile   = "/*"
    &avfile     = "ttFl"

    &maxfrm     = 1
    &bf1        = "ttFl.pname ttFl.persid"
    &help-label = "'F1 ��ᬮ��|ESC ��室'"

    &nodel      = yes
    &look       = "pb_sotr.f1 "
    &return     = "return2.cqr "
        &rfld   = persid
}
{intrface.del}
