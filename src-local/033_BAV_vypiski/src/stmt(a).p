{globals.i}
{intrface.get print}

def input param beg like op-entry.op-date.
def input param dob like op-entry.op-date.
def input param ac  like acct.acct no-undo.
def input param cur like acct.currency no-undo.

DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mMaxWidthTxt AS INT64     NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
/* ��⠢�� ���� ���� */
DEFINE BUFFER   half-entry   FOR op-entry.   /* ���� ��� stmt-tst.i */
/* ����� ��⠢�� ���� ���� */
{initstrp.i}
{op-print.pro}

{stmt.i new}
end-date = dob. /* �⮡� ���४⭮ ��।������ ��� �� � ᫥�. ������� */
beg-date = beg.

{getstmt.i}

{find-act.i
    &bact = acct
    &acct = ac
    &curr = cur
}

{stmt-prt.i &NEXT    ="RETURN"
            &begdate = beg
            &enddate = dob
            &bufacct = acct
}

{stmtview.i}
{intrface.del}

