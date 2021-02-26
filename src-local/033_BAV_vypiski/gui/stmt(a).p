{globals.i}
{intrface.get tmess}

/* +++ stmt(a).p was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:47am +++ */

{globals.i}
{intrface.get print}

def input param beg like op-entry.op-date.
def input param dob like op-entry.op-date.
def input param ac  like acct.acct no-undo.
def input param cur like acct.currency no-undo.

DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mMaxWidthTxt AS INT64     NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
/* Вставка Плюс банк */
DEFINE BUFFER   half-entry   FOR op-entry.   /* буфер для stmt-tst.i */
/* Конец вставки Плюс банк */
{initstrp.i}
{op-print.pro}

{stmt.i new}
end-date = dob. /* чтобы корректно определился год ЗО в след. инклюднике */
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


/* --- stmt(a).p was humbly modified by (c)blodd converter v.1.11 on 5/25/2017 6:47am --- */
