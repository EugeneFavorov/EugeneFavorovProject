{globals.i}
{intrface.get tmess}

/* +++ pb_acct##.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:24am +++ */

/* ��⮤ Look �� �� acctb: ����1�����, ����2�����, ���⁂���� */

def input param in-class as char no-undo.
def input param in-acct  as char no-undo.
def input param level    as INT64  no-undo.

run acct#.p (ENTRY(1, in-acct),ENTRY(2, in-acct),level).

/* --- pb_acct##.p was humbly modified by (c)blodd converter v.1.09 on 5/30/2016 7:24am --- */
