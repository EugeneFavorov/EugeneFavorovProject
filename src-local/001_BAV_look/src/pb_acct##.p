/* Метод Look на ДР acctb: Карт1ВнСчет, Карт2ВнСчет, КартБВнСчет */

def input param in-class as char no-undo.
def input param in-acct  as char no-undo.
def input param level    as INT64  no-undo.

run acct#.p (ENTRY(1, in-acct),ENTRY(2, in-acct),level).
