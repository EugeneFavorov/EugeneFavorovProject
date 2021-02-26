
disable triggers for load of loan.
disable triggers for load of loan-acct.
disable triggers for load of kau.
disable triggers for load of kau-entry.
disable triggers for load of kau-pos.
disable triggers for load of loan-cond.
disable triggers for load of signs.
disable triggers for load of kau-cur.

def input param name_old as char no-undo.
def var str_ as char.
/*def var name_old as char.*/
def var name_new as char.

name_new = entry(2,name_old,";").
name_old = entry(1,name_old,";").

{setdest.i}
output to "test.d".



for each loan where loan.contract = "dps"
                and loan.doc-ref = name_old
                  .
    str_ = loan.cont-code.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    loan.cont-code = str_.

    str_ = loan.doc-ref.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    loan.doc-ref = str_.

end.

for each loan-cond where loan-cond.contract = "dps"
                and loan-cond.cont-code begins name_old
                  .
    str_ = loan-cond.cont-code.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    loan-cond.cont-code = str_.

end.

for each signs where signs.file = "loan"
                 and signs.surrogate begins "dps," + name_old
               .
    str_ = signs.surrogate .
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    signs.surrogate  = str_.
end.

for each signs where signs.file = "loan-cond"
                 and signs.surrogate begins "dps," + name_old
               .
    str_ = signs.surrogate .
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    signs.surrogate  = str_.
end.

for each loan-acct where loan-acct.contract = "dps"
                     and loan-acct.cont-code begins name_old
                  .
    str_ = loan-acct.cont-code .
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    loan-acct.cont-code  = str_.
end.

for each kau where kau.kau begins "dps," + name_old
                  .
    str_ = kau.kau.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    kau.kau = str_.
end.
for each kau-entry where kau-entry.kau begins "dps," + name_old
                  .
    str_ = kau-entry.kau.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    kau-entry.kau = str_.
end.
for each kau-pos where kau-pos.kau begins "dps," + name_old
                  .
    str_ = kau-pos.kau.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    kau-pos.kau = str_.
end.
for each kau-cur where kau-cur.kau begins "dps," + name_old
                  .
    str_ = kau-cur.kau.
    str_ = replace(str_,name_old,name_new).
MESSAGE str_ view-as alert-box.
    kau-cur.kau = str_.
end.
output close.

{preview.i}