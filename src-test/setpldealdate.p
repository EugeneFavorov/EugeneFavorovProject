/* kam */

{globals.i}
{intrface.get xclass}
{tmprecid.def}
{setdest.i}

def var surr as char.
def var PLDealDate as char no-undo.
def var i as int init 0.
def var tmpStr as char no-undo.
FOR EACH tmprecid no-lock,
    EACH loan WHERE
    RECID(loan) EQ tmprecid.id NO-lock:

    surr = loan.contract + ',' + loan.cont-code.
    tmpStr = GetXAttrValueEx("loan", surr, "PLDealID", "").
    if trim(tmpStr) = "" then do:
        UpdateSigns(loan.class-code,
            surr,
            "PLDealID",
            STRING(recid(loan)),
            ?).
    end.
    
    PLDealDate = STRING(loan.open-date,"99/99/9999").
    PLDealDate = PLDealDate + ' ' + STRING(TIME,"HH:MM:SS.000").
    tmpStr = GetXAttrValueEx("loan", surr, "PLDealDate", "").
    if trim(tmpStr) = "" then do:
       /* PLDealDate выгружается datetime (DD/MM/YYYY HH:MM:SS.MMM) */
       UpdateSigns(loan.class-code,
            surr,
            "PLDealDate",
            PLDealDate,
            ?).
    end.
   
   
    put unformatted loan.cont-code skip.

        i = i + 1.
  
END.
  put unformatted 'всего: ' string(i) ' договоров' skip.

{preview.i}
