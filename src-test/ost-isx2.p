Define Input Param cAccountNumber As Charact No-undo. /* номер счета клиента      */
Define Input Param dCurrentDate   As Date    No-undo. /* дата операционного дня   */

Define Output Param dOstatokR     As Decimal No-undo. /* исходящий дебет           */
Define Output Param dOstatokC     As Decimal No-undo. /* исходящий дебет           */

Define Var xdDebet      As Decimal No-undo. /* исходящий дебет              */
Define Var xdCredit     As Decimal No-undo. /* исходящий кредит             */
Define Var xdVarDebet   As Decimal No-undo. /* обороты по дебету            */
Define Var xdVarCredit  As Decimal No-undo. /* обороты по кредиту           */
Define Var xdMaxDate    As Date    No-undo. /* дата последнего закрытого дня*/
Define Var cValute      As Charact No-undo. /* дата операционного дня   */
cValute = Substring(cAccountNumber,6,3).
/* находим закрыт или нет указанный день */
Find First acct-pos Where acct-pos.since = dCurrentDate And acct-pos.acct = cAccountNumber no-lock no-error.
If Avail acct-pos Then dOstatokR = Abs(acct-pos.balance).
Else 
Do:  /* если день еще не закрыт */
  /* находим последний закрытый день по данному счету */
  Find Last acct-pos Where acct-pos.acct = cAccountNumber And acct-pos.since < dCurrentDate no-lock no-error.
  If Avail acct-pos Then 
  Assign
    xdMaxDate = acct-pos.since
    dOstatokR = Abs(acct-pos.balance).
  Else 
  do:
  Assign
    xdMaxDate = Date( "01.11.2000" )
    dOstatokR = 0.
  end.
  /* находим сумму дебетовых и кредитовых оборотов по счету за период от 
	  даты последнего закрытого дня до указанной даты */
	xdVarDebet  = 0.
  xdVarCredit = 0.
  For Each op-entry Where 
      op-entry.op-date > xdMaxDate And 
      op-entry.op-date <= dCurrentDate And
      ( op-entry.acct-db = cAccountNumber OR op-entry.acct-cr = cAccountNumber ) No-lock:
    If op-entry.acct-db = cAccountNumber Then
      xdVarDebet = xdVarDebet + op-entry.amt-rub.
    If op-entry.acct-cr = cAccountNumber Then
      xdVarCredit = xdVarCredit + op-entry.amt-rub.
  End.
  /* находим исходящие остатки на dCurrentDate */
	dOstatokR = Abs(dOstatokR - xdVarDebet + xdVarCredit).
End.
If cValute <> '810' Then
Do:
  /* находим закрыт или нет указанный день */
  Find First acct-cur Where acct-cur.since = dCurrentDate And acct-cur.acct = cAccountNumber No-lock No-error.
  If Avail acct-cur Then dOstatokC = Abs(acct-cur.balance).
  Else 
  Do:  /* если день еще не закрыт */
    /* находим последний закрытый день по данному счету */
    Find Last acct-cur Where acct-cur.acct = cAccountNumber And acct-cur.since < dCurrentDate No-lock No-error.
    If Avail acct-cur Then 
    Assign
      xdMaxDate = acct-cur.since
      dOstatokC = Abs(acct-cur.balance).
    Else 
    Assign
      xdMaxDate = Date( "01.11.2000" )
      dOstatokC = 0.
    /* находим сумму дебетовых и кредитовых оборотов по счету за период от 
  	  даты последнего закрытого дня до указанной даты */
  	xdVarDebet  = 0.
    xdVarCredit = 0.
    For Each op-entry Where 
        op-entry.op-date > xdMaxDate And 
        op-entry.op-date <= dCurrentDate And
        ( op-entry.acct-db = cAccountNumber OR op-entry.acct-cr = cAccountNumber ) No-lock:
      If op-entry.acct-db = cAccountNumber Then
        xdVarDebet = xdVarDebet + op-entry.amt-cur.
      If op-entry.acct-cr = cAccountNumber Then
        xdVarCredit = xdVarCredit + op-entry.amt-cur.
    End.
    /* находим исходящие остатки на dCurrentDate */
  	dOstatokC = Abs(dOstatokC - xdVarDebet + xdVarCredit).
  End.
End.
Return.
