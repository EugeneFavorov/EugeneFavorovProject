Define Input Param cAccountNumber As Charact No-undo. /* ����� ��� ������      */
Define Input Param dCurrentDate   As Date    No-undo. /* ��� ����樮����� ���   */

Define Output Param dOstatokR     As Decimal No-undo. /* ��室�騩 �����           */
Define Output Param dOstatokC     As Decimal No-undo. /* ��室�騩 �����           */

Define Var xdDebet      As Decimal No-undo. /* ��室�騩 �����              */
Define Var xdCredit     As Decimal No-undo. /* ��室�騩 �।��             */
Define Var xdVarDebet   As Decimal No-undo. /* ������ �� ������            */
Define Var xdVarCredit  As Decimal No-undo. /* ������ �� �।���           */
Define Var xdMaxDate    As Date    No-undo. /* ��� ��᫥����� �����⮣� ���*/
Define Var cValute      As Charact No-undo. /* ��� ����樮����� ���   */
cValute = Substring(cAccountNumber,6,3).
/* ��室�� ������ ��� ��� 㪠����� ���� */
Find First acct-pos Where acct-pos.since = dCurrentDate And acct-pos.acct = cAccountNumber no-lock no-error.
If Avail acct-pos Then dOstatokR = Abs(acct-pos.balance).
Else 
Do:  /* �᫨ ���� �� �� ������ */
  /* ��室�� ��᫥���� ������� ���� �� ������� ���� */
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
  /* ��室�� �㬬� ����⮢�� � �।�⮢�� ����⮢ �� ���� �� ��ਮ� �� 
	  ���� ��᫥����� �����⮣� ��� �� 㪠������ ���� */
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
  /* ��室�� ��室�騥 ���⪨ �� dCurrentDate */
	dOstatokR = Abs(dOstatokR - xdVarDebet + xdVarCredit).
End.
If cValute <> '810' Then
Do:
  /* ��室�� ������ ��� ��� 㪠����� ���� */
  Find First acct-cur Where acct-cur.since = dCurrentDate And acct-cur.acct = cAccountNumber No-lock No-error.
  If Avail acct-cur Then dOstatokC = Abs(acct-cur.balance).
  Else 
  Do:  /* �᫨ ���� �� �� ������ */
    /* ��室�� ��᫥���� ������� ���� �� ������� ���� */
    Find Last acct-cur Where acct-cur.acct = cAccountNumber And acct-cur.since < dCurrentDate No-lock No-error.
    If Avail acct-cur Then 
    Assign
      xdMaxDate = acct-cur.since
      dOstatokC = Abs(acct-cur.balance).
    Else 
    Assign
      xdMaxDate = Date( "01.11.2000" )
      dOstatokC = 0.
    /* ��室�� �㬬� ����⮢�� � �।�⮢�� ����⮢ �� ���� �� ��ਮ� �� 
  	  ���� ��᫥����� �����⮣� ��� �� 㪠������ ���� */
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
    /* ��室�� ��室�騥 ���⪨ �� dCurrentDate */
  	dOstatokC = Abs(dOstatokC - xdVarDebet + xdVarCredit).
  End.
End.
Return.
