DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE VARIABLE mGar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGarVid AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGarDias AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDogType AS CHARACTER NO-UNDO.
DEFINE VARIABLE mObType AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mI AS INT64 NO-UNDO.
DEFINE VARIABLE mNum AS INT64 NO-UNDO.

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.
DEFINE BUFFER term-obl1 FOR term-obl.
DEFINE BUFFER op FOR op.
DEFINE BUFFER op-entry FOR op-entry.

ASSIGN
mGar = "�।��,�।��,�।���,�।���"
mGarVid = "��⮬�����;!��⮬�����,*"
mGarDias = "��⮬�����,�����⢮,��,�����⥫��⢮"
.
/*
OUTPUT TO VALUE("tag8.txt") CONVERT TARGET "1251".
*/
FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
	FOR EACH term-obl 
		WHERE term-obl.contract EQ loan.contract
		AND term-obl.cont-code EQ loan.cont-code
		AND term-obl.idnt EQ 5
		AND term-obl.sop-date EQ ?
		NO-LOCK
		BY term-obl.nn:
		mSurr = term-obl.contract + "," + term-obl.cont-code + "," + "5" + "," + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn).
		mDogType = GetXAttrValueEx ("term-obl", 
									mSurr, 
									"��������", 
									"").
		DO mI = 1 TO NUM-ENTRIES(mGar):
			IF ENTRY(mI, mGar) EQ mDogType THEN
				ASSIGN
					mNum = mI
					mI = 5.
		END.
		IF mNum EQ 1 OR mNum EQ 2 THEN
		DO:
			mNum = (IF GetXAttrValueEx ("term-obl", 
										mSurr, 
										"�����", 
										"") EQ "��⮬�����" THEN 1 ELSE 2).
		END.
		mObType = ENTRY(mNum, mGarDias).

		FIND LAST loan-acct
			WHERE loan-acct.contract EQ loan.contract
			AND loan-acct.cont-code EQ loan.cont-code
			AND loan-acct.acct-type EQ IF term-obl.nn EQ 0 THEN ENTRY(mNum, mGar) ELSE ENTRY(mNum, mGar) + TRIM(STRING(term-obl.nn))
		    NO-LOCK NO-ERROR.

		FIND LAST loan-cond
			WHERE loan-cond.contract EQ loan.contract
			AND loan-cond.cont-code EQ loan.cont-code
		    NO-LOCK NO-ERROR.

		FOR EACH term-obl1
			WHERE term-obl1.contract EQ loan.contract
			AND term-obl1.cont-code EQ loan.cont-code
			AND term-obl1.idnt = 2
			NO-LOCK
			BY term-obl1.end-date:
/*			AND term-obl1.end-date EQ loan-cond.since
		    NO-LOCK NO-ERROR.
*/
			LEAVE.
		END.
		PUT UNFORMATTED
	/* ��� ���ᯥ祭�� */
		"0"
		"^"
	/* �����䨪��� ��������⥫� �� �������� */
		STRING(term-obl.fop)
		"^"
	/* ���. ������ ᤥ���  */
		mObType
		"^"
	/* ����� ������� */
		GetXAttrValueEx ("term-obl", 
						mSurr, 
						"��������", 
						"")
		"^"
	/* ��� �������  */
		STRING(YEAR(term-obl.fop-date), "9999") + STRING(MONTH(term-obl.fop-date), "99") + STRING(DAY(term-obl.fop-date), "99")
		"^"
	/* ��� �����஢���� */
		STRING(YEAR(term-obl.fop-date), "9999") + STRING(MONTH(term-obl.fop-date), "99") + STRING(DAY(term-obl.fop-date), "99")
		"^"
	/* ��� ����砭�� ����⢨� ������� (��������) */
		STRING(YEAR(term-obl.end-date), "9999") + STRING(MONTH(term-obl.end-date), "99") + STRING(DAY(term-obl.end-date), "99")
		"^"
	/* �ப (� ����) */
		TRIM(STRING(term-obl.end-date - term-obl.fop-date + 1, ">>>>>>9"))
		"^"
	/* �㬬� �।�� */
		(IF AVAILABLE(term-obl1) THEN TRIM(STRING(term-obl1.amt-rub, ">>>>>>>>>>>9.99")) ELSE "0.00")
		"^"
	/* ����� */
		IF term-obl.currency EQ "" THEN "810" ELSE term-obl.currency
		"^"
	/* �㬬�, �ਭ��� � ���ᯥ祭��    (��������� �⮨�����) */
		TRIM(STRING(term-obl.amt-rub, ">>>>>>>>>>>9.99"))
		"^"
	/* ��ࠢ������� �⮨����� */
		"0"
		"^"
	/* �����䨪��� ���客�⥫� */
		""
		"^"
	/* �㬬� ���客���� */
		""
		"^"
	/* ����� ���客��� ������� */
		""
		"^"
	/* ��� ���客��� ������� */
		""
		"^"
	/* ��� ����砭�� ����⢨� ���客��� ������� */
		""
		"^"
	/* �����䨪��� ��ᯥ���  */
		""
		"^"
	/* �ਬ�砭�� ��� 91312 */
		IF AVAILABLE(loan-acct) THEN TRIM(ENTRY(1, loan-acct.acct, "@")) ELSE "��� ���"
		"^"
	/* ������ */
		""
		"^"
	/* �����䨪��� ������� ���ᯥ祭�� �� ���譥� ��⥬� */
		STRING(RECID(term-obl))
		"^"
	/* �����䨪��� �।�⭮�� ������� �� ���譥� ��⥬� */
		iRecIDloan 
		"^"
	/* �����樥�� ��⥣�ਨ ����⢠ */
		GetXAttrValueEx ("term-obl", 
						mSurr, 
						"��玡�ᯥ�", 
						"")
		"^"
	/* ���⥬� */
		'��� "���� ����"'
		CHR(13) + CHR(10)
		.
	END.
END.
/*
OUTPUT CLOSE.
*/
{intrface.del}
