FUNCTION ConvertLoanInt CHAR(INPUT iId_K AS INT64, INPUT iId_D AS INT64):
    DEFINE VAR sRes AS CHAR NO-UNDO.
    sRes = ?.
    CASE iId_K:
        WHEN 3 THEN DO:
        	IF iId_D = 0 THEN sRes = '1'. /* 1 - �뤠� */
        END.
        WHEN 2 THEN DO:
        	IF iId_D = 1 THEN sRes = '2'. /* 2 - 㯫�� �� */
        END.
        WHEN 7 THEN DO:
        	IF iId_D = 5 THEN sRes = '2'. /* 2 - 㯫�� �� */
        END.
        WHEN 29 THEN DO:
        	CASE iId_D:
        		WHEN 30 THEN sRes = '4'. /* 4 - ����襭�� % */
        	END CASE.
        END.
        WHEN 248 THEN DO:
        	CASE iId_D: 	
        		WHEN 8 THEN sRes = '4'. /* 4 - ����襭�� % */
        	END CASE.
        END.
        WHEN 48 THEN DO:
        	CASE iId_D:
        		WHEN 30 THEN sRes = '4'. /* 4 - ����襭�� % */
        	END CASE.
		END.
		WHEN 6 THEN DO:
        	CASE iId_D:
        		WHEN 352 THEN sRes = '4'. /* 4 - ����襭�� % */
        	END CASE.
		END.
        WHEN 35 THEN DO:
        	CASE iId_D:
        		WHEN 5 THEN sRes = '4'. /* 4 - ����襭�� % */
        	END CASE.
        END.
        WHEN 301 THEN DO:
            sRes = '8'. /* 8 - ����襭�� �����ᨨ */
        END.
        WHEN 377 THEN DO:
            sRes = '8'. /* 8 - ����襭�� �����ᨨ */
        END.
    END CASE.
    RETURN sRes.
END FUNCTION.   



/*
DOG_ID	�������� ID �।�⭮�� �������, ������ ᮢ������ � DOG_ID �� 䠩�� CRED_DOG												
PAYDATE	��� ���⥦�												
DateBeginPer	��� ��砫� ��ਮ�� ���� (����易⥫쭮� ����)												
DateEndPer	��� ���� ��ਮ�� ���� (����易⥫쭮� ����)												
������ 梥⮬ �뤥����� ����襭��, � ���஬ �뫮 ��� (���筮� ����筮� ����襭��); ����襭�� �� �� ��䨪� 12731.56 � ��� �� �㬬� 25000.00													
OPERATION	������ :												
	1 �뤠� �।��												
	2 ����襭�� �।��												
	4 ����襭�� ��業⮢												
	8 ����襭�� �����ᨨ (�� ����稨)												
SUMMA_PLAT	�㬬� ���⥦� (��� ࠧ����⥫�� ��㯯� ࠧ�冷�, ��� �������� �����)												
CUR	����� �।�� (�᫮��� ���, ���ਬ�� 810)												
SP	��㦥���� ����, �ᥣ�� '0' (����)												
*/


DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
DEFINE INPUT  PARAMETER iI AS INT NO-UNDO.
DEF VAR datepereschet AS DATE NO-UNDO.

{tmprecid.def} 
{globals.i} 
{sh-defs.i}
{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR strOP AS CHAR.
DEFINE VAR lastOdDate AS DATE NO-UNDO.
DEFINE VAR lastPrDate AS DATE NO-UNDO.

lastOdDate = DATE("01/01/2001").
lastPrDate = DATE("01/01/2001").

IF iI = 0 THEN DO:
    PUT UNFORMATTED 'DOG_ID;;PAYDATE;DateBeginPer;DateEndPer;OPERATION;SUMMA_PLAT;CUR;SP' + CHR(13) + CHR(10).
END.

datepereschet = DATE("31/05/2016").

FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:
/*
	if loan.since <> datepereschet AND
	   AVAIL loan-int then do:
	    RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
	           loan.cont-code,      /* ����� �������. */
	           date(datepereschet),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
	           FALSE,		/* �������/�� ������� ������ �祭�� ������� */
	           TRUE).		/* �뢮����/ �� �뢮���� ��⮪�� �� �࠭ */
	end.
*/


  FOR EACH loan-int OF loan WHERE loan-int.mdate <= datepereschet NO-LOCK:
  	strOP = ConvertLoanInt(loan-int.id-k,loan-int.id-d).
  	IF strOP <> ? THEN DO:
	PUT UNFORMATTED
/* ID �।�⭮�� ������� */
	iRecIDloan 
	";"
	";"
/* ��� ���⥦� */
	STRING(DAY(loan-int.mdate), "99") + '.' + STRING(MONTH(loan-int.mdate), "99") + '.' + STRING(YEAR(loan-int.mdate), "9999") 
	";"
/* DateBeginPer */
	';'
/* DateEndPer */
    ';'
/* OPERATION */
    strOP
    ';'
/* SUMMA_PLAT */
	TRIM(STRING(ABS(loan-int.amt-rub), ">>>>>>>>>>>9.99"))
	';'
/* CUR */	
	'810;'
/* SP */
	'0'
	CHR(13) + CHR(10)
	.
	IF strOP = '4' AND lastPrDate < loan-int.mdate THEN
        lastPrDate = loan-int.mdate.
	IF strOP = '2' AND lastOdDate < loan-int.mdate THEN
        lastOdDate = loan-int.mdate.
    END.
  END.
/*  message string(lastPrDate) + '  ' + string(lastOdDate) view-as alert-box. */
  FOR EACH term-obl OF loan WHERE 
      term-obl.fop-date <> ? AND
    (
      (term-obl.end-date > lastOdDate AND term-obl.idnt = 3 AND term-obl.amt-rub <> 0)
      OR (term-obl.end-date > lastPrDate AND term-obl.idnt = 1 AND term-obl.amt-rub <> 0)
     )
      NO-LOCK BY term-obl.fop-date:
    IF term-obl.idnt = 3 THEN strOP = '2'.
    ELSE strOP = '4'.
	PUT UNFORMATTED
/* ID �।�⭮�� ������� */
	iRecIDloan 
	";"
	";"
/* ��� ���⥦� */
	STRING(DAY(term-obl.end-date), "99") + '.' + STRING(MONTH(term-obl.end-date), "99") + '.' + STRING(YEAR(term-obl.end-date), "9999") 
	";"
/* DateBeginPer */
	';'
/* DateEndPer */
    ';'
/* OPERATION */
    strOP
    ';'
/* SUMMA_PLAT */
	TRIM(STRING(ABS(term-obl.amt-rub), ">>>>>>>>>>>9.99"))
	';'
/* CUR */	
	'810;'
/* SP */
	'0'
	CHR(13) + CHR(10)
	.  
  END.
END.
OUTPUT CLOSE.

{intrface.del}
