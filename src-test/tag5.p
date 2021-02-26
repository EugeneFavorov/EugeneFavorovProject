FUNCTION ConvertLoanInt CHAR(INPUT iId_K AS INT64, INPUT iId_D AS INT64):
    DEFINE VAR sRes AS CHAR NO-UNDO.
    sRes = ?.
    
    CASE iId_K:
        WHEN 377 THEN DO:
        	sRes = '13'. /*13*/
        END.
        WHEN 173 THEN DO:
        	CASE iId_D:
        		WHEN 377 THEN sRes = '5'. /* 5 */
        	END CASE.
        END.
        WHEN 3 THEN DO:
        	IF iId_D = 0 THEN sRes = '1'. /* 1 -   «Ġ砠ʠŤȢ */
        END.
        WHEN 0 THEN DO:
        	CASE iId_D:
        		WHEN 2 THEN sRes = '2'. /* 2 -   Ϯà襭ȥ ᠮ筮ɠᡣĭΩ ǠĮ˦ŭͮᢨ */
        		WHEN 7 THEN sRes = '3'. /* 3 -   ΤΠ̫ŭȥ ϠΡ஧ŭͮî Ρͮ­ΣΠĮˣ */
        	END CASE.
        END.
        WHEN 7 THEN DO:
        	CASE iId_D: 	
        		WHEN 0 THEN sRes = '21'. /* 21 - ΢̥͠ «ͮᠠ΄ ͠ ϠΡ஧ʣ */
        		OTHERWISE sRes = '4'. /* 4 -   Ϯà襭ȥ ϠΡ஧ŭͮɠᡣĭΩ ǠĮ˦ŭͮᢨ */
        	END CASE.
        END.
        WHEN 4 THEN DO:
        	CASE iId_D:
/*        		WHEN 6 OR WHEN 5 OR WHEN 24 THEN sRes = '6'. /* 6 -   Ϯà襭ȥ ϠΦŭ⮢ ͠ ᠮ筣ᣤͣ$ΫƥͭΡ⬠*/
*/        		WHEN 10 THEN sRes = '7'. /* 7 -   ΤΠ̫ŭȥ ϠΡ஧ŭͫ堯஦ŭ⮢ */
        		WHEN 48 THEN sRes = '25'. /* 25 - ϥ७Ρ ϠΡ஧ŭͫ堯஦ŭ⮢ Ǡ `ˠ͡ */
        	END CASE.
		END.
		WHEN 35 THEN DO:
        	CASE iId_D:
        		WHEN 5 THEN sRes = '6'. /* 6 -   Ϯà襭ȥ ϠΦŭ⮢ ͠ ᠮ筣ᣤͣ$ΫƥͭΡ⬠*/
        	END CASE.
		END.
        WHEN 5 THEN DO:
        	CASE iId_D:
        		WHEN 4 THEN sRes = '5'. /* 5 -   ͠稡˥ͨŠϠΦŭ⮢ ͠ ᠮ筣ᣤͣ$ΫƥͭΡ⬠*/
        	END CASE.
        END.
        WHEN 16 OR WHEN 33 OR WHEN 34 OR WHEN 8 THEN DO:
        	CASE iId_D:
        		WHEN 10 THEN sRes = '7'. /* 7 -   ΤΠ̫ŭȥ ϠΡ஧ŭͫ堯஦ŭ⮢ */
        	END CASE.
        END.
        WHEN 5 THEN DO:
        	CASE iId_D:
        		WHEN 4 THEN sRes = '5'. /* 5 -   ͠稡˥ͨŠϠΦŭ⮢ ͠ ᠮ筣ᣤͣ$ΫƥͭΡ⬠*/
	       		WHEN 8 OR WHEN 9 THEN sRes = '11'. /* 11 - ͠稡˥ͨŠ袠$΢ Ϯ ϠΡ஧ụ̆ͮ Ρͮ­ά㠤Ϋã */
        	END CASE.
        END.
        WHEN 10 THEN DO:
        	CASE iId_D:
        		WHEN 6 OR WHEN 5 OR WHEN 351 OR WHEN 24 THEN sRes = '8'. /* 8 -   Ϯà襭ȥ ϠΡ஧ŭͫ堯஦ŭ⮢ */
        	END CASE.
        END.
        WHEN 233 THEN DO:
        	CASE iId_D:
        		WHEN 210 THEN sRes = '9'. /* 9 -   ͠稡˥ͨŠϠΦŭ⮢ ͠ ϠΡ஧ŭͣᣤͣ$ΫƥͭΡ⬠*/
        	END CASE.
        END.
        WHEN 210 THEN DO:
        	CASE iId_D:
        		WHEN 6 OR WHEN 5 OR WHEN 351 OR WHEN 24  OR WHEN 10 THEN sRes = '10'. /* 10 - Ϯà襭ȥ ϠΦŭ⮢ ͠ ϠΡ஧ŭͣᣤͣ$ΫƥͭΡ⬠*/
        	END CASE.
        END.
        WHEN 8 OR WHEN 9 THEN DO:
        	CASE iId_D:
        		WHEN 6 OR WHEN 4 OR WHEN 5 OR WHEN 94 THEN sRes = '12'. /* 12 - Ϯà襭ȥ 袠$΢ Ϯ ϠΡ஧ụ̆ͮ Ρͮ­ά㠤Ϋã  */
        	END CASE.
        END.
        WHEN 12 THEN DO:
        	CASE iId_D:
        		WHEN 6 OR WHEN 5 OR WHEN 24 THEN sRes = '18'. /* 18 - Ϯà襭ȥ 袠$΢ Ϯ ϠΡ஧ŭ̠ͫϠΦŭ⠬  */
        	END CASE.
        END.
        WHEN 25 THEN sRes = '26'. /* 26 - ϥ७Ρ ϠΡ஧ŭͮɠᡣī Ǡ `ˠ͡ */
        WHEN 11 THEN DO:
        	CASE iId_D:
        		WHEN 5 THEN sRes = '28'. /* 28 - ͠稡˥ͨŠ袠$΢ Ϯ ϠΡ஧ŭ̠ͫϠΦŭ⠬  */
        	END CASE.
        END.

    END CASE.
    RETURN sRes.
END FUNCTION.   


DEFINE INPUT  PARAMETER iRecIDloan AS RECID NO-UNDO. 
DEF VAR datepereschet AS DATE NO-UNDO.

{tmprecid.def} 
{globals.i} 
{sh-defs.i}

{intrface.get xclass}

DEFINE BUFFER loan FOR loan.
DEFINE BUFFER loan-int FOR loan-int.
DEFINE VAR strOP AS CHAR.
/*
OUTPUT TO VALUE(tag5.txt) CONVERT TARGET "1251".
*/

datepereschet = end-date.
/* DATE("30/08/2016"). */

FIND FIRST loan
    WHERE RECID(loan) EQ iRecIDloan
    NO-LOCK NO-ERROR.
IF AVAILABLE(loan) THEN
DO:

	if loan.since <> datepereschet 
	  then do:
	    RUN l-calc2.p ("�।��",       /* �����祭�� �������. */
	           loan.cont-code,      /* ����� �������. */
	           date(datepereschet),   /* ����砭�� ������� + ���� ��� �믮������ ��⮬. */
	           FALSE,		/* �������/�� ������� ������ �祭�� ������� */
	           TRUE).		/* �뢮����/ �� �뢮���� ��⮪�� �� ��࠭ */
	end.



  FOR EACH loan-int OF loan where loan-int.mdate < datepereschet NO-LOCK:
  	strOP = ConvertLoanInt(loan-int.id-k,loan-int.id-d).
  	IF strOP <> ? THEN
	PUT UNFORMATTED
/* Ȥŭ⨤Ȫ"Π Įî®ࠠ® ­Ũͥɠᨡ⥬Š*/
	iRecIDloan 
	"^"
/* ҨϠίŠ&Ȩ */
	strOP
	"^"
/* Ġ⠠ίŠ&Ȩ */
	STRING(YEAR(loan-int.mdate), "9999") + STRING(MONTH(loan-int.mdate), "99") + STRING(DAY(loan-int.mdate), "99")
	"^"
/* ѣ̬ ίŠ&Ȩ */
	STRING(loan-int.amt-rub, ">>>>>>>>>>>9.99")
	"^"
/* ���⥬� */
	'��� "���� ����"'
	CHR(13) + CHR(10)
	.
  END.

  FOR EACH loan-int OF loan where loan-int.mdate = datepereschet and loan-int.id-k = 32 and loan-int.id-d = 33 NO-LOCK:
  	strOP = '5'.
  	IF strOP <> ? THEN
	PUT UNFORMATTED
/* Ȥŭ⨤Ȫ"Π Įî®ࠠ® ­Ũͥɠᨡ⥬Š*/
	iRecIDloan 
	"^"
/* ҨϠίŠ&Ȩ */
	strOP
	"^"
/* Ġ⠠ίŠ&Ȩ */
	STRING(YEAR(loan-int.mdate), "9999") + STRING(MONTH(loan-int.mdate), "99") + STRING(DAY(loan-int.mdate), "99")
	"^"
/* ѣ̬ ίŠ&Ȩ */
	STRING(loan-int.amt-rub, ">>>>>>>>>>>9.99")
	"^"
/* ���⥬� */
	'��� "���� ����"'
	CHR(13) + CHR(10)
	.
  END.



END.
OUTPUT CLOSE.

{intrface.del}
