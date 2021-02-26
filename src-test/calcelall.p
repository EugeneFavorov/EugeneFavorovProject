{globals.i}

DEFINE VARIABLE mDate    AS DATE NO-UNDO.
DEFINE VARIABLE mDateEnd AS DATE NO-UNDO.
DEFINE VARIABLE mStart   AS INT  NO-UNDO.
DEFINE VARIABLE mFileNm  AS CHAR NO-UNDO.

FIND LAST DataBlock 
    WHERE DataBlock.dataclass-id EQ "safetyp"
      AND DataBlock.branch-id    EQ shFilial
      AND DataBlock.Data-Source  EQ "99" 
NO-LOCK NO-ERROR.
IF AVAIL(DataBlock) THEN
DO:
	/* MESSAGE "Последняя расчитанная дата для филиала " + STRING(shFilial)
			  + " " + STRING(DataBlock.beg-date,"99.99.9999") 
	VIEW-AS ALERT-BOX. */
	gbeg-date = DataBlock.beg-date + 1.
	gend-date = gbeg-date.
END.

{getdates.i}

ASSIGN
	mDate = beg-date
	mDateEnd = end-date
	mFileNm = "CALCTIME_" + shFilial + "_" + REPLACE(STRING(TIME, "hh:mm:ss"), ":", "-") + ".log"
.

{setdest.i &filename = "mFileNm" &APPEND = " APPEND "}
PUT UNFORMATTED 
	"Филиал: " shFilial SKIP
	"Начало расчета " STRING(TIME, "hh:mm:ss") SKIP(1)
	"          Время  " SKIP
	"Дата      расчета" SKIP
	"-----------------" SKIP
.

DO WHILE mDate LE mDateEnd:
	mStart = TIME.
	RUN calcelecp.p(mDate).
	PUT UNFORMATTED 
		STRING(mDate, "99/99/99") "  "
		LEFT-TRIM(STRING((mStart - TIME) / 60, "->>9"), " -")
		" мин. "
		SKIP
	.
	mDAte = mDate + 1.

END.

PUT UNFORMATTED 
	SKIP(1)
	"Расчет завершен " 
	STRING(TIME, "hh:mm:ss")
	SKIP
.

{preview.i &filename = "mFileNm" &APPEND = " APPEND "}
