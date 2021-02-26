/* Комиссия за СпецСчета */
/* fev */

{globals.i}
{pp-corr.p}
{sh-defs.i}
{ksh-defs.i NEW}

DEF VAR nameCl  AS char  no-undo.
DEF VAR tmpdate AS date  no-undo.
DEF VAR opdate  AS date  no-undo.
DEF VAR time16  AS int64 no-undo.

DEF VAR mFlag   AS INT  NO-UNDO.
DEF VAR mSum    AS DEC  NO-UNDO INIT 0.
DEF VAR mKolDocZagrTarif AS DEC  NO-UNDO INIT 0.

DEF VAR fname AS CHAR NO-UNDO.

DEF new shared stream vvs.

DEF BUFFER bacct      FOR acct.
DEF BUFFER bop        FOR op.
DEF BUFFER bop2       FOR op.
DEF BUFFER bop-entry  FOR op-entry.
DEF BUFFER bop-entry2 FOR op-entry.
  
DEFINE TEMP-TABLE otchet
        FIELD first_date         AS DATE                /* первая дата */
        FIELD second_date        AS DATE                /* вторая дата */
        FIELD acct_cl            AS CHAR                /* счёт клиента */
        FIELD name_cl            AS CHAR                /* наименование клиента */
        FIELD count_klb_do_16    AS INT64               /* количество КЛБ проводок до 16:00 */
        FIELD count_bum_do_16    AS INT64               /* количество БУМ проводок до 16:00 */
        FIELD count_klb_posle_16 AS INT64               /* количество КЛБ проводок после 16:00 */
        FIELD count_bum_posle_16 AS INT64               /* количество БУМ проводок после 16:00 */
        FIELD count_small        AS INT64
        FIELD count_big          AS INT64
        FIELD count_both         AS INT64
        FIELD count_zagrad_tarif AS INT64
        FIELD sum1               AS DEC
        FIELD sum2               AS DEC
        FIELD kom1               AS CHAR
        FIELD kom2               AS CHAR
        INDEX acct_cl acct_cl       
    .


{setdest.i &col=10}





  for each mutex no-lock:
  export mutex.               
END. 


{preview.i &col=170}

{intrface.del} /* Выгрузка инструментария. */