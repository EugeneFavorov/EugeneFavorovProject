/*
процедура собирает временную таблицу для запуска 
процедуры создания уведомления в Нотариат (notar-cre.p)
Выборка делается либо по tmprecid.id кредитных договоров
либо из файла kd.txt
Параметры:
    iDate - Дата опердня, в котором будут выбираться проводки
*/
{globals.i}
{intrface.get xclass}
/*
DEFINE INPUT  PARAMETER iFile AS CHARACTER NO-UNDO.
*/
DEFINE BUFFER loan FOR loan.
DEFINE BUFFER term-obl FOR term-obl.

/* 'Сообщение в нотариат о появлении, изменении и прекращении залога' */
/* DEFINE BUFFER NF FOR PLEDGENOTIFICATION. */
/* Строка параметров для передачи в процедуру создания или изменения залога*/
DEFINE VARIABLE mParStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNotifRet AS INT64 NO-UNDO.
DEFINE VARIABLE mreg-zalog-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNN AS INT64 NO-UNDO.
DEFINE VARIABLE mTermSurr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mVidOb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOst91312 AS DECIMAL NO-UNDO.
DEFINE VARIABLE mNotifCreNumber AS CHARACTER NO-UNDO.
/*
{tmpobj.def}
*/

DEFINE NEW GLOBAL SHARED TEMP-TABLE tmprecid2 NO-UNDO
         FIELD id AS RECID
         FIELD tablename AS CHARACTER
         INDEX id id
         INDEX tableid tablename
.



/*
{tmprecid.def}
*/
  /*      RUN rid-keep.p (TABLE tmprecid). 
          RUN rid-keep.p ( TABLE tmprecid2).
          */

DEF TEMP-TABLE tt-notif NO-UNDO
FIELD notif AS CHAR
.
    
/* 
Отработка сообщений
о возникновении залога
об изменении залога
*/

DEFINE BUFFER NF0 FOR PLEDGENOTIFICATION.
DEFINE BUFFER NF00 FOR PLEDGENOTIFICATION.
/*
{setdest.i}

PUT UNFORMATTED "Обработанные договоры" SKIP.
*/
  
/*
FIND FIRST tmprecid2 NO-LOCK NO-ERROR.
if not avail tmprecid2 then message "not found" view-as alert-box.
IF AVAIL tmprecid2 THEN DO:
     message "found" view-as alert-box.
FIND FIRST term-obl WHERE RECID(term-obl) EQ tmprecid2.id NO-LOCK NO-ERROR.

IF AVAIL term-obl THEN DO:
    FIND FIRST loan
        WHERE loan.contract EQ term-obl.contract
        AND loan.cont-code EQ term-obl.cont-code
        NO-LOCK NO-ERROR.
        */
        find first loan where loan.doc-ref = '45-00-65062-АПК' and loan.contract = 'Кредит' no-lock no-error.
        
    IF AVAILABLE(loan) THEN
    DO:
        {empty tt-notif}
       
        FOR EACH NF0 WHERE NF0.contractnumber = loan.doc-ref 
            AND NF0.STATUS_ = 7
            AND NF0.notificationtype = 1
            NO-LOCK:            
            FIND FIRST NF00 WHERE 
                NF00.contractnumber = loan.doc-ref 
                AND NF00.STATUS_ <> 5 AND NF00.STATUS_ <> 8 AND NF00.STATUS_ <> 9 
                AND NF00.notificationtype <> 1
                AND NF00.CREATIONREFERENCENUMBER = NF0.NOTIFICATIONREFERENCENUMBER NO-LOCK NO-ERROR.
                IF NOT AVAIL NF00 THEN DO:
                    CREATE tt-notif.
                    tt-notif.notif = NF0.NOTIFICATIONREFERENCENUMBER.
                    MESSAGE "+" + NF0.NOTIFICATIONREFERENCENUMBER VIEW-AS ALERT-BOX.
                END.
        END.

        run notarview44.p(table tt-notif).        

 
        /*
        RETURN. 
          */
message "after brw" VIEW-AS ALERT-BOX.
        
        mNotifCreNumber = pick-value.
        
        MESSAGE mNotifCreNumber VIEW-AS ALERT-BOX.
        
        IF mNotifCreNumber EQ ? THEN
            PUT UNFORMATTED 
            ENTRY(1, loan.cont-code, "@") 
            " ; "
            "нет регистрационного номера уведомления. Пропускаем."
            SKIP.
        ELSE
        DO:
            mParStr = loan.contract + "," + loan.cont-code + "," + TRIM(STRING(RECID(loan))) + "," + TRIM(STRING(RECID(term-obl))) + ",2,1,0," + mNotifCreNumber.
           /* RUN notar-cre44.p(INPUT mParStr, INPUT-OUTPUT mNotifRet). */
           MESSAGE mParStr VIEW-AS ALERT-BOX.
            IF mNotifRet NE 0 THEN
            DO:
                PUT UNFORMATTED 
                ENTRY(1, loan.cont-code, "@") 
                " ; "
                "Снятие."
                SKIP.
                UpdateSigns("term-obl-gar", 
                term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn),
                "NotifId",
                STRING(mNotifRet), 
                ?).
            END.
            ELSE
                PUT UNFORMATTED 
                ENTRY(1, loan.cont-code, "@") 
                " ; "
                "не создано уведомление"
                SKIP.
        END.

        
      MESSAGE "end!" VIEW-AS ALERT-BOX.
      


        

    END.
    /*
END.
END.
*/
{intrface.del}
{preview.i}

     

