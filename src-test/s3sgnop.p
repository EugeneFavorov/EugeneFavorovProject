/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: s3sgnop.p
      Comment: Пересчет субаналитики
   Parameters: cont-code
         Uses:
      Used by:
      Created:       
     Modified:      
*/

DEFINE INPUT PARAMETER in-cont-code AS CHARACTER NO-UNDO.

IF in-cont-code <> ""
AND in-cont-code <> ?
THEN DO:
  RUN s3sgnop_.p(in-cont-code) .
  RETURN.
END.
{globals.i}
{sh-defs.i}
{tmprecid.def}
{defoptr.i new}
{intrface.get xclass}
{intrface.get separate}


&GLOB ACCT%          "70606*,47411*"
&GLOB CODES-%        "НачПрС1,НачПрВ,НачПр"
&GLOB CODES-OST      "ОстВклВ,ОстВклС"
&GLOB LST-ACCT-TYPE  "loan-dps-t,loan-dps-p,loan-dps-int"

DEFINE VAR work-status   AS CHARACTER INITIAL "П" NO-UNDO.
DEFINE VAR action-name   AS CHAR NO-UNDO .
DEFINE VAR vListOst      AS CHAR NO-UNDO .
DEFINE VAR vKau          AS CHAR NO-UNDO .       
DEFINE VAR vSumm         AS DECIMAL NO-UNDO .  
DEFINE VAR CUR-OP-DATE   AS DATE NO-UNDO .
DEFINE VAR ii            AS INT64 NO-UNDO .
DEFINE VAR vDate         AS DATE  NO-UNDO .
DEFINE VAR vvDate        AS DATE  NO-UNDO .
DEFINE VAR vListAcct     AS CHAR NO-UNDO .
DEFINE VAR vDateClose    AS DATE NO-UNDO .
DEFINE VAR dob           AS DATE NO-UNDO . 
DEFINE VAR fl_kau        AS LOGICAL NO-UNDO .
DEFINE VAR max-loan      AS INT64 NO-UNDO .
DEFINE VAR t-loan        AS INT64 NO-UNDO .


DEFINE BUFFER xkau-entry FOR kau-entry .
DEFINE BUFFER bloan-acct for loan-acct .
define buffer buf-loan-acct for loan-acct .
define buffer bacct for loan-acct .
DEFINE BUFFER pkau-cur FOR kau-cur.
DEFINE BUFFER pkau-pos FOR kau-pos .


{s3sgnop.lib}
{ksh-defs.i NEW}

DEFINE VAR summ-%        AS DECIMAL   NO-UNDO.
DEFINE VAR summ-ost      AS DECIMAL   NO-UNDO.

DEFINE VAR i             AS INT64   NO-UNDO.
DEFINE VAR j             AS INT64   NO-UNDO .
DEFINE VAR mess          AS CHARACTER NO-UNDO.


DEFINE VAR MESS-ID     AS INT64
                       INITIAL 0    NO-UNDO.

DEFINE TEMP-TABLE out-protocol   NO-UNDO
   FIELD id          AS INT64
   FIELD type        AS CHARACTER
   FIELD mess        AS CHARACTER
INDEX ID   IS UNIQUE ID
.


PROCEDURE SAVE_PROTOCOL:
   DEFINE INPUT PARAMETER in-type AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER in-text AS CHARACTER NO-UNDO.

   MESS-ID = MESS-ID + 1.
   CREATE out-protocol.
   ASSIGN
      out-protocol.id         = MESS-ID
      out-protocol.type       = in-type
      out-protocol.mess       = in-text
   .
END PROCEDURE.


/*конец текущего месяца*/
FUNCTION GET-DATE-KM RETURNS DATE (INPUT in-date AS DATE):
   DEF VAR i AS INT64 NO-UNDO.
   DO WHILE month(in-date) EQ month(in-date + i):
      i = i + 1.
   END.
   in-date = in-date + i - 1.
   RETURN in-date.
END FUNCTION.


FUNCTION Select-cod-ost RETURNS CHARACTER (BUFFER buf-open FOR op-entry,
                                           BUFFER buf-loan FOR loan,  
                                           INPUT in-acct-type AS CHARACTER,
                                           INPUT in-currency  AS CHARACTER,
                                           INPUT db-cr        AS LOGICAL):

   DEFINE VAR summ-entry    AS DECIMAL NO-UNDO.
   DEFINE VAR SUMM-OST-%    AS DECIMAL NO-UNDO .
   
   DEFINE BUFFER b-entry   FOR op-entry.
 
   summ-entry = if in-currency > '' then buf-open.amt-cur
                     else buf-open.amt-rub .
   IF NOT DB-CR THEN DO:
    CASE IN-ACCT-TYPE :
       WHEN 'LOAN-DPS-T' THEN DO:
          /* проверяем корреспондирующий счет */
             IF buf-opeN.acct-db <> ?
             AND ((SUBSTRING(buf-open.acct-db,1,5)) = '47411' 
                   OR (SUBSTRING(buf-open.acct-db,1,5)) = '70606')
             THEN RETURN ENTRY(1,{&CODES-%}) + ',' + string(summ-entry) .
             ELSE IF buf-open.acct-db = ?
             THEN DO :
               FIND FIRST b-entry OF buf-open WHERE 
                          SUBSTRING(b-ENTRY.acct-db,1,5) = '47411'
                       OR SUBSTRING(b-ENTRY.acct-db,1,5) = '70606'
                    NO-LOCK NO-ERROR .
               IF AVAIL  b-entry
               THEN RETURN ENTRY(1,{&CODES-%}) + ',' + string(summ-entry).
               ELSE RETURN ENTRY(2,{&CODES-OST}) + ',' + string(summ-entry) .
             END.
             ELSE return ENTRY(2,{&CODES-OST}) + ',' + string(summ-entry).
         END.    
       WHEN 'LOAN-DPS-P' THEN DO :
        IF buf-opeN.acct-db <> ?
             AND ((SUBSTRING(buf-open.acct-db,1,5)) = '47411' 
                   OR (SUBSTRING(buf-open.acct-db,1,5)) = '70606')
             THEN RETURN ENTRY(2,{&CODES-%}) + ',' + string(summ-entry) .
             ELSE IF buf-open.acct-db = ?
             THEN DO :
               FIND FIRST b-entry OF buf-open  WHERE 
                          SUBSTRING(b-ENTRY.acct-db,1,5) = '47411'
                       OR SUBSTRING(b-ENTRY.acct-db,1,5) = '70606'
                    NO-LOCK NO-ERROR .
               IF AVAIL  b-entry
               THEN RETURN ENTRY(2,{&CODES-%}) + ',' + string(summ-entry).
               ELSE RETURN ENTRY(1,{&CODES-OST}) + ',' + string(summ-entry) .
             END.
             ELSE return ENTRY(1,{&CODES-OST}) + ',' + string(summ-entry).
        
       END. 
       WHEN 'LOAN-DPS-INT' THEN RETURN ENTRY(3,{&CODES-%}) + ',' + string(summ-entry) .
    END CASE.
  END.
  ELSE DO :
   case in-acct-type:
   WHEN 'LOAN-DPS-T' THEN DO:
      /* смотрим  субаналитические остатки */
        assign
        ksh-bal = 0
        ksh-val = 0 .
        RUN  kau-pos.p(buf-open.acct-db ,
                       in-currency ,
                       buf-open.op-date,
                       buf-open.op-date,
                       'Ф',
                       buf-loan.contract + ',' + loan.cont-code + ',' +
                       ENTRY(1,{&CODES-%})) .
        summ-ost-% = ABS(IF in-currency EQ "" THEN ksh-bal
                                              ELSE ksh-val).               
        if summ-ost-% >= summ-entry
        then return  ENTRY(1,{&CODES-%}) + ',' + string(summ-entry).
        else if summ-ost-% > 0 
        then          
            return ENTRY(1,{&CODES-%}) + ',' + string(summ-ost-%) + ';'
           + ENTRY(2,{&CODES-OST}) + ',' + string(summ-entry - summ-ost-%) .
           else return
                ENTRY(2,{&CODES-OST}) + ',' + string(summ-entry - summ-ost-%) .                                                      
                         
      
     END.
     WHEN 'LOAN-DPS-P' THEN DO:
           /* смотрим  субаналитические остатки */
        assign
        ksh-bal = 0
        ksh-val = 0 .
        RUN  kau-pos.p(buf-open.acct-db ,
                       in-currency ,
                       buf-open.op-date,
                       buf-open.op-date,
                       'Ф',
                       buf-loan.contract + ',' + loan.cont-code + ',' +
                       ENTRY(2,{&CODES-%})) .
        summ-ost-% = ABS(IF in-currency EQ "" THEN ksh-bal
                                              ELSE ksh-val).               
        if summ-ost-% >= summ-entry
        then return  ENTRY(2,{&CODES-%}) + ',' + string(summ-entry).
        else if summ-ost-% > 0 
        then          
            return ENTRY(2,{&CODES-%}) + ',' + string(summ-ost-%) + ';'
           + ENTRY(1,{&CODES-OST}) + ',' + string(summ-entry - summ-ost-%) .
           else return
                ENTRY(1,{&CODES-OST}) + ',' + string(summ-entry - summ-ost-%) .                                                      
      END.                   
      WHEN 'LOAN-DPS-INT' THEN RETURN ENTRY(3,{&CODES-%}) + ','+  string(summ-entry)  .
  END CASE.
 END. 
END FUNCTION.

vDate = Get_OpenDate_Cat("b").
IF vDate EQ ? THEN
DO:
   MESSAGE "Не определена дата начального решения"
      VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
   RETURN.
END.

MESSAGE "Удалять старую субаналитику?"
   VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO
   UPDATE mIsDelKau AS LOGICAL.

FOR EACH tmprecid :
  max-loan = max-loan  + 1.
END.
action-name = "Привязка проводок по вкладам:" + STRING(max-loan).

{init-bar.i """ + action-name + ""}

def stream out1.
output stream out1 to out1.txt.


LOOP_LOAN:
FOR EACH TMPRECID ,
    FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK
    TRANSACTION  ON ERROR UNDO, RETRY  ON ENDKEY UNDO, RETRY:
   t-loan = t-loan + 1. 
   {move-bar.i t-loan max-loan}
   
   put stream out1 unformatted
      loan.cont-code skip.
   .
   
   IF RETRY THEN DO :
      RUN SAVE_PROTOCOL("E", MESS ).
      NEXT LOOP_LOAN .
   END.
   vListAcct = '' .
   /* удаляем существующую субаналитику */
   IF mIsDelKau EQ YES THEN
      RUN del_kau( BUFFER loan).
   
   /* Начальное решение по пролонгированному вкладу или вкладудо востребования,
      открытыму  до даты начального решения по Бисквиту Вк */
   /* Проверяем необходимость создания документа с субаналитикoй для пролонги-
      рованных вкладов и вкладов. открытых до даты нач. решения */
   
   
   DO j = 1 TO NUM-ENTRIES({&LST-ACCT-TYPE}) :     
      FOR EACH loan-acct WHERE loan-acct.contract  EQ loan.contract
                      AND loan-acct.cont-code EQ loan.cont-code
                      AND loan-acct.acct-type = ENTRY(j,{&LST-ACCT-TYPE})
                      NO-LOCK:

         fl_kau = NO.
         FIND FIRST signs WHERE 
                    signs.code      EQ 'prol'
                AND signs.file-name EQ 'loan'
                AND signs.surrogate EQ loan.contract  + ',' + loan.cont-code
              NO-LOCK NO-ERROR.
         IF AVAIL signs AND 
            signs.code-val EQ 'да'
            THEN fl_kau = YES.

         vKau = loan.contract + ',' + loan.cont-code + ',' 
              + (IF loan-acct.acct-type EQ "loan-dps-t" THEN "ОстВклС"
                 ELSE IF loan-acct.acct-type EQ "loan-dps-p" THEN "ОстВклВ"
                 ELSE "НачПр").
         
         IF loan.open-date LE vDate AND 
            NOT fl_kau AND 
            /* а есть ли проводочки на дату открытия - ну всякое бывает
               и заодно субпроводочки  */
            NOT CAN-FIND(FIRST op-entry WHERE 
                               op-entry.op-date  EQ loan.open-date and 
                               op-entry.acct-cr  EQ loan-acct.acct and 
                               op-entry.currency EQ loan-acct.currency) AND
            NOT CAN-FIND(FIRST kau-entry where 
                               kau-entry.op-date  EQ loan.open-date AND
                               kau-entry.acct     EQ loan-acct.acct AND
                               kau-entry.currency EQ loan-acct.currency AND
                               NOT kau-entry.debit AND
                               kau-entry.kau      EQ vKau)
            THEN fl_kau = YES.

         /* Если счет loan-dps-int, то проверим наличие субаналитики */
         /* Если субаналитика есть, то не создаем                    */
         IF loan-acct.acct-type EQ "loan-dps-int" AND
            CAN-FIND(FIRST kau-entry where 
                               kau-entry.acct     EQ loan-acct.acct AND
                               kau-entry.currency EQ loan-acct.currency AND
                               kau-entry.kau      EQ vKau) 
            THEN fl_kau = NO.

         vvDate =  IF loan.open-date LE vDate 
            THEN vDate + 1 
            ELSE loan.open-date.

         IF fl_kau THEN DO:
           ASSIGN 
              sh-in-val = 0
              sh-in-bal = 0
           .
           RUN acct-pos IN h_base (loan-acct.acct,loan-acct.currency,
                                   vvDate ,   
                                   vvDate ,'П').  
           vSumm = ABS(IF loan-acct.currency GT '' THEN sh-in-val
                                                   ELSE sh-in-bal) .
           
           /* создаем документ и субпроводку */
           IF vSumm <> 0 THEN DO:  
              cur-op-date = IF vvDate NE loan.open-date 
                 THEN vvDate -  1
                 ELSE vvDate.
              CREATE op .
              {op(sess).cr 
                 &op-status=chr(251)
              }
              /* если на дату открытия договора нет опердня, 
                 меняем на ближайший опердень */
              IF loan.open-date EQ cur-op-date AND
                 NOT CAN-FIND(FIRST op-date WHERE
                                    op-date.op-date EQ cur-op-date) THEN
              DO:
                 FIND FIRST op-date WHERE 
                         op-date.op-date GE cur-op-date 
                      NO-LOCK NO-ERROR.
                 IF AVAIL op-date THEN
                    op.op-date = op-date.op-date.
              END.   
              ASSIGN 
                 op.class-code = 'opkau'
                 op.details = IF loan-acct.acct-type EQ "loan-dps-t" 
                            THEN 'Начальное решение по пролонгированному вкладу'
                            ELSE IF loan-acct.acct-type EQ "loan-dps-p"
                            THEN 'Начальное решение по вкладу до востребования'
                            ELSE 'Начальное решение по начисленным процентам'
                 op.doc-date = vvDate
                 op.doc-num  = 'opkau'
                 op.doc-type = 'opkau'
                 op.acct-cat = 'b'
                 op.contract-date = cur-op-date  
                 op.filial-id     = loan.filial-id 
              .
              CREATE kau-entry.
              {g-ken.ass}
              ASSIGN 
                 kau-entry.acct       =  loan-acct.acct
                 kau-entry.currency   =  loan-acct.currency
                 kau-entry.kau        =  vKau
                 kau-entry.kau-id     =  loan-acct.acct-type
                 kau-entry.op-status  = op.op-status
                 kau-entry.op-date    = cur-op-date
                 kau-entry.debit      = no
                 kau-entry.amt-cur    = IF loan-acct.currency > ''
                                        THEN vSumm
                                        ELSE 0
                 kau-entry.amt-rub    = IF loan-acct.currency = ''
                                        THEN vSumm
                                        ELSE 0 
                 kau-entry.acct-cat    = 'b' 
              .

              FIND LAST kau WHERE 
                        kau.kau      EQ vKau
                    AND kau.acct     EQ loan-acct.acct
                    AND kau.currency EQ loan-acct.currency 
                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
              IF NOT AVAIL kau THEN DO:   
                 {kau(op).cr
                    &op-entry      =  kau-entry
                    &UserCurrency  =  loan-acct.currency
                    &UserKau       =  kau-entry.kau 
                 }
              END.
              ELSE DO:
                 FIND FIRST acct WHERE 
                            acct.acct     EQ kau.acct
                        AND acct.currency EQ kau.currency
                      NO-LOCK NO-ERROR.
              END.

              {kau(off).cal &op-entry = kau-entry
                 &ssum="(IF kau-entry.currency = '' then 
                         - vSumm else 0) "
                 &inc=1
                 &scur=" (IF kau-entry.currency EQ '' THEN 0
                          ELSE -  vSumm)" 
              }            
              
              IF loan.open-date LT vvDate /*OR
                 (loan.open-date GE vvDate AND
                 /* если такого опердня нет, то тоже создаем */
                 NOT CAN-FIND(op-date WHERE op.op-date EQ loan.open-date)*/ THEN
              DO:
                 dob = cur-op-date.
                 IF kau-entry.currency > ''
                 THEN DO :
                    {closekau.i
                       &op-entry = kau-entry
                       &bal      = yes
                       &cur      = cur
                       &amt      = vSumm
                    }
                 END. 
                 ELSE DO :
                    {closekau.i
                       &op-entry = kau-entry
                       &bal      = yes
                       &cur      = pos
                       &amt      = vSumm
                    }
                 END.
              END.
           END.    
         END.                                                                
      END.
   END. 
   
   DO j = 1 TO NUM-ENTRIES({&LST-ACCT-TYPE}) :     
      FOR EACH loan-acct WHERE loan-acct.contract  EQ loan.contract
                      AND loan-acct.cont-code EQ loan.cont-code
                      AND loan-acct.acct-type = ENTRY(j,{&LST-ACCT-TYPE})
                      NO-LOCK :
         {additem.i vlistAcct "loan-acct.acct + '/' + loan-acct.currency"}
         
         /*Проводки по кредиту счета */
         FOR EACH op-entry WHERE 
                  op-entry.acct-cr   EQ loan-acct.acct
              AND op-entry.op-date   GE loan.open-date
              AND op-entry.op-status GE work-status
             EXCLUSIVE-LOCK:
                             
            /* исключаем переоценку */
            IF op-entry.currency > "" AND op-entry.amt-cur = 0
            THEN NEXT .
            FIND FIRST op OF op-entry NO-LOCK NO-ERROR.

            IF {assigned op-entry.kau-cr} THEN NEXT.
            
            /* Определяем коды остатков и суммы остатков по ним */
            vListOst = Select-cod-ost(BUFFER op-entry,
                                      BUFFER LOAN,  
                                  loan-acct.acct-type,
                                  loan-acct.currency,
                                  NO).
            if op-entry.op-date = loan.open-date
               and loan-acct.acct-type = 'loan-dps-t'                      
               and entry(1,vListOst) = 'НачПрС1'
            then 
               vListOst = 'ОстВклС' + ',' + entry(2,vListOst). 
                                  
            IF vListOst = "" THEN DO:
               mess = 'Вклад ' + loan.cont-code + 
                      ' ошибка при привязке документа ' +
                      op.doc-num + ' за дату ' + string(op.op-date) +
                      ' счет ' + loan-acct.acct .
               undo loop_loan, retry loop_loan .                
            
            END.

            DO i = 1 TO NUM-ENTRIES(vListOst,';'):
              vKau = loan.contract + ',' + loan.cont-code + ',' + 
                     entry(1,entry(i,vListOst,';')) .
            
              find last kau where 
                        kau.kau eq vKau and
                        kau.acct eq loan-acct.acct and 
                        loan-acct.currency EQ kau.currency  
                   exclusive-lock no-wait no-error.

              if not avail kau
              then do :
                {kau(op).cr
                  &op-entry      =  op-entry
                  &db-cr         =  -cr
                  &UserCurrency  =  loan-acct.currency
                  &UserKau       =  vKau 
                }
              end.
              vSumm = DEC(entry(2,entry(i,vListOst,';'))).
              
              {kau(off).cal &op-entry = op-entry
                  &ssum="(IF kau.currency = '' then 
                         vSumm else 0) "
                  &inc=1
                  &scur=" (IF kau.currency EQ '' THEN 0
                          ELSE vSumm)" 
              }
              
              {kauen.cr &op-entry=op-entry 
                        &db-cr=-cr 
                        &scur=amt-cur 
                        &ssum=amt-rub 
                        &kau-id = kau.kau-id }
              assign
                 kau-entry.kau      = kau.kau
                 kau-entry.currency = kau.currency
                 kau-entry.amt-cur  = IF kau.currency EQ "" THEN 0
                                      ELSE vSumm 
                 kau-entry.amt-rub  = IF kau.currency > "" THEN 0
                                      ELSE vSumm 
              .           
              RELEASE kau-entry. 
            END.
            op-entry.kau-cr = vKau .
       END.
 
       /* Проводки по дебету счета */
       FOR EACH op-entry WHERE 
                op-entry.acct-db   EQ loan-acct.acct
            AND op-entry.op-date   GE loan.open-date
            AND op-entry.op-status GE work-status
           EXCLUSIVE-LOCK:
            /* исключаем переоценку */
            IF op-entry.currency > "" AND op-entry.amt-cur = 0
            THEN NEXT .
            FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
 
            IF {assigned op-entry.kau-db} THEN NEXT.
            /*
            if op-entry.op-date = loan.open-date
               and loan-acct.acct-type = 'loan-dps-int'
            then next .
            */
            vListOst = Select-cod-ost(BUFFER op-entry,
                                  buffer loan,
                                  loan-acct.acct-type,
                                  loan-acct.currency,
                                  yes).

            IF vListOst = "" THEN DO:
               mess = 'Вклад ' + loan.cont-code + 
                      ' ошибка при привязке документа ' +
                       op.doc-num + ' за дату ' + string(op.op-date) +
                      ' счет ' + loan-acct.acct .
               undo loop_loan, retry loop_loan .                
            
            END.
            DO i = 1 TO NUM-ENTRIES(vListOst,';'):
              find last kau where 
                        kau.kau eq (loan.contract + ',' + loan.cont-code
                        + ',' + entry(1,entry(i,vListOst,';')))
                    and kau.acct eq loan-acct.acct
                   AND loan-acct.currency EQ kau.currency 
                exclusive-lock no-wait no-error .
              vKau = loan.contract + ',' + loan.cont-code + ',' + 
                     entry(1,entry(i,vListOst,';')) .

              if not avail kau
              then do :
                {kau(op).cr
                  &op-entry      =  op-entry
                  &db-cr         =  -db
                  &UserCurrency  =  acct.currency
                  &UserKau       =  vKau
                }
              end.
              vSumm = DEC(entry(2,entry(i,vListOst,';'))) .

              {kau(off).cal &op-entry = op-entry
                  &ssum="(IF kau.currency = '' then 
                         - vSumm else 0) "
                  &inc=1
                  &scur=" (IF kau.currency EQ '' THEN 0
                          ELSE -  vSumm)" 
              }            
              {kauen.cr &op-entry=op-entry 
                        &db-cr=-db 
                        &scur=amt-cur 
                        &ssum=amt-rub 
                        &kau-id = kau.kau-id 
                        &nodef=/*
              }
              assign
                 kau-entry.kau      = kau.kau
                 kau-entry.currency = kau.currency
                 kau-entry.amt-cur  = IF kau.currency EQ "" THEN 0
                                      ELSE vSumm 
                 kau-entry.amt-rub  = IF kau.currency > "" THEN 0
                                      ELSE vSumm 
              .           
              RELEASE kau-entry. 
            END.

            op-entry.kau-db = if num-entries(vListOst,';') = 1 
                              then vKau
                              else (loan.contract + ',' + loan.cont-code + ','
                                   + '?').
    
       END. /* Проводки */
       
    END /* счет */
    . 
  END.  /* do */
  
  /*  цикл по расходам банка */
  for each loan-acct of loan where 
           loan-acct.acct-type = 'loan-expens' no-lock, 
      each bloan-acct of loan where 
           bloan-acct.acct-type = 'loan-dps-out' no-lock,
      each op-entry where 
           op-entry.acct-cr = bloan-acct.acct
       and op-entry.acct-db = loan-acct.acct
       and op-entry.op-date gt loan.open-date no-lock, 
      first op of op-entry no-lock :
     
     if lookup(loan.doc-ref,op.details,' ') > 0 
     then do :
        UpdateSigns(op.class-code,string(op.op),'Вклад',loan-acct.cont-code,?) .
        UpdateSigns('op-entry',string(op.op) + ',' + string(op-entry.op-entry),
                  'NВклад',loan-acct.cont-code,?). 
     end.             
     else do :
        find first bacct where 
                   bacct.acct = bloan-acct.acct
               and bacct.currency = bloan-acct.currency 
             no-lock no-error.
        if not  ambiguous bacct then do :
          UpdateSigns(op.class-code,string(op.op),'Вклад',loan-acct.cont-code,?) .
          UpdateSigns('op-entry',string(op.op) + ',' + string(op-entry.op-entry),
                  'NВклад',loan-acct.cont-code,?). 
        end.            
     END.
  END.          
  /* расчет остатков по закрытым дням  */
  vDateClose = Get_Date_Cat('b',?).
  IF vDateClose <> ? then do:
    /* раньше шли по опердням, но был счет с датой открытия в выходной
       и он выпадал, поэтому теперь идем по дня с даты открытия или 
       от даты НР + 1*/
    vvDate =  IF loan.open-date LE vDate
       THEN vDate + 1
       ELSE loan.open-date.
    
    DO dob = vvDate TO vdateClose:  
      DO i = 1 TO NUM-ENTRIES(vListAcct) :
         /* ОТБИРАЕМ ПРОВОДКИ ПО Кредиту */
         FOR EACH kau-entry WHERE 
                  kau-entry.acct = ENTRY(1,ENTRY(i,vListAcct),'/')
              AND kau-entry.currency = ENTRY(2,ENTRY(i,vListAcct),'/')
              AND kau-entry.op-date = dob
              AND kau-entry.kau begins 
                  loan.contract + ',' + loan.cont-code + ','
              AND NOT kau-entry.debit no-lock:
            
            find first acct where 
                       acct.acct = kau-entry.acct 
                 no-lock no-error.
            find first bal-acct of acct 
                 no-lock no-error.
            IF kau-entry.currency > ''
            THEN DO :
               {closekau.i
                   &op-entry = kau-entry
                   &bal      = yes
                   &cur      = cur
                   &amt      = kau-entry.amt-cur
               }
            END. 
            ELSE DO :
               {closekau.i
                  &op-entry = kau-entry
                  &bal      = yes
                  &cur      = pos
                  &amt      = kau-entry.amt-rub
               }
            END. 
         END.               
         /* Отбираем проводки по дебету*/
         FOR EACH kau-entry WHERE 
                  kau-entry.acct = ENTRY(1,ENTRY(i,vListAcct),'/')
              AND kau-entry.currency = ENTRY(2,ENTRY(i,vListAcct),'/')
              AND kau-entry.op-date = dob
              AND kau-entry.kau begins 
                  loan.contract + ',' + loan.cont-code + ','
              AND kau-entry.debit no-lock:
           
            find first acct where 
                       acct.acct = kau-entry.acct 
                 no-lock no-error.
            find first bal-acct of acct 
                 no-lock no-error.

            IF kau-entry.currency > ''
            THEN DO :
             {closekau.i
              &op-entry = kau-entry
              &bal      = yes
              &db       = yes
              &cur      = cur
              &amt      = kau-entry.amt-cur
             }
            END. 
            ELSE DO :
             {closekau.i
              &op-entry = kau-entry
              &bal      = yes
              &db       = yes
              &cur      = pos
              &amt      = kau-entry.amt-rub
             }
           END. 
         END.               
         
       END.
   END.
    
 END.
 
END.   /*  внешний цикл */ 
      
{intrface.del}      

{del-bar.i}
{setdest.i}
FOR EACH out-protocol WHERE out-protocol.type BEGINS "E" :
                                
   DISPLAY
      out-protocol.type       FORMAT "x(4)"
                              LABEL  ""
      out-protocol.mess       FORMAT "x(30)"
                              LABEL  "ПРИМЕЧАНИЕ"
   WITH FRAME PROTOCOL DOWN WIDTH 150.
   DOWN WITH FRAME PROTOCOL.
END.
IF CAN-FIND(FIRST out-protocol  WHERE out-protocol.type BEGINS "E")
then do :
 {preview.i}
end.
/* $LINTFILE='s3sgnop.p' */
/* $LINTMODE='1,6,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='sados' */
/* $LINTDATE='15/07/2016 12:54:16.416+03:00' */
/*prosignBQM9au6wbq48kDblbRusgA*/