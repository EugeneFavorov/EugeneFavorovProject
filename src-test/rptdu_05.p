/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2015 ЗАО "Банковские информационные системы"
     Filename: RPTEPS_XL.P
      Comment: Отчет "Уведомление о полной стоимости кредита (формат MS EXCEL)"
   Parameters: Нет
         Uses:
      Used by:
      Created: 09.08.2010 18:57 BOES    
     Modified: 09.08.2010 18:57 BOES    
*/

&GLOB nodate YES
{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}
{lshpr.pro} 
{t-otch.i NEW}

{svarloan.def NEW}
{loan.pro}
{intrface.get comm}
{intrface.get instrum}
{intrface.get card}

{intrface.get cust}
{intrface.get tmess}
{sh-defs.i new}
{ksh-defs.i NEW}
{pb_exf_exl.i}          /** Библиотека для выгрузки в XL */

DEF VAR vNameCl AS CHARACTER NO-UNDO.
DEF VAR vCurStr AS CHARACTER NO-UNDO.
DEF VAR cFl     AS CHARACTER NO-UNDO.
DEF VAR cXL     AS CHARACTER NO-UNDO.

DEF VAR mBalance  AS DECIMAL NO-UNDO.
DEF VAR mStrTable AS CHAR    NO-UNDO.
DEF VAR mSt       AS CHAR    NO-UNDO.
DEF VAR mStd      AS CHAR    NO-UNDO.
DEF VAR mSrok     AS int64   NO-UNDO.
DEF VAR mAmt-rub  AS DECIMAL NO-UNDO.
DEF VAR m-int-period     AS CHAR    NO-UNDO.
DEF VAR mGorod    AS CHAR    NO-UNDO.
DEF VAR end-date-01  AS DATE    NO-UNDO.


/*
Клиент	Договор	Тип вклада	Валюта	Срок	Сумма	Ставка	Ставка растор.	Дата открытия	Дата окончания	Выплата процентов	Неснижаемый остаток	Счет	Остаток на 01.06.2018	Счет переч.	Остаток переч.	Счет проц.	Остаток проц.
Вотчель Олег Иванович	57-26/52-74/63617354@0500	avtoyr	810	1826	0.00	0.10	0.10	09.08.2011	10.08.2021	в конце срока	0.00	42307810405200010065	140700.23		0.00	47411810305200010108	254.89
*/


DEFINE TEMP-TABLE t-loan
    field ttname-short     as char 
    field ttloan           as char 
    field ttcont-type      as char 
    field ttcurrency       as char
    field ttNDays          as int64
    field ttamt-rub        as dec
    field tt-proc-dep      as CHAR
    field tt-proc-dep-dsr  as CHAR
    field tt-open-date     as date
    field tt-datasogl      as date
    field tt-end-date      as date
    field tt-int-period    as char
    field tt-acct          as char
    field tt-acct-ost      as dec
    field ttGorod          as char
    index ind01 ttGorod tt-datasogl
.



/*Функция получения ставки по карте*/

end-date = date("01/06/2018").

{getdate.i &noinit    = "YES"} /*теперь отчет на заданную дату*/

for each loan where loan.contract = "Депоз"
                and loan.open-date <= end-date
                and (loan.close-date >= end-date or loan.close-date = ?)
/*                and loan.filial-id = shfilial  */
                and loan.cont-type <> "МежбанДРЕПО"
                and loan.filial-id <> "0400"
                NO-LOCK.

   find FIRST term-obl WHERE term-obl.contract EQ loan.contract 
                         AND term-obl.idnt EQ 2            
                         AND term-obl.cont-code EQ loan.cont-code                             
                         /* AND term-obl.end-date EQ loan.open-date */
                         no-lock no-error.
   mAmt-rub = 0.
   if avail term-obl then do:
      mAmt-rub = term-obl.Amt-rub.
   end. 
   find last loan-cond  WHERE loan-cond.contract EQ loan.contract  
		    AND loan-cond.cont-code EQ loan.cont-code 
                    AND loan-cond.since LE term-obl.end-date   
              no-lock no-error.
   if avail loan-cond then do:
      find first code where code.class EQ 'КредПериод'     
                        AND code.parent EQ 'КредПериод'
                        and code.code = loan-cond.int-per
                 no-lock no-error.
      m-int-period = "Не определён".
      if avail code then do:
         m-int-period = code.name .
      end.
      
   end. 
      vNameCl = "Не определён".
      if loan.cust-cat = "Ю" Then do:
         find first cust-corp where cust-corp.cust-id = loan.cust-id no-lock no-error.
         if  avail cust-corp then do:
            vNameCl =  cust-corp.name-corp .
         end.
      end.
      if loan.cust-cat = "Ч" Then do:
         find first person where person.person-id = loan.cust-id no-lock no-error.
         if  avail person then do:
            vNameCl =  person.name-last + " " + person.first-names .
         end.
      end.
      find last  loan-acct where loan-acct.contract = loan.contract
                             and loan-acct.cont-code = loan.cont-code
                             and loan-acct.acct-type = "Депоз" no-lock no-error.
      find  FIRST acct where acct.acct = loan-acct.acct NO-LOCK no-error.
      if not avail acct then do:
         mBalance = 0.
         next.
      end.
      else do:
         RUN acct-pos IN h_base (acct.acct,acct.currency,end-date - 1,end-date - 1,gop-status).
         if acct.currency <> "" then do:
             mBalance = - sh-val.
         end.
         else do:
             mBalance = - sh-bal.
         end.
      end.
      end-date-01 = if loan.open-date <= end-date then end-date else loan.open-date .
      FIND LAST comm-rate WHERE comm-rate.commission EQ "%Деп"
                            AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code 
                            AND comm-rate.since      GE loan.open-date 
                            AND comm-rate.since      LE end-date-01 
                            NO-LOCK NO-ERROR.
      if not avail comm-rate then do:
         mSt = "Не определена".
      end.                     
      else do:
         mSt = STRING(comm-rate.rate-comm,">>9.9999").
      end.        
      FIND LAST comm-rate WHERE comm-rate.commission EQ "%ДепДоср"
                            AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code 
                            AND comm-rate.since      GE loan.open-date 
                            AND comm-rate.since      LE end-date-01 
                            NO-LOCK NO-ERROR.
      if not avail comm-rate then do:
         mStd = "Не определена".
      end.                     
      else do:
         mStd = STRING(comm-rate.rate-comm,">>9.9999").
      end.        
      mSrok  =    loan.end-date - loan.open-date .

      find FIRST branch WHERE branch.branch-id EQ loan.branch-id 
                 no-lock no-error.
      mGorod = "Не определена".
      if avail branch then do:
         mGorod = entry(2,branch.Address) .
      end. 
      Create t-loan.
      ASSIGN
         ttname-short      = vNameCl
         ttloan            = loan.doc-ref
         ttcont-type       = loan.cont-type
         ttcurrency        = if loan.curr <> "" then loan.curr else "810" 
         ttNDays           = mSrok
         ttamt-rub         = mAmt-rub
         tt-proc-dep       = mSt
         tt-proc-dep-dsr   = mStd
         tt-open-date      = loan.open-date
         tt-datasogl       = date(GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"датасогл",?))
         tt-end-date       = loan.end-date
         tt-int-period     = m-int-period
         tt-acct           = acct.acct
         tt-acct-ost       = mBalance
         ttGorod           = mGorod
      .
END.
/*
output to "rptdu_05.txt".
for each t-loan.
   export t-loan.
end.
output close.
*/

cFl = "./cred-fl01.xml".

OUTPUT TO VALUE(cFl).

PUT UNFORMATTED XLHead("tmp", "CCCCCNNNCCCCCN", "230,90,90,60,60,90,60,60,90,90,90,90,210,90,90").
PUT UNFORMATTED XLRow(0) XLCellHat("Договоры Юридических лиц за " + STRING(end-date, "99.99.9999"), 13) XLRowEnd().


/*
Клиент	Договор	Тип вклада	Валюта	Срок	Сумма	Ставка	Ставка растор.	Дата открытия	Дата окончания	
Выплата процентов Счет	Остаток на 01.06.2018
*/

cXL = XLCellHead("Клиент                                            ",0,0,0)
    + XLCellHead("Договор                                           ",0,0,0)
    + XLCellHead("Тип вклада                                        ",0,0,0)
    + XLCellHead("Валюта                                            ",0,0,0)
    + XLCellHead("Срок                                              ",0,0,0)
    + XLCellHead("Сумма                                             ",0,0,0)
    + XLCellHead("Ставка                                            ",0,0,0)
    + XLCellHead("Ставка растор.                                    ",0,0,0)
    + XLCellHead("Дата заключения                                   ",0,0,0)
    + XLCellHead("Дата открытия                                     ",0,0,0)
    + XLCellHead("Дата окончания                                    ",0,0,0)
    + XLCellHead("Выплата процентов                                 ",0,0,0)
    + XLCellHead("Счет                                              ",0,0,0)
    + XLCellHead("Входящий остаток на дату отчёта                   ",0,0,0)
    + XLCellHead("Город                                             ",0,0,0)
    .

PUT UNFORMATTED XLRow(0) cXL XLRowEnd().


cXL = XLCellHead("1",0,0,0)
    + XLCellHead("2",0,0,0)
    + XLCellHead("3",0,0,0)
    + XLCellHead("4",0,0,0)
    + XLCellHead("5",0,0,0)
    + XLCellHead("6",0,0,0)
    + XLCellHead("7",0,0,0)
    + XLCellHead("8",0,0,0)
    + XLCellHead("9",0,0,0)
    + XLCellHead("10",0,0,0)
    + XLCellHead("11",0,0,0)
    + XLCellHead("12",0,0,0)
    + XLCellHead("13",0,0,0)
    + XLCellHead("14",0,0,0)
    + XLCellHead("15",0,0,0)
    .
PUT UNFORMATTED XLRow(0) cXL XLRowEnd().

FOR EACH t-loan  NO-LOCK.
   cXL = XLCell(STRING(       t-loan.ttname-short    ))                    
       + XLCell(STRING(       t-loan.ttloan          ))                    
       + XLCell(STRING(       t-loan.ttcont-type     ))              
       + XLCell(STRING(       t-loan.ttcurrency      ))                 
       + XLNumCell(           t-loan.ttNDays         )                  
       + XLNumCell(           t-loan.ttamt-rub       )                      
       + XLCell(STRING(       t-loan.tt-proc-dep     ))                  
       + XLCell(STRING(       t-loan.tt-proc-dep-dsr ))              
       + XLCell(STRING(       t-loan.tt-open-date    ))                     
       + XLCell(STRING(       t-loan.tt-datasogl     ))                 
       + XLCell(STRING(       t-loan.tt-end-date     ))           
       + XLCell(STRING(       t-loan.tt-int-period   ))   
       + XLCell(STRING(       t-loan.tt-acct         ))   
       + XLNumCell(           t-loan.tt-acct-ost     )        
       + XLCell(STRING(       t-loan.ttGorod         ))        
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
END.

PUT UNFORMATTED XLEnd().
OUTPUT CLOSE.


RUN sndbispc ("file=" + cFl + ";class=bq").

/*
RUN pb_mail.p ("v.ignatchenko", "QQQ Report " + branch.branch-id , "QQQ Report " + branch.branch-id, cFl).
*/

{intrface.del} 
