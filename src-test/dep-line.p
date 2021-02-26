   DEFINE INPUT PARAMETER in-str   as char NO-UNDO.
   {globals.i}
   {justasec}
   {intrface.get refer}    /* Библиотека службы справочников. */
   {intrface.get date}

   DEF VAR vRecLoan      AS RECID   NO-UNDO.
   DEF VAR vRecCond      AS RECID   NO-UNDO.
   DEF VAR vtt-term-amt  AS DEC     NO-UNDO.
   DEF VAR iMode         AS INT64   NO-UNDO.
   DEF VAR vChangeSumm   AS LOG     NO-UNDO.
   DEF VAR vChangePr     AS LOG     NO-UNDO.
   DEF VAR vChangeDate   AS LOG     NO-UNDO.
   DEF VAR vChangePer    AS LOG     NO-UNDO.
   DEF VAR vCondCount    AS INT64   NO-UNDO.

   def var stracct       as char    no-undo .
   def var strmess       as char    no-undo.
   def var i             as INT64   no-undo .
   def var j             as INT64   no-undo .
   def var fl            as INT64   no-undo .
   def var lcount        as INT64   no-undo .
   def var count-total   as INT64   no-undo .
   def var bacct-rec     as char    no-undo.
                                   
   def var b-end-date    as date    no-undo.
   def var datebeg_      as date    no-undo.
   def var dateend_      as date    no-undo.
   def var iDate         as date    no-undo.
                                    
   def var bloan         as char    no-undo.
   def var binn          as char    no-undo.
   def var bInnDoc       as char    no-undo.
   def var bSrok         as char    no-undo.
   def var bSurr         as char    no-undo.
   def var bDataSogl     as char    no-undo.
   DEF VAR vWork         AS LOG     NO-UNDO.
   def var iBranch       as char    no-undo.
   DEF var user_         AS CHAR    NO-UNDO.
   def var bInnSend      as char    no-undo.
   user_ =  USERID("bisquit").     
   iBranch = GetXattrValueEx("_user",user_,"Отделение",?).
 
   pick-value = "".


   bSurr = entry(2,in-str,chr(1)).

   find first loan where   loan.close-date EQ ?     
                     AND loan.contract EQ 'Депоз' 
                     and loan.cont-code = bSurr
                     AND loan.filial-id EQ loan.filial-id
                     EXCLUSIVE-LOCK no-error.
   if not avail loan then do:
      message "Не найден депозит " bSurr  view-as alert-box.
      return .
   end.   
   FIND FIRST loan-cond
   	WHERE loan-cond.contract EQ loan.contract
          AND loan-cond.cont-code EQ loan.Cont-Code
          EXCLUSIVE-LOCK NO-ERROR.
   if not avail loan-cond then do:
      message "Для договора "  loan.Cont-Code " не найдены условия." view-as alert-box.
      return .    
   end.
   find first term-obl where term-obl.contract  = loan.contract
                         and term-obl.idnt      = 2
                         and term-obl.cont-code = loan.cont-code 
                         and term-obl.end-date  = loan.open-date   
                         EXCLUSIVE-LOCK no-error.
   if not avail term-obl then do:
      message "Для договора "  loan.Cont-Code " не найдена сумма договора." view-as alert-box.
      return .    
   end.
   vRecLoan       = recid(loan).     
   vRecCond       = recid(loan-cond).   
   vtt-term-amt   = term-obl.amt-rub.
   iMode          = 2       .   
   vChangeSumm    = yes     .   
   vChangePr      = yes     .   
   vChangeDate    = no      .   
   vChangePer     = yes     .   
   vCondCount     = 1       .

   RUN mm-to.p(vRecLoan,
               vRecCond,
               vtt-term-amt,
               iMode,
               vChangeSumm,
               vChangePr  ,
               vChangeDate,
               vChangePer,
               ?,
               vCondCount) NO-ERROR.

   pick-value = bSurr.
return. 
   
   
   
   
   
   