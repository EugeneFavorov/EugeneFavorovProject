{globals.i}
/*
{intrface.get tmess}
{intrface.get xclass}
*/

{intrface.get xclass}
{intrface.get refer}

DEF        PARAM BUFFER bop      FOR op.
DEF INPUT  PARAM        iParam  AS  CHAR NO-UNDO.
DEF OUTPUT PARAM        oResult AS  LOG  NO-UNDO INIT ?.
def var vOk  as logic.


DEFINE VARIABLE iParam-249   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProc        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mSeek_       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vErrorText   AS CHARACTER NO-UNDO.
   vErrorText = "".
   iParam-249 = "".
   vErrorText = "".
   iParam-249 = string(bop.op).
   RUN check-gku-sl (iParam-249,OUTPUT vErrorText) NO-ERROR.                    
   if vErrorText = "" then do:
      oResult = yes.
   end.
   else do:
      oResult = no.
     return.
   end.

PROCEDURE check-gku-sl :
   DEFINE INPUT  PARAMETER iParams      AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER ErrorText    AS CHARACTER  NO-UNDO.

   define var tData-Id          as int  no-undo.
   define var vKppRec           as char no-undo.
   define var vInnRec           as char no-undo.
   define var vVisa4            as char no-undo.
   define var vV-gkh-acct-mask  as char no-undo.
   define var iV-gkh-acct-mask  as char no-undo.

   ErrorText = "".
   find first op where op.op = int(iParams) no-lock no-error.
   if not avail op then do:
      return .
   end.
   find FIRST op-bank  OF op NO-LOCK no-error.
   find FIRST op-entry OF op NO-LOCK no-error.
   if op-entry.acct-db begins "30102" then return.
   vVisa4 = GetXAttrValueEx("op", 
                                  STRING(op.op), 
                                 "Виза4", 
                                 ?).
/*09/01/2018 Т.И. добавить проверку по классификатору "СчетаЖКХ" */
   find first acct where acct.acct = op-entry.acct-db no-lock no-error.
   vV-gkh-acct-mask = GetRefVal("СчетаЖКХ",today,shFilial).
   if acct.cust-cat = "В" then do:
      if not CAN-DO(vV-gkh-acct-mask,op-entry.acct-db) then return.
   end.

   iV-gkh-acct-mask = GetRefVal("ИсклИзЖКУ",today,shFilial).
   if CAN-DO(iV-gkh-acct-mask,op-entry.acct-db) then return.

/**/
   if vVisa4 = "Утверждена" or vVisa4 = "Не требуется" then return.
   vKppRec = GetXAttrValueEx("op", 
                                  STRING(op.op), 
                                 "kpp-rec", 
                                 ?).
   vInnRec = GetXAttrValueEx("op", 
                                  STRING(op.op), 
                                 "ИННЖКУ", 
                                 ?).

   if vKppRec <> ? and vInnRec <> ? then return.
   if avail op-bank then do:
      Find last  DataBlock  WHERE DataBlock.DataClass-ID EQ 'PostJKU' NO-LOCK no-error.
      if not avail DataBlock then do:
         message " DataBlock не найден "  view-as alert-box.
         return.
      end.
      tData-Id = DataBlock.Data-Id.
      find first DataLine WHERE DataLine.Data-ID      EQ tData-Id  
                            and DataLine.Sym3         eq  op.inn 
                            AND num-entries(Dataline.Txt,CHR(10)) ge 3                  
                            AND ENTRY(3,Dataline.Txt,CHR(10)) eq op.ben-acct
               no-lock no-error.
      if avail DataLine  then do:
         ErrorText = "OFGKU".
      end.
   end.
   else do:
      find first acct where acct.acct = op-entry.acct-cr 
                        and acct.close-date = ?
                        no-lock no-error.
      IF acct.cust-cat = "Ю" THEN
      DO:

         FIND FIRST cust-corp WHERE cust-corp.cust-id EQ acct.cust-id NO-LOCK NO-ERROR.
         IF NOT AVAIL cust-corp THEN RETURN.
         vKppRec = GetXAttrValueEx("cust-corp", 
      						STRING(cust-corp.cust-id), 
      						"КПП", 
						"").

	 vInnRec = cust-corp.inn.
         Find last  DataBlock  WHERE DataBlock.DataClass-ID EQ 'PostJKU' NO-LOCK no-error.
         if not avail DataBlock then do:
            message " DataBlock не найден "  view-as alert-box.
            return.
         end.
         tData-Id = DataBlock.Data-Id.
         find first DataLine WHERE DataLine.Data-ID EQ tData-Id  
                              and  DataLine.Sym3 eq  vInnRec 
                              and  DataLine.Sym4 eq  vKppRec 
                  no-lock no-error.
         if avail DataLine  then do:
            ErrorText = "OFGKU".
         end.
      END.
   end.
end PROCEDURE.













