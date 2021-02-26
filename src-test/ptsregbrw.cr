/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: ptsregbrw.CR
      Comment: Движение(регистрация) документов - создание
   Parameters:
         Uses:
      Used by:
      Created:    
     Modified:  
*/

ASSIGN                       
    DateValue = now  
    Branch_ = Branch_Cur_User                   
    pl_indocsreg.create_date = now
    pl_indocsreg.file_name = "term-obl"
    pl_indocsreg.surrogate = ContCode
    pl_indocsreg.doc_type = "ПТС"
    NameEvent   = ""
    Details     = ""
    Descriptions = ""
    PrinalFIO = ""
.

