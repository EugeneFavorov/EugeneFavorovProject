/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ptsregbrw.CR
      Comment: ��������(ॣ������) ���㬥�⮢ - ᮧ�����
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
    pl_indocsreg.doc_type = "���"
    NameEvent   = ""
    Details     = ""
    Descriptions = ""
    PrinalFIO = ""
.

