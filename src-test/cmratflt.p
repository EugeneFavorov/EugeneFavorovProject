/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CMRATFLT.P
      Comment: ������� �������᪮�� 䨫��� �� �������㠫�� �������
   Parameters:
         Uses:
      Used by:
      Created: 04.11.2003 12:09 ilvi  (21484)  
     Modified: 16/01/2014 sasa (0196467) ���ꥬ ��� 0181514
*/

DEFINE INPUT PARAMETER ipClassChar AS CHARACTER NO-UNDO. /* ����� ��ꥪ�*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */

DEFINE VARIABLE list-class AS CHARACTER NO-UNDO.
DEFINE VARIABLE num-class  AS INT64   NO-UNDO.

DEFINE VARIABLE mCodePaySys AS CHARACTER NO-UNDO.
IF work-module EQ "pr" THEN
DO TRANS:
   RUN b-paysys.p ("���⑨��",
                   "���⑨��",
                   "�������� ��� ��������� �������", 
                   4). 
   mCodePaySys = IF {assigned pick-value} THEN
                    pick-value
                 ELSE "*".
END.
{flt-file.add
   &cat     = 1
   &labelt  = "'�᭮��� ४������'"
   &tablef  = "'comm-rate'"
   &include = "'class-code,commission,1,acct,currency,2,min-value,rate-comm,kau,SetFirstFrm,contract,parent,oth8,Editlog,ActionLock,InTitle,branch-id'"
   &double  = "'min-value,rate-comm'"
   &hiddenf = "'SetFirstFrm,kau,contract,parent,oth8,Editlog,ActionLock,InTitle'"
   &sensitivef = "'class-code'"
}

{flt-file.atr
    &asgn         = yes
    &a-table      = "'comm-rate'"
    &xcode        = "'filial-id'"
    &a-code-value = "shFilial"
}

{flt-file.atr   
    &asgn       = yes
    &a-table    = "'comm-rate'"
    &xcode      = "'class-code'"
    &a-label    = "'�����:'"
    &a-code-value = "'comm-rate'"
    &a-initial    = "'comm-rate'"
}

IF work-module EQ "pr" THEN DO:
   {flt-file.atr
      &asgn          = YES
      &xcode         = "'sv-1'"
      &a-label       = "'������⑨��:'"
      &a-help        = "'��� ���⥦��� ��⥬�'"
      &a-datatype    = "'CHARACTER'"
      &a-table       = "'comm-rate'"
      &a-class       = "'comm-rate-pr'"
      &a-code-value  = mCodePaySys
      &a-initial     = mCodePaySys
      &a-basic-signs = "'������⑨��'"
      &a-sensitive   = NO
   }
END.
{flt-file.atr
    &asgn   = yes
    &xcode  = "'kau'"
    &a-op   = "'EQ'"
    &a-initial = "''"
}

{flt-file.add
   &cat     = 2
   &labelt  = "'����'"
   &tablef  = "'comm-rate'"
   &include = "'3,period,4,since,5,IsEndDte,mDateOut'"
   &double  = "'period,since,mDateOut'"
}

{flt-file.atr
   &asgn        = yes
   &xcode       = "'commission'"
   &a-label     = "'��� �����ᨨ:'"
   &a-basic     = "'commission'"
   &a-help      = "'��� �����ᨨ'"
   &a-procename = "'browseld'"
   &a-param     = "'"commission",,,,4'"
}

{flt-file.atr
    &asgn        = yes
    &xcode       = "'since2'"
    &a-initial   = "IF gend-date NE ? THEN STRING(gend-date) ELSE ''"
    &a-procename = "'calend'"
}
   
{flt-file.atr
    &asgn       = yes
    &xcode      = "'since1'"
    &a-procename = "'calend'"
}

{flt-file.atr
    &asgn       = yes
    &xcode      = "'period1'"
    &a-initial  = "'0'"
}

{flt-file.atr
    &asgn       = yes
    &xcode      = "'period2'"
    &a-initial  = "'9999999'"
}

{flt-file.atr
    &asgn       = yes
    &xcode      = "'rate-fixed'"
    &a-help     = "'����஢���� ��� (=), ��業� (%), �� (?)'"
    &a-initial  = "'?'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'SetFirstFrm'"
    &a-datatype   = "'integer'"
    &a-initial    = "'0'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'Editlog'"
    &a-initial    = "'no'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'contract'"
    &a-initial    = "'*'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'ActionLock'"
    &a-initial    = "''"
 
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'IsEndDte'"
    &a-datatype   = "'logical'"
    &a-label      = "'�����뢠�� ��ப�:'"
    &a-help       = "'F1 - �������� ���祭��'"
    &a-format     = "'���㠫�� �� ����/��'"
    &a-initial    = "'yes'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'oth8'"
    &a-initial    = "'no'"
}

{flt-file.atr
    &asgn         = yes
    &a-datatype   = "'DATE'"       
    &a-format     = "'99/99/9999'"    
    &xcode        = "'mDateOut1'"  
    &a-label      = "'��� ��।�� � ��娢 ��:'"
    &a-help       = "'��� ��।�� � ��娢'"    
    &a-initial    = '?'
    &a-multi      = NO       
    &a-procename  = "'calend'"    
    &a-basic     = "''"    
}

{flt-file.atr
    &asgn         = yes
    &a-datatype   = "'DATE'"
    &a-format     = "'99/99/9999'"           
    &xcode        = "'mDateOut2'"
    &a-label      = "'��� ��।�� � ��娢 ��:'"
    &a-help       = "'��� ��।�� � ��娢'"   
    &a-initial    = '?'
    &a-multi      = NO           
    &a-procename  = "'calend'"
    &a-basic      = "''"          
}

{emptyfld.atr 1}
{emptyfld.atr 2}
{emptyfld.atr 3}
{emptyfld.atr 4}
{emptyfld.atr 5}

{flt-file.end}

RETURN.
