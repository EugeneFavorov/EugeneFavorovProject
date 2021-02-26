/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-1998 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ACCT-FLT.P
      Comment: ������� 䨫��� ��⮢.
   Parameters:
         Uses:
      Used by: 
      Created:  SG   26 Nov 97    6:48 pm
     Modified:  29/10/2003 Om
     Modified:  30/11/2006 Ariz  ��������� ��ࠬ���� ��� 䨫���樨 
                                 �� ���⪠� � ����⠬ �� ��⠬.
     Modified: 21.01.2010 19:21 ksv      (0121399) + AcctPosStatus                                 
*/

DEFINE INPUT PARAMETER iClassCode AS CHAR NO-UNDO. /* ����� ��ꥪ�. */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */

DEFINE VARIABLE mProc AS CHARACTER INIT ""   NO-UNDO.
IF fGetSetting("FltAcctByCust","","") = "��" THEN mProc = 'custlist'  .

DEF VAR list-class AS CHAR NO-UNDO. /* ���᮪ ����� � �������ᮢ. */
DEF VAR num-class  AS INT64  NO-UNDO. /* ���稪. */

{flt-file.add
   &cat     = 1
   &labelt  = "'��. ४������'"
   &tablef  = "'acct'"
   &include = "'acct-cat,bal-acct,class-code,iClassCode,acct,contract,currency,user-id,side,branch-id,cust-cat,cust-id,details,UserConf,ViewSC,kau-id,RetFld,mPersGr'"
   &hiddenf = "'UserConf,ViewSC,iClassCode,RetFld,mPersGr'"
   &classf  = iClassCode
   &methf   = "'acct-cat'"
   &sortf   = "'*'"
}

/* ���孨� ����� ��ꥪ�. */
{flt-file.atr 
   &asgn       = YES
   &xcode      = "'iClassCode'"
   &a-initial  = iClassCode
}

{flt-file.add
   &cat     = 2
   &labelt  = "'����'"
   &tablef  = "'acct'"
   &include = "'open-date,1,close-date'"
   &double  = "'open-date,close-date'"
   &sortf   = "'*'"   
}

{flt-file.add
   &cat     = 3
   &tablef  = "''"
   &labelt  = "'���. � ������'"
   &include = "'SldDate,SldType,mPosVal,mPosBal,AcctPosStatus,TurnDate,TurnType,sh-v,sh-b'"
   &double  = "'mPosVal,mPosBal,TurnDate,sh-v,sh-b'"
}

{flt-file.add
   &cat     = 4
   &tablef  = "'blockobject'"
   &labelt  = "'�����஢��'"
   &include = "'avail_blockobject,block-type,beg-datetime,end-datetime,txt[1],v-user-id,txt[2],txt[3],txt[4],txt[5],txt[6],txt[7],txt[8]'"
   &double  = "'beg-datetime,end-datetime'"
   &classf  = "'blockacct'"
}

{flt-file.add 
   &cat     = 5
   &labelt  = "'���. ४�����'"
   &tablef  = "'acct'"
   &include = "'sc-1,sv-1,3,sc-2,sv-2,4,sc-3,sv-3,5,sc-4,sv-4,6,sc-5,sv-5'"
}

{flt-file.add
   &cat     = 6
   &labelt  = "'���'"
   &tablef  = "'acct'"
   &include = "'view-type,2,sort-type,extra-title'"
   &hiddenf = "'extra-title'"
}

{flt-file.add
   &cat     = 7
   &tablef  = "'xlink'"
   &include = "'link-code,link-direction,link-object-id'"
   &labelt  = "'��裡'"
   &sortf   = "'*'"
}
                        /* ������� ��㯯� ⮫쪮 ��� ������� */
/*IF IsUserAdm(USERID("bisquit")) THEN*/
/*DO:                                 */
   {flt-file.add
      &cat     = 8
      &tablef  = "''"
      &include = "'GroupList,GroupFltType'"
      &labelt  = "'��㯯�'"
      &sortf   = "'*'"
   }

   {flt-file.atr
      &asgn          = yes
      &xcode         = "'GroupList'"
      &a-basic       = "''"
      &a-label       = "'��㯯�:'"
      &a-help        = "'���᮪ ��㯯 ����㯠 ��楢�� ��⮢ (� �ଠ� CAN-DO)'"
      &a-datatype    = "'CHARACTER'"
      &a-format      = "'x(2000)'"
      &a-op          = "'EQ'"
      &a-procename   = "'browseld'"
      &a-param       = "'acct-group,,,,2'"
      &a-multi       = YES
   }

   {flt-file.atr
      &asgn          = yes
      &xcode         = "'GroupFltType'"
      &a-label       = "'�⡨��� ��� �ਭ������騥:'"
      &a-help        = "'���᮪ ��㯯 ����㯠 ��楢�� ��⮢ (� �ଠ� CAN-DO)'"
      &a-view-as     = "'RADIO-SET'"
      &a-list        = "'any,all'"
      &a-val-labels  = "'�� �� 㪠������ ��㯯,���쪮 㪠����� ��㯯��'"
      &a-initial     = "'any'"
      &a-code-value  = "'any'"
   }
/*END.*/

{flt-file.atr
   &asgn          = yes
   &xcode         = "'close-date1'"
   &a-code-value  = string(gbeg-date)
   &a-initial     = string(gbeg-date)
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'bal-acct'"
   &a-code-value  = "'*'"
   &a-initial     = "'*'"
   &a-datatype    = "'CHARACTER'"
   &a-op          = "'can-do'"
   &a-op-update   = NO
   &a-multi       = YES
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'cust-id'"
   &a-code-value  = "'*'"
   &a-initial     = "'*'"
   &a-datatype    = "'CHARACTER'"
   &a-multi       = YES
   &a-op          = "'CAN-DO'"
   &a-op-update   = NO
   &a-procename   = mProc
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'acct-cat'"
   &a-sensitive   = "   flt-attr.attr-code-value EQ '*' ~
                     OR flt-attr.attr-code-value EQ ''"
}
/* ����室��� ��� �ࠢ��쭮�� �ନ஢���� �����.
** �᫨ ���祭�� ࠢ�� "", � ���� ������� �� EQ.
** �� �⮩ ��稭� "*" ������ ���� ��砫�� ���祭���. */
IF flt-attr.attr-initial EQ ""
   THEN flt-attr.attr-initial = "*".

{flt-file.atr
   &asgn          = yes
   &xcode         = "'side'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'*,�,�,��'"
   &a-val-labels  = "'��,��⨢��,���ᨢ��,��⨢��-���ᨢ��'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'cust-cat'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'*,�,�,�,�'"
   &a-val-labels  = "'��,�ਤ��᪨� ���,����� ���,�����,����ਡ�����᪨� ���'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'sort-type'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'1,2,3,4,5'"
   &a-val-labels  = "' ��. 2-�� ���. - ���. �/� - ���.,'          +
                     ' ��. 2-�� ���. - ���. 11 ��. �/� - ���.,'  +
                     ' ���� 8 ��. �/� - ���. 11 ��. �/�,'      +
                     ' ��᫥���� 7 ��. �/� - ��� ���. ���,'     +
                     ' �� 㬮�砭��'"
   &a-code-value  = "'5'"
   &a-initial     = "'5'"
   &a-label       = "'����஢��'"
   &a-help        = "'����஢��'"
}

{flt-file.atr
   &asgn          = yes
   &xcode         = "'view-type'"
   &a-view-as     = "'radio-set'"
   &a-list        = "'0,1,2,4'"
   &a-val-labels  = "' ������������, ���줮, ���. ४�����, ���줮 �� ����'"
   &a-code-value  = "'0'"
   &a-initial     = "'0'"
   &a-label       = "'�⮡ࠦ����'"
   &a-help        = "'�⮡ࠦ����'"
}
                        /* ��� �� ०���, ���� ������ ���� 4 �३�. */
IF shModeMulty
THEN ASSIGN
   flt-attr.attr-list         = flt-attr.attr-list + ",3"
   flt-attr.attr-val-labels   = flt-attr.attr-val-labels + ", ������"
   flt-attr.attr-initial      = "3"
.

{flt-file.atr
   &asgn          = yes
   &xcode         = "'extra-title'"
   &a-code-value  = """"
   &a-initial     = """"
}

IF iClassCode EQ "acctd" THEN
DO:
   {flt-file.atr
      &asgn        = YES
      &xcode       = "'currency'"
      &a-label     = "'��� ��:'"
      &a-help      = "'��� 業��� �㬠��'"
      &a-procename = "'browseld'"
      &a-param     = "'sec-code,,,,2'"
   }
END.
ELSE
DO:
   {flt-file.atr
      &asgn        = YES
      &xcode       = "'currency'"
      &a-label     = "'��� ������:'"
      &a-help      = "'��� ������'"
      &a-procename = "'browseld'"
      &a-param     = "'currency,,,,2'"
   }
END.

{flt-file.atr
   &asgn          = yes
   &a-table       = "'acct'"
   &xcode         = "'class-code'"
   &a-code        = "'class-code'"
   &a-multi       = "Yes"
   &a-label       = "'����� ���:'"
   &a-help        = "'F1 - �ࠢ�筨� ����ᮢ ��⮢'"
   &a-procename   = "'getclass'"
   &a-param       = "'?,acct,Yes,R,6'"
}

{flt-file.atr
   &asgn          = yes
   &a-table       = "'acct'"
   &xcode         = "'kau-id'"
   &a-code        = "'kau-id'"
   &a-multi       = "Yes"
   &a-label       = "'��� ���:'"
   &a-help        = "'F1 - �ࠢ�筨� 蠡����� ���'"
   &a-procename   = "'kau-temp'"
   &a-param       = "'�������,�������,,2'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGr'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'SldDate'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-initial    = "STRING(gend-date)"
   &a-label      = "'���⪨ ��:'"
   &a-help       = "'��� ���� ���⪮�'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'SldType'"
   &a-datatype   = "'CHARACTER'"
   &a-view-as    = "'radio-set'"
   &a-list       = "'�,�,*'"
   &a-val-labels = "'����⮢��,�।�⮢��,��'"   
   &a-initial    = "'*'"
   &a-label      = "'��� ᠫ줮:'"
   &a-help       = "'��� ᠫ줮'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosVal1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'���줮 � ��.���. ��:'"
   &a-help       = "'���줮 �� ��⠬ � �����࠭��� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosVal2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'��:'"
   &a-help       = "'���줮 �� ��⠬ � �����࠭��� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosBal1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'���줮 � ���.���. ��:'"
   &a-help       = "'���줮 �� ��⠬ � ��樮���쭮� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mPosBal2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'��:'"
   &a-help       = "'���줮 �� ��⠬ � ��樮���쭮� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnType'"
   &a-view-as     = "'radio-set'"
   &a-datatype   = "'CHARACTER'"   
   &a-list        = "'�,�,�,*'"
   &a-val-labels  = "'����⮢�,�।�⮢�,�� ��,��'"   
   &a-initial    = "'*'"
   &a-label      = "'��� �����:'"
   &a-help       = "'��� �����(��,��,�� ��� ��, �� � ��)'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'AcctPosStatus'"
   &a-datatype   = "'CHARACTER'"   
   &a-initial    = "'*'"
   &a-label      = "'������ ���-⮢:'"
   &a-help       = "'���뢠��� ������ ���㬥�⮢ �� ���� ���⪮�/����⮢'"
   &a-basic      = "'op-status'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnDate1'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-label      = "'������ �:'"
   &a-help       = "'��砫쭠� ��� ���� ����⮢'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-initial    = '?'
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'TurnDate2'"
   &a-datatype   = "'DATE'"   
   &a-format     = "'99/99/9999'"
   &a-label      = "'��:'"
   &a-help       = "'����筠� ��� ���� ����⮢'"
   &a-basic      = "''"
   &a-op-update  = "NO"
   &a-initial    = '?'
   &a-multi      = NO
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-v1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'������ � ��.���. ��:'"
   &a-help       = "'������ �� ��⠬ � �����࠭��� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-v2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'��:'"
   &a-help       = "'������ �� ��⠬ � �����࠭��� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-b1'"
   &a-datatype   = "'DECIMAL'"   
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'0.00'"
   &a-label      = "'������ � ���.���. ��:'"
   &a-help       = "'������ �� ��⠬ � ��樮���쭮� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sh-b2'"
   &a-datatype   = "'DECIMAL'"
   &a-format     = "'>>>,>>>,>>>,>>>,>>9.99'"
   &a-initial    = "'999,999,999,999,999.99'"   
   &a-label      = "'��:'"
   &a-help       = "'������ �� ��⠬ � ��樮���쭮� �����'"
   &a-basic      = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'beg-datetime1'"
   &a-label      = "'��砫� ��:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'beg-datetime2'"
   &a-label      = "'��砫� ��:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'end-datetime1'"
   &a-label      = "'����砭�� ��:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'end-datetime2'"
   &a-label      = "'����砭�� ��:'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_blockobject'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'����稥 ����ᥩ:'"
   &a-help       = "'F1 - �������� ���祭��'"
   &a-format     = "'���/�� ���뢠��'"
   &a-list       = "'���,�� ���뢠��'"
   &a-initial    = "'no'"
}   

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[1]'"
   &a-label      = "'��।�����'"
   &a-op         = "'='"
   &a-procename  = "'pclass'"
   &a-param      = "'order-pay,order-pay,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'v-user-id'"
   &a-basic      = "'user-id'"
   &a-table      = "'blockobject'"
   &a-label      = "'�⢥�ᯮ���⥫�'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[2]'"
   &a-label      = "'��� �࣠��'"
   &a-help       = "'���ଠ�� �� ��⠭�������'"
   &a-procename  = "'pclass'"
   &a-param      = "'������࣠�,������࣠�,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[3]'"
   &a-label      = "'��� ���⠭�������'"
   &a-help       = "'���ଠ�� �� ��⠭�������'"
   &a-procename  = "'pclass'"
   &a-param      = "'�।��ᠭ��,�।��ᠭ��,,4'"
   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[4]'"
   &a-label      = "'���⠭�������'"
   &a-help       = "'���ଠ�� �� ��⠭�������'"
   &a-procename  = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[5]'"
   &a-label      = "'��� �࣠��'"
   &a-help       = "'���ଠ�� � ��⨨'"
   &a-procename  = "'pclass'"
   &a-param      = "'������࣠�,������࣠�,,4'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[6]'"
   &a-label      = "'��� ���⠭�������'"
   &a-help       = "'���ଠ�� � ��⨨'"
   &a-procename  = "'pclass'"
   &a-param      = "'�।��ᠭ��,�।��ᠭ��,,4'"   
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[7]'"
   &a-label      = "'���⠭�������'"
   &a-help       = "'���ଠ�� � ��⨨'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'txt[8]'"
   &a-label      = "'���. ���ଠ��'"
   &a-help       = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-code'"
   &a-label      = "'��� �裡:'"
   &a-multi      = NO
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-direction'"
   &a-label      = "'��ꥪ�:'"
   &a-view-as    = 'radio-set'
   &a-list       = 'S,T,?'
   &a-val-labels = "'����㯠�� ���筨��� �裡,�ਪ९���,����� �裡'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'link-object-id'"
   &a-basic      = "'link-object-id'"
   &a-table      = "''"
   &a-label      = "'�����䨪��� ��ꥪ�:'"
}

{emptyfld.atr 1}
{emptyfld.atr 2}
{emptyfld.atr 3}
{emptyfld.atr 4}
{emptyfld.atr 5}
{emptyfld.atr 6}
{emptyfld.atr 7}

{flt-file.end}

RETURN.
