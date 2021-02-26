/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2020 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: dpsfltop.p
      Comment: ������ ���㬥�⮢ ���
   Parameters: in-cat
         Uses:
      Used by:
      Created: ... ����� �����
     Modified: 21.02.2003 16:48 SEMA     �� ��� 0014217 ���������� ����� ��ࠬ��஢ � 䨫��� �� ���㬥�⠬
     Modified: 05.05.2003 15:27 SEMA     �� ��� 0014217 �������� ���ᠭ��
     Modified: 14.07.2003 kraw (0018600) �⮡� or&and �� ���ਭ������ ��� ���४�����
     Modified: 23.07.2003 ilvi (14217) ��������� ���� � 䨫��� PrintProc - ��楤�� ����
     Modified: 09.07.2004 ilvi (21766) �������஢�� ���. ��������� ���� �㡠����⨪�
     Modified: 24.04.2005 xaro         �� ��� 0034389
     Modified: 09.07.2005 Om  ��ࠡ�⪠.
                        � �������� "������/��ᯮ��" ��������� ���� "�६� �ᯮ��".
                        ��� ����� "�६� �ᯮ��" � "�६� ������" ����� ��������
                        ���ࢠ�.
     Modified: 09.10.2007 Muta 0080920 ��� ��㧥� �஢���� ��������� ����������� �⮡���
                                       �஢���� ����騥/�� ����騥 �易���� ��ꥪ⮢.

*/
{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */

DEFINE INPUT PARAMETER in-cat     LIKE op-entry.acct-cat NO-UNDO.

DEFINE VARIABLE list-class    AS CHARACTER  NO-UNDO. /* ᯨ᮪ ����� � �������ᮢ*/
DEFINE VARIABLE num-class     AS INT64    NO-UNDO. /* N ����� */

DEFINE VARIABLE mClassCode    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mProgressCode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE xlist-id      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mAccessType   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE mProc AS CHARACTER INIT ""   NO-UNDO.
DEFINE VARIABLE mParam1 AS CHARACTER INIT ""   NO-UNDO.
DEFINE VARIABLE mParam2 AS CHARACTER INIT ""   NO-UNDO.

{list-id.i}
mClassCode = in-cat.
IF in-cat BEGINS "op" THEN
   in-cat = "".

mProgressCode = GetXclassProgress(mClassCode).
IF fGetSetting("FltOpByAcct","","") = "��" AND mProgressCode EQ "op" THEN
DO:
   mProc   = 'acctlist'  .
   mParam1 = 'db'.
   mParam2 = 'cr'.
END.

&SCOPED-DEFINE pref  X

/* �������� 䨫��� */
{flt-file.add
   &cat        =  1
   &tablef     =  "'op'"
   &include    =  ""PrintProc,SetFirstFrm,CalcProc,flag-date,cust-role-id,VygprInd,op-date,IsEndDte,1,user-id,user-inspector,sv-10,2,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,op-error,details,sc-9,sv-9,name-ben,inn,op-transaction,op,UserConf,SrcClass,SrcSurr,File-Name,Surrogate,Title,Return,RetFld,branch-id,InsTrans,DelTrans,mPersGrDb,mPersGrCr,RelQFrames,contract-date,mExDblPO,AllowNoSelection,OpF8F12""
   &sortf      =  ""op-date,user-id,user-inspector,doc-date,due-date,op-value-date,ins-date,order-pay,doc-num,doc-type,acct-cat,op-status,op-kind,class-code,ben-acct,op-error,details,name-ben,inn,contract-date""
   &DOUBLE     =  ""op-date,contract-date""
   &classf     =  "'op' + in-cat"
   &hiddenf    =  ""File-Name,Surrogate,PrintProc,SetFirstFrm,CalcProc,sc-9,flag-date,cust-role-id,VygprInd,UserConf,SrcClass,SrcSurr,Title,IsEndDte,Return,RetFld,InsTrans,DelTrans,mPersGrDb,mPersGrCr,RelQFrames,mExDblPO,OpF8F12""
   &sensitivef =  "'PrintProc,SetFirstFrm,CalcProc,SrcSurr,flag-date,cust-role-id,VygprInd,InsTrans,DelTrans,RelQFrames,mExDblPO'"
}

&IF DEFINED(ORACLE) &THEN       
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'details'"
      &a-basic      = "''"
   }
&ENDIF    

{flt-file.atr
   &asgn         = yes
   &xcode        = "'File-Name'"
   &a-basic      = "'File-Name'"
   &a-table      = "'obj-transaction'"
   &a-class      = "'obj-transaction'"
   &a-label      = "'��ꥪ�'"
   &a-code-value = "''"
   &a-initial    = "''"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'Surrogate'"
   &a-basic      = "'Surrogate'"
   &a-table      = "'obj-transaction'"
   &a-class      = "'obj-transaction'"
   &a-label      = "'�����䨪���'"
   &a-code-value = "''"
   &a-initial    = "''"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'Return'"
    &a-initial    = "'field'"
}

{flt-file.atr
    &asgn         = yes
    &xcode        = "'RelQFrames'"
    &a-code-value = "'14'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mExDblPO'"
   &a-basic      = "''"
   &a-table      = "''"
   &a-class      = "''"
   &a-label      = "'�᪫. ��������� PackObject'"
   &a-code-value = "'NO'"
   &a-initial    = "'NO'"
}

{flt-file.add
   &cat     = 2
   &tablef  = "'op-entry'"
   &include = "'value-date,prev-year,op-cod,type,3,acct-flt-new,acct-db,or&and,acct-cr,currency,qty,symbol,amt-cur,amt-rub,kau-db,kau-cr,sc-22,sv-22,op-entry'"
   &sortf   = ""*""
   &DOUBLE  = "'qty,amt-cur,amt-rub'"
   &hiddenf = "'op-entry' + (IF mProgressCode NE 'op' THEN ',acct-flt-new' ELSE '')"
   &classf  = "'op-entry'"
}

{flt-file.atr
   &asgn        = YES
   &xcode       = "'acct-flt-new'"
   &a-table     = "''"
   &a-basic     = "''"
   &a-label     = "'�⡮� �� �஢�����:'"
   &a-datatype = "'LOGICAL'"
   &a-format    = "'��-������/��-��஬�'"
   &a-initial   = "'NO'"
   &a-help      = "'���ᮡ 䨫���樨 ���㬥�⮢ �� ४����⠬ �஢����'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'kau-cr'"
   &a-op    = "'begins'"
   &a-label = "'��� �� �।���:'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'kau-db'"
   &a-op    = "'begins'"
   &a-label = "'��� �� ������:'"
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'acct-db'"
   &a-procename = mProc
   &a-param     = mParam1
}

{flt-file.atr
   &asgn    = YES
   &xcode   = "'acct-cr'"
   &a-procename = mProc 
   &a-param     = mParam2
}

{flt-file.add
   &cat     = 3
   &tablef  = "'op-bank'"
   &labelt  = "'�����'"
   &include = "'op-bank-type,bank-code-type,bank-code,corr-acct,bank-name'"
   &sortf   = ""*""
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'bank-code'"
   &a-label   = "'���祭��:'"
}


{flt-file.atr
   &asgn      = yes
   &xcode     = "'bank-code-type'"
   &a-multi      = "no"
   &a-procename  = "'pclass'"
   &a-param      = "'"��������","��������","����_���������������_�����",2'"
}

{flt-file.add
   &cat     = 4
   &labelt  = "'������/��ᯮ��'"
   &tablef  = "'op-impexp'"
   &include = "'avail_op-impexp,mail-user-num,op-reference,bank-reference,imp-batch,exp-batch,op-date,imp-date,imp-time,exp-date,exp-time'"
   &DOUBLE  = "'imp-time,exp-time'"
   &sortf   = "'*'"
}

{flt-file.add
   &cat     = 7
   &labelt  = "'���.४�. ����.'"
   &tablef  = "'op'"
   &include = "'sc-5,sv-5,31,sc-31,sv-31,32,sc-32,sv-32,33,sc-33,sv-33,34,sc-34,sv-34'"
}

{flt-file.add
   &cat     = 8
   &labelt  = "'�����. ���./��.'"
   &tablef  = "'packet'"
   &include = "'avail_packet,State,AbonentID,PackDate,PackTime,PackError,mail-format,Kind,FileExchID,ClassError'"
   &DOUBLE  = "'PackDate,PackTime'"
   &sortf   = "'*'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'AbonentID'"
   &a-label   = "'�����-� �������:'"
}

{flt-file.atr
   &asgn    =  yes
   &xcode   =  "'branch-id'"
   &a-label =  "'���ࠧ�������:'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGrDb'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn          = YES
   &xcode         = "'mPersGrCr'"
   &a-basic       = "''"
   &a-initial     = "'*'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'ClassError'"
   &a-label      = "'������-� �訡��:'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'ClassError'"
   &a-help       = "'F1-��㧥� ����c�䨪��஢ �訡��, �ᯮ��㥬�� � �࠭������'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'mail-format'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'mail-format'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'Kind'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'Kind'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'PackTime1'"
   &a-view-as = "'time'"
   &a-format  = "'<<<<,<<9'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'PackTime2'"
   &a-view-as = "'time'"
   &a-format  = "'<<<<,<<9'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'State'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'state'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'PackError'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'PackError'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_packet'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'����稥 ����ᥩ:'"
   &a-help       = "'F1 - �������� ���祭��'"
   &a-format     = "'���/�� ���뢠��'"
   &a-list       = "'���,�� ���뢠��'"
   &a-initial    = "'no'"
}

{flt-file.add
    &cat     = 10
    &tablef  = "'Seance'"
    &labelt  = "'������ ���./��.'"
    &include = "'avail_seance,Number,SeanceDate,SeanceOpKind,Direct'"
    &sortf   = "'Number,SeanceDate,op-kind,Direct'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_seance'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'����稥 ����ᥩ:'"
   &a-help       = "'F1 - �������� ���祭��'"
   &a-format     = "'���/�� ���뢠��'"
   &a-list       = "'���,�� ���뢠��'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'Number'"
   &a-help      = "'����� ᥠ��'"
   &a-help       = "'F1 - ��㧥� �⠭������ �࠭���権'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'SeanceOpKind'"
   &a-basic      = "'op-kind'"
   &a-label      = "'�࠭�����:'"
   &a-help       = "'F1 - ��㧥� �⠭������ �࠭���権'"
}

{flt-file.add
    &cat     = 11
    &tablef  = "'Reference'"
    &labelt  = "'��뫪� ���./��.'"
    &include = "'RefClass,RefDate,RefValue'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'�����:'"
   &a-procename  = "'fltopnav'"
   &a-param      = "'RefClass'"
   &a-help       = "'F1-��㧥� ��뫮�'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefDate'"
   &a-basic      = "'op-date'"
   &a-label      = "'���:'"
   &a-help       = "'��� ��뫮�'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'RefValue'"
   &a-label      = "'���祭��:'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'sc-22'"
   &a-class = "'op-entry'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'sv-22'"
   &a-class = "'op-entry'"
}
{flt-file.add
    &cat     = 16
    &tablef  = "'PackObject'"
    &labelt  = "'���. ᮮ��.'"
    &include = "'PackObjKind'"
}                    

{flt-file.atr
   &asgn         = yes
   &xcode        = "'PackObjKind'"
   &a-basic      = "'Kind'"
    &a-label     = "'��� �裡'"
   &a-param      = "'PackObjKind'"
}  

{flt-file.add
   &cat     = 17
   &tablef  = "'xlink'"
   &include = "'link-code,link-direction,link-object-id'"
   &labelt  = "'��裡'"
   &sortf   = "'*'"
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

IF work-module EQ "dps" THEN DO:
   /* ��������� ������ �������� ��-ࠧ���� ��ࠡ��뢠����
   ** � ��㧥� ���㬥�⮢ � �஢����.
   ** � ���㬥��� �१ ����塞� ����.
   ** � �஢����� �१ �����. */
   {flt-file.add
      &cat     = 5
      &tablef  = "'acct'"
      &labelt  = "'���'"
      &include = "'sv-1,sv-2,sv-3,sv-4,sv-7,sv-8,5,sv-6'"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-1'"
      &a-label       =  "'������ ��� �����:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'�������,�������,,4'"
      &a-op          =  "'EQ'"
      &a-datatype    =  "'CHARACTER'"
      &a-procename   =  "'codelay'"
      &a-param       =  "'�������,�������,,4'"
   } 
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-2'"
      &a-label       =  "'            �।��:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'�������,�������,,4'"
      &a-op          =  "'EQ'"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-3'"
      &a-label       =  "'��� ������ ��� �����:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'��ொ�,��ொ�,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-4'"
      &a-label       =  "'                 �।��:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'��ொ�,��ொ�,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-7'"
      &a-label       =  "'�����祭�� ��� �����:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'�������,�������,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-8'"
      &a-label       =  "'                �।��:'"
      &a-basic       =  "''"
      &a-procename   =  "'codelay'"
      &a-param       =  "'�������,�������,,4'"
      &a-op          =  "'EQ'"
      &a-multi       =  "NO"
   }
   /* ������騥 ���� � ��㧥� �஢���� ��ࠡ��뢠�� ��� �����. */
   IF mProgressCode  EQ "op-entry"
   THEN DO:
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-1'"
         &a-table       =  "'dbAcct'"
         &a-class       =  "'acctb'"
         &a-basic-signs =  "'�������'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-2'"
         &a-table       =  "'crAcct'"
         &a-class       =  "'acctb'"
         &a-basic-signs =  "'�������'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-3'"
         &a-table       =  "'dbAcct'"
         &a-class       =  "'acct'"
         &a-basic-signs =  "'��ொ�'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-4'"
         &a-table       =  "'crAcct'"
         &a-class       =  "'acct'"
         &a-basic-signs =  "'��ொ�'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-7'"
         &a-basic       =  "'contract'"
         &a-table       =  "'dbAcct'"
      }
      {flt-file.atr
         &asgn          =  YES
         &xcode         =  "'sv-8'"
         &a-basic       =  "'contract'"
         &a-table       =  "'crAcct'"
      }
   END.

   {flt-file.atr
      &asgn          =  YES
      &xcode         =  "'sv-6'"
      &a-label       =  "'��ᯮ��஢��� ���㬥��:'"
      &a-help        =  "'�� - �ᯮ��஢���, ��� - �� �ᯮ��஢���, ? - ��'"
      &a-datatype    =  "'CHARACTER'"
      &a-format      =  "'��/���'"
      &a-table       =  "'op'"
      &a-class       =  "'op' + in-cat"
      &a-code-value  =  "'?'"
      &a-initial     =  "'?'"
      &a-basic-signs =  "'��ᯮ��'"
   }
   {emptyfld.atr 5}
END.

IF work-module EQ "incass" THEN
DO:

      {flt-file.add
         &cat        = 9
         &tablef     = "'op'"
         &labelt     = "'��������'"
         &include    = "'loan-id,type-client,num-client,inc-acct,sum_nac,n_marsh,n_marsh_id,n_baul,pr_rur_cur,pr_evncash'"
         sensitivef  = "'class-code'"
      }


      {flt-file.atr
         &asgn         = yes
         &xcode        = "'loan-id'"
         &a-datatype   = "'character'"
         &a-label      = "'����� �������:'"
         &a-help       = "'����� ������� �� �������'"
         &a-format     = "'x(9)'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'loaninc'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'type-client'"
         &a-datatype   = "'character'"
         &a-label      = "'��� ������:'"
         &a-help       = "'��� ������'"
         &a-format     = "'x(1)'"
         &a-basic      = "''"

      }  

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'num-client'"
         &a-datatype   = "'integer'"
         &a-format     = "'>>>>>>>9'"
         &a-label      = "'������ �:'"
         &a-help       = "'������ �'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'num-client'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'inc-acct'"
         &a-datatype   = "'character'"
         &a-format     = "'x(20)'"
         &a-label      = "'������ ���:'"
         &a-help       = "'������ ���'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'acct'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'sum_nac'"
         &a-datatype   = "'decimal'"
         &a-format     = "'>>>>>>>>9.99'"
         &a-label      = "'�㬬� ���������:'"
         &a-help       = "'�㬬� ���������'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'sum_nac'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_marsh'"
         &a-datatype   = "'integer'"
         &a-format     = "'>>>>>9'"
         &a-label      = "'����� ������� (������):'"
         &a-help       = "'����� ������� (������)'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'marsh'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_marsh_id'"
         &a-datatype   = "'character'"
         &a-format     = "'x(8)'"
         &a-label      = "'�����. ⨯����� �������:'"
         &a-help       = "'�����䨪��� ⨯����� �������'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'marsh_id'"
      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'n_baul'"
         &a-datatype   = "'character'"
         &a-format     = "'x(12)'"
         &a-label      = "'����� �㬪�:'"
         &a-help       = "'����� �㬪�'"
         &a-basic      = "''"
         &a-procename  = "'fltprinc'"
         &a-param      = "'baul'"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'pr_rur_cur'"
         &a-datatype   = "'logical'"
         &a-format     = "'��/���'"
         &a-initial    = "'yes'"
         &a-label      = "'��� �������:'"
         &a-help       = "'��� �������'"
         &a-basic      = "''"

      }

      {flt-file.atr
         &asgn         = yes
         &xcode        = "'pr_evncash'"
         &a-datatype   = "'logical'"
         &a-format     = "'��/���'"
         &a-initial    = "'?'"
         &a-label      = "'������ ����:'"
         &a-help       = "'������ ����'"
         &a-basic      = "''"

      }

    /* ��������� ��� ���������� 䨫��� */


END.


{emptyfld.atr 1}
{emptyfld.atr 2}
{emptyfld.atr 3}
{emptyfld.atr 31}
{emptyfld.atr 32}
{emptyfld.atr 33}
{emptyfld.atr 34}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-transaction'"
   &a-initial    = '0'
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'class-code'"
   &a-label      = "'����� ���㬥��'"
   &a-label      = "'����� ���㬥��'"
   &a-procename  = "'getclass'"
   &a-param      = "'?,op,Yes,R,2'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'or&and'"
   &a-basic      = "''"
   &a-label      = "'�/���:'"
   &a-help       = "'��� - ���� � ����� ���� � �।��, � - � � ����� � � �।��'"
   &a-datatype   = "'logical'"
   &a-format     = "'�/���'"
   &a-table      = "'op-entry'"
   &a-code-value = "'yes'"
   &a-initial    = "'yes'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sv-10'"
   &a-basic      =  "''"
   &a-label      = "'����� ��:'"
   &a-help       = "'��� - ����� �� ��⠬, ���㬥�� - ����� �� ���㬥�⠬'"
   &a-datatype   = "'logical'"
   &a-format     = "'��⠬/���㬥�⠬'"
   &a-list       = "'��⠬,���㬥�⠬'"
   &a-code-value = "'no'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'sv-9'"
   &a-label      = "'���� ���㬥��:'"
   &a-help       = "'�� - ���� ���㬥��, ��� - �� ���� ���㬥��'"
   &a-datatype   = "'logical'"
   &a-format     = "'��/���'"
   &a-table      = "'op'"
   &a-code-value = "'no'"
   &a-initial    = "'no'"
}

IF mClassCode EQ "opb-card" THEN
DO:
{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-date1'"
   &a-code-value = STRING(gbeg-date)
   &a-initial    = "'01/01/1901'"
   &a-sort       = YES
   &a-sort-order = 1
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-date2'"
   &a-code-value = STRING(gend-date)
   &a-initial    = "'01/01/3001'"
}
END.
ELSE
DO:
{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-date1'"
   &a-code-value = STRING(gbeg-date)
   &a-initial    = STRING(gbeg-date)
   &a-sort       = YES
   &a-sort-order = 1
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-date2'"
   &a-code-value = STRING(gend-date)
   &a-initial    = STRING(gend-date)
}
END.

{flt-file.atr
   &asgn         = yes
   &xcode        = "'op-entry'"
   &a-initial    = "'?'"
}


{flt-file.atr
    &asgn         = yes
    &xcode        = "'IsEndDte'"
    &a-datatype   = "'logical'"
    &a-label      = "'��� ��� ���:'"
    &a-help       = "'F1 - �������� ���祭��'"
    &a-format     = "'��/���'"
    &a-initial    = "'no'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'acct-cat'"
   &a-code-value = in-cat
   &a-initial    = "''"
   &a-sensitive  = "if in-cat = """" then yes else no"
}


{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-status'"
   &a-code-value = rightview
   &a-initial    = rightview
   &a-list       = rightview
   &a-sort       = YES
   &a-sort-order = 2
}
/* > 0133808  */
IF LOOKUP(list-id,'*') EQ 0 THEN xlist-id = "%������_�����������%" + "," + list-id.
                            ELSE xlist-id = list-id.
/* < 0133808  */
{flt-file.atr
   &asgn         = YES
   &xcode        = "'user-id'"
   &a-label      = '����㤭���:'
   &a-sort       = YES
   &a-sort-order = 3
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'user-inspector'"
   &a-initial    = "'*'"
   &a-label      = '����஫���:'
}
                        /* �����뢠�� ᯨ᮪ ���稭����� */
{&pref}flt-attr.attr-code-value = list-id NO-ERROR.
                        /* �᫨ �� �� ����頥��� � ������, � "*" */
IF ERROR-STATUS:ERROR THEN
   {&pref}flt-attr.attr-code-value = "*".

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op'"
   &a-sort       = YES
   &a-sort-order = 4
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'op-error'"
   &a-op         = "'matches'"
}

IF in-cat EQ "d" THEN DO:
   {flt-file.atr
      &asgn    = yes
      &xcode   ='"currency"'
      &a-label ='"��� ��:"'
    }
END.

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_op-impexp'"
   &a-basic      = "''"
   &a-datatype   = "'logical'"
   &a-label      = "'����稥 ����ᥩ:'"
   &a-help       = "'F1 - �������� ���祭��'"
   &a-format     = "'���/�� ���뢠��'"
   &a-list       = "'���,�� ���뢠��'"
   &a-initial    = "'no'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'op-reference'"
   &a-label = "'���७� � �����:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'bank-reference'"
   &a-label = "'���७� � ��:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'op-date'"
   &a-code  = "'op-date3'"
   &a-label = "'��� ����.���:'"
}
{flt-file.atr
   &asgn    = yes
   &xcode   = "'imp-date'"
   &a-label = "'��� ������:'"
}

{flt-file.atr
   &asgn      = yes
   &xcode     = "'imp-time1'"
   &a-label   = "'�६� ������ ��:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'imp-time2'"
   &a-label   = "'�६� ������ ��:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'exp-time1'"
   &a-label   = "'�६� �ᯮ�� ��:'"
   &a-view-as = "'time'"
}
{flt-file.atr
   &asgn      = yes
   &xcode     = "'exp-time2'"
   &a-label   = "'�६� �ᯮ�� ��:'"
   &a-view-as = "'time'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = "'exp-date'"
   &a-label = "'��� �ᯮ��:'"
}

{flt-file.atr
   &asgn    = yes
   &xcode   = '"ins-date"'
   &a-label = '"��� ����㯫����:"'
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'flag-date'"
   &a-code-value = "'yes'"
   &a-initial    = "'yes'"
   &a-basic      = "'op-date'"
}

/* SetFirstFrm  - �� �ᯮ�짮����� ��८�।���� ����� ��ࢮ��
                  �����뢠����� �३��.
                  ��ଠ� - �᫮ �� 0 �� 5
                  (�� 㪠����� 0 - ���� �⠭���⭠� ��ࠡ�⪠,
                   � ���� ��砥 㪠�뢠���� ������� ����� �३��)
*/
{flt-file.atr
   &asgn         = yes
   &a-table      = "'op'"
   &xcode        = "'SetFirstFrm'"
   &a-datatype   = "'integer'"
   &a-format     = ""9""
   &a-initial    = "'0'"
   &a-code-value = "'0'"
   &a-basic      = "'op-date'"
}

/* PrintProc  - �� �ᯮ�짮����� ��।���� ��楤��� ����, ��뢠���� �� Ctrl-G
                �ᯮ������ ��� �뢮�� ��⮪��� ���� � 402 �ଥ */
{flt-file.atr
   &asgn          =  YES
   &xcode         =  "'PrintProc'"
   &a-initial     =  "''"
}

/* CalcProc  - �� �ᯮ�짮����� ��।���� ��楤��� +*/
{flt-file.atr
   &asgn         = yes
   &xcode        = "'CalcProc'"
   &a-initial    = "''"
   &a-code-value = "''"
}

/* Title - ���������� � �⠭���⭮�� ��������� ᯨ᪠ */
{flt-file.atr
   &asgn         = yes
   &xcode        = "'Title'"
   &a-initial    = "''"
}

{flt-file.atr
   &asgn       = yes
   &xcode      = "'sc-9'"
   &a-multi    = yes
}

IF mProgressCode EQ "op" THEN
DO:
   {flt-file.add
      &cat     = 6
      &labelt  = "'��ୠ� ���������'"
      &tablef  = "'history'"
      &include = "'modify,modif-merge,modif-date,modif-time,user-id1'"
      &DOUBLE  = "'modif-date,modif-time'"
      &sortf   = "'modify,modif-date,modif-time,user-id1'"
   }
   FOR EACH flt-attr WHERE flt-attr.attr-code BEGINS "Modif-Time":
      flt-attr.attr-view-as    = "time".
   END.
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'modify'"
      &a-procename  = "'sel-list'"
      &a-param      = "'c;w,modify'"
      &a-help       = "'��� ��������� (c - ᮧ�����, w - ����䨪���)'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'modif-merge'"
      &a-basic      = "''"
      &a-datatype   = "'logical'"
      &a-label      = "'���� � �६�'"
      &a-help       = "'F1 - �������� ���祭��'"
      &a-format     = "'��ꥤ�����/�� ��ꥤ�����'"
      &a-list       = "'��ꥤ�����,�� ��ꥤ�����'"
      &a-initial    = "'no'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'user-id1'"
      &a-basic      = "'user-id'"
      &a-label      = "'����㤭��'"
      &a-help       = "'��� ���㤭���, ���ᨢ襣� ���������'"
   }

END.

{flt-file.add
      &cat        =  12
      &labelt     =  "'���. ��ꥪ��'"
      &tablef     =  "'cust-role'"
      &include    =  "'CRClass,cust-cat,cust-id,35,open-date,36,close-date'"
      &sortf      =  "'*'"
      &double     =  "'open-date,close-date'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'CRClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'����:'"
   &a-help       = "'���� ��ꥪ�'"
   &a-procename  = "'clascode'"
   &a-param      = "'cust-role,op-cust,4'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'cust-cat'"
   &a-basic      = "'cust-cat'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'cust-id'"
   &a-basic      = "'cust-id'"
}


{flt-file.atr
   &asgn         = YES
   &xcode        = "'open-date1'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��砫� ��:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'open-date2'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��砫� ��:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'close-date1'"
   &a-basic      = "'close-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'����砭�� ��:'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'close-date2'"
   &a-basic      = "'close-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'����砭�� ��:'"
   &a-procename  = "'calend'"
}

{flt-file.add
      &cat        =  13
      &labelt     =  "'����襭��'"
      &tablef     =  "'cstrlopentry'"
      &classf     =  "'cstrlopentry'"
      &include    =  "'avail_cstrlopentry,VClass,v-cust-cat,v-cust-id,37,v-open-date,38,issue-date'"
      &sortf      =  "'*'"
      &double     =  "'v-open-date,issue-date'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'VClass'"
   &a-basic      = "'Class-Code'"
   &a-label      = "'����:'"
   &a-help       = "'���� ��ꥪ�'"
   &a-procename  = "'clascode'"
   &a-param      = "'cust-role,opent-cust,4'"

}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'avail_cstrlopentry'"
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
   &xcode        = "'v-cust-cat'"
   &a-basic      = "'cust-cat'"
   &a-label      = "'��� ������:'"
   &a-help       = "'��� ������'"
}

{flt-file.atr
   &asgn         = yes
   &xcode        = "'v-cust-id'"
   &a-basic      = "'cust-id'"
   &a-label      = "'������ N:'"
   &a-help       = "'���浪��� ����� ������'"

}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'v-open-date1'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��� ������ ��:'"
   &a-help       = "'��� ������ ����襭��'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'v-open-date2'"
   &a-basic      = "'open-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��� ������ ��:'"
   &a-help       = "'��� ������ ����襭��'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'issue-date1'"
   &a-basic      = "'issue-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��� ����襭�� ��:'"
   &a-help       = "'��� ����襭��'"
   &a-procename  = "'calend'"
}

{flt-file.atr
   &asgn         = YES
   &xcode        = "'issue-date2'"
   &a-basic      = "'issue-date'"
   &a-format     = "'99/99/9999'"
   &a-label      = "'��� ����襭�� ��:'"
   &a-help       = "'��� ����襭��'"
   &a-procename  = "'calend'"
}

IF work-module = "pn" THEN DO:
   FIND FIRST flt-attr WHERE flt-attr.attr-code = "due-date" EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL flt-attr THEN DELETE flt-attr.

   {flt-file.add
      &cat        =  14
      &tablef     =  "''"
      &labelt     =  "'���⥦� ��ᥫ����'"
      &include    =  "'due-date,mPNIsTrans,mPNCodeParam,mPNNaimParam,mPNReceiver,mPNAcct,mPNSum'"
      &sortf      =  "'due-date,mPNIsTrans,mPNCodeParam,mPNNaimParam,mPNReceiver,mPNAcct,mPNSum'"
      &double     =  "'due-date,mPNSum'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'due-date1'"
      &a-basic      = "'due-date'"
      &a-table      = "'op'"
      &a-class      = "''"
      &a-label      = "'��� ���⥦� ��:'"
      &a-initial    = "'?'"
      &a-datatype   = "'date'"
      &a-format     = "'99/99/9999'"
      &a-help       = "'��� ���⥦�'"
   }
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'due-date2'"
      &a-basic      = "'due-date'"
      &a-table      = "'op'"
      &a-class      = "''"
      &a-label      = "'��� ���⥦� ��:'"
      &a-initial    = "'?'"
      &a-datatype   = "'date'"
      &a-format     = "'99/99/9999'"
      &a-help       = "'��� ���⥦�'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNIsTrans'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'����᫥�:'"
      &a-initial    = "'?'"
      &a-datatype   = "'logical'"
      &a-format     = "'��/���'"
      &a-help       = "'���⥦ ����᫥� ��� ���'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNCodeParam'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'��� ���⥦�:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'code,��������⥦�,class,��������⥦�,class,4'"
      &a-help       = "'��� ���⥦�'"
   }
  
   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNNaimParam'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'������������:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'name,��������⥦�,class,��������⥦�,class,4'"
      &a-help       = "'������������ ���� ���⥦�'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNReceiver'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'�����⥫�:'"
      &a-initial    = "'*'"
      &a-procename  = "'pn-sel-recv'"
      &a-param      = "'NO,4'"
      &a-help       = "'�����⥫�'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNAcct'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'��� �����⥫�:'"
      &a-initial    = "'*'"
      &a-procename  = "'browseldvar'"
      &a-param      = "'acct,loan-acct-pn,acct-type,���⍠�,acct-type,4'"
      &a-format     = "'x(20)'"
      &a-help       = "'��� �����⥫�'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNSum1'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'�㬬� ��:'"
      &a-initial    = "'0.00'"
      &a-datatype   = "'decimal'"
      &a-format     = "'>>>,>>>,>>9.99'"
      &a-help       = "'�㬬� ���⥦�'"
   }

   {flt-file.atr
      &asgn         = yes
      &xcode        = "'mPNSum2'"
      &a-basic      = "''"
      &a-table      = "''"
      &a-class      = "''"
      &a-label      = "'�㬬� ��:'"
      &a-initial    = "'0.00'"
      &a-datatype   = "'decimal'"
      &a-format     = "'>>>,>>>,>>9.99'"
      &a-help       = "'�㬬� ���⥦�'"
   }

END.

/* 0174879 AllowNoSelection  - ����� ᮧ����� tmprecid � tmprecid.cqr �� Ctrl+Enter */
{flt-file.atr
   &asgn         = YES
   &a-table      = "'op'"
   &xcode        = "'AllowNoSelection'"
   &a-datatype   = "'LOGICAL'"
   &a-format     = "'YES/NO'"
   &a-label      = "'����� ��ᬮ��'"
   &a-initial    = "'NO'" 
   &a-hidden     = YES
}


/* ������� ��㯯� ⮫쪮 ��� ������� */
/*IF IsUserAdm(USERID("bisquit")) THEN*/
/*DO:                                 */
   {flt-file.add
      &cat     = 15
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
      &a-basic       = "''"
      &a-label       = "'�⡨��� ��� �ਭ���.:'"
      &a-help        = "'���᮪ ��㯯 ����㯠 ��楢�� ��⮢ (� �ଠ� CAN-DO)'"
      &a-view-as     = "'RADIO-SET'"
      &a-list        = "'any,all'"
      &a-val-labels  = "'�� �� 㪠������ ��㯯,���쪮 㪠����� ��㯯��'"
      &a-initial     = "'any'"
      &a-code-value  = "'any'"
   }
/*END.*/

{emptyfld.atr 35}
{emptyfld.atr 36}
{emptyfld.atr 37}
{emptyfld.atr 38}

{flt-file.end}

            /* �� ���℮��������� (�᫨ �� �� ����� �� ���짮��⥫�, ������ ���
            ** ��砫쭮� ���祭�� � ����奬�) ��।����, ����� ���祭��� �㤥�
            ** �।��⠭���������� ���� "����� ��" ��㧥஢ ���㬥�⮢ � �஢����.
            ** �᫨ �� �� ����� (�� ���짮��⥫� � ���⮥ ��砫쭮� ���祭��), �
            ** ���祭�� ���� ��।������ �ࠢ����: �᫨ ���짮��⥫� ����� �ࠢ�
            ** �� ��ᬮ�� ��㯯 ��⮢, ��⠭���������� ����� "�� ��⠬", ����
            ** ��⠥��� ���祭�� ��-㬮�砭�� (ᥩ�� "�� ���㬥�⠬"). �᫨ ������
            ** ���祭�� ��, � ���� 䨫��� ��⠭���������� � �������� ���祭��. */
mAccessType = GetXAttrValueEx ("_user", USERID("bisquit"), "���℮���������", GetXAttrInit ("_user","���℮���������")).
IF    (mAccessType              EQ ""
   AND GetRightAcctGroups ("r") NE "")
    OR mAccessType              EQ "��⠬"
THEN
   RUN SetFltField ('sv-10','yes').
IF mAccessType EQ "���㬥�⠬" THEN
   RUN SetFltField ('sv-10','no').

RETURN.
/* $LINTFILE='dpsfltop.p' */
/* $LINTMODE='1' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='13/04/2016 11:05:00.693+04:00' */
/*prosignMS49Sjpdxcm+8wpO6ZJ/7A*/