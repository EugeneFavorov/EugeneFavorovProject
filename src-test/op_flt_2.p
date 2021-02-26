/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2000 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: op_nach.p
      Comment: ������� �஢���� �� ���㬥�⠬, ��࠭�� � ������� 䨫���
   Parameters:
         Uses:
      Used by:
      Created: 24/01/00 Om
     Modified: 01/02/00 Om
               ���������� ���ଠ樨 �� ��室��� ���㬥�� � ������ ���.
               �஢�ઠ �� ������ ����� �࠭���樨.
               ���४�஢�� ��᢮���� ��室�� ���㬥�⠬ ���浪����� �����
     Modified: 16/10/00 Om ���⪨� �࠭���樨, ���� ����讣� ���-�� ���㬥�⮢.
                           �㬥��� ᮤ������� ���㬥�⮢.
     Modified: 27/11/00 Om �訡��:
                                  - system error (40),
                                  - �᪮७�� ��㯯����� 㤠����� ���㬥�⮢.
     Modified: 21/12/00 Om ��ࠡ�⪠:
                                - ����⪠ ��ॢ���� 䨫��஢ � �������
                                  �����㬥����.
     Modified: 28/12/00 Om ��ࠡ�⪠: ��ࠢ����� �訡�� �������樨 ��
                                      ����䨪�樨 nach2flt.p
     Modified: 04/01/01 Om ��ࠡ�⪠: ����������� ��㯯�஢��� ��室��
                                      ���㬥��� � ������� �㬬� ��㯯�.
     Modified: 01/02/01 Om ��ࠡ�⪠: �ਢ������ ����䥩� 䨫�⮢ � ������� ����.
                           (�⠭����� ���� ���, ��������� ��㯯)
     Modified: 07/03/01 Om ��ࠡ�⪠: ����������� ᮧ������ 1/����� ���㬥�⮢.
     Modified: 29/03/01 Om �訡��   : �� �ନ஢���� ���稪 �� ������⢨�
                                      ���� ���஢��.
     Modified: 07/09/01 Om ��ࠡ�⪠: ��ନ஢���� ������ ���㬥�� �� �����
                                      ���� ���஢��.
     Modified: 30.04.2002 16:15 SEMA     �� ��� 0006062 ��堭��� 䨫��஢ � ���४������ 蠡����� �࠭���権
     Modified: 02/06/2002 Om ��ࠡ�⪠: ��������� ����� QUERY �
                                        �஢�ઠ �� ����⢮����� OP.
     Modified: 18/12/02 kraw (0012336) � �맮�� dpsfltop.p 㤠��� ��譨� ��ࠬ���
     Modified: 10/02/03 Olenka (0012527) - ��������� �맮�� ��⮤��
     Modified: 07/09/01 Om ��ࠡ�⪠: ��������� �奬� ��⮭㬥�樨 "��������".
     Modified: 01/11/04 laav 35099    ��������� ����������� ����᪠ ��᪮�쪨� �࠭���権 � ��㯯���� ०���.
     Modified: 03.07.2007 11:46 KSV      (0078824) �����஢�� ��� ���ᬠ��
     Modified: 30.11.2010 14:44 MUTA  0135739 ����������� ����������� ��ࠡ��뢠�� ��᪮�쪮 蠡����� � ࠧ�묨 䨫��ࠬ�
*/

form "~n@(#) OP_FLT.P 1.0 Om 23/09/99"
with frame sccs-id stream-io width 250.

/* �室�� ��६���� */
define input param in-op-date as date  no-undo. /* ��� ����. ��� */
define input param oprid      as recid no-undo. /* Recid op-kind */
{globals.i}
{intrface.get tmess}
{chktempl.i}               /* ���� �࠭���樨 � 蠡����� */

/*=======================================================================*/
/* ��।������ */

{g-defs.i} /* ��।������ �ᯮ����⥫��� ��६����� */
{a-defs.i}
{def_work.i   new} /* �� all_flt.p */
{flt_var.def  new} /* �� all_flt.i */
{all_note.def new} /* �� all_flt.p ������ � recid, ��࠭��� �� 䨫���� ����ᥩ Shared */
{rep_tabl.def new} /* �� all_flt.i ��।������ �������㠫��� ⠡��� ��� ���⭮�⥩ Shared */
{rid_tabl.def new} /* �६����� ⠡��� ��室��� ���㬥�⮢ ��� ��ࠡ�⪨ - ᮡ�⢥���� ��� opn_flt.p */

{tmprecid.def new} /* ��� �롮� �ॡ㥬�� ����ᥩ.*/
&IF DEFINED (oracle) &THEN
   {empty tmprecid}
&ENDIF

{def-wf.i new}
{defframe.i new}  /* ����室�� ��� parssen.p */
{g-docnum.def}    /* ��� �奬 ��⮭㬥�樨. */

/* ���� */
DEFINE BUFFER src_op       FOR op.
DEFINE BUFFER src_op-entry FOR op-entry.
DEFINE BUFFER xwop         FOR wop. /* ��� g-currv1.i */
DEFINE BUFFER bOp-template FOR op-template.

/* ������� ��६���� */
DEF VAR ope_count  AS INT64     NO-UNDO.
   DEF VAR vOpRecid   AS RECID   NO-UNDO. /* Recid ���㬥�� */
DEF VAR dval       AS DATE    NO-UNDO. /* ����室��� ��� parssen.p */
DEF VAR main-first AS LOGICAL INIT YES NO-UNDO. /* ��� g-acctv1.i */
DEF VAR fler       AS LOGICAL NO-UNDO. /* ��� ���⮢ */
DEF VAR mParentprocHdl  AS CHAR NO-UNDO. /*��� �맢��襩(த�⥫�᪮�) ��楤���*/
DEFINE  VARIABLE mRVTdRun   AS CHAR      NO-UNDO. /* ������ ��楤��� TODAY_RUN. */

/* ��� ����᪠ ��⮤�� */
DEF VAR procname AS CHAR NO-UNDO.
DEF VAR params   AS CHAR NO-UNDO.
DEF VAR ii       AS INT64  NO-UNDO.

DEF VAR f-proc-name LIKE user-config.proc-name NO-UNDO.
DEF VAR f-sub-code  LIKE user-config.sub-code  NO-UNDO.
DEF VAR f-descript  LIKE user-config.descr     NO-UNDO.

cur-op-date = in-op-date.

/*===================================================================*/
/* ������⥪� */

{intrface.get pbase}    /* ������� ������⥪� �����㬥�⮢*/
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get kau}      /* ������⥪� ��� ࠡ��� � ����⠬�. */

                        /* ���樠������ ����� ��⮪���஢����. */
RUN Init-SysMes (op-kind.op-kind, "", "").

{g-currv1.i}
{currency.def}

DEFINE VAR h_flt AS HANDLE NO-UNDO.
RUN flttool.p PERSISTENT SET h_flt (in-op-date).

/*=================================================================*/

/* ������祭�� ����� ������⥪� - ��.�� �࠭���樨 "parslib" */
{plibinit.i}

/*============================================================*/
/* ���뢠�� ����ன�� �࠭���樨 */

/* ���� ����ந��� ��ࠬ��஢ �࠭���樨 */
RUN LOAD_OP_KIND_SETTING IN h_flt (RECID (op-kind)).
IF RETURN-VALUE NE "" THEN DO:
   MESSAGE RETURN-VALUE
   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

   RUN End-SysMes.         /* ����� ����. */
   {intrface.del}          /* ���㧪� �����㬥����. */
   RETURN.
END.

/* �஢�ઠ �� ������ ����� �࠭���樨 */
RUN today_run IN h_flt (RECID(op-kind), in-op-date, Yes).
mRVTdRun = RETURN-VALUE.
IF mRVTdRun EQ "-1" THEN DO:
   {setdest2.i}
   RUN disp_err_log IN h_flt.
   {preview2.i}
END.
IF INT64(mRVTdRun) LT 0
THEN DO:
   RUN End-SysMes.         /* ����� ����. */
   {intrface.del}          /* ���㧪� �����㬥����. */
   RETURN.
END.

DebugParser = INT64(GetXattrValue("op-kind",op-kind.op-kind,"DebugParser"))
              NO-ERROR.

/*============================================================*/
/* ����ன�� � ����� ���짮��⥫� */

/*��६ � ���������� ��� �� ��楤���, �����⨢��� ��� �࠭�����*/
mParentprocHdl  = IF GetCallOpkind(2,"PROCHDL") = ?
                  THEN ""
                  ELSE GetCallOpkind(2,"PROCHDL").

RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").
RUN SetSysConf IN h_base ("gLogProc", "?").


OUTPUT TO "./open-err.log".
OUTPUT TO TERMINAL.

/* ���� ��砫쭮� ���ଠ樨 */
RUN GET_BEG_INFO IN h_flt ("1,1,1,0,0,1,1,1,1" + "," + mParentprocHdl).
IF RETURN-VALUE NE ""
THEN DO:
   RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
   RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").

   RUN End-SysMes.         /* ����� ����. */
   {intrface.del}          /* ���㧪� �����㬥����. */
   RETURN.
END.

/*
   ���� � ����� �஢�����!!!
   ����� ���㬥�� � ����� �஢����
   ��࠭����� � ����⥫쭮� ������ 楫�� �ᥫ.
   (��ઠ�쭮� ��ࠦ���� ���� �� PU ������� op-entry)
*/
{optr.i &DoBefore=YES}

&GLOBAL-DEFINE OP_HIDDEN        src_op.op * (-1)
&GLOBAL-DEFINE OFSET            500
&GLOBAL-DEFINE OPE_HIDDEN       ope_count + 1  + {&OFSET}

FOR EACH bop-template OF op-kind
          NO-LOCK,
    FIRST signs WHERE
          signs.FILE-NAME EQ "op-template"
      AND signs.surrogate EQ bop-template.op-kind + "," + STRING(bop-template.op-template)
      AND signs.code      EQ '���������'
          NO-LOCK:

  /* ���� ����ந��� ��ࠬ��஢ 蠡���� */
   RUN LOAD_OP_TMPL_SETTING IN h_flt (RECID (bop-template), '���������').
   IF RETURN-VALUE NE "" THEN DO:
      MESSAGE RETURN-VALUE
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   
      RUN End-SysMes.         /* ����� ����. */
      {intrface.del}          /* ���㧪� �����㬥����. */
      RETURN.
   END.

   /*==================================================================*/
   /* ��ࠡ�⪠ 䨫��� */
   {op_flt.qry}


   /*=================================================================*/
   /* main, 横� �� ���㬥�⠬ */
   
   /*
       ������ � recid op ᮧ���� ��� ��������� �롮� ⮫쪮
       �� ᮧ������ ���㬥�⮢, ���室��� ��� �᫮��� �롮ન.
   */

NEXT_OP:
   FOR EACH op_rid WHERE
            op_rid.op-template EQ bop-template.op-template, 
   FIRST src_op WHERE
      RECID(src_op) EQ op_rid.op_rid
   NO-LOCK
   BREAK BY op_rid.sort_fld
   TRANSACTION
   ON ERROR  UNDO NEXT_OP, NEXT  NEXT_OP
   ON ENDKEY UNDO NEXT_OP, LEAVE NEXT_OP:

       /*===================================================================*/
       /* ��ࠡ�⪠ � ��砫� ���樨  - �� �믄��� */
     
       IF all_settings.run_bef_cr > "" and
          SearchPfile(all_settings.run_bef_cr) then do:
     
          run value(all_settings.run_bef_cr + ".p") (op_rid.op_rid).
     
       end.
       /*===================================================================*/
      /* ����祭�� �㬬� ���㬥��. */
      IF FIRST-OF (op_rid.sort_fld) THEN
         ASSIGN
            send-amt   = 0 /* ����⭠� �㬬� */
            remove-amt = 0 /* �㡫���� �㬬� */
            bncr       = 0
         .
     
      /* �������� ������ �஢���� �� �।��饬� ���㬥��� */
      {wophiden.del}
     
      ope_count = 0.
      FOR EACH src_op-entry OF src_op
         NO-LOCK:
     
         /* ��ନ஢���� ���� �� ��㯯� ���㬥�⮢. */
         IF all_settings.sort_fld NE ? THEN
            RUN CREATE_SORT_GROUP IN h_flt (
               op_rid.sort_fld,
               BUFFER src_op,
               BUFFER src_op-entry).
     
         /* �������� ������ ��室��� ���㬥�⮢ */
         CREATE wop.
         {wophiden.ass
            &op_pref       = src_
            &op_entry_pref = src_
         }
     
         ope_count = ope_count + 1.
      END. /*   FOR EACH src_op-entry OF src_op */
     
      /* ����祭�� �㬬� ��㯯� ���㬥�⮢. */
      FOR EACH wop WHERE wop.op-templ GT {&OFSET}:
         ASSIGN
            send-amt   = send-amt   + wop.amt-cur /* � ����� �஢���� (Err) */
            remove-amt = remove-amt + wop.amt-rub /* � ���. ����� */
         .
      END.
     
      ASSIGN
         down_op = down_op + 1
         bncr    = bncr    + 1 /* ���-�� ��室��� ���㬥�⮢ � ��㯯� */
      .
     
      /* �������� ���㬥�� � �஢���� */
/*      IF FIRST-OF (op_rid.sort_fld) THEN DO: */
         {op_flt.i
            &op_flt=YES}
/*      END. */
     
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "��ࠡ�⠭�" down_op "/" tot_rid "��室��� ���㬥�⮢!".
     
   END.
END.
{optr.i &DoAfter=YES}


IF tot_rid EQ 0
THEN DO:
   MESSAGE "��� �� ������ �⮡࠭���� ���㬥��!"
   VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.
   HIDE MESSAGE NO-PAUSE.
   RUN End-SysMes.         /* ����� ����. */
   {intrface.del}          /* ���㧪� �����㬥����. */ 
   RETURN.
END.

/* ����� �������⥩ � ᮮ�饭�� ��᫥ ����砭�� ᮧ����� ���㬥�⮢ */

IF flager NE 9 THEN DO:
{op_flt.prt}
END.

{plibdel.i}
IF VALID-HANDLE (h_flt)
    THEN DELETE PROCEDURE h_flt.

/* ��१����뢠�� sysconf */
RUN setSysConf IN h_base ("gLogMessage", "No")  .
RUN setSysConf IN h_base ("NotCheckBlockAcctDb", "").
RUN setSysConf IN h_base ("NotCheckBlockAcctCr", "").

RUN End-SysMes.         /* ����� ����. */

&IF DEFINED (oracle) &THEN
   {empty tmprecid}
&ENDIF

{intrface.del}          /* ���㧪� �����㬥����. */
