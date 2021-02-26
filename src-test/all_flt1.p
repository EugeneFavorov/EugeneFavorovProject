/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2020 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: all_flt.p
      Comment: ������� �஢���� �� ��࠭�� ������.
   Parameters: ���, �࠭�����
         Uses:
      Used by:
      Created: 03/03/00 Om
     Modified: 27/11/00 Om ��ࠡ�⪠: 㢥��祭�� ᪮��� ��㯯����� 㤠�����
                                      ���㬥�⮢.
     Modified: 21/12/00 Om ��ࠡ�⪠: ��।������ ��६�����,
                                      ��ꥤ������ �����㬥����.
     Modified: 28/12/00 Om ��ࠡ�⪠: ��ࠢ����� �訡�� �������樨, � �裡
                                      � �ࠢ��� nach2flt.p
     Modified: 27/11/01 Om ��ࠡ�⪠: ��ࠡ�⪠ �������� ����.
     Modified: 16/05/02    ��ࠡ�⪠: ���������� ����᪠ ��⮤�� befor & after transaction.

     Modified: 28/06/02 ����:  �������� ��⠢�� ��� ������
                        (��������� ����������� ������ ��楤��� ��ࠡ�⪨ �⬥祭��� ����ᥩ,
                         ��蠥��� �� ���. ४����� aft-fill-prog �࠭���樨 )
     Modified: 05/07/02 ����: ��������� �� g-func.p
     Modified: 03/09/02 ����: ����������� �뢮�� ��⮪���� �뭥� � ���. ४������ �࠭���樨,
                               �뢮� �᭮����� ��⮪��� ��� ४����� print-rep = yes  ��� ��
                               㪠��� ��� �࠭���樨, ����������� �뢮�� "᢮��" ��⮪����
                               ����� �㤥� ��ࠡ��뢠���� � "᢮��" ��楤�� �뢮��, �.� � ���
                               ����� ������ ������� �������⥫�� ����⢨�. ���. ४����� ����������
                               �뢮�� ᢮�� ᮮ�饭�� print-self = "��" ��� �� 㪠���
                               ������� �஢��� �� ����稥 ��楤�� 㪠������ � ���. ४������
     Modified: 27/1/2006 mitr tt 0057197 - �������� �맮� RUN NoGarbageCollect IN h_base.
     Modified: 12/7/2006 gorm  (0064663) - �࠭ �맮� RUN NoGarbageCollect IN h_base - �ਢ���� � �訡���.
     Modified: 13/9/2006 laav  (0044028) - �������� ᡮઠ ��⥬��� ᮮ�饭�� � ⥬�-⠡����, �� ��������
                                           �뢮���� �� � �⤥��� ��⮪��
.
*/

form "~n@(#) all_flt.p 1.0 Om 03/03/00"
with frame sccs-id stream-io width 250.

&GLOB temp-define YES

/* �室�� ��ࠬ���� */
DEF INPUT PARAM in-op-date AS DATE  NO-UNDO. /* ��� ����. ��� */
DEF INPUT PARAM oprid      AS RECID NO-UNDO. /* Recid op-kind */

{globals.i}
{intrface.get tmess}
{intrface.get pbase}
{intrface.get loan}
{intrface.get prsfn}
{intrface.get aclog}
{over-def.def new}

RUN NoGarbageCollect IN h_base.

DEF VAR h_flt         AS HANDLE NO-UNDO.
DEF VAR hgar          AS HANDLE NO-UNDO.
DEF VAR nprprog       AS CHAR   NO-UNDO.
DEF VAR l-PrevOpKind  AS CHAR   NO-UNDO.
DEF VAR after         AS CHAR   NO-UNDO.
DEF VAR mAft-fill-prog AS CHAR  NO-UNDO.
DEF VAR mMake-prog    AS CHAR   NO-UNDO.
DEF VAR print-rep     AS LOG    NO-UNDO.
DEF VAR mRVTdRun      AS CHAR   NO-UNDO. /* ������ ��楤��� TODAY_RUN. */
DEF VAR quest         AS CHAR   NO-UNDO INIT "false,01/01/1900,false".
DEF VAR mSysMessCntr  AS INT64  NO-UNDO.
DEF VAR mProcname     AS CHAR   NO-UNDO.
DEF VAR mParams       AS CHAR   NO-UNDO.
DEF VAR vEmptyOk      AS LOG    NO-UNDO.
DEF VAR vEmptyOk2     AS LOG    NO-UNDO.
DEF VAR vEmptyOk3     AS LOG    NO-UNDO.
DEF VAR mLogFileName  AS CHAR   NO-UNDO. /* ��� ���-䠩�� */
DEF VAR mViewAcctLog  AS LOG    NO-UNDO.
DEF VAR mLdProg       AS CHAR   NO-UNDO.
def var mErr          AS INT64  NO-UNDO.
DEF VAR mAutotest     AS CHAR   NO-UNDO. /* SysConf "AUTOTEST:autotest" �� �室� � ��楤��� */

DEF NEW SHARED TEMP-TABLE over NO-UNDO
 FIELD acct           LIKE acct.acct
 FIELD cur            LIKE acct.currency
 FIELD cont-code-agr  LIKE loan.cont-code
 FIELD agr-class-code LIKE loan.class-code
 FIELD cont-code-per  AS CHAR
 FIELD limit          AS DECIMAL
 FIELD bal            AS DECIMAL
 FIELD overtr         AS LOG INIT NO
 .

DEFINE TEMP-TABLE bSysMes  NO-UNDO LIKE tt-SysMes.
DEFINE TEMP-TABLE bProcMes NO-UNDO LIKE tt-ProcMes.

/* ��孨�᪨� �������⥫�� ��६���� */
{g-defs.i}
{def_work.i   new} /* ��।������ ⠡���� ��� ���� ��業⮢ */
{flt_var.def  new} /* ��६����, ��騥 ��� ��� ⨯�� 䨫��஢ Shared */
{aux_var.def  new} /* �������������� ��६����, ࠧ������� ��������ﬨ */
{all_note.def new} /* ������ � recid, ��࠭��� �� 䨫���� ����ᥩ Shared */
{rep_tabl.def new} /* ��।������ �������㠫��� ⠡��� ��� ���⭮�⥩ Shared */
{svarloan.def new}
{def-wf.i new}
/* -------------------------------- ACTIONS -------------------------------- */

{chktempl.i}    /* ���� �࠭���樨 � 蠡����� ERROR return */

RUN flttool.p PERSISTENT SET h_flt (in-op-date).
RUN "ga-pfunc.p" PERSISTENT SET hgar.

EMPTY TEMP-TABLE bSysMes.
RUN Get-BufMes(OUTPUT TABLE bProcMes, OUTPUT TABLE bSysMes).

mSysMessCntr = 0.
FOR EACH bSysMes:
   IF bSysMes.Mes-Num > mSysMessCntr
      THEN mSysMessCntr = bSysMes.Mes-Num.
END.

RUN DelLogTbl IN h_aclog.

MainBlock:
DO:
    /* ���樠������ ������⥪� PBASE
    ** ����� ���冷� ����� (᭠砫� initBaseLibrary ��⥬ Init-SysMes), �.�.
    ** � Init-SysMes ����� �ᯮ�짮������ �����. */
    RUN InitBaseLibrary IN h_pbase (op-kind.op-kind,in-op-date,THIS-PROCEDURE).

    /* ���樠������ ��⮪��� �࠭���樨 */
    RUN Init-SysMes IN h_tmess (op-kind.op-kind,"","").

    /* ������� ��� �⪠�� input ��ࠬ��஢ ��楤�� ��।����� !!! */
    ASSIGN
        op_rid     = oprid
        in_op_date = in-op-date
    .

    /* ��।������ ⨯� 䨫��� */

    IF NOT AvailXattr("op-kind",op-kind.op-kind,"flt_type" ) THEN
    DO:
        RUN Fill-SysMes IN h_tmess ("","","-1","��易⥫�� ४����� flt_type � �࠭���樨 �� ��⠭�����.").
        LEAVE MainBlock.
    END.

    ASSIGN
        all_settings.flt_type = LC(GetXattrValue("op-kind",op-kind.op-kind,"flt_type" ))
        svPlanDate            = in-op-date
        all_settings.user-res      =  "not-do-op"
        /* �믮������ ��⮤� before ��। �믮������� �࠭���樨 */
        l-PrevOpKind  = GetXattrValueEx("op-kind", op-kind.op-kind, "PrevOpKind", ?)
        /*��楤�� ����� ��室��� ���ଠ樨 ��� ����窠 ��� ���*/
        nprprog       = GetXattrValueEx("op-kind", op-kind.op-kind, "nprprog", ?)
        /* ��楤�� ����祭�� ������஢ */
        mLdProg       = GetXattrValueEx("op-kind", op-kind.op-kind, "LdProg", ?)
        /*��楤��, ��뢠���� ��᫥ ���������� recid ��࠭��� ����ᥩ*/
        mAft-fill-prog = GetXattrValueEx("op-kind", op-kind.op-kind, "Aft-fill-prog", ?)
        /*᢮� ��楤�� ᮧ����� ���㬥��*/
        mMake-prog     = GetXattrValueEx("op-kind", op-kind.op-kind, "makeprog", ?)
        /*������ �� ��⮪�� ( �⠭����� ᮮ�饭�� ��楤��� all_flt.p ) ?*/
        print-rep     = GetXattrValueEx("op-kind", op-kind.op-kind, "print-rep", "��") = "��"
        DebugParser   = INT64(GetXattrValueEx("op-kind",op-kind.op-kind,"debugparser","0"))
        mLogFileName  = "opacct-" + TRIM(op-kind.op-kind) + ".log"
        mViewAcctLog  = LOGICAL(FGetSetting("ViewAcctLog", ?, "NO"))
        all_settings.ZeroTestLog = (IF GetXAttrValueEx("op-kind",
                                                       op-kind.op-kind,
                                                       "ZeroTestLog",
                                                       "��") EQ "��"
                                    THEN YES
                                    ELSE NO)
        NO-ERROR
        .
    mAutotest = GetSysConf("AUTOTEST:autotest").
    IF GetXattrValueEx("op-kind", op-kind.op-kind, "��_�뢮���⪫", "") EQ "���" THEN
    DO:
       IF mAutotest NE "YES" THEN
          RUN SetSysConf IN h_base ("AUTOTEST:autotest", "YES").
    END.

    IF nprprog NE ? THEN
    DO:
      IF SEARCH(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                 THEN 255
                                 ELSE INDEX(nprprog, "((") - 1) + ".p") <> ?
      OR SEARCH(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                 THEN 255
                                 ELSE INDEX(nprprog, "((") - 1) + ".r") <> ?
      THEN
      DO:
         RUN VALUE(SUBSTR(nprprog, 1, IF INDEX(nprprog, "((") EQ 0
                                  THEN 255
                                  ELSE INDEX(nprprog, "((") - 1))
               (SUBSTR(nprprog, (IF INDEX(nprprog, "((") EQ 0
                                 THEN ?
                                 ELSE INDEX(nprprog, "((") + 2))) .
         IF KEYFUNCTION(LASTKEY) EQ "END-ERROR"
         THEN
            LEAVE MainBlock.
      END.
      ELSE DO:
         RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ���. ४����� before �࠭���樨.").
         LEAVE MainBlock.
      END.
    end.
    {befexpr.i &befopkind = op-kind.op-kind}

    /* ��⮤ ����� ��室��� ���ଠ樨 */
    /* �६����� ��ࠡ�⪠  ��� ��८�।������ ��⮤� ����� ��室��� ���ଠ樨 - �᫨ �����  ���. ४����� - nprprog - �� ���ᯥ稢��� ���� ��室��� ���ଠ樨
      � �������� ������ �� ��-� , �᫨ �� �����, � �⠭���⭠� ࠡ�� */
    IF nprprog  = ? THEN
    DO:
       {flt_file.run
           &flt_type="all_settings.flt_type"
           &action_type="frm1"
       }
       svPlanDate            = in-op-date.
      
      IF method_error THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ����� ��砫쭮� ���ଠ樨.").
         LEAVE MainBlock.
      END.
      ELSE
         IF RETURN-VALUE EQ "-1" THEN
            LEAVE MainBlock.
    END.
    /* �஢�ઠ �� ������ ����� �࠭���樨 */
    IF NOT {assigned mLdProg} THEN
    DO:
       RUN today_run IN h_flt (RECID(op-kind), in-op-date, YES).
       mRVTdRun = RETURN-VALUE.
       IF mRVTdRun EQ "-1" THEN
       DO:
          {setdest2.i}
          RUN disp_err_log IN h_flt.
          {preview2.i}
       END.
       IF INT64(mRVTdRun) LT 0 THEN
          LEAVE MainBlock.
    END.

    IF {assigned mLdProg} THEN
    DO:
       IF    SEARCH(mLdProg + ".p") <> ? 
          OR SEARCH(mLdProg + ".r") <> ? THEN
       DO:
          RUN VALUE(mLdProg + ".p").
          IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
             LEAVE MainBlock. 
       END.
       ELSE DO:
           RUN Fill-SysMes IN h_tmess ("","","0",
                                       "�� ������� ��楤�� ���. ४����� LdProc �࠭���樨.").
           LEAVE MainBlock.
       END.    
       RUN RunOp_plus IN THIS-PROCEDURE (OUTPUT mErr).
       IF mErr EQ 1 THEN
          LEAVE MainBlock.    
    END.
    ELSE
    DO:

       /* ����㧪� ����஥� 䨫��஢ */
       RUN LoadFltTemplate IN h_flt (op-kind.op-kind,
                                     INPUT-OUTPUT TABLE flt-template,
                                     INPUT-OUTPUT TABLE flt-setup).
       IF NOT CAN-FIND(FIRST flt-setup) THEN
           LEAVE MainBlock.
       FOR EACH flt-setup NO-LOCK BY flt-setup.flt-num:
           ASSIGN
              mFltNum                 = flt-setup.flt-num
              all_settings.flt_user   = flt-setup.user-id
              all_settings.flt_proc   = flt-setup.proc-name
              all_settings.flt_module = flt-setup.sub-code
              all_settings.flt_name   = flt-setup.descr
              all_settings.user-res   =  "not-do-op"
           .

           IF mFltNum > 1 AND all_settings.flt_type = "loan" THEN
              RUN SetSysConf IN h_base ("multiple_loan_filters", "YES").

           /* ��楤�� ���������� recid ��࠭��� ����ᥩ */
           {flt_file.run
               &flt_type="all_settings.flt_type"
               &action_type="ld"
           }
           IF method_error THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ���������� �롮ન.").
              LEAVE MainBlock.
           END.
           ELSE IF RETURN-VALUE EQ "-1" THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ����ன�� 䨫���.").
              LEAVE MainBlock.
           END.
 
           RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
           RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk2).
           RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk3).
           IF NOT vEmptyOk  OR
              NOT vEmptyOk2 OR
              NOT vEmptyOk3 THEN
           DO:
              RUN Fill-SysMes IN h_tmess ("","","0","�ந��諠 �訡�� ��⪨ ⠡���� ���஢���� ������ �� �믮������ ������� �㭪権.").
              LEAVE MainBlock.
           END.

           RUN RunOp_plus IN THIS-PROCEDURE (OUTPUT mErr).
           IF mErr EQ 1 THEN
              LEAVE MainBlock.

       END. /* FOR EACH flt-setup */
    END. /* mLdProg */ 
    RUN DeleteOldDataProtocol IN h_base ("multiple_loan_filters").

    /*���⠥� ��⮪�� �᫨ ��⠭����� ���. ४�����*/

    EMPTY TEMP-TABLE bSysMes.

    RUN Get-BufMes(OUTPUT TABLE bProcMes, OUTPUT TABLE bSysMes).

    FOR EACH bSysMes WHERE
             bSysMes.mes-num GT mSysMessCntr:

          CREATE loan_qst.
          ASSIGN
          loan_qst.txt     = REPLACE(bSysMes.mes-text,"~n"," ")
          .

       END.

    IF print-rep THEN DO:
      /* ��楤�� ���� */
      {flt_file.run
        &flt_type="all_settings.flt_type"
        &action_type="prn"
      }
    END.

    IF method_error THEN
    DO:
       RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ���� �������⥩.").
       LEAVE MainBlock.
    END.

    /* �������� ᢮� ��楤��� ����
    ** ����� ��楤��� ��᫥ �믮������ �࠭���樨 */
    after = op-kind.after.
    IF {assigned after} THEN
      IF SEARCH(after + '.p') <> ? OR SEARCH(after + '.r') <> ?
      THEN
         RUN VALUE(after + '.p')(op-kind.op-kind) .
      ELSE
      DO:
         RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ���� 㪠������ � ���. ४����� after �࠭���樨.").
         LEAVE MainBlock.
      END.
    RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk).
    RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
    RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk2).

    IF mViewAcctLog THEN /* �뢥��� ��� */
       RUN PrintLogFile IN h_aclog (mLogFileName).
END.

FINALLY:
   /* ��⨬ ��� ᮧ����� ��⮢ */
   RUN DelLogTbl IN h_aclog.
   /* ����뢠�� ���⥪�� �࠭���樨 � PBASE */
   RUN InitBaseLibrary IN h_pbase (?,?,?).
   /* ����뢠�� ��⮪���஢���� */
   RUN End-SysMes IN h_tmess.
   /* �����頥� ����ன�� ���⭮ */
   RUN SetSysConf IN h_base ("AUTOTEST:autotest", mAutotest).
 
   PUBLISH 'done'.

   IF VALID-HANDLE (h_flt) THEN
      DELETE PROCEDURE h_flt.
   IF VALID-HANDLE(hgar) THEN
      DELETE PROCEDURE hgar.

   RUN GarbageCollect2 IN h_base.
   {intrface.del}
END FINALLY.

PROCEDURE RunOp_plus:
   DEF OUTPUT PARAM oErr AS INT64 NO-UNDO.
   main:
   do:
       oErr = 1.
       /*����� ��楤��� ��ࠡ�⪨ ��࠭��� ����ᥩ*/
       IF mAft-fill-prog <> ? THEN 
       DO:
          IF INDEX(mAft-fill-prog, "((") > 0 THEN
             ASSIGN
                 mProcname = SUBSTR(mAft-fill-prog, 1, INDEX(mAft-fill-prog, "((") - 1)
                 mParams   = SUBSTR(mAft-fill-prog, INDEX(mAft-fill-prog, "((") + 2)
            .
          ELSE
             ASSIGN
                mProcname = mAft-fill-prog
                mParams   = ""
                 .
      
          IF SearchPfile(mProcname) THEN 
          DO: /* ������� ⠪�� ��楤�� */
             ASSIGN
                mParams = STRING(mParams + "," + string(oprid)).
             RUN run_Params in h_xclass (mProcname,mParams,".p",?).
             IF KEYFUNCTION(LASTKEY) EQ "END-ERROR" THEN
                LEAVE Main.
          END.
          ELSE
          DO:
             RUN Fill-SysMes IN h_tmess 
                 ("","","0","�� ������� ��楤�� ���. ४����� mAft-fill-prog �࠭���樨.").
             LEAVE Main.
          END.
          RUN EmptyLPResult IN THIS-PROCEDURE (OUTPUT vEmptyOk).
          RUN EmptyLPResult IN h_loan  (OUTPUT vEmptyOk).
          RUN EmptyLPResult IN h_prsfn (OUTPUT vEmptyOk).
       END.

       IF mMake-prog = ? THEN 
       DO:
          {flt_file.run
             &flt_type="all_settings.flt_type"
             &action_type="op"
          } 
       END.
       /*����᪠�� ᢮� ��楤��� ᮧ����� ���㬥�⮢*/
       ELSE IF SEARCH(mMake-prog + ".p") <> ? 
            OR SEARCH(mMake-prog + ".r") <> ? THEN
          RUN VALUE(mMake-prog + '.p').
       ELSE
          method_error = YES.
    
       IF method_error THEN
       DO:
          RUN Fill-SysMes IN h_tmess ("","","0","�� ������� ��楤�� ᮧ����� ���㬥��.").
          LEAVE Main.
       END.
       oErr = 0. 
   end.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='29/02/2016 12:21:13.338+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='all_flt.p' */
/*prosign0fOGS2ShFJkyAWTgiiUXQQ*/