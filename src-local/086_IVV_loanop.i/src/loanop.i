/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: LOANOP.I
      Comment:
   Parameters:
         Uses:
      Used by:
      Created:
     Modified:
     Modified:
     Modified: 18.12.2002 17:48 SEMA     �� ��� 0012531 ������� �맮� �����㬥�� parssign
     Modified: 30/07/2004  Om �������� ���㫨஢������ ���㬥��.
*/

form "~n@(#) loanop.p 1.0 Om 06/03/00 Om 26/04/00"
with frame sccs-id stream-io width 250.

&GLOB  RoleOver  "for g-currv1.i"
&GLOB  BYrole    "for g-currv1.i"
&GLOB  New-Check "��। Check-Op-Entry"

DEF VAR in-op-date AS DATE NO-UNDO.

{g-docnum.def}     /* ��� �奬 ��⮭㬥�樨. */
{flt_var.def}      /* ��।������ Shared ��६����� */
{all_note.def}     /* ��।������ ⠡���� � �롮મ� �� loan */
{aux_var.def}      /* ��।������ ⠡��� ���⮢, �������⥩ */
{def-wf.i new}     /* ��।������ wop */
{g-defs.i}         /* ��।������ �ᯮ����⥫��� ��६����� */
{defframe.i new}   /* ����室�� ��� parssen.p */
{intrface.get instrum}      /* �����㬥��� ��ॢ��� �㬬 �� ����� ������ � �����*/
{intrface.get ovl} /* ����䥩� ࠡ��� � ������⮬*/
{intrface.get xclass}
{intrface.get terr}

def var main-first  as logical init Yes no-undo. /* ��� g-acctv1.i */
def var dval        as date    no-undo. /* ����室��� ��� parssen.p */
def var total_value as INT64     no-undo. /* ��� progress bar */
def var fler        as logical no-undo. /* ��� ���⮢ */

def var in-contract  like loan.contract  no-undo. /* ��� g-acctv1.i */
def var in-cont-code like loan.cont-code no-undo. /* ��� g-acctv1.i */

DEF VAR mChkTemplate      AS CHAR   NO-UNDO.    /*��楤�� �஢ન 蠡����� ��
                                              ���४⭮���*/
DEF VAR mErrorTempl       AS CHAR   NO-UNDO.    /*ᯨ᮪ �ਢ�� 蠡�����*/
DEF VAR mAutoRecalc       AS LOG    NO-UNDO.    /* ��⮬���᪨� ������ ������஢ */
DEF VAR mAutoRecalcClose  AS LOG    NO-UNDO.    /* �ய�᪠�� ������� ������� */
DEF VAR proc-name         AS CHAR   NO-UNDO.
DEF VAR mParams           AS CHAR   NO-UNDO.
DEF VAR vOverEr           AS LOG INIT NO NO-UNDO.
DEF VAR mMethodAfter      AS CHAR   NO-UNDO.

DEF VAR vAutoNum          AS LOG  NO-UNDO. /* �ᯮ�짮���� ��⮭㬥��� ���㬥�⮢ - ���祭��
                                              �� DocNumSch NE "2" */
DEF VAR result            AS INT64  NO-UNDO. /* ��� bef-tran.i - ��楤��� ��蠡������ ��ࠡ�⪨ */
                          
DEF VAR mLibHandle        AS HANDLE NO-UNDO.    /*㪠��⥫� �� ������⥪� �
                                                  ��楤�ࠬ� ��⠭���� ���祭�� �
                                                  sysconf ��� ��ࠡ�⪨ ��楤�ன
                                                  �����஢���� */
DEF VAR mLibProc          AS CHAR   NO-UNDO.    /* �� ᠬ� ������⥪�*/
DEF VAR vTrList           AS CHAR   NO-UNDO. /* ���᮪ ����ᮢ ������⮢ � �࠭蠬� */
DEF VAR vMList            AS CHAR   NO-UNDO. /* ���᮪ ����ᮢ �࠭襩 ������⮢ */


/*��६���뭥 ��� ��࠭���� ��ࠬ��஢ �஢���� �� op-entry.upd, �.�
⠬ ����� ��-����� ����������, ���ਬ��, ���⠢��� �� ����⥪�*/
DEF VAR mOldOp       LIKE op-entry.op       NO-UNDO. /*���ண�� ���㬥��*/
DEF VAR mOldOpEntry  LIKE op-entry.op-entry NO-UNDO. /*����� �஢����*/
DEF VAR mOldSumRub   LIKE op-entry.amt-rub  NO-UNDO. /*�㬬� �஢���� ��.*/
DEF VAR mOldSumCur   LIKE op-entry.amt-cur  NO-UNDO. /*�㬬� �஢���� ���.*/
DEF VAR mOldCurrency LIKE op-entry.currency NO-UNDO. /*����� �஢����*/
DEF VAR mMjbank AS LOG NO-UNDO.
DEF VAR vRef AS CHAR NO-UNDO .
DEF VAR vEmptyOk      AS LOG  NO-UNDO.
DEFINE VARIABLE mNumFilters LIKE flt-setup.flt-num INITIAL 0               NO-UNDO.
DEFINE VARIABLE mBarMessage AS   CHARACTER         INITIAL "���㭤���..." NO-UNDO.
DEF VAR mRunAfterCalc  AS CHAR NO-UNDO.
DEF VAR mIsOk AS INT64 NO-UNDO.

def var ph                   as handle no-undo.
def var parssen-library-proc as handle no-undo.

def buffer xwop for wop. /* ��� g-currv1.i */
DEF BUFFER bLoan FOR loan.

DEF NEW SHARED STREAM err.

DEF TEMP-TABLE DelRec NO-UNDO
    FIELD recid1 AS RECID
    INDEX recid1 IS UNIQUE recid1
    .
/* ��� ��࠭���� ��ࠬ��஢ �।��饣� 蠡���� �� �㫥��� �㬬� �஢���� */
define temp-table save_wop no-undo like wop.

DEFINE VARIABLE mTerrCheck AS LOGICAL     NO-UNDO. /* �஢����� �� ���ਧ�? */
/* �� ����� � �� LegTerr
   NO  = ����� "NO"
   YES = ��祣� �� ����� */
DEFINE VARIABLE mLegTerr   AS LOGICAL     NO-UNDO INITIAL YES.
DEFINE VARIABLE mCustName  AS CHARACTER   NO-UNDO EXTENT 3.
/* ��� ��࠭���� 㦥 �஢�७��� �� ���ਧ� �����⮢ */
DEFINE TEMP-TABLE tt-terrchk NO-UNDO
   FIELD cust-cat AS CHARACTER
   FIELD cust-id  AS INT64
   FIELD terr     AS LOGICAL
   INDEX cust-cat-id IS UNIQUE PRIMARY cust-cat cust-id
.

{g-currv1.i &BYrole = "Yes" }    /* ���� ������  �ᯮ���� xwop */
{currency.def}                  /* �஢���� */
{details.def}                   /* ࠧ��� ���� "ᮤ�ঠ���"*/

ASSIGN
    in-op-date    = in_op_date
    cur-op-date   = in-op-date
    vContractDate = svPlanDate  /* ��� ���४�஢�� �������� ���� ���㬥�� */
.

/*���樠�����㥬 ����*/
SetOpDate(vContractDate).

FIND FIRST op-kind WHERE RECID(op-kind) EQ op_rid NO-LOCK NO-ERROR.
IF NOT AVAIL op-kind THEN DO:
    {intrface.del}
    RETURN "-1".
END.

/*
  �᫨ �� ������ �ᮡ�� ᮮ⢥��⢨� 䨫��஢ � 蠡�����, ᮧ����
  �⠭���⭮�: ���� ����ன�� 䨫���, �ᯮ��㥬�� ��� ��� 蠡�����
*/
IF mFltNum = ? OR mFltNum = 0 THEN DO:
    mFltNum = 1.
    {empty flt-setup}
    CREATE flt-setup.
    ASSIGN
        flt-setup.flt-num   = mFltNum
        flt-setup.user-id   = all_settings.flt_user
        flt-setup.proc-name = all_settings.flt_proc
        flt-setup.sub-code  = all_settings.flt_module
        flt-setup.descr     = all_settings.flt_name
    .
    {empty flt-template}
    FOR EACH op-template OF op-kind NO-LOCK:
        CREATE flt-template.
        ASSIGN
            flt-template.flt-num     = mFltNum
            flt-template.op-kind     = op-template.op-kind
            flt-template.op-template = op-template.op-template
        .
    END.
END.

ASSIGN
  DebugParser = INT64(GetXattrValueEx("op-kind", op-kind.op-kind, "DebugParser", "0"))
  mLibProc    = GetXattrValue("op-kind",op-kind.op-kind,"parslib")
  .

/* �᫨ 㪠���� ��楤�� ��⠭���� ��ࠬ��஢ ��ࠡ�⪨ �訡��,
   � ��⠥��� �������� �� */
IF mLibProc <> "" THEN
DO:
   IF SearchPFile(mLibProc) THEN
   DO:
      RUN VALUE(mLibProc + '.p') PERSISTENT SET mLibHandle .
   END.
   ELSE mLibProc = ?.
END.


FIND LAST all_recids NO-ERROR.

IF NOT AVAIL all_recids THEN
DO:
   {intrface.del}
   RETURN "-2".
END.
ELSE
    total_value = all_recids.count.

RUN "a-yes.p"   PERSISTENT SET ph.                   /*�����誠*/
RUN "g-lolib.p" PERSISTENT SET parssen-library-proc. /*�������� ������ �-樨*/

ASSIGN
   mChkTemplate = GetXattrValueEx("op-kind", op-kind.op-kind, "ChkTempl", ?)
   /* ������뢠�� ��⮬���᪨ ������� �� ���� ����.��� */
   mAutoRecalc      = GetXattrValueEx("op-kind",op-kind.op-kind,"auto-recalc"      , ?) = "��"
   mAutoRecalcClose = GetXattrValueEx("op-kind",op-kind.op-kind,"auto-recalc-close", ?) = "��"
   mTerrCheck       = GetXattrValueEx("op-kind",op-kind.op-kind,"loan-terrcheck"  , ?) EQ "��"
   .

IF mChkTemplate <> ? THEN
DO:
   IF NOT SearchPFile(mChkTemplate) THEN
   DO:
      mChkTemplate = ?.
      MESSAGE "�訡�� � ��।������ ��楤��� �஢�ન 蠡�����!" SKIP
             "�஢�ઠ �ந��������� �� �㤥�."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

vAutoNum = GetXAttrValueEx ("op-kind", op-kind.op-kind, "DocNumSch", "") NE "2".

FOR EACH flt-setup NO-LOCK BY flt-setup.flt-num DESCENDING:
    mNumFilters = flt-setup.flt-num.
    LEAVE.
END.
IF mNumFilters > 1 THEN
    mBarMessage = "������ " + STRING(mFltNum) + "/" + STRING(mNumFilters) + ". " +
                  mBarMessage.

{init-bar.i """ + mBarMessage + """}

{setdest.i &stream = "stream err"}

   /* ���஢���� ������� �㭪権 */
IF GetXAttrValueEx ("op-kind", op-kind.op-kind, "������እ�", "") EQ "��" THEN
   RUN FlgLnCache IN h_oldpr (TRUE).

NEXT_LOAN:
FOR EACH all_recids  TRANSACTION ON ERROR UNDO, NEXT ON ENDKEY UNDO, LEAVE:
   FIND loan WHERE RECID(loan) = all_recids.rid NO-LOCK NO-ERROR .
   IF NOT AVAIL loan
   THEN NEXT NEXT_LOAN .
       /* ���⪠ ��� ��� ���� ������� */
   RUN EmptyLnCache IN h_oldpr.

   vRef = loan.doc-ref .
   ASSIGN
       vOverEr = NO.

   &IF DEFINED(over) <> 0
   &THEN
      /*���� &GLOB ��।���� � loanop1.p - ��� ��㯯����� ����襭��
        ���������  ������஢ ����室��� ����᪠�� �� �� 蠡���� �࠭���樨
        (� ����ᨬ��� �� ����� ������� ) */
      FIND FIRST overdr-debt WHERE
                 overdr-debt.rid1 = RECID(all_recids) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE overdr-debt THEN NEXT.
      chpar1 = STRING(overdr-debt.rpmt-amt).
   &ENDIF

   IF GetXAttrValueEx("op-kind",op-kind.op-kind,"�࠭����","") NE "" THEN
      RUN SetSysConf IN h_base ("�࠭������",GetXAttrValueEx("op-kind",op-kind.op-kind,"�࠭����","")).

   /* ������ ������஢ */
   FOR EACH bLoan WHERE      bLoan.contract  EQ loan.contract                 /* ��� �࠭�. � �墠�.������஢ ������뢠�� ��� */
                         AND bLoan.cont-code BEGINS ENTRY(1, loan.cont-code, " ")
                        NO-LOCK
                        BY NUM-ENTRIES(bLoan.cont-code, " ") DESCENDING: 

      IF              bLoan.cont-code  NE loan.cont-code
         AND (        bLoan.open-date  GT svPlanDate
              OR (    bLoan.close-date NE ?
                  AND (    mAutoRecalcClose
                       OR  bLoan.close-date LE svPlanDate)
                 )
             )
      THEN
         NEXT. 

      IF bLoan.since <> svPlanDate THEN
      DO:
         IF mAutoRecalc THEN
         DO:

            /* ����稬 ��楤���, ॠ�������� ������ (��⮤ Calc) */
            {get_meth.i 'Calc' 'loanclc'}

            RUN VALUE(proc-name + ".p") (bLoan.contract,bLoan.cont-code,svPlanDate).

            IF bLoan.since <> svPlanDate THEN
            DO:
               CREATE loan_err.
               ASSIGN
                  loan_err.cont-code = vRef
                  loan_err.template  = ?
                  loan_err.error     = "�訡�� �� ������ ������� " + bLoan.doc-ref +
                                       " ������� �� �����⠭ �� �������� ���� ����樨."
                  .
               IF all_recids.overtr EQ YES THEN DO: /* �᫨ �࠭� ᮧ��� �⮩ �࠭���樥�,
                                                       � 㡨���� ���, �.�. �� ࠢ�� �����
                                                       ������� �ய�᪠���� */
                  CREATE DelRec.
                  ASSIGN
                     DelRec.recid1 = RECID(loan).
               END.
               NEXT NEXT_LOAN.
            END.
         END. /* IF mAutoRecalc */
         ELSE DO:
            /* �� ������뢠�� */
            CREATE loan_err.
            ASSIGN
               loan_err.cont-code = vRef
               loan_err.template  = ?
               loan_err.error     = "������� " + bLoan.doc-ref +
                                    " �� �����⠭ �� �������� ���� ����樨."
               .
            IF all_recids.overtr EQ YES THEN DO: /* �᫨ �࠭� ᮧ��� �⮩ �࠭���樥�,
                                                    � 㡨���� ���, �.�. �� ࠢ�� �����
                                                    ������� �ய�᪠���� */
               CREATE DelRec.
               ASSIGN
                  DelRec.recid1 = RECID(loan).
            END.
            NEXT NEXT_LOAN.
         END.
      END.
   END. /* FOR EACH bLoan... */
   ASSIGN
      in-contract     = loan.contract
      in-cont-code    = loan.cont-code
      ph:PRIVATE-DATA = loan.contract + "," + loan.cont-code + ",loan_flt"
      .

   /*����室��� ��� ������� (��祬� ��㣮�� �� ���।��*/
   set_loan(loan.contract,loan.cont-code).

   FOR EACH save_wop:
     DELETE save_wop.
   END.
   FOR EACH wop:
     DELETE wop.
   END.

   /* �஢�ઠ ������ ������� �� ���ਧ� */
   IF mTerrCheck THEN
   DO:
      FIND FIRST tt-terrchk WHERE tt-terrchk.cust-cat EQ loan.cust-cat
                              AND tt-terrchk.cust-id  EQ loan.cust-id
      NO-LOCK NO-ERROR.
      IF AVAILABLE tt-terrchk THEN
      DO:
         /* ��� ������ 㦥 �஢�७ */
         mLegTerr = tt-terrchk.terr.
      END.
      ELSE
      DO:
         RUN GetCustName IN h_base (loan.cust-cat, loan.cust-id, ?,
                                    OUTPUT mCustName[1],
                                    OUTPUT mCustName[2],
                                    INPUT-OUTPUT mCustName[3]).
         CREATE tt-terrchk.
         ASSIGN
            tt-terrchk.cust-cat = loan.cust-cat
            tt-terrchk.cust-id  = loan.cust-id
            tt-terrchk.terr     = CompareName(mCustName[2], "plat")
            mLegTerr            = tt-terrchk.terr
         .
      END.
   END.

   GEN:
   FOR EACH flt-template WHERE
       flt-template.flt-num = mFltNum
   NO-LOCK,
   FIRST op-template WHERE
       op-template.op-kind     = flt-template.op-kind AND
       op-template.op-template = flt-template.op-template
   NO-LOCK
      ON ERROR  UNDO gen, NEXT gen
      ON ENDKEY UNDO gen, LEAVE gen:

         /* �맮� ��楤��� ��蠡������ ��ࠡ�⪨ */
      {bef-tran.i &BEF-UNDO = "UNDO gen, NEXT gen. " }

      /* ����⠭�������� �।��騥 wop'� */
      IF CAN-FIND (FIRST save_wop)
      THEN DO:
         FOR EACH save_wop:
            CREATE wop.
            BUFFER-COPY save_wop to wop.
         END.
      END.

      /*�᫨ ���� ��楤�� �஢�ન � ��뢠��*/
      IF mChkTemplate <> ? THEN
      DO:
         RUN VALUE(mChkTemplate + '.p') (RECID(loan),
                                         RECID(op-template),
                                         OUTPUT mErrorTempl) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN mErrorTempl = "".

      END.

      &IF DEFINED(over) <> 0
      &THEN
         /* ����砥� ᯨ᮪ ����ᮢ �墠�뢠��� ������஢ � ����ᮢ �࠭襩 */
         vMList = FGetSetting("���������࠭�","���墠�࠭�","").
         IF NUM-ENTRIES(vMList,"|") GT 1 THEN
            ASSIGN
               vTrList = ENTRY(2,vMList,"|")
               vMList  = ENTRY(1,vMList,"|").
         /* �஢�ઠ ᮮ⢥��⢨� ����ன�� ᯨ᪮� */
         IF NUM-ENTRIES(vTrList) NE NUM-ENTRIES(vMList) THEN 
         do:
            CREATE loan_err.
            ASSIGN
               loan_err.cont-code = loan.doc-ref
               loan_err.template  = op-template.op-template
               loan_err.error       = "�����४⭠� ����ன�� �� ���墠�࠭�. ��ᮢ������� ���-�� ����ᮢ ᯨ᪮�".
            IF NOT({assignex mChkTemplate}) THEN
            FOR EACH loan_ved WHERE
                     loan_ved.cont-code = loan.cont-code:
                DELETE loan_ved.
            END.

            IF {assignex mChkTemplate}
            THEN DO:
               RUN Save_Post.
               UNDO GEN, NEXT GEN.
            END.
            ELSE DO:
               /* �᫨ �࠭� ᮧ��� �⮩ �࠭���樥�, � 㡨���� ���, 
               �.�. �� ࠢ�� ����� ������� �ய�᪠���� */
               IF all_recids.overtr EQ YES THEN 
               DO: 
                  CREATE DelRec.  
                  ASSIGN
                     DelRec.recid1 = RECID(loan).
               END.
               UNDO NEXT_LOAN, NEXT NEXT_LOAN.
            END.
         end.
         
         chose-templ = GetXattrValueEx(
                       "op-template",
                       op-kind.op-kind + "," + STRING(op-template.op-template),
                       "���_�������", "").

         IF CAN-DO(vMList, loan.class-code)          AND
            overdr-debt.debt-kind = "BAD__DEBT"      AND
            chose-templ           <> "1"             THEN
            NEXT.

         IF CAN-DO(vMList, loan.class-code)          AND
            overdr-debt.debt-kind = "GOOD_DEBT"      AND
            chose-templ           <> "2"             THEN
            NEXT.

         IF CAN-DO(vTrList, loan.class-code)        AND
            overdr-debt.debt-kind = "BAD__DEBT"     AND
            chose-templ           <> "3"            THEN
            NEXT.

         IF CAN-DO(vTrList, loan.class-code)        AND
            overdr-debt.debt-kind = "GOOD_DEBT"     AND
            chose-templ           <> "4"            THEN
            NEXT.
      &ENDIF

      {move-bar.i "all_recids.count" total_value}

      /*�᫨� 蠡��� �ਢ��, � �ய�᪠��*/

      IF CAN-DO(mErrorTempl,STRING(op-template.op-template))
      THEN DO:
         FIND FIRST wop WHERE wop.op-templ EQ op-template.op-template.
         
         /* �᫨ 蠡��� �ய�᪠���� � ���㬥�� �� ᮧ������, � ����塞 ���⠭��� �㬬�, ���� ��⮬ ��� �� �訡�� �� �ᯮ�짮������ */
         wop.amt-cur = 0.
     
         RUN Save_Post.
         DELETE wop.
         
         FIND FIRST xattr WHERE 
                    xattr.class-code EQ op-kind.class-code 
                AND xattr.Xattr-Code EQ "NoUndo"
         NO-LOCK NO-ERROR.
         
         IF  AVAILABLE xattr 
         AND xattr.Data-Type EQ "logical" 
         AND GetXattrValue("op-kind",
                           op-kind.op-kind,
                           "NoUndo") EQ ENTRY(1,xattr.Data-Format,"/") THEN
            NEXT GEN.
         ELSE
            UNDO GEN, NEXT GEN.
      END.

      /*��孮�����᪨� ��᮪. */

      FOR EACH wop WHERE
               wop.op-templ GE op-template.op-template:
         DELETE wop.
      END.

      CREATE wop.
      {asswop.i}

      ASSIGN
         wop.con-date = svPlanDate
         tacct-cr     = op-template.acct-cr
         tacct-db     = op-template.acct-db
         .

      /* ���� ��⮢ �� �⠭����� �㭪�� */
      {g-acctv1.i
         &vacct  = tacct
         &BYrole = YES }

      ASSIGN
         wop.acct-db = tacct-db
         wop.acct-cr = tacct-cr
         .

      /* ��।������ ������ �஢���� */
      RUN cur_def (RECID(wop)).
      IF RETURN-VALUE NE "" THEN
      DO:
         CREATE loan_err.
         ASSIGN
            loan_err.cont-code = loan.doc-ref
            loan_err.template  = op-template.op-template
            loan_err.error     = RETURN-VALUE
            .
         IF NOT({assignex mChkTemplate}) THEN
         FOR EACH loan_ved WHERE
                  loan_ved.cont-code = loan.cont-code:
             DELETE loan_ved.
         END.

         IF {assignex mChkTemplate}
         THEN DO:
            RUN Save_Post.
            UNDO GEN, NEXT GEN.
         END.
         ELSE DO:
            IF all_recids.overtr EQ YES THEN DO: /* �᫨ �࠭� ᮧ��� �⮩ �࠭���樥�,
                                                    � 㡨���� ���, �.�. �� ࠢ�� �����
                                                    ������� �ய�᪠���� */
                CREATE DelRec.
                ASSIGN
                    DelRec.recid1 = RECID(loan).
            END.
            UNDO NEXT_LOAN, NEXT NEXT_LOAN.
         END.
      END.
      wop.currency = GetCurr(op-templ.currency).

      RUN parssen.p (RECID(wop),in-op-date,OUTPUT fler).
      IF fler THEN
      DO:
         CREATE loan_err.
         ASSIGN
            loan_err.cont-code = loan.doc-ref
            loan_err.template  = op-template.op-template
            loan_err.error       = "�訡�� �� ���� �㬬�"
            .
         IF NOT({assignex mChkTemplate}) THEN 
         FOR EACH loan_ved WHERE
                  loan_ved.cont-code  = loan.doc-ref:
            DELETE loan_ved.
         END.

         /*UNDO NEXT_LOAN, NEXT NEXT_LOAN.*/
         IF {assignex mChkTemplate}
         THEN DO:
            RUN Save_Post.
            UNDO GEN, NEXT GEN.
         END.
         ELSE DO:
           IF all_recids.overtr EQ YES THEN DO: /* �᫨ �࠭� ᮧ��� �⮩ �࠭���樥�,
                                                   � 㡨���� ���, �.�. �� ࠢ�� ����� �������
                                                   �ய�᪠���� */
               CREATE DelRec.
               ASSIGN
                   DelRec.recid1 = RECID(loan).
           END.
           UNDO NEXT_LOAN, NEXT NEXT_LOAN.
         END.


      END.
      IF wop.amt-rub  EQ     0                         OR
        (wop.amt-sign BEGINS ">" AND wop.amt-rub <= 0) OR
        (wop.amt-sign BEGINS "<" AND wop.amt-rub >= 0) THEN
      DO:
         /*IF mChkTemplate = ? THEN
         DO:*/
            CREATE loan_err.
            ASSIGN
               loan_err.cont-code = loan.doc-ref
               loan_err.template  = op-template.op-template
               loan_err.error     = "�㫥��� �㬬� �஢����"
              .
         /*END.*/

         RUN Save_Post.
         UNDO GEN, NEXT GEN.
      END.
      /*�᫨ ���� ᮧ������ ���㬥��*/
      IF all_settings.do_op  THEN
      DO:

        /* �ॡ�� op(sess).cr */
        cur-op-date = if op-template.op-status eq "�"
                      then ?
                      else in-op-date.

         CREATE op.      /* ��������� ������. */
         {op(sess).cr}   /* ���������� */
&GLOB opreq-OP-DOC-NUM-USE-AUTO-COUNTER vAutoNum
         {g-op.ass}      /* ⠡���� OP */
&UNDEFINE opreq-OP-DOC-NUM-USE-AUTO-COUNTER
         IF NOT vAutoNum THEN
            ASSIGN
               op.doc-num = STRING(all_settings.flt_first_num).
         /* �������� ��易⥫��� �� �� ���㬥�� */
         RUN grsigns.p (RECID(op)).

         IF NUM-ENTRIES(session:parameter) GE 7 THEN
            UpdateSigns(op.Class-Code, STRING(op.op), "��������⮪",
                        ENTRY(7,session:parameter) + "|" + ENTRY(3,session:parameter),?).

         /* �������� ��, ��������� �� 蠡���� �࠭���樨. */
         RUN parssign2.p ("PARSSEN_",
                          in-op-date,    /* ��� ��. */
                          "op-template", /* ������������ ⠡���� � ��㫮�. */

                                       /* �����䨪��� ����. */
                          op-templ.op-kind + "," + string(op-templ.op-templ),
                          op-templ.class-code,
                          "op",          /* ������������ ⠡���� ��ꥪ�. */
                          STRING(op.op), /* �����⥫� �� ��ꥪ ��� ᮧ����� ��.*/
                          op.class-code,
                          RECID(wop)).   /* �����⥫� �� WOP ��� �����. */

         /* �᫨ ������ �� �������� �� ������ - ��⠭�������� �� LegTerr
            (�᫨ �஢�ઠ �� ����祭�, � mLegTerr �ᥣ�� = YES) */
         IF mLegTerr EQ NO THEN
            UpdateSigns(op.Class-Code, STRING(op.op), "LegTerr", "NO", ?).

         CREATE op-entry.
         /* ���������� op-entry */
         {g-en.ass}

         /*ᮤ�ঠ���*/
         RUN ProcessDetails (RECID(wop), INPUT-OUTPUT wop.details).

         ASSIGN
            op-entry.acct-db  = wop.acct-db
            op-entry.acct-cr  = wop.acct-cr
            op-entry.currency = wop.currency
            op.details        = wop.details
            op-entry.amt-cur  = IF wop.currency EQ "" THEN 0 ELSE wop.amt-cur
            op-entry.amt-rub  = wop.amt-rub
            mOldOp            = op-entry.op
            mOldOpEntry       = op-entry.op-entry
            mOldSumRub        = op-entry.amt-rub
            mOldSumCur        = op-entry.amt-cur
            mOldCurrency      = op-entry.currency
            all_settings.flt_first_num = all_settings.flt_first_num + 1 WHEN vAutoNum
            .

         CREATE loan_err.
         ASSIGN
            loan_err.cont-code = loan.doc-ref
            loan_err.template  = op-template.op-template
            loan_err.error     = "�������⭠� �訡��"
          .


         mMjbank = GetXattrValueEx("op-template",
                                    op-kind.op-kind + "," + STRING(op-template.op-template),
                                   "�������",
                                   "���") = "��".
        
         IF  (op-template.mfo-needed OR mMjbank)
         AND (GetXattrValueEx ("loan",
                               loan.contract + "," + loan.cont-code,
                               "������",
                               "���") = "��") THEN
         DO:    
            RUN opfromsi.p(RECID(op-template), RECID(op-entry),OUTPUT mIsOk).
         END.


         /* �������� ����樨 �ந�������� �� �������� ����. */
         RUN op_kau.p (RECID(op-entry),
                       RECID(op-template),
                       RECID(loan),
                       svPlanDate).

         if entry(2,return-value,'|') ne '0'
         then do:
            CREATE loan_err.
            ASSIGN
              loan_err.cont-code = loan.doc-ref
              loan_err.template  = op-template.op-template
              loan_err.error     = entry(3,return-value,'|')
            .
            Undo Gen, leave Gen.
         end.

         {op-entry.upd
            &Offopupd  = "/*"
            &Ofnext    = "/*"
            &open-undo = "undo GEN, leave GEN"
            &open-undo-trans = "mVL_Except = STRING(op.op-transaction). UNDO GEN,LEAVE NEXT_LOAN"
         }

         /*���⠭����� �� ����⥪� ����� ᭥�� �஢����*/
         IF AVAIL op-entry THEN DO:
            UpdateSigns (op-entry.class-code,
                         STRING(op-entry.op) + "," + STRING(op-entry.op-entry),
                         "������",
                         loan.contract + "," + loan.cont-code,
                         ?).
                          
               /* ��⠭���� ४����⮢ �� �஢���� */
            RUN "parssign2.p" ("PARSSEN_ENTRY_",
                               in-op-date,
                               "op-template",
                               op-kind.op-kind + "," + STRING(op-templ.op-templ),
                               op-templ.class-code,
                               "op-entry",
                               STRING(op-entry.op) + "," + STRING(op-entry.op-entry),
                               op-entry.class-code,
                               RECID(wop)).
            /* �᫨ �㬬� �������� �, ���४��㥬 loan-int*/
            IF op-entry.amt-rub <> mOldSumRub  OR
              (op-entry.amt-cur <> mOldSumCur) THEN

               FOR EACH loan-int WHERE
                        loan-int.contract  = loan.contract
                    AND loan-int.cont-code = loan.cont-code
                    AND loan-int.op        = mOldOp
                    AND loan-int.op-entry  = mOldOpEntry
               EXCLUSIVE-LOCK:
                  /*���� �㤥� ᤥ���� �஢��� �� ��墠�*/
                  ASSIGN
                     loan-int.amt = IF op-entry.currency EQ "" AND
                                       mOldCurrency      EQ "" THEN
                                       op-entry.amt-rub
                                     ELSE
                                     IF op-entry.currency = mOldCurrency THEN
                                        op-entry.amt-cur
                                     ELSE
                                        CurFromBase("����",
                                                    mOldCurrency,
                                                    op-entry.op-date,
                                                    op-entry.amt-rub).
               END. /*FOR EACH loan-int*/

            /* �᫨ ���� ᮧ���� ��ॡ��� �।�� ��� �����⨪�
            ** �� ������ࠬ, � 㤠�塞 ᮧ����� � �஢���� �ਧ���. */
            RUN op_kau.p (RECID(op-entry),
                          RECID(op-template),
                          RECID(loan),
                          svPlanDate).

            /* ��� �������� ��襤�� ���㬥�⮢ */
            CREATE loan_ved.
            ASSIGN
               loan_ved.cont-code = loan.cont-code
               loan_ved.l_currenc = loan.currency
               loan_ved.op        = op.op
               loan_ved.op-entry  = op-entry.op-entry
               loan_ved.doc-type  = op.doc-type
               loan_ved.doc-num   = op.doc-num
               loan_ved.acct-db   = op-entry.acct-db
               loan_ved.acct-cr   = op-entry.acct-cr
               loan_ved.currency  = op-entry.currency
               loan_ved.amt-cur   = op-entry.amt-cur
               loan_ved.amt-rub   = op-entry.amt-rub
               .
         mMethodAfter = GetXAttrValueEx("op-template", 
                                         op-kind.op-kind + "," 
                                            + STRING(op-template.op-template),
                                        "RunAfterCreate", "").
         
         IF mMethodAfter NE "" THEN
         DO:
         
            IF SearchPFile(mMethodAfter) THEN
            DO:
               RUN VALUE(mMethodAfter + ".p") (RECID(op-entry)).
       
            END.
         END.
  
         END.
         /*�᫨ �஢���� 㤠����, � ᭮ᨬ loan-int*/
         ELSE DO:
            FOR EACH loan-int WHERE
                     loan-int.contract  = loan.contract
                 AND loan-int.cont-code = loan.cont-code
                 AND loan-int.op        = mOldOp
                 AND loan-int.op-entry  = mOldOpEntry
            EXCLUSIVE-LOCK:
               /*���� �㤥� ᤥ���� �஢��� �� ��墠�*/
               DELETE loan-int.
            END.
         END.

         DELETE loan_err.

      END. /*if all_settings.do_op*/
      {init-bar.i """ + mBarMessage + """}
      {move-bar.i "all_recids.count" total_value}
      /*��孮�����᪨� ��᮪.*/

   END. /*GEN*/

   /* �஢�ઠ - �᫨ �� ���� 蠡��� �� ��ࠡ�⠫
      ��� �訡�� � �࠭� �� ᮧ��� ��⮬���᪨
      �⮩ �� �࠭���樥�, � 㤠�塞 �࠭� */

   FOR EACH flt-template WHERE
       flt-template.flt-num = mFltNum
   NO-LOCK,
   FIRST op-template WHERE
       op-template.op-kind     = flt-template.op-kind AND
       op-template.op-template = flt-template.op-template
   NO-LOCK:
       FIND FIRST save_wop WHERE save_wop.op-templ EQ op-template.op-template NO-LOCK NO-ERROR.
       IF NOT AVAIL save_wop THEN
           vOverEr = YES. /* ��� �� ���� 蠡��� �ࠢ��쭮 ��ࠡ�⠫ */
   END.
   IF all_recids.overtr EQ YES AND vOverEr EQ NO THEN DO: /* �᫨ ᮧ��� �⮩ �࠭���樥�, � �������� �訡��, � ᭮ᨬ �࠭� */
      FIND FIRST over_error WHERE over_error.cont-code BEGINS ENTRY(1,loan.cont-code," ") NO-ERROR.
      IF AVAIL over_error THEN
         DELETE over_error.
      FIND CURRENT loan EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL loan THEN
         DELETE loan.
   END.
      /* �㬥��� �� ������ࠬ */
   IF NOT vAutoNum AND vOverEr THEN
      all_settings.flt_first_num = all_settings.flt_first_num + 1.
END. /*NEXT_LOAN*/
RUN DeleteOldDataProtocol IN h_base ("�࠭������").
   /* ��頥� ��� � �⪫�砥� ���஢���� ������� �㭪権 */
RUN EmptyLnCache IN h_oldpr.
RUN FlgLnCache IN h_oldpr (FALSE).

/* ����塞 loan �᫨ �訡�� �������� � cur_def,
   � ⠪�� 㡨���� ᮮ�饭�� � ᮧ����� ������ �祭�� -
   �.�. ��� � ᠬ�� ���� - �� ᮧ������ !*/

FOR EACH DelRec NO-LOCK:
    FIND FIRST loan WHERE RECID(loan) EQ delrec.recid1 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL loan THEN
    DO:
       FIND FIRST over_error WHERE over_error.cont-code BEGINS ENTRY(1,loan.cont-code," ") NO-ERROR.
       DELETE over_error.
       DELETE loan.
    END.
END.

PUBLISH 'done'.

IF VALID-HANDLE(ph) THEN
   DELETE PROCEDURE (ph). /* �������� ���� �� �����. */

IF VALID-HANDLE(parssen-library-proc)
THEN DO:
   RUN EmptyLPResult (OUTPUT vEmptyOk).
   DELETE PROCEDURE parssen-library-proc.
END.


IF VALID-HANDLE(mLibHandle) THEN
DO:
   DELETE PROCEDURE mLibHandle.
END.


{intrface.del}
{del-bar.i}
IF flager = 9 THEN
   RETURN 'AvtReqUndo' + ',' + mVL_Except.

/* ���࠭���� ��ࠬ��஢ �।��饣� 蠡���� �� �㫥��� �㬬� �஢���� */
PROCEDURE save_post:
   CREATE save_wop.
   BUFFER-COPY wop TO save_wop.
END PROCEDURE.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='17/11/2015 10:36:12.548+04:00' */
/* $LINTUSER='lakd' */
/* $LINTMODE='1' */
/* $LINTFILE='loanop.i' */
/*prosign8ACqgV7O3P1g94EtQLO5jQ*/