/*               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: DPS-TR1.P
      Comment: ����� "����� ������",
      Comment: �࠭����� ��७�� ��業⮢ �� ��� ��� �㡠����⨪�,
      Comment: � ஫�� "loan-dps-pro"
      Comment: ��७�� �� ����.
   Parameters:
         Uses:
      Used by:
      Created: ??/??/???? ???
     Modified: 31/05/2001 Om
     modified: 25/03/2002 kostik  ��� �ࠢ��쭮� ࠡ��� �㭪樨 ���� � ������
                                 (g-acctv1.i) �������� ��� ⠡���� wop ������
                                 ���樠����஢����� �� g-acctv1.i
  Last change:
     Modified: 24.01.2003 15:01 SEMA     �� ��� 0012068 �뢥����� ���� �� �㬬�� ����権 �뭥ᥭ� � �⤥���
                                         �����㬥��
     MODIFIED: 20/03/2003 KOSTIK 0013784 �����⨥ ��������� �� ��� (13793)
     Modified: 07/05/2003 ���� - ��ଠ�஢���� + ��⠢��� ������
                                  g-psigns.i  � ��ࠡ�⪮� ४����⮢
                                  ᮧ�������� ����஬
                                  + ������� ���� �뤠����� � logg.p
                                  ��७�ᥭ� � dps-logs.i
     Modified: 06/06/03 Gorm (�. 12539) �뭥ᥭ� � ��砫� �ணࠬ�� ��।������ 
                             ��६����� vPutStr � prtfile.i
                             ���������� �� �⠫쭮�� ����稢���� op � ������� 
                             ��࠭����-����⠭������� op �१ �६����� ⠡���� Save_wop                             
     Modified: 11.11.2003 16:47 alvel    
     Modified: 12.11.2003 16:56 alvel    
     Modified: 04.02.2004 14:55 FEAK     
     Modified: 01.07.2007 12:44 KSV      (0078824) �����஢�� ��� ���ᬠ��
*/

form "~n@(#) dps-tr1.p 1.0 ??? ??/??/???? Om 31/05/2001"
with frame sccs-id stream-io width 250.

{globals.i}
{intrface.get tmess}
{intrface.get debug}
{intrface.get xclass}

DEFINE VARIABLE mFileNameDest AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mStreamDest AS INT64  NO-UNDO.
&GLOBAL-DEFINE def-dest
&SCOPED-DEFINE Nachkin-tt Nchk-tt

DEFINE NEW GLOBAL SHARED STREAM err.
DEFINE STREAM err-ved.
DEFINE                   STREAM doc.

{g-defs.i}
{dpsproc.def}
/*��� �����, �⮡� ����᪠� "������� �奬� ���᫥���" */
RUN SetSysConf IN h_base ("TypeNach", "����ࢨ஢����").
{def-wf.i new}
{prn-ved.def
    &new-nach = new-nach
    &tt = {&Nachkin-tt}
}
{defframe.i new}
{dps-logs.i}
{prn_ved_nach.i 
   &Nachkin-tt = {&Nachkin-tt}
   &no-ved_op  = YES
}
{cr-nach.i
       no-svget="/*"
}

DEFINE INPUT PARAM in-op-date AS DATE  NO-UNDO.
DEFINE INPUT PARAM oprid      AS RECID NO-UNDO.

DEF VARIABLE in-contract-date LIKE op.op-date NO-UNDO.

DEFINE VAR in-templ     AS INT64  NO-UNDO.
DEFINE VAR beg-templ    AS CHAR     NO-UNDO.
DEFINE VAR lst-tmpl-op  AS CHAR     NO-UNDO.
DEFINE VAR cred         AS DATE     NO-UNDO.
DEFINE VAR ntempl       AS INT64      NO-UNDO.
DEFINE VAR name-klient  AS CHAR     NO-UNDO.
DEFINE VAR fler         AS LOGICAL  NO-UNDO.
DEFINE VAR i            AS INT64  NO-UNDO.
DEFINE VAR k            AS INT64  NO-UNDO.
DEFINE VAR summ         AS DECIMAL  NO-UNDO.
DEFINE VAR str-num      AS CHAR     NO-UNDO.
DEFINE VAR loan_h       AS HANDLE   NO-UNDO.
DEFINE VAR mess1        AS CHAR
                        INIT ?      NO-UNDO.
DEFINE VAR c-type       AS CHAR
                        INIT '*'    NO-UNDO.

DEFINE VAR fll          AS LOGICAL
                        EXTENT 30   NO-UNDO.
DEFINE VAR tt-num       AS INT64.
DEFINE VAR rid1         AS RECID EXTENT 30.
DEFINE VAR num-iten     AS INT64      NO-UNDO.
DEFINE VAR beg          AS INT64      NO-UNDO.
DEFINE VAR delta        AS DECIMAL  NO-UNDO.
DEFINE VAR sec          AS INT64      NO-UNDO.
DEFINE VAR minute       AS INT64      NO-UNDO.
DEFINE VAR hour         AS INT64      NO-UNDO.
DEFINE VAR result       AS INT64      NO-UNDO .
DEFINE VAR vTmpOutput   AS LOGICAL  NO-UNDO.
DEF    VAR vSave%       AS LOGICAL  NO-UNDO. /*���࠭��� �� ����� � ���᫥���� %%*/
DEFINE VAR t-type       AS CHAR     NO-UNDO. /*���� ������� ��� ��ࠡ�⪨*/
DEF    VAR vi           AS INT64      NO-UNDO.
DEFINE VAR vFileName    AS CHAR     NO-UNDO.
DEF    VAR mErrText     AS CHAR     NO-UNDO. /* ������ ����室��� ��࠭��� ⥪�� ��������� �訡�� */  

DEFINE VAR in-contract  LIKE loan.contract          NO-UNDO.
DEFINE VAR in-cont-code LIKE loan.cont-code         NO-UNDO.
DEFINE VAR dval         LIKE op-entry.value-date    NO-UNDO.
DEFINE VAR curr1        LIKE loan.currency          NO-UNDO.

DEFINE BUFFER xxop-entry    FOR op-entry.
DEFINE BUFFER xwop          FOR wop.

DEF VAR stat-min        AS CHAR                  NO-UNDO.
DEF VAR stat-new        AS CHAR                  NO-UNDO.
DEF VAR mCodOst         AS CHAR                  NO-UNDO.
DEF VAR n               AS INT64                   NO-UNDO.
DEF VAR fname           AS CHAR                  NO-UNDO.
DEF VAR mHPQuery        AS HANDLE                NO-UNDO.
DEF VAR mRowId          AS ROWID                 NO-UNDO.
DEF VAR mRek AS CHAR   NO-UNDO.

DEFINE VARIABLE save-rid AS RECID               NO-UNDO. 

DEFINE VARIABLE vPutStr AS CHARACTER NO-UNDO.
DEF VAR mFl-w AS LOG NO-UNDO. /*(䫠� ࠡ��� � ��㧥஬ ������஢,
                                     ������� ��� �������� �।��)*/

DEFINE VARIABLE mUser   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mMenuId AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipPotok AS INT64 NO-UNDO.
DEFINE VARIABLE mDelim  AS INT64 NO-UNDO.




SUBSCRIBE TO "tmess-fill-sysmes-event" ANYWHERE.


IF NUM-ENTRIES(session:parameter) GE 8 THEN
ASSIGN
   ipPotok = INT64(ENTRY(3,session:parameter))
   mDelim  = INT64(ENTRY(4,session:parameter))
   mUser   =  ENTRY(7,session:parameter) 
   auto    = Yes
   in-op-date = IF ENTRY(8,session:parameter) NE "*" THEN DATE(ENTRY(8,session:parameter))  ELSE  today - 1
   in-contract-date = in-op-date .
ELSE
  RETURN.


mFl-w =  GetSysConf("PlacementCLSW_RSHB") = "��" . /* ࠡ�⠥� � ��㧥஬ ? */

{prtfile.i &NUM-COL = 120
          &APPEND  = YES}

&Scop Ofsrch        0.
&Scop DoContract    0.
&Scop Col-lab       '�������� ���� ����������'
&Scop BYrole        YES.

/*OUTPUT STREAM doc TO TERMINAL.*/

{g-currv1.i &OFbase="/*"}
{details.def}   /* �����㬥�� �맮�� �����୮� ��ࠡ�⪨ ���� op.details */
{savewop.i}

FORM
   loan.doc-ref
   loan.currency
   summ
      COLUMN-LABEL "�㬬�"
WITH FRAME out-doc.

FUNCTION Set_type RETURNS LOGICAL (INPUT l-type AS CHAR) IN loan_h.
FUNCTION Set_ost  RETURNS LOGICAL (INPUT l-type AS CHAR) IN loan_h.

RUN "l-type.p" PERSISTENT SET loan_h .

FUNCTION chk_op_t RETURNS LOG (INPUT num-templ AS INT64):   
   DEF BUFFER xxop FOR op.
   
   FIND LAST xxop WHERE
             xxop.op-transaction EQ cur-op-trans
         AND xxop.op-template    EQ num-templ
   NO-LOCK NO-ERROR.
   
   RETURN AVAIL xxop.   
END FUNCTION.

MAIN:
DO ON ERROR  UNDO, LEAVE
   ON ENDKEY UNDO, LEAVE:                 

   FIND FIRST op-kind WHERE RECID(op-kind) EQ oprid
      NO-LOCK NO-ERROR.
      
   RUN Init-SysMes IN h_tmess (op-kind.op-kind,"","").
   
   {plibinit.i}
   
   ASSIGN
      DebugParser = INT64(GetXAttrValueEx('op-kind',
                                        op-kind.op-kind,
                                        'debugparser',
                                        '0'))
      lst-tmpl-op = list-op-templ(op-kind.op-kind,"op")
      .
   
   FIND LAST op-templ OF op-kind
      NO-LOCK NO-ERROR.
   
   IF NOT AVAIL op-templ THEN
   DO:
      RUN Fill-SysMes IN h_tmess (op-kind.op-kind,
                                  "",
                                  "-1",
                                  "��� �� ������ 蠡����.").      
      UNDO MAIN, LEAVE MAIN.
   END.
   
   ntempl = op-templ.op-templ.
   
   IF auto EQ NO THEN
   DO:
      OUTPUT STREAM doc TO TERMINAL.
      
      
      {g-frame3.i
	      &op      = t-op
	      &DoTable = yes
	   }
      {g-frame3.i
	      &DoFrame = Yes
	      &row     = 10
	      &op      = t-op
	   }
   END.
   ELSE
/*      OUTPUT STREAM doc TO "_spooldoc.tmp".*/
   
   cur-op-date = in-op-date.
   
   IF mFl-w THEN /* �� ࠡ�� � ��㧥஬ */
      cred = in-op-date.
   
   {prn-ved.i
      &EmptyTempTable     = "��頥� ⠡����"
      &DefPrintProcedures = "���塞 ��楤��� ����"
      &Stream             = " STREAM err"
      &FileName           = _spool1.tmp
      &PutPlanDate        = cred
   }
   
   /*  ���������� �६����� ⠡���� ��ࠬ��ࠬ� �� 蠡���� */
   FOR EACH op-templ OF op-kind
      NO-LOCK
         ON ERROR  UNDO, LEAVE
         ON ENDKEY UNDO, LEAVE:
   
      /* �஢�ઠ ���������� ���᫥��� �� ������ ��ࠧ�� 
      ** �஢������ ⮫쪮 �᫨ ࠭�� �� �� ������ ⠪�� 蠡���, �.�. �� ��
      ** ����祭 YES */
      
      IF NOT vTmpOutput THEN      
         RUN Get-Old-Ref in h_dpspc (RECID(op-templ),
                                             OUTPUT vTmpOutput).
      CREATE t-op.
   
      ASSIGN
         t-op.op             = op-templ.op-templ
         t-op.doc-type       = op-templ.doc-type
         t-op.doc-num        = "1"
         t-op.op-date        = in-op-date
         t-op.contract-date  = in-contract-date
         t-op.doc-date       = in-op-date
         t-op.details        = op-templ.details
         .
   
      IF op-templ.op-templ GT 1 THEN
         t-op.contract-date = cred.
   
      IF auto EQ NO THEN
      DO:
         {g-frame3.i
	         &op       = t-op
	         &DoBefore = Yes
	      }
         {g-frame3.i
	         &op       = t-op
	         &DoDisp   = YES
	         &DoStream = Yes
	      }
         {g-frame3.i
	         &op    = t-op
	         &DoSet = Yes
	      }
      END.
   
      IF op-templ.op-templ EQ 1 THEN
         cred = t-op.contract-date.
   
      IF op-templ.op-templ EQ 1  AND
         t-op.doc-num      NE '' AND
         t-op.doc-num      NE ?  THEN
      DO:
   
         tt-num  = INT64(t-op.doc-num) NO-ERROR.
         str-num = IF ERROR-STATUS:ERROR THEN
                      t-op.doc-num
                   ELSE
                      ''.
      END.
   END.
   IF tt-num EQ 0 THEN tt-num = 1.
   /* ��室 �� "Esc" */
   IF  KEYFUNCTION(LASTKEY) EQ 'end-error' THEN
   DO:
      HIDE FRAME opreq NO-PAUSE.
      UNDO MAIN, LEAVE MAIN.      
   END.
   
   FIND FIRST op-templ OF op-kind
      NO-LOCK NO-ERROR.
   
   curr1 = op-templ.currency.
   
   IF GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?) <> ? THEN
      c-type = GetXAttrValueEx("op-kind",op-kind.op-kind,"cont-type",?).
   
   FORM
      loan.doc-ref
      loan.open-date
      num-iten
      COLUMN-LABEL '����������'
      hour
      COLUMN-LABEL '����'
      minute
      COLUMN-LABEL '������'
      sec
      COLUMN-LABEL '������.'
      WITH FRAME prn OVERLAY CENTERED.
   
   beg = TIME.
   
   
   /* ������  �६����� ������ �� ������祭�� 䨫���, �⡨ࠥ� ������ ��
      ������ loan-status-cl � ��ᢠ����� ������ loan-status */
   RUN loanst.p(op-kind.op-kind, OUTPUT stat-min, OUTPUT stat-new) .
   
   
   /* �롮� ������஢, �� ⨯� + �, �� ����� �� ����� �ࠢ� */
   IF mFl-w THEN DO: /* �롮� �������, �᫨ ࠡ�⠥� � ��㧥஬ */
          in-cont-code =  GetSysConf("PlacementCLSW_RSHB_cont-code").
          c-type = "*".
   END.
   
   /* �����⮢�� ����� ������ ��� ��࠭���� %% */
   vSave% = (FGetSetting('�������',?,?) = "��").
   IF vSave%  THEN DO:
      RUN prep_cr_data (cred,
                        GetThisUserOtdel(),
                        mDataClass-ID). 
   END.
   
   /* ��।������ ⨯�� ������� ��� ��ࠡ�⪨ */
   FOR EACH CODE WHERE CODE.class = "cont-type"
         AND CODE.PARENT = "cont-type" NO-LOCK:
         IF  CAN-DO(c-type, CODE.CODE)  THEN DO:
            {additem.i t-type CODE.CODE}
         END.
   END.
   
   RUN qrybrwld.p ("dep_person",THIS-PROCEDURE:HANDLE,OUTPUT mHPQuery) NO-ERROR.
   /* � ��६����� mHBrwQuery (bstty.def) �������� ��� ����㦥���� DS-���������� */
   IF NOT VALID-HANDLE (mHPQuery)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "-1", "�訡�� ����㧪� ��楤��� �ନ஢���� ����� ��� ����� ~"dep_person~"").
      UNDO MAIN, LEAVE MAIN.
   END.
   
   DO vi = 1 TO NUM-ENTRIES(t-type): 
   
      RUN SetDSContext IN mHPQuery ("icontr~001close-date1~001close-date2~001cont-type" + "~001since2~001FieldOper",
                                    "dps~001?~001?~001" + ENTRY(vi, t-type) + "~001" + STRING(in-op-date) + "~001since2,LE",
                                    "SortBy",
                                    "loan.currency,loan.cont-code") NO-ERROR.
      /* ��ࠡ�⪠ �訡�� */
      IF ERROR-STATUS:ERROR THEN 
         UNDO MAIN, LEAVE MAIN.
                              
      /* ����⨥ ����� */
      RUN Open-Query IN mHPQuery.
      /* ��ࠡ�⪠ �訡�� */
      IF ERROR-STATUS:ERROR THEN 
         UNDO MAIN, LEAVE MAIN.
      
                                     
      
      BL_LOAN:
      DO WHILE TRUE  
         TRANSACTION 
         ON ERROR  UNDO BL_LOAN, NEXT  BL_LOAN
         ON ENDKEY UNDO BL_LOAN, LEAVE BL_LOAN:
          IF RETRY THEN .
         
         RUN Get_Query_Record IN mHPQuery ("NEXT",
                                           "loan-cond",
                                           OUTPUT mRowId).
         IF mRowId EQ ?  /* or i GE 5 */  THEN
            LEAVE BL_LOAN.
            
         FIND FIRST loan-cond WHERE ROWID(loan-cond) EQ mRowId
            NO-LOCK NO-ERROR.
         IF AVAIL loan-cond THEN
            FIND FIRST loan WHERE loan.contract  EQ loan-cond.contract
                              AND loan.cont-code EQ loan-cond.cont-code
               NO-LOCK NO-ERROR.

         IF INT64(RECID(loan)) MODULO mDelim NE ipPotok THEN NEXT bl_loan.

         IF     AVAIL loan-cond
            AND AVAIL loan THEN
         DO:
            IF     mFl-w 
               AND NOT (    loan.cont-code GE in-cont-code
                        AND loan.cont-code LE in-cont-code) THEN
               NEXT bl_loan.            
       
            mErrText = {&ErrorAcctOperLogg}.                                         
                                                      
            IF NOT mFl-w THEN DO:   /* �� ࠡ�� � ��㧥஬ �⪫�稬 �஢��� ����ᮢ */
               IF (stat-min <> ? AND loan.loan-status LT  stat-min) OR
                  /* ��������� ����������� ����஫�஢��� ���. ����� */
                  (stat-min  = ? AND loan.loan-status NE '�')       OR
                  (stat-new <> ? AND loan.loan-status GE stat-new)  THEN
               DO:
                  RUN CreateErr({&ErrorDpsStatus}).
                  NEXT bl_loan.
               END.
            END.
            PAUSE 0.
         
            ASSIGN
               num-iten     = num-iten + 1
               delta        = TIME - beg
               sec          = delta MODULO 60
               delta        = (delta - sec) / 60
               minute       = delta MODULO 60
               hour         = (delta - minute) / 60
               in-contract  = loan.contract
               in-cont-code = loan.cont-code
               .
            IF auto EQ NO THEN
            DISPLAY
               loan.doc-ref
               loan.open-date
               num-iten
               hour
               minute
               sec
               WITH FRAME prn.
         
            FIND person WHERE
                 person.person-id = loan.cust-id NO-LOCK NO-ERROR.
         
            name-klient = IF AVAILABLE person THEN
                             person.name-last + ' ' + person.first-name
                          ELSE
                             ''.
         
            IF RETRY THEN
            DO:
         
               IF mess1 EQ ? THEN
                  mess1 = '���⥬��� �訡��'.
               RUN CreateErr("����� � " + loan.cont-code + mess1).
               NEXT BL_LOAN.
            END.
         
            /* ������祭�� ��⮤� �������. ��ࠡ�⪨ �������� �����⢫��� �஢�ન,
               ������騥 ��ࠡ��� ������ � �࠭���樨 */
            set_type(loan.cont-code).
         
            {aft-kndi.i
               &nosearch-met  = "UNDO BL_LOAN, LEAVE MAIN. "
               &name_meth     = "�믄���"
               &in-op-date    = cred
               &aft-undo-kind = "UNDO BL_LOAN, RETRY BL_LOAN. "
               &mess    = "�� ������� ��楤�� ������樮���� ��ࠡ�⪨. ~
                           �믮������ �࠭���樨 �४�饭� "
               &putmess = " IF RETURN-VALUE <> ? AND RETURN-VALUE <> '' THEN ~
                            DO: ~
                                RUN CreateErr(return-value). ~
                            END."
            }
         
            ELSE
            DO: /*��� ⮣�, �⮡� �� ��७����. �࠭� ������祭�� ����஫�
                  ��� ����. ��⠢���� �� 㬮��. */
              /* IF NOT Chk_Date(RECID(loan-cond),cred) THEN
               DO :
                  RUN CreateErr({&BadCapitDate}).
                  NEXT BL_LOAN.
               END.*/
            END.
         
            mess1 = ?.
            RUN DelSaveWop  .
            {empty wop}
         
            RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).
         
            BL_TEMPL:
            FOR EACH op-templ OF op-kind
               NO-LOCK
                  ON ERROR  UNDO,         LEAVE bl_loan
                  ON ENDKEY UNDO bl_loan, LEAVE bl_loan:
         
               IF NOT op-templ.cr-class-code  MATCHES '*op*' THEN
                  NEXT .
         
               RUN RestoreWop.
         
               RELEASE op.
               RELEASE op-entry.
         
               FIND t-op WHERE
                    t-op.op EQ op-templ.op-templ NO-LOCK NO-ERROR.
         
               IF NOT AVAILABLE t-op THEN 
                  NEXT.
         
               CREATE op.
               {op(sess).cr}
               {g-op.ass}
               i= i + 1.
               ASSIGN
                  op.doc-type         = t-op.doc-type                  
                  op.contract-date    = t-op.contract-date
                  .
         
               FIND LAST xxop-entry WHERE
                         xxop-entry.op EQ op.op NO-LOCK NO-ERROR.
         
               k = IF AVAILABLE xxop-entry THEN
                      xxop-entry.op-entry + 1
                   ELSE
                      1.
         
               CREATE op-entry.
               VALIDATE op NO-ERROR.
               {g-en.ass &ind=k}

               IF NUM-ENTRIES(session:parameter) GE 7 THEN
                   UpdateSigns(op.Class-Code, STRING(op.op), "��������⮪",
                         ENTRY(7,session:parameter) + "|" + ENTRY(3,session:parameter),?).
               op-entry.value-date = in-op-date.
               op-entry.currency   = IF op-templ.currency NE ?
                  THEN getcurr(op-templ.currency)
                  ELSE "".
         
               op-entry.currency   = IF op-templ.currency NE ?
                  THEN getcurr(op-templ.currency)
                  ELSE loan.currency.
         
               IF op-entry.currency NE loan.currency THEN
               DO :
                  RUN CreateErr({&DifCurrInDpsTempl}).
                  UNDO BL_TEMPL, LEAVE BL_TEMPL.
               END.
               FOR EACH wop WHERE
                     wop.op-templ GE op-templ.op-templ:
                  DELETE wop.
               END.
         
               CREATE wop.
               ASSIGN
                  wop.op-templ = op-templ.op-templ
                  wop.op-kind  = op-kind.op-kind      /* ��⠢���� Sema 23/05/99 */
                  cur-op-date  = in-op-date
                  wop.currency = op-entry.currency
                  tcur         = op-entry.currency
                  dval         = op-entry.value-date
                  wop.op-recid = RECID(op-entry)
                  wop.con-date = op.contract-date
                  .
         
               /* ����ᥭ ��⮢ */
               {g-acctv1.i
                  &OFbase  = YES
                  &BYrole  = YES
                  &vacct   = tacct
                  }
               IF (op-templ.prep-amt-rub    NE ? AND op-templ.prep-amt-rub    NE "") OR (
                  op-templ.prep-amt-natcur NE ? AND op-templ.prep-amt-natcur NE "") THEN 
               DO:
         
                  {asswop.i}
         
                  set_type(loan.cont-code).
         
                  mCodOst = Get-Ost-templ(op-kind.op-kind,op-templ.op-templ).
         
                  IF op-templ.amt-rub MATCHES '*���*' AND mCodOst NE ? THEN
                     Set_ost(mCodOst).
                  RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler).
         
                  IF fler THEN
                  DO:
                     RUN CreateErr({&ErrorParsenLogg}).
                     UNDO BL_TEMPL, LEAVE BL_TEMPL.
                  END.
                  IF (tacct-db   EQ ? OR tacct-cr   EQ ?) AND wop.amt-rub NE 0 THEN 
                  DO:
         
                     IF tacct-db EQ ? THEN
                        mess1 = '�� ��।���� ��� ����� � 蠡���� ' +
                                 STRING(op-templ.op-templ) + ' '.
                     IF tacct-cr EQ ?
                        THEN mess1 = IF mess1 NE ? THEN
                                        mess1
                                     ELSE
                                      '�� ��।���� ��� �।�� � 蠡���� ' +
                                      STRING(op-templ.op-templ) + ' '.
         
                     UNDO BL_Loan, RETRY bl_loan.
                  END.
         
                  ASSIGN
                     op-entry.amt-rub = wop.amt-rub
                     op-entry.amt-cur = IF op-entry.currency <> "" THEN
                                           wop.amt-cur
                                        ELSE
                                           op-entry.amt-cur
                     op-entry.user-id = USERID('bisquit')
                     op-entry.acct-db = tacct-db
                     op-entry.acct-cr = tacct-cr
                     .
      
                  IF op-entry.amt-rub NE 0 OR op-entry.amt-cur NE 0 THEN 
                  DO:
                     IF op.op-status BEGINS "�" THEN
                        ASSIGN
                           op.op-date       = ?
                           op-entry.op-date = op.op-date
                           .
                     mRek = GetXattrValue ("op-template",
                                           op-template.op-kind + "," + STRING (op-template.op-template),
                                           "��������"). 
                     IF NOT {assigned mRek} THEN
                     DO:
                        ASSIGN
                            op.doc-num       = IF str-num EQ '' THEN
                                                  STRING(tt-num)
                                               ELSE
                                                  str-num + STRING(tt-num)
                        .
                     END.
                     ASSIGN
                        tt-num              = tt-num + 1
                        .
                  /* ��ନ஢���� ���� op.details */
                  ASSIGN
                     save-rid = wop.op-recid
                     wop.op-recid = RECID(op)            /*��� �-樨 NDoc2*/
                  .

                  RUN ProcessDetails (RECID(wop), INPUT-OUTPUT op.detail).
                     wop.op-recid = save-rid.            /*�����頥� �� ����*/

                     {aft-temp.i
                        &aft-undo   = " UNDO bl_loan,RETRY bl_loan."}                     
                     {op-entry.upd
                        &871        = YES
                        &open-date  = "UNDO bl_loan,RETRY bl_loan"
                        &open-undo  = "DO:
                                          RUN CreateErr(mErrText).
                                          UNDO bl_loan, RETRY bl_loan.
                                       END"
                     }
                     /* �������� �� ⨯� PARSEN_<�����������>*/
                     {g-psigns.i}
                     /* �������� �� �� ���㬥�� �� ���� ������ ��� �����⨪�.
                     ** ���� ��� "loan-dps-pro" */
                     {crs.i}
                     /* output stream err1 close. */
      
                     /* �����⮢�� ������ ��� ���� ���⮢ �� 蠡����� */
                     RUN Create-Op-Dpstr(t-op.details).
                  END.
                  ELSE
                  DO:
                     RUN SaveWop.
                     UNDO BL_TEMPL, RETRY BL_TEMPL.
                  END.
               END.
            END. /* END OF  FOR EACH op-templ   */
            {aft-kndi.i
               &nosearch-met  = "UNDO bl_loan, LEAVE MAIN. "
               &name_meth     ="�믏�᫥��"
               &in-op-date    = in-op-date
               &aft-undo-kind = "UNDO bl_loan,RETRY bl_loan. "
               &mess    = "�� ������� ��楤�� �������樮���� ��ࠡ�⪨. ~
                           �믮������ �࠭���樨 �४�饭� "
               &putmess = " IF RETURN-VALUE <> ? AND RETURN-VALUE <> '' THEN ~
                            DO: ~
                               RUN CreateErr(return-value). ~
                            END. "
            }
            {cdealend.i 
             &mt = "UNDO bl_loan, RETRY bl_loan. "
             &p = "n"
            }
         END.
      END. /* END OF   FOR EACH loan, LAST loan-cond  */
      
      RUN Close-Query IN mHPQuery (NO).
   END.
   RUN Close-Query IN mHPQuery (YES).
   
/*   /*Exit*/                                             */
/*   RUN Fill-SysMes IN h_tmess ("", "", "0", STRING(vi)).*/
/*   UNDO MAIN, LEAVE MAIN.                               */
   
   /*����� �������⥩*/
   {prn-ved.i
        &RunReportPrint  = "���⠥� ���������"}
   
   IF not {assigned mReportProcName} THEN DO:   
      output stream err CLOSE.
      OUTPUT stream err TO VALUE ( ISO-DATE(in-op-date) + "_" + shFilial + "_" + op-kind.op-kind + "_nach" + ".log" + STRING(ipPotok)) APPEND.

      /* ��������� ���᫥���� ��業⮢ */
      RUN ved_nach (in-op-date,
                    cred).
      output stream err CLOSE.
   END.
   
   IF vSave%  THEN DO:
      RUN setlevel IN h_debug (mTmpLevel).
   END.
   
   HIDE FRAME opreq NO-PAUSE.
   
   DELETE PROCEDURE(loan_h).
   
   HIDE FRAME prn NO-PAUSE.
   
   /* ���࠭���� � 䠩�� ���ଠ樨 �� 蠡����� */
   fname = ISO-DATE(in-op-date) + "_" + shFilial + "_" + op-kind.op-kind + "_template<&>.log" + STRING(ipPotok).
   RUN PrintVed (in-op-date,fname).

   output stream err-ved CLOSE.
   OUTPUT stream err-ved TO VALUE ( ISO-DATE(in-op-date) + "_" + shFilial + "_" + op-kind.op-kind + "_error" + ".log" + STRING(ipPotok)) APPEND.
   RUN ved_err ("").
   output stream err-ved CLOSE.
  
END.
UNSUBSCRIBE ALL.

RUN DeleteOldDataProtocol IN h_base ("OldRef").
RUN DeleteOldDataProtocol IN h_base ("TypeNach").

RUN End-SysMes IN h_tmess.

{plibdel.i}

IF AVAIL op-kind THEN
DO:
   /* �������� �� ��᫥ �믮������ �࠭���樨 */
   {cmd-exec.i
      &cmd        = "'Postcmd'"
      }
END.
{intrface.del}          /* ���㧪� �����㬥����. */ 

RETURN.

PROCEDURE tmess-fill-sysmes-event:
DEFINE INPUT PARAMETER iBuffer AS HANDLE NO-UNDO.
   IF iBuffer:BUFFER-FIELD("Mes-Type"):BUFFER-VALUE GE "3" THEN
   DO:
       pick-value = "YES".
   END.
END PROCEDURE.
