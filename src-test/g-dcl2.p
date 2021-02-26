/*
                ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright:  (C) 1992-1996 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename:  g-cash2.p
      Comment:  ���� �� ᫮����, ���������� ���㬥�⮢

         Uses:  -
      Used by:  opkindnav.p
      Created:  04/01/1998 Peter from g-cash.p
     Modified:  03/12/2001 NIK �맮� kautrig.p ������� ��  Kau-Trigger in h_op.
     modified: 20/01/2002 kostik ����� 4488 ������ �� ���
     Modified: 06/05/2003 ���� - ��ଠ�஢���� + ��⠢��� ������
                                  g-psigns.i � ��ࠡ�⪮� ४����⮢
                                  ᮧ�������� ����஬
     Modified: 28/11/2003 FeaK  �������� ��ࠬ��� OUTPUT fl-err � �맮�� 
                                ��楤��� g_sroch2.p.                             
     Modified: 24.11.2006 16:26 OZMI     (0070663)
     
     Modified: 17.03.2015 pja ������� ᯥ樠�쭮 � ⮫쪮 ��� ������� ����� ������� � �ப.
                          ��������� �맢��� ����室������� ������ ������ �� �����⨥ ������
*/

{g-defs.i}
{crdps.def}
{g-error.def}
{wordwrap.def}
{globals.def}
{dpsproc.def}
{def-wf.i new}
{defframe.i new}
{intrface.get tmess}
{intrface.get xclass}
{intrface.get lnbh}
{intrface.get cust}
{intrface.get brnch}
{intrface.get tmcod}


DEFINE INPUT PARAMETER in-op-date LIKE op.op-date.
DEFINE INPUT PARAMETER oprid      AS   RECID.

{chktempl.i}

DEFINE VARIABLE fmt    AS CHARACTER             NO-UNDO.
DEFINE VARIABLE dval   LIKE op-entry.value-date NO-UNDO.
DEFINE VARIABLE fler   AS   LOGICAL             NO-UNDO.
DEFINE VARIABLE RESULT AS   INT64             NO-UNDO.
DEFINE VARIABLE msg       AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE hProc     AS HANDLE    NO-UNDO.
DEFINE VARIABLE acctkey   AS INT64   NO-UNDO.
DEFINE VARIABLE temp-acct AS CHARACTER NO-UNDO.
DEFINE VARIABLE loan_h    AS HANDLE    NO-UNDO .

DEFINE VARIABLE need-valdate AS LOGICAL FORMAT "��� �����஢����/" NO-UNDO.
DEFINE VARIABLE f-close-acct AS LOGICAL INITIAL YES                  NO-UNDO.

DEFINE VARIABLE remain-amt   LIKE op-entry.amt-rub NO-UNDO.

DEFINE BUFFER xxop      FOR op.
DEFINE BUFFER xwop      FOR wop.
DEFINE BUFFER xop-kind  FOR op-kind.
DEFINE BUFFER xop-entry FOR op-entry.

DEFINE BUFFER xperson     FOR person.
DEFINE BUFFER xcust-ident FOR cust-ident.

DEFINE NEW GLOBAL SHARED STREAM err .

DEFINE VARIABLE cod-ost      AS   CHARACTER  INITIAL ? NO-UNDO .
DEFINE VARIABLE mforeq       AS   LOGICAL              NO-UNDO.
DEFINE VARIABLE vmfo         LIKE op-bank.bank-code.
DEFINE VARIABLE vcorr-acct   LIKE op-bank.corr-acct.
DEFINE VARIABLE fl-err     AS   INT64            INITIAL -1.
DEFINE VARIABLE str_recids AS   CHARACTER          NO-UNDO.
DEFINE VARIABLE i          AS   INT64            NO-UNDO.
DEFINE VARIABLE fl_u       AS   INT64            NO-UNDO .
DEFINE VAR      h_dpsb2p      AS   HANDLE             NO-UNDO.
DEFINE VAR mSysConfDocument AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfDocType  AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfIssue    AS   CHAR    NO-UNDO. 
DEFINE VAR mSysConfDate     AS   DATE    NO-UNDO. 
DEFINE VAR mInt             AS   INT64 NO-UNDO.
/*�ਧ��� ���� 蠯�� ����*/
DEFINE VARIABLE vHeaderlog AS LOGICAL INITIAL YES NO-UNDO.

/* � ��砥, ����� ����� ����뢠���� �� �����稪�� */
DEFINE VAR mCustType        AS CHAR NO-UNDO.
DEFINE VAR mProxy           AS CHAR NO-UNDO.
DEFINE VAR mAgentID         AS CHAR NO-UNDO.
DEFINE VAR mAgentLastName   AS CHAR NO-UNDO.
DEFINE VAR mAgentFirstNames AS CHAR NO-UNDO.
DEFINE VAR mAgentDocType    AS CHAR NO-UNDO.
DEFINE VAR mAgentDocNum     AS CHAR NO-UNDO.
DEFINE VAR mAgentDocDate    AS DATE NO-UNDO.
DEFINE VAR mSingleDoc       AS CHAR NO-UNDO.
DEFINE VAR mAgentIssue      AS CHAR NO-UNDO.
DEFINE VAR mThirdPerson     AS CHAR NO-UNDO.

DEFINE VARIABLE mPodrA AS CHARACTER NO-UNDO.
DEFINE VARIABLE summa  AS   DECIMAL NO-UNDO.

/*���⪠ ���ଠ樨 � ���᫥���� ��業��*/
RUN DeleteOldDataProtocol IN h_base ("�������%%%").
RUN DeleteOldDataProtocol IN h_base ("������������").
RUN DELETEOLDDATAPROTOCOL IN h_base ("����⠫���%%%") .
/******************/


&SCOPED-DEFINE BYrole    YES .
&SCOPED-DEFINE DoLoan    YES .
&SCOPED-DEFINE Dotacct   YES.
&SCOPED-DEFINE DoOp      YES .
&GLOBAL-DEFINE mess_type '�� ��ࠡ�⪥ 㪠������� ������ ����� �ᯮ�짮���� �믮��塞�� �࠭����� .'
&GLOBAL-DEFINE undo_type RETURN "no-apply"

FUNCTION g-checkbank  RETURNS LOGICAL (INPUT  vmfo       AS CHAR,
                                       INPUT  iCodeType  AS CHAR,
                                       INPUT  vcorr-acct AS CHAR,
                                       INPUT  benacct    AS CHAR,
                                       OUTPUT result     AS INT64,
                                       OUTPUT msg        AS CHAR) IN hproc.
FUNCTION Set_type     RETURNS LOGICAL (INPUT l-type  AS CHARACTER) IN loan_h .
FUNCTION Set_ost      RETURNS LOGICAL (INPUT l-type  AS CHARACTER) IN loan_h .

RUN "g-func.p" PERSISTENT SET hProc.
RUN "l-type.p" PERSISTENT SET loan_h.

{setdest2.i
   &stream   = "stream err"
   &filename = _spool1.tmp
   &cols     = 120}

{g-currv1.i
   &OFbase  = "/*"}
{g-frame.i
   &DoFrame = YES
   &OFcash  = YES
   &row     = 1}

RELEASE dacct.
RELEASE cacct.

{chkacces.i}
{g-trig.i 
    &DoLoan = Yes
    &OFcash=*
    &Ret_Name-Ben_Doc-Name = YES
 }
/*mMultiCurr = YES - ���⨢����� �����
  mMultiCurr = NO -  ����� ����� */
{tt-multicurr.i}

GEN:
DO TRANSACTION WITH FRAME opreq ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
   {optr.i &DoBefore=YES}

   /* �����⨥ �墠�뢠�饣� ���⨢���⭮�� ������� */
   IF mMultiCurr EQ YES THEN 
   DO:   
      FIND FIRST Loan WHERE loan.cont-code EQ in-cont-code
                        AND loan.contract  EQ in-contract
                            NO-ERROR.
      IF NOT AVAILABLE Loan THEN  UNDO gen, LEAVE gen.   
      ASSIGN
         loan.close-date  = in-op-date
         loan.loan-status = '�'
         in-doc-ref = loan.doc-ref
      .
   END.

   FOR EACH tt-multicur WITH FRAME opreq
       ON ENDKEY UNDO gen, LEAVE gen 
       ON ERROR  UNDO gen, LEAVE gen: 

      FIND FIRST Loan WHERE loan.cont-code EQ tt-multicur.multicur-cont-code
                        AND loan.contract  EQ in-contract
                        NO-ERROR.
      /*㤠����� ࠭�� ����㦥��� ������⥪�*/
      RUN peb-unl.p("dps-b2p.p").

      DebugParser = INT64(GetXAttrValueEx("op-kind",
                                        op-kind.op-kind,
                                        "DebugParser",
                                        "0")).      
      {plibinit.i}
      ASSIGN
         tcur     = ?
         tacct-db = ?
         tacct-cr = ?
         tamt     = 0
         in-cont-code = tt-multicur.multicur-cont-code
         in-loan  = if avail loan then loan.doc-ref else in-loan
         in-cont-cur = if avail loan then (if loan.currency = '' then '810'
                                             else  loan.currency)
                                     else in-cont-cur          
      .

      /* ������� ⨯ ������� �� ������ */
      Set_type(in-cont-code) . 
      
      DOC:
      FOR EACH op-templ OF op-kind WHERE
               op-template.cr-class-code MATCHES "*OP*"
      NO-LOCK WITH FRAME opreq ON ENDKEY UNDO gen, LEAVE gen
                               ON ERROR  UNDO gen, LEAVE gen
      BREAK BY op-templ.op-templ:
         {bef-tran.i}
         IF op-template.op-status < "�" THEN
            f-close-acct = NO.
                           /* �᫨ � 蠫��� ����砫쭮 �⮨� ����� "�",
                           ** � �ᯮ��㥬 ��� ��� 蠫�� ��� ���� 蠫����.
                           ** ����᪠�� �� ���� �����, �� �� ᮧ���� ���㬥���. */
         IF       op-template.op-status   EQ "�"
            AND   GetXattrValueEx (
                     "op-template",
                     op-template.op-kind + "," + STRING (op-template.op-template),
                     "PrsnTmpl",
                     "���")   EQ "��"
            THEN mDsplSet = NO.
            ELSE mDsplSet = YES.
     
         need-valdate = CAN-DO("��,YES,TRUE",
                               Get_Param("��⠂��",RECID(op-templ))).
      
         IF need-valdate EQ ? THEN
            need-valdate = NO.
      
         {g-frame.i
            &DoBefore= YES
            &OFcash  = YES
            &DoTacct = YES
            &run_b2p          = YES
            &OP-UNDO = " DELETE op-entry.
                         DELETE op .
                         NEXT doc .
                       "
            &DoBeforeAfterSet = "RUN PrintHeader."
         }

         /* �஢�ઠ ������ ���⨢���⭮�� ������ */
         IF mMultiCurr EQ NO THEN 
         DO:       
            FIND FIRST loan WHERE loan.contract = "dps" AND 
                                  loan.cont-code = in-cont-code
                                  NO-ERROR.
            IF AVAILABLE loan AND loan.parent-cont-code NE '' THEN 
            DO:
               MESSAGE "����� ���⨢�����." SKIP
                       "��� ������� �⮣� ������ �롨�� �����" SKIP
                       "�࠭����� � ���⨢����� �墠�뢠�訩 �������." SKIP
                       VIEW-AS ALERT-BOX ERROR.
               UNDO gen, LEAVE gen.
            END.
         END.
         /*IF loan.end-date <= vContractDate THEN
         DO:
            MESSAGE "����� ����� ������� ����筮!"
               VIEW-AS ALERT-BOX ERROR.
            UNDO gen, LEAVE gen.
         END.*/
         RUN CreateFrmFields (?,"LoanRecid","",STRING(RECID(loan))).

         IF FIRST(op-template.op-template) THEN 
         DO:
            /* ����⨥ ��⮢ */
            Set_type(in-cont-code).
            RUN SetSysConf IN h_base ("PlacementFO_RSHB_ShowForms", "���").
            RUN g-ac4.p(in-op-date, RECID(op-kind), RECID(loan), OUTPUT fl-err, INPUT-OUTPUT summa) .
            RUN DeleteOldDataProtocol IN h_base ("PlacementFO_RSHB_ShowForms").
            IF fl-err LT 0 THEN
               UNDO gen,LEAVE gen.
         END.
         
         {g-frame.i
             &DoDisp         = YES
             &ChkBlockAction = "UNDO GEN, LEAVE GEN."
         }
      
         mforeq = op-templ.mfo-needed OR
                  (GetXAttrValue('op-template',
                                op-kind.op-kind + ',' + STRING(op-templ.op-templ),
                                '�������')  = '��').
         IF mforeq THEN
         DO:
            {g-frame.i &const-recip = Yes}
         END.   

         SSET:
         DO ON ERROR UNDO, RETRY ON ENDKEY UNDO gen, LEAVE gen:
      
            /* if op-templ.amt-rub matches '*���*'  and cod-ost ne ? then
               Set_ost(cod-ost). */
      
            IF op-templ.cr-class-code MATCHES '*kau*' THEN
            DO :
               RUN Kau-Trigger IN h_op (RECID(op-entry),
                                        OUTPUT flager,
                                        YES).
               IF flager NE 0 THEN
                  UNDO,RETRY.
      
               IF AVAILABLE op-entry THEN
                  DELETE op-entry .
      
               ASSIGN
                  op.doc-num = '���' + STRING(op-template.op-template).
               PAUSE 0 .
            END.
            IF AVAILABLE op-entry THEN
            DO :
               IF LAST(op-template.op-template) THEN
               DO:
                  remain-amt = IF wop.currency NE ? AND wop.currency NE "" THEN
                                  wop.amt-cur
                               ELSE
                                  wop.amt-rub.
                  set_type(loan.cont-code).
                  {g-frame.i
                     &DoSet  = YES
                     &OFcash = YES
                     &OFSum  = YES
                     &NODISPLAY = YES
                  }
                  
                  remain-amt = remain-amt - (IF op-entry.currency NE ?  AND
                                                op-entry.currency NE "" THEN
                                                op-entry.amt-cur
                                             ELSE
                                                op-entry.amt-rub).
               END.
               ELSE
               DO:
                  cod-ost = GetXattrValueEx ("op-template",
                                             op-template.op-kind + "," + STRING (op-template.op-template),
                                             "���_���",
                                             "").
                  IF NOT {assigned cod-ost} THEN
                  DO:
                     cod-ost = Get-Ost-templ(op-kind.op-kind,op-templ.op-templ).
                  END.
                  IF op-templ.amt-rub MATCHES '*���*' AND cod-ost <> ?  THEN
                     Set_ost(cod-ost).
                  ELSE
                     set_type(in-cont-code).
                  {g-frame.i
                     &DoSet  = YES
                     &OFcash = YES
                  }
               END.
               IF op.op-status BEGINS "�" THEN
                  ASSIGN
                     op.op-date       = ?
                     op-entry.op-date = op.op-date
                     .
               {op-entry.upd &871=yes
                  &open-undo = "UNDO gen, LEAVE gen" 
               }
                /* �������� �� ⨯� PARSEN_<�����������>*/
               {g-psigns.i}
      
               /*�᫨ ����� ����뢠���� �� ����७����*/
               IF  cacct.contract   EQ "����" THEN DO:
                   /* ��।��塞, ������ �� � �஢������ ����樨 ��-����, �஬� �������� ������ */
                   DO ON ERROR UNDO gen, LEAVE gen:
                     /* ����� */
                     RUN dps-pers.p (loan.cust-id,
                                     in-op-date,
                                     loan.cont-code,
                                     op-kind.op-kind,
                                     OUTPUT fler, OUTPUT mCustType,
                                     OUTPUT mProxy,
                                     OUTPUT mAgentID,      OUTPUT mAgentLastName, OUTPUT mAgentFirstNames,
                                     OUTPUT mAgentDocType, OUTPUT mAgentDocNum,   OUTPUT mAgentDocDate,
                                     0 ).
                     IF NOT fler THEN DO:
                       UNDO gen, LEAVE gen.
                     END.
                     fler = FALSE.
                   END.
                   /****************************************************************************/
                   /* ��।��塞 �।�塞� ���㬥�� */
                   DO ON ERROR UNDO gen, LEAVE gen:
                     /* �롮� �।�塞��� ���㬥�� */
                     IF mAgentID <> "" AND mSingleDoc = "" /* agent-id ��।����, �� ���㬥�� �� �� ��।���� */ THEN DO:
      
                        FIND FIRST xcust-ident WHERE xcust-ident.cust-cat = "�"
                                                 AND xcust-ident.cust-id  = INT64(mAgentID)
                                                NO-LOCK NO-ERROR.
                        IF AVAILABLE xcust-ident THEN DO: /* ���㬥�� �� �����⢥��� - �㦭� ����� ���짮��⥫�,
                                                             ����� �� ��ॣ����஢����� ���㬥�⮢ ������ ������� �।���? */
                           pick-value = "unknown". /* �ਧ��� ⮣�, �� ���㬥�� �� ��ࠫ� */
                           
                           RUN browseld.p ("p-cust-ident",
                                           "cust-cat~001cust-id~001close-date1~001close-date2",
                                           "�" + CHR(1) + STRING(mAgentID) + CHR(1) + "?" + CHR(1) + "?",
                                           "cust-cat~001cust-id~001close-date1~001close-date2",
                                           3).
                                           
                           IF pick-value = "unknown" THEN DO: /* �⪠� ����� ���㬥�� ���뢠�� ������ */
                             UNDO gen, LEAVE gen.
                           END.
                           mSingleDoc = pick-value.
                        END.
                     END.
                   END.
      
                   IF mSingleDoc <> ? AND mSingleDoc <> "" THEN DO:
                     FIND FIRST xcust-ident WHERE xcust-ident.cust-code-type =         ENTRY(1, ENTRY(1, mSingleDoc, "~001"), ",")
                                              AND xcust-ident.cust-code      =         ENTRY(2, ENTRY(1, mSingleDoc, "~001"), ",")
                                              AND xcust-ident.cust-type-num  = INT64(ENTRY(3, ENTRY(1, mSingleDoc, "~001"), ","))
                                            NO-LOCK NO-ERROR.
                     FIND FIRST xperson WHERE xperson.person-id = INT64(ENTRY(3, mSingleDoc, "~001"))
                                        NO-LOCK NO-ERROR.
                     mAgentID = ENTRY(3, mSingleDoc, "~001").
      
                     IF AVAILABLE xperson THEN DO:
                       mAgentLastName   = xperson.name-last.
                       mAgentFirstNames = xperson.first-names.
                     END.
                     IF AVAILABLE xcust-ident THEN DO:
                       mAgentDocType = xcust-ident.cust-code-type.
                       mAgentDocNum  = xcust-ident.cust-code.
                       mAgentDocDate = xcust-ident.open-date.
                       mAgentIssue   = xcust-ident.issue.
                       mPodrA = GetXattrValueEx("cust-ident",
                                                                  xcust-ident.cust-code-type + "," + xcust-ident.cust-code + "," + STRING(xcust-ident.cust-type-num),
                                                                 "���ࠧ�",
                                                                 "").
                       mAgentIssue = mAgentIssue + IF {assigned mPodrA} THEN (" �/� " + mPodrA)
                                                                                                         ELSE ("").   

                     END.
                     ELSE IF AVAILABLE xperson THEN 
                     DO:
                       mAgentDocType = xperson.document-id.
                       mAgentDocNum  = xperson.document.
                       mAgentDocDate = DATE(GetXAttrValue("person", STRING(xperson.person-id), "Document4Date_vid")).
                     END.
                   END.
                   ELSE DO: /* ���㬥�� �� �����-� ��稭� �� ������:
                               �饬 ���������� ���ᮭ� � �� ��� ��।��塞 ���㬥��� */
                      mAgentID = IF mCustType <> "Agent"
                                 THEN STRING(loan.cust-id)
                                 ELSE STRING(mAgentID).
                      FIND FIRST xperson WHERE xperson.person-id = INT64(mAgentID)
                                         NO-LOCK NO-ERROR.
                     IF AVAILABLE xperson THEN 
                     DO:
                        ASSIGN
                           mAgentLastName   = xperson.name-last
                           mAgentFirstNames = xperson.first-names
                           mAgentDocType = xperson.document-id
                           mAgentDocNum  = xperson.document
                           mAgentDocDate = DATE(GetXAttrValue("person", STRING(xperson.person-id), "Document4Date_vid"))
                           .
                        FIND FIRST cust-ident WHERE cust-ident.cust-cat       EQ "�"
                                                AND cust-ident.cust-id        EQ xperson.person-id
                                                AND cust-ident.Class-code     EQ "p-cust-ident"
                                                AND (   cust-ident.close-date GE in-op-date
                                                     OR cust-ident.close-date EQ ?)
                                                AND cust-ident.cust-code-type EQ xperson.document-id
                           NO-LOCK NO-ERROR.
                        IF AVAIL cust-ident THEN
                           ASSIGN
                              mAgentDocDate = cust-ident.open-date 
                              mAgentIssue   = cust-ident.issue
                              .
                              mPodrA = GetXattrValueEx("cust-ident",
                                                                        cust-ident.cust-code-type + "," + cust-ident.cust-code + "," + STRING(cust-ident.cust-type-num),
                                                                        "���ࠧ�",
                                                                        "").
                               mAgentIssue = mAgentIssue + IF {assigned mPodrA} THEN (" �/� " + mPodrA)
                                                                                                                 ELSE ("").  
                     END.
                   END.
      
                   /*------------------------------------------------------------------*/
                   /* ���������, �� ����� ����७���� ���뢠���� �����               */
                   /*------------------------------------------------------------------*/
                   IF mCustType = "Agent" 
                      AND mProxy NE "owner"
                      AND {assigned mProxy} THEN
                   DO:                
                       UpdateSigns(op.class-code, STRING(op.op),
                                   "proxy-code", mProxy, ?).
                   END.
                   /*------------------------------------------------------------------*/
                   /* ���������, ����� ���㬥�� �� �।���                         */
                   /*------------------------------------------------------------------*/
                  mSysConfDocument = GetSysConf("��ᯮ��륄���륏���ﭭ��������⥫�").
                  mSysConfDocType  = GetSysConf("��ᯮ��륄���륏���ﭭ��������⥫�.document-id").
      
                  IF  {assigned mSysConfDocument}
                     AND {assigned mSysConfDocType} THEN
      
                  DO:
                     IF INDEX(mSysConfDocument,"�뤠�") > 0 THEN
                     DO:
                        /* ��ଠ� "... �뤠� ..." */
                        mSysConfIssue = TRIM(SUBSTR(mSysConfDocument,
                                             INDEX(mSysConfDocument,"�뤠�") + 6)).
                        mSysConfDocument = TRIM(SUBSTR(mSysConfDocument, 1,
                                                INDEX(mSysConfDocument,"�뤠�") - 1)).
                        /* ��।��塞 ���� */
                        mSysConfDate = ?.
                        mInt = NUM-ENTRIES(mSysConfIssue," ").
                        mSysConfDate = DATE(ENTRY(mInt, mSysConfIssue, " ")) NO-ERROR.
                     
                        IF NOT ERROR-STATUS:ERROR THEN 
                           ASSIGN
                              ENTRY(mInt, mSysConfIssue, " ") = ""
                              mSysConfIssue = TRIM(mSysConfIssue)
                           .
                        ELSE 
                        DO:
                           mSysConfDate = DATE(ENTRY(1, mSysConfIssue, " ")) NO-ERROR.
                           IF NOT ERROR-STATUS:ERROR THEN 
                              ASSIGN
                                 ENTRY(1, mSysConfIssue, " ") = ""
                                 mSysConfIssue = TRIM(mSysConfIssue)
                              .
                           ELSE 
                              mSysConfDate = ?.
                        END.
                     
                        ASSIGN
                           mAgentDocNum  = mSysConfDocument
                           mAgentIssue   = mSysConfIssue
                           mAgentDocDate = mSysConfDate
                           mAgentDocType = mSysConfDocType
                        .
                     END.
                     ELSE 
                        ASSIGN
                           mAgentDocNum  = mSysConfDocument
                           mAgentIssue   = ""
                           mAgentDocDate = ?
                           mAgentDocType = mSysConfDocType
                        .
                  END.
                /*�᫨ ������ ᮢ��蠥��� �������楬 ������ (��ன output ��ࠬ��� 
                  dps-pers.p = owner), � ������� ४����⮢ ��࠭��� �� ���� (��� �뫮 �� 47 ��������� � VSS dvp)*/
                  IF mProxy NE "owner"
                     AND {assigned mProxy} THEN
                  DO:
                     /*�������㬥��3�. �᫨ ४����� ��⠭����� � ��, � name-send � name-ben ��࠭��� � ४����⠬� ��ᯮ��
                       ���� - ⮫쪮 ���*/
                     mThirdPerson = GetXAttrValueEx("op-kind", op-kind.op-kind, "�������㬥��3�", "���").
                     IF mThirdPerson EQ "��" THEN 
                     DO:
                        UpdateSigns(op.class-code, 
                                    STRING(op.op),
                                     "name-send",
                                     mAgentLastName + " " + mAgentFirstNames + ", " +
                                     mAgentDocType + " " + mAgentDocNum +
                                    (IF    {assigned mAgentIssue}
                                        OR mAgentDocDate <> ? THEN 
                                       (" �뤠� " + (IF {assigned mAgentIssue}
                                                     THEN mAgentIssue + " "
                                                     ELSE "")
                                                  + (IF mAgentDocDate <> ?
                                                     THEN STRING(mAgentDocDate, "99/99/9999")
                                                     ELSE "") + ".")
                                     ELSE "."),
                                     ?).
                        op.name-ben = mAgentLastName + " " + mAgentFirstNames + ", " +
                                      mAgentDocType + " " + mAgentDocNum +
                                      (IF    {assigned mAgentIssue}
                                          OR mAgentDocDate <> ? THEN 
                                       (" �뤠� " + (IF {assigned mAgentIssue}
                                                     THEN mAgentIssue + " "
                                                     ELSE "")
                                                  + (IF mAgentDocDate <> ?
                                                     THEN STRING(mAgentDocDate, "99/99/9999")
                                                    ELSE "") + ".")
                                       ELSE ".").
                  
                     END.
                     ELSE
                     DO:
                        UpdateSigns(op.class-code, 
                                    STRING(op.op),
                                    "name-send",
                                    mAgentLastName + " " + mAgentFirstNames,
                                    ?).
                        op.name-ben = mAgentLastName + " " + mAgentFirstNames.
                     END.
                     UpdateSigns(op.class-code, 
                                 STRING(op.op), 
                                 "document-id", 
                                 mAgentDocType, 
                                 ?).
                     UpdateSigns(op.class-code, 
                                 STRING(op.op),
                                 "����",
                                 mAgentDocNum +
                                 (IF    {assigned mAgentIssue}
                                     OR mAgentDocDate <> ? THEN 
                                    (" �뤠� " + (IF {assigned mAgentIssue}
                                                  THEN mAgentIssue + " "
                                                  ELSE "")
                                               + (IF mAgentDocDate <> ?
                                                  THEN STRING(mAgentDocDate, "99/99/9999")
                                                  ELSE "") + ".")
                                  ELSE "."),
                                 ?).

                     UpdateSigns(op.class-code,STRING(op.op),"���",mAgentLastName + " " + mAgentFirstNames,?).
                     UpdateSigns(op.class-code,STRING(op.op),"�����","��",?).
                     IF GetXattrValueEx("loan","proxy," + mProxy,"single-mark",?) = "��" THEN
                     DO:
                        DEFINE BUFFER proxy-loan FOR loan.
                        FIND FIRST proxy-loan WHERE 
                                   proxy-loan.contract = "proxy" AND
                                   proxy-loan.cont-code = mProxy
                           EXCLUSIVE-LOCK.
                        ASSIGN
                           proxy-loan.end-date = op.op-date
                           proxy-loan.close-date = op.op-date
                           proxy-loan.loan-status = CHR(251)
                        .
                        RELEASE proxy-loan.
                     END.
                  END.
               END.
               {crs.i}
            END.
            IF AVAILABLE op-entry OR
               CAN-FIND(FIRST xxop WHERE
                              xxop.op-transaction EQ op.op-transaction
                          AND RECID(xxop)         NE RECID(op))
            THEN
            DO:
               IF mforeq AND
                  NOT g-checkbank(vmfo,
                                  mbank-code-type,
                                  vcorr-acct,
                                  op.ben-acct,
                                  OUTPUT result,
                                  OUTPUT msg)
               THEN
               DO:
                  IF op-templ.mfo-needed THEN
                  DO:
                     MESSAGE SKIP msg SKIP(1) VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     CASE result:
                        WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN
                           NEXT-PROMPT vmfo.
                        WHEN {&egmissingcorracct} THEN
                           NEXT-PROMPT vcorr-acct.
                        OTHERWISE NEXT-PROMPT op.ben-acct.
                     END CASE.
                     UNDO, RETRY.
                  END.
                  ELSE
                  DO:
                     MESSAGE SKIP
                        "����୮ ������ ������᪨� ४������. �த������?" SKIP(1)
                        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
                        UPDATE choice AS LOGICAL.
                     IF choice <> TRUE THEN
                     DO:
                        CASE RESULT:
                           WHEN {&egmissingmfo} OR WHEN {&egmissingbank} THEN
                              NEXT-PROMPT vmfo.
                           WHEN {&egmissingcorracct} THEN
                              NEXT-PROMPT vcorr-acct.
                           OTHERWISE NEXT-PROMPT op.ben-acct.
                        END CASE.
                        UNDO, RETRY.
                     END.
                  END.
               END.
               RUN "g-bank.p" (op-kind.op-kind,
                               op-templ.op-templ,
                               op.op,
                               4,
                               OUTPUT fl-err).
               IF fl-err LT 0 THEN
                  UNDO, RETRY.
               IF ({assigned vmfo}) OR ({assigned vcorr-acct}) THEN
               DO:
                  {opbnkcr.i op.op """" ""���-9"" vmfo vcorr-acct}
                  {op-type.upd &check-format=Yes}
               END.
               {op-type.chk}
               RUN post.
            END.
      
            IF op-templ.acct-cr MATCHES '*loan-dps-*' THEN
               Set_type(in-cont-code) .
      
            IF LAST(op-templ.op-templ) THEN
            DO:
               IF remain-amt > 0 THEN
               DO:
                  FIND xop-kind WHERE
                       xop-kind.op-kind EQ loan.op-kind NO-LOCK NO-ERROR.
                  IF AVAILABLE xop-kind THEN
                  DO:
                     RUN g_sroch2.p(in-op-date,
                                    loan.cust-id,
                                    remain-amt,
                                    RECID(xop-kind),
                                    OUTPUT fl-err).
                     IF fl-err LT 0 THEN
                         UNDO gen, LEAVE gen.
                  END.
                  ELSE
                  DO:
                     MESSAGE "�� ���� ᮧ���� ���� �����, �.�. " SKIP
                        "��� �࠭���樨 ᮧ����� "  VIEW-AS ALERT-BOX ERROR.
                     UNDO gen,LEAVE gen.
                  END.
               END.
            END.
         END.
         
         {aft-temp.i 
            &aft-undo = " UNDO gen, LEAVE gen."}
      END.

      IF AVAILABLE loan THEN
      DO:
         RUN dpsclo.p (loan.cont-code,in-op-date,op-kind.op-kind).
         IF RETURN-VALUE = "YES" THEN
            RUN acctclo.p (loan.cont-code,in-op-date).
         ELSE 
            IF mMultiCurr EQ YES THEN UNDO gen,LEAVE gen.
      END.
   END.  /* tt-multicur END */

   {optr.i &DoAfter=YES}
END.

{g-print1.i}
RUN DeleteOldDataProtocol IN h_base ("��ᯮ��륄���륏���ﭭ��������⥫�.document-id").
RUN DeleteOldDataProtocol IN h_base ("��ᯮ��륄���륏���ﭭ��������⥫�.���������������㬥��").
RUN DeleteOldDataProtocol IN h_base ("��ᯮ��륄���륏���ﭭ��������⥫�").
{preview2.i
   &stream   = "stream err"
   &filename = _spool1.tmp}

HIDE FRAME opreq NO-PAUSE.

 
DELETE PROCEDURE(hProc).
DELETE PROCEDURE(loan_h) .
IF VALID-HANDLE (h_dpsb2p) THEN
   DELETE PROCEDURE(h_dpsb2p).
{plibdel.i}

/* �������� �� ��᫥ �믮������ �࠭���樨 */
{cmd-exec.i
   &cmd        = "'Postcmd'"
}
{intrface.del}          /* ���㧪� �����㬥����. */ 
   
RETURN.

PROCEDURE PrintHeader:

   IF vHeaderLog THEN
   DO:
      {for_ved.i
         &NO_DEF_STREAM = YES
         &ContractDate  = vContractDate}
      vHeaderLog = NO.
   END.
   IF op-template.amt-rub MATCHES '*��������*' THEN
      PUT STREAM err '������ ࠭�� ���᫥���� ��業⮢' SKIP(1).
   ELSE
   IF op-template.amt-rub MATCHES '*������*' THEN
      PUT STREAM err '�����᫥��� ��業⮢ �� ���������� �⠢��' SKIP(1).

END PROCEDURE.

PROCEDURE post.

   {xttrentr.i
      &OP-BUF       = op
      &OP-TEMPL-BUF = op-template}
   IF AVAILABLE op THEN
   DO:
      {additem.i str_recids STRING(RECID(op))}
   END.
   IF tcur = ? THEN
      tcur = op-entry.currency.
   ASSIGN
      wop.op-recid = RECID(op-entry)
      wop.acct-db  = op-entry.acct-db
      wop.acct-cr  = op-entry.acct-cr
      wop.currency = op-entry.currency
      wop.amt-cur  = IF op-entry.currency <> "" THEN
                        op-entry.amt-cur
                     ELSE
                        op-entry.amt-rub
      wop.amt-rub  = op-entry.amt-rub
      .
END PROCEDURE.
