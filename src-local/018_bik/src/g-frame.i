/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: G-FRAME.I
      Comment: �ଠ ����� ���㬥��
   Parameters: ���
         Uses:
      Used by:
      Created: ...
     Modified: 06/12/1998 Olenka - ������⥫�� ������. ����筠� ��� ��
                                   ����� ������ �� ����஥筮�� ��ࠬ���
                                   ��⠇�.
     Modified: 03.12.2001 12:04 SEMA     �� ��� 0003724
     Modified: 08/12/2001 shin �������� ��⮪����� ����� + ����������� �������᪨ ��� ��������
     modified: 27/02/2002 kostik 0001323 ���� op-entry.amt-rub � op-entry.amt-cur ���樠����஢����� �ࠧ�
                                         ��᫥ �맮�� parssen, �� ���﫮 �� �⮡ࠦ���� ���⪠ �� ��⠬
                                         ����� � �।��. ����� ।���஢���� ���� �१ ⠡���� WOP.
     modified: 29/06/2002 kostik 0003150 (0008150)
                                 ��� ࠧ�襭�� ।���஢���� �㬬� � �����
                                 "doc-templ" �������� ���. ४�����.
                                 "EditAmtFlag". ����� �᫨ �㬬� ��।����� �
                                 蠡���� ��� ����஬, � ।���஢����
                                 �����஢�����, ⥯��� ����� ࠧ����,
                                 ��⠭���� ४����� � ���祭�� "��"
     modified: 23/09/2002 kraw (0008428) - ��ࠡ�⪠ "EditAmtFlag" � � ��砥, �᫨ OFSum �� ��⠭�����,
                                           �� �� 㬮�砭�� "��"
     Modified: 11.11.2002 15:26 SEMA     �� ��� 0011932 ���, �ᯮ�짮���訩 ⠡���� frm-field �뭥ᥭ � frmfield.fun
     Modified: 22.11.2002 20:43 rija     1734
     Modified: 17.01.2003 16:15 kavi     0013352  㢥��祭 �ଠ� ���� in-cont-code �� 60 � �������� �஫����
     Modified: 05.02.2003 11:10 KAVI
     Modified: 9/02/2004 fEAk �. 13798 - ����������� �㭪樮���쭮���, ��� ࠡ��� ����. ���� � 
                                          ��室�� ��� � ��� ����権 �� ���㦨����� 䨧. ���.               
     Modified: 09.02.2004 18:32 FEAK
     Modified: 04.03.2004 10:24 rija
     Modified: 04.03.2004 10:45 rija     24138: ����������� ���ᮢ�� ������ "�ਥ� �������
                                         �������� �।�� �� ���. ���".
     Modified: 07.07.2004       abko     19280: ����� �㬥��� ���㬥�⮢, �᫨ ���� op.op 
                                         ����� 1000000.
     Modified: 22/09/2004  Om    ��ࠡ�⪠, ��ࠡ�⪠ 䫠�� ����砭�� �࠭���樨.
     Modified: 07.04.2005 12:26 SAP
     Modified: 23.06.2006 15:54 fEAk     <comment>
     Modified: 22/05/2009 kraw (0102904) �����⭠� ��⮭㬥���
     Modified: 06.10.2009 19:03 ksv      (0118168) QBIS ������. ��ࠢ����� 
     Modified: 12.08.2010 19:57 ksv      (0127883) QBIS ��ࠡ�⪠ �।. ��ࠢ�����
*/

&IF DEFINED (VokTitle) = 0 &THEN
&GLOBAL-DEFINE VokTitle
{voktitle.i}
&ENDIF
RUN id_vok_just (OUTPUT vChIdVokJust).

&IF DEFINED(DoFrame) &THEN
   DEF VAR fl_SearchOP     AS LOGICAL NO-UNDO.
   DEF VAR fl_editOP       AS LOGICAL NO-UNDO.
   DEF VAR atempl          AS CHARACTER NO-UNDO.   
   DEF VAR dt-zo           AS DATE   NO-UNDO.
   DEF VAR fl_edit_rub     AS LOG    NO-UNDO.
   DEF VAR fl_edit_cur     AS LOG    NO-UNDO.
   DEF VAR flag-edit-summ  AS LOG    NO-UNDO.
   DEF VAR flag-edit-acct  AS LOG    NO-UNDO.
   DEF VAR mDataZoStr      AS CHAR   NO-UNDO.
   DEF VAR mDsplSet        AS LOG    NO-UNDO
                           INIT YES. 
   DEF VAR mSetCntxt4Loan  AS LOG    NO-UNDO. /* ���� �।��⠭���� ���⥪�� �� LOAN. */
   DEF VAR mKpp-rec       AS CHARACTER FORMAT "x(9)" NO-UNDO.

   DEFINE BUFFER bOp-templ FOR op-template.

   {objopkind.def}      /* ���⥪�� ��ꥪ�. */
                        /* ����祭�� ���⥪�� ��ꥪ�.
                        ** ��� ����, �᫨ �࠭����� ����饭� �� ��ꥪ�. */
   PUBLISH "GetObjectContent" (OUTPUT DATASET dsObjOpKind).

   &IF DEFINED (DoLoan) &THEN
      FOR FIRST ttObjLoan:
         ASSIGN 
            in-contract    = ttObjLoan.contract    WHEN  LENGTH (ttObjLoan.contract)   >  0
            in-cont-code   = ttObjLoan.cont-code   WHEN  LENGTH (ttObjLoan.cont-code)  >  0
            in-doc-ref     = ttObjLoan.doc-ref     WHEN  LENGTH (ttObjLoan.doc-ref)    >  0
            in-cont-cur    = ttObjLoan.currency    WHEN  LENGTH (ttObjLoan.currency)   >  0
            mSetCntxt4Loan = YES
         .
      END.
   &ENDIF
   FOR FIRST ttObjOp:
      IF ttObjOp.contract-date <> ? THEN
         vContractDate = ttObjOp.contract-date.
   END.

   mDataZoStr = FGetSetting("��⠇�",?,"").
   dt-zo = if mDataZoStr <> "" 
           then date(INT64(substr(mDataZoStr,4,2)),
                     INT64(substr(mDataZoStr,1,2)),
                     year(in-op-date))
           else date(3,31,year(in-op-date)).

   &IF DEFINED(browse-entry) &THEN
      DEFINE QUERY q1 FOR op-entry.
      DEFINE BROWSE b1 QUERY q1
      DISPLAY
         op-entry.acct-db
         op-entry.acct-cr
         op-entry.currency
         op-entry.amt-cur
         op-entry.amt-rub
         /* enable all */
         WITH 3 DOWN WIDTH 76 TITLE "[ �������� ]" SEPARATORS
            NO-ROW-MARKERS.
   &ENDIF

   FORM
      &IF DEFINED(DoLoan) &THEN
      "����� ��������" /*in-cont-code*/ in-doc-ref FORMAT "x(60)" VIEW-AS FILL-IN SIZE 25 BY 1
      "�������� ����" vContractDate
      in-cont-cur-label FORMAT "x(3)" VIEW-AS FILL-IN SIZE 3 BY 1 
      in-cont-cur FORMAT "x(3)" VIEW-AS FILL-IN SIZE 3 BY 1
      SKIP
      &ENDIF
      "��������" op.doc-type ":" doc-type.name "N" op.doc-num "��" at 64 op.doc-date at 69 SKIP
      need-valdate op.op-value-date "����㯨�" op.ins-date    
      &IF DEFINED(OFcash) = 0 &THEN
      "���.����."  AT 51  op.order-pay FORMAT "x(2)"  HELP "��।����� ���⥦�" at 61
      "�ப"  op.due-date HELP "�ப ���⥦�" SKIP
      &ENDIF
      &IF DEFINED(DoLoan) &THEN
      &IF DEFINED(NO_LOAN_LABEL) = 0 &THEN
      in-loan-label FORMAT "x(7)" in-loan FORMAT "x(20)" VIEW-AS FILL-IN SIZE 20 BY 1 SKIP
      &ENDIF
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
      "������������������������������������������������������������������������������"
      "�����:" op-entry.acct-db  "�������:" AT 38 op-entry.acct-cr SKIP
      " " name-db[1] "� " AT 38 name-cr[1] SKIP
      " " name-db[2] "� " AT 38 name-cr[2] SKIP
      "�������:" bal-db LIKE acct-pos.balance "��������:" AT 38 bal-cr LIKE acct-pos.balance SKIP
      "������������������������������������������������������������������������������"
      "���� ��   " "���" "{&in-ua-amtfc}     {&in-uf-amtncn}" &IF DEFINED(ONqty) <> 0 &THEN "  ���-��" &ENDIF "��" "��" "��" SKIP
      op-entry.value-date HELP "��� ����, �� ������ ����⠭ ���������� � ���. �����"
      &IF DEFINED(casho) <> 0 &THEN
         op-entry.currency AT 12 op-entry.amt-cur format ">>>,>>>,>>>,>>9.999" AT 17 op-entry.amt-rub format ">>>,>>>,>>>,>>9.999" AT 38
      &ELSE
         op-entry.currency AT 12 op-entry.amt-cur AT 17 op-entry.amt-rub
      &ENDIF
      &IF DEFINED(ONqty) <> 0 &THEN
      op-entry.qty format ">>>>9.99"
      &ENDIF
      &IF DEFINED(g_cash1_sym) <> 0 &THEN
      mSymbol
      &ELSE 
      op-entry.symbol
      &ENDIF
      op-entry.prev-year op-entry.op-cod format "x(6)" SKIP
      &ENDIF
      &IF DEFINED(likeDiaSoft) = 0 &THEN
      &IF DEFINED(NOben) = 0 &THEN
      "������:" op.name-ben VIEW-AS FILL-IN SIZE 37 BY 1 "���:" op.inn VIEW-AS FILL-IN SIZE 12 BY 1 
      "���:" mKpp-rec VIEW-AS FILL-IN SIZE 9 BY 1
      &ENDIF
      &ENDIF
      &IF DEFINED(NOmfo) = 0 &THEN
      "�[" doc-kind format "x(18)" "]�������������������������������������������������������"
      "���:" vmfo help "�����䨪�樮��� ��� �����" bank1.name FORMAT "x(47)" AT 32 SKIP
      "�/�:" vcorr-acct bank2.name FORMAT "x(47)" AT 32 SKIP
      "�/�:" op.ben-acct
      &ENDIF
      &IF DEFINED(likeDiaSoft) &THEN
      &IF DEFINED(NOben) = 0 &THEN
      SKIP "������:" op.name-ben VIEW-AS FILL-IN SIZE 37 BY 1 
      "���:" op.inn VIEW-AS FILL-IN SIZE 12 BY 1 
      "���:" mKpp-rec VIEW-AS FILL-IN SIZE 9 BY 1
      &ENDIF
      &ENDIF
      "�[ ���������� �������� ]�����������������[ F1, F3-�।., F4-᫥�., CTRL-F10 ]�"
      op.details VIEW-AS EDITOR INNER-CHARS 78 
      INNER-LINES 3
      &IF DEFINED(browse-entry) &THEN
      "������������������������������������������������������������������������������"
      b1
      &ENDIF
   WITH FRAME opreq 1 DOWN OVERLAY CENTERED NO-LABEL ROW {&row} TITLE COLOR bright-white
   "[ �������� : " + op-kind.name + "; �� " + STRING(in-op-date, "99/99/99") + vChIdVokJust + " ]".

   &IF DEFINED(cycle) <> 0 &THEN
      wh_opr = frame opreq:handle. /* ��ᢠ���� 㪠��⥫� �� �३� ���㬥�� */
   &ENDIF

&ELSEIF DEFINED(DoDisp) &THEN
   &IF DEFINED(SESSION-REMOTE) > 0
   &THEN
      &GLOB NODISPLAY YES
   &ENDIF

   &IF DEFINED(ChkBlockAction) = 0 &THEN
      {chkblock.i
         &surr   = STRING(in-op-date)
         &msg    = "�� �� ����� �ࠢ� ࠡ���� � �������஢����� ����樮���� ���!"
         &action = "PAUSE. HIDE FRAME opreq. UNDO cycle, LEAVE cycle."
      }
   &ELSE
      {chkblock.i
         &surr   = STRING(in-op-date)
         &msg    = "�� �� ����� �ࠢ� ࠡ���� � �������஢����� ����樮���� ���!"
         &action = "{&ChkBlockAction}"
      }
   &ENDIF
   
   IF op-template.doc-type BEGINS "(" THEN DO:               
      
      atempl = TRIM(op-template.doc-type,"(").
      atempl = TRIM(atempl,")").

      FIND FIRST bOp-templ WHERE bOp-templ.op-kind =  op-templ.op-kind AND 
                                 bOp-templ.op-templ =  INT64(atempl) NO-ERROR.
      op.doc-type = ENTRY(1,bOp-templ.doc-type).
          
   END.

   DISPLAY
      &IF DEFINED(DoLoan) &THEN
         /*in-cont-code*/ in-doc-ref
         vContractDate
         in-cont-cur
      &ENDIF
      ENTRY(1, op-templ.doc-type) WHEN NUM-ENTRIES(op-templ.doc-type) > 1 AND NOT (AVAIL(bop-templ) AND op-template.doc-type BEGINS "(") @ op.doc-type
      op.doc-type WHEN op.doc-type <> "" and (num-entries(op-templ.doc-type) = 1 OR (AVAIL(bop-templ) AND op-template.doc-type BEGINS "("))
      doc-type.name WHEN AVAIL doc-type
      op.doc-num  WHEN op.doc-num <> "" AND op.doc-num:SCREEN-VALUE IN FRAME opreq =  ""
      op.doc-num:SCREEN-VALUE IN FRAME opreq WHEN op.doc-num  =  "" AND op.doc-num:SCREEN-VALUE IN FRAME opreq <> ""
      op.doc-date WHEN op.doc-date <> ?
      op.ins-date
      need-valdate
      op.op-value-date  WHEN need-valdate
      &IF DEFINED(OFcash) = 0 &THEN
         op.order-pay
         op.due-date
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
          &IF DEFINED(ONqty) &THEN
              "" @ op-entry.qty
             op-entry.qty when need-qty
          &ENDIF
      &ENDIF
      &IF DEFINED(NOben) = 0 &THEN
         op.name-ben WHEN op.name-ben <> ?
         &IF DEFINED(SESSION-REMOTE) &THEN
            op.name-ben:INPUT-VALUE WHEN op.name-ben = ? @ op.name-ben
         &ENDIF
         op.inn WHEN op.inn <> ?
         &IF DEFINED(SESSION-REMOTE) &THEN
            op.inn:INPUT-VALUE WHEN op.inn = ? @ op.inn
         &ENDIF
         mKpp-rec WHEN mKpp-rec <> ?
         &IF DEFINED(SESSION-REMOTE) &THEN
            mKpp-rec:INPUT-VALUE WHEN mKpp-rec = ? @ mKpp-rec
         &ENDIF
      &ENDIF
      &IF DEFINED(browse-entry) = 0 &THEN
         op-entry.acct-db WHEN op-entry.acct-db <> ?
         op-entry.acct-cr WHEN op-entry.acct-cr <> ?
         wop.amt-cur      WHEN AVAIL wop AND wop.currency <> "" AND wop.amt-cur <> ? @ op-entry.amt-cur
         0                WHEN AVAIL wop AND wop.currency <> "" AND wop.amt-cur =  ? @ op-entry.amt-cur
         wop.amt-rub      WHEN AVAIL wop AND wop.amt-rub <> ?    @ op-entry.amt-rub
         0                WHEN AVAIL wop AND wop.amt-rub =  ?    @ op-entry.amt-rub
         op-entry.amt-cur WHEN NOT AVAIL wop AND op-entry.currency <> ""
         op-entry.amt-rub WHEN NOT AVAIL wop
         op-entry.value-date
         op-entry.currency WHEN op-entry.currency <> ?
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol WHEN op-templ.symbol <> "-"
         &ELSE
         op-entry.symbol WHEN op-templ.symbol <> "-"
         &ENDIF
         op-entry.op-cod
      &ENDIF
      op.details
      &IF DEFINED(Nomfo) = 0 &THEN
         vcorr-acct vmfo
         op.ben-acct
         doc-kind
      &ENDIF
   .
   &IF DEFINED(browse-entry) &THEN
      OPEN QUERY q1 FOR EACH op-entry OF op.
      ENABLE b1 WITH FRAME opreq.
   &ENDIF

   /* ����� */
   &IF DEFINED(cycle) <> 0 &THEN
      {g-cycle.dsp}
   &ENDIF

&ELSEIF DEFINED(DoSet) &THEN
   &IF DEFINED(DoLoan) &THEN
      IF in-doc-ref >  '' THEN
      DO WITH FRAME opreq:
         vContractDate:SENSITIVE = YES.
         vContractDate:READ-ONLY = YES.
         vContractDate:PFCOLOR   = 3.
      END.
   &ENDIF
   RUN InitFormatFrameFields IN h_xclass(FRAME opreq:HANDLE, "", "op-template", op-kind.op-kind + "," + STRING(op-templ.op-templ)).
   &IF DEFINED(browse-entry) = 0 &THEN
      IF  {assigned op-entry.currency}
      AND NOT CAN-FIND(FIRST acct WHERE acct.acct     =       op-entry.acct-cr
                                    AND acct.currency =       ""
                                    AND acct.contract MATCHES "*����*")
      AND NOT CAN-FIND(FIRST acct WHERE acct.acct     =       op-entry.acct-db
                                    AND acct.currency =       ""
                                    AND acct.contract MATCHES "*����*")
      THEN DO:
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol = "" .
         DISPLAY mSymbol WITH FRAME opreq.
         &ELSE 
         op-entry.symbol = "" .
         DISPLAY op-entry.symbol WITH FRAME opreq.
         &ENDIF
      END.
   &ENDIF
   flag-edit-acct = GetXAttrValueEx("op-template",
                                    op-template.op-kind + "," + STRING(op-template.op-template),
                                    "EditAcctFlag", &IF DEFINED(EdtFlg) =  0 &THEN "���" &ELSE {&EdtFlg} &ENDIF) =  "��".
   flag-edit-summ = GetXAttrValueEx("op-template",
                                    op-template.op-kind + "," + STRING(op-template.op-template),
                                    "EditAmtFlag",
                                    &IF DEFINED(Ofsum) =  0 &THEN 
                                    &IF DEFINED(EdtFlg) =  0 &THEN "���" &ELSE {&EdtFlg} &ENDIF
                                    &ELSE "��" &ENDIF
                                   ) =  "��".
   &IF DEFINED(Ofsum) =  0 &THEN
      ASSIGN
         fl_edit_rub = (AVAIL wop AND (wop.amt-rub =  ? OR wop.amt-rub =  0)) OR
                       (NOT AVAIL wop AND (op-entry.amt-rub =  ? OR op-entry.amt-rub =  0)) OR
                       flag-edit-summ.
         fl_edit_cur = (AVAIL wop AND (wop.amt-cur =  ? OR wop.amt-cur =  0)) OR
                       (NOT AVAIL wop AND (op-entry.amt-cur =  ? OR op-entry.amt-cur =  0)) OR
                       flag-edit-summ
      .
   &ELSE
      ASSIGN
         fl_edit_rub = NO
         fl_edit_cur = NO
      .
   &ENDIF
   
   fl_editOP = &IF DEFINED(Regim-OneDoc) =  0 
               &THEN YES 
               &ELSE fl_SearchOP =  NO 
               &ENDIF.

   /* �ਭ㤨⥫쭮 ������ �뢮� ����� ���㬥�� � ��।����, �⮡� SET
   ** ��࠭�� �� ��������� � QBIS  */
   &IF DEFINED( MANUAL-REMOTE ) &THEN  
   IF NOT RETRY THEN DISPLAY op.doc-num op.order-pay WITH FRAME opreq.
   &ENDIF
                        /* �� �ॡ������ ���짮��⥫� ���� ������ ����� �� �ந���������. */
   IF mDsplSet THEN
   DO:
      IF AVAIL(op) THEN
      DO:
         FIND FIRST code WHERE 
                  code.class =  "acct-cat"
            AND   code.code  =  op.acct-cat
         NO-LOCK NO-ERROR.
         IF AVAIL(code) 
            AND   code.description[3] <> "���" 
         THEN mNeedZO = YES. 
      END.
      SET
         op.doc-type WHEN fl_editOP AND (op.doc-type = "" or num-entries(op-templ.doc-type) > 1)
         op.doc-num  WHEN fl_editOP 
         &IF DEFINED(opreq-OP-DOC-NUM-SET) &THEN
            AND ( {&opreq-OP-DOC-NUM-SET})
         &ENDIF
         op.doc-date WHEN fl_editOP
         op.op-value-date WHEN fl_editOP AND need-valdate
         op.ins-date      WHEN fl_editOP
         &IF DEFINED(OFcash) = 0 &THEN
            op.order-pay  WHEN fl_editOP
            op.due-date   WHEN fl_editOP
         &ENDIF
         &IF DEFINED(browse-entry) = 0 &THEN
            op-entry.acct-db WHEN op-entry.acct-db = ? OR op-entry.currency = ? OR flag-edit-acct
            op-entry.acct-cr WHEN op-entry.acct-cr = ? OR op-entry.currency = ? OR flag-edit-acct
            op-entry.value-date WHEN op-templ.ent-value-date = ?
            op-entry.currency WHEN op-templ.currency = ? AND op-entry.currency = ?
            op-entry.amt-cur  WHEN (op-entry.currency = ? OR op-entry.currency <> "")
                                   &IF DEFINED(Ofsum) =  0 &THEN
                                   AND fl_edit_cur
                                   &ENDIF
            op-entry.amt-rub &IF DEFINED(Ofsum) =  0 &THEN WHEN fl_edit_rub &ENDIF
            &IF DEFINED(ONqty) &THEN
               op-entry.qty when need-qty
            &ENDIF
            &IF DEFINED(g_cash1_sym) <> 0 &THEN
               mSymbol WHEN op-templ.symbol <> "-"
            &ELSE
               op-entry.symbol WHEN op-templ.symbol <> "-"
            &ENDIF
            op-entry.prev-year when (op-entry.op-date <= dt-zo and mNeedZO)
            op-entry.op-cod when op-entry.op-cod =  ?
         &ENDIF
         &IF DEFINED(likeDiaSoft) = 0 &THEN
            &IF DEFINED(NOben) = 0 &THEN
               op.name-ben WHEN fl_editOP AND (
                                &IF DEFINED(NOCheck) =  0 &THEN
                                   (op.name-ben = ? or op.name-ben = "") AND
                                &ENDIF
                            NOT transit
                            AND mRecip-data =  "")
               op.inn      WHEN fl_editOP AND (
                                &IF DEFINED(NOCheck) =  0 &THEN
                                   (op.inn = ? or op.inn = "" ) AND
                                &ENDIF
                            NOT transit
                            AND mRecip-data =  "")
               mKpp-rec    WHEN fl_editOP AND (
                                &IF DEFINED(NOCheck) =  0 &THEN
                                   (mKpp-rec = ? or mKpp-rec = "" ) AND
                                &ENDIF
                            NOT transit
                            AND mRecip-data =  "")
            &ENDIF
         &ENDIF
         &IF DEFINED(Nomfo) = 0 &THEN
            vmfo WHEN mforeq
               &IF DEFINED(NOCheck) =  0 &THEN
                  AND (TRIM(vmfo,"0") = "" OR RETRY) AND NOT transit
                  AND mRecip-data =  ""
               &ENDIF
            vcorr-acct WHEN mforeq AND
                       &IF DEFINED(NOCheck) =  0 &THEN
                          (vcorr-acct = ? or vcorr-acct = "" OR RETRY) AND
                       &ENDIF
                       NOT transit
            op.ben-acct WHEN fl_editOP AND (
                        mforeq  and
                        &IF DEFINED(NOCheck) =  0 &THEN
                           (op.ben-acct = ? or op.ben-acct = "") and
                        &ENDIF
                        NOT transit
                        AND mRecip-data =  "")
         &ENDIF
         &IF DEFINED(likeDiaSoft) &THEN
            &IF DEFINED(NOben) = 0 &THEN
               op.name-ben WHEN fl_editOP AND (&IF DEFINED(NOCheck) =  0 &THEN
                                   (op.name-ben = ? or op.name-ben = "") AND
                                &ENDIF
                            NOT transit)
               op.inn      WHEN fl_editOP AND (&IF DEFINED(NOCheck) =  0 &THEN
                                   (op.inn = ? or op.inn = "" )  and
                                &ENDIF
                            NOT transit)
               mKpp-rec    WHEN fl_editOP AND (
                                &IF DEFINED(NOCheck) =  0 &THEN
                                   (mKpp-rec = ? or mKpp-rec = "" ) AND
                                &ENDIF
                            NOT transit
                            AND mRecip-data =  "")
            &ENDIF
         &ENDIF
         op.details WHEN fl_editOP AND not transit
      .
   END.
   &IF DEFINED(Ofsum) =  0 AND DEFINED(browse-entry) = 0 &THEN
      ASSIGN
         op-entry.amt-rub = wop.amt-rub WHEN AVAIL wop AND NOT fl_edit_rub
         op-entry.amt-cur = wop.amt-cur WHEN AVAIL wop AND NOT fl_edit_cur
         wop.amt-rub      = op-entry.amt-rub WHEN AVAIL wop AND fl_edit_rub
         wop.amt-cur      = op-entry.amt-cur WHEN AVAIL wop AND fl_edit_cur
      .
   &ENDIF

   &IF DEFINED(DoLoan) &THEN
      IF in-doc-ref >  '' THEN
      DO WITH FRAME opreq:
         vContractDate:SENSITIVE = NO.
      END.
   &ENDIF

&ELSEIF DEFINED(DoBefore) &THEN

   CLEAR FRAME opreq NO-PAUSE.

   COLOR DISPLAY BRIGHT-GREEN
      name-db name-cr doc-type.name
      &IF DEFINED(NOmfo) = 0 &THEN
         bank1.name bank2.name
      &ENDIF
   WITH FRAME opreq.

   &IF DEFINED(NOmfo) = 0 &THEN
      COLOR DISPLAY input
         doc-kind
      WITH FRAME opreq.
   &ENDIF

   &IF DEFINED(DoLoan) &THEN
      IF vContractDate =  ?
         THEN vContractDate = in-op-date.
   &ENDIF

   COLOR DISPLAY bright-white
      &IF DEFINED(DoLoan) &THEN
         /*in-cont-code*/ in-doc-ref
         in-cont-cur
         vContractDate
      &ENDIF
      op.doc-type op.doc-num op.doc-date op.ins-date
      &IF DEFINED(browse-entry) = 0 &THEN
         op-entry.acct-db op-entry.acct-cr op-entry.amt-cur
         op-entry.amt-rub 
         &IF DEFINED(g_cash1_sym) <> 0 &THEN
         mSymbol
         &ELSE
         op-entry.symbol
         &ENDIF
         op-entry.op-cod
      &ENDIF
      &IF DEFINED(NOmfo) = 0 &THEN
         vmfo vcorr-acct op.ben-acct
      &ENDIF
      &IF DEFINED(NOben) = 0 &THEN
         op.name-ben
      &ENDIF
      op.details
   WITH FRAME opreq.

   {get-fmt.i &nodeffmt="/*" &obj='" + op-templ.acct-cat + ""-Acct-Fmt"" + "'}
   &IF DEFINED(browse-entry) = 0 &THEN
      ASSIGN
         op-entry.acct-db:FORMAT IN FRAME opreq = fmt
         op-entry.acct-cr:FORMAT IN FRAME opreq = fmt
         cur-op-date = in-op-date
         &IF DEFINED(NOmfo) = 0 &THEN
            vmfo        = ""
            vcorr-acct  = ""
         &ENDIF
         rate-date   = in-op-date
      .
      &IF DEFINED(ONqty) <> 0 &THEN
         op-entry.qty:format = ">>>>9.99".
         IF need-qty THEN DO:
            FIND FIRST signs WHERE signs.FILE-NAME = "op-template" AND
                             signs.surrogate = op-kind.op-kind + "," + STRING(op-templ.op-templ) AND
                             signs.CODE = "QtyFormat" NO-LOCK NO-ERROR.
            IF AVAIL signs THEN DO:
               op-entry.qty:format = signs.xattr-val no-error.
               IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                  MESSAGE "�������⨬� �ଠ� ��� ������⢠. �ᯮ������ �⠭�����...".
                  PAUSE 3 NO-MESSAGE.
               END.
            END.
         END.
         op-entry.qty:col    = 66 - length(op-entry.qty:format).
      &ENDIF
   &ENDIF

   &IF DEFINED(nocreate) = 0 &THEN

      &IF DEFINED(Regim-OneDoc) <> 0 &THEN
         fl_SearchOP = NO.
         IF op-template.doc-type BEGINS "(" THEN DO:  

            atempl = TRIM(op-template.doc-type,"(").
            atempl = TRIM(atempl,")").

            FIND FIRST wop WHERE wop.op-templ =  INT64(atempl) NO-ERROR.
            FIND op WHERE RECID(op) =  wop.op-recid NO-LOCK NO-ERROR.
            IF AVAIL(op) AND CAN-FIND(FIRST signs WHERE signs.file-name  =  "op"
                                                    AND signs.code       =  "op-bal"
                                                    AND (signs.code-value EQ STRING(op.op) OR signs.xattr-value EQ STRING(op.op)))
            THEN DO : 
               /*�뫠 ���⠭���� �� ����⥪� */
               RELEASE op.           
            END.    
            IF AVAIL op THEN
            fl_SearchOP = YES.
         END.
      &ENDIF
      IF &IF DEFINED(Regim-OneDoc) =  0 
         &THEN YES 
         &ELSE fl_SearchOP = NO &ENDIF 
      THEN DO:
         fl_editOP = YES.
      CREATE op.
      {op(sess).cr}
      &IF DEFINED(op-doc-num-format) =  0 &THEN
         &SCOPED-DEFINE op-doc-num-format op.doc-num:FORMAT
      &ENDIF
      mKpp-rec = "".

      &IF DEFINED(pl-date) &THEN
         {g-op.ass &pl-date=&pl-date}
      &ELSE
         {g-op.ass}
      &ENDIF
      END.
      
      
   &ENDIF
   IF AVAIL op THEN
   FIND doc-type OF op NO-LOCK NO-ERROR.

   {frmfield.fun &DefDeleteTempTable = YES
      &WHERE = "WHERE frm-fields.op-templ = op-templ.op-templ"}

   &IF DEFINED(nocreate) = 0 &THEN
      CREATE op-entry.

      &IF DEFINED(ORACLE) &THEN
         VALIDATE op NO-ERROR.
      &ENDIF
      {g-en.ass}
      ASSIGN
         op-entry.value-date = in-op-date.
         op-entry.currency = IF op-templ.currency <> ? THEN GetCurr(op-templ.currency) ELSE op-entry.currency.
      IF op-entry.currency <> ? THEN 
         tcur = op-entry.currency.
   &ENDIF

   &IF DEFINED(NOmfoTotal) = 0 &THEN
   &IF DEFINED(NOmfo) = 0 &THEN
      {g-bankv1.i &kpp=mKpp-rec}
      DEF VAR bank-cnt AS INT64 NO-UNDO.
      DEF VAR is-rec   AS LOGICAL INIT NO NO-UNDO.
      DEF VAR is-send  AS LOGICAL INIT NO NO-UNDO.
      ASSIGN
         is-rec  = NO
         is-send = NO
      .
      DO bank-cnt = 1 TO NUM-ENTRIES(op-templ.bank-op-templ) BY 4:
         IF      ENTRY(bank-cnt, op-templ.bank-op-templ) = "rec"  THEN is-rec  = YES.
         ELSE IF ENTRY(bank-cnt, op-templ.bank-op-templ) = "send" THEN is-send = YES.
      END.
      IF is-rec OR NOT is-send THEN
      ASSIGN
         op.doc-kind = "rec"
         doc-kind    = {&rec-label}
      .
      ELSE
      ASSIGN
         op.doc-kind = "send"
         doc-kind    = {&send-label}
      .
   &ELSE
      {g-bankv1.i &OFmfo="/*" &kpp=mKpp-rec}
   &ENDIF
   &ENDIF

   &IF DEFINED(nocreate) = 0 &THEN
      FOR EACH wop WHERE wop.op-templ >= op-templ.op-templ:
         DELETE wop.
      END.
   &ENDIF
   &IF DEFINED(DoLoan) &THEN
      DISPLAY
         /*in-cont-code*/ in-doc-ref
         vContractDate
         in-cont-cur
      .
      SET
         in-doc-ref      WHEN in-doc-ref   =  ''
         /* in-cont-code when in-cont-code eq ''*/
         vContractDate   WHEN /*in-cont-code*/ in-doc-ref =  '' OR
                              op-templ.op-templ =  1
         in-cont-cur
            WHEN in-cont-cur = '' OR in-cont-cur = ?  

        .
        IF AVAIL op THEN op.contract-date = vContractDate.
   &ENDIF

   /* ����⢨� ��᫥ ��᢮���� �������㠫�� ��� ������ ��楤��� */
   {&DoBeforeAfterSet}
   &IF DEFINED(nocreate) = 0 &THEN
      CREATE wop.
      ASSIGN
         wop.acct-db  = op-template.acct-db  /* ���樠������ �� 蠡����. */
         wop.acct-cr  = op-template.acct-cr  /* ���樠������ �� 蠡����. */
         wop.currency = op-entry.currency
         dval         = op-entry.value-date
         wop.op-templ = op-templ.op-templ
         wop.op-kind  = op-kind.op-kind.
      &IF DEFINED(DoOp) &THEN
      ASSIGN wop.op-recid = RECID(op-entry)
         wop.con-date = vContractDate
         wop.op-kind  = op-kind.op-kind.
      &ENDIF
   &ENDIF
  
   &IF DEFINED(DoLoan) &THEN
      IF /*in-cont-code*/ /*in-doc-ref*/ in-loan <> "" AND /*in-cont-code*/ /*in-doc-ref*/ in-loan
      <> ? THEN
      DO:
         FIND loan WHERE loan.filial-id =  shFilial
                     AND loan.contract  =  in-contract
                     AND /*loan.cont-code*/ loan.doc-ref =  /*in-doc-ref*/ in-loan
         NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
         DO:
            in-cont-code = loan.cont-code.
            in-cont-cur = if loan.currency = '' then '810' else loan.currency .  
         END.
      END.
   &ENDIF

   &IF DEFINED(browse-entry) = 0 &THEN
      {g-acctv1.i &OFbase=Yes &vacct=op-entry.acct {&*}}
  
      RUN transmes.p (op-entry.acct-db,
                      op-entry.currency,
                      op-kind.op-kind,
                      "�����",
                      INPUT-OUTPUT mClMessList).
      RUN transmes.p (op-entry.acct-cr,
                      op-entry.currency,
                      op-kind.op-kind,
                      "������",
                      INPUT-OUTPUT mClMessList).
   
      /* �᫨ ��࠭� �४�饭�� �࠭���樨. */
      IF sStop
      THEN UNDO gen, LEAVE gen.

      RUN CreateFrmFields (op-templ.op-templ,"opreq","acct-db", op-entry.acct-db).
      RUN CreateFrmFields (op-templ.op-templ,"opreq","acct-cr", op-entry.acct-cr).
   &ENDIF
   &IF DEFINED(nocreate) = 0 &THEN
      &IF DEFINED(DoTacct) &THEN
         ASSIGN
            tacct-cr = op-entry.acct-cr
            tacct-db = op-entry.acct-db
         .
      &ENDIF
      

      {asswop.i}
      op-entry.currency = IF op-templ.currency <> ? 
                          THEN GetCurr(op-templ.currency) 
                          ELSE op-entry.currency. 
      wop.currency = op-entry.currency.
      IF op-entry.currency <> ? THEN 
         tcur = op-entry.currency.

      DEF VAR vAmtPrep    LIKE op-entry.amt-rub NO-UNDO.
      DEF VAR vAmtAftAcct AS   LOGICAL          NO-UNDO.

      IF (op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "") OR
         (op-templ.prep-amt-natcur <> ? AND op-templ.prep-amt-natcur <> "")
      THEN DO:
         IF op-templ.prep-amt-rub <> ? AND op-templ.prep-amt-rub <> "" 
         THEN
            vAmtPrep = DECIMAL(op-templ.prep-amt-rub) NO-ERROR.
         ELSE 
            vAmtPrep = DECIMAL(op-templ.prep-amt-natcur) NO-ERROR.

         vAmtAftAcct = GetXAttrValueEx("op-template",
                                       op-template.op-kind + "," + STRING(op-template.op-template),
                                       "AmtAftAcct","���") =  "��".
         /* ��� 蠡����� � �� AmtAftAcct = �� ����塞 �㬬� �ࠧ� ⮫쪮 �᫨ ������ ����ﭭ�� �㬬� ��� ������ ��� ���. 
            ��� ��⠫��� 蠡����� ����塞 �㬬� �ࠧ� */
         IF NOT vAmtAftAcct OR
            vAmtPrep > 0 OR 
            ({assigned op-entry.acct-db} AND {assigned op-entry.acct-cr}) 
         THEN DO:
            RUN parssen.p (RECID(wop), in-op-date, OUTPUT fler).

            /* �᫨ ��࠭� �४�饭�� �࠭���樨. */
            IF sStop
               THEN UNDO gen, LEAVE gen.

            IF fler THEN UNDO gen, LEAVE gen.

            IF     wop.amt-rub = 0 
               OR (wop.amt-sign BEGINS ">" AND wop.amt-rub <= 0) 
               OR (wop.amt-sign BEGINS "<" AND wop.amt-rub >= 0) THEN 
            DO:
               HIDE FRAME opreq NO-PAUSE.
               &IF DEFINED(OP-UNDO) &THEN {&OP-UNDO}
               &ELSE
                 UNDO doc ,  NEXT doc    .
               &ENDIF
            END.
         END.
      END.
      DISPLAY
         wop.amt-rub WHEN wop.amt-rub <> ?        @ op-entry.amt-rub
                   0 WHEN wop.amt-rub =  ?        @ op-entry.amt-rub
         wop.amt-cur WHEN op-entry.currency <> "" @ op-entry.amt-cur
                   0 WHEN op-entry.currency =  "" OR wop.amt-cur =  ? @ op-entry.amt-cur
      WITH FRAME opreq.
   &ENDIF

&ELSEIF DEFINED(const-recip) &THEN

   mRecip-data = GetXAttrValueEx("op-template",
                                 op-kind.op-kind + "," + STRING(op-templ.op-templ),
                                 "const-recip",
                                 "").
   IF mRecip-data <> "" THEN DO:
      &IF DEFINED(noben) = 0 &THEN
         ASSIGN
            op.name-ben:SCREEN-VALUE = ENTRY(5,mRecip-data,"^")
            op.name-ben              = op.name-ben:SCREEN-VALUE
            op.inn:SCREEN-VALUE      = ENTRY(3,mRecip-data,"^")
            op.inn                   = op.inn:SCREEN-VALUE
         NO-ERROR.
      &ENDIF
      &IF DEFINED(nomfo) = 0 &THEN
         ASSIGN
            vmfo              = ENTRY(1,mRecip-data,"^")
            vmfo:SCREEN-VALUE = vmfo
            op.ben-acct       = ENTRY(2,mRecip-data,"^")
            op.ben-acct:SCREEN-VALUE = op.ben-acct
         .
      &ENDIF
   END.

&ENDIF
/* $LINTFILE='g-frame.i' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='seei' */
/* $LINTDATE='07/04/2017 12:16:03.819+03:00' */
/*prosignVVHlbA5vfQmJugN1TzenBA*/