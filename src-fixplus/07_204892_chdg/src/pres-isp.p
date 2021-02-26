/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PRES-ISP.P
      Comment: ������ �ᯮ������ ��� �� ����筮� ����襭��.
   Parameters: ���
         Uses:
      Used by:
      Created: 31.06.2007 13:24 Fepa 75124
     Modified: 06.10.2009 16:25 ksv      (0118160) ��㯯�஢�� ��ࠬ��஢ ���
                                         �������樨 � ᯥ�.���ᨨ       
*/

DEF INPUT PARAM vStatusTO AS CHAR NO-UNDO.
DEF INPUT PARAM vPreschTo AS CHAR NO-UNDO.
DEF PARAM BUFFER term-obl FOR term-obl.
DEF OUTPUT PARAM vRefresh AS LOGICAL NO-UNDO.

{globals.i}
{intrface.get xclass} /* ����㧪� �����㬥���� ����奬�  */
{intrface.get tmess}
{intrface.get loan}
{loan.pro}

DEF VAR mSummToPay      AS DEC   NO-UNDO.
DEF VAR mOldAnSumm      AS DEC   NO-UNDO.
DEF VAR mCredOffset     AS CHAR  NO-UNDO.
DEF VAR mOffset         AS CHAR  NO-UNDO INIT ",->,<-".
DEF VAR mDelayOffset    AS CHAR  NO-UNDO.  /* ᤢ�� ���� ����砭�� ����.��ਮ�� (��.����) */
DEF VAR mDelayOffsetInt AS CHAR  NO-UNDO.  /* ᤢ�� ���� ����砭�� ����.��ਮ�� (��業��) */
DEF VAR vLCRecid        AS RECID NO-UNDO.
DEF VAR vCrOffset       AS CHAR  NO-UNDO.
DEF VAR mNewAnSumm      AS DEC   NO-UNDO.
DEF VAR vbcondSur       AS CHAR  NO-UNDO.
DEF VAR vicondSur       AS CHAR  NO-UNDO.
DEF VAR vcondSur        AS CHAR  NO-UNDO.
DEF VAR vDblAnnSince    AS DATE  NO-UNDO.
DEF VAR vRecalcAnn      AS LOG   NO-UNDO.
DEF VAR vLCRecNew       AS RECID NO-UNDO.
DEF VAR vLCSummNew      AS DEC   NO-UNDO.
DEF VAR vLoanCondSince  AS DATE  NO-UNDO.
DEF VAR mPayScheme      AS CHAR  NO-UNDO.
DEF VAR mPerSum         AS CHAR  NO-UNDO.
DEF VAR mOffSet2        AS CHAR  NO-UNDO.

DEFINE BUFFER bloan-cond FOR loan-cond.
DEFINE BUFFER iloan-cond FOR loan-cond.
DEFINE BUFFER xterm-obl  FOR term-obl.
DEFINE BUFFER yterm-obl  FOR term-obl.
DEFINE BUFFER bterm-obl  FOR term-obl.
DEFINE BUFFER xcomm-rate FOR comm-rate.
DEFINE BUFFER xloan-cond FOR loan-cond.
DEFINE BUFFER dloan-cond FOR loan-cond.

MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   IF vStatusTO NE "����" THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","������ �ᯮ������ ����㯭� ⮫쪮 ��� ����ᥩ � ����ᮬ ����.").
      LEAVE.
   END.
   FIND FIRST loan WHERE loan.contract  EQ term-obl.contract
                     AND loan.cont-code EQ term-obl.cont-code
      NO-LOCK NO-ERROR.

   IF term-obl.end-date LE loan.since THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","��� ������ ������� ������ ���� ����� ���� ��砫� ����⢨� �᫮��� ����筮�� ����襭��. �����⠩� �������.").
      UNDO MAIN, LEAVE MAIN.
   END.

   FIND LAST bloan-cond WHERE bloan-cond.contract  EQ term-obl.contract
                          AND bloan-cond.cont-code EQ term-obl.cont-code
                          AND bloan-cond.since     LT term-obl.end-date NO-LOCK NO-ERROR.
   IF NOT AVAIL bloan-cond THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","�� �������� ��� �᫮���.").
      UNDO MAIN, LEAVE MAIN.
   END.

   mPayScheme = GetXAttrInit(bloan-cond.class-code, "�奬�����").

   /* 㤠�塞 ��⮬���᪮� �᫮��� */
   IF loan.class-code = "loan_dbl_ann" THEN
   DO:
        blc:
        FOR EACH xloan-cond WHERE
                 xloan-cond.contract  EQ loan.contract
             AND xloan-cond.cont-code EQ loan.cont-code
             AND xloan-cond.since     GT term-obl.end-date
        NO-LOCK :
            IF GetXattrValue("loan-cond",
                             xloan-cond.contract + ","
                             + xloan-cond.cont-code + ","
                             + STRING(xloan-cond.since),
                             "AutoCond")  EQ "��"
            THEN DO:
               FIND FIRST dloan-cond WHERE RECID(dloan-cond) = RECID(xloan-cond)
                   EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
               IF AVAIL  dloan-cond THEN DO:    
                      /* � �� ��������� �� ��筮� 㤠����� �᫮��� */
                   RUN lc-dell.p(dloan-cond.contract,
                                 dloan-cond.cont-code ,
                                 dloan-cond.since
                                 ).
                   DELETE dloan-cond.      
               END.
               LEAVE blc.
            END.
        END.
   END.

   /* ��।��塞 ������� ���⮪, �������騩 �� ���� �ᯮ������ ����. �����. */
   FIND LAST bterm-obl WHERE bterm-obl.contract  EQ bloan-cond.contract
                         AND bterm-obl.cont-code EQ bloan-cond.cont-code
                         AND bterm-obl.idnt      EQ 2
                         AND bterm-obl.end-date  LE term-obl.end-date
     EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   /* ����塞 �㬬�, ������ ��⠫��� �������, ��᫥ ����筮�� ����襭�� */
   mSummToPay = bterm-obl.amt-rub - term-obl.amt-rub.


   /* ��࠭��� ���� �뫠... �� ������ �� ����...
     ASSIGN
        bterm-obl.amt-rub = mSummToPay.*/

   mOldAnSumm = DEC(GetXAttrValueEx("loan-cond",
                                    bloan-cond.contract  + "," +
                                    bloan-cond.cont-code + "," +
                                    STRING(bloan-cond.since),
                                    "����⏫��",
                                    "0")).

   FIND FIRST iloan-cond WHERE iloan-cond.contract  EQ term-obl.contract
                           AND iloan-cond.cont-code EQ term-obl.cont-code
                           AND iloan-cond.since     EQ term-obl.end-date 
   NO-LOCK NO-ERROR.

   IF AVAIL iloan-cond THEN
   DO:
      vicondSur = GetSurrogateBuffer("loan-cond",(BUFFER iloan-cond:HANDLE)).
      
      /* �᫨ �� ��� ���� 㦥 ������� �᫮��� �� �� �� �㬬� � ⮦� ���������,
      ** � ���� �⠢�� ��� ��, ���� �訡�� */
      IF     GetXAttrValue("loan-cond",vicondSur,"PaySum")  EQ STRING(mSummToPay)
         AND GetXAttrValue("loan-cond",vicondSur,"PayType") EQ "���������" THEN
      DO:
         UpdateSigns(term-obl.class-code,
                     term-obl.contract          + "," +
                     term-obl.cont-code         + "," +
                     STRING(term-obl.idnt)      + "," +
                     STRING(term-obl.end-date)  + "," +
                     STRING(term-obl.nn),
                     "term-obl-status",
                     "���",
                     ?).
      END.
      ELSE
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","������� �᫮��� �� ���� ��砫� ����⢨� ��� �� ����筮� ����襭��.").
         UNDO MAIN, LEAVE MAIN.
      END.
   END. 
   
   /* ��뢠�� ��楤��� ᮧ����� �᫮��� �� ���� */
   RUN CrCond IN h_loan (term-obl.contract,
                          term-obl.cont-code,
                         term-obl.end-date,
                         "%���,%����,�������"). 

   FIND FIRST loan-cond WHERE 
              loan-cond.contract  EQ term-obl.contract
          AND loan-cond.cont-code EQ term-obl.cont-code
          AND loan-cond.since     EQ term-obl.end-date
   NO-LOCK NO-ERROR.

              
  /* ������塞 �� PayType ��� ᮧ����� �᫮��� � ⨯�� ���⥦� ��������� */        
   UpdateSigns(loan-cond.class-code,
               loan-cond.contract + "," +
              loan-cond.cont-code + "," +
              STRING(loan-cond.since),
              "PayType",
               "���������",
               ?).                        
 /* ������塞 �� PaySum ��� �뢮�� ���४⭮� �㬬� ���⥦� */   
   UpdateSigns(loan-cond.class-code,
               loan-cond.contract + "," +
              loan-cond.cont-code + "," +
              STRING(loan-cond.since),
               "PaySum",
               TRIM(STRING(term-obl.amt-rub,">>>,>>>,>>>,>>>,>>9.99")),
              ?). 
 
   /* �᫮��� ᮧ����, ���塞 ����� �� ��� */
   UpdateSigns(term-obl.class-code,
               term-obl.contract          + "," +
               term-obl.cont-code         + "," +
               STRING(term-obl.idnt)      + "," +
               STRING(term-obl.end-date)  + "," +
               STRING(term-obl.nn),
               "term-obl-status",
               "���",
               ?) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᬥ�� ����� ��� �� ����筮� ����襭��!").
      UNDO MAIN, LEAVE MAIN.
   END.

   /* ����ନ�㥬 ��䨪� � ��⮬ ������ �᫮��� */
   ASSIGN
      mCredOffset          = GetXAttrValueEx("loan-cond",vcondSur,"cred-offset","")
      mDelayOffset         = GetXAttrValueEx("loan-cond",vcondSur,"delay-offset","")
      mDelayOffsetInt      = GetXAttrValueEx("loan-cond",vcondSur,"delay-offset-int","")
   .


   RUN SetSysConf IN h_base("������������� �� �������� �����",
                            STRING(LOOKUP(mCredOffset,mOffset))).
   RUN SetSysConf IN h_base("������� �� ��������� �����",
                            STRING(LOOKUP(mCredOffset,mOffset))).

   vLCRecid = RECID(loan-cond).

   /*���������� ���. ४�� ��� ����७�஢�����*/
   IF mPayScheme EQ "����७�஢�����" THEN 
   DO:
      /* �饬 �।��騥 �᫮���  */
      FIND LAST bloan-cond WHERE bloan-cond.contract  EQ term-obl.contract
                       AND bloan-cond.cont-code EQ term-obl.cont-code
                       AND bloan-cond.since     LT term-obl.end-date 
      NO-LOCK NO-ERROR.
      IF AVAIL bloan-cond THEN 
      DO:
         mPerSum  = GetXAttrValueEx("loan-cond", bloan-cond.contract + "," + bloan-cond.cont-code + "," + STRING(bloan-cond.since), "�।����",""). 
         mOffSet2 = GetXAttrValueEx("loan-cond", bloan-cond.contract + "," +
                     bloan-cond.cont-code + "," + STRING(bloan-cond.since), "int-offset",""). 
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "cred-offset",
                     mOffset2,
                     ?).

      END. 

      IF term-obl.symbol EQ "�" THEN
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "�।����",
                     "",
                     ?).
      IF term-obl.symbol EQ "�" THEN
         UpdateSigns(loan-cond.class-code,
                     loan-cond.contract + "," +
                     loan-cond.cont-code + "," +
                     STRING(loan-cond.since),
                     "�।����",
                     mPerSum,
                     ?).
   END. /*IF mPayScheme EQ "����७�஢�����"*/
           
   /* �᫨ �� ��� ���� 㦥 ������� ������ � ��䨪� ��.���. � �����塞 �㬬� */
   FIND FIRST xterm-obl WHERE xterm-obl.contract  EQ term-obl.contract
                          AND xterm-obl.cont-code EQ term-obl.cont-code
                          AND xterm-obl.idnt      EQ 2
                          AND xterm-obl.end-date  EQ term-obl.end-date
                          AND xterm-obl.nn        EQ bterm-obl.nn
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.                   

   IF NOT AVAIL xterm-obl THEN
      CREATE xterm-obl.

   ASSIGN
      xterm-obl.contract     = term-obl.contract
      xterm-obl.cont-code    = term-obl.cont-code
      xterm-obl.end-date     = term-obl.end-date
      xterm-obl.idnt         = 2                  
      xterm-obl.nn           = bterm-obl.nn
      xterm-obl.amt          = mSummToPay
      xterm-obl.dsc-beg-date = term-obl.dsc-beg-date
   .
   
   RELEASE xterm-obl NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN
   DO:
      RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᮧ����� ��������� ���⥦�!").
      UNDO MAIN, LEAVE MAIN.
   END.

   /* ������뢠�� ����. �.�. � १���� ����筮�� 
   ** ����襭�� ᮪�頥��� �ப ����⢨� �।�� */
   IF vPreschTo EQ "����" THEN
   DO:
      /* ���࠭塞 ����� �㬬� ���⥦� �㬬� �����⭮�� ���⥦� 
      ** �� ����� �᫮��� */
      UpdateSigns(loan-cond.class-code,
                  loan-cond.contract  + "," +
                  loan-cond.cont-code + "," +
                  STRING(loan-cond.since),
                  "����⏫��",
                  IF mPayScheme EQ "����७�஢�����" THEN "" ELSE STRING(mOldAnSumm),
                  ?).
      RELEASE loan-cond NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᮧ����� �᫮���!").
         UNDO MAIN, LEAVE MAIN.
      END.

      RUN i-avterm.p (RECID(loan),vLCRecid,mSummToPay).

   END.

   /* ������뢠�� �㬬�. �.�. � १���� ����筮��
   ** ����襭��, �ப ��⠥��� �०���, �� ᮪�頥��� �㬬� �������� �믫�� */
   ELSE 
   DO:
      FIND LAST comm-rate WHERE comm-rate.kau        EQ loan.contract + "," + loan.cont-code
                            AND comm-rate.commission EQ "%�।" 
                            AND comm-rate.since      LT term-obl.end-date NO-LOCK NO-ERROR.

      vCrOffset = GetXAttrValue("loan-cond",
                                vcondSur,
                                "cred-offset").

      FIND FIRST xterm-obl WHERE
                 xterm-obl.contract  EQ loan.contract 
             AND xterm-obl.cont-code EQ loan.cont-code
             AND xterm-obl.idnt      EQ 1
             AND xterm-obl.end-date  EQ loan-cond.since
      NO-LOCK NO-ERROR.

      IF NOT AVAIL xterm-obl THEN
      DO:
         FIND FIRST xterm-obl WHERE
                    xterm-obl.contract  EQ loan.contract 
                AND xterm-obl.cont-code EQ loan.cont-code
                AND xterm-obl.idnt      EQ 1
                AND xterm-obl.end-date  GT loan-cond.since
         NO-LOCK NO-ERROR.
         IF AVAIL xterm-obl THEN
            vLoanCondSince = xterm-obl.end-date.
         ELSE
            vLoanCondSince = loan-cond.since.

      END.
      ELSE
         vLoanCondSince = loan-cond.since.

      /* �����⠥� �㬬� �����⭮�� ���⥦� � ��⮬ �㬬� ����筮�� ����襭�� */
      RUN CalcAnnuitet (loan.contract,
                        loan.cont-code,
                        vLoanCondSince,
                        loan.end-date,
                        mSummToPay,
                        comm-rate.rate-comm,
                        loan-cond.cred-date,
                        loan-cond.cred-period,
                        "1",
                        0,
                        LOOKUP(vCrOffset,"--,->,<-"),
                        INT64(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "����⊮��",
                                            ?)
                            ),
                        DEC(GetXAttrValueEx("loan", 
                                            loan.contract + "," + loan.cont-code, 
                                            "Sum-depos",
                                            "0")
                            ),
                        INT64(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "FirstPeriod",
                                            "0")
                            ),
                        DEC(GetXAttrValueEx("loan-cond", 
                                            loan.contract + "," + loan.cont-code + "," + STRING(loan-cond.since), 
                                            "PartAmount",
                                            "0")
                            ),
                        OUTPUT mNewAnSumm).

      /* ���࠭塞 ����� �㬬� �����⭮�� ���⥦� */
      UpdateSigns(loan-cond.class-code,
                  loan-cond.contract  + "," +
                  loan-cond.cont-code + "," +
                  STRING(loan-cond.since),
                  "����⏫��",
                  IF mPayScheme EQ "����७�஢�����" THEN "" ELSE STRING(mNewAnSumm),
                  ?).
      
      RELEASE loan-cond NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᮧ����� �᫮���!").
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN i-avterm.p (RECID(loan),vLCRecid,mSummToPay).
   END.

   /* ���⠢�� �� � ��� ���� dsc-beg-date */
   FIND FIRST yterm-obl WHERE yterm-obl.contract  EQ term-obl.contract
                          AND yterm-obl.cont-code EQ term-obl.cont-code
                          AND yterm-obl.idnt      EQ 1
                          AND yterm-obl.end-date  EQ term-obl.end-date
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL yterm-obl THEN
      ASSIGN
         yterm-obl.dsc-beg-date = term-obl.dsc-beg-date.

   FIND FIRST yterm-obl WHERE yterm-obl.contract  EQ term-obl.contract
                          AND yterm-obl.cont-code EQ term-obl.cont-code
                          AND yterm-obl.idnt      EQ 3
                          AND yterm-obl.end-date  EQ term-obl.end-date
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAIL yterm-obl THEN
      ASSIGN
         yterm-obl.dsc-beg-date = term-obl.dsc-beg-date.

   /* ��।��塞 ���� ��砫� ��ண� �᫮��� */
   IF loan.class-code EQ "loan_dbl_ann" THEN
   DO:
      FIND FIRST loan-cond WHERE RECID(loan-cond) = vLCRecid 
          NO-LOCK NO-ERROR.
      IF NOT AVAIL loan-cond THEN
      DO:
         RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᮧ����� �᫮���!").
         UNDO MAIN, LEAVE MAIN.
      END.
      RUN GetDateDblAnn(loan.contract, loan.cont-code, loan-cond.since, OUTPUT vDblAnnSince).
      vRecalcAnn = IF vPreschTo EQ "����" THEN NO ELSE YES.
      /* �������, �᫨ �㦭�, ��⮬���᪮� �᫮���  */
      IF loan-cond.since < vDblAnnSince THEN DO:            

         RUN Cr_Cond_DblAnn IN h_Loan (loan.contract, 
                                       loan.cont-code, 
                                       loan-cond.since, 
                                       NO, 
                                       vRecalcAnn,
                                       vDblAnnSince,
                                       OUTPUT vLCRecNew,
                                       OUTPUT vLCSummNew) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            RUN fill-sysmes IN h_tmess ("","","0","�訡�� ᮧ����� �᫮��� ��ਮ�� 㢥��祭��!").
            UNDO MAIN, LEAVE MAIN.
         END.
         /* �� �� ��� ࠧ� �� �뢮���� ��䨪� */
         RUN SetSysConf IN h_Base("�� �������� ������� �� �����","��").
         RUN i-avterm.p (RECID(loan),vLCRecNew,vLCSummNew).
         RUN DeleteOldDataProtocol IN h_Base("�� �������� ������� �� �����").
      END.
   END.
   
   RUN DeleteOldDataProtocol IN h_base("������� �� ��������� �����").
   RUN DeleteOldDataProtocol IN h_base("������������� �� �������� �����").

END.
vRefresh = YES.
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:49:52.833+04:00' */
/* $LINTFILE='pres-isp.p' */
/*prosign0NeA1NBQmoHfS5AHZno9Aw*/