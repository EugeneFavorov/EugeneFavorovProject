/* 
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: chst(op.i
      Comment: ����� ��� ᬥ�� �����
   Parameters: BUFFER op, vstatus
         Uses:
      Used BY: many
      Created: 28.03.2001 serge
     Modified: 09.06.2003 NIK ����஫� ���㬥�� � 楫��
     MODIFIED: 20/05/2003 KOSTIK 0005638  ���� ��易⥫��� ४����⮢ ��᫥ ᬥ�� �����
     Modified: 07.07.2004 abko   0029145  �� ᬥ�� ����� ���㬥�� ��� �஢���� 
                                          �� ������� ����� � �㡠����⨪�
     Modified: 04/12/2004 kostik 0021328  �� ᬥ�� ����� ���� ��易⥫쭮 
                                          �஢����� � ���㬥�� ����襭�� ������
     Modified: 17.12.2004 abko   0038777 �⪫�祭�� ����஫� 117� �� ���㫨஢����
     Modified: 15.03.2007 kraw (0062505) ���㬥�� ��� �஢����, �� � �㡠����⨪��.
*/

{intrface.get autho}
{intrface.get email}

IF AVAIL op THEN DO:
&IF DEFINED(Def_Var) = 0 
&THEN 
   {intrface.get op}
   {intrface.get xclass}

   DEF VAR vUIN                   AS CHARACTER  NO-UNDO.
   DEF VAR vCodeMes               AS INT64      NO-UNDO.
   DEF VAR vMes                   AS CHARACTER  NO-UNDO.
   DEF VAR vCount1                AS INT64      NO-UNDO.
   DEF VAR vCount2                AS INT64      NO-UNDO.
   DEF VAR cdate                  AS DATE       NO-UNDO.
   DEF VAR ctime                  AS INT64    NO-UNDO.
   DEF VAR v-xattr-cr-CrClassCode AS CHARACTER  NO-UNDO.
   DEF VAR v-xattr-cr-I           AS INT64    NO-UNDO.
   DEF VAR cddif                  AS LOGICAL    NO-UNDO.
   DEF VAR vSelectQuest           AS CHARACTER  INITIAL ? NO-UNDO. /**/
   DEF VAR vDelProc               AS CHARACTER  NO-UNDO.
   DEF VAR vDelProcParam          AS CHARACTER  NO-UNDO.
   DEF VAR vPrevStat              AS CHARACTER  NO-UNDO.
   DEF VAR vFlagAnn               AS LOGICAL    NO-UNDO. /* ���㬥�� �����頥��� �� ���㫨஢�����*/
   DEF VAR vLinks                 AS CHARACTER  NO-UNDO.
   DEF VAR vLstClass              AS CHARACTER  NO-UNDO.
   DEF VAR vOk                    AS LOGICAL    NO-UNDO. 
   DEF VAR vStrTmp                AS CHARACTER  NO-UNDO.
   DEF VAR mErrMsgDb              AS CHAR       NO-UNDO.
   DEFINE VARIABLE vTypeAutor     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vOpAuthoStatus    LIKE op.op-status NO-UNDO.
   DEFINE VARIABLE vNoAuthorization  AS LOGICAL NO-UNDO .
   DEFINE VARIABLE vNoMakeDoc        AS LOGICAL NO-UNDO .
   DEFINE VARIABLE vCorrChk          AS CHARACTER INIT "off-corracct"
                                                  NO-UNDO.
   DEFINE VARIABLE vEMail            AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vFileTxt          AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vSubjectText      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mEmail            AS CHARACTER  NO-UNDO.

   DEFINE BUFFER gc-op FOR op.

   vOpAuthoStatus = FGetSetting("������", "���������", gop-status).
   IF FGetSetting("��������ᯑ��","","") = "��" THEN vCorrChk = "".

   &IF DEFINED(visa) NE 0 &THEN
      DEFINE VARIABLE vProcContr   AS CHARACTER   NO-UNDO. /* ��楤�� ����஫�     */
      DEFINE VARIABLE vProcContrIn AS CHARACTER   NO-UNDO. /* ��楤�� ����஫�     */      
      DEFINE VARIABLE vNewStat   AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaStat  AS CHARACTER   NO-UNDO. /* ����� ��� ����஢���� */
      DEFINE VARIABLE oResult    AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE vVisaList  AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaAll   AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaN     AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vStatQuest AS CHARACTER   NO-UNDO. 
      DEFINE VARIABLE vPos       AS INT64       NO-UNDO.
      DEFINE VARIABLE vHistStat  AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisa      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE vVisaRes   AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE vJ         AS INT64     NO-UNDO.
      DEFINE BUFFER bCode  FOR code.
      DEFINE BUFFER grCode FOR code. 
     
      PROCEDURE RunProcControl:
         DEFINE INPUT  PARAMETER iProc    AS CHARACTER NO-UNDO.
         DEFINE INPUT  PARAMETER iParam   AS CHARACTER NO-UNDO.
         DEFINE OUTPUT PARAMETER oResult  AS LOGICAL NO-UNDO.

         IF SearchPfile(iProc) THEN
     
            RUN VALUE(iProc + ".p")(BUFFER op, iParam, OUTPUT oResult).
     
         ELSE oResult = NO.
      
      END PROCEDURE.
   &ENDIF
   
   &GLOBAL-DEFINE Def_var YES
&ENDIF

   IF NOT (vstatus BEGINS "�") THEN DO:
      FIND op-entry OF op NO-LOCK NO-ERROR.
      IF AVAIL(op-entry) THEN DO:
         RUN ValidateCust115fl(BUFFER op,
                              op-entry.acct-db,
                              op-entry.currency,
                              op-entry.amt-rub)
         NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
            &IF DEFINED(open-undo) NE 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF  
      END.
   END.

   IF vstatus BEGINS "�" THEN DO:
      RUN GetClassMethod IN h_xclass (op.class-code, "Delete", "", "", OUTPUT vDelProc, OUTPUT vDelProcParam). 
      IF vDelProc = "pn-op-trans-del" OR vDelProc = "pn-op-svod-del" THEN DO:
         pick-value = "no".
         RUN RunClassMethod in h_xclass (op.class-code, "Delete", "", "", ?, STRING(RECID(op))).
         IF pick-value <> "yes" THEN DO:
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF
         END.
      END.

      IF GetXattrValue("op",STRING(op.op),"����������⥦�") EQ "�����祩�⢮" THEN DO:
      
         vLinks = GetLinks(op.class-code,
                           STRING(op.op),
                           "",
                           "dockz_RKC",
                           ",",
                           cur-op-date).

         IF {assigned vLinks} THEN DO:

            vLstClass = Ls-Class("opb-trancfKZ").
          
            IF CAN-DO(vLstClass,op.class-code) THEN DO:
               RUN DelLinksCode(op.class-code,
                                "dockz_RKC",
                                STRING(op.op),
                                "",
                                "",
                                OUTPUT vOK).
            END.
            ELSE DO:
               RUN Fill-SysMes IN h_tmess (
                  "", "", "0",
                  "���㫨஢���� ����饭�, ����室��� ᭠砫� ���㫨஢��� ᢮��� ���㬥�� � 㤠���� �裡.").
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.
         END.
      END.
   END.

   cdate = today. ctime = time.
   FIND code WHERE code.class EQ "�����"
               AND code.code  EQ  vstatus NO-LOCK NO-ERROR.

   vPrevStat = op.op-status.

   /* �� ���㫨஢���� � ��� ���� �����頥� ���� */
   vFlagAnn = NOT (vStatus BEGINS "�")
              AND op.op-status BEGINS "�".
   IF vFlagAnn THEN 
      op.op-date = cur-op-date.
   /* �᫨ ������� ����� */
   IF vstatus NE op.op-status THEN ASSIGN
      op.user-inspector = userid("bisquit")
      op.op-status      = vstatus
   .
   IF vstatus BEGINS "�" THEN
      op.op-date = ?.

   &IF DEFINED(visa) NE 0 &THEN

   ASSIGN
      vVisaList = ""
      vVisaAll  = "".

   FOR EACH bcode WHERE
            bCODE.class   EQ "����" AND
            bCODE.parent  EQ "����"
            NO-LOCK:

       vVisaN = GetXattrValue("op", STRING(op.op), bCode.CODE).

       IF CAN-DO("�ॡ����,�� �⢥ত���", vVisaN) THEN
          {additem.i vVisaList bCode.CODE}

       IF {assigned vVisaN} THEN
          {additem.i vVisaAll bCode.CODE}

   END.

   IF {assigned vVisaAll} THEN DO: 
      /* �� ������ ���㬥�� � �����, �� ���஬ ������ ��楤��� ����஫�, �� ����� ��� ����஢���� ��� ����筮�� ����� �� ����* 㤠������  */

      ASSIGN
         vProcContr = GetCodeMisc("�����", vStatus, 4)
         vVisaStat  = GetCodeMisc("��楤��늮���", vProcContr, 1) 
      .

      /* �㤥� ����� ����� ��室��, �᫨ �� ��� ������ ��楤�� ����஫� � ����� ��� ����஢���� 
       � �������� ���室 �� �⮣� ����� � ⥪�騩 ����� */
       
      IF {assigned vProcContr} AND {assigned vVisaStat} AND (CAN-DO(GetCode("�����", vStatus), vPrevStat)  OR vPrevStat EQ vVisaStat) THEN 
         hist: 
         DO:
         FOR EACH history WHERE
                  history.file-name EQ "op"
              AND history.field-ref EQ STRING(op.op)
              AND history.modify    EQ "W"
                  NO-LOCK
               BY history.history-id DESCENDING:
        
            vPos = LOOKUP("op-status",history.field-value).
            IF vPos GT 0 THEN DO:
               vHistStat = ENTRY(vPos + 1,history.field-value).

               IF vHistStat EQ vStatus THEN DO:   /* 㡥�����, �� ���㬥�� 㦥 �� � �⮬ ����� */
                   
                  DO vJ = 1 TO NUM-ENTRIES(vVisaAll):
                     UpdateSigns("op",
                                 STRING (op.op),
                                 ENTRY(vJ, vVisaAll),  
                                 "",
                                 YES).
                  END.  
                  vVisaList = "".
                  LEAVE hist.  
               END.
            END.
         END.
      END.
   END.
                                                                                                      
   IF {assigned vVisaList} and ( vStatus > vPrevStat ) THEN DO:
      IF vStatQuest NE "2" THEN DO:
         pick-value = "1". 
         RUN messmenu.p(10 ,
                     "[ ��������� ]",
                     "��ॢ�� ���㬥��  " + op.doc-num + "~n� ����� " + vStatus + " ����������.~n" + 
                     "�� ���㬥�� ���������� ���� " + vVisaList + ".",
                     "�ய�����,�ய����� ��� ���").
         IF pick-value EQ "2" THEN
            vStatQuest = pick-value.  

      END.


      &IF DEFINED(open-undo) NE 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
       
   END.

   ASSIGN
      vProcContr   = GetCodeMisc("�����", vPrevStat, 4)
      vProcContrIn = GetCodeMisc("�����", vStatus, 8)
   . 

   IF {assigned vProcContr} AND NOT vstatus BEGINS "�" THEN DO:

      FIND FIRST bCode WHERE
                 bCODE.class EQ "��楤��늮���"
             AND bCode.code  EQ vProcContr NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:

         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF

      END.
      vVisaStat  = bCode.misc[1].
      IF NOT {assigned vVisaStat} THEN DO:  /* �᫨ ����� ��� ����஢���� �� 㪠���, 
                                               ��楤�� ����஫� �믮������ ��� ��堭���� ����஢���� */

         IF {assigned bCode.val} THEN DO: /* ��楤�� */
         
            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).

            IF oResult NE YES THEN
            &IF DEFINED(proc_cntrl_undo) NE 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) NE 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         
         END.
         ELSE DO:   /* ��㯯� ��楤�� */
         
            FOR EACH grCode WHERE 
                     grCode.class  EQ "��楤��늮���"
                 AND grCode.parent EQ bCode.CODE
                 AND grCode.val    NE ""
                     NO-LOCK:
         
               RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
         
               IF oResult NE YES THEN
               &IF DEFINED(proc_cntrl_undo) NE 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) NE 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
         
            END.
         END.      

      END.
      ELSE DO:   /* ��堭��� ��� */

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) NE "�⢥ত���" THEN DO: /* ��楤�� */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult EQ ?  THEN DO:
               &IF DEFINED(open-undo) NE 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult NE YES THEN DO:

               op.op-status = vPrevStat. 
               validate op.
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ����N*/
                           "�ॡ����",
                           YES).

               op.op-status = bCode.misc[1]. 
               vVisaRes = YES.
            END.
            ELSE 
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ����N*/
                           "�� �ॡ����",
                           YES).
         END.
         ELSE DO:

            FOR EACH grCode WHERE 
                     grCode.class  EQ "��楤��늮���"
                 AND grCode.parent EQ bCode.CODE
                 AND grCode.val    NE ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 
               IF vVisa NE "�⢥ত���" AND NOT (vVisa EQ "�ॡ����" AND op.op-status EQ bCode.misc[1]) THEN DO:

                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult EQ ?  THEN DO:
                     &IF DEFINED(open-undo) NE 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.
                 
                  IF oResult NE YES THEN DO:
                 
                     op.op-status = vPrevStat.
                     validate op.
                
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ����N*/
                                 "�ॡ����",
                                 YES).
                     op.op-status = bCode.misc[1].
                     vVisaRes = YES.                 
                  END.
                  ELSE 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ����N*/
                                 "�� �ॡ����",
                                 YES).
               END.  
            END.
         END.          
      END.
   END.
   IF {assigned vProcContrIn} AND NOT vVisaRes THEN DO:
      FIND FIRST bCode WHERE
                 bCODE.class EQ "��楤��늮���"
             AND bCode.code  EQ vProcContrIn NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:
         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.
      vVisaStat  = bCode.misc[1].
      IF NOT {assigned vVisaStat} THEN DO:  /* �᫨ ����� ��� ����஢���� �� 㪠���, 
                                               ��楤�� ����஫� �믮������ ��� ��堭���� ����஢���� */

         IF {assigned bCode.val} THEN DO: /* ��楤�� */
            RUN RunProcControl(INPUT bCode.val,  INPUT bCode.description[1], OUTPUT oResult).     

            IF oResult NE YES THEN
            &IF DEFINED(proc_cntrl_undo) NE 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) NE 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         END.      
         ELSE DO:   /* ��㯯� ��楤�� */
        
            FOR EACH grCode WHERE 
                     grCode.class  EQ "��楤��늮���"
                 AND grCode.parent EQ bCode.CODE
                 AND grCode.val    NE ""
                     NO-LOCK:
        
               RUN RunProcControl(INPUT grCode.val,   INPUT grCode.description[1], OUTPUT oResult).
        
               IF oResult NE YES THEN
               &IF DEFINED(proc_cntrl_undo) NE 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) NE 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
        
            END.
         END.
      END.
      ELSE DO:

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) NE "�⢥ত���" THEN DO: /* ��楤�� */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult EQ ?  THEN DO:
               &IF DEFINED(open-undo) NE 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult NE YES THEN DO:

               op.op-status =  vPrevStat. 
               validate op.

               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ����N*/
                           "�ॡ����",
                           YES).
               op.op-status = bCode.misc[1].
            END.
            ELSE 
               UpdateSigns("op",
                           STRING (op.op),
                           bCode.misc[2],  /* ����N*/
                           "�� �ॡ����",
                           YES).
         END.
         ELSE DO:

            FOR EACH grCode WHERE 
                     grCode.class  EQ "��楤��늮���"
                 AND grCode.parent EQ bCode.CODE
                 AND grCode.val    NE ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 

               IF vVisa NE "�⢥ত���" AND NOT (vVisa EQ "�ॡ����" AND op.op-status EQ bCode.misc[1]) THEN DO:
                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult EQ ?  THEN DO:
                     &IF DEFINED(open-undo) NE 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.

                  IF oResult NE YES THEN DO:
                 
                     op.op-status = vPrevStat.
                     validate op.
                 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ����N*/
                                 "�ॡ����",
                                 YES). 
                     op.op-status = bCode.misc[1].
                  END.
                  ELSE 
                     UpdateSigns("op",
                                 STRING (op.op),
                                 grCode.misc[2],  /* ����N*/
                                 "�� �ॡ����",
                                 YES).
               END.
            END.
         END. 
      END.
      
   END.
   &ENDIF
         
   cddif = NO.
   FIND FIRST op-entry OF op WHERE op-entry.acct-db EQ ? NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN cddif = YES.
   ELSE DO:
      FIND FIRST op-entry OF op WHERE op-entry.acct-cr EQ ? NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN cddif = YES.
   END.

   pick-value = "no".
   RUN RunClassMethod IN h_xclass(op.class-code,"chkupd","","",
                              ?,string(recid(op)) + ",status").
   IF NOT CAN-DO("no-method,no-proc",RETURN-VALUE) AND pick-value NE "yes" THEN DO:
      &IF DEFINED(open-undo) NE 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
   END.

   RUN SetSysConf IN h_base("NoFmsDocChk","YES").
                        /* �஢�ઠ ���㬥��. */
   IF       code.misc[1]         EQ ""
      OR    TRIM(code.misc[1])   EQ "��"  THEN 
   DO:
      RUN SetSysConf IN h_base ("�।�����", vPrevStat).
      RUN Check-Op IN h_op (RECID(op),
                            "{&871}",
                            "{&OffKNF}",
                            "{&OFcur-bal}",
                            "{&chkupd}",
                            IF vstatus EQ "�" THEN "Off-VO" ELSE "").
      RUN DeleteOldDataProtocol IN h_base ("�।�����").
   END.
   ELSE /*��騥 �஢�ન �⪫�祭�, �� ������ �஢���� �஢���� ����*/
      RUN Check-Op-balance IN h_op (RECID(op),"{&OFcur-bal}").

   IF RETURN-VALUE NE "" THEN DO:
      RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").
      &IF DEFINED(open-undo) NE 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF     
   END.
   RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").  


   IF vstatus >= gop-status THEN DO: /* ��� ४������ */
      RUN chsigns.p (op.class-code,"OP",STRING(op.op), NO, OUTPUT v-xattr-cr-I).
      IF v-xattr-cr-I = 2 THEN DO: /* �᫨ ��� ���� (� �ॡ��� ।���஢����) */
                                     /* � ��뢠�� ��楤��� ।���஢���� ��� ४����⮢ */
           &IF DEFINED(xattr-undo) NE 0 &THEN
              IF vSelectQuest EQ ? THEN
              RUN messmenu.p(10 ,
                          "[ ��������� ]",
                          "� ���㬥�� " + op.doc-num + " �� �������~n ��易⥫�� ���. ४������",
                          "�ய�����,�ய����� ��� ���,�������� �����,�������� ����� ��� ���").
              ASSIGN
              vSelectQuest = pick-value WHEN CAN-DO("2,4",pick-value).
              IF    CAN-DO("1,2",pick-value)
                 OR vSelectQuest EQ "2" 
                 OR LASTKEY EQ KEYCODE("ESC") THEN DO:
                 {&xattr-undo}.
              END.
           &ELSE
              UNDO,LEAVE.
           &ENDIF
      END.
   END.
&IF DEFINED(BUFF-BOP-ENTRY) &THEN
&ELSE
   &GLOBAL-DEFINE BUFF-BOP-ENTRY YES
   DEFINE BUFFER bop-entry1 FOR op-entry.
   DEFINE BUFFER bkau-entry1 FOR kau-entry.
&ENDIF


   IF CAN-FIND(FIRST op-entry OF op) THEN
      FOR EACH kau-entry OF op NO-LOCK:

         FIND FIRST bkau-entry1 EXCLUSIVE-LOCK WHERE RECID(bkau-entry1) = RECID(kau-entry) NO-WAIT NO-ERROR.
   
         IF LOCKED bkau-entry1 THEN 
         DO:
            RUN wholocks2.p (RECID(kau-entry), "kau-entry", "������ � kau-entry �������஢���").
            &IF DEFINED(open-undo) NE 0 &THEN
               {&open-undo}.
            &ELSE
            UNDO, RETRY.
            &ENDIF     
         END.
   
         ASSIGN bkau-entry1.op-status = op.op-status
                bkau-entry1.op-date   = op.op-date
         .
      END.
   ELSE
/* ���� ���㬥��� ��� �஢����, �� � �㡠����⨪�� */
      FOR EACH kau-entry OF op NO-LOCK:
         RUN chst_kau.p(kau-entry.op, kau-entry.kau-entry, kau-entry.op-entry, op.op-status, op.op-date).
      END.
   
   FOR EACH op-entry OF op NO-LOCK:
      FIND FIRST bop-entry1 EXCLUSIVE-LOCK WHERE RECID(bop-entry1) = RECID(op-entry) NO-WAIT NO-ERROR.

      IF LOCKED bop-entry1 THEN 
      DO:
         RUN wholocks2.p (RECID(op-entry), "op-entry", "������ � op-entry �������஢���").
         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
         UNDO, RETRY.
         &ENDIF     
      END.
      IF NOT cddif                            AND
         cur-op-date      NE op-entry.op-date AND
         bop-entry1.amt-cur NE 0                AND
         bop-entry1.curr    GT ""                    THEN 
      DO:
         bop-entry1.amt-rub    = CurToBase("�������",
                                           bop-entry1.currency,
                                         cur-op-date,
                                           bop-entry1.amt-cur).
         bop-entry1.value-date = cur-op-date.
      END.

      ASSIGN
         bop-entry1.op-status  = op.op-status
         bop-entry1.op-date    = op.op-date
      .
      FOR EACH kau-entry OF op-entry NO-LOCK:

         FIND FIRST bkau-entry1 EXCLUSIVE-LOCK WHERE RECID(bkau-entry1) = RECID(kau-entry) NO-WAIT NO-ERROR.

         IF LOCKED bkau-entry1 THEN 
         DO:
            RUN wholocks2.p (RECID(kau-entry), "kau-entry", "������ � kau-entry �������஢���").
            &IF DEFINED(open-undo) NE 0 &THEN
               {&open-undo}.
            &ELSE
            UNDO, RETRY.
            &ENDIF     
         END.

         ASSIGN bkau-entry1.op-status = op.op-status
                bkau-entry1.op-date   = op.op-date.
      END.

      IF code.misc[1]       EQ ""   OR
         TRIM(code.misc[1]) EQ "��" THEN DO :

         IF vPrevStat > op.op-status AND op.op-status < "�" THEN
            RUN SetSysConf IN h_base ( "�⬥����஢��㑮��⨩",
                                       op.op-kind         + "!" + STRING(RECID(op-entry)) + "#" + "d_dsp"
                                       ).       

         RUN SetSysConf IN h_base ( "�।�����", vPrevStat).    
         RUN SetSysConf IN h_base("NoFmsDocChk","YES").
         RUN Check-Op-Entry IN h_op (BUFFER op-entry,
                                     INPUT  op-entry.op-date,
                                     INPUT  NO,
                                     INPUT  "status",
                                     INPUT  vCorrChk,
                                     OUTPUT flager).

         RUN DeleteOldDataProtocol IN h_base ("�⬥����஢��㑮��⨩").
         RUN DeleteOldDataProtocol IN h_base ("�।�����").
         RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").  

         IF flager > 0 THEN DO:
           vStrTmp = GetSysConf("block-acct-msg") NO-ERROR.
           IF vStrTmp = ? THEN DO:
              &IF DEFINED(open-undo) NE 0 &THEN
                 {&open-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
           END.
           ELSE DO:
              RUN DeleteOldDataProtocol IN h_base ("block-acct-msg").
              RUN putlog(vStrTmp).
              &IF DEFINED(del-undo) NE 0 &THEN
                 {&del-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
           END.
         END.
      END.
      IF AVAIL op-entry THEN
      RUN Anl-Stb IN h_op (BUFFER op-entry, OUTPUT flager).
      IF flager > 0 THEN DO:
         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.

      /* ���ਧ��� �� ����襭��/��������� ����� */
      IF {assigned vPrevStat} THEN
      DO:
         vTypeAutor = ?.
         /* ����蠥� ����� > �������쭮�� ����� ���ਧ�樨 */

         IF (vPrevStat < vOpAuthoStatus) AND (vstatus >= vOpAuthoStatus) THEN
         DO:
            vTypeAutor = "D".
            /* �� ᬥ�� ����� ������ �� ���㫨஢����� */
            IF vPrevStat BEGINS "�" THEN
            DO:
               vTypeAutor = "U".
   END.
END.
         /* ������� ����� < �������쭮�� ����� ���ਧ�樨 */
         IF (vPrevStat >= vOpAuthoStatus) AND (vstatus < vOpAuthoStatus) THEN
         DO:
            vTypeAutor = "R".
            /* �� ᬥ�� ����� �� ���㫨஢���� */
            IF vstatus BEGINS "�" THEN
            DO:
               vTypeAutor = "A".
            END.
         END.

         RUN VerFuturDay  IN h_autho (
             ROWID(op-entry),
             OUTPUT vNoMakeDoc,
             OUTPUT vNoAuthorization).

         IF vNoMakeDoc THEN DO: /* ����饭� ᮧ������ ���㬥��� */
            flager = 9.
            &IF DEFINED(open-undo) NE 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF
         END.

         IF {assigned vTypeAutor} AND
             vNoAuthorization = NO THEN   /* ��� ����� �� ���ਧ��� */
         DO:
            RUN AuthorizationRequest IN h_autho (ROWID(op-entry), vTypeAutor, OUTPUT mVL_Except).
            IF mVL_Except NE "0" THEN DO:
               mVL_Except = 'ERR_AVT'.
               flager = 9.
               &IF DEFINED(open-undo) NE 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.
         END.

      END.
      IF op-entry.op-status GE CHR(251) and op-entry.kau-db begins "�����" AND NUM-ENTRIES(op-entry.kau-db) GE 2 THEN DO:
         RUN run-chkdb.p(RECID(op-entry),cur-op-date,OUTPUT mErrMsgDb).
         IF mErrMsgDb NE "" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","0",mErrMsgDb).
              &IF DEFINED(open-undo) NE 0 &THEN
                 {&open-undo}.
              &ELSE
                 UNDO, RETRY.
              &ENDIF
         END.
      END.
   END. /* for each op-entry */
     /* ��⥣��� � ����⮩ ��஭�� */
   vUIN   = GetXAttrValue("op",STRING(op.op),"UIN").   
   vCount1 = 0.
   IF {assigned vUIN} THEN
   DO:
      FOR EACH signs WHERE signs.file-name   EQ "op"
                       AND signs.code        EQ "UIN"
                       AND signs.code-value EQ vUIN
         NO-LOCK,
         EACH gc-op WHERE gc-op.op EQ INT64(signs.surrogate) AND gc-op.op-status LT CHR(251) NO-LOCK:
         vCount1 = vCount1 + 1.
      END.
   END.
   IF op.op-status GE CHR(251) AND {assigned vUIN} AND vCount1 EQ 0
   THEN 
   DO:
      DO vCount2 = 1 TO 3:
         RUN confirmation.p (INPUT op.op,Output vCodeMes,Output vMes) NO-ERROR.
         IF vCodeMes NE 0 THEN PAUSE 6 MESSAGE "��।����� ᮮ�饭�� � ������� ��஭�.".
      END.
      IF {assigned vMes} THEN DO:
         Assign
            mEmail = FGetSetting("LimCtrlParams", "BadEmail", "SStrahov@bis.ru").
         ASSIGN
            vEMail       = mEmail
            vSubjectText = "�訡�� ��।�� � ������� ��஭�"
            vFileTxt     = "�� 㤠���� ��ࠢ��� ���⢥ত���� ��ॢ���"  + "~n" +
                           "(Service Confirmation)".
         RUN SendFMail(vEMail, vSubjectText, vFileTxt, "", "sendemail.log").
         MESSAGE vMes VIEW-AS ALERT-BOX.
         &IF DEFINED(open-undo) NE 0 &THEN
            {&open-undo}.
         &ELSE
            UNDO, RETRY.
         &ENDIF
      END.
   END.
END.
/* $LINTFILE='chst(op.i' */
/* $LINTMODE='1' */
/* $LINTENV ='2st' */
/* $LINTVSS ='*' */
/* $LINTUSER='krok' */
/* $LINTDATE='31/03/2016 18:26:14.794+04:00' */
/*prosign1v6k3FUSlIMctAHGxSh7iw*/