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

   &IF DEFINED(visa) <> 0 &THEN
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
            &IF DEFINED(open-undo) <> 0 &THEN
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

      IF GetXattrValue("op",STRING(op.op),"����������⥦�") =  "�����祩�⢮" THEN DO:
      
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
   FIND code WHERE code.class =  "�����"
               AND code.code  =   vstatus NO-LOCK NO-ERROR.

   vPrevStat = op.op-status.

   /* �� ���㫨஢���� � ��� ���� �����頥� ���� */
   vFlagAnn = NOT (vStatus BEGINS "�")
              AND op.op-status BEGINS "�".
   IF vFlagAnn THEN 
      op.op-date = cur-op-date.
   /* �᫨ ������� ����� */
   IF vstatus <> op.op-status THEN DO:
      &IF DEFINED(recheck-op-status) > 0 &THEN
         IF vPrevStat <> mOrigStatus THEN DO:
            RUN SetReturnValue IN THIS-PROCEDURE
               (SUBSTITUTE("����� ���㬥�� &1 㦥 �� ������ � &2 �� &3",
                           op.doc-num,
                           mOrigStatus,
                           vPrevStat)).
            &IF DEFINED(open-undo) = 0 &THEN
               UNDO, RETRY.
            &ELSE
               {&open-undo}.
            &ENDIF
         END.
      &ENDIF
      ASSIGN
      op.user-inspector = userid("bisquit")
      op.op-status      = vstatus
   .
   END.
   IF vstatus BEGINS "�" THEN
      op.op-date = ?.

   &IF DEFINED(visa) <> 0 &THEN

   ASSIGN
      vVisaList = ""
      vVisaAll  = "".

   FOR EACH bcode WHERE
            bCODE.class   =  "����" AND
            bCODE.parent  =  "����"
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
       
      IF {assigned vProcContr} AND {assigned vVisaStat} AND (CAN-DO(GetCode("�����", vStatus), vPrevStat)  OR vPrevStat =  vVisaStat) THEN 
         hist: 
         DO:
         FOR EACH history WHERE
                  history.file-name =  "op"
              AND history.field-ref =  STRING(op.op)
              AND history.modify    =  "W"
                  NO-LOCK
               BY history.history-id DESCENDING:
        
            vPos = LOOKUP("op-status",history.field-value).
            IF vPos >  0 THEN DO:
               vHistStat = ENTRY(vPos + 1,history.field-value).

               IF vHistStat =  vStatus THEN DO:   /* 㡥�����, �� ���㬥�� 㦥 �� � �⮬ ����� */
                   
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
                                                                                                     
   IF {assigned vVisaList} THEN DO:

      IF vStatQuest <> "2" THEN DO:
         pick-value = "1". 
         RUN messmenu.p(10 ,
                     "[ ��������� ]",
                     "��ॢ�� ���㬥��  " + op.doc-num + "~n� ����� " + vStatus + " ����������.~n" + 
                     "�� ���㬥�� ���������� ���� " + vVisaList + ".",
                     "�ய�����,�ய����� ��� ���").
         IF pick-value =  "2" THEN
            vStatQuest = pick-value.  

      END.


      &IF DEFINED(open-undo) <> 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
       
   END.

   ASSIGN
      vProcContr   = GetCodeMisc("�����", vPrevStat, 4)
      vProcContrIn = GetCodeMisc("�����", vStatus, 8)
   . 

   IF {assigned vProcContr} AND 
       ((NOT vstatus BEGINS "�" )
         OR 
         /* ���客. ��� ������� �஢���� ���� ���� �����஢�� �㬬� � ����ᨭ�� ���� ��� ��릠 �訫� */
        (vstatus BEGINS "�" AND GetXattrValueEx("op",STRING(op.op),"�����㬬����","") NE "" ))
            THEN DO:

      FIND FIRST bCode WHERE
                 bCODE.class =  "��楤��늮���"
             AND bCode.code  =  vProcContr NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:

         &IF DEFINED(open-undo) <> 0 &THEN
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

            IF oResult <> YES THEN
            &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         
         END.
         ELSE DO:   /* ��㯯� ��楤�� */
         
            FOR EACH grCode WHERE 
                     grCode.class  =  "��楤��늮���"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:
         
               RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
         
               IF oResult <> YES THEN
               &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) <> 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
         
            END.
         END.      

      END.
      ELSE DO:   /* ��堭��� ��� */

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) <> "�⢥ত���" THEN DO: /* ��楤�� */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult =  ?  THEN DO:
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult <> YES THEN DO:

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
                     grCode.class  =  "��楤��늮���"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 
               IF vVisa <> "�⢥ত���" AND NOT (vVisa =  "�ॡ����" AND op.op-status =  bCode.misc[1]) THEN DO:

                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult =  ?  THEN DO:
                     &IF DEFINED(open-undo) <> 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.

                  IF oResult <> YES THEN DO:
                 
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
                 bCODE.class =  "��楤��늮���"
             AND bCode.code  =  vProcContrIn NO-LOCK NO-ERROR.
      IF NOT AVAIL(bCode) THEN DO:
         &IF DEFINED(open-undo) <> 0 &THEN
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

            IF oResult <> YES THEN
            &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                  {&proc_cntrl_undo}
            &ELSE
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            &ENDIF
         END.      
         ELSE DO:   /* ��㯯� ��楤�� */
        
            FOR EACH grCode WHERE 
                     grCode.class  =  "��楤��늮���"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:
        
               RUN RunProcControl(INPUT grCode.val,   INPUT grCode.description[1], OUTPUT oResult).
        
               IF oResult <> YES THEN
               &IF DEFINED(proc_cntrl_undo) <> 0 &THEN
                     {&proc_cntrl_undo}
               &ELSE
                  &IF DEFINED(open-undo) <> 0 &THEN
                     {&open-undo}.
                  &ELSE
                     UNDO, RETRY.
                  &ENDIF
               &ENDIF
        
            END.
         END.
      END.
      ELSE DO:

         IF {assigned bCode.val} AND GetXattrValue("op",STRING(op.op),bCode.misc[2]) <> "�⢥ত���" THEN DO: /* ��楤�� */

            RUN RunProcControl(INPUT bCode.val, INPUT bCode.description[1], OUTPUT oResult).
            IF oResult =  ?  THEN DO:
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.

            IF oResult <> YES THEN DO:

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
                     grCode.class  =  "��楤��늮���"
                 AND grCode.parent =  bCode.CODE
                 AND grCode.val    <> ""
                     NO-LOCK:

               vVisa = GetXattrValue("op",STRING(op.op),grCode.misc[2]). 

               IF vVisa <> "�⢥ত���" AND NOT (vVisa =  "�ॡ����" AND op.op-status =  bCode.misc[1]) THEN DO:
                  RUN RunProcControl(INPUT grCode.val, INPUT grCode.description[1], OUTPUT oResult).
                  IF oResult =  ?  THEN DO:
                     &IF DEFINED(open-undo) <> 0 &THEN
                        {&open-undo}.
                     &ELSE
                        UNDO, RETRY.
                     &ENDIF
                  END.

                  IF oResult <> YES THEN DO:
                 
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
   FIND FIRST op-entry OF op WHERE op-entry.acct-db =  ? NO-LOCK NO-ERROR.
   IF AVAIL op-entry THEN cddif = YES.
   ELSE DO:
      FIND FIRST op-entry OF op WHERE op-entry.acct-cr =  ? NO-LOCK NO-ERROR.
      IF AVAIL op-entry THEN cddif = YES.
   END.

   pick-value = "no".
   RUN RunClassMethod IN h_xclass(op.class-code,"chkupd","","",
                              ?,string(recid(op)) + ",status").
   IF NOT CAN-DO("no-method,no-proc",RETURN-VALUE) AND pick-value <> "yes" THEN DO:
      &IF DEFINED(open-undo) <> 0 &THEN
         {&open-undo}.
      &ELSE
         UNDO, RETRY.
      &ENDIF
   END.

   RUN SetSysConf IN h_base("NoFmsDocChk","YES").
                        /* �஢�ઠ ���㬥��. */
   IF       code.misc[1]         =  ""
      OR    TRIM(code.misc[1])   =  "��"  THEN 
   DO:
      RUN SetSysConf IN h_base ("�।�����", vPrevStat).
      RUN Check-Op IN h_op (RECID(op),
                            "{&871}",
                            "{&OffKNF}",
                            "{&OFcur-bal}",
                            "{&chkupd}",
                            IF vstatus =  "�" THEN "Off-VO" ELSE "").
      RUN DeleteOldDataProtocol IN h_base ("�।�����").
   END.
   ELSE /*��騥 �஢�ન �⪫�祭�, �� ������ �஢���� �஢���� ����*/
      RUN Check-Op-balance IN h_op (RECID(op),"{&OFcur-bal}").

   IF RETURN-VALUE <> "" THEN DO:
      RUN DeleteOldDataProtocol IN h_base ("NoFmsDocChk").
      &IF DEFINED(open-undo) <> 0 &THEN
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
           &IF DEFINED(xattr-undo) <> 0 &THEN
              IF vSelectQuest =  ? THEN
              RUN messmenu.p(10 ,
                          "[ ��������� ]",
                          "� ���㬥�� " + op.doc-num + " �� �������~n ��易⥫�� ���. ४������",
                          "�ய�����,�ய����� ��� ���,�������� �����,�������� ����� ��� ���").
              ASSIGN
              vSelectQuest = pick-value WHEN CAN-DO("2,4",pick-value).
              IF    CAN-DO("1,2",pick-value)
                 OR vSelectQuest =  "2" 
                 OR LASTKEY =  KEYCODE("ESC") THEN DO:
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
            &IF DEFINED(open-undo) <> 0 &THEN
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
         &IF DEFINED(open-undo) <> 0 &THEN
            {&open-undo}.
         &ELSE
         UNDO, RETRY.
         &ENDIF     
      END.
      IF NOT cddif                            AND
         cur-op-date      <> op-entry.op-date AND
         bop-entry1.amt-cur <> 0                AND
         bop-entry1.curr    >  ""                    THEN 
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
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
            UNDO, RETRY.
            &ENDIF     
         END.

         ASSIGN bkau-entry1.op-status = op.op-status
                bkau-entry1.op-date   = op.op-date.
      END.

      IF code.misc[1]       =  ""   OR
         TRIM(code.misc[1]) =  "��" THEN DO :

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
              &IF DEFINED(open-undo) <> 0 &THEN
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
         &IF DEFINED(open-undo) <> 0 &THEN
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
            &IF DEFINED(open-undo) <> 0 &THEN
               {&open-undo}.
            &ELSE
               UNDO, RETRY.
            &ENDIF
         END.

         IF {assigned vTypeAutor} AND
             vNoAuthorization = NO THEN   /* ��� ����� �� ���ਧ��� */
         DO:
            RUN AuthorizationRequest IN h_autho (ROWID(op-entry), vTypeAutor, OUTPUT mVL_Except).
            IF mVL_Except <> "0" THEN DO:
               mVL_Except = 'ERR_AVT'.
               flager = 9.
               &IF DEFINED(open-undo) <> 0 &THEN
                  {&open-undo}.
               &ELSE
                  UNDO, RETRY.
               &ENDIF
            END.
         END.

      END.
      IF op-entry.op-status >= CHR(251) and op-entry.kau-db begins "�����" AND NUM-ENTRIES(op-entry.kau-db) >= 2 THEN DO:
         RUN run-chkdb.p(RECID(op-entry),cur-op-date,OUTPUT mErrMsgDb).
         IF mErrMsgDb <> "" THEN DO:
            RUN Fill-SysMes IN h_tmess ("","","0",mErrMsgDb).
              &IF DEFINED(open-undo) <> 0 &THEN
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
      FOR EACH signs WHERE signs.file-name   =  "op"
                       AND signs.code        =  "UIN"
                       AND signs.code-value =  vUIN
         NO-LOCK,
         EACH gc-op WHERE gc-op.op =  INT64(signs.surrogate) AND gc-op.op-status <  CHR(251) NO-LOCK:
         vCount1 = vCount1 + 1.
      END.
   END.
   IF op.op-status >= CHR(251) AND {assigned vUIN} AND vCount1 =  0
   THEN 
   DO:
      DO vCount2 = 1 TO 3:
         RUN confirmation.p (INPUT op.op,Output vCodeMes,Output vMes) NO-ERROR.
         IF vCodeMes <> 0 THEN PAUSE 6 MESSAGE "��।����� ᮮ�饭�� � ������� ��஭�.".
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

   IF vStatus GE CHR(251)  THEN
   DO:
      IF CAN-DO("01���,01��",op.doc-type) THEN
	 FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "30102" NO-LOCK,
            EACH acct WHERE acct.acct     EQ op-entry.acct-cr 
                        AND acct.currency EQ op-entry.currency
            NO-LOCK:
                IF NOT CAN-DO(FGetSetting("���菫����","",""),USERID("bisquit")) AND NOT IsUserAdm(USERID("bisquit")) THEN
                DO:
                   RUN Fill-SysMes IN h_tmess ("","","",
                      SUBSTITUTE("���짮��⥫� &1 �� ����� �ࠢ� ��ॢ����� ���譨� ���⥦� � ����� �",USERID("bisquit"))).      
                   &IF DEFINED(open-undo) NE 0 &THEN
                      {&open-undo}.
                   &ELSE
                      UNDO, RETRY.
                   &ENDIF
             END.
         END.
   
      FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "407" OR op-entry.acct-cr BEGINS "40802" NO-LOCK,
         EACH acct WHERE acct.acct     EQ op-entry.acct-cr 
                     AND acct.currency EQ op-entry.currency
         NO-LOCK:
           IF CAN-DO("1,7",STRING(WEEKDAY(op.op-date))) THEN
           DO:
                pick-value = "NO".
                RUN Fill-SysMes IN h_tmess ("","","",
                   SUBSTITUTE("�������� !!! &1 ���⥦� �� ��� ��/�� ����饭� ��� ����� � � ��室�� ��� !!!",CHR(10))).      
                pick-value = "NO".
                IF pick-value NE "YES" THEN
                DO:
                   &IF DEFINED(open-undo) NE 0 &THEN
                      {&open-undo}.
                   &ELSE
                      UNDO, RETRY.
                   &ENDIF
                END.
          END.
      END.
   END.

   IF vStatus EQ "���" 
      AND op.filial-id EQ "0500" 
      AND CAN-FIND(FIRST op-bank OF op WHERE op-bank.bank-name MATCHES '*ᡥࡠ��*' USE-INDEX op-bank NO-LOCK) 
      AND STRING(TIME + (360 - TIMEZONE) * 60,"HH:MM:SS") LT REPLACE(SUBSTR(TRIM(FGetSetting('������ᯮ�℮',?,"14:00")),1,5),"-",":")
   THEN 
   DO:
      FOR EACH op-entry OF op WHERE op-entry.acct-cr EQ "30102810152090000884     @0500" 
                                AND CAN-DO("!30110*,*",op-entry.acct-db)
                                AND op-entry.amt-rub LE 5000000
      EXCLUSIVE-LOCK:
         op-entry.acct-cr = "30110810101100000047     @0500".
      END.
   END.

   IF vStatus EQ "���" 
   THEN 
   DO:
      FOR EACH op-entry OF op WHERE op-entry.acct-cr BEGINS "30102" 
                                AND op-entry.amt-rub GE 100000000
                                AND op-entry.type NE "��"
      EXCLUSIVE-LOCK:
         RUN Fill-SysMes IN h_tmess ("","","",
             SUBSTITUTE("��������: �㬬� �� ���⥦���� ����祭�� &1 �ॢ�蠥� �㬬� 100 ��� �㡫��. �ॡ���� ��ࠢ�� ����!",op.doc-num)).
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