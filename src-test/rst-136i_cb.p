/*
   ॥���� 136-� �� ��ਮ� ��� �஢������ ��

               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: RST-136I.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 19.10.2010 16:22 elus    
     Modified: 19.10.2010 16:22 elus    
*/

DEFINE INPUT  PARAMETER iParam AS CHARACTER   NO-UNDO.
{globals.i}     /* ����� ���� �� */
{wordwrap.def}
{intrface.get cust}  
{intrface.get xclass}

FUNCTION GetPassport  CHAR (INT64) FORWARD.
FUNCTION CutFio       CHAR (CHARACTER) FORWARD.

DEFINE VARIABLE mLines     AS CHARACTER   NO-UNDO EXTENT 30. /* �����窠 */
DEFINE VARIABLE mCols      AS INT64       NO-UNDO. /* ��ਭ� ���� */

DEFINE VARIABLE mIsCheckBuy AS LOGICAL   NO-UNDO. /* Yes - ����� 祪�� �㯨�� */
DEFINE VARIABLE mIsCheckSel AS LOGICAL   NO-UNDO. /* Yes - ����� 祪�� �த��� */
DEFINE VARIABLE mLogComm    AS LOGICAL   NO-UNDO. /* ������ � ������ﬨ */
DEFINE VARIABLE mStatus     AS CHARACTER NO-UNDO. /* ����� ���㬥�⮢ ��������� � ॥��� */
DEFINE VARIABLE mSpace      AS CHARACTER NO-UNDO.
/* DEFINE VARIABLE mFullItg  AS LOGICAL NO-UNDO.*/

DEFINE TEMP-TABLE ttRegister
   FIELD op       AS INTEGER   /* id ���㬥��          */
   FIELD doc-num  AS CHARACTER /* ����� ���㬥��       */
   FIELD doc-time AS INTEGER   /* �६� ����樨        */
   FIELD doc-date AS DATE      /* ��� ᮧ����� ����   */
   FIELD doc-kind AS CHARACTER /* ���������             */
   FIELD rate     AS DECIMAL   /* ����                  */
   FIELD ChRate   AS CHARACTER
   FIELD curr-db  AS CHARACTER /* ����� �ਭ��        */
   FIELD amt-db   AS DECIMAL   /* �㬬� �ਭ��         */
   FIELD curr-cr  AS CHARACTER /* ����� �뤠��         */
   FIELD amt-cr   AS DECIMAL   /* �㬬� �뤠��          */
   FIELD card     AS LOGICAL   /* �ਧ��� ����樨 � �� */
   FIELD cheq-qty AS DECIMAL   /* ����� 祪��           */
   FIELD currcheq AS CHARACTER /* ����� 祪��          */
   FIELD amt-cheq AS DECIMAL   /* �㬬� 祪��           */
   FIELD acct     AS CHARACTER /* ��� 䨧. ���        */
   FIELD dover    AS LOGICAL   /* ����७�����          */
   FIELD country  AS CHARACTER /* ��� ��࠭� �.�.       */
   FIELD dpr-id   AS INTEGER     /* id ᬥ�� ���㬥��    */
   FIELD forminfo AS CHARACTER /* ���ଠ�� �� 祪�� �� ����饥  */
   FIELD vain     AS LOGICAL   /* ���⮩ */

   INDEX idx dpr-id doc-date doc-time
   .

DEFINE TEMP-TABLE ttRegItog
   FIELD doc-date AS DATE      /* ��� ॥���          */
   FIELD doc-kind AS CHARACTER /* ���������             */
   FIELD ChRate   AS CHARACTER
   FIELD curr-db  AS CHARACTER /* ����� �ਭ��        */
   FIELD amt-db   AS DECIMAL   /* �㬬� �ਭ��         */
   FIELD curr-cr  AS CHARACTER /* ����� �뤠��         */
   FIELD amt-cr   AS DECIMAL   /* �㬬� �뤠��          */
   FIELD cheq-qty AS DECIMAL   /* ����� 祪��           */
   FIELD currcheq AS CHARACTER /* ����� 祪��          */
   FIELD amt-cheq AS DECIMAL   /* �㬬� 祪��           */
   FIELD Summa    AS DECIMAL  /* �㬬� �����ᨩ        */
	
   INDEX idx doc-date doc-kind curr-db curr-cr currcheq
   .

DEFINE TEMP-TABLE ttReg-Det NO-UNDO

   FIELD Op        AS INTEGER      LABEL "����� ����樨"
   FIELD doc-kind AS CHARACTER  LABEL "��� ����樨"
   FIELD Currency  AS CHARACTER  LABEL "��� ������"         
   FIELD SummSign  AS CHARACTER  LABEL "�ਧ��� �㬬�"
   FIELD Summa     AS DECIMAL    LABEL "�㬬� "             
   FIELD SummaR    AS DECIMAL    LABEL "�㡫��� ��������"

   INDEX idx Op Currency SummSign
   .

beg-date = today - 1.
end-date = today - 1.
{getdates.i &noinit = YES}

{setdest.i}

ASSIGN
   mLogComm = ENTRY(1,iParam,";") EQ "�����ᨨ"
   mStatus  = ENTRY(2,iParam,";") WHEN NUM-ENTRIES(iParam,";") > 1 
   mSpace   = FILL(" ",INT(ENTRY(3,iParam,";"))) WHEN NUM-ENTRIES(iParam,";") > 2
   mStatus  = CHR(251) WHEN NOT CAN-FIND(FIRST code WHERE 
                                               code.class  EQ "�����"
                                           AND code.code   EQ mStatus)
   .
ASSIGN
   mLines[ 1] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ"
   mLines[ 2] = "����浪����६�   ���� � ����  �        ������ ������� �।�⢠       ����-��ਭ�� (�뤠��) ���ᮢ�  �    ����� ���     �����-��ࠦ-�    ��� 䨧��-     �   ���㬥��,㤮�⮢����騩    �   ���� ���� ��⥫�-   ���� � ����    ������-�"
   mLines[ 3] = "�� �����ᮢ���-������ ���-  �����������������������������������������Ĵ⥦-�ࠡ�⭨��� 祪�� (� ⮬    �                    �७- ����- �    ᪮�� ���      �   ��筮���                   �   �⢠ (���� �ॡ�-    �஦����� 䨧�-  ���   �"
   mLines[ 4] = "�����樨���� ���-����-� ��࠭-�    �ਭ��         �    �뤠��          ���� ��᫥ ��஦��� 祪��),     �                    �������⢮ �                    �                              �   �����)                ��᪮�� ���    ���    �"
   mLines[ 5] = "�        ��樨   ��- � ���   �    ���ᮢ�        �    ���ᮢ�        ����-��������쭠� �⮨����� ���-�                    �     �䨧�-�                    �                              �                         �                �����-�"
   mLines[ 6] = "�        � ��.��  �樨 � ������    ࠡ�⭨���      �    ࠡ�⭨���      ��  ���� 㪠���� � �����࠭���  �                    �     ��-  �                    �                              �                         �                ���  �"
   mLines[ 7] = "�        �        �    �(����-�                    �                    �    ������                     �                    �     �᪮- �                    �                              �                         �                �      �"
   mLines[ 8] = "�        �        �    � -����)��������������������������������������������������������������������������ĳ                    �     ���   �                    �                              �                         �                �      �"
   mLines[ 9] = "�        �        �    � �� ��-����  �              ����  �              �    �����- ����  �              �                    �     ���� �                    �                              �                         �                �      �"
   mLines[10] = "�        �        �    � ��樨�����-�    �㬬�     �����-�    �㬬�     �    ���⢮�����-�    �㬬�     �                    �     �     �                    �                              �                         �                �      �"
   mLines[11] = "�        �        �    �       ���   �              ���   �              �    �祪�� ���   �              �                    �     �     �                    �                              �                         �                �      �"
 /*mLines[12] = "�        �        �     �             �      �                   �      �                    �         �     ���     �      �                   �                     �        �                �"*/
 /*mLines[13] = "�        �        �     �             �      �                   �      �                    �         �     �����    �      �                   �                     �        �                �"*/
   mLines[14] = "����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ������������������������������������������������Ĵ"
   mLines[15] = "�   1    �    2   � 3  �   4   �  5  �      6       �  7  �      8       �  9 �  10  � 11  �      12      �         13         � 14  � 15  �        16          �              17              �           18            �       19       �  20  �"
   mLines[21] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
   mLines[22] = "�        �        �    �       � ,   �             ,�     �              �    �      �     �              �                    �     �     �                    �                              �                         �                �      �"
   mLines[23] = "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������"
   mLines[24] = "��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������"
   mLines[25] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
   mLines[26] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
   mLines[27] = "��⮣� �� �������:� ,  �       �     �              �     �              �    �      �     �             ,�                    �     �     �                    �                              �                         �                �,     �"
   mLines[28] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
   mLines[29] = "������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
   mLines[30] = "�        �        �    �       �     �              �     �              �    �      �     �              �                    �     �     �,                   �                              �                         �                �      �"
   mCols      = LENGTH(mSpace + mLines[1])
   .
&GLOBAL-DEFINE cols mCols

{agr-beg.def 
   &NameTitle = "������ �������� � ������� � ������ 136-�"
   &TypeDoc   = '"book"'
   &NameRep   = "'������136'"}

{limitsum.chk}
{rst-tab.fun}   /* ��।������ �㬬, ����� ����� � ���-�� 祪�� ��� ������� ॥��� */

/*DEFINE VARIABLE mCodOurCur    AS CHARACTER NO-UNDO INIT "810".
DEFINE VARIABLE mByRate   AS LOGICAL NO-UNDO.*/
mByRate = true.
   /* CreateTemp-Table */
   DEFINE VARIABLE vCurDprId  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vVidOpNalV AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOperIskl  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOperRate  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOperCard  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOperAcct  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vOpGr15    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAcctComm  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPechFIO   AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vPrSumm    AS CHARACTER   NO-UNDO /* ��� ��� �����ᨩ */ EXTENT 2.
   /*DEFINE VARIABLE i          AS INT64       NO-UNDO.*/
   DEFINE VARIABLE vOpTime    AS INT64       NO-UNDO.
   DEFINE VARIABLE vOpLnk     AS INT64       NO-UNDO.
   DEFINE VARIABLE vOpDate    AS DATE        NO-UNDO.
   DEFINE VARIABLE vCurrDb    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSummaDb   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vCurrCr    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSummaCr   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vQtyCheq   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vCurrCheq  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSumCheq   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vSumRub    AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vAcctCli   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCountry   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCommSign  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vCurrCom   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vSummCom   AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vSummComR  AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vForminfo  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vRate      AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vChrRate   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vIsSvod    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vOp        AS INT64       NO-UNDO.
   DEFINE VARIABLE vDover     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vSpaceRate AS CHARACTER   NO-UNDO INIT "           ".

   DEFINE BUFFER bop       FOR op.
   DEFINE BUFFER xop       FOR op.
   DEFINE BUFFER bop-entry FOR op-entry.
   DEFINE BUFFER blinks    FOR links.
   DEFINE BUFFER bsessions FOR sessions.
   DEF STREAM vvs.

   ASSIGN
      vOperIskl = FGetSetting("������136-�","�����᪫","")
      vOperRate = FGetSetting("������136-�","�����������","")
      vOperCard = FGetSetting("������136-�","��������","14,15,16,17")
      vOperAcct = FGetSetting("������136-�","��������","51,52,53,54,59,60,61,62,63,64,65")
      vAcctComm = FGetSetting("�113","��⊮���","")
      vOpGr15   = fGetSetting("�113","����15","")
      vPechFIO  = fGetSetting("��甈�","","���") EQ "��"
      vOpLnk    = getXLinkID ("opbv-svod","opvSvod")
	  .

   FOR EACH code WHERE
            code.class EQ "���㬬�" 
        AND code.val   NE ""
      NO-LOCK:
      DO i = 1 TO NUM-ENTRIES(code.val):
         {additem.i vPrSumm[1] SUBSTRING(ENTRY(i,code.val),1,5)}
         {additem.i vPrSumm[2] code.code}
      END.
   END.

   DEF BUFFER bsigns1 FOR signs.
   DEFINE VARIABLE fname AS CHAR NO-UNDO.

   fname = 'ree' + STRING(YEAR(beg-date)) + STRING(MONTH(beg-date), '99') + STRING(DAY(beg-date), '99') + ".csv".
   OUTPUT STREAM vvs TO VALUE (fname)
    UNBUFFERED CONVERT  TARGET "1251" /*UTF-8"*/  SOURCE "IBM866".

  put stream vvs unformatted 
        "���" ";"
        "���ࠧ�" ";"
        "ᬥ��" ";"
        "�����" ";"
        "� ���" ";"
        "�६�" ";"
        "��� ��." ";"
        "����"  ";"
        "����� �ਭ��" ";"
        "�㬬� �ਭ��" ";"
        "����� �뤠��" ";"
        "�㬬� �뤠��" ";"
        "��.����" ";"
        "���-祪��" ";"
        "�����" ";"
        "�㬬� 祪��" ";"
        "����� ���" ";"
        "���." ";"
        "�ࠦ����⢮" ";"
        "���" ";"
        "���㬥��" ";"
        "����" ";"
        "��� ���� ஦�����" ";"
        "�������" ";"
        SKIP
      .


   FOR EACH sessions
     WHERE sessions.filial-id EQ shFilial 
       AND sessions.op-date >= beg-date AND sessions.op-date <= end-date
     NO-LOCK /*,
      FIRST bsigns1 /*OUTER-JOIN */
        WHERE bsigns1.file-name EQ 'sessions'
          AND bsigns1.surrogate EQ STRING(sessions.dpr-id)
          AND bsigns1.code eq 'hran' NO-LOCK*/ :
      /*IF NOT AVAIL bsigns1 THEN
         PUT UNFORMATTED " ᬥ�� �� hran " + STRING(sessions.dpr-id) SKIP.*/
      vCurDprId = STRING( sessions.dpr-id).
      mCuDate = sessions.op-date.
/*Message "ᬥ�� " vCurDprId VIEW-AS ALERT-BOX.*/

      {empty ttRegister}
      {empty ttReg-Det}
      {empty ttRegItog}
      FOR EACH kau WHERE
               kau.kau-id BEGINS "��������"
           AND kau.kau    EQ     vCurDprId
         NO-LOCK,
          EACH kau-entry WHERE
               kau-entry.kau-id    EQ kau.kau-id
           AND kau-entry.kau       EQ kau.kau
           AND kau-entry.op-date   EQ mCuDate
           AND kau-entry.op-status >= mStatus
         NO-LOCK,
         FIRST xop OF kau-entry WHERE
         NO-LOCK
         BREAK 
         BY kau-entry.op:

         IF FIRST-OF(kau-entry.op) THEN 
         DO:
            vIsSvod = NO.
            FOR EACH blinks WHERE 
                     blinks.link-id   EQ vOpLnk
                 AND blinks.source-id EQ STRING(xop.op) 
               NO-LOCK:
               vOp = INT64(blinks.target-id).
               FIND FIRST op WHERE 
                          op.op = vOp
                  NO-LOCK NO-ERROR.
               {rst-136i.i}
               vIsSvod = YES.
            END.

            IF NOT CAN-FIND(blinks WHERE 
                            blinks.link-id   EQ vOpLnk 
                        AND blinks.target-id EQ STRING(xop.op) NO-LOCK)
               AND vIsSvod EQ NO THEN
            DO:
               FIND FIRST op WHERE 
                          op.op = xop.op 
                  NO-LOCK NO-ERROR.
               {rst-136i.i}
            END.
         END.
      END.
      /* �������� ������ ॥��஢ */ 
      IF fGetSetting("������136-�","�᥄�������","��") EQ "���" THEN
      DO:
         IF NOT CAN-FIND(FIRST ttRegister WHERE 
                               ttRegister.dpr-id EQ INT(vCurDprId)) THEN
         DO:
            CREATE ttRegister.
            ASSIGN
               ttRegister.dpr-id   = INT(vCurDprId)
               ttRegister.vain     = YES
               ttRegister.doc-date = mCuDate
               .
            RELEASE ttRegister.
         END.
      END.   
      ELSE
      DO: 
         FIND FIRST bsessions WHERE 
                    bsessions.dpr-id EQ INT64(vCurDprId)
            NO-LOCK NO-ERROR.
         IF AVAIL bsessions THEN
         DO:
            DO i = 0 TO ((IF bsessions.dpr-status EQ "�������" THEN bsessions.dpr-close ELSE TODAY) - bsessions.dpr-open):
               IF NOT CAN-FIND(FIRST ttRegister WHERE 
                                     ttRegister.dpr-id   EQ INT(vCurDprId)
                                 AND ttRegister.doc-date EQ bsessions.dpr-open + i) THEN
               DO:
                  CREATE ttRegister.
                  ASSIGN
                     ttRegister.dpr-id   = INT(vCurDprId)
                     ttRegister.vain     = YES
                     ttRegister.doc-date = bsessions.dpr-open + i
                     .
                  RELEASE ttRegister.
               END.
            END.                 
         END.
      END.



   /* PrintRst */
   DEFINE VARIABLE vREGN       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vLogDocNum  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vAdresPch   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBank       AS CHARACTER   NO-UNDO EXTENT 2.
   DEFINE VARIABLE vBranchAttr AS CHARACTER   NO-UNDO EXTENT 2.
   DEFINE VARIABLE vRstNum     AS INT64       NO-UNDO.
   DEFINE VARIABLE vDocNum     AS INT64       NO-UNDO.
   DEFINE VARIABLE vStrDate    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vFirst      AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE vFio        AS CHARACTER   NO-UNDO EXTENT 5.
   DEFINE VARIABLE vPasport    AS CHARACTER   NO-UNDO EXTENT 5.
   DEFINE VARIABLE vAdress     AS CHARACTER   NO-UNDO EXTENT 5.
   DEFINE VARIABLE vBirthDay   AS CHARACTER   NO-UNDO EXTENT 5.
   DEFINE VARIABLE vSessBrnch  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vKodFil     AS CHARACTER   NO-UNDO.
   /*DEFINE VARIABLE vOperAcct   AS CHARACTER   NO-UNDO.*/
   DEFINE VARIABLE vLinePtint  AS INT64       NO-UNDO.
   DEFINE VARIABLE vDoverNum   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vDoverCid   AS INT64       NO-UNDO.
   /*DEFINE VARIABLE mCuBrchID   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE mBank         AS CHARACTER NO-UNDO.*/

   DEFINE BUFFER bttRegister FOR ttRegister.
   /* DEFINE BUFFER bsessions   FOR sessions. */

   FIND FIRST bsessions WHERE bsessions.dpr-id EQ INT64( sessions.dpr-id /*mCuDprID*/ ) NO-LOCK NO-ERROR.
   IF AVAIL bsessions THEN
      vSessBrnch = bsessions.branch-id.
   mCuBrchID = bsessions.branch-id.

   RUN HeadInRep.
   RUN FootInRep.
      
   IF NOT {assigned vSessBrnch} THEN
      vSessBrnch = mCuBrchID.

   ASSIGN
      vREGN          = FGetSetting("REGN",?,"")
      vREGN          = vREGN
      vAdresPch      = FGetSetting("����_��",?,?)
      vBank[1]       = mBank
      vKodFil        = FGetSetting("������",?,"")
      vBranchAttr[1] = IF vKodFil NE vSessBrnch THEN mBranchName + " " + mVOKAddr[1]
                                                ELSE ""
      vLogDocNum     = fGetSetting("������136-�","�����","���") EQ "���"
      vOperAcct      = FGetSetting("������136-�","��������","51,52,53,54,59,60,61,62,63,64,65")
      .

   {wordwrap.i
     &s = vBank
     &n = 2
     &l = 60
   }
   {wordwrap.i
     &s = vBranchAttr
     &n = 2
     &l = 60
   }

   FOR EACH ttRegister 
      NO-LOCK
      BREAK 
      BY ttRegister.dpr-id
      BY ttRegister.doc-date 
      BY ttRegister.doc-time:
      IF FIRST-OF(ttRegister.dpr-id) THEN
      DO:
         vRstNum = 0.
         RUN GetRepFioByRef(mNameRep, mCuBrchID,ttRegister.op,ttRegister.dpr-id).
      END.
      IF FIRST-OF(ttRegister.doc-date) THEN
      DO:
         ASSIGN
            vRstNum  = vRstNum + 1 WHEN NOT ttRegister.vain
            vDocNum  = 0
            vStrDate = STRING(ttRegister.doc-date,"99.99.9999") /* {strdate.i ttRegister.doc-date} */
            .
         IF ttRegister.vain THEN
         DO:
            PUT UNFORMATTED
               mSpace SPACE(3) "������ (᮪�饭���) �ଥ���� ������������"          SPACE(11) vBank[1]       SKIP
               mSpace SPACE(3) "㯮�����祭���� �����"                                SPACE(33) vBank[2]       SKIP
               mSpace SPACE(3) "(������������ 䨫����)"                                                        SKIP
               mSpace SPACE(3) "�������樮��� ����� 㯮�����祭���� �����/"         SPACE(10) STRING(vREGN)  SKIP
               mSpace SPACE(3) "���浪��� ����� 䨫����"                                                      SKIP
               mSpace SPACE(3) "���⮭�宦����� (����) 㯮�����祭���� �����"        SPACE(9) vAdresPch       SKIP
               mSpace SPACE(3) "(䨫����)"                                                                     SKIP
               mSpace SPACE(3) "������������ ����७���� ������୮�� ���ࠧ�������"  SPACE(3) vBranchAttr[1]  SKIP
               mSpace SPACE(3) "㯮�����祭���� ����� � ��� ���⮭�宦����� (����)"  SPACE(3) vBranchAttr[2]  SKIP
               mSpace SPACE(3) "��� ���������� �ࠢ��"
                      SPACE(31) "���Ŀ ���Ŀ �������Ŀ" SKIP
               mSpace SPACE(57) "�" SUBSTRING(vStrDate,1,1) "�" SUBSTRING(vStrDate,2,1) "�.�" SUBSTRING(vStrDate,4,1) "�" SUBSTRING(vStrDate,5,1)
               "�.�" SUBSTRING(vStrDate,7,1) "�" SUBSTRING(vStrDate,8,1) "�" SUBSTRING(vStrDate,9,1) "�" SUBSTRING(vStrDate,10,1) "�" SKIP
               mSpace SPACE(57) "����� ����� ���������" SKIP
               mSpace SPACE(57) "����  �����    ���   " SKIP


               PADC("������� �� ���������� �������� � �������� ������� � ������",110)                   SKIP(1)
               mSpace SPACE(3) "� �祭�� ࠡ�祣� ��� ����樨 � ����筮� �����࠭��� ����⮩ � 祪��� �� �����⢫﫨��." SKIP(1)
            .
            DO i = 1 TO mTotalSign:
               RUN PrintFioAndPostP(CutFio(mFioInRep[i]),"���ᮢ� ࠡ�⭨�",length(mSpace)).
            END.
            NEXT.
         END.
         PUT UNFORMATTED
            mSpace SPACE(3) "������ (᮪�饭���) �ଥ���� ������������"          SPACE(11) vBank[1]              SKIP
            mSpace SPACE(3) "㯮�����祭���� �����"                                SPACE(33) vBank[2]              SKIP
            mSpace SPACE(3) "(������������ 䨫����)"                                                               SKIP
            mSpace SPACE(3) "�������樮��� ����� 㯮�����祭���� �����/"         SPACE(10) STRING(vREGN)         SKIP
            mSpace SPACE(3) "���浪��� ����� 䨫����"                                                             SKIP
            mSpace SPACE(3) "���⮭�宦����� (����) 㯮�����祭���� �����"        SPACE(9) vAdresPch              SKIP
            mSpace SPACE(3) "(䨫����)"                                                                            SKIP
            mSpace SPACE(3) "������������ ����७���� ������୮�� ���ࠧ�������"  SPACE(3) vBranchAttr[1]         SKIP
            mSpace SPACE(3) "㯮�����祭���� ����� � ��� ���⮭�宦����� (����)"  SPACE(3) vBranchAttr[2]         SKIP
            mSpace SPACE(3) "��� ���������� ॥���"
                   SPACE(31) "���Ŀ ���Ŀ �������Ŀ" SKIP
            mSpace SPACE(57) "�" SUBSTRING(vStrDate,1,1) "�" SUBSTRING(vStrDate,2,1) "�.�" SUBSTRING(vStrDate,4,1) "�" SUBSTRING(vStrDate,5,1)
            "�.�" SUBSTRING(vStrDate,7,1) "�" SUBSTRING(vStrDate,8,1) "�" SUBSTRING(vStrDate,9,1) "�" SUBSTRING(vStrDate,10,1) "�" SKIP
            mSpace SPACE(57) "����� ����� ���������" SKIP
            mSpace SPACE(57) "����  �����    ���   " SKIP(1)
            mSpace SPACE(3) "���浪��� ����� ������ � �祭�� ࠡ�祣� ���"      SPACE(7) vRstNum SKIP
            PADC("������ ����権 � ����筮� ����⮩ � 祪���",mCols)                                       SKIP(1)
         .
         DO i = 1 TO 20:
            IF mLines[i] NE "" THEN
               PUT UNFORMATTED mSpace mLines[i] SKIP.
         END.
      END.
      
      IF     LAST-OF(ttRegister.doc-date) 
         AND NOT CAN-FIND(FIRST ttRegItog WHERE 
                                ttRegItog.doc-date EQ ttRegister.doc-date) THEN
      DO:
         vLinePtint = GetFioPostLine() + 2.
         IF mLogComm THEN
            FOR EACH ttReg-Det WHERE
                     ttReg-Det.op EQ ttRegister.op
               NO-LOCK:
               vLinePtint = vLinePtint + 1.
            END.         
         IF LINE-COUNTER + vLinePtint > PAGE-SIZE THEN 
            PAGE.         
      END.
     
     IF CAN-DO(vOperAcct,ttRegister.doc-kind) AND NOT ttRegister.dover THEN 
        RUN GetFioByAcct(ttRegister.op,OUTPUT vFio[1],OUTPUT vPasport[1],OUTPUT vAdress[1],OUTPUT vBirthDay[1]).
     ELSE 
        IF ttRegister.dover THEN DO:
         vDoverNum = GetXAttrValue("op", STRING(ttRegister.op), "proxy-code").
         FIND FIRST loan 
             WHERE loan.cont-code = vDoverNum
             AND   loan.contract  = "proxy"
         NO-LOCK NO-ERROR.
         IF AVAIL loan THEN
            vDoverCid = INTEGER(GetXAttrValue("loan", loan.contract + "," + vDoverNum, "agent-id")).
            FIND FIRST person
                 WHERE person.person-id = vDoverCid
            NO-LOCK NO-ERROR.
            IF AVAIL person THEN
               ASSIGN
                vFio[1]      = person.name-last + " " + person.first-names
                vAdress[1]   = person.address[1]
                vBirthDay[1] = STRING(person.birthday) + " " +
                               GetXAttrValueEx("person",STRING(person.person-id),"BirthPlace","")
                vPasport[1]  = person.document-id + " " + person.document + " " + fGetDocIssue(person.person-id)
               .
        END.
        ELSE
          ASSIGN
          vFio[1]      = GetXAttrValue("op", STRING(ttRegister.op), "���")
          vAdress[1]   = GetXAttrValue("op", STRING(ttRegister.op), "����")
          vBirthDay[1] = GetXAttrValue("op", STRING(ttRegister.op), "birthday") + "  " +
                     GetXAttrValueEx("op", STRING(ttRegister.op), "birthPlace","")
          vPasport[1]  = GetPassport(ttRegister.op)
          .

      vDocNum = vDocNum + 1.


     /* � 䠩� */

     PUT STREAM vvs UNFORMATTED
        STRING( ttRegister.doc-date) ";"
        '="' mCuBrchID '"' ";"
        ttRegister.dpr-id ";"
        mFIOInRep[ 1] ";"
        STRING(IF vLogDocNum THEN vDocNum ELSE ttRegister.doc-num,">>>>>9") ";"
        STRING(ttRegister.doc-time,"HH:MM") ";"
        STRING('="' + ttRegister.doc-kind + '"') ";"
        (IF (ttRegister.amt-db EQ 0 OR ttRegister.amt-cr EQ 0 OR ttRegister.card EQ TRUE) THEN "" ELSE STRING(TRIM(STRING(ttRegister.ChRate))))  ";"
        STRING(ttRegister.curr-db) ";"
        (IF ttRegister.amt-db NE 0 THEN STRING(ttRegister.amt-db, ">>>,>>>,>>9.99") ELSE "") ";"
        STRING(ttRegister.curr-cr) ";"
        (IF ttRegister.amt-cr NE 0 THEN STRING(ttRegister.amt-cr, ">>>,>>>,>>9.99") ELSE "") ";"
        STRING(ttRegister.card,"X/")  ";"
        (IF ttRegister.cheq-qty NE 0 THEN STRING(ttRegister.cheq-qty, ">>>,>>9") ELSE "") ";"
        STRING(ttRegister.currcheq) ";"
        (IF ttRegister.amt-cheq NE 0 THEN STRING(ttRegister.amt-cheq, ">>>,>>>,>>9.99") ELSE "") ";"
        STRING('="' + ENTRY( 1, ttRegister.acct, '@') + '"') ";"
        STRING(ttRegister.dover,"X/") ";"
        STRING(ttRegister.country) ";"
        '"' + REPLACE( REPLACE(STRING(vFio[1]), '"', "'"), ';', ":") '"' ";"
        '"' + REPLACE( REPLACE(STRING(vPasport[1]), '"', "'"), ';', ":") '"'  ";"
        '"' + REPLACE( REPLACE(STRING(vAdress[1]), '"', "'"), ';', ":") '"' ";"
        STRING(vBirthDay[1]) ";".
     FIND FIRST ttReg-Det WHERE 
             ttReg-Det.op EQ ttRegister.op 
          AND ttReg-Det.currency NE "810"
     NO-LOCK NO-ERROR.
     IF AVAIL ttReg-Det THEN 
     PUT STREAM vvs UNFORMATTED
         (IF ttReg-Det.Summa NE 0 THEN STRING(ttReg-Det.Summa, ">>9.99") ELSE "")
         SKIP.
     ELSE PUT STREAM vvs UNFORMATTED /*ENTRY(4,mLines[27])*/ SKIP.
     /* */



      {wordwrap.i
       &s = vFio
       &n = 5
       &l = 20
      }
      {wordwrap.i
       &s = vPasport
       &n = 5
       &l = 30
      }
      {wordwrap.i
       &s = vAdress
       &n = 5
       &l = 25
      }
      {wordwrap.i
       &s = vBirthDay
       &n = 5
       &l = 17
      }

      PUT UNFORMATTED 
           mSpace mLines[21] SKIP mSpace
           "� "  STRING(IF vLogDocNum THEN vDocNum ELSE ttRegister.doc-num,">>>>>9") 
          " � "  REPLACE(STRING(ttRegister.doc-time,"HH:MM"),":",".") /* STRING(ttRegister.doc-time,"HH:MM") */
         "  � "  STRING(ttRegister.doc-kind,"x(2)")
        " �"   (IF (ttRegister.amt-db EQ 0 OR ttRegister.amt-cr EQ 0 OR ttRegister.card EQ TRUE) THEN "       " ELSE STRING(TRIM(STRING(ttRegister.ChRate)), "x(7)")) 
           "� "  STRING(ttRegister.curr-db,"x(3)")
          " �"   (IF ttRegister.amt-db NE 0 THEN STRING(ttRegister.amt-db, ">>>,>>>,>>9.99") ELSE "              ")
           "� "  STRING(ttRegister.curr-cr,"x(3)")
          " �"   (IF ttRegister.amt-cr NE 0 THEN STRING(ttRegister.amt-cr, ">>>,>>>,>>9.99") ELSE "              ")
           "�  " STRING(ttRegister.card,"X/") 
          " �"   (IF ttRegister.cheq-qty NE 0 THEN STRING(ttRegister.cheq-qty, ">>>,>>9") ELSE "      ")
           "� "  STRING(ttRegister.currcheq,"x(3)")
          " �"   (IF ttRegister.amt-cheq NE 0 THEN STRING(ttRegister.amt-cheq, ">>>,>>>,>>9.99") ELSE "              ")
           "�"   STRING(ttRegister.acct,"x(20)")
           "� "  STRING(ttRegister.dover,"X/")
      "   � "  STRING(ttRegister.country,"x(3)")
        " �"   STRING(vFio[1], "x(20)")
           "�"   STRING(vPasport[1], "x(30)") 
           "�"   STRING(vAdress[1],"x(25)")
           "�"   STRING(vBirthDay[1], "x(16)").
     FIND FIRST ttReg-Det WHERE 
             ttReg-Det.op EQ ttRegister.op 
          AND ttReg-Det.currency NE "810"
     NO-LOCK NO-ERROR.
     IF AVAIL ttReg-Det THEN 
     PUT UNFORMATTED
         "�"  (IF ttReg-Det.Summa NE 0 THEN STRING(ttReg-Det.Summa, ">>9.99") ELSE "      ") "�"
         SKIP.
     ELSE PUT UNFORMATTED "� " ENTRY(4,mLines[27]) SKIP.

     DO i = 2 TO 5:
        IF    vFio[i]      NE ""
          OR vBirthDay[i] NE "" 
          OR vAdress[i]   NE "" 
          OR vPasport[i]  NE "" THEN
          PUT UNFORMATTED
            ENTRY(1,mLines[30]) 
            STRING(vFio[i], "x(20)")
         "�" STRING(vPasport[i], "x(30)") 
            "�" STRING(vAdress[i],"x(25)")
            "�" STRING(vBirthDay[i], "x(16)") "� " ENTRY(4,mLines[27])
            SKIP.
     END.


      /*IF mLogComm THEN
      DO:
         FOR EACH ttReg-Det WHERE
                  ttReg-Det.op EQ ttRegister.op
            NO-LOCK:
            PUT UNFORMATTED 
               mSpace ENTRY(1,mLines[22]) ttReg-Det.Currency "  � " STRING(ttReg-Det.SummSign,"x(3)") " " STRING(ttReg-Det.Summa, ">>>,>>>,>>9.99") ENTRY(3,mLines[22]) SKIP.
               Message STRING(ttReg-Det.Summa, ">>>,>>>,>>9.99") VIEW-AS ALERT-BOX.
         END.
      END.*/

      IF LAST-OF(ttRegister.doc-date) THEN
      DO:
         vFirst = YES.
         IF NOT CAN-FIND(FIRST ttRegItog WHERE 
                               ttRegItog.doc-date EQ ttRegister.doc-date) THEN
         DO:
            PUT UNFORMATTED 
               mSpace mLines[24] SKIP.
         END.
         ELSE
         DO:
            IF mFullItg THEN
            DO:
               PUT UNFORMATTED 
                  mSpace mLines[28] SKIP
                  mSpace "� �⮣� �� ����� ����権 � ࠧ१� ���ᮢ                                                                                                                                                           � " SKIP.
               FOR EACH ttRegItog WHERE 
                        ttRegItog.doc-date EQ ttRegister.doc-date
                    AND ttRegItog.ChRate   NE ""
                  NO-LOCK:
                  IF vFirst THEN
                  DO:
                     vFirst = NO.
                     PUT UNFORMATTED 
                        mSpace mLines[29] SKIP.
                  END.
                  ELSE
                     PUT UNFORMATTED 
                        mSpace mLines[26] SKIP.
   
                  PUT UNFORMATTED 
                     mSpace ENTRY(1,mLines[27])
                     STRING(ttRegItog.doc-kind,"x(2)")
           " �"   (IF (ttRegItog.amt-db EQ 0 OR ttRegItog.amt-cr EQ 0) THEN "       " ELSE STRING(TRIM(STRING(ttRegItog.ChRate)),"x(7)") )
            "� "  STRING(ttRegItog.curr-db,"x(3)")
           " �"   (IF ttRegItog.amt-db NE 0 THEN STRING(ttRegItog.amt-db, ">>>,>>>,>>9.99") ELSE "              ")
            "� "  STRING(ttRegItog.curr-cr,"x(3)")
           " �"   (IF ttRegItog.amt-cr NE 0 THEN STRING(ttRegItog.amt-cr, ">>>,>>>,>>9.99") ELSE "              ")
            "�    �" (IF ttRegItog.cheq-qty NE 0 THEN STRING(ttRegItog.cheq-qty, ">>>,>>9") ELSE "      ")
            "� "  STRING(ttRegItog.currcheq,"x(3)")
              " �"   (IF ttRegItog.amt-cheq NE 0 THEN STRING(ttRegister.amt-cheq, ">>>,>>>,>>9.99") ELSE "              ")
                     ENTRY(3,mLines[27]) 
                (IF ttRegItog.Summa NE 0 THEN STRING(ttRegItog.Summa,">>9.99") ELSE "      ")
             "�"   SKIP.
               END.
               vFirst = YES.
               PUT UNFORMATTED 
                  mSpace mLines[28] SKIP
                  mSpace "� �⮣� �� ����� ����権                                                                                                                                                                            � " SKIP.
            END.

            FOR EACH ttRegItog WHERE 
                     ttRegItog.doc-date EQ ttRegister.doc-date
                 AND (ttRegItog.ChRate   EQ "" OR NOT mFullItg)
               NO-LOCK
               BREAK BY ttRegItog.doc-date:
                  
               IF vFirst THEN
               DO:
                  vFirst = NO.
                   
                  PUT UNFORMATTED mSpace
                     (IF mFullItg THEN mLines[29] ELSE mLines[25]) SKIP.
               END.
               ELSE
                  PUT UNFORMATTED 
                     mSpace mLines[26] SKIP.

               IF     LAST-OF(ttRegItog.doc-date) THEN
               DO:
                  vLinePtint = GetFioPostLine() + 1.    
                  IF LINE-COUNTER + vLinePtint > PAGE-SIZE THEN
                  DO: 
                     PAGE.
                  END.   
               END.

               PUT UNFORMATTED 
                  mSpace ENTRY(1,mLines[27])
                 STRING(ttRegItog.doc-kind,"x(2)")
        " �"   (IF (ttRegItog.amt-db EQ 0 OR ttRegItog.amt-cr EQ 0) THEN "       " ELSE STRING(TRIM(STRING(ttRegItog.ChRate)),"x(7)"))
         "� "  STRING(ttRegItog.curr-db,"x(3)")
        " �"   (IF ttRegItog.amt-db NE 0 THEN STRING(ttRegItog.amt-db, ">>>,>>>,>>9.99") ELSE "              ")
         "� "  STRING(ttRegItog.curr-cr,"x(3)")
        " �"   (IF ttRegItog.amt-cr NE 0 THEN STRING(ttRegItog.amt-cr, ">>>,>>>,>>9.99") ELSE "              ")
         "�    �" (IF ttRegItog.cheq-qty NE 0 THEN STRING(ttRegItog.cheq-qty, ">>>,>>9") ELSE "      ")
         "� "  STRING(ttRegItog.currcheq,"x(3)")
        " �"   (IF ttRegItog.amt-cheq NE 0 THEN STRING(ttRegister.amt-cheq, ">>>,>>>,>>9.99") ELSE "              ")
                 ENTRY(3,mLines[27]) 
             (IF ttRegItog.Summa NE 0 THEN STRING(ttRegItog.Summa,">>9.99") ELSE "      ")
         "�"    SKIP.
            END.
            PUT UNFORMATTED 
               mSpace mLines[23] SKIP(1).
         END.

         DO i = 1 TO mTotalSign:
            RUN PrintFioAndPostP(CutFio(mFioInRep[i]),"���ᮢ� ࠡ�⭨�",length(mSpace)).
         END.

         PAGE.
         /* �ਫ������ � ॥���� */
         IF mRstPril THEN
         DO:
            vDocNum = 0.
            PUT UNFORMATTED
               mSpace SPACE(3) "���������� � ������� �������� � �������� ����������� ������� � ������"
               SKIP(1)
               mSpace SPACE(3) "���� ������୮�� ���ࠧ�������         " mVOKAddr[1]
               SKIP(1)
               mSpace SPACE(3) "��� ���������� �ਫ������               " {strdate.i mCuDate}
               SKIP(1)
               mSpace SPACE(3) "���浪��� ����� ������, � ���஬�     " SKIP
               mSpace SPACE(3) "�ਫ������� ������ �ਫ������            " STRING(vRstNum)
               SKIP(1)                                                                 
               SKIP mSpace "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ"
               SKIP mSpace "� �����         ��� 䨧��᪮�� ���         � ��� � ���� ஦�����  �                      ���� 䨧��᪮�� ���                     �         ������������ ���㬥��, 㤮�⮢����饣� ��筮���         �"
               SKIP mSpace "�      �                                      � 䨧��᪮�� ���       �                                                                 �                                                                  �"
               SKIP mSpace "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
               SKIP mSpace "�  1   �                  2                   �            3           �                                4                                �                                   5                              �"
               .
            
            FOR EACH bttRegister WHERE
                     bttRegister.dpr-id   EQ ttRegister.dpr-id
                 AND bttRegister.doc-date EQ ttRegister.doc-date NO-LOCK
               BY bttRegister.doc-time:
            
               IF CAN-DO(vOperAcct,bttRegister.doc-kind) THEN
                  RUN GetFioByAcct(bttRegister.op,OUTPUT vFio[1],OUTPUT vPasport[1],OUTPUT vAdress[1],OUTPUT vBirthDay[1]).
               ELSE
                  ASSIGN
                     vFio[1]      = GetXAttrValue("op", STRING(bttRegister.op), "���")
                     vAdress[1]   = GetXAttrValue("op", STRING(bttRegister.op), "����")
                     vBirthDay[1] = GetXAttrValue("op", STRING(bttRegister.op), "birthday") + "  " +
                                    GetXAttrValueEx("op", STRING(bttRegister.op), "birthPlace","")
                     vPasport[1]  = GetPassport(bttRegister.op)
                  .
               {wordwrap.i
                 &s = vFio
                 &n = 5
                 &l = 35
               }
               {wordwrap.i
                 &s = vBirthDay
                 &n = 5
                 &l = 20
               }
               {wordwrap.i
                 &s = vAdress
                 &n = 5
                 &l = 64
               }
               {wordwrap.i
                 &s = vPasport
                 &n = 5
                 &l = 64
               }
               /* ����� �ਫ������ */
               vDocNum = vDocNum + 1.
               PUT UNFORMATTED
                  SKIP mSpace "�����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ"
                  SKIP mSpace "�" STRING(IF vLogDocNum THEN vDocNum ELSE bttRegister.doc-num, ">>>>>9")
                       "� " STRING(vFio[1], "x(36)")
                       " � " STRING(vBirthDay[1], "x(22)")
                       " � " STRING(vAdress[1],"x(63)")
                       " � " STRING(vPasport[1], "x(64)") " �"
                  .
               DO i = 2 TO 5:
                  IF    vFio[i]      NE ""
                     OR vBirthDay[i] NE "" 
                     OR vAdress[i]   NE "" 
                     OR vPasport[i]  NE "" THEN
                     PUT UNFORMATTED
                   SKIP mSpace "�      � " STRING(vFio[i], "x(36)")
                        " � " STRING(vBirthDay[i], "x(22)") 
                        " � " STRING(vAdress[i],"x(63)")
                        " � " STRING(vPasport[i], "x(64)") " �".
               END.
            END.
            
            PUT UNFORMATTED
               SKIP mSpace "�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������"
               SKIP(1)
               .
            
            PAGE.
         END.
      END.
   END.
   END.   
output stream vvs close.
{preview.i}

RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}

PROCEDURE GetCountry.

   DEFINE VARIABLE vFIO      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vPassport AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vAddress  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vResident AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBirthDay AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vBirthPlace AS CHARACTER NO-UNDO.

   DEFINE OUTPUT PARAMETER oCountry AS CHARACTER   NO-UNDO.
   IF NOT AVAIL op THEN
      RETURN.

   oCountry = GetXAttrValue("op",STRING(op.op), "country-pers").
   IF NOT {assigned oCountry} THEN
      RUN GetClParamByAcct(op.op, YES, OUTPUT oCountry, OUTPUT vFIO, OUTPUT vPassport, OUTPUT vAddress, OUTPUT vResident,OUTPUT vBirthDay,OUTPUT vBirthPlace).
   ELSE
      IF     oCountry NE "nnn" 
         AND oCountry NE "999" THEN
      DO:
         FIND FIRST country WHERE 
                    country.country-id EQ oCountry
            NO-LOCK NO-ERROR.                
         oCountry = IF AVAILABLE country THEN
                        STRING(country.country-alt-id,"999")
                     ELSE
                        oCountry.
      END.
      ELSE
         oCountry = "".

END PROCEDURE.

FUNCTION GetPassport CHAR (iOp AS INT64):
   DEF VAR vPassport   AS CHAR NO-UNDO.
   DEF VAR vCustDocWho AS CHAR NO-UNDO.
   DEF VAR vKp         AS CHAR NO-UNDO. 
   /* �஢��塞, ������ �� ��ᯮ�� �� ���㬥�� */
   vPassport   = GetXAttrValue("op", STRING(iOp), "����").
   IF vPassport NE "" THEN
   DO:
   /* ����砥� ����� ���㬥�� */
      vKp         = GetXAttrValue("op", STRING(iOp), "���ࠧ�").
      vCustDocWho = GetXAttrValue("op", STRING(iOp), "cust-doc-who").
      IF {assigned vCustDocWho} THEN
         IF {assigned vKp} THEN 
            IF NUM-ENTRIES(vCustDocWho) > 1 THEN
               ENTRY(2,vCustDocWho) = " �\� " + vKp.
            ELSE
               vCustDocWho = vCustDocWho + ", �\� " + vKp.
      ELSE
         IF NUM-ENTRIES(vCustDocWho) > 1 THEN
            ENTRY(2,vCustDocWho) = " �\� " + ENTRY(2,vCustDocWho).

      vPassport   =  GetCodeName("�������",GetXAttrValue("op", STRING(iOp), "document-id"))  
         + ", " + vPassport
         + ", " + vCustDocWho
         + ", " + GetXAttrValue("op", STRING(iOp), "Document4Date_vid")
         .
   END.
   /* ���ࠥ� ��譨� ������ */
   vPassport = TRIM(REPLACE(REPLACE(vPassport, ", , ", ", "), ", , ", ", "), ", ").

   RETURN vPassport.
END FUNCTION.

/* ��।������ ��ࠬ��஢ ������ �� ��⠬ �஢���� */
PROCEDURE GetFioByAcct. 
   DEFINE INPUT  PARAMETER iOp       AS INT64       NO-UNDO.
   DEFINE OUTPUT PARAMETER oFio      AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oPasport  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oAddress  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oBirthDay AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE vCountry   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vResident  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE vBirthPlace  AS CHARACTER   NO-UNDO.

   RUN GetClParamByAcct(iOp, YES, OUTPUT vCountry, OUTPUT oFio, OUTPUT oPasport, OUTPUT oAddress, OUTPUT vResident, OUTPUT oBirthDay,OUTPUT vBirthPlace).
   oBirthDay = oBirthDay + " " + vBirthPlace.

END PROCEDURE. /* GetFioByAcct */

/* ��।������ ��ࠬ��஢ ������ �� ��⠬ �஢���� */
FUNCTION CutFio CHAR (iFio AS CHARACTER):

   DEFINE VARIABLE i    AS INT64       NO-UNDO.
   DEFINE VARIABLE vIO  AS CHARACTER   NO-UNDO.
   
   IF NUM-ENTRIES(iFio," ") > 2 THEN
   DO:
      DO i = NUM-ENTRIES(iFio," ") TO (NUM-ENTRIES(iFio," ") - 1) BY -1:
         vIO = SUBSTRING(ENTRY(i,iFio," "),1,1) + "." + vIO.      
      END.
      DO i = NUM-ENTRIES(iFio," ") TO (NUM-ENTRIES(iFio," ") - 2) BY -1:
         ENTRY(i,iFio," ") = " ".
      END.
   END.

   RETURN RIGHT-TRIM(iFio) + " " + vIO.

END FUNCTION. /* GutFio */

PROCEDURE CrItog.
   DEFINE INPUT  PARAMETER iChrRate AS CHARACTER   NO-UNDO.
   FIND FIRST ttRegItog WHERE 
              ttRegItog.doc-date EQ ttRegister.doc-date
          AND ttRegItog.doc-kind EQ ttRegister.doc-kind
          AND ttRegItog.ChRate   EQ iChrRate
          AND ttRegItog.curr-db  EQ ttRegister.curr-db
          AND ttRegItog.curr-cr  EQ ttRegister.curr-cr
          AND ttRegItog.currcheq EQ ttRegister.currcheq
      EXCLUSIVE-LOCK NO-ERROR.
   FIND FIRST ttReg-Det WHERE
			  ttReg-Det.Op      = ttRegister.Op 
		  AND ttReg-Det.doc-kind = ttRegister.doc-kind
		  AND ttReg-Det.currency NE "810"
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL ttRegItog THEN
   DO:
      CREATE ttRegItog.
      ASSIGN
         ttRegItog.doc-date = ttRegister.doc-date
         ttRegItog.doc-kind = ttRegister.doc-kind
         ttRegItog.ChRate   = iChrRate
         ttRegItog.curr-db  = ttRegister.curr-db 
         ttRegItog.amt-db   = ttRegister.amt-db
         ttRegItog.curr-cr  = ttRegister.curr-cr
         ttRegItog.amt-cr   = ttRegister.amt-cr
         ttRegItog.cheq-qty = ttRegister.cheq-qty
         ttRegItog.currcheq = ttRegister.currcheq
         ttRegItog.amt-cheq = ttRegister.amt-cheq
         .
	  IF AVAIL ttReg-Det THEN
		 ttRegItog.Summa   = ttReg-Det.Summa.
   END.
   ELSE DO:
      ASSIGN
         ttRegItog.amt-db   = ttRegItog.amt-db   + ttRegister.amt-db
         ttRegItog.amt-cr   = ttRegItog.amt-cr   + ttRegister.amt-cr
         ttRegItog.cheq-qty = ttRegItog.cheq-qty + ttRegister.cheq-qty
         ttRegItog.amt-cheq = ttRegItog.amt-cheq + ttRegister.amt-cheq
         .
	  IF AVAIL ttReg-Det THEN
		 ttRegItog.Summa   = ttRegItog.Summa + ttReg-Det.Summa.
	END.
END PROCEDURE.

PROCEDURE PrintFioAndPostP.

   DEFINE INPUT PARAMETER iFio    AS CHARACTER NO-UNDO. /* ��� ���஥ ���⠥�               */
   DEFINE INPUT PARAMETER iPost   AS CHARACTER NO-UNDO. /* ��������� ������ ���⠥�         */
   DEFINE INPUT PARAMETER iOtstup AS INT64     NO-UNDO. /* ������ �� ���, �᫨ �� �㦥� � 0 */

   DEFINE VARIABLE vPost AS CHARACTER NO-UNDO EXTENT 7.
   DEFINE VARIABLE vSignLength AS INT64       NO-UNDO.
   DEFINE VARIABLE i     AS INT64     NO-UNDO.

   vSignLength    = 22.
   mMaxLengthFio  = MAX(mMaxLengthFio,0).
   mMaxLengthPost = MIN(50,mMaxLengthPost).
   IF LENGTH(iPost) > 50 THEN
   DO:
      mMaxLengthPost = 50.
      vPost[1] = iPost.
      {wordwrap.i
         &s = vPost
         &n = 6 
         &l = mMaxLengthPost
      }
      DO i = 1 TO 6:
         IF vPost[i + 1] EQ "" THEN 
         DO:
            PUT UNFORMATTED 
               SPACE(iOtstup) PADC(vPost[i],mMaxLengthPost) SPACE(3) FILL('_', vSignLength) SPACE(3) PADC(iFio,mMaxLengthFio) SKIP.
            LEAVE.
         END.
         ELSE
            PUT UNFORMATTED 
               SPACE(iOtstup) PADC(vPost[i],mMaxLengthPost) SKIP.
      END.
   END.
   ELSE
   DO:
      PUT UNFORMATTED
         SPACE(iOtstup) PADC(iPost,mMaxLengthPost) SPACE(3) FILL('_', vSignLength) SPACE(3) PADC(iFio,mMaxLengthFio) SKIP.
   END.

   PUT UNFORMATTED
         SPACE(iOtstup) SPACE(mMaxLengthPost) SPACE(3) PADC("(�������)",vSignLength) SKIP(1).

END PROCEDURE.
