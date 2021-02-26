/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: acctjoin.p
      Comment: �맮� ��楤�� �� join-��楢�� ��⮢
   Parameters: ���
         Uses:
      Used by:
      Created:
     Modified: 01.11.00 serge ��� �������᪨
     Modified: 13.02.01 SAP -  ��������� ���� inp-param � �� acctjoin  ��� ��।�� ��ࠬ��஢ � ��楤��� (�����ন������ �� 5 ��ࠬ��஢).
                               �������� join "�����ᨨ � ��業��"
     Modified 18.03.2005 13:55 grab - 0041465 - ��।�����, �⮡� ���� ��⮤�� ����奬� JOIN
     Modified: 19.07.2005 11:49 Om �訡��.
                        ��������� ���� �� ⠡��� cust-role.
     Modified: 22.09.2005 kraw (0046159) "��易��� ������" ��� ����७��� ��⮢.
     Modified: 06.03.2006 ZIAL (0025900) "�����뢠�� ��������, �易��� � ��楢� ��⮬"
     Modified: 12.12.2006 MUTA 0068805. � ���⥪�⭮� ���� ��⮢ �������� �㭪� "��宫��஢���� �㬬�"
     Modified: 22.06.2007 16:54 KSV      (0078824) ������� ࠧ�����
                                         SESSION-REMOTE
     Modified: 25.07.2007 14:13 Om       <comment>
     Modified: 30.10.2007 12:44 MUTA    0082120 ��������� ������⢥����� ���
                                         業��� �㬠� � ������� ���㫥

*/

{joinpar.i}
{globals.i}

DEF VAR mShowLine AS LOGICAL NO-UNDO.
DEF VAR mSurr     AS CHAR    NO-UNDO.

DEFINE BUFFER op-int FOR op-int. /* ���������� ����. */

FIND acct WHERE ROWID(acct) = TO-ROWID(iROWID) NO-LOCK NO-ERROR.

RUN CreateJoin( "�஢����", "op-en(a)", YES ).

IF acct.acct-cat NE "d" THEN
DO:
   RUN CreateJoin( "�������� (�����) - ��஥", "acur(a)", CAN-FIND( FIRST
         acct-cur OF acct ) ).
   RUN CreateJoin( "��������/�ண��� (�����)", "acur", acct.currency > "" ).
   RUN CreateJoin( "�������� (���.�����) - ��஥", "apos(a)", CAN-FIND( FIRST
         acct-pos OF acct ) ).
   RUN CreateJoin( "��������/�ண��� (���.�����)", "apos", YES ).
END.

RUN CreateJoin( "������ �� ����", "pos-acct", YES ).
RUN CreateJoinLd("�����஢�� ���",
                 "browseld",
                 "BlockAcct",
                 "file-name~001surrogate",
                 "acct~001" + acct.acct + ";" + acct.currency,
                 "file-name~001surrogate",
                 level + 1,
                 YES ).

IF AvailXattr("acct",acct.acct + "," + acct.currency,"sec-code")
   OR acct.acct-cat EQ "d" THEN
   RUN CreateJoin( "��������/�ண��� (������⢮)", "aqty", CAN-FIND(FIRST acct-qty OF acct)).

RUN CreateJoin( "����७�� ���", "dpty(a)", CAN-FIND( FIRST deputy OF acct ) ).

IF CAN-FIND(FIRST loan WHERE loan.cust-cat EQ acct.cust-cat  
                         AND loan.cust-id  EQ acct.cust-id   
                         AND loan.contract EQ "proxy"        
                         AND loan.class-code EQ "proxy-base")
THEN DO:                                                     

   RUN CreateJoin("����७���� ���",
                     "proxy_acct`" + acct.cust-cat + "," + STRING(acct.cust-id) + ",acct," + acct.acct + "," +  STRING(level + 1),
                     YES).

END.

RUN CreateJoin( "�����ᨨ � ��業��", "rate(ac)" + "`" + acct.acct + "," +
   acct.currency + "," + acct.contract + "," + STRING( Level + 1 ),
CAN-FIND( FIRST comm-rate WHERE
comm-rate.acct = acct.acct AND comm-rate.currency EQ acct.currency ) ).


DEFINE VARIABLE kau-proc LIKE acct.kau-id INITIAL ? NO-UNDO.
IF acct.kau-id EQ ? OR acct.kau-id EQ "" THEN
DO:
FIND FIRST bal-acct OF acct NO-LOCK NO-ERROR.
IF bal-acct.kau-id NE ? AND bal-acct.kau-id NE "" THEN
kau-proc = bal-acct.kau-id.
END.
ELSE
kau-proc = acct.kau-id.
RUN CreateJoin( "�����⨪�", "kauacctv", kau-proc NE ? ).

RUN CreateJoin( "�������⥫�� ४������", "acctsign", YES ).
RUN CreateJoinLd(
   "�������⥫�� �裡",
   "browseld",
   "links",
   "class-code"      + CHR(1) +  "surrogate-id"                   + CHR(1) +  "ActionLock",
   acct.class-code   + CHR(1) +  acct.acct + "~002" + acct.currency  + CHR(1) +  "F2",
   "class-code",
   level + 1,
   YES
).
RUN CreateJoinLd(
   "�����஭�� ���㬥���",
   "browseld",
   "eDocument",
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1)  + "surrogate",
   "eDocument"  + CHR(1) + "��"        + CHR(1) + "acct"       + CHR(1) +  acct.acct + ";" + acct.currency,
   "class-code" + CHR(1) + "contract" + CHR(1) + "file-name" + CHR(1) + "surrogate",
   level + 1,
   YES
).
/*IF IsUserAdm (USERID ("bisquit")) THEN*/
                        /* � ������ ��砥 ��㧥� �।�����祭 ��� �⮡ࠦ����
                        ** ��㯯, �����祭��� ����. F1 �⪫�祭�, �⮡� ��
                        ** �뫮 ���������� ��।���஢��� ���ᠭ�� ��㯯� �� F1->F9.
                        ** F9 �⪫�祭� �� ⮩ �� ��稭�. */
   RUN CreateJoinLD ("��㯯� ����㯠",
                     "browseld",
                     "acct-group",
                     "acct-surrogate",
                     acct.acct + "~002" + acct.currency,
/*                     "acct-surrogate"                   + CHR (1) + "ActionLock",*/
/*                     acct.acct + "~002" + acct.currency + CHR (1) + "F1~002F9",  */
                     "acct-surrogate",
                     level + 1,
                     YES).

RUN CreateJoin( "��ୠ� ���������", "hi(acct",
CAN-FIND( FIRST history WHERE
history.file-name EQ "acct" AND
history.field-ref = acct.acct + "," +
acct.currency ) ).

FIND FIRST loan-acct WHERE loan-acct.acct      EQ acct.acct
                       AND loan-acct.currency  EQ acct.currency
                       AND (loan-acct.contract EQ 'card-pers' 
                        OR loan-acct.contract  EQ 'card-corp') NO-LOCK NO-ERROR.
IF AVAILABLE loan-acct THEN
   RUN CreateJoin IN THIS-PROCEDURE( "��ୠ� �ਢ離� ��⮢",
      "hi(file`?,?," + loan-acct.contract      + ';' +
                       loan-acct.cont-code     + ';' +
                       loan-acct.acct-type     + ';' +
                       STRING(loan-acct.since) + "," +
                       "loan-acct"             + "," + 
                       STRING(level + 1),
      CAN-FIND( FIRST history WHERE 
         history.file-name EQ "loan-acct" AND
         history.field-ref EQ loan-acct.contract  + "," +
                              loan-acct.cont-code + "," +
                              loan-acct.acct-type + "," +
                              STRING(loan-acct.since))).

RUN CreateJoin( "�ࠢ��� ������", "acctml-j",YES).

RUN CreateJoin( "����饭�� �ᯮ��-������", "acctpack",
CAN-FIND( FIRST PackObject
WHERE PackObject.file-name EQ "acct" AND
PackObject.Surrogate EQ acct.acct + "," +
acct.currency ) ).

RUN CreateJoin( "��易��� ��ꥪ��",
                "a_cust",
                CAN-FIND (FIRST cust-role WHERE
                                 cust-role.FILE-NAME EQ "acct"
                          AND    cust-role.surrogate EQ acct.acct + ',' + acct.currency)).

IF acct.cust-cat EQ "�" THEN
   RUN CreateJoin( "��易��� ������", "link-cli", YES ).


RUN CreateJoin( 
"��������", 
"acct2loan" + 
   "`" + acct.acct + 
   "," + acct.currency + 
   "," + STRING( Level + 1),CAN-FIND(FIRST loan-acct WHERE loan-acct.acct EQ acct.acct)).

RUN CreateJoinLd("��� ����ᨭ��",                
                 "browseld", 
                 "acctp",
                 "link-object-id-sks",
                 acct.acct + ";" + acct.currency,
                 "",
                 level + 1,
                 fGetLinkCodeAny ("acctp","�����",
                                  acct.acct + "," + acct.currency,
                                  ?,
                                  gend-date)
                ).

FOR FIRST xlink WHERE xlink.class-code EQ "acctp"
                  AND xlink.link-code  EQ "�����"
   NO-LOCK,
    EACH  links WHERE (    links.link-id   EQ xlink.link-id                               
                       AND links.beg-date  LE gend-date
                       AND (   links.end-date GE gend-date         
                            OR links.end-date EQ ?)
                       AND links.target-id BEGINS acct.acct)        
                   OR (    links.link-id   EQ xlink.link-id                            
                       AND links.beg-date  LE gend-date        
                       AND (   links.end-date GE gend-date         
                            OR links.end-date EQ ?)        
                       AND links.source-id BEGINS acct.acct) 
   NO-LOCK:

   mSurr = IF xlink.link-direction BEGINS "s" THEN links.source-id
                                              ELSE links.target-id.

   FIND FIRST op-int WHERE op-int.file-name  EQ "acct"
                       AND op-int.surrogate  EQ mSurr
                       AND op-int.class-code EQ "����╫�" NO-LOCK NO-ERROR.

   IF NOT AVAIL op-int THEN NEXT.

   mShowLine = YES.
   LEAVE.   
END.

IF AcctLookBuffer(BUFFER acct:HANDLE) THEN
   RUN CreateJoinLd("����� ���ﭨ� ��� � ��",                
                    "browseld", 
                    "����╫�",
                    "mSCSAcct",
                    Acct.acct + ";" + acct.currency,
                    "mSCSAcct",
                    Level + 1,
                    mShowLine).

RUN CreateJoin ("���� ��� �� 302-�", 
               "actjoi302`" + acct.acct + ";" + acct.currency + "," + STRING (Level + 1), 
                GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "�����302","") NE ""
                ).

RUN CreateJoin ("������", 
                "evntjoi`" + "acct~001" + STRING(acct.acct) + "~002" + STRING(acct.currency) + "," + 
                "���� " + acct.acct  + "," +
                STRING (Level + 1), YES).

{ procjoin.i
&prefix = acct
&frametitle = "'[ ���� ' + acct.number + (if acct.currency = '' then '' else ('/' + acct.currency)) + ' ]'"
&params = "(acct.acct, acct.currency, Level + 1)"
}
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='16/06/2015 09:29:52.818+04:00' */
/* $LINTUSER='soav' */
/* $LINTMODE='1' */
/* $LINTFILE='acctjoin.p' */
/*prosignQPkrr/eKwJD4wY3tIH3dPQ*/