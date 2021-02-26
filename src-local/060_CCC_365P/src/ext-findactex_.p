/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ext-findactex_.p
      Comment: TT:0259807 ������. 365-� �।��⠢����� ��饩 �믨᪨ �� ��⠬ �� � ��
   Parameters:
         Uses:
      Used by:
      Created: 22/03/2014 15:15 KMBIS ���室��� ��� ���㧪� ������ �� ���譥� ��⥬�.
                                      ���� ��� �� ���譥� ��⥬� � ��⮬ ४����⮢ ������.
     Modified: 
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.
{globals.i}
{extexch.def} /* ����ন� ���ᠭ�� �६����� ⠡���� ttExtAcct */

DEF INPUT  PARAM iInn        AS  CHAR   NO-UNDO.   /* ��� ������                                */
DEF INPUT  PARAM iKpp        AS  CHAR   NO-UNDO.   /* ��� ������                                */
DEF INPUT  PARAM iName       AS  CHAR   NO-UNDO.   /* ������������ ������                       */
DEF INPUT  PARAM iAcct       AS  CHAR   NO-UNDO.   /* ����� ���                                */
DEF INPUT  PARAM iAllFilials AS  LOG    NO-UNDO.   /* �� �ᥬ 䨫����� ��� ⮫쪮 ⥪�騩        */ 
DEF OUTPUT PARAM TABLE       FOR ttExtAcct.        /* ������ �� �������� ��⠬                */

DEFINE VARIABLE tthndl AS handle NO-UNDO.
DEFINE VARIABLE res    AS INTEGER NO-UNDO.
DEFINE VARIABLE bh AS HANDLE NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE mCID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCIDIP AS CHARACTER NO-UNDO.

/*
   �᫨ ��� ������ � ��ன ��⥬�, �饬 ��� �������� � ���, � ��⮬ ��।����� ���, ���
   ��।������ cust-cat, cust-id:
   1. cust-cat = "�", �᫨ ������ ��室���� � ⠡��� person
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� cust-corp
      cust-cat = "�", �᫨ ������ ��室���� � ⠡��� banks
      cust-cat = "" (����), �᫨ ������ �� ��ன ��⥬� �� ������ � ���

   2. cust-id = person.person-id/cust-corp.cust-id/banks.bank-id - ᮮ⢥��⢥���
      cust-id = -1, �᫨ ������ �� ������

   3. ������ ������ ��� ᮮ⢥���� ���� ������ � ⠡��� ttExtAcct. ���� �᫨ � ��⥬� �� 
      ��᪮�쪮, � ������� �ਭ������� ⮫쪮 ����(��� �� �ਭ�������). �᫨ ��� �ਭ�������
      ��㣮�� ������, � ��� ࠧ���� ������ ������.

   4. ��楤�� �।�ᬠ�ਢ��� ��� ०��� ࠡ���:
      1. ��।����� ����� ��� ��� �஢�ન �ਭ��������� ��� ������
      2. ��।����� ��᪠ "*" ��� �ନ஢���� ��� ��⮢ ������, ��������� ��ࠢ�� � ��.

   ���稥 ����� acct.number �� acct.acct: 
      - acct.number �� ᮤ�ন� �ਧ���� 䨫���쭮��, 
      - acct.acct ᮤ�ন� �ਧ��� 䨫���쭮��, ��᫥ @
*/

IF shFilial EQ '0000' OR shFilial EQ '0300' OR shFilial EQ '0500' THEN
DO ON ERROR UNDO, RETURN ERROR
   ON STOP UNDO, RETURN ERROR:

/*MESSAGE "ext-findactex_.p: shFilial = " shFilial*/
/*VIEW-AS ALERT-BOX.                              */

   IF (NOT CONNECTED("bank")) OR (NOT CONNECTED("bismfr"))
      THEN CONNECT -pf VALUE("/home2/bis/quit41d/conf/bismfr.pf").

   IF iAcct EQ "*" THEN
   DO:
      IF     {assigned iInn} 
         AND LENGTH(iInn) EQ 10 THEN
      DO:
         FIND FIRST cust-corp WHERE
            cust-corp.inn EQ iInn
         NO-LOCK NO-ERROR.
         IF AVAIL(cust-corp) THEN
         ASSIGN
            mCID = GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"CID","").
      END.
      ELSE IF {assigned iInn} 
         AND  LENGTH(iInn) EQ 12 THEN
      DO:
         FIND FIRST person WHERE
            person.inn EQ iInn
         NO-LOCK NO-ERROR.
         IF AVAIL(person) THEN
         ASSIGN
            mCID   = GetXAttrValueEx("person",STRING(person.person-id),"CID","")
            mCIDIP = GetXAttrValueEx("person",STRING(person.person-id),"CIDIP","").
         IF {assigned mCID}
            AND {assigned mCIDIP} 
         THEN mCID = mCID + "," + mCIDIP.
      END.
/*MESSAGE "ext-findactex_.p: mCID = " mCID*/
/*VIEW-AS ALERT-BOX.                      */
      IF {assigned mCID} THEN
      DO:
      	{empty ttExtAcct}      
         
         CREATE TEMP-TABLE tthndl.
         RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
         res = PROC-STATUS
            (
            "select * from BANKER.TOBIS365PCLOSED
			   where CID in (" + mCID + ")"
            ).

         bh = tthndl:DEFAULT-BUFFER-HANDLE.
         CREATE QUERY qh.
         qh:SET-BUFFERS(bh).
         qh:QUERY-PREPARE("for each " + bh:name).
         qh:QUERY-OPEN.
         REPEAT:
            qh:GET-NEXT().
            IF qh:QUERY-OFF-END THEN LEAVE.
/*MESSAGE "ext-findactex_.p: 1 buffer-field(account) = " bh:buffer-field("account"):buffer-value*/
/*VIEW-AS ALERT-BOX.                                                                            */
            RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                              iAllFilials,
                              INPUT-OUTPUT TABLE ttExtAcct).
         END.
      END.
      ELSE
      DO:
      	{empty ttExtAcct}
         
         CREATE TEMP-TABLE tthndl.
         RUN STORED-PROCEDURE bank.send-sql-statement LOAD-RESULT-INTO tthndl
         res = PROC-STATUS 
            (
            "select * from BANKER.TOBIS365PCLOSED t 
		       where t.inn = '" + iInn + "'"
            ).
      
         bh = tthndl:DEFAULT-BUFFER-HANDLE.
         CREATE QUERY qh.
         qh:SET-BUFFERS(bh).
         qh:QUERY-PREPARE("for each " + bh:name).
         qh:QUERY-OPEN.
         REPEAT:
            qh:GET-NEXT().
/*MESSAGE "ext-findactex_.p: 2 buffer-field(account) = " bh:buffer-field("account"):buffer-value*/
/*VIEW-AS ALERT-BOX.                                                                            */
            IF qh:QUERY-OFF-END THEN LEAVE.
            RUN ext-findact.p(bh:buffer-field("account"):buffer-value,
                              iAllFilials,
                              INPUT-OUTPUT TABLE ttExtAcct).
         END.
      END.
   END.
   ELSE
   DO:
      /* ���� ��� ������ */
      RUN ext-findact.p(iAcct,iAllFilials,INPUT-OUTPUT TABLE ttExtAcct).
   END.
END.

/*MESSAGE "ext-findactex_.p: before For Each ttExtAcct"*/
/*VIEW-AS ALERT-BOX.                                   */

/*For Each ttExtAcct No-Lock:                                                        */
/*   MESSAGE "ext-findactex_.p: For Each ttExtAcct: ttExtAcct.acct = " ttExtAcct.acct*/
/*   VIEW-AS ALERT-BOX.                                                              */
/*End.                                                                               */

CATCH eAnyError AS Progress.Lang.Error:
   message RETURN-VALUE + " " + eAnyError:GetMessage(1) view-as alert-box.

   RETURN ERROR RETURN-VALUE + " " + eAnyError:GetMessage(1).
END CATCH.


