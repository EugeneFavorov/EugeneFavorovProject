/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2015 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pp-grd.p
      Comment: ������⥪� ��楤�� ������ � ���⥦��� ��⥬�� ��த
   Parameters: ���
         Uses:
      Used BY:
      Created: 24.07.2015 KSBIS TT:0235518 ������. �ਥ� ���⥦�� �� ��⥬� "��த"
     Modified: 
*/

{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "grd"
   &LIBNAME       = "������ ����� � ��த"
   &DESCRIPTION   = "��⮤� ������ ������"
}

{globals.i}
{exchange.equ}
{g-trans.equ}

{intrface.get kau}
{intrface.get card}
{intrface.get exch}
{intrface.get pack}
{intrface.get xrkc}
{intrface.get blkob}
{intrface.get count}
{intrface.get pbase}
{intrface.get refer}
{intrface.get rfrnc}
{intrface.get strng}
{intrface.get tmess}
{intrface.get trans}
{intrface.get xclass}
{intrface.get filex}

DEFINE VARIABLE mBicMFO          AS CHAR        NO-UNDO. 
DEFINE VARIABLE mNumStr          AS INT64       NO-UNDO. 
DEFINE VARIABLE mSumm            AS DEC         NO-UNDO.
DEFINE VARIABLE vFileID          AS INT64       NO-UNDO.	
DEFINE VARIABLE vFileName        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vFileNameNew     AS CHAR        NO-UNDO.

mBicMFO = GetXAttrValueEx("branch", shFilial, "�������", "").
/* ��楤�� 㤠����� ���㬥�⮢ */

{pck-op-del.pro}


/*======================================================================================================================*/
/*=== �஢�ઠ - �᫨ ��� �����⥫� (ben-acct) ���室�� ��� ��᪨ �� �� bal-nalog, ===================================*/
/*=== � �������� ४������ ���㬥�� �� ��������� (�訡�� ������ �ࠡ��뢠��, �᫨ ��� �� ���� ४����� �� ��������): */
/*=== �����, �����, �����, �����, �����, �����, ���, �����-�����, Kpp-rec,  Kpp-send====================================*/

{pfuncdef 
   &DEFPROC="ChkBalNalog"
   &DESCRIPTION="�஢�ઠ ��������� ४����⮢"
   &PARAMETERS="['YLKM"
   &RESULT="������� ����樨"
}

PROCEDURE ChkBalNalog:

   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.


MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

IF {assigned iWOP::rec-acct} AND
   CAN-DO(FGetSetting("���","bal-nalog",""),iWOP::rec-acct) AND  
   NOT ({assigned iWOP::pokdd$} AND   
        {assigned iWOP::poknd$} AND   
        {assigned iWOP::poknp$} AND   
        {assigned iWOP::pokop$} AND   
        {assigned iWOP::pokst$} AND   
        {assigned iWOP::poktp$} AND   
        {assigned iWOP::kbk-rec} AND   
        {assigned iWOP::okato-rec} AND   
        {assigned iWOP::kpp-rec} AND   
        {assigned iWOP::kpp-send})

THEN
DO:
   RUN AddErrorFormat IN h_exch (iWOP,iList) .
END. 

vFlagSet = YES.

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. 



/*===============================================================================================*/
/*=== �஢�ઠ �� ᮢ������� ��� ���⥫�騪� � ��� 䨫�����======================================*/

{pfuncdef 
   &DEFPROC="ChkBIK"
   &DESCRIPTION="�஢�ઠ �� ᮢ������� � ��� 䨫����"
   &PARAMETERS="��� ���⥫�騪�"
   &RESULT="������� ����樨"
}

PROCEDURE ChkBIK:

   DEF INPUT PARAM iWOP   AS HANDLE NO-UNDO.
   DEF INPUT PARAM iList  AS CHAR   NO-UNDO.

DEF VAR vFlagSet  AS  LOG     NO-UNDO INIT ?.


MAIN:
DO ON ERROR UNDO MAIN, LEAVE MAIN:
   {do-retry.i MAIN}

IF mBicMFO NE iWOP::bank-bik-send THEN
DO:
   RUN AddErrorFormat IN h_exch (iWOP,iList) .
END. 

vFlagSet = YES.

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */

{doreturn.i vFlagSet}

END PROCEDURE. 


{pfuncdef 
   &DEFPROC="GorodReeImp"
   &DESCRIPTION="������ ��ப ॥��� �� ��⥬� ��த"
   &PARAMETERS="��� �����,㪠��⥫� �� ��ꥪ�"
   &RESULT="������� ����樨"
}

PROCEDURE GorodReeImp:



   DEF INPUT PARAM iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.


DEFINE VARIABLE vPacketID        AS INT64       NO-UNDO.
DEF VAR vBicMask  AS  CHAR  NO-UNDO.  /* ��᪠ �����⨬�� ���                                    */
DEF VAR vCorrAcct  AS  CHAR  NO-UNDO.  /* ��᪠ �����⨬�� ���                                    */
DEF VAR vFlagSet  AS  LOG   NO-UNDO INIT ?.
DEF VAR vFlagPub   AS  LOG     NO-UNDO INIT NO.
DEF VAR vFilialId AS  CHAR  NO-UNDO.  /* ��� ���ࠧ������� �����⥫� ���⥦�                    */
DEF VAR vOpKind   AS  CHAR  NO-UNDO.  /* ��� �࠭���樨 ��� ���᫥���                           */
DEF VAR vTmpStr   AS  CHAR  NO-UNDO.
DEF VAR vBuffer    AS  CHAR    NO-UNDO. /* ��ࠡ��뢠���� ��ப� �� 䠩��                       */


DEF BUFFER bAcct     FOR acct.
DEF BUFFER bCode     FOR Code.
DEF BUFFER mail-user FOR mail-user.

MAIN:
DO ON ERROR UNDO MAIN, RETRY MAIN:
   {do-retry.i MAIN}


   /* ����祭�� ��ࠡ��뢠���� ��ப�: */
   /* ����稬 ����㦥���� ��ப� � ��࠭�� �� �� ����� */
   PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).
   IF vFlagPub EQ YES THEN 
      RUN PacketTextSave IN h_pack(iExch::PacketID, vBuffer + "~n").

   /* �஢�ઠ �� ᮮ⢥�ᢨ� �ଠ�� */
   IF GetSysConf("ErrorString") NE "NoError" THEN
   DO:
      PUBLISH "I-POS-ONE-LINE" (OUTPUT vBuffer, OUTPUT vFlagPub).


         RUN Fill-SysMes IN h_tmess("", "", "0", SUBST("��ப� �� ᮮ⢥����� �ଠ��: '&1'",
                                                       vBuffer)).

      vFlagSet = YES.
      LEAVE MAIN.
   END.



   /* �஢�ઠ ����ன�� �����䨪��� EXCH-MSG */
   IF EXCH-MSGBuff(iExch::mail-format, BUFFER bCode) NE YES THEN
      UNDO MAIN, LEAVE MAIN.



   RUN InstanceCreate IN h_exch(?, iExch).


   /*=== ���樠������ ��६����� ===*/
   ASSIGN
      vOpKind              = TRIM(iExch::op-kind)
      iExch::doc-num       = TRIM(iExch::doc-num2)
      iExch::acct-rec      = TRIM(iExch::rec-acct)
      iExch::bank-bik-send = TRIM(iExch::bank-bik-send)
      iExch::bank-code-send = TRIM(iExch::bank-bik-send)
      iExch::bank-code-rec  = TRIM(iExch::bank-bik-rec)
      iExch::ben-acct      = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::rec-acct)
      iExch::Amt-rub       = ABS(INT64(iExch::Amt-rub) / 100)

      /* ������ acct-cr �������� �� 蠡���� */
      /* �᫨ ���� ᮢ������, � ����७��� ���⥦, ���� ���������᪨� */
/*      iExch::acct-cr       = IF mBicMFO EQ iExch::bank-bik-rec THEN TRIM(iExch::rec-acct) ELSE iExch::acct-cr*/

      iExch::name-ben      = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::name-rec) 
      iExch::inn           = IF mBicMFO EQ iExch::bank-bik-rec THEN "" ELSE TRIM(iExch::inn-rec) 
      iExch::details       = TRIM(iExch::details)
      iExch::kpp-rec       = TRIM(iExch::kpp-rec)
      iExch::kbk$           = TRIM(iExch::kbk-rec)
      iExch::okato-nalog$   = TRIM(iExch::okato-rec)
      iExch::order-pay     = TRIM(iExch::order-pay)

      /* ��᪠ �����⨬�� ��� ������ �����⥫�� */
      vBicMask             = TRNSettingValue("","BICMask","!*")

   NO-ERROR. {&NO-ERROR}


/* ������ acct-cr �������� �� 蠡���� */
/*
   vCorrAcct = GetRefVal("���", gend-date, SUBST("&1,&2", "�����" , mBicMFO)).

   /* ��� ᮢ������ �� ��᪥ � ���� � ���, � ���⥦ ���䨫.*/
   IF CAN-DO(vBicMask, iExch::bank-bik-rec) AND {assigned iExch::bank-bik-rec} AND {assigned vCorrAcct}  THEN
   DO:
      iExch::acct-cr = vCorrAcct.
   END.
*/
/*
   /* �㬥��� ���㬥�⮢ */
   iExch::doc-num            = SetCounterValue(TRIM(iExch::CounterName), ?, gend-date).

   IF NOT {assigned iExch::doc-num} THEN
   DO:
      RUN Fill-SysMes IN h_tmess("","","-1","�訡�� ����祭�� ���祭�� ��� ���稪� ���㬥�⮢").
      LEAVE MAIN.

   END. /* IF NOT {assigned iExch::doc-num} THEN */
*/

   /*=== ������⨧��� ���㬥�� ===*/
   vOpKind = GetEXCHOpKind(0, {&DIR-IMPORT}, BUFFER mail-user) NO-ERROR.
   IF    iExch::op-kind EQ vOpKind   /* ��������� � ⥪�饩                     */
      OR ERROR-STATUS:ERROR          /* �������� �訡�� � �㭪樨 ������⨧�樨 */
      OR NOT {assigned vOpKind}      /* ��� �࠭���樨 ����                     */
   THEN
   DO:
      vTmpStr = "�� ��।����� �࠭����� ᮧ����� ���㬥��".
      RUN Fill-SysMes IN h_tmess("","","-1",vTmpStr).
      LEAVE MAIN.
   END. /* IF    iExch::op-kind EQ vOpKind */
   ELSE
      iExch::op-kind = vOpKind.

   /*=== ����᪠�� �࠭����� ���᫥��� ===*/

   RUN RunTransaction IN h_pbase (iExch::op-kind) NO-ERROR. 

   /*=== �஢�ઠ ᮧ����� ���㬥�� ===*/
   IF iExch::op EQ "0" THEN
   DO:

      vTmpStr = "�訡�� ᮧ����� ���㬥�� � ����஬ '&1'".
      RUN Fill-SysMes IN h_tmess("","","-1",SUBST(vTmpStr, iExch::doc-num)).
      LEAVE MAIN.
   END. /* IF iExch::op EQ "0" THEN */


   vFlagSet = YES.

   mNumStr = mNumStr + 1.
   mSumm   = mSumm   + iExch::Amt-rub.



/*----------------------------------------------------------- ���ઠ ���� --*/
   RUN InstanceJunk (iExch,    0).

END. /* MAIN: DO ON ERROR UNDO MAIN, RETRY MAIN: */



{doreturn.i vFlagSet}



END PROCEDURE. 



PROCEDURE GorodReeImpFF:
   DEF INPUT PARAM iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.

   RUN Fill-SysMes IN h_tmess("","","",SUBST("�ᥣ� ����㦥�� ���⥦�� &1 �� �㬬�: &2",STRING(mNumStr),STRING(mSumm," -zzz,zzz,zzz,zz9.99"))).

   mNumStr = 0.
   mSumm   = 0.

END PROCEDURE. 


PROCEDURE  GorodReeImpFH:

   DEFINE INPUT  PARAMETER iFormat  AS CHARACTER  NO-UNDO.
   DEF INPUT PARAM iExch    AS  HANDLE  NO-UNDO.

   vFileID     = INT64(iExch::FileExchID).
   vFileName   = GetPureName(FileGetPath(vFileID)).

   IF vFileNameNew NE vFileName THEN
   DO:
      RUN Fill-SysMes("","","", SUBST("��ࠡ��뢠���� 䠩�: &1",vFileName)).
      vFileNameNew = vFileName.
   END.
END PROCEDURE.