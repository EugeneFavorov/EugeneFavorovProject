/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: PP-TMESS.P
      Comment: ����䥩� ��⥬��� ᮮ�饭��
   Parameters:
         Uses: 
      Used by:
      Created: 21.08.2003 TSL
     Modified: 12.05.2005 14:41 KSV      (0044952) ��������� ��६����
                                         vAnswers � vAutoAnswer.
     Modified: 10.03.2006 Om �訡��.
                        ����୮� 㤠����� ���ᠭ�� �����.
     Modified: 07.11.2006 19:39 Om       (0078824) �����஢�� ��� ࠡ��� �
                                         ���ᬠ��
     Modified: 26.10.07 ler - 83715 ���࠭��� �訡�� run-time 4088 ��� ������XL (d34)     
     Modified: 27.11.2007 18:50 KSV      (0085164) ��� ���ᬠ�� �ᯮ������ �
                                         �� ��� ���䠩�� �� � ��� ��᪢��
     Modified: 24.04.2008 09:50 Om       <comment>

<tmess.pro>     

   Init-SysMes       ���樠������ 
   Fill-SysMes       �������� � �뢮� ᮮ�饭��
   Fill-ProgressErr     ���࠭�� � �뢮��� ����ᮢ� �訡��
   End-SysMes        �뢮� ��⮪���
     
*/     

{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */

{pptmess.def}           /* ��६����, �ᯮ��㥬� ��� �㦡�.*/
{pptmess.fun}           /* �㭪樨 ��� ࠡ��� �㦡�. */
{tmess.pro}             /* ��楤��� ��� ࠡ��� �㦡�. */
{pp-tmess-api-bs.i}     /* ��楤��� ��� ࠡ��� �㦡�. */

/* ����᪠���� �� ����㧪� ������⥪�. */
PROCEDURE StartInterface.
   DEF VAR vSCI AS CHAR   NO-UNDO. /* ���祭�� SERVER-CONNECTION-ID. */
   /* ��� APPL-SRV �ନ�㥬 ��� 䠩��.
   ** Commented by KSV: �� ����᪥ ��楤��� �� WebSpeedServer ��ਡ��
   ** SESSION:REMOTE �㤥� NO, �.�. ��� ���ᬠ�� ��� ��� �� �믮������. */
   IF SESSION:REMOTE 
   THEN DO:
      /* �ਬ��� �ଠ� SESSION:SERVER-CONNECTION-ID
      ** 192.168.1.7::bq41d_bss::3097::352b2f8e7566887c:b1c5fa:10f4d15e185:-7fe1 */
      IF SESSION:REMOTE THEN vSCI  =  SESSION:SERVER-CONNECTION-ID.

      /* vSCI �� �� �����⥭ � STARTUP ��楤��. */
      vSCI  =  IF LENGTH (vSCI) GT 1
                  THEN ("(" + ENTRY (1, vSCI, ":") + ENTRY (NUM-ENTRIES (vSCI, ":"), vSCI, ":") + ")")
                  ELSE GetGUID ().
      ASSIGN
         mPrefLog =  USERID ("bisquit")   
         + vSCI
         vLogFile =  mPrefLog             + vLogFile
      .
   END.
   
   RETURN.
END PROCEDURE.
