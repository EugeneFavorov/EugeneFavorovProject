/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2011 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: GRACCTBRW.P
      Comment: ��㧥� ��㯯 ��楢�� ��⮢
   Parameters:
         Uses:
      Used by:
      Created: 26.05.2011 14:28 ariz    
     Modified: 26.05.2011 14:28 ariz    
*/

{globals.i}             /* �������� ��६���� ��ᨨ. */
{flt-file.i}            /* ��।������ �������� �������᪮�� 䨫���. */
{ttretval.def}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */

DEF VAR in-class         AS CHAR NO-UNDO. /*  ����� ������ (��� �����䨪���) */
DEF VAR in-parent        AS CHAR NO-UNDO. /*  த�⥫� �����䨪���           */

DEF VAR mAcctGroupLinkID AS INT64  NO-UNDO.
DEF VAR mAcctSurrogate   AS CHAR   NO-UNDO.
DEF VAR mOK              AS LOG    NO-UNDO.
DEF VAR mLinkSurr        AS CHAR   NO-UNDO.

DEF BUFFER dcode FOR code.

MAIN_BLOCK:
DO:
/*   IF NOT IsUserAdm (USERID ("bisquit")) THEN                                                  */
/*   DO:                                                                                         */
/*      RUN Fill-SysMes IN h_tmess ("", "", "1", "�� �� ����� �p��� ����㯠 � �⮩ ���p��樨").*/
/*      LEAVE MAIN_BLOCK.                                                                        */
/*   END.                                                                                        */

   ASSIGN
      in-class    = GetFltVal ("class")
      in-parent   = GetFltVal ("parent")
   .
                        /* ��।��塞 ���祭�� ���� 䨫��� acct-surrogate.
                        ** ����������� ���� acct-surrogate ���� �ਧ�����
                        ** �⮡ࠦ���� ��㯯 ����㯠, �����祭��� ����,
                        ** ���ண�� ���ண� 㪠��� � ���� 䨫���. */
   mAcctSurrogate = GetFltVal ("acct-surrogate").
   /* mAcctSurrogate = "10201810100020010043     @002,". */
   IF {assigned mAcctSurrogate} THEN
   DO:
      mAcctGroupLinkID = GetXLinkID ("acct","acct-group").
      IF mAcctGroupLinkID EQ ? THEN
      DO:
         RUN Fill-SysMes IN h_tmess ("", "", "-1", "�訡�� ����ன�� ����奬�. �� ������� ��� 'acct-group' ����� ����ᠬ� 'acct' � 'acct-group'.").
         LEAVE MAIN_BLOCK.
      END.

      RUN SetFltField ("title", "����  " + mAcctSurrogate + ": ������ �������").
   END.
   
   {gracctbrw.frm}           /* ���� ��㧥�. */
   {gracctbrw.qry}           /* ������ ��� ��㧥�. */
   
   
   {navigate.cqr
      &file          = "code"
      &filt          = YES
      &AutoSigns     = YES
      &tmprecid      = YES 
      &local-recid   = YES

      &access-class  = "'code'"
      &access-surr   = "code.class + ',' + code.code"
      &access-read   = r

      &bf1           = "code.code code.name"

      &edit          = "gracctbrw.edt "
      &look          = "bis-tty.nav "
         &BEFORE-RUN-METHOD = "codvlbrw.bfe "
         &AFTER-RUN-METHOD  = "gracctbrw.aft "
         &class_upper       = "'acct-group'"
         &class_avail       = "'acct-group'"
         
      &oth6          = "flt-file.f6 "
      
      &delete        = "gracctbrw.del "
      &befdel        = "gracctbrw.bdl "
      
      &return        = "gracctbrw.ret "
   }

END.

{intrface.del}          /* ���㧪� �����㬥����. */
