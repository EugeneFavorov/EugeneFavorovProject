{globals.i}

/* +++ op-ened.p was humbly modified by (c)blodd converter v.1.11 on 6/19/2017 7:03am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2017 �� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: OP-ENED.P
      Comment: ������஢���� �஢����.
   Parameters: ���
         Uses:
      Used by:
      Created: ??/??/???? ???
     Modified: 10/10/00 Om ������� �㡫����� �������� �� ���������
                           ���� ��� ����⭮� �㬬�.
     Modified: 10/10/01 NIK ����஫� ���४����⮢ acct-db � acct-� �� ���㬥��
                            �������� ��⮢ ��७�ᥭ� �� op-ened.ed � �ਣ���.
     Modified: 03/06/02 NIK �맮� � ०��� ।���஢���� ����� �஢����,
                            ���� ��㯯� �஢����
     Modified: 11.09.2002 Gunk signs.fun -> intrface.get xclass
     Modified: 22.05.2004 17:29 KSV      (0030403) ������⥫�� ��ࠢ�����.
     Modified: 21.10.2004 kraw (0036155) �⪫�祭�� �஢�ન �஢���� �� ����஥筮�� 
             :                           ��ࠬ���� opEntUpdVldOff
     Modified: 22.09.2005 14:57 KSV      (0046989) ����㧪� PP-OP.P �뭥ᥭ�
                                         ������.
     Modified: 14.03.2006 13:24 IGIV     
     Modified: 18.06.2006 Om  ��ࠡ�⪠.
                        �஢�ઠ �ࠢ ����㯠 �����⢫���� � �஢����.
*/
{globals.i}             /* �������� ��६���� ��ᨨ. */

{intrface.get instrum}  /* ������⥪� ��� ࠡ��� � 䨭. �����㬥�⠬�. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get op}       /* ������⥪� ��� ࠡ��� � ���㬥�⠬�. */
{intrface.get rights}   /* ������⥪� ��� ࠡ��� � �ࠢ��� � ��஫ﬨ. */
{intrface.get tmcod}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{intrface.get acct}
{intrface.get xclass}
{chkopmf.i}

{formpar.i}
{save_kau_info.i}
{dps-open-lnk.pro}
{sesslimit.fun}

DEF VAR in-cat     LIKE acct.acct-cat NO-UNDO.
DEF VAR in-op-date AS DATE            NO-UNDO.
DEF VAR flag-date  AS LOGICAL         NO-UNDO.
DEF VAR in-single  AS LOGICAL         NO-UNDO.

DEF VAR in-user-id      LIKE op-entry.user-id   NO-UNDO.
DEF VAR in-op           LIKE op.op              NO-UNDO.
DEF VAR cur-op-date     LIKE in-op-date         NO-UNDO.
DEF VAR vKeepDB         AS CHAR   NO-UNDO.
DEF VAR vKeepCR         AS CHAR   NO-UNDO.
DEF VAR flag-error      AS INT64    NO-UNDO.
DEF VAR mContinueEdit   AS LOG    NO-UNDO.
DEF VAR vAcctId         AS CHAR   NO-UNDO.
DEF VAR vNumber         AS CHAR   NO-UNDO.
DEF VAR flager          AS INT64    NO-UNDO.
DEF VAR fmt             AS CHAR   NO-UNDO
                        INIT "x(25)".
DEF VAR mRightsResult   AS LOG    NO-UNDO.
DEFINE VARIABLE mStrTMP AS CHARACTER NO-UNDO.
DEFINE VARIABLE mFmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCanEdit AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR vKau-num AS INT64  NO-UNDO.
DEF VAR vKau-Amt AS DEC  NO-UNDO.
DEF VAR vOp-Amt  AS DEC  NO-UNDO.
DEF VAR vAmtTot  AS INT64  NO-UNDO.
DEF VAR vSaveKau AS LOG  NO-UNDO.
DEF VAR vMess    AS CHAR NO-UNDO.

DEFINE VARIABLE mZKS AS CHARACTER NO-UNDO.

DEF BUFFER xop    FOR op.
DEF BUFFER xopen  FOR op-entry.
DEF BUFFER xxopen FOR op-entry.
DEF BUFFER xacct  FOR acct.
DEF BUFFER bacct  FOR acct.

IF     in-rec-id = 0
   AND iMode = 'F1' THEN
   RETURN.

ASSIGN
   in-op       = INT64(ENTRY(2,iInstanceList,CHR(3))) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  1
   in-cat      = ENTRY(3,iInstanceList,CHR(3)) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  2
   in-op-date  = DATE(ENTRY(4,iInstanceList,CHR(3))) WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  3
   flag-date   = (ENTRY(5,iInstanceList,CHR(3)) = 'yes') WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  4
   in-single   = (ENTRY(6,iInstanceList,CHR(3)) = 'yes') WHEN NUM-ENTRIES(iInstanceList,CHR(3)) >  5
NO-ERROR.

IF iSurrogate >  "" THEN DO:
   FIND FIRST op-entry WHERE
              op-entry.op       =  INT64(ENTRY(1,iSurrogate))
          AND op-entry.op-entry =  INT64(ENTRY(2,iSurrogate))
   NO-LOCK NO-ERROR.
   IF NOT AVAIL op-entry THEN
      RETURN.
   in-rec-id = RECID(op-entry).
   FIND FIRST op OF op-entry NO-LOCK NO-ERROR.
END.
ELSE
   FIND FIRST op WHERE 
        RECID(op) = in-op NO-LOCK NO-ERROR.

IF NOT AVAIL op THEN
   RETURN.
ASSIGN
   in-user-id = op.user-id
   in-op      = op.op
.

IF iMode =  "F9" THEN
DO:
   IF     in-user-id <> USERID ("bisquit")
      AND NOT GetSlavePermission (USERID ("bisquit"), in-user-id, "w")
   THEN RETURN.
END.



mRightsResult = YES.
   /* �஢��塞 �ࠢ� �� ��ᬮ��/।���஢���� �஢���� */
IF AVAIL op-entry THEN
BLK:
DO:
         /* ����� */
   FIND FIRST bacct WHERE bacct.acct =  op-entry.acct-db
                      AND bacct.curr =  op-entry.currency
   NO-LOCK NO-ERROR.
   IF     AVAIL bacct
      AND bacct.cust-cat =  "�"
      AND NOT GetPersonPermission(bacct.cust-id)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(bacct.cust-id)).
      mRightsResult = NO.
      LEAVE BLK.
   END.
         /* �।�� */
   FIND FIRST bacct WHERE bacct.acct =  op-entry.acct-cr
                      AND bacct.curr =  op-entry.currency
   NO-LOCK NO-ERROR.
   IF     AVAIL bacct
      AND bacct.cust-cat =  "�"
      AND NOT GetPersonPermission(bacct.cust-id)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "ap16", "", "%s=" + STRING(bacct.cust-id)).
      mRightsResult = NO.
      LEAVE BLK.
   END.
END.
IF NOT mRightsResult THEN RETURN.

IF CheckSrc(op.class-code,op.op) OR CheckTrg(op.class-code,op.op,mTargetId) THEN DO:
   RUN Fill-SysMes IN h_tmess ("", "", "", "��������! ������஢���� ���㬥�� ����������. ���� �易��� �室�騩 ���㬥�� � ��㣮� 䨫����.").
   RETURN.
END.

RUN chk-ed-klb.p(op.op,OUTPUT mCanEdit).
IF NOT mCanEdit THEN DO:
   RUN Fill-SysMes IN h_tmess ("", "", "", "��������! ������஢���� ���㬥�� ����������.~n ���㬥�� �� ������-�����.").
   RETURN.
END.


FIND FIRST op-kind  OF op      NO-LOCK NO-ERROR.
FIND FIRST op-template OF op-kind NO-LOCK NO-ERROR.
FIND FIRST op-kind-tmpl OF op-kind WHERE NO-LOCK NO-ERROR.

IF    NOT AVAILABLE(op-kind)
   OR (    NOT AVAILABLE(op-template) 
       AND NOT AVAILABLE(op-kind-tmpl))
THEN 
DO:
   DO:
      mblodd_char_Tmp01 = pick-value.
      RUN Fill-AlertSysMes IN h_tmess("","",4,IF NOT AVAIL (op-kind) THEN ("�࠭����� ���㬥�� � ����� " + STRING(op.op-kind) + " 㤠����.~n") ELSE "" + CHR(32) + IF NOT AVAIL (op-template) AND NOT AVAIL(op-kind-tmpl) THEN "������ ���㬥�� 㤠��� ! ~n�த������ ।���஢����?" ELSE "").
      mContinueEdit = (pick-value = "YES").
      pick-value = mblodd_char_Tmp01.
   END.


   IF NOT mContinueEdit THEN 
      RETURN.
END.

FORM
   op-entry.op-date
      SKIP
   op.doc-type
   op.doc-num
      FORMAT "x(10)"
   op-entry.acct-db
      LABEL "�����"
   op-entry.kau-db
      FORMAT "x(40)"
      LABEL "��. ���"
   op-entry.acct-cr
      LABEL "�।��"
   op-entry.kau-cr
      FORMAT "x(40)"
      LABEL "��. ���"
   op-entry.currency
      FORMAT "xxxx"
      SKIP
   op-entry.value-date
   op-entry.qty
   op-entry.amt-cur
      SKIP
   op-entry.amt-rub
      SKIP
   op-entry.symbol
   op-entry.op-cod 
      FORMAT "x(20)"
   op-entry.prev-year
   op-entry.type
      SKIP
WITH FRAME edit 1 DOWN TITLE COLOR BRIGHT-WHITE "[ �������� ]" 1 COL.

ASSIGN
   cur-op-date = in-op-date
   level       = 4
.

CASE op.acct-cat:
    WHEN "d" THEN DO:
        ASSIGN
            op-entry.currency:LABEL  IN FRAME edit = "��� ��"
            op-entry.currency:HELP   IN FRAME edit = "��� 業��� �㬠��"
            op-entry.currency:FORMAT IN FRAME edit = "x(12)"
        .
    END.
END CASE.

ON LEAVE OF op-entry.symbol IN FRAME edit 
DO:
   mZKS = GetXAttrValueEx("op-entry",STRING(op-entry.op) + "," + 
                                     STRING(op-entry.op-entry),"���","").
   IF mZKS <> "" AND op-entry.symbol:SCREEN-VALUE <> "" AND iMode =  "F9" THEN
   DO:
      RUN Fill-AlertSysMes IN h_tmess("","",-1,"�� �஢���� 㦥 ����� �������ᮢ� ���ᮢ� ᨬ��� ��� = " + STRING(mZKS) + "." + CHR(32) + "~n" + CHR(32) + "��⠭���� �����ᮢ��� ���ᮢ��� ᨬ���� ����������.").

      {return_no_apply.i}
   END.
END.
ON GO OF FRAME edit ANYWHERE
DO:
   IF iMode =  "F9" THEN
   DO:
      APPLY "LEAVE" TO op-entry.symbol.
      IF mZKS <> "" AND op-entry.symbol:SCREEN-VALUE <> "" THEN
      DO:
         APPLY "ENTRY" TO op-entry.symbol.
         {return_no_apply.i}
      END.
   END.
   RETURN.
END.

{rec-ed.i
   &FormLD        = YES
   &ModView       = YES
   &file          = op-entry
   &access-class  = op-entry.class-code
   &access-surr   = STRING(op-entry.op)+","+STRING(op-entry.op-entry)
   
   &ef         = "op-entry.uf "
   &befupd     = "op-en.bup "
   &update     = "op-ened.upd "
   &postfind   = "op-entry.fnd "
   &create     = "op-en(o).cr "
   &lookup     = "op-en(o).nau "
   &eh         = "op-entry.eh "
   &editing    = "op-entry.ed "
   &look       = "op-entry.nav &cr=-cr &db=-db &op-entry=op-entry. "
   &Offques    = "/*"
   &Ofseries   = "/*"
   &Ofstatus   = "/*"
   &open-undo  = "undo outr,retry outr "
   &Offopupd   = "IF FGetSetting('opEntUpdVldOff', '', '') NE '��' THEN DO: &SCOPED-DEFINE compile_comment_begin yes"
}

{intrface.del}          /* ���㧪� �����㬥����. */
/* $LINTFILE='op-ened.p' */
/* $LINTMODE='11,-9,6,3' */
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTUSER='soav' */
/* $LINTDATE='07/03/2017 11:27:43.383+03:00' */
/*prosign6UMR8dXLy3drwVI8m+xyqw*/
/* --- op-ened.p was humbly modified by (c)blodd converter v.1.11 on 6/19/2017 7:03am --- */
