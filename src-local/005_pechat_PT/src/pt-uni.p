/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: pt-uni.p
      Comment: ������ᠫ쭠� ��楤�� ���� ���⥦���� �ॡ������
      Comment:
   Parameters: Input: RID -  RecID(op)
         Uses: Globals.I ChkAcces.I StrtOut3.I EndOut3.I
      Used by:
      Created: 02.03.2000 Kostik
     Modified: 11.09.2002 Gunk signs.fun -> intrface.get xclass
     Modified: 12/01/2005 kraw 0040840 ����䨪��� ����� ���⥫�騪� (�� "�106�_��⏫��")
     Modified: 20/07/2007 kraw (0026826) ������஢���筮� ������ᨮ���� ��
*/
Form "~n@(#) pt-uni.p 1.0 Kostik 02.03.2000 Kostik 02.03.2000 ������ᠫ�� �/�" with frame sccs-id width 250.

{globals.i}                                 /* �������� ��६����         */
{chkacces.i}
&GLOB tt-op-entry yes
&GLOBAL-DEFINE OFFSIGNS YES
&IF defined(NEW_1256) NE 0 &THEN
   &SCOP NFORM    611
&ELSE
   &SCOP NFORM    61
&ENDIF
{intrface.get xclass}
&SCOP TEST YES
&IF "{&TEST}" EQ "YES" &THEN
   DEF STREAM test.
   OUTPUT STREAM test TO "test.pp".
&ENDIF

{pp-uni.var}                                /* ��।������ ��६�����        */
{pt-uni.var}

DEFINE VARIABLE mDateMarcRec     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPrnStr-El-Doc   AS CHARACTER NO-UNDO EXTENT 7 FORMAT "x(27)".
DEFINE VARIABLE mMark-El-Doc     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mPrnStr-PT-UFEBS AS CHARACTER NO-UNDO EXTENT 7 FORMAT "x(27)".
DEFINE VARIABLE mMark-PT-UFEBS   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mSposobPoluch  AS CHARACTER NO-UNDO.

&IF DEFINED(FRM_PRN) EQ 0 &THEN
   {pt-uni.frm}                                /* ��।������ �३��            */
&ELSE
   {{&FRM_PRN}}
&ENDIF

{pp-uni.err}                                /* ᮮ�饭�� �� �訡���          */

{pp-uni.prg}                     /* ���ᠭ�� �⠭������ ��楤�� */
{pt-uni.prg}

{pp-uni.chk &allcur=YES &multy-op-ontry=YES}                                /* �஢�ઠ �室��� ������       */

{pp-uni.run}                                /* �����।�⢥��� ����        */
{pt-uni.run}

&IF DEFINED(pt-el) NE 0 &THEN /* ��� ���஭���� ���⥦���� �ॡ������ */
{{&pt-el} &pt-run=YES}
&ENDIF

&IF "{&TEST}" EQ "YES" &THEN
   OUTPUT STREAM test CLOSE.
&ENDIF
PROCEDURE GetHeader:
   RUN DefHeader.
   ASSIGN
      NameOrder  = "��������� ���������� N"
      NumberForm = "0401061"
   .

END PROCEDURE.

{nal_name.i}



{strtout3.i &cols=80 &option=Paged}  /* �����⮢�� � �뢮��           */
mDateCart = DATE(GetXAttrValueEx("op", STRING(op.op), "��⠏���饭����",?)).
mDateMarcRec = GetXAttrValueEx("op", STRING(op.op), "��⠎⬁���", "").
mSposobPoluch = GetXAttrValueEx("op",STRING(op.op),"���ᮡ�����","").

mMark-El-Doc = LOGICAL(GetXAttrValueEx("op-kind",
op.op-kind,
"Mark-El-Doc",
"���"), "��/���").
IF mMark-El-Doc THEN
DO:
mPrnStr-El-Doc[1] = REPLACE(FGetSetting("PrnStr-El-Doc", "", ""), "|", "~n").
{wordwrap.i &s=mPrnStr-El-Doc &n=7 &l=27}
END.

mMark-PT-UFEBS = LOGICAL(GetXAttrValueEx("op-kind",op.op-kind,"Mark-PT-UFEBS","���"), "��/���").
IF mMark-PT-UFEBS THEN
DO:
 mPrnStr-PT-UFEBS[1] = REPLACE(FGetSetting("PrnStr-PT-UFEBS", "", ""), "|", "~n").
 {wordwrap.i &s=mPrnStr-PT-UFEBS &n=7 &l=27}
END.

{pt-uni.prn}                                /* �����।�⢥��� �뢮�         */

&IF defined(NEW_1256) NE 0 AND defined(NO_GRAPH) EQ 0 &THEN
IF iDoc EQ 1 THEN DO:
   {lazerprn.lib &DATARETURN = 0401061.prg
                 &FORM-DOC   = "{&NFORM}"}
END.
&ENDIF
{endout3.i  &nofooter=yes}                   /* �����襭�� �뢮��             */
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='20/11/2014 16:43:41.414+04:00' */
/* $LINTFILE='pt-uni.p' */
/*prosignzfUCG5VRfUT39LG/mKF5+A*/