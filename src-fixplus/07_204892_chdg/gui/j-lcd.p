{globals.i}
{intrface.get tmess}

/* +++ j-lcd.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2014 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: j-lcd.p
      Comment: ��⮤ join  �᫮��� ������஢
   Parameters: ���
         Uses: l-con(l).p
      Used by:
      Created: ����
     Modified:
     Modified: 19/05/2005 mitr ��� 0046810 : �訡�� �맮�� ��⮤� "JOIN"
     Modified: 25/12/2005 mitr 0056115: �訡�� �맮�� ��⮤� "JOIN"
     Modified: 11/04/2006 GORM (58474) ��ॢ���� �� ��������.
     Modified: 19/05/2009 Jadv (0077883)
*/
{joinpar.i}             /* ��ࠬ���� �맮�� ��� ��⮤� join */

DEF VAR mIskl      AS LOG  NO-UNDO.
DEF VAR mSurr      AS CHAR NO-UNDO.

   /* �஢��窨 �� ����稥 ⥪�饣� �᫮��� � ������� */
FIND FIRST loan-cond WHERE
     ROWID(loan-cond) EQ TO-ROWID(iRowId)
NO-LOCK NO-ERROR.
IF NOT AVAIL loan-cond THEN
   RETURN "-1".
FIND FIRST loan WHERE
           loan.contract  EQ loan-cond.contract
   AND     loan.cont-code EQ loan-cond.cont-code
NO-LOCK NO-ERROR.
IF NOT AVAIL loan THEN
   RETURN "-1".

ASSIGN
   mSurr = loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(loan-cond.since)
   mIskl = GetXattrValueEx("loan-cond", mSurr, "�᪫���", "NO") EQ "YES"
.

RUN CreateJoinLd ("������� ���⮪",
                  "browseld",
                  "term-obl-sum",
                  "contract"         + CHR(1) + "cont-code",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code,
                  "",
                  level + 1,
                  YES).

IF CAN-DO("loan_deb_scf,loan_agr_scf,loan_agr_factor", loan.Class-Code) THEN
   RUN CreateJoin   ("��業�� �⠢��",
                     "lrat-fct`" +
                     loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                     YES).
ELSE
   RUN CreateJoin   ("��業�� �⠢��",
                     "l-rat(c)`" +
                     loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                     YES).

RUN CreateJoinLd ("�������� ����襭�� ����",
                  "browseld",
                  "term-obl-debt",
                  "contract"         + CHR(1) + "cont-code"         + CHR(1) + "since",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code + CHR(1) + STRING(loan-cond.since),
                  "",
                  level + 1,
                  YES).

RUN CreateJoinLd ("������� ���⥦� %%",
                  "browseld",
                  "term-obl-per",
                  "contract"         + CHR(1) + "cont-code"         + CHR(1) + "since",
                  loan-cond.contract + CHR(1) + loan-cond.cont-code + CHR(1) + STRING(loan-cond.since),
                  "",
                  level + 1,
                  YES).

 RUN CreateJoin  ("������� ���⥦�",
                  "ps_brw`" +
                  loan-cond.contract + "," +
                  loan-cond.cont-code  + "," +
                  STRING(loan-cond.since) + ","  + "0" + "," +
                  STRING(level + 1),
                  YES).

RUN CreateJoinLd("��䨪 �����ᨩ",
                 "browseld",
                 "term-obl-comm",
                 "contract"  + CHR(1) + "cont-code",
                 loan-cond.contract + CHR(1) + loan-cond.cont-code,
                 "",
                 level + 1,
                 YES).

RUN CreateJoin   ("�᪫�祭�� �� ��䨪� ����襭��",
                  "l-trmexc`" +
                  loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                  mIskl).


RUN CreateJoinLd("��� �� ����筮� ����襭��", 
                 "browseld",
                 "presched",
                 "contract"  + CHR(1) + "cont-code",
                 loan-cond.contract + CHR(1) + loan-cond.cont-code,
                 "",
                 STRING(level + 1), 
                 YES).

RUN CreateJoin   ("�������⥫�� ४������",
                  "l_con(s)`" +
                  loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                  YES).

RUN CreateJoin("����� ��䨪��",
               "histbrow`" +
               loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
               YES).

   /* ������� �㭪� ����, �᫨ �� ������ loan ���� �� domain-code = "term-obl" */
FIND FIRST xattr WHERE
           xattr.class-code  EQ loan.class-code
   AND     xattr.domain-code EQ "term-obl"
NO-LOCK NO-ERROR.
IF AVAIL xattr THEN
   RUN CreateJoin ("��稥",
                   "l-termch`" +
                   loan-cond.contract + "," + loan-cond.cont-code  + "," + STRING(loan-cond.since) + "," + STRING(level + 1),
                   YES).

IF     FGetSetting("����������","","���") EQ "��"
   AND loan.contract                      EQ "�।��"
   AND LOGICAL (GetXattrInit(loan-cond.Class-Code,"�奬�����"),
                "�����⭠�/����७�஢�����") EQ NO THEN
   RUN CreateJoin("���. ��䨪� ����.��襭�� ����",
                  "to-ttdebt-brw`" +
                  loan-cond.contract + "," + loan-cond.cont-code + "," + STRING(level + 1),
                  YES).

   /* ����� �ନ஢���� Join ���� */
{procjoin.i
    &Prefix     = "loan-cond"
    &frametitle = "'[ ���. ����. ]'"
    &parms      = "(loan-cond.contract,loan-cond.cont-code,loan-cond.since,level + 1)"
}

RETURN "0".
/* $LINTUSER='BIS' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTDATE='05/12/2014 16:45:48.634+04:00' */
/* $LINTFILE='j-lcd.p' */
/*prosignbavq6dOgMjQFyVv+QQaicQ*/
/* --- j-lcd.p was humbly modified by (c)blodd converter v.1.09 on 7/15/2015 6:32am --- */
