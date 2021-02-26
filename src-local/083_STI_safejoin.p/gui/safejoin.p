{globals.i}
{intrface.get tmess}

/* +++ safejoin.p was humbly modified by (c)blodd converter v.1.11 on 8/24/2017 1:29pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2008 ��� "������᪨� ���ଠ樮��� ��⥬�"               
     Filename: safejoin.p
      Comment: ���� �易���� ������ ��� ��㧥� ������஢ �७�� ᥩ䮢�� �祥�.
   Parameters:
         Uses:
      Used by:
      Created: 27.03.2008 18:07 ALEXK   
     Modified: 27.03.2008 18:07 ALEXK    <comment>
     Modified: 31/10/2008 kraw (0094516) ���ꥬ � �᭮���� �����      
     Modified: 02/03/2009 kraw (0107147) �஢���� �⤥�쭮 �����ᮢ� � ��������ᮢ�
     Modified: 29/03/2010 kraw (0120487) ������� ������஢ �� �������� �������樨
*/

{joinpar.i}
{globals.i}             /* �������� ��६���� ��ᨨ. */
{intrface.get xclass}   /* ������⥪� �����㬥�⮢ ����奬�. */
{intrface.get db2l}     /* �����㬥�� ��� �������᪮� ࠡ��� � ��. */
{intrface.get trans}
/* ������� ⠡���� TmpObj */
{tmpobj.def}

DEFINE VARIABLE mCodLst  AS CHARACTER  NO-UNDO. /* ���᮪ �����                         */
DEFINE VARIABLE mValLst  AS CHARACTER  NO-UNDO. /* ���᮪ ���祭��                      */

DEFINE VARIABLE mContract AS CHARACTER    NO-UNDO.
DEFINE VARIABLE mContCode AS CHARACTER    NO-UNDO.

DEFINE BUFFER loan  FOR loan.
DEFINE BUFFER bloan FOR loan.

FIND loan WHERE ROWID(loan) = TO-ROWID(iROWID) NO-LOCK.

mContract =   loan.contract.
mContCode =   loan.cont-code.

/* ᮧ���� ����㭪� ���� ����ﭨ� ������� */
/*
RUN CreateJoin("����ﭨ� �������",
               "lpar-axd`" + 
               loan.contract + "," + 
               loan.cont-code + 
               ",1,1," +             /*����窠*/
               STRING(level + 1),
               YES
              ).
*/

RUN CreateJoinLd("����⥪� ��⮢",
                 "browseld",
                 "loan-acct",
                 "contract,cont-code",
                 loan.contract + "," + loan.cont-code,
                 "",
                 STRING(level + 1),                 
                 YES
               ).

RUN SetSysConf IN h_base ("SFContract", loan.contract).
RUN SetSysConf IN h_base ("SFContCode", loan.cont-code).
/*
RUN AddAttr2TableEx ("",0,-1,"",0,"_SFContract",loan.contract).
RUN AddAttr2TableEx ("",0,-1,"",0,"_SFContCode",loan.cont-code).
*/

IF iclass NE "loan-count" THEN
DO:

   ASSIGN
      mCodLst = "�������������KauClass" 

      mValLst = loan.cont-code + CHR(1) + loan.contract + CHR(1) + "������"
   .
   
   RUN CreateJoin("���㬥���"
                  ,
                  "s-br(op)`" + "," + mCodLst + "," + mValLst + ",," + STRING(level + 1)
                  ,
                  YES
                 ).

   RUN CreateJoin("�஢����",
                  "l-sf-jb`"
                  + iClass + ","
                  + iROWID
                  + ",1,"
                  + STRING(level + 1) + ",,", 
                  YES).

   RUN CreateJoinLd("�᫮��� �������",
                    "browseld", 
                    "rent-cond",
                    "contract,cont-code",
                    loan.contract + "," + loan.cont-code,
                    "",
                    STRING(Level + 1) ,
                    YES).
   
   RUN CreateJoin("����७�� ���",
                  "l_cust`" + loan.contract + "," + loan.cont-code + ", 1,"
                  + STRING(Level + 1),
                  YES).
END.
ELSE
DO:

   RUN CreateJoinLd("��㣨",
                    "browseld",
                    "term-obl-count",
                    "contract,cont-code",
                    loan.contract + "," + loan.cont-code,
                    "",
                    STRING(level + 1),                 
                    YES
                  ).

   FOR EACH xlink WHERE 
            xlink.class-code EQ 'loan-count' NO-LOCK,
       EACH links WHERE  
            links.link-id = xlink.link-id 
        AND links.source-id = loan.contract + "," + loan.cont-code
        AND links.beg-date LE end-date
        AND (links.end-date GE end-date OR links.end-date EQ ?)
            NO-LOCK,
       FIRST op WHERE op.op EQ INT(links.target-id)
            NO-LOCK:
   
      CREATE TmpObj.
      TmpObj.rid = RECID(op).
   
   END.

   mTmpObjHand = TEMP-TABLE TmpObj:HANDLE. 

   RUN CreateJoinLd("���㬥���",
                    "browseld",
                    "opb",
                    "UseTmpObjInQuery",
                    STRING(mTmpObjHand),
                    "",
                    STRING(Level + 1),
                    YES).
END.

RUN CreateJoinLd("���-䠪����",
                 "browseld",
                 "axd-sf",
                 /* ᯨ᮪ ���� �।��⠭���������� ����� */
                 "parent-contract"   + CHR(1) +
                 "parent-cont-code"  + CHR(1) +
                 "class-code"        + CHR(1) +
                 "contract"          + CHR(1) +
                 "TITLE"             + CHR(1) +
                 "SetFirstFrm"       + CHR(1) +
                 "mLinks-code"       + CHR(1) +
                 "mLinks-surr",

                 /* ᯨ᮪ �।��⠭���������� ���祭�� ����� */
                 loan.contract       + CHR(1) +
                 loan.cont-code      + CHR(1) +
                 "axd-sf"            + CHR(1) +
                 "sf-out"            + CHR(1) +
                 "������ ������������ ������-������" + "(~"" + loan.cont-code + "~")" + CHR(1) +
                 "2"                 + CHR(1) + 
                 "sf-loan-rent"      + CHR(1) +
                 loan.contract + ";" + loan.cont-code,

                 /* ᯨ᮪ ���� ������㥬�� ����� */
                 "parent-contract"   + CHR(1) +
                 "parent-cont-code"  + CHR(1) +
                 "class-code"        + CHR(1) +
                 "contract"          + CHR(1) + 
                 "mLinks-code"       + CHR(1) + 
                 "mLinks-surr",
                 STRING(Level + 1),
                 YES).

/* ᮧ���� ����㭪� ���� �������⥫�� ४������*/
RUN CreateJoin("�������⥫�� ४������",  
               "loansign`"
               + loan.contract + ","
               + loan.cont-code + ",1,1,"
               + STRING(Level + 1),
                YES).

/* ᮧ���� ����㭪� ���� �������⥫�� �裡*/
RUN CreateJoin("�������⥫�� �裡",
                "xlink-ed`"
                + loan.class-code + ","
                + loan.contract + ";"
                + loan.cont-code + ","
                + STRING(Level + 1),
                YES).
                
RUN CreateJoin("��ୠ� ���������",  "hi(loan`" +
               loan.contract + "," +
               loan.cont-code + "," +
               "1,1," +                 
               STRING(Level + 1) ,
               YES).                

IF iclass NE "loan-count" THEN
DO:

   FIND FIRST bloan WHERE bloan.parent-contract  = mContract
                      AND bloan.parent-cont-code = mContCode
                      AND bloan.class-code       = "loan-rent-dop" 
   NO-LOCK NO-ERROR.
   IF AVAIL(bloan) THEN
   RUN CreateJoinLd("�������⥫�� ᮣ��襭��",
                    "browseld", 
                    "rent-cond-dop",
                    "contract,cont-code",
                    bloan.contract + "," + bloan.cont-code,
                    "",
                    STRING(Level + 1) ,
                    YES).      
END.
                
RUN SetSysConf IN h_base ("safeContCode", loan.cont-code).

/*����� �ନ஢���� Join ����*/
{procjoin.i
   &Prefix = "loan"
   &frametitle = "'[ ������� ' + loan.doc-ref + ' ]'"  
}

RUN SetSysConf IN h_base ("safeContCode", "").

/* �������� ᮧ������ � sysconf ����ᥩ  */
RUN DeleteOldDataProtocol IN h_base ("SFContract").
RUN DeleteOldDataProtocol IN h_base ("SFContCode"). 

{intrface.del}

RETURN "NEXT REFRESH".
/* $LINTFILE='safejoin.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='soda' */
/* $LINTDATE='20/07/2017 16:38:27.833+03:00' */
/*prosign7tm2ZvlgcGa/Eju5Jswxdg*/
/* --- safejoin.p was humbly modified by (c)blodd converter v.1.11 on 8/24/2017 1:29pm --- */
