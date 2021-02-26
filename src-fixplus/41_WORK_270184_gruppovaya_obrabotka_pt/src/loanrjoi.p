/*
               KSV Editor
    Copyright: (C) 2000-2005 Serguey Klimoff (bulklodd)
     Filename: LOANRJOI.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 22.05.2006 17:10 VASOV   
     Modified: 29.05.2006 11:18 VASOV    <comment>
*/

{joinpar.i}
{flt-file.i}
{svarloan.def NEW}

DEFINE VARIABLE vTitle AS CHARACTER NO-UNDO.
DEFINE BUFFER bloan FOR loan.
FIND loan WHERE ROWID (loan) EQ TO-ROWID (iRowId)
    NO-LOCK.

RUN CreateJoinLd ("�᫮��� �������", 
                  "browseld",
                  "loanr-cond",
                  "contract"    + CHR(1) + "cont-code",
                  loan.contract + CHR(1) + loan.cont-code,
                  "",
                  STRING (Level + 1),
                  YES).
RUN CreateJoin ("����� ���⪮�", 
                "term1`" + STRING (loan.contract) + "," + 
                           STRING (loan.cont-code) + "," + 
                           GetFltVal ("open-date2") + "," +
                           STRING (Level + 1),
                YES).
RUN CreateJoin ("���ᯫ��", 
                "term2`" + STRING (loan.contract) + "," + 
                           STRING (loan.cont-code) + "," + 
                           GetFltVal ("open-date2") + "," +
                           STRING (Level + 1),
                YES).
RUN CreateJoinLd ("���ᮢ� ���", 
                  "browseld",
                  "loancsreq",
                  "contract~001Parent-Contract~001Parent-Cont-Code",
                  "���ᇠ�~001" + loan.contract + "~001" + loan.cont-code,
                  "contract",
                  STRING (Level + 1),
                  YES).
FIND LAST loan-acct OF loan WHERE
    loan-acct.acct-type =  loan.contract AND
    loan-acct.since     <= DATE(GetFltVal("open-date2"))
NO-LOCK NO-ERROR.
IF AVAILABLE loan-acct THEN
    RUN CreateJoinLd("��� ���ନ஢����",
                     "browseld",
                     "loan-trans-sms",
                     "contract" + CHR(1) + "cont-code",
                     loan.contract + CHR(1) + loan.cont-code,
                     "contract" + CHR(1) + "cont-code",
                     STRING(level + 1),
                     YES).
RUN CreateJoin ("�������⥫�� ४������",
                "loansig_`" + STRING (loan.contract) + "," + 
                              STRING (loan.cont-code) + "," + 
                              GetFltVal ("open-date2") + "," +
                              STRING (Level + 1),
                YES).
RUN CreateJoin ("��ୠ� ���������",
                "hi(loan1`" + STRING (loan.contract) + "," + 
                              STRING (loan.cont-code) + "," + 
                              GetFltVal ("open-date2") + "," +
                              STRING (Level + 1),
                YES).
RUN CreateJoin ("����ﭨ� �������",
                "l-par(l)`" + STRING (loan.contract) + "," + 
                              STRING (loan.cont-code) + ",1,1," + 
                              STRING (Level + 1),
                YES).
RUN CreateJoin ("����⥪� ��⮢",
                "dacc`" + STRING (loan.contract) + "," + 
                          STRING (loan.cont-code) + ",0,0," + 
                          STRING (Level + 1),
                YES).

RUN CreateJoinLd ("��易��� ��ꥪ��", 
                  "browseld",
                  "loanr" + chr(1) +  "loanr-cust",
                  "in-class" + chr(1) + "in-surr" + CHR(1) + "in-xattr",
                  "loanr" + chr(1) + loan.contract + ";" + loan.cont-code + CHR(1) + "loanr-cust",
                  "",
                  STRING (Level + 1),
                  YES).

RUN CreateJoin ("���������� �����ᨨ",
                "dscbrw`" + STRING (loan.contract) + "," + 
                            STRING (loan.cont-code) + ",?,?," + 
                            STRING (Level + 1),
                YES).
IF CAN-FIND(FIRST bloan WHERE bloan.class-code EQ "proxy-base" AND bloan.cust-cat EQ loan.cust-cat AND bloan.cust-id EQ loan.cust-id) THEN DO:
   RUN CreateJoin("����७���� ���",
                     "proxy_acct`" + loan.cust-cat + "," + STRING(loan.cust-id) + ",RKO," + loan.cont-code + "," +  STRING(level + 1),
                     YES).

END.

RUN CreateJoinLd ("���.ᮣ��襭�� � ��楯�", 
                  "browseld",
                  "loanr_ac",
                  "contract~001Parent-Contract~001Parent-Cont-Code",
                  "���ᑮ����~001" + loan.contract + "~001" + loan.cont-code,
                  "contract",
                  STRING (Level + 1),
                  YES).

                        /* ������祭�� �⠭������ �࠭���権. */   
{jobjopkind.i
   &ClassCode = "loan.class-code"
   &Surrogate = "loan.contract + "";"" + loan.cont-code"
}

vTitle = "[ ������� " + STRING(loan.cont-code) + " ]". 

{procjoin.i
    &prefix = loan
    &frametitle = vTitle
}

