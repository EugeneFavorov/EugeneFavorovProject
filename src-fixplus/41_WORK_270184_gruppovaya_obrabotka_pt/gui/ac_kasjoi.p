{globals.i}
{intrface.get tmess}

/* +++ ac_kasjoi.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:27pm +++ */

/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2016 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ac_kasjoi.p
      Comment: Join ��� �������⥫��� ᮣ��襭�� � �������� ���.
   Parameters: 
         Uses:
      Used BY:
      Created: 02.06.2016 Sami 0270184
     Modified:      
*/

{joinpar.i}
{flt-file.i}
{svarloan.def NEW}

DEFINE VARIABLE vTitle AS CHARACTER NO-UNDO.
DEFINE BUFFER bloan FOR loan.
FIND loan WHERE ROWID (loan) EQ TO-ROWID (iRowId)
    NO-LOCK.

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

vTitle = "[ ���.ᮣ��襭�� " + STRING(loan.cont-code) + " ]". 

{procjoin.i
    &prefix = loan
    &frametitle = vTitle
}


/* --- ac_kasjoi.p was humbly modified by (c)blodd converter v.1.09 on 9/7/2016 12:27pm --- */
