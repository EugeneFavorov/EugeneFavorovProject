/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: mcmratbrw.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 04.03.2010 11:35 Chumv   
     Modified: 04.03.2010 11:35 Chumv   
*/

{globals.i}
{flt-file.i}

{intrface.get xclass}
{intrface.get tmess}

DEF VAR mKauModif AS CHAR NO-UNDO.
DEF VAR mZnak     AS CHAR NO-UNDO.

DEFINE BUFFER bcomm-rate FOR comm-rate.

   /* ������塞 ���祭�� 䨫��� */
mKauModif = GetFltVal("kau").
RUN SetFltField("kau","����䨪���," + mKauModif).

{mcmratbrw.frm}
{mcmratbrw.qry}

{navigate.cqr
   &file       = "comm-rate"
   &avfile     = "comm-rate"
   &files      = "comm-rate"
   &filt       = YES

   &bf1        = "comm-rate.commission mZnak comm-rate.rate-comm comm-rate.since"

   &help-label = "'F9�Ins�Del' "

   &postfind   = "mcmratbrw.fnd "

   &edit       = "edit-ef.cqr "
      &create  = "mcmratbrw.cr "
      &eh      = "mcmratbrw.eh "
      &ef      = "mcmratbrw.ef "
      &lookup  = "mcmratbrw.nau "
      &update  = "mcmratbrw.upd "
}

{intrface.del}

