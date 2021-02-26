/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2005 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: INTR_CRN.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
*/

   {intrface.get xclass}
   {intrface.get date}
   {intrface.get dpspc}
   {globals.i}
   {sh-defs.i}
   {def_work.i NEW}
   {cr-nach.i}



   def input  param mStrNach   as char                 no-undo.

   def var mOp        like op.op              no-undo.
   def var mOp-entry  like op-entry.op-entry  no-undo.


   def var fl-nach as int64.

   def var sDateBl    as char no-undo.
   def var sBranch    as char no-undo.
   def var sOp        as char no-undo.
   def var sCont-code as char no-undo.
   def var sDateBeg   as char no-undo.
   def var sDateEnd   as char no-undo.
   def var sBaseNach  as char no-undo.
   def var sColDay    as char no-undo.
   def var sProcSt    as char no-undo.
   def var sSummaProc as char no-undo.
   def var sCodeComm  as char no-undo.
   def var sContType  as char no-undo.

   def var dDateBl    as date no-undo.
   def var dDateBeg   as date no-undo.
   def var dDateEnd   as date no-undo.


   if num-entries(mStrNach,"|") <> 10 then return.
   mStrNach = replace(mStrNach,"^",",").
   sOp         = entry(1,mStrNach,"|").
   mOp         = int64(entry(1,sOp)).
   mOp-entry   = int64(entry(2,sOp)).
   sCont-code  = entry(2,mStrNach,"|").
   sDateBeg    = entry(3,mStrNach,"|").
   sDateEnd    = entry(4,mStrNach,"|").
   sBaseNach   = entry(5,mStrNach,"|").
   sColDay     = entry(6,mStrNach,"|").
   sProcSt     = entry(7,mStrNach,"|").
   sSummaProc  = entry(8,mStrNach,"|").
   sCodeComm   = entry(9,mStrNach,"|").
   sContType   = entry(10,mStrNach,"|").

   dDateBeg = date(sDateBeg).
   dDateEnd = date(sDateEnd).

   find first op where op.op  = mOp no-lock no-error.
   if not avail op then do :
      message " ���㬥�� " + string(mOp) + " �� ������. "  view-as alert-box.
      return.
   end.

/*   sCont-code = "dps,1-57952/RUR@0000".
*/
   find first op-entry where op-entry.op = mOp 
                         and op-entry.op-entry = mOp-entry
                         no-lock no-error.
   if not avail op-entry then do :
      message " �஢���� " + string(mOp-entry) + " ��� ���㬥�� " + string(mOp) + " �� �������. " view-as alert-box.
      return.
   end.

   dDateBl  = op.contract-date.
   sBranch  = op.branch-id.
   
   RUN SetSysConf IN h_base("op-contract-date",string(op.contract-date)).
   RUN SetSysConf IN h_base("op-id", string(op-entry.op) + "," + string(op-entry.op-entry)).

   RUN CrDataNch ( 
                dDateBl,  /* ��� ᮧ����� ����� */
                sBranch, /* ���ࠧ������� */
                string(op-entry.op) + "," + string(op-entry.op-entry), /* op-entry.op,op-entry.op-entry */
                sCont-code ,    /* dps, cont-code */
                dDateBeg,                     /* ��砫� ���᫥��� */
                dDateEnd,                     /* ����� ���᫥���  */
                sBaseNach   + "," + /*���⮪ ������ VAL1*/
                sColDay     + "," + /*������⢮ ���� VAL2*/ 
                sProcSt     + "," + /*% �⠢��  VAL3 */
                sSummaProc  + "," + /*�㬬� %%  VAL4 */
                sCodeComm   + "," +     /* ��� �����ᨨ TXT1 */
                sContType ,        /* ⨯ ������� ���2 */
                OUTPUT  fl-nach,
                BUFFER Dataline).

   IF fl-nach = -1  THEN DO:
      MESSAGE "���ଠ�� � %% ��࠭��� ����������" VIEW-AS ALERT-BOX.
   END.





