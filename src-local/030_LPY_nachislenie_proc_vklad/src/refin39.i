{com_ref.i {&*}}

procedure Get_Proc_Ref :
 def input  param b-date as date no-undo.
 def input  param e-date as date no-undo .
 def output param xresult1 as decimal init 0 no-undo .
 def var fl-print as logical init no no-undo .
 DEF VAR vCBRefComm AS CHARACTER NO-UNDO.
 DEF VAR gCBRefPred AS CHARACTER NO-UNDO.
 /* &scop dat-commi dat-commi
 */

 vCBRefComm = gCBRefPred.
 { {1}
     &d-beg       = b-date
     &d-end       = e-date
     &dir         = last
     &comm-rate   = comm-rate
     &sum         = fost
     &since       = since
     &rcom        = vCBRefComm
     &proc        = xresult1
     &balance     = balance
     &since1      = " lt b-date "
     &dat-comm    = b-date
     &dat-commi   = b-date   
   }
end procedure .

if beg eq (beg-date1 - 1) then
  run Get_Proc_Ref(beg-date1,end-date1,output xresult-ref) .
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='10/06/2015 21:44:34.841+04:00' */
/* $LINTUSER='var' */
/* $LINTMODE='1' */
/* $LINTFILE='refin39.i' */
/*prosignFsoLmCMZui4L7AXz1F46GQ*/