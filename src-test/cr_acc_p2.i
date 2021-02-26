    {cordt(op.i}
    {intrface.get kau}
    &glob bef-undo next FE$.
    &glob undo-no-proc undo loan_code, leave loan_code .
   DEFINE VARIABLE CR_ACC_P-vSinceDate AS DATE NO-UNDO. /*Дата для привязки нового счета*/
   DEFINE VAR str-par AS CHAR NO-UNDO .
   DEFINE VARIABLE vTokacct AS CHARACTER INIT "" NO-UNDO. /* "развернутая" маска 
                                                           счета */
   DEFINE VAR vKau     AS CHARACTER   NO-UNDO.
   DEFINE VAR vICntr   AS CHARACTER   NO-UNDO.
   DEF VAR vAttachAcct         AS CHARACTER  NO-UNDO.
   DEF VAR vIK         AS INT64       NO-UNDO.
   DEF BUFFER b-loan-acct FOR loan-acct.
   DEF BUFFER c-loan-acct FOR loan-acct.

   DEF BUFFER bAcct4Kau FOR acct.

   RUN SetH_tmp_acct IN h_dpspc (TEMP-TABLE tmp-acct:HANDLE).

   RUN CorrectDateForDPS(INPUT  in-op-date,
                         cred,
                         OUTPUT CR_ACC_P-vSinceDate).

   run get_last_param  in h_dpspc (recid(loan),cred,cred,'ИзменДатыПриПрол', output str-par) .
   IF str-par = 'Да' AND FGetSetting("Учет%%План", ?, "") EQ "Да" THEN
  /* cr_acc_p-vsincedate = cr_acc_p-vsincedate - 1. */
   FE$:
   FOR EACH op-template OF op-kind  where
      CAN-DO(templ-acct,STRING(op-template.op-template))
      on error undo loan_code , leave loan_code on endkey undo loan_code, leave loan_code
      :

    release cr-acct .
     {bef-tran.i}
      ASSIGN
         in-acct-type = Get_Param('acct-type',RECID(op-template))
      .
      /* только для счетов с ролью loan-dps-t  ищем последний счет и определяем бал.счет 2-го порядка */
      def var tmp-bal2 like acct.bal-acct no-undo.
      tmp-bal2 = 0.
      if in-acct-type begins "loan-dps-t" then do:
        FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
                              AND loan-acct.cont-code EQ loan.cont-code
                              AND loan-acct.acct-type EQ in-acct-type  NO-LOCK NO-ERROR.
        IF AVAIL loan-acct THEN find acct of loan-acct NO-LOCK NO-ERROR.
        IF AVAIL acct THEN tmp-bal2 = acct.bal-acct.
      end.
      /* ------------------------------------------------------------------------------------------- */
      
      
      FIND LAST loan-acct WHERE loan-acct.contract  EQ loan.contract
                            AND loan-acct.cont-code EQ loan.cont-code
                            AND loan-acct.acct-type EQ in-acct-type

                            AND (if in-acct-type eq 'loan-dps-p' then loan-acct.since le CR_ACC_P-vSinceDate
                             else loan-acct.since  eq CR_ACC_P-vSinceDate)
                                                     NO-LOCK NO-ERROR.
      
      IF NOT AVAIL loan-acct THEN DO :

         &IF DEFINED(Publish_CDate) NE 0 &THEN                  
         RUN SetSysConf IN h_base ("CDate4dpsb2p", STRING({&Publish_CDate} - 1)). 
         &ENDIF

         DO ON ENDKEY undo, leave: /* перехватить end-error и выгрузить библиотеку из памяти */
             /* MDY - 0036909 
             run "Cr_acct" in  h_dpspc (BUFFER cr-acct,
             */                                           
             run "Cm_add_loanacc" in  h_dpspc (BUFFER cr-acct,
                                              BUFFER loan,
                                              BUFFER op-template,
                                              ?,
                                              in-op-date,
                                              loan.currency,
                                              output mess,
                                              OUTPUT vTokacct 
             ) no-error. /* no-error обязательно! */ 
         END.
         IF AVAILABLE cr-acct THEN
              RUN parssign.p (in-op-date,
                             "op-template",
                             op-template.op-kind + "," + string(op-template.op-templ),
                             op-template.class-code,
                             "acct",
                             cr-acct.acct + "," + cr-acct.currency,
                             cr-acct.class-code,
                             ?).
                       
         if error-status:error or last-event:function = "end-error" then do:         
            undo loan_code , leave loan_code .
         end.
         ret-str = return-value .
         if ret-str eq '-1' then do :
          undo loan_code, leave loan_code.
         end.               
         /* удалить созданный счет, если роль счета = "loan-dps-t" и бал.счет 2 порядка совпадает с предыдущим в loan-acct*/
         if in-acct-type begins "loan-dps-t" /*and (new cr-acct)*/ and cr-acct.bal-acct = tmp-bal2 
         then do:
           if DebugParser > 0 then 
             message "Новый счет открывается на том-же балансовом счете." skip
                     "Операция отменяется."
             view-as alert-box.
             
           undo FE$, next FE$.             
         end.

         RUN CheckAcctMulti IN h_kau (cr-acct.acct,
                                      cr-acct.currency,
                                      in-acct-type,
                                      OUTPUT vKau).
         IF {assigned vKau} THEN
         DO:
            FIND FIRST bAcct4Kau WHERE RECID(bAcct4Kau) EQ RECID(cr-acct) 
               EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL bAcct4Kau THEN 
            DO vIK = 1 TO NUM-ENTRIES(vKau):
               bAcct4Kau.kau-id = ENTRY(vIK, vKau).             
               VALIDATE bAcct4Kau.
            END.
         END.     

         IF NEW cr-acct THEN
            cr-acct.cust-id = IF cr-acct.cust-cat NE 'В' THEN loan.cust-id 
                                                         ELSE cr-acct.cust-id.

       &if defined(Dispacct) &then
        if in-acct-type eq 'loan-dps-p' then
         disp cr-acct.acct @ in-dop-acct with frame opreq .
       &endif
       CREATE sroch-loan-acct .
       ASSIGN
            sroch-loan-acct.cont-code = loan.cont-code
            sroch-loan-acct.contract  = loan.contract
            sroch-loan-acct.acct      = cr-acct.acct
            sroch-loan-acct.currency  = cr-acct.currency
            sroch-loan-acct.acct-type = in-acct-type
            sroch-loan-acct.since     = cr_acc_p-vsincedate
            .
       RELEASE sroch-loan-acct.
       FIND FIRST sroch-loan-acct WHERE sroch-loan-acct.contract  = loan.contract
                                    AND sroch-loan-acct.cont-code = loan.cont-code
                                    AND sroch-loan-acct.acct-type = in-acct-type
                                    AND sroch-loan-acct.since     = cr_acc_p-vsincedate
          NO-LOCK NO-ERROR.
         IF NEW cr-acct THEN
         tmp-recid = STRING(RECID(cr-acct)) + ',' + STRING(RECID(sroch-loan-acct)).

         vAttachAcct = GetXAttrValueEx ("op-template",
                               op-template.op-kind + "," + STRING (op-template.op-template),
                               "ПривязкаОстРоль",
                               "Нет").
         IF vAttachAcct EQ "Да" THEN
         DO:
            FIND LAST  c-loan-acct OF loan WHERE c-loan-acct.acct-type EQ in-acct-type /*новый счет*/
                                             AND c-loan-acct.since EQ cr_acc_p-vsincedate 
               NO-LOCK NO-ERROR. 
            FIND LAST b-loan-acct OF loan  WHERE b-loan-acct.acct-type EQ in-acct-type  /*тот, что был раньше*/ 
                                            AND b-loan-acct.since     LT cr_acc_p-vsincedate 
               NO-LOCK NO-ERROR. 
           IF AVAIL c-loan-acct AND NOT AVAIL b-loan-acct AND c-loan-acct.acct-type EQ  'loan-dps-p'
              THEN
           FIND LAST b-loan-acct OF loan  WHERE b-loan-acct.acct-type BEGINS 'loan-dps-t'  /*тот, что был до штрафной пролонгации*/
                                            AND b-loan-acct.since     LT cr_acc_p-vsincedate
              NO-LOCK NO-ERROR. 

            IF     AVAIL (b-loan-acct)
               AND AVAIL (c-loan-acct) THEN
               RUN crloanacct.p (RECID (loan),
                                 RECID (b-loan-acct), 
                                 RECID (c-loan-acct), 
                                 b-loan-acct.acct-type, 
                                 cr_acc_p-vsincedate).
         END.                     
                                 
         /*RUN parssign.p (in-op-date,
                         "op-template",
                         op-template.op-kind + "," + string(op-template.op-templ),
                         op-template.class-code,
                         "acct",
                         cr-acct.acct + "," + cr-acct.currency,
                         cr-acct.class-code,
                         ?).
         if valid-handle(h_dpsb2p) then delete procedure h_dpsb2p.
         if error-status:error or last-event:function = "end-error" then do:         
            undo loan_code , leave loan_code .
         end.
         ret-str = return-value .
         if ret-str eq '-1' then do :
          undo loan_code, leave loan_code.
         end. */                                     
      END.
   end.
/* $LINTENV ='1ut' */
/* $LINTVSS ='$/ws2-tst/bq/' */
/* $LINTDATE='25/02/2016 20:17:07.174+04:00' */
/* $LINTUSER='kozv' */
/* $LINTMODE='1' */
/* $LINTFILE='cr_acc_p.i' */
/*prosignQy2fgMaaAZ9sSlGmtYHhHA*/