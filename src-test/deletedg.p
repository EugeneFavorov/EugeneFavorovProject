{globals.i} 
{tmprecid.def}

{intrface.get xclass}   /* Инструменты для работы с метасхемой.  */


FOR EACH tmprecid NO-LOCK,
	FIRST loan WHERE RECID(loan) = tmprecid.id NO-LOCK:

    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_Date_dosr_g", "", ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_Date_pod_zay", "", ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_summ_dosr_g", "", ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_type_dosr_g", "", ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_type_dosr_g2", "", ?) NO-ERROR.
    UpdateSigns(loan.class-code, loan.contract + ',' + loan.cont-code, "dg_perechisl", "", ?) NO-ERROR.

end.