{globals.i}             /** �������� ��।������ */
{tmprecid.def}          /** �ᯮ��㥬 ���ଠ�� �� ��㧥� */
{intrface.get xclass}
{pb_logit.i}

DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.

FOR EACH tmprecid 
    NO-LOCK,
FIRST loan
    WHERE (RECID(loan) EQ tmprecid.id)
    NO-LOCK:

    RUN LogIt("������� " + loan.cont-code, "./cre.log").
    cTmp = GetXAttrValue("loan", loan.contract + "," + loan.cont-code, "CREsince").
    cTmp = STRING(DATE(cTmp) - 1).
    RUN LogIt("  CREsince = " + cTmp + " - "
            + STRING(UpdateSigns("loan", loan.contract + "," + loan.cont-code, "CREsince", cTmp, YES)) , "./cre.log").
END.

{intrface.del}
