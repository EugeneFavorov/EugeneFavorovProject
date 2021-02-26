/* 

kam

����� ������� �� ������ �ப��᪨� ��� ������ �ନ஢����� ᫥���騬 ��ࠧ��:

��  + ����� ��ᯮ�� ���客�⥫� �᫮��� ���� 6 ��� + / + ��᫥���� 5 ��� VIN �����客������ �� + �� _________ 

�ਬ��  �� �540087/00842 �� 20.10.2017

*/

{globals.i}
{intrface.get xclass}

DEF INPUT PARAMETER iDocument    AS CHARACTER NO-UNDO.
DEF INPUT PARAMETER iContCode    AS CHARACTER NO-UNDO.

DEF SHARED VARIABLE pick-value   AS CHARACTER NO-UNDO.
DEF VAR vin AS CHAR NO-UNDO.
DEF VAR mTermSurr AS CHAR NO-UNDO.
DEF VAR mVidObNotar AS CHAR NO-UNDO.


iDocument = TRIM(REPLACE(REPLACE(idocument,' ',''),' ','')).
/*
IF LENGTH(iDocument) > 6 THEN iDocument = SUBSTRING(iDocument,1,6).
*/
IF LENGTH(iDocument) > 6 THEN iDocument = SUBSTRING(iDocument,(LENGTH(iDocument) - 5)).


vin = ''.
FOR EACH term-obl
                        WHERE term-obl.contract EQ '�।��'
                        AND term-obl.cont-code EQ iContCode
                        AND term-obl.idnt EQ 5
                        NO-LOCK BY term-obl.fop-date DESC:

                        mTermSurr = term-obl.contract + "," + term-obl.cont-code + ",5,"  + STRING(term-obl.end-date, "99/99/99") + "," + STRING(term-obl.nn).  
                        mVidObNotar = GetXAttrValueEx("term-obl", 
                                                mTermSurr, 
                                                "�����",
                                                "0").
                        IF mVidObNotar EQ "��⮬�����"
                        THEN
                        DO:
                            vin = TRIM(GetXAttrValueEx ("term-obl", mTermSurr, "TCVIN", "")).
                        END.
                        IF vin <> '' THEN LEAVE.                        
END.

/*
MESSAGE iDocument VIEW-AS ALERT-BOX.
MESSAGE vin VIEW-AS ALERT-BOX.
*/

IF LENGTH(vin) > 5 THEN vin = SUBSTRING(vin,(LENGTH(vin) - 4)).

pick-value = iDocument + '/' + vin.

{intrface.del}

RETURN pick-value.