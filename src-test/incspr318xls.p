{globals.i}
{sh-defs.i}
{chkacces.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEF INPUT PARAM RID AS RecID NO-UNDO.

DEF TEMP-TABLE ttTemp NO-UNDO
	FIELD vNumM   AS CHAR /*����� �������*/
	FIELD vNumS   AS CHAR /*����� �㬪�*/
	FIELD vSum    AS DEC  /*�㬬� �஢����*/
	FIELD vSumI   AS DEC  /*����誨*/
	FIELD vSumN   AS DEC /*�������*/
	FIELD vDateM  AS DATE. 
	
DEF VAR vTmpNum1 AS CHAR.
DEF VAR vTmpNum2 AS CHAR.
DEF VAR vOpDate1 AS DATE. 
DEF VAR vTSumN   AS CHAR.
DEF VAR vTSumI   AS CHAR.

DEF VAR sum-tot  AS DEC.
DEF VAR sumi-tot AS DEC.
DEF VAR sumn-tot AS DEC.
DEF VAR sump-tot AS DEC.

DEF VAR c-cel    AS CHAR.
DEF VAR c-ost    AS CHAR.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"

/*FIND FIRST tmprecid NO-LOCK NO-ERROR.
FIND FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK NO-ERROR.
	IF AVAIL(op) THEN vOpDate1 = op.op-date.*/
	
{getdate.i}

FOR EACH op-entry
 WHERE op-entry.filial-id EQ shFilial
  AND  op-entry.acct-db begins '20202810505001000000'
  AND  op-entry.op-date GE end-date - 7
  AND  op-entry.op-date LE end-date + 14 /*�ࠧ�����*/
   NO-LOCK,
FIRST op OF op-entry WHERE op.op-kind BEGINS '0303i' 
					 AND   op.op-date EQ end-date  
					 AND   op.op-status NE '�' NO-LOCK:
	vTmpNum1 = GetXattrValue('op',string(op.op),'������������').
	vTmpNum2 = GetXattrValue('op',string(op.op),'�����_�㬪�').
	FIND FIRST ttTemp 
		WHERE ttTemp.vNumM  EQ vTmpNum1
		AND   ttTemp.vNumS  EQ vTmpNum2 NO-ERROR.
	IF NOT AVAIL(ttTemp) THEN DO:
		vTSumN = GetXattrValue('op',string(op.op),'�㬬��������').
		vTSumI = GetXattrValue('op',string(op.op),'�㬬������').
		CREATE ttTemp.
		ASSIGN 
			ttTemp.vNumM  = vTmpNum1
			ttTemp.VNumS  = vTmpNum2
			ttTemp.vSum   = op-entry.amt-rub
			ttTemp.vSumN  = (IF vTSumN NE '' THEN DEC(vTSumN) ELSE 0)
			ttTemp.vSumI  = (IF vTSumN NE '' THEN DEC(vTSumI) ELSE 0)
			ttTemp.vDateM = op.doc-date
		.
	END.
	ELSE ttTemp.vSum = ttTemp.vSum + op-entry.amt-rub.
END.

RUN Insert_TTName("date", string(day(end-date),"99") + " "
							 + entry(month(end-date),{&Months}) + " " 
							 + string(year(end-date)) + " �.").

FOR EACH ttTemp 
  BREAK BY ttTemp.vNumS: 
  
  ACCUMULATE ttTemp.vSum (total) 
             ttTemp.vSumI (total) 
			 ttTemp.vSumN (total)
             ttTemp.vNumS (count).			 
  
  IF LAST(ttTemp.vNumS) THEN DO:
	
	sum-tot  = ACCUM total ttTemp.vSum.
	sumi-tot = ACCUM total ttTemp.vSumI.
	sumn-tot = ACCUM total ttTemp.vSumN.
	sump-tot = sum-tot - sumi-tot + sumn-tot.

	RUN amtgend.p (ACCUM count ttTemp.vNumS, '�', OUTPUT c-cel, OUTPUT c-ost).
	RUN Insert_TTName ('numsump', STRING(ACCUM count ttTemp.vNumS) + " (" +  c-cel + ")").
	RUN Insert_TTName ('numsum', STRING(ACCUM count ttTemp.vNumS)).
	
	RUN amtstr2.p (sump-tot, "", OUTPUT c-cel, OUTPUT c-ost).
	RUN Insert_TTName ('summonp', "(" + c-cel + " " + c-ost + " ���.)").
	RUN Insert_TTName ('summon', STRING(sump-tot,">>>,>>>,>>>,>>9.99")).
	
	RUN amtstr2.p (sum-tot, "", OUTPUT c-cel, OUTPUT c-ost).
	RUN Insert_TTName ('sumperp', "(" + c-cel + " " + c-ost + " ���.)").
	RUN Insert_TTName ('sumper', STRING(sum-tot,">>>,>>>,>>>,>>9.99")).
	
	RUN Insert_TTName ('sumizl', STRING(sumi-tot,">>>,>>>,>>>,>>9.99")).
	
	RUN Insert_TTName ('sumned', STRING(sumn-tot,">>>,>>>,>>>,>>9.99")).
	
  END.
  
END.

RUN printvd.p ('incspr318', INPUT TABLE ttnames).  