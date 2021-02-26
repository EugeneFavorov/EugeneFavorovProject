/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-1996 ТОО "Банковские информационные системы"
     Filename: G-COMP1.P
      Comment: Процедура ввода сложных операций
   Parameters: recid(op-kind)
         Uses: -
      Used by: opgennav.p
      Created: 26/11/1997 Peter (from g-op1.p)
     Modified: 15/09/99 15:51 Sema (обработка предыдущих операций, заполнение полей из них)
     Modified: 19/10/00 Om Корректировка работы процедуры
                           (убрана прогрессовая ругань).
     Modified: 11.03.2002 17:54 SEMA     по заявке 0006077 Введена возможность формировать доп.реквизиты документа с
                                         помощью парсера
     Modified: 13.03.2002 14:37 SEMA     по заявке 0005574 подъем фикса по сейфингу
     Modified: 26.04.2002 16:40 SEMA     по заявке 0006077 исправление ошибки
     Modified: 03.05.2002 14:22 KSV      (0006452) Добавлен макрос NoOutput, запрещающий вывод любой
                                         информации на экран.
                                         Добавлены макросы: 
                                         NoDispEntries - не выводятся проводки,
                                         NoDispOP - не выводится документ
                                         NoPrint - не выводится запрос на печать
                                         Переставлен инклюдник g-trbe.i для реализации возможности
                                         использования парсерных функций в номерах счетов.
     Modified: 16.12.2002 18:11 SEMA     по заявке 0012711 сохранение нескольких значений из фрейма для использования в
                                         парсерных функциях
     Modified: 18.12.2002 17:48 SEMA     по заявке 0012531 изменен вызов инструмента parssign
     Modified: 10.12.2007 17:25 KSV      (0085509) Добавлен DISPLAY перед SET
                                         opreq, иначе значения формы не
                                         сохраняются
     Modified: 20/07/2009 kraw (0070076) Вызов parssign.p для каждой проводки документа
*/

&IF DEFINED(NoOutput) &THEN
  &GLOBAL-DEFINE NoDispEntries  YES
  &GLOBAL-DEFINE NoDispOp       YES
  &GLOBAL-DEFINE NoPrint        YES
  &GLOBAL-DEFINE no-frame-disp  YES
  &GLOBAL-DEFINE nodspwop       YES
&ENDIF

&IF DEFINED(no-frame-disp) = 0 
&THEN
   &GLOBAL-DEFINE noParsIfAssBacct  YES
&ENDIF

&GLOBAL-DEFINE RunParsSigns YES
&GLOBAL-DEFINE empty-acct-check YES

def input param in-op-date like op.op-date.
def input param oprid as recid.
{g-defs.i}
{wordwrap.def}

{chkblock.i
  &surr=string(in-op-date)
  &msg="Вы не имеете права работать в заблокированном операционном дне!"
  &action="return."
}

DEF VAR frate1 AS DEC INIT 0 NO-UNDO.
DEF VAR frate2 AS DEC INIT 0 NO-UNDO.

def var i as INT64 no-undo.
def var nn as INT64 no-undo.
def var tstr as char no-undo.
def var ser as char no-undo.
def var deb as char extent 2 form "x(70)" no-undo.

def var flreg as INT64 no-undo.
def var cl-cat like acct.cust-cat no-undo.
def var cl-id like acct.cust-id no-undo.
def var tcur-db like op-templ.currency no-undo.
def var tcur-cr like op-templ.currency no-undo.
def var fler as logical no-undo.
def var frep as logical no-undo.
def var difround as logical no-undo.
def var main-first as logical no-undo.
def var vcontr like op-templ.acct-db.
def var ssumra as decimal decimals 10 form ">>9.9999999999".
def var ssumsa as decimal decimals 10 form ">>9.9999999999".
def var ssumras as decimal decimals 10 form ">>9.9999999999" no-undo.
def var diference as decimal form ">>9.9999999999".
def var delta as decimal decimals 10 form ">>9.9999999999".
def var deltaprev as decimal decimals 10 form ">>9.9999999999".
def var mdelta as decimal decimals 10 form ">>9.9999999999".
def var mdeltaprev as decimal decimals 10 form ">>9.9999999999".
def var noe like op-entry.op-entry no-undo.
def var dval like op-entry.value-date no-undo.
def var brate like instr-rate.rate-instr no-undo.
def var vcomm like comm-rate.rate-comm no-undo.
def var koef as dec decimals 10.
def var nprprog as char no-undo.
def var pers-handle as handle.
def var parssen-library-proc as handle no-undo.

def var fsprate as dec init 0 no-undo.

DEFINE VARIABLE mNumTemplDb AS INT64 NO-UNDO. /* Номер шаблона содеражщего (БД) иначе 1 */
DEFINE VARIABLE mNumTemplCr AS INT64 NO-UNDO. /* Номер шаблона содеражщего (БК) иначе 1 */

DEFINE BUFFER g-comp-op-buffer FOR op.

&glob spr
def var fspr as char no-undo.


{def-wf.i new}
{defframe.i new}
def buffer xwop for wop.

{dform.i}
release dacct.
release cacct.

{chkacces.i}
{chktempl.i}
/* --- 05.12.97 Peter */
  FIND FIRST signs WHERE signs.FILE-NAME = "op-kind" AND
                         signs.surrogate = op-kind.op-kind AND
                         signs.CODE = "DebugParser" NO-LOCK NO-ERROR.
  IF AVAIL signs THEN DebugParser = INT64(signs.code-val) .
/* --- */
  FIND FIRST signs WHERE signs.FILE-NAME = "op-kind" AND
                         signs.surrogate = op-kind.op-kind AND
                         signs.CODE = "sprate" NO-LOCK NO-ERROR.
  IF AVAIL signs THEN fspr = signs.code-val.

def new shared temp-table temprecid no-undo
      field id as recid
      index id id.
def var l-PrevOpKind as char no-undo.

{frmfield.fun
   &DefTriggerFunction = YES
   &DefProcCheckCustomField = YES
}

{g-currv1.i}
/* plus.vvv 02/09/2014 */



/* ------------ находим безналичный курс ---------------*/
FUNCTION Get_Rates RETURNS DECIMAL
   (
    cur_db AS CHAR,
    cur_cr AS CHAR,
    ftype  AS CHAR
   ):
DEF VAR frate AS DEC INIT 0 NO-UNDO.
/**/
DEF VAR fsell AS CHAR INIT "БНалПр" NO-UNDO.
DEF VAR fbuy  AS CHAR INIT "БНалПок" NO-UNDO.
DEF VAR fspr  AS CHAR INIT "Специальный" NO-UNDO.



	/* для транзакций с безналичными курсами VAL->RUR, RUR->VAL, VAL->VAL */
	IF ftype = "all" THEN
		DO:
			/**/
			IF cur_db = "810" THEN
				/**/
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fsell
					AND instr-rate.instr-code = cur_cr
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/**/
								frate = instr-rate.rate-instr.
								/**/
							END.
						/**/
					/**/
				END.
				/**/
			IF cur_cr = "810" THEN
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fbuy
					AND instr-rate.instr-code = cur_db
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/**/
								frate = instr-rate.rate-instr.
								/**/
							END.
						/**/			
					/**/
				END.
			/**/
		END.



	/* для транзакций с безналичными курсами VAL->VAL */
	IF ftype = "all" THEN
		DO:
			/**/
			IF cur_db <> "810" AND cur_cr <> "810" THEN
				/**/
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fsell
					AND instr-rate.instr-code = cur_cr
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate1 = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fbuy
					AND instr-rate.instr-code = cur_db
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate2 = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
					frate = frate2 / frate1.
					/**/
				END.
			/**/
		END.



	/* для транзакций с безналичными специальными курсами VAL->RUR, RUR->VAL */
	IF ftype = "onlyspr" THEN
		DO:
			/**/
			IF cur_db = "810" THEN
				/**/
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_cr
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
						/**/
					/**/
				END.
				/**/
			IF cur_cr = "810" THEN
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_db
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
						/**/			
					/**/
				END.
			/**/
		END.



	/* для транзакций с безналичными специальными курсами VAL->RUR, RUR->VAL */
	IF ftype = "onlyspr" THEN
		DO:
			/**/
			IF cur_db <> "810" THEN
				/**/
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_cr
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
						/**/
					/**/
				END.
				/**/
			IF cur_cr <> "810" THEN
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_db
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
						/**/			
					/**/
				END.
			/**/
		END.



	/* для транзакций с безналичными специальными курсами VAL->VAL */
	IF ftype = "norate" THEN
		DO:
			/**/
			IF cur_db <> "810" AND cur_cr <> "810" THEN
				/**/
				DO:
					/**/
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_cr
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate1 = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
					FIND FIRST instr-rate
					WHERE instr-rate.instr-cat = "currency"
					AND instr-rate.rate-type = fspr
					AND instr-rate.instr-code = cur_db
					AND instr-rate.since = in-op-date
					NO-LOCK NO-ERROR.
						/**/
						IF AVAIL instr-rate THEN
							DO:
								/* запомним в переменную */
								frate2 = instr-rate.rate-instr.
								/* после чего удалим */
								/*DELETE instr-rate.*/
								/**/
							END.
					frate = frate2 / frate1.
					/**/
				END.
			/**/
		END.
	/**/



	RETURN frate.
END FUNCTION.
/* ------------ находим безналичный курс ---------------*/
    


gen:
do trans with frame opreq
on endkey undo gen, leave gen
on error  undo gen, leave gen:

&IF DEFINED (SelfLibrary) &THEN
    IF RETRY THEN LEAVE.
&ENDIF

    ASSIGN
        l-PrevOpKind = GetXattrValueEx('op-kind', op-kind.op-kind, 'PrevOpKind',?)
        nprprog      = GetXattrValueEx("op-kind",op-kind.op-kind,"nprprog",?)
    .
    if nprprog ne ?
    then do:
        run value(substr(nprprog, 1, if index(nprprog, "((") eq 0
                                 then 255
                                 else index(nprprog, "((") - 1))
              (substr(nprprog, (if index(nprprog, "((") eq 0
                                then ?
                                else index(nprprog, "((") + 2))).
        If keyfunction(lastkey) eq "end-error"
            then return.
    end.


&IF DEFINED (SelfLibrary) &THEN
    /* удаляем предыдущюю библиотеку (если случаено осталась в памяти) */
    pers-handle  = session:first-procedure.

    search-pers-loop:
    do while pers-handle ne ?:

        if pers-handle:file-name              eq "{&SelfLibrary}" and
           pers-handle:private-data           ne ? and
           entry(1, pers-handle:private-data) eq "parssen library"
        then do:

            delete procedure pers-handle.
            pers-handle = session:first-procedure.
        end.
        else
            pers-handle = pers-handle:next-sibling.
    end.

    /* вызываем новую библиотеку для парсера */
    run "{&SelfLibrary}" persistent set parssen-library-proc NO-ERROR.
    IF ERROR-STATUS:ERROR OR pick-value NE "yes" THEN DO:
        UNDO gen, LEAVE gen.
    END.
    &IF DEFINED (SelfLibraryInit) &THEN
        {&SelfLibraryInit}
        IF ERROR-STATUS:ERROR OR pick-value NE "yes" THEN DO:
            UNDO gen, LEAVE gen.
        END.
    &ENDIF
    {g-trbe.i {&*}}
    /* *** */
&ELSE
    {g-trbe.i {&*}}
    if l-PrevOpKind ne ? and
       l-PrevOpKind ne ""
    then do:

        run "op(ggg).p" (in-op-date, l-PrevOpKind, "no-due-date", 6).

        if keyfunction(lastkey) eq "end-error"
            then undo gen, leave gen.

        find first temprecid
        no-lock no-error.

        if not avail temprecid
            then undo gen, leave gen.

        find first op where
            recid(op) eq temprecid.id
        no-lock no-error.

        ASSIGN
            cur-op-trans = op.op-tra
            /* удаляем предыдущюю библиотеку (если случаено осталась в памяти) */
            pers-handle  = session:first-procedure
        .

        search-pers-loop:
        do while pers-handle ne ?:

            if pers-handle:file-name              eq "g-comlib.p" and
               pers-handle:private-data           ne ? and
               entry(1, pers-handle:private-data) eq "parssen library"
            then do:

                delete procedure pers-handle.
                pers-handle = session:first-procedure.
            end.
            else
                pers-handle = pers-handle:next-sibling.
        end.

        /* вызываем новую библиотеку для парсера */
        run "g-comlib.p" persistent set parssen-library-proc.
        run Init_G-Comp1_Library in parssen-library-proc (recid(op), output bacct-db, output bacct-cr, output remove-amt, output send-amt).
        /* *** */
    end.
    if bacct-db ne ?
       then do:

          {find-act.i
             &bact = dacct
             &acct = bacct-db
          }
          if avail dacct then do:
             bcur-db = dacct.currency.

             &IF DEFINED (no-frame-disp) = 0 &THEN
             {dispacct.i
                 &pref=d
                 &side=db
                 &acct="dacct.acct @ bacct-db dacct.currency @ bcur-db remove-amt "
                 &wrapname=Yes
             }
             &ENDIF
         end.
     end.

     if bacct-cr ne ?
     then do:
        {find-act.i
           &bact = cacct
           &acct = bacct-cr
        }
         if avail cacct
         then do:
             bcur-cr = cacct.currency.
             &IF DEFINED (no-frame-disp) = 0 &THEN
             {dispacct.i
                 &pref=c
                 &side=cr
                 &acct="cacct.acct @ bacct-cr cacct.currency @ bcur-cr send-amt "
                 &wrapname=Yes
             }
             &ENDIF
         end.
     end.    
&ENDIF

/* plus.vvv 02/09/2014 */
ON LEAVE OF bacct-cr IN FRAME opreq
DO:
    /* сравниваем валюты счетов */
	/**/
    IF SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3) <> SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3) AND (bacct-db:SCREEN-VALUE NE ? OR bacct-cr:SCREEN-VALUE NE ?) THEN
		DO:
			/**/
			sprate = Get_Rates(SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3), SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3), fspr).
			fsprate = sprate.
			sprate:SCREEN-VALUE = STRING(sprate).
			/**/
		END.
    /*
    RETURN NO-APPLY.
	*/
END.

    prov:
    do
    on error  undo prov, retry prov
    on endkey undo gen,  leave gen:
       	
		&IF DEFINED (no-frame-disp) = 0 &THEN

        &IF DEFINED(SESSION-REMOTE) &THEN
        IF sprate = ? THEN sprate = 0.
        /* Commented by KSV: Для Биссмарт необходим DISPLAY перед SET */
        DISP 
           brate vcomm sprate remove-amt send-amt
           WITH FRAME opreq.
        &ENDIF
        /**/
        set
            bacct-db    when bacct-db eq ?
            bcur-db     when bcur-db  eq ?
            bacct-cr    when bacct-cr eq ?
            bcur-cr     when bcur-cr  eq ?
            dval        when flopval-d
			/* plus.vvv 02/09/2014 */
			/*
            brate       when fspr eq "all"
            vcomm       when fspr eq "all"
            sprate      when fspr eq "onlyspr"
            sprate      when fspr eq "norate"
			*/
            remove-amt
                help "Введите сумму которую вы хотите снять со счета"
            send-amt
                help "Введите сумму которую вы хотите перевести на счет"
        with frame opreq editing:

            readkey.
            {g-op.ed &SearchSecondAcct=Yes &wrapname=Yes &ACCTMESS = yes}

        end.
        &ELSE
            def var tt-acct like op-entry.acct-db no-undo. /* объявляется в g-op.ed */
            {g-assacc.def &currency=t-curr &currlikeparam=Yes}
        &ENDIF
		/**/

		/* plus.vvv 02/09/2014 */
		IF sprate <> fsprate OR sprate = 0 OR sprate = ? THEN
			DO:
				/**/
                                MESSAGE "Не установлен безналичный " + ( IF ((fspr eq "norate") OR (fspr eq "onlyspr")) THEN "СПЕЦИАЛЬНЫЙ " ELSE "" ) + "курс для валюты " + ( IF bcur-db = ? OR bcur-db = ""  THEN bcur-cr ELSE bcur-db )+ " на дату " + STRING(in-op-date) VIEW-AS ALERT-BOX.
				RETURN NO-APPLY.
				/**/
			END.
		/**/
        pause 0.
		
        do
        on error undo prov, leave gen
        on endkey undo gen, leave gen
        with frame edtempl:
		
            RUN CreateFrmFields(?, "opreq", "dval",  STRING(dval, "99/99/9999")).
            RUN CreateFrmFields(?, "opreq", "brate", STRING(brate)).
            RUN CreateFrmFields(?, "opreq", "vcomm", STRING(vcomm)).
            
            ASSIGN
                cl-cat = ""
                i      = 0
            .

            if avail dacct and
               dacct.cust-cat ne "В"
            then ASSIGN
                cl-cat = dacct.cust-cat
                cl-id  = dacct.cust-id
            .

            if avail cacct and
               cacct.cust-cat ne "В"
            then ASSIGN
                cl-cat = cacct.cust-cat
                cl-id  = cacct.cust-id
            .

            flreg = 0.

            if remove-amt ne 0 and
               send-amt   eq 0
            then flreg = 1.
            else if send-amt   ne 0 and
                    remove-amt eq 0
            then flreg = 2.

            {g-opcal.i}
			
            for each wop,
                each op where
                        recid(op) eq wop.op-recid:
               op.op-value-date = dval.
            end.
            /*   disp remove-amt send-amt with frame opreq. */

            for each wop where
                not wop.doc-type begins "(",
            each op where
                recid(op) eq wop.op-recid,
            each op-entry of op:

                create wentry.

                wentry.entry-recid = recid(op-entry).

                /* by Sema 22/02/01 TS 0001105 */
                if op.op-status begins "А" then
                assign
                  op.op-date       = ?
                  op-entry.op-date = op.op-date
                  .

            end.

            &IF DEFINED(NoDispEntries) = 0 &THEN
            run "op-en(ok.p" (13).

            if keyfunc(lastkey) eq "END-ERROR"
                then undo gen, retry gen.
            &ENDIF         
        end.
    end.

    for each wop where not wop.doc-type begins "(",
        op where recid(op) eq wop.op-recid:

        FIND FIRST op-templ WHERE
                op-templ.op-kind EQ wop.op-kind AND
                op-templ.op-templ EQ wop.op-templ NO-LOCK NO-ERROR.

        &IF DEFINED(NoDispOp) = 0 &THEN
 
           vCopDocTempl = GetXAttrValue("op-template", 
                                         op-templ.op-kind + "," + string(op-templ.op-templ), 
                                        "КопДокНомер").

           IF vCopDocTempl <> "" THEN DO:
              FIND FIRST g-comp-op-buffer USE-INDEX op-transaction WHERE
                         g-comp-op-buffer.op-trans EQ cur-op-trans AND
                         g-comp-op-buffer.op-kind  EQ op-templ.op-kind AND
                         g-comp-op-buffer.op-templ EQ INT64(vCopDocTempl)
              NO-LOCK NO-ERROR.

              IF AVAIL g-comp-op-buffer THEN
                op.doc-num = g-comp-op-buffer.doc-num.
           END.

           fler = YES.

           RUN mtdocme.p (RECID(wop),6,OUTPUT fler).
           IF fler THEN UNDO gen, RETRY gen.
        &ENDIF
        {op.upd &undo="undo gen, retry gen"}
    end.
end.



/* удаление спецкурса после транзакции RUB -> VAL */
IF ((SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3) EQ "810") AND (SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3) NE "810") AND (bacct-cr:SCREEN-VALUE NE "") AND ((fspr = "onlyspr") OR (fspr = "norate")))
THEN
   DO:
      FIND FIRST instr-rate WHERE instr-rate.instr-cat = "currency"
                              AND instr-rate.rate-type = "Специальный"
                              AND instr-rate.instr-code = SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3)
                              AND instr-rate.since = in-op-date
                            EXCLUSIVE-LOCK NO-ERROR.
           DELETE instr-rate.
   END.

/* удаление спецкурса после транзакции VAL -> RUB */
IF ((SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3) NE "810") AND (SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3) EQ "810") AND (bacct-cr:SCREEN-VALUE NE "") AND ((fspr = "onlyspr") OR (fspr = "norate")))
THEN
   DO:
      FIND FIRST instr-rate WHERE instr-rate.instr-cat = "currency"
                              AND instr-rate.rate-type = "Специальный"
                              AND instr-rate.instr-code = SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3)
                              AND instr-rate.since = in-op-date
                            EXCLUSIVE-LOCK NO-ERROR.
           DELETE instr-rate.
   END.

/* удаление спецкурса после транзакции VAL -> VAL */
IF ((SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3) NE "810") AND (SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3) NE "810") AND (bacct-cr:SCREEN-VALUE NE "") AND ((fspr = "onlyspr") OR (fspr = "norate")))
THEN
   DO:
      FIND FIRST instr-rate WHERE instr-rate.instr-cat = "currency"
                              AND instr-rate.rate-type = "Специальный"
                              AND instr-rate.instr-code = SUBSTRING(bacct-db:SCREEN-VALUE, 7, 3)
                              AND instr-rate.since = in-op-date
                            EXCLUSIVE-LOCK NO-ERROR.
           DELETE instr-rate.

      FIND FIRST instr-rate WHERE instr-rate.instr-cat = "currency"
                              AND instr-rate.rate-type = "Специальный"
                              AND instr-rate.instr-code = SUBSTRING(bacct-cr:SCREEN-VALUE, 7, 3)
                              AND instr-rate.since = in-op-date
                            EXCLUSIVE-LOCK NO-ERROR.
           DELETE instr-rate.
   END.



&IF DEFINED(NoPrint) = 0 &THEN
{g-print1.i}
&ENDIF

if valid-handle(parssen-library-proc)
    then delete procedure parssen-library-proc.

hide frame opreq.
hide frame edtempl.
