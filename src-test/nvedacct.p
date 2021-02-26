/*
	…“. ‚¥¤®¬®áâì ®âªàëâëå/§ ªàëâëå áç¥â®¢ (â¥ªãé¨å ¨«¨ ¢ª« ¤­ëå) §  ¯¥à¨®¤ 
*/

{globals.i}
{client.i}
{tmprecid.def}
{wordwrap.def}
{navigate.def}

DEFINE TEMP-TABLE SelTT	
	field office as character
    field acct as character format "x(20)" 
    field open-date as date
    field close-date as date
    field user_id as character
    field close_user_id as character
.
def new shared stream vvs.
def var fname as char  init "./nvacct.csv"  no-undo.
def var delim as char init ";" format "x(1)" no-undo.
def var eol as char format "x(2)" no-undo.
eol = chr(13) + chr(10). 

def var ntarget as char no-undo.
def var ntype as int no-undo.
def var ntypemask as char no-undo.
def var sel-target as char no-undo init "1".
def var sel-type as char no-undo init "1".
def var stmask as char format "x(30)" no-undo.
def buffer bhistory for history.
def var bhuser as char no-undo.
def var bhcldate as date no-undo.

form
  sel-target view-as radio-set vertical
  radio-buttons " ’¥ªãé¨¥ áç¥â  ”‹ (40817*, 40820*) ","1",
                " ‚ª« ¤­ë¥ áç¥â  ”‹ (423*, 426*) ","2",
				/**/
				"  áç¥â­ë¥ áç¥â  ‹ (40701*, 40702*, 40703*)","3",
				" ‚ª« ¤­ë¥ áç¥â  ‹ (421*, 422*)","4"
				/**/
  no-label " " skip
 "______________________________________________________" skip
  sel-type view-as radio-set vertical
  radio-buttons " ‚ë¡®à ¯® ¬ áª¥ ¯®¤à §¤¥«¥­¨ï ","1",
                " ‚ë¡®à ¯® user-id á®âàã¤­¨ª  ","2"
  no-label " " skip
  
  stmask label "‚¢¥¤¨â¥ ¬ áªã ä¨«ìâà  " skip
  
  with frame frame-set overlay side-label centered row 9.
  
do with frame frame-set title " ‚¢¥¤¨â¥ ¤ ­­ë¥ "
  on error undo, leave on endkey undo, leave:
  pause 0.
  update sel-target sel-type stmask.
end.
hide frame frame-set no-pause.
if keyfunc(lastkey) eq "END-ERROR" then return.

case sel-target:
  when "1" then do: ntarget = "40817*,40820*". end.
  when "2" then do: ntarget = "423*,426*". end.
  when "3" then do: ntarget = "40701*,40702*,40703". end.
  when "4" then do: ntarget = "421*,422*". end.
end case.

{getdates.i}

{setdest.i &col=170 }

ntypemask = stmask. 

/*put unformatted 
	"‚å®¤­ë¥ ¤ ­­ë¥:" skip
	"ntarget = " ntarget skip
	"ntypemask = " ntypemask skip
	"ntype = " ntype skip
    "beg-date = " string(beg-date) skip
	"end-date = " string(end-date) skip
.*/

case sel-type:
  /* ¯® ¬ áª¥ ¯®¤à §¤¥«¥­¨ï */
  when "1" then do:   					
	ntype = 1.  
	if sel-target = "1" then put unformatted "				‚¥¤®¬®áâì ¯® â¥ªãé¨¬ áç¥â ¬ ª«¨¥­â®¢ ”‹ (40817*, 40820*)" skip.
	if sel-target = "2" then put unformatted "				‚¥¤®¬®áâì ¯® ¢ª« ¤­ë¬ áç¥â ¬ ª«¨¥­â®¢ ”‹ (423*, 426*)" skip.
	if sel-target = "3" then put unformatted "				‚¥¤®¬®áâì ¯® à áç¥â­ë¬ áç¥â ¬ ª«¨¥­â®¢ ‹ (40702*, 40703*)" skip.
	if sel-target = "4" then put unformatted "				‚¥¤®¬®áâì ¯® ¢ª« ¤­ë¬ áç¥â ¬ ª«¨¥­â®¢ ‹ (42106*)" skip.
	
	PUT UNFORMATTED
	" Cç¥â , ®âªàëâë¥ ¢ ¯¥à¨®¤ á " + string(beg-date) + " ¯® " string(end-date) skip
    "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" SKIP
    "³ ”ˆ‘ ³    Œ… ‘—…’€       ³  „€’€ ’Š ³  „€’€ ‡€Š ³ ’‚…’.ˆ‘-‹œ, ’Š›‚˜ˆ‰ ‘—…’³ ’‚…’.ˆ‘-‹œ, ‡€Š›‚˜ˆ‰ ‘—…’³" SKIP
    "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" SKIP
	.
	for each acct 
		where can-do(ntypemask, acct.branch-id)  
		and can-do (ntarget,acct.acct)
		and acct.open-date GE beg-date
		and acct.open-date LE end-date
		no-lock:	
			bhuser = "".
			bhcldate = ?.
			/*
			if acct.close-date <> ? then 
				find last bhistory where bhistory.file-name EQ 'acct' 
					and bhistory.modify EQ 'W' 
					and bhistory.field-ref begins acct.acct 
					/*and bhistory.field-value begins 'close-date'*/ no-lock no-error.
			if avail bhistory then do: 
				bhuser = bhistory.user-id.
				bhcldate = bhistory.modif-date.
			end.
			*/
			
			if acct.close-date <> ? then do:
				/* ¯à®æ¥¤ãàª  ¯®¨áª  ¤ âë § ªàëâ¨ï ¨ ªâ® § ªàë« */
				RUN GetClose(INPUT acct.acct,INPUT acct.currency,INPUT acct.close-date,INPUT-OUTPUT bhuser,INPUT-OUTPUT bhcldate).
				/*----------------------------------------------*/
			end.
			/**/
			create SelTT.
			assign 
				SelTT.office = acct.branch-id 
				SelTT.acct = acct.acct
				SelTT.open-date = acct.open-date
				SelTT.close-date = acct.close-date
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"‘®âàâªà‘ç")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "³ "
				SelTT.office " ³ " 
				delfilfromacct(SelTT.acct) " ³ "
				string(SelTT.open-date,"99/99/9999") " ³ "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " ³ "
				string(SelTT.user_id,"x(27)") " ³ "
				string(SelTT.close_user_id,"x(27)" ) " ³ "
			skip.
		end.
		
		
	Put unformatted
	"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" skip.

	PUT UNFORMATTED
	" Cç¥â , § ªàëâë¥ ¢ ¯¥à¨®¤ á " + string(beg-date) + " ¯® " string(end-date) skip
    "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" SKIP
    "³ ”ˆ‘ ³    Œ… ‘—…’€       ³  „€’€ ’Š ³  „€’€ ‡€Š ³ ’‚…’.ˆ‘-‹œ, ’Š›‚˜ˆ‰ ‘—…’³ ’‚…’.ˆ‘-‹œ, ‡€Š›‚˜ˆ‰ ‘—…’³" SKIP
    "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" SKIP
	.
	for each acct 
		where can-do(ntypemask, acct.branch-id)  
		and can-do (ntarget,acct.acct)
		and acct.close-date GE beg-date
		and acct.close-date LE end-date
		no-lock:	
			bhuser = "".
			bhcldate = ?.
			/*
			if acct.close-date <> ? then do:
				find last bhistory where bhistory.file-name EQ 'acct' 
					and bhistory.modify EQ 'W' 
					and bhistory.field-ref begins acct.acct 
					/*and bhistory.field-value begins 'close-date' */ no-lock no-error.
			if avail bhistory then do: 
				bhuser = bhistory.user-id.
				bhcldate = bhistory.modif-date.
			end.	
			*/
			
			if acct.close-date <> ? then do:
				/* ¯à®æ¥¤ãàª  ¯®¨áª  ¤ âë § ªàëâ¨ï ¨ ªâ® § ªàë« */
				RUN GetClose(INPUT acct.acct,INPUT acct.currency,INPUT acct.close-date,INPUT-OUTPUT bhuser,INPUT-OUTPUT bhcldate).
				/*----------------------------------------------*/
			end.
			/**/
		
			create SelTT.
			assign 
				SelTT.office = acct.branch-id 
				SelTT.acct = acct.acct
				SelTT.open-date = acct.open-date
				SelTT.close-date = acct.close-date
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"‘®âàâªà‘ç")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "³ "
				SelTT.office " ³ " 
				delfilfromacct(SelTT.acct) " ³ "
				string(SelTT.open-date,"99/99/9999") " ³ "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " ³ "
				string(SelTT.user_id,"x(27)") " ³ "
				string(SelTT.close_user_id,"x(27)" ) " ³ "
			skip.
		end.
		
	Put unformatted
	"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" skip.
  
  end. 
  
	
  /* ¯® user-id ¯®«ì§®¢ â¥«ï */
  when "2" then do: 					
	ntype = 2. 
	find first _user where _userid = ntypemask no-lock no-error.
	if not avail _user then do:
		message "®«ì§®¢ â¥«ì " + ntypemask + " ­¥ ­ ©¤¥­ ¢ â ¡«¨æ¥ ¯®«ì§®¢ â¥«¥©! "
			view-as alert-box.
		return.
	end.
  
	if sel-target = "1" then put unformatted "				‚¥¤®¬®áâì ¯® â¥ªãé¨¬ áç¥â ¬ ª«¨¥­â®¢ ”‹ (40817*, 40820*)" skip.
	if sel-target = "2" then put unformatted "				‚¥¤®¬®áâì ¯® ¢ª« ¤­ë¬ áç¥â ¬ ª«¨¥­â®¢ ”‹ (423*, 426*)" skip.
	if sel-target = "3" then put unformatted "				‚¥¤®¬®áâì ¯® à áç¥â­ë¬ áç¥â ¬ ª«¨¥­â®¢ ‹ (40702*, 40703*)" skip.
	if sel-target = "4" then put unformatted "				‚¥¤®¬®áâì ¯® ¢ª« ¤­ë¬ áç¥â ¬ ª«¨¥­â®¢ ‹ (42106*)" skip.

	
	PUT UNFORMATTED
	" Cç¥â , ®âªàëâë¥ ¢ ¯¥à¨®¤ á " + string(beg-date) + " ¯® " string(end-date) skip
    "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" SKIP
    "³ ”ˆ‘ ³    Œ… ‘—…’€       ³  „€’€ ’Š ³  „€’€ ‡€Š ³ ’‚…’.ˆ‘-‹œ, ’Š›‚˜ˆ‰ ‘—…’³ ’‚…’.ˆ‘-‹œ, ‡€Š›‚˜ˆ‰ ‘—…’³" SKIP
    "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" SKIP
	.
	for each acct 
		where ntypemask = acct.user-id
		and can-do (ntarget,acct.acct)
		and acct.open-date GE beg-date
		and acct.open-date LE end-date
		no-lock:	
			bhuser = "".
			bhcldate = ?.
			/*
			if acct.close-date <> ? then 
				find last bhistory where bhistory.file-name EQ 'acct' 
					and bhistory.field-ref begins acct.acct 
					and bhistory.modify EQ 'W' 
					/*and bhistory.field-value begins 'close-date'*/ no-lock no-error.
			if avail bhistory then do: 
				bhuser = bhistory.user-id.
				bhcldate = bhistory.modif-date.
			end.	
			*/
			if acct.close-date <> ? then do:
				/* ¯à®æ¥¤ãàª  ¯®¨áª  ¤ âë § ªàëâ¨ï ¨ ªâ® § ªàë« */
				RUN GetClose(INPUT acct.acct,INPUT acct.currency,INPUT acct.close-date,INPUT-OUTPUT bhuser,INPUT-OUTPUT bhcldate).
				/*----------------------------------------------*/
			end.
			/**/
			
			create SelTT.
			assign 
				SelTT.office = acct.branch-id 
				SelTT.acct = acct.acct
				SelTT.open-date = acct.open-date
				SelTT.close-date = acct.close-date
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"‘®âàâªà‘ç")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "³ "
				SelTT.office " ³ " 
				delfilfromacct(SelTT.acct) " ³ "
				string(SelTT.open-date,"99/99/9999") " ³ "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " ³ "
				string(SelTT.user_id,"x(27)") " ³ "
				string(SelTT.close_user_id,"x(27)" ) " ³ "
			skip.
		end.
	Put unformatted
	"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" skip.

	PUT UNFORMATTED
	" Cç¥â , § ªàëâë¥ ¢ ¯¥à¨®¤ á " + string(beg-date) + " ¯® " string(end-date) skip
    "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" SKIP
    "³ ”ˆ‘ ³    Œ… ‘—…’€       ³  „€’€ ’Š ³  „€’€ ‡€Š ³ ’‚…’.ˆ‘-‹œ, ’Š›‚˜ˆ‰ ‘—…’³ ’‚…’.ˆ‘-‹œ, ‡€Š›‚˜ˆ‰ ‘—…’³" SKIP
    "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" SKIP
	.
	for each acct 
		where can-do (ntarget,acct.acct)
		and acct.close-date GE beg-date
		and acct.close-date LE end-date
		no-lock:	
			bhuser = "".
			bhcldate = ?.
			/*
			if acct.close-date <> ? then 
				find last bhistory where bhistory.file-name EQ 'acct' 
					and bhistory.modify EQ 'W' 
					and bhistory.field-ref begins acct.acct 
					/*and bhistory.field-value begins 'close-date' */ no-lock no-error.
			if avail bhistory then do: 
				bhuser = bhistory.user-id.
				bhcldate = bhistory.modif-date.
			end.	
			*/
			if acct.close-date <> ? then do:
				/* ¯à®æ¥¤ãàª  ¯®¨áª  ¤ âë § ªàëâ¨ï ¨ ªâ® § ªàë« */
				RUN GetClose(INPUT acct.acct,INPUT acct.currency,INPUT acct.close-date,INPUT-OUTPUT bhuser,INPUT-OUTPUT bhcldate).
				/*----------------------------------------------*/			
			end.
			/**/
			
			if bhuser = ntypemask then do:
				create SelTT.
				assign 
					SelTT.office = acct.branch-id 
					SelTT.acct = acct.acct
					SelTT.open-date = acct.open-date
					SelTT.close-date = acct.close-date
					SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"‘®âàâªà‘ç")
					SelTT.close_user_id = bhuser
				.			
				find first _user where _user._userid = SelTT.user_id no-lock no-error.
					if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
				find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
					if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
				put unformatted "³ "
					SelTT.office " ³ " 
					delfilfromacct(SelTT.acct) " ³ "
					string(SelTT.open-date,"99/99/9999") " ³ "
					if SelTT.close-date <> ? 
						then string(SelTT.close-date,"99/99/9999")
						else "    --    " " ³ "
					string(SelTT.user_id,"x(27)") " ³ "
					string(SelTT.close_user_id,"x(27)" ) " ³ "
				skip.
			end.	
		end.
	Put unformatted
	"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" skip.
	
  end. 
  
end case.

{preview.i &col=170}

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream vvs unformatted
  	"®¤à §¤¥«¥­¨¥" delim
    "áç¥â" delim
    "„ â  ®âªàëâ¨ï" delim
    "„ â  § ªàëâ¨ï" delim
    "®«ì§-«ì, ®âªàë¢è¨© áç¥â" delim
    "®«ì§-«ì, § ªàë¢è¨© áç¥â" eol.

  for each SelTT no-lock:
    put stream vvs unformatted
  	SelTT.office delim
    SelTT.acct delim
    SelTT.open-date delim
    SelTT.close-date delim
    SelTT.user_id delim
    SelTT.close_user_id eol.
end.
output stream vvs close.

MESSAGE "„ ­­ë¥ ¢ë£àã¦¥­ë ¢ ä ©« " + fname + "." VIEW-AS ALERT-BOX.
RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}


/* ¯®¨áª ¤ ­­ëå ® § ªàëâ¨¨ áç¥â  */
/* RUN GetClose(INPUT acct.acct,INPUT acct.currency,INPUT acct.close-date,INPUT-OUTPUT bhuser,INPUT-OUTPUT bhcldate). */
PROCEDURE GetClose:
	DEF INPUT PARAMETER acct AS CHARACTER.
    DEF INPUT PARAMETER curr AS CHARACTER.
	DEF INPUT PARAMETER date AS DATE.
	DEF INPUT-OUTPUT PARAMETER suser AS CHARACTER.
	DEF INPUT-OUTPUT PARAMETER dclose AS DATE.
	
	/*--------------------------------------*/
	def var my_indx	as decimal init 0 no-undo.
	/*--------------------------------------*/
	
	/* ¯®á¬®âà¨¬ ¯®á«¥¤­îî § ¯¨áì ¨áâ®à¨¨ */
	find last bhistory
		where bhistory.file-name EQ 'acct' 
		and bhistory.field-ref EQ acct + ',' + curr
		and bhistory.modify EQ 'W' 
	no-lock no-error.
		
	if avail bhistory then do:
		my_indx =  index(bhistory.field-value, "close-date").
		/* ¥á«¨ ¢ ­¥© ¥áâì ¤ ­­ë¥ ® § ªàëâ¨¨ */
		if my_indx > 0 then
			do:
				suser = bhistory.user-id.
				dclose = bhistory.modif-date.
			end.
		else
		/*  ¨­ ç¥ ¨¤¥¬ ¯® ¢á¥© ¨áâ®à¨¨ c ¤ â®© à ¢­®© ¨«¨ ¡®«ìè¥ ãª § ­­®© ¤ âë § ªàëâ¨ï*/
			do:
				for each bhistory
					where bhistory.file-name EQ 'acct' 
						and bhistory.field-ref EQ acct + ',' + curr
						and bhistory.modify EQ 'W' 
						and bhistory.modif-date GE date
				no-lock:
				
				my_indx =  index(bhistory.field-value, "close-date").
				
					if my_indx > 0 then do:
						suser = bhistory.user-id.
						dclose = bhistory.modif-date.
						leave.
					end.
				end.
			end.
	end.
	
	/* ¥á«¨ ­¨ç¥£® ­¥ ­ è«¨ ¯à¨¤¥âáï ¨¤â¨ ¯® ¢á¥© ¨áâ®à¨¨ */
	if bhuser = ? and bhcldate = ? then do:
		
		for each bhistory
			where bhistory.file-name EQ 'acct' 
				and bhistory.field-ref EQ acct + ',' + curr
				and bhistory.modify EQ 'W' 
				and bhistory.modif-date LE date
		no-lock:
		
		my_indx =  index(bhistory.field-value, "close-date").
		
			if my_indx > 0 then do:
				suser = bhistory.user-id.
				dclose = bhistory.modif-date.
				leave.
			end.
		end.
	end.
END.
