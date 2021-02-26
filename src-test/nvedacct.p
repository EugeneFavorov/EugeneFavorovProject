/*
	�����. ��������� �������/�������� ��⮢ (⥪��� ��� ��������) �� ��ਮ� 
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
  radio-buttons " ����騥 ��� �� (40817*, 40820*) ","1",
                " ������� ��� �� (423*, 426*) ","2",
				/**/
				" ������ ��� �� (40701*, 40702*, 40703*)","3",
				" ������� ��� �� (421*, 422*)","4"
				/**/
  no-label " " skip
 "______________________________________________________" skip
  sel-type view-as radio-set vertical
  radio-buttons " �롮� �� ��᪥ ���ࠧ������� ","1",
                " �롮� �� user-id ���㤭��� ","2"
  no-label " " skip
  
  stmask label "������ ���� 䨫��� " skip
  
  with frame frame-set overlay side-label centered row 9.
  
do with frame frame-set title " ������ ����� "
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
	"�室�� �����:" skip
	"ntarget = " ntarget skip
	"ntypemask = " ntypemask skip
	"ntype = " ntype skip
    "beg-date = " string(beg-date) skip
	"end-date = " string(end-date) skip
.*/

case sel-type:
  /* �� ��᪥ ���ࠧ������� */
  when "1" then do:   					
	ntype = 1.  
	if sel-target = "1" then put unformatted "				��������� �� ⥪�騬 ��⠬ �����⮢ �� (40817*, 40820*)" skip.
	if sel-target = "2" then put unformatted "				��������� �� ������� ��⠬ �����⮢ �� (423*, 426*)" skip.
	if sel-target = "3" then put unformatted "				��������� �� ����� ��⠬ �����⮢ �� (40702*, 40703*)" skip.
	if sel-target = "4" then put unformatted "				��������� �� ������� ��⠬ �����⮢ �� (42106*)" skip.
	
	PUT UNFORMATTED
	" C��, ������ � ��ਮ� � " + string(beg-date) + " �� " string(end-date) skip
    "�������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
    "� ���� �    ����� �����       �  ���� ���� �  ���� ���� � �����.���-��, ��������� ����� �����.���-��, ��������� �����" SKIP
    "�������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
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
				/* ��楤�ઠ ���᪠ ���� ������� � �� ����� */
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
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"���������")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "� "
				SelTT.office " � " 
				delfilfromacct(SelTT.acct) " � "
				string(SelTT.open-date,"99/99/9999") " � "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " � "
				string(SelTT.user_id,"x(27)") " � "
				string(SelTT.close_user_id,"x(27)" ) " � "
			skip.
		end.
		
		
	Put unformatted
	"���������������������������������������������������������������������������������������������������������������������" skip.

	PUT UNFORMATTED
	" C��, ������� � ��ਮ� � " + string(beg-date) + " �� " string(end-date) skip
    "�������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
    "� ���� �    ����� �����       �  ���� ���� �  ���� ���� � �����.���-��, ��������� ����� �����.���-��, ��������� �����" SKIP
    "�������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
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
				/* ��楤�ઠ ���᪠ ���� ������� � �� ����� */
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
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"���������")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "� "
				SelTT.office " � " 
				delfilfromacct(SelTT.acct) " � "
				string(SelTT.open-date,"99/99/9999") " � "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " � "
				string(SelTT.user_id,"x(27)") " � "
				string(SelTT.close_user_id,"x(27)" ) " � "
			skip.
		end.
		
	Put unformatted
	"���������������������������������������������������������������������������������������������������������������������" skip.
  
  end. 
  
	
  /* �� user-id ���짮��⥫� */
  when "2" then do: 					
	ntype = 2. 
	find first _user where _userid = ntypemask no-lock no-error.
	if not avail _user then do:
		message "���짮��⥫� " + ntypemask + " �� ������ � ⠡��� ���짮��⥫��! "
			view-as alert-box.
		return.
	end.
  
	if sel-target = "1" then put unformatted "				��������� �� ⥪�騬 ��⠬ �����⮢ �� (40817*, 40820*)" skip.
	if sel-target = "2" then put unformatted "				��������� �� ������� ��⠬ �����⮢ �� (423*, 426*)" skip.
	if sel-target = "3" then put unformatted "				��������� �� ����� ��⠬ �����⮢ �� (40702*, 40703*)" skip.
	if sel-target = "4" then put unformatted "				��������� �� ������� ��⠬ �����⮢ �� (42106*)" skip.

	
	PUT UNFORMATTED
	" C��, ������ � ��ਮ� � " + string(beg-date) + " �� " string(end-date) skip
    "�������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
    "� ���� �    ����� �����       �  ���� ���� �  ���� ���� � �����.���-��, ��������� ����� �����.���-��, ��������� �����" SKIP
    "�������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
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
				/* ��楤�ઠ ���᪠ ���� ������� � �� ����� */
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
				SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"���������")
				SelTT.close_user_id = bhuser
			.			
			find first _user where _user._userid = SelTT.user_id no-lock no-error.
				if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
			find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
				if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
			put unformatted "� "
				SelTT.office " � " 
				delfilfromacct(SelTT.acct) " � "
				string(SelTT.open-date,"99/99/9999") " � "
				if SelTT.close-date <> ? 
					then string(SelTT.close-date,"99/99/9999")
					else "    --    " " � "
				string(SelTT.user_id,"x(27)") " � "
				string(SelTT.close_user_id,"x(27)" ) " � "
			skip.
		end.
	Put unformatted
	"���������������������������������������������������������������������������������������������������������������������" skip.

	PUT UNFORMATTED
	" C��, ������� � ��ਮ� � " + string(beg-date) + " �� " string(end-date) skip
    "�������������������������������������������������������������������������������������������������������������������Ŀ" SKIP
    "� ���� �    ����� �����       �  ���� ���� �  ���� ���� � �����.���-��, ��������� ����� �����.���-��, ��������� �����" SKIP
    "�������������������������������������������������������������������������������������������������������������������Ĵ" SKIP
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
				/* ��楤�ઠ ���᪠ ���� ������� � �� ����� */
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
					SelTT.user_id = getxattrvalue ("acct",acct.acct + "," + acct.currency,"���������")
					SelTT.close_user_id = bhuser
				.			
				find first _user where _user._userid = SelTT.user_id no-lock no-error.
					if avail _user then assign SelTT.user_id = SelTT.user_id + " " + _user._user-name.
				find first _user where _user._userid = SelTT.close_user_id no-lock no-error.
					if avail _user then assign SelTT.close_user_id = SelTT.close_user_id + " " + _user._user-name.
				put unformatted "� "
					SelTT.office " � " 
					delfilfromacct(SelTT.acct) " � "
					string(SelTT.open-date,"99/99/9999") " � "
					if SelTT.close-date <> ? 
						then string(SelTT.close-date,"99/99/9999")
						else "    --    " " � "
					string(SelTT.user_id,"x(27)") " � "
					string(SelTT.close_user_id,"x(27)" ) " � "
				skip.
			end.	
		end.
	Put unformatted
	"���������������������������������������������������������������������������������������������������������������������" skip.
	
  end. 
  
end case.

{preview.i &col=170}

output stream vvs to value (fname)
    UNBUFFERED  CONVERT  TARGET "1251"  SOURCE "IBM866".
  put stream vvs unformatted
  	"���ࠧ�������" delim
    "���" delim
    "��� ������" delim
    "��� �������" delim
    "����-��, ����訩 ���" delim
    "����-��, �����訩 ���" eol.

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

MESSAGE "����� ���㦥�� � 䠩� " + fname + "." VIEW-AS ALERT-BOX.
RUN sndbispc ("file=" + fname + ";class=bq").

{intrface.del}


/* ���� ������ � �����⨨ ��� */
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
	
	/* ��ᬮ�ਬ ��᫥���� ������ ���ਨ */
	find last bhistory
		where bhistory.file-name EQ 'acct' 
		and bhistory.field-ref EQ acct + ',' + curr
		and bhistory.modify EQ 'W' 
	no-lock no-error.
		
	if avail bhistory then do:
		my_indx =  index(bhistory.field-value, "close-date").
		/* �᫨ � ��� ���� ����� � �����⨨ */
		if my_indx > 0 then
			do:
				suser = bhistory.user-id.
				dclose = bhistory.modif-date.
			end.
		else
		/*  ���� ���� �� �ᥩ ���ਨ c ��⮩ ࠢ��� ��� ����� 㪠������ ���� �������*/
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
	
	/* �᫨ ��祣� �� ��諨 �ਤ���� ��� �� �ᥩ ���ਨ */
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
