[Op-kind 4.1d] // ��ᯮ�� ������ �ந���� GSBIS 16/12/2014 12:00:08 �� "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"CreateCom" "��⮬���᪮� ᮧ����� �����ᨩ �� ����襭�� �����" "g-trans" "WideProc" "BASE" "������ᠫ쭠� �࠭����� ��� ��⮬���᪮�� ᮧ����� �����ᨩ �� ����襭�� �����  � ��⠭���� �痢�
" "common-op-kind" "" "@_ResultCom = @flag;
SetSysConf(""_ResultCom"", ""1"");" "" "_ResultComflag(GETVAR(1(SETVAR(2""_ResultCom""""1""(SetSysConf(2"
.
[Op-kind-tmpl 4.1d] // ��ᯮ�� ������ �ந���� GSBIS 16/12/2014 12:00:08 �� "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
71371 "CreateCom" "op-kind-tmpl" "opb" "// ����, ��।����騩, ᮧ��� ���㬥�� �����ᨨ ��� ���, //1- ᮧ���, 0 - �� ᮧ���

@flag = 1;
" "//�஢�ઠ ��� ���⥫�騪�, ���.���, ���� ���㬥��
if can-do('405*,406*,407*,40802,40804,40807',substr(@acct-db(15),1,5)) &    can-do('30102,30223',substr(@acct-cr(15),1,5)) &    can-do('01,02',@doc-type(10))     
  then @flag = 1;
   else @flag = 0;                                                    
endif;                                                                                                                       
//�஢�ઠ ��।���� ���㬥��, �孮����� � �������
if @flag == 1  
  then 
  (if can-do('1,2',@order-pay(10))||can-do('��,""""',@type(15))||����_������(@op(10) == ""��"")||(@order-pay(10) == '3') & can-do('401*',substr(@ben-acct(10),1,5))
        then @flag = 0;
         else 1;
       endif);
 else 1;     
endif;
                                                           
//�஢�ઠ ����� ������ 䨫����� �� ���⥦�� �१ ��� �� 䨫���
//if @bank-code(16) == ""044030790""
// then @flag = 0;                   
//  else 1;                          
//endif;                             " "flag1(SETVAR(2" "'405*,406*,407*,40802,40804,40807'acct-db15(GETVAR(215(substr(3(can-do(2'30102,30223'acct-cr15(GETVAR(215(substr(3(can-do(2(&'01,02'doc-type10(GETVAR(2(can-do(2(&(jumpcon(+5flag1(SETVAR(2(jumpnoc(+4flag0(SETVAR(2(nop(0flag(GETVAR(11(==(jumpcon(+42'1,2'order-pay10(GETVAR(2(can-do(2'��,""""'type15(GETVAR(2(can-do(2(||op10(GETVAR(2""��""(==(����_������(1(||order-pay10(GETVAR(2'3'(==(||'401*'ben-acct10(GETVAR(215(substr(3(can-do(2(&(jumpcon(+5flag0(SETVAR(2(jumpnoc(+21(nop(0(jumpnoc(+21(nop(0" 0 10 "���� ������" "���� � ��." 0 ""
71376 "CreateCom" "op-kind-tmpl" "op-entry" "" "" "" "" 71371 15 "op-entry" "���� � ��." 0 "�஢����"
71377 "CreateCom" "op-kind-tmpl" "op-bank" "" "" "" "" 71371 16 "op-bank" "���� � ��." 0 "������᪨� ४������"
71378 "CreateCom" "op-kind-tmpl" "op-bank" "" "" "" "" 71371 17 "op-bank" "���� � ��." 0 "������᪨� ४������ �����"
71372 "CreateCom" "op-kind-tmpl" "acctb" "" "" "" "" 0 20 "���� ������" "���� � ��." 0 "���� ��� �� �� �᭮����� ���㬥��"
71373 "CreateCom" "op-kind-tmpl" "opb" "if @flag == 1
then
@acct-inc = ��ࠢ�筨���(""�焮吠��"",""�������,"" + @currency(15) + "","" +������(),���(),���);
if (@���ᮡ�����(10) == """") || (@���ᮡ�����(10) == ?) 
 then                             
   @com = ""�������"";
  else
   @com = ""�������"";
endif;
if @currency(15) == """"
then
 @sum-val = 0;
 @sum-rub = ������������(@amt-rub(15),@com,@acct-db(15),"""",""�����"",""*"");   else
 @sum-val =������������(@amt-cur(15),@com,@acct-db(15),@currency(15),""�����"",""*""); @sum-rub = ��������(@sum-val,@currency(15),"""");
endif;
@doc-stat = ""�"";
@iop-kind = ""CreateCom"";  

if can-do('40807',substr(@acct-db(15),1,5))
then  
@doc-details = ""{VO99090} ������� �� �ਥ� � �ᯮ������ �/�, �।��⠢�. � ���� ᮣ��᭮ ��䠬 �����. ��� �� ����������."";   
else 
@doc-details = ""������� �� �ਥ� � �ᯮ������ �/�, �।��⠢�. � ���� ᮣ��᭮ ��䠬 �����. ��� �� ����������."";       
endif;     
@curr = @currency(15);
else
0;
endif;" "" "flag(GETVAR(11(==(jumpcon(+115acct-inc""�焮吠��""""�������,""currency15(GETVAR(2(+"",""(+(������(0(+(���(0���(��ࠢ�筨���(4(SETVAR(2���ᮡ�����10(GETVAR(2""""(==���ᮡ�����10(GETVAR(2?(==(||(jumpcon(+5com""�������""(SETVAR(2(jumpnoc(+4com""�������""(SETVAR(2(nop(0currency15(GETVAR(2""""(==(jumpcon(+19sum-val0(SETVAR(2sum-rubamt-rub15(GETVAR(2com(GETVAR(1acct-db15(GETVAR(2""""""�����""""*""(������������(6(SETVAR(2(jumpnoc(+26sum-valamt-cur15(GETVAR(2com(GETVAR(1acct-db15(GETVAR(2currency15(GETVAR(2""�����""""*""(������������(6(SETVAR(2sum-rubsum-val(GETVAR(1currency15(GETVAR(2""""(��������(3(SETVAR(2(nop(0doc-stat""�""(SETVAR(2iop-kind""CreateCom""(SETVAR(2'40807'acct-db15(GETVAR(215(substr(3(can-do(2(jumpcon(+5doc-details""{VO99090} ������� �� �ਥ� � �ᯮ������ �/�, �।��⠢�. � ���� ᮣ��᭮ ��䠬 �����. ��� �� ����������.""(SETVAR(2(jumpnoc(+4doc-details""������� �� �ਥ� � �ᯮ������ �/�, �।��⠢�. � ���� ᮣ��᭮ ��䠬 �����. ��� �� ����������.""(SETVAR(2(nop(0currcurrency15(GETVAR(2(SETVAR(2(jumpnoc(+20(nop(0" "" 0 50 "���� ������" "��������" 0 "�������"
71379 "CreateCom" "op-kind-tmpl" "op-entry" "" "" "" "" 71373 55 "op-entry" "��������" 0 "�஢����"
71374 "CreateCom" "op-kind-tmpl" "xlink" "@flag;" "" "flag(GETVAR(1" "" 0 60 "���� ������" "���� � ��." 0 "���� �裡"
71375 "CreateCom" "op-kind-tmpl" "links" "@flag;" "" "flag(GETVAR(1" "" 0 70 "���� ������" "��������" 0 "�������� �裡"
.
[Op-kind-tmpl-ln 4.1d] // ��ᯮ�� ������ �ந���� GSBIS 16/12/2014 12:00:08 �� "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
401104 71371 "opb" "op" "" "search(@_OpMain)" "_OpMain(GETVAR(1(search(1" 1 "��ࠦ����"
401125 71376 "op-entry" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "��ࠦ����"
401126 71377 "op-bank" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "��ࠦ����"
401127 71377 "op-bank" "op-bank-type" "" "search("""")" """""(search(1" 2 "��ࠦ����"
401128 71377 "op-bank" "bank-code-type" "" "search(""���-9"")" """���-9""(search(1" 3 "��ࠦ����"
401129 71378 "op-bank" "op" "" "search(@op(10))" "op10(GETVAR(2(search(1" 1 "��ࠦ����"
401130 71378 "op-bank" "op-bank-type" "" "search(""ben"")         " """ben""(search(1" 2 "��ࠦ����"
401131 71378 "op-bank" "bank-code-type" "" "search(""BIC"")" """BIC""(search(1" 3 "��ࠦ����"
401105 71372 "acctb" "bal-acct" "" "search(substr(@acct-db(15),1,5))" "acct-db15(GETVAR(215(substr(3(search(1" 1 "��ࠦ����"
401106 71372 "acctb" "acct" "" "search(@acct-db(15))" "acct-db15(GETVAR(2(search(1" 2 "��ࠦ����"
401107 71372 "acctb" "currency" "" "search(@currency(15)) " "currency15(GETVAR(2(search(1" 3 "��ࠦ����"
401108 71373 "opb" "doc-type" "017" "" "" 1 "���祭��"
401109 71373 "opb" "doc-num" "" "���稪(""����थ�"") " """����थ�""(���稪(1" 2 "��ࠦ����"
401110 71373 "opb" "doc-date" "" "���() " "(���(0" 3 "��ࠦ����"
401111 71373 "opb" "due-date" "" "���() " "(���(0" 4 "��ࠦ����"
401112 71373 "opb" "op-date" "" "���() " "(���(0" 5 "��ࠦ����"
401113 71373 "opb" "ins-date" "" "���() " "(���(0" 6 "��ࠦ����"
401114 71373 "opb" "order-pay" "5" "" "" 7 "���祭��"
401115 71373 "opb" "op-status" "" "@doc-stat" "doc-stat(GETVAR(1" 8 "��ࠦ����"
401116 71373 "opb" "op-kind" "" "@iop-kind" "iop-kind(GETVAR(1" 9 "��ࠦ����"
401117 71373 "opb" "details" "" "@doc-details" "doc-details(GETVAR(1" 10 "��ࠦ����"
401118 71373 "opb" "op-transaction" "" "@op-transaction(10)" "op-transaction10(GETVAR(2" 11 "��ࠦ����"
401132 71379 "op-entry" "op-status" "" "@op-status(50)" "op-status50(GETVAR(2" 1 "��ࠦ����"
401133 71379 "op-entry" "op-cod" "000000" "" "" 2 "���祭��"
401134 71379 "op-entry" "op-date" "" "@op-date(50)" "op-date50(GETVAR(2" 3 "��ࠦ����"
401135 71379 "op-entry" "acct-db" "" "@acct-db(15)" "acct-db15(GETVAR(2" 4 "��ࠦ����"
401136 71379 "op-entry" "acct-cr" "" "@acct-inc
" "acct-inc(GETVAR(1" 5 "��ࠦ����"
401137 71379 "op-entry" "currency" "" "@curr" "curr(GETVAR(1" 6 "��ࠦ����"
401138 71379 "op-entry" "amt-cur" "" "@sum-val;
                   
 " "sum-val(GETVAR(1" 7 "��ࠦ����"
401139 71379 "op-entry" "amt-rub" "" "@sum-rub;" "sum-rub(GETVAR(1" 8 "��ࠦ����"
401140 71379 "op-entry" "type" "��" "" "" 9 "���祭��"
401141 71379 "op-entry" "op-transaction" "" "@op-transaction(50);" "op-transaction50(GETVAR(2" 10 "��ࠦ����"
401119 71374 "xlink" "link-code" "" "search(""�����"")
" """�����""(search(1" 1 "��ࠦ����"
401120 71374 "xlink" "target-class" "op" "" "" 2 "���祭��"
401121 71375 "links" "beg-date" "" "���()" "(���(0" 1 "��ࠦ����"
401122 71375 "links" "link-id" "" "@link-id(60)" "link-id60(GETVAR(2" 2 "��ࠦ����"
401123 71375 "links" "source-id" "" "@op(10)" "op10(GETVAR(2" 3 "��ࠦ����"
401124 71375 "links" "target-id" "" "@op(50)" "op50(GETVAR(2" 4 "��ࠦ����"
.
[Signs.op-kind] // ��ᯮ�� ������ �ந���� GSBIS 16/12/2014 12:00:08 �� "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"��_�뢮����࠭" "" "CreateCom" "op-kind" "��" ? ?
"Executable" "���" "CreateCom" "op-kind" "" ? ?
"parslib" "" "CreateCom" "op-kind" "pacct,pcrd,pop,pcomm" ? ?
"ProgressBar" "��" "CreateCom" "op-kind" "" ? ?
"TmplOrder" "70" "CreateCom" "op-kind" "" 70 ?
"Transaction" "" "CreateCom" "op-kind" "��" ? ?
.
[Signs.op-template] // ��ᯮ�� ������ �ந���� GSBIS 16/12/2014 12:00:08 �� "-db /home2/bis/quit41d/db-demo/bisquit,-ld bisquit,-Mm 16384,-spin 5000"
"AttrOptimized" "���" "71371" "op-kind-tmpl" "" ? ?
"Transaction" "" "71371" "op-kind-tmpl" "��" ? ?
"AttrOptimized" "���" "71372" "op-kind-tmpl" "" ? ?
"Transaction" "" "71372" "op-kind-tmpl" "��" ? ?
"AttrOptimized" "���" "71373" "op-kind-tmpl" "" ? ?
"Transaction" "" "71373" "op-kind-tmpl" "��" ? ?
"AttrOptimized" "���" "71374" "op-kind-tmpl" "" ? ?
"Transaction" "" "71374" "op-kind-tmpl" "��" ? ?
"AttrOptimized" "���" "71375" "op-kind-tmpl" "" ? ?
"Transaction" "" "71375" "op-kind-tmpl" "��" ? ?
.
