[DataClass 4.1d] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "REFERENCE" "" "�����������" 910 yes "����⥪� �ࠢ�筨���." no "" 1 "" "" "" "" "" "" "" "" "" "����������� ��� ������ �����" 101
.
[DataClass 4.1d] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "Sec" "Reference" "����������� ��� ������ �����" 101 yes "" no "*" 1 "" "" "" "" "" "" "" "OLTP,oltpb" "*" "����������� ��� ������ �����" 101
.
[DataClass 4.1d] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
no "�࣐���" "Sec" "�࣠����樨 ��� ᤥ��� ����" 20 no "" no "*" 1 "" "" "" "" "" "" "" "OLTP,oltpb" "*" "����������� ��� ������ �����" 101
.
"OLAPDataBlock" "BISquit 4.1D124" "�࣐���" "" "01/01/2016" "01/01/2016" ? "" 113706 "�࣠����樨" "" 61 " // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� '-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500'"
"" "0000" "0000010207" "1" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "" "0000010208" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "840" "0000010211" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010207" "978" "0000010214" "2" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010208" "�" "0000010209" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010209" "                   139690" "0000010210" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424810801300010001"
"0000010211" "�" "0000010212" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010212" "                   139690" "0000010213" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424840300000000002"
"0000010214" "�" "0000010215" "3" 0 0 0 0 0 0 0 0 0 0 0 0 ""
"0000010215" "                   139690" "0000010216" "4" 0 0 0 0 0 0 0 0 0 0 0 0 "30424978600000000001"
.
[IndicationAttr] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"�࣐���|113706" "filial-id" "��� 䨫����" "character" "x(8)" "������" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.filial-id" 1 no 2 "" 12/12/16 ?
"�࣐���|113706" "currency" "��� ������" "character" "xxx" "��� ������" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.currency" 2 no 2 "" 12/12/16 ?
"�࣐���|113706" "cust-cat" "��� ������" "character" "X" "��� ������" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.cust-cat" 3 no 2 "" 12/12/16 ?
"�࣐���|113706" "cust-id" "������ N" "integer" "9999999" "������ N" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.cust-id" 4 no 2 "" 12/12/16 ?
"�࣐���|113706" "acct" "��楢�� ���" "character" "x(25)" "��楢�� ���" "Sum" "" "" "" "" "" "" "" "" "" "" "" "" "" "acct.acct" 5 yes 2 "" 12/12/16 ?
.
[Signs] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "Indication" "�࣐���" "dataclass" "" ? ?
"�����" "��" "�࣐���" "dataclass" "" ? ?
"module" "" "REFERENCE" "dataclass" "base" ? ?
"class-code" "Indication" "Sec" "dataclass" "" ? ?
"�����" "��" "Sec" "dataclass" "" ? ?
"module" "" "Sec" "dataclass" "base" ? ?
.
[Signs.IndicationAttr] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "" "�࣐���|113706,filial-id,12/12/16" "DataAttr" "IndicationAttr" ? ?
"��᪠" "0" "�࣐���|113706,filial-id,12/12/16" "DataAttr" "" 0 ?
"���������ࢠ�" "" "�࣐���|113706,filial-id,12/12/16" "DataAttr" "��" ? ?
"class-code" "" "�࣐���|113706,currency,12/12/16" "DataAttr" "IndicationAttr" ? ?
"��᪠" "0" "�࣐���|113706,currency,12/12/16" "DataAttr" "" 0 ?
"���������ࢠ�" "" "�࣐���|113706,currency,12/12/16" "DataAttr" "��" ? ?
"class-code" "" "�࣐���|113706,cust-cat,12/12/16" "DataAttr" "IndicationAttr" ? ?
"��᪠" "0" "�࣐���|113706,cust-cat,12/12/16" "DataAttr" "" 0 ?
"���������ࢠ�" "" "�࣐���|113706,cust-cat,12/12/16" "DataAttr" "��" ? ?
"class-code" "" "�࣐���|113706,cust-id,12/12/16" "DataAttr" "IndicationAttr" ? ?
"��᪠" "0" "�࣐���|113706,cust-id,12/12/16" "DataAttr" "" 0 ?
"���������ࢠ�" "" "�࣐���|113706,cust-id,12/12/16" "DataAttr" "��" ? ?
"class-code" "" "�࣐���|113706,acct,12/12/16" "DataAttr" "IndicationAttr" ? ?
"��᪠" "0" "�࣐���|113706,acct,12/12/16" "DataAttr" "" 0 ?
"���������ࢠ�" "" "�࣐���|113706,acct,12/12/16" "DataAttr" "��" ? ?
.
[Signs.IndicationBlock] // ��ᯮ�� ������ �ந���� 0000BAV 15/12/2016 11:20:40 �� "-db /home2/bis/quit41d/db/bisquit,-ld bisquit,-L 200000,-n 500,-B 8000,-omsize 1400,-spin 10000,-U qbis,-P,-n 500"
"class-code" "IndicationBlock" "113706" "DataBlock" "" ? ?
.
