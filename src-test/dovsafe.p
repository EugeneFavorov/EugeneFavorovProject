{globals.i}
{tmprecid.def}
{prn-doc.def &with_proc=YES}

DEF INPUT PARAM vTmpName AS CHAR NO-UNDO.

DEF VAR vOpenDate	AS CHAR	NO-UNDO.
DEF VAR vDovNom		AS CHAR	NO-UNDO.
DEF VAR vBnkVkl		AS CHAR NO-UNDO.
DEF VAR vBnkVklTmp  AS CHAR NO-UNDO.
DEF VAR vClosDate	AS CHAR	NO-UNDO.
DEF VAR vDovName	AS CHAR	NO-UNDO.
DEF VAR vDovBirth	AS CHAR	NO-UNDO.
DEF VAR vDovBirthPl	AS CHAR	NO-UNDO.
DEF VAR vDovTDoc	AS CHAR	NO-UNDO.
DEF VAR vDovNDoc	AS CHAR	NO-UNDO.
DEF VAR vDovDDoc	AS CHAR	NO-UNDO.
DEF VAR vDovKDoc	AS CHAR	NO-UNDO.
DEF VAR vDovAdr		AS CHAR	NO-UNDO.
DEF VAR vDovINN         AS CHAR	NO-UNDO.
DEF VAR vPolName	AS CHAR	NO-UNDO.
DEF VAR vPolBirth	AS CHAR	NO-UNDO.
DEF VAR vPolBirthPl	AS CHAR	NO-UNDO.
DEF VAR vPolTDoc	AS CHAR	NO-UNDO.
DEF VAR vPolNDoc	AS CHAR	NO-UNDO.
DEF VAR vPolDDoc	AS CHAR	NO-UNDO.
DEF VAR vPolKDoc	AS CHAR	NO-UNDO.
DEF VAR vPolAdr		AS CHAR	NO-UNDO.
DEF VAR vPolINN         AS CHAR	NO-UNDO.
DEF VAR vPolNDov        AS CHAR	NO-UNDO.
DEF VAR vDovDateBeg     AS CHAR	NO-UNDO.
DEF VAR vTmpDat		AS DATE NO-UNDO.
DEF VAR vTmpStr		AS CHAR NO-UNDO.
DEF VAR vTmpDec		AS DEC  NO-UNDO.
DEF VAR vTmpInt		AS INT  NO-UNDO.

DEF BUFFER ploan 	FOR loan.
DEF BUFFER b1cust-role 	FOR cust-role.
DEF BUFFER b2cust-role 	FOR cust-role.
DEF VAR vloan   	AS CHAR	NO-UNDO.

&GLOB Months "ﭢ���,䥢ࠫ�,����,��५�,���,���,���,������,ᥭ����,~
������,�����,�������"
&GLOB Days "��ࢮ�,��஥,����,�⢥�⮥,��⮥,��⮥,ᥤ쬮�,���쬮�,����⮥,����⮥,��������⮥,�������⮥,�ਭ���⮥,���ୠ��⮥,~
��⭠��⮥,��⭠��⮥,ᥬ����⮥,��ᥬ����⮥,����⭠��⮥,�����⮥,������� ��ࢮ�,������� ��஥,������� ����,~
������� �⢥�⮥,������� ��⮥,������� ��⮥,������� ᥤ쬮�,������� ���쬮�,������� ����⮥,�ਤ�⮥,�ਤ��� ��ࢮ�"
&GLOB Years "��ࢮ��,��ண�,���쥣�,�⢥�⮣�,��⮣�,��⮣�,ᥤ쬮��,���쬮��,����⮣�,����⮣�,�������⮣�,�������⮣�,�ਭ���⮣�,~
���ୠ��⮣�,��⭠��⮣�,��⭠��⮣�,ᥬ����⮣�,��ᥬ����⮣�,����⭠�⮣�,�����⮣�,������� ��ࢮ��,������� ��ࢮ��,~
������� ��ண�,������� ���쥣�,������� �⢥�⮣�,������� ��⮣�,��⮣�,������� ᥤ쬮��,������� ���쬮��,������� ����⮣�,�ਤ�⮣�,�ਤ��� ��ࢮ��,~
�ਤ��� ��ࢮ��,�ਤ��� ��ண�,�ਤ��� ���쥣�,�ਤ��� �⢥�⮣�,�ਤ��� ��⮣�,�ਤ��� ��⮣�,�ਤ��� ᥤ쬮��,�ਤ��� ���쬮��,�ਤ��� ����⮣�"

DEFINE VARIABLE mUser     AS CHARACTER          NO-UNDO.
DEF    var iBranch  AS CHAR  NO-UNDO.
DEF    var user_   AS CHAR  NO-UNDO.
DEF    var str_title   AS CHAR  NO-UNDO.
user_ =  USERID("bisquit").   
iBranch = GetXattrValueEx("_user",user_,"�⤥�����",?).
str_title = "[ �롥�� �����ᠭ� (" + user_ + " " + iBranch + ")]".
run signat.p ("dovsafe","UserName",str_title,OUTPUT mUser).

FOR EACH tmprecid,
   FIRST cust-role WHERE recid(cust-role) EQ tmprecid.id 
		    NO-LOCK:
   {empty ttnames}
   IF cust-role.cust-cat = "�"
   THEN DO:

      RUN Insert_TTName("pod-user-dolg", GetXAttrValueEx("_User", mUser, "���������", "")).
      RUN Insert_TTName("pod-user-fio", GetXAttrValueEx("_User", mUser, "����", "")).
      RUN Insert_TTName("pod-user-dov-rp", GetXAttrValueEx("_User", mUser, "����᭒����", "")).

      vloan = entry(2,cust-role.surrogate).
      Find FIRST loan WHERE  loan.contract = "������"                        
                             AND  loan.cont-code = vloan
	                  NO-LOCK no-error.
      if not avail loan then return.  
      vBnkVklTmp = " � " + loan.doc-ref + " �� " + STRING(DAY(loan.open-date)) + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " " + STRING(YEAR(loan.open-date)) + " ����".

      RUN Insert_TTName ("loan-num",vBnkVklTmp).
      vOpenDate = STRING(DAY(loan.open-date)) + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " " + STRING(YEAR(loan.open-date)).
      RUN Insert_TTName ("date",vOpenDate).
      RUN Insert_TTName ("date",vTmpStr).

      vTmpStr = ENTRY(DAY(loan.open-date),{&Days}) + " " + ENTRY(MONTH(loan.open-date),{&Months}) + " ��� ����� " + ENTRY(YEAR(loan.open-date) - 2000,{&Years})+ " ����".

/*      RUN Insert_TTName ("datep",vTmpStr).
*/
      RUN Insert_TTName ("date",vTmpStr).

      FIND FIRST branch WHERE branch.branch-id EQ loan.branch-id NO-LOCK NO-ERROR.
      IF AVAIL branch THEN 
         RUN Insert_TTName ("city",TRIM(ENTRY(2,ENTRY(2,branch.address),'.'))).

      Find FIRST b1cust-role WHERE  b1cust-role.file-name EQ cust-role.file-name                        
                               AND  b1cust-role.surrogate EQ cust-role.surrogate
                               and  b1cust-role.Class-Code  EQ "rent-cust-rent"
                               and  b1cust-role.open-date <= today
                               NO-LOCK no-error.
      if not avail b1cust-role then return.
      FIND FIRST person where person.person-id = dec(b1cust-role.cust-id) NO-LOCK NO-ERROR.
      if not avail person then return.
      vDovName = person.name-last + " " + person.first-names.
      RUN Insert_TTName ("dfio",vDovName).
      vDovBirth = STRING(DAY(person.birthday)) + " " + ENTRY(MONTH(person.birthday),{&Months}) + " " + STRING(YEAR(person.birthday)).
      RUN Insert_TTName ("dbir",vDovBirth).
      vDovBirthPl = getxattrvalue ("person",string(person.person-id), "BirthPlace").
      RUN Insert_TTName ("dbipl",vDovBirthPl).
      vDovTDoc = person.document-id.
      vDovNDoc = person.document.
      RUN Insert_TTName ("ddnu",vDovNDoc).
      vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
      vDovDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
      RUN Insert_TTName ("ddvy",vDovDDoc).
      vDovKDoc = person.issue.
      RUN Insert_TTName ("ddkem",vDovKDoc).
      RUN RetAdr.p(loan.cust-id,loan.cust-cat,"����ய",?,OUTPUT vDovAdr).
      RUN Insert_TTName ("dadr",vDovAdr).

      vDovINN = person.inn.
      if vDovINN = ? then vDovINN = "              ".
      RUN Insert_TTName ("dinn",vDovINN).


      Find LAST b1cust-role WHERE  b1cust-role.file-name EQ cust-role.file-name                        
                          AND  b1cust-role.surrogate EQ cust-role.surrogate
                          and  b1cust-role.Class-Code  EQ "rent-cust-trusted"
                          and  b1cust-role.open-date <= today
                          NO-LOCK no-error.
      if not avail b1cust-role then return.

      vPolNDov = getxattrvalue ("cust-role",string( b1cust-role.cust-role-id ), "TrustNum").
      RUN Insert_TTName ("ndov",vPolNDov).
      vDovDateBeg = STRING(DAY(b1cust-role.open-date)) + " " + ENTRY(MONTH(b1cust-role.open-date),{&Months}) + " " + STRING(YEAR(b1cust-role.open-date)).

      RUN Insert_TTName ("vDovDateBeg",vDovDateBeg).
      vTmpStr = STRING(DAY(b1cust-role.close-date)) + " " + ENTRY(MONTH(b1cust-role.close-date),{&Months}) + " " + STRING(YEAR(b1cust-role.close-date)).
      RUN Insert_TTName ("dsrok",vTmpStr).		

      FIND FIRST person WHERE person.person-id EQ dec(b1cust-role.cust-id) NO-LOCK NO-ERROR.
      if not avail person then return.
      vPolName = person.name-last + " " + person.first-names.
      RUN Insert_TTName ("pfio",vPolName).
      vPolBirth = STRING(DAY(person.birthday)) + " " + ENTRY(MONTH(person.birthday),{&Months}) + " " + STRING(YEAR(person.birthday)).
      RUN Insert_TTName ("pbir",vPolBirth).		
      vPolBirthPl = getxattrvalue ("person",string(person.person-id), "BirthPlace").
      RUN Insert_TTName ("pbipl",vPolBirthPl).		
      vPolTDoc = person.document-id.
      vPolNDoc = person.document.
      RUN Insert_TTName ("pdnu",vPolNDoc).		
      vTmpDat = DATE(getxattrvalue ("person",string(person.person-id), "Document4Date_vid")).
      vPolDDoc = STRING(DAY(vTmpDat)) + " " + ENTRY(MONTH(vTmpDat),{&Months}) + " " + STRING(YEAR(vTmpDat)).
      RUN Insert_TTName ("pdvy",vPolDDoc).		
      vPolKDoc = person.issue.
      RUN Insert_TTName ("pdkem",vPolKDoc).		
      RUN RetAdr.p(person.person-id,"�","����ய",?,OUTPUT vPolAdr).
      RUN Insert_TTName ("padr",vPolAdr).
      vPolINN = person.inn.
      if vPolINN = ? then vPolINN = "              ".
      RUN Insert_TTName ("pinn",vPolINN).
/* ४������ ���㤭��� , ����� ���� ������� */
      FIND FIRST _User WHERE _User._Userid EQ mUser NO-LOCK NO-ERROR.
      if not avail _User then return.     
      IF NUM-ENTRIES(STRING(_user._User-Name),".") > 1 THEN vTmpStr = STRING(_user._User-Name).
      ELSE vTmpStr = ENTRY(1,STRING(_user._User-Name)," ") + " " +
			   SUBSTRING(ENTRY(2,STRING(_user._User-Name)," "),1,1,"CHARACTER") + "." +
			   SUBSTRING(ENTRY(3,STRING(_user._User-Name)," "),1,1,"CHARACTER") + ". ".
      RUN Insert_TTName ("sotfi.o.",vTmpStr).
      vTmpStr = GetXAttrValueEx("_user", mUser, "User-nameTP", "").
      RUN Insert_TTName ("sotfiotp",vTmpStr).
      vTmpStr = GetXAttrValueEx("_user", mUser, "����᭍����", "").
      RUN Insert_TTName ("sotdn",vTmpStr).
      vTmpStr = GetXAttrValueEx("_user", mUser, "����᭄��", "").
      IF NUM-ENTRIES(vTmpStr,"/") GE 3 THEN
      vTmpStr = ENTRY(1,vTmpStr,"/") + " " + ENTRY(INT64(ENTRY(2,vTmpStr,"/")),{&Months}) + " " + ENTRY(3,vTmpStr,"/").
      RUN Insert_TTName ("sotdd",vTmpStr).
      vTmpStr = GetXAttrValueEx("_user", mUser, "���������", "").
      vTmpStr = CAPS(SUBSTRING(vTmpStr,1,1)) + SUBSTRING(vTmpStr,2).
      RUN Insert_TTName ("sotdol",vTmpStr).
      vTmpStr = GetXAttrValueEx("_user", mUser, "��������쒏", "").
      vTmpStr = LC(SUBSTRING(vTmpStr,1,1)) + SUBSTRING(vTmpStr,2).
      RUN Insert_TTName ("sotdoltp",vTmpStr).
      RUN printvd.p (vTmpName,INPUT TABLE ttnames).
   end.
END.
   
   
