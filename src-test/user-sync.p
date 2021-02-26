{globals.i}
{sh-defs.i}
   
DEFINE VARIABLE mCreate AS LOGICAL NO-UNDO.   
   
ETIME(YES).

OUTPUT TO VALUE("/home2/bis/quit41d/log/user-sync/user-sync.log") APPEND UNBUFFERED.
PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") SKIP.
FOR EACH qbis.employees EXCLUSIVE-LOCK:
   FIND FIRST bisquit._user WHERE
      bisquit._user._Userid EQ employees.USERID_
   NO-LOCK NO-ERROR.
   IF NOT AVAIL(bisquit._user) THEN
   DO:
      PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " employees удаление - " 
                      qbis.employees.USERID_ ";" qbis.employees.USER_NAME SKIP.
      DELETE qbis.employees.
   END.
END.

FOR EACH signs WHERE
       signs.file-name  EQ "_User" 
   AND signs.code       EQ "gt-isrole"
   AND signs.code-value EQ "нет" NO-LOCK,
   EACH _user WHERE 
   _User._Userid EQ signs.surrogate NO-LOCK:
   FIND FIRST qbis.employees WHERE
          qbis.employees.USERID_ EQ bisquit._user._Userid
   EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL(qbis.employees) THEN
   DO:
      PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " employees создание - " 
                      bisquit._user._Userid ";" bisquit._user._User-Name SKIP.
      mCreate = YES.
      CREATE qbis.employees.
   END.
   IF NOT AVAIL(qbis.employees) 
      OR bisquit._user._User-Name NE qbis.employees.USER_NAME THEN
   DO:
      ASSIGN
         qbis.employees.USERID_         = SUBSTRING(bisquit._user._Userid,1,12)
         qbis.employees.USER_NAME       = SUBSTRING(bisquit._user._User-Name,1,40)
         qbis.employees.USER_MISC       = bisquit._user._User-Misc.
      ASSIGN
	      qbis.employees.U_MISC1         = STRING(bisquit._user._U-misc1[1]) + "|" +
	                                       STRING(bisquit._user._U-misc1[2]) + "|" +
	                                       STRING(bisquit._user._U-misc1[3]) + "|" +
	                                       STRING(bisquit._user._U-misc1[4]) + "|" +
	                                       STRING(bisquit._user._U-misc1[5]) + "|" +
	                                       STRING(bisquit._user._U-misc1[6]) + "|" +
	                                       STRING(bisquit._user._U-misc1[7]) + "|" +
	                                       STRING(bisquit._user._U-misc1[8]).
      ASSIGN	                                       
         qbis.employees.U_MISC2         = TRIM(bisquit._user._U-misc2[1])   + "|" +
                                          TRIM(bisquit._user._U-misc2[2])   + "|" +
                                          TRIM(bisquit._user._U-misc2[3])   + "|" +
                                          TRIM(bisquit._user._U-misc2[4])   + "|" +
                                          TRIM(bisquit._user._U-misc2[5])   + "|" +
                                          TRIM(bisquit._user._U-misc2[6])   + "|" +
                                          TRIM(bisquit._user._U-misc2[7])   + "|" +
                                          TRIM(bisquit._user._U-misc2[8]).
      ASSIGN
         qbis.employees.USER_NUMBER     = bisquit._user._Group_number
         qbis.employees.GROUP_NUMBER    = bisquit._user._Group_number
         qbis.employees.GIVEN_NAME      = SUBSTRING(bisquit._user._Given_name,1,25)
         qbis.employees.MIDDLE_INITIAL  = SUBSTRING(bisquit._user._User-Name,1,1)
         qbis.employees.SURNAME         = SUBSTRING(bisquit._user._Surname,1,25)
         qbis.employees.TELEPHONE       = SUBSTRING(bisquit._user._Telephone,1,20)
         qbis.employees.EMAIL           = SUBSTRING(bisquit._user._Email,1,50  )
         qbis.employees.DESCRIPTION     = SUBSTRING(bisquit._user._Description,1,64)
         qbis.employees.DISABLED        = IF bisquit._user._Disabled THEN "YES" ELSE "NO"
         qbis.employees.CREATE_DATE     = bisquit._user._Create_date
         qbis.employees.ACCOUNT_EXPIRES = bisquit._user._Account_expires
         qbis.employees.PWD_EXPIRES     = bisquit._user._Pwd_expires
         qbis.employees.PWD_DURATION    = bisquit._user._Pwd_duration
         qbis.employees.LAST_LOGIN      = bisquit._user._Last_login
         qbis.employees.LOGINS          = bisquit._user._Logins
         qbis.employees.MAX_LOGINS      = bisquit._user._Max_logins
         qbis.employees.MAX_TRIES       = bisquit._user._Max_tries
         qbis.employees.SPARE1          = bisquit._user._Spare1
         qbis.employees.SPARE2          = bisquit._user._Spare2
         qbis.employees.SPARE3          = SUBSTRING(bisquit._user._Spare3,1,1)
         qbis.employees.SPARE4          = SUBSTRING(bisquit._user._Spare4,1,1).
      IF mCreate EQ NO THEN
         PUT UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " employees изменение - " 
                         employees.USERID_ ";" employees.USER_NAME SKIP.
   END.
END.   

OUTPUT CLOSE.

RETURN.
