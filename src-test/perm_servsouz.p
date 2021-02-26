/*DEF input param User_SURR AS CHAR.               /* на кого предоставить права */*/
DEF VAR User_SURR AS CHAR NO-UNDO INIT 'SERVSOUZ'. /* на кого предоставить права */
DEF VAR i         AS DEC NO-UNDO.
DEF VAR j         AS DEC INIT 0 NO-UNDO.
DEF VAR k         AS DEC INIT 0 NO-UNDO.
DEF VAR vChv      AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR users     AS CHAR INIT "" NO-UNDO.
DEF VAR User_ID   AS CHAR NO-UNDO INIT
'
_SERV,
0000BAV,
0000DIV,
0000GYV,
0000IDN,
0000IVV,
0000KDN,
0000MVP,
0000RNV,
0000SLV,
0000VLEI,
0000VVV,
0000ZAV,
0000SER,
0300VLEI,
0500VLEI,
0500SGI,
ABBIS,
AKO_BNV,
AKO_KIS,
AKO_PVP,
AKO_SEL,
AKO_VNV,
ANBIS,
BIBIS,
BIS,
ESBIS,
GSBIS,
I0300SGI,
I0400CII,
I0400FEV,
I0400KAM,
I0400LPY,
I0400PEO,
I0400SGI,
I0400STS,
I0400ZSS,
IBBIS,
IEBIS,
IRBIS,
K0400IEV,
K0400KNE,
K0400RDS,
K0400SYV,
K0400YSA,
LKBIS,
MABIS,
MIBIS,
NKBIS,
NSBIS,
OIK_KEA,
PKKO_SES,
PKO_ZKO,
POBIS,
QBIS,
RABIS,
RSBIS,
SABIS,
SERV0000,
SERV0300,
SERV0400,
SERV0500,
SERVCRED,
SERVREP,
SERVSOUZ,
SOBIS,
SSBIS,
VMBIS,
YABIS,
ZEBIS,
END
'.



MESSAGE "Вы уверены что хотите запустить копирование прав на "
        + User_SURR "?" view-as alert-box QUESTION BUTTONS YES-NO title "ВОПРОС" set vChv.
IF vChv
THEN DO:

/*MESSAGE string(LENGTH(User_ID)) view-as alert-box.*/

DO i = 1 TO LENGTH(User_ID):
   IF ((LENGTH(ENTRY(INT(i), User_ID, ',')) < 2) OR (ENTRY(INT(i), User_ID, ',') = "END"))
   THEN LEAVE.
   ELSE DO: 
            j = j + 1. 
            /*MESSAGE string(LENGTH(ENTRY(INT(i), User_ID, ','))) view-as alert-box.*/
        END.
END.

/*MESSAGE string(j) view-as alert-box.*/



i = 0.
FOR EACH permission WHERE /*CAN-DO(User_ID, permission.user-id)
                    AND*/ permission.class-code EQ "slave"
                      AND permission.surrogate  EQ User_SURR
                    /*AND permission.method-id  EQ "r"*/
                    /*AND permission.allow      EQ YES*/
         EXCLUSIVE-LOCK:
         IF AVAIL permission 
            THEN DO:
                     i = i + 1.
                     /*MESSAGE string(permission.class-code) string(permission.surrogate) string(permission.user-id) string(permission.method-id) string(permission.allow) view-as alert-box.*/
                     /*users = users + "," + permission.user-id.*/
                     delete permission.
                 END.
END.
MESSAGE "Удалено записей" string(int(i / 3)) "!" view-as alert-box title "СООБЩЕНИЕ".



DO i = 1 TO j:

   /*MESSAGE entry(int(i), User_ID) view-as alert-box.*/

   /*IF NOT CAN-DO(users, entry(int(i), User_ID))
   THEN DO:*/
  
   k = k + 1.
   create permission.
   assign permission.class-code = "slave"
   permission.surrogate  = User_SURR
   permission.user-id    = entry(int(i), User_ID)
   permission.method-id  = "r"
   permission.allow      = yes
   .
 
   create permission.
   assign permission.class-code = "slave"
   permission.surrogate  = User_SURR
   permission.user-id    = entry(int(i), User_ID)
   permission.method-id  = "w"
   permission.allow      = yes
   .

   create permission.
   assign
   permission.class-code = "slave"
   permission.surrogate  = User_SURR
   permission.user-id    = entry(int(i), User_ID)
   permission.method-id  = "d"
   permission.allow      = yes
   .      

   /*END.*/

END.
 


MESSAGE "Добавлено записей" string(k) "!" view-as alert-box title "СООБЩЕНИЕ".

END.
ELSE RETURN "ERROR".