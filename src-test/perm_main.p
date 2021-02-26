{globals.i}
{setdest.i}
{tmprecid.def}
{chkacces.i}
{sh-defs.i}
{card.i}
{clg-cr.err}          /* Ошибки при открытии договоров ПК */
{intrface.get count}  /* Чтобы работал GetCounterNextValue */
{intrface.get xclass} /* Чтобы работало получение начального значения реквизита */
{intrface.get corr}
{intrface.get date}
{intrface.get jloan}
{intrface.get instrum}
{intrface.get db2l}
{intrface.get tmess}
{intrface.get dpspc}
{intrface.get acct}
{intrface.get trnsl}
{intrface.get loan}
{intrface.get refer}    /* Библиотека службы справочников. */
{intrface.get tmcod}
{intrface.get rights}   /* Библиотека для работы с правами и паролями. */

/*DEF input param User_SURR AS CHAR.             /* на кого предоставить права */*/
DEF VAR User_SURR   AS CHAR NO-UNDO.             /* на кого предоставить права */
DEF VAR i           AS DEC NO-UNDO.
DEF VAR j           AS DEC INIT 0 NO-UNDO.
DEF VAR k           AS DEC INIT 0 NO-UNDO.
DEF VAR vChv        AS LOGICAL INIT FALSE NO-UNDO.
DEF VAR vFilial     AS CHAR NO-UNDO.
DEF VAR users       AS CHAR INIT "" NO-UNDO.
DEF VAR User_ID     AS CHAR NO-UNDO INIT
'
0000BAV,
0000BES,
0000BTA,
0000CTN,
0000CYG,
0000GLYS,
0000GTS,
0000IVV,
0000KAAK,
0000KLV,
0000KOAV,
0000KOSA,
0000KSA,
0000LAO,
0000LOO,
0000LSV,
0000PTV,
0000SPS,
0000SYV,
0000TEA,
0000TLA,
0000TTA,
0000TUMA,
0000YOY,
0000VMA,
0000VNY,
0000ZES,
0000ZIV,
0000ZTA,
0000ZYY,
0500KYV,
0500LAA,
0500RYN,
0500SGI,
0500ZSB,
A0400BIV,
A0400CKS,
A0400GNV,
A0400LVL,
B0400NMA,
BIS,
BUH_BVA,
BUH_MNG,
BUH_MOY,
BUH_SIV,
E0400FSA,
I0300SGI,
I0400CII,
I0400FEV,
I0400KAM,
I0400LPY,
I0400PEO,
I0400SAN,
I0400SGI,
I0400STS,
I0400ZSS,
K0400MVS,
K0400SNV,
K0400SYV,
K0400VAK,
K0400VEI,
M0400HNV,
M0400KNA,
M0400KNA,
O0400PDV,
OFM_TTV,
OFM_VOV,
OKO_REV,
OKO_TAK,
OO_CYD,
OU_NNA,
OU_NOA,
OU_SSA,
OU_VGV,
OVC_SEN,
OVC_SOY,
QBIS,
RKO_CYS,
U0400LUV,
U0400SOV,
URB_RUV,
END
'.



form
     User_SURR label "UserID     "

with frame www overlay side-labels 1 col centered row 6 title color bright-white
"[ " + "ВВЕДИТЕ ПОЛЬЗОВАТЕЛЯ" + " ]" width 28.

do on endkey undo, return on error undo, retry with frame www:
display User_SURR.
set 
 User_SURR
editing:
readkey.
apply lastkey.
end.
end.
do on endkey undo, leave on error undo, leave with frame prn:
END.



/*MESSAGE string(LENGTH(User_ID)) view-as alert-box.*/



MESSAGE "Вы уверены что хотите запустить копирование прав на пользователя" User_SURR "?" view-as alert-box QUESTION BUTTONS YES-NO title "ВОПРОС" set vChv.
IF vChv
THEN DO:

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



MESSAGE "Добавлено MAIN-записей" string(k) "!" view-as alert-box title "СООБЩЕНИЕ".
i = k.


 
FOR EACH _user WHERE _userid <> "" NO-LOCK:
    IF AVAIL _user 
         AND GetXattrValueEx("_user", STRING(_userid), "Blocked", "") = "Не блокирован"
         AND GetXattrValueEx("_user", STRING(_userid), "filial-id", "") = GetXattrValueEx("_user", STRING(User_SURR), "filial-id", "")
         AND NOT CAN-DO(User_ID, _userid)
    THEN DO:
             /*MESSAGE _userid view-as alert-box.*/
             k = k + 1.

             create permission.
             assign permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "r"
             permission.allow      = yes
             .
 
             create permission.
             assign permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "w"
             permission.allow      = yes
             .

             create permission.
             assign
             permission.class-code = "slave"
             permission.surrogate  = User_SURR
             permission.user-id    = _userid
             permission.method-id  = "d"
             permission.allow      = yes
             .
         END.
END.



MESSAGE "Добавлено ЕЩЁ записей" string(k - i) "!" view-as alert-box title "СООБЩЕНИЕ".



END.
ELSE RETURN "ERROR".