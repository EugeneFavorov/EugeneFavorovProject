DEFINE VARIABLE mHandle     AS INT64     NO-UNDO.
DEFINE VARIABLE mUser       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIPAddress  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIPAddrOff  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIString    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCnt        AS INT64     NO-UNDO.
DEFINE VARIABLE mIP         AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE mCFO        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOffice     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mGroups     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mUserFilial AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOk         AS LOGICAL   NO-UNDO.

/*RUN dbgprint.p ("userinfo.i","Begin").*/

/**/
mUser = USERID('bisquit').
FIND FIRST _user WHERE 
   _user._userid EQ mUser 
NO-LOCK NO-ERROR.
IF AVAIL(_user) THEN
   ASSIGN 
      mUser       = mUser + "/" + TRIM(_user._user-name)
      mUserFilial = GetXattrValue("_user",_user._userid,"filial-id").
ELSE RETURN.

RUN STORED-PROC qbis.send-sql-statement mHandle = PROC-HANDLE
     ("Select IGP.Set_User_Info('" + mUser + "') from dual").
FOR EACH qbis.proc-text-buffer WHERE PROC-HANDLE = mHandle:
   /*RUN dbgprint.p ("userinfo.i",TRIM(proc-text)).*/
END.
IF ERROR-STATUS:ERROR
THEN RUN dbgprint.p ("userinfo.i","Ошибка заполнения User_Info.").
ELSE CLOSE STORED-PROCEDURE qbis.send-sql-statement WHERE PROC-HANDLE = mHandle.
/**/
mIPAddress = "...".
FILE-INFO:FILE-NAME = "remhost.txt".
IF FILE-INFO:FILE-TYPE NE ? THEN
DO:
   INPUT FROM VALUE("./remhost.txt").
   REPEAT:
      IMPORT UNFORMATTED mIString.
      IF  mIString NE "" 
         AND (INDEX(mIString,"192.168") GT 0 OR INDEX(mIString,"10.100") GT 0) 
      THEN 
      DO:
         IF INDEX(mIString,"192.168") GT 0 THEN
            mIPAddress = SUBSTRING(mIString,INDEX(mIString,"192.168")).
         IF INDEX(mIString,"10.100") GT 0 THEN
            mIPAddress = SUBSTRING(mIString,INDEX(mIString,"10.100")).
      END.
   END.
   INPUT CLOSE.   
END.
DO mCnt = 1 TO NUM-ENTRIES(mIPAddress,"."):
   mIP[mCnt] = ENTRY(mCnt,mIPAddress,".").
   mIP[mCnt] = TRIM(STRING(INT64(mIP[mCnt]),">>9")).   
END.
mIPAddress = mIP[1] + "." + mIP[2] + "." + mIP[3] + "." + mIP[4].
mIPAddrOff = mIP[1] + "." + mIP[2] + "." + mIP[3] + ".1" .

RUN dbgprint.p ("userinfo.i","IP-address = " + mIPAddress).

IF mIPAddress EQ "0.0.0.0" THEN
DO:
   /*RUN dbgprint.p ("userinfo.i","Не удалось определить IP-address.").*/
END.
ELSE
DO:
   RUN STORED-PROCEDURE send-sql-statement mHandle = PROC-HANDLE NO-ERROR
          ("SELECT IGP.GET_IP_CFO(null,'" + mIPAddress + "') FROM dual").
   IF ERROR-STATUS:ERROR
   THEN
   DO: 
      ASSIGN
         mCFO  = "-1".
      RUN dbgprint.p ("userinfo.i","Не удалось определить код ЦФО.").
   END.
   ELSE 
   DO:
      CLOSE STORED-PROCEDURE send-sql-statement WHERE PROC-HANDLE = mHandle.
      FOR EACH proc-text-buffer WHERE PROC-HANDLE = mHandle:
         ASSIGN
            mCFO  = TRIM(proc-text).
      END.

      IF AVAIL(_user)  THEN
      DO:
         IF mCFO NE "" THEN
            UpdateSigns("_user", STRING(_user._userid), "cfo-user", mCFO, ?).

         FOR EACH signs WHERE signs.file-name   EQ "branch"
                          AND signs.code        EQ "ip-mask"    
                          AND signs.xattr-value EQ mIPAddrOff
             NO-LOCK:
              mOffice = signs.surrogate.
             LEAVE.
         END.

         IF    mIPAddress EQ "192.168.134.70"    /* CPO_V1    Задворнова С.А.  */
            OR mIPAddress EQ "192.168.134.69"    /* CPO_V2    Квинт О.А.       */
            OR mIPAddress EQ "192.168.134.66"    /* CPO_V3    Умиртасова М.Г.  */
            OR mIPAddress EQ "192.168.134.221"   /* CPO_4     Погорелова Н.А.  */
            OR mIPAddress EQ "192.168.133.108"   /* RKO_VIP01                  */
            OR mIPAddress EQ "192.168.140.112"   /* RKO_VIP02 Рудик В.Г.       */
            OR mIPAddress EQ "192.168.140.111"   /* RKO_VIP03                  */
            OR mIPAddress EQ "192.168.134.100"   /* FIZ_OCHV0 Дружняев О.П.    */
            OR mIPAddress EQ "192.168.134.103"   /* FIZ_OCHV1 Федотова Т.А.    */
            OR mIPAddress EQ "192.168.134.102"   /* FIZ_OCHV2                  */
            OR mIPAddress EQ "192.168.134.43"    /* FIZ_OCHV3                  */
            OR mIPAddress EQ "192.168.134.112"   /* FIZ_ORO1                   */
            OR mIPAddress EQ "192.168.134.115"   /* FIZ_ORO2                   */
            OR mIPAddress EQ "192.168.134.104"   /* FIZ_ORO3  Гутова Т.В.      */
            OR mIPAddress EQ "192.168.134.106"   /* FIZ_ORO4  Старостенко Т.А. */
            OR mIPAddress EQ "192.168.137.61"    /* OPER7     Витвицкая Г.В.   */
         THEN 
         ASSIGN
            mOffice = "0518"
            mGroups = "0598,0592".

         IF NOT {assigned mOffice}
         AND mIPAddrOff BEGINS "192.168" THEN mOffice = "0500". 
         
         IF {assigned mOffice} 
         AND mUserFilial EQ "0500" THEN
         DO:
            IF      mIPAddress NE "192.168.140.31"    /* RKO01     Половинкина Е.В. */ 
                AND mIPAddress NE "192.168.137.61"    /* OPER7     Витвицкая Г.В.   */
                AND mIPAddress NE "192.168.134.221"   /* CPO_4     Погорелова Н.А.  */
                AND mIPAddress NE "192.168.131.11"    /* BOF15     Лоптева Ю.В.     */
                AND mIPAddress NE "192.168.133.108"   /* RKO_VIP01 Лашина Е.К.      */
                AND mIPAddress NE "192.168.139.105"   /* IT24      Фаворов Е.В.     */
                AND mIPAddress NE "192.168.140.30"    /* OPER5     Мухина В.Ю.      */
            THEN
            DO:
               IF NOT {assigned mGroups} THEN mGroups = mOffice.
               mOk = UpdateSigns("_user", STRING(_user._userid), "office",        mGroups, ?).
               mOk = UpdateSigns("_user", STRING(_user._userid), "Отделение",     mOffice, ?).
               mOk = UpdateSigns("_user", STRING(_user._userid), "ОтделениеТемп", mOffice, ?).
               mOk = UpdateSigns("_user", STRING(_user._userid), "str-branch",    mOffice, ?).

               IF mOffice EQ "0517"
               THEN
                  mOk = UpdateSigns("_user",STRING(_user._userid),"Принтеры", "0120*",?).
               ELSE
               DO:
                  IF mOffice EQ "0518"
                  THEN 
                     mOk = UpdateSigns("_user",STRING(_user._userid),"Принтеры", "0100*,0500*",?).
                  ELSE
                     mOk = UpdateSigns("_user",STRING(_user._userid),"Принтеры", "01" + SUBSTRING(mOffice,3,2) + "*,05" + SUBSTRING(mOffice,3,2) + "*",?).
               END.
            END.
         END.
      END.
      RUN dbgprint.p ("userinfo.i","ip-address = " + mIPAddress + ";" + 
                                   "mOffice = "    + mOffice + ";" +
                                   "mGroups = "    + mGroups + ";" +
                                   "CFO = "        + mCFO
                     ).
   END.
END.
