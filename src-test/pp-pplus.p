/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ПАО "Плюс Банк"
     Filename: pp-pplus.p
      Comment: (0019702) Библиотека функций
               парсера Плюс Банк
   Parameters: нет
      Created: 12.09.2003 12:40 guva
*/

{globals.i}
{intrface.get acct}
{intrface.get tmess}
{intrface.get instrum}
{intrface.get trans}
{intrface.get xclass}
{intrface.get tparam}
{intrface.get rights}
{intrface.get data}
{intrface.get db2l}
{intrface.get count}
{intrface.get strng}
{intrface.get brnch}
{intrface.get refer}
{intrface.get cust}
{intrface.get osyst}
{intrface.get print}
{intrface.get prnvd}
{intrface.get kau}
{intrface.get parsr}
{intrface.get widg}
{intrface.get tmcod}
{intrface.get oldpr}
{initstrp.i}

{form.def}
{tmprecid.def}
{g-trans.equ}
{ksh-defs.i}
{typeclentbyacct.i}
DEFINE VARIABLE mOpDate       AS DATE       NO-UNDO.
DEFINE VARIABLE mTmplID       AS INT64    NO-UNDO.
DEFINE VARIABLE mProcHdl      AS HANDLE     NO-UNDO.
DEFINE VARIABLE mQTemplate    AS HANDLE     NO-UNDO.
DEFINE VARIABLE mDebugLevel   AS INT64    NO-UNDO.
DEFINE VARIABLE mOpkind       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mBreak        AS INT64    NO-UNDO.

DEFINE STREAM sSpoolTXT.
DEFINE VARIABLE mMaxWidthTxt AS INT64   NO-UNDO.
DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE mIsMacro     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mDate446     AS DATE      NO-UNDO.
DEFINE VARIABLE mDate446Str  AS CHARACTER NO-UNDO.

mDate446Str = FGetSetting("ЦБ446П","Дата446П","").
IF LENGTH(mDate446Str) GT 0 THEN DO:
   mDate446 = DATE(mDate446Str) NO-ERROR.
END.
/****************************************************************************/
DEF temp-table tt-tmprecop NO-UNDO
    FIELD op LIKE op.op
.
/*****************************************************************************
 *       Объявление данных для управления циклическим выполнением транзакции *
 ****************************************************************************/
/* Commented BY KSV: Хранит информацию обо всех циклах, запущенных из
** транзакции */
DEFINE TEMP-TABLE tLoop NO-UNDO
   FIELD loop-id     AS INT64
   FIELD src-op-kind AS CHARACTER
   FIELD dst-op-kind AS CHARACTER
   FIELD counter     AS INT64
   INDEX iLoop-id IS PRIMARY UNIQUE loop-id
   INDEX iDst-op-kind dst-op-kind
   INDEX iSrc-op-kind src-op-kind.
/* Commented BY KSV: Счетчик циклов - просто выдает последовательность целых
** чисел */
DEFINE VARIABLE mLoopSeq AS INT64    NO-UNDO.

/* Commented BY KSV: Открытие нового цикла */
&GLOBAL-DEFINE CREATE_TLOOP CREATE tLoop. ~
                            ASSIGN ~
                              tLoop.src-op-kind = mOpkind ~
                              tLoop.loop-id     = mLoopSeq + 1. ~
                            mLoopSeq = mLoopSeq + 1. ~
                            DEFINE VARIABLE vLoopID AS INT64    NO-UNDO. ~
                            vLoopID = tLoop.loop-id.
/* Commented BY KSV: Получение информации о текущем цикле */
&GLOBAL-DEFINE GET_LOOPINFO FIND FIRST tLoop WHERE tLoop.loop-id = vLoopID NO-ERROR.
/* Commented BY KSV: Удаление информации о цикле */
&GLOBAL-DEFINE DEL_TLOOP    ~{&GET_LOOPINFO~} ~
                            IF AVAIL tLoop THEN DELETE tLoop.

/* "Приемник" возвращаемых значений. */
{ttretval.def}

/* Commented BY KSV: Стек запущенных транзакций. Хранит контекст запущенных
** транзакций */
DEFINE TEMP-TABLE tStack NO-UNDO
   FIELD fStackID          AS INT64     /* Идентификатор стека */
   FIELD fTmplID           AS INT64     /* Идентификатор шаблона */
   FIELD fOpkind           AS CHARACTER   /* Идентификатор транзакции  */
   FIELD fOpdate           AS DATE        /* Дата опердня */
   FIELD fProcHdl          AS HANDLE      /* Хэндл процедуры транзакции */
   FIELD fObjTransaction   AS INT64     /* Идентификатор операции */
   FIELD fOpTransaction    AS INT64     /* Идентификатор объекта операции */
   FIELD fQTemplate        AS HANDLE      /* Хэндл запроса по шаблонам транзакции */
   INDEX iStackID IS PRIMARY UNIQUE fStackID
   .

&GLOBAL-DEFINE PUSH_TSTACK CREATE tStack.~
                           ASSIGN ~
                              tStack.fStackID = mLoopSeq + 1 ~
                              tStack.fTmplID  = mTmplID ~
                              tStack.fOpkind  = mOpkind ~
                              tStack.fProcHdl = mProcHdl ~
                              tStack.fQTemplate = mQTemplate ~
                              tStack.fOpdate  = mOpDate. ~
                           tStack.fObjTransaction = INT64(GetSysConf("obj-transaction")) NO-ERROR.~
                           tStack.fOpTransaction = INT64(tGetParam("op-transaction",?,?)) NO-ERROR. ~
                           mLoopSeq = mLoopSeq + 1.
&GLOBAL-DEFINE POP_TSTACK  FIND LAST tStack NO-ERROR. ~
                           IF AVAILABLE tStack THEN ~
                           DO: ~
                              ASSIGN ~
                                 mTmplID  = tStack.fTmplID ~
                                 mOpkind  = tStack.fOpkind ~
                                 mProcHdl = tStack.fProcHdl ~
                                 mQTemplate = tStack.fQTemplate ~
                                 mOpDate  = tStack.fOpdate. ~
                              RUN SetSysConf IN h_base ("obj-transaction",tStack.fObjTransaction). ~
                              RUN tSetParam IN h_tparam ("op-transaction",tStack.fOpTransaction,mProcHdl,?). ~
                              DELETE tStack. ~
                           END. ~
                           ELSE ~
                           DO: ~
                              RUN Fill-SysMes("","trans30","","%s=" + mOpkind + ~
                                                              "%s=" + STRING(mTmplID)). ~
                              UNDO, RETURN ERROR. ~
                           END.


{pfuncdef
   &LIBDEF        = "YES"
   &NAME          = "PPLUS"
   &LIBNAME       = "Библиотека функций парсера Плюс Банк."
   &DESCRIPTION   = "Работа с шаблонами и транзакциями, Плюс Банк."
   }

{pfuncdef
   &NAME          = "ОФФИС"
   &DESCRIPTION   = "Возвращает код оффиса текущ. пользователя. Если список филиалов ~
   не задан, работает во всех филиалам. Если задан - по филиалам из списка ~
   возвращает код оффиса, из других филиалов - возвращает код подразделения. ~
   &PARAMETERS    = "[СПИСОК ФИЛИАЛОВ]"
   &RESULT        = "КОД ОФФИСА"
   &SAMPLE        = "ОФФИС('0500') = '0500' - код оффиса текущего пользователя  ~n~
ОФФИС() = '0500' - подразделение текущего пользователя"
   }
   DEFINE INPUT  PARAMETER iFilialList AS CHARACTER   NO-UNDO.
/*   DEFINE INPUT  PARAMETER iUserID     AS CHARACTER   NO-UNDO. */
   DEFINE OUTPUT PARAMETER out_Result  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.

   DEFINE VARIABLE mUserID AS CHARACTER NO-UNDO.

   out_result = "".

   iFilialList = TRIM(iFilialList,"~"'").
   mUserID = USERID("bisquit").
   IF iFilialList EQ ? THEN
      iFilialList = "*".
   IF CAN-DO(iFilialList, shFilial) THEN
      out_result = GetXAttrValueEx("_user",mUserID,"office","").
   IF out_Result EQ "" THEN
      out_result = GetUserBranchId(mUserID).
END PROCEDURE.


{pfuncdef
    &NAME          = "ОФФИС2"
    &DESCRIPTION   = "Возвращает код оффиса текущ. пользователя. Если список фили
    не задан, работает во всех филиалам. Если задан - по филиалам из списка ~
    возвращает код оффиса, из других филиалов - возвращает код подразделения. ~
    &PARAMETERS    = "[СПИСОК ФИЛИАЛОВ]"^
    &RESULT        = "КОД ОФФИСА"
    &SAMPLE        = "ОФФИС2('0500') = '0500' - код оффиса текущего пользователя
    ОФФИС2() = '0500' - подразделение текущего пользователя"
    }
    
       DEFINE INPUT  PARAMETER iFilialList AS CHARACTER   NO-UNDO.
       /*   DEFINE INPUT  PARAMETER iUserID     AS CHARACTER   NO-UNDO. */
          DEFINE OUTPUT PARAMETER out_Result  AS CHARACTER   NO-UNDO.
             DEFINE OUTPUT PARAMETER is-ok       AS INT64     NO-UNDO.
    DEFINE VARIABLE mUserID AS CHARACTER NO-UNDO.
    out_result = "".
    iFilialList = TRIM(iFilialList,"~"'").
    mUserID = USERID("bisquit").
    IF iFilialList EQ ? THEN
    iFilialList = "*".
    IF CAN-DO(iFilialList, shFilial) THEN
    out_result = GetXAttrValueEx("_user",mUserID,"office","").
    IF substr(out_result,1,4) EQ "0598"
    THEN out_result = "0598".
    IF out_Result EQ "" THEN
    out_result = GetUserBranchId(mUserID).
    END PROCEDURE.