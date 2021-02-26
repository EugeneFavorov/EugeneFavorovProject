{globals.i}
{intrface.get tmess}

/* +++ safejoin.p was humbly modified by (c)blodd converter v.1.11 on 8/24/2017 1:29pm +++ */

/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2008 ЗАО "Банковские информационные системы"               
     Filename: safejoin.p
      Comment: Меню связанных данных для браузера договоров аренды сейфовых ячеек.
   Parameters:
         Uses:
      Used by:
      Created: 27.03.2008 18:07 ALEXK   
     Modified: 27.03.2008 18:07 ALEXK    <comment>
     Modified: 31/10/2008 kraw (0094516) подъем в основную версию      
     Modified: 02/03/2009 kraw (0107147) проводки отдельно балансовые и внебалансовые
     Modified: 29/03/2010 kraw (0120487) Ведение договоров на оказание консультации
*/

{joinpar.i}
{globals.i}             /* Глобальные переменные сессии. */
{intrface.get xclass}   /* Библиотека инструментов метасхемы. */
{intrface.get db2l}     /* Инструмент для динамической работы с БД. */
{intrface.get trans}
/* объявление таблицы TmpObj */
{tmpobj.def}

DEFINE VARIABLE mCodLst  AS CHARACTER  NO-UNDO. /* Список кодов                         */
DEFINE VARIABLE mValLst  AS CHARACTER  NO-UNDO. /* Список значений                      */

DEFINE VARIABLE mContract AS CHARACTER    NO-UNDO.
DEFINE VARIABLE mContCode AS CHARACTER    NO-UNDO.

DEFINE BUFFER loan  FOR loan.
DEFINE BUFFER bloan FOR loan.

FIND loan WHERE ROWID(loan) = TO-ROWID(iROWID) NO-LOCK.

mContract =   loan.contract.
mContCode =   loan.cont-code.

/* создаем подпункт меню Состояние договора */
/*
RUN CreateJoin("Состояние договора",
               "lpar-axd`" + 
               loan.contract + "," + 
               loan.cont-code + 
               ",1,1," +             /*затычка*/
               STRING(level + 1),
               YES
              ).
*/

RUN CreateJoinLd("Картотека счетов",
                 "browseld",
                 "loan-acct",
                 "contract,cont-code",
                 loan.contract + "," + loan.cont-code,
                 "",
                 STRING(level + 1),                 
                 YES
               ).

RUN SetSysConf IN h_base ("SFContract", loan.contract).
RUN SetSysConf IN h_base ("SFContCode", loan.cont-code).
/*
RUN AddAttr2TableEx ("",0,-1,"",0,"_SFContract",loan.contract).
RUN AddAttr2TableEx ("",0,-1,"",0,"_SFContCode",loan.cont-code).
*/

IF iclass NE "loan-count" THEN
DO:

   ASSIGN
      mCodLst = "НомДогНазнДогKauClass" 

      mValLst = loan.cont-code + CHR(1) + loan.contract + CHR(1) + "АРЕНДА"
   .
   
   RUN CreateJoin("Документы"
                  ,
                  "s-br(op)`" + "," + mCodLst + "," + mValLst + ",," + STRING(level + 1)
                  ,
                  YES
                 ).

   RUN CreateJoin("Проводки",
                  "l-sf-jb`"
                  + iClass + ","
                  + iROWID
                  + ",1,"
                  + STRING(level + 1) + ",,", 
                  YES).

   RUN CreateJoinLd("Условия договора",
                    "browseld", 
                    "rent-cond",
                    "contract,cont-code",
                    loan.contract + "," + loan.cont-code,
                    "",
                    STRING(Level + 1) ,
                    YES).
   
   RUN CreateJoin("Доверенные лица",
                  "l_cust`" + loan.contract + "," + loan.cont-code + ", 1,"
                  + STRING(Level + 1),
                  YES).
END.
ELSE
DO:

   RUN CreateJoinLd("Услуги",
                    "browseld",
                    "term-obl-count",
                    "contract,cont-code",
                    loan.contract + "," + loan.cont-code,
                    "",
                    STRING(level + 1),                 
                    YES
                  ).

   FOR EACH xlink WHERE 
            xlink.class-code EQ 'loan-count' NO-LOCK,
       EACH links WHERE  
            links.link-id = xlink.link-id 
        AND links.source-id = loan.contract + "," + loan.cont-code
        AND links.beg-date LE end-date
        AND (links.end-date GE end-date OR links.end-date EQ ?)
            NO-LOCK,
       FIRST op WHERE op.op EQ INT(links.target-id)
            NO-LOCK:
   
      CREATE TmpObj.
      TmpObj.rid = RECID(op).
   
   END.

   mTmpObjHand = TEMP-TABLE TmpObj:HANDLE. 

   RUN CreateJoinLd("Документы",
                    "browseld",
                    "opb",
                    "UseTmpObjInQuery",
                    STRING(mTmpObjHand),
                    "",
                    STRING(Level + 1),
                    YES).
END.

RUN CreateJoinLd("Счета-фактуры",
                 "browseld",
                 "axd-sf",
                 /* список имен предустанавливаемых полей */
                 "parent-contract"   + CHR(1) +
                 "parent-cont-code"  + CHR(1) +
                 "class-code"        + CHR(1) +
                 "contract"          + CHR(1) +
                 "TITLE"             + CHR(1) +
                 "SetFirstFrm"       + CHR(1) +
                 "mLinks-code"       + CHR(1) +
                 "mLinks-surr",

                 /* список предустанавливаемых значений полей */
                 loan.contract       + CHR(1) +
                 loan.cont-code      + CHR(1) +
                 "axd-sf"            + CHR(1) +
                 "sf-out"            + CHR(1) +
                 "ЖУРНАЛ ВЫСТАВЛЕННЫХ СЧЕТОВ-ФАКТУР" + "(~"" + loan.cont-code + "~")" + CHR(1) +
                 "2"                 + CHR(1) + 
                 "sf-loan-rent"      + CHR(1) +
                 loan.contract + ";" + loan.cont-code,

                 /* список имен блокируемых полей */
                 "parent-contract"   + CHR(1) +
                 "parent-cont-code"  + CHR(1) +
                 "class-code"        + CHR(1) +
                 "contract"          + CHR(1) + 
                 "mLinks-code"       + CHR(1) + 
                 "mLinks-surr",
                 STRING(Level + 1),
                 YES).

/* создаем подпункт меню Дополнительные реквизиты*/
RUN CreateJoin("Дополнительные реквизиты",  
               "loansign`"
               + loan.contract + ","
               + loan.cont-code + ",1,1,"
               + STRING(Level + 1),
                YES).

/* создаем подпункт меню Дополнительные связи*/
RUN CreateJoin("Дополнительные связи",
                "xlink-ed`"
                + loan.class-code + ","
                + loan.contract + ";"
                + loan.cont-code + ","
                + STRING(Level + 1),
                YES).
                
RUN CreateJoin("Журнал изменений",  "hi(loan`" +
               loan.contract + "," +
               loan.cont-code + "," +
               "1,1," +                 
               STRING(Level + 1) ,
               YES).                

IF iclass NE "loan-count" THEN
DO:

   FIND FIRST bloan WHERE bloan.parent-contract  = mContract
                      AND bloan.parent-cont-code = mContCode
                      AND bloan.class-code       = "loan-rent-dop" 
   NO-LOCK NO-ERROR.
   IF AVAIL(bloan) THEN
   RUN CreateJoinLd("Дополнительные соглашения",
                    "browseld", 
                    "rent-cond-dop",
                    "contract,cont-code",
                    bloan.contract + "," + bloan.cont-code,
                    "",
                    STRING(Level + 1) ,
                    YES).      
END.
                
RUN SetSysConf IN h_base ("safeContCode", loan.cont-code).

/*инклюд формирования Join меню*/
{procjoin.i
   &Prefix = "loan"
   &frametitle = "'[ ДОГОВОР ' + loan.doc-ref + ' ]'"  
}

RUN SetSysConf IN h_base ("safeContCode", "").

/* Удаление созданных в sysconf записей  */
RUN DeleteOldDataProtocol IN h_base ("SFContract").
RUN DeleteOldDataProtocol IN h_base ("SFContCode"). 

{intrface.del}

RETURN "NEXT REFRESH".
/* $LINTFILE='safejoin.p' */
/* $LINTMODE='1,5,6,3' */
/* $LINTENV ='common' */
/* $LINTVSS ='$/ws3-dpl/common/bq/' */
/* $LINTUSER='soda' */
/* $LINTDATE='20/07/2017 16:38:27.833+03:00' */
/*prosign7tm2ZvlgcGa/Eju5Jswxdg*/
/* --- safejoin.p was humbly modified by (c)blodd converter v.1.11 on 8/24/2017 1:29pm --- */
