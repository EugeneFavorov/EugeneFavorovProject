/*
               Банковская интегрированная система БИСквит
    Copyright: (C) MCMXCII-MCMXCVIII ТОО "Банковские информационные системы"
     Filename: Us1-Inf.P
      Comment: Отчет по правам пользователя
   Parameters: -
         Uses: Globals.I WordWrap.Def WordWrap.I SetDest.I PreView.I
         Uses: ChkPage WClass.I
      Used by: BQ
      Created: ??/??/?? ??:?? ????
     Modified: 19/08/98 12:07 Dima - bugfixing
     Modified: 15/11/12 12:30 soav (0159039)
*/
Form "~n@(#) Us1-Inf.P 1.0 ??? ??/??/?? Dima 19/08/98"
     with frame sccs-id stream-io width 250.
&GLOB count1 50

def var in-userid /*like _user._userid initial ""*/ AS CHARACTER NO-UNDO.
def var User-Match as character.
def var strtp as character form "x(20)".
def var strpsw as character form "x(20)".
def var depth as INT64 no-undo.
def var is-cdt as log init yes no-undo.
def var is-mod as log init yes no-undo.
def var is-dat as log init yes no-undo.
def var is-trn as log init yes no-undo.
def var is-prc as log init yes no-undo.
def var is-dcl as log init yes no-undo.
def var is-cls as log init yes no-undo.
def buffer dcode for code.
def buffer bclass for class.
def buffer buf-op-kind for op-kind .
def buffer buf1-op-kind for op-kind .
def buffer buf2-op-kind for op-kind .
def buffer buf-user-proc for user-proc.
def buffer buf1-user-proc for user-proc.
def buffer buf2-user-proc for user-proc.
def var usermisc3 as char format "x(35)" extent 9 no-undo.
def var usermisc6 as char format "x(35)" extent 9 no-undo.
def var rightview as char no-undo.
def var rightass  as char no-undo.
def var rightundo as char no-undo.
def var rightann  as char no-undo.
{globals.i}
{wclass.i}
{wordwrap.def}
{intrface.get rights}
{ttretval.def}

/* 
проверка доступа к классам op-kind и user-proc 
необходима, т.к. GetSurrPermissionUserBasic в pp-right 
отрабатывает некорректно для этих классов
GetUsrPerm ("user-proc", STRING(user-proc.public-number), "run", user-match)   
*/
function GetUsrPerm returns log(
   input iclass      as char,          /* код класса */
   input isurrogate  as char,          /* суррогат */
   input imethod-id  as char,          /* код метода доступа */
   input iuser-id    as char           /* код пользователя */
   ):

   def buffer bperm for perm.

   /* Если передается неизвестный метод, то прав не даем. */
   if lookup (imethod-id, "r,c,w,d,run,db,cr,dbl_stat,cfoWrite") = 0 then return no.

   case iclass:
   when "user-proc" 
   or when "op-kind" 
   then do:
     find first bperm where 
       bperm.class-code    = iclass
       and bperm.surrogate = isurrogate
/*       and bperm.cond-type = "s"*/
       and bperm.method-id = imethod-id
       and bperm.user-id   = iuser-id
       and bperm.allow     = yes
       no-lock no-error.
       return avail bperm.
   end.
   otherwise return GetSurrPermissionUser(iclass, isurrogate, imethod-id, iuser-id).

   end case.
   return no.
end.

if not IsUserAdm(userid("bisquit")) and (userid("bisquit") <> "U0400MAY") and (userid("bisquit") <> "0000GAI") then do:
   IF(GetXAttrValueEx("_user",userid("bisquit"), "ПросмПрав","НЕТ") NE "ДА") THEN DO:
      message "Вы не имеете пpава доступа к этой инфоpмации".
      return.
   END.
end.

/* message "Введите код пользователя: " update in-userid. */
FORM
   in-userid view-as fill-in    size 30 by 1 label "    Пользователи" format "x(4000)" skip
   is-cdt    view-as toggle-box              label " Общие данные"                  skip
   is-mod    view-as toggle-box              label " Доступ к модулям"              skip
   is-dat    view-as toggle-box              label " Доступ к данным"               skip
   is-trn    view-as toggle-box              label " Доступ к транзакциям"          skip
   is-prc    view-as toggle-box              label " Доступ к процедурам"           skip
   is-dcl    view-as toggle-box              label " Доступ к классам данных"       skip
   is-cls    view-as toggle-box              label " Доступ к классам метасхемы"    skip

WITH FRAME fParam
OVERLAY CENTERED SIDE-LABELS
TITLE "[ ПАРАМЕТРЫ ОТЧЕТА ]" ROW 5.

ON F1 OF in-userid IN FRAME fParam
DO:
   RUN browseld.p ("_user",
                   "RetRcp"      + CHR(1) +
                   "RetFld"      + CHR(1) +
                   "RetType",
                   STRING (TEMP-TABLE ttRetVal:HANDLE) + CHR(1) +
                   "_Userid"   + CHR(1) +
                   "Multi",
                   "",
                   5).
   FOR EACH ttRetVal:
      IF LOOKUP (ttRetVal.PickValue,in-userid:SCREEN-VALUE) EQ 0 THEN
         {additem.i in-userid:SCREEN-VALUE ttRetVal.PickValue}
   END.
   RETURN NO-APPLY.
END.

PAUSE 0.
UPDATE 
  in-userid 
  is-cdt
  is-mod
  is-dat
  is-trn
  is-prc
  is-dcl
  is-cls
WITH FRAME fParam.
HIDE FRAME fParam.
IF NOT {assigned in-userid} THEN RETURN.
IF is-cdt = no and 
   is-mod = no and 
   is-dat = no and 
   is-trn = no and
   is-prc = no and
   is-dcl = no and
   is-cls = no
THEN RETURN.


{setdest.i}
def var counter as INT64                                                                no-undo.
def var T-Name  as char format "x(16)"   column-label "ТИП"                             no-undo.
def var F-Name  as char format "x(40)"   column-label "          ТАБЛИЦА/КЛАССИФИКАТОР" no-undo.
def var P-Mod   as char format "x(8)"    column-label  "МОДУЛЬ"                         no-undo.
def var P-Name  as char format "x(50)"   column-label "          НАИМЕНОВАНИЕ"          no-undo.
def var P-PART  as char format "x(8)"    column-label "РАЗДЕЛ"                          no-undo.
def var P-Proc  as char format 'x(8)'    column-label "ПРОЦЕД"                          no-undo.
def var RYes    as log  format " + / - " column-label "ДОСТУП".
def var r       as log  format " + / - " column-label "R".
def var c       as log  format " + / - " column-label "Add".
def var w       as log  format " + / - " column-label "Wri".
def var d       as log  format " + / - " column-label "Del".
def var ts      as char extent {&count1}                                                no-undo.

main:
for each _User where CAN-DO (in-userid,_user._Userid)  no-lock:
  form
   _userid column-label "КОД"
   _User-Name column-label "ФАМИЛИЯ И.О."
  header
   "лист" AT 67 STRING(PAGE-NUMBER,">>>>>9") TO 78
  with page-top.
  readkey pause 0.
  if keyfunction(lastkey) = "END-ERROR" then
    leave main.

  display _userid _User-Name.
  User-Match = _Userid.

  /* Общие данные */
  if is-cdt then do:
    put screen row screen-lines + 3 col 15 user-match + '          '.
    ASSIGN
       strtp  = IF IsUserAdm(_userid)
                   THEN "Администратор" 
                   ELSE "Пользователь"
       strpsw = IF _user._Password NE ENCODE("")
                   THEN "Установлен"
                   ELSE "Не установлен"
    .
    put skip(2) "О Б Щ И Е   Д А Н Н Ы Е :" at 10 skip.
    put         "-----------------------"   at 10 skip.
    put "Статус сотрудника       : " strtp        skip.
    put "Пароль                  : " strpsw       skip.
    put "Принтер                 : " getUserPrinter(_user._userid) skip.
  end.

  /* Доступ к модулям */
  IF is-mod THEN DO:
    put skip(2) "Д О С Т У П   К   М О Д У Л Я М :" at 10 skip.
    put         "-------------------------------"   at 10 skip.

    FOR EACH user-proc WHERE
             user-proc.parent EQ 0
    NO-LOCK:
       IF GetSurrPermissionUser ("user-proc", STRING(user-proc.public-number), "run", user-match)
       THEN DO:
          P-Name = name-proc.
          display P-Name at 7 with frame pp1 down width 80 no-labels.
       END.
    END.
    put skip.
  END.

  /* Доступ к данным */
  IF is-dat THEN DO:
    put skip(2) "Д О С Т У П   К   Д А Н Н Ы М :" at 10 skip.
    put         "-----------------------------"   at 10 skip(1).
    run XAttrAll in h_xclass('_User',output table xattrid).
    for each XAttrID where not XAttrID.Progress-Field
        no-lock,
        first XAttr where recid(XAttr) = XAttrID.XAttrID,
        first signs where
          signs.file-name = XAttr.Class-Code and
          signs.code      = XAttr.Xattr-Code and
          signs.surrogate = _user._userid
        no-lock
        by XAttrID.XAttr-Code:
      ts[1] = if xattr.indexed then signs.code-value else signs.xattr-value.
      {wordwrap.i
         &s=ts
         &n={&count1}
         &l=30
      }
      put '  ' xattr.name form 'x(45)' ': ' ts[1] form 'x(30)' skip.
      do counter = 2 to {&count1}:
        if ts[counter] <> "" then
          put ts[counter] form 'x(30)' at 50 skip.
      end.
    end.

    for each signs where signs.code = "ГрупТабл"
                     and signs.file-name = "_file" no-lock,
        first dcode where dcode.class = "ГрупТабл"       and 
                          dcode.code  = signs.code-value
                          no-lock
     break by dcode.name:
 
     if first-of(dcode.name) then do:
       if first(dcode.name) then do:
          down 1 with frame nn.
          disp
            " Таблицы" @ t-name
           with frame nn down width 80.
       end.
       down 1 with frame nn.
       disp
         "   " + caps(dcode.name) @ f-name
       with frame nn down width 80.
       down with frame nn.
     end.

     find _File where _File._file-name = signs.surrogate no-lock no-error.
      disp
        trim(_file._desc) when available _File @ F-name
        _file._file-name  when available _File and _file._desc = ? @ f-name
        can-do(_file._can-read,   user-match) when avail _File @ r
        can-do(_file._can-create, user-match) when avail _File @ c
        can-do(_file._can-write,  user-match) when avail _File @ w
        can-do(_file._can-delete, user-match) when avail _File @ d
      with frame nn down width 80.
    end.
    
    for each signs where signs.code = "ГрупТабл"                                             
                     and signs.file-name = "code" no-lock,                                   
        first dcode where dcode.class = "ГрупТабл"       and                                 
                          dcode.code  = signs.code-value                                     
                          no-lock                                                            
     break by dcode.name:                                                                    
                                                                                             
     if first-of(dcode.name) then do:                                                        
       if first(dcode.name) then do:                                                         
          down 1 with frame nn2.                                                             
          disp                                                                               
            " Классификаторы" @ t-name                                                       
           with frame nn2 down width 80.                                                     
       end.                                                                                  
       down 1 with frame nn2.                                                                
       disp                                                                                  
         "   " + caps(dcode.name) @ f-name                                                   
       with frame nn2 down width 80.                                                         
       down with frame nn2.                                                                  
     end.                                                                                    
                                                                                             
     find code where code.class = entry(1,signs.surrogate) and                               
                     code.code = entry(2,signs.surrogate) no-lock no-error.                  
     IF AVAIL code THEN                                                                      
        DISPLAY                                                                              
           code.name @ F-name                                                                
  GetSurrPermissionUser ("code",signs.surrogate, "r", _userid) @ r                           
  GetSurrPermissionUser ("code",signs.surrogate, "c", _userid) @ c                           
  GetSurrPermissionUser ("code",signs.surrogate, "w", _userid) @ w                           
  GetSurrPermissionUser ("code",signs.surrogate, "d", _userid) @ d                           
        WITH FRAME nn2 DOWN WIDTH 80.                                                        
    end.                                                                                     
    {chkpage 6} else put skip.
  end.

  /* доступ к транзакциям */
  if is-trn then do:
    put skip(2) "Д О С Т У П   К   Т Р А Н З А К Ц И Я М:" at 10 skip.                                         
    put         "---------------------------------------"  at 10 skip.                                         
    for each op-kind where                                                                             
        op-kind.parent = ""                                                                            
    no-lock break by op-kind.module by op-kind.name:                                                   
       IF GetUsrPerm("op-kind", op-kind.op-kind, "run", user-match)                        
       THEN DO:                                                                                        
          P-Name = op-kind.Name-Opkind.                                                                
          if first-of(op-kind.module) then P-Mod = op-kind.module.                                     
          display P-Name                                                                               
                  P-Mod when first-of(op-kind.module)                                                  
                  "без модуля" when first-of(op-kind.module) and P-Mod = "" @ P-Mod                    
                  with frame mm down width 80.                                                         
          down with frame mm.                                                                          
          if op-kind.proc = "" then                                                                    
          for each buf-op-kind where buf-op-kind.module = op-kind.module and                           
                      buf-op-kind.parent = op-kind.op-kind                                             
          no-lock break by buf-op-kind.name:                                                           
             IF GetUsrPerm("op-kind", buf-op-kind.op-kind, "run", user-match)              
             THEN DO:                                                                                  
                P-Name = "  " + buf-op-kind.Name-Opkind.                                               
                display P-Name with frame mm down width 80.                                            
                down with frame mm.                                                                    
                if buf-op-kind.proc = "" then                                                          
                for each buf1-op-kind where buf1-op-kind.module = op-kind.module and                   
                          buf1-op-kind.parent = buf-op-kind.op-kind                                    
                no-lock break by buf1-op-kind.name:                                                    
                   IF GetUsrPerm("op-kind", buf1-op-kind.op-kind, "run", user-match)       
                   THEN DO:                                                                            
                      P-Name = "    " + buf1-op-kind.Name-Opkind.                                      
                      display P-Name with frame mm down width 80.                                      
                      down with frame mm.                                                              
                      if buf1-op-kind.proc = "" then                                                   
                      for each buf2-op-kind where buf2-op-kind.module = op-kind.module and             
                              buf2-op-kind.parent = buf1-op-kind.op-kind                               
                      no-lock break by buf2-op-kind.name:                                              
                         IF GetUsrPerm("op-kind", buf2-op-kind.op-kind, "run", user-match) 
                         THEN DO:                                                                      
                            P-Name = "    " + buf2-op-kind.Name-Opkind.                                
                            display P-Name with frame mm down width 80.                                
                            down with frame mm.                                                        
                         END.                                                                          
                      end.                                                                             
                   END.                                                                                
                end.                                                                                   
             END.                                                                                      
          end.                                                                                         
       END.                                                                                            
    end.                                                                                               
    {chkpage 10} else put skip.                                                                     
  end.

  /* доступ к процедурам */
  if is-prc then do:
    put skip(2) "Д О С Т У П   К   П Р О Ц Е Д У Р А М:" at 10 skip.                                                 
    put         "-------------------------------------"  at 10 skip.                                                 
    form                                                                                                     
      P-Name form 'x(45)'                                                                                    
      P-Mod  form 'x(6)' column-label 'МОДУЛЬ'                                                               
      P-Part form 'x(9)' column-label 'РАЗДЕЛ'                                                               
      P-Proc form 'x(8)' column-label 'ПРОЦЕД.'                                                              
    with frame mm1 width 80.                                                                                 
    procedure ListProcLevel.                                                                                 
      def input param p like user-proc.parent no-undo. /* parent    */                                       
      def input param l as   INT64              no-undo. /* tab level */                                     
      def buffer xu for user-proc.                                                                           
      for each xu where                                                                                      
        xu.parent = p                                                                                        
      no-lock break by xu.module by xu.partition by xu.name:                                                 
         IF GetUsrPerm("user-proc", STRING(xu.public-number), "run", user-match)          
         THEN DO:                                                                                            
            disp                                                                                             
              fill('  ',l) + trim(xu.name-proc)   @ p-name                                                   
              xu.procedure when xu.procedure <> ? @ p-proc                                                   
            with frame mm1.                                                                                  
            down with frame mm1.                                                                             
            if xu.partition = '' or xu.procedure = ? then                                                    
              run ListProcLevel(xu.public-number,l + 1).                                                     
         END. 
      end.                                                                                                   
    end procedure. /* ListProcLevel */                                                                       
    for each user-proc                                                                                       
      where user-proc.parent = 0                                                                             
    no-lock break by user-proc.module by user-proc.partition by user-proc.name:                              
       IF GetUsrPerm ("user-proc", STRING(user-proc.public-number), "run", user-match)            
       THEN DO:                                                                                              
          P-Name = trim(user-proc.Name).                                                                     
          if first-of(user-proc.module)    then P-Mod  = user-proc.module.                                   
          if first-of(user-proc.partition) then P-Part = user-proc.partition.                                
          disp                                                                                               
            P-Name                                                                                           
            P-Mod  when first-of(user-proc.module)                                                           
            P-Part when first-of(user-proc.partition)                                                        
            "без модуля"  when first-of(user-proc.module)    and P-Mod  = ? @ P-Mod                          
            "без раздела" when first-of(user-proc.partition) and P-Part = ? @ P-Part                         
          with frame mm1 down width 80.                                                                      
          down 1 with frame mm1.                                                                             
          run ListProcLevel(user-proc.public-number,1).                                                      
          down with frame mm1.                                                                               
       END.                                                                                                  
    end.                                                                                                     
  /**************************************************************************/                               
    procedure DC_rights . /* для контроля прав по подклассам - taken from {_sv(cl).i} */                     
      def input param in-DataClass-Id like DataClass.DataClass-Id no-undo.                                   
      def output param r as log no-undo.                                                                     
      def output param c as log no-undo.                                                                     
      def output param w as log no-undo.                                                                     
      def output param d as log no-undo.                                                                     
      def buffer buf_file for DataClass.                                                                     
      def var r1 as log no-undo.                                                                             
      def var c1 as log no-undo.                                                                             
      def var w1 as log no-undo.                                                                             
      def var d1 as log no-undo.                                                                             
      find first buf_file where                                                                              
        buf_file.Parent-id = in-DataClass-id                                                                 
      no-lock no-error.                                                                                      
      if not avail buf_file then                                                                             
        find first buf_file where                                                                            
          buf_file.DataClass-Id = in-DataClass-Id                                                            
        no-lock no-error.                                                                                    
      assign                                                                                                 
        r = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "r", user-match)              
        c = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "c", user-match)              
        w = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "w", user-match)              
        d = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "d", user-match)              
      .                                                                                                      
      for each buf_file where buf_file.Parent-id = in-dataclass-id no-lock:                                  
        if buf_file.IsFolder then                                                                            
          run DC_rights (input buf_file.DataClass-Id, output r1, output c1, output w1, output d1).           
        else                                                                                                 
          assign                                                                                             
            r1 = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "r", user-match)         
            c1 = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "c", user-match)         
            w1 = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "w", user-match)         
            d1 = GetSurrPermissionUser ("dataclass", STRING(buf_file.dataclass-id), "d", user-match)         
          .                                                                                                  
        assign                                                                                               
          r = if r = r1 then r else ?                                                                        
          c = if c = c1 then c else ?                                                                        
          w = if w = w1 then w else ?                                                                        
          d = if d = d1 then d else ?                                                                        
        .                                                                                                    
      end.                                                                                                   
    end procedure.                                                                                           
      form                                                                                                   
        dataclass.name format "x(62)" column-label "КЛАССЫ ДАННЫХ" help "Наименование класса данных"         
        r c w d                                                                                              
      with frame dc-r down.                                                                                  
    procedure ListDataClassRights.                                                                           
      def input param p like DataClass.Parent-ID no-undo.                                                    
      def input param l as   INT64                 no-undo.                                                  
      def buffer xd for DataClass.                                                                           
      for each xd where xd.parent-id = p no-lock:                                                            
        if xd.isfolder then do:                                                                              
          run DC_Rights(xd.dataclass-id,output r,output c,output w,output d).                                
          disp                                                                                               
            fill('  ',l) + caps(xd.name) @ dataclass.name                                                    
            r c w d                                                                                          
          with frame dc-r.                                                                                   
          down with frame dc-r.                                                                              
          run ListDataClassRights(xd.dataclass-id,l + 1).                                                    
        end.                                                                                                 
        else do:                                                                                             
          assign                                                                                             
            r = GetSurrPermissionUser ("dataclass", STRING(xd.dataclass-id), "r", user-match)                
            c = GetSurrPermissionUser ("dataclass", STRING(xd.dataclass-id), "c", user-match)                
            w = GetSurrPermissionUser ("dataclass", STRING(xd.dataclass-id), "w", user-match)                
            d = GetSurrPermissionUser ("dataclass", STRING(xd.dataclass-id), "d", user-match)                
          .                                                                                                  
          disp                                                                                               
            fill('  ',l) + xd.name @ dataclass.name                                                          
            r c w d                                                                                          
          with frame dc-r.                                                                                   
          down with frame dc-r.                                                                              
        end.                                                                                                 
      end.                                                                                                   
    end procedure. /* ListDataClassRights */                                                                 
    {chkpage 6} else put skip.                                                                            
  end.

  /* доступ к классам данных */
  if is-dcl then do:
    put skip(2) "Д О С Т У П   К   К Л А С С А М   Д А Н Н Ы Х:" at 10 skip.  
    put         "---------------------------------------------"  at 10 skip.  
    run ListDataClassRights('',0).                                    
  end.

  procedure classrights:
  def var r1 as log no-undo.                                                                             
  def var c1 as log no-undo.                                                                             
  def var w1 as log no-undo.                                                                             
  def var d1 as log no-undo.                                                                             
  def buffer dcode for code.

  end.

  form                                                                                                   
    class.name format "x(62)" column-label "КЛАССЫ МЕТАСХЕМЫ" help "Наименование класса данных метасхемы"         
    r c w d                                                                                              
  with frame frmcls down.                                                                                  

  /* доступ к классам метасхемы */
  if is-cls then do:
    put skip(2) "Д О С Т У П   К   К Л А С С А М   М Е Т А С Х Е М Ы:" at 10 skip.  
    put         "---------------------------------------------------"  at 10 skip.  
    for each class where class.parent-classes = "" no-lock: 
      r = GetSurrPermissionUser ("class", class.class-code, "r", user-match).
      c = GetSurrPermissionUser ("class", class.class-code, "c", user-match).
      w = GetSurrPermissionUser ("class", class.class-code, "w", user-match).
      d = GetSurrPermissionUser ("class", class.class-code, "d", user-match).
      if not (r = no and c = no and w = no and d = no) then
      disp                                                                                               
        caps(class.name) @ class.name                                                          
        r c w d                                                                                          
      with frame frmcls.                                                                                   
      down with frame frmcls.
      RUN SubClassPerm(class.class-code, 2).
    end.
  end.
  
/**************************************************************************/
end.

PROCEDURE SubClassPerm:
   DEF INPUT PARAM iPrntClss AS CHAR NO-UNDO. /* класс родителя */
   DEF INPUT PARAM iFill     AS INT  NO-UNDO.
   DEF BUFFER bPClass   FOR class.
   DEF BUFFER bSClass   FOR class.
   DEF VAR    mClassNum AS  INT NO-UNDO.
FEbPClass:
      FOR EACH bPClass WHERE bPClass.parent-classes = iPrntClss NO-LOCK:
         r = GetSurrPermissionUser ("class", bPClass.class-code, "r", user-match).
         c = GetSurrPermissionUser ("class", bPClass.class-code, "c", user-match).
         w = GetSurrPermissionUser ("class", bPClass.class-code, "w", user-match).
         d = GetSurrPermissionUser ("class", bPClass.class-code, "d", user-match).
         IF NOT (r = NO and c = NO and w = NO and d = NO) THEN
         DISP                                                                     
           FILL(" ", iFill) + bPClass.name @ class.name 
           r c w d                                                                 
         WITH FRAME frmcls.                                                          
         DOWN WITH FRAME frmcls.
         RUN SubClassPerm(bPClass.class-code, iFill + 2).
      END.
   IF mClassNum = 0 THEN RETURN.
END PROCEDURE.

{signatur.i &user-only=yes}
{preview.i}
{intrface.del}
