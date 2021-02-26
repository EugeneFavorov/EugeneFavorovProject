output to "his.txt".
{globals.i}
{pb_exf_exl.i}          /** ������⥪� ��� ���㧪� � XL */

def var i        as int  NO-UNDO.
def var i2       as int  NO-UNDO.
def var mUserID  as char NO-UNDO.
def var mFIO     as char NO-UNDO.
def var mTel     as char NO-UNDO.
def var mStatus  as char NO-UNDO.
def var mClient  as char NO-UNDO.
def var mAcct-db as char NO-UNDO.
def var mDoc-Num as char NO-UNDO.
def var mAmt-rub as char NO-UNDO.
def var mDate    as char NO-UNDO.
def var mGroup   as char NO-UNDO.
DEFINE VARIABLE cFl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXL   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttDoc
   FIELD mbranch      AS CHARACTER
   FIELD mUserID      AS CHARACTER
   FIELD mFIO         AS CHARACTER
   FIELD mTel         AS CHARACTER
   FIELD mStatus      AS CHARACTER
   FIELD mClient      AS CHARACTER
   FIELD mAcct-db     AS CHARACTER
   FIELD mDoc-Num     AS CHARACTER
   FIELD mAmt-rub     AS CHARACTER
   FIELD mDate        AS CHARACTER
   FIELD op           AS CHARACTER
   FIELD mStatusDoc   AS CHARACTER
   FIELD mGroup       AS CHARACTER
   INDEX ind1 mbranch mStatusDoc .




{getdates.i}

message beg-date end-date view-as alert-box.

for each op where op.due-date >= beg-date
              and op.due-date <= end-date
              and op.filial-id = shfilial
              no-lock.
   find FIRST op-entry OF op NO-LOCK no-error.
   mUserID = "".
   mFIO    = "".
   mTel    = "".
   mStatus = "".
   FOR each history WHERE (history.modify NE "RUN" AND history.modify NE "PRINT")
                       AND history.field-ref EQ string(op.op)                              
                       AND history.file-name EQ 'op' 
                       no-lock.
       if CAN-DO(history.field-value,"*����_���") then do:
          mUserID = history.user-id.
       end.
       if CAN-DO(history.field-value,"*����_⥫�䮭") then do:
          mUserID = history.user-id.
       end.
       if CAN-DO(history.field-value,"op-status") then do:
          do i = 1 to num-entries(history.field-value):
             if entry(i,history.field-value) = "op-status" then do:
                if entry(i + 1,history.field-value) = "����" then do:
                   mStatus = entry(i + 1,history.field-value).
                   if  mUserID = "" then  mUserID = history.user-id.
                end.
             end.
          end.
       end.
   end.
   if mStatus = "����" then do:
      mTel = GetXAttrValueEx("op",STRING(op.op), "����_⥫�䮭","").
      mFIO = GetXAttrValueEx("op",STRING(op.op), "����_���","").
      mFIO = GetXAttrValueEx("op",STRING(op.op), "����_���","").
      find first acct where acct.acct = op-entry.acct-db.
      mGroup= GetXAttrValueEx("acct",STRING(acct.acct + "," + acct.curr), "groupOABS","").
      FIND FIRST _user WHERE _user._Userid EQ mUserID NO-LOCK NO-ERROR.
      create ttDoc.
      assign
         ttDOc.mbranch     =  shfilial 
         ttDOc.mClient     =  acct.Details
         ttDOc.mAcct-db    =  acct.acct
         ttDOc.mDoc-Num    =  op.doc-num
         ttDOc.mAmt-rub    =  string(op-entry.amt-rub)
         ttDOc.mTel        =  mTel 
         ttDOc.mFIO        =  mFIO 
         ttDOc.mUserID     =  STRING(_user._User-Name)
         ttDOc.mStatus     =  mStatus
         ttDOc.mDate       =  if string(op.op-date) <> ? then string(op.op-date) else " "
         ttDOc.op          =  string(op.op)
         ttDOc.mStatusDoc  =  op.op-status
         ttDOc.mGroup      =  mGroup
      .
      export ttDOc.
   end.
end.  
output close.

   cFl = "./vklb.xml".
   OUTPUT TO VALUE(cFl).

   PUT UNFORMATTED XLHead("tmp", "CCCCNCCCCCC", "70,250,160,90,80,130,130,120,70,100,80").
   
   cXL = XLCellHead("� �/�",0,0,0)
       + XLCellHead("������������ ������",0,0,0)
       + XLCellHead("� ���⭮�� ���",0,0,0)
       + XLCellHead("� ���⥦���� ���㬥��",0,0,0)
       + XLCellHead("�㬬�",0,0,0)          	
       + XLCellHead("����: �.�.� ������ ᮣ�. ���⥦",0,0,0)
       + XLCellHead("����: � ���⠪⭮�� ⥫�䮭�",0,0,0)
       + XLCellHead("�.�.�. ��� ��������襣� ४������ ������� (6,7)",0,0,0)
       + XLCellHead("��� ᯨᠭ�� �।�� (���㬥�� � ����� <���>)",0,0,0)
       + XLCellHead("�� ���㬥��",0,0,0)
       + XLCellHead("����� ���㬥��",0,0,0)
       + XLCellHead("��㯯� ᮯ஢�������",0,0,0)
       .
   PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
   i2 = 1.
   FOR EACH ttDoc NO-LOCK.
      cXL = XLCell(STRING(i2))                 
          + XLCell(STRING(ttDoc.mClient))          
          + XLCell(STRING(entry(1,ttDoc.mAcct-db,"@")))          
          + XLCell(STRING(ttDoc.mDoc-Num))          
          + XLNumCell(dec(ttDoc.mAmt-rub))           
          + XLCell(STRING(ttDOc.mFIO))                                   
          + XLCell("`" + STRING(ttDoc.mTel))                                   
          + XLCell(STRING(ttDOc.mUserID))                                        
          + XLCell("`" + STRING(ttDOc.mDate))                         
          + XLCell("`" + STRING(ttDOc.op))                         
          + XLCell(STRING(ttDOc.mStatusDoc))                         
          + XLCell(STRING(ttDOc.mGroup))                         
          .
      PUT UNFORMATTED XLRow(0) cXL XLRowEnd().
      i2 = i2 + 1.
   END.
   PUT UNFORMATTED XLEnd().
   OUTPUT CLOSE.

RUN sndbispc ("file=" + cFl + ";class=bq").















    