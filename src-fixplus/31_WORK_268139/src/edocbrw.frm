FORM
   indocs.create-date
      COLUMN-LABEL "!."
      HELP " โ  คฎก ขซฅญจ๏ "
      FORMAT "99/99/9999"
   indocs.exp-date
      COLUMN-LABEL "!"
      HELP "เฎช ๅเ ญฅญจ๏ "
      FORMAT "99/99/9999"
   mName
      FORMAT "x(38)"
      COLUMN-LABEL ""
      HELP " จฌฅญฎข ญจฅ คฎชใฌฅญโ "
      SPACE(0)
   mMark
      FORMAT ">/"
      COLUMN-LABEL " "
      HELP "ฅเ เๅจ๏"  
      NO-TAB-STOP 
   indocs.doc-cat
      FORMAT "x(8)"
      COLUMN-LABEL "!"
      HELP "ฎค ฏฎ "
   indocs.doc-type
      COLUMN-LABEL "!"
      FORMAT "x(5)"
      HELP "จฏ คฎชใฌฅญโ "
WITH FRAME browse1 TITLE COLOR bright-white 
    "[   " + mFrmLabel + "]".

FORM
   indocs.create-date
      COLUMN-LABEL "!."
      HELP " โ  คฎก ขซฅญจ๏ "
      FORMAT "99/99/9999"
   indocs.exp-date
      COLUMN-LABEL "!"
      HELP "เฎช ๅเ ญฅญจ๏ "
      FORMAT "99/99/9999"
   mName
      FORMAT "x(20)"
      COLUMN-LABEL ""
      HELP " จฌฅญฎข ญจฅ คฎชใฌฅญโ "
      SPACE(0)
   mMark
      FORMAT ">/"    
      COLUMN-LABEL " "
      HELP "ฅเ เๅจ๏"  
      NO-TAB-STOP 
   indocs.doc-cat
      FORMAT "x(8)"
      COLUMN-LABEL "!"
      HELP "ฎค ฏฎ "
   indocs.doc-type
      COLUMN-LABEL "!"
      FORMAT "x(5)"
      HELP "จฏ คฎชใฌฅญโ "
   mClassname
      COLUMN-LABEL "!"
      FORMAT "x(17)"
      HELP "เจข๏งช  คฎชใฌฅญโ  ช โ กซจๆฅ"      
WITH FRAME browse2 TITLE COLOR bright-white 
    "[   ]".
/*prosignbR6f617lnb701RDJNQPkfA*/