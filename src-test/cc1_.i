{&Ofd}
   DEF VAR cur-dd AS DATE.
   DEF VAR isum1  LIKE isum.
   DEF VAR fl-pr  AS LOGICAL INIT NO NO-UNDO .
{comment} */

&IF DEFINED(loan-cond) = 0
&THEN 
 &GLOB loan-cond loan-cond
&ENDIF
ASSIGN
   fl      = YES
   bal-dob = 0
   d1      = {&d-beg}
   .

FIND LAST {&rate} WHERE
          {&rate}.commi  = lrate[{&lr} + {&cd-p}]
      AND {&rate}.since <= d1
   NO-LOCK NO-ERROR.

IF NOT AVAIL {&rate} THEN
   FIND FIRST {&rate} WHERE
              {&rate}.commi  = lrate[{&lr} + {&cd-p}]
          AND {&rate}.since >= d1
          AND {&rate}.since <= {&d-end}
      NO-LOCK NO-ERROR.

FIND LAST {&sum} {&where1}
          {&sum}.{&since} <= d1
   NO-LOCK NO-ERROR.
IF NOT AVAIL {&sum} THEN
   FIND FIRST {&sum} {&where1}
              {&sum}.{&since} >= d1
          AND {&sum}.{&since} <= {&d-end}
      NO-LOCK NO-ERROR.

IF AVAIL {&sum}  AND
   AVAIL {&rate} THEN
DO:

   ASSIGN
      d1     = IF AVAIL {&rate}            AND
                  {&rate}.since > {&d-beg} THEN
                  {&rate}.since
               ELSE
                  {&d-beg}
      d1     = IF AVAIL {&sum}         AND
                  {&sum}.{&since} > d1 THEN
                  {&sum}.{&since}
               ELSE
                  d1
      comm-t = {&rate}.rate
      fix-t  = {&rate}.fix
      bal-t  = {&sum}.{&balance}
      .

   IF ({&loan-cond}.disch-type >= 20 AND
       {&loan-cond}.disch-type <  30) OR
      CAN-DO('32,33,34',string({&loan-cond}.disch-type)) THEN
   DO:
      FIND LAST {&sum1} {&where2}
                {&sum1}.{&since1} <= d1
         NO-LOCK NO-ERROR.

      ASSIGN
          bal-dob  = IF AVAIL {&sum1} THEN
                        {&sum1}.{&balance1}
                     ELSE
                        0
          dat      = IF AVAIL {&sum1} THEN
                        {&sum1}.{&since1}
                     ELSE
                        loan.since
          .

      {&Ofterm}
      IF loan.since <= d1 THEN
         FOR EACH xterm-obl WHERE
                  xterm-obl.contract  = loan.contract
              AND xterm-obl.cont-code = loan.cont-code
              AND xterm-obl.idnt      = 1
              AND xterm-obl.end-date >= dat
              AND xterm-obl.end-date <  term-obl.end-date
            NO-LOCK:
            bal-dob = bal-dob + xterm-obl.amt .
         END.
      {comment} */
   END.
   ELSE
      bal-dob = 0.

   DO WHILE d1 < {&d-end}  {&qq1}:

      fl-pr = NO.

      FORM bal-t
         d1     COLUMN-LABEL "С"
         d1_    COLUMN-LABEL "ПО"
         comm-t COLUMN-LABEL "СТАВКА" FORM ">,>>9.99"
         ndays  COLUMN-LABEL "ДНЕЙ"   FORM ">,>>9"
         ssum   COLUMN-LABEL "СУММА ПРОЦЕНТОВ" {&qq1}
         .

      FIND FIRST {&rate} WHERE
                 {&rate}.commi  = lrate[{&lr} + {&cd-p}]
             AND {&rate}.since <= {&d-end}
             AND {&rate}.since >  d1
             AND {&rate}.rate  <> comm-t
         NO-LOCK NO-ERROR.
      FIND FIRST {&sum} {&where1}
                 {&sum}.{&since}   >  d1
             AND {&sum}.{&since}   <= {&d-end}
             AND {&sum}.{&balance} <> bal-t
         NO-LOCK NO-ERROR.

      ASSIGN
         d1_ = IF AVAIL {&rate} THEN
                  {&rate}.since
               ELSE
                  {&d-end}
         d1_ = IF AVAIL {&sum}          AND
                  {&sum}.{&since} < d1_ THEN
                  {&sum}.{&since}
               ELSE
                  d1_
         .

      IF d1_ > d1 THEN
      DO:
            /* Производим поиск плановой даты обязательства %% 
            ** нужно для обработки фиксированных ставок fix-t = true */
         FIND FIRST yterm-obl WHERE
                     yterm-obl.contract  EQ loan.contract
            AND      yterm-obl.cont-code EQ loan.cont-code
            AND      yterm-obl.idnt      EQ 1
            AND      yterm-obl.end-date  GE d1
            AND      yterm-obl.end-date  LT d1_
         NO-LOCK NO-ERROR.

         FIND FIRST {&sum1} {&where2}
                    {&sum1}.{&since1} > d1
                AND {&sum1}.{&since1} < d1_
            NO-LOCK NO-ERROR.
         d-t = d1 .
         IF AVAIL {&sum1} THEN
         DO:
            ASSIGN
               d-t    = {&sum1}.{&since1}
               cur-dd = d1
               isum1  = 0
               .
            DO WHILE cur-dd < d1_ :
               ASSIGN
                   cur   = cur-dd
                   nldt  = d-t
                   nldt2 = nldt
                   ndays = 0
                   .

               {ndays.ii ndays}
               n1 = IF {&cond}.disch-type = 0  OR
                       {&cond}.disch-type = 10 OR
                       {&cond}.disch-type = 12 OR
                       {&cond}.disch-type = 14 OR
                       {&cond}.disch-type = 15 OR
                       {&cond}.disch-type = 16 OR
                       {&cond}.disch-type = 2  OR
                       ({&cond}.disch-type GE 19  AND
                        {&cond}.disch-type LT 30) THEN
                       36000
                    ELSE IF {&cond}.disch-type = 40 THEN
                       36500
                    ELSE
                       {&ch_day} * 100.

                /*Ставки "Пеня-К, Пеня%К" для форм расчета 35,36 задаются в годовых, 
                 для других форм расчета значение пени нужно делить на 100 */
               IF ({&lr} + {&cd-p} EQ 3 
                   OR {&lr} + {&cd-p} EQ 5)
                AND {&cond}.disch-type NE 35 
                AND {&cond}.disch-type NE 36 THEN
                  n1 = 100.

               IF bal-t + bal-dob EQ 0 
               THEN
                  ssum = 0.
               ELSE DO:
                  IF fix-t 
                  THEN DO:
                     ssum =  IF AVAIL yterm-obl 
                             THEN comm-t 
                             ELSE 0.
                  END.
                  ELSE DO:
                     CASE mFormRasch:
                        WHEN "1" THEN
                           ssum =  (bal-t + bal-dob) * ndays * comm-t / n1.
                        WHEN "2" THEN
                        /* т.к. n1 уже приведенное число дней в году в зависимости от формы
                        ** расчета, то берем это число и делим на 100 (проблем округления не будет) */
                           ssum =  ((bal-t + bal-dob) * comm-t / 100) * ndays / ( n1 / 100 ).
                     END CASE.
                  END.
               END.
                  /* запуск метода начисления по схеме индивидуальной комиссии NACHCNST */
               RUN cc1_ss1.p (loan.contract,
                              loan.cont-code,
                              d1,
                              d1_ - 1,
                              lrate[{&lr} + {&cd-p}],
                              bal-t + bal-dob,
                              INPUT-OUTPUT ssum).

               IF (({&loan-cond}.disch-type GE 20 AND
                    {&loan-cond}.disch-type LT 30) OR
                   CAN-DO('33,34,32',STRING({&loan-cond}.disch-type))) THEN
               DO:
                  {{&fll}}
                  {&Nodisp}
                  {dispp.i d-t}
                  fl-pr = yes .
                  {comment} */
               END.
               ASSIGN
                  isum1  = isum1 + ROUND(ssum,2)
                  cur-dd = d-t
                  .
               IF (({&loan-cond}.disch-type GE 20 AND
                    {&loan-cond}.disch-type lt 30) OR
                  CAN-DO('33,34,32',string({&loan-cond}.disch-type))) AND
                  AVAIL {&sum1}
               THEN
                  bal-dob = {&sum1}.{&balance1} .
               /* else bal-dob = 0. */
               FIND FIRST {&sum1} {&where2}
                          {&sum1}.{&since1} > cur-dd
                      AND {&sum1}.{&since1} < d1_
                  NO-LOCK NO-ERROR.
               d-t = IF NOT AVAIL {&sum1} /*or {&sum1}.{&since1} ge d1_*/ THEN
                        d1_
                     ELSE
                        {&sum1}.{&since1}.
            END. /*DO WHILE cur-dd < d1_ :*/

            IF  fl-pr THEN
                d1 = cur-dd.
            ASSIGN
               ssum  = isum1
               isum  = isum + ssum
               cur   = d1
               nldt  = d1_
               nldt2 = nldt
               ndays = 0
               .
            {ndays.ii ndays}
         END. /*IF AVAIL {&sum1} THEN*/
         ELSE
         DO:
            ASSIGN
               cur   = d1
               nldt  = d1_
               nldt2 = nldt
               ndays = 0
               .
            {ndays.ii ndays}
            n1 = IF {&cond}.disch-type = 0  OR
                    {&cond}.disch-type = 10 OR
                    {&cond}.disch-type = 12 OR
                    {&cond}.disch-type = 14 OR
                    {&cond}.disch-type = 15 OR
                    {&cond}.disch-type = 16 OR
                    {&cond}.disch-type = 2  OR
                    ({&cond}.disch-type GE 19  AND
                     {&cond}.disch-type LT 30) THEN
                    36000
                 ELSE IF {&cond}.disch-type = 40 THEN
                    36500
                 ELSE 
                    {&ch_day} * 100.

           /*Ставки "Пеня-К, Пеня%К" для форм расчета 35,36 задаются в годовых, 
                 для других форм расчета значение пени нужно делить на 100 */
            IF ({&lr} + {&cd-p} EQ 3 
                   OR {&lr} + {&cd-p} EQ 5)
                AND {&cond}.disch-type NE 35 
                AND {&cond}.disch-type NE 36 THEN
               n1 = 100.

            IF bal-t + bal-dob EQ 0 
            THEN
               ssum = 0.
            ELSE DO:
               IF fix-t 
               THEN DO:
                  ssum =  IF AVAIL yterm-obl
                          THEN comm-t 
                          ELSE 0.
               END.
               ELSE DO:
                  CASE mFormRasch:
                     WHEN "1" THEN
                        ssum = (bal-t + bal-dob) * ndays * comm-t / n1.
                     WHEN "2" THEN
                        /* т.к. n1 уже приведенное число дней в году в зависимости от формы
                        ** расчета, то берем это число и делим на 100 (проблем округления не будет) */
                        ssum = ((bal-t + bal-dob) * comm-t / 100) * ndays / ( n1 / 100 ) .
                  END CASE.
               END.
            END.
               /* запуск метода начисления по схеме индивидуальной комиссии NACHCNST */
            RUN cc1_ss1.p (loan.contract,
                           loan.cont-code,
                           d1,
                           d1_ - 1,
                           lrate[{&lr} + {&cd-p}],
                           bal-t + bal-dob,
                           INPUT-OUTPUT ssum).

            IF (({&loan-cond}.disch-type GE 20  AND
                 {&loan-cond}.disch-type LT 30) OR
               CAN-DO('33,34,32',STRING({&loan-cond}.disch-type))) AND
               AVAIL {&sum1}            then
            DO :
               {&Nodisp}
               {{&fll}}
               {dispp.i d1_}
               fl-pr = yes .
               {comment} */
            END.
            isum = isum + round(ssum,2) .
         END.
      END. /*IF d1_ > d1 THEN*/
      IF NOT fl-pr THEN
      DO:
         {{&fll}}
         {{&disp}{&*}}
      END.

      d1 = d1_ .

      IF AVAIL {&rate}      AND
         {&rate}.since = d1 THEN
         ASSIGN
            comm-t = {&rate}.rate
            fix-t  = {&rate}.fix
         .

      IF AVAIL {&sum}         AND
         {&sum}.{&since} = d1 THEN
         bal-t = {&sum}.{&balance}.

      ssum = 0.
   END. /*DO WHILE d1 < {&d-end} {&qq1}*/
   ASSIGN
      all-sum = all-sum + isum
      isum    = 0
      .
END.
ELSE
   d1 = {&d-end} .

fl = YES.

RELEASE {&sum1} .
