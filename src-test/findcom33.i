/* &rcom  - код комиссии
   &rsum  - остаток на который начисляется комиссия
   &dir   -last,first,next */



find {&dir} {&comm-rate} where
        {&comm-rate}.commi eq {&rcom}  and
        {&comm-rate}.acct  eq acct.acct  and
        {&comm-rate}.currency  eq acct.currency  and
        {&comm-rate}.min-val <= {&rsum} and
        /* Учет периода, если он определен */
         &IF DEFINED (vPeriodInt) ne 0
         &THEN
          {&comm-rate}.period le {&vPeriodInt} and
         &ENDIF
        {&comm-rate}.since {&since1} no-lock no-error.
