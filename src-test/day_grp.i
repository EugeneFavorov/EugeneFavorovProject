/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: U:\GUVA\TITUL\DAY_GRP.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 27.08.2012 10:15 gva     
     Modified: 27.08.2012 14:40 gva      
     Modified: 
*/

CREATE tt-day-itog.
ASSIGN 
    tt-day-itog.city-id = tt-op-day.city-id
    tt-day-itog.city = tt-op-day.city
    tt-day-itog.kko = tt-op-day.kko
    tt-day-itog.currency = tt-op-day.currency
    tt-day-itog.acct-cat = tt-op-day.acct-cat
    tt-day-itog.razdel = tt-op-day.razdel
    tt-day-itog.save-type = tt-op-day.save-type
    tt-day-itog.itog = mSumm
    .
