/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2016 ЗАО "Банковские информационные системы"
     Filename: RUN-ONCE.P
      Comment: Единовременный запуск задачи из планировщика
   Parameters: recid(schedule)
         Uses:
      Used by:
      Created: 15/03/97 Serge
     Modified: 15/03/97 Serge
*/
Form "~n@(#) RUN-ONCE.P 1.0 Serge 15/03/97 Serge 15/03/97 Единовременный запуск задачи из планировщика"
with frame sccs-id stream-io width 250.

def input param iShedId  as ROWID no-undo.

{globals.i}
{sched_tz.i}
def var servpath as char no-undo.

servpath = os-getenv("BQ") + "/".

do transaction:
   FIND FIRST schedule WHERE ROWID(schedule) EQ iShedId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF AVAIL schedule THEN
   assign
      schedule.prev-date = GetDateTz()
      schedule.prev-time = GetTimeTz().
end.

FIND FIRST schedule WHERE ROWID(schedule) EQ iShedId NO-LOCK NO-ERROR.

  pause 0 before-hide.

/*  SETUSERID("SERV0000", "123", "BISQUIT").*/
  run SetUser in h_base.                                                                           
  assign                                                                                           
     usr-printer = schedule.printer                                                                
     beg-date    = GetDateTz()
     end-date    = GetDateTz()
     gbeg-date   = GetDateTz()
     gend-date   = GetDateTz()
  .                                                                                                
                                                                                                   
  put unformatted                                                                                  
     "ПЛАНИРОВЩИК: " string(GetDateTz(),"99/99/9999") " " string(GetTimeTz(),"hh:mm:ss")
     " Запуск задачи " schedule.schedule-name                                                      
     if schedule.procedure begins "!!" then                                                        
        ", тихая команда " + substr(schedule.procedure,3) + " " + schedule.inputparam              
     else if schedule.procedure begins "!" then                                                    
        ", команда " + substr(schedule.procedure,2) + " " + schedule.inputparam                    
     else                                                                                          
        ", процедура " + schedule.procedure + " (" + schedule.inputparam + ")"                     
     skip.                                                                                         
                                                                                                   
  if schedule.procedure begins "!!" then                                                           
     os-command silent value(substr(schedule.procedure,3)) value(schedule.inputparam).             
  else if schedule.procedure begins "!" then                                                       
     os-command value(substr(schedule.procedure,2)) value(inputparam).                             
  else do:                                                                                         
     if can-find(first norm where norm.norm = schedule.procedure) then                             
        run norm-batch.p ("", schedule.procedure, dept.Branch, GetDateTz(), GetDateTz()).
     else do:                                                                                      
        if search(schedule.procedure + ".r") <> ? or search(schedule.procedure + ".p") <> ? then   
          if schedule.inputparam ne "" then                                                        
            run value(schedule.procedure + ".p") (schedule.inputparam).                            
          else                                                                                     
            run value(schedule.procedure + ".p").                                                  
        else                                                                                       
          message "Не могу запустить" schedule.procedure " - процедура не найдена или отчет пуст!".
     end.                                                                                          
  end.                                                                                             
                                                                                                   
  put unformatted                                                                                  
     "ПЛАНИРОВЩИК: " string(GetDateTz(),"99/99/9999") " " string(GetTimeTz(),"hh:mm:ss")
     " Окончание задачи " schedule.schedule-name "." skip (1).                                     
/* $LINTFILE='run-once.p' */
/* $LINTMODE='1' */
/* $LINTENV ='dpl' */
/* $LINTVSS ='$/ws3-dpl/bq' */
/* $LINTUSER='soav' */
/* $LINTDATE='10/06/2016 12:36:22.818+03:00' */
/*prosignLHMrthSuDD305/2NdBPfog*/