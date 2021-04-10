(rule (big-shot ?person)
      (and (job ?person (?type . ?details))
           (not 
            (and 
             (supervisor ?person ?superior) 
             (job ?superior (?type . ?other-details))))))
