      ***---
      * C$SLEEP ha problemi su thin-client e ritorna il controllo 
      * al menu perciò viene sostituita da questa routine
       WAIT-3-SECS.
           set ScartaSecondi to false.
           accept como-time from time.
           evaluate como-ss
           when 57    move 0 to como-ss-end
                      set ScartaSecondi to true
           when 58    move 1 to como-ss-end
                      set ScartaSecondi to true
           when 59    move 2 to como-ss-end
                      set ScartaSecondi to true
           when 60    move 3 to como-ss-end
                      set ScartaSecondi to true
           when other add  3 to como-ss giving como-ss-end
           end-evaluate.
           perform until 1 = 2
              accept como-time from time
              if como-ss >= como-ss-end
                 if not ScartaSecondi
                    exit perform
                 else
                    |Altrimenti se i secondi iniziali sono 57
                    |al 58(> di 0) uscirebbe subito senza attendere
                    if como-ss not = 57 and
                       como-ss not = 58 and
                       como-ss not = 59 and
                       como-ss not = 60
                       exit perform
                    end-if
                 end-if
              end-if
           end-perform.

