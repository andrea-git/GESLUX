      * Non considera gli stati di inserito, accettato e chiuso.
      ***---
       AGGIORNA-STATO-ORDF.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "tordforn" to geslock-nome-file.

           set tutto-ok to true.
           read tordforn lock key tof-chiave invalid continue end-read.

           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              initialize geslock-messaggio
              string "Ord. fornitore anno: " tof-anno " n. " tof-numero
              x"0d0a""Risulta bloccato su un altro terminale."
              x"0d0a""Sarà impossibile aggiornarne lo stato."
              delimited size
                 into geslock-messaggio
              end-string
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read tordforn lock key tof-chiave
                        invalid continue 
                   end-read
              when ignora exit perform
              end-evaluate
           end-perform.

           if not RecLocked
              move low-value  to rof-chiave
              move tof-chiave to rof-chiave-testa
              start rordforn key >= rof-chiave
                    invalid continue
                not invalid
                    set evaso-tutto to true
                    move 0 to tot-pz-ord
                    move 0 to tot-pz-arr
                    perform until 1 = 2
                       read rordforn next at end exit perform end-read
                       if rof-chiave-testa not = tof-chiave
                          exit perform
                       end-if
                       move 0 to rof-qta-evasa
                       move low-value  to reva-rec
                       move rof-chiave to reva-chiave-ordf
                       start reva key >= reva-chiave-ordf
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read reva next no lock
                                     at end exit perform
                                end-read
                                if reva-chiave-ordf not = rof-chiave
                                   exit perform
                                end-if
                                add reva-qta to rof-qta-evasa
                             end-perform
                       end-start
                       rewrite rof-rec
                       if rof-qta-ord > rof-qta-evasa
                          set evaso-tutto to false
                       end-if
                       add rof-qta-ord   to tot-pz-ord
                       add rof-qta-evasa to tot-pz-arr
                    end-perform
                    move tot-pz-ord to tof-pz-tot
                    move tot-pz-arr to tof-pz-arrivati

                    |STATO ORDINE
                    if tof-inserito or tof-chiusura-man |or tof-accettato
                       continue
                    else
                       if tof-pz-arrivati not = 0
                          if evaso-tutto
                             if tof-da-confermare-no
                                set tof-chiuso        to true
                                set tof-chiusura-auto to true
                              accept tof-data-chiusura from century-date
                                accept tof-ora-chiusura  from time
                                move user-codi to tof-utente-chiusura
                             end-if
                          else
                             set tof-in-lavorazione to true
                          end-if
                       else
                          set tof-inviato to true
                       end-if

                       |STATO EVASIONE
                       if tof-pz-arrivati = 0
                          set tof-inevaso to true
                       else
                          if not evaso-tutto
                             set tof-evas-parz to true
                          else
                             set tof-evas-tot to true
                          end-if
                       end-if

                       rewrite tof-rec invalid continue end-rewrite
                    end-if

              end-start         
              unlock tordforn all records
           end-if.
