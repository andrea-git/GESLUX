      ***---
       PROMO-FITTIZIA-TESTA.
           accept tpr-gdo from environment "GDO_FITTIZIA".
           move mto-data-ordine to tpr-ini-dpo.
           compute como-data = 
                   function integer-of-date (mto-data-ordine).
           add 730 to como-data.
           compute tpr-fine-dpo = function date-of-integer (como-data).
           move tpr-ini-dpo    to tpr-ini-volantino.
           move tpr-fine-dpo   to tpr-fine-volantino.
           set tpr-nazionale   to true.
           set tpr-fittizia-si to true.
           move mto-chiave     to tpr-chiave-master.

           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti no lock.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.

           string mto-anno     delimited size
                  "_"          delimited size
                  mto-numero   delimited size
                  "_"          delimited size
                  cli-ragsoc-1 delimited low-value
                  into tpr-descrizione
           end-string.
           inspect cli-ragsoc-1 replacing trailing low-value by spaces.

           accept tpr-data-creazione from century-date.
           accept tpr-ora-creazione  from time.
           move user-codi to tpr-utente-creazione.
           
           move 999999 to tpr-codice.
           perform until 1 = 2
              write tpr-rec
                    invalid
                    add 1 to tpr-codice
                not invalid
                    exit perform
              end-write
           end-perform.
           set si-promo to true.
           move tpr-codice to save-tpr-codice mro-promo 
                              mto-promo-fittizia.

      ***---
       RIGHE-PROMO-FITTIZIE.
           move low-value to rpr-rec.
           move mto-promo-fittizia to rpr-codice.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next at end exit perform end-read
                    if rpr-codice not = mto-promo-fittizia
                       exit perform
                    end-if
                    delete rpromo record invalid continue end-delete
                 end-perform
           end-start.
           move low-value to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-promo = 0 or mro-promo = save-tpr-codice
                       perform PROMO-FITTIZIA
                       move save-tpr-codice to mro-promo
                       rewrite mro-rec
                    end-if
                 end-perform
           end-start.

      ***---
       PROMO-FITTIZIA.
           move save-tpr-codice  to rpr-codice.
           move mro-cod-articolo to rpr-articolo.
           |In caso di articolo doppio
           read rpromo no lock
                invalid compute rpr-qta = mro-qta - mro-qta-e
            not invalid compute rpr-qta = 
                                rpr-qta + ( mro-qta - mro-qta-e )
           end-read.     
           move mro-prz-unitario to rpr-prz-ven.
           accept rpr-data-creazione from century-date.
           accept rpr-ora-creazione  from time.
           move user-codi to rpr-utente-creazione.
           write rpr-rec invalid rewrite rpr-rec end-write.
           set mto-si-promo to true.
           rewrite mto-rec.

      ***---
       CANCELLA-PROMO-FITTIZIA.
           if mto-promo-fittizia not = 0
              move mto-promo-fittizia to tpr-codice
              delete tpromo record invalid continue end-delete
              move low-value to rpr-rec
              move mto-promo-fittizia to rpr-codice
              start rpromo key >= rpr-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rpromo next at end exit perform end-read
                       if rpr-codice not = mto-promo-fittizia
                          exit perform
                       end-if
                       delete rpromo record invalid continue end-delete
                    end-perform
              end-start
              set no-promo to true
              move low-value  to mro-rec
              move mto-chiave to mro-chiave-testa
              start mrordini key >= mro-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read mrordini next at end exit perform end-read
                       if mro-chiave-testa not = mto-chiave
                          exit perform
                       end-if
                       if mro-promo = mto-promo-fittizia
                          move 0 to mro-promo
                          rewrite mro-rec
                       else
                          if mro-promo not = 0
                             set si-promo to true
                          end-if
                       end-if
                    end-perform
              end-start
              move 0 to mto-promo-fittizia save-tpr-codice
           end-if.
