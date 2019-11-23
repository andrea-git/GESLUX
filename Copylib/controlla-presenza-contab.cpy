      ***---
       CONTROLLA-PRESENZA-CONTAB.
           set trovato-contab to false.
           move link-mese to sts-mese    of statsett.
           move spaces    to sts-tipocli of statsett.
           move 0         to sts-marca   of statsett.
           start statsett key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next at end exit perform end-read
                    if sts-mese of statsett not = link-mese 
                       exit perform 
                    end-if
                    if sts-kg-corr      of statsett not = 0 or
                       sts-qta-corr     of statsett not = 0 or
                       sts-fat-corr     of statsett not = 0 or
                       sts-csm-corr     of statsett not = 0 or
                       sts-adeguam-corr of statsett not = 0
                       set trovato-contab to true
                       exit perform
                    end-if
                 end-perform
           end-start.
           if not trovato-contab
              |Devo comunque aprirlo così dopo la close non va in errore
              initialize path-tmp-tendenza
              set tutto-ok to true
              accept  path-tmp-tendenza from environment "PATH-ST"
              inspect path-tmp-tendenza 
                     replacing trailing spaces by low-value
              string  path-tmp-tendenza delimited by low-value
                      "tmp-tendenza"    delimited by size
                      "_"               delimited by size
                      como-data         delimited by size
                      "_"               delimited by size
                      como-ora          delimited by size
                      into path-tmp-tendenza
              end-string
              set trovato to true

              open output tmp-tendenza

              move "NO CONTAB" to line-riga
              write line-riga
           end-if.
