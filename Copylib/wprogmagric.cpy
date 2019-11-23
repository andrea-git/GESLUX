      ***---
       WPROGMAGRIC-BATCH.
           move 0 to link-wprogmag-status.
           if link-magazzino = spaces
              move low-value to mag-rec
              start tmagaz key is >= mag-chiave invalid continue 
              end-start
              perform until 1 = 2
                 read tmagaz next at end exit perform end-read
                 if si-mag-principale exit perform end-if
              end-perform
              move mag-codice     to prr-cod-magazzino
           else
              move link-magazzino to prr-cod-magazzino
           end-if.

           evaluate true
           when link-close
                close progmagric

           when link-batch 
                |scrive un record COMPLETO DI CHIAVE (figlio) di progmagric
                initialize prr-rec
                move link-articolo to prr-cod-articolo
                move link-imballo  to prr-tipo-imballo
                move link-peso     to prr-peso
                move link-utf      to prr-peso-utf
                move link-non-utf  to prr-peso-non-utf
                call "C$CALLEDBY" using nome-pgm
                if nome-pgm = "insdati"
                   move link-magazzino to prr-cod-magazzino
                else
                   move mag-codice     to prr-cod-magazzino
                end-if
                move link-user     to prr-utente-creazione
                accept prr-data-creazione from century-date
                accept prr-ora-creazione  from time
                write prr-rec invalid continue end-write
                |scrive un record VUOTO (padre) di progmagric
                initialize prr-rec
                move link-articolo to prr-cod-articolo 
                move link-utf      to prr-peso-utf
                move link-non-utf  to prr-peso-non-utf
                move link-user     to prr-utente-creazione
                accept prr-data-creazione from century-date
                accept prr-ora-creazione  from time
                write prr-rec invalid continue end-write

           when link-update |aggiorna i valori di progmagric in base alla causale
                perform AGGIORNA-PROGMAGRIC-CAUSALE

           when link-delete
                perform CANCELLA-PROGMAGRIC

           when link-value-changed
                perform CAMBIO-VALORI-ARTICOLO

           when link-costo-ultimo
                perform AGGIORNA-COSTO-ULTIMO

           end-evaluate.
