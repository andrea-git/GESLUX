      ***---
       RECUPERO-ANAGRAFICA.
           move prg-cod-articolo of progmag to art-codice.
           read articoli no  lock
                invalid  continue
            not invalid
                compute costo-mp =
                        art-prezzo-acquisto - 
                     (( art-prezzo-acquisto * 
                        art-perce-sconto-acquisto ) / 100 )
                move 0 to  risultato-imposte
                           imposta-cou
                           imposta-cobat
                           imposta-consumo
                           TipoImposta
                initialize mar-rec 
                           replacing numeric data by zeroes
                                alphanumeric data by spaces
                move art-marca-prodotto to mar-codice
                read tmarche no  lock
                     invalid continue
                 not invalid perform CALCOLA-IMPOSTE
                end-read
                add imposta-cobat imposta-consumo imposta-cou
                 to costo-mp
           end-read.

      ***---
       COPY-IMPOSTE.
           copy "imposte.cpy".
