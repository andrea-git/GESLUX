      ***---
       CALCOLA-PRZ-FINITO.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move rlis-tipo-tratt-imposte to imf-codice.
           read impforn no lock 
                invalid initialize imf-rec
           end-read.

           perform VALUTA-SCELTA-PARTENZA.

           |START TASSE
           move rlis-scelta to start-tasse.

      *    applico il secondo sconto
           compute start-tasse = start-tasse -
                               ( start-tasse * rlis-sconto-2 ) / 100.
      *    applico il terzo sconto  
           compute start-tasse = start-tasse -
                               ( start-tasse * rlis-sconto-3 ) / 100.
      *    applico il quarto sconto
           compute start-tasse = start-tasse -
                               ( start-tasse * rlis-sconto-4 ) / 100.
      *    applico il quinto sconto
           compute start-tasse = start-tasse -
                               ( start-tasse * rlis-sconto-5 ) / 100.

           perform CALCOLA-COSTI-AGG.

           compute start-tasse = start-tasse + costi-agg.

           move start-tasse to prz-confronto 
                               prz-reale
                               como-prz-unitario
                               imponibile-merce.

           move rlis-articolo    to art-codice of articoli.
           read articoli no lock 
                invalid continue
            not invalid
                if prg-peso-utf = 0
                   move art-peso-utf     of articoli to prg-peso-utf
                end-if
                if prg-peso-non-utf  = 0
                   move art-peso-non-utf of articoli to prg-peso-non-utf
                end-if
                if prg-peso = 0
                   compute prg-peso = prg-peso-utf + prg-peso-non-utf
                end-if
                if como-data-ordine = 0
                   accept como-data-ordine from century-date
                end-if
                read impforn no lock 
                     invalid initialize imf-rec
                end-read
                if ProgrammaInUso = "gordforn" or = "gordfornvar"
                   if imf-prz-reale-utf-zero
                      move 0 to imposta-consumo
                   end-if
                   if imf-prz-reale-cou-zero
                      move 0 to imposta-cou
                   end-if
                   if imf-prz-reale-cobat-zero
                      move 0 to imposta-cobat
                   end-if
                   if imf-prz-reale-pb-zero
                      move 0 to add-piombo
                   end-if
                else
                   perform CALCOLO-IMPOSTE-FORNITORE
                end-if

                evaluate true
                when imf-prz-reale-utf-piu
                     add imposta-consumo to prz-reale
                when imf-prz-reale-utf-meno
                     subtract imposta-consumo from imponibile-merce
                end-evaluate

                evaluate true
                when imf-prz-reale-cou-piu 
                     add imposta-cou     to prz-reale
                when imf-prz-reale-cou-meno
                     subtract imposta-cou from imponibile-merce
                end-evaluate

                evaluate true
                when imf-prz-reale-cobat-piu 
                     add imposta-cobat   to prz-reale
                when imf-prz-reale-cobat-meno
                     subtract imposta-cobat from imponibile-merce
                end-evaluate

                evaluate true
                when imf-prz-reale-pb-piu
                     add add-piombo      to prz-reale
                when imf-prz-reale-pb-meno
                     subtract add-piombo from imponibile-merce
                end-evaluate

                if imf-conf-utf-si
                   compute prz-confronto =  
                           prz-confronto + imposta-consumo-conf
                end-if
                if imf-conf-cou-si
                   compute prz-confronto = 
                           prz-confronto + imposta-cou-conf
                end-if
                if imf-conf-cobat-si
                   compute prz-confronto = 
                           prz-confronto + imposta-cobat-conf
                end-if
                if imf-conf-pb-si
                   compute prz-confronto = 
                           prz-confronto + add-piombo-conf
                end-if
           end-read.

           if como-trasporto-f = 1 or como-trasporto-c = 1
              perform CALCOLA-TRASPORTO
              add costo-trasporto to prz-confronto
           end-if.

           move 0 to premio-FA.
           if rlis-PFA-si
              if desf-premio-netto-si
                 compute premio-FA = imponibile-merce *
                                     desf-perce-premi-fine-anno / 
                                     100
              else
                 compute premio-FA = prz-reale *
                                     desf-perce-premi-fine-anno /
                                     100
              end-if
              subtract premio-FA from prz-confronto
           end-if.

           call "nomepgm" using NomePgm.
           if NomePgm  not = "pordini"     and
                       not = "crea-ordfor" and
                       not = "art-ordforn"
              |Gli articoli con il flag "PRODUZIONE" attivato in scorta
              |usano il costo medio anzichè il prezzo di confronto
              move art-scorta of articoli to sco-codice
              read tscorte no lock
                   invalid continue
               not invalid
                   if sco-produzione-si
                      |USARE PROGRESSIVO PADRE
                      initialize prg-chiave 
                                 replacing numeric data by zeroes
                                      alphanumeric data by spaces
                      move art-codice of articoli to prg-cod-articolo
                      read progmag no lock invalid continue end-read
                      perform CALCOLA-COSTO-MP
                      add 0,005 to costo-mp giving costo-mp-2dec
                      move costo-mp-2dec to prz-confronto

                      move low-value     to dis-rec
                      move rlis-articolo to dis-articolo-finale
                      start distinteb key >= k-articolo
                            invalid continue
                        not invalid
                            read distinteb next 
                            if dis-articolo-finale = rlis-articolo
                               move 0 to idx
                               perform varying idx from 1 by 1 
                                         until idx > 10
                                  if dis-articolo(idx) = 0
                                     exit perform
                                  end-if
                                  if dis-no-somma(idx)
                                     move dis-articolo(idx) to 
                                          art-codice of articoli
                                     read articoli no lock 
                                          invalid continue
                                     end-read
                                     move dis-chiave-progmag-el(idx) 
                                       to prg-chiave
                                     read progmag no lock 
                                          invalid continue 
                                     end-read
                                     perform CALCOLA-COSTO-MP
                                     add 0,005 to costo-mp 
                                           giving costo-mp-2dec
                                     add costo-mp-2dec to prz-confronto
                                  end-if
                               end-perform
                            end-if
                      end-start
                   end-if
              end-read
           end-if. 

      ***---
       VALUTA-SCELTA-PARTENZA.
           move rlis-prz-acq to prz-scontato.
      *    applico il primo sconto
           compute prz-scontato = prz-scontato -
                                ( prz-scontato * rlis-sconto-1 ) / 100.

           move prz-scontato to rlis-scelta.
           if rlis-netto < prz-scontato and
              rlis-netto > 0
              move rlis-netto to rlis-scelta
           end-if.
 
           if prz-scontato = 0 and rlis-netto > 0
              move rlis-netto to rlis-scelta
           end-if.

           set rlis-sconti-zero-no to true.
           if rlis-scelta = rlis-netto
              set rlis-sconti-zero-si to true
           end-if.

      ***---
       CALCOLA-COSTI-AGG.
           move 0 to costi-agg.
           if rlis-perce-agg not = 0
              compute costi-agg =
                      start-tasse * rlis-perce-agg / 100
              add 0,00005 to costi-agg
           end-if.

           add rlis-costi-agg to costi-agg.
