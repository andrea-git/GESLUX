       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      reset-situazione.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Files GESLUX
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tcontat.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tparamge.sl".
           copy "statraff.sl".
           copy "statsett.sl".
           copy "provvig.sl".  
           copy "trasporti.sl".
           copy "statmese.sl".
           copy "tmovtrat.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
      *    Files GESLUX
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tcontat.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tparamge.fd".
           copy "statraff.fd".
           copy "statsett.fd".
           copy "provvig.fd".  
           copy "trasporti.fd".
           copy "statmese.fd".
           copy "tmovtrat.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
       77  status-tordini         pic xx.
       77  status-rordini         pic xx.
       77  status-tnotacr         pic xx.
       77  status-rnotacr         pic xx.
       77  status-tcontat         pic xx.
       77  status-tmovmag         pic xx.
       77  status-rmovmag         pic xx.
       77  status-tparamge        pic xx.
       77  status-statraff        pic xx.
       77  status-statsett        pic xx.
       77  status-provvig         pic xx.
       77  status-trasporti       pic xx.
       77  status-statmese        pic xx.
       77  status-tmovtrat        pic xx.
       77  status-progmag         pic xx.

       77  scelta                 pic 9.


      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                tordini  
                rordini  
                tnotacr  
                rnotacr  
                tcontat  
                tmovmag  
                rmovmag  
                tparamge 
                statraff 
                statsett 
                provvig  
                trasporti
                statmese
                progmag.
           COPY "DECLXER2".

       MAIN-PRG.
           open input tparamge.
           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.
           close tparamge.

           display message "**RESET TOTALE?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-ARCHIVI-ORDINI
              perform PULISCI-TCONTAT-ANNO-IN-CORSO
              perform RESETTA-ARCHIVI-MAGAZZINO
              perform RESETTA-ARCHIVI-STATISTICHE
              perform RESETTA-PROVVIGIONI
              perform RESETTA-TRASPORTI

              display message "Operazione terminata!"
                        title "Reset situazione"

              goback
           end-if.
           
           display message "**RESET TOTALE MAGAZZINO?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-FATTURE-CONTABILIZZATE
              perform RESETTA-ARCHIVI-MAGAZZINO
              perform RESETTA-ARCHIVI-STATISTICHE
              perform RESETTA-PROVVIGIONI
              perform RESETTA-TRASPORTI

              display message "Operazione terminata!"
                        title "Reset situazione"

              goback
           end-if.

           display message "**RESET ORDINI?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".

           if scelta = 1 |mb-yes
              perform RESETTA-ARCHIVI-ORDINI
           else        

              display message "**RESET FATTURE CONTABILIZZATE?**"
                         type 2 |mb-yes-no
                      default 2 |mb-no
                         icon 2 |warning icon
                       giving scelta
                        title "Reset situazione"

              if scelta = 1
                 perform RESETTA-FATTURE-CONTABILIZZATE
              else
                 display message "**DA FATTURE A BOLLE?**"
                            type 2 |mb-yes-no
                         default 2 |mb-no
                            icon 2 |warning icon
                          giving scelta
                           title "Reset situazione"
                 if scelta = 1
                    perform DA-FATTURA-A-BOLLE
                 else
                    display message "PRENOTA FATTURE?"
                               type 2 |mb-yes-no
                            default 2 |mb-no
                               icon 2 |warning icon
                             giving scelta
                              title "Reset situazione"
                    if scelta = 1
                       perform PRENOTA-FATTURE
                    else
                       display message "**RESET BOLLE?**"
                                  type 2 |mb-yes-no
                               default 2 |mb-no
                                  icon 2 |warning icon
                                giving scelta
                                 title "Reset situazione"
                       if scelta = 1
                          perform RESET-BOLLE
                       end-if
                    end-if
                 end-if
              end-if       
           end-if.

           display message "**RESET CONTATORI?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform PULISCI-TCONTAT-ANNO-IN-CORSO
           end-if.

           display message "**RESET MAGAZZINO?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-ARCHIVI-MAGAZZINO
           end-if.

           display message "**RESET STATISTICHE?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-ARCHIVI-STATISTICHE
           end-if.

           display message "**RESET PROVVIGIONI?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-PROVVIGIONI
           end-if.

           display message "**RESET TRASPORTI?**"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-TRASPORTI
           end-if.

           display message "RESET PROGMAG?"
                      type 2 |mb-yes-no
                   default 2 |mb-no
                      icon 2 |warning icon
                    giving scelta
                     title "Reset situazione".
           if scelta = 1
              perform RESETTA-PROGMAG
           end-if.

           display message "Operazione terminata!"
                     title "Reset situazione"

           goback.

      ***---
       RESETTA-ARCHIVI-ORDINI.
           open output tordini
                       rordini
                       tnotacr
                       rnotacr.

           close       tordini
                       rordini
                       tnotacr
                       rnotacr.

      ***---
       PULISCI-TCONTAT-ANNO-IN-CORSO.
           open i-o tcontat.
           move tge-anno to con-anno.
           delete tcontat record invalid continue end-delete.
           initialize con-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tge-anno to con-anno.
           write con-rec invalid continue end-write.
           close tcontat.

      ***---
       RESETTA-FATTURE-CONTABILIZZATE.
           open i-o tordini.
           move low-value to tor-rec.
           set  tor-si-agg-contab to true.
           start tordini key is >= k-contab
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-si-agg-contab 
                       if tor-data-fattura not = 0
                          set tor-no-agg-contab to true
                          rewrite tor-rec
                       end-if
                    else
                       exit perform
                    end-if
                 end-perform
           end-start.
           close tordini.

           open i-o tnotacr.
           move low-value to tno-rec.
           set  tno-si-agg-contab to true.
           start tnotacr key is >= k-contab
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                    if tno-si-agg-contab 
                       if tno-data-fattura not = 0
                          set tno-no-agg-contab to true
                          rewrite tno-rec
                       end-if
                    else
                       exit perform
                    end-if
                 end-perform
           end-start.
           close tnotacr.
            
      ***---
       DA-FATTURA-A-BOLLE.
           open i-o tordini.
           move low-value  to tor-rec.
           move high-value to tor-anno-fattura tor-num-fattura.
           start tordini key is <= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini previous at end exit perform end-read
                    if tor-num-fattura  = 0 or
                       tor-anno-fattura = 0
                       exit perform
                    end-if
                    move 0 to tor-data-fattura
                    move 0 to tor-anno-fattura
                    move 0 to tor-num-fattura
                    move 0 to tor-num-prenot
                    rewrite tor-rec invalid continue end-rewrite
                 end-perform
           end-start.
           close tordini.

           open i-o tnotacr.
           move low-value  to tno-rec. 
           move high-value to tno-anno-fattura tno-num-fattura.
           start tnotacr key is <= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr previous at end exit perform end-read
                    if tno-num-fattura  = 0 or
                       tno-anno-fattura = 0
                       exit perform
                    end-if
                    move 0 to tno-data-fattura
                    move 0 to tno-anno-fattura
                    move 0 to tno-num-fattura
                    move 0 to tno-num-prenot
                    rewrite tno-rec invalid continue end-rewrite
                 end-perform
           end-start.
           close tnotacr.

      ***---
       PRENOTA-FATTURE.
           open i-o tordini.
           move low-value  to tor-rec.
           set  tor-fatt-no-prenotata to true.
           start tordini key is >= k4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-num-fattura  not = 0 or
                       tor-data-fattura not = 0 or
                       tor-num-prenot   not = 0 or
                       tor-fatt-si-prenotata
                       exit perform
                    end-if
                    if tor-data-bolla not = 0 and
                       tor-anno-bolla not = 0 and
                       tor-num-bolla  not = 0
                       set tor-fatt-si-prenotata to true
                       rewrite tor-rec invalid continue end-rewrite
                    end-if
                 end-perform
           end-start.
           close tordini.

      ***---
       RESET-BOLLE.
           open i-o tordini.
           move low-value  to tor-rec.
           move high-value to tor-anno-bolla tor-num-bolla.
           start tordini key is <= k-bolla
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini previous at end exit perform end-read
                    if tor-num-bolla  = 0 or
                       tor-anno-bolla = 0
                       exit perform
                    end-if
                    move 0 to tor-data-bolla
                    move 0 to tor-anno-bolla
                    move 0 to tor-num-bolla
                    rewrite tor-rec invalid continue end-rewrite
                 end-perform
           end-start.
           close tordini.

      ***---
       RESETTA-ARCHIVI-MAGAZZINO.
           open output tmovmag
                       rmovmag.

           close       tmovmag
                       rmovmag.

           open i-o tcontat.
           move tge-anno to con-anno.
           read tcontat no lock
                invalid continue
            not invalid
                move 0 to con-ult-num-movim
                rewrite con-rec
           end-read.
           close tcontat.

      ***---
       RESETTA-ARCHIVI-STATISTICHE.
           open output statraff
                       statsett
                       statmese.

           close       statraff 
                       statsett 
                       statmese.

      ***---
       RESETTA-PROVVIGIONI.
           open output provvig.
           close       provvig.

           open i-o tmovtrat.
           move tge-anno to tra-anno.
           read tmovtrat no lock
                invalid continue
            not invalid
                move 0 to tra-ult-mov-provv
                rewrite tra-rec
           end-read.
           close tmovtrat.

      ***---
       RESETTA-TRASPORTI.
           open output trasporti.
           close       trasporti.

           open i-o tmovtrat.
           move tge-anno to tra-anno.
           read tmovtrat no lock
                invalid continue
            not invalid
                move 0 to tra-ult-mov-tras
                rewrite tra-rec
           end-read.
           close tmovtrat.

      ***---
       RESETTA-PROGMAG.
           open i-o progmag.
           move low-value to prg-chiave.
           start progmag key is >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              initialize prg-sezione-consolidati
                         prg-sezione-dinamici
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              rewrite prg-rec
           end-perform.
           close progmag.
