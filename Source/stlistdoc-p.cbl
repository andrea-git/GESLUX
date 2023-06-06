       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stlistdoc-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "clienti.sl".
           copy "tvettori.sl".   
           copy "lineseq.sl".
           copy "ttipocli.sl".

       select sort-tordini assign to 
              status status-sort-tordini.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "clienti.fd". 
           copy "tvettori.fd".
           copy "lineseq.fd".   
           copy "ttipocli.fd".

       SD sort-tordini.
       01  sort-rec.
         05 sort-key.
            10 sort-dt-bolla  pic 9(8).
            10 sort-num-bolla pic 9(8).
         05 sort-causale   pic x(3).
         05 sort-evasione  pic 9(8).
         05 sort-vettore   pic 9(5).
         05 sort-cli       pic 9(5).
         05 sort-dest      pic 9(5).

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
       77  status-tordini        pic xx.
       77  status-lineseq        pic xx.
       77  status-clienti        pic xx.
       77  status-tvettori       pic xx. 
       77  status-ttipocli       pic xx.
       77  status-sort-tordini   pic xx.
       77  wstampa               pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  filler                pic 9.
           88  TestataGiaFatta   value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       78  titolo value "Lista Documenti".

       77  separatore       pic x.

       01  st-titolo.
           05 filler        pic x(28).
           05 st-fisso      pic x(6) value "Lista ".
           05 st-titdoc     pic x(20).

       01  st-testa.
           05 st-tp         pic x(7)  value "Causale".
           05 filler        pic x(5).             
           05 st-nr         pic x(3)  value "Nr.".
           05 filler        pic x.
           05 st-dt         pic x(10) value "Data bozza".
           05 filler        pic x.
           05 st-vt         pic x(7)  value "Vettore".
           05 filler        pic x(4).
           05 st-cli        pic x(7)  value "Cliente".
           05 filler        pic x.
           05 st-ds         pic x(7)  value "Destino".
           05 filler        pic x(9).               
           05 st-rag-soc    pic x(15) value "Ragione sociale".
           05 filler        pic x(11).
           05 st-tc         pic x(14) value "Tipol. cliente".
           05 filler        pic x(11).
           05 st-bolla      pic x(5)  value "Bolla".
           05 filler        pic x(4).
           05 st-del        pic x(3)  value "del".
           05 filler        pic x(5).
      *****     05 st-fatt       pic x(5)  value "Fatt.".
      *****     05 filler        pic x(5).
      *****     05 st-lotto      pic x(5)  value "Lotto".
       01  st-line-riga     pic x(83) value all "-".

       01  struttura-stampa.
         03 st-dati.
            05 st-causale    pic x(4).
            05 filler        pic x.
            05 st-ordine     pic z(7).
            05 filler        pic x.
            05 st-vettore    pic x(3).
            05 filler        pic x.
            05 st-codcli     pic z(5).
            05 filler        pic x value "-".
            05 st-destino    pic 99b.
            05 filler        pic x.
            05 st-ragsoc     pic x(28).
            05 filler        pic x(2).
            05 st-num-bolla  pic z(8).
            05 filler        pic x(2).
            05 st-data       pic x(8).
      *****      05 st-num-fatt   pic z(8).
      *****      05 st-num-pren   pic z(8).

      ***** 78  max-righe         value 60.
      ***** 77  num-righe         pic 99.

       LINKAGE SECTION.
       77 TipoDoc                  pic  xx.
           88 BollePrenotate       value  "BO". 
           88 OrdiniInSospeso      value  "OS". 
           88 OrdiniBloccati       value  "OB".
           88 BolleNonFatturate    value  "BN". 
           88 FatturePrenotate     value  "FP". 

       77  Link-Path               pic x(256).

      ******************************************************************
       PROCEDURE DIVISION using TipoDoc link-path.

       DECLARATIVES.
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere. "
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              if BolleNonFatturate
                 sort sort-tordini
                      on ascending key sort-key
                         with duplicates
                         input  procedure is ELABORAZIONE-SORT
                         output procedure is SCORRI-SORT
              else                 
                 perform ELABORAZIONE-NORMALE
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                 
           accept separatore from environment "SEPARATORE"
                  on exception move "," to separatore
              not on exception
                  if separatore = space
                     move "," to separatore
                  end-if
           end-accept.

           set tutto-ok to true.
           set TestataGiaFatta to false.
           accept  wstampa      from environment "PATH-ST".
           accept  como-data         from century-date.
           accept  como-ora          from time.
           inspect wstampa      replacing trailing 
                                     spaces by low-value.
           string wstampa       delimited low-value
                  "listadoc"    delimited size
                  "_"           delimited size
                  como-data     delimited size
                  "_"           delimited size
                  como-ora      delimited size
                  ".csv"        delimited size
      *****            ".txt"        delimited size
             into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open output lineseq.
           open input  tordini clienti tvettori ttipocli.
      
      ***---
       ELABORAZIONE-NORMALE.
           set tutto-ok to true.
           set trovato to false.
           move low-value to tor-rec.
           evaluate true
           when BollePrenotate
                set tor-bolla-si-prenotata to true
                start tordini key is >= k3 |KEY BOLLA
                      invalid set errori to true 
                end-start
           when OrdiniInSospeso
           when OrdiniBloccati
                set tor-bolla-no-prenotata to true
                start tordini key is >= k3 |KEY BOLLA 
                      invalid set errori to true 
                end-start
           when BolleNonFatturate                 
                set tor-fatt-no-prenotata to true 
                start tordini key is >= k4 |KEY FATTURA
                      invalid set errori to true 
                end-start
           when FatturePrenotate
                set tor-fatt-si-prenotata to true 
LUBEXX          start tordini key is >= k-agfatt |KEY FATTURA
                      invalid set errori to true 
                end-start
           end-evaluate.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 evaluate true
                 when BollePrenotate
                      if tor-anno-bolla = 0 and
                         tor-data-bolla = 0 and
                         tor-num-bolla  = 0
                         if tor-bolla-si-prenotata
                            move "Bolle prenotate" to st-titdoc
                            perform MOVE-DATI
                         end-if
                      else
                         exit perform
                      end-if

                 when OrdiniInSospeso
                      if tor-anno-bolla = 0 and
                         tor-data-bolla = 0 and
                         tor-num-bolla  = 0 
                         if tor-bolla-no-prenotata
                            move "Ordini in sospeso" to st-titdoc
                            perform MOVE-DATI
                         else
                            exit perform
                         end-if         
                      else
                         exit perform
                      end-if

                 when OrdiniBloccati
                      if tor-anno-bolla = 0 and
                         tor-data-bolla = 0 and
                         tor-num-bolla  = 0
                         if tor-bolla-no-prenotata
                            if tor-bloccato
                               move "Ordini bloccati" to st-titdoc
                               perform MOVE-DATI
                            end-if
                         else
                            exit perform
                         end-if         
                      else
                         exit perform
                      end-if

                 when BolleNonFatturate
                      if tor-anno-fattura   = 0 and
                         tor-data-fattura   = 0
                         if tor-fatt-no-prenotata
                            if tor-anno-bolla not = 0 and
                               tor-data-bolla not = 0 and
                               tor-num-bolla  not = 0
                               move "Bolle non fatturate" to st-titdoc
                               perform MOVE-DATI
                            end-if
                         else
                            exit perform
                         end-if
                      else
                         exit perform
                      end-if

                 when FatturePrenotate
                      if tor-anno-fattura = 0 and
                         tor-data-fattura = 0 and
                         tor-num-prenot   = 0
                         if tor-fatt-si-prenotata
                            move "Fatture prenotate" to st-titdoc
                            perform MOVE-DATI
                         end-if
                      else
                         exit perform
                      end-if

                 end-evaluate
              end-perform
           end-if.

           if not trovato
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.

      ***---
       ELABORAZIONE-SORT.
           set tutto-ok to true.
           set trovato to false.
           move low-value to tor-rec.             
           set tor-fatt-no-prenotata to true 
           start tordini key is >= k4 |KEY FATTURA
                 invalid set errori to true 
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read

                 if tor-anno-fattura   = 0 and
                    tor-data-fattura   = 0
                    if tor-fatt-no-prenotata
                       if tor-anno-bolla not = 0 and
                          tor-data-bolla not = 0 and
                          tor-num-bolla  not = 0
                          move tor-cod-cli     to sort-cli
                          move tor-prg-destino to sort-dest
                          move tor-data-bolla  to sort-dt-bolla  
                          move tor-num-bolla   to sort-num-bolla 
                          move tor-causale     to sort-causale   
                          move tor-numero      to sort-evasione
                          move tor-vettore     to sort-vettore
                          release sort-rec
                          set trovato to true
                       end-if
                    else
                       exit perform
                    end-if
                 else
                    exit perform
                 end-if
              end-perform
           end-if.

           if not trovato
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.

      ***---
       SCORRI-SORT.
           perform until 1 = 2
              return sort-tordini at end exit perform end-return
              move sort-dt-bolla  to tor-data-bolla
              move sort-num-bolla to tor-num-bolla
              move sort-causale   to tor-causale
              move sort-evasione  to tor-numero
              move sort-vettore   to tor-vettore
              move sort-cli       to tor-cod-cli
              move sort-dest      to tor-prg-destino
              move "Bolle non fatturate" to st-titdoc
              perform MOVE-DATI
           end-perform.

      ***---
       MOVE-DATI.            
           set trovato to true.
           if not TestataGiaFatta
              set TestataGiaFatta to true  

              initialize line-riga
              string separatore delimited size
                     separatore delimited size
                     separatore delimited size
                     separatore delimited size
                     separatore delimited size
                     st-fisso   delimited size
                     st-titdoc  delimited size
                into line-riga
              end-string
              write line-riga

              initialize line-riga
              string st-tp      delimited size
                     separatore delimited size
                     st-nr      delimited size
                     separatore delimited size   
                     st-dt      delimited size
                     separatore delimited size   
                     st-vt      delimited size
                     separatore delimited size   
                     st-cli     delimited size
                     separatore delimited size   
                     st-ds      delimited size
                     separatore delimited size   
                     st-rag-soc delimited size
                     separatore delimited size   
                     st-tc      delimited size
                     separatore delimited size   
                     st-bolla   delimited size
                     separatore delimited size   
                     st-del     delimited size
                     separatore delimited size
                into line-riga
              end-string
              write line-riga

      *****        write line-riga from st-titolo after 1
      *****        write line-riga from space after 1
      *****        write line-riga from st-testa after 1
      *****        write line-riga from st-line-riga after 1
      *****        write line-riga from space after 1

      *****        move 6   to num-righe
           end-if.
           move tor-causale      to st-causale.
           move tor-numero       to st-ordine.
           move tor-vettore      to vet-codice.
           read tvettori invalid move spaces    to st-vettore
                     not invalid move vet-sigla to st-vettore
           end-read.
           move tor-cod-cli      to st-codcli.
           move tor-prg-destino  to st-destino.

           move tor-cod-cli     to cli-codice
           set cli-tipo-C       to true    
           read clienti invalid move spaces       to st-ragsoc
                    not invalid move cli-ragsoc-1 to st-ragsoc
           end-read.

           move cli-tipo to tcl-codice
           read ttipocli no lock 
                invalid move spaces to tcl-descrizione 
           end-read.
                 
           move tor-num-bolla to st-num-bolla.
           if tor-data-bolla not = 0
              move tor-data-bolla(3:2) to st-data(7:2)
              move "/"                 to st-data(6:1)
              move tor-data-bolla(5:2) to st-data(4:2)
              move "/"                 to st-data(3:1)
              move tor-data-bolla(7:2) to st-data(1:2)
           else
              move spaces to st-data
           end-if.  

           initialize line-riga
           string st-causale   delimited size
                  separatore   delimited size
                  st-ordine    delimited size
                  separatore   delimited size   
                  tor-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(1:4) delimited size
                  separatore   delimited size
                  st-vettore   delimited size
                  separatore   delimited size
                  st-codcli    delimited size
                  separatore   delimited size
                  st-destino   delimited size
                  separatore   delimited size
                  st-ragsoc    delimited size
                  separatore   delimited size
                  tcl-descrizione delimited size
                  separatore   delimited size
                  st-num-bolla delimited size
                  separatore   delimited size
                  st-data      delimited size
                  separatore   delimited size
             into line-riga
           end-string.
           write line-riga.

      *****     if num-righe >= max-righe
      *****        write line-riga from spaces after page
      *****        write line-riga from x"09" after 1
      *****        move 0 to num-righe
      *****     end-if.
      *****     write line-riga from struttura-stampa after 1.
      *****     add 1 to num-righe.

      ***---
       CLOSE-FILES.
           close tordini, lineseq, clienti, tvettori ttipocli.

      ***---
       EXIT-PGM.
           move wstampa to link-path.
           goback.
