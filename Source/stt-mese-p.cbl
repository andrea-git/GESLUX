       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stt-mese-p.
       AUTHOR.                          Filippo.
       REMARKS.  Stampe mensili: estrazione, su file .csv, dei 
                 movimenti (TRASPORTI) valorizzando la tariffa 
                 totale nella colonna IMPORTO.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "trasporti.sl".
           copy "tvettori.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tregioni.sl".
           copy "eordini.sl".
           copy "tordini.sl".
           copy "btnotacr.sl".
           copy "tescons.sl".
           copy "tmp-trasporti.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "trasporti.fd".
           copy "tvettori.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tregioni.fd".
           copy "eordini.fd".
           copy "tordini.fd".
           copy "btnotacr.fd".
           copy "tescons.fd".
           copy "tmp-trasporti.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampe mensili".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-numero-x         pic x(10).
       77  scelta                pic 9.
       77  user-codi             pic x(10).

       77  status-lineseq        pic xx.
       77  status-trasporti      pic xx.
       77  status-tvettori       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tregioni       pic xx.
       77  status-eordini        pic xx.
       77  status-tordini        pic xx.
       77  status-btnotacr       pic xx.
       77  status-tescons        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tmp-trasporti  pic xx.

       77  path-csv              pic x(256).
       77  path-txt              pic x(256).
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  mese-path             pic x(256).
       77  SaveMarca             pic 9(5) value 0.
       77  prg-cod-articolo-edit pic z(6).
       77  prg-peso-edit         pic zz9,999.
       77  imballi-ed            pic z.zz9.
       77  wk-movimenti          pic s9(12)v99.
       77  qta-edit              pic z.zzz.
       77  prg-riga              pic 9(10).
       77  com-importo           pic 9(9)v99.
       77  data6                 pic 9(6).

       77  tipo-file             pic x.
           88 FileExcel          value "E".
           88 FileTxt            value "T".

       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  filler                pic 9.
           88  trovata-bozza-reso value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       77 anno-prev            pic 9(4).
       77 anno-next            pic 9(4).      
       77 anno-curr            pic 9(4).
       77 mese-esteso-curr     PIC x(15).
       77 mese-esteso-prev     PIC x(15).
       77 mese-esteso-next     PIC x(15).
       77 NomeFile             PIC x(10).
       77 PathPod              PIC x(256).
       77 FilePod              PIC x(256).
       77 mese                 PIC 99.
       01 file-info.
           05 file-size        PIC X(8)
                      USAGE IS COMP-X.
           05 file-date        PIC  9(8)
                      USAGE IS COMP-X.
           05 file-time        PIC  9(8)
                      USAGE IS COMP-X.

       LINKAGE SECTION.
       copy "link-stt-mese.def".

      ******************************************************************
       PROCEDURE DIVISION using stt-mese-linkage.

       DECLARATIVES.

       TMP-TRASPORTI-ERR SECTION.
           use after error procedure on tmp-trasporti.
           set tutto-ok  to true.
           evaluate status-tmp-trasporti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-TRASPORTI-ERR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message"File [TMP-TRASPORTI-ERR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message 
                        "[TMP-TRASPORTI-ERR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-TMP-TRASPORTI
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           set tutto-ok  to true.
           evaluate status-trasporti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File trasporti [TRASPORTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TRASPORTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TRASPORTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TREGIONI-ERR SECTION.
           use after error procedure on tregioni.
           set tutto-ok  to true.
           evaluate status-tregioni
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella regioni [TREGIONI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TREGIONI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TREGIONI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       EORDINI-ERR SECTION.
           use after error procedure on EORDINI.
           set tutto-ok  to true.
           evaluate status-eordini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File destini [EORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [EORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[EORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
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
           when "93"
                initialize geslock-messaggio
                string   "Chiudere File Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate
       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.

           perform EXIT-PGM.

      ***---
       INIT.
      *-
           move sta-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           move 0 to prg-riga.
           initialize path-tmp wstampa path-txt.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  mese-path     from environment "PATH-ST".
           inspect mese-path replacing trailing spaces by low-value.
           inspect user-codi     replacing trailing spaces by low-value.
           string  mese-path     delimited by low-value
                   "stt-mese"    delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   "_"           delimited by size
                   como-data     delimited by size
                   "_"           delimited by size
                   como-ora      delimited by size
                   ".tmp"        delimited by size
                   into path-tmp
           end-string.
           string  mese-path     delimited by low-value
                   "stt-mese"    delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   ".csv"        delimited by size
                   into path-csv
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-TRASPORTI.
           if tutto-ok
              open input trasporti
                         tvettori
                         clienti 
                         destini 
                         tregioni
                         eordini
                         tordini
                         btnotacr
                         tescons
                         tcaumag
           end-if.

      ***---
       OPEN-OUTPUT-TMP-TRASPORTI.
           open output tmp-trasporti.

      ***---
       ELABORAZIONE.
           move low-value       to trs-rec.
           move sta-data-from   to trs-data-bolla.

           start trasporti key is >= k-data-bolla
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read trasporti next no lock
                    at end
                       exit perform
                 end-read
                 if trs-data-bolla >= sta-data-from and
                    trs-data-bolla <= sta-data-to

                   if trs-vettore >= sta-vet-from  and
                      trs-vettore <= sta-vet-to    and
                      (sta-cliente = 0 or
                       trs-cliente = sta-cliente) and  
                      (sta-prov = spaces or
                      trs-provincia = sta-prov ) and   
                      (sta-regione = 0 or trs-regione = sta-regione)
                       
                       perform INIZIALIZZA-RIGA
                       if trs-destino not = 0
                          perform READ-DESTINI
                       else
                          move spaces to ttrs-destino
                       end-if
                       if sta-localita = spaces or
                          des-localita = sta-localita
                          perform VALORIZZA-RIGA
                       end-if
                    end-if
                 else
                    exit perform
                 end-if
              end-perform
           end-if.

           if not trovato
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              close      tmp-trasporti
              open input tmp-trasporti
              if tutto-ok
                 perform OPEN-OUTPUT-LINESEQ
                 if tutto-ok
                    perform GENERA-FILE-EXCEL
                 end-if
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           move path-csv to wstampa.
           open output lineseq.

      ***---
       GENERA-FILE-EXCEL.
           set FileExcel to true.
           move low-value to ttrs-rec.
           start tmp-trasporti key is >= ttrs-chiave
                 invalid continue
           end-start
           perform until 1 = 2
              read tmp-trasporti next at end exit perform end-read

              if prima-volta
                 perform SCRIVI-INTESTAZIONE
              end-if

              inspect ttrs-note replacing trailing space by low-value
              initialize line-riga
              string ttrs-data-bolla    delimited by size
                     separatore         delimited by size
                     ttrs-num-bolla     delimited by size
                     separatore         delimited by size
                     ttrs-vettore       delimited by size
                     separatore         delimited by size
                     ttrs-vet-desc      delimited by size
                     separatore         delimited by size
                     ttrs-cliente       delimited by size
                     separatore         delimited by size
                     ttrs-cli-desc      delimited by size
                     separatore         delimited by size
                     ttrs-destino       delimited by size
                     separatore         delimited by size
                     ttrs-dest-desc     delimited by size
                     separatore         delimited by size
                     ttrs-dest-citta    delimited by size
                     separatore         delimited by size
                     ttrs-regione       delimited by size
                     separatore         delimited by size
                     ttrs-provincia     delimited by size
                     separatore         delimited by size
                     ttrs-qta-kg        delimited by size
                     separatore         delimited by size
                     ttrs-qta-arrot     delimited by size
                     separatore         delimited by size
                     ttrs-tariffa       delimited by size
                     separatore         delimited by size
                     ttrs-importo       delimited by size
                     separatore         delimited by size
                     ttrs-qta-kg-SHI    delimited by size
                     separatore         delimited by size
                     ttrs-qta-arrot-SHI delimited by size
                     separatore         delimited by size
                     ttrs-tariffa-SHI   delimited by size
                     separatore         delimited by size
                     ttrs-importo-SHI   delimited by size
                     separatore         delimited by size
                     ttrs-qta-kg-GET    delimited by size
                     separatore         delimited by size
                     ttrs-qta-arrot-GET delimited by size
                     separatore         delimited by size
                     ttrs-tariffa-GET   delimited by size
                     separatore         delimited by size
                     ttrs-importo-GET   delimited by size
                     separatore         delimited by size
                     ttrs-note          delimited by low-value
                     separatore         delimited by size
                     ttrs-esito         delimited by size
                     separatore         delimited by size
                     ttrs-pod           delimited by size
                     into line-riga
              end-string
              write line-riga
           end-perform.

           close lineseq.
           perform CALL-EXCEL.

      ***---
       SCRIVI-INTESTAZIONE.
           set prima-volta to false.
           initialize line-riga.

           string "Data"
                  separatore
                  "Bolla"
                  separatore
                  "Cod. vet."
                  separatore
                  "Vettore"
                  separatore
                  "Cod. cli"
                  separatore
                  "Cliente"
                  separatore
                  "Destino"
                  separatore
                  "Rag.soc. Destino"
                  separatore
                  "Città Destino"
                  separatore
                  "Regione"
                  separatore
                  "Pr"
                  separatore
                  "Kg. bolla"
                  separatore        
                  "Q.li/FT"
                  separatore        
                  "Tariffa"    
                  separatore
                  "IMPORTO"
                  separatore
                  "Kg. bolla SHI"
                  separatore        
                  "Q.li/FT SHI"
                  separatore        
                  "Tariffa SHI"
                  separatore
                  "IMPORTO SHI" 
                  separatore
                  "Kg. bolla GET"
                  separatore        
                  "Q.li/FT GET"
                  separatore        
                  "Tariffa GET"
                  separatore
                  "IMPORTO GET" 
                  separatore
                  "NOTE" 
                  separatore
                  "ESITO" 
                  separatore 
                  "POD" delimited by size
                  into line-riga
           end-string

           write line-riga.

      ***---
       TROVA-POD.
           move "N"                  to ttrs-pod.
           move tor-anno-bolla       to anno-curr.
           move tor-data-bolla(5:2)  to mese.
           evaluate mese
           when 01  move "GENNAIO"   to mese-esteso-curr
                    move "DICEMBRE"  to mese-esteso-prev
                    move "FEBBRAIO"  to mese-esteso-next

                    subtract 1 from anno-curr giving anno-prev
                    move anno-curr to anno-next
                                                                   
           when 02  move "FEBBRAIO"  to mese-esteso-curr
                    move "GENNAIO"   to mese-esteso-prev
                    move "MARZO"     to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 03  move "MARZO"     to mese-esteso-curr
                    move "FEBBRAIO"  to mese-esteso-prev
                    move "APRILE"    to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 04  move "APRILE"    to mese-esteso-curr
                    move "MARZO"     to mese-esteso-prev
                    move "MAGGIO"    to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 05  move "MAGGIO"    to mese-esteso-curr
                    move "APRILE"    to mese-esteso-prev
                    move "GIUGNO"    to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 06  move "GIUGNO"    to mese-esteso-curr
                    move "MAGGIO"    to mese-esteso-prev
                    move "LUGLIO"    to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 07  move "LUGLIO"    to mese-esteso-curr
                    move "GIUGNO"    to mese-esteso-prev
                    move "AGOSTO"    to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 08  move "AGOSTO"    to mese-esteso-curr
                    move "LUGLIO"    to mese-esteso-prev
                    move "SETTEMBRE" to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 09  move "SETTEMBRE" to mese-esteso-curr
                    move "AGOSTO"    to mese-esteso-prev
                    move "OTTOBRE"   to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 10  move "OTTOBRE"   to mese-esteso-curr
                    move "SETTEMBRE" to mese-esteso-prev
                    move "NOVEMBRE"  to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 11  move "NOVEMBRE"  to mese-esteso-curr
                    move "OTTOBRE"   to mese-esteso-prev
                    move "DICEMBRE"  to mese-esteso-next 

                    move anno-curr   to anno-next
                    move anno-curr   to anno-prev
                    
           when 12  move "DICEMBRE"  to mese-esteso-curr
                    move "NOVEMBRE"  to mese-esteso-prev
                    move "GENNAIO"   to mese-esteso-next 

                    move anno-curr   to anno-prev
                    add 1 to anno-curr giving anno-next
           end-evaluate.

           accept  PathPod from environment "PATH_POD".
           inspect PathPod replacing trailing spaces by low-value.
           move tor-num-bolla  to NomeFile.
           call "C$JUSTIFY" using NomeFile, "L".
           inspect NomeFile replacing leading x"30" by x"20".
           call "C$JUSTIFY" using NomeFile, "L".
           inspect NomeFile      replacing trailing spaces by low-value.
           inspect vet-sigla-pod replacing trailing spaces by low-value.
           inspect mese-esteso-prev replacing trailing 
                                    spaces by low-value.
           inspect mese-esteso-curr replacing trailing 
                                    spaces by low-value.
           inspect mese-esteso-next replacing trailing 
                                    spaces by low-value.
                                               
           initialize FilePod.
           string PathPod          delimited low-value
                  anno-prev        delimited size
                  "\"              delimited size
                  mese-esteso-prev delimited low-value
                  "\"              delimited size
                  vet-sigla-pod    delimited low-value
                  "\"              delimited size
                  NomeFile         delimited low-value
                  ".pdf"           delimited size
                  into FilePod
           end-string.
       
           initialize file-info.
           CALL "C$FILEINFO" using FilePod, FILE-INFO
                            giving status-code.

           if status-code not = 0
              initialize FilePod
              string PathPod          delimited low-value
                     anno-curr        delimited size
                     "\"              delimited size
                     mese-esteso-curr delimited low-value
                     "\"              delimited size
                     vet-sigla-pod    delimited low-value
                     "\"              delimited size
                     NomeFile         delimited low-value
                     ".pdf"           delimited size
                     into FilePod
              end-string
       
              initialize file-info
              CALL "C$FILEINFO" using FilePod, FILE-INFO
                               giving status-code
                                
              if status-code not = 0             
                 initialize FilePod
                 string PathPod          delimited low-value
                        anno-next        delimited size
                        "\"              delimited size
                        mese-esteso-next delimited low-value
                        "\"              delimited size
                        vet-sigla-pod    delimited low-value
                        "\"              delimited size
                        NomeFile         delimited low-value
                        ".pdf"           delimited size
                        into FilePod
                 end-string
       
                 initialize file-info
                 CALL "C$FILEINFO" using FilePod, FILE-INFO
                                  giving status-code
                
                 if status-code = 0
                    move "S" to ttrs-pod
                 end-if

              else           
                 move "S" to ttrs-pod
              end-if

           else
              move "S" to ttrs-pod
           end-if.                  

      ***---
       READ-DESTINI.
           initialize des-rec.
           move trs-cliente to des-codice.
           move trs-destino to des-prog
                               ttrs-destino.
           read destini no lock
                invalid move "** NON TROVATO **" to des-ragsoc-1
           end-read.
           move des-ragsoc-1 to ttrs-dest-desc.
           move des-localita to ttrs-dest-citta.

      ***---
       INIZIALIZZA-RIGA.
           initialize ttrs-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
      ***---
       VALORIZZA-RIGA.
           add 1             to prg-riga.
           set trovato       to true.

           move prg-riga            to ttrs-prog.

           move trs-data-bolla(3:2) to data6(5:2).
           move trs-data-bolla(5:2) to data6(3:2).
           move trs-data-bolla(7:2) to data6(1:2).
           move data6 to ttrs-data-bolla.

           move trs-num-bolla       to ttrs-num-bolla
           initialize vet-rec
           move trs-vettore         to vet-codice
           read tvettori no lock invalid continue end-read.
           move vet-codice          to ttrs-vettore.
           move vet-descrizione     to ttrs-vet-desc.

           initialize cli-rec
           set  cli-tipo-C  to true.
           move trs-cliente to cli-codice
                               ttrs-cliente.
           read clienti no lock
                invalid move "** NON TROVATO **" to cli-ragsoc-1
           end-read.
           move cli-ragsoc-1 to ttrs-cli-desc.

           initialize reg-rec.
           move trs-regione to reg-codice.

           read tregioni no lock invalid continue end-read.
           move reg-descrizione to ttrs-regione.

           move trs-provincia to ttrs-provincia.

           move trs-qta-kg    to ttrs-qta-kg.
           move trs-qta-arrot to ttrs-qta-arrot.
           move trs-tariffa   to ttrs-tariffa.
           compute com-importo = trs-qta-arrot * trs-tariffa.
           move com-importo   to ttrs-importo.
           
           move trs-qta-kg-SHI    to ttrs-qta-kg-SHI.
           move trs-qta-arrot-SHI to ttrs-qta-arrot-SHI.
           move trs-tariffa-SHI   to ttrs-tariffa-SHI.
           compute com-importo = trs-qta-arrot-SHI * trs-tariffa-SHI.
           move com-importo   to ttrs-importo-SHI.

           move trs-qta-kg-GET    to ttrs-qta-kg-GET.
           move trs-qta-arrot-GET to ttrs-qta-arrot-GET.
           move trs-tariffa-GET   to ttrs-tariffa-GET.
           compute com-importo = trs-qta-arrot-GET * trs-tariffa-GET.
           move com-importo   to ttrs-importo-GET.

           set tec-forza-pod-no to true.
           move trs-note to ttrs-note.
           set trovata-bozza-reso to false.
           |23/05/2012 
           move spaces               to ttrs-esito.
           move trs-num-bolla        to tor-num-bolla.
           move trs-data-bolla(1:4)  to tor-anno-bolla.
           read tordini key k-bolla
                invalid continue
            not invalid
                |Cerco la bozza di reso prima tramite bolla, poi tramite fattura
                move tor-anno-bolla to btno-anno-bolla
                move tor-num-bolla  to btno-num-bolla
                read btnotacr key k-bolla
                     invalid
                     move tor-anno-fattura to btno-anno-fatt
                     move tor-num-fattura  to btno-num-fatt
                     read btnotacr key k-fattura
                          invalid continue
                      not invalid
                          set trovata-bozza-reso to true
                     end-read
                 not invalid     
                     set trovata-bozza-reso to true
                end-read
                if trovata-bozza-reso
                   set trovata-bozza-reso to false
                   move btno-causale to tca-codice
                   read tcaumag no lock invalid continue end-read
                   if tca-movim-resi-cli-pos
                      set trovata-bozza-reso to true
                   end-if
                end-if

                move tor-chiave to eor-tor-chiave
                move high-value to eor-num-riga
                start eordini key <= eor-chiave
                      invalid continue
                  not invalid
                      move 0 to como-data
                      perform until 1 = 2
                         read eordini previous 
                              at end exit perform 
                         end-read
                         if eor-tor-chiave not = tor-chiave
                            exit perform
                         end-if
                         if eor-dt-consegna > como-data
                            move eor-dt-consegna to como-data
                            move eor-esito to tec-codice
                            read tescons no lock
                                 invalid 
                                 move eor-esito to tec-descrizione
                            end-read
                            move tec-descrizione to ttrs-esito
                         end-if
                      end-perform
                end-start
           end-read.
           |23/05/2012
           if tec-forza-pod-si and tor-anno-bolla not = 0
              move "S" to ttrs-pod
           else
              perform TROVA-POD
           end-if.

           |26/08/2016: se trovo una bozza di reso collegata
           if trovata-bozza-reso and ttrs-pod = "N"
              move btno-numero to como-numero-x
              call "C$JUSTIFY" using como-numero-x, "L"
              inspect como-numero-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using como-numero-x, "L"
              inspect como-numero-x 
                      replacing trailing spaces  by low-value
              move "S" to ttrs-pod
              initialize ttrs-esito
              string "MERCE RIENTRATA " delimited size
                     "BOZZA NC "        delimited size
                     como-numero-x      delimited low-value
                into ttrs-esito
              end-string
           end-if.

           write ttrs-rec invalid continue end-write.

      ***---
       CLOSE-FILES.
           close trasporti tvettori clienti destini tregioni eordini
                 tordini tescons btnotacr tcaumag.
           close tmp-trasporti.
           delete file tmp-trasporti.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
