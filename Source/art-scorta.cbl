       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          art-scorta.
       AUTHOR.              Andrea.
       REMARKS.
           Gli articoli BLOCCATI --> scorta 0

           Gli articoli ATTIVI:                               
           - scorta 2 con Giacenza + Ordinato > 0 --> scorta 2
           - scorta 0 con Giacenza + Ordinato <= 0 --> scorta 0
           giacenza dinamica diversa da ROT
           
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "progmag.sl". 
           COPY "articoli.sl".
           COPY "lineseq.sl".
           copy "timbalqta.sl".
           copy "tmagaz.sl".

       DATA DIVISION.
       FILE SECTION.
           COPY "progmag.fd". 
           COPY "articoli.fd".
           COPY "lineseq.fd".  
           copy "timbalqta.fd".
           copy "tmagaz.fd".

       WORKING-STORAGE      SECTION.
       78  titolo value "Batch articoli scorta 0/2".
       
       77  status-articoli   pic xx.
       77  status-progmag    pic xx.
       77  status-timbalqta  pic xx. 
       77  status-lineseq    pic xx.
       77  status-tmagaz     pic xx.
       77  wstampa           pic x(256).

       01  r-inizio              pic x(25).

       77  como-riga    pic x(200).
       77  riga-stampa  pic x(200).

       01  controllo    pic xx.    
         88 tutto-ok    value "OK".
         88 errori      value "ER".
                                         
       77  filler       pic 9.
         88 RecLocked   value 1, false 0.
       77  filler       pic 9.
         88 RichiamoSchedulato value 1, false 0.
                                 
       77  como-data    pic 9(8).
       77  como-ora     pic 9(8). 
       77  giacenza     pic s9(8).
       77  ordinato     pic s9(8).
       77  somma        pic s9(8).

       77  nargs        pic 99 comp-1 value 0.
                                 
       77  counter      pic 9(9).
       77  counter2     pic 9(9).
       77  counter-edit pic zzz.zzz.zz9.

       LINKAGE SECTION.
       copy "link-batch.def".

       PROCEDURE DIVISION USING batch-linkage.
                
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
           set tutto-ok to true.
           call "C$NARG" using nargs.
           if nargs not = 0
              set RichiamoSchedulato to true
           else                             
              set RichiamoSchedulato to false
           end-if.                
           initialize wstampa.
           if RichiamoSchedulato        
              move 0 to batch-status
              accept  wstampa  from environment "SCHEDULER_PATH_LOG"
           else                                                     
              accept  wstampa  from environment "PATH_ST"
           end-if.                
           accept como-data from century-date.
           accept como-ora  from time.
           inspect wstampa  replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "ART-SCORTA_" delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".log"        delimited size
              into wstampa
           end-string.              
           if RichiamoSchedulato    
              move wstampa to batch-log
           end-if.
           open output lineseq.

      ***---
       OPEN-FILES.
           open i-o   articoli.
           open input progmag timbalqta tmagaz.
           if RichiamoSchedulato and errori
              move -1 to batch-status
           end-if.

      ***---
       ELABORAZIONE.              
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           move 0 to counter counter2.
           move low-value to art-rec. 
           start articoli key >= art-chiave.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-bloccato or art-disattivo
                 move 0 to art-scorta
              else
                 if art-scorta = 0 or
                    art-scorta = 2 
                    move 0 to giacenza ordinato
                    move low-value  to prg-chiave
                    move art-codice to prg-cod-articolo
                    start progmag key >= prg-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read progmag next 
                                  at end exit perform 
                             end-read              
                             if prg-cod-articolo not = art-codice
                                exit perform
                             end-if   
                             if prg-cod-magazzino = spaces
                                exit perform cycle
                             end-if                
                             if RichiamoSchedulato
                                perform CONTATORE-VIDEO
                             end-if
                             move prg-cod-magazzino to mag-codice
                             read tmagaz no lock
                                  invalid continue
                              not invalid
                                  if mag-per-promo-si 
                                     add prg-giacenza   to giacenza
                                     add prg-ordinato-6 to ordinato
                                  end-if
                             end-read
                          end-perform
                    end-start
                    move art-imballo-standard to imq-codice
                    read timbalqta no lock      

                    initialize como-riga
                    compute somma = giacenza + ordinato
                    evaluate art-scorta also somma
                    when 0 also >= imq-qta-imb
                         move 2 to art-scorta
                         rewrite art-rec    
                         string "ELABORATO ARTICOLO: " delimited size
                                art-codice             delimited size
                                " - SCORTA: "          delimited size
                                art-scorta             delimited size
                                " - QTA IMBALLI: "     delimited size
                                imq-qta-imb            delimited size
                                " - GIACENZA: "        delimited size
                                giacenza               delimited size
                                " - ORDINATO: "        delimited size
                                ordinato               delimited size
                                " - DA SCORTA 0 a 2"   delimited size
                           into como-riga
                         end-string                   
                    when 2 also < imq-qta-imb
                         move 0 to art-scorta
                         rewrite art-rec          
                         string "ELABORATO ARTICOLO: " delimited size
                                art-codice             delimited size
                                " - SCORTA: "          delimited size
                                art-scorta             delimited size
                                " - QTA IMBALLI: "     delimited size
                                imq-qta-imb            delimited size
                                " - GIACENZA: "        delimited size
                                giacenza               delimited size
                                " - ORDINATO: "        delimited size
                                ordinato               delimited size
                                " - DA SCORTA 2 a 0"   delimited size
                           into como-riga
                         end-string           
                    when other                    
                         string "ELABORATO ARTICOLO: "    delimited size
                                art-codice                delimited size
                                " - SCORTA: "             delimited size
                                art-scorta                delimited size
                                " - QTA IMBALLI: "        delimited size
                                imq-qta-imb               delimited size
                                " - GIACENZA: "           delimited size
                                giacenza                  delimited size
                                " - ORDINATO: "           delimited size
                                ordinato                  delimited size
                                " - NESSUNA SOSTITUZIONE" delimited size
                           into como-riga 
                         end-string        
                    end-evaluate           
                    perform SETTA-RIGA-STAMPA
                 end-if
              end-if
           end-perform.
           move "FINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       CLOSE-FILES.
           close tmagaz progmag articoli timbalqta.

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2.
           if counter2 = 300
              move counter to counter-edit
              display counter-edit
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
             into riga-stampa
           end-string.
           write line-riga of lineseq from riga-stampa.

      ***---
       EXIT-PGM.
           if RichiamoSchedulato
              close lineseq
              display "                                          "
                 upon batch-win-handle
                 line 25,00
               column 35,00
           end-if.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
