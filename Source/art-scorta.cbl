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

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
           COPY "progmag.sl". 
           COPY "articoli.sl".
           COPY "lineseq.sl".

       DATA                 DIVISION.
       FILE                 SECTION.
           COPY "progmag.fd". 
           COPY "articoli.fd".
           COPY "lineseq.fd".

       WORKING-STORAGE      SECTION.
       78  titolo value "Batch articoli scorta 0/2".
       
       77  status-articoli pic xx.
       77  status-progmag  pic xx.
       77  status-lineseq  pic xx.
       77  wstampa         pic x(256).

       01  r-inizio.
         05 filler      pic x(2) value " [".
         05 r-data.                     
            10 r-gg     pic xx.
            10 filler   pic x value "/".
            10 r-mm     pic xx.
            10 filler   pic x value "/".
            10 r-aa     pic xx.             
         05 filler      pic x(5) value "] - [".
         05 r-ora.
            10 r-hh     pic xx.
            10 filler   pic x value x"22".
            10 r-min    pic xx.
            10 filler   pic x value "'".
            10 r-sec    pic xx.             
         05 filler      pic x(2) value "] ".

       77  como-riga    pic x(100).
       77  riga-stampa  pic x(100).

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
           if RichiamoSchedulato 
              move 0 to batch-status
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa  from environment "SCHEDULER_PATH_LOG"
              inspect wstampa  replacing trailing spaces by low-value
              string  wstampa       delimited low-value
                      "ART-SCORTA_" delimited size
                      como-data     delimited size
                      "_"           delimited size
                      como-ora      delimited size
                      ".log"        delimited size
                 into wstampa
              end-string
              move wstampa to batch-log
              open output lineseq
           end-if.

      ***---
       OPEN-FILES.
           open i-o   articoli.
           open input progmag.
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
                             if RichiamoSchedulato
                                perform CONTATORE-VIDEO
                             end-if
                             if prg-cod-magazzino not = "ROT" and
                                prg-cod-magazzino not = spaces
                                add prg-giacenza   to giacenza
                                add prg-ordinato-6 to ordinato
                             end-if
                          end-perform
                    end-start
                    compute somma = giacenza + ordinato
                    evaluate art-scorta also somma
                    when 0 also > 2
                         move 2 to art-scorta
                         rewrite art-rec
                    when 2 also <= 0
                         move 0 to art-scorta
                         rewrite art-rec
                    end-evaluate
                 end-if
              end-if
           end-perform.
           move "FINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       CLOSE-FILES.
           close progmag articoli.

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
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           else
              display line-riga upon syserr
           end-if.

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.
                                       
           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(3:2) to r-hh.
           move como-ora(5:2) to r-min.
           move como-ora(7:2) to r-sec.

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
