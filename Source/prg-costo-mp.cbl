       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          prg-costo-mp.
       AUTHOR.              Andrea.
       REMARKS.
           Calcola il valore cel costo medio e lo inserisce in tabella.
           
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "progmag.sl". 
           COPY "lineseq.sl".
           COPY "articoli.sl".   
           copy "timposte.sl".
           copy "tmarche.sl".

       DATA DIVISION.
       FILE SECTION.
           COPY "progmag.fd".
           COPY "lineseq.fd".
           COPY "articoli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".

       WORKING-STORAGE      SECTION.
       copy "costo-medio.def".      
       copy "imposte.def".  
       77  status-progmag    pic xx.
       77  status-lineseq    pic xx.
       77  status-articoli   pic xx.
       77  status-timposte   pic xx.
       77  status-tmarche    pic xx.
       77  wstampa           pic x(256).

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

       77  como-riga    pic x(200).
       77  riga-stampa  pic x(200).
       77  tipo         pic x(20).
       77  costo-mp-z   pic ----.---.--9,99.
       77  prg-peso-z   pic --.--9,99.
       77  art-errato   pic 9(6).

       01  controllo    pic xx.    
         88 tutto-ok    value "OK".
         88 errori      value "ER".
                                         
       77  filler       pic 9.
         88 RecLocked   value 1, false 0.
       77  filler       pic 9.
         88 RichiamoSchedulato value 1, false 0.
                                 
       77  como-data    pic 9(8).
       77  como-ora     pic 9(8). 
                                         
       77  n-agg        pic 9(7) value 0.
       77  n-err        pic 9(7) value 0.
       77  n-elab       pic 9(7) value 0.

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
           string  wstampa         delimited low-value
                   "PRG-COSTO-MP_" delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".log"          delimited size
              into wstampa
           end-string.              
           if RichiamoSchedulato    
              move wstampa to batch-log
           end-if.
           open output lineseq.

      ***---
       OPEN-FILES.
           open i-o progmag.
           open input articoli tmarche timposte.
           if RichiamoSchedulato and errori
              move -1 to batch-status
           end-if.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.    
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           move 0 to counter counter2.
           move low-value to prg-rec. 
           move 0 to art-codice art-errato.
           start progmag key >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              if RichiamoSchedulato
                 perform CONTATORE-VIDEO
              end-if  
              add 1 to n-elab

              if prg-cod-articolo = art-errato
                 exit perform cycle
              end-if

              if prg-cod-articolo not = art-codice
                 move prg-cod-articolo to art-codice
                 read articoli no lock
                      invalid
                      move prg-cod-articolo to art-errato
                      initialize como-riga
                      string "***!! ERRORE !!*** ARTICOLO: "   
                                                 delimited size
                             art-codice          delimited size
                             " NON TROVATO ****" delimited size
                        into como-riga
                      end-string
                      perform SETTA-RIGA-STAMPA
                      set errori to true
                      exit perform cycle
                 end-read
              end-if
              add 1 to n-agg

              perform CALCOLA-COSTO-MP-COMPLETO    
              add 0,005 to costo-mp giving costo-mp-2dec
              move costo-mp-2dec to prg-costo-mp costo-mp-z

              evaluate true
              when recupero-iniziale   move "(INIZIALE)"   to tipo
              when recupero-anagrafica move "(ANAGRAFICA)" to tipo
              when recupero-ini        move "(INIZIALI)"   to tipo
              when recupero-acq        move "(ACQUISTO)"   to tipo
              when recupero-normale    move "(NORMALE)"    to tipo
              when recupero-ultimo     move "(ULTIMO)"     to tipo
              end-evaluate
                                 
              move prg-peso to prg-peso-z
              initialize como-riga
              string prg-cod-articolo  delimited size
                     "-"               delimited size
                     prg-cod-magazzino delimited size
                     "-"               delimited size
                     prg-tipo-imballo  delimited size
                     "-"               delimited size
                     prg-peso-z        delimited size
                     ": "              delimited size
                     costo-mp-z        delimited size
                     " "               delimited size
                     tipo              delimited size
                into como-riga
              end-string
              perform SETTA-RIGA-STAMPA

              rewrite prg-rec
           end-perform.                         
                   
           initialize como-riga
           string "ELABORATI PROGRESSIVI: " delimited size
                  n-elab                    delimited size
                  " - DI CUI ERRATI:  "     delimited size
                  n-err                     delimited size
                  " - DI CUI AGGIORNATI: "  delimited size
                  n-agg                     delimited size
             into como-riga
           end-string
           perform SETTA-RIGA-STAMPA

           move "FINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       CLOSE-FILES.
           close progmag articoli tmarche timposte.

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
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
