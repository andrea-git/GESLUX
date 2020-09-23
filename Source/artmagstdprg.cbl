       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      artmagstdprg.
       AUTHOR.                          Andrea.
       REMARKS. 
           Data una lista di codici articoli (art-lista.csv in "Archivi")
           cambia il magazzino std in EXD e crea un relativo progressivo
           con peso ed imballo std e mag EXD

  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  status-articoli          pic xx.
       77  status-progmag           pic xx.
       77  status-lineseq           pic xx.
       77  wstampa                  pic x(256).

       78  78-mag                   value "EXD".

       01  controlli                pic xx.    
         88 tutto-ok                value "OK".
         88 errori                  value "ER". 

       01  filler                   pic 9.    
         88 RecLocked               value 1, false 0.
                                                      
       77  nRec                     pic 9(10) value 0.
       77  nRecLock                 pic 9(10) value 0.
       77  nRecPrgOk                pic 9(10) value 0.
       77  nRecPrgKo                pic 9(10) value 0.
       77  nRecArt                  pic 9(10) value 0.
       77  nRecKo                   pic 9(10) value 0.

       77  r-art-codice             pic x(5).

       78  titolo                   value "Lista articoli".

       PROCEDURE DIVISION.
       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           if status-articoli = "99"
              set RecLocked to true
           end-if.
       END DECLARATIVES.

      ***---
       MAIN.
           perform INIT-PGM.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT-PGM.
           move "art-lista.csv" to wstampa.
           set tutto-ok  to true.
           set RecLocked to false.

      ***---
       OPEN-FILES.
           open input lineseq.
           if status-lineseq not = "00"
              display message "File input: " wstampa " non trovato."
                       x"0d0a""Esecuzione interrotta"
                       title titolo
                        icon 3
              set errori to true
              goback
           end-if.
           open i-o articoli.
           open i-o progmag.

      ***---
       ELABORAZIONE.
           read lineseq next.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to nRec
              move line-riga to r-art-codice
              call "C$JUSTIFY" using r-art-codice, "R"
              inspect r-art-codice replacing leading x"20" by x"30"
              move r-art-codice to art-codice
              read articoli no lock
                   invalid 
                   display message 
                           "Articolo: " art-codice " non trovato."
                             title titolo
                              icon 3
                   add 1 to nRecKo
               not invalid
                   set RecLocked to false
                   read articoli lock
                   if RecLocked
                      display message "Articolo: " art-codice " in uso."
                                title titolo
                                 icon 3
                      add 1 to nRecLock
                      exit perform cycle
                   end-if
                   move 78-mag to art-mag-std
                   rewrite art-rec
                          invalid continue
                      not invalid 
                          add 1 to nRecArt
                          perform AGGIUNGI-PROGRESSIVO
                   end-rewrite
                   unlock articoli all records
              end-read
           end-perform.

      ***---
       AGGIUNGI-PROGRESSIVO.
           initialize prg-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move art-codice           to prg-cod-articolo.
           move 78-mag               to prg-cod-magazzino.
           move art-imballo-standard to prg-tipo-imballo.
           compute prg-peso = art-peso-utf + art-peso-non-utf.
           move art-peso-utf         to prg-peso-utf.
           move art-peso-non-utf     to prg-peso-non-utf.
           set prg-attivo to true.
           accept prg-ora-creazione  from time.
           accept prg-data-creazione from century-date.
           move "BATCH" to prg-utente-creazione.
           write prg-rec
                 invalid 
                 add 1 to nRecPrgKo
             not invalid 
                 add 1 to nRecPrgOk
           end-write.              

      ***---
       CLOSE-FILES.
           close progmag articoli lineseq.

      ***---
       EXIT-PGM.
           if nRec > 0
              display message "Riepilogo elaborazione"
                       x"0d0a""Record elaborati: " nRec
                       x"0d0a""Articoli già in uso: " nRecLock
                       x"0d0a""Progressivi non creati: " nRecPrgKo
                       x"0d0a""Progressivi creati: " nRecPrgOk
                       x"0d0a""Articoli aggiornati: " nRecArt
                       x"0d0a""Articoli non validi: " nRecKo
                        title titolo
                         icon 2
           end-if.
           goback.
