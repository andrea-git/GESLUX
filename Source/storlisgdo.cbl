       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      storlisgdo.
       REMARKS. Storicizzazione listini GDO.
                Data una lista csv, prende le righe presenti e le 
                trasferisce su un altro file.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "listini.sl". 
           copy "lineseq.sl".                 

       SELECT tmp-klis
           ASSIGN       TO path-tmp-klis
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-klis
           RECORD KEY   IS tlst-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "listini.fd".
           copy "lineseq.fd".

       FD  tmp-klis.
       01 tlst-rec.
           05 tlst-chiave.
               10 tlst-gdo          PIC  x(5).
               10 tlst-data         PIC  9(8).
               10 tlst-articolo     PIC  9(6).
           05 tlst-dati.
               10 tlst-cod-art-cli  PIC  x(15).
               10 tlst-prezzo       PIC  9(12)v99.
               10 tlst-comuni.
                   15 tlst-data-creazione           PIC  9(8).
                   15 tlst-ora-creazione            PIC  9(8).
                   15 tlst-utente-creazione         PIC  x(10).
                   15 tlst-data-modifica            PIC  9(8).
                   15 tlst-ora-modifica PIC  9(8).
                   15 tlst-utente-modifica          PIC  x(10).
               10 tlst-vuoti.
                   15 tlst-num-vuoto-1  PIC  9(15).
                   15 tlst-num-vuoto-2  PIC  9(15).
                   15 tlst-num-vuoto-3  PIC  9(15).
                   15 tlst-prg-chiave.
                       20 tlst-prg-cod-articolo         PIC  9(6).
                       20 tlst-prg-cod-magazzino        PIC  X(3).
                       20 tlst-prg-tipo-imballo         PIC  X(3).
                       20 tlst-prg-peso     PIC  9(5)v9(3).
                   15 tlst-alfa-vuoto-1 PIC  x(2).
                   15 tlst-alfa-vuoto-2 PIC  x(20).
                   15 tlst-alfa-vuoto-3 PIC  x(20).

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "comune.def".
           copy "fonts.def".
           copy "link-geslock.def".

       78  titolo    value "Storicizzazione listini GDO".
                  
       77  counter          pic 9(10) value 0.
       77  counter2         pic 9(10) value 0.
       77  counter-edit     pic zzz.zzz.zz9.

       77  n-csv            pic 9(10) value 0.
       77  n-gen            pic 9(10) value 0.
       77  n-csvNotFound    pic 9(10) value 0.
       77  trovati          pic 9(10) value 0.
       77  n-rest           pic 9(10) value 0.
                  
       77  path-tmp-klis    pic x(256).
       77  status-tmp-klis  pic xx.
       77  status-listini   pic xx.  
       77  status-lineseq   pic xx.
       77  wstampa          pic x(256).
       77  path-file-sto    pic x(256).
       77  path-file        pic x(256).
       77  form1-handle     handle of window.
       77  cmd              pic x(500).

       77  r-gdo            pic x(5).
       77  r-art            pic x(6).
       77  como-articolo    pic 9(6).

       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-listini 
           when "39"
                set errori to true
                display message "File [LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.    
       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.                                                   
           accept como-data from century-date.                 
           accept como-ora  from time.                 
           set RecLocked to false.
           set tutto-ok  to true.      
           accept  path-tmp-klis from environment "PATH_ST".
           inspect path-tmp-klis replacing trailing spaces by low-value.
           string  path-tmp-klis delimited low-value
                   "TMP-KLIS_"   delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
              into path-tmp-klis
           end-string.
           inspect path-tmp-klis replacing trailing low-value by spaces.
           
      ***---
       OPEN-FILES.              
           perform OPEN-IO-LISTINI.
           if tutto-ok     
              move "gdoart.csv" to wstampa
              open input lineseq  

              open output tmp-klis
              close       tmp-klis
              open i-o    tmp-klis
           else                 
              goback
           end-if.

      ***---
       OPEN-IO-LISTINI.
           string   "Il file dei listini GDO" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilit� ad"
             x"0d0a""aggiornare gli articoli." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok   to true.
           move "LISTINI" to geslock-nome-file.
           perform until 1 = 2
              set RecLocked to false
              open i-o listini allowing readers
              if not RecLocked exit perform end-if
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
                           exit perform
              end-evaluate
           end-perform.              
      
      ***---
       ELABORAZIONE.
           set ElaborazioneXX to true.
           perform ACCESSOXX.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to n-csv
              unstring line-riga delimited by ";" into r-gdo r-art
              call "C$JUSTIFY" using r-art, "R"
              inspect r-art replacing leading x"20" by x"30"
              move r-art         to como-articolo
              move low-value     to lst-rec
              move r-gdo         to lst-gdo
              move como-articolo to lst-articolo
              move 0 to trovati
              start listini key >= lst-k-articolo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read listini next at end exit perform end-read
                       if lst-gdo      not = r-gdo or
                          lst-articolo not = como-articolo
                          exit perform
                       end-if
                       add 1 to trovati n-gen
                       add 1 to counter counter2
                       if counter2 = 1000
                          move 0 to counter2
                          move counter to counter-edit
                          display "FASE 1 - DELETE LISTINI:  " 
                                  upon form1-handle 
                                  at column  2,00
                                       line  5,00
                          display counter-edit upon form1-handle 
                                  at column 30,00
                                       line  5,00
                       end-if
                       move lst-rec to tlst-rec
                       write tlst-rec          
                       delete listini record end-delete
                    end-perform
              end-start
              if trovati = 0
                 add 1 to n-csvNotFound
              end-if
           end-perform.
           if n-gen > 0    
              accept  path-file from environment "PATH_ARCHIVI"
              inspect path-file 
                      replacing trailing spaces by low-value
              string  path-file delimited low-value
                      "listini" delimited size
                 into path-file
              end-string    
                            
              unlock listini all records
              close listini 
              accept  path-file-sto from environment "PATH_ARCHIVI"
              inspect path-file-sto 
                      replacing trailing spaces by low-value
              string  path-file-sto delimited low-value
                      "listini_sto" delimited size
                 into path-file-sto
              end-string                              
              display "FASE 2 - SPOSTAMENTO LISTINI E CREAZIONE STORICO" 
                      upon form1-handle 
                      at column  2,00
                           line  5,00
              initialize cmd 
              string "move "       delimited size
                     path-file     delimited low-value
                     " "           delimited size
                     path-file-sto delimited low-value
                into cmd
              end-string          stop "K"
              call "C$RUN" using cmd

              open output listini
              move 0 to counter counter2
              move low-value to tlst-rec
              start tmp-klis key >= tlst-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-klis next at end exit perform end-read
                       add 1 to counter counter2 n-rest
                       if counter2 = 1000
                          move 0 to counter2
                          move counter to counter-edit
                          display "FASE 3 - RESTORE LISTINI:  " 
                                  upon form1-handle              
                                  at column  2,00
                                       line  5,00
                          display counter-edit upon form1-handle 
                                  at column 23,00
                                       line  5,00
                       end-if
                       move tlst-rec to lst-rec
                       write lst-rec
                    end-perform
              end-start
           end-if.

           perform DESTROYXX.

      ***---
       CLOSE-FILES.
           close lineseq tmp-klis.
           delete file tmp-klis.

      ***---
       EXIT-PGM.       
           display message "Elaborazione terminata"
                    x"0d0a""- elaborati da csv: " n-csv
                    x"0d0a""- che hanno influito su righe: " n-gen
                    x"0d0a""- non trovati: " n-csvNotFound
                    x"0d0a""- conservati: " n-rest
                    x"0d0a""Creato file: " path-file-sto
                     title titolo
           goback.

           copy "accessoxx.cpy".                                                 