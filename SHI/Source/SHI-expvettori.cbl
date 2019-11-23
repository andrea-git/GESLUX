       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-expvettori.
       AUTHOR.                          Luciano.
       REMARKS. EXPORT dei vettori
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "fileseq.sl".
           copy "tvettori.sl".
           copy "paramshi.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "fileseq.fd".
           copy "tvettori.fd".
           copy "paramshi.fd".

       WORKING-STORAGE SECTION.
       copy "exp-ws.def".

       78  titolo                value "Export Vettori".

       77  status-lineseq        pic xx.
       77  status-tvettori       pic xx.
       77  status-paramshi       pic xx.
       77  wstampa               pic x(256).

       01  exp-vet.
           05 exp-vet-prefisso     pic x.
           05 exp-vet-codice       PIC  9(2).
           05 exp-vet-descrizione  PIC  X(40).
           05 exp-vet-indirizzo    PIC  X(100).
           05 exp-vet-sigla        PIC  X(3).
           05 exp-vet-url          PIC  x(250).
           05 exp-vet-piva         PIC  x(11).
           05 exp-vet-n-albo       PIC  x(50).


       LINKAGE SECTION.
           copy "link-exp.def".

      ******************************************************************
       PROCEDURE DIVISION using exp-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                move 
            "Impossibile procedere. File vettori [TVETTORI] inesistente"
                    to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TVETTORI] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TVETTORI] Indexed file corrupt!" 
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

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
           perform INITIALIZE-FLAG
           set exp-vettori      to true.

           set tutto-ok         to true.
           open input paramshi.
           move space  to shi-codice
           read paramshi
              invalid
                 continue
           end-read
           close paramshi
           inspect shi-path-exp replacing trailing space by low-value

           string shi-path-exp           delimited by low-value
                  "\"                    delimited by size
                  shi-file-vettori       delimited by size
                  into wstampa.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input tvettori
           end-if.

      ***---
       ELABORAZIONE.
           move low-value to vet-codice.
           start tvettori key is >= vet-chiave
              invalid  
                 set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tvettori next 
                    at end       
                       exit perform 
                 end-read
                 if vet-codice not = zero and vet-codice not = space
                    perform GENERA-FILE
                 end-if
              end-perform
           end-if.
      

           perform SCRIVI-RIEPILOGO.

      ***---
       SCRIVI-RIEPILOGO.       
           initialize como-messaggio
           move num-rec-exp  to num-rec-ed
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "  delimited by size
                   num-rec-ed   delimited by low-value
                   " record."   delimited by size
                   into como-messaggio
           perform SCRIVI-MESSAGGIO.

      ***---
       GENERA-FILE.
           add 1                to num-rec-exp
           move "L"             to exp-vet-prefisso  
           move vet-codice      to exp-vet-codice      
           move vet-descrizione to exp-vet-descrizione 
           move vet-indirizzo   to exp-vet-indirizzo   
           move vet-sigla       to exp-vet-sigla       
           move vet-url         to exp-vet-url         
           move vet-piva        to exp-vet-piva        
           move vet-n-albo      to exp-vet-n-albo      

           move exp-vet   to line-riga
           write line-riga.

      ***---
       CLOSE-FILES.
           close tvettori lineseq.
  
      ***---
           COPY "exp-procedure.cpy".
