       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-expclassi.
       AUTHOR.                          Luciano.
       REMARKS. EXPORT delle classi degli articoli
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "fileseq.sl".
           copy "tcla1art.sl".
           copy "paramshi.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "fileseq.fd".
           copy "tcla1art.fd".
           copy "paramshi.fd".

       WORKING-STORAGE SECTION.
       copy "link-geslock.def".
       copy "exp-ws.def".

       78  titolo                value "Export Classi Articoli".

       77  status-lineseq        pic xx.
       77  status-tcla1art       pic xx.
       77  status-paramshi       pic xx.
       77  wstampa               pic x(256).

       01  exp-cl1.
           05 exp-cl1-codice       PIC  9(4).
           05 exp-cl1-descrizione  PIC  x(30).



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
           end-evaluate.

      ***---
       TCLA1ART-ERR SECTION.
           use after error procedure on tcla1art.
           set tutto-ok  to true.
           evaluate status-tcla1art
           when "35"
                move "Impossibile procedere."  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TCLA1ART] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TCLA1ART] Indexed file corrupt!"
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
           set exp-classi       to true.
           set tutto-ok         to true.
           open input paramshi.
           move space  to shi-codice
           read paramshi
              invalid
                 continue
           end-read
           close paramshi
           inspect shi-path-elab-exp 
                   replacing trailing space by low-value.

           string shi-path-elab-exp delimited low-value
                  "\"               delimited size
                  shi-file-classi   delimited size
             into wstampa.
           move wstampa to exp-shi-anagrafiche-file-classi.


      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input tcla1art
           end-if.

      ***---
       ELABORAZIONE.
           move low-value to cl1-codice.
           start tcla1art key is >= cl1-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tcla1art next 
                    at end       
                       exit perform 
                 end-read
                 perform GENERA-FILE
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
           move cl1-codice      to exp-cl1-codice      
           move cl1-descrizione to exp-cl1-descrizione 

           move exp-cl1         to line-riga
           write line-riga.

      ***---
       CLOSE-FILES.
           close tcla1art lineseq.
  
      ***---
           COPY "exp-procedure.cpy".
