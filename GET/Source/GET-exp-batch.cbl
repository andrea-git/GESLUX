       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      get-exp-batch.
       AUTHOR.                          Luciano.
       REMARKS. programma di esportazione dati get. Batch 
                schedulato sul server. Il programma scrive un file di log 
                genrale .
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "paramget.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "paramget.fd".


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "exp-get-ws.def".

       01  r-inizio              pic x(25).

       77  status-paramget       pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).
                                            
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(200).
       77  riga-stampa           pic x(200).

       01 invio-ftp        PIC  9  VALUE IS 1.
           88 si-invio-ftp VALUE IS 1    WHEN SET TO FALSE  0. 

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       PARAMget-ERR SECTION.
           use after error procedure on paramget.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-paramget 
           when "35"
                move "File [PARAMget] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [PARAMget] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[PARAMget] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[LINESEQ] Indexed file corrupt!"  to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!"                     to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           set tutto-ok      to true.
           set si-invio-ftp  to true.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.

           |Lanciando di notte non devo farte 
           |particolari controlli sul lock
           if tutto-ok
              open input paramget
           end-if.
           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
           else
              move "APERTURA FILES RIUSCITA" to como-riga
           end-if.
           perform SETTA-RIGA-STAMPA.

      ***---
       ELABORAZIONE.
           move space  to get-codice
           read paramget
              invalid
                 initialize get-dati
           end-read.

           move "CREAZIONE FILE DI LOG ESPORTAZIONE"
             to como-riga.
           perform SETTA-RIGA-STAMPA.

           perform CREA-LOG.
      
           if tutto-OK
              move "CREAZIONE FILE DI LOG RIUSCITA"  to como-riga
              perform SETTA-RIGA-STAMPA
              PERFORM PRE-ESPORTA
           else
              move "CREAZIONE FILE DI LOG FALLITA"   to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       PRE-ESPORTA.
           set   tutto-ok to true
           move "CONTROLLO CARTELLE DI EXPORT"  to como-riga
           perform SETTA-RIGA-STAMPA

           perform CHECK-CARTELLE

           if tutto-ok
              perform ESPORTA
           else
              move 
              "IMPOSSIBILE CONTINUARE: CARTELLE DI EXPORT INESISTENTI"  
                       to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       ESPORTA.
           move wstampa   to exp-path-log
           set tutto-ok   to true

           initialize como-riga
           string "PER I DETTAGLI DELL'IMPORTAZIONE " delimited by size
                  "CONSULTARE IL FILE "               delimited by size
                  wstampa                             delimited by size
                  into como-riga
           perform SETTA-RIGA-STAMPA

           perform EXP-ORDINI.

           if tutto-ok
              perform FTP
           end-if.

      ***---
       EXP-ORDINI.
           move "INIZIO ESPORTAZIONE ORDINI CLIENTI"  to como-riga
           perform SETTA-RIGA-STAMPA
           if get-file-tordini     = space or 
              get-file-rordini     = space or
              get-file-note-ordini = space or
              get-file-articoli    = space or 
              get-file-ean         = space or
              get-file-prodener    = space
              move "Esportazione Ordini Clienti impossibile! Nome file n
      -            "on valorizzato"  to como-riga
              perform SETTA-RIGA-STAMPA
           else
              call   "get-expordini" using exp-linkage,
                                           expordini-linkage
              cancel "get-expordini" 
              evaluate true
              when exp-ok
                   move "CREAZIONE FILE TERMINATA"  to como-riga
              when exp-err-bloccante
                   set errori  to true
                   move "CREAZIONE FILE TERMINATA CON ERRORI GRAVI"  
                                                        to como-riga
                   perform SETTA-RIGA-STAMPA
                   move "NON SARA' ESEGUITA LA TRASMISSIONE FTP"  
                                                        to como-riga
              when exp-err
                   move "CREAZIONE FILE TERMINATA CON ERRORI"  
                                                        to como-riga

              end-evaluate
              perform SETTA-RIGA-STAMPA
           end-if.
           move "FINE ESPORTAZIONE ORDINI CLIENTI"  to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       FTP.
      *    per prima cosa controllo che non ci sia già il semaforo
           move "CONTROLLO SEMAFORO SU SITO FTP"  to como-riga
           perform SETTA-RIGA-STAMPA

           set ftp-contr-sem-exp to true
           call   "get-esegui-ftp" using esegui-ftp-linkage
           cancel "get-esegui-ftp" 

           if ftp-si-sem 
              move     "Impossibile Esportare su FTP. File semaforo già 
      -                "presente" to como-riga
              perform SETTA-RIGA-STAMPA
           else
              perform SCRIVI-FTP

           end-if.
      
      ***---
       ARCHIVIAZIONE.
           move "INIZIO ARCHIVIAZIONE DATI" to como-riga
           perform SETTA-RIGA-STAMPA
           set crea-ordini   to true
           perform COPIA-FILES
           if RENAME-STATUS = ZERO or 2
              move "FILE ARCHIVIATO" to como-riga
           else
              move "ARCHIVIAZIONE FALLITA" to como-riga
           end-if
           perform SETTA-RIGA-STAMPA.

      ***---
       AGGIORNA-ORDINI.
           move "INIZIO AGGIORNAMENTO ORDINI" to como-riga
           perform SETTA-RIGA-STAMPA
           call   "get-agg-ord-exp" using exp-linkage,
                                          expordini-linkage
           cancel "get-agg-ord-exp" 

           move "FINE AGGIORNAMENTO ORDINI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       SCRIVI-FTP.
           move "INIZIO SCRITTURA SU SITO FTP"to como-riga           
           perform SETTA-RIGA-STAMPA

           set ftp-export-ord to true
           call   "get-esegui-ftp" using esegui-ftp-linkage
           cancel "get-esegui-ftp" 

           if ftp-ok| = zero
              move "SCRITTURA SEMAFORO SU SITO FTP"  to como-riga           
              perform SETTA-RIGA-STAMPA
              set ftp-metti-sem-exp   to true 
              call   "get-esegui-ftp" using esegui-ftp-linkage
              cancel "get-esegui-ftp" 
           else
              move "SCRITTURA SU SITO FTP FALLITA"  to como-riga           
              perform SETTA-RIGA-STAMPA
              move "Consultare il file LOG_FTP per maggiori dettagli" 
                                                     to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

           move "SCRITTURA SU SITO FTP TERMINATA"    to como-riga           
           perform SETTA-RIGA-STAMPA.

           if ftp-OK
              perform ARCHIVIAZIONE
           end-if.

      ***--
       CLOSE-FILES.
           close paramget.

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.
           display riga-stampa upon syserr.

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           
           goback.


       MSG-DIR-ERR.
           inspect MSG-Folder-Name, 
                   replacing trailing SPACES by LOW-VALUES.
           initialize como-riga.
           string  "IMPOSSIBILE APRIRE DIRECTORY: "  delimited size,
                   MSG-Folder-Name                  delimited LOW-VALUE,
              into como-riga.


      ***---
       PARAGRAFO-COPY.
           copy "exp-get-procedure.cpy".
           copy "setta-inizio-riga.cpy".

