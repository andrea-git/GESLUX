       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-exp-batch.
       AUTHOR.                          Luciano.
       REMARKS. programma di esportazione dati SHI. Batch 
                schedulato sul server. Il programma scrive un file di log 
                genrale .
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "paramshi.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "paramshi.fd".


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "exp-shi-ws.def".

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

       77  status-paramshi       pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).
                                            
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(200).
       77  riga-stampa           pic x(200).

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
       PARAMSHI-ERR SECTION.
           use after error procedure on paramshi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-paramshi 
           when "35"
                move "File [PARAMSHI] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [PARAMSHI] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[PARAMSHI] Indexed file corrupt!" to como-riga
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

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.

           |Lanciando di notte non devo farte 
           |particolari controlli sul lock
           if tutto-ok
              open input paramshi
           end-if.
           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
           else
              move "APERTURA FILES RIUSCITA" to como-riga
           end-if.
           perform SETTA-RIGA-STAMPA.

      ***---
       ELABORAZIONE.
           move space  to shi-codice
           read paramshi
              invalid
                 initialize shi-dati
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
           if shi-file-tordini     = space or 
              shi-file-rordini     = space or
              shi-file-note-ordini = space or
              shi-file-articoli    = space or 
              shi-file-ean         = space or
              shi-file-prodener    = space
              move "Esportazione Ordini Clienti impossibile! Nome file n
      -            "on valorizzato"  to como-riga
              perform SETTA-RIGA-STAMPA
           else
              call   "SHI-expordini" using exp-linkage,
                                           expordini-linkage
              cancel "SHI-expordini" 
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
           call   "SHI-esegui-ftp" using esegui-ftp-linkage
           cancel "SHI-esegui-ftp" 

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
           perform COPIA-FILES.
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
           call   "SHI-agg-ord-exp" using exp-linkage,
                                          expordini-linkage
           cancel "SHI-agg-ord-exp" 

           move "FINE AGGIORNAMENTO ORDINI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       SCRIVI-FTP.
           move "INIZIO SCRITTURA SU SITO FTP"to como-riga           
           perform SETTA-RIGA-STAMPA

           set ftp-export-ord to true
           call   "SHI-esegui-ftp" using esegui-ftp-linkage
           cancel "SHI-esegui-ftp" 

           if ftp-ok| = zero
              move "SCRITTURA SEMAFORO SU SITO FTP"  to como-riga           
              perform SETTA-RIGA-STAMPA
              set ftp-metti-sem-exp   to true 
              call   "SHI-esegui-ftp" using esegui-ftp-linkage
              cancel "SHI-esegui-ftp" 
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
           close paramshi.

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
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           
           goback.


       MSG-DIR-ERR.
           inspect MSG-Folder-Name, 
                   replacing trailing SPACES by LOW-VALUES.
           initialize como-riga.
           string  "IMPOSSIBILE APRIRE DIRECTORY: " delimited size,
                   MSG-Folder-Name                  delimited low-value,
              into como-riga.


      ***---
       PARAGRAFO-COPY.
           copy "exp-shi-procedure.cpy".

