       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      get-imp-batch.
       AUTHOR.                          Luciano.
       REMARKS. programma di importazione aggiornamento articoli. Batch 
                schedulato sul server.
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
           copy "imp-get-ws.def".

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
      *
      * 01 r-stampa.
      *   05 r-tipo         pic x.
      *   05 r-codice       pic z(6).
      *   05 filler         pic x(3) value " - ".
      *   05 r-descrizione  pic x(30).
      *   05 filler         pic x(2). 
      *   05 r-marca        pic x(25).
      *   05 filler         pic x(2).
      *   05 r-prz          pic ----.--9,99.
      *   05 filler         pic x(2).
      *   05 r-mese         pic x.

       77  status-paramget         pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).
      * 77  path-rep-recupero     pic x(256) value spaces.
                                            

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(100).
       77  riga-stampa           pic x(100).

       01                    pic 9.
           88 imp-ordini     value 1.
           88 imp-articoli   value 2.
           88 imp-carichi    value 3.

       77  como-messaggio pic x(200).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.

       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       77  stato                 pic 9.
           88 nessun-errore      value 1.
           88 ok-recupero        value 2.
           88 errore-ko          value 3.

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
                move "File [paramget] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [paramget] mismatch size!" to como-riga
                set errori to true
           when "98"
                move "[paramget] Indexed file corrupt!" to como-riga
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           else
              set errore-ko to true
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           set tutto-ok      to true.
           set nessun-errore to true.
           set prima-volta   to true.

           accept imp-user      from environment "USER_IMPORT_ARTICOLI".

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
                 continue
           end-read

           set   tutto-ok to true
           move "CONTROLLO CARTELLE DI EXPORT"  to como-riga
           perform SETTA-RIGA-STAMPA

           perform CHECK-CARTELLE

           if tutto-ok
              perform PRE-FTP
           else
              move 
              "IMPOSSIBILE CONTINUARE: CARTELLE DI IMPORT INESISTENTI"  
                       to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       PRE-FTP.
      *    per prima cosa controllo che non ci sia già il semaforo
           set ftp-contr-sem-imp to true
           call   "get-esegui-ftp" using esegui-ftp-linkage
           cancel "get-esegui-ftp" 

           if ftp-no-sem 
              move "Impossibile Importare da FTP. File non pronti"
                                                  to como-riga
              perform SETTA-RIGA-STAMPA
              set errori  to true
           else
              perform LEGGI-FTP
           end-if.

           if tutto-ok
              perform PRE-IMPORTA
           end-if.

      ***---
       LEGGI-FTP.
           move "DOWNLOAD DATI SITO FTP."   to como-riga
           perform SETTA-RIGA-STAMPA

           set ftp-import to true
           call   "get-esegui-ftp" using esegui-ftp-linkage
           cancel "get-esegui-ftp" 

           if ftp-ok| = zero
              move "RIMOZIONE SEMAFORO SITO FTP."   to como-riga
              perform SETTA-RIGA-STAMPA
              set ftp-togli-sem-imp to true
              call   "get-esegui-ftp" using esegui-ftp-linkage
              cancel "get-esegui-ftp" 
           end-if.

      ***---
       PRE-IMPORTA.
           move "CREAZIONE FILE LOG IMPORTAZIONE"   to como-riga
           perform SETTA-RIGA-STAMPA
           perform CREA-LOG              
           if tutto-ok
              move wstampa   to imp-path-log
              perform IMPORTA
           else
              move "IMPOSSIBILE CREARE IL FILE DI LOG"   to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       IMPORTA.
           if get-file-articoli-imp = space 
              move "Importazione Articoli impossibile! Nome file non val
      -            "orizzato"  to como-riga
              perform SETTA-RIGA-STAMPA
           else
              perform IMPORT-ARTICOLI
           end-if.

           if get-file-tordini-imp = space or
              get-file-rordini-imp = space
              move "Importazione Oridni impossibile! Nome file non val
      -            "orizzato"  to como-riga
              perform SETTA-RIGA-STAMPA
           else
              perform IMPORT-ORDINI
           end-if.

      ***---
       IMPORT-ARTICOLI.
           set tutto-ok   to true
           initialize como-riga
           move "INIZIO IMPORTAZIONE ARTICOLI" to como-riga
           perform SETTA-RIGA-STAMPA.
           
           if tutto-ok
              perform IMPORT-FILE
           end-if.

           move "FINE IMPORTAZIONE ARTICOLI"   to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       IMPORT-ORDINI.
           set tutto-ok   to true
           initialize como-riga
           move "INIZIO IMPORTAZIONE ORDINI" to como-riga
           perform SETTA-RIGA-STAMPA.
           
           if tutto-ok
              perform IMPORT-FILE-ORDINI
           end-if.

           move "FINE IMPORTAZIONE ORDINI"   to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       IMPORT-FILE.
           inspect get-file-articoli-imp 
                                replacing trailing space by low-value
           call   "get-imparticoli" using imp-linkage
           cancel "get-imparticoli" 
           if imp-status = zero
              string "FILE: "   delimited by size
                     get-file-articoli-imp  delimited by low-value
                     " IMPORTATO CORRETTAMENTE"   delimited by size
                     into como-riga
           else
              string "FILE: "   delimited by size
                     get-file-articoli-imp  delimited by low-value
                     " IMPORTATO CON ERRORI"  delimited by size
                     into como-riga
              set errori  to true
           end-if.
           perform SETTA-RIGA-STAMPA

           if tutto-ok
              move "INIZIO ARCHIVIAZIONE DATI" to como-riga
              perform SETTA-RIGA-STAMPA
              
              perform COPIA-ARTICOLI

              if RENAME-STATUS = ZERO or 2
                 move "FILE ARCHIVIATO" to como-riga
              else
                 move "ARCHIVIAZIONE FALLITA" to como-riga
              end-if
              perform SETTA-RIGA-STAMPA

           end-if.

      ***---
       IMPORT-FILE-ORDINI.
           inspect get-file-tordini-imp 
                                replacing trailing space by low-value
           inspect get-file-rordini-imp 
                                replacing trailing space by low-value
           call   "get-impordini" using imp-linkage
           cancel "get-impordini" 
           if imp-status = zero
              string "FILE: "   delimited by size
                     get-file-tordini-imp   delimited by low-value
                     " e "                  delimited by size
                     get-file-rordini-imp   delimited by low-value
                     " IMPORTATI CORRETTAMENTE"   delimited by size
                     into como-riga
           else
              string "FILE: "   delimited by size
                     get-file-tordini-imp   delimited by low-value
                     " e "                  delimited by size
                     get-file-rordini-imp   delimited by low-value
                     " IMPORTATI CON ERRORI"  delimited by size
                     into como-riga
              set errori  to true
           end-if.
           perform SETTA-RIGA-STAMPA

           if tutto-ok
              move "INIZIO ARCHIVIAZIONE DATI" to como-riga
              perform SETTA-RIGA-STAMPA
              
              perform COPIA-ORDINI

              if RENAME-STATUS = ZERO or 2
                 move "FILE ARCHIVIATO" to como-riga
              else
                 move "ARCHIVIAZIONE FALLITA" to como-riga
              end-if
              perform SETTA-RIGA-STAMPA

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
           string  "IMPOSSIBILE APRIRE DIRECTORY: "  delimited size,
                   MSG-Folder-Name                  delimited LOW-VALUE,
              into como-riga.


      ***---
       PARAGRAFO-COPY.
           copy "imp-get-procedure.cpy".           
