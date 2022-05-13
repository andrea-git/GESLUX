       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rebuild-bat.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmp-listdir.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmp-listdir.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
       77  status-tmp-listdir   pic xx.
       77  wstampa              pic x(256).
       77  path-tmp             pic x(256).

       78  titolo    value "Batch ricostruzione archivi".

      * FLAGS
       01  controlli            pic xx.
         88 tutto-ok            value "OK".
         88 errori              value "ER".

      * VARIABILI
       77  dir-handle           usage handle.
       77  como-data            pic 9(8).
       77  como-ora             pic 9(8).
       77  path-archivi         pic x(256).
       77  nome-file            pic x(100).
       77  como-riga            pic x(80).
       77  CountChar            pic 9(10).
       77  path-vutil           pic x(256).
       77  comando              pic x(200).
       77  tot-chiamate         pic 9(5).
       77  tot-ok               pic 9(5).
       77  tot-ko               pic 9(5).  
       77  tot-chiamate-edit    pic zz.zz9.
       77  tot-ok-edit          pic zz.zz9.
       77  tot-ko-edit          pic zz.zz9.
       77  status-call          signed-short.
       77  status-call-x        pic z(5).

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.

       01  r-inizio              pic x(25).

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       TMP-LISTDIR-ERR SECTION.
           use after error procedure on tmp-listdir.
           set tutto-ok  to true.
           evaluate status-tmp-listdir
           when "39"
                set errori to true
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File [TMP-LISTDIR] mismatch size!"delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           when "98"
                set errori to true
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                     "[TMP-LISTDIR] Indexed file corrupt!"delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           when "35"
                set errori to true
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                    delimited size
                       "[TMP-LISTDIR] inesistente" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           when "93"
           when "99"
                set errori    to true
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                   delimited size
                       "[TMP-LISTDIR] già in uso" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
           end-if.
           if tutto-ok
              perform ELABORAZIONE
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
                    
           move spaces to path-vutil
           accept path-vutil from environment "PATH_VUTIL"
                  on exception
                     set errori to true
                     initialize como-riga
                     string r-inizio delimited size
                    "Variabile di configurazione PATH_VUTIL inesistente"
                            delimited size
                            into como-riga
                     end-string
                     display como-riga upon syserr

                not on exception
                       if path-vutil = spaces
                          set errori to true
                          initialize como-riga
                          string r-inizio delimited size
                "Variabile di configurazione PATH_VUTIL non valorizzata"
                                 delimited size
                                 into como-riga
                          end-string
                          display como-riga upon syserr
                       end-if
           end-accept.
           inspect path-vutil replacing trailing spaces by low-value.

           accept   path-archivi from environment "FILE_PREFIX".
           unstring path-archivi delimited by ";" into path-archivi.

           accept   wstampa      from environment "PATH_ST".
           inspect  wstampa      replacing trailing spaces by low-value.
           string   wstampa      delimited low-values
                    "rebuild"    delimited size
                    "_"          delimited size
                    como-data    delimited size
                    "_"          delimited size
                    como-ora     delimited size
                    ".tmp"       delimited size
                    into path-tmp
           end-string.
           string  wstampa      delimited low-values
                   "rebuild"    delimited size
                   "_"          delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".txt"       delimited size
                   into wstampa
           end-string.

           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       OPEN-FILES.
           open output tmp-listdir.
      
      ***---
       ELABORAZIONE.
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-archivi,
                                         "*.vix".

           move return-code           to dir-handle.

           perform until 1 = 2
               call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                             dir-handle,
                                             nome-file

              if nome-file not = "." and nome-file not = ".."
                 if nome-file = spaces
                    exit perform
                 else
                    
                    move nome-file to tml-nome-file

                    write tml-rec invalid continue end-write
                 end-if
              end-if
           end-perform.

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                         dir-handle.

           close      tmp-listdir.
           open input tmp-listdir.

           move low-value to tml-chiave.
           start tmp-listdir key is >= tml-chiave
                 invalid continue
             not invalid
                 move 0 to tot-chiamate tot-ok tot-ko
                 inspect path-archivi replacing trailing 
                                      spaces by low-value
                 perform until 1 = 2
                    read tmp-listdir next at end exit perform end-read
                    inspect tml-nome-file replacing trailing 
                                          spaces by low-value
                    move 0 to CountChar
                    inspect tml-nome-file  tallying CountChar
                            for characters before low-value
                    move tml-nome-file(1: CountChar - 4) to nome-file
                    inspect nome-file replacing trailing 
                                      spaces by low-value
                    perform SETTA-INIZIO-RIGA
                    initialize como-riga
                    string r-inizio         delimited size
                           "REBUILD FILE: " delimited size
                           nome-file        delimited low-value
                           into como-riga
                    end-string
                    display como-riga upon syserr

                    initialize comando
                    string path-vutil   delimited low-value
                           "vutil32"    delimited size
                           " "          delimited size
                           "-rebuild"   delimited size
                           " -a -q "    delimited size
                           path-archivi delimited low-value
                           nome-file    delimited low-value
                           into comando
                    end-string

                    add 1 to tot-chiamate
                    move 0 to status-call
                    call "C$SYSTEM" using comando
                                   giving status-call
                    initialize como-riga
                    perform SETTA-INIZIO-RIGA
                    if status-call not = 0
                       add 1 to tot-ko
                       move status-call to status-call-x
                       string r-inizio      delimited size
                              "ERRORE "     delimited size
                              status-call-x delimited size
                              into como-riga
                       end-string
                    else
                       add 1 to tot-ok
                       string r-inizio                 delimited size
                              "OPERAZIONE EFFETTUATA!" delimited size
                              into como-riga
                       end-string
                    end-if
                    display como-riga upon syserr
                 end-perform
                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 move tot-chiamate to tot-chiamate-edit
                 move tot-ok       to tot-ok-edit
                 move tot-ko       to tot-ko-edit
                 string r-inizio          delimited size
                        "TOT CHIAMATE: "  delimited size
                        tot-chiamate-edit delimited size
                        " - "             delimited size
                        "ESEGUITE: "      delimited size
                        tot-ok-edit       delimited size
                        " ERRATE: "       delimited size
                        tot-ko-edit       delimited size
                        into como-riga
                  end-string
                  display como-riga upon syserr
           end-start.

      ***---
       CLOSE-FILES.
           close tmp-listdir.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.
           initialize como-riga.
           if tot-secondi < 60
              move tot-secondi to ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                      ss                           delimited size
                      " SECONDI"                   delimited size
                      into como-riga
              end-string
           else
              divide tot-secondi by 60 giving mm remainder ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                      mm                           delimited size
                      " MINUTI E "                 delimited size
                      ss                           delimited size
                      " SECONDI"                   delimited size
                      into como-riga
              end-string
           end-if.
           display como-riga upon syserr.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
