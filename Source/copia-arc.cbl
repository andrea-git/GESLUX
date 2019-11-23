       IDENTIFICATION DIVISION.
       PROGRAM-ID.         copia-arc.
       REMARKS. Lanciato ad ogni consolidamento.

       SPECIAL-NAMES. decimal-point is comma.

      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".

       77  status-tparamge  pic xx.
       78  titolo           value "Copia Archivi".

       01  file-info. 
           02  file-size    pic x(8) comp-x value 0. 
           02  file-date    pic 9(8) comp-x value 0. 
           02  file-time    pic 9(8) comp-x value 0.
        
       77  status-call      signed-short.

       77  LogSuServer      pic x(300).
       77  PercorsoRar      pic x(256) value spaces.
       77  PathArchivi      pic x(256) value spaces.
       77  PathToZip        pic x(256) value spaces.
       77  PathToLog        pic x(256) value spaces.
       77  PathObject       pic x(256) value spaces.
       77  ArchivioRar      pic x(256) value spaces.
       77  comando          pic x(150) value spaces.
       77  dir-handle       usage handle.
       77  FileOrigLogin    pic x(150) value spaces.
       77  FileDestLogin    pic x(150) value spaces.
       77  FileOrigGordc    pic x(150) value spaces.
       77  FileDestGordc    pic x(150) value spaces.
       77  FileOrigGordcvar pic x(150) value spaces.
       77  FileDestGordcvar pic x(150) value spaces.
       77  FileOrigGmovmag  pic x(150) value spaces.
       77  FileDestGmovmag  pic x(150) value spaces.
       77  FileOrigGmovcvar pic x(150) value spaces.
       77  FileDestGmovcvar pic x(150) value spaces.

       LINKAGE SECTION.
       copy "link-copia-arc.def".

      ******************************************************************
       PROCEDURE DIVISION using copia-arc-linkage.

      ***---
       MAIN-PRG.
           perform INIT.
           if status-call = 0
              perform CHECK-ESISTENZA-FOLDER-TO-ZIP
              if status-call = 0
                 perform RESETTA-LOG
                 if status-call = 0
                    perform COPIA-ARCHIVI
                 end-if
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to link-status status-call.
           accept  PercorsoRar from environment "PATH_RAR".
           accept  PathArchivi from environment "PATH_ARCHIVI".
           accept  PathToZip   from environment "PATH_TO_ZIP".
           accept  PathToLog   from environment "PATH_TO_LOG".
           accept  PathObject  from environment "PATH_OBJECT".
           open input  tparamge.
           move spaces to tge-chiave.
           read  tparamge no lock invalid continue end-read.
           close tparamge.

           if PercorsoRar = spaces or 
              PathArchivi = spaces or
              PathToZip   = spaces or
              PathToLog   = spaces
              move 1 to status-call
              display message 
              "Valorizzare correttamente le variabili d'ambiente"
              x"0d0a""Contattare assistenza."
                        title titolo
                         icon 2
           end-if.

      ***---
       CHECK-ESISTENZA-FOLDER-TO-ZIP.
           move 0 to return-code, dir-handle.
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         PathToZip,
                                         "*.*".

           move return-code to dir-handle.
           if dir-handle = 0
              initialize comando
              string "mkdir "  delimited size
                     PathToZip delimited size
                     into comando
              end-string

              perform 10 times
                 call "c$system" using comando, 32
                                giving status-call
                 if status-call = 0
                    move 0 to dir-handle return-code
                    call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                                  PathToZip,
                                                  "*.*"
                    move return-code to dir-handle
                    if dir-handle not = 0
                       call "c$list-directory" using LISTDIR-CLOSE, 
                                                     dir-handle
                       exit perform
                    else
                       move 1 to status-call
                    end-if
                 end-if
              end-perform
              if status-call not = 0
                 display message
                         "Impossibile creare la cartella per la copia." 
                  x"0d0a""Riprovare o contattare assistenza."
                           title titolo
                            icon 2
              end-if
           else
              call "c$list-directory" using LISTDIR-CLOSE, 
                                            dir-handle
           end-if.

      ***---
       RESETTA-LOG.
           initialize file-info replacing numeric data by zeroes
                                     alphanumeric data by spaces.
           call "c$fileinfo" using PathToLog,
                                   file-info,
                            giving status-call
 
           if file-size not = 0
              perform 10 times
                 call "C$DELETE" using PathToLog, "S"
                                giving status-call
                 if status-call = 0
                    exit perform
                 end-if
              end-perform
           else
              move 0 to status-call
           end-if.

           if status-call not = 0
              initialize LogSuServer
              string PathToLog              delimited low-value
                     " presente su server." delimited size
                     into LogSuServer
              end-string

              inspect PathToLog replacing trailing low-value by spaces
              display message 
                      "Impossibile cancellare il file di log:"
               x"0d0a"LogSuServer
               x"0d0a""Cancellarlo a mano o contattare assistenza"
                        title titolo
                         icon 2
           end-if.

      ***---
       COPIA-ARCHIVI.
           inspect PathArchivi replacing trailing spaces by low-value.
           inspect PathToZip   replacing trailing spaces by low-value.
           inspect PathToLog   replacing trailing spaces by low-value.
           inspect PathObject  replacing trailing spaces by low-value.
           inspect PercorsoRar replacing trailing spaces by low-value.

           if link-azzeramento-fine-anno
              string PathToZip                      delimited low-value
                     "Archivi_"                     delimited size
                     "AZZERAMENTO"                  delimited size
                     "_"                            delimited size
                     tge-data-consolid-progmag(1:4) delimited size
                     ".rar"                         delimited size
                     into ArchivioRar
              end-string
           else
              string PathToZip                      delimited low-value
                     "Archivi_"                     delimited size
                     tge-data-consolid-progmag(5:2) delimited size
                     "_"                            delimited size
                     tge-data-consolid-progmag(1:4) delimited size
                     ".rar"                         delimited size
                     into ArchivioRar
              end-string
           end-if.
           call "C$DELETE" using ArchivioRar.

           inspect ArchivioRar replacing trailing spaces by low-value.

           string PercorsoRar                      delimited low-value
                  "rar a "                         delimited size
                  " -ilog "                        delimited size
                  ArchivioRar                      delimited low-value
                  " "                              delimited size
                  PathArchivi                      delimited low-value
                  "*"                              delimited size
                  into comando
           end-string.

           string PathObject      delimited low-value
                  "loginscrn.acu" delimited size
                  into FileOrigLogin
           end-string.

           string PathObject       delimited low-value
                  "xloginscrn.acu" delimited size
                  into FileDestLogin
           end-string.

           string PathObject  delimited low-value
                  "gordc.acu" delimited size
                  into FileOrigGordc
           end-string.

           string PathObject   delimited low-value
                  "xgordc.acu" delimited size
                  into FileDestGordc
           end-string.

           string PathObject    delimited low-value
                  "selordc.acu" delimited size
                  into FileOrigGordcvar
           end-string.

           string PathObject     delimited low-value
                  "xselordc.acu" delimited size
                  into FileDestGordcvar
           end-string.

           string PathObject    delimited low-value
                  "gmovmag.acu" delimited size
                  into FileOrigGmovmag
           end-string.

           string PathObject     delimited low-value
                  "xgmovmag.acu" delimited size
                  into FileDestGmovmag
           end-string.

           string PathObject    delimited low-value
                  "selmovc.acu" delimited size
                  into FileOrigGmovcvar
           end-string.

           string PathObject     delimited low-value
                  "xselmovc.acu" delimited size
                  into FileDestGmovcvar
           end-string.

           call "RENAME" using FileOrigLogin,    FileDestLogin.
           call "RENAME" using FileOrigGordc,    FileDestGordc.
           call "RENAME" using FileOrigGordcvar, FileDestGordcvar.
           call "RENAME" using FileOrigGmovmag,  FileDestGmovmag.
           call "RENAME" using FileOrigGmovcvar, FileDestGmovcvar.

           move 0 to status-call.
           call "C$SYSTEM" using comando
                          giving status-call.

           call "RENAME" using FileDestLogin,    FileOrigLogin.
           call "RENAME" using FileDestGordc,    FileOrigGordc.
           call "RENAME" using FileDestGordcvar, FileOrigGordcvar.
           call "RENAME" using FileDestGmovmag,  FileOrigGmovmag.
           call "RENAME" using FileDestGmovcvar, FileOrigGmovcvar.

           initialize file-info replacing numeric data by zeroes
                                     alphanumeric data by spaces.
           call "c$fileinfo" using PathToLog,
                                   file-info,
                            giving status-call.

           if file-size not = 0
              initialize LogSuServer
              string PathToLog              delimited low-value
                     " presente su server." delimited size
                     into LogSuServer
              end-string
              display message 
                   "Copia degli archivi non riuscita. Riprovare."
            x"0d0a""I dettagli dell'errore sono elencati nel file"
            x"0d0a"LogSuServer
            x"0d0a""Probabile causa: accessi di altri utenti."
            x"0d0a""Contattare assistenza."
                        title titolo
                         icon 2

              move 1 to status-call
           else
              move 0 to status-call
           end-if.

      ***---
       EXIT-PGM.
           move status-call to link-status.
           goback.
