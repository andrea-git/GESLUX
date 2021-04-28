       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-fido-p.
       AUTHOR.                          Andrea.
       REMARKS.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                 
       SELECT lineseq
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.  

      ***** SELECT iniFtp
      *****     ASSIGN       TO  iniFtpPath
      *****     ORGANIZATION IS LINE SEQUENTIAL
      *****     ACCESS MODE  IS SEQUENTIAL
      *****     FILE STATUS  IS STATUS-iniFtp.

       copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                        
       FD  lineseq.
       01 line-riga       PIC  x(20000).

       FD  logfile.
       01 log-riga        PIC  x(900). 

      ***** FD  iniFtp.
      ***** 01 iniFtp-riga        PIC  x(1000). 

       copy "clienti.fd".  

       WORKING-STORAGE SECTION.                                     
      *    COPY              
           copy "acucobol.def".

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

      *    FILE-STATUS                   
       77  status-lineseq        pic xx.
       77  status-logfile        pic xx.
       77  status-clienti        pic xx.
      ***** 77  status-iniFtp         pic xx.

       77  path-logfile          pic x(256).
       77  wstampa               pic x(256).
       77  wstampa2              pic x(256). 
      ***** 77  iniFtpPath            pic x(256).
       77  status-call           signed-short.

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.  
       77  como-fido             pic 9(11)v9(2).
                                         
       77  tot-cli               pic 9(5) value 0.
       77  tot-ok                pic 9(5) value 0.
       77  tot-ko                pic 9(5) value 0.

       77  path-import           pic x(256).    
       77  path-backup           pic x(256).
       77  path-log              pic x(256).
       77  nome-file             pic x(256).
       77  file-backup           pic x(256).  
       77  cmd                   pic x(200).
       77  como-riga             pic x(200).
       77  como-data-2mesi       pic 9(8).  
    
       77  PathGetFTP            pic x(256).
       77  StatusGetFTP          pic s9.

       01  ftp-import.
         03 ftp-server      pic x(50).
         03 ftp-port        pic x(4).
         03 ftp-user        pic x(100).
         03 ftp-password    pic x(100).
         03 ftp-remote-dir  pic x(100).

       77  dir-import-handle     HANDLE.
       77  dir-backup-handle     HANDLE.
       77  dir-log-handle        HANDLE.    
       
       77  nargs                 pic 99  comp-1 value 0.


       01  record-GENERICO.
         05 r-cod-cli               pic x(6).   
         05 r2                      pic x.
         05 r3                      pic x.
         05 r4                      pic x.
         05 r5                      pic x.
         05 r6                      pic x.
         05 r7                      pic x.
         05 r8                      pic x.
         05 r9                      pic x.
         05 r10                     pic x.
         05 r11                     pic x.
         05 r12                     pic x.
         05 r13                     pic x.
         05 r14                     pic x.
         05 r15                     pic x.
         05 r16                     pic x.
         05 r17                     pic x.
         05 r18                     pic x.
         05 r19                     pic x.
         05 r20                     pic x.
         05 r21                     pic x.
         05 r22                     pic x.
         05 r23                     pic x.
         05 r24                     pic x.
         05 r25                     pic x.
         05 r26                     pic x.
         05 r27                     pic x.
         05 r28                     pic x.
         05 r29                     pic x.
         05 r30                     pic x.
         05 r31                     pic x.
         05 r32                     pic x.
         05 r33                     pic x.
         05 r34                     pic x.
         05 r35                     pic x.
         05 r36                     pic x.
         05 r37                     pic x.
         05 r38                     pic x.
         05 r39                     pic x.
         05 r40                     pic x.
         05 r41                     pic x.
         05 r42                     pic x.
         05 r43                     pic x.
         05 r44                     pic x.
         05 r45                     pic x.
         05 r46                     pic x.
         05 r47                     pic x.
         05 r48                     pic x.
         05 r49                     pic x.
         05 r50                     pic x.
         05 r51                     pic x.
         05 r52                     pic x.
         05 r53                     pic x.
         05 r54                     pic x.
         05 r55                     pic x.
         05 r56                     pic x.
         05 r57                     pic x.
         05 r58                     pic x.
         05 r59                     pic x.
         05 r60                     pic x.
         05 r61                     pic x.
         05 r62                     pic x.
         05 r63                     pic x.
         05 r64                     pic x.
         05 r65                     pic x.
         05 r66                     pic x.
         05 r67                     pic x.
         05 r68                     pic x.
         05 r69                     pic x.
         05 r70                     pic x.
         05 r71                     pic x.
         05 r-fido                  pic 9(10).
         05 r-fido-x                pic x(10).

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.

       01 controlli              pic xx.
           88 tutto-ok           value "OK".
           88 errori             value "ER".
      
       01 filler                 pic 9 value 0.
           88 trovato                  value 1, false 0.       

       01 filler                 pic 9 value 0.
           88 RecLocked          value 1, false 0. 

       01  filler                pic 9.
           88 prima-volta        value 1, false 0.   
       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data-i.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler           pic x(2)     value "] ".
                                                        
       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.
       DECLARATIVES.
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           evaluate status-clienti
           when "99" set RecLocked to true
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
           end-if.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                         
           accept como-data from century-date.
           accept como-ora  from time.
                      
           initialize path-import path-backup path-log.
           accept  path-import from environment "PATH_FIDO_IMPORT".
           accept  path-backup from environment "PATH_FIDO_BACKUP".

      *****     accept iniFtpPath   from environment "PATH_FIDO_FTP_INI". 
      *****     open output iniFtp.  
      *****
      *****     accept ftp-server
      *****            from environment "SITUACONT_FTP_SERVER"
      *****     accept ftp-port
      *****            from environment "SITUACONT_FTP_PORT"
      *****     accept ftp-user
      *****            from environment "SITUACONT_FTP_USER"
      *****     accept ftp-password
      *****            from environment "SITUACONT_FTP_PASSWORD"
      *****     accept ftp-remote-dir
      *****            from environment "FIDO_FTP_REMOTE_DIR"
      *****
      *****     inspect ftp-server     replacing trailing spaces by low-value
      *****     inspect ftp-user       replacing trailing spaces by low-value
      *****     inspect ftp-port       replacing trailing spaces by low-value
      *****     inspect ftp-password   replacing trailing spaces by low-value
      *****     inspect ftp-remote-dir replacing trailing spaces by low-value
      *****                          
      *****     initialize iniFtp-riga.
      *****     string "open ftp://" delimited size
      *****            ftp-user      delimited low-value
      *****            ":"           delimited size
      *****            ftp-password  delimited low-value
      *****            "@"           delimited size
      *****            ftp-server    delimited low-value
      *****            ":"           delimited size
      *****            ftp-port      delimited low-value
      *****            " -explicit"  delimited size
      *****       into iniFtp-riga
      *****     end-string.
      *****     write iniFtp-riga.
      *****                       
      *****     initialize iniFtp-riga.
      *****     string "get "         delimited size
      *****            ftp-remote-dir delimited low-value
      *****            "PMITRADE_"    delimited size
      *****            como-data      delimited size
      *****            ".csv "        delimited size
      *****            path-import    delimited size
      *****       into iniFtp-riga
      *****     end-string.
      *****     write iniFtp-riga.
      *****
      *****     move "exit" to iniFtp-riga.
      *****     write iniFtp-riga.
      *****
      *****     close iniFtp.  
      *****
      *****     accept PathGetFTP from environment "PATH_FIDO_FTP_GET".
      *****     move 0 to StatusGetFTP.
      *****     call "C$SYSTEM" using PathGetFTP, 0
      *****                    giving StatusGetFTP.

                                                       
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
              move -1 to batch-status  
              accept path-log from environment "SCHEDULER_PATH_LOG"
           else
              set RichiamoSchedulato to false
              accept path-log from environment "PATH_ST"
           end-if.
           set tutto-ok     to true.
           set prima-volta  to true.   

           compute como-data-2mesi = 
                   function integer-of-date (como-data).

           subtract 60 from como-data-2mesi.                   

           compute como-data-2mesi = 
                   function date-of-integer (como-data-2mesi).
                                                                    
           if path-import = spaces
              set errori to true
           else                 
      *       CONTROLLO L'ESISTENZA DELLA CARTELLA
              call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                            path-import,
                                            "*.*"

              move RETURN-CODE        to Dir-import-Handle

              if Dir-import-Handle = ZERO
                 set errori             to true
              else
      *          cartella di backup
                 call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                               path-backup,
                                               "*.*"
         
                 move RETURN-CODE        to Dir-backup-Handle
                 if dir-backup-handle = 0
                    set errori to true
                 else
                    call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                                  Dir-backup-Handle
      *             cartella di log
                    call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                                  path-log,
                                                  "*.*"
         
                    move RETURN-CODE        to Dir-log-Handle
                    if dir-log-handle = 0
                       set errori to true
                    else
                       call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                                     Dir-log-Handle
                    end-if
                 end-if
              end-if                 
              inspect path-import replacing trailing spaces by low-value
              inspect path-backup replacing trailing spaces by low-value
              inspect path-log    replacing trailing spaces by low-value
           end-if.              

      ***---
       OPEN-FILES.
           initialize path-logfile.
           string path-log        delimited low-value
                  "LOG_IMP-FIDO_" delimited size
                  como-data       delimited size
                  "_"             delimited size
                  como-ora        delimited size
                  ".log"          delimited size
                  into path-logfile
           end-string.
           open output logfile.
                                                  
           accept como-ora  from time.  

           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.           

                                                               
           move path-import to como-riga.
           perform SCRIVI-RIGA-LOG.
           move path-backup to como-riga.
           perform SCRIVI-RIGA-LOG.
           move path-log to como-riga.
           perform SCRIVI-RIGA-LOG.
           move "CONTROLLO ESISTENZA CARTELLE OK" to como-riga.
           perform SCRIVI-RIGA-LOG.
           open i-o clienti.

      ***---
       ELABORAZIONE.
           if RichiamoSchedulato
              move 0 to batch-status
           end-if.

           move "AZZERAMENTO FIDO CERVED" to como-riga.
           perform SCRIVI-RIGA-LOG.

           set cli-tipo-C to true.
           move low-value to cli-codice
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-fido-data > 0 and 
                       cli-fido-data < como-data-2mesi
                       move 0 to cli-fido
                       rewrite cli-rec
                       initialize como-riga
                       string "AZZERATO CLIENTE: " delimited size
                              cli-codice           delimited size
                              " - DATA: "          delimited size
                              cli-fido-data        delimited size
                         into como-riga
                       end-string                           
                       perform SCRIVI-RIGA-LOG
                    end-if
                 end-perform
           end-start.              

           move "FINE AZZERAMENTO FIDO CERVED" to como-riga.
           perform SCRIVI-RIGA-LOG.

           perform until 1 = 2
              call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                            dir-import-handle,
                                            nome-file

              if nome-file = spaces exit perform end-if

              if nome-file not = "."      and
                           not = ".."     and
                           not = "Backup" and
                           not = "backup" and
                           not = ".DS_Store"

                 initialize wstampa
                 inspect nome-file  replacing trailing spaces
                                    by low-value
                 string path-import delimited low-value
                        nome-file   delimited low-value
                   into wstampa
                 end-string
                 open input lineseq
                 if status-lineseq not = "00"
                    initialize como-riga
                    string "ELABORAZIONE FILE: "  delimited size
                           nome-file              delimited low-value
                           " NON RIUSCITA. ERR: " delimited size
                           status-lineseq         delimited size
                      into como-riga
                    end-string
                    perform SCRIVI-RIGA-LOG
                    if RichiamoSchedulato
                       move 1 to batch-status
                    end-if
                 else
                    initialize como-riga
                    string "ELABORAZIONE FILE: " delimited size
                           nome-file             delimited low-value
                      into como-riga
                    end-string
                    perform SCRIVI-RIGA-LOG
                    perform ELABORA-FILE
                    close lineseq
                    initialize file-backup
                    string  path-backup delimited low-value
                            nome-file   delimited low-value
                            "_"         delimited size
                            como-data   delimited size
                            "_"         delimited size
                            como-ora    delimited size
                       into file-backup
                    end-string
                    inspect wstampa   replacing trailing spaces 
                                      by low-value
                    initialize cmd
                    string "move "     delimited size
                           wstampa     delimited low-value
                           " "         delimited size
                           file-backup delimited size
                           into cmd
                    end-string            
                    move 0 to status-call
                    call "C$SYSTEM" using cmd, 225
                                   giving status-call

                    initialize como-riga
                    string "ESECUZIONE COMANDO: " delimited size
                           cmd                    delimited size
                      into como-riga
                    end-string
                    perform SCRIVI-RIGA-LOG

                    if status-call = 0    
                       move "BACKUP EFFETTUATO" to como-riga
                       perform SCRIVI-RIGA-LOG
                    else
                       move "BACKUP NON RIUSCITO" to como-riga
                       perform SCRIVI-RIGA-LOG
                       if RichiamoSchedulato
                          move 1 to batch-status
                       end-if
                    end-if
                 end-if
                 exit perform |SOLO UNO
              end-if
           end-perform.   
           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                         Dir-import-Handle.   

      ***---
       ELABORA-FILE.
           |salto l'intestazione
           read lineseq next 
                at end 
                move "FILE ELABORATO NON VALIDO" to como-riga
                perform SCRIVI-RIGA-LOG
                if RichiamoSchedulato
                   move -1 to batch-status
                end-if
            not at end
                perform ELABORA-FILE-OK
           end-read.

      ***---
       ELABORA-FILE-OK.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              initialize record-GENERICO
              unstring line-riga delimited by "|"
                  into r-cod-cli
                       r2
                       r3
                       r4
                       r5
                       r6
                       r7
                       r8
                       r9
                       r10
                       r11
                       r12
                       r13
                       r14
                       r15
                       r16
                       r17
                       r18
                       r19
                       r20
                       r21
                       r22
                       r23
                       r24
                       r25
                       r26
                       r27
                       r28
                       r29
                       r30
                       r31
                       r32
                       r33
                       r34
                       r35
                       r36
                       r37
                       r38
                       r39
                       r40
                       r41
                       r42
                       r43
                       r44
                       r45
                       r46
                       r47
                       r48
                       r49
                       r50
                       r51
                       r52
                       r53
                       r54
                       r55
                       r56
                       r57
                       r58
                       r59
                       r60
                       r61
                       r62
                       r63
                       r64
                       r65
                       r66
                       r67
                       r68
                       r69
                       r70
                       r71
                       r-fido-x
              end-unstring 
              move r-fido-x to r-fido convert
             |Posson capitare dei codici con lettere che vanno scartati
              call "C$JUSTIFY" using  r-cod-cli, "R"
              inspect r-cod-cli replacing leading x"20" by x"30"
              if r-cod-cli is numeric
                 add 1 to tot-cli
                 set cli-tipo-C to true
                 move r-cod-cli to cli-codice
                 read clienti no lock
                      invalid 
                      add  1 to tot-ko
                      if RichiamoSchedulato
                         move 1 to batch-status        
                      end-if
                      initialize como-riga
                      string "CLIENTE "     delimited size
                             cli-codice     delimited size
                             " NON TROVATO" delimited size
                        into como-riga
                      end-string
                      perform SCRIVI-RIGA-LOG
                  not invalid   
                      initialize como-riga
                      string "ELABORAZIONE CLIENTE " delimited size
                             cli-codice              delimited size
                        into como-riga
                      end-string
                      perform SCRIVI-RIGA-LOG

                      move r-fido    to como-fido
      *****                if cli-fido not = como-fido or 
      *****                   como-data = 20150207 
      *****                   |solo il primo lancio deve valorizzare tutte 
      *****                   |le date, da li in poi aggiornare solo quelle 
      *****                   |che hanno importo fido differente
      *****                   accept cli-fido-data from century-date
      *****                end-if
                      |16/06/2015: 
                      |Nel caso in cui la cella fido del file è VUOTA:
                      |considerarlo come se fosse 0 (e quindi importare 0) 
                      |MA CANCELLARE LA DATA.
                      if r-fido-x = spaces
                         move 0 to cli-fido-data
                      else
                         accept cli-fido-data from century-date
                      end-if
                      move como-fido to cli-fido
                      rewrite cli-rec
                              invalid continue
                          not invalid 
                              if not RecLocked
                                 add 1 to tot-ok
                              end-if
                      end-rewrite
                 end-read
              end-if
           end-perform. 

      ***---
       SCRIVI-RIGA-LOG.
           if path-logfile = spaces exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize log-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into log-riga
           end-string.
           write log-riga. 

      ***--
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM.
           if path-logfile not = spaces
              if RichiamoSchedulato
                 move path-logfile to batch-log
              end-if

              move spaces to como-riga
              perform SCRIVI-RIGA-LOG   

              initialize como-riga
              string "*** CLIENTI TRATTATI: " delimited size
                    tot-cli                   delimited size
                    " di cui: "               delimited size
                    tot-ok                    delimited size
                    " AGGIORNATI "            delimited size
                    tot-ko                    delimited size
                    " DA VERIFICARE ****"     delimited size
               into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              
              move spaces to como-riga
              perform SCRIVI-RIGA-LOG

              accept como-ora from time
              move como-ora(1:2) to hh
              move como-ora(3:2) to mm
              move como-ora(5:2) to ss

              compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss
              compute tot-secondi = end-secondi - start-secondi

              if tot-secondi < 60
                 move tot-secondi to ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                        ss, " SECONDI" delimited size
                        into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              else
                 divide tot-secondi by 60 giving mm remainder ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI" delimited size
                         into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if
              close logfile

           end-if.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
