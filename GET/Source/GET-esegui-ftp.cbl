       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      get-esegui-ftp.
       AUTHOR.                          Luciano.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "paramget.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "paramget.fd".

       working-storage section.

       77  status-paramget pic xx.
       77  status-lineseq  pic xx.
       77  wstampa         pic x(256).

       77  Status-ftp  pic s9.
       77  comando     pic x(200).
       77  como-data   pic 9(8).
       77  como-ora    pic 9(8).
       77  path-temp   pic x(256).

       77  path-parametri pic x(256).
       77  path-bat       pic x(256).
       77  path-sem       pic x(256).

       77  como-sito      pic x(50).

       77  cont           pic 9(3).
       77  cont-sem       pic 9(3).

       77  como-path      pic x(100).
       77  como-file      pic x(50).
       77  file-crt       pic x(256).

       77  conta-put      pic 9(3).
       77  conta-get      pic 9(3).

       77  como-cont      pic 9(3).

       78  78-put-ok      value "226 Transfer".
       78  78-get-ok      value "226 Transfer".

       LINKAGE SECTION.
           copy "link-esegui-ftp.def".

      ******************************************************************
       PROCEDURE DIVISION using esegui-ftp-linkage.

       DECLARATIVES.
       DECLX1 SECTION.
              USE AFTER STANDARD ERROR PROCEDURE ON
                     lineseq.
              continue
       END DECLARATIVES.

      ***---
       MAIN.
           perform INIT.
           perform SCRIVI-PARAMETRI
           perform FAI-FTP

           if ftp-ok
              perform CONTROLLO-FTP
           end-if.

           perform ELIMINA-FILE

           goback.

      ***---
       CONTROLLO-FTP.
           move file-crt  to wstampa
           evaluate true
           when ftp-contr-sem-exp
           when ftp-contr-sem-imp
           when ftp-contr-sem-impcar
                perform CONTROLLO-SEM
           when ftp-export-art
           when ftp-export-ord
           when ftp-export-ana
                perform CONTROLLO-PUT
           when ftp-import
           when ftp-importart
           when ftp-importcar
                perform CONTROLLO-GET
           end-evaluate.

           if ftp-ok
              delete file lineseq
      *     else
      *        display message box "Mandare indietro file errore ftp"
           end-if.

      ***---
       ELIMINA-FILE.
           move path-bat        to wstampa
           delete file lineseq
           move path-parametri  to wstampa
           delete file lineseq.

           evaluate true
           when ftp-metti-sem-exp
           when ftp-metti-sem-imp
           when ftp-metti-sem-impcar
                move path-sem   to wstampa
                delete file lineseq
           end-evaluate.


      ***---
       INIT.
           move zero   to conta-put
                          conta-get.

           accept wstampa    from environment "PATH_ST"
           move  wstampa  to path-temp
           inspect path-temp replacing trailing space by low-value
           accept como-ora   from time
           accept como-data  from century-date.

           open input paramget
           move space  to get-codice
           read paramget
              invalid
                 continue
           end-read
           close paramget.

           evaluate true
           when ftp-metti-sem-exp
           when ftp-metti-sem-imp
           when ftp-metti-sem-impcar
                perform CREA-SEM
           end-evaluate.

      *    creo il nome del file di controllo semaforo
           initialize file-crt
           inspect get-path-report-server 
                              replacing trailing space by low-value
           string get-path-report-server delimited by low-value
                  "/LOG_FTP_"            delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  ".txt"                 delimited by size
                  into file-crt.




      ***---
       SCRIVI-PARAMETRI.
           move path-temp to wstampa
           string wstampa       delimited by low-value 
                  "parametri_"  delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".txt"        delimited by size
                  into wstampa
           inspect wstampa replacing trailing space by low-value
           move wstampa   to path-parametri.

           open output lineseq.

           evaluate true
           when ftp-export-art
           when ftp-export-ord
           when ftp-export-ana
                perform EXPORT
           when ftp-metti-sem-exp
           when ftp-metti-sem-imp
           when ftp-metti-sem-impcar
                perform METTI-SEM

           when ftp-contr-sem-exp
           when ftp-contr-sem-imp
           when ftp-contr-sem-impcar
                perform CONTR-SEM

           when ftp-import
                perform IMPORTORD
                
           when ftp-importart
                perform IMPORTART

           when ftp-importcar
                perform IMPORTCAR

           when ftp-togli-sem-exp
           when ftp-togli-sem-imp
           when ftp-togli-sem-impcar
                perform TOGLI-SEM

           end-evaluate.

           move "quit"  to line-riga
           write line-riga

           close lineseq.

      ***---
       EXPORT.
           perform SCRIVI-USER-PWD-EXP

           move get-path-remota-exp   to como-path
           perform SCRIVI-CD

           move get-path-exp   to como-path
           perform SCRIVI-LCD

           perform SCRIVI-BINARY

           evaluate true
           when ftp-export-art
                move get-file-articoli  to como-file
                perform SCRIVI-PUT
                move get-file-ean  to como-file
                perform SCRIVI-PUT
                move get-file-prodener  to como-file
                perform SCRIVI-PUT

           when ftp-export-ord
                move get-file-tordini       to como-file
                perform SCRIVI-PUT
                move get-file-rordini       to como-file
                perform SCRIVI-PUT
                move get-file-note-ordini   to como-file
                perform SCRIVI-PUT
                move get-file-articoli      to como-file
                perform SCRIVI-PUT
                move get-file-ean           to como-file
                perform SCRIVI-PUT
                move get-file-prodener      to como-file
                perform SCRIVI-PUT
           when ftp-export-ana
                move get-file-vettori  to como-file
                perform SCRIVI-PUT
                move get-file-fornitori  to como-file
                perform SCRIVI-PUT
                move get-file-classi  to como-file
                perform SCRIVI-PUT
           end-evaluate.

      ***---
       IMPORTORD.
           perform SCRIVI-USER-PWD-IMP.

           move get-path-remota-imp   to como-path.
           perform SCRIVI-CD.

           move get-path-imp   to como-path.
           perform SCRIVI-LCD.

           perform SCRIVI-BINARY.

           move get-file-tordini-imp  to como-file.
           perform SCRIVI-GET.

           move get-file-rordini-imp  to como-file.
           perform SCRIVI-GET.

           move get-file-tordini-imp  to como-file.
           perform SCRIVI-DEL.

           move get-file-rordini-imp  to como-file.
           perform SCRIVI-DEL.

      ***---
       IMPORTART.
           perform SCRIVI-USER-PWD-IMP.

           move get-path-remota-imp   to como-path.
           perform SCRIVI-CD.

           move get-path-imp   to como-path.
           perform SCRIVI-LCD.

           perform SCRIVI-BINARY.

           move get-file-articoli-imp  to como-file.
           perform SCRIVI-GET.

           move get-file-articoli-imp  to como-file.
           perform SCRIVI-DEL.

      ***---
       IMPORTCAR.
           perform SCRIVI-USER-PWD-IMPCAR

           move get-path-remota-impcar   to como-path
           perform SCRIVI-CD

           move get-path-impcar   to como-path
           perform SCRIVI-LCD

           perform SCRIVI-BINARY

           move get-file-carichi-impcar  to como-file
           perform SCRIVI-GET

           move get-file-carichi-impcar  to como-file
           perform SCRIVI-DEL.

      ***---
       METTI-SEM.
           perform SCRIVI-USER-PATH-SEM

      *     move get-path-exp   to como-path
           move path-temp       to como-path
           perform SCRIVI-LCD

           perform SCRIVI-BINARY

           move get-nome-semaforo  to como-file
           perform SCRIVI-PUT.

      ***---
       SCRIVI-USER-PATH-SEM.
           evaluate true
           when ftp-metti-sem-exp
                perform SCRIVI-USER-PWD-EXP
                move get-path-remota-exp    to como-path
           when ftp-metti-sem-imp
                perform SCRIVI-USER-PWD-IMP
                move get-path-remota-imp    to como-path
           when ftp-metti-sem-impcar
                perform SCRIVI-USER-PWD-IMPCAR
                move get-path-remota-impcar to como-path
           end-evaluate
           perform SCRIVI-CD.


      ***---
       TOGLI-SEM.
           evaluate true
           when ftp-togli-sem-exp
                perform SCRIVI-USER-PWD-EXP
                move get-path-remota-exp    to como-path
           when ftp-togli-sem-imp
                perform SCRIVI-USER-PWD-IMP
                move get-path-remota-imp    to como-path
           when ftp-togli-sem-impcar
                perform SCRIVI-USER-PWD-IMPCAR
                move get-path-remota-impcar to como-path
           end-evaluate
           perform SCRIVI-CD.

           perform SCRIVI-BINARY

           move get-nome-semaforo  to como-file
           perform SCRIVI-DEL.


      ***---
       SCRIVI-USER-PWD-EXP.
           initialize line-riga
           string "user "        delimited by size
                  get-user-exp   delimited by size
                  into line-riga
           write line-riga
           move get-pwd-exp      to line-riga
           write line-riga.

      ***---
       SCRIVI-USER-PWD-IMP.
           initialize line-riga
           string "user "        delimited by size
                  get-user-imp   delimited by size
                  into line-riga
           write line-riga
           move get-pwd-imp      to line-riga
           write line-riga.

      ***---
       SCRIVI-USER-PWD-IMPCAR.
           initialize line-riga
           string "user "          delimited by size
                  get-user-impcar  delimited by size
                  into line-riga
           write line-riga
           move get-pwd-impcar     to line-riga
           write line-riga.

      ***---
       SCRIVI-CD.
           if como-path not = space
              initialize line-riga
              string "cd "      delimited by size
                    como-path   delimited by low-value
                    into line-riga
              write line-riga
           end-if.

      ***---
       SCRIVI-LCD.
           if como-path not = space
              perform TOGLI-BARRA
              initialize line-riga
              string "lcd "     delimited by size
                    como-path   delimited by low-value
                    into line-riga
              write line-riga
           end-if.

      ***---
       TOGLI-BARRA.
           inspect como-path replacing trailing space by low-value
           initialize cont
           inspect como-path tallying cont 
                                for characters before low-value
           if como-path(cont:1) = "\"
              move low-value to como-path(cont:1)
           end-if.

      ***---
       SCRIVI-PUT.
           initialize line-riga
           string "put "     delimited by size
                   como-file delimited by size
                   into line-riga
           write line-riga.
           add 1 to conta-put.

      ***---
       SCRIVI-GET.
           initialize line-riga
           string "get "     delimited by size
                   como-file delimited by size
                   into line-riga
           write line-riga.
           add 1 to conta-get.

      ***---
       SCRIVI-DEL.
           initialize line-riga
           string "del "     delimited by size
                   como-file delimited by size
                   into line-riga
           write line-riga.


      ***---
       SCRIVI-BINARY.
           move "binary"  to line-riga
           write line-riga.


      ***---
       FAI-FTP.
           evaluate true
           when ftp-export-art
           when ftp-export-ord
           when ftp-export-ana
           when ftp-metti-sem-exp 
           when ftp-togli-sem-exp 
           when ftp-contr-sem-exp 
                move get-sito-exp     to como-sito
           when ftp-metti-sem-imp 
           when ftp-togli-sem-imp 
           when ftp-contr-sem-imp 
           when ftp-import        
                move get-sito-imp     to como-sito
           when ftp-metti-sem-impcar
           when ftp-togli-sem-impcar
           when ftp-contr-sem-impcar
           when ftp-importcar
                move get-sito-impcar  to como-sito
           end-evaluate

           move path-temp to wstampa

           inspect wstampa replacing trailing space by low-value
           string wstampa    delimited by low-value 
                  "FTP_"     delimited by size
                  como-data  delimited by size
                  "_"        delimited by size
                  como-ora   delimited by size
                  ".bat"     delimited by size
                  into wstampa
           inspect wstampa replacing trailing space by low-value
           move wstampa   to path-bat.

           open output lineseq.

           initialize line-riga

      *     string "ftp -n -s:"     delimited by size
      *            path-parametri   delimited by low-value
      *            " "              delimited by size
      *            como-sito        delimited by size
      *            into line-riga.
      *
      *     evaluate true
      *     when ftp-contr-sem-imp
      *     when ftp-contr-sem-exp
      *     when ftp-contr-sem-impcar
      *          initialize como-file
      *          string "ctrl-sem_" delimited by size
      *                 como-data   delimited by size
      *                 "_"         delimited by size
      *                 como-ora    delimited by size
      *                 ".txt"      delimited by size
      *                 into como-file
      *
      *          inspect line-riga replacing trailing space by low-value
      *          string line-riga   delimited by low-value
      *                 " >"        delimited by size
      *                 path-temp   delimited by low-value
      *                 como-file   delimited by size
      *                 into line-riga
      *          inspect line-riga replacing trailing low-value by space
      *     end-evaluate

           inspect como-sito replacing trailing space by low-value

           string "ftp -n -s:"     delimited by size
                  path-parametri   delimited by low-value
                  " "              delimited by size
                  como-sito        delimited by low-value
                  " >"             delimited by size
                  file-crt         delimited by low-value
                  into line-riga

           write line-riga

           close LINESEQ.

      *    flag c$system 
      *    CSYS-COMPATIBILITY   18
      *    CSYS-SHELL           64
      *    CSYS-HIDDEN          32

           call "C$SYSTEM" using wstampa, 112|80
                          giving Status-ftp.

           if status-ftp = zero
              set ftp-ok  to true
           else
              set ftp-ok  to false
           end-if.

      ***---
       CREA-SEM.
           initialize wstampa
           string path-temp           delimited by low-value
                  get-nome-semaforo   delimited by size
                  into wstampa.
           move wstampa   to path-sem
           open output lineseq
           close lineseq.

      ***---
       CONTR-SEM.
           evaluate true
           when ftp-contr-sem-exp
                perform SCRIVI-USER-PWD-EXP
                move get-path-remota-exp    to como-path
           when ftp-contr-sem-imp
                perform SCRIVI-USER-PWD-IMP
                move get-path-remota-imp    to como-path
           when ftp-contr-sem-impcar
                perform SCRIVI-USER-PWD-IMPCAR
                move get-path-remota-impcar to como-path
           end-evaluate
           perform SCRIVI-CD.

           move path-temp   to como-path
           perform SCRIVI-LCD.

           initialize line-riga
           string "dir "              delimited by size
                  get-nome-semaforo   delimited by size
                  into line-riga
           write line-riga.


      ***---
       CONTROLLO-SEM.
      *     move file-crt  to wstampa

           inspect get-nome-semaforo 
                                   replacing trailing space by low-value
           initialize cont-sem
           inspect get-nome-semaforo 
                    tallying cont-sem for characters before low-value
           set ftp-si-sem to true

           open input LINESEQ
           perform until 1 = 2
              initialize line-riga
              read LINESEQ next
                 at end
                    exit perform
              end-read
              initialize cont
              inspect line-riga tallying cont 
                                   for all get-nome-semaforo(1:cont-sem)
              if cont not = zero
                 initialize cont
                 inspect line-riga tallying cont for all "No such file"
                 if cont > zero
                    set ftp-no-sem to true
                    exit perform
                 end-if
              end-if

           end-perform.

           close LINESEQ.

           open input LINESEQ
           perform until 1 = 2
              initialize line-riga
              read LINESEQ next
                 at end
                    exit perform
              end-read
              initialize cont 
              inspect line-riga tallying cont 
                    for all "The system cannot find the file specified"
              if cont > zero
                 set ftp-no-sem to true
                 exit perform
              end-if
           end-perform.
           close LINESEQ.
         
      
      ***---
       CONTROLLO-PUT.
           move zero   to como-cont

           open input LINESEQ
           perform until 1 = 2
              initialize line-riga
              read LINESEQ next
                 at end
                    exit perform
              end-read
              initialize cont
              inspect line-riga tallying cont 
                                   for all 78-put-ok
              if cont not = zero
                 add 1 to como-cont
              end-if
           end-perform.

           close LINESEQ.

           if como-cont not = conta-put
              set ftp-ok  to false
           end-if.

      ***---
       CONTROLLO-GET.
           move zero   to como-cont

           open input LINESEQ
           perform until 1 = 2
              initialize line-riga
              read LINESEQ next
                 at end
                    exit perform
              end-read
              initialize cont
              inspect line-riga tallying cont 
                                   for all 78-get-ok
              if cont not = zero
                 add 1 to como-cont
              end-if
           end-perform.

           close LINESEQ.

           if como-cont not = conta-get
              set ftp-ok  to false
           end-if.
