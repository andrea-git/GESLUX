       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      movepod.
       AUTHOR.                          Andrea.
       REMARKS. Recupera i POD presenti nella cartella del vettore FTP
                e li sposta nella cartella del vettore in locale
                ANNO\MESE\SIGLA
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tvettori.sl".
           copy "tordini.sl".
           copy "lineseq.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tvettori.fd".
           copy "tordini.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.   
           copy "acucobol.def".

      *    COSTANTI
       78  titolo           value "MovePOD".

      *    FILE STATUS
       77  status-tvettori  pic xx.
       77  status-tordini   pic xx.  
       77  status-lineseq   pic xx.
       77  wstampa          pic x(256).   

       01  r-inizio              pic x(25).

      *    FLAGS     
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.  

       01  controlli        pic x(2).
           88 tutto-ok      value "OK".
           88 errori        value "ER". 
     
       77  dirCheck-handle  handle.
       77  dirSource-handle handle.
       77  path-pod         pic x(200).
       77  nome-file        pic x(200).
       77  nome-file1       pic x(200).
       77  nome-file2       pic x(200).
       77  DirToCheck       pic x(200).
       77  cmd              pic x(200).
       77  CountChar        pic 9(10).
       77  como-bolla       pic x(50).
       77  como-bolla2      pic x(8).
       77  mese-bolla       pic 99.
       77  mese-esteso      pic x(10). 
       77  como-data        pic 9(8). 
       77  como-ora         pic 9(8). 
       77  como-riga        pic x(200).
       77  riga-stampa      pic x(200).

       77  nargs                 pic 99 comp-1 value 0. 

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.

      ******************************************************************
       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           accept  path-pod from environment "PATH_POD".
           inspect path-pod replacing trailing spaces by low-value. 

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              move  0 to batch-status
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa       delimited low-value
                      "MOVEPOD_"    delimited size
                      como-data     delimited size
                      "_"           delimited size
                      como-ora      delimited size
                      ".log"        delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.   

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.   
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           else
              display riga-stampa upon syserr
           end-if.

      ***---
       OPEN-FILES.
           open input tvettori tordini.

      ***---
       ELABORAZIONE.   
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           move 0 to counter counter2.
           set tutto-ok to true.
           move low-value to vet-chiave.
           start tvettori key >= vet-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tvettori next at end exit perform end-read
                    if vet-path-pod not = spaces
                       inspect vet-sigla-pod
                               replacing trailing spaces by low-value

                       initialize como-riga
                       string "ELABORAZIONE " delimited size
                              vet-sigla-pod   delimited low-value
                              " - "           delimited size
                              vet-path-pod    delimited size
                         into como-riga
                       end-string
                       perform SETTA-RIGA-STAMPA

                       move 0 to return-code
      *                CONTROLLO L'ESISTENZA DELLA CARTELLA
                       call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                                     vet-path-pod,
                                                     "*.pdf"

                       move RETURN-CODE   to dirSource-handle
                       if dirSource-handle not = 0
                          perform SCAN-DIRECTORY
                       end-if
                    end-if
                 end-perform
           end-start.   
           move "FINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       SCAN-DIRECTORY.
           perform until 1 = 2
              call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                            dirSource-handle,
                                            nome-file
              if nome-file = spaces exit perform end-if
              if nome-file not = "."  and not = ".."    
                 if RichiamoSchedulato
                    perform CONTATORE-VIDEO
                 end-if

                 inspect nome-file 
                         replacing trailing spaces by low-value    

                 initialize como-riga
                 string "ELABORAZIONE " delimited size
                        nome-file       delimited low-value
                   into como-riga
                 end-string
                 perform SETTA-RIGA-STAMPA

                 move 0 to CountChar
                 inspect nome-file tallying CountChar for characters
                         before low-value
                 subtract 3 from CountChar
                 move spaces to nome-file(CountChar:)
                 move nome-file to como-bolla
                 inspect como-bolla replacing leading x"30" by x"20"
                 call "c$JUSTIFY" using como-bolla, "L"
                 move como-bolla to como-bolla2
                 call "c$JUSTIFY" using como-bolla2, "R"
                 inspect como-bolla2 replacing leading x"20" by x"30"
                 accept como-data from century-date
                 move como-data(1:4) to tor-anno-bolla
                 move como-bolla2    to tor-num-bolla 

                 read tordini key k-bolla
                      invalid 
                      |Se non lo trovo lo cerco nell'anno precedente
                      subtract 1 from tor-anno-bolla
                      read tordini key k-bolla
                           invalid 
                          |Se non lo trovo lo cerco nell'anno successivo
                           add 2 to tor-anno-bolla
                           read tordini key k-bolla
                                invalid
                                move "BOLLA NON TROVATA" to como-riga
                                perform SETTA-RIGA-STAMPA
                            not invalid 
                                perform SPOSTA-POD
                           end-read
                       not invalid 
                           perform SPOSTA-POD
                      end-read
                  not invalid
                      perform SPOSTA-POD
                 end-read
              end-if
           end-perform. 

      ***---
       SPOSTA-POD.
           move tor-data-bolla(5:2) to mese-bolla.
           perform DECODIFICA-MESE-ESTESO.
           set tutto-ok to true.
           perform CHECK-CARTELLA-ANNO.
           if tutto-ok
              perform CHECK-CARTELLA-MESE
              if tutto-ok
                 perform CHECK-CARTELLA-VET
                 if tutto-ok
                    perform SPOSTA-FILE
                 end-if
              end-if
           end-if.

      ***---
       DECODIFICA-MESE-ESTESO.
           evaluate mese-bolla
           when 1  move "GENNAIO"   to mese-esteso
           when 2  move "FEBBRAIO"  to mese-esteso
           when 3  move "MARZO"     to mese-esteso
           when 4  move "APRILE"    to mese-esteso
           when 5  move "MAGGIO"    to mese-esteso
           when 6  move "GIUGNO"    to mese-esteso
           when 7  move "LUGLIO"    to mese-esteso
           when 8  move "AGOSTO"    to mese-esteso
           when 9  move "SETTEMBRE" to mese-esteso
           when 10 move "OTTOBRE"   to mese-esteso
           when 11 move "NOVEMBRE"  to mese-esteso
           when 12 move "DICEMBRE"  to mese-esteso
           end-evaluate.
           inspect mese-esteso replacing trailing spaces by low-value.
 
      ***---
       CHECK-CARTELLA-ANNO.
           initialize DirToCheck.
           string path-pod       delimited low-value
      *            "\"            delimited size
                  tor-anno-bolla delimited size
                  into DirToCheck
           end-string.
           perform VERIFICA-PRESENZA.
 
      ***---
       CHECK-CARTELLA-MESE.
           initialize DirToCheck.
           string path-pod        delimited low-value
      *            "\"             delimited size
                  tor-anno-bolla  delimited size          
                  "\"             delimited size
                  mese-esteso     delimited low-value
                  into DirToCheck
           end-string.
           perform VERIFICA-PRESENZA.
 
      ***---
       CHECK-CARTELLA-VET.
           initialize DirToCheck.
           string path-pod         delimited low-value
      *            "\"              delimited size
                  tor-anno-bolla   delimited size          
                  "\"              delimited size
                  mese-esteso      delimited low-value
                  "\"              delimited size
                  vet-sigla-pod    delimited low-value
                  into DirToCheck
           end-string.
           perform VERIFICA-PRESENZA.

      ***---
       VERIFICA-PRESENZA.
           move 0 to return-code
      *    CONTROLLO L'ESISTENZA DELLA CARTELLA
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         DirToCheck,
                                         "*.*"

           move RETURN-CODE   to dirCheck-handle
      *    SE NON C'E' LA CREO
           if dirCheck-handle = 0 or null
              call "c$makedir" using DirToCheck giving return-code
              if return-code not = 0
                 set errori to true
                 inspect DirToCheck 
                         replacing trailing space by low-value
                 initialize como-riga
                 string "Errore nella creazione della cartella: " 
                        delimited size
                        DirToCheck delimited size
                   into como-riga
                 end-string
                 set errori to true
              end-if
           end-if.

      ***---
       SPOSTA-FILE.
           inspect como-bolla2 replacing leading x"30" by x"20".
           call "C$JUSTIFY" using como-bolla2, "L".
                                                   
           inspect vet-path-pod replacing trailing spaces by low-value.
           inspect nome-file    replacing trailing spaces by low-value.
           initialize nome-file1.
           string  vet-path-pod delimited low-value
                   nome-file    delimited low-value
                   ".pdf"       delimited size
              into nome-file1
           end-string.
           inspect nome-file1 replacing trailing spaces by low-value.

           initialize nome-file2.
           inspect DirToCheck replacing trailing spaces by low-value.
           string DirToCheck  delimited low-value
                  "\"         delimited size
                  como-bolla2 delimited space
                  ".pdf"      delimited size
             into nome-file2
           end-string.

           inspect nome-file2 replacing trailing spaces by low-value.
           initialize cmd.
           string "move "     delimited size
                 x"22"        delimited size         
                  nome-file1  delimited low-value
                 x"22"        delimited size     
                  " "         delimited size     
                 x"22"        delimited size     
                  nome-file2  delimited low-value
                 x"22"        delimited size     
                  into cmd
           end-string.

           move 0 to return-code.            
           call "C$SYSTEM" using cmd, 225
                          giving return-code.

           inspect cmd replacing trailing spaces by low-value.
           if return-code = 0          
              initialize como-riga
              string "OK - " delimited size
                     cmd     delimited low-value 
                into como-riga
              end-string
           else
              initialize como-riga
              string "** KO - " delimited size
                     cmd        delimited low-value 
                into como-riga
              end-string
           end-if.      
           perform SETTA-RIGA-STAMPA.

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2

           if counter2 = 10
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.      

      ***---
       CLOSE-FILES.
           close tordini tvettori.
           if RichiamoSchedulato
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
              close lineseq
           end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
