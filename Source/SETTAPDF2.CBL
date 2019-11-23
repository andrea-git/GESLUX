       IDENTIFICATION DIVISION.
       PROGRAM-ID.  settaPDF2.    
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "lineseq.sl".
           copy "user.sl".

       DATA DIVISION.
       FILE SECTION.
           COPY "lineseq.fd".
           copy "user.fd".

       Working-Storage Section.
           copy "link-readutente.def".

       77  status-lineseq  pic xx.
       77  status-user     pic xx.
                                     
       77  path-ini      pic x(256).
       77  path-bullzip  pic x(256).
       77  wstampa       pic x(256).
       77  OutputFile    pic x(256).
       77  StatusFile    pic x(256).
       77  cmd-lancio    pic x(200).

       77  como-data     pic 9(8).
       77  como-ora      pic 9(8).
                                  
       77  idx           pic 9(5).
       77  idx2          pic 9(5).
       77  line-riga2    pic x(500).
       77  FilePDF       pic x(500).
                                  
       01  como-ini.
         05 como-ora-ini pic 9(2).
         05 como-min-ini pic 9(2).
         05 como-sec-ini pic 9(2).

       01  como-end.
         05 como-ora-end pic 9(2).
         05 como-min-end pic 9(2).
         05 como-sec-end pic 9(2).
                                  
       77  sec-ini       pic 9(8).
       77  sec-end       pic 9(8).
       77  sec           pic 9(8).
       77  tot-sec       pic 99.
       77  NomePgm       pic x(20).

       77  status-run    pic s9.

       Linkage Section.
           copy "link-settaPDF.def".

      *****************************************************************
       PROCEDURE DIVISION using settaPDF-linkage.
       DECLARATIVES.
      * <TOTEM:EPT. INIT:conford, INIT:conford, BeforeDeclarative>
      * <TOTEM:END>
       LINESEQ-ERR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON lineseq.


       END DECLARATIVES.

       MainLanc.               
           open input user.

           call "C$CALLEDBY" using NomePgm.
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente"
           accept como-data from century-date.
           accept como-ora  from time.

           move ru-user to user-cod.
           initialize user-path-bullzip.
           read user no lock invalid continue end-read.
           if user-path-bullzip = spaces or = low-value
              evaluate true
              when ru-SO-XP 
                   accept path-ini from environment "PATH_INI_XP"
              when ru-SO-7  
                   accept path-ini from environment "PATH_INI_7"
              |Schedulato da server in stand alone
              when other    
                   accept path-ini from environment "PATH_INI_S"
              end-evaluate
           else
              move user-path-bullzip to path-ini
           end-if.

           |PATH DELL'INI PER BULLZIP DA METTERE IN LOCALE DEL CLIENT
           inspect path-ini replacing trailing spaces by low-value.         
           string  path-ini           delimited low-value
                   "defaults.ini"     delimited size
                   into path-ini
           end-string.
           inspect path-ini replacing trailing spaces by low-value.

           inspect settaPDF-percorso 
                   replacing trailing space by low-value
           inspect settaPDF-nome-file
                   replacing trailing space by low-value.

           |NOME DEL PDF DA CREARE (SU SERVER)
           string  settaPDF-percorso  delimited low-value
                   settaPDF-nome-file delimited low-value
                   ".pdf"             delimited size
                   into OutputFile
           end-string.      

           accept  path-bullzip from environment "PATH_BULLZIP".
           inspect path-bullzip replacing trailing spaces by low-value.

           |NOME DELLO STATUS FILE DA CREARE (SU CARTELLA CONDIVISA)
           string  path-bullzip       delimited low-value
                   settaPDF-nome-file delimited low-value
                   ".txt"             delimited size
                   into StatusFile
           end-string.
                                       
           inspect ru-user replacing trailing spaces by low-value.  
           |FILE INI SU SERVER CHE VERRA' POI SPOSTATO
           string  path-bullzip  delimited low-value
                   ru-user       delimited low-value
                   "_"           delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".ini"        delimited size
                   into wstampa
           end-string.         

           if settaPDF-setta   
              open output lineseq
              if status-lineseq not = "00"
                 set settaPDF-Ok to false       
                 evaluate NomePgm
                 when "stfatt-p"
                      move wstampa to settaPDF-nome-file  |il messaggio lo metto nel log
                 when "st-ordine-m" 
                      display message 
                                "NON RIESCO AD ACCEDERE AL FILE INI"
                                title "PDF NON CREATO"
                                 icon 2
                 end-evaluate
              else                        
                 set settaPDF-Ok to true
                 move "[PDF Printer]" to line-riga
                 write line-riga
                 write line-riga from spaces
                 string "Output="  delimited size
                        OutputFile delimited size
                        into line-riga
                 end-string
                 write line-riga
                 string "StatusFile="  delimited size
                        StatusFile     delimited size
                        into line-riga
                 end-string
                 write line-riga
                 close lineseq  

                 inspect wstampa replacing trailing spaces by low-value
                 initialize cmd-lancio
                 string "move  "   delimited size
                       x"22"       delimited size         
                        wstampa    delimited low-value
                       x"22"       delimited size
                        " "        delimited size
                       x"22"       delimited size         
                        path-ini   delimited low-value
                       x"22"       delimited size
                   into cmd-lancio
                 end-string       
                 call "C$SYSTEM" using cmd-lancio, 224
                                giving status-run

                 move 2 to tot-sec
                 perform SLEEP                             

      *           call "C$SLEEP" using 2
              end-if            
           else
              set settaPDF-OK to false
              move StatusFile to wstampa
              perform 30 times             
                 open input lineseq
                 if status-lineseq = "00"
                    read lineseq next
                    read lineseq next
                    |Se il file PDF è stato creato correttamente
                    if line-riga(20:1) = "0" 
                       set settaPDF-OK to true
                       ||||||||||||||||||||||||||||||||||||||||||||
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       inspect line-riga 
                               replacing trailing spaces by low-value
                       initialize line-riga2
                       move 0 to idx2
                       |intercetto la locazione del file pdf realmente
                       |creato da Bullzip e normalizzo il valore
                       perform varying idx from 1 by 1
                                 until idx > 500
                          if line-riga(idx:1) not = low-value
                             if not 
                              ( line-riga(idx:1) = spaces and idx2 = 0 )
                                add 1 to idx2
                                move line-riga(idx:1) 
                                  to line-riga2(idx2:1)
                             end-if
                          end-if
                       end-perform

                       initialize FilePDF
                       move line-riga2(8:) to FilePDF

                       |e lo sposto dove si aspetta il programma
                       inspect FilePDF replacing 
                               trailing spaces by low-value
                       initialize cmd-lancio
                       string "move  "   delimited size
                             x"22"       delimited size         
                              FilePDF    delimited low-value
                             x"22"       delimited size
                              " "        delimited size
                             x"22"       delimited size         
                              OutputFile delimited low-value
                             x"22"       delimited size
                         into cmd-lancio
                       end-string       
                       call "C$SYSTEM" using cmd-lancio, 224
                                      giving status-run 

                       |E faccio un'attesa di sicurezza di 3 secondi
                       move 3 to tot-sec
                       perform SLEEP                     
                       |||||||||||||||||||||||||||||||||||||||||||||||||

                    else
                       read lineseq next
                       read lineseq next
                       read lineseq next
                       read lineseq next
                    end-if
                    close lineseq
                    exit perform
                 end-if        

                 move 2 to tot-sec
                 perform SLEEP

      *           call "C$SLEEP" using 1
              end-perform
              if not settaPDF-OK                     
                 evaluate NomePgm
                 when "stfatt-p" 
                      move wstampa to settaPDF-nome-file |il messaggio lo scrivo nel log
                 when "st-ordine-m"
                      display message line-riga
                                title "PDF NON CREATO"
                                 icon 2
                 end-evaluate
              else
                 move OutputFile to settaPDF-nome-file
              end-if
              delete file lineseq
              call "C$DELETE" using StatusFile
           end-if.

           close user.

           goback.

      ***---
       SLEEP.
           move 0 to sec-ini sec-end sec.
           accept como-ini from time.
           compute sec-ini = 
                   ( como-ora-ini * 3600 ) +
                   ( como-min-ini * 60   ) + como-sec-ini.
           perform until 1 = 2
              accept como-end from time              
              compute sec-end = 
                      ( como-ora-end * 3600 ) +
                      ( como-min-end * 60   ) + como-sec-end
              compute sec = sec-end - sec-ini
              if sec >= tot-sec
                 exit perform
              end-if
           end-perform.


      *****        inspect wstampa    replacing trailing spaces by low-value
      *****        inspect OutputFile replacing trailing spaces by low-value
      *****        inspect StatusFile replacing trailing spaces by low-value
      *****        set settaPDF-OK to false
      *****        initialize cmd-lancio
      *****        string "echo [PDF Printer] > " delimited size    
      *****              x"22"                    delimited size         
      *****               wstampa                 delimited low-value
      *****              x"22"                    delimited size
      *****          into cmd-lancio
      *****        end-string
      *****        call "C$SYSTEM" using cmd-lancio, 225
      *****                       giving status-run
      *****        if status-run = 0          
      *****           initialize cmd-lancio
      *****           string "echo. >> " delimited size             
      *****                 x"22"        delimited size         
      *****                  wstampa     delimited low-value
      *****                 x"22"        delimited size
      *****             into cmd-lancio
      *****           end-string                   
      *****           call "C$SYSTEM" using cmd-lancio, 225
      *****                          giving status-run
      *****           if status-run = 0
      *****              initialize cmd-lancio
      *****              string "echo Output=" delimited size
      *****                     OutputFile     delimited low-value
      *****                     " >> "         delimited size
      *****                    x"22"           delimited size         
      *****                     wstampa        delimited low-value
      *****                    x"22"           delimited size
      *****                into cmd-lancio
      *****              end-string       
      *****              call "C$SYSTEM" using cmd-lancio, 225
      *****                             giving status-run
      *****              if status-run = 0
      *****                 initialize cmd-lancio
      *****                 string "echo StatusFile=" delimited size
      *****                        StatusFile         delimited low-value
      *****                        " >> "             delimited size
      *****                       x"22"               delimited size         
      *****                        wstampa            delimited low-value
      *****                       x"22"               delimited size
      *****                   into cmd-lancio
      *****                 end-string       
      *****                 call "C$SYSTEM" using cmd-lancio, 225
      *****                                giving status-run
      *****                 if status-run = 0
      *****                    set settaPDF-OK to true
      *****                 end-if
      *****              end-if
      *****           end-if
      *****        end-if                 
      *****        if not settaPDF-OK        
      *****           display message "NON RIESCO AD ACCEDERE AL FILE INI"
      *****                     title "PDF NON CREATO"
      *****                      icon 2
      *****        end-if
