       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      evacontras.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl". 
           copy "mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "mtordini.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
                                     
       77  status-tordini   pic xx.                     
       77  status-rordini   pic xx.  
       77  status-mtordini  pic xx.  

       01  filler           pic 9.
           88 RecLocked     value 1 false 0.

       01  filler           pic 9 value 0.
         88 no-contras            value 0.
         88 si-contras            value 1.

       LINKAGE SECTION.                                 
       77  eva-anno         pic 9(4).
       77  eva-from         pic 9(8).
       77  eva-to           pic 9(8).                   

      ******************************************************************
       PROCEDURE DIVISION using eva-anno eva-from eva-to.

       DECLARATIVES.     
       TORDINI-ERR SECTION.
           use error procedure on tordini.
           evaluate status-tordini
           when "99" set RecLocked      to true
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.

      ***---
       OPEN-FILES.
           open input mtordini rordini.
           open i-o tordini.
      
      ***---
       ELABORAZIONE.
           move eva-anno to tor-anno.
           move eva-from to tor-numero.
           start tordini key >= tor-chiave
                invalid continue
            not invalid
                perform until 1 = 2
                   read tordini next no lock
                     at end exit perform 
                   end-read
                   if tor-anno not = eva-anno or
                      tor-numero > eva-to
                      exit perform
                   end-if            

                   perform READ-TORDINI-LOCK

                   if not RecLocked
                      perform LOOP-RIGHE
                      if si-contras                     
                         set tor-contrassegno-si to true
                      else
                         set tor-contrassegno-no to true
                      end-if
                      rewrite tor-rec
                   end-if

                end-perform
           end-start.      

      ***---
       READ-TORDINI-LOCK.
           initialize geslock-linkage.
           move "tordini" to geslock-nome-file.
                                  
           read tordini lock key tor-chiave invalid continue end-read.
      
           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              initialize geslock-messaggio
              string "L'evasione anno: " tor-anno " n. " tor-numero
              x"0d0a""Risulta bloccato su un altro terminale."
              x"0d0a""Sarà impossibile aggiornarne lo stato."
              delimited size
                 into geslock-messaggio
              end-string
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read tordini lock
              when ignora 
                   read tordini no lock
                   set RecLocked to true
                   exit perform
              end-evaluate
           end-perform.

      ***---
       LOOP-RIGHE.
           set si-contras to true.
           move low-value  to ror-rec
           move tor-chiave to ror-chiave
           start rordini key is >= ror-chiave
                 invalid continue
           end-start
           perform until 1 = 2

              read rordini next no lock at end exit perform end-read
              if ror-anno       not = tor-anno      or
                 ror-num-ordine not = tor-numero
                 exit perform
              end-if   
              move ror-chiave-ordine-testa to mto-chiave
              read mtordini no lock
                   invalid continue
               not invalid
                   if mto-contrassegno-no
                      set no-contras to true
                      exit perform
                   end-if
              end-read
           end-perform.

      ***---
       CLOSE-FILES.
           close tordini mtordini rordini.

      ***---
       EXIT-PGM.                                
           goback.
