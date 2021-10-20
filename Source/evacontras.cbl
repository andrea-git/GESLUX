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
                                     
       77  status-tordini   pic xx.                     
       77  status-rordini   pic xx.  
       77  status-mtordini  pic xx.  

       01  filler           pic 9 value 0.
         88 no-contras            value 0.
         88 si-contras            value 1.

       LINKAGE SECTION.                                 
       77  eva-anno         pic 9(4).
       77  eva-from         pic 9(8).
       77  eva-to           pic 9(8).                   

      ******************************************************************
       PROCEDURE DIVISION using eva-anno eva-from eva-to.

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
                   read tordini next at end exit perform end-read
                   if tor-anno not = eva-anno or
                      tor-numero > eva-to
                      exit perform
                   end-if            

                   perform LOOP-RIGHE
                   if si-contras                     
                      set tor-contrassegno-si to true
                   else
                      set tor-contrassegno-no to true
                   end-if
                   rewrite tor-rec
                end-perform
           end-start.      

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
