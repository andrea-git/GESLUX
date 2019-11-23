       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-vol-forzato.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare il codice volantino forzato 
           sulle righe di alcuni master per errore

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag".

      * FILE-STATUS
       77  status-mrordini           pic xx.

      *****************************************************************

       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o mrordini.

      ***---
       ELABORAZIONE.
           move low-value to mro-rec.
           move 2011  to mro-anno.
           move 12295 to mro-numero.
           start mrordini key >= mro-chiave.
           perform until 1 = 2 
              read mrordini next at end exit perform end-read
              if mro-numero = 12295 or 
                            = 12296 or
                            = 12297 or
                            = 12298 or
                            = 12299 or
                            = 12300 or
                            = 12301 or
                            = 12303 or
                            = 12304 or
                            = 12305 or
                            = 12307 or
                            = 12308 or
                            = 12310 or
                            = 12311 or
                            = 12314 or
                            = 12315 or
                            = 12316 or
                            = 12317 or
                            = 12318 or 
                            = 12319 or
                            = 12320 or
                            = 12322 or
                            = 12324 or
                            = 12325 or
                            = 12327 or
                            = 12328 or
                            = 12329 or
                            = 12330 or
                            = 12331 or
                            = 12332 or
                            = 12333 or 
                            = 12334 or
                            = 12335 or 
                            = 12336 or
                            = 12337 or
                            = 12338 or
                            = 12339 or
                            = 12340 or
                            = 12341 or
                            = 12342 or
                            = 12343 or
                            = 12344 or
                            = 12345
                 move 4737 to mro-promo
                 rewrite mro-rec
              end-if
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close mrordini.

      ***---
       EXIT-PGM.
           goback.
