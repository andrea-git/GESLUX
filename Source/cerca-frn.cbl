       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cerca-frn.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Ricerca Codice fornitore".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       77  save-art              pic 9(6) value 0.
       77  como-file             pic x(20).
       77  stato-zoom            signed-long.

       LINKAGE SECTION.
       77  link-articolo         pic 9(6).
       77  link-frn              pic x(15).

      ******************************************************************
       PROCEDURE DIVISION USING link-articolo link-frn.

       DECLARATIVES.
       ARTICOLI SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           move 0 to link-articolo.

      ***---
       OPEN-FILES.
           open input articoli.

      ***---
       ELABORAZIONE.
           move link-frn  to art-cod-art-frn.
           move low-value to art-chiave.
           start articoli key >= art-k-frn
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-cod-art-frn not = link-frn
                       exit perform
                    end-if
                    if save-art not = 0
                       perform ZOOM      
                       exit perform
                    else
                       move art-codice to save-art
                    end-if
                 end-perform
           end-start.
           move save-art to link-articolo.

      ***---
       ZOOM. 
           move   "articoli-frn" to     como-file.
           call   "zoom-gt"  using  como-file, art-rec of articoli
                             giving stato-zoom.
           cancel "zoom-gt".
      
           if stato-zoom = 0
              move art-codice to save-art
           end-if.

      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.
