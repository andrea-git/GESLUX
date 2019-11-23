       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cerca-ean.
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
       78  titolo value "Ricerca Codice EAN".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.
       77  link-articolo         pic 9(6).
       77  link-ean              pic 9(13).

      ******************************************************************
       PROCEDURE DIVISION USING link-articolo link-ean.

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
           move link-ean  to art-codice-ean-1.
           read articoli key art-codice-ean-1
                invalid continue
            not invalid
                move art-codice to link-articolo
           end-read.

           if link-articolo = 0
              move link-ean  to art-codice-ean-2
              read articoli key art-codice-ean-2
                   invalid continue
               not invalid
                   move art-codice to link-articolo
              end-read
           end-if.  

           if link-articolo = 0
              move link-ean  to art-codice-ean-3
              read articoli key art-codice-ean-3
                   invalid continue
               not invalid
                   move art-codice to link-articolo
              end-read
           end-if.

           if link-articolo = 0
              move link-ean  to art-codice-ean-4
              read articoli key art-codice-ean-4
                   invalid continue
               not invalid
                   move art-codice to link-articolo
              end-read
           end-if.

           if link-articolo = 0
              move link-ean  to art-codice-ean-5
              read articoli key art-codice-ean-5
                   invalid continue
               not invalid
                   move art-codice to link-articolo
              end-read
           end-if.

      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.
