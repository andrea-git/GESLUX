       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cerca-ean.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "tmp-articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "articoli.fd".
           copy "tmp-articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Ricerca Codice EAN".

      * FILE STATUS  
       77  status-articoli       pic xx.
       77  status-tmp-articoli   pic xx.
       77  path-tmp-articoli     pic x(256).
       77  wstampa               pic x(256).

       01  filler                pic 9 value 0.
           88 non-trovati        value 0.
           88 trovati-ean-1      value 1.
           88 trovati-ean-2      value 2.
           88 trovati-ean-3      value 3.
           88 trovati-ean-4      value 4.
           88 trovati-ean-5      value 5.
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  n-articoli            pic 9 value 0.

       77  como-file             pic x(20).
       77  stato-zoom            signed-long.

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
           set non-trovati to true.
           move link-ean  to art-codice-ean-1.
           start articoli key >= art-codice-ean-1
                  invalid continue
              not invalid
                  perform until 1 = 2
                     read articoli next at end exit perform end-read
                     if art-codice-ean-1 not = link-ean
                        exit perform
                     end-if
                     if n-articoli = 1
                        set trovati-ean-1 to true
                        exit perform
                     end-if
                     add 1 to n-articoli
                     move art-codice to link-articolo
                  end-perform
           end-start.

           if link-articolo = 0
              move link-ean  to art-codice-ean-2
              start articoli key >= art-codice-ean-2
                     invalid continue
                 not invalid
                     perform until 1 = 2
                        read articoli next at end exit perform end-read
                        if art-codice-ean-2 not = link-ean
                           exit perform
                        end-if
                        if n-articoli = 1
                           set trovati-ean-2 to true
                           exit perform
                        end-if
                        add 1 to n-articoli
                        move art-codice to link-articolo
                     end-perform
              end-start
           end-if.  

           if link-articolo = 0
              move link-ean  to art-codice-ean-3
              start articoli key >= art-codice-ean-3
                     invalid continue
                 not invalid
                     perform until 1 = 2
                        read articoli next at end exit perform end-read
                        if art-codice-ean-3 not = link-ean
                           exit perform
                        end-if
                        if n-articoli = 1
                           set trovati-ean-3 to true
                           exit perform
                        end-if
                        add 1 to n-articoli
                        move art-codice to link-articolo
                     end-perform
              end-start
           end-if.

           if link-articolo = 0
              move link-ean  to art-codice-ean-4
              start articoli key >= art-codice-ean-4
                     invalid continue
                 not invalid
                     perform until 1 = 2
                        read articoli next at end exit perform end-read
                        if art-codice-ean-4 not = link-ean
                           exit perform
                        end-if
                        if n-articoli = 1
                           set trovati-ean-4 to true
                           exit perform
                        end-if
                        add 1 to n-articoli
                        move art-codice to link-articolo
                     end-perform
              end-start
           end-if.

           if link-articolo = 0
              move link-ean  to art-codice-ean-5
              start articoli key >= art-codice-ean-5
                     invalid continue
                 not invalid
                     perform until 1 = 2
                        read articoli next at end exit perform end-read
                        if art-codice-ean-5 not = link-ean
                           exit perform
                        end-if
                        if n-articoli = 1
                           set trovati-ean-5 to true
                           exit perform
                        end-if
                        add 1 to n-articoli
                        move art-codice to link-articolo
                     end-perform
              end-start
           end-if.

           if non-trovati exit paragraph end-if.

           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp-articoli.
           accept  path-tmp-articoli from environment "PATH_ST".
           inspect path-tmp-articoli 
                   replacing trailing spaces by low-value.
           string  path-tmp-articoli delimited low-value
                   "TMP-ARTICOLI_"   delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
              into path-tmp-articoli
           end-string.
           open output tmp-articoli.
           close       tmp-articoli.
           open i-o    tmp-articoli.

           move link-ean to art-codice-ean-1 
                            art-codice-ean-2
                            art-codice-ean-3
                            art-codice-ean-4
                            art-codice-ean-5

           move "tmp-articoli" to como-file.
           evaluate true
           when trovati-ean-1 
                move link-ean  to art-codice-ean-1
                start articoli key >= art-codice-ean-1
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read articoli next 
                               at end exit perform 
                          end-read
                          if art-codice-ean-1 not = link-ean
                             exit perform
                          end-if
                          move art-rec to tmp-art-rec
                          write tmp-art-rec
                       end-perform
                end-start
           when trovati-ean-2 move "articoli-ean-2" to como-file
           when trovati-ean-3 move "articoli-ean-3" to como-file
           when trovati-ean-4 move "articoli-ean-4" to como-file
           when trovati-ean-5 move "articoli-ean-5" to como-file
           end-evaluate.

           close tmp-articoli.

           call   "zoom-gt" using como-file, art-rec
                           giving stato-zoom.
           cancel "zoom-gt".

           delete file tmp-articoli.

           if stato-zoom = 0
              move art-codice to link-articolo 
           end-if.

      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.
