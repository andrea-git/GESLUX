       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      wassorcli.
       AUTHOR.                          Andrea.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "assorcli.sl".
           copy "tmp-assorcli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "assorcli.fd".
           copy "tmp-assorcli.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
       77  status-assorcli       pic xx.
       77  status-tmp-assorcli   pic xx.
       77  path-tmp-assorcli     pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  FlagTrovato           pic 9.
           88  trovato           value 1 false 0.

       77  controlli             pic xx.
           88  errori            value "ER".
           88  tutto-ok          value "OK".

       LINKAGE SECTION.
           copy "link-wassorcli.def".

      ******************************************************************
       PROCEDURE DIVISION USING link-wassorcli.

       DECLARATIVES.
      
      ***---
       ASSORCLI-ERR SECTION.
           use after error procedure on assorcli.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File assortimento clienti [ASSORCLI] inesistente"
                          title "*--* Errore *--*"
                           icon 2
                set errori to true
           when "39"
                display message "File [ASSORCLI] Mismatch size!"
                          title "*--* Errore *--*"
                           icon 3
                set errori to true
           when "98"
                display message "[ASSORCLI] Indexed file corrupt!"
                          title "*--* Errore *--*"
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TMP-ASSORCLI-ERR SECTION.
           use after error procedure on tmp-assorcli.
           set tutto-ok  to true.
           evaluate status-tmp-assorcli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-ASSORCLI] inesistente"
                          title "*--* Errore *--*"
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-ASSORCLI] Mismatch size!"
                          title "*--* Errore *--*"
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-ASSORCLI] Indexed file corrupt!"
                          title "*--* Errore *--*"
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform EXIT-PGM.

      ***---
       INIT.
           accept  path-tmp-assorcli from environment "PATH_ST".
           accept  como-data         from century-date.
           accept  como-ora          from time.
           inspect path-tmp-assorcli replacing trailing 
                                     spaces by low-value.
           string path-tmp-assorcli  delimited by low-value
                  "tmp-assorcli"     delimited by size
                  "_"                delimited by size
                  como-data          delimited by size
                  "_"                delimited by size
                  como-ora           delimited by size
                  into path-tmp-assorcli
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-assorcli.
           open input  assorcli.
      
      ***---
       ELABORAZIONE.
           set trovato to false.
           move low-value to asc-rec.
           move link-gdo  to asc-cod-gruppo-gdo.
           start assorcli key is >= asc-chiave 
                 invalid continue end-start.
           perform until 1 = 2
              read assorcli next at end exit perform end-read
              if asc-cod-gruppo-gdo  not = link-gdo exit perform end-if
              if asc-cod-cliente     not = 0 and
                 asc-cod-cliente     not = link-cliente
                 exit perform
              end-if
              set tutto-ok to true
              if asc-progressivo-destino not = 0 and
                 link-destino            not = 0
                 if asc-progressivo-destino not = link-destino
                    set errori to true
                 end-if
              end-if
              if tutto-ok
                 move asc-rec to tmp-asc-rec
                 write tmp-asc-rec
                 set trovato to true
              end-if
           end-perform.

      ***---
       EXIT-PGM.
           close assorcli tmp-assorcli.
           if trovato
              move path-tmp-assorcli to link-path-tmp-assorcli
           else
              call "C$DELETE" using path-tmp-assorcli, "I"
           end-if.
           goback.
