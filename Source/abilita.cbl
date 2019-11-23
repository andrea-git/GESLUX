       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      abilita.
       AUTHOR.                          Luciano.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".

       working-storage section.

       77  status-lineseq  pic xx.
       77  wstampa         pic x(256).

       77  Status-ftp  pic s9.
       77  comando     pic x(200).
       77  como-data   pic 9(8).
       77  como-ora    pic 9(8).
       77  path-temp   pic x(256).

       77  path-parametri pic x(256).
       77  path-bat       pic x(256).
       77  path-blocca    pic x(256).

       01  funzione       pic x(50).
           88 EDI         value "EDI".
           88 ORDFORN     value "ORDFORN".
           88 MASTER      value "MASTER".

       01  valore         pic x(50).
           88 blocca      value "BLOCCA", "BLOCCO", "1", "S", "SI".

       77  cont           pic 9(3).

       procedure division.
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
           perform INTERPRETA-ABILITAZIONI


           perform ELIMINA-FILE

           goback.

      ***---
       ELIMINA-FILE.
           move path-bat        to wstampa
           delete file lineseq
           move path-blocca     to wstampa
           delete file lineseq
           move path-parametri  to wstampa
           delete file lineseq.



      ***---
       INIT.
           accept wstampa    from environment "TEMP"
           move  wstampa  to path-temp
           accept como-ora   from time
           accept como-data  from century-date.


      ***---
       SCRIVI-PARAMETRI.
           inspect wstampa replacing trailing space by low-value
           string wstampa delimited by low-value 
                  "\parametri_" delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".txt"        delimited by size
                  into wstampa
           inspect wstampa replacing trailing space by low-value
           move wstampa   to path-parametri.

           open output lineseq.

           move "user goodworks.spt.it"  to line-riga
           write line-riga

           move "goodworks"  to line-riga
           write line-riga
           move "cd /abilitazioni"  to line-riga
           write line-riga
      *     move "pwd"  to line-riga
      *     write line-riga
      *     move "put ftp.txt"  to line-riga
      *     write line-riga
           initialize line-riga
           inspect path-temp replacing trailing space by low-value
           string "lcd "     delimited by size
                  path-temp  delimited by low-value
                  into line-riga
           write line-riga
           move "get blocca.txt"  to line-riga
           write line-riga
           move "quit"  to line-riga
           write line-riga

           close lineseq.

      ***---
       FAI-FTP.
           accept wstampa    from environment "TEMP"

           inspect wstampa replacing trailing space by low-value
           string wstampa delimited by low-value 
                  "\FTP_" delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".bat"        delimited by size
                  into wstampa
           inspect wstampa replacing trailing space by low-value
           move wstampa   to path-bat.

           open output lineseq.

           initialize line-riga

           string "ftp -n -s:"              delimited by size
                  path-parametri            delimited by low-value
                  " storage.spaziotempo.it" delimited by size
                  into line-riga.
           write line-riga

           close LINESEQ.
      *    flag c$system 
      *    CSYS-COMPATIBILITY   18
      *    CSYS-SHELL           64
      *    CSYS-HIDDEN          32

           call "C$SYSTEM" using wstampa, 112|80
                          giving Status-ftp.

      ***---
       INTERPRETA-ABILITAZIONI.
           accept wstampa    from environment "TEMP"

           inspect wstampa   replacing trailing space by low-value
           string wstampa    delimited by low-value
                  "\blocca.txt"   delimited by size
                  into wstampa   
           move wstampa   to path-blocca
           open input LINESEQ
           if STATUS-lineseq = zero

              perform until 1 = 2
                 initialize line-riga
                 read LINESEQ next
                    at end
                       exit perform
                 end-read

                 perform INTERPRETA-BLOCCO
              end-perform
              close lineseq
           end-if.

      ***---
       INTERPRETA-BLOCCO.
           move zero   to cont
           inspect line-riga tallying cont for characters before space
           move line-riga(1:cont)  to funzione
           add 1 to cont
           move line-riga(cont:)   to valore

           call "C$TOUPPER" USING valore, VALUE 50
           call "C$JUSTIFY" using valore, "L"

           evaluate true also true
           when EDI also blocca
                set environment "EDI-BLOCCATO" to "1"
           when ORDFORN also blocca
                set environment "ORDFORN-BLOCCATO" to "1"
           when MASTER also blocca
                set environment "MASTER-BLOCCATO" to "1"
           end-evaluate.

