       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      impclassi.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcla1art.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcla1art.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  status-tcla1art         pic xx.
       77  status-lineseq          pic xx.
       77  wstampa                 pic x(256).

       77  como-liv                pic xx.
       77  prg                     pic 9(4).


      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           move "lista_classi.csv" to wstampa.
           open input  lineseq.
           open output tcla1art.  

           perform until 1 = 2
              add 1 to prg
              read lineseq next at end exit perform end-read
              initialize cl1-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              unstring line-riga delimited by ";"
                  into cl1-descrizione
                       como-liv
              call "C$JUSTIFY" using como-liv, "R"
              inspect como-liv replacing leading x"20" by x"30"
              move como-liv to cl1-livello convert

              move prg to cl1-codice
              
              move "IMPORT" to cl1-utente-ultima-modifica
              accept cl1-ora-ultima-modifica  from time
              accept cl1-data-ultima-modifica from century-date

              write cl1-rec
           end-perform.
           display message "FINE".
           close tcla1art.
           close lineseq.
           goback.
