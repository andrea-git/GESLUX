       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azztrasp.
       AUTHOR.                          Andrea.
       REMARKS. In tutti i listini fornitore sposta il valore del flag
           fornitori sui clienti ed azzera il fornitore
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".

       WORKING-STORAGE SECTION.
       77  status-tlistini  pic xx.

       PROCEDURE DIVISION.

      ***---
       MAIN.                                  
           open i-o tlistini.

           move low-value to tlis-rec.
           start tlistini key >= tlis-chiave.
           perform until 1 = 2
              read tlistini next no lock at end exit perform end-read
              if tlis-trasp-f = 1
                 move tlis-trasp-f to tlis-trasp-c
                 move 0 to tlis-trasp-f
                 rewrite tlis-rec
              end-if
           end-perform.

           close tlistini.

           goback.
