       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      teva-no-tmo.
       AUTHOR.                          Andrea.
       REMARKS. Verifica (e riapre) la presenza di bozze d'evasione
                con magazzino di riferimento collegato inesistente
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "teva.sl".
           copy "tmovmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "teva.fd".
           copy "tmovmag.fd".

       WORKING-STORAGE SECTION.

       77  status-teva     pic xx.
       77  status-tmovmag  pic xx.          

       77  n pic 9(5) value 0.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open input teva.
           open i-o   tmovmag.
           move low-value to teva-rec.
           move low-value to teva-anno.
           start teva key >= teva-chiave.
           perform until 1 = 2
              read teva next at end exit perform end-read
              if teva-anno-movim  not = 0 or
                 teva-num-movim   not = 0
                 move teva-k-mov to tmo-chiave
                 read tmovmag no lock
                      invalid add 1 to n
                 end-read
              end-if
           end-perform.
           close teva tmovmag.
           display message n.
           goback.
