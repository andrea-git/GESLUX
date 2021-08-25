       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzflagcnt.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".

       WORKING-STORAGE SECTION.
       77  status-tordini           pic xx.  
       77  tot-f                    pic 9(5).
       77  tot-cont                 pic 9(5).
                                                
       78  78-ini-fattura           value 28917.
       78  78-fin-fattura           value 29365.

       procedure division.
      ***---
       MAIN.
           open i-o tordini.                      
           move 2021           to tor-anno-fattura.
           move 78-ini-fattura to tor-num-fattura.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-num-fattura > 78-fin-fattura
                       exit perform
                    end-if
                    add 1 to tot-f
                    if tor-si-agg-contab
                       add 1 to tot-cont
                       set tor-no-agg-contab to true
                       rewrite tor-rec
                    end-if
                 end-perform
           end-start. 

           display message "FATTURE LETTE: " tot-f
                    x"0d0a""FATTURE CONVERTITE: " tot-cont
                 
           close tordini.

           goback.
