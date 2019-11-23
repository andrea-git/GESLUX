       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      dati-progmagric.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "progmagric.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "progmagric.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione valori progressivi".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-progmagric     pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI                         
       77  i-peso               pic 9(15).
       77  i-ini-udm            pic 9(15)v999.
       77  i-ini-kg             pic 9(15)v999.
       77  i-ini-valore         pic 9(15)v999.
       77  i-giacenza-udm       pic 9(15)v999. 
       77  i-giacenza-kg        pic 9(15)v999.

       77  r-cod-art-cli         pic x(15).
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       PROGMAGRIC-ERR SECTION.
           use after error procedure on progmagric.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmagric
           when "39"
                set errori to true
                display message "File [progmagric] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[progmagric] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [progmagric] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [LINESEQ] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
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
           move "progmagric.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   progmagric.
           open input lineseq.

      ***---
       ELABORAZIONE.             
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga 
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into prr-cod-articolo 
                            prr-cod-magazzino
                            prr-tipo-imballo 
                            i-peso         
                            i-ini-udm     
                            i-ini-kg      
                            i-ini-valore  
                            i-giacenza-udm                    
                            i-giacenza-kg 
              end-unstring                               
              divide i-peso by 1000 giving prr-peso
              read progmagric no lock
                   invalid add 1 to num-rec-ko
               not invalid              
                   add 1 to num-rec-ok                        
                   move   i-ini-udm      to prr-ini-udm
                   divide i-ini-kg       by 1000 giving prr-ini-kg
                   divide i-ini-valore   by 1000 giving prr-ini-valore
                   move   i-giacenza-udm to prr-giacenza-udm
                   divide i-giacenza-kg  by 1000 giving prr-giacenza-kg
                   rewrite prr-rec
              end-read                                        
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""PROGMAGRIC: ", num-rec,
                    x"0d0a""IMPORTATI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko  
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close progmagric lineseq.

      ***---
       EXIT-PGM.
           goback.
