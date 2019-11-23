       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      listmov-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "mrordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd". 
           copy "rordini.fd".
           copy "mrordini.fd".

       WORKING-STORAGE SECTION.

       78  titolo value "Collegamento promo evasioni/Master".

       77  status-tordini          pic x(2).
       77  status-rordini          pic x(2).
       77  status-mrordini         pic x(2).

       77  collegamenti-f          pic 9(7) value 0.
       77  collegamenti-n          pic 9(7) value 0.

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input mrordini tordini.
           open i-o rordini.
      
      ***---
       ELABORAZIONE.
           |1. NON FATTURATI.
           move 0 to tor-anno-fattura, 
                     tor-data-fattura, 
                     tor-num-fattura, 
                     tor-num-prenot.
           set tor-fatt-no-prenotata to true.
           start tordini key >= K4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura not = 0
                       exit perform
                    end-if
                    if tor-da-ordine-si
                       perform LOOP-RIGHE
                    end-if
                 end-perform
           end-start.
           |1. FATTURATI DA GENNAIO IN POI.
           move low-value to tor-rec.
           move 2010      to tor-anno-fattura.
           move 20100101  to tor-data-fattura.
           start tordini key >= K4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-data-fattura < 20100101
                       exit perform
                    end-if
                    if tor-da-ordine-si
                       perform LOOP-RIGHE
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           move low-value  to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    move ror-chiave-ordine-testa to mro-chiave-testa
                    move ror-progr-master        to mro-progr
                    read mrordini key mro-k-progr
                         invalid continue
                     not invalid
                         if mro-promo not = ror-promo
                            move mro-promo to ror-promo
                            if tor-anno-fattura not = 0
                               add 1 to collegamenti-f
                            else
                               add 1 to collegamenti-n
                            end-if
                            rewrite ror-rec
                         end-if
                    end-read
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close mrordini rordini tordini.

      ***---
       EXIT-PGM.
           display message "FINE!!!! "
                    x"0d0a""PROMO COLLEGATE FATTURATE: " collegamenti-f
                x"0d0a""PROMO COLLEGATE NON FATTURATE: " collegamenti-n
                     title titolo.
           goback.
