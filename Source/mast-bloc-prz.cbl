       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mast-bloc-prz.
       AUTHOR.                          Andrea.
       REMARKS. 
           Gli ordini (del 2015) devono avere il flag bloccato 
           per prezzo solo se con un prezzo > 99999
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl". 
           copy "mrordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd". 
           copy "mrordini.fd".

       WORKING-STORAGE SECTION.

       78  titolo value "Blocco master prezzo".

       77  status-mtordini         pic x(2).
       77  status-mrordini         pic x(2).

       77  num-rec                 pic 9(5) value 0.
       01  filler                  pic 9.
         88 trovato                value 1, false 0.

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.

      ***---
       OPEN-FILES.
           open i-o mtordini mrordini.

      ***---
       ELABORAZIONE.
           move low-value to mto-rec.
           move 2015 to mto-anno
           start mtordini key >= mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-causale-blocco-prezzo
                       perform LOOP-RIGHE
                       if not trovato
                          add 1 to num-rec
                          move spaces to mto-causale-blocco
                          if mto-bloccato 
                             set mto-attivo to true
                          end-if
                          rewrite mto-rec
                       end-if
                    end-if
                 end-perform
           end-start.
           if num-rec not = 0
              display message "Elaborati " num-rec " ordini"
                        title titolo
                         icon 2
              
           else
              display message "Nessun ordine elaborato!"
                        title titolo
                         icon 2
           end-if.  

      ***---
       LOOP-RIGHE.
           set trovato to false.
           move low-value  to mro-chiave.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-prz-unitario < 999999 and
                       mro-bloccato-prezzo-si
                       set mro-bloccato-prezzo-no to true
                       rewrite mro-rec
                    end-if
                    if mro-prz-unitario >= 999999
                       set trovato to true
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close mtordini mrordini.

      ***---
       EXIT-PGM.
           goback.
