       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      eva-cli-errati.
       AUTHOR.                          Andrea.
       REMARKS. Controllo che l'evasione sia collegati a master dello
                stesso cliente.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "mtordini.fd".

       WORKING-STORAGE SECTION.
       77  status-tordini         pic xx.
       77  status-rordini         pic xx.
       77  status-mtordini        pic xx.

       78  titolo    value "Controllo Master collegati ad un'evasione".
       78  78-clear              value 
           "                                                          ".

       
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
           open input mtordini tordini rordini.
      
      ***---
       ELABORAZIONE.
           move low-value to tor-rec.
           move 2010      to tor-anno.
           start tordini key >= tor-chiave.
           perform until 1 = 2
              read tordini next at end exit perform end-read
              if tor-da-ordine-si
                 move low-value  to ror-rec
                 move tor-chiave to ror-chiave
                 start rordini key >= ror-chiave
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    move ror-chiave-ordine-testa to mto-chiave
                    read mtordini no lock 
                    if mto-cod-cli     not = tor-cod-cli  or
                       mto-prg-destino not = tor-prg-destino
                       display message 
                       "ATTENZIONE!"
                x"0d0a""EVASIONE " tor-anno "/"tor-numero 
                       " CREATA IL " tor-data-creazione(7:2)
                       "/"           tor-data-creazione(5:2)
                       "/"           tor-data-creazione(1:4)
                x"0d0a""FA RIFERIMENTO A MASTER "
                       "CON CLIENTE/DESTINO ERRATO"
                                 title titolo
                                  icon 2
                       exit perform
                    end-if
                 end-perform
              end-if
           end-perform.
          
      ***---
       CLOSE-FILES.
           close tordini rordini mtordini.

      ***---
       EXIT-PGM.
           display message "FINE"
                     title titolo.
           goback.
