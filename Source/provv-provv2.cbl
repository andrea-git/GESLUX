       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      provv-provv2.
       AUTHOR.                          Andrea.
       REMARKS. 

           Stesso batch di "provv-propvv" cambia solo la valorizzazione 
           iniziale del campo pvv-prezzo-netto-agente con quello 
           recuperato dal istino 9999 + articolo.
           Il prezzo da recuperare (old o new) è in base a dove 
           è contenuta la data di partenza.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "provvig.sl".
           copy "lisagente.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "provvig.fd".
           copy "lisagente.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Provvigioni agente".

      * FILE STATUS
       77  status-provvig        pic xx.
       77  status-lisagente      pic xx.
       77  wstampa               pic x(256).

       77  como-data             pic 9(8).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  num-rec-upd           pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

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

      ***---
       OPEN-FILES.
           open i-o   provvig.
           open input lisagente.

      ***---
       ELABORAZIONE.
           move low-value to pvv-rec.
           move 20110706  to pvv-data-fat como-data.
           start provvig key >= k-data-fat
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read provvig next at end exit perform end-read
                    move 9999         to lis-codice
                    move pvv-articolo to lis-articolo
                    read lisagente no lock
                         invalid continue
                     not invalid
                         if como-data >= lis-data-inizio-old and
                            como-data <= lis-data-fine-old
                            move lis-prezzo-old 
                              to pvv-prezzo-netto-agente
                         end-if
                         if como-data >= lis-data-inizio-new and
                            como-data <= lis-data-fine-new
                            move lis-prezzo-new
                              to pvv-prezzo-netto-agente
                         end-if
                    end-read        
                    if pvv-agente = 1 or
                       pvv-agente = 9
                       if pvv-prezzo-netto-agente < pvv-prezzo-unit-vend
                          compute pvv-val-provvig = 
                              ((( pvv-prezzo-unit-vend - 
                                  pvv-prezzo-netto-agente ) * 
                                  pvv-qta-vend ) / 2 ) +
                               (( 0,05 * pvv-peso-um ) * pvv-qta-vend )
                       else
                          move 0 to pvv-val-provvig
                       end-if
                    else
                       if pvv-prezzo-netto-agente < pvv-prezzo-unit-vend
                          compute pvv-val-provvig = 
                                ( pvv-prezzo-unit-vend - 
                                  pvv-prezzo-netto-agente ) * 
                                  pvv-qta-vend
                       else
                          move 0 to pvv-val-provvig
                       end-if
                    end-if
                    rewrite pvv-rec
                    add 1 to num-rec-upd
                 end-perform
           end-start.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  num-rec-upd,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close provvig lisagente.

      ***---
       EXIT-PGM.
           goback.
