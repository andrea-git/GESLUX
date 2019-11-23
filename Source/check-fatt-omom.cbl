       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-fatt-omom.
       AUTHOR.                          Andrea.
       REMARKS.
           Controllo fatture senza codice IVA con causale diversa da OMOM

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.

      *    COSTANTI
       78  titolo               value "Verifica fatture omaggio".

      *    FILE STATUS
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.
       77  status-tcaumag       pic xx.

      * VARIABILI
       77  counter              pic 9(10).
       77  counter2             pic 9(10).
       77  counter-edit         pic z(10).
       77  user-codi            pic x(10).

      * FLAGS
       01  controlli            pic xx.
           88 errori            value "ER".
           88 tutto-ok          value "OK".

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0       to counter counter2.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input tordini rordini tcaumag.
      
      ***---
       ELABORAZIONE.
           move low-value to tor-rec.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-causale = "FTTR" or
                       tor-causale = "FTND" or
                       tor-causale = "FTMA"
                       set tca-no-emissione to true
                    else
                       move tor-causale to tca-codice
                       read tcaumag no lock
                    end-if
                    if tca-si-emissione
                       move low-value  to ror-rec
                       move tor-anno   to ror-anno
                       move tor-numero to ror-num-ordine
                       start rordini key >= ror-chiave
                             invalid continue
                         not invalid
                             set errori to true
                             perform until 1 = 2
                                read rordini next 
                                     at end exit perform 
                                end-read
                                if ror-anno       not = tor-anno or
                                   ror-num-ordine not = tor-numero
                                   exit perform
                                end-if
                                if ror-cod-iva not = "E15" and
                                   ror-imponib-merce > 0
                                   set tutto-ok to true
                                   exit perform
                                end-if
                             end-perform
                       end-start
                       if errori
                          display message tor-chiave
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tordini rordini tcaumag.

      ***---
       EXIT-PGM.
           goback.
