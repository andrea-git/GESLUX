       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mov-mp-p.
       AUTHOR. Andrea.
       REMARKS. Imposta su tutte le righe passate in linkage il costo MP
                e le imposte a zero.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rmovmag.sl".
           copy "progmag.sl".
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "rmovmag.fd".
           copy "progmag.fd". 
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
           copy "costo-medio.def".
       77  status-progmag        pic x(2).
       77  status-rmovmag        pic X(2).
       77  status-articoli       pic x(2).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-anno            pic 9(4).
       77  link-num-from        pic 9(8).
       77  link-num-to          pic 9(8).
       77  link-handle          handle of window.

      ******************************************************************
       PROCEDURE DIVISION USING link-anno link-num-from link-num-to 
                                link-handle.     

       MAIN-PRG.           
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o   rmovmag.
           open input progmag articoli.

      ***---
       ELABORAZIONE.
           move low-value     to rmo-rec.
           move link-anno     to rmo-anno.
           move link-num-from to rmo-movim.
           start rmovmag key >=  rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno not = link-anno or
                       rmo-movim > link-num-to
                       exit perform
                    end-if
                    
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 50
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 18,00 line 11,00
                       move 0 to counter2
                    end-if

                    |LO CALCOLO SUL PADRE
                    move rmo-chiave-progmag to prg-chiave
                    move spaces to prg-tipo-imballo
                    move spaces to prg-cod-magazzino
                    move 0      to prg-peso
                    read progmag no lock 
                         invalid continue 
                     not invalid
                         perform CALCOLA-COSTO-MP
                         if costo-mp = 0
                            perform RECUPERO-ANAGRAFICA
                         end-if
                         add 0,005 to costo-mp giving rmo-listino
                         move rmo-listino to rmo-netto
                         move 0           to rmo-imp-cons rmo-coubat   
                         rewrite rmo-rec
                    end-read
                 end-perform
           end-start.       
           
      ***---
       RECUPERO-ANAGRAFICA.
           move prg-cod-articolo of progmag to art-codice.
           read articoli no  lock
                invalid  continue
            not invalid
                compute costo-mp =
                        art-prezzo-acquisto - 
                     (( art-prezzo-acquisto * 
                        art-perce-sconto-acquisto ) / 100 )
           end-read.

      ***---
       CLOSE-FILES.
           close rmovmag progmag articoli.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
