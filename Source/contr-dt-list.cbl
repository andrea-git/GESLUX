       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cont-dt-list.
       AUTHOR.                          Luciano.
      ******************************************************************
       SPECIAL-NAMES. 
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".

       WORKING-STORAGE SECTION. 
           copy "acucobol.def".
           copy "acugui.def".

       78  titolo 
               value "Controllo sovrapposizione date Offerte Fornitori".
                                     
       77  status-tlistini   pic xx.

       77  como-data            pic 9(8).

       77  val-ini  pic 99/99/9999.
       77  val-fine pic 99/99/9999.

       01              pic x.
           88 tutto-ok value "O".
           88 errori   value "E".
           
       LINKAGE SECTION.
           copy "link-contr-dt-list.def".

      ******************************************************************
       PROCEDURE DIVISION using contr-dt-list-linkage.

       DECLARATIVES.
       TLISTINI-ERR SECTION.
           use after error procedure on TLISTINI.
           set tutto-ok  to true.
           evaluate status-TLISTINI
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TLISTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TLISTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TLISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.
       END DECLARATIVES.


      ***--- 
       MAIN-PRG.
           perform OPEN-FILES
           perform ELABORAZIONE
           perform CLOSE-FILES
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input tlistini.

      ***---
       ELABORAZIONE.
           set cdl-sovrapposizione to false.

           move cdl-fornitore   to tlis-fornitore
           move cdl-destino     to tlis-destino
      
           move low-value to tlis-ini-val
                             tlis-fine-val.

           start tlistini key not < tlis-chiave-ricerca 
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read tlistini next 
                       at end 
                          exit perform 
                    end-read
                    if tlis-fornitore not = cdl-fornitore    or
                       tlis-destino   not = cdl-destino
                       exit perform
                    end-if

                    if cdl-ini-val >= tlis-ini-val and 
                       cdl-ini-val <= tlis-fine-val 
                       set cdl-sovrapposizione   to true
                    end-if

                    if cdl-fine-val >= tlis-ini-val and 
                       cdl-fine-val <= tlis-fine-val 
                       set cdl-sovrapposizione   to true
                    end-if

      *    controllo che il listino salvato che stò controllando non sia 
      *    compreso  nelle date che sto testando 
                    if cdl-ini-val <= tlis-ini-val and 
                       cdl-fine-val >= tlis-fine-val 
                       set cdl-sovrapposizione   to true
                    end-if

      *    se ho valorizzato il codice listino devo controllare di non star
      *    controllando lo stesso listino
                    if cdl-sovrapposizione
                       if cdl-codice not = zero
                          if tlis-codice = cdl-codice
                             set cdl-sovrapposizione to false
                          end-if
                       end-if
                    end-if

                    if cdl-sovrapposizione
                       move tlis-ini-val(7:2) to como-data(1:2)
                       move tlis-ini-val(5:2) to como-data(3:2)
                       move tlis-ini-val(1:4) to como-data(5:4)
                       move como-data to val-ini
      
                       move tlis-fine-val(7:2) to como-data(1:2)
                       move tlis-fine-val(5:2) to como-data(3:2)
                       move tlis-fine-val(1:4) to como-data(5:4)
                       move como-data to val-fine
      
                       display message 
                             "Sovrapposizione date validità!"
                             x"0d0a"
                             "Codice: ", tlis-codice 
                       x"0d0a" tlis-descrizione 
                       x"0d0a""Periodo validità: " val-ini " - " 
                                                              val-fine
                               title "ATTENZIONE!!!"
                               icon 2
                       exit perform
                    end-if
                end-perform
           end-start.


      ***---
       CLOSE-FILES.
           close tlistini.

      ***---
       EXIT-PGM.
           goback.


