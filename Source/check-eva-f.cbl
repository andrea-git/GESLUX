       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-eva-f.
       REMARKS. Controlla che le imposte inserite (se presenti) siano corrette
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "teva.sl".
           copy "reva.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "progmag.sl".
           copy "tpiombo.sl".
           copy "param.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "teva.fd".
           copy "reva.fd".
           copy "articoli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "progmag.fd".
           copy "tpiombo.fd".
           copy "param.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
           copy "imposte.def".
           copy "trova-parametro.def".

       77  status-teva          pic X(2).
       77  status-reva          pic X(2).
       77  status-articoli      pic x(2).
       77  status-timposte      pic x(2).
       77  status-tmarche       pic x(2).
       77  status-progmag       pic x(2).
       77  status-tpiombo       pic x(2).
       77  status-param         pic x(2).
       77  status-clienti       pic x(2).

       77  cont                 pic 9(6) value 0.
       77  scelta               pic 9.
       
       LINKAGE SECTION.
           copy "link-check-eva-f.def".
            
      ******************************************************************
       PROCEDURE DIVISION USING cke-linkage.
      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input teva reva articoli param clienti
                      tmarche timposte progmag tpiombo.

      ***---
       ELABORAZIONE.          
           accept imp-data from century-date. 
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           initialize cke-errori replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move 0 to cke-stato.
           move cke-chiave to teva-chiave.
           read teva no lock
                invalid 
                display message "ERRORE"
            not invalid
                move low-value   to reva-chiave
                move teva-chiave to reva-chiave
                start reva key >= reva-chiave
                      invalid display message "ERRORE"
                  not invalid
                      perform until 1 = 2
                         read reva next at end exit perform end-read
                         if reva-chiave-testa not = teva-chiave
                            exit perform
                         end-if                  
                         if reva-imp-cons not = 0 or
                            reva-coubat   not = 0 or
                            reva-add-pb   not = 0
                            set TrattamentoGDO to true
                            move reva-articolo to art-codice
                            read articoli no lock
                            move reva-chiave-progmag to prg-chiave
                            read progmag  no lock
                            move art-marca-prodotto to mar-codice
                            read tmarche  no lock
                            perform CALCOLA-IMPOSTE
                         end-if                          

                         if reva-imp-cons not = 0  and
                            reva-imp-cons not = imposta-consumo
                            move reva-articolo 
                              to cke-articolo(reva-riga)
                            move -1 to cke-stato
                            set cke-imp-cons-er(reva-riga) to true
                         end-if
                         if reva-coubat not = 0 and
                            reva-coubat not =  
                          ( imposta-cobat + imposta-cou )
                            move reva-articolo 
                              to cke-articolo(reva-riga)
                            move -1 to cke-stato
                            set cke-coubat-er(reva-riga) to true
                         end-if
                         if reva-add-pb not = 0

                            move teva-data          to como-data-ordine 
                                                       tpb-data
                            move art-marca-prodotto to tpb-marca
                            move reva-prz-unit      to como-prz-unitario
                            move 0 to como-prm-cliente como-prm-destino
                            perform ADDIZIONALE-PIOMBO
                            if reva-add-pb not = 0 and
                               reva-add-pb not = add-piombo  
                               move reva-articolo 
                                 to cke-articolo(reva-riga)
                               move -1 to cke-stato
                               set cke-add-pb-er(reva-riga) to true
                            end-if
                         end-if
                      end-perform
                end-start
           end-read.
                         

      ***---
       CLOSE-FILES.
           close teva reva articoli tmarche clienti
                 timposte progmag tpiombo param.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
           copy "addizionale-piombo.cpy".
           copy "trova-parametro.cpy".
