       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-contab-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Effettua una somma delle fatture/note credito su 3 livelli:
           1. STAMPA (corretta) per vedere ciò che è uscito in Lubex
           2. POSTEL (errata)   per vedere ciò che è arrivato al cliente
           3. CONTABILITA' G2   per vedere ciò che è entrato in contabilità

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "PNT.sl".    
           copy "PNR.sl".
           copy "TBLCO.sl".
           copy "tcaumag.sl".
           copy "tivaese.sl".
           copy "tmp-contab.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "PNT.fd".    
           copy "PNR.fd".
           copy "TBLCO.fd".
           copy "tcaumag.fd".
           copy "tivaese.fd".
           copy "tmp-contab.fd".

       WORKING-STORAGE SECTION.
           copy "acugui.def".
           copy "common-excel.def".
      *    COSTANTI
       78  titolo            value "Controlla contabilità".

      *    FILE STATUS
       77  status-lineseq    pic xx.
       77  status-tordini    pic xx.
       77  status-rordini    pic xx.
       77  status-tnotacr    pic xx.
       77  status-rnotacr    pic xx.
       77  status-PNT        pic xx.
       77  status-PNR        pic xx.
       77  status-TBLCO      pic xx.
       77  status-tcaumag    pic xx.
       77  status-tivaese    pic xx.
       77  status-tmp-contab pic xx.

       77  sw-errore         pic 9.

       77  path-tmp-contab   pic x(256).
       77  wstampa           pic x(256).

      *    FLAGS
       01  controlli         pic x(2).
           88 tutto-ok       value "OK".
           88 errori         value "ER".

       01  filler            pic 9.
           88 trovato        value 1, false 0.

       77  FlagTrovataIVA        pic 9.
           88  TrovataIVA        value 1, false 0.

      *    VARIABILI
       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).
       77  valore-riga       pic s9(9)v99.
       77  valore-riga-pt    pic s9(9)v99.
       77  r1                pic zzz.zzz.zz9,99.
       77  r2                pic zzz.zzz.zz9,99.
       77  r3                pic zzz.zzz.zz9,99.

       01  como-record-r.
           05 como-cod-articolo   pic 9(6).
           05 como-des-libera     pic x(150).
           05 como-qta            pic 9(8).
           05 como-prz-unitario   pic 9(9)v9(2).
           05 como-imp-consumo    pic 9(4)v9(2).
           05 como-imp-cou-cobat  pic 9(4)v9(2).
           05 como-imponib-merce  pic 9(9)v9(2).
           05 como-omaggio        pic X(1).
              88 como-si-omaggio  value "S". 
              88 como-no-omaggio  value "N". 
           05 como-cod-iva        pic x(3).

       77  importo-netto         pic 9(9)v99.
       77  imponibile-merce      pic 9(9)v99.
       77  tot-imponibile        pic 9(9)v99.
       77  tot-iva               pic 9(9)v99.
       77  tot-fattura           pic 9(9)v99.

       01  tabella-iva           occurs 3. 
         05 cod-iva              pic x(3).
         05 tipo-iva             pic 9.
            88 iva-sigla         value 1, false 0.
         05 imponibile-iva       pic 9(9)v99.
         05 importo-iva          pic 9(15)v99.
         05 articolo-iva         pic x(30).

       77  perce-iva-x           pic x(3).
       77  perce-iva-9di3        pic 9(3).

       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.

       77  idx                   pic 9(3).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-handle           handle of window.
       77  link-data-from        pic 9(8).
       77  link-data-to          pic 9(8).
       77  user-codi             pic x(10).

      ******************************************************************
       PROCEDURE DIVISION USING link-handle,
                                link-data-from,
                                link-data-to.

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
           set trovato  to false.
           set tutto-ok to true.
           initialize path-tmp-contab.
           accept  como-data       from century-date.
           accept  como-ora        from time.
           accept  path-tmp-contab from environment "PATH_ST".
           inspect path-tmp-contab replacing trailing 
                                   spaces by low-values.
           string  path-tmp-contab delimited low-value
                   "TMP_CONTAB"    delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".tmp"          delimited size
                   into path-tmp-contab
           end-string.

           accept  wstampa         from environment "PATH_ST".
           inspect wstampa         replacing trailing 
                                   spaces by low-values.
           string  wstampa         delimited low-value
                   "TMP_CONTAB"    delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".csv"          delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open input tordini rordini tnotacr rnotacr 
                      pnt pnr tivaese tcaumag tblco.
           open output tmp-contab.
           close tmp-contab.
           open i-o tmp-contab.
      
      ***---
       ELABORAZIONE.
           move 0 to sw-errore.
           set tutto-ok to true.
           |ELABORO LE FATTURE
           move 0                   to counter counter2.
           move low-value           to tor-rec.
           move link-data-from(1:4) to tor-anno-fattura.
           move link-data-from      to tor-data-fattura.
           start tordini key >= k4 
                 invalid move 1 to sw-errore
           end-start.

           perform until sw-errore = 1

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display "F" 
                    upon link-handle  at column 01 line 10
                 display counter-edit
                    upon link-handle  at column 03 line 10
                 move 0 to counter2
              end-if

              read tordini next at end exit perform end-read
              if tor-data-fattura > link-data-to or
                 tor-anno-fattura not = link-data-to(1:4)
                 exit perform
              end-if
              move tor-causale to tca-codice
              read tcaumag no lock invalid continue end-read
              if tca-si-emissione

                 move 0 to idx
                 perform 3 times
                    add 1 to idx
                    initialize tabella-iva(idx)
                               replacing numeric data by zeros
                                    alphanumeric data by space
                 end-perform 
                 move 0 to importo-netto 
                           tot-fattura 
                           imponibile-merce
                           tot-imponibile

                 initialize tmcont-rec
                 set  tmcont-fattura   to true
                 move tor-num-fattura  to tmcont-codice
                 move tor-data-fattura to tmcont-data
                 move tor-cod-cli      to tmcont-cod-cli
                 move tor-causale      to tmcont-causale
                 
                 move tor-anno   to ror-anno
                 move tor-numero to ror-num-ordine
                 move low-value  to ror-num-riga

                 start rordini key >= ror-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rordini next at end exit perform end-read
                          if ror-anno       not = tor-anno or
                             ror-num-ordine not = tor-numero
                             exit perform
                          end-if
                          move "IV"        to tbliv-codice1
                          move ror-cod-iva to tbliv-codice2
                          read tivaese no lock invalid continue end-read

                          move ror-qta           to como-qta
                          move ror-prz-unitario  to como-prz-unitario
                          move ror-imp-consumo   to como-imp-consumo
                          move ror-imp-cou-cobat to como-imp-cou-cobat
                          move ror-imponib-merce to como-imponib-merce
                          move ror-omaggio       to como-omaggio
                          move ror-cod-iva       to como-cod-iva

                          if como-no-omaggio
                             perform CALCOLO-POSTEL
                          end-if

                       end-perform
                 end-start
                 perform CALCOLO-TOTALE-POSTEL
                 move tot-fattura to tmcont-totale-postel

                 move 0 to idx
                 perform 3 times
                    add 1 to idx
                    initialize tabella-iva(idx)
                               replacing numeric data by zeros
                                    alphanumeric data by space
                 end-perform 
                 move 0 to importo-netto 
                           tot-fattura 
                           imponibile-merce
                           tot-imponibile
                 
                 move tor-anno   to ror-anno
                 move tor-numero to ror-num-ordine
                 move low-value  to ror-num-riga

                 start rordini key >= ror-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rordini next at end exit perform end-read
                          if ror-anno       not = tor-anno or
                             ror-num-ordine not = tor-numero
                             exit perform
                          end-if
                          move "IV"        to tbliv-codice1
                          move ror-cod-iva to tbliv-codice2
                          read tivaese no lock invalid continue end-read
                                                
                          move ror-qta           to como-qta          
                          move ror-prz-unitario  to como-prz-unitario 
                          move ror-imp-consumo   to como-imp-consumo  
                          move ror-imp-cou-cobat to como-imp-cou-cobat
                          move ror-imponib-merce to como-imponib-merce
                          move ror-omaggio       to como-omaggio
                          move ror-cod-iva       to como-cod-iva

                          perform CALCOLO-STANDARD

                       end-perform
                 end-start

                 perform CALCOLO-TOTALE-STANDARD
                 move tot-fattura to tmcont-totale-ok
                 write tmcont-rec invalid continue end-write
              end-if
           end-perform.

           |ELABORO LE NOTE CREDITO
           display "                                     " 
                    upon link-handle  at column 01 line 10.
           move 0 to counter counter2 sw-errore.
           move low-value to tno-rec.
           move link-data-from(1:4) to tno-anno-fattura.
           move link-data-from      to tno-data-fattura.
           start tnotacr key >= k4 
                 invalid move 1 to sw-errore
           end-start.

           perform until sw-errore = 1

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display "N"
                    upon link-handle  at column 01 line 10
                 display counter-edit
                    upon link-handle  at column 03 line 10
                 move 0 to counter2
              end-if

              read tnotacr next at end exit perform end-read
              if tno-data-fattura > link-data-to or
                 tno-anno-fattura not = link-data-to(1:4)
                 exit perform
              end-if
              move tno-causale to tca-codice
              read tcaumag no lock invalid continue end-read
              if tca-si-emissione

                 move 0 to idx
                 perform 3 times
                    add 1 to idx
                    initialize tabella-iva(idx)
                               replacing numeric data by zeros
                                    alphanumeric data by space
                 end-perform
                 move 0 to importo-netto 
                           tot-fattura 
                           imponibile-merce
                           tot-imponibile

                 initialize tmcont-rec
                 set  tmcont-nota      to true
                 move tno-num-fattura  to tmcont-codice
                 move tno-data-fattura to tmcont-data
                 move tno-cod-cli      to tmcont-cod-cli
                 move tno-causale      to tmcont-causale

                 move tno-anno   to rno-anno
                 move tno-numero to rno-numero
                 move low-value  to rno-num-riga
                 start rnotacr key >= rno-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rnotacr next at end exit perform end-read
                          if rno-anno   not = tno-anno or
                             rno-numero not = tno-numero
                             exit perform
                          end-if

                          move "IV"        to tbliv-codice1
                          move rno-cod-iva to tbliv-codice2
                          read tivaese no lock invalid continue end-read

                          move rno-qta           to como-qta          
                          move rno-prz-unitario  to como-prz-unitario 
                          move rno-prz-unitario  to como-imponib-merce
                          move rno-imp-consumo   to como-imp-consumo  
                          move rno-imp-cou-cobat to como-imp-cou-cobat
                          move rno-cod-iva       to como-cod-iva
                          if rno-prz-unitario  = 0
                             set como-si-omaggio to true
                          else
                             set como-no-omaggio to true
                          end-if
                          set trovato to true

                          if como-no-omaggio
                             perform CALCOLO-POSTEL
                          end-if

                       end-perform
                 end-start

                 perform CALCOLO-TOTALE-POSTEL
                 move tot-fattura to tmcont-totale-postel

                 move 0 to idx
                 perform 3 times
                    add 1 to idx
                    initialize tabella-iva(idx)
                               replacing numeric data by zeros
                                    alphanumeric data by space
                 end-perform
                 move 0 to importo-netto 
                           tot-fattura 
                           imponibile-merce
                           tot-imponibile
                 
                 move tno-anno   to rno-anno
                 move tno-numero to rno-numero
                 move low-value  to rno-num-riga
                 start rnotacr key >= rno-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rnotacr next at end exit perform end-read
                          if rno-anno   not = tno-anno or
                             rno-numero not = tno-numero
                             exit perform
                          end-if

                          move "IV"        to tbliv-codice1
                          move rno-cod-iva to tbliv-codice2
                          read tivaese no lock invalid continue end-read

                          move rno-qta           to como-qta          
                          move rno-prz-unitario  to como-prz-unitario 
                          move rno-prz-unitario  to como-imponib-merce
                          move rno-imp-consumo   to como-imp-consumo  
                          move rno-imp-cou-cobat to como-imp-cou-cobat
                          move rno-cod-iva       to como-cod-iva
                          if rno-prz-unitario  = 0
                             set como-si-omaggio to true
                          else
                             set como-no-omaggio to true
                          end-if
                          set trovato to true
                                                  
                          perform MOVE-RIGHE-NOTE-TO-ORDINE
                          perform CALCOLO-STANDARD

                       end-perform
                 end-start

                 perform CALCOLO-TOTALE-STANDARD
                 move tot-fattura to tmcont-totale-ok

                 write tmcont-rec invalid continue end-write
              end-if
           end-perform.
                      
           |ELABORO I PARTITARI
           display "                                     "
                    upon link-handle  at column 01 line 10.
           move 0 to counter counter2 sw-errore.
           move link-data-from to pnt-data-registrazione.
           start pnt key >= pnt-codice1 
                 invalid move 1 to sw-errore
           end-start.

           perform until sw-errore = 1

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display "C"
                    upon link-handle  at column 01 line 10
                 display counter-edit
                    upon link-handle  at column 03 line 10
                 move 0 to counter2
              end-if

              read pnt next at end exit perform end-read
              if pnt-data-registrazione > link-data-to
                 exit perform
              end-if
              move "CO"          to tblco-codice1
              move pnt-codice-co to tblco-codice2
              read tblco no lock invalid continue end-read
              if tblco-segno-registro = "-"
                 set tmcont-nota    to true
              else
                 set tmcont-fattura to true
              end-if
              move pnt-numero-documento   to tmcont-codice
              move pnt-data-registrazione to tmcont-data
              read tmp-contab no lock
                   invalid continue
               not invalid 
                    move pnt-progressivo to pnr-progressivo
                    move 1               to pnr-riga
                    read pnr
                         invalid continue
                     not invalid move pnr-importo to tmcont-totale-pnt
                                 rewrite tmcont-rec 
                                         invalid continue 
                                 end-rewrite
                    end-read
              end-read
           end-perform.

           if trovato
              perform CREA-CSV
           else
              display message "Nessuna corrispondenza trovata"
                        title titolo
                         icon 2
           end-if.


      ***---
       CREA-CSV.
           |Creo il csv da visualizzare
           perform ACCETTA-SEPARATORE.
           open output lineseq.
           initialize line-riga.
           string "Tipo Documento" delimited size
                  separatore       delimited size
                  "Data Documento" delimited size
                  separatore       delimited size
                  "N. Documento"   delimited size
                  separatore       delimited size
                  "Causale"        delimited size
                  separatore       delimited size
                  "Cliente"        delimited size
                  separatore       delimited size
                  "Stampa"         delimited size
                  separatore       delimited size
                  "Postel"         delimited size
                  separatore       delimited size
                  "Contabilità"    delimited size
                  into line-riga
           end-string.
           write line-riga.

           move low-value to tmcont-rec.
           start tmp-contab key >= tmcont-chiave 
                 invalid continue 
           end-start.
           perform until 1 = 2
              read tmp-contab next at end exit perform end-read
              move tmcont-totale-ok     to r1
              move tmcont-totale-postel to r2
              move tmcont-totale-pnt    to r3
              initialize line-riga
              string tmcont-tipo      delimited size
                     separatore       delimited size
                     tmcont-data(7:2) delimited size
                     "/"              delimited size
                     tmcont-data(5:2) delimited size
                     "/"              delimited size
                     tmcont-data(1:4) delimited size
                     separatore       delimited size
                     tmcont-codice    delimited size
                     separatore       delimited size
                     tmcont-causale   delimited size
                     separatore       delimited size
                     tmcont-cod-cli   delimited size
                     separatore       delimited size
                     r1               delimited size
                     separatore       delimited size
                     r2               delimited size
                     separatore       delimited size
                     r3               delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.
           close lineseq.
           perform CALL-EXCEL.

      ***---
       CALCOLO-POSTEL.
           if como-qta = 0 move 1 to como-qta end-if.

           compute importo-netto = como-qta *
           (como-imponib-merce + como-imp-consumo + como-imp-cou-cobat).

           compute imponibile-merce = imponibile-merce + 
                                    ( como-qta * como-imponib-merce ).
           compute tot-imponibile   = tot-imponibile   + importo-netto.

           move 0 to como-iva.

           if tbliv-percentuale not = 0
              move tbliv-percentuale to perce-iva-9di3
              move perce-iva-9di3    to perce-iva-x
              call "C$JUSTIFY" using perce-iva-x, "R"
              inspect perce-iva-x replacing leading x"30" by x"20"
           else
              move tbliv-codice2     to perce-iva-x
           end-if.
           
           move         1 to idx.
           set TrovataIVA to false.
           perform until idx > 3
              if cod-iva(idx) = perce-iva-x
                 set TrovataIVA to true
                 exit perform
              end-if
              add  1 to idx
           end-perform.

           if not TrovataIVA
              evaluate true
              when cod-iva(1) = spaces move perce-iva-x to cod-iva(1)
                                       move 1 to idx
              when cod-iva(2) = spaces move perce-iva-x to cod-iva(2)
                                       move 2 to idx
              when cod-iva(3) = spaces move perce-iva-x to cod-iva(3)
                                       move 3 to idx
              end-evaluate
              if tbliv-percentuale = 0 set iva-sigla(idx) to true
              else                     set iva-sigla(idx) to false
              end-if
           end-if.

           compute imponibile-iva(idx) = 
                   imponibile-iva(idx) + importo-netto.

      ***---
       CALCOLO-TOTALE-POSTEL.
           move    1 to idx.
           perform 3 times
              if not iva-sigla(idx)
                 move cod-iva(idx)    to tbliv-percentuale convert
                 move 0 to como-iva
                 compute como-iva = 
                   ( ( imponibile-iva(idx) * tbliv-percentuale ) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              else
                 move "IV"          to tbliv-codice1
                 move cod-iva(idx)  to tbliv-codice2
                 read tivaese no lock 
                      invalid move spaces to tbliv-descrizione1
                 end-read
                 move tbliv-descrizione1 to articolo-iva(idx)
                 move 0 to como-iva-2dec
              end-if
              move como-iva-2dec to importo-iva(idx)
              add 1 to idx
           end-perform.

           compute tot-iva = importo-iva(1) +
                             importo-iva(2) +
                             importo-iva(3).

           add tot-imponibile  to tot-iva giving tot-fattura.

      ***---
       MOVE-RIGHE-NOTE-TO-ORDINE.
           initialize ror-rec   replacing numeric data by zeroes
                                     alphanumeric data by spaces.

           move rno-des-libera        to ror-des-libera.
           move rno-anno              to ror-anno.
           move rno-numero            to ror-num-ordine.
           move rno-num-riga          to ror-num-riga.
           move rno-cod-articolo      to ror-cod-articolo.
           move rno-qta               to ror-qta.
           move rno-prz-unitario      to ror-imponib-merce.
           move rno-imp-consumo       to ror-imp-consumo.
           move rno-imp-cou-cobat     to ror-imp-cou-cobat.
           move rno-cod-iva           to ror-cod-iva.
           move rno-perce-sconto      to ror-perce-sconto.
           move rno-prg-chiave        to ror-prg-chiave.
           move rno-stato             to ror-stato.
           if rno-prz-unitario  +
              rno-imp-consumo   +
              rno-imp-cou-cobat = 0
              set ror-si-omaggio to true
           else
              set ror-no-omaggio to true
           end-if.

      ***---
       CALCOLO-STANDARD.
           perform SINGOLA-RIGA.

           if ror-qta-omaggi not = 0
              move "E15"          to ror-cod-iva
              set  ror-si-omaggio to true
              move ror-qta-omaggi to ror-qta
              move 0 to ror-qta-omaggi
              perform SINGOLA-RIGA
           end-if.

      ***---
       SINGOLA-RIGA.
           initialize record-tbliv
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           subtract ror-qta-omaggi from ror-qta.

           if ror-qta = 0 |ad esempio NCNC FTMA
              move 1 to ror-qta
           end-if.

           if ror-no-omaggio

              compute imponibile-merce  =
                      ror-imponib-merce +
                      ror-imp-consumo   +
                      ror-imp-cou-cobat

              compute importo-netto    = ror-qta * imponibile-merce

              compute imponibile-merce = imponibile-merce + 
                                       ( ror-qta * ror-imponib-merce )
              compute tot-imponibile   = tot-imponibile + importo-netto

           end-if.

           move "IV"            to tbliv-codice1.
           move ror-cod-iva     to tbliv-codice2.
           read tivaese invalid initialize record-tbliv end-read.
           move 0 to como-iva.

           if tbliv-percentuale not = 0
              move tbliv-percentuale to perce-iva-9di3
              move perce-iva-9di3    to perce-iva-x
              call "C$JUSTIFY" using perce-iva-x, "R"
              inspect perce-iva-x replacing leading x"30" by x"20"
           else
              move tbliv-codice2     to perce-iva-x
           end-if.
           
           move         1 to idx.
           set TrovataIVA to false.
           perform until idx > 3
              if cod-iva(idx) = perce-iva-x
                 set TrovataIVA to true
                 exit perform
              end-if
              add  1 to idx
           end-perform.

           if not TrovataIVA
              evaluate true
              when cod-iva(1) = spaces move perce-iva-x to cod-iva(1)
                                       move 1 to idx
              when cod-iva(2) = spaces move perce-iva-x to cod-iva(2)
                                       move 2 to idx
              when cod-iva(3) = spaces move perce-iva-x to cod-iva(3)
                                       move 3 to idx
              end-evaluate
              if tbliv-percentuale = 0 set iva-sigla(idx) to true
              else                     set iva-sigla(idx) to false
              end-if
           end-if.

           compute imponibile-iva(idx) = 
                   imponibile-iva(idx) + importo-netto.
                            
           if ror-si-omaggio
              move 0                   to imponibile-iva(idx)
              move 0                   to imponibile-merce 
           end-if.

      ***---
       CALCOLO-TOTALE-STANDARD.
           move    1 to idx.
           perform 3 times
              if not iva-sigla(idx)
                 move cod-iva(idx)    to tbliv-percentuale convert
                 move 0 to como-iva
                 compute como-iva = 
                   ( ( imponibile-iva(idx) * tbliv-percentuale ) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              else
                 move "IV"          to tbliv-codice1
                 move cod-iva(idx)  to tbliv-codice2
                 read tivaese no lock 
                      invalid move spaces to tbliv-descrizione1
                 end-read
                 move 0 to como-iva-2dec
              end-if
              move como-iva-2dec to importo-iva(idx)
              add 1 to idx
           end-perform.

           compute tot-iva = importo-iva(1) +
                             importo-iva(2) +
                             importo-iva(3).

           add tot-imponibile  to tot-iva giving tot-fattura.


      ***---
       CLOSE-FILES.
           close tordini rordini tnotacr rnotacr pnt
                 pnr tmp-contab tcaumag tivaese tblco.
           delete file tmp-contab.

      ***---
       EXIT-PGM.
           display "                                     "
                    upon link-handle  at column 01 line 10.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
