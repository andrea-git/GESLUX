       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      calfido.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "CLI.sl".
           copy "CLZ.sl".
           copy "TBLPC.sl".
           copy "TBLTR.sl".
           copy "DOCES.sl".
           copy "PAR.sl".
           copy "PAS.sl".
           copy "G2.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           COPY "tparamge.sl".
           copy "tconvanno.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".   
           copy "tivaese.sl".

       SELECT logfile
           ASSIGN       TO path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "CLI.fd".
           copy "CLZ.fd".
           copy "TBLPC.fd".
           copy "TBLTR.fd".
           copy "DOCES.fd".
           copy "PAR.fd".
           copy "PAS.fd".
           copy "G2.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           COPY "tparamge.fd".
           copy "tconvanno.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".  
           copy "tivaese.fd".
                                        
       FD  logfile.
       01 riga-log        PIC  x(900).

       WORKING-STORAGE SECTION.
      * COSTANTI
       78  titolo value "Calcolo Fido".

       77  como-riga pic x(200).
       77  como-ora  pic 9(8).
       77  como-data pic 9(8).

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(12)  value "] (CALFIDO) ".

      * FILE STATUS AND VARIABLES
       77  status-cli         pic xx.
       77  status-clz         pic xx.
       77  status-tblpc       pic xx.
       77  status-tbltr       pic xx.
       77  status-doces       pic xx.
       77  status-par         pic xx.
       77  status-pas         pic xx.
       77  status-tordini     pic xx.
       77  status-rordini     pic xx.
       77  status-G2          pic xx.
       77  status-tparamge    pic xx.
       77  status-tconvanno   pic xx.
       77  status-mtordini    pic xx.
       77  status-mrordini    pic xx.
       77  selclz             pic x(512).
       77  status-logfile     pic xx.
       77  path-logfile       pic x(256).
       77  status-tivaese     pic xx.

      * FLAGS
       01  controlli          pic xx.
         88 tutto-ok          value "OK".
         88 errori            value "ER".

       01  filler             pic 9.
         88 RecLocked         value 1, false 0.

      * VARIABILI
       77  j                  pic 9(4).
       77  j-cnt              pic 9(4).
       77  cnt-cnt            pic 9(4).
       77  data-lavoro        pic 9(8).
       77  PathFile1          pic x(256).
       77  PathFile2          pic x(256).
       77  PathFile3          pic x(256).
       77  esercizio-G2       pic x(2).
       77  batch-notturno     pic x value space.

       LINKAGE SECTION.
       copy "link-calfido.def".

       PROCEDURE DIVISION USING calfido-linkage.

       DECLARATIVES.
      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           evaluate status-CLI
           when "39"
                set errori to true
                if batch-notturno not = "S"
                   display message "File [CLI] mismatch size!"
                             title titolo
                              icon 3
                else
                   move "File [CLI] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true
                if batch-notturno not = "S"
                   display message "[CLI] Indexed file corrupt!"
                          title titolo
                           icon 3         
                else
                   move "[CLI] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [CLI] inesistente"
                        title = titolo
                        icon 2       
                else
                   move "File [CLI] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       CLZ-ERR SECTION.
           use after error procedure on CLZ.
           evaluate status-CLZ
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [CLZ] mismatch size!"
                          title titolo
                           icon 3   
                else
                   move "File [CLZ] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[CLZ] Indexed file corrupt!"
                          title titolo
                           icon 3    
                else
                   move "[CLZ] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [CLZ] inesistente"
                        title = titolo
                        icon 2     
                else
                   move "File [CLZ] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TBLPC-ERR SECTION.
           use after error procedure on TBLPC.
           evaluate status-TBLPC
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [TBLPC] mismatch size!"
                          title titolo
                           icon 3   
                else
                   move "File [TBLPC] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[TBLPC] Indexed file corrupt!"
                          title titolo
                           icon 3   
                else
                   move "[TBLPC] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TBLPC] inesistente"
                        title = titolo
                        icon 2    
                else
                   move "File [TBLPC] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       TBLTR-ERR SECTION.
           use after error procedure on TBLTR.
           evaluate status-TBLTR
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [TBLTR] mismatch size!"
                          title titolo
                           icon 3  
                else
                   move "File [TBLTR] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[TBLTR] Indexed file corrupt!"
                          title titolo
                           icon 3 
                else
                   move "[TBLTR] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG 
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TBLTR] inesistente"
                        title = titolo
                        icon 2 
                else
                   move "File [TBLTR] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG 
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       PAS-ERR SECTION.
           use after error procedure on PAS.
           evaluate status-PAS
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [PAS] mismatch size!"
                          title titolo
                           icon 3 
                else
                   move "File [PAS] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[PAS] Indexed file corrupt!"
                          title titolo
                           icon 3 
                else
                   move "[PAS] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG  
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [PAS] inesistente"
                        title = titolo
                        icon 2 
                else
                   move "File [PAS] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG    
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       PAR-ERR SECTION.
           use after error procedure on PAR.
           evaluate status-PAR
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [PAR] mismatch size!"
                          title titolo
                           icon 3  
                else
                   move "File [PAR] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG  
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                     display message "[PAR] Indexed file corrupt!"
                          title titolo
                           icon 3  
                else
                   move "[PAR] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG 
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [PAR] inesistente"
                        title = titolo
                        icon 2  
                else
                   move "File [PAR] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG  
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       DOCES-ERR SECTION.
           use after error procedure on DOCES.
           evaluate status-DOCES
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [DOCES] mismatch size!"
                          title titolo
                           icon 3
                else
                   move "File [DOCES] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[DOCES] Indexed file corrupt!"
                          title titolo
                           icon 3
                else
                   move "[DOCES] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [DOCES] inesistente"
                        title = titolo
                        icon 2 
                else
                   move "File [DOCES] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG  
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       G2-ERR SECTION.
           use after error procedure on G2.
           evaluate status-G2
           when "39"
                set errori to true 
                if batch-notturno not = "S"
                   display message "File [G2] mismatch size!"
                          title titolo
                           icon 3
                else
                   move "File [G2] mismatch size!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "98"
                set errori to true 
                if batch-notturno not = "S"
                   display message "[G2] Indexed file corrupt!"
                          title titolo
                           icon 3 
                else
                   move "[G2] Indexed file corrupt!" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
           when "35"               
                if batch-notturno not = "S"
                   display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [G2] inesistente"
                        title = titolo
                        icon 2   
                else
                   move "File [G2] inesistente" to como-riga
                   perform SCRIVI-RIGA-LOG
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       END DECLARATIVES.

      ***---
       MAIN.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              if batch-notturno = "S"
                 move "APERTURA FILES OK" to como-riga
                 perform SCRIVI-RIGA-LOG
              end-if
              perform CALCOLA-FIDO
              perform CLOSE-FILES
           else
              move -1 to calfido-status
           end-if.
           perform EXIT-PGM.

      ***---                                                
       INIT.
           move 0 to calfido-status.
           |Settata da scheduler e da mail-fido-m, 
           |serve per non dare messaggi a video e scrivere il log
           accept batch-notturno from environment "BATCH_NOTTURNO".
           if batch-notturno = "S"
              move calfido-path-log to path-logfile
              if path-logfile not = spaces
                 open extend logfile
              end-if
              move "INIZIO PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           set tutto-ok  to true.
           set RecLocked to false.
           accept data-lavoro  from century-date.
      *****     accept esercizio-G2 from environment "ESERCIZIO_G2".

           initialize SELCLZ.
           accept SELCLZ from environment "FILE_PREFIX".
           unstring selclz delimited by ";" 
               into PathFile1
                    PathFile2
                    PathFile3
           end-unstring.
           inspect PathFile2 replacing trailing spaces by low-value.
           move PathFile2(2:) to PathFile2(1:).

      ***---
       OPEN-FILES.                
           if batch-notturno = "S"
              move "APERTURA FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.                                                   
           open input PAS PAR CLI G2 DOCES TBLPC TBLTR tordini rordini
                      tparamge tconvanno mtordini mrordini tivaese.

      ***---
       CALCOLA-FIDO.                              
           if batch-notturno = "S"
              move "CALCOLO FIDO" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno to cnv-anno-geslux.
           read tconvanno no lock invalid continue end-read.
           move cnv-anno-g2 to esercizio-G2.
           move "IV"            to tbliv-codice1.
           move tge-cod-iva-std to tbliv-codice2.
           read tivaese no lock.

           move spaces to G2-codice.
           read G2 no lock invalid continue end-read.
           initialize record-cli record-clz
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move link-cli-codice to cli-codice-g2 clz-codice.
           
           |Mi compongo il path di CLZ per l'anno in corso
           initialize SELCLZ.
           string PathFile2    delimited low-value
                  "/"          delimited size
                  "CLZ"        delimited size
                  esercizio-G2 delimited size
                  into SELCLZ
           end-string.
           open input CLZ.

           read CLI no lock invalid continue end-read.
           read CLZ no lock invalid continue end-read.

           close CLZ.

           perform CLIEFF thru FINE-CLIEFF.
           perform ANNO-PRECEDENTE.
                         
           if batch-notturno = "S"
              move "CALCOLO SALDI" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           compute dare-saldo  = dare-precedente  + clz-dare.
           compute avere-saldo = avere-precedente + clz-avere.
           if dare-saldo > avere-saldo
              subtract  avere-saldo from dare-saldo
              move 0 to avere-saldo
           else
              subtract  dare-saldo  from avere-saldo
              move 0 to dare-saldo
           end-if.
           compute saldo = dare-saldo - avere-saldo.

           move 0 to ordini-in-essere.
           move link-cli-codice   to tor-cod-cli.
           set  tor-no-agg-contab to true.
           start tordini key >= k-andamento-cliente
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-cod-cli not = link-cli-codice or
                       tor-si-agg-contab
                       exit perform
                    end-if
                    if tor-attivo
                       perform LOOP-RIGHE
                    end-if
                 end-perform
           end-start.

           move low-value to mto-rec.
           move link-cli-codice   to mto-cod-cli.
           start mtordini key >= mto-k-clides
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-cod-cli not = link-cli-codice
                       exit perform
                    end-if
                    if mto-chiuso
                       continue
                    else
                       perform LOOP-RIGHE-MASTER
                    end-if
                 end-perform
           end-start.           

           |21012018: AGGIUNTA IVA STD
           compute ordini-in-essere =
                   ordini-in-essere * 
                  (( 100 + tbliv-percentuale ) / 100).

           |Divisione tra SCADUTO e IN SCADENZA
           move 0 to saldo-scaduto.
           move 0 to saldo-scadenza.
           move low-value     to record-pas.
           move "C"           to pas-tipo-cfm.
           move cli-codice-G2 to pas-codice-cfm.
           start PAS key >= pas-codice2
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read PAS next at end exit perform end-read
                    if pas-codice-cfm not = cli-codice-G2
                       exit perform
                    end-if
                    if pas-data-scadenza < data-lavoro
                       compute saldo-scaduto  = 
                               saldo-scaduto  + ( pas-importo-dare - 
                                                  pas-importo-avere )
                    else
                       compute saldo-scadenza = 
                               saldo-scadenza + ( pas-importo-dare - 
                                                  pas-importo-avere )
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    if ( ror-imponib-merce +
                         ror-imp-cou-cobat + 
                         ror-imp-consumo   +
                         ror-add-piombo ) < 999999
                       compute ordini-in-essere =
                               ordini-in-essere +
                          (( ror-qta - ror-qta-omaggi ) * 
                           ( ror-imponib-merce +
                             ror-imp-cou-cobat +
                             ror-imp-consumo   +
                             ror-add-piombo ))
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-MASTER.
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
                    if mro-chiuso
                       continue
                    else
                       if ( mro-imponib-merce  +
                             mro-imp-cou-cobat + 
                             mro-imp-consumo   +
                             mro-add-piombo ) < 999999
                          compute ordini-in-essere =
                                  ordini-in-essere +
                             (( mro-qta - mro-qta-e ) * 
                              ( mro-imponib-merce + 
                                mro-imp-cou-cobat + 
                                mro-imp-consumo   +
                                mro-add-piombo ))
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CLIEFF.                         
           if batch-notturno = "S"
              move "CALCOLO EFFETTI" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move 0 to effetti-rischio.
           move "C"   to pas-tipo-cfm.
           move link-cli-codice to pas-codice-cfm.
           move 1     to pas-situazione.
           move space to pas-data-scadenza.
           move space to pas-codice.
           move "TR"  to tbltr-codice1.
           start TBLTR key >= tbltr-codice
                 invalid go FINE-CLIEFF
           end-start.
       LEGGO-TBLTR.
           read tbltr next at end go FINE-CLIEFF end-read
           if tbltr-codice1 not = "TR"  go FINE-CLIEFF.
           if tbltr-rischio not = space go LEGGO-TBLTR.
           move "C"           to pas-tipo-cfm.
           move cli-codice-G2 to pas-codice-cfm.
           move 2             to pas-situazione.
           move data-lavoro   to pas-data-scadenza.
           move gg in pas-data-scadenza to j.
           if tbltr-giorni-rischio < j
              subtract tbltr-giorni-rischio from j
              move j (03 : 02) to gg in pas-data-scadenza
           else
              move mm in pas-data-scadenza to j-cnt
              move aa in pas-data-scadenza to cnt-cnt
              perform until tbltr-giorni-rischio < j
                subtract j from tbltr-giorni-rischio
                subtract 1 from j-cnt
                if j-cnt = 0
                  subtract 1 from cnt-cnt
                  move 12 to j-cnt
                end-if
                evaluate j-cnt
                when 01
                when 03
                when 05
                when 07
                when 08
                when 10
                when 12
                     move 31 to j
                when 04
                when 06
                when 09
                when 11
                     move 30 to j
                when 02
                     move 28 to j
                end-evaluate
              end-perform
              subtract tbltr-giorni-rischio from j
              move j(3:2)     to gg in pas-data-scadenza
              move j-cnt(3:2) to mm in pas-data-scadenza
              move cnt-cnt    to aa in pas-data-scadenza
           end-if.
           move space to pas-codice.
           start pas key >= pas-codice4
                 invalid go FINE-TBLTR
           end-start.
       LEGGO-CLIEFF.
           read PAS next at end go FINE-TBLTR end-read.
           if pas-tipo-cfm      not = "C"           go FINE-TBLTR.
           if pas-codice-cfm    not = cli-codice-G2 go FINE-TBLTR.
           if pas-situazione    not = 2             go FINE-TBLTR.
           if pas-scadenza-insoluta = "S"           go LEGGO-CLIEFF.
           if pas-codice-tr     not = tbltr-codice2 go LEGGO-CLIEFF.
           if pas-data-registrazione-d < pas-data-registrazione-a
              add pas-importo-dare    to effetti-rischio

           end-if.
           if pas-data-registrazione-d > pas-data-registrazione-a
              subtract pas-importo-dare    from effetti-rischio

           end-if.
           if pas-data-registrazione-d = pas-data-registrazione-a
              perform CLIEFF-PAR thru FINE-CLIEFF-PAR
           end-if.
           go LEGGO-CLIEFF.
       FINE-TBLTR.
           go LEGGO-TBLTR.
       FINE-CLIEFF.

      ***---
       CLIEFF-PAR.
           initialize record-par.
           move pas-codice(1:8) to par-codice.
           start PAR key >= par-codice
                 invalid go FINE-CLIEFF-PAR
           end-start
           read PAR next at end go FINE-CLIEFF-PAR end-read.
           if par-codice(1:8) not = pas-codice(1:8)
              go FINE-CLIEFF-PAR
           end-if.

           if par-dare-avere = "D"
              add pas-importo-dare to effetti-rischio
           else
              subtract pas-importo-dare    from effetti-rischio
           end-if.

       FINE-CLIEFF-PAR.

      ***---
       ANNO-PRECEDENTE.                         
           if batch-notturno = "S"
              move "CALCOLO ANNO PRECEDENTE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           initialize resto-record-doces.
           initialize dare-precedente avere-precedente.
           move "ES"            to doces-codice.
           move G2-codice-login to doces-codice2.
           move esercizio-G2    to doces-codice2(4:).
           move 0               to doces-codice3.
           read doces no lock invalid continue end-read.
            
           if doces-esercizio-precedente not = spaces
           
              |Mi compongo il path di CLZ per l'anno precedente
              initialize SELCLZ
              string PathFile2                  delimited low-value
                     "/"                        delimited size
                     "CLZ"                      delimited size
                     doces-esercizio-precedente delimited size
                     into SELCLZ
              end-string

              open input CLZ
              initialize record-clz record-tblpc
              move link-cli-codice to clz-codice
              read CLZ  no lock invalid continue end-read
              move "PC"                 to tblpc-codice1
              move cli-codice-pc(1:2)   to tblpc-codice2
              read TBLPC no lock invalid  continue end-read
              if tblpc-tipo-conto = "P"
                 move clz-dare  to dare-precedente
                 move clz-avere to avere-precedente
              end-if
              close CLZ

              |Mi compongo il path di CLZ per l'anno d'esercizio
              initialize SELCLZ
              string PathFile2    delimited low-value
                     "/"          delimited size
                     "CLZ"        delimited size
                     esercizio-G2 delimited size
                     into SELCLZ
              end-string
              open input CLZ
              initialize record-clz
              move link-cli-codice to clz-codice
              read CLZ  no lock invalid continue end-read

           end-if.

      ***---
       SCRIVI-RIGA-LOG.
           if path-logfile = spaces exit paragraph end-if.
           initialize riga-log.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-log
           end-string.
           write riga-log.

      ***---
       CLOSE-FILES.                  
           close PAS PAR TBLPC TBLTR DOCES CLI G2 CLZ tordini rordini
                 tparamge tconvanno mtordini mrordini tivaese.
           if batch-notturno = "S" 
              move "CHIUSURA FILES" to como-riga
              perform SCRIVI-RIGA-LOG   
           end-if.

      ***---
       EXIT-PGM.                        
           if batch-notturno = "S" 
              move "FINE PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
              if path-logfile not = spaces
                 close logfile
              end-if
           end-if.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
