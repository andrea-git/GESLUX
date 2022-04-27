       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-ordine-m.
       AUTHOR.                          Andrea.
       REMARKS. Stampa l'ordine appena dopo ch'esso viene salvato.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".
           copy "check-rordini.sl".
           copy "check-rordini2.sl".
           copy "mtordini.sl". 
           copy "mrordini.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "ttipocli.sl".
           copy "tcodpag.sl".     
           copy "tivaese.sl". 
           copy "rlistini.sl".
           copy "timposte.sl".
           copy "distinteb.sl".
           copy "tscorte.sl".
           copy "impforn.sl".
           copy "tparamge.sl".
           copy "tpiombo.sl".
           copy "destinif.sl".
           copy "tlistini.sl".
           copy "param.sl".
           copy "agenti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".
           copy "check-rordini.fd".
           copy "check-rordini2.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "ttipocli.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".  
           copy "rlistini.fd". 
           copy "timposte.fd".
           copy "distinteb.fd".
           copy "tscorte.fd".
           copy "impforn.fd".
           copy "tparamge.fd".
           copy "tpiombo.fd".
           copy "destinif.fd".
           copy "tlistini.fd".
           copy "param.fd".
           copy "agenti.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".
           copy "costo-medio.def".
           copy "prz-finito-forn.def".
           copy "link-readutente.def".
           copy "link-settaPDF.def".
           copy "link-EDI-stbozze.def".  
           COPY "imposte-fornitore.def".     
           COPY  "trova-parametro.def".
       
       78  titolo                value "GESLUX - Scontrino MASTER".
       78  MaxRighe              value 52.
                                 
       77  status-check-rordini  pic xx.
       77  status-check-rordini2 pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-articoli       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-progmag        pic xx.
       77  status-ttipocli       pic xx.   
       77  status-tcodpag        pic xx.
       77  status-tivaese        pic xx.
       77  status-rlistini       pic xx.
       77  status-timposte       pic xx.
       77  status-distinteb      pic xx.
       77  status-scorte         pic xx.
       77  status-impforn        pic xx.
       77  status-tparamge       pic xx.
       77  status-tscorte        pic xx.
       77  status-tpiombo        pic xx.
       77  status-destinif       pic xx.
       77  status-tlistini       pic xx.
       77  status-param          pic xx.
       77  status-agenti         pic xx.

       77  path-check-rordini    pic x(256).
       77  path-check-rordini2   pic x(256).

       77  TimesNewRoman20B      handle of font.
       77  CourierNew10          handle of font.
       77  CourierNew10B         handle of font.
       77  CourierNew12B         handle of font.
       77  Font-Copia-TimesNewRoman30    handle of font.      

       77  como-nome-file   pic x(156).
       77  como-time        pic 9(8).
       01  FILE-INFO.
           02  FILE-SIZE    PIC X(8) COMP-X.
           02  FILE-DATE    PIC 9(8) COMP-X.
           02  FILE-TIME    PIC 9(8) COMP-X.

       77  old-SIZE         PIC X(8) COMP-X.

       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.
       77  como-prz              pic 9(9)v99.

       77  status-code       pic 9.

       01                    pic 9.
           88 TIME-OUT-EXIT  value 1 false zero.    
       01                    pic 9.   
           88 trovato        value 1 false zero. 
       01                    pic 9 value 0.   
           88 StampaAgente   value 1 false zero. 

       01                    pic 9 value 0.
           88 stampa-bozza-EDI value 1, false 0.   
       77 calcolo-piombo   PIC  x.
           88 nuovo-calcolo-piombo VALUE IS "N". 

       77  minuti-partenza   pic 99.
       77  minuti-arrivo     pic 99.

       01  tab-iva           occurs 3 indexed by idx-iva.
         05 el-iva           pic x(3).
         05 el-aliq          pic 9(5)v99.
         05 el-imponib       pic 9(10)v99.

       77  PgmChiamante          pic x(20).
       77  messaggio             pic x(150)  value spaces.
       77  wfont-status          pic s9(5)   value zero.
       77  font-size-dply        pic z(5)    value zero.
       77  pos-piede             pic 9(3)v99 value zero.
       77  max-righe-corpo       pic 9(3)v99 value zero.
       77  num-pagina            pic 9(3)    value zero.

       77  NomeFile               pic x(256).
       77  DestFile               pic x(256).
       77  cont                   pic 999.
                                           
       77  data-oggi              pic 9(8).
       77  como-data              pic 9(8).
       77  como-ora               pic 9(8).
       77  como-qta               pic 9(8).
       77  scelta                 pic 9.
       77  riga                   pic 99.
       77  imballi-ed             pic z(4).
       77  tot-colli              pic 9(5).
       77  tot-peso-tot           pic 9(6)v999.
       77  tot-peso-utf           pic 9(6)v999.
       77  tot-peso-non-utf       pic 9(6)v999.
       77  totale                 pic 9(6)v999.
       77  sw-aggiunte            pic 9(3).
       77  sw-modificate          pic 9(3).
       77  sw-eliminate           pic 9(3).
       77  tot-peso-aggiunte      pic 9(6)v999.
       77  tot-utf-aggiunte       pic 9(6)v999.
       77  tot-non-utf-aggiunte   pic 9(6)v999.
       77  tot-colli-aggiunte     pic 9(4).
       77  tot-peso-modificate    pic s9(6)v999.
       77  tot-utf-modificate     pic s9(6)v999.
       77  tot-non-utf-modificate pic s9(6)v999.
       77  tot-colli-modificate   pic s9(4).
       77  tot-peso-eliminate     pic 9(6)v999.
       77  tot-utf-eliminate      pic 9(6)v999.
       77  tot-non-utf-eliminate  pic 9(6)v999.
       77  tot-colli-eliminate    pic 9(4).
       77  idx                    pic 9(3) value 0.
       77  importo-singolo2       pic 9(5)v99.
       77  importo-singolo        pic 9(5)v99.
       77  r-prz-z                pic zz.zz9,99.
       77  codice-x               pic x(5).
       77  costo-ultimo           PIC 9(10)v99999.
       77  min-value              pic 9(10)v99999.
       77  prezzo-confronto       pic 9(10)v99999.
      *
       01  sw-font               pic 9.

       77  filler                pic 9   value 0.
           88  testata-bold      value 1 false 0.
           88  testata-SC        value 2 false 0.
           88  testata-M         value 3 false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  divisorio   pic x(90) value all "=".

       01  t1.
         05 t1-cli-ragsoc        pic x(40).
         05 filler               pic x(2).
         05 t1-des-ragsoc        pic x(40).

       01  t2.
         05 t2-cli-indirizzo     pic x(40).
         05 filler               pic x(2).
         05 t2-des-indirizzo     pic x(40).

       01  t3.
         05 t3-cli-cap           pic x(5).
         05 filler               pic x.
         05 t3-cli-localita      pic x(34).
         05 filler               pic x(2).
         05 t3-des-cap           pic x(5).
         05 filler               pic x.
         05 t3-des-localita      pic x(34).

       01  t4.
         05 filler               pic x(21) value "Dati Ordine........:".
         05 t4-numero            pic z(12).
         05 filler               pic x(5)  value " del ".
         05 t4-del               pic x(10).
         05 filler               pic x(3)  value " - ".
         05 t4-hh                pic 9(2).
         05 filler               pic x     value ":".
         05 t4-mm                pic 9(2). 
         05 filler               pic x(14) value " Inserita da: ".
         05 t4-utente            pic x(20).

       01  t5.
         05 filler               pic x(21) value "Dati Ordine Cliente:".
         05 t5-num-ord-cli       pic x(50).
         05 filler               pic x(5)  value " del ".
         05 t5-del               pic x(10).
         05 filler               pic x(3).
         05 t5-ritira            pic x(15).

       01  t6.
         05 filler               pic x(21) value "Tipologia Pagamento:".
         05 t6-cod-pag           pic x(3).
         05 filler               pic x(3)  value " - ".
         05 t6-des-pag           pic x(25).
         05 filler               pic x(1).
         05 filler               pic x(8) value "Agente: ".
         05 t6-cod-age           pic z(5). 
         05 filler               pic x(2).
         05 t6-des-age           pic x(30).

       01  t7.
         05 filler               pic x(21) value "Note di consegna...:".
         05 t7-note1             pic x(20).
         05 t7-data              pic x(10).
         05 filler               pic x(2)  value spaces.
         05 t7-note2             pic x(30).

       01  t8.
         05 filler               pic x(21) value spaces.
         05 t8-note3             pic x(30).
         05 filler               pic x(2)  value spaces.
         05 t8-note4             pic x(30).

       01  testata.
         05 filler     pic x(6)  value "  Art.".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(5)  value "Colli".
         05 filler     pic x(2)  value spaces.
         05 filler     pic x(7) value "Imballo".
         05 filler     pic x(5)  value spaces.
         05 filler     pic x(30) value "Descrizione".
         05 filler     pic x(1)  value spaces.     
         05 filler     pic x(5)  value " Q.tà".
         05 filler     pic x(2)  value spaces.
         05 filler     pic x(5)  value "Saldo".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(9)  value "    PROD".

       01  testata-prz-bold.
         05 filler     pic x(9) value "   Prezzo".
         
       01  r-riga.
         05 r-art      pic z(6).
         05 filler     pic x(1).
         05 r-colli    pic z.zz9.
         05 filler     pic x(2).
         05 r-imb      pic x(11).
         05 filler     pic x(1).
         05 r-des      pic x(30).         
         05 filler     pic x(1).
         05 r-qta      pic zzzz9.
         05 filler     pic x(2).
         05 r-saldo    pic zzzz9.
         05 filler     pic x(1).
         05 r-prod     pic z.zz9,99.

       01  el-r-riga   occurs 999.
         05 el-r-art   pic z(6).
         05 filler     pic x(1).
         05 el-r-colli pic z.zz9.
         05 filler     pic x(1).
         05 el-r-imb   pic x(11).
         05 filler     pic x(1).
         05 el-r-des   pic x(30).
         05 filler     pic x(1).
         05 el-r-qta   pic zzzz9.
         05 filler     pic x(2).
         05 el-r-saldo    pic zzzz9.
         05 filler     pic x(1).
         05 el-qta-pos-neg pic x.           
            88 el-qta-pos  value "+".
            88 el-qta-neg  value "-".
         05 filler     pic x(1).     
         05 el-r-prz   pic z.zz9,99 blank zero.
         05 el-tipo    pic x.
            88 el-aggiunta   value "A".
            88 el-modificata value "M".
            88 el-eliminata  value "E".

       01  r-riga-prz-bold.
         05 r-prz      pic x(10).

       01  r-totale.
         05 filler     pic x(70).
         05 filler     pic x(8) value "TOTALE: ".
         05 r-tot      pic zzz.zz9,99.

       01  r-totale-iva.
         05 filler     pic x(64).
         05 filler     pic x(14) value "TOTALE IVATO: ".
         05 r-tot-iva  pic zzz.zz9,99.

       01  r-colli-tot.
         05 filler     pic x(15) value "*** Tot. colli:".
         05 r-colli-t  pic ----9.

       01  r-peso-t-utf.
         05 filler         pic x(14) value "*** PESO ---> ".
         05 filler         pic x(45).
         05 filler         pic x(5)  value "UTF: ".
         05 r-peso-utf     pic ---.--9,999.

       01  r-peso-t-non-utf.
         05 filler         pic x(55).
         05 filler         pic x(9)  value "NON UTF: ".
         05 r-peso-non-utf pic ---.--9,999.

       01  r-peso-t-tot.
         05 filler         pic x(59).
         05 filler         pic x(5)  value "TOT: ".
         05 r-peso-tot     pic ---.--9,999.

       01  r-data-ora.
         05 filler     pic x(17) value "Stampata in data ".
         05 r-data     pic x(10).
         05 filler     pic x(10) value " alle ore ".
         05 r-ora      pic x(5).

       01              pic 9.
           88 passaggio-normale   value zero   false 1.
      *****     88 passaggio-bloccato  value 1 false zero.

       LINKAGE SECTION.
       01  link-chiave.
           05 link-anno         pic 9(4).
           05 link-numero       pic 9(8).
       77  link-path            pic x(256).
       01  tipo-ope             pic x.
           88 manuale           value "N".
           88 conferma-ordine   value "C".
           88 inserimento       value "I".
           88 modifica          value "M".
           88 tradizionale      value "T".
           88 prezzo-master     value "P".
           88 elenco            value "E".
       
      ******************************************************************
       PROCEDURE DIVISION using link-chiave, link-path, tipo-ope.

       DECLARATIVES.

      ***---      
       CHECK-RORDINI2-ERR SECTION.
           use after error procedure on check-rordini2.
           set tutto-ok  to true.
           evaluate status-check-rordini2
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [CHECK-RORDINI2] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [CHECK-RORDINI2] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CHECK-RORDINI2] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set tutto-ok  to true.
           evaluate status-mtordini 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [MTORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [MTORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---      
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [MRORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Clienti [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Clienti [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Destini [DESTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       END DECLARATIVES.
      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              set passaggio-normale   to true
              perform ELABORAZIONE
              if conferma-ordine
                 if settaPDF-OK
                    perform CHIUDI-STAMPA
                    perform ASPETTA-PDF
                 end-if                
              else
                 perform CHIUDI-STAMPA
              end-if

           end-if.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente".
                         
           accept data-oggi from century-date.

           move 0 to riga.
           set     tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-check-rordini2.
           accept  path-check-rordini2 from environment "PATH_ST".
           inspect path-check-rordini2 replacing trailing 
                                       spaces by low-value.
           string  path-check-rordini2 delimited low-value
                   "CHECK-RORDINI_"    delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-check-rordini2
           end-string.

      ***---
       OPEN-FILES.              
           open input mtordini mrordini clienti destini articoli progmag 
                      ttipocli tcodpag tivaese distinteb tlistini param
                      timposte tscorte impforn tparamge tpiombo destinif
                      rlistini agenti.
           if modifica
              move link-path to path-check-rordini
              open  input check-rordini
              open output check-rordini2
              close check-rordini2
              open i-o check-rordini2
           end-if.         
      
      ***---
       ELABORAZIONE.
           move link-chiave to mto-chiave.
           read mtordini no lock invalid set errori to true end-read.
           if tutto-ok
              if passaggio-normale
                 perform APRI-STAMPA
              else
                 perform SALTO-PAGINA
              end-if
              if not spl-sta-annu
                 perform STAMPA-TESTA
                 move low-value  to mro-rec
                 move mto-chiave to mro-chiave
                 start mrordini key is >= mro-chiave
                       invalid continue
                 end-start
                 initialize tab-iva(1) replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 initialize tab-iva(2) replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 initialize tab-iva(3) replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move  0 to tot-colli 
                            tot-peso-tot 
                            tot-peso-utf
                            tot-peso-non-utf
                            totale
                 move 15 to riga
                 move  0 to totale sw-aggiunte 
                                   sw-modificate 
                                   sw-eliminate
                 move 0 to tot-peso-aggiunte
                           tot-utf-aggiunte
                           tot-non-utf-aggiunte
                           tot-colli-aggiunte
                           tot-peso-modificate
                           tot-utf-modificate
                           tot-non-utf-modificate
                           tot-colli-modificate
                           tot-peso-eliminate
                           tot-utf-eliminate
                           tot-non-utf-eliminate
                           tot-colli-eliminate
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read

                    if mto-anno   not = mro-anno   or
                       mto-numero not = mro-numero
                       exit perform
                    end-if

                    if modifica
                       move mro-prg-chiave  to cror-prg-chiave2
                       move mro-des-imballo to cror-des-imballo2
                       if mro-si-blister
                          move 0 to cror-qta-imballi2
                       else
                          move mro-qta-imballi to cror-qta-imballi2
                       end-if
                       move mro-blister     to cror-blister2
                       read check-rordini2 no lock
                            invalid 
                            initialize cror-dati2
                                       replacing numeric data by zeroes
                                            alphanumeric data by spaces
                            move mro-peso-utf     to cror-peso-utf2
                            move mro-peso-non-utf to cror-peso-non-utf2
                        not invalid
                            add 1 to cror-righe2
                       end-read
                       add mro-num-colli to cror-num-colli2
                       add mro-qta to cror-qta2
                       compute cror-importo2 =
                               cror-importo2 + 
                             ( mro-qta * mro-prz-unitario )
                       write cror-rec2 
                             invalid rewrite cror-rec2 
                       end-write
                    end-if

                    move mro-cod-articolo to r-art art-codice
                    read articoli no lock 
                         invalid move spaces to art-descrizione 
                    end-read

                    move mro-num-colli   to r-colli

                    add mro-num-colli    to tot-colli

                    compute tot-peso-utf =
                            tot-peso-utf + 
                          ( mro-peso-utf * mro-qta)
                    compute tot-peso-non-utf =
                            tot-peso-non-utf + 
                          ( mro-peso-non-utf * mro-qta )
                    compute tot-peso-tot =
                            tot-peso-tot + 
                        (( mro-peso-utf + 
                       mro-peso-non-utf ) * mro-qta )

                    move art-descrizione to r-des

                    compute totale = 
                            totale + 
                           ( mro-qta * ( mro-imponib-merce +
                                         mro-add-piombo    +
                                         mro-imp-consumo   +
                                         mro-imp-cou-cobat ) )

                    if mro-si-blister
                       move mro-des-imballo to r-imb
                    else

                       move mro-qta-imballi to imballi-ed
                       inspect mro-des-imballo replacing trailing 
                                                spaces by low-value
                       move mro-qta-imballi to imballi-ed
                       call "C$JUSTIFY" using imballi-ed, "L"
                       initialize r-imb
                       string  |mro-des-imballo delimited by low-value
                               |" da "           delimited by size
                               imballi-ed       delimited by spaces
                               " x "            delimited by size
                               art-udm-imballo  delimited by size
                               into r-imb
                       end-string

                    end-if
                    
                    if mro-qta > mro-qta-e
                       compute como-qta = mro-qta - mro-qta-e
                    else
                       move 0 to como-qta
                    end-if
                    move como-qta          to r-qta

      *    Luciano modifca per qta
                    move mro-qta         to r-qta
      *    
                    if mro-qta > mro-qta-e
                       compute como-qta = mro-qta - mro-qta-e
                    else
                       move 0 to como-qta
                    end-if
                    move como-qta          to r-saldo

                    move mro-imponib-merce to r-prod
                    move r-riga to spl-riga-stampa
                    perform SCRIVI

                    if conferma-ordine
                       perform AGGIUNGI-IVA
                    end-if
      *    Luciano 07/06/2010
      *              subtract 1 from riga
      *    Luciano fine                    
                    move CourierNew10B     to spl-hfont
                    set testata-bold       to true
      *****              move mro-prz-unitario  to r-prz-z
                    compute como-prz = 
                            mro-imponib-merce +
                            mro-add-piombo    +
                            mro-imp-consumo   +
                            mro-imp-cou-cobat                
                    move como-prz  to r-prz-z
                    move r-prz-z           to r-prz(1:9)
                    if mro-si-prz-promo
                       move "*"    to r-prz(10:1)
                    else
                       move spaces to r-prz(10:1)
                    end-if

                    move r-riga-prz-bold   to spl-riga-stampa
                    perform SCRIVI
                    set testata-bold       to false

                    perform STAMPA-SOTTO-COSTO

      *             Luciano
                    perform STAMPA-MANUALE
      *             Luciano fine
                    move CourierNew10      to spl-hfont   

                    if riga >= MaxRighe
      *****                 if passaggio-bloccato
      *****                    perform STAMPA-BLOCCATO
      *****                    move CourierNew10      to spl-hfont
      *****                 end-if
                       perform SALTO-PAGINA
                    end-if

                 end-perform    

                 if riga < MaxRighe
                    move divisorio to spl-riga-stampa
                    perform SCRIVI
                 end-if
                                                
                 move CourierNew10B to spl-hfont
                 move totale   to r-tot
                 move r-totale to spl-riga-stampa
                 perform SCRIVI

                 if riga >= MaxRighe - 7
                    perform SALTO-PAGINA
                 end-if

                 if conferma-ordine
                    perform CALCOLA-IVA
                    move r-totale-iva to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew12B to spl-hfont
                 else
                    move CourierNew12B to spl-hfont
                    move spaces to spl-riga-stampa
                    perform SCRIVI
                 end-if

                 move tot-colli   to r-colli-t
                 move r-colli-tot to spl-riga-stampa
                 perform SCRIVI

                 initialize spl-riga-stampa
                 move tot-peso-utf     to r-peso-utf
                 move r-peso-t-utf     to spl-riga-stampa
                 perform SCRIVI

                 move tot-peso-non-utf to r-peso-non-utf
                 move r-peso-t-non-utf to spl-riga-stampa
                 perform SCRIVI

                 move tot-peso-tot     to r-peso-tot
                 move r-peso-t-tot     to spl-riga-stampa
                 perform SCRIVI

                 accept como-data from century-date
                 accept como-ora  from time
                 string como-data(7:2) delimited size
                        "/"            delimited size
                        como-data(5:2) delimited size
                        "/"            delimited size
                        como-data(1:4) delimited size
                        into r-data
                 end-string
                 string como-ora(1:2) delimited size
                        ":"           delimited size
                        como-ora(3:2) delimited size
                        into r-ora
                 end-string

                 move spaces to spl-riga-stampa
                 perform SCRIVI

                 subtract 0,2 from spl-riga
                 move r-data-ora to spl-riga-stampa
                 perform SCRIVI

      * SE CONTROLLO STAMPA LA QUARTA COPIA
      *****           if passaggio-bloccato
      *****              perform STAMPA-BLOCCATO
      *****           end-if

                 if modifica
                    perform CONFRONTA-FILES
                 end-if
              end-if
           end-if.

      ***---
       APRI-STAMPA.
           if passaggio-normale
              evaluate true
              when conferma-ordine
                   perform CREA-PDF
                   if settaPDF-OK
                      accept selprint-stampante 
                      from environment "CONFERMA_STAMPANTE_DIRETTA"
                   else
                      move spaces to selprint-stampante
                   end-if

              when manuale
                   call   "selprint" using selprint-linkage
                   cancel "selprint"
              when elenco
                   move link-path to selprint-stampante
              when other
                   set cli-tipo-C to true
                   move mto-cod-cli to cli-codice
                   read clienti  no lock
                   move cli-tipo to tcl-codice
                   read ttipocli no lock invalid continue end-read   
                   move tcl-stampante-m to selprint-stampante
                   if selprint-stampante = spaces
                      if tcl-gdo-si or tcl-gdo-opz                 
                         evaluate true
                         when ru-SO-XP
                              accept selprint-stampante 
                              from environment "STAMPANTE_MASTER_GDO_XP"
                         when ru-SO-VISTA
                              accept selprint-stampante 
                              from environment "STAMPANTE_MASTER_GDO_V"
                         when ru-SO-7
                              accept selprint-stampante 
                              from environment "STAMPANTE_MASTER_GDO_7"
                         end-evaluate
                      else
                         evaluate true
                         when ru-SO-XP
                              accept selprint-stampante 
                              from environment"STAMPANTE_MASTER_TRAD_XP"
                         when ru-SO-VISTA
                              accept selprint-stampante 
                              from environment "STAMPANTE_MASTER_TRAD_V"
                         when ru-SO-7
                              accept selprint-stampante 
                              from environment "STAMPANTE_MASTER_TRAD_7"
                         end-evaluate
                      end-if
                   end-if
              end-evaluate
           else
      *    se è il secondo passaggio non devo far nulla, visto che ho 
      *    già scelto la stampante
              continue
           end-if
           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              set spl-vertical         to true
              set spl-apertura         to true
              move 1                   to spl-margine-sinistro
              move 1                   to spl-margine-destro
              move 1                   to spl-margine-inf
              move "GESLUX - Stampa MASTER"     to spl-nome-job
              call "spooler"        using spooler-link

              compute max-righe-corpo = spl-altezza - 10
              compute pos-piede       = spl-altezza -  8
           else
              set spl-sta-annu to true
           end-if.

           if spl-sta-annu 
              cancel "spooler"
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move CourierNew10 to spl-hfont
              call "spooler" using spooler-link
           end-if.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".

      ***---
       CREA-PDF.
           accept DestFile from environment "CONFERMA_PATH_PDF".
      *    tolgo l'eventuale barra finale
      *****     inspect DestFile replacing trailing spaces by low-value.
      *****     initialize cont.
      *****     inspect DestFile tallying cont
      *****             for characters before low-value.
      *****     if DestFile(cont:1) = "\" 
      *****        move low-value  to DestFile(cont:1)
      *****     end-if.
      *****     inspect DestFile replacing trailing low-value by spaces.
      *****
           accept como-data from century-date.
           accept como-ora  from time.                      

           move mto-cod-cli to cli-codice.
           set  cli-tipo-C  to true.
           read clienti no lock.

           inspect cli-ragsoc-1 replacing trailing  space by low-value.
           inspect cli-ragsoc-1 replacing all "."   by spaces.
           inspect cli-ragsoc-1 replacing all space by "_".

           string "CONFERMA_ORDINE_" delimited size
                  cli-ragsoc-1       delimited low-value
                  "_("               delimited size
                  como-data(7:2)     delimited size
                  "-"                delimited size
                  como-data(5:2)     delimited size
                  "-"                delimited size
                  como-data(1:4)     delimited size
                  "_"                delimited size
                  como-ora(1:2)      delimited size
                  "-"                delimited size   
                  como-ora(3:2)      delimited size
                  "-"                delimited size   
                  como-ora(5:2)      delimited size
                  ")"                delimited size
                  into NomeFile
           end-string.

           set settaPDF-setta to true.

           move NomeFile  to settaPDF-nome-file.
           move DestFile  to settaPDF-percorso.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".

      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     string NomeFile   delimited low-value
      *****            ".pdf"     delimited size
      *****            into NomeFile
      *****            
      *****     inspect DestFile replacing trailing spaces by low-value.
      *****         
      *****     initialize link-path.       
      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     else
      *****        string DestFile   delimited by low-value
      *****               "\"        delimited by size
      *****               NomeFile   delimited by low-value
      *****               into link-path
      *****     end-if.           
           

      ***---
       STAMPA-TESTA.
           set StampaAgente      to false.
           set spl-stringa       to true.
           move TimesNewRoman20B to spl-hfont.
           move 0,5              to spl-riga.
           move 4,5              to spl-colonna.
           evaluate true
           when inserimento
           when tradizionale
           when elenco
                call "C$CALLEDBY" using PgmChiamante
                evaluate PgmChiamante
                when "gordc" 
                when "gordct" 
                when "evaord" 
                     move "SCONTRINO ORDINE INSERITO" to spl-riga-stampa
                when "EDI-selordini"
                when "edi-selordini"
                     move "SCONTRINO ORDINE INSERITO" to spl-riga-stampa
                     set stampa-bozza-EDI to true
                when "ordine"
                when "ordinevar"
                when "stscontrini"
                     move "SCONTRINO MASTER"          to spl-riga-stampa
                     set StampaAgente to true
                when other
                     move "NOTIFICA PREZZO MODIFICATO"to spl-riga-stampa
                end-evaluate
           when modifica
                move "NOTIFICA MASTER MODIFICATO" to spl-riga-stampa
           when prezzo-master
                move "NOTIFICA PREZZO MODIFICATO" to spl-riga-stampa
           when manuale
                move "STAMPA MANUALE MASTER" to spl-riga-stampa
           when conferma-ordine
                move "CONFERMA ORDINE"       to spl-riga-stampa
           end-evaluate.
           call "spooler" using spooler-link.

           move CourierNew10 to spl-hfont.

           move 2           to spl-riga.
           
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           if mto-prg-destino not = 0
              move mto-cod-cli     to des-codice
              move mto-prg-destino to des-prog
              read destini no lock invalid continue end-read
           end-if.

           move cli-codice   to codice-x.
           inspect codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using codice-x, "L".
           inspect codice-x replacing trailing spaces by low-value.
           initialize t1-cli-ragsoc.
           string codice-x     delimited low-value
                  " - "        delimited size
                  cli-ragsoc-1 delimited size
                  into t1-cli-ragsoc
           end-string.
                                                                   
           initialize t1-des-ragsoc.
           if des-prog not = 0
              move des-prog    to codice-x
              inspect codice-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using codice-x, "L"
              inspect codice-x replacing trailing spaces by low-value
              string codice-x     delimited low-value
                     " - "        delimited size
                     des-ragsoc-1 delimited size
                     into t1-des-ragsoc
              end-string
           end-if.

           move t1           to spl-riga-stampa.
           perform SCRIVI.

           move cli-indirizzo to t2-cli-indirizzo.
           move des-indirizzo to t2-des-indirizzo.
           move t2           to spl-riga-stampa.
           perform SCRIVI.

           move cli-cap      to t3-cli-cap.
           if cli-prov not = spaces
              inspect cli-localita 
                      replacing trailing spaces by low-value
              initialize t3-cli-localita
              string cli-localita delimited low-value
                     " - "        delimited size
                     cli-prov     delimited size
                into t3-cli-localita
              end-string
           else
              move cli-localita to t3-cli-localita
           end-if.
           move des-cap      to t3-des-cap.
           if des-prov not = spaces
              inspect des-localita 
                      replacing trailing spaces by low-value
              initialize t3-des-localita
              string des-localita delimited low-value
                     " - "        delimited size
                     des-prov     delimited size
                into t3-des-localita
              end-string
           else
              move des-localita to t3-des-localita
           end-if.
           move t3           to spl-riga-stampa.
           perform SCRIVI.

           add 0,3 to spl-riga.

           move mto-numero to t4-numero.
           string mto-data-creazione(7:2) delimited size
                  "/"                     delimited size
                  mto-data-creazione(5:2) delimited size
                  "/"                     delimited size
                  mto-data-creazione(1:4) delimited size
                  into t4-del
           end-string.
           move mto-ora-creazione(1:2) to t4-hh.
           move mto-ora-creazione(3:2) to t4-mm.
           move mto-utente-creazione   to t4-utente.
           move t4           to spl-riga-stampa.
           perform SCRIVI.

           move mto-num-ord-cli to t5-num-ord-cli.
           string mto-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(1:4) delimited size
                  into t5-del
           end-string.
           if mto-ritira-si
              move "RITIRA IN LUBEX" to t5-ritira
           else
              move spaces            to t5-ritira
           end-if.                             
           move t5           to spl-riga-stampa.
           perform SCRIVI.

           move "PA"              to tblpa-codice1.
           move mto-cod-pagamento to tblpa-codice2 t6-cod-pag.
           read tcodpag no lock 
                invalid move "** NON TROVATO **" to tblpa-descrizione1
           end-read.
           move tblpa-descrizione1 to t6-des-pag.

           move spaces to age-ragsoc-1.
           move mto-cod-agente to age-codice t6-cod-age.
           read agenti no lock invalid continue end-read.
           move age-ragsoc-1 to t6-des-age.

           move t6                 to spl-riga-stampa.
           perform SCRIVI.

      *****     add 0,3 to spl-riga.

           move mto-note1 to t7-note1.
           string mto-data-note1(7:2) delimited size
                  "/"                 delimited size
                  mto-data-note1(5:2) delimited size
                  "/"                 delimited size
                  mto-data-note1(1:4) delimited size
                  into t7-data
           end-string.
           move mto-note2 to t7-note2.
           move t7        to spl-riga-stampa.
           perform SCRIVI.

           move mto-note3 to t8-note3.
           move mto-note4 to t8-note4.
           move t8        to spl-riga-stampa.
           perform SCRIVI.
                          
           move divisorio to spl-riga-stampa.
           perform SCRIVI.
           
           move testata  to spl-riga-stampa.
           perform SCRIVI.
                                           
           move testata-prz-bold  to spl-riga-stampa.
           move CourierNew10B     to spl-hfont.
           set testata-bold       to true.
           perform SCRIVI.
           set testata-bold to false.

           move CourierNew10 to spl-hfont.

           move divisorio to spl-riga-stampa.
           perform SCRIVI.

      ***---
       CONFRONTA-FILES.
           move 0 to idx.
           move low-value to cror-rec2.
           start check-rordini2 key >= cror-chiave2 end-start.
           perform until 1 = 2
              read check-rordini2 next at end exit perform end-read
              move cror-chiave2 to cror-chiave
              read check-rordini no lock
                   invalid
                   add  1 to idx

                   move cror-prg-cod-articolo2 to mro-cod-articolo
                   move cror-num-colli2        to mro-num-colli
                   move cror-blister2          to mro-blister
                   move cror-qta-imballi2      to mro-qta-imballi
                   move cror-des-imballo2      to mro-des-imballo
                   move cror-qta2              to mro-qta
                                
                   compute mro-prz-unitario =
                           cror-importo2 / cror-qta2

                   move 1 to sw-aggiunte
                   perform ELEMENTO
                   set el-aggiunta(idx)  to true
                   set el-qta-pos(idx)   to true
                   add mro-num-colli to tot-colli-aggiunte
                   compute tot-utf-aggiunte  =
                           tot-utf-aggiunte  +
                         ( mro-qta * cror-peso-utf2)

                   compute tot-non-utf-aggiunte =
                           tot-non-utf-aggiunte +
                         ( mro-qta * cror-peso-non-utf2)

                   compute tot-peso-aggiunte =
                           tot-peso-aggiunte +
                         ( mro-qta * cror-peso-utf2 ) +
                         ( mro-qta * cror-peso-non-utf2 )
               not invalid
                   if cror-qta     not = cror-qta2 |or
      *****                cror-importo not = cror-importo2
                      add 1 to idx
                      move cror-rec2 to mro-rec
                      move 1 to sw-modificate

                      move cror-prg-cod-articolo to mro-cod-articolo
                      move cror-blister          to mro-blister
                      move cror-qta-imballi      to mro-qta-imballi
                      move cror-des-imballo      to mro-des-imballo
                      evaluate true
                      when cror-qta2 > cror-qta
                           compute mro-qta = cror-qta2 - cror-qta
                      when cror-qta2 < cror-qta
                           compute mro-qta = cror-qta - cror-qta2
                      when other
                           move cror-qta2 to mro-qta
                      end-evaluate
      
                      compute mro-prz-unitario =
                              cror-importo2 / cror-qta2

      *****                compute importo-singolo =
      *****                        cror-importo / cror-qta
      *****
      *****                compute importo-singolo2 =
      *****                        cror-importo2 / cror-qta2
      *****
      *****                evaluate true
      *****                when importo-singolo2 > importo-singolo
      *****                     compute mro-prz-unitario = 
      *****                             cror-importo2 - 
      *****                             cror-importo
      *****                when importo-singolo2 < importo-singolo
      *****                     compute mro-prz-unitario = 
      *****                             cror-importo - 
      *****                             cror-importo2
      *****                when other
      *****                     move importo-singolo to mro-prz-unitario
      *****                end-evaluate

                      evaluate true
                      when cror-num-colli2 > cror-num-colli
                           compute mro-num-colli  = 
                                  cror-num-colli2 - 
                                  cror-num-colli
                      when cror-num-colli2 < cror-num-colli
                           compute mro-num-colli = 
                                  cror-num-colli - 
                                  cror-num-colli2
                      when other
                           move cror-num-colli2 to mro-num-colli
                      end-evaluate

                      perform ELEMENTO

                      if cror-qta2 < cror-qta
                         set el-qta-neg(idx)   to true
                      else
                         set el-qta-pos(idx)   to true
                      end-if

                      set el-modificata(idx) to true

                      if el-qta-pos(idx)
                         add mro-num-colli to tot-colli-modificate
                         compute tot-utf-modificate  =
                                 tot-utf-modificate  +
                               ( mro-qta * cror-peso-utf)

                         compute tot-non-utf-modificate =
                                 tot-non-utf-modificate +
                               ( mro-qta * cror-peso-non-utf)

                         compute tot-peso-modificate =
                                 tot-peso-modificate +
                                 ( mro-qta * cror-peso-utf ) +
                                 ( mro-qta * cror-peso-non-utf)
                      else
                        subtract mro-num-colli from tot-colli-modificate
                         compute tot-utf-modificate  =
                                 tot-utf-modificate  -
                               ( mro-qta * cror-peso-utf)

                         compute tot-non-utf-modificate =
                                 tot-non-utf-modificate -
                               ( mro-qta * cror-peso-non-utf)

                         compute tot-peso-modificate =
                                 tot-peso-modificate -
                                 ( mro-qta * cror-peso-utf ) -
                                 ( mro-qta * cror-peso-non-utf)
                      end-if 
                   end-if
              end-read
           end-perform.  

           move low-value to cror-rec.
           start check-rordini key >= cror-chiave end-start.
           perform until 1 = 2
              read check-rordini next at end exit perform end-read
              move cror-chiave to cror-chiave2
              read check-rordini2 no lock
                   invalid
                   add  1 to idx
                   move cror-prg-cod-articolo to mro-cod-articolo
                   move cror-num-colli        to mro-num-colli
                   move cror-blister          to mro-blister
                   move cror-qta-imballi      to mro-qta-imballi
                   move cror-des-imballo      to mro-des-imballo
                   move cror-qta              to mro-qta

                   compute mro-prz-unitario =
                           cror-importo / cror-qta

                   move 1 to sw-eliminate
                   perform ELEMENTO
                   set el-eliminata(idx) to true
                   set el-qta-neg(idx)   to true
                   add mro-num-colli to tot-colli-eliminate
                   compute tot-utf-eliminate  =
                           tot-utf-eliminate  +
                         ( mro-qta * cror-peso-utf)

                   compute tot-non-utf-eliminate =
                           tot-non-utf-eliminate +
                         ( mro-qta * cror-peso-non-utf)

                   compute tot-peso-eliminate =
                           tot-peso-eliminate +
                         ( mro-qta * cror-peso-utf ) +
                         ( mro-qta * cror-peso-non-utf)
              end-read
           end-perform.

           if sw-aggiunte = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-AGGIUNTE
              add 1   to riga
           end-if.

           if sw-modificate = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-MODIFICATE
              add 1   to riga
           end-if.

           if sw-eliminate = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-ELIMINATE
              add 1   to riga
           end-if.

      ***---
       ELEMENTO.
           move mro-cod-articolo to r-art art-codice.
           read articoli no lock 
                invalid move spaces to art-descrizione 
           end-read.

           move mro-num-colli   to r-colli.
           move art-descrizione to r-des.

           if mro-si-blister
              move mro-des-imballo to r-imb
           else
              move mro-qta-imballi to imballi-ed
              inspect mro-des-imballo replacing trailing 
                                       spaces by low-value
              call "C$JUSTIFY" using imballi-ed, "L"
              initialize r-imb
              string  mro-des-imballo delimited by low-value
                      " da "           delimited by size
                      imballi-ed       delimited by spaces
                      " x "            delimited by size
                      art-udm-imballo  delimited by size
                      into r-imb
              end-string
           end-if.
           
           if mro-qta > mro-qta-e
              compute como-qta = mro-qta - mro-qta-e
           else
              move 0 to como-qta
           end-if.
           move como-qta          to r-qta.
           move mro-prz-unitario  to r-prz-z.
           move r-prz-z           to r-prz.

           move r-riga to el-r-riga(idx).
           move mro-prz-unitario  to el-r-prz(idx).
      *****     move 0      to el-r-prz(idx).

      ***---
       STAMPA-AGGIUNTE.
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-aggiunta(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if

                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B    to spl-hfont
                    move "RIGHE AGGIUNTE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if

                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.
                                                
           add 0,2                 to spl-riga.
           move CourierNew12B      to spl-hfont.
           move tot-colli-aggiunte to r-colli-t.
           move r-colli-tot        to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-aggiunte to r-peso-utf.
           move r-peso-t-utf     to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-aggiunte to r-peso-non-utf.
           move r-peso-t-non-utf     to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-aggiunte to r-peso-tot.
           move r-peso-t-tot      to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-MODIFICATE.   
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-modificata(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if
                                   
                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B      to spl-hfont
                    move "RIGHE MODIFICATE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if
                                       
                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.
                                             
           add 0,2                   to spl-riga.
           move CourierNew12B        to spl-hfont.
           move tot-colli-modificate to r-colli-t.
           move r-colli-tot          to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-modificate to r-peso-utf.
           move r-peso-t-utf       to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-modificate to r-peso-non-utf.
           move r-peso-t-non-utf       to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-modificate to r-peso-tot.
           move r-peso-t-tot        to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-ELIMINATE.
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-eliminata(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if
                                   
                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B     to spl-hfont
                    move "RIGHE ELIMINATE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if
                                               
                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.

           move CourierNew12B       to spl-hfont.
           add 0,2                  to spl-riga.
           move tot-colli-eliminate to r-colli-t.
           move r-colli-tot         to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-eliminate to r-peso-utf.
           move r-peso-t-utf      to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-eliminate to r-peso-non-utf.
           move r-peso-t-non-utf      to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-eliminate to r-peso-tot.
           move r-peso-t-tot       to spl-riga-stampa.
           perform SCRIVI.

      ***---
       SCRIVI.
           evaluate true
           when testata-bold
                subtract 0,5 from spl-riga
                move 16,95      to spl-colonna
           when testata-SC
                subtract 0,5 from spl-riga
                move 19,2      to spl-colonna
           when testata-M
                subtract 0,5 from spl-riga
      *    Luciano 07/06/2010
      *          move 20,0      to spl-colonna
                move 19,75     to spl-colonna
      *    Luciano fine
           when other
                move 0,3 to spl-colonna
           end-evaluate.
           set  spl-stringa  to true.
           call "spooler" using spooler-link.
           add  0,5 to spl-riga.
      *    Luciano 07/06/2010
      *     add  1   to riga.
           evaluate true
           when testata-bold
           when testata-SC
           when testata-M
                continue
           when other
                add  1   to riga
           end-evaluate.
      *    Luciano fine

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
      *    luciano
           set spl-stringa to true
           |Mi riposiziono ad inizio foglio
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
           move spaces to spl-riga-stampa.
           perform SCRIVI.
      *    luciano
           move 0 to riga.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.

           initialize WFONT-DATA.
           move 10                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew10, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 10                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew10B, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 12                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew12B, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize wfont-data.
           move 20                  to wfont-size.
           move "Times New Roman"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               TimesNewRoman20B, wfont-data
                               giving wfont-status.

           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize WFONT-DATA.
           move 30                  to WFONT-SIZE.
           move "Times New Roman"   to WFONT-NAME.
           set WFONT-BOLD           to true.
           set WFCHARSET-DONT-CARE  to true    
           set WFONT-ITALIC         to false   
           set WFONT-UNDERLINE      to false   
           set WFONT-STRIKEOUT      to false   
           set WFONT-FIXED-PITCH    to false   
           move ZERO                to WFONT-CHAR-SET
           set WFDEVICE-WIN-PRINTER to true.
           CALL "W$FONT" USING WFONT-GET-CLOSEST-FONT, 
                               Font-Copia-TimesNewRoman30, WFONT-DATA
                               giving WFONT-STATUS.

           if WFONT-STATUS = WFONTERR-FONT-NOT-FOUND
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.


      ***---
       DISTRUGGI-FONT.
           Destroy CourierNew10.
           Destroy TimesNewRoman20B.
           destroy Font-Copia-TimesNewRoman30.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close mtordini mrordini articoli clienti distinteb timposte
                 destini  progmag  ttipocli tcodpag tivaese tscorte
                 impforn tparamge tpiombo destinif tlistini param 
                 rlistini agenti.

           if modifica
              close check-rordini check-rordini2
              delete file check-rordini2
           end-if.

      ***---
       EXIT-PGM.
           destroy TimesNewRoman20B 
                   CourierNew10 
                   CourierNew10B

                   CourierNew12B.

           if stampa-bozza-EDI
              move mto-anno   to EDI-stb-da-anno EDI-stb-a-anno
              move mto-numero to EDI-stb-da-num  EDI-stb-a-num
              move 0          to EDI-stb-da-data
              move 99999999   to EDI-stb-a-data
              move selprint-stampante   to EDI-stb-stampante
              call   "EDI-stbozze-p" using edi-stb-limiti
              cancel "EDI-stbozze-p"
           end-if.

           goback.

      ***---
       STAMPA-SOTTO-COSTO.
           initialize prg-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces
           move mro-prg-cod-articolo to prg-cod-articolo
           read progmag no lock invalid continue end-read.
           move prg-costo-ultimo to costo-ultimo

           move mro-prg-chiave  to prg-chiave
           read progmag no lock invalid continue end-read.
           perform CALCOLA-COSTO-MP
           
           perform TROVA-LISTINO-PIU-BASSO.

           if costo-mp         = 0
              move 99999999 to costo-mp
           end-if
           if costo-ultimo     = 0 
              move 99999999 to costo-ultimo
           end-if.
           if prezzo-confronto = 0
              move 99999999 to prezzo-confronto
           end-if.

           compute min-value = 
              function MIN (costo-mp, costo-ultimo, prezzo-confronto).

              if mro-cod-articolo = 9721 stop "K" end-if

           if ( mro-imp-consumo   +
                mro-imp-cou-cobat +
                mro-add-piombo    +
                mro-imponib-merce )
               < min-value
              set testata-SC to true                       
              set spl-rosso  to true
      *        move 20        to spl-colonna
              move "SC"      to spl-riga-stampa
              perform SCRIVI
              set testata-SC to false
              set spl-nero   to true
           end-if.      

      ***---
       TROVA-LISTINO-PIU-BASSO.
           move 99999999 to prezzo-confronto.
           move low-value        to rlis-chiave-ricerca
           move prg-cod-articolo to rlis-articolo

           start rlistini key >= rlis-k-art 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    if rlis-articolo not = prg-cod-articolo
                       exit perform
                    end-if

                    if data-oggi >= rlis-ini-val and 
                       data-oggi <= rlis-fine-val  
                       move rlis-fornitore     to cli-codice
                       set cli-tipo-F to true
                       read clienti no lock
                            invalid continue
                        not invalid
                            move rlis-fornitore to desf-codice
                            move rlis-destino   to desf-prog
                            read destinif no lock 
                                 invalid continue
                            end-read
                       end-read

                       move rlis-codice       to tlis-codice
                       read tlistini no lock invalid continue end-read 
                       move tlis-fornitore    to desf-codice
                       move rlis-destino      to desf-prog
                       read destinif no lock 
                            invalid
                            display message 
                            "Fornitore: " cli-codice, " - " cli-ragsoc-1
                     x"0d0a""Destino: " desf-prog     
                     x"0d0a""Non trovato."
                     x"0d0a""Verrà considerato come italiano per il "
                            "calcolo del trasporto"
                                     title titolo
                                      icon 2
                            move "ITA" to desf-nazione
                       end-read
                                                                
                       move tlis-trasp-f     to como-trasporto-f
                       move tlis-trasp-c     to como-trasporto-c
                       perform CALCOLA-PRZ-FINITO
                       add 0,0005             to prz-confronto
                       add 0,005              to prz-confronto

                       add 0,0005             to prz-reale
                       add 0,005              to prz-reale
           
                       if prz-confronto < prezzo-confronto
                          move prz-confronto to prezzo-confronto
                       end-if

                    end-if
                 end-perform
           end-start.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock.   
           if como-trasporto-f = 1
              compute costo-trasporto = 
                      prg-peso * tge-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    ( prg-peso * tge-trasp-c)
           end-if. 

      ***---
       STAMPA-MANUALE.
           if mro-prz-manuale-si
              set testata-M  to true                       
              set spl-blu    to true
      *        move 20,0      to spl-colonna
              move "M"       to spl-riga-stampa
              perform SCRIVI
              set testata-M  to false
              set spl-nero   to true
           end-if.   

      ********---
      ***** STAMPA-BLOCCATO.
      *****     move Font-Copia-TimesNewRoman30 to spl-hfont
      *****     move 25                         to spl-riga
      *****     move 06                         to spl-colonna
      *****     move "BLOCCATO"                 to spl-riga-stampa
      *****     call "spooler"         using spooler-link.
                                        

      ***---
      *    paragrafo inutile, inserito solo per compilare visto che è 
      *    richiesto dalla copy costo-medio.cpy
       RECUPERO-ANAGRAFICA.

      ***---
       ASPETTA-PDF.
      *****     move link-path to como-nome-file.
      *****     move 0 to cont.
      *****     inspect como-nome-file 
      *****             tallying cont for characters before ")".
      *****     move ".pdf " to como-nome-file(cont + 1: 5).
      *****
      *****     set trovato to false.
      *****     perform 60 times
      *****        CALL "C$FILEINFO" USING link-path,
      *****                                file-info, 
      *****                         GIVING status-code
      *****        if status-code = 0
      *****           set trovato to true
      *****           exit perform
      *****        else
      *****           CALL "C$FILEINFO" USING como-nome-file,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****           if status-code = 0
      *****              move como-nome-file to link-path
      *****              set trovato to true
      *****              exit perform
      *****           end-if
      *****        end-if
      *****        call "c$sleep" using 1
      *****     end-perform.
      *****
      *****     if trovato
      *****        move 0  to old-size
      ******       aspetto finchè non esiste fisicamente il file
      *****        move 99 to minuti-partenza
      *****        perform until 1 = 2
      *****           CALL "C$FILEINFO" USING link-path,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****        
      *****           if status-code = 0
      *****              if FILE-SIZE not = 0
      *****                 if FILE-SIZE = old-size
      *****                    exit perform
      *****                 else
      *****                    move FILE-SIZE to old-size
      *****                    call "c$sleep" using 1
      *****                 end-if
      *****              end-if
      *****           else
      *****              perform TIME-OUT
      *****              if time-out-exit
      *****                 exit perform
      *****              end-if
      *****           end-if
      *****        
      *****        end-perform
      *****     end-if.

           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".  

           if settaPDF-OK
              move settaPDF-nome-file to link-path
           end-if.  

      ***---
       AGGIUNGI-IVA.
           move 0 to idx-iva.
           perform 3 times
              add 1 to idx-iva
              if mro-cod-iva = el-iva(idx-iva)
                 exit perform
              end-if
              if el-iva(idx-iva) = spaces

                 move "IV"        to tbliv-codice1
                 move mro-cod-iva to tbliv-codice2
                 read tivaese 
                 move tbliv-percentuale to el-aliq(idx-iva)
                 move mro-cod-iva to el-iva(idx-iva)

                 exit perform
              end-if
           end-perform.

           compute el-imponib(idx-iva) = 
                   el-imponib(idx-iva) +
                (( mro-imponib-merce   + 
                   mro-add-piombo      + 
                   mro-imp-consumo     + 
                   mro-imp-cou-cobat ) * mro-qta ).

      ***---
       CALCOLA-IVA.
           move 0 to totale.
           move 0 to idx-iva.
           perform 3 times
              add 1 to idx-iva
              move 0 to como-iva
              if el-iva(idx-iva) = spaces
                 exit perform
              end-if
              if el-aliq(idx-iva)    not = 0 and
                 el-imponib(idx-iva) not = 0
                 compute como-iva = 
                   ( el-imponib(idx-iva) * el-aliq(idx-iva) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              end-if
              compute totale = totale + como-iva + el-imponib(idx-iva)
           end-perform.
           move totale to r-tot-iva.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "prz-finito-forn.cpy".
           copy "imposte-fornitore.cpy".
           copy "addizionale-piombo-fornitore.cpy".  
           copy "trova-parametro.cpy".
