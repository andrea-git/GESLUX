      ***---
       CARICA-FONT.
      * Arial 8
           initialize wfont-data Arial8B.
           move 8 to wfont-size.
           move "Arial"              to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Arial8B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.
      *----
          
      * Courier New 7
           initialize wfont-data CourierNew7.
           move 7 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew7, wfont-data
                        giving WFONT-STATUS.

                        
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.
          
      * Courier New 6
           initialize wfont-data CourierNew6.
           move 6 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew6, wfont-data
                        giving WFONT-STATUS.

                        
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.
 
      * Courier New 9
           initialize wfont-data CourierNew9.
           move 9 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew9, wfont-data
                        giving WFONT-STATUS.
 

           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Courier New 11
           initialize wfont-data CourierNew11.
           move 11 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew11, wfont-data
                        giving WFONT-STATUS.

           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by low-value.

           display message messaggio.
       
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
           move rno-add-piombo        to ror-add-piombo.
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
       MOVE-TESTA-NOTA-TO-ORDINE.
           initialize tor-rec   replacing numeric data by zeroes
                                     alphanumeric data by spaces.
           move tno-anno                    to tor-anno.
           move tno-numero                  to tor-numero.
           move tno-causale                 to tor-causale.
           move tno-cod-cli                 to tor-cod-cli.
           move tno-prg-destino             to tor-prg-destino.
           move tno-data-passaggio-ordine   to tor-data-passaggio-ordine
           move tno-cod-agente              to tor-cod-agente.
           move tno-cod-pagamento           to tor-cod-pagamento.
           move tno-cod-ese-iva             to tor-cod-ese-iva.
           move tno-spostam-ric-ago         to tor-spostam-ric-ago.
           move tno-spostam-ric-dic         to tor-spostam-ric-dic.
           move tno-note                    to tor-note1.
           move tno-invio                   to tor-invio.
           move tno-fattura                 to tor-fattura.
           move tno-agg-contab              to tor-agg-contab.
           move tno-stato                   to tor-stato.
           move spaces                      to tor-num-ord-cli.

      ***---
       STAMPA-INTESTAZIONE.
           initialize st-intestazione
                      st-riga
                      st-riga-totali
                      cli-rec
                      record-tblpa
                      des-rec
                      rec-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           move CourierNew11 to spl-hfont.
           if NumPagina not = 0
              set spl-salto-pagina to true
              call "spooler" using spooler-link
           end-if.

           set spl-stringa to true.
           |Mi riposiziono ad inizio foglio
           move 0      to spl-riga.
           move spaces to spl-riga-stampa.
           call "spooler" using spooler-link.

           set NewPage to false.
           add 1       to NumPagina.

           perform STAMPA-BITMAP.

           set spl-stringa to true.
           move 0,5        to spl-passo.
           initialize spl-riga-stampa.
           set EsisteDichiarazione to false.                     
           move tor-cod-cli  to rec-codice, cli-codice.
           set  cli-tipo-C   to true.
           read clienti no   lock invalid continue end-read.
           if cli-dich-esp   not = spaces or
              cli-num-reg    not = spaces or
              cli-data-dich  not = 0      or
              cli-data-reg   not = 0
              set EsisteDichiarazione to true
           end-if.

           |Leggo CLI di G2 per vedere se il cliente ha lo
           |spostamento delle scadenze di Agosto e Dicembre
           move cli-codice to cli-codice-G2.
           read CLI no lock invalid continue end-read.

           move "PA"              to tblpa-codice1.
           move tor-cod-pagamento to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.

           set des-no-invio to true.

           if tor-prg-destino not = 0
              move cli-codice      to des-codice
              move tor-prg-destino to des-prog
              read destini invalid continue end-read
           end-if.
                          
           set EsisteRecapito to true
           if des-si-invio
              move des-ragsoc-1  to rec-ragsoc-1
              move des-indirizzo to rec-indirizzo
              move des-cap       to rec-cap
              move des-localita  to rec-localita
              move des-prov      to rec-provincia
           else
              read recapiti no lock
                   invalid
                   set EsisteRecapito to false
               not invalid
      *    ISACCO (1/6/04 - CORREZIONE AL PROBLEMA DI ALLINEAMENTO SENZA
      *    I RECAPITI VALORIZZATI)
                   if rec-ragsoc-1       = spaces and  
                      rec-indirizzo      = spaces and  
                      rec-cap            = spaces and  
                      rec-localita       = spaces and  
                      rec-provincia      = spaces
                      set EsisteRecapito to false
                   end-if
      * FINE MODIFICA
              end-read
           end-if.

           move 4,5 to spl-riga.
           move 0,5 to spl-passo.

           initialize st-riga-cli-rec-3.
           if EsisteRecapito
      
              move rec-ragsoc-1       to st-rec-ragsoc-1
      
              move rec-indirizzo      to st-rec-indirizzo
      
              move rec-cap            to st-rec-cap
              move rec-localita       to st-rec-localita
              move rec-provincia      to st-rec-provincia

              move cli-ragsoc-1       to st-cli-ragsoc-1
              move st-riga-cli-rec-1  to spl-riga-stampa
              move 1                  to spl-tipo-colonna
              perform SCRIVI

              move spaces             to spl-riga-stampa 
              move 1                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI
      
              move cli-indirizzo      to st-cli-indirizzo
              move st-riga-cli-rec-2  to spl-riga-stampa 
              move 1                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI
      
              move cli-cap of clienti to st-cli-cap
              move cli-localita       to st-cli-localita
              move cli-prov           to st-cli-prov
              move st-riga-cli-rec-3  to spl-riga-stampa 
              move 2                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI

           else

              move cli-ragsoc-1       to st-rec-ragsoc-1
              move st-riga-cli-rec-1  to spl-riga-stampa
              move 1                  to spl-tipo-colonna
              perform SCRIVI

              move spaces             to spl-riga-stampa
              move 1                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI

              move cli-indirizzo      to st-rec-indirizzo
              move st-riga-cli-rec-2  to spl-riga-stampa
              move 1                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI

              move cli-cap of clienti to st-rec-cap
              move cli-localita       to st-rec-localita
              move cli-prov           to st-rec-provincia
              move st-riga-cli-rec-3  to spl-riga-stampa
              move 2                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI

           end-if.
      *
           move 2,4 to spl-passo.

           move tor-num-fattura to st-tor-num-fattura.
           call "C$JUSTIFY"  using st-tor-num-fattura, "R".

           move all "/"               to st-tor-data-fattura.
           move tor-data-fattura(3:2) to st-tor-data-fattura(7:2).
           move tor-data-fattura(5:2) to st-tor-data-fattura(4:2).
           move tor-data-fattura(7:2) to st-tor-data-fattura(1:2).
                                            
           initialize st-tco-descrizione.
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value
           string  tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into st-tco-descrizione
           end-string.

           move cli-abi               to st-cli-abi. 
           move cli-cab               to st-cli-cab.
           move st-riga-int-1         to spl-riga-stampa.
           move 3                     to spl-tipo-colonna.
           add spl-passo              to spl-riga.
           perform SCRIVI.

           move 0,95 to spl-passo.

           move tor-num-ord-cli      to st-tor-num-ord-cli.

           if tor-data-ordine not = 0
              move all "/"              to st-tor-data-ordine
              move tor-data-ordine(3:2) to st-tor-data-ordine(7:2)
              move tor-data-ordine(5:2) to st-tor-data-ordine(4:2)
              move tor-data-ordine(7:2) to st-tor-data-ordine(1:2)
           else
              move spaces               to st-tor-data-ordine
           end-if.

           evaluate true
           when elaborazione-note-credito
           when elaborazione-manuali
                move spaces to st-tor-data-bolla
                move spaces to st-tor-num-bolla
                move spaces to st-tor-data-ordine
           when elaborazione-fatture
                move tor-num-bolla       to st-tor-num-bolla
                call "C$JUSTIFY"      using st-tor-num-bolla, "R"
                move all "/"             to st-tor-data-bolla
                move tor-data-bolla(3:2) to st-tor-data-bolla(7:2)
                move tor-data-bolla(5:2) to st-tor-data-bolla(4:2)
                move tor-data-bolla(7:2) to st-tor-data-bolla(1:2)
           end-evaluate.

           move cli-codice     to codice-ed.

           move spaces to st-cli-age.
           string codice-ed delimited by size
                  "/"       delimited by size
                  into st-cli-age
           end-string.

           if tor-cod-agente = 0
              move "00"           to st-cli-age(7:5)
           else
              move tor-cod-agente to codice-ed
              call "C$JUSTIFY" using codice-ed, "L"
              move codice-ed      to st-cli-age(7:5)
           end-if.
           inspect st-cli-age replacing trailing spaces by low-value.
           string  st-cli-age delimited low-value
                   "/"        delimited size   
                   cli-tipo   delimited size
              into st-cli-age
           end-string.                                               
           inspect st-cli-age replacing trailing low-value by spaces.
           call "C$JUSTIFY"  using st-cli-age, "C".

           if cli-fisica
              move cli-codfis to st-piva-codfis
           else
              move cli-piva   to st-piva-codfis
           end-if.

           move st-riga-int-2 to spl-riga-stampa.
           move 4             to spl-tipo-colonna.
           add spl-passo      to spl-riga.
           perform SCRIVI.
                 
           move 1 to spl-passo.
           move des-ragsoc-1   to st-des-ragsoc-1.
           move des-prog       to st-des-prog.
           call "C$JUSTIFY" using st-des-prog, "R".
           move st-riga-int-3  to spl-riga-stampa.
           move 5              to spl-tipo-colonna.
           add spl-passo       to spl-riga.
           perform SCRIVI.

           move 0,6 to spl-passo.
           move des-indirizzo to st-des-indirizzo.
           if des-prov = spaces
              move des-localita  to st-des-localita
           else
              initialize st-des-localita
              inspect des-localita 
                      replacing trailing spaces by low-value
              string  des-localita delimited low-value
                      " - "        delimited size
                      des-prov     delimited size
                 into st-des-localita
              end-string
              inspect des-localita 
                      replacing trailing low-value by spaces
           end-if.
           move st-riga-int-4 to spl-riga-stampa.
           move 6             to spl-tipo-colonna.
           add spl-passo      to spl-riga.
           perform SCRIVI.

           evaluate true
           when elaborazione-note-credito
                perform STAMPA-NOTE-INTESTAZIONE
           when elaborazione-manuali
                move tor-note to tno-note
                perform STAMPA-NOTE-INTESTAZIONE
           end-evaluate.

      ***---
       STAMPA-NOTE-INTESTAZIONE.
           if prima-pagina set prima-pagina  to false
           else            exit paragraph
           end-if.

           initialize intestazioni-note.

           move 0               to idx.
           move 1               to cont-inizio.
           set exit-perform-int to false.

           perform 100 times
              if cont-inizio >= 500 exit perform end-if
              add  1 to idx                 
              move 0 to cont-char
      *    controllo di non andare oltre i 500 caratteri
              if cont-inizio > 451
                 compute cont-per = 500 - cont-inizio
                 set exit-perform-int to true
              else
                 move 30  to cont-per
              end-if

              inspect tno-note(cont-inizio:cont-per) tallying cont-char
                       for all x"0D"
              if cont-char = zero
                 |move 50  to cont-per
                 continue
              else
                 initialize cont-per
                 inspect tno-note(cont-inizio:30) tallying cont-per
                       for characters before x"0D"
              end-if
              if cont-per not = zero
                 move tno-note(cont-inizio:cont-per) 
                                         to intestazione-note(idx)
      *    se appena dopo i 30 caratteri premo invio devo ignorarlo
                 if cont-per = 30
                    add cont-per  to cont-inizio
                    if cont-inizio < 499
                             and tno-note(cont-inizio:1) = x"0D"
                       add 2 to cont-inizio
                    end-if
                    subtract cont-per from cont-inizio
                 end-if
              else
                 move space  to intestazione-note(idx)
              end-if
              if cont-char = zero
                 add 30   to cont-inizio
              else
                 compute cont-inizio = cont-inizio + cont-per + 2                                                         
              end-if
              if exit-perform-int
                 exit perform
              end-if
           end-perform.

           initialize num-righe-note
           perform varying idx from 1 by 1 until idx > 100
              if intestazione-note(idx) not = space
                 move idx to num-righe-note
              end-if
           end-perform.

           move CourierNew9  to spl-hfont. 
           move 11,9         to spl-riga.
           move 0,4          to spl-passo.
           move 1,75         to spl-colonna.
           perform varying idx from 1 by 1 until idx > num-righe-note
              move intestazione-note(idx) to spl-riga-stampa
              move 0                      to spl-tipo-colonna
              add spl-passo               to spl-riga
              perform SCRIVI
              add 1 to WrittenRows
           end-perform.
           add spl-passo to spl-riga.

      ***---
       STAMPA-BITMAP.
           set spl-bitmap to true.
           move 1,5 to spl-colonna.
           move 3 to spl-riga.

           evaluate true
           when elaborazione-manuali
           when elaborazione-fatture
                move BitmapSfondoHandle   to spl-hbitmap
           when elaborazione-note-credito
                move BitmapSfondoNcHandle to spl-hbitmap
           end-evaluate.         
                                          
           move 26,8 to spl-bitmap-height.
           move 19,5 to spl-bitmap-width.

           call "spooler" using spooler-link.
                      
      ***---
       STAMPA-RIGHE.
OMAGGI     perform SCRITTURA-RIGA.

OMAGGI     if ror-qta-omaggi not = 0
OMAGGI        move tge-cod-iva-omag to ror-cod-iva
OMAGGI        set  ror-si-omaggio   to true
OMAGGI        move ror-qta-omaggi   to ror-qta
OMAGGI        move 0 to ror-qta-omaggi
OMAGGI        perform SCRITTURA-RIGA
OMAGGI     end-if.

OMAGGI***---
OMAGGI SCRITTURA-RIGA.
           if WrittenRows = ( RowsPerPage - 1 - num-righe-note )
              |Dato che dopo la prima pagina l'intestazione
              |non viene più stampata non la devo considerare
              |come limite di righe nelle pagine successive
              move 0 to num-righe-note
              if RowsToDo > 1
                 move 0             to WrittenRows
                 add  0,5           to spl-riga
                 move "...Segue >>" to spl-riga-stampa(100:)
                 perform SCRIVI
                 perform STAMPA-STRINGA-COPIA
                 initialize spooler-link
                 perform STAMPA-INTESTAZIONE
                 move CourierNew9 to spl-hfont
                 move 12,3        to spl-riga
                 move 0,4         to spl-passo
              end-if
           end-if.

           if RigheFinali
              move ror-des-libera      to st-totale
              move ror-imp-cou-cobat   to st-valore
              call "C$JUSTIFY"      using st-valore,   "R"
              move st-riga-tot-imposte to spl-riga-stampa
              move 7,5                 to spl-tipo-colonna
           else
              perform RIGA-NORMALE
              move 7                 to spl-tipo-colonna
           end-if.

           perform SCRIVI.

           add spl-passo to spl-riga.

           add 1 to WrittenRows.
           subtract 1 from RowsToDo.

      ***---
       RIGA-NORMALE.
           initialize record-tbliv
                      art-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           if ror-cod-articolo not = 0
              move ror-cod-articolo to st-ror-cod-articolo art-codice
              read articoli no lock invalid continue end-read
              move art-descrizione(1:30) to st-art-descrizione
              move art-unita-di-misura   to st-art-unita-di-misura
           else
              move space  to st-ror-cod-articolo art-codice
              
              move ror-des-libera(1:43)  to st-art-descrizione
              move space                 to st-art-unita-di-misura              
           end-if

           subtract ror-qta-omaggi from ror-qta.
           move ror-qta             to st-ror-qta.

           if ror-qta = 0 |ad esempio NCNC FTMA
              move 1 to ror-qta
           end-if.

           if ror-no-omaggio

              move ror-imponib-merce   to st-ror-imponib-merce

              move ror-imp-consumo     to st-ror-imp-consumo
              compute como-imposta = ror-imp-cou-cobat  + 
                                     ror-add-piombo
              move como-imposta        to st-ror-imp-cou-cobat

              compute como-imponibile   =
                      ror-imponib-merce +
                      ror-imp-consumo   +
                      ror-imp-cou-cobat +
                      ror-add-piombo

              compute importo-netto    = ror-qta * como-imponibile

              compute imponibile-merce = imponibile-merce + 
                                       ( ror-qta * ror-imponib-merce )
              compute tot-consumo      = tot-consumo      + 
                                       ( ror-qta * ror-imp-consumo )
              compute tot-cou          = tot-cou          + 
                                       ( ror-qta * como-imposta )
              compute tot-imponibile   = tot-imponibile + importo-netto

              move importo-netto        to st-importo-netto

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
              when cod-iva(1) = spaces
                   |Se la prima aliquota è "omaggio" devo
                   |metterla come ultimo elemento dell'occurs
                   if tbliv-codice2 = tge-cod-iva-omag
                      move 3           to idx
                   else
                      move 1           to idx
                   end-if
                   move perce-iva-x to cod-iva(idx)
              when cod-iva(2) = spaces 
                   move perce-iva-x to cod-iva(2)
                   move 2           to idx
              when cod-iva(3) = spaces 
                   move perce-iva-x to cod-iva(3)
                   move 3           to idx
              end-evaluate
              if tbliv-percentuale = 0 set iva-sigla(idx) to true
              else                     set iva-sigla(idx) to false
              end-if
           end-if.

           compute imponibile-iva(idx) = 
                   imponibile-iva(idx) + importo-netto.

           move tbliv-percentuale to st-aliquota.
                            
           if ror-si-omaggio
              move 0                   to imponibile-iva(idx)
              move 0                   to como-imponibile 
              move "OMAGGIO"           to st-ror-omaggio
              move st-ror-cod-articolo to st-ror-cod-articolo-om

              move st-art-descrizione
                to st-art-descrizione-om
              move st-art-unita-di-misura
                to st-art-unita-di-misura-om
              move st-ror-qta         to st-ror-qta-om
              move tbliv-descrizione1 to st-iva-omaggio
              move st-aliquota        to st-aliquota-om
              call "C$JUSTIFY"  using st-ror-cod-articolo-om, "R"
              call "C$JUSTIFY"  using st-ror-qta-om,          "R"
              call "C$JUSTIFY"  using st-ror-omaggio,         "R"
              call "C$JUSTIFY"  using st-aliquota-om,         "R"
              move st-riga-omaggio    to spl-riga-stampa
           else
              call "C$JUSTIFY"  using st-ror-cod-articolo,    "R"
              call "C$JUSTIFY"  using st-ror-qta,             "R"
              call "C$JUSTIFY"  using st-ror-imponib-merce,   "R"
              call "C$JUSTIFY"  using st-ror-imp-consumo,     "R"
              call "C$JUSTIFY"  using st-ror-imp-cou-cobat,   "R"
              call "C$JUSTIFY"  using st-importo-netto,       "R"
              call "C$JUSTIFY"  using st-aliquota,            "R"
              move st-riga         to spl-riga-stampa
           end-if.

      ***---
       STAMPA-PIE-PAGINA.
           move CourierNew11   to spl-hfont.
           move 0,9            to spl-passo.
           move 23,6           to spl-riga.
           move 8              to spl-tipo-colonna.

           perform CALCOLA-IVA.

           move imponibile-merce to st-importo st-importo2.
           perform VALUTA-IVA.
           move tot-imponibile   to st-importo-totale st-importo-totale2
           perform JUSTIFY-RIGHT.
           if UsaPrimaRiga move st-riga-totali   to spl-riga-stampa
           else            move st-riga-totali2  to spl-riga-stampa
           end-if.
           perform SCRIVI.
           add spl-passo       to spl-riga.

           add tot-imponibile  to tot-iva giving tot-fattura.
           move tot-consumo    to st-importo st-importo2.
           perform VALUTA-IVA.
           move tot-iva        to st-importo-totale st-importo-totale2.
           perform JUSTIFY-RIGHT.
           if UsaPrimaRiga move st-riga-totali   to spl-riga-stampa
           else            move st-riga-totali2  to spl-riga-stampa
           end-if.
           perform SCRIVI.
           add spl-passo      to spl-riga.
           add tot-imponibile to tot-iva giving tot-fattura.
           move tot-cou       to st-importo st-importo2.
           perform VALUTA-IVA.
           move tot-fattura   to st-importo-totale st-importo-totale2.
           perform JUSTIFY-RIGHT.
           if UsaPrimaRiga move st-riga-totali  to spl-riga-stampa
           else            move st-riga-totali2 to spl-riga-stampa
           end-if.
           perform SCRIVI.
           add spl-passo       to spl-riga.

           perform STAMPA-STRINGA-COPIA.   

           move CourierNew11 to spl-hfont.

      ***---
       CALCOLA-IVA.
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

      ***---
       STAMPA-SCADENZE.
           initialize variabili-varsca replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move tor-cod-pagamento to sca-codice-pa.
           move tor-data-fattura  to sca-data-fattura.
           move tor-data-fattura  to sca-data-conteggio.
           move tot-fattura       to sca-importo-fattura.
           move tot-fattura       to sca-importo-fattura-va.
           move tot-iva           to sca-iva.
           move tot-iva           to sca-iva-va.
           
           move cli-mese1         to sca-mese1.
           move cli-giorno1       to sca-giorno1.
           move cli-mese2         to sca-mese2.
           move cli-giorno2       to sca-giorno2.

           move cli-escluso-dal-giorno1 to sca-escluso-dal-giorno1.
           move cli-escluso-dal-giorno2 to sca-escluso-dal-giorno2.

           call   "calsca" using variabili-varsca.
           cancel "calsca".

           move CourierNew6 to spl-hfont.
           initialize st-riga-scadenze.
           move 25,95 to riga-scad.
           |12 è il numero massimo delle scadenze STAMPABILI
           perform varying idx-sca from 1 by 1 until idx-sca > 13
              if sca-importo(idx-sca) = 0 exit perform end-if
              evaluate idx-sca
              when 1 
              when 2 
              when 3
              when 4
                   move 11 to spl-tipo-colonna
              when 5
                   move 25,95 to riga-scad
                   move 11,10 to spl-tipo-colonna
              when 6
              when 7
              when 8
                   move 11,10 to spl-tipo-colonna
              when 9
                   move 25,95 to riga-scad
                   move 11,20 to spl-tipo-colonna
              when 10
              when 11
              when 12
                   move 11,20 to spl-tipo-colonna
              when 13
                   initialize st-riga-scadenze
                   move 25,95     to riga-scad
                   move 11,30     to spl-tipo-colonna
                   move idx-sca   to scad-num
                   call "C$JUSTIFY" using scad-num, "R"
                   inspect scad-num replacing leading x"30" by x"20"
                   move ") "             to scad-hyphen
                   move "..."            to scad-data
                   move st-riga-scadenze to spl-riga-stampa
                   move riga-scad        to spl-riga
                   perform SCRIVI
                   exit perform
              end-evaluate
              move idx-sca to scad-num
              call "C$JUSTIFY" using scad-num, "R"
              inspect scad-num replacing leading x"30" by x"20"
              move sca-importo(idx-sca)       to scad-importo
              move ") "                       to scad-hyphen
              move " -"                       to scad-div
              if sca-a-vista(idx-sca) = "S"
                 move "(A VISTA);"            to scad-end
                 move tor-data-fattura(7:2)   to scad-data(1:2)
                 move "/"                     to scad-data(3:1)
                 move tor-data-fattura(5:2)   to scad-data(4:2)
                 move "/"                     to scad-data(6:1)
                 move tor-data-fattura(1:4)   to scad-data(7:4)
              else
                 move gg of sca-data(idx-sca) to scad-data(1:2)
                 move "/"                     to scad-data(3:1)
                 move mm of sca-data(idx-sca) to scad-data(4:2)
                 move "/"                     to scad-data(6:1)
                 move aa of sca-data(idx-sca) to scad-data(7:4)
                 move " ;"                    to scad-end
              end-if
              move st-riga-scadenze           to spl-riga-stampa
              move riga-scad                  to spl-riga
              perform SCRIVI
              add 0,20                        to riga-scad
           end-perform.
           
           move 0 to tot-consumo
                     tot-cou
                     tot-iva
                     tot-fattura
                     tot-solo-cou
                     tot-cobat
                     tot-piombo.
           perform INIT-TABELLA-IVA.

      ***---
       STAMPA-STRINGA-COPIA.
           if link-tipo-stampa = 1
              move 10             to spl-tipo-colonna
              move CourierNew7    to spl-hfont
              move 26,50          to spl-riga
              move st-riga-copia  to spl-riga-stampa
              perform SCRIVI
           end-if.

      ***---
       STAMPA-TOTALI.
           if visualizza-totali = "S"
              if righe-finali > 0
                 set RigheFinali to true
                 initialize ror-rec
                            replacing numeric data by zeroes
                                 alphanumeric data by spaces
                 move spaces to st-riga-tot-imposte
                 perform SCRITTURA-RIGA

                 if si-cou
                    move "* T O T A L E   C. O. U. *" 
                      to ror-des-libera
                    move tot-solo-cou to ror-imp-cou-cobat
                    perform SCRITTURA-RIGA
                 end-if
                 if si-cobat
                    move "* T O T A L E   C O B A T *" 
                      to ror-des-libera
                    move tot-cobat to ror-imp-cou-cobat
                    perform SCRITTURA-RIGA
                 end-if
                 if si-piombo
                    move "* T O T A L E   P I O M B O *" 
                      to ror-des-libera
                    move tot-piombo to ror-imp-cou-cobat
                    perform SCRITTURA-RIGA
                 end-if
                 set RigheFinali to false
              end-if
           end-if.

      ***---
       STAMPA-DICHIARAZIONE.
           if EsisteDichiarazione
              move 9               to spl-tipo-colonna
              move Arial8B         to spl-hfont
              move 22,74           to spl-riga
              inspect cli-dich-esp replacing trailing space by low-value
              inspect cli-num-reg  replacing trailing space by low-value
              string "Dich. esportatore n. " delimited by size
                     cli-dich-esp            delimited by low-value
                     " del "                 delimited by size
                     cli-data-dich(7:2)      delimited by size
                     "/"                     delimited by size
                     cli-data-dich(5:2)      delimited by size
                     "/"                     delimited by size
                     cli-data-dich(3:2)      delimited by size
                     " reg. il "             delimited by size
                     cli-data-reg(7:2)       delimited by size
                     "/"                     delimited by size
                     cli-data-reg(5:2)       delimited by size
                     "/"                     delimited by size
                     cli-data-reg(3:2)       delimited by size
                     " n. "                  delimited by size
                     cli-num-reg             delimited by low-value
                     into st-valori-dich
              end-string
      *        call "C$JUSTIFY" using st-valori-dich, "R"
              move st-riga-dich   to spl-riga-stampa
              perform SCRIVI
           end-if.

      ***---
       VALUTA-IVA.
           move space to st-aliquota-tot st-aliquota-tot2
                                         st-articolo-iva2.
           move     0 to st-imponibile   st-importo-iva idx.
           move     0 to st-imponibile2.

           if cod-iva(1) not = spaces
              if iva-sigla(1)
                 move spaces to cod-iva(1) 
              end-if
              if articolo-iva(1) not = spaces
                 if imponibile-iva(1) = 0
                    move spaces to articolo-iva(1)
                 end-if
                 set  UsaSecondaRiga    to true
                 move st-importo        to st-importo2
                 move articolo-iva(1)   to st-articolo-iva2
                 move imponibile-iva(1) to st-imponibile2
                 move cod-iva(1)        to st-aliquota-tot2
              else
                 set  UsaPrimaRiga      to true
                 move cod-iva(1)        to st-aliquota-tot
                 move importo-iva(1)    to st-importo-iva
                 move imponibile-iva(1) to st-imponibile
              end-if
              move spaces to cod-iva(1)
           else
              if cod-iva(2) not = spaces
                 if iva-sigla(2)
                    move spaces to cod-iva(2) 
                 end-if
                 if articolo-iva(2) not = spaces
                    if imponibile-iva(2) = 0
                       move spaces to articolo-iva(2)
                    end-if
                    set  UsaSecondaRiga    to true
                    move st-importo        to st-importo2
                    move articolo-iva(2)   to st-articolo-iva2
                    move imponibile-iva(2) to st-imponibile2
                    move cod-iva(2)        to st-aliquota-tot2
                 else
                    set  UsaPrimaRiga      to true
                    move cod-iva(2)        to st-aliquota-tot
                    move importo-iva(2)    to st-importo-iva
                    move imponibile-iva(2) to st-imponibile
                 end-if
                 move spaces to cod-iva(2)
              else
                 if cod-iva(3) not = spaces
                    if iva-sigla(3)
                       move spaces to cod-iva(3)
                    end-if
                    if articolo-iva(3) not = spaces
                       if imponibile-iva(3) = 0
                          move spaces to articolo-iva(3)
                       end-if
                       set UsaSecondaRiga     to true
                       move st-importo        to st-importo2
                       move articolo-iva(3)   to st-articolo-iva2
                       move imponibile-iva(3) to st-imponibile2
                       move cod-iva(3)        to st-aliquota-tot2
                    else
                       set  UsaPrimaRiga      to true
                       move cod-iva(3)        to st-aliquota-tot
                       move importo-iva(3)    to st-importo-iva
                       move imponibile-iva(3) to st-imponibile
                    end-if
                    move spaces to cod-iva(3)
                 end-if
              end-if
           end-if.

      ***---
       INIT-TABELLA-IVA.
           move 0 to idx.
           perform 3 times
              add 1 to idx
              initialize tabella-iva(idx)replacing numeric data by zeros
                                              alphanumeric data by space
           end-perform.

      ***---
       JUSTIFY-RIGHT.
           call "C$JUSTIFY" using st-importo,         "R".
           call "C$JUSTIFY" using st-imponibile,      "R".
           call "C$JUSTIFY" using st-importo-iva,     "R".
           call "C$JUSTIFY" using st-importo-totale,  "R".
           call "C$JUSTIFY" using st-importo2,        "R".
           call "C$JUSTIFY" using st-imponibile2,     "R".
           call "C$JUSTIFY" using st-importo-totale2, "R".

      ***---
       SCRIVI.
           call "spooler"  using spooler-link.
           initialize spl-riga-stampa.
