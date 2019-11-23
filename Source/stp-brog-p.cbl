       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stp-brog-p.
       AUTHOR.                          Filippo.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "provvig.sl".
           copy "articoli.sl".
           copy "clienti.sl".
           copy "agenti.sl".
           copy "tparamge.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "provvig.fd".
           copy "articoli.fd".
           copy "clienti.fd".
           copy "agenti.fd".
           copy "tparamge.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       78  titolo        value "Stampa dettaglio periodica per agente".
       78  MargineFisso  value 2,5.
       
       77  messaggio             pic x(150) value SPACES.
       77  font-size-dply        pic z(5).      
       77  Verdana10B            handle of font.
       77  Verdana10I            handle of font.
       77  Verdana8              handle of font.
       77  Verdana8B             handle of font.
       77  Verdana7              handle of font.
       77  WFONT-STATUS          pic s9(5) value ZERO.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  data6                 pic 9(6).
       77  scelta                pic 9.
       77  passo                 pic 9v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.

       77  status-provvig        pic xx.
       77  status-articoli       pic xx.
       77  status-clienti        pic xx.
       77  status-agenti         pic xx.
       77  status-tparamge       pic xx.

       77  sav-agente            pic 9(5) value 99999.
       77  sav-fat               pic 9(8) value 99999999. 
       77  pag                   pic 9(4) value 1.

       77  como-peso             pic s9(9)v999.

       77  filler                pic 9.
           88 trovata-marca      value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  sw-omaggi             pic 9.
       77  sw-primo-omaggio      pic 9.

      * RIGHE PER LA STAMPA
       01  line-riga                pic x(900).
       01  riga-titolo              pic x(131).

       01  riga-pagina.
           05 filler                pic x(5) value "Pag. ".
           05 st-pag                pic z(4).

       01  riga-trattini            pic x(131) value all "-".

       01  riga-intestazione.
           05 filler                pic x(5)  value "Fatt.".   |6
           05 filler                pic x(2)  value "Rg".      |8
           05 filler                pic x(7)  value "Cliente". |15
           05 filler                pic x(7)  value "N.list.". |22
           05 filler                pic x(7)  value "Listino". |29
           05 filler                pic x(4)  value "Sc.%".    |33
           05 filler                pic x(5)  value "Chili".   |38
           05 filler                pic x(4)  value "Cod.".    |42
           05 filler                pic x(8)  value "Prodotto".|50
           05 filler                pic x(2)  value "UM".      |52
           05 filler                pic x(4)  value "Q.tà".    |56
           05 filler                pic x(6)  value "Prezzo".  |62
           05 filler                pic x(6)  value "Provv.".  |68

       01  r-riga.
           05 r-num-fat             pic z(6).            
           05 r-riga-fat            pic 9(2).            |7
           05 r-des-cliente         pic x(25).           |9
           05 r-num-listino         pic x(4).            |34
           05 r-prezzo-netto-agente pic zzz.zz9,99.      |38
           05 r-sconto-listino      pic  z9,99.          |48
           05 r-peso-um             pic zz.zz9.          |53
           05 r-tipo-vend           pic x.               |59
           05 r-articolo            pic z(6).            |60
           05 r-des-art             pic x(45).           |66            
           05 r-um                  pic x(2).            |106
           05 r-qta-vend            pic zzz.zz9.         |108
           05 r-prezzo-unit-vend    pic zzz.zz9,99.      |115
           05 r-val-provvig         pic zzz.zz9,99.      |125

       01  r-agente.                
           05 r-age-codice          pic z(5).
           05 r-age-desc            pic x(40).

       LINKAGE SECTION.
       copy "link-stp-brog.def".

      ******************************************************************
       PROCEDURE DIVISION using stp-brog-linkage.

       DECLARATIVES.

       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           set tutto-ok  to true.
           evaluate status-provvig
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File provvigioni [PROVVIG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [PROVVIG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[PROVVIG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File agenti [AGENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [AGENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[AGENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File parametri [TPARAMGE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
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
              perform LEGGI-PARAMETRI
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.

           perform EXIT-PGM.

      ***---
       INIT.
      *-
           set environment "PRINTER" to "-P SPOOLER".
           move 0,5 to passo.
           accept como-data from century-date.
           accept como-ora  from time.

           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
          
      ***---
       CARICA-FONT.
      * Verdana 10B
           initialize wfont-data Verdana10B.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8
           initialize wfont-data Verdana8.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8B
           initialize wfont-data Verdana8B.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 7
           initialize wfont-data Verdana7.
           move 7 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana7, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10I
           initialize wfont-data Verdana10I.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10I, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
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
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       OPEN-FILES.
           if tutto-ok
              if tutto-ok
                 open input provvig
                            clienti
                            articoli
                            agenti
                            tparamge
              end-if
           end-if.
 
      ***---
       LEGGI-PARAMETRI.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.

      ***---
       ELABORAZIONE.
           |STAMPA BROGLIACCIO
           initialize pvv-rec.
           move 0 to sw-omaggi

           start provvig key is >= k-agente
              invalid    set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read provvig next no lock 
                    at end
                    if not prima-volta
                       if save-riga > save-altezza-pagina - MargineFisso
                          perform SALTO-PAGINA
                       end-if
                    end-if
                    exit perform
                 end-read

                 if pvv-data-fat = sta-data and
                    not pvv-omaggio |TIPO VENDITA
                    perform STAMPA-BROGLIACCIO
                 end-if

                 if errori
                    exit perform
                 end-if

              end-perform
           end-if.

           |STAMPA BROGLIACCIO OMAGGI
           set prima-volta to true.
           move 0     to sw-primo-omaggio
           move 99999 to sav-agente
           move 1 to sw-omaggi
           initialize pvv-rec.

           start provvig key is >= k-agente
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read provvig next no lock
                    at end
                    if not prima-volta
                       if save-riga > save-altezza-pagina - MargineFisso
                          perform SALTO-PAGINA
                       end-if
                    end-if
                    exit perform
                 end-read

                 if pvv-data-fat = sta-data and
                    pvv-tipo-vend = "O"
                    if sw-primo-omaggio = 0
                       perform SALTO-PAGINA
                       move 1 to sw-primo-omaggio
                    end-if
                    perform STAMPA-BROGLIACCIO
                 end-if

                 if errori
                    exit perform
                 end-if

              end-perform
           end-if.

           if not trovato
              if spl-sta-annu
                 display message "Procedura interrotta dall'utente!"
                           title titolo
                            icon 2
              else
                 display message "Nessuna provvigione trovata"
                           title titolo
                            icon 2
              end-if
           else
              if not spl-sta-annu 
                 set spl-chiusura to true
                 call   "spooler" using spooler-link
                 cancel "spooler"
              end-if
           end-if.

      ***---
       STAMPA-BROGLIACCIO.
           initialize spooler-link
           if prima-volta
              call   "selprint" using selprint-linkage
              cancel "selprint"

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE

                 move "GESLUX - Lista provv. gg" to spl-nome-job
                 set spl-apertura   to true
                 set spl-horizontal to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 if spl-sta-annu 
                    set errori to true
                 else
                    move spl-altezza to save-altezza-pagina
                    perform CARICA-FONT
                    perform SCRIVI-INTESTAZIONE
                 end-if
              else
                 set spl-sta-annu to true
                 set errori to true
              end-if
           end-if

           if tutto-ok
              if pvv-agente not = sav-agente
                 move pvv-agente  to sav-agente
                 move 99999999    to sav-fat
                 if not prima-volta
                    move spaces to line-riga
                    perform SCRIVI
                    move spaces to line-riga
                    perform SCRIVI
                 end-if
                 perform SCRIVI-INTE-AGENTE
              end-if
              
              if save-riga > ( save-altezza-pagina - MargineFisso )
                 perform SALTO-PAGINA
              end-if
              
              initialize line-riga
              move pvv-num-fat     to r-num-fat
              move pvv-riga-fat    to r-riga-fat
              
              initialize cli-rec
              move "C" to cli-tipo-cf
              move pvv-cliente to cli-codice
              read clienti no lock
                   invalid move "** NON TROVATO **" to cli-ragsoc-1
              end-read
              move cli-ragsoc-1    to r-des-cliente
              if pvv-num-listino = tge-listino-promo
                 move "PROM" to r-num-listino
              else
                 move pvv-num-listino to r-num-listino
                 call "C$JUSTIFY" using r-num-listino, "R"
                 inspect r-num-listino replacing leading x"30" by x"20"
              end-if
              move pvv-prezzo-netto-agente to r-prezzo-netto-agente
              move pvv-sconto-listino to r-sconto-listino
              compute como-peso = pvv-peso-um * pvv-qta-vend
LUBEXX        add 0,5                 to como-peso
LUBEXX        move como-peso          to r-peso-um
              move pvv-tipo-vend      to r-tipo-vend

              initialize art-rec
              move pvv-articolo to art-codice
              read articoli no lock
                   invalid
                   move "** NON TROVATO **" to art-descrizione
                   move 0 to art-codice
              end-read
              move art-codice            to r-articolo
              move art-descrizione       to r-des-art
              move pvv-um                to r-um
              move pvv-qta-vend          to r-qta-vend
              move pvv-prezzo-unit-vend  to r-prezzo-unit-vend
              move pvv-val-provvig       to r-val-provvig       

              if pvv-num-fat = sav-fat
                 move pvv-num-fat  to sav-fat
                 move 0            to r-num-fat
                 move spaces       to r-des-cliente
              end-if
              move pvv-num-fat     to sav-fat

              move r-riga          to line-riga
      
              move 27              to spl-tipo-colonna
              move Verdana7        to spl-hfont
              perform SCRIVI
            
              set trovato to true
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           perform STAMPA-FRAME.

           set prima-volta        to false.
           initialize riga-titolo.
           if sw-omaggi = 0
              string "BROGLIACCIO PROVVIGIONI del:  " delimited by size
                  sta-data(7:2)                    delimited by size
                  "/"                              delimited by size
                  sta-data(5:2)                    delimited by size
                  "/"                              delimited by size
                  sta-data(1:4)                    delimited by size
                  into riga-titolo
              end-string
           else
              string "BROGLIACCIO OMAGGI del:  " delimited by size
                  sta-data(7:2)                    delimited by size
                  "/"                              delimited by size
                  sta-data(5:2)                    delimited by size
                  "/"                              delimited by size
                  sta-data(1:4)                    delimited by size
                  into riga-titolo
              end-string
           end-if

           move pag               to st-pag 
           move riga-pagina       to riga-titolo(120:)

           move 0                 to save-riga.
           move 24                to spl-tipo-colonna.
           move riga-titolo       to line-riga.
           move Verdana10B        to spl-hfont
           perform SCRIVI.

           add 0,35               to save-riga.

      ***---
       SCRIVI-INTE-AGENTE.
           initialize line-riga
           move pvv-agente to age-codice
           read agenti no lock
              invalid
                 initialize age-rec
           end-read
           move age-codice   to r-age-codice
           move age-ragsoc-1 to r-age-desc
           move r-agente to line-riga
           move 25                to spl-tipo-colonna
           move Verdana8B         to spl-hfont
           perform SCRIVI
           PERFORM SCRIVI-TESTA.

      ***---
       SCRIVI-TESTA.
           move Verdana8          to spl-hfont
           perform STAMPA-LINEA.

           move 26                to spl-tipo-colonna.
           compute save-riga = save-riga - 0,3.
           move riga-intestazione to line-riga.
           perform SCRIVI.

           perform STAMPA-LINEA.

      ***---
       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           move 9,0                  to spl-colonna.
           move 19,9                 to spl-colonna-fine.
           move 0,4                  to spl-riga.
           move 1,2                  to spl-riga-fine.
           set  spl-oggetto          to true.
           set  spl-rettangolo-round to true.
           set  spl-brush-ltgray     to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           move line-riga     to spl-riga-stampa.
           call "spooler"  using spooler-link.
           initialize spl-riga-stampa.
            
      ***---
       STAMPA-LINEA.
           move 3                  to spl-pen-with.
           move 0,7                to spl-colonna.
           move 27,5               to spl-colonna-fine.
           add  passo 0,2          to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

            
      ***---
       SALTO-PAGINA.
           add 1 to pag.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.
           if sw-primo-omaggio = 0 and
              sw-omaggi = 1
              continue
           else
              perform SCRIVI-TESTA
           end-if.

      ***---
       CLOSE-FILES.
           close provvig articoli clienti agenti tparamge.
  
      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana10B.       
           destroy Verdana8.
           destroy Verdana8B.
           destroy Verdana7.
           destroy Verdana10I.

           cancel "spooler".
           initialize spooler-link.

           goback.
