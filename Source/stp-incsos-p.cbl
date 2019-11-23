       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stp-incsos-p.
       AUTHOR.                          Filippo.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "provvig.sl".
           copy "agenti.sl".
           copy "articoli.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "provvig.fd".
           copy "agenti.fd".
           copy "articoli.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       78  tit-incassato value
           "Liquidazione provvigioni: stampa incassato".
       78  tit-sospeso   value
           "Liquidazione provvigioni: stampa sospeso".
       77  titolo                pic x(50). 
       
       77  messaggio             pic x(150) value SPACES.
       77  font-size-dply        pic z(5).      
       77  Verdana12B            handle of font.
       77  Verdana12I            handle of font.
       77  Verdana10             handle of font.
       77  Verdana9              handle of font.
       77  WFONT-STATUS          pic s9(5) value ZERO.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  data6                 pic 9(6).
       77  scelta                pic 9.
       77  passo                 pic 9v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.

       77  status-provvig        pic xx.
       77  status-agenti         pic xx.
       77  status-articoli       pic xx.
       77  status-clienti        pic xx.

       77  tot-val-provvig       pic s9(12)v99 value 0.

       77  sav-agente            pic 9(5) value 99999.
       77  sav-fat               pic 9(8) value 99999999. 

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

      * RIGHE PER LA STAMPA
       01  line-riga             pic x(900).

       01  riga-trattini         pic x(131) value all "-".

       01  riga-intestazione.
           05 filler             pic x(7)  value "Agente ".
           05 i0-age             pic z(5).
           05 filler             pic x(3)  value ":  ".
           05 i0-age-desc        pic x(40).

       01  com-riga              pic x(55) value spaces.

       01  riga-intestazione1.
           05 i1-titolo          pic x(50).
           05 filler             pic x(10)  value "alla data ".
           05 i1-data            pic 99/99/99.
           05 filler             pic x(20) value " *****         Pag. ".
           05 i1-pag             pic z(4).

       01  riga-intestazione2.
           05 filler             pic x(4)  value "Cod.".      
           05 filler             pic x(15) value "Ragione sociale". | 5
           05 filler             pic x(6)  value "N.fat.".          |20
           05 filler             pic x(4)  value "Data".            |26
           05 filler             pic x(2)  value "rg".              |30
           05 filler             pic x(8)  value "Prodotto".        |32
           05 filler             pic x(2)  value "UM".              |40
           05 filler             pic x(4)  value "Q.ta".            |42
           05 filler             pic x(6)  value "Prezzo".          |46
           05 filler             pic x(7)  value "Importo".         |52

       01  r-riga.
           05 r-cliente             pic z(5).
           05 r-des-cliente         pic x(30).                      | 6
           05 r-num-fat             pic z(6).                       |36
           05 r-data-fat            pic 99/99/99 blank zero.        |42 
           05 r-riga-fat            pic 9(2).                       |50
           05 r-des-art             pic x(30).                      |52
           05 r-um                  pic x(2).                       |82
           05 r-qta-vend            pic ---.--9.                    |84
           05 r-prezzo-unit-vend    pic ---.--9,99.                 |91
           05 r-val-provvig         pic ---.--9,99.                 |101
       01  r-totali.                
           05 filler                pic x(16) value "Totale  ------->".
           05 r-tot-provvig         pic -.---.--9,99.

       77  st-agente                pic z(5).
       77  st-age-ragsoc            pic x(30).
       77  CountChar                pic 999.
       77  pag                      pic 9(4) value 1.

       LINKAGE SECTION.
       copy "link-stp-incsos.def".

      ******************************************************************
       PROCEDURE DIVISION using stp-incsos-linkage.

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

       END DECLARATIVES.

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
      * Verdana 12B
           initialize wfont-data Verdana12B.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10
           initialize wfont-data Verdana10.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 9
           initialize wfont-data Verdana9.
           move 9 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana9, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 12I
           initialize wfont-data Verdana12I.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12I, wfont-data
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

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
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

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       OPEN-FILES.
           if tutto-ok
              if tutto-ok
                 open input provvig
                            agenti
                            articoli
                            clienti
              end-if
           end-if.



      ***---
       ELABORAZIONE.
           initialize pvv-rec.
           move sta-da-age    to pvv-agente

           start provvig key is >= k-agente
              invalid
                 set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read provvig next no lock 
                    at end
                       if trovato
                          if save-riga > save-altezza-pagina - 1,5
                             perform SALTO-PAGINA
                          end-if
                          perform TOTALI-GENERALI
                       end-if
                       exit perform
                 end-read

                 evaluate true
                    when sta-incassato
                       if pvv-agente   >= sta-da-age   and
                          pvv-agente   <= sta-a-age    and
LUBEXX                    pvv-data-liq  = sta-data-liq
LUBEXX                    if pvv-normale or pvv-promo
LUBEXX                       perform STAMPA-LIQ
LUBEXX                    end-if
                       end-if
                    when sta-sospeso
                       if pvv-agente   >= sta-da-age   and
                          pvv-agente   <= sta-a-age    and
                          pvv-data-fat <= sta-data-liq and
                          pvv-data-liq = 0
LUBEXX                    if pvv-normale or pvv-promo
LUBEXX                       perform STAMPA-LIQ
LUBEXX                    end-if
                       end-if
                 end-evaluate
                 if errori 
                    exit perform 
                 end-if

              end-perform
           end-if.

           if not trovato
              evaluate true
                 when sta-incassato
                    move tit-incassato to titolo
                 when sta-sospeso
                    move tit-sospeso   to titolo
              end-evaluate
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
       STAMPA-LIQ.
           initialize spooler-link
           if prima-volta
              call "selprint" using selprint-linkage
              cancel "selprint"

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE

                 move "GESLUX - Stampa inc./sosp." to spl-nome-job
                 set spl-apertura    to true
                 set spl-horizontal  to true
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
                 if sav-agente not = 99999
                    perform TOTALI-GENERALI
                    move 0 to tot-val-provvig
                    perform SALTO-PAGINA
                 end-if
                 move pvv-agente to sav-agente
                 move 99999999   to sav-fat
              end-if
           end-if
           
           if tutto-ok
              if save-riga > ( save-altezza-pagina - passo )
                 perform SALTO-PAGINA
              end-if
              
              initialize line-riga

              initialize cli-rec
              move "C" to cli-tipo-cf
              move pvv-cliente to cli-codice
              read clienti no lock
                   invalid move "** NON TROVATO **" to cli-ragsoc-1
              end-read
              move cli-codice   to r-cliente
              move cli-ragsoc-1 to r-des-cliente

              move pvv-num-fat     to r-num-fat

              move pvv-data-fat(3:2) to data6(5:2)
              move pvv-data-fat(5:2) to data6(3:2)
              move pvv-data-fat(7:2) to data6(1:2)
              move data6           to r-data-fat

              move pvv-riga-fat    to r-riga-fat
              
              initialize art-rec
              move pvv-articolo to art-codice
              read articoli no lock
                 invalid
                    move "** NON TROVATO **" to art-descrizione
              end-read
              
              move art-descrizione       to r-des-art
              move pvv-um                to r-um
              move pvv-qta-vend          to r-qta-vend
              move pvv-prezzo-unit-vend  to r-prezzo-unit-vend
              move pvv-val-provvig       to r-val-provvig

              if pvv-num-fat = sav-fat
                 move pvv-num-fat  to sav-fat
                 move 0            to r-cliente
                 move spaces       to r-des-cliente
                 move 0            to r-num-fat
                 move 0            to r-data-fat
              end-if
              move pvv-num-fat     to sav-fat
      
              move r-riga          to line-riga
      
              move 35              to spl-tipo-colonna
              move Verdana9        to spl-hfont
              perform SCRIVI
            
              add pvv-val-provvig  to tot-val-provvig
              set trovato to true
           end-if.
      

      ***---
       SCRIVI-INTESTAZIONE.
           perform STAMPA-FRAME.

           set prima-volta        to false.

           move pvv-agente to age-codice
           read agenti no lock
              invalid
                 initialize age-rec
           end-read
           move age-codice   to i0-age
           move age-ragsoc-1 to i0-age-desc

           move 0                  to save-riga.
           move 32                 to spl-tipo-colonna.

           initialize com-riga
           move riga-intestazione  to com-riga
           call "C$JUSTIFY" using  com-riga, "C".
           move com-riga           to line-riga.
           move Verdana12B         to spl-hfont
           perform SCRIVI.
           add 0,2                 to save-riga.

           evaluate true
            when sta-incassato
             move "***** PROVVIGIONI MATURATE(fatture incassate SBF) "
                  to i1-titolo
            when sta-sospeso
             move "***** PROVVIGIONI SOSPESE (fatture da incassare)  "
                  to i1-titolo
           end-evaluate

           move sta-data-liq(7:2)  to data6(1:2)
           move sta-data-liq(5:2)  to data6(3:2)
           move sta-data-liq(3:2)  to data6(5:2)
           move data6              to i1-data
           move pag                to i1-pag.

           move 33                 to spl-tipo-colonna.
           move riga-intestazione1 to line-riga.
           move Verdana12B         to spl-hfont
           perform SCRIVI.
           add 0,35                to save-riga.
                              
           perform STAMPA-LINEA.

           subtract 0,25         from save-riga.
           move 34                 to spl-tipo-colonna.
           move riga-intestazione2 to line-riga.
           perform SCRIVI.

           perform STAMPA-LINEA.

      ***---
       TOTALI-GENERALI.
           move spaces to line-riga.
           perform SCRIVI.
           
           add 0,25 to save-riga.
           perform STAMPA-LINEA.
           subtract 0,25 from save-riga.

           move tot-val-provvig  to r-tot-provvig.

           move Verdana12I to spl-hfont.
           move r-totali   to line-riga.
           move 36         to spl-tipo-colonna.
           perform SCRIVI.
           
           perform STAMPA-LINEA.
           


      ***---
       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           move 2                    to spl-colonna.
           move 27,7                 to spl-colonna-fine.
           move 0,4                  to spl-riga.
           move 1,8                  to spl-riga-fine.
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
           move 28,8               to spl-colonna-fine.
           add  passo 0,3          to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

            
      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           add 1 to pag.
           perform SCRIVI-INTESTAZIONE.



      ***---
       CLOSE-FILES.
           close provvig agenti articoli clienti.
  
      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana12B.
           destroy Verdana10.
           destroy Verdana9.
           destroy Verdana12I.

           cancel "spooler".
           initialize spooler-link.

           goback.
