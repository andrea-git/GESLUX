       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-st-listini.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tgrupgdo.sl".
           copy "tmp-listini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tgrupgdo.fd".
           copy "tmp-listini.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tgrupgdo       pic xx.
       77  status-tmp-listini    pic xx.
       77  path-tmp-listini      pic x(256).

      * COSTANTI
       78  titolo                value "GESLUX - Stampa Listini".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 45.
      * RIGHE PER LA STAMPA
       01  r-data                pic x(10).
       01  r-periodo             pic x(25).

       01  r-titolo              pic x(40).

       01  r-intesta.
           05 filler             pic x(2)  value "N.".
           05 filler             pic x(4)  value "Art.".
           05 filler             pic x(8)  value "Val. dal".
           05 filler             pic x(7)  value "Cod Cli".
           05 filler             pic x(11) value "Descrizione".
           05 filler             pic x(3)  value "Imb".
           05 filler             pic x(3)  value "EAN".
           05 filler             pic x(5)  value "Prod.".
           05 filler             pic x(5)  value "Cons.".
           05 filler             pic x(3)  value "Cou".
           05 filler             pic x(6)  value "Add.Pb".
           05 filler             pic x(6)  value "Prezzo".
           05 filler             pic x(19) value "Prezzo Promozionale".
           05 filler             pic x(4)  value "Faro".

       01  r-riga.
           05 r-num              pic zz9.
           05 r-articolo         pic z(6).
           05 r-val-dal          pic x(8).
           05 r-cod-cli          pic x(15).
           05 r-descr            pic x(47).
           05 r-imb              pic x(3).
           05 r-ean              pic x(13).
           05 r-prod             pic zz.zz9,99.
           05 r-cons             pic z.zz9,99. 
           05 r-cou              pic z.zz9,99.
           05 r-add              pic z.zz9,99.

       01  r-promo-faro.
           05 r-promo            pic x(50).
           05 r-faro             pic zz.zz9,99 blank zero.

       01  r-prezzo              pic x(9).

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.

      * VARIABILI
       77  prezzo-z              pic zz.zz9,99.
       77  logo-handle           handle of bitmap.
       77  perce                 pic 9(4)v99.
       77  idx                   pic 9(5).

       77  num-righe             pic 9(5).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana14B            handle of font.
       77  Verdana12B            handle of font.
       77  Verdana8BI            handle of font.
       77  Verdana8B             handle of font.
       77  Verdana6              handle of font.
       77  Verdana6B             handle of font.
       77  Verdana6I             handle of font.
       77  Verdana6BI            handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
           copy "link-st-listini.def".

      ******************************************************************
       PROCEDURE DIVISION using st-listini-linkage.

       DECLARATIVES.

      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "39"
                set errori to true
                display message "File [TGRUPGDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TGRUPGDO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TGRUPGDO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TMP-LISTINI-ERR SECTION.
           use after error procedure on tmp-listini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-listini
           when "39"
                set errori to true
                display message "File [TMP-LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-LISTINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tmp-listini
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99" set RecLocked to true
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
           accept como-data from century-date.
           move 0    to counter counter2.
           move 0    to num-righe idx.
           move 0,5  to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok    to true.
           set RecLocked   to false.
           set trovato     to false.
           move stlst-path to path-tmp-listini.

      ***---
       OPEN-FILES.
           open input tmp-listini tgrupgdo.

      ***---
       ELABORAZIONE.
           initialize spooler-link.
           call   "selprint" using selprint-linkage.
           cancel "selprint".
      
           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      
              move titolo to spl-nome-job
              set spl-apertura to true
              set spl-vertical to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
              if spl-sta-annu
                 set errori to true
              else                                  
                 move spl-altezza to save-altezza-pagina
                 perform CARICA-FONT
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.
      
           if tutto-ok
              move stlst-righe to num-righe
              move 1 to pagina
              evaluate true
              when num-righe < 78-MaxRows
                   move 1 to tot-pag
              when num-righe = 78-MaxRows
                   move 2 to tot-pag
              when other
                   divide num-righe by 78-MaxRows 
                   giving tot-pag   remainder resto
                   if resto > 0 add 1 to tot-pag end-if
              end-evaluate

              move low-value to tlst-rec
              start tmp-listini key >= tlst-chiave
                    invalid continue
                not invalid
                    move stlst-gdo to gdo-codice
                    read tgrupgdo  no lock invalid continue end-read

                    if gdo-logo not = spaces
                       call "W$BITMAP" using 3, gdo-logo |wbitmap-load
                                      giving logo-handle
                    end-if

                    perform SCRIVI-INTESTAZIONE
                    
                    perform until 1 = 2
                       read tmp-listini next
                            at end exit perform 
                       end-read

                       evaluate tlst-prezzo
                       when 0                                 
                            move "SP"        to r-prezzo
                            if stlst-escludi-sp = 1
                               exit perform cycle
                            end-if                         
                       when 999999,99                         
                            move "FA"        to r-prezzo
                            if stlst-escludi = 1
                               exit perform cycle
                            end-if  
                       when other
                            move tlst-prezzo       to prezzo-z
                            move prezzo-z          to r-prezzo
                       end-evaluate

      *****                 if tlst-prezzo not = 999999,99 |"FA" NON VA IN STAMPA
                          add 1 to idx
                          move idx                  to r-num
                          move tlst-data-vigore(3:2)to r-val-dal(7:2)
                          move "/"                  to r-val-dal(6:1)
                          move tlst-data-vigore(5:2)to r-val-dal(4:2)
                          move "/"                  to r-val-dal(3:1)
                          move tlst-data-vigore(7:2)to r-val-dal(1:2)
                          move tlst-art-descrizione to r-descr
                          move tlst-articolo        to r-articolo
                          move tlst-cod-art-cli     to r-cod-cli
                          move tlst-imb             to r-imb     
                          if tlst-ean not = 0
                             move tlst-ean          to r-ean     
                          else
                             move spaces            to r-ean
                          end-if
                          move tlst-prod            to r-prod    
                          move tlst-cons            to r-cons    
                          move tlst-cou             to r-cou
                          move tlst-add             to r-add
                          move tlst-promo           to r-promo   
                          move tlst-faro            to r-faro

                          perform STAMPA-DIVISORIO-RIGA

                          move r-riga               to spl-riga-stampa
                          set  spl-nero             to true
                          move Verdana6             to spl-hfont
                          move 81                   to spl-tipo-colonna
                          perform SCRIVI

                          subtract passo from save-riga
                          move r-prezzo  to spl-riga-stampa
                          move Verdana6B to spl-hfont
                          move 81,5      to spl-tipo-colonna
                          perform SCRIVI

                          subtract passo from save-riga
                          set  spl-rosso    to true
                          move r-promo-faro to spl-riga-stampa
                          move Verdana6I    to spl-hfont
                          move 82           to spl-tipo-colonna
                          perform SCRIVI

                          subtract 0,1 from save-riga

                          add 1 to num-righe

                          if num-righe >= 78-MaxRows
                             perform SALTO-PAGINA
                          end-if
      *****                 end-if
      
                 end-perform
      
                 if num-righe >= 78-MaxRows - 2
                    perform SALTO-PAGINA
                 end-if
      
                 perform ULTIMO-AGGIORNAMENTO
                 perform PIE-DI-PAGINA
      
                 set spl-chiusura to true
                 call   "spooler" using spooler-link
      
           end-start.

      ***---
       SCRIVI-INTESTAZIONE.
           |Stampo il logo
           if gdo-logo not = spaces
              move logo-handle to spl-hbitmap
              set  spl-bitmap  to true
              move 1,7 to spl-riga
              move 2,4 to spl-colonna
              move 3,0 to spl-bitmap-height
              move 3,0 to spl-bitmap-width

              call "spooler" using spooler-link
           end-if.

           move 0        to num-righe.
      
           move 0,8              to save-riga.
           move gdo-intestazione to spl-riga-stampa.
           move Verdana20BI      to spl-hfont.
           move 58               to spl-tipo-colonna.
           set spl-rosso         to true.
           perform SCRIVI.

      *****     move 1,4 to save-riga.
      *****     move "Listino in vigore dal" to spl-riga-stampa.
      *****     move Verdana14B              to spl-hfont.
      *****     move 58                      to spl-tipo-colonna.
      *****     set spl-nero                 to true.
      *****     perform SCRIVI.
      *****
      *****     string gdo-data-vigore(7:2) delimited size
      *****            "/"                  delimited size
      *****            gdo-data-vigore(5:2) delimited size
      *****            "/"                  delimited size
      *****            gdo-data-vigore(1:4) delimited size
      *****            into r-data
      *****     end-string.
      *****                           
      *****     move 2,0 to save-riga.
      *****     move r-data                  to spl-riga-stampa.
      *****     move Verdana14B              to spl-hfont.
      *****     move 58                      to spl-tipo-colonna.
      *****     set spl-rosso                to true.
      *****     perform SCRIVI.            

           move 2,1 to save-riga.
           move "Data Richiesta"        to spl-riga-stampa.
           move Verdana14B              to spl-hfont.
           move 0                       to spl-tipo-colonna.
           move 16,6                    to spl-colonna.
           set spl-nero                 to true.
           perform SCRIVI.

           if stlst-data-ric = 0
              string stlst-data-ric-dal(7:2) delimited size
                     "/"                     delimited size
                     stlst-data-ric-dal(5:2) delimited size
                     "/"                     delimited size
                     stlst-data-ric-dal(1:4) delimited size
                     " - "                   delimited size
                     stlst-data-ric-al(7:2)  delimited size
                     "/"                     delimited size
                     stlst-data-ric-al(5:2)  delimited size
                     "/"                     delimited size
                     stlst-data-ric-al(1:4)  delimited size
                     into r-periodo
              end-string                                   
              move Verdana12B              to spl-hfont  
              move 14,0                    to spl-colonna    
              move r-periodo               to spl-riga-stampa
           else
              string stlst-data-ric(7:2) delimited size
                     "/"                 delimited size
                     stlst-data-ric(5:2) delimited size
                     "/"                 delimited size
                     stlst-data-ric(1:4) delimited size
                     into r-data
              end-string
              move Verdana14B              to spl-hfont
              move r-data                  to spl-riga-stampa
              move 17,0                    to spl-colonna
           end-if.
                                                       
           move 2,8                     to save-riga.
           move 0                       to spl-tipo-colonna.
           set spl-rosso                to true.
           perform SCRIVI.
      
           perform STAMPA-FRAME-DATA.
           perform STAMPA-FRAME-TESTA.

           move 0   to spl-tipo-colonna.
           move 0,2 to spl-colonna.

           set spl-nero                 to true.

           move 3,2 to save-riga.
           move "Riferimento" to spl-riga-stampa.
           move Verdana8BI    to spl-hfont.
           perform SCRIVI.
                                
           move Verdana8B     to spl-hfont.
                 
           add 0,2 to save-riga.
           move "Linea Bosch" to spl-riga-stampa.
           perform SCRIVI.
                                 
           move "Linea MA-FRA" to spl-riga-stampa.
           perform SCRIVI.
                                 
           move "Linea Ursus"  to spl-riga-stampa.
           perform SCRIVI.
                                 
           move "Altro"        to spl-riga-stampa.
           perform SCRIVI.            

           move 1,9 to spl-colonna.

           set spl-rosso      to true.

           move 3,2 to save-riga.

           move 79 to spl-tipo-colonna.
           move gdo-attenzione to spl-riga-stampa.
           move Verdana8BI     to spl-hfont.
           perform SCRIVI.
           
           set  spl-blu        to true.
           move Verdana6BI     to spl-hfont.
                 
           add 0,25 to save-riga.
           move gdo-listino-bosch  to spl-riga-stampa.
           perform SCRIVI.
                                 
           move gdo-listino-ma-fra to spl-riga-stampa.
           perform SCRIVI.
                                 
           move gdo-linea-ursus    to spl-riga-stampa.
           perform SCRIVI.
                                 
           move gdo-altro          to spl-riga-stampa.
           perform SCRIVI.

           perform STAMPA-FRAME-INTESTAZIONE.
           move 6,8 to save-riga.

      ***---
       STAMPA-FRAME-DATA.
           move 12    to spl-pen-with.

           move 1,2        to save-riga.
           move save-riga  to spl-riga.

           move 2,3        to spl-riga-fine.

           move 0,1   to spl-colonna.
           move 20,6  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-blu           to true.
           call "spooler"         using spooler-link.

      ***---
       STAMPA-FRAME-TESTA.
           move 12         to spl-pen-with.

           move 4,26       to save-riga.
           move save-riga  to spl-riga.

           move 6,49       to spl-riga-fine.
                                                                 
           move 0,1   to spl-colonna.
           move 20,6  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-blu           to true.
           call "spooler"         using spooler-link.

           move 12         to spl-pen-with.

           move 4,32       to save-riga.
           move save-riga  to spl-riga.

           move 6,44       to spl-riga-fine.

           move 0,14       to spl-colonna.
           move 21,15      to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

      ***---
       STAMPA-FRAME-INTESTAZIONE.
           move 13         to spl-pen-with.

           move 6,7        to save-riga.
           move save-riga  to spl-riga.

           move 7,2        to spl-riga-fine.

           move 0,1   to spl-colonna.
           move 20,6  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-grigio        to true.
           call "spooler"         using spooler-link.

           perform STAMPA-LINEE-VERTICALI.

           move 80         to spl-tipo-colonna.
           move 6,3        to save-riga.
           set  spl-blu    to true.
           move r-intesta  to spl-riga-stampa.
           move Verdana6BI to spl-hfont.
           perform SCRIVI.

      ***---
       STAMPA-DIVISORIO-RIGA.
           add 0,4 to save-riga.
           set spl-grigio to true.
           perform STAMPA-LINEA.
           
           move save-riga to spl-riga
           add  0,42      to spl-riga giving spl-riga-fine.
           perform STAMPA-LINEE-VERTICALI.
           move 12         to spl-pen-with.

           move 15,98 to spl-colonna.
           move 19,51 to spl-colonna-fine.

           add   0,10 to save-riga giving spl-riga.
           add   0,25 to spl-riga  giving spl-riga-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-azzurro       to true.
           call "spooler"         using spooler-link.

           move 19,70 to spl-colonna.
           move 20,49 to spl-colonna-fine.

           add   0,10 to save-riga giving spl-riga.
           add   0,25 to spl-riga  giving spl-riga-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.
           subtract 0,4 from save-riga.

      ***---
       STAMPA-LINEA.
           move 7                  to spl-pen-with.
           move 0,1                to spl-colonna.
           move 20,58              to spl-colonna-fine.
           add 0,44 to save-riga   giving spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-LINEE-VERTICALI.
           move 6                  to spl-pen-with.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.

           |INIZIO
           move 0,1                to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |N.
           move 0,6                to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Art.
           move 1,50               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Val. dal
           move 2,6                to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Cod Cli
           move 4,25                to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Descrizione
           move 8,4                to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Imb
           move 8,95               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Ean
           move 10,8               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Prod
           move 11,9               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Cons
           move 12,7               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |COU
           move 13,5               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link. 
           |Add.Pb
           move 14,7               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Prezzo
           move 15,9               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |Prezzo Promozionale
           move 19,6               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.
           |FINE
           move 20,6               to spl-colonna spl-colonna-fine.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           perform ULTIMO-AGGIORNAMENTO.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.

      ***---
       ULTIMO-AGGIORNAMENTO.
           move 13                 to spl-pen-with.
           move 0,1                to spl-colonna.
           move 20,58              to spl-colonna-fine.
           move 25,29              to save-riga spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-grigio         to true.
           call "spooler"       using spooler-link.

           move 13                 to spl-pen-with.
           move 4,8                to spl-colonna.
           move 15,4               to spl-colonna-fine.
           move 25,53              to save-riga spl-riga.
           move 26,07              to spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-rettangolo     to true.
           set  spl-brush-solid    to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

           move 24,94       to save-riga.
           set  spl-nero    to true.
           move 58          to spl-tipo-colonna.
           move Verdana14B  to spl-hfont.

           set spl-giallo   to true.
           
           string tlst-data-modifica(7:2) delimited size
                  "/"                     delimited size
                  tlst-data-modifica(5:2) delimited size
                  "/"                     delimited size
                  tlst-data-modifica(1:4) delimited size
                  into r-data
           end-string.

           initialize spl-riga-stampa.
           string "Ultimo aggiornamento: " delimited size
                  r-data                   delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.

      ***---
       CARICA-FONT.
      * Verdana 20BI
           initialize wfont-data Verdana20BI.
           move 20 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana20BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 14B
           initialize wfont-data Verdana14B.
           move 14 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana14B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

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
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
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
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8BI
           initialize wfont-data Verdana8BI.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 6
           initialize wfont-data Verdana6.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 6B
           initialize wfont-data Verdana6B.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 6I
           initialize wfont-data Verdana6I.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.

           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6I, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 6BI
           initialize wfont-data Verdana6BI.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tmp-listini tgrupgdo.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana14B.
           destroy Verdana12B.
           destroy Verdana8B.
           destroy Verdana8BI.
           destroy Verdana6.
           destroy Verdana6B.
           destroy Verdana6I.
           destroy Verdana6BI.
           destroy font-pie-pagina.
           destroy logo-handle.

           cancel "spooler".
           initialize spooler-link.
           display "                                                "
              upon stlst-handle at column 05,00 line 07,00.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
