       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-st-promo-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "tgrupgdo.sl".
           copy "articoli.sl". 
           copy "blister.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "tgrupgdo.fd".
           copy "articoli.fd". 
           copy "blister.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tpromo           pic xx.
       77  status-rpromo           pic xx.
       77  status-tgrupgdo         pic xx.
       77  status-articoli         pic xx.
       77  status-blister          pic xx.

      * COSTANTI
       78  titolo                value "GESLUX - Stampa Promozioni".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 48.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(70).

       01  r-data                pic x(10).

       01  r-data-ini            pic x(10).

       01  r-data-fine           pic x(10).

       01  r-intesta.
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(50) value "Descrizione".
           05 filler             pic x(13) value "Prz. Vendita".
           05 filler             pic x(13) value "Prz. Acquisto".
           05 filler             pic x(7)  value "Q.tà".

       01  r-riga.
           05 r-art              pic z(8).
           05 r-art-descrizione  pic x(50).
           05 r-ven              pic x(13).
           05 r-acq              pic x(13).
           05 r-qta              pic zzz.zz9.

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
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       77  filler                pic 9.
           88 ScriviIntestazione value 1, false 0.

      * VARIABILI
       77  num-righe             pic 9(5).
       77  bordo                 pic 99v99 value 0.

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana16BI           handle of font.
       77  Verdana14BI           handle of font.
       77  Verdana10BI           handle of font.
       77  Verdana8BI            handle of font.
       77  Verdana8B             handle of font.
       77  Verdana6B             handle of font.
       77  passo                 pic 99v99.
       77  store-riga            pic 9(7)v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  valore-z              pic zz.zz9,99.
       77  valore-x              pic x(9).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-st-promo.def".

      ******************************************************************
       PROCEDURE DIVISION USING st-promo-linkage.

       DECLARATIVES.
      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "39"
                set errori to true
                display message "File [TPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "39"
                set errori to true
                display message "File [RPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-blister
           when "39"
                set errori to true
                display message "File [BLISTER] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[BLISTER] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [BLISTER] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

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
           move 0        to counter counter2.
           accept como-data from century-date.
           move 0   to num-righe.
           move 0,5 to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok    to true.
           set RecLocked   to false.
           set trovato     to false.
           set prima-volta to true.

      ***---
       OPEN-FILES.
           open input tpromo rpromo articoli tgrupgdo blister.

      ***---
       ELABORAZIONE.
           perform CONTA-RIGHE.
           move low-value to tpr-rec.
           if link-volantino = 0
              if link-data-from not = 0
                 move link-data-from to tpr-ini-dpo
              end-if
              if link-gdo not = spaces
                 move link-gdo   to tpr-gdo
              end-if
              start tpromo key >= tpr-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo next at end exit perform end-read
                       if link-gdo not = spaces
                          if tpr-gdo not = link-gdo
                             exit perform
                          end-if
                       end-if
                       if tpr-ini-dpo > link-data-to
                          exit perform
                       end-if
                       if tpr-ini-dpo  >= link-data-from and
                          tpr-fine-dpo <= link-data-to
                          perform LOOP-RIGHE
                          if spl-sta-annu
                             exit perform
                          end-if
                       end-if
                    end-perform
              end-start
           else
              move link-volantino to tpr-codice
              read tpromo no lock
                   invalid continue
               not invalid perform LOOP-RIGHE
              end-read
           end-if.  

           if not trovato
              if spl-sta-annu
                 display message "Stampa annullata"
                           title titolo
                            icon 2
              else
                 display message "Nessun volantino trovato"
                           title titolo
                            icon 2
              end-if
           else
              perform PIE-DI-PAGINA

              set spl-chiusura to true
              call   "spooler" using spooler-link
           end-if.

      ***---
       LOOP-RIGHE.
           set ScriviIntestazione to true.
           move low-value  to rpr-rec.
           move tpr-codice to rpr-codice.
           start rpromo key >= k-stampa
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next 
                         at end
                         add 1   to num-righe
                         add 0,1 to save-riga
                         exit perform
                    end-read

                    if rpr-codice not = tpr-codice
                       add 1   to num-righe
                       add 0,1 to save-riga
                       exit perform
                    end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 20
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 22 line 10
                       move 0 to counter2
                    end-if
                               
                    if prima-volta
                       set prima-volta to false
                       initialize spooler-link
                       if link-nome-stampante = spaces
                          call   "selprint" using selprint-linkage
                          cancel "selprint"
                       else
                          move link-nome-stampante to selprint-stampante
                       end-if

                       if selprint-stampante not = space
                          move selprint-num-copie to spl-num-copie
                          move selprint-stampante to spl-nome-stampante

                          move titolo to spl-nome-job
                          set spl-apertura to true
                          set spl-vertical to true
                          set wfdevice-win-printer    to true
                          call "spooler" using spooler-link
                          if spl-sta-annu
                             set errori to true
                          else                                    
                             move spl-altezza to save-altezza-pagina
                             perform CARICA-FONT
                          end-if
                          move 0,7  to save-riga
                          if link-volantino = 0
                             string link-data-from(7:2) delimited size
                                    "/"                 delimited size
                                    link-data-from(5:2) delimited size
                                    "/"                 delimited size
                                    link-data-from(1:4) delimited size
                                    into r-data-ini
                             end-string
                             string link-data-to(7:2) delimited size
                                    "/"               delimited size
                                    link-data-to(5:2) delimited size
                                    "/"               delimited size
                                    link-data-to(1:4) delimited size
                                    into r-data-fine
                             end-string

                             initialize spl-riga-stampa
                             string "Volantini dal " delimited size
                                    r-data-ini       delimited size
                                    " al "           delimited size
                                    r-data-fine      delimited size
                                    into spl-riga-stampa
                             end-string
                             add  3   to num-righe
                             move 1,5 to bordo
                             move Verdana16BI   to spl-hfont
                             move 58            to spl-tipo-colonna
                             set spl-blu        to true
                             perform SCRIVI
                             move 0,2   to save-riga
                          else
                                             
                             move Verdana10BI   to spl-hfont
                             move 14,25 to spl-colonna
                             move "ID VOLANTINO " 
                               to spl-riga-stampa
                             perform SCRIVI

                             subtract passo from save-riga
                             move 17,50 to spl-colonna
                             move tpr-codice
                               to spl-riga-stampa
                             inspect spl-riga-stampa replacing
                                     leading x"30" by x"20"
                             call "C$JUSTIFY" using spl-riga-stampa "L"
                             perform SCRIVI
                             add 0,2 to save-riga

                             if link-si-firma
                                move Verdana16BI   to spl-hfont
                                move 0             to spl-tipo-colonna
                                set spl-blu        to true

                                move 1,0 to spl-colonna
                                move "BIGINO PROMO" to spl-riga-stampa
                                perform SCRIVI

                                subtract passo from save-riga
                                move 10,25 to spl-colonna
                                move "Inserito da _____________" 
                                  to spl-riga-stampa
                                perform SCRIVI

                                if tpr-locale
                                   set  spl-rosso     to true
                                   add  0,15          to save-riga
                                   move Verdana14BI   to spl-hfont
                                   move 6,65 to spl-colonna
                                   move "* VOLANTINO LOCALE *" 
                                     to spl-riga-stampa
                                   perform SCRIVI
                                   subtract passo 1,15 from save-riga
                                else
                                   subtract 1,4 from save-riga
                                end-if

                                add  3   to num-righe
                                move 1,5 to bordo
                                   
                             else
                                subtract 0,2 from save-riga
                             end-if
                          end-if
                       else
                          set spl-sta-annu to true
                          set errori to true
                          exit perform
                       end-if
                    end-if
                    set trovato to true
                    if ScriviIntestazione
                       set ScriviIntestazione to false
                       if num-righe > 78-MaxRows - 4
                          perform SALTO-PAGINA
                       end-if
                       perform SCRIVI-INTESTAZIONE
                       if num-righe >= 78-MaxRows
                          perform SALTO-PAGINA
                       end-if
                    end-if
                    initialize art-rec bli-rec
                    move rpr-articolo to art-codice
                    read articoli no lock 
                         invalid
                         move art-codice to bli-codice
                         read blister no lock 
                              invalid continue 
                          not invalid
                              move bli-descrizione to art-descrizione
                         end-read
                    end-read
                    move art-codice          to r-art
                    move art-descrizione     to r-art-descrizione
                    perform FORMAT-VEN
                    perform FORMAT-ACQ
                    move rpr-qta   to r-qta
                    set spl-nero   to true
                    move r-riga    to spl-riga-stampa
                    move 83        to spl-tipo-colonna
                    move Verdana8B to spl-hfont
                    perform SCRIVI
                    perform STAMPA-LINEA
                    add 1 to num-righe
                    if num-righe >= 78-MaxRows
                       if pagina not = tot-pag
                          perform SALTO-PAGINA
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI-INTESTAZIONE.
           add  0,45   to save-riga.
           move 0     to spl-tipo-colonna.
           add  bordo to save-riga.
           move 0     to bordo.
           perform STAMPA-FRAME.
           add 0,15 to save-riga.
           set spl-rosso to true.
           move Verdana8BI to spl-hfont.
           move tpr-gdo to gdo-codice.
           read tgrupgdo no lock invalid continue end-read.
           move 1,2 to spl-colonna.
           move "Cliente" to spl-riga-stampa.
           perform SCRIVI.

           subtract passo from save-riga.
           move 2,65 to spl-colonna.
           move gdo-intestazione to spl-riga-stampa.
           perform SCRIVI.

           subtract passo from save-riga.
           move 9,8 to spl-colonna.
           move "Volantino" to spl-riga-stampa.
           perform SCRIVI.

           subtract passo from save-riga.
           move 11,5 to spl-colonna.
           move tpr-descrizione to spl-riga-stampa.
           perform SCRIVI.

           if tpr-locale
              set spl-blu    to true
              move save-riga to store-riga
              subtract 0,25 from save-riga
              move Verdana6B to spl-hfont
              move 9,8       to spl-colonna
              move "  *LOCALE*"  to spl-riga-stampa
              perform SCRIVI
              move Verdana8BI to spl-hfont
              move store-riga to save-riga
              set spl-rosso to true
           end-if.

      *****     add 0,1 to save-riga.
           
           move "Vol. dal" to spl-riga-stampa.
           move 1,2 to spl-colonna.
           perform SCRIVI.

           string tpr-ini-volantino(7:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(5:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(1:4) delimited size
                  into r-data-ini
           end-string.
           subtract passo from save-riga.
           move r-data-ini to spl-riga-stampa.
           move 2,65 to spl-colonna.
           perform SCRIVI.

           move "al" to spl-riga-stampa.
           move 4,85 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.
                                                                        
           string tpr-fine-volantino(7:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(5:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(1:4) delimited size
                  into r-data-fine
           end-string.
           move r-data-fine to spl-riga-stampa.
           move 5,35 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.

           move "DPO dal" to spl-riga-stampa.
           move 9,8 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.

           string tpr-ini-dpo(7:2) delimited size
                  "/"              delimited size
                  tpr-ini-dpo(5:2) delimited size
                  "/"              delimited size
                  tpr-ini-dpo(1:4) delimited size
                  into r-data-ini
           end-string.
           move r-data-ini to spl-riga-stampa.
           move 11,5 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.

           move "al" to spl-riga-stampa.
           move 13,9 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.

           string tpr-fine-dpo(7:2) delimited size
                  "/"               delimited size
                  tpr-fine-dpo(5:2) delimited size
                  "/"               delimited size
                  tpr-fine-dpo(1:4) delimited size
                  into r-data-fine
           end-string.
           move r-data-fine to spl-riga-stampa.
           move 14,35 to spl-colonna.
           subtract passo from save-riga.
           perform SCRIVI.

           add 0,05 to save-riga.
           move r-intesta to spl-riga-stampa.
           move 83 to spl-tipo-colonna.
           perform SCRIVI.
           add 4 to num-righe.
           add 0,3 to save-riga.

      ***---
       STAMPA-FRAME.
           move 10    to spl-pen-with.
           move 01,00 to spl-colonna.
           move 19,00 to spl-colonna-fine.

           add  0,4   to save-riga giving spl-riga.
           add  2,3   to save-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-grigio        to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 3                  to spl-pen-with.
           move 1,0                to spl-colonna.
           move 19,0               to spl-colonna-fine.
           add 0,4 to save-riga    giving spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       FORMAT-VEN.
           move rpr-prz-ven    to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-ven
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-ven
           end-string.

      ***---
       FORMAT-ACQ.
           move rpr-prz-acq    to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-acq
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-acq
           end-string.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           move 0 to num-righe.
           move 0,5 to save-riga.

      ***---
       CONTA-RIGHE.
           move low-value to tpr-rec.
           if link-volantino = 0
              if link-data-from not = 0
                 move link-data-from to tpr-ini-dpo
              end-if
              if link-gdo not = spaces
                 move link-gdo   to tpr-gdo
              end-if
              start tpromo key >= tpr-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo next at end exit perform end-read

                       if link-gdo not = spaces
                          if tpr-gdo not = link-gdo
                             exit perform
                          end-if
                       end-if

                       if tpr-ini-dpo > link-data-to
                          exit perform
                       end-if

                       if tpr-ini-dpo  >= link-data-from and
                          tpr-fine-dpo <= link-data-to
                          add 4 to num-righe
                          move tpr-codice to rpr-codice
                          move low-value  to rpr-articolo
                          start rpromo key >= rpr-chiave
                                invalid continue
                            not invalid
                                perform until 1 = 2
                                   read rpromo next 
                                        at end add 1 to num-righe
                                               exit perform 
                                   end-read
                                   if rpr-codice not = tpr-codice
                                      add 1 to num-righe
                                      exit perform
                                   end-if
                                   add 1 to num-righe
                                end-perform
                          end-start
                       end-if
                    end-perform
              end-start
              add 3 to num-righe
           else
              move link-volantino to tpr-codice
              read tpromo no lock
                   invalid continue
               not invalid 
                   add 4 to num-righe
                   move tpr-codice to rpr-codice
                   move low-value  to rpr-articolo
                   start rpromo key >= rpr-chiave
                         invalid continue
                     not invalid
                         perform until 1 = 2
                            read rpromo next 
                                 at end exit perform 
                            end-read
                            if rpr-codice not = tpr-codice
                               exit perform
                            end-if
                            add 1 to num-righe
                         end-perform
                   end-start
              end-read
           end-if.

           if num-righe > 1
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
           end-if.
           move 0 to num-righe.

      ***---
       CARICA-FONT.
      * Verdana 16BI
           initialize wfont-data Verdana16BI.
           move 16 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana16BI, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 14BI
           initialize wfont-data Verdana14BI.
           move 14 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana14BI, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10BI
           initialize wfont-data Verdana10BI.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10BI, wfont-data
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
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
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
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

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing space by low-value.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited low-value
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
       CLOSE-FILES.
           close tpromo rpromo articoli tgrupgdo blister.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana16BI.
           destroy Verdana14BI.
           destroy Verdana10BI.
           destroy Verdana8BI.
           destroy Verdana8B.
           destroy Verdana6B.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 
           display "                                                   "
              upon link-handle at column 22 line 10.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
