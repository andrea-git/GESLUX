      ***---
       LISTINO-PROMO-PERS.
           if sta-data = 0
              accept sta-data from century-date
           end-if.
           perform INIT-PROMO.
           perform OPEN-FILES-PROMO.
           if tutto-ok
              perform ELABORAZIONE-PROMO
              perform CLOSE-FILES-PROMO
           end-if.

           perform EXIT-PGM-PROMO.

      ***---
       INIT-PROMO.
      *-                       
           initialize path-tmp wstampa.
           set environment "PRINTER" to "-P SPOOLER".
           move 0,5 to passo.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  como-data    from century-date.
           accept  como-ora     from time.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           inspect sta-user     replacing trailing spaces by low-value.

           string  wstampa            delimited by low-value
                   "listino_promo"    delimited by size
                   "_"                delimited by size
                   sta-user           delimited by low-value
                   ".csv"             delimited by size
                   into wstampa
           end-string.                  
                               
           accept  path-tmp     from environment "PATH-ST".
           inspect path-tmp     replacing trailing spaces by low-value.

           string  path-tmp           delimited by low-value
                   "tmp-promo"        delimited by size
                   "_"                delimited by size
                   como-data          delimited by size
                   "_"                delimited by size
                   como-ora           delimited by size
                   ".tmp"             delimited by size
                   into path-tmp
           end-string.
          
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

      * Verdana 8I
           initialize wfont-data Verdana8I.
           move  8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8I, wfont-data
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
       OPEN-FILES-PROMO.
           perform OPEN-OUTPUT-TMP-PROMO.
           if tutto-ok
              if sta-si-excel
                 perform OPEN-OUTPUT-LINESEQ
              end-if
              if tutto-ok
                 open input articoli
                            lisagente
                            timposte
                            tmarche
                 if errori
                    if sta-si-excel
                       close lineseq
                       delete file lineseq
                    end-if
                 end-if
              else
                 close tmp-promo
                 delete file tmp-promo
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP-PROMO.
           open output tmp-promo.

      ***---
       ELABORAZIONE-PROMO.
           perform RIEMPI-TMP-PROMO.
           if trovato
              move low-value         to promo-rec
              start tmp-promo key is >= promo-chiave
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read tmp-promo next no lock 
                         at end
                         if not sta-si-excel
                            if save-riga < save-altezza-pagina - 0,5
                               perform STAMPA-LINEA
                            end-if
                         end-if
                         exit perform
                    end-read
   
                    if prima-volta
                       if sta-si-excel
                          perform ACCETTA-SEPARATORE
                          perform SCRIVI-INTESTAZIONE-PROMO
                       end-if
                       if not sta-si-excel
                          if sta-promo
                             move "GESLUX - Listino Promo" 
                               to spl-nome-job
                          else
                             move "GESLUX - Listino Pers"
                               to spl-nome-job
                          end-if
                          call   "selprint" using selprint-linkage
                          cancel "selprint"
                          if selprint-stampante not = space
                             move selprint-num-copie 
                               to SPL-NUM-COPIE
                             move selprint-stampante 
                               to SPL-NOME-STAMPANTE

                             set spl-apertura              to true
                             set spl-vertical              to true
                             set WFDEVICE-WIN-PRINTER      to true
                             call "spooler" using spooler-link
   
                             if spl-sta-annu
                                set errori to true
                                exit perform
                             else
                                move spl-altezza to save-altezza-pagina
                                perform CARICA-FONT
                                perform SCRIVI-INTESTAZIONE-PROMO
                             end-if
                          else
                             set spl-sta-annu to true
                             set errori to true
                             exit perform
                          end-if

                       end-if
                    end-if

                    if not sta-si-excel
                       if save-riga > ( save-altezza-pagina - passo )
                          perform SALTO-PAGINA-PROMO
      *                    perform SCRIVI-INTESTAZIONE-PROMO
                       end-if
                    end-if

                    perform VALORIZZA-RIGA-PROMO
                     
                    if errori exit perform end-if

                 end-perform

                 if sta-si-excel
                    perform CALL-EXCEL
                 else
                    if not spl-sta-annu
                       set spl-chiusura to true
                       call   "spooler" using spooler-link
                       cancel "spooler"
                    end-if
                 end-if
              end-if

           else

              if not spl-sta-annu
                 display message  
                      "Nessun articolo trovato nei limiti richiesti"
                           title titolo
                            icon 2
              end-if

           end-if.

      ***---
       RIEMPI-TMP-PROMO.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
           move sta-listino       to lis-codice.
           move  low-value        to lis-articolo.
           start lisagente key is >= lis-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read lisagente next no lock 
                      at end
                      exit perform
                 end-read

                 if lis-codice not = sta-listino
                    exit perform
                 end-if

                 set record-ok to false
                 move lis-articolo to art-codice
                 read articoli no lock
                      invalid continue
                  not invalid set record-ok to true
                 end-read

                 if record-ok
                    if sta-sett not = 0
                       if sta-sett = art-settore-merceologico
                          set record-ok to true
                       else
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    if sta-marca not = 0
                       if sta-marca = art-marca-prodotto
                          set record-ok to true
                       else
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    if sta-classe not = 0
                       if sta-classe = art-classe-1
                          set record-ok to true
                       else
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok

                    set record-ok to false
                    if sta-data >= lis-data-inizio-old and
                       sta-data <= lis-data-fine-old
                       move lis-data-inizio-old     to promo-data-inizio
                       move lis-data-fine-old       to promo-data-fine
                       move lis-prezzo-old          to prezzo-listino
                       move lis-calcolo-imposte-old to calcolo-imposte
                       set record-ok to true
                    end-if

                    if sta-data >= lis-data-inizio-new and
                       sta-data <= lis-data-fine-new
                       move lis-data-inizio-new     to promo-data-inizio
                       move lis-data-fine-new       to promo-data-fine
                       move lis-prezzo-new          to prezzo-listino
                       move lis-calcolo-imposte-new to calcolo-imposte
                       set record-ok to true
                    end-if

                    if record-ok

                       move art-marca-prodotto to mar-codice
                       read tmarche no lock invalid continue end-read   

                       set trovato to true

                       move 0 to como-perce
                       if sta-promo
                          compute promo-valore = 
                                  art-prezzo-vendita - prezzo-listino
                          compute como-perce  = 
                                ( promo-valore * 100 ) / 
                                  art-prezzo-vendita
                       end-if

                       move 0 to promo-cou promo-ic
                       if sta-promo
                          perform COMPUTE-IMPOSTE
                       else
                          if si-calcolo
                             perform COMPUTE-IMPOSTE
                          end-if
                       end-if
                       compute promo-valore = prezzo-listino + 
                                              promo-ic       +
                                              promo-cou
               
                       move promo-valore        to promo-totale
                       move como-perce          to promo-sconto
                       move prezzo-listino      to promo-prezzo
                       move art-descrizione     to promo-art-descrizione
                       move art-codice          to promo-articolo
                       move art-marca-prodotto  to promo-marca
                       move lis-codice          to promo-codice

                       write promo-rec 
                             invalid set errori to true 
                       end-write
                    end-if

                 end-if

                 if errori exit perform end-if

              end-perform
           end-if.

           if trovato
              close tmp-promo
              open input tmp-promo
           end-if.

      ***---
       SCRIVI-INTESTAZIONE-PROMO.
           add 1 to PageCounter.
           set YesString to false.
           perform STAMPA-FRAME.

           set prima-volta        to false.
           if promo-da = spaces
              string promo-data-inizio(7:2) delimited by size
                     "/"                    delimited by size
                     promo-data-inizio(5:2) delimited by size
                     "/"                    delimited by size
                     promo-data-inizio(3:2) delimited by size
                     into promo-da
              end-string
           end-if.

           if promo-a = spaces
              string promo-data-fine(7:2)   delimited by size
                     "/"                    delimited by size
                     promo-data-fine(5:2)   delimited by size
                     "/"                    delimited by size
                     promo-data-fine(3:2)   delimited by size
                     into promo-a
              end-string
           end-if.

           if sta-promo perform TITOLO-PROMO
           else         perform TITOLO-PERS
           end-if.

      ***---
       TITOLO-PROMO.
           move 0                  to save-riga.
           move 17                 to spl-tipo-colonna.
           move Verdana12I         to spl-hfont.
           move 0,05               to save-riga.

           move 1 to tipo-riga.
           move intestazione-promo to line-riga.
           perform SCRIVI.

           add 0,15               to save-riga.

           move 2 to tipo-riga.
           move Verdana10I          to spl-hfont.
           add 0,25                 to save-riga.
           move intestazione2-promo to line-riga.
           perform SCRIVI.

           if not sta-si-excel
              move 17,5                to spl-tipo-colonna
              move Verdana8I           to spl-hfont
              add 0,05                 to save-riga
              move PageCounter         to int-pag
              initialize line-riga
              string "Pag. " delimited size
                     int-pag delimited size
                     into line-riga
              end-string
              perform SCRIVI
              subtract 0,35 from save-riga
              move 17  to spl-tipo-colonna
           end-if.

           perform STAMPA-LINEA.

           subtract 0,3         from save-riga.
           move 18                to spl-tipo-colonna.
           move Verdana10B        to spl-hfont.
           move titolo-promo      to line-riga.
           move 3 to tipo-riga.
           perform SCRIVI.

           subtract 0,1         from save-riga.
           perform STAMPA-LINEA.

      ***---
       TITOLO-PERS.
           if pers-codice = spaces
              move promo-codice to pers-codice
           end-if.
           move 0                  to save-riga.
           move 17                 to spl-tipo-colonna.
           move Verdana12I         to spl-hfont.
           move 0,05               to save-riga.

           move 1 to tipo-riga.
           move intestazione-pers  to line-riga.
           perform SCRIVI.

           add 0,15               to save-riga.

           move 2 to tipo-riga.
           move Verdana10I          to spl-hfont.
           add 0,25                 to save-riga.
           move intestazione2-promo to line-riga.
           perform SCRIVI.

           perform STAMPA-LINEA.

           subtract 0,3         from save-riga.
           move 18                to spl-tipo-colonna.
           move Verdana10B        to spl-hfont.
           move titolo-promo      to line-riga.
           move 3 to tipo-riga.
           perform SCRIVI.

           subtract 0,1         from save-riga.
           perform STAMPA-LINEA.

      ***---
       VALORIZZA-RIGA-PROMO.
           move Verdana10               to spl-hfont.

           move promo-articolo          to rp-articolo.
           move promo-art-descrizione   to rp-des.
           move promo-sconto            to rp-sc.
           move promo-prezzo            to rp-prezzo.
           move promo-ic                to rp-ic.
           move promo-cou               to rp-cou.
           move promo-totale            to rp-totale.

           perform JUSTIFY-RIGHT.
           move riga-promo              to line-riga.
           move 19                      to spl-tipo-colonna.
           set YesString to true.
           perform SCRIVI.

      ***---
       COMPUTE-IMPOSTE.
      *****     compute promo-ic  = ( lis-prezzo * art-perce-imposte ) / 100.
      *****     compute promo-cou = ( lis-prezzo * art-perce-cou     ) / 100.
           move art-peso-utf      to prg-peso-utf.
           move art-peso-non-utf  to prg-peso-non-utf.
           set TrattamentoGDO to false.
           perform CALCOLA-IMPOSTE.
           move imposta-consumo to promo-ic.
           move imposta-cou     to promo-cou.

      ***---
       STAMPA-FRAME.
           if sta-si-excel exit paragraph end-if.

           move 2                    to spl-pen-with.
           move  5,2                 to spl-colonna.
           move 15,5                 to spl-colonna-fine.
           move 0,4                  to spl-riga.
           move 1,2                  to spl-riga-fine.
           set  spl-oggetto          to true.
           set  spl-rettangolo-round to true.
           set  spl-brush-ltgray     to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI.
           initialize spl-riga-stampa. 
           if sta-si-excel
              if YesString
                 initialize line-riga
                 string rp-articolo   delimited size
                        separatore    delimited size
                        rp-des        delimited size
                        separatore    delimited size
                        rp-sc         delimited size
                        separatore    delimited size
                        rp-prezzo     delimited size
                        separatore    delimited size
                        rp-ic         delimited size
                        separatore    delimited size
                        rp-cou        delimited size
                        separatore    delimited size
                        rp-totale     delimited size
                        into line-riga
                 end-string
                 write line-riga
              else
                 evaluate tipo-riga
                 when 1
                      initialize line-riga
                      if sta-promo
                         string " "                 delimited size
                                separatore          delimited size
                                "*** PRODOTTI "     delimited size
                                "IN PROMOZIONE ***" delimited size
                                into line-riga
                         end-string
                      else         
                         string " "                 delimited size
                                separatore          delimited size
                                "*** PRODOTTI "     delimited size
                                "DA LISTINO N° "    delimited size
                                pers-codice         delimited size
                                " ***"              delimited size
                                into line-riga
                         end-string
                      end-if
                      write line-riga
                 when 2         
                      initialize line-riga
                      string " "                 delimited size
                             separatore          delimited size
                             "Validità dal "     delimited size
                             promo-da            delimited size
                             " al "              delimited size
                             promo-a             delimited size
                             into line-riga
                      end-string
                      write line-riga
                      move spaces to line-riga
                      write line-riga
                 when 3
                      initialize line-riga
                      string "Codice"            delimited size
                             separatore          delimited size
                             "PRODOTTI NORMALI"  delimited size
                             separatore          delimited size
                             "Sc.%"              delimited size
                             separatore          delimited size
                             "Prezzo"            delimited size
                             separatore          delimited size
                             "Imposta"           delimited size
                             separatore          delimited size
                             "COU"               delimited size
                             separatore          delimited size
                             "TOTALE"            delimited size
                             into line-riga
                      end-string
                      write line-riga
                      move spaces to line-riga
                      write line-riga
                 end-evaluate
              end-if
           else
              add  passo         to save-riga
              move save-riga     to spl-riga
              set  spl-stringa   to true
              move line-riga     to spl-riga-stampa
              call "spooler"  using spooler-link
           end-if.
            
      ***---
       STAMPA-LINEA.
           if sta-si-excel exit paragraph end-if.

           move 3                  to spl-pen-with.
           move 0,7                to spl-colonna.
           move 19,5               to spl-colonna-fine.
           add  passo 0,3          to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       JUSTIFY-RIGHT.
           if sta-si-excel exit paragraph end-if.
        
           call "C$JUSTIFY" using rp-articolo, "R".
           call "C$JUSTIFY" using rp-sc,       "R".
           call "C$JUSTIFY" using rp-prezzo,   "R".
           call "C$JUSTIFY" using rp-ic,       "R".
           call "C$JUSTIFY" using rp-cou,      "R".
           call "C$JUSTIFY" using rp-totale,   "R".

      ***---
       SALTO-PAGINA-PROMO.
           if sta-si-excel exit paragraph end-if.

           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE-PROMO.

      ***---
       CLOSE-FILES-PROMO.
           close articoli lisagente timposte tmarche.
           if sta-si-excel
              close lineseq
           end-if.
           if trovato
              close tmp-promo
              delete file tmp-promo
           end-if.
  
      ***---
       EXIT-PGM-PROMO.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana12B.
           destroy Verdana10. 
           destroy Verdana12I.

           cancel "spooler".
           initialize spooler-link.

           goback.
