       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-pesi-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "listini.sl".
           copy "articoli.sl". 
           copy "tmp-pesi.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "listini.fd".
           copy "articoli.fd".
           copy "tmp-pesi.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-listini        pic xx.
       77  status-articoli       pic xx.
       77  status-tmp-pesi       pic xx.
       77  path-tmp-pesi         pic x(256).

      * COSTANTI
       78  titolo                value "GESLUX - Pesi Specifici".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 42.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(70).

       01  r-data                pic x(10).

       01  r-intesta.
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(11) value "Cod Cliente".
           05 filler             pic x(11) value "Descrizione".
           05 filler             pic x(3)  value "Ean".
           05 filler             pic x(10) value "Peso (Kg.)".

       01  r-riga.
           05 r-art              pic z(6).
           05 r-art-cli          pic x(15).
           05 r-art-descrizione  pic x(50).
           05 r-ean              pic x(13).

       01  r-peso                pic zz.zz9,999.

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
       77  link-data             pic 9(8).
       77  num-righe             pic 9(3).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana16B            handle of font.
       77  Verdana10BI           handle of font.
       77  Verdana8              handle of font.
       77  Verdana8B             handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       LINKAGE SECTION.
       77  link-handle           handle of window.
       77  link-gdo              pic x(5).
       77  link-gdo-intestazione pic x(30).

      ******************************************************************
       PROCEDURE DIVISION USING link-handle, 
                                link-gdo, 
                                link-gdo-intestazione.

       DECLARATIVES.
      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-listini
           when "39"
                set errori to true
                display message "File [LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LISTINI] inesistente"
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
       TMP-INEVASO-ERR SECTION.
           use after error procedure on tmp-pesi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-pesi
           when "39"
                set errori to true
                display message "File [TMP-PESI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-PESI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-PESI] inesistente"
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
                     open output tmp-pesi
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
           move 0   to num-righe.
           move 0,5 to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-pesi.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-pesi   from environment "PATH_ST".
           inspect path-tmp-pesi   replacing trailing 
                                   spaces by low-value.
           string  path-tmp-pesi   delimited low-value
                   "TMP-PESI"      delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".tmp"          delimited size
                   into path-tmp-pesi
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-pesi.
           if tutto-ok
              close    tmp-pesi
              open i-o tmp-pesi allowing readers
              open input listini articoli
           end-if.

      ***---
       ELABORAZIONE.
           |RECUPERO LA DATA DELL'ULTIMO LISTINO VALIDO PER IL GDO
           move 0          to    link-data.
           move high-value to    lst-chiave.
           move link-gdo   to    lst-gdo.
           start listini  key <= lst-chiave
                 invalid continue
           end-start.
           read listini previous
                at end continue
            not at end
                if lst-gdo = link-gdo
                   move lst-data to link-data
                end-if
           end-read.

           if link-data not = 0
              move low-value    to lst-rec
              move link-gdo     to lst-gdo

              start listini key >= lst-chiave
                    invalid set errori to true
              end-start

              if tutto-ok
                 perform until 1 = 2
                    read listini next at end exit perform end-read

                    if lst-gdo  not = link-gdo or
                       lst-data not = link-data
                       exit perform
                    end-if

                    initialize art-rec
                               tpp-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move lst-articolo to art-codice tpp-articolo
                    read articoli  no lock invalid continue end-read

                    if art-peso-utf not = 0
                       set  trovato           to true
                       move art-descrizione   to tpp-art-descrizione

                       move  lst-cod-art-cli  to tpp-cod-art-cli
                       move art-codice-ean-1  to tpp-ean
                       move  art-peso-utf     to tpp-peso
                       write tpp-rec 
                             invalid continue 
                         not invalid add 1 to num-righe
                       end-write
                    end-if

                 end-perform
              end-if

           end-if.  

           if not trovato
              display message "Nessun articolo trovato"
                        title titolo
                         icon 2
           else
              perform STAMPA
           end-if.

      ***---
       STAMPA.
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
              move low-value to tpp-rec
              start tmp-pesi key >= tpp-chiave
                    invalid continue
                not invalid
                    string link-data(7:2) delimited size
                           "/"            delimited size
                           link-data(5:2) delimited size
                           "/"            delimited size
                           link-data(1:4) delimited size
                           into r-data
                    end-string
                    perform SCRIVI-INTESTAZIONE
                    perform until 1 = 2
                       read tmp-pesi next 
                            at end exit perform 
                       end-read

                       if num-righe >= 78-MaxRows
                          perform SALTO-PAGINA
                       end-if

                       move tpp-articolo        to r-art
                       move tpp-cod-art-cli     to r-art-cli
                       move tpp-art-descrizione to r-art-descrizione
                       if tpp-ean not = 0
                          move tpp-ean          to r-ean
                       else
                          move spaces           to r-ean
                       end-if
                       move tpp-peso            to r-peso

                       move Verdana8  to spl-hfont
                       move r-riga    to spl-riga-stampa
                       move 78        to spl-tipo-colonna
                       set  spl-nero  to true
                       perform SCRIVI                    

                       subtract passo from save-riga
                       move Verdana8B to spl-hfont
                       move r-peso    to spl-riga-stampa
                       move 78,5      to spl-tipo-colonna
                       set  spl-nero  to true
                       perform SCRIVI

                       add 1 to num-righe

                 end-perform

                 perform PIE-DI-PAGINA

                 set spl-chiusura to true
                 call   "spooler" using spooler-link

           end-start.

      ***---
       SCRIVI-INTESTAZIONE.
           move 0        to num-righe.

           move 0,4                            to save-riga.
           move "** LUBEX - Pesi Specifici **" to spl-riga-stampa.
           move Verdana20BI                    to spl-hfont.
           move 58                             to spl-tipo-colonna.
           set spl-blu                         to true.
           perform SCRIVI.

           move 1,5                            to save-riga.
           initialize r-titolo.
           string "Listino in vigore dal " delimited size
                  r-data                   delimited size
                  into r-titolo
           end-string.
           move r-titolo                       to spl-riga-stampa.
           move Verdana16B                     to spl-hfont.
           move 58                             to spl-tipo-colonna.
           set spl-rosso                       to true.
           perform SCRIVI.


           move 2,5                            to save-riga.
           move link-gdo-intestazione          to spl-riga-stampa.
           move Verdana16B                     to spl-hfont.
           move 58                             to spl-tipo-colonna.
           set spl-blu                         to true.
           perform SCRIVI.

           move 4,2 to save-riga.
           set spl-blu to true.
           perform STAMPA-LINEA.

           move 3,8                to save-riga.
           move r-intesta          to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 77                 to spl-tipo-colonna.
           set spl-blu to true.
           perform SCRIVI.

           move 4,8 to save-riga.
           set spl-blu to true.
           perform STAMPA-LINEA.

           move 4,6                to save-riga.
            
      ***---
       STAMPA-LINEA.
           move 10                 to spl-pen-with.
           move 1,1                to spl-colonna.
           move 19,3               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

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
           perform SCRIVI-INTESTAZIONE.

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
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 16B
           initialize wfont-data Verdana16B.
           move 16 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana16B, wfont-data
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
       CLOSE-FILES.
           close articoli listini.
           delete file tmp-pesi.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana16B.
           destroy Verdana10BI.
           destroy Verdana8B.
           destroy Verdana8.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
