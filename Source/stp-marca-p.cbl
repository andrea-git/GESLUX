       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stp-marca-p.
       AUTHOR.                          Filippo.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "provvig.sl".
           copy "agenti.sl".
           copy "tmarche.sl".
           copy "tmp-stp-marca.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "provvig.fd".
           copy "agenti.fd".
           copy "tmarche.fd".
           copy "tmp-stp-marca.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       78  titolo        value "Stampa a livello marca per agente".
       
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
       77  status-tmarche        pic xx.
       77  status-tmp-stp-marca  pic xx.

       77  com-kg                pic 9(8)      value 0.
       77  com-fatt              pic s9(10)v99 value 0.

       77  tot-kg                pic 9(09)     value 0.
       77  tot-provvig           pic s9(10)v99 value 0.
       77  tot-media-kg          pic s9(10)v99 value 0.
       77  tot-fatt              pic s9(12)v99 value 0.

       77  como-media-kg         pic s9(10)v99 value 0.

       77  sav-agente            pic 9(5) value 99999.
       77  sav-marca             pic 9(4) value 9999.

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
       01  line-riga                pic x(900).
       01  riga-titolo              pic x(131).

       01  riga-trattini            pic x(131) value all "-".

       01  riga-intestazione.
           05 filler                pic x(7)  value "Agente:".
           05 int-age               pic z(5).
           05 filler                pic x(3).
           05 int-age-desc          pic x(40).

       01  riga-intestazione1.
           05 filler                pic x(5)  value "Marca".      
           05 filler                pic x(3)  value "Kg.".        |6
           05 filler                pic x(8)  value "Provvig.".   |9
           05 filler                pic x(9)  value "Media/Kg.".  |17
           05 filler                pic x(10) value "Fatt./Euro". |26

       01  r-riga.
           05 r-marca               pic z(4).
           05 r-marca-desc          pic x(30).                    | 5
           05 r-kg                  pic    --.---.--9.            |35
           05 r-val-provvig         pic   ---.---.--9,99.         |45
           05 r-media-kg            pic   ---.---.--9,99.         |59
           05 r-fatt                pic -.---.---.--9,99.         |73

       01  r-totali.                
           05 filler                pic x(06) value "TOTALI".
           05 r-tot-kg              pic     ---.---.--9.          | 7
           05 r-tot-provvig         pic   -.---.---.--9,99.       |18
           05 r-tot-media-kg        pic   -.---.---.--9,99.       |34
           05 r-tot-fatt            pic ---.---.---.--9,99.       |50

       77  st-agente                pic z(5).
       77  st-age-ragsoc            pic x(30).
       77  CountChar                pic 999.

       77  path-tmp-stp-marca       pic x(256).

       LINKAGE SECTION.
       copy "link-stp-marca.def".

      ******************************************************************
       PROCEDURE DIVISION using stp-marca-linkage.

       DECLARATIVES.
      ***---
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

      ***---
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
       TMP-STP-MARCA-ERR SECTION.
           use after error procedure on tmp-stp-marca.
           set tutto-ok  to true.
           evaluate status-tmp-stp-marca
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TMP-STP-MARCA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-STP-MARCA] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-STP-MARCA] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
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

           initialize path-tmp-stp-marca.
           accept  path-tmp-stp-marca from environment "PATH_ST".
           inspect path-tmp-stp-marca replacing trailing 
                                      spaces by low-value.
           string  path-tmp-stp-marca delimited low-value
                   "tmp-stp-marca"    delimited size
                   "_"                delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-stp-marca
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

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       OPEN-FILES.
           open output tmp-stp-marca.
           if tutto-ok
              close tmp-stp-marca
              open i-o tmp-stp-marca
              open input provvig
                         agenti
                         tmarche
           end-if.

      ***---
       ELABORAZIONE.
           initialize pvv-rec.
           move sta-da-age    to pvv-agente.
           move sta-data-from to pvv-data-fat.

           start provvig key is >=  k-data-fat
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read provvig next no lock at end exit perform end-read

                 if pvv-data-fat > sta-data-to
                    exit perform
                 end-if

                 if pvv-agente >= sta-da-age and
                    pvv-agente <= sta-a-age
LUBEXX              if pvv-normale or pvv-promo
LUBEXX                 perform VALORIZZA-RIGA
LUBEXX              end-if
                 end-if

                 if errori
                    exit perform
                 end-if

              end-perform
           end-if.

           if not trovato
              display message "Nessuna provvigione trovata"
                        title titolo
                         icon 2
           else
              perform STAMPA
              if not spl-sta-annu
                 set spl-chiusura to true
                 call   "spooler" using spooler-link
                 cancel "spooler"
              end-if
           end-if.

      ***---
       VALORIZZA-RIGA.
           move pvv-agente to tmp-stp-agente.
           move pvv-marca  to tmp-stp-cod-marca.
           read tmp-stp-marca
                invalid 
                move spaces to mar-descrizione
                move 0 to tmp-stp-kg
                move 0 to tmp-stp-provv
                move 0 to tmp-stp-fatt
                move pvv-marca to mar-codice
                read tmarche no lock invalid continue end-read
                move mar-descrizione to tmp-stp-mar-descrizione
           end-read.

           compute com-kg rounded = pvv-qta-vend         * pvv-peso-um.
           compute com-fatt       = pvv-prezzo-unit-vend * pvv-qta-vend.

           add com-kg          to tmp-stp-kg.
           add pvv-val-provvig to tmp-stp-provv.
           add com-fatt        to tmp-stp-fatt.
           write tmp-stp-rec invalid rewrite tmp-stp-rec end-write.
           set trovato to true.

      ***---
       STAMPA.
           move 0 to sav-agente.
           move low-value to tmp-stp-rec.
           start tmp-stp-marca key is >= k-des-marca
                 invalid continue
           end-start.
           perform until 1 =  2
              read tmp-stp-marca next at end exit perform end-read
              if sav-agente = 0 move tmp-stp-agente to sav-agente end-if
              if tmp-stp-agente not = sav-agente
                 perform TOTALI-GENERALI
                 move 0 to tot-kg
                 move 0 to tot-provvig
                 move 0 to tot-media-kg
                 move 0 to tot-fatt
                 perform SALTO-PAGINA
                 move tmp-stp-agente to sav-agente
              end-if
              perform STAMPA-RIGA
              if spl-sta-annu exit perform end-if
           end-perform.

           if tutto-ok
              if save-riga > save-altezza-pagina - 1,5
                 perform SALTO-PAGINA
              end-if
              perform TOTALI-GENERALI
           else
              if spl-sta-annu
                 display message "Procedura interrotta dall'utente!"
                           title titolo
                            icon 2
              end-if
           end-if.

      ***---
       STAMPA-RIGA.
           if prima-volta
              initialize spooler-link
              call   "selprint" using selprint-linkage
              cancel "selprint"

              if selprint-stampante not = space
                 move selprint-num-copie to spl-num-copie
                 move selprint-stampante to spl-nome-stampante

                 move "GESLUX - Stampa marca provv." to spl-nome-job
                 set spl-apertura         to true
                 set spl-vertical         to true
                 set wfdevice-win-printer to true
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
                 set errori       to true
              end-if
           end-if. 

           if tutto-ok
              if save-riga > ( save-altezza-pagina - passo )
                 perform SALTO-PAGINA
              end-if

              initialize line-riga
              move tmp-stp-cod-marca       to r-marca
              move tmp-stp-mar-descrizione to r-marca-desc

              move tmp-stp-kg        to r-kg
              add  tmp-stp-kg        to tot-kg

              move tmp-stp-provv     to r-val-provvig
              add  tmp-stp-provv     to tot-provvig

              compute como-media-kg rounded = tmp-stp-provv / tmp-stp-kg
              move como-media-kg    to r-media-kg

              move tmp-stp-fatt     to r-fatt
              add tmp-stp-fatt      to tot-fatt
      
              move r-riga           to line-riga
              
              move 30               to spl-tipo-colonna
              move Verdana10        to spl-hfont
              perform SCRIVI
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           set prima-volta        to false.
           initialize riga-titolo.
           move tmp-stp-agente to age-codice int-age
           read agenti no lock
                invalid initialize age-rec
           end-read.
           move age-ragsoc-1 to int-age-desc.

           string "PROVVIGIONI periodo: " delimited by size
                  sta-data-from(7:2)      delimited by size
                  "/"                     delimited by size
                  sta-data-from(5:2)      delimited by size
                  "/"                     delimited by size             
                  sta-data-from(3:2)      delimited by size             
                  " - "                   delimited by size             
                  sta-data-to(7:2)        delimited by size             
                  "/"                     delimited by size             
                  sta-data-to(5:2)        delimited by size             
                  "/"                     delimited by size             
                  sta-data-to(3:2)        delimited by size             
                  into riga-titolo
           end-string.

           |move 0 to CountChar.
           |inspect riga-titolo replacing trailing spaces by low-value.
           |inspect riga-titolo tallying CountChar 
           |                    for characters before low-value.

           perform STAMPA-FRAME.

           move 0                 to save-riga.
           move 28                to spl-tipo-colonna.

           move riga-intestazione to line-riga.
           move Verdana12B        to spl-hfont.
           perform SCRIVI.
           add 0,2                to save-riga.

           move riga-titolo       to line-riga.
           move Verdana12B        to spl-hfont
           perform SCRIVI.
           add 0,35               to save-riga.
                              
           perform STAMPA-LINEA.

           subtract 0,25         from save-riga.
           move 29                 to spl-tipo-colonna.
           move riga-intestazione1 to line-riga.
           perform SCRIVI.

           perform STAMPA-LINEA.

      ***---
       TOTALI-GENERALI.
           move spaces to line-riga.
           perform SCRIVI.
           
           add 0,25 to save-riga.
           perform STAMPA-LINEA.
           subtract 0,25 from save-riga.

           move tot-kg       to r-tot-kg.   
           move tot-provvig  to r-tot-provvig.
           compute tot-media-kg rounded = tot-provvig / tot-kg.
           move tot-media-kg to r-tot-media-kg.
           move tot-fatt     to r-tot-fatt.

           move Verdana12I to spl-hfont.
           move r-totali   to line-riga.
           move 31         to spl-tipo-colonna.
           perform SCRIVI.
           perform STAMPA-LINEA.

      ***---
       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           |compute spl-colonna = 3 + (90 - CountChar) * 0,27
           |compute spl-colonna-fine = 27 - (90 - CountChar) * 0,27
           move 1,5                  to spl-colonna.
           move 19                   to spl-colonna-fine.
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
           move 19,5               to spl-colonna-fine.
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
           perform SCRIVI-INTESTAZIONE.

      ***---
       CLOSE-FILES.
           close provvig agenti tmarche tmp-stp-marca.
           delete file tmp-stp-marca.
  
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
