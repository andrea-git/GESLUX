       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stbozze-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".
           copy "btnotacr.sl".
           copy "brnotacr.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tvettori.sl".
           copy "articoli.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "CLI.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".
           copy "btnotacr.fd".
           copy "brnotacr.fd".
           copy "clienti.fd".
           copy "destini.fd". 
           copy "tvettori.fd".
           copy "articoli.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "CLI.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION. 
           copy "acugui.def".
           copy "spooler.def".
           copy "fonts.def".
           copy "varsca".
           copy "selprint.lks".
       78  copia                 value "Copia uso int. amm.vo".
       77  messaggio             pic X(150) value spaces.
       77  font-size-dply        pic Z(5).
       77  Arial16B              handle of font.
       77  Arial8B               handle of font.
       77  CourierNew7           handle of font.
       77  CourierNew8           handle of font.
       77  CourierNew9           handle of font.
       77  CourierNew9B          handle of font.
       77  CourierNew11B         handle of font.
       77  CourierNew11          handle of font.
       77  BitmapSfondoHandle    pic S9(9) comp-4.
       77  BitmapSfondoNcHandle  pic S9(9) comp-4.
       77  status-tparamge       pic xx.
       77  status-btnotacr       pic xx.
       77  status-brnotacr       pic xx.
       77  status-destini        pic xx.
       77  status-clienti        pic xx.
       77  status-tvettori       pic xx.
       77  status-articoli       pic xx.
       77  status-tcodpag        pic xx.
       77  status-tivaese        pic xx.
       77  status-tcaumag        pic xx.
       77  status-CLI            pic xx.

       77  tot-solo-cou          pic 9(5)v99.
       77  tot-cobat             pic 9(5)v99.
       77  tot-piombo            pic 9(5)v99.
       77  tot-pezzi-n           pic 9(8) value 0.
       77  tot-pezzi-r           pic 9(8) value 0.
       77  tot-pezzi-a           pic 9(8) value 0.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  codice-x              pic x(5).

       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  filler                pic 9.
           88  NewPage           value 1, false 0.
       77  filler                pic 9.
           88  PrimaVolta        value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  FlagTrovataIVA        pic 9.
           88  TrovataIVA        value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".      
       77  filler                pic 9.
           88  UsaPrimaRiga      value 1.
           88  UsaSecondaRiga    value 2. 
       78  BitmapDiSfondo        value "sfondo_bozze.jpg".
       78  titolo                value "Stampa Bozze".
       77  NumPagina             pic  999.
       78  RowsPerPage           value 26.
       78  RowsPerPagePlus1      value 27.
       77  WrittenRows           pic   99.
       77  scelta                pic    9.
       77  codice-ed             pic z(5).

       77  importo-netto         pic 9(9)v99.
       77  RowsToDo              pic 9(6).
       77  como-imposta          pic 9(9)v99. |COU + COBAT + PIOMBO 
       77  como-imponibile       pic 9(9)v99. |IMPONIBILE IVA
       77  imponibile-merce      pic 9(9)v99.
       77  tot-imponibile        pic 9(9)v99.
       77  tot-consumo           pic 9(9)v99.
       77  tot-cou               pic 9(9)v99.
       77  tot-iva               pic 9(9)v99.
       77  tot-fattura           pic 9(9)v99.

       77  tot-pag               pic 9(2).
       77  num-pag               pic 9(2).
       77  resto                 pic 9(3).
       77  como-z                pic z.zz9.

       01  pagina-di.
         05 filler               pic x(7) value "Pagina ".
         05 num-pag-ed           pic x(2).
         05 filler               pic x(4) value " di ".
         05 tot-pag-ed           pic x(2).

       01  tabella-iva           occurs 3. 
         05 cod-iva              pic x(3).
         05 tipo-iva             pic 9.
            88 iva-sigla         value 1, false 0.
         05 imponibile-iva       pic 9(9)v99.
         05 importo-iva          pic 9(15)v99.
         05 articolo-iva         pic x(30).

       77  perce-iva-x           pic x(3).
       77  perce-iva-9di3        pic 9(3).

       77  riga-scad             pic 9(3)v99.

       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.
       77  NumStampa             pic 999.

       77  WFONT-STATUS          pic s9(5) value ZERO.

       01 st-intestazione.
         02  st-riga-cli-rec-1.
           03 st-cli-ragsoc-1        pic x(40).
           03 st-des-ragsoc-1        pic x(40).

         02  st-riga-cli-rec-2.
           03 st-cli-indirizzo       pic x(40).
           03 st-des-indirizzo       pic x(40).

         02  st-riga-cli-rec-3.
           03 st-cli-cap             pic 9(5) blank zero.
           03 st-cli-localita        pic x(30).
           03 st-cli-prov            pic x(2).
           03 st-des-cap             pic 9(5) blank zero.
           03 st-des-localita        pic x(30).
           03 st-des-provincia       pic x(2).

         02 st-riga-int-1.
           03 st-btno-num            pic z(8).
           03 st-btno-data           pic x(8).
           03 st-tco-descrizione     pic x(27).
           03 st-cli-abi             pic X(5).
           03 st-cli-cab             pic X(5).

         02 st-riga-int-2.
           03 st-btno-num-fatt       pic x(10).
           03 st-btno-data-fatt      pic x(8).
           03 st-btno-num-bolla      pic x(10).
           03 st-btno-data-bolla     pic X(8).
           03 st-btno-data-ingresso  pic X(11).
           03 st-piva-codfis         pic x(16).

       01 st-riga.
         05 st-brno-cod-articolo     pic z(6).
         05 st-art-descrizione       pic x(50).
         05 st-art-unita-di-misura   pic x(2).
         05 st-brno-qta              pic zz.zzz.zzz.
         05 st-brno-imponib-merce    pic zzz.zzz.zz9,99.
         05 st-brno-imp-consumo      pic z.zz9,99.
         05 st-brno-imp-cou-cobat    pic z.zz9,99.
         05 st-importo-netto         pic zzz.zzz.zz9,99.
         05 st-aliquota              pic zz.

       01 st-riga-omaggio.
         05 st-brno-cod-articolo-om   pic z(6).
         05 st-art-descrizione-om     pic x(50).
         05 st-art-unita-di-misura-om pic x(2).
         05 st-brno-qta-om            pic zz.zzz.zzz.
         05 st-brno-omaggio           pic x(14).
         05 st-iva-omaggio            pic x(30).
         05 st-aliquota-om            pic zz.

       01 st-riga-dich.
         05 filler                 pic x.
         05 st-valori-dich         pic x(71).
         05 filler                 pic x.

       01 st-riga-totali.
         05 st-importo             pic zzz.zzz.zz9,99.
         05 st-aliquota-tot        pic xxx.
         05 st-imponibile          pic zzz.zzz.zz9,99 blank zero.
         05 st-importo-iva         pic z.zzz.zzz.zz9,99 blank zero.
         05 filler                 pic x(2) value " €".
         05 st-importo-totale      pic z.zzz.zz9,99.

       01 st-riga-totali2.
         05 st-importo2            pic zzz.zzz.zz9,99.
         05 st-aliquota-tot2       pic xxx.
         05 st-imponibile2         pic zzz.zzz.zz9,99 blank zero.
         05 st-articolo-iva2       pic x(16).
         05 filler                 pic x(2) value " €".
         05 st-importo-totale2     pic z.zzz.zz9,99.

       01  st-riga-colpa-corriere.
         05 st-btno-colpa           pic x(20).
         05 st-btno-vet             pic z(5).
         05 st-btno-vet-descrizione pic x(40).

       01  st-riga-copia.
         05 filler                 pic x.
         05 st-copia               pic x(21) value copia.

       77  como-campo   pic x(120).
       01  riga-campo   pic x(40)  occurs 3.
       77  num-righe               pic 9(2) value 0.

       77  idx                           pic 9(3).
       77  idx-sca                       pic 9(2).
       77  cont-inizio                   pic 9(3).
       77  cont-per                      pic 9(3).
       77  cont-char                     pic 9(3).

       01  filler                        pic 9.
           88 prima-pagina               value 1 false 0.
       01  filler                        pic 9.
           88 exit-perform-int           value 1 false 0.
       01  filler                        pic 9.
           88 prezzo-zero                value 1 false 0.
       01  tipo-doc                      pic x.
           88 nota-addebito              value "A".
           88 nota-credito               value "N".
           88 nota-rotta                 value "R".

       LINKAGE SECTION.
           copy "link-stordcp.def". 

      ******************************************************************
       PROCEDURE DIVISION using stordcp-limiti.

       DECLARATIVES.
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPARAMGE] inesistente"
                        title = titolo
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

      ***---
       BTNOTACR SECTION.
           use after error procedure on btnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-btnotacr 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [BTNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [BTNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[BTNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
       BRNOTACR SECTION.
           use after error procedure on brnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-brnotacr
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [BRNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [BRNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[BRNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.  

      ***---
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
           end-evaluate.  

      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set tutto-ok  to true.
           evaluate status-cli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLI] Indexed file corrupt!"
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
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title titolo
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
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
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
           end-evaluate.

      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"
                display message "Impossibile procedere."
                 x"0d0a""Tabella codici pagamento [TCODPAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                display message "Impossibile procedere."
                 x"0d0a""Tabella codici iva [TIVAESE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok    to true.

           set environment "PRINTER" to "-P SPOOLER".
           set PrimaVolta to true.
           set NewPage    to true.
           move 0 to NumPagina.

           copy resource ".\STAMPA\sfondo_bozze.jpg".
           call "W$BITMAP" using WBITMAP-LOAD, BitmapDiSfondo,
                          giving BitmapSfondoHandle.

      ***---
       OPEN-FILES.
           open input clienti   destini  
                      tvettori  CLI
                      articoli  tparamge
                      tcodpag   tivaese 
                      btnotacr  brnotacr
                      tcaumag.

      ***---
       ELABORAZIONE.
           move spaces   to tge-chiave.
           read tparamge no lock invalid continue end-read.
           perform ELABORA-BOZZE.

           if spl-sta-annu
              display message "Procedura interrotta dall'utente"
                        title titolo
                         icon 2
           else
              if trovato
                 set spl-chiusura to true
                 call   "spooler" using spooler-link
                 cancel "spooler"
              else
                 display message "Nessun documento presente "
                                 "avente il criterio selezionato"
                           title titolo
                            icon 2
              end-if
           end-if.

      ***---
       ELABORA-BOZZE.
           move stordc-da-anno to btno-anno.
           move stordc-da-num  to btno-numero.
           
           start btnotacr key is >= btno-chiave
                 invalid set errori to true
             not invalid
                 perform until 1 = 2
                    read btnotacr next at end exit perform end-read
                    if btno-anno not = stordc-da-anno or
                       btno-numero  >  stordc-a-num
                       exit perform
                    end-if

                    perform CONTA-RIGHE-TOTALI
                                          
                    set trovato       to true
                    set prima-pagina  to true
                    set nota-credito  to true
                    perform LOOP-RIGHE
                    if spl-sta-annu
                       exit perform
                    end-if
           
                    |Controllo che esistano righe per addebito
                    move  btno-anno     to brno-anno
                    move  btno-numero   to brno-numero
                    move  low-value     to brno-num-riga
                    set   brno-addebito to true
                    start brnotacr key  is >= brno-chiave
                          invalid continue
                      not invalid
                          read brnotacr next
                          if brno-anno   = btno-anno   and
                             brno-numero = btno-numero and
                             brno-addebito
                             set prima-pagina  to true
                             set nota-addebito to true
                             add 1 to num-pag
                             perform LOOP-RIGHE
                          end-if
                    end-start

                    |Controllo che esistano righe per merce rotta
                    move  btno-anno         to brno-anno
                    move  btno-numero       to brno-numero
                    move  low-value         to brno-num-riga
                    set   brno-merce-rotta  to true
                    start brnotacr key  is >= brno-chiave
                          invalid continue
                      not invalid
                          read brnotacr next
                          if brno-anno   = btno-anno   and
                             brno-numero = btno-numero and
                             brno-merce-rotta
                             set prima-pagina  to true
                             set nota-rotta to true
                             add 1 to num-pag
                             perform LOOP-RIGHE
                          end-if
                    end-start
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           initialize spooler-link.
           move "GESLUX - Stampa Bozze" to spl-nome-job.
           if PrimaVolta
      *    nuova selezione
              call   "selprint" using selprint-linkage
              cancel "selprint"

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE
      *    NUOVA SELEZIONE
                 set spl-apertura to true
                 set spl-vertical to true
                 set WFDEVICE-WIN-PRINTER    to true
                 call "spooler" using spooler-link
                 set PrimaVolta   to false
                 if spl-sta-annu 
                    exit paragraph 
                 else
                    perform CARICA-FONT
                 end-if
      *    NUOVA SELEZIONE
              else
                 set spl-sta-annu to true
                 exit paragraph
              end-if
      *    NUOVA SELEZIONE
           end-if.

           move  tipo-doc     to brno-tipo.

           perform STAMPA-INTESTAZIONE.
                                               
           move  btno-anno    to brno-anno.
           move  btno-numero  to brno-numero.
           move  low-value    to brno-num-riga.
           start brnotacr key is >= brno-chiave
                 invalid continue
           end-start.

           move 0 to WrittenRows.
           move 0 to importo-netto tot-imponibile tot-consumo
                     tot-cou       tot-iva        tot-fattura
                     RowsToDo      imponibile-merce
                     tot-cobat tot-piombo tot-solo-cou.

           perform INIT-TABELLA-IVA.

           perform CONTA-RIGHE.

           |Non ho mai l'intestazione perciò l'inizio
           |è fisso e il carattere dev'essere settato!!
           move CourierNew9 to spl-hfont.
           move 0,4         to spl-passo.
           move 12,2        to spl-riga.

           perform until 1 = 2
              read brnotacr next at end exit perform end-read
              if brno-anno   not = btno-anno   or
                 brno-numero not = btno-numero or
                 brno-tipo   not = tipo-doc
                 exit perform
              end-if
              perform STAMPA-RIGHE
           end-perform.

           perform STAMPA-PIE-PAGINA.
           perform STAMPA-PAGINA-DI.

      ***---
       CONTA-RIGHE-TOTALI.
           move 0 to tot-pezzi-a tot-pezzi-n tot-pezzi-r.
           set prezzo-zero to false.
           move 0 to RowsToDo.
           move 0 to tot-pag 
           move 1 to num-pag
           move btno-anno     to brno-anno.
           move btno-numero   to brno-numero.
           set  brno-addebito to true.
           start brnotacr key >= brno-chiave
                 invalid  continue
           end-start.
           perform until 1 = 2
              read brnotacr  next at end exit perform end-read
              if   brno-anno   not = btno-anno   or
                   brno-numero not = btno-numero
                 exit perform
              end-if
              if not brno-addebito
                 exit perform
              end-if

              add 1        to RowsToDo
              add brno-qta to tot-pezzi-a
           end-perform.

           if RowsToDo not = 0
              if RowsToDo > RowsPerPagePlus1
                 move 0 to resto
                 divide RowsToDo by RowsPerPagePlus1 giving tot-pag
                        remainder resto
                 if resto > 0
                    add 1 to tot-pag
                 end-if
              else
                 add 1 to tot-pag
              end-if
           end-if.
           
           move btno-anno     to brno-anno.
           move btno-numero   to brno-numero.
           set  brno-nota     to true.
           start brnotacr key >= brno-chiave
                 invalid  continue
           end-start.
           perform until 1 = 2
              read brnotacr  next at end exit perform end-read
              if   brno-anno   not = btno-anno   or
                   brno-numero not = btno-numero 
                 exit perform
              end-if
              if not brno-nota
                 exit perform
              end-if
              add 1        to RowsToDo
              add brno-qta to tot-pezzi-n
           end-perform.

           if RowsToDo > RowsPerPagePlus1
              move 0 to resto
              divide RowsToDo by RowsPerPagePlus1 giving tot-pag
                     remainder resto
              if resto > 0
                 add 1 to tot-pag
              end-if
           else
              add 1 to tot-pag
           end-if.

           move btno-anno     to brno-anno.
           move btno-numero   to brno-numero.
           set  brno-merce-rotta   to true.
           start brnotacr key >= brno-chiave
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read brnotacr  next at end exit perform end-read
                    if   brno-anno   not = btno-anno   or
                         brno-numero not = btno-numero
                         exit perform
                    end-if
                    if not brno-merce-rotta
                       exit perform
                    end-if
                    add 1        to RowsToDo
                    add brno-qta to tot-pezzi-r
                 end-perform

                 if RowsToDo > RowsPerPagePlus1
                    move 0 to resto
                    divide RowsToDo by RowsPerPagePlus1 giving tot-pag
                           remainder resto
                    if resto > 0
                       add 1 to tot-pag
                    end-if
                 else
                    add 1 to tot-pag
                 end-if
           end-start.
 
      ***---
       CONTA-RIGHE.
           move 0 to RowsToDo.
           perform until 1 = 2
              read brnotacr  next at end exit perform end-read
              if brno-anno   not = btno-anno   or
                 brno-numero not = btno-numero or
                 brno-tipo   not = tipo-doc
                 exit perform
              end-if
              add 1 to RowsToDo
              if brno-prz-unitario = 0
                 set prezzo-zero to true
              end-if
           end-perform.

           move tipo-doc      to brno-tipo.
           move btno-anno     to brno-anno.
           move btno-numero   to brno-numero.
           move  low-value    to brno-num-riga.
           start brnotacr key is >= brno-chiave
                 invalid continue
           end-start.

      ***---
       CARICA-FONT.
      * Arial 8B
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

      * Arial 16B
           initialize wfont-data Arial16B.
           move 16 to wfont-size.
           move "Arial"              to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Arial16B, wfont-data
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
          
      * Courier New 8
           initialize wfont-data CourierNew8.
           move 8 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew8, wfont-data
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
 
      * Courier New 9B
           initialize wfont-data CourierNew9B.
           move 9 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew9B, wfont-data
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

      * Courier New 11B
           initialize wfont-data CourierNew11B.
           move 11 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew11B, wfont-data
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
       STAMPA-INTESTAZIONE.
           initialize st-intestazione
                      st-riga
                      st-riga-totali
                      cli-rec
                      record-tblpa
                      des-rec
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

           evaluate true
           when nota-credito
           when nota-rotta
                move btno-cod-cli    to cli-codice
           when nota-addebito
                move btno-cod-cli-fm to cli-codice
           end-evaluate.
           set  cli-tipo-C    to true.
           read clienti no    lock invalid continue end-read.

           |Leggo CLI di G2 per vedere se il cliente ha lo
           |spostamento delle scadenze di Agosto e Dicembre
           move cli-codice to cli-codice-G2.
           read CLI no lock invalid continue end-read.

           move "PA"         to tblpa-codice1.
           move btno-cod-pag to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.

           set des-no-invio to true.

           if nota-addebito
              move btno-prg-destino-fm to btno-prg-destino
           end-if.

           if btno-prg-destino not = 0
              move cli-codice       to des-codice
              move btno-prg-destino to des-prog
              read destini invalid continue end-read
           end-if.

           move 4,5 to spl-riga.
           move 0,5 to spl-passo.

           initialize st-riga-cli-rec-3.

           initialize st-cli-ragsoc-1.
           move cli-codice         to codice-x.
           inspect codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using codice-x, "L".
           inspect codice-x replacing trailing x"20" by low-value.

           string codice-x     delimited low-value
                  " "          delimited size
                  cli-ragsoc-1 delimited size
                  into st-cli-ragsoc-1
           end-string.

           initialize st-des-ragsoc-1.
           move des-prog           to codice-x.
           inspect codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using codice-x, "L".
           inspect codice-x replacing trailing x"20" by low-value.

           string codice-x     delimited low-value
                  " "          delimited size
                  des-ragsoc-1 delimited size
                  into st-des-ragsoc-1
           end-string.

           move st-riga-cli-rec-1  to spl-riga-stampa.
           move 1                  to spl-tipo-colonna.
           perform SCRIVI.

           move spaces             to spl-riga-stampa.
           move 1                  to spl-tipo-colonna.
           add spl-passo           to spl-riga.
           perform SCRIVI.

           move cli-indirizzo      to st-cli-indirizzo.
           move des-indirizzo      to st-des-indirizzo.
           move st-riga-cli-rec-2  to spl-riga-stampa.
           move 1                  to spl-tipo-colonna.
           add spl-passo           to spl-riga.
           perform SCRIVI.

           move cli-cap of clienti to st-cli-cap.
           move cli-localita       to st-cli-localita.
           move cli-prov           to st-cli-prov.
      
           move des-cap            to st-des-cap.
           move des-localita       to st-des-localita.
           move des-prov           to st-des-provincia.

           move st-riga-cli-rec-3  to spl-riga-stampa.
           move 2                  to spl-tipo-colonna.
           add spl-passo           to spl-riga.
           perform SCRIVI.
      *
           evaluate true
           when nota-credito
                move btno-causale to tca-codice
                read tcaumag no lock invalid continue end-read
                if tca-tipo-nota-reso
                   move "BOZZA NOTA CREDITO RESO MERCE" 
                     to spl-riga-stampa
                else
                   move "BOZZA NOTA CREDITO DIFF PREZZO"
                     to spl-riga-stampa
                end-if
           when nota-addebito
                if btno-fm-vettore
                   move "BOZZA NOTA ADDEBITO CORRIERE"
                                                     to spl-riga-stampa
                else
                   move "BOZZA NOTA ADDEBITO CLIENTE"
                                                     to spl-riga-stampa
                end-if
           when nota-rotta
                move "MERCE ROTTA"                   to spl-riga-stampa
           end-evaluate.

           move 0 to spl-tipo-colonna.
           move 0,1 to spl-colonna.
           move 6,8 to spl-riga.
           move Arial16B to spl-hfont.
           perform SCRIVI.

           move 0    to spl-tipo-colonna.
           move 14,7 to spl-colonna.
           move 6,8  to spl-riga.
           move Arial16B      to spl-hfont.
           move btno-num-reso to como-z.
           initialize spl-riga-stampa.
           string "RESO N. " delimited size
                  como-z     delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.

           move 6   to spl-riga
           move 2,4 to spl-passo.
           move CourierNew11 to spl-hfont.
           move btno-numero     to st-btno-num.
           call "C$JUSTIFY"  using st-btno-num, "R".

           move all "/"        to st-btno-data
           move btno-data(3:2) to st-btno-data(7:2).
           move btno-data(5:2) to st-btno-data(4:2).
           move btno-data(7:2) to st-btno-data(1:2).
                                            
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

           move btno-num-fatt        to st-btno-num-fatt.
           call "C$JUSTIFY"       using st-btno-num-fatt, "L".
           inspect st-btno-num-fatt replacing leading x"30" by x"20".
           call "C$JUSTIFY"       using st-btno-num-fatt, "R".
           move all "/"              to st-btno-data-fatt.
           move btno-data-fatt(3:2)  to st-btno-data-fatt(7:2).
           move btno-data-fatt(5:2)  to st-btno-data-fatt(4:2).
           move btno-data-fatt(7:2)  to st-btno-data-fatt(1:2).

           move btno-num-bolla       to st-btno-num-bolla.
           call "C$JUSTIFY"       using st-btno-num-bolla, "R".
           move all "/"              to st-btno-data-bolla.
           move btno-data-bolla(3:2) to st-btno-data-bolla(7:2).
           move btno-data-bolla(5:2) to st-btno-data-bolla(4:2).
           move btno-data-bolla(7:2) to st-btno-data-bolla(1:2).

           move all "/"                 to st-btno-data-ingresso(1:8).
           move btno-data-ingresso(3:2) to st-btno-data-ingresso(7:2).
           move btno-data-ingresso(5:2) to st-btno-data-ingresso(4:2).
           move btno-data-ingresso(7:2) to st-btno-data-ingresso(1:2).


           move cli-codice     to codice-ed.

           if cli-piva not = spaces
              move cli-piva   to st-piva-codfis
           else
              move cli-codfis to st-piva-codfis
           end-if.
           move st-riga-int-2 to spl-riga-stampa.
           move 4             to spl-tipo-colonna.
           add spl-passo      to spl-riga.
           perform SCRIVI.
                 
           move 1 to spl-passo.
           move CourierNew11B  to spl-hfont.
           move 0,4            to spl-passo.
           perform STAMPA-MOTIVO-CONTESTAZIONE.
           move CourierNew11  to spl-hfont.
           perform STAMPA-NOTE-INTESTAZIONE.
           set prima-pagina to false.

      ***---
       STAMPA-MOTIVO-CONTESTAZIONE.
           if not prima-pagina   exit paragraph end-if.
           move btno-motivo-cont to como-campo.
           move 0,4              to spl-colonna.
           move 9,7              to spl-riga.
           perform SPEZZA-CAMPO.
           perform SCRIVI-RIGHE.

      ***---
       STAMPA-NOTE-INTESTAZIONE.
           if not prima-pagina exit paragraph end-if.
           move btno-note      to como-campo.
           move 10,1           to spl-colonna.
           move 9,7            to spl-riga.
           perform SPEZZA-CAMPO.
           perform SCRIVI-RIGHE. 

      ***---
       SPEZZA-CAMPO.
           initialize riga-campo(1).
           initialize riga-campo(2).
           initialize riga-campo(3).

           move 0               to idx.
           move 1               to cont-inizio.
           set exit-perform-int to false.

           perform 100 times
              if cont-inizio >= 120 or idx >= 3 exit perform end-if
              add  1 to idx
              move 0 to cont-char
      *    controllo di non andare oltre i 200 caratteri
              if cont-inizio > 81
                 compute cont-per = 120 - cont-inizio
                 set exit-perform-int to true
              else
                 move 40  to cont-per
              end-if

              inspect como-campo(cont-inizio:cont-per) 
                      tallying cont-char for all x"0D"
              if cont-char = 0
                 |move 40  to cont-per
                 continue
              else
                 initialize cont-per
                 inspect como-campo(cont-inizio:40) 
                         tallying cont-per for characters before x"0D"
              end-if
              if cont-per not = 0
                 move como-campo(cont-inizio:cont-per) 
                                         to riga-campo(idx)
      *    se appena dopo i 40 caratteri premo invio devo ignorarlo
                 if cont-per = 40
                    add cont-per  to cont-inizio
                    if cont-inizio < 119 and 
                       como-campo(cont-inizio:1) = x"0D"
                       add 2 to cont-inizio
                    end-if
                    subtract cont-per from cont-inizio
                 end-if
              else
                 move space  to riga-campo(idx)
              end-if
              if cont-char = zero
                 add 40   to cont-inizio
              else
                 compute cont-inizio = cont-inizio + cont-per + 2                                                         
              end-if
              if exit-perform-int
                 exit perform
              end-if
           end-perform.

           initialize num-righe.
           perform varying idx from 1 by 1 until idx > 3
              if riga-campo(idx) not = space
                 move idx to num-righe
              end-if
           end-perform.

      ***---
       SCRIVI-RIGHE.
           perform varying idx from 1 by 1 until idx > num-righe
              move riga-campo(idx)    to spl-riga-stampa
              move 0                  to spl-tipo-colonna
              add spl-passo           to spl-riga
              perform SCRIVI
           end-perform.
           add spl-passo to spl-riga.

      ***---
       STAMPA-BITMAP.
           set spl-bitmap to true.
           move 1,5 to spl-colonna.
           move   3 to spl-riga. 

           move BitmapSfondoHandle   to spl-hbitmap.

           move 26,8 to spl-bitmap-height.
           move 19,5 to spl-bitmap-width.

           call "spooler" using spooler-link.

      ***---
       STAMPA-RIGHE.
           perform SCRITTURA-RIGA. 

      ***---
       SCRITTURA-RIGA.
           if WrittenRows =  RowsPerPage 
              if RowsToDo > 1
                 move 0             to WrittenRows
                 move "...Segue >>" to spl-riga-stampa(100:)
                 perform SCRIVI

                 perform STAMPA-PAGINA-DI

                 add 1 to num-pag

                 initialize spooler-link
                 perform STAMPA-INTESTAZIONE
                 move CourierNew9 to spl-hfont
                 move 0,4         to spl-passo
                 move 12,2        to spl-riga
              end-if
           end-if.

           perform RIGA-NORMALE.
           move 7                to spl-tipo-colonna.

           perform SCRIVI.

           add spl-passo to spl-riga.

           add 1 to WrittenRows.
           subtract 1 from RowsToDo.

      ***---
       RIGA-NORMALE.
           initialize record-tbliv
                      art-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move brno-cod-articolo to st-brno-cod-articolo art-codice.
           read articoli no lock invalid continue end-read.
           move art-descrizione(1:30) to st-art-descrizione.
           move art-unita-di-misura   to st-art-unita-di-misura.

           move brno-qta              to st-brno-qta.

           if brno-imponib-merce not = 0 or
              brno-imp-cou-cobat not = 0 or
              brno-imp-consumo   not = 0 or
              brno-prz-unitario  not = 0 or
              brno-cod-iva       not = tge-cod-iva-omag

              if stordc-magg > 0                   

                 compute brno-prz-unitario =
                         brno-prz-unitario * 
                      (( 100 + stordc-magg ) / 100)
                 compute brno-imponib-merce =
                         brno-prz-unitario  -
                         brno-imp-cou-cobat -
                         brno-imp-consumo   -
                         brno-add-piombo
              end-if

              move brno-imponib-merce   to st-brno-imponib-merce

              move brno-imp-consumo     to st-brno-imp-consumo
              compute como-imposta = brno-imp-cou-cobat  + 
                                     brno-add-piombo
              move como-imposta         to st-brno-imp-cou-cobat

              compute como-imponibile   =
                      brno-imponib-merce +
                      brno-imp-consumo   +
                      brno-imp-cou-cobat +
                      brno-add-piombo

              compute importo-netto    = brno-qta * como-imponibile

              compute imponibile-merce = imponibile-merce + 
                                       ( brno-qta * brno-imponib-merce )
              compute tot-consumo      = tot-consumo      + 
                                       ( brno-qta * brno-imp-consumo )
              compute tot-cou          = tot-cou          + 
                                       ( brno-qta * como-imposta )
              compute tot-imponibile   = tot-imponibile + importo-netto

              move importo-netto        to st-importo-netto

           end-if.

           move "IV"            to tbliv-codice1.
           move brno-cod-iva    to tbliv-codice2.
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
                            
           if brno-imponib-merce = 0 and
              brno-imp-cou-cobat = 0 and
              brno-imp-consumo   = 0 and
              brno-prz-unitario  = 0 and
              brno-cod-iva       = tge-cod-iva-omag
              move 0                   to imponibile-iva(idx)
              move 0                   to como-imponibile 
              move "OMAGGIO"           to st-brno-omaggio
              move st-brno-cod-articolo to st-brno-cod-articolo-om

              move st-art-descrizione
                to st-art-descrizione-om
              move st-art-unita-di-misura
                to st-art-unita-di-misura-om
              move st-brno-qta         to st-brno-qta-om
              move tbliv-descrizione1 to st-iva-omaggio
              move st-aliquota        to st-aliquota-om
              call "C$JUSTIFY"  using st-brno-cod-articolo-om, "R"
              call "C$JUSTIFY"  using st-brno-qta-om,          "R"
              call "C$JUSTIFY"  using st-brno-omaggio,         "R"
              call "C$JUSTIFY"  using st-aliquota-om,          "R"
              move st-riga-omaggio    to spl-riga-stampa
           else
              call "C$JUSTIFY"  using st-brno-cod-articolo,    "R"
              call "C$JUSTIFY"  using st-brno-qta,             "R"
              call "C$JUSTIFY"  using st-brno-imponib-merce,   "R"
              call "C$JUSTIFY"  using st-brno-imp-consumo,     "R"
              call "C$JUSTIFY"  using st-brno-imp-cou-cobat,   "R"
              call "C$JUSTIFY"  using st-importo-netto,       "R"
              call "C$JUSTIFY"  using st-aliquota,            "R"
              move st-riga         to spl-riga-stampa
           end-if.

      ***---
       STAMPA-PAGINA-DI.
           move num-pag to num-pag-ed.
           move tot-pag to tot-pag-ed.
           inspect num-pag-ed replacing leading x"30" by x"20".
           inspect tot-pag-ed replacing leading x"30" by x"20".
           call "C$JUSTIFY" using tot-pag-ed, "L".
           move 0           to spl-tipo-colonna.
           move 27,7        to spl-riga.
           move 17,8        to spl-colonna.
           move Arial8B     to spl-hfont.
           move pagina-di   to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-PIE-PAGINA.
           evaluate true
           when nota-addebito
                move tot-pezzi-a to como-z
           when nota-credito
                move tot-pezzi-n to como-z
           when nota-rotta
                move tot-pezzi-r to como-z
           end-evaluate.

           string "TOT " delimited size
                  como-z delimited size
                  into spl-riga-stampa
           end-string.
           move 0      to spl-tipo-colonna.
           move 8,4    to spl-colonna.
           move 23,2   to spl-riga.
           move CourierNew9B to spl-hfont.
           perform SCRIVI.

           move CourierNew11   to spl-hfont.
           move 0,9            to spl-passo.
           move 23,6           to spl-riga.
           move 8,5            to spl-tipo-colonna.

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

           move CourierNew11 to spl-hfont.

           evaluate true
           when btno-corriere    move "CORRIERE"  to st-btno-colpa
           when btno-magazzino   move "MAGAZZINO" to st-btno-colpa
           when btno-fornitore   move "FORNITORE" to st-btno-colpa
           when btno-cliente     move "CLIENTE"   to st-btno-colpa
           when btno-ufficio     move "UFFICIO"   to st-btno-colpa
           end-evaluate.

           move 26,2 to spl-riga.
           move 9,5  to spl-tipo-colonna.
           move btno-vettore to vet-codice st-btno-vet.
           read tvettori no lock invalid continue end-read.
           move vet-descrizione        to st-btno-vet-descrizione.
           move st-riga-colpa-corriere to spl-riga-stampa.
           perform SCRIVI.
           
           if prezzo-zero 
              set spl-rosso to true
              move "ATTENZIONE!!! PREZZO ZERO!!!" to spl-riga-stampa
              move 0   to spl-tipo-colonna
              move 0,1 to spl-colonna
              add  0,7 to spl-riga
              move Arial16B to spl-hfont
              perform SCRIVI
              set spl-nero to true
           end-if.


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

      ***---
       CLOSE-FILES.
           close btnotacr clienti  destini CLI
                 tvettori brnotacr tparamge
                 articoli tcodpag  tivaese tcaumag.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Arial8B.
           destroy Arial16B.
           destroy CourierNew7.
           destroy CourierNew9.
           destroy CourierNew9B.
           destroy CourierNew11.
           destroy CourierNew11B.
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoHandle.
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoNcHandle.

           cancel "spooler".
           initialize spooler-link.

           goback.
