       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-cli-det.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "tgrupgdo.sl".
           copy "agenti.sl".
           copy "tivaese.sl".
           copy "tcodpag.sl".
           copy "destini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "tgrupgdo.fd".
           copy "agenti.fd".
           copy "tivaese.fd".
           copy "tcodpag.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-agenti         pic xx.
       77  status-tivaese        pic xx.
       77  status-tcodpag        pic xx.
       77  status-destini        pic xx.

      * COSTANTI
       78  titolo                value "GESLUX - Dettaglio cliente".
       78  78-MaxRows            value 34.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).

       01  r1-fissa.
           05 filler             pic x(13) value "Provincia".
           05 filler             pic x(8)  value "C.A.P.".
           05 filler             pic x(8)  value "Nazione".

       01  r1.
           05 r1-prov            pic x(13).
           05 r1-cap             pic x(8).
           05 r1-naz             pic x(8).

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
       77  r-fido                pic x(14).
       77  cliente-ed            pic z(5).
       77  des-prog-ed           pic x(5).
       77  valore-z              pic zzz.zzz.zz9,99.
       77  valore-x              pic x(14).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana14B            handle of font.
       77  Verdana12BI           handle of font.
       77  Verdana12B            handle of font.
       77  Verdana8              handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       LINKAGE SECTION.
       77  link-cliente         pic 9(5).

      ******************************************************************
       PROCEDURE DIVISION using link-cliente.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-destini
           when "39"
                set errori to true
                display message "File [DESTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TTIPOCLI] inesistente"
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

      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-agenti
           when "39"
                set errori to true
                display message "File [AGENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[AGENTI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [AGENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "39"
                set errori to true
                display message "File [TIVAESE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIVAESE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on TCODPAG.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TCODPAG
           when "39"
                set errori to true
                display message "File [TCODPAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TCODPAG] inesistente"
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
           move 0,7  to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open input clienti destini ttipocli 
                      tgrupgdo agenti tivaese tcodpag.

      ***---
       ELABORAZIONE.
           set  cli-tipo-C   to true.
           move link-cliente to cli-codice.
           read clienti no lock 
                invalid set errori to true
           end-read.
           if tutto-ok
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read

              move cli-gdo to gdo-codice
              read tgrupgdo no lock invalid continue end-read
                                                             
              move cli-agente to age-codice
              read agenti no lock invalid continue end-read
                                                             
              move "IV"        to tbliv-codice1
              move cli-iva-ese to tbliv-codice2
              read tivaese no lock invalid continue end-read
                                                             
              move "PA"    to tblpa-codice1
              move cli-pag to tblpa-codice2
              read tcodpag no lock invalid continue end-read

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
            
              move 0,5              to save-riga
              move "** DETTAGLIO CLIENTE **" to spl-riga-stampa
              move Verdana20BI      to spl-hfont
              move 58               to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-FRAMES

              perform STAMPA-LINEA

              move link-cliente to cliente-ed
              initialize r-titolo
              string "CODICE: "  delimited size
                     cliente-ed  delimited size
                     into r-titolo
              end-string

              move 1,7  to save-riga

              move Verdana14B to spl-hfont
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 2,8 to save-riga
              move Verdana12B to spl-hfont
              move cli-ragsoc-1 to spl-riga-stampa
              perform SCRIVI

              move 3,8 to save-riga
              move Verdana12BI to spl-hfont
              move "INDIRIZZO" to spl-riga-stampa
              perform SCRIVI

              move 4,3 to save-riga
              move Verdana12B to spl-hfont
              move cli-indirizzo to spl-riga-stampa
              perform SCRIVI

              move 5,3 to save-riga
              move Verdana12BI to spl-hfont
              move "LOCALITA'" to spl-riga-stampa
              perform SCRIVI

              move 5,8 to save-riga
              move Verdana12B to spl-hfont
              move cli-localita to spl-riga-stampa
              perform SCRIVI

              move 6,8         to save-riga
              move Verdana12BI to spl-hfont
              move r1-fissa    to spl-riga-stampa
              move 75 to spl-tipo-colonna
              perform SCRIVI

              move 7,3         to save-riga
              move Verdana12B  to spl-hfont
              move cli-cap     to r1-cap
              move cli-prov    to r1-prov
              move cli-nazione to r1-naz
              move r1          to spl-riga-stampa
              move 75 to spl-tipo-colonna
              perform SCRIVI

              move 58 to spl-tipo-colonna
              move 8,3 to save-riga
              move Verdana12BI to spl-hfont
              move "TIPOLOGIA CLIENTE" to spl-riga-stampa
              perform SCRIVI

              move 8,8 to save-riga
              move Verdana12B to spl-hfont
              initialize r-titolo
              string tcl-codice      delimited size
                     " - "           delimited size
                     tcl-descrizione delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 9,8 to save-riga
              move Verdana12BI to spl-hfont
              move "GRUPPO GDO" to spl-riga-stampa
              perform SCRIVI

              move 10,3 to save-riga
              move Verdana12B to spl-hfont
              initialize r-titolo
              string gdo-codice       delimited size
                     " - "            delimited size
                     gdo-intestazione delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 11,3 to save-riga
              move Verdana12BI to spl-hfont
              move "AGENTE" to spl-riga-stampa
              perform SCRIVI

              move 11,8 to save-riga
              move Verdana12B to spl-hfont
              initialize r-titolo
              string age-codice      delimited size
                     " - "           delimited size
                     age-ragsoc-1    delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 12,8 to save-riga
              move Verdana12BI to spl-hfont
              move "ESENZIONE IVA" to spl-riga-stampa
              perform SCRIVI

              move 13,3 to save-riga
              move Verdana12B to spl-hfont
              initialize r-titolo
              string cli-iva-ese        delimited size
                     " - "              delimited size
                     tbliv-descrizione1 delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 14,3 to save-riga
              move Verdana12BI to spl-hfont
              move "CODICE PAGAMENTO" to spl-riga-stampa
              perform SCRIVI

              move 14,8 to save-riga
              move Verdana12B to spl-hfont
              initialize r-titolo
              string cli-pag            delimited size
                     " - "              delimited size
                     tblpa-descrizione1 delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 15,8 to save-riga
              move Verdana12BI to spl-hfont
              move "FIDO" to spl-riga-stampa
              perform SCRIVI

              perform FORMAT-FIDO
              move 16,3 to save-riga
              move Verdana12B to spl-hfont
              move r-fido to spl-riga-stampa
              perform SCRIVI

              perform STAMPA-DESTINI

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       STAMPA-DESTINI.
           move 17,7 to save-riga.
           move "** DESTINI COLLEGATI **" 
             to spl-riga-stampa.
           move 58         to spl-tipo-colonna.
           move Verdana12B to spl-hfont.
           perform SCRIVI.
      
           move low-value  to des-chiave.
           move cli-codice to des-codice.
           start destini key >= des-chiave
                 invalid continue
             not invalid
                 move Verdana8 to spl-hfont
                 move 18,5 to save-riga
                 perform until 1 = 2
                    read destini next at end exit perform end-read
                    if des-codice not = cli-codice
                       exit perform
                    end-if
                    if save-riga >= save-altezza-pagina - 0,7
                       perform SALTO-PAGINA
                       move 0,6 to save-riga
                       move "** DESTINI COLLEGATI **" 
                         to spl-riga-stampa
                       move 58         to spl-tipo-colonna
                       move Verdana12B to spl-hfont
                       perform SCRIVI
                       move 1,5 to save-riga
                       move Verdana8 to spl-hfont
                    end-if
                    move des-prog to des-prog-ed
                    inspect des-prog-ed replacing leading x"30" by x"20"
                    inspect des-ragsoc-1  replacing trailing 
                                          spaces by low-value
                    inspect des-indirizzo replacing trailing 
                                          spaces by low-value
                    inspect des-localita  replacing trailing 
                                          spaces by low-value
                    initialize r-titolo
                    string des-prog-ed    delimited size
                           ") "           delimited size
                           des-ragsoc-1   delimited low-value
                           " - "          delimited size
                           des-indirizzo  delimited low-value
                           " - "          delimited size
                           des-cap        delimited size
                           " - "          delimited size
                           des-localita   delimited low-value
                           " - "          delimited size
                           des-prov       delimited size
                           " - "          delimited size
                           des-piva       delimited size
                           into r-titolo
                    end-string
                    move r-titolo to spl-riga-stampa
                    move 76       to spl-tipo-colonna
                    perform SCRIVI
                    perform STAMPA-LINEA-DESTINI
                 end-perform
           end-start.

      ***---
       FORMAT-FIDO.
           move cli-fido           to valore-z.
           move valore-z           to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-fido.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-fido
           end-string.

      ***---
       STAMPA-FRAMES.
           move 3,5   to save-riga.

           move 8     to spl-pen-with.

           move save-riga to spl-riga.
           add 0,5 to spl-riga giving spl-riga-fine.

           move 1,5   to spl-colonna.
           move 18,5  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.
      
           move  3,0  to spl-colonna.
           move  7,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move   8,0  to spl-colonna.
           move  12,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move 13,0  to spl-colonna.
           move 17,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.
      
           move  3,0  to spl-colonna.
           move  7,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move   8,0  to spl-colonna.
           move  12,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move 13,0  to spl-colonna.
           move 17,0  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

      * QUADRATI RIEMPITIVI
           move 4,52 to save-riga.

           add 0,0  to save-riga giving spl-riga.
           add 0,46 to spl-riga  giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link. 

           add 1,5  to save-riga giving spl-riga.
           add 0,46 to spl-riga  giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move  3,02 to spl-colonna.
           move  6,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move  8,02 to spl-colonna.
           move 11,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move 13,02 to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link. 

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 8                  to spl-pen-with.
           move 1,5                to spl-colonna.
           move 18,5               to spl-colonna-fine.
           move 18,0               to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-LINEA-DESTINI.
           move 8                  to spl-pen-with.
           move 1,5                to spl-colonna.
           move 18,5               to spl-colonna-fine.
           add 0,5 to save-riga    giving spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.

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

      * Verdana 12BI
           initialize wfont-data Verdana12BI.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12BI, wfont-data
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
           close clienti destini ttipocli 
                 tgrupgdo agenti tivaese tcodpag.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana14B.
           destroy Verdana12BI.
           destroy Verdana12B.
           destroy Verdana8.

           cancel "spooler".
           initialize spooler-link.
           goback.
