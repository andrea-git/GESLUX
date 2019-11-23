       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-bozze.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "teva.sl". 
           copy "reva.sl".
           copy "articoli.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "progmag.sl".
           copy "tmagaz.sl".
           copy "tcaumag.sl".
           copy "tcodpag.sl".
           copy "tivaese.sl".
           copy "rordforn.sl".
           copy "rlistini.sl".
           copy "nordforn.sl".
           copy "tordforn.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "teva.fd".
           copy "reva.fd".
           copy "articoli.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "progmag.fd".
           copy "tmagaz.fd".
           copy "tcaumag.fd".
           copy "tcodpag.fd".
           copy "tivaese.fd".
           copy "rordforn.fd".
           copy "rlistini.fd".
           copy "nordforn.fd".
           copy "tordforn.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".                  
           
       01  r-stof-riga.
           05 r-stof-art          pic z(6).
           05 filler pic x   value space.
           05 r-stof-des              pic x(40).         
           05 R-STOF-COD-FORN         pic z(8).
           05 R-STOF-DATA             pic x(10).
           05 r-stof-qta              pic x(7).
      *     05 r-stof-qta-eva          pic x(7).
           05 r-stof-imb              pic x(22).
      *     05 r-stof-cod-dog          pic x(8).
           05 r-stof-peso-netto       pic x(12).
           05 r-stof-peso-tot-netto   pic x(12).
           05 r-stof-peso-tot-utf     pic x(12).
           05 r-stof-prz-listino.|      pic x(13).|
              10 r-stof-prz-list   PIC  x(20).|
              10 r-stof-prz-list-e PIC  x.|
           05 r-stof-sconto-1-ed.
              10 r-stof-sconto-1      pic x(6).
              10 r-stof-perce-1       pic x.
           05 r-stof-sconto-2-ed.
              10 r-stof-sconto-2      pic x(6).
              10 r-stof-perce-2       pic x.
           05 r-stof-sconto-3-ed.
              10 r-stof-sconto-3      pic x(6).
              10 r-stof-perce-3       pic x.
           05 r-stof-sconto-4-ed.
              10 r-stof-sconto-4      pic x(6).
              10 r-stof-perce-4       pic x.
           05 r-stof-sconto-5-ed.
              10 r-stof-sconto-5      pic x(6).
              10 r-stof-perce-5       pic x.
           05 r-stof-prz-netto.|        pic x(13).|
              10 r-stof-prz-net   PIC  x(20).|
              10 r-stof-prz-net-e PIC  x.|

           05 r-stof-imp-consumo.|      pic x(12).|
              10 r-stof-imp-cons   PIC  x(20).|
              10 r-stof-imp-cons-e PIC  x.|
           05 r-stof-imp-cou-cobat.    |PIC  x(11).|
              10 r-stof-imp-cou   PIC  x(20).|
              10 r-stof-imp-cou-e PIC  x.|
           05 r-stof-add-piombo.|       PIC  x(11).|
              10 r-stof-add-pb        PIC  x(20).|
              10 r-stof-add-pb-e      PIC  x.|
           05 r-stof-costi-aggi.|       PIC  x(13).|
              10 r-stof-costi-agg   PIC  x(20).|
              10 r-stof-costi-aggi-e PIC  x.|
           05 r-stof-prz-finale.|       PIC  x(13).|
              10 r-stof-prz-fin   PIC  x(20).|
              10 r-stof-prz-fin-e PIC  x.|
           05 r-stof-prz-tot-finale.
              10 r-stof-prz-tot-fin   PIC  x(20).|
              10 r-stof-prz-tot-fin-e PIC  x.|

       01  r-stof-riga-testa.
           05 rt-stof-art             pic X(30).
           05 rt-stof-des             pic X(30).
           05 Rt-STOF-COD-FORN        pic X(30).
           05 Rt-STOF-DATA            pic X(30).
           05 rt-stof-qta             pic X(30).
           05 rt-stof-imb             pic X(30).
           05 rt-stof-peso-netto      pic X(30).
           05 rt-stof-peso-tot-netto  pic X(30).
           05 rt-stof-peso-tot-utf    pic X(30).
           05 rt-stof-prz-listino     pic X(30).
           05 rt-stof-sconto-1        pic X(30).
           05 rt-stof-sconto-2        pic X(30).
           05 rt-stof-sconto-3        pic X(30).
           05 rt-stof-sconto-4        pic X(30).
           05 rt-stof-sconto-5        pic X(30).
           05 rt-stof-prz-netto       pic X(30).
           05 rt-stof-imp-consumo     pic X(30).
           05 rt-stof-imp-cou-cobat   pic X(30).
           05 rt-stof-add-piombo      pic X(30).
           05 rt-stof-costi-aggi      pic X(30).
           05 rt-stof-prz-finale      pic X(30).
           05 rt-stof-prz-tot-finale  pic X(30).


       77  como-pic-z5 pic z(5).
       77  como-pic-z8 pic z(8).
       77  como-pic-96v93 pic ----.--9,999.
       77  como-pic-96v92 pic ---.--9,99.
      * 77  como-pic-97v92 pic ----.--9,99.
      * 77  como-pic-93v92 pic --9,99.
      * 77  como-pic-94v92 pic -.--9,99.
       77  como-pic-96v94 pic ----.--9,9999.

       77  como-pic-97v92 pic --.---.--9,99.
       77  como-pic-97v94 pic --.---.--9,9999.

       77  como-pic-93v92 pic --9,99.
       77  como-pic-93v94 pic --9,9999.

       77  como-pic-94v92 pic --.--9,99.
       77  como-pic-94v94 pic --.--9,9999.

       77  logo-handle           handle of bitmap.

       78  titolo                value "Stampa Bozza d'evasione".
       78  MaxRighe              value 33.
                                 
       77  status-teva       pic xx.
       77  status-reva       pic xx.
       77  status-articoli   pic xx.
       77  status-clienti    pic xx.
       77  status-timballi   pic xx.
       77  status-timbalqta  pic xx.
       77  status-progmag    pic xx.
       77  status-tmagaz     pic xx.
       77  status-tcaumag    pic xx.
       77  status-tcodpag    pic xx.
       77  status-tivaese    pic xx.
       77  status-rordforn   pic xx.
       77  status-rlistini   pic xx.
       77  status-nordforn   pic xx.
       77  status-tordforn   pic xx.

       77  Arial14BI         handle of font.
       77  Arial20BI         handle of font.
       77  arial5B           handle of font.
       77  arial7B           handle of font.
       77  arial5            handle of font.
       77  arial6            handle of font.
       77  arial6b           handle of font.
       77  arial4            handle of font.
       77  font-note         handle of font.

       77  messaggio         pic x(150)  value spaces.
       77  wfont-status      pic s9(5)   value zero.
       77  font-size-dply    pic z(5)    value zero.
       77  num-pagina        pic 9(3)    value zero.
       77  num-pag-ed        pic z(3)    value zero.

       77  como-peso         PIC  9(6)v9(3).
       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).
       77  riga              pic 99.
       77  pronostico        pic 99.
       77  imballi-ed        pic z(4).

       77  tot-imposta       pic 9(6)v9999.
       77  tot-imposta-3dec       pic 9(6)v9(3).
       77  tot-imposta-2dec       pic 9(6)v9(2).


       77  tot-cou                pic 9(6)v9(4). 
       77  tot-cou-3dec           pic 9(6)v9(3). 
       77  tot-cou-2dec           pic 9(6)v9(2). 

       77  tot-piombo             pic 9(6)v9(4). 
       77  tot-piombo-3dec        pic 9(6)v9(3). 
       77  tot-piombo-2dec        pic 9(6)v9(2). 

       77  como-pic-z7 pic z(7).
       77  tot-peso-utf      pic 9(6)v999.
       77  tot-qta           pic 9(5).
       77  tot-peso          pic 9(6)v999.
       77  totale                 pic 9(7)v9(4).
       77  tot-iva                pic 9(9)v9(4).

      *
       77  controllo         pic xx.
           88  tutto-ok      value "OK".
           88  errori        value "ER".

       77  como-prezzo      PIC  9(9)v9(4).
       77  como-prezzo-3dec PIC  9(9)v9(3).
       77  como-prezzo-2dec PIC  9(9)v9(2).

       01  tab-iva.
           05 cod-iva  pic x(3) occurs 50.
           05 imp-iva  pic 9(9)v9999 occurs 50.

       77  idx   pic 9(3).

       77  como-iva              pic 9(9)v9(5).
       77  como-iva-4dec         pic 9(9)v9(4).
       77  como-iva-3dec         pic 9(9)v9(3).
       77  como-iva-2dec         pic 9(9)v9(2).


       77  como-xx              pic x(2).

       77  cont                 pic 9(3).
       77  cont2                pic 9(3).
       77  num-ord              pic x(30).
       77  num-ord-ed           pic z(8).
       77  como-numero          pic 9(8).

       78  78-max-ordini        value 100.

       01  elenco-ordini.
           05 el-ordini   pic 9(8) occurs 78-max-ordini.

       01                       pic 9.
           88 ordine-singolo    value 1 false zero.

       01  como-tof-chiave.
           10 como-tof-anno     PIC  9(4).
           10 como-tof-numero   PIC  9(8).

       01                       pic 9.
           88 prima-bozza       value 1 false zero.

       LINKAGE SECTION.
           copy "link-st-bozze.def".

      ******************************************************************
       PROCEDURE DIVISION using st-bozze-linkage.

       DECLARATIVES.

      ***---
       teva-ERR SECTION.
           use after error procedure on teva.
           set tutto-ok  to true.
           evaluate status-teva 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [teva] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [teva] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[teva] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---      
       reva-ERR SECTION.
           use after error procedure on reva.
           set tutto-ok  to true.
           evaluate status-reva
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [reva] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [reva] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[reva] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Clienti [CLIENTI] inesistente"
                        title = titolo
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                        title = titolo
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
       NORDFORN-ERR SECTION.
           use after error procedure on NORDFORN.
           set tutto-ok  to true.
           evaluate status-NORDFORN
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [NORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [NORDFORN] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[NORDFORN] Indexed file corrupt!"
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
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to riga.
           set     tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.
           COPY RESOURCE "logo-st.bmp".
           CALL "w$bitmap" USING WBITMAP-LOAD "logo-st.BMP", 
                   GIVING logo-handle.

      ***---
       OPEN-FILES.              
           open input teva  
           open input reva  
           open input clienti 
           open input articoli
           open input timballi.
           open input timbalqta.
           open input progmag.
           open input tmagaz.
           open input tcaumag.
           open input tcodpag.
           open input tivaese.
           open input rordforn.
           open input rlistini.
           open input nordforn.
           open input tordforn.

      ****---
       ELABORAZIONE.
           set prima-bozza   to true
           move stobz-da-teva-chiave  to teva-chiave
           start TEVA key not < teva-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read teva next
                       at end
                          exit perform
                    end-read
                    if teva-chiave > stobz-a-teva-chiave
                       exit perform
                    end-if
                    perform STAMPA-BOZZA
                    if spl-sta-annu
                       exit perform
                    end-if
                 end-perform
           end-start.

           if not prima-bozza
              perform CHIUDI-STAMPA
           end-if.


      ***---
       STAMPA-BOZZA.
      *     move stobz-da-teva-chiave to teva-chiave.
      *     read teva no lock 
      *        invalid 
      *           set errori to true 
      *     end-read.
           move teva-cod-forn to cli-codice
           set cli-tipo-f    to true
           read CLIENTI no  lock
              invalid
                 continue
           end-read
           move teva-mag     to mag-codice
           read tmagaz
              invalid
                 move space  to mag-descrizione
           end-read.

           if tutto-ok
              perform VALORIZZA-ORDINI

              if prima-bozza
                 set prima-bozza   to false
                 perform APRI-STAMPA
                 perform STAMPA-TESTA
              else
                 move zero   to num-pagina
                 perform SALTO-PAGINA
              end-if

              if not spl-sta-annu
                 |perform STAMPA-TESTA
                 move low-value  to reva-rec
                 move teva-chiave to reva-chiave
                 start reva key not < reva-chiave
                    invalid 
                       continue
                 end-start
                 move  0 to tot-qta
                            tot-peso
                            tot-peso-utf
                            totale 
                            tot-iva
                            tot-imposta
                            tot-cou
                            tot-piombo

                 move zero   to riga
                 perform until 1 = 2
                    read reva next 
                       at end 
                          exit perform 
                    end-read
      
                    if teva-anno   not = reva-anno   or
                       teva-numero not = reva-numero
                       exit perform
                    end-if
                    perform SCRIVI-RIGA
                 end-perform

                 perform SCRIVI-PIEDE
      *
              end-if
           end-if.

      ***---
       SCRIVI-RIGA.
           perform VALORIZZA-CORRELATI

           add 1 to riga
           if riga > MaxRighe
              perform SALTO-PAGINA
              move 1   to riga
           end-if.

           initialize r-stof-riga
           move reva-articolo to r-stof-art 
                                 art-codice
           read articoli no lock 
              invalid 
                 move spaces to art-descrizione 
           end-read

           move reva-chiave-progmag  to prg-chiave
           read progmag
              invalid
                 continue
           end-read

           add reva-qta   to tot-qta

           compute tot-peso =
                   tot-peso + 
                 ( reva-peso * reva-qta)

           compute tot-peso-utf =
                   tot-peso-utf + 
                 ( prg-peso-utf * reva-qta )

           compute tot-imposta = tot-imposta + 
                                (reva-imp-cons * reva-qta)
           compute tot-cou = tot-cou + 
                                (reva-coubat * reva-qta)
           compute tot-piombo = tot-piombo + 
                                (reva-add-pb * reva-qta)

           move art-descrizione to r-stof-des
      *     move rlis-art-forn to r-stof-cod-forn.
           move reva-numero-ordf   to num-ord-ed
           move num-ord-ed         to r-stof-cod-forn.
           
           move reva-chiave-testa-ordf to tof-chiave.
           read tordforn no lock 
                invalid move 0 to tof-data-ordine
           end-read.
           initialize r-stof-data.
           string tof-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  tof-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  tof-data-ordine(1:4) delimited size
                  into r-stof-data
           end-string.


      *     move reva-qta              to como-pic-z5
      *     move como-pic-z5           to r-stof-qta

           move reva-imballo   to imq-codice
           read timbalqta
              invalid
                 move space  to imq-tipo
                 move zero   to imq-qta-imb
           end-read
           move imq-tipo  to imb-codice
           read timballi
              invalid
                 move space  to imb-descrizione
           end-read

           move imq-qta-imb to imballi-ed
           call "C$JUSTIFY" using imballi-ed, "L"
           inspect imb-descrizione replacing trailing 
                                                spaces by low-value
           initialize r-stof-imb
           string imballi-ed       delimited by spaces
                  " x "            delimited by size
                  art-udm-imballo  delimited by size
                  into r-stof-imb
     
           if reva-peso = zero
              move space           to r-stof-peso-netto
           else
              move reva-peso        to como-pic-96v93
              move como-pic-96v93  to r-stof-peso-netto
           end-if

           compute como-peso = reva-peso * reva-qta
           if como-peso = zero
              move space  to r-stof-peso-tot-netto
           else
              move como-peso       to como-pic-96v93
              move como-pic-96v93  to r-stof-peso-tot-netto
           end-if

           compute como-peso = prg-peso-utf * reva-qta

           if como-peso = zero
              move space           to r-stof-peso-tot-utf
           else
              move como-peso       to como-pic-96v93
              move como-pic-96v93  to r-stof-peso-tot-utf
           end-if

           if rof-prz-unitario = zero
              move space  to r-stof-prz-listino
           else
              move rof-prz-unitario(12:2)  to como-xx
              if como-xx not = "00"
                 move rof-prz-unitario   to como-pic-96v94
                 move como-pic-96v94(2:) to r-stof-prz-list
              else
                 move rof-prz-unitario   to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-list
              end-if
              call "C$JUSTIFY" using r-stof-prz-list, "R"
              move "€"                   to r-stof-prz-list-e
           end-if.

           if rof-sconto-1 = zero 
              move space           to r-stof-perce-1
                                      r-stof-sconto-1
           else
              move rof-sconto-1    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-1
              move "%"             to r-stof-perce-1
           end-if.
      
           if rof-sconto-2 = zero 
              move space           to r-stof-perce-2
                                      r-stof-sconto-2
           else
              move rof-sconto-2    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-2
              move "%"             to r-stof-perce-2
           end-if.
      
           if rof-sconto-3 = zero 
              move space           to r-stof-perce-3
                                      r-stof-sconto-3
           else
              move rof-sconto-3    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-3
              move "%"             to r-stof-perce-3
           end-if.
      
           if rof-sconto-4 = zero 
              move space           to r-stof-perce-4
                                      r-stof-sconto-4
           else
              move rof-sconto-4    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-4
              move "%"             to r-stof-perce-4
           end-if.
      
           if rof-sconto-5 = zero 
              move space           to r-stof-perce-5
                                      r-stof-sconto-5
           else
              move rof-sconto-5    to como-pic-93v92
              move como-pic-93v92  to r-stof-sconto-5
              move "%"             to r-stof-perce-5
           end-if.

      *     compute como-prezzo = reva-netto + 
      *                           reva-imp-cons      

           if reva-netto = zero
      *        move space  to r-stof-prz-netto
              move "OMAGGIO"          to r-stof-prz-netto
           else
      *        move reva-netto(12:2)  to como-xx
      *        if como-xx not = "00"
      *           move reva-netto  to como-pic-96v94
      *           move como-pic-96v94(2:) to r-stof-prz-net
      *        else
                 move reva-netto  to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-net
      *        end-if
              call "C$JUSTIFY" using r-stof-prz-net, "R"
              move "€"                   to r-stof-prz-net-e

           end-if

           if reva-imp-cons = zero
              move space                 to r-stof-imp-consumo
           else
      *        move reva-imp-cons(7:2)    to como-xx
      *        if como-xx not = "00"
      *           move reva-imp-cons      to como-pic-94v94
      *           move como-pic-94v94     to r-stof-imp-cons
      *        else
                 move reva-imp-cons      to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cons
      *        end-if
              call "C$JUSTIFY" using r-stof-imp-cons, "R"
              move "€"                to r-stof-imp-cons-e
           end-if

           if reva-coubat = zero
              move space  to r-stof-imp-cou-cobat
           else
      *        move reva-coubat(7:2)  to como-xx
      *        if como-xx not = "00"
      *           move reva-coubat  to como-pic-94v94
      *           move como-pic-94v94     to r-stof-imp-cou
      *        else
                 move reva-coubat  to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cou
      *        end-if
              call "C$JUSTIFY" using r-stof-imp-cou, "R"
              move "€"                   to r-stof-imp-cou-e
           end-if

           if reva-add-pb = zero
              move space           to r-stof-add-piombo
           else
      *        move reva-add-pb(7:2)  to como-xx
      *        if como-xx not = "00"
      *           move reva-add-pb  to como-pic-94v94
      *           move como-pic-94v94     to r-stof-add-pb
      *        else
                 move reva-add-pb  to como-pic-94v92
                 move como-pic-94v92     to r-stof-add-pb
      *        end-if
              call "C$JUSTIFY" using r-stof-add-pb, "R"
              move "€"                   to r-stof-add-pb-e
           end-if

           move space  to r-stof-costi-aggi.     

      *     move reva-netto   to como-prezzo
           compute como-prezzo = reva-netto    + 
                                 reva-imp-cons +
                                 reva-coubat   +
                                 reva-add-pb

           if como-prezzo = zero
              move space           to r-stof-prz-finale
           else
              move space           to r-stof-prz-finale
              move como-prezzo(12:2)  to como-xx
              if como-xx not = "00"
                 move como-prezzo  to como-pic-96v94
                 move como-pic-96v94     to r-stof-prz-fin
              else
                 move como-prezzo  to como-pic-96v92
                 move como-pic-96v92     to r-stof-prz-fin
              end-if
              call "C$JUSTIFY" using r-stof-prz-fin, "R"
              move "€"                   to r-stof-prz-fin-e
           end-if.

           compute como-prezzo = como-prezzo * reva-qta
           if como-prezzo = zero
              move space           to r-stof-prz-tot-finale
           else
      *        move como-prezzo     to como-pic-97v92
      *        move como-pic-97v92  to r-stof-prz-tot-finale(1:11)
      *        move "€"             to r-stof-prz-tot-finale(12:1)
                 add 0,0005              to como-prezzo
                 move como-prezzo        to como-prezzo-3dec
                 add 0,005               to como-prezzo-3dec
                 move como-prezzo-3dec   to como-prezzo-2dec
                 move como-prezzo-2dec   to como-prezzo


                 move como-prezzo  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e

           end-if.


           compute totale = totale + como-prezzo

           move r-stof-riga to spl-riga-stampa

           set  spl-oggetto     to true.
           set  SPL-linea       to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           move spl-riga-fine   to spl-riga
           add 0,4              to spl-riga-fine
           perform RIGHE-VERTICALI

      *    riga orizzontale
           move spl-riga-fine   to spl-riga
           move spl-riga to spl-riga-fine
           move 0,1       to spl-colonna.
           move 28,5      to spl-colonna-fine.
           call "spooler"       using spooler-link

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.   
           subtract 0,3      from spl-riga.
           call "spooler" using spooler-link.

      *     move reva-chiave-ordf   to rof-chiave
      *     read rordforn
      *        invalid
      *           continue
      *     end-read
           perform varying idx from 1 by 1 until idx > 50
              if rof-cod-iva = cod-iva(idx)
                 add como-prezzo   to imp-iva(idx)
                 exit perform
              else
                 if cod-iva(idx) = space
                    move rof-cod-iva  to cod-iva(idx)
                    move como-prezzo  to imp-iva(idx)
                    exit perform
                 end-if
              end-if
           end-perform.   

           initialize r-stof-riga

           move reva-qta              to como-pic-z7
           move como-pic-z7           to r-stof-qta
           move r-stof-riga to spl-riga-stampa
           move arial6b       to spl-hfont.
           move 1            to spl-tipo-colonna.

           call "spooler" using spooler-link.

      ***---
       APRI-STAMPA.
           if st-bozze-selprint-stampante = space
              call   "selprint" using selprint-linkage
              cancel "selprint"
           else
              move st-bozze-selprint-linkage   to selprint-linkage
           end-if.

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      
              set spl-horizontal      to true
              set spl-apertura        to true
              move 1                  to spl-margine-sinistro
              move 1                  to spl-margine-destro
              move 1                  to spl-margine-inf
              move "Stampa Bozze Evasione"   
                                      to spl-nome-job
              call "spooler"        using spooler-link
           else
              set spl-sta-annu to true
           end-if.
      
           if spl-sta-annu 
              cancel "spooler"
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move arial6 to spl-hfont
              call "spooler" using spooler-link
           end-if.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".

      ***---
       STAMPA-TESTA.
           add 1 to num-pagina

           move 0        to riga.
           perform FINCATURA.

           move teva-cod-forn to cli-codice
           set cli-tipo-F    to true
           read CLIENTI 
              invalid
                 move space  to cli-ragsoc-1
           end-read

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           move 2,35          to spl-riga.
           move 2,1          to spl-colonna.
           move cli-ragsoc-1 to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           move cli-referente-ord   to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           move cli-tel-dir-ref-ord  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           move cli-mail-ref-ord  to spl-riga-stampa
           call "spooler" using spooler-link.

           move 2,35          to spl-riga.
           move 13,7          to spl-colonna.
      *     initialize spl-riga-stampa
      *     string teva-numero(6:3) delimited by size
      *            "-"              delimited by size
      *            teva-anno        delimited by size
      *            into spl-riga-stampa
           move num-ord   to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           initialize spl-riga-stampa
           string teva-data(7:2)  delimited by size
                  "/"                    delimited by size
                  teva-data(5:2)  delimited by size
                  "/"                    delimited by size
                  teva-data(1:4)  delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3                    to spl-riga.
           move teva-utente-creazione  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3                    to spl-riga.
      *     move teva-fax  to spl-riga-stampa
           accept spl-riga-stampa from environment "ORDINI_FORN_TEL_DIR"
           call "spooler" using spooler-link.

           move 2,35          to spl-riga.
           move 20,6          to spl-colonna.


           add 0,6           to spl-riga.

           move cli-pag   to tblpa-codice2
           move "PA"       to tblpa-codice1
           read tcodpag
              invalid
                 initialize tblpa-descrizione1
                            tblpa-descrizione2
           end-read
           initialize spl-riga-stampa
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  |tof-cod-pagamento  delimited by size
                   |" - "              delimited by size
                   tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into spl-riga-stampa
           call "spooler" using spooler-link.


           move mag-descrizione to spl-riga-stampa

           add 0,3           to spl-riga.

           call "spooler" using spooler-link.

           move 18,9  to spl-riga
           move 9,9   to spl-colonna
           move arial4       to spl-hfont.
           move "LA LUBEX S.p.A. NON SI ASSUME ALCUNA RESPONSABILITA' PE
      -         "R GLI ORDINI NON ACCETTATI E FIRMATI DALLA DIREZIONE"
                    to spl-riga-stampa
           call "spooler" using spooler-link.

           move arial6       to spl-hfont.
           move 27,3  to spl-colonna
           initialize spl-riga-stampa
           move num-pagina   to num-pag-ed
           string "PAGINA "  delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      *    posizionamento per partenza righe
           move 5,1 to spl-riga-fine.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
           set spl-stringa to true
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
           perform STAMPA-TESTA
           move 0 to riga.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.
           initialize wfont-data.
           move 14                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial14BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 20                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial20BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5B, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 7                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial7B, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 5                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial5, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial6, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial6b, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 4                  to wfont-size.
           move "Arial"   to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               arial4, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.


           set tutto-ok             to true.
           initialize wfont-data.
           move 6                   to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               font-note, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

      *
      ***---
       DISTRUGGI-FONT.
           Destroy Arial14BI.
           Destroy Arial20BI.
           Destroy arial5B.
           Destroy arial7B.
           Destroy arial5.
           Destroy arial6.
           Destroy arial6b.
           Destroy arial4.
      *
      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.
      
           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.
      
           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.
      
           inspect messaggio replacing trailing SPACE by LOW-VALUE.
      
           display message messaggio.
      
      ***---
       CLOSE-FILES.
           close teva 
           close reva  
           close articoli 
           close clienti.
           close timballi.
           close timbalqta.
           close progmag.
           close tmagaz.
           close tcaumag.
           close tcodpag.
           close tivaese.
           close rordforn.
           close rlistini
           close tordforn.

      ***---
       EXIT-PGM.
           destroy Arial14BI 
                   Arial20BI 
                   arial5b
                   arial5
                   arial6
                   arial6b
           destroy logo-handle.

           goback.

      ***---  
       SCRIVI.
           call "spooler" using spooler-link.

      ***---
       FINCATURA.
      *    logo 
           move logo-handle to spl-hbitmap
           set  spl-bitmap  to true
           move 2,0 to spl-riga
           move 6,5 to spl-colonna
           move 1,2 to spl-bitmap-height
           move 2,8 to spl-bitmap-width
           call "spooler" using spooler-link.

      *    dati Lubex
           perform DATI-LUBEX.

      *    riga Ordine
           set spl-stringa       to true.
           move Arial14BI to spl-hfont.
           move 1,4              to spl-riga.
           move 17,2              to spl-colonna.
      *     move "BOZZA D'EVASIONE"  to spl-riga-stampa
           move teva-numero  to como-pic-z8
           call "C$JUSTIFY"  using como-pic-z8, "L"
           inspect como-pic-z8 replacing trailing space by low-value
           initialize spl-riga-stampa
           string "BOZZA D'EVASIONE " delimited by size
                  como-pic-z8         delimited by low-value
                  " - "               delimited by size
                  teva-anno           delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.


           set spl-nero   to true.
           move 20                 to spl-pen-with.
           move 0,6                to spl-colonna.
           move 16,5               to spl-colonna-fine.
           move 1,8   to spl-riga 
                         spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      *     move 22,8                to spl-colonna.
           evaluate true
           when teva-numero < 100
                move 24,8                to spl-colonna
           when teva-numero < 1000
                move 25,1                to spl-colonna
           when teva-numero < 10000
                move 25,4                to spl-colonna
           when other
                move 25,8                to spl-colonna
           end-evaluate
           
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 2,2   to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,6                to spl-colonna.
           move 28,5               to spl-colonna-fine.
           call "spooler"       using spooler-link.


           add 14,8   to spl-riga 
           move spl-riga  to spl-riga-fine.
           call "spooler"       using spooler-link.

           perform RIQUARDO-FORNITORE.
           perform RIQUADRO-ORDINE.
           perform RIQUADRO-CONSEGNA.

           perform GRIGLIA-RIGHE.

      ***---
       DATI-LUBEX.
           move zero   to spl-tipo-colonna
           set spl-stringa       to true.
           move arial5B  to spl-hfont.
           move 0,1              to spl-riga.
           move 3,4              to spl-colonna.
           move "LUBEX S.p.a."  to spl-riga-stampa
           call "spooler" using spooler-link.

           set spl-stringa       to true.
           move arial5  to spl-hfont.
           add 0,2              to spl-riga.
           move "Via G. Di Vittorio, 13/15"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "20090 VIMODRONE"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move "(MILANO - ITALY)"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,4              to spl-riga.
           move ">> Tel:  02 26 51 551"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,2              to spl-riga.
           move ">> Fax: 02 26 515 549"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 4,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           move "Capitale Sociale € 2.180.000 i.v."  to spl-riga-stampa
           call "spooler" using spooler-link.
           add 0,2              to spl-riga.
           move "P.IVA:   IT00785630963"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 3,1  to spl-colonna
           move 0,1              to spl-riga.

           add 0,2              to spl-riga.
           add 0,2              to spl-riga.

           initialize spl-riga-stampa
           string "Vimodrone, " delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " "              delimited by size
                  como-ora(1:2)    delimited by size
                  "."              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUARDO-FORNITORE.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 0,9                to spl-colonna.
           add 10,7 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,1        to spl-colonna
           subtract 0,6   from spl-colonna-fine

           add 0,5        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,15      from spl-riga.
           subtract 0,9       from spl-colonna.
           move "Spett.le:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           add 0,4           to spl-colonna.
           move "Sig.:"      to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           add 0,1           to spl-colonna.
           move "Tel:"       to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           subtract 0,1      from spl-colonna.
           move "Mail:"      to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-ORDINE.
           move 8     to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 11,8                to spl-colonna.
           add 6,7 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.

           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,8        to spl-colonna
           subtract 0,5   from spl-colonna-fine

           add 0,5        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.
           add 0,3        to spl-riga
           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.
           move arial6       to spl-hfont.
           subtract 1,1      from spl-riga.
           subtract 0,95       from spl-colonna.
           move "Ordini n°:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           subtract 0,25        from  spl-colonna.
           move "Data Bozza:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3           to spl-riga.
           add 0,3           to spl-colonna.
           move "Fatto da:"  to spl-riga-stampa
           call "spooler" using spooler-link.

           add 0,3              to spl-riga.
           subtract 0,2         from spl-colonna.
           move "Tel Diretto:"  to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       RIQUADRO-CONSEGNA.
           move 4         to spl-pen-with.
           move 2,1  to spl-riga.
           add 1,7  to spl-riga giving spl-riga-fine

           move 18,7                to spl-colonna.
           add 8,5 to spl-colonna giving spl-colonna-fine.

           set  spl-oggetto        to true.

           set  SPL-linea to true.
           add 1,8        to spl-colonna
           subtract 0,9   from spl-colonna-fine

           add 1,1        to spl-riga

           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           add 0,3        to spl-riga

           move spl-riga  to spl-riga-fine
           call "spooler"         using spooler-link.

           set spl-stringa   to true.

           move arial6       to spl-hfont.

           subtract 0,85      from spl-riga.
           add 0,3              to spl-riga.
           subtract 1,25        from  spl-colonna.
           move "Pagamento:"    to spl-riga-stampa
           call "spooler" using spooler-link.


      *     subtract 1,15      from spl-riga.
           add 0,05          to spl-colonna.
           move "Magazzino:"  to spl-riga-stampa
           add 0,3              to spl-riga.
           call "spooler" using spooler-link.

      ***---
       GRIGLIA-RIGHE.
      *     set  spl-oggetto     to true.
      *     set  SPL-linea       to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *     
      **    righe orizzontali
      *     move 4         to spl-pen-with.
      *     move 4,5       to spl-riga 
      *     move spl-riga  to spl-riga-fine.
      *     move 0,1       to spl-colonna.
      *     move 28,0      to spl-colonna-fine.
      *     call "spooler"       using spooler-link.
      *
      *     add 0,6   to spl-riga 
      *     move spl-riga  to spl-riga-fine
      *     call "spooler"       using spooler-link
      *
      *     move 4,5 to spl-riga
      *
      *     perform RIGHE-VERTICALI
      *
      **    intestazioni colonne
      *
      *     set spl-stringa   to true.
      *     move arial5B       to spl-hfont.
      *
      *     move 4,55          to spl-riga.
      *     move 0,3          to spl-colonna.
      *     move "CODICE"     to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 0,35          to spl-colonna.
      *     move "LUBEX"     to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *
      *     move 4,7          to spl-riga.
      *     move 1,3          to spl-colonna.
      *     move "DESCRIZIONE PRODOTTO"     to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 8,70          to spl-colonna.
      *     move "PEZZI"      to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 9,75          to spl-colonna.
      *     move "PZ X LT/KG" to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *
      *     move 4,55          to spl-riga.
      *     move 11,20          to spl-colonna.
      *     move "TARIFFA"     to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 11,1          to spl-colonna.
      *     move "DOGANALE"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 12,60          to spl-colonna.
      *     move "PESO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 12,55          to spl-colonna.
      *     move "NETTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 13,65          to spl-colonna.
      *     move "TOTALE KG"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 13,85          to spl-colonna.
      *     move "(NETTI)"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 14,95          to spl-colonna.
      *     move "TOTALE KG"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 15,15          to spl-colonna.
      *     move "(U.T.F.)"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      **     move 4,55          to spl-riga.
      **     move 15,10          to spl-colonna.
      **     move "PREZZO"   to spl-riga-stampa
      **     call "spooler" using spooler-link.
      **
      **     move 4,8          to spl-riga.
      **     move 15,05          to spl-colonna.
      **     move "UNITARIO"   to spl-riga-stampa
      **     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 16,55          to spl-colonna.
      *     move "NETTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 18,10          to spl-colonna.
      *     move "IMPOSTA AL"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 18,3          to spl-colonna.
      *     move "PEZZO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 20,0          to spl-colonna.
      *     move "NETTO "   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 19,6          to spl-colonna.
      *     move "(COMPRESA IMP)"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,55          to spl-riga.
      *     move 21,45          to spl-colonna.
      *     move "CONTRIBUTO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.
      *     move 21,50          to spl-colonna.
      *     move "CONSORZIO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 23,2          to spl-colonna.
      *     move "ADD.PIOMBO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *
      *     move 4,55          to spl-riga.
      *     move 25,05          to spl-colonna.
      *     move "TOTALE"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,8          to spl-riga.            
      *     move 25,0          to spl-colonna.
      *     move "UNITARIO"   to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move 4,7          to spl-riga.
      *     move 26,35        to spl-colonna.
      *     move "IMPORTO TOTALE"   to spl-riga-stampa
      *     call "spooler" using spooler-link.

           set  spl-oggetto     to true.
           set  SPL-linea       to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.
           
      *    righe orizzontali
           move 4         to spl-pen-with.
           move 4,5       to spl-riga 
           move spl-riga  to spl-riga-fine.
           move 0,1       to spl-colonna.
           move 28,5      to spl-colonna-fine.
           call "spooler"       using spooler-link.

           add 0,6   to spl-riga 
           move spl-riga  to spl-riga-fine
           call "spooler"       using spooler-link

           move 4,5 to spl-riga

           perform RIGHE-VERTICALI

      *    intestazioni colonne

           set spl-stringa   to true.
           move arial5B      to spl-hfont.
           move 2            to spl-tipo-colonna

      *    riga alta per intestazioni doppie
           move 4,55          to spl-riga.
           initialize r-stof-riga-testa.
           move "COD."                   to rt-stof-art
      *     move "SALDO"                  to rt-stof-qta
           move "PZ X "                  to rt-stof-imb
           move "PESO"                   to rt-stof-peso-netto
           move "TOTALE KG"              to rt-stof-peso-tot-netto
           move "TOTALE KG"              to rt-stof-peso-tot-utf
           move "LISTINO"                to rt-stof-prz-listino
           move "IMPOSTA AL"             to rt-stof-imp-consumo
           move "CONTRIBUTO"             to rt-stof-imp-cou-cobat
           move "ADD."                   to rt-stof-add-piombo
           move "COSTI"                  to rt-stof-costi-aggi
           move "TOTALE"                 to rt-stof-prz-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.

      *    riga bassa per intestazioni doppie
           move 4,8          to spl-riga.
           initialize r-stof-riga-testa.
           move "LUBEX"                  to rt-stof-art
           move "LT/KG"                  to rt-stof-imb
           move "NETTO"                  to rt-stof-peso-netto
           move "(NETTI)"                to rt-stof-peso-tot-netto
           move "(U.T.F.)"               to rt-stof-peso-tot-utf
           move "RISERVATO"              to rt-stof-prz-listino 
           move "PEZZO"                  to rt-stof-imp-consumo
           move "CONSORZIO"              to rt-stof-imp-cou-cobat
           move "PIOMBO"                 to rt-stof-add-piombo
           move "AGGIUNTIVI"             to rt-stof-costi-aggi
           move "UNITARIO"               to rt-stof-prz-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.


      *    riga per intestazioni singole
           move 4,7          to spl-riga.
           initialize r-stof-riga-testa.
           move "DESCRIZIONE PRODOTTO"   to rt-stof-des
      *     move "CODICE FORNITORE"       to Rt-STOF-COD-FORN
           move "ORDINE"                 to Rt-STOF-COD-FORN
           move "DATA"                   to Rt-STOF-DATA
           move "PEZZI"                  to rt-stof-qta
           move "SC.%"                   to rt-stof-sconto-1
           move "SC.%"                   to rt-stof-sconto-2
           move "SC.%"                   to rt-stof-sconto-3
           move "SC.%"                   to rt-stof-sconto-4
           move "SC.%"                   to rt-stof-sconto-5
           move "NETTO"                  to rt-stof-prz-netto
           move "IMPORTO TOTALE"         to rt-stof-prz-tot-finale

           move r-stof-riga-testa        to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero                     to spl-tipo-colonna.


      ***---
       RIGHE-VERTICALI.
      **    righe verticali
      **    ultima riga
      *     move 28,0       to spl-colonna
      *                        spl-colonna-fine.
      *     call "spooler"       using spooler-link
      **    1 prima riga 
      *     move 0,1       to spl-colonna
      *                       spl-colonna-fine.
      *     call "spooler"       using spooler-link
      *
      **    2 riga codice articolo
      *     add 1,1 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    3 riga descrizione articolo
      *     add 7,2 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    4 riga pezzi
      *     add 1,1 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    riga imballo
      *     add 1,5 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    riga doganale
      *     add 1,2 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    riga peso netto
      *     add 1,3 to spl-colonna
      *
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    riga peso netto totale
      *     add 1,3 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      **    riga peso UTF totale
      *     add 1,3 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link
      *
      ***    riga listino
      **     add 1,3 to spl-colonna
      **     move spl-colonna  to spl-colonna-fine
      **     call "spooler"       using spooler-link
      *
      **    riga prz-uni
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *
      **    riga prz-uni
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *
      **    riga prz-uni
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *
      **    riga prz-uni
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *
      **    costi aggiuntivi
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *
      **    costi aggiuntivi
      *     add 1,7 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link.
      *    ultima riga
           move 28,5       to spl-colonna
                              spl-colonna-fine.
           call "spooler"       using spooler-link

      *    1 riga codice articolo
           move 0,1       to spl-colonna
                             spl-colonna-fine.
           call "spooler"       using spooler-link

      *    2 riga descrizione articolo
           |add 1 to spl-colonna
           add 0,7 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    3 riga pezzi 
           |add 5,05 to spl-colonna
           add 4,95 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

           |add 2,1 to spl-colonna
           add 0,82 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

           |add 2,1 to spl-colonna
           add 1,23 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      **    4 riga pezzi evasi
      *     add 0,8 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    5 riga imballo
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      **    6 riga doganale
      *     add 1,2 to spl-colonna
      *     move spl-colonna  to spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    7 riga peso netto
           add 1,1 to spl-colonna
           |add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    8 riga riga peso netto totale
           add 1 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    9 riga peso UTF totale
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    10 riga listino
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    11 riga sc 1
           add 1,2 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    12 riga sc 2
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    13 riga sc 3
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    14 riga sc 4
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    15 riga sc 5
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    16 riga prz-uni
           add 0,9 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link

      *    17 riga imposta
           add 1,2 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    18 riga contributo consorzio
           add 1,3 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    19 riga add piombo
           add 1,4 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    20 costi aggiuntivi
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    21 tot unitario
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      *    22 importo totale
           add 1,25 to spl-colonna
           move spl-colonna  to spl-colonna-fine
           call "spooler"       using spooler-link.

      ***---
       SCRIVI-PIEDE.
      *    calcolo il totale dell'IVA
           perform varying idx from 1 by 1 until idx > 10
              if cod-iva(idx) = space
                 exit perform
              end-if

              move "IV"         to tbliv-codice1
              move cod-iva(idx) to tbliv-codice2   
              read tivaese no lock 
                 invalid 
                    move zero   to TBLIV-PERCENTUALE
              end-read

              compute como-iva = 
                   ( ( imp-iva(idx) * tbliv-percentuale ) / 100 )

              add 0,00005          to como-iva
              move como-iva        to como-iva-4dec

      *        add 0,0005           to como-iva-4dec
      *        move como-iva-4dec   to como-iva-3dec
      *
      *        add 0,005            to como-iva-3dec
      *        move como-iva-3dec   to como-iva-2dec

              add como-iva-4dec to tot-iva
           end-perform.

           add 0,0005           to tot-iva
           move tot-iva         to como-iva-3dec

           add 0,005            to como-iva-3dec
           move como-iva-3dec   to como-iva-2dec

           move como-iva-2dec   to tot-iva


           perform SCRIVI-TOTALI.

           perform GRIGLIA-NOTE.


      ***---
       SCRIVI-TOTALI.
      *    guardo se ci sto con il piede nella stampa se no cambio pagina
           add 4 to riga giving pronostico
           if pronostico > maxrighe
              perform SALTO-PAGINA
           end-if.

           move spl-riga-fine   to spl-riga
           add 0,4              to spl-riga-fine

           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.
           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           add 0,1              to spl-riga

           set spl-stringa   to true.
           move zero         to spl-tipo-colonna
           move arial5       to spl-hfont.


           move  7,72          to spl-colonna
           move "TOT. PEZZI"   to spl-riga-stampa
           call "spooler" using spooler-link.


           |move  10,9          to spl-colonna
           move  10,85          to spl-colonna
           move "TOTALE KG"  to spl-riga-stampa
           call "spooler" using spooler-link.

           |move  12,2          to spl-colonna
           move  12,30          to spl-colonna
           move "KG U.T.F."  to spl-riga-stampa
           call "spooler" using spooler-link.

      *     move  16,0          to spl-colonna
           move  20,30          to spl-colonna
           move "TOT.IMPOSTA"  to spl-riga-stampa
           call "spooler" using spooler-link.

      *     move  18,5          to spl-colonna
           move  21,75         to spl-colonna
           move "TOT.CONS."  to spl-riga-stampa
           call "spooler" using spooler-link.

           move  23,00         to spl-colonna
           move "TOT.ADD.PIOMBO"  to spl-riga-stampa
           call "spooler" using spooler-link.

      *    rettangoli prima linea
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga


      *    totale pezzi
      *     move 7,95 to spl-colonna
           move 7,75 to spl-colonna
           add 0,9 to spl-colonna giving spl-colonna-fine
           call "spooler"       using spooler-link
      *     add 0,8 to spl-colonna 
      *     add 0,8 to spl-colonna giving spl-colonna-fine
      *     call "spooler"       using spooler-link

      *    peso netto
           |add 2,8  to spl-colonna
           add 3,0  to spl-colonna
           add 1,3  to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.
      
      *    peso U.T.F.
           set  SPL-BRUSH-LTGRAY   to true.
           add 1,3  to spl-colonna
           add 1,3  to spl-colonna-fine
           call "spooler"       using spooler-link.

           set  spl-brush-null  to true.

      *    Imposta
           |add 5,2  to spl-colonna
           add 8,2  to spl-colonna
      *     add 1,25  to spl-colonna giving  spl-colonna-fine
           add 1,3  to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link

      *    cou/cobat
           add 1,30   to spl-colonna
           add 1,40   to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.

      *    cou/cobat
           add 1,40   to spl-colonna
           add 1,25   to spl-colonna giving  spl-colonna-fine
           call "spooler"       using spooler-link.

      *    totale
           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

      **    totali prima linea
      *     initialize r-stof-riga.
      *
      *     move tot-qta         to como-pic-z5
      *     move como-pic-z5     to r-stof-qta
      *
      *     move tot-peso        to como-pic-96v93
      *     move como-pic-96v93  to r-stof-peso-tot-netto
      *     move tot-peso-utf    to como-pic-96v93
      *     move como-pic-96v93  to r-stof-peso-tot-utf
      *
      *     if tot-imposta = zero
      *        move space           to r-stof-imp-consumo
      *     else
      *        move tot-imposta   to como-pic-94v92
      *        move como-pic-94v92  to r-stof-imp-consumo(1:8)
      *        move "€"             to r-stof-imp-consumo(9:1)
      *     end-if
      *
      *     if tot-cou = zero
      *        move space           to r-stof-imp-cou-cobat
      *     else
      *        move tot-cou     to como-pic-94v92
      *        move como-pic-94v92  to r-stof-imp-cou-cobat(1:8)
      *        move "€"             to r-stof-imp-cou-cobat(9:1)
      *     end-if
      *
      *     if tot-piombo = zero
      *        move space           to r-stof-add-piombo
      *     else
      *        move tot-piombo     to como-pic-94v92
      *        move como-pic-94v92  to r-stof-add-piombo(1:8)
      *        move "€"             to r-stof-add-piombo(9:1)
      *     end-if
      *
      *
      **
      *     move totale          to como-pic-97v92
      *     move como-pic-97v92  to r-stof-prz-tot-finale(1:11)
      *     move "€"             to r-stof-prz-tot-finale(12:1)
      *
      *     set spl-stringa   to true.
      *     move 1            to spl-tipo-colonna
      *     move arial6       to spl-hfont.
      *     move 1            to spl-tipo-colonna
      *     add 0,1           to  spl-riga.
      **     subtract 0,3      from spl-riga.
      *     move r-stof-riga  to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move zero         to spl-tipo-colonna
      *     move "SUB TOTALE"     to spl-riga-stampa
      *     move 24,9         to spl-colonna
      *     call "spooler" using spooler-link.
      *
      **    spese di spedizione (sempre vuote)
      *
      *     set  spl-oggetto     to true.
      *     set  SPL-RETTANGOLO  to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *
      *     add 0,4              to spl-riga-fine
      *     subtract 0,4         from spl-riga-fine giving spl-riga
      *
      *     move 28  to spl-colonna-fine
      *     subtract 1,7   from spl-colonna-fine giving spl-colonna
      *     call "spooler"       using spooler-link.
      *
      *     set spl-stringa   to true.
      *     move arial6       to spl-hfont.
      *     add 0,1           to  spl-riga.
      *     move zero         to spl-tipo-colonna
      *     move "SPESE DI TRASPORTO"to spl-riga-stampa  
      *     move 23,9           to spl-colonna
      *     call "spooler" using spooler-link.
      *
      **    IVA
      *     set  spl-oggetto     to true.
      *     set  SPL-RETTANGOLO  to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *
      *     add 0,4              to spl-riga-fine
      *     subtract 0,4         from spl-riga-fine giving spl-riga
      *
      *     move 28  to spl-colonna-fine
      *     subtract 1,7   from spl-colonna-fine giving spl-colonna
      *     call "spooler"       using spooler-link.
      *
      *     initialize r-stof-riga.
      *
      *     if tot-iva = zero
      *        move space           to r-stof-prz-tot-finale
      *     else
      *        move tot-iva     to como-pic-97v92
      *        move como-pic-97v92  to r-stof-prz-tot-finale(1:11)
      *        move "€"             to r-stof-prz-tot-finale(12:1)
      *     end-if.
      *
      *
      *     set spl-stringa   to true.
      *     move 1            to spl-tipo-colonna
      *     move arial6       to spl-hfont.
      *     move 1            to spl-tipo-colonna
      *     add 0,1           to  spl-riga.
      **     subtract 0,3      from spl-riga.
      *     move r-stof-riga  to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move zero         to spl-tipo-colonna
      *     move "IVA"to spl-riga-stampa  
      *     move 25,9           to spl-colonna
      *     call "spooler" using spooler-link.
      *
      **    TOTALE FATTURA
      *     set  spl-oggetto     to true.
      *     set  SPL-RETTANGOLO  to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *
      *     add 0,4              to spl-riga-fine
      *     subtract 0,4         from spl-riga-fine giving spl-riga
      *
      *     move 28  to spl-colonna-fine
      *     subtract 1,7   from spl-colonna-fine giving spl-colonna
      *     call "spooler"       using spooler-link.
      *
      *     initialize r-stof-riga.
      *
      *     add tot-iva to totale
      *     move totale          to como-pic-97v92
      *     move como-pic-97v92  to r-stof-prz-tot-finale(1:11)
      *     move "€"             to r-stof-prz-tot-finale(12:1)
      *     set spl-stringa   to true.
      *     move 1            to spl-tipo-colonna
      *     move arial7B      to spl-hfont.
      *     move 1            to spl-tipo-colonna
      *     add 0,1           to  spl-riga.
      **     subtract 0,3      from spl-riga.
      *     move r-stof-riga  to spl-riga-stampa
      *     call "spooler" using spooler-link.
      *
      *     move zero         to spl-tipo-colonna
      *     move "TOTALE FATTURA"to spl-riga-stampa  
      *     move 24,05           to spl-colonna
      *     call "spooler" using spooler-link.
      *    totali prima linea
           initialize r-stof-riga.

           move space           to r-stof-qta

           move tot-peso              to como-pic-96v93
           move como-pic-96v93(2:11)  to r-stof-peso-tot-netto
           move tot-peso-utf          to como-pic-96v93
           move como-pic-96v93(2:11)  to r-stof-peso-tot-utf

           add 0,0005                 to tot-imposta
           move tot-imposta           to tot-imposta-3dec
           add 0,005                  to tot-imposta-3dec
           move tot-imposta-3dec      to tot-imposta-2dec
           move tot-imposta-2dec      to tot-imposta
           if tot-imposta = zero
              move space              to r-stof-imp-consumo
           else
              move tot-imposta(9:2)   to como-xx
              if como-xx not = "00"
                 move tot-imposta     to como-pic-96v94
                 move como-pic-96v94(2:12)  to r-stof-imp-consumo|(1:11)
      *           move como-pic-96v94  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
              else
                 move tot-imposta     to como-pic-96v92
                 |move como-pic-96v92(1:11)  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
                 move como-pic-96v92  to r-stof-imp-cons|r-stof-imp-consumo(1:10)
              end-if

              call "C$JUSTIFY" using r-stof-imp-cons, "R"
              move "€"                to r-stof-imp-cons-e
           end-if


           add 0,0005                 to tot-cou
           move tot-cou               to tot-cou-3dec
           add 0,005                  to tot-cou-3dec
           move tot-cou-3dec          to tot-cou-2dec
           move tot-cou-2dec          to tot-cou

           if tot-cou = zero
              move space  to r-stof-imp-cou-cobat
           else
              move tot-cou(9:2)  to como-xx
              if como-xx not = "00"
                 move tot-cou  to como-pic-94v94
                 move como-pic-94v94     to r-stof-imp-cou|r-stof-imp-cou-cobat(1:10)
              else
                 move tot-cou  to como-pic-94v92
                 move como-pic-94v92     to r-stof-imp-cou|r-stof-imp-cou-cobat(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-imp-cou, "R"
              move "€"                   to r-stof-imp-cou-e
           end-if

           if totale = zero
              move space           to r-stof-prz-tot-finale
           else
              move space  to r-stof-prz-tot-finale
              move totale(10:2)  to como-xx
              if como-xx not = "00"
                 move totale  to como-pic-97v94
                 move como-pic-97v94     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(1:13)
              else
                 move totale  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(2:12)
              end-if
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           add 0,0005                    to tot-piombo
           move tot-piombo               to tot-piombo-3dec
           add 0,005                     to tot-piombo-3dec
           move tot-piombo-3dec          to tot-piombo-2dec
           move tot-piombo-2dec          to tot-piombo

           if tot-piombo = zero
              move space           to r-stof-add-piombo
           else
              move tot-piombo(9:2)  to como-xx
              if como-xx not = "00"
                 move tot-piombo  to como-pic-96v94
                 move como-pic-96v94     to r-stof-add-pb|r-stof-add-piombo(1:10)
              else
                 move tot-piombo  to como-pic-96v92
                 move como-pic-96v92     to r-stof-add-pb|r-stof-add-piombo(3:8)
              end-if
              call "C$JUSTIFY" using r-stof-add-pb, "R"
              move "€"                   to r-stof-add-pb-e
           end-if

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.
           move r-stof-riga  to spl-riga-stampa
           call "spooler" using spooler-link.

           initialize r-stof-riga
           move tot-qta               to como-pic-z7
           move como-pic-z7           to r-stof-qta

           move r-stof-riga to spl-riga-stampa
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna.
           call "spooler" using spooler-link.

           move zero         to spl-tipo-colonna
           move "SUB TOTALE" to spl-riga-stampa
           move 25,3           to spl-colonna
           call "spooler" using spooler-link.

      **    spese di spedizione (sempre vuote)
      *     set  spl-oggetto     to true.
      *     set  SPL-RETTANGOLO  to true.
      *     set  spl-brush-null  to true.
      *     set  spl-nero        to true.
      *
      *     add 0,4              to spl-riga-fine
      *     subtract 0,4         from spl-riga-fine giving spl-riga
      *
      *     move 28  to spl-colonna-fine
      *     subtract 1,5   from spl-colonna-fine giving spl-colonna
      *     call "spooler"       using spooler-link.
      *
      *     set spl-stringa   to true.
      *     move arial6       to spl-hfont.
      *     add 0,1           to  spl-riga.
      *     move zero         to spl-tipo-colonna
      *     move "SPESE DI TRASPORTO"to spl-riga-stampa  
      *     move 24,1           to spl-colonna
      *     call "spooler" using spooler-link.

      *    IVA
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga

           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           initialize r-stof-riga.

           if tot-iva = zero
              move space           to r-stof-prz-tot-finale
           else
              move space  to r-stof-prz-tot-finale
      *        move tot-iva(09:2)  to como-xx
      *        if como-xx not = "00"
      *           move tot-iva  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-finale(1:13)
      *        else
                 move tot-iva  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
      *        end-if
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e
           end-if.

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial6       to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.
           move r-stof-riga  to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero         to spl-tipo-colonna
           move "IVA"to spl-riga-stampa  
           move 26,3           to spl-colonna
           call "spooler" using spooler-link.

      *    TOTALE FATTURA
           set  spl-oggetto     to true.
           set  SPL-RETTANGOLO  to true.
           set  spl-brush-null  to true.
           set  spl-nero        to true.

           add 0,4              to spl-riga-fine
           subtract 0,4         from spl-riga-fine giving spl-riga

           move 28,5  to spl-colonna-fine
           subtract 1,8   from spl-colonna-fine giving spl-colonna
           call "spooler"       using spooler-link.

           initialize r-stof-riga.

           add tot-iva to totale
      *     move totale          to como-pic-97v92
      *     move como-pic-97v92  to r-stof-prz-tot-finale(1:13)
      *     move "€"             to r-stof-prz-tot-finale(14:1)

              move space  to r-stof-prz-tot-finale
      *        move totale(10:2)  to como-xx
      *        if como-xx not = "00"
      *           move totale  to como-pic-97v94
      *           move como-pic-97v94     to r-stof-prz-tot-finale(1:13)
      *        else
                 move totale  to como-pic-97v92
                 move como-pic-97v92     to r-stof-prz-tot-fin|r-stof-prz-tot-finale(3:12)
      *        end-if
              call "C$Justify" using r-stof-prz-tot-fin, "R"
              move "€"                   to r-stof-prz-tot-fin-e

           set spl-stringa   to true.
           move 1            to spl-tipo-colonna
           move arial7B      to spl-hfont.
           move 1            to spl-tipo-colonna
           add 0,1           to  spl-riga.
      *     subtract 0,3      from spl-riga.
           move r-stof-riga        to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero               to spl-tipo-colonna
           move "TOTALE FATTURA"   to spl-riga-stampa  
           move 24,45              to spl-colonna
           call "spooler" using spooler-link.


      ***---
       GRIGLIA-NOTE.
           if riga > maxrighe - 10
              perform SALTO-PAGINA
           end-if.

           move zero   to spl-tipo-colonna
           move 8     to spl-pen-with.
           move 15,4  to spl-riga.
           add 3,2  to spl-riga giving spl-riga-fine
      
           move 0,6                to spl-colonna.
           add 20,3 to spl-colonna giving spl-colonna-fine.
      
           set  spl-oggetto        to true.
           set  SPL-RETTANGOLO-ROUND  to true.
           set  spl-brush-null     to true.
           set  spl-nero           to true.
           call "spooler"         using spooler-link.
      
           set  SPL-linea to true.
           move 4         to spl-pen-with.
           add 1,1        to spl-colonna
           subtract 0,1   from spl-colonna-fine
      
      *    righe interne
           add 0,3        to spl-riga
           perform 9 times
              add 0,3        to spl-riga
              move spl-riga  to spl-riga-fine
              call "spooler"         using spooler-link
           end-perform.

           set spl-stringa   to true.
           |move arial6       to spl-hfont.
           move font-note     to spl-hfont.
           subtract 2,65      from spl-riga.
           subtract 0,6      from spl-colonna.
           move "Note:"      to spl-riga-stampa
           call "spooler" using spooler-link.

           subtract 0,3      from spl-riga.
           add 0,6           to spl-colonna

      *    scrivo le 3 note fisse
           add 0,3  to spl-riga
           move "Il presente ordine si considera da Voi accettato se non
      -         " contestato entro 3 giorni. CONAI compreso ove dovuto."
                                                  to spl-riga-stampa
           call "spooler" using spooler-link

           add 0,3  to spl-riga
           move "PRENOTAZIONI: LUBEX 02-26515539, C/DEPOSITO MTN 0371-69
      -         "7620, C/DEPOSITO SLI 0143-677767 - lo scarico non preno
      -         "tato verrà respinto"
                                                  to spl-riga-stampa
           call "spooler" using spooler-link

           add 0,3  to spl-riga
           move "I BANCALI RICEVUTI SU CUI E' POSTA LA MERCE VERRANNO CO
      -         "NSIDERATI A PERDERE"             to spl-riga-stampa
           call "spooler" using spooler-link.
      *

           if ordine-singolo
              move como-tof-chiave   to nof-chiave-ordine
              move low-value         to nof-num-nota
              start nordforn key >= nof-chiave
                    invalid continue
                not invalid
                    perform 6 times
                       read nordforn next at end exit perform end-read
                       if como-tof-chiave not = nof-chiave-ordine
                          exit perform
                       end-if
                       add 0,3  to spl-riga
                       move nof-nota  to spl-riga-stampa
                       call "spooler" using spooler-link
                    end-perform
              end-start
           end-if.

      ***---
       VALORIZZA-CORRELATI.
           move reva-chiave-ordf   to rof-chiave
           read rordforn no lock
                invalid move 0 to rof-cod-listino
           end-read
           move rof-cod-listino to rlis-codice
           move reva-articolo   to rlis-articolo

           move rof-cod-listino to rlis-codice
           read rlistini
                invalid move space  to rlis-art-forn
           end-read.
      
      ***---
      *    valorizzo l'elenco degli ordini correlati
       VALORIZZA-ORDINI.
           set ordine-singolo   to true
           initialize elenco-ordini.
           move teva-chiave to reva-chiave
           start reva key >= reva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read reva next at end exit perform end-read
      
                    if reva-anno   not = teva-anno   or
                       reva-numero not = teva-numero
                       exit perform
                    end-if

                    perform varying cont from 1 by 1 
                              until cont > 78-max-ordini
                       if el-ordini(cont) = reva-numero-ordf
                          exit perform
                       else
                          if el-ordini(cont) = 0
                             move reva-numero-ordf   to el-ordini(cont)
                             move reva-chiave-testa-ordf   
                                                     to como-tof-chiave
                             if cont > 1
                                set ordine-singolo   to false
                             end-if
                             exit perform
                          end-if
                       end-if
                    end-perform
                 end-perform
           end-start.

      *    ordino gli ordini
           perform varying cont from 1 by 1 
                     until cont > 78-max-ordini
              if el-ordini(cont) = 0
                 exit perform
              end-if
              perform varying cont2 from cont by 1 
                        until cont2 > 78-max-ordini
                 if el-ordini(cont2) = 0
                    exit perform
                 end-if
                 if el-ordini(cont2) < el-ordini(cont)
                    move el-ordini(cont2)   to como-numero
                    move el-ordini(cont)    to el-ordini(cont2)
                    move como-numero        to el-ordini(cont)
                 end-if
              end-perform
           end-perform.

      *    edito la stringa dei numeri
      *     initialize num-ord
           perform varying cont from 1 by 1 
                     until cont > 78-max-ordini
              if el-ordini(cont) = zero
                 exit perform
              end-if
              move el-ordini(cont) to num-ord-ed
              call "C$JUSTIFY" using num-ord-ed, "L"

              if cont = 1
                 move num-ord-ed   to num-ord

              else
                 inspect num-ord replacing trailing space by low-value
                 string num-ord    delimited by low-value
                        " - "      delimited by size
                        num-ord-ed delimited by size
                        into num-ord
                 inspect num-ord replacing trailing low-value by space
              end-if                 
           end-perform.
