       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-art-det.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "tnomen.sl".
           copy "tcla1art.sl".
           copy "tsetmerc.sl".
           copy "tudm.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "tivaese.sl".
           copy "progmag.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "tmagaz.sl".
           copy "prodener.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "tnomen.fd".
           copy "tcla1art.fd".
           copy "tsetmerc.fd".
           copy "tudm.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "tivaese.fd".
           copy "progmag.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "tmagaz.fd".
           copy "prodener.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       77  status-tmarche        pic xx.
       77  status-articoli       pic xx.
       77  status-tnomen         pic xx.
       77  status-tcla1art       pic xx.
       77  status-tsetmerc       pic xx.
       77  status-tudm           pic xx.
       77  status-timballi       pic xx.
       77  status-timbalqta      pic xx.
       77  status-tivaese        pic xx.
       77  status-progmag        pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.
       77  status-tmagaz         pic xx.
       77  status-prodener       pic xx.

      * COSTANTI
       78  titolo                value "GESLUX - Dettaglio articolo".
       78  78-MaxRows            value 34.
       78  78-distanza           value 0,2.
       78  78-NoteCrtRiga        value 87.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).
       77  r-peso-utf            pic zz.zz9,999.

       01  r-codice.
         05 filler              pic x(9) value "Codice".
         05 r-art-codice        pic z(6).
         05 filler              pic x.
         05 rc-art-descrizione  pic x(30).

       01  r-collegato.
         05 filler              pic x(9) value "Collegato".
         05 r-art-coll          pic z(6).
         05 filler              pic x.
         05 rc-art-des-coll    pic x(30).

       01  r1.
         05 filler                     pic x(14)
                                       value "Descrizione".
         05 r-art-descrizione          pic x(35).
         05 filler                     pic x(8).
         05 filler                     pic x(24)
                                       value "Descrizione alternativa".
         05 r-art-descrizione-2        pic x(35).

       01  r2.
         05 filler                     pic x(19) 
                                       value "Sett. merceologico".
         05 r-art-settore-merceologico pic z(4).
         05 filler                     pic x.
         05 r-sme-descrizione          pic x(28).
         05 filler                     pic x(5).
         05 filler                     pic x(11) value "Codice IVA".
         05 r-art-codice-iva           pic x(3).
         05 filler                     pic x.
         05 r-tbliv-descrizione1       pic x(30).

       01  r3.
         05 filler                     pic x(19) value "Marca".
         05 r-art-marca-prodotto       pic z(4).
         05 filler                     pic x.
         05 r-mar-descrizione          pic x(28).
         05 filler                     pic x(5).
         05 r-mag-codice               pic x(3).
         05 filler                     pic x(2).
         05 r-mag-descrizione          pic x(30).
         05 filler                     pic x(5).
         05 r-tit-limite-scorta        pic x(12).
         05 r-limite-scorta            pic z(7) blank zero.

       01  r4.
         05 filler                     pic x(19) value "Classe 1".
         05 r-art-classe-1             pic zzz9.
         05 filler                     pic x.
         05 r-cl1-descrizione          pic x(28).
         05 filler                     pic x(5).
         05 filler                     pic x(15) value "Classe 2".
         05 r-art-classe-2             pic zz9.
         05 filler                     pic x(7).
         05 filler                     pic x(15) value "Classe 3".
         05 r-art-classe-3             pic zz9.
         05 filler                     pic x(7).
         05 filler                     pic x(16) value "Classe 4".
         05 r-art-classe-4             pic zz9.

       01  r5.
         05 filler                     pic x(19)
                                       value "Unità di misura".
         05 r-unita-di-misura          pic x(3).
         05 filler                     pic x(2).
         05 r-udm-descrizione          pic x(28).
         05 filler                     pic x(5).
         05 filler                     pic x(15) value "Imb standard".
         05 r-art-imballo-standard     pic x(3).
         05 filler                     pic x(3).
         05 r-imballo                  pic x(16).
         05 filler                     pic x(1).
         05 filler                     pic x(11) value "Udm x imb".
         05 r-art-udm-imballo          pic x(5).
         05 filler                     pic x(2).
         05 filler                     pic x(6) value "Scorta".
         05 r-art-scorta               pic ---.--9.

       01  r6.
         05 filler                     pic x(19) value "Cod. Doganale".
         05 r-art-cod-doganale         pic 9(4).
         05 filler                     pic x.
         05 r-nom-descrizione          pic x(28).
         05 filler                     pic x(5).
         05 filler                     pic x(15) value "Trattam. UTF".
         05 r-tratt-utf                pic x(12).
         05 filler                     pic x(3).
         05 filler                     pic x(3) value "UTF".
         05 r-art-peso-utf             pic zz.zz9,999.
         05 filler                     pic x(7).
         05 filler                     pic x(9) value "NON UTF".
         05 r-art-peso-non-utf         pic zz.zz9,999.

       01  r6-bis.
         05 filler                  pic x(19) value "Prod. Energetico".
         05 r-art-prod-ener            pic x(10).
         05 filler                     pic x.
         05 r-prod-ener-descrizione    pic x(52).
         05 filler                     pic x(5).
         05 filler                    pic x(16) value "Tipo Stoccaggio".
         05 r-tipo-stoc                pic x(15).

       01  r7.
         05 filler                     pic x(19) value "Sogg. imposte".
         05 r-sogg-imposte             pic x(4).
         05 filler                     pic x(34).
         05 filler                     pic x(11) value "% Imposte".
         05 r-art-perce-imposte        pic zz9,999.
         05 filler                     pic x(8).
         05 filler                     pic x(10) value "% C.O.U.".
         05 r-art-perce-cou            pic zz9,999.
         05 filler                     pic x(7).
         05 label-peso-reale           pic x(9).
         05 r-art-peso-reale           pic zz.zz9,999.

       01  r8.
         05 filler                      pic x(19) value "Sogg. COBAT".
         05 r-sogg-cobat                pic x(4).
         05 filler                      pic x(34).
         05 filler                      pic x(15) value "Amperaggio".
         05 r-art-amperaggio            pic zz9.
         05 filler                      pic x(5).
         05 filler                      pic x(18) value "Tipo COBAT".
         05 r-tipo-cobat                pic x(4).
         05 filler                      pic x(5).
         05 filler                      pic x(5) value "Litri".
         05 filler                      pic x(5).
         05 r-litri                     pic z.zz9,999.

       01  r9.
         05 filler                      pic x(23)
                                        value "Prezzo acquisto".
         05 r-art-prezzo-acquisto       pic zz.zz9,99.
         05 filler                      pic x(25).
         05 filler                      pic x(13)
                                        value "% Sc. acq.".
         05 r-art-perce-sconto-acquisto pic z9,99.
         05 filler                      pic x(5).
         05 filler                      pic x(12) value "Prz Vendita".
         05 r-art-prezzo-vendita        pic zz.zz9,99.
         05 filler                      pic x(6).
         05 filler                      pic x(14) value "% Sc.agente".
         05 r-art-perce-sconto-agente   pic z9,99.

       01  r10.  
         05 filler                      pic x(21)
                                        value "Prz Banco (IVA Comp.)".
         05 filler                      pic xx.
         05 r-prz-banco                 pic zz.zz9,99.
         05 filler                      pic x(25).
         05 filler                      pic x(22) 
                                        value "Pz Mn Ven (IVA Comp.)".
         05 filler                      pic x.
         05 r-prz-min-vend              pic zz.zz9,99.
         05 filler                      pic x(4).
         05 filler                      pic x(11)
                                        value "Cod Art Frn".
         05 filler                      pic x(2).
         05 r-cod-art-frn               pic x(15).

       01  r11.
         05 filler                      pic x(19) value "Codice EAN 1".
         05 r-art-codice-ean-1          pic x(13).
         05 filler                      pic x(25).
         05 filler                      pic x(19) value "Codice EAN 2".
         05 r-art-codice-ean-2          pic x(13).

       01  r12.
         05 filler                      pic x(19) value "Codice EAN 3".
         05 r-art-codice-ean-3          pic x(13).
         05 filler                      pic x(25).
         05 filler                      pic x(19) value "Codice EAN 4".
         05 r-art-codice-ean-4          pic x(13).

       01  r13.
         05 filler                      pic x(19) value "Codice EAN 5".
         05 r-art-codice-ean-5          pic x(13).
         05 filler                      pic x(25).
      *   05 filler                      pic x(17) value"Cod. Fornitore".
      *   05 r-art-cod-fornitore         pic zzzz9.
      *   05 filler                      pic x(2).
      *   05 r-cli-ragsoc-1              pic x(30).

       01  r13-bis.
         05 filler                      pic x(19) value"Cod. Fornitore".
         05 r-art-cod-fornitore         pic zzzz9.
         05 filler                      pic x(1).
         05 r-cli-ragsoc-1              pic x(31).
         05 filler                      pic x(1).
         05 filler                      pic x(17) value"Cod. Destino".
         05 r-art-cod-dest              pic zzzz9.
         05 filler                      pic x(1).
         05 r-dest-ragsoc-1             pic x(30).

       01  r14.
         05 filler                      pic x(23) 
                                        value "Altezza bancale cm".
         05 r-art-altezza               pic zz.zz9,99.
         05 filler                      pic x(2).
         05 filler                      pic x(18) 
                                        value "Largh. bancale cm".
         05 r-art-larghezza             pic zz.zz9,99.
         05 filler                      pic x(2).
         05 filler                      pic x(17) 
                                        value "Prof. bancale cm".
         05 r-art-profondita            pic zz.zz9,99.
         05 filler                      pic x(2).
         05 filler                      pic x(3) value "ALA".
         05 filler                      pic x. 
         05 r-ala                       pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3). |value "ALA".
         05 filler                      pic x.
         05 r-gruppi                    pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3) value "C/E".
         05 filler                      pic x(1).
         05 r-specialist                pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3). |value "CEP".
         05 filler                      pic x.
         05 r-cepsa                     pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3). |value "WEB".
         05 filler                      pic x(1).
         05 r-web                       pic x.                
         05 filler                      pic x(1).
         05 filler                      pic x(3) value "GDA".
         05 filler                      pic x(1).
         05 r-gda                       pic x.

       01  r14-bis.
         05 filler                      pic x(23) 
                                        value "Altezza pezzo cm".
         05 r-art-altezza-pz            pic zz.zz9,99.
         05 filler                      pic x(2).
         05 filler                      pic x(18) 
                                        value "Largh. pezzo cm".
         05 r-art-larghezza-pz          pic zz.zz9,99.
         05 filler                      pic x(2).
         05 filler                      pic x(17) 
                                        value "Prof. pezzo cm".
         05 r-art-profondita-prz        pic zz.zz9,99. 
         05 filler                      pic x(2).
         05 filler                      pic x(6) value "GDS/DO".
         05 filler                      pic x(1).
         05 r-do                        pic x.

       01  r15.
         05 filler                      pic x(23)
                                        value "Qta bancale EPAL PZ".
         05 r-art-qta-epal              pic z.zzz.zz9.
         05 filler                      pic x(2).
         05 filler                      pic x(18)
                                        value "Qta bancale STD PZ".
         05 r-art-qta-std               pic z.zzz.zz9.
         05 filler                      pic x(30).            
         05 filler                      pic x(3). |value "Age".
         05 filler                      pic x.
         05 r-agenti                    pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3). |value "Est".
         05 filler                      pic x.
         05 r-estero                    pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3) value "   ".
         05 filler                      pic x.
         05 r-gds                       pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3). |value "SPI".
         05 filler                      pic x(1).
         05 r-spi                       pic x.
         05 filler                      pic x(1).
         05 filler                      pic x(3) value "TRA".
         05 filler                      pic x(1).
         05 r-at                        pic x.                
         05 filler                      pic x(1).
         05 filler                      pic x(3) value "TEX".
         05 filler                      pic x(1).
         05 r-texaco                    pic x.

       01  r15-bis.
         05 filler                      pic x(23)
                                        value "Confezioni per Cartone".
         05 r-art-conf-cart             pic z.zzz.zz9.
         05 filler                      pic x(2).
         05 filler                      pic x(18)
                                        value "Cartoni per UDC".
         05 r-art-UDC                   pic z.zzz.zz9.
         05 filler                      pic x(2).
         05 filler                      pic x(10) 
                                        value "Peric. ADR".
         05 filler                      pic x.
         05 r-adr                       pic x(15).              

       01  r16.
         05 filler                      pic x(21) value "Foto".
         05 r-art-foto                  pic x(100).    

       01  r17.
         05 filler                      pic x(21) value"Scheda tecnica".
         05 r-art-scheda-tecnica        pic x(90).
         05 filler                      pic x(1).
         05 filler                      pic x(6) value "Stato".
         05 r-stato                     pic x(9).

       01  r17-bis.
         05 filler                      pic x(21) 
                                        value"Scheda tossicologica".
         05 r-art-tossi                 pic x(100).    

       01  r17-tris.
         05 filler                      pic x(21) 
                                        value"Logo Brand".
         05 r-art-brand                 pic x(100).    

       01  r18.
         05 filler                      pic x(21)
                                        value "Note aggiuntive".
         05 r-art-note-agg              pic x(100).

       01  r19.
         05 r19filler                   pic x(21)
                                        value "Note".
         05 r-art-note                  pic x(78-NoteCrtRiga).


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
       77  imballi-ed            pic zzz9.

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  CourierNew10B         handle of font.
       77  Verdana16B            handle of font.
       77  Verdana20B            handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  idx                   pic 999.
       77  tot-char              pic 999.
       77  idx-note              pic 999.
       77  num-riga              pic 999.

       LINKAGE SECTION.
       77  link-articolo         pic 9(6).

      ******************************************************************
       PROCEDURE DIVISION using link-articolo.

       DECLARATIVES.
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
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-destinif
           when "39"
                set errori to true
                display message "File [DESTINIF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINIF] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINIF] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TSETMERC-ERR SECTION.
           use after error procedure on tsetmerc.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tsetmerc
           when "39"
                set errori to true
                display message "File [TSETMERC] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TSETMERC] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TSETMERC] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TUDM-ERR SECTION.
           use after error procedure on tudm.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tudm
           when "39"
                set errori to true
                display message "File [TUDM] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TUDM] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TUDM] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timballi
           when "39"
                set errori to true
                display message "File [TIMBALLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMBALLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMBALLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "39"
                set errori to true
                display message "File [TIMBALQTA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMBALQTA] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMBALQTA] inesistente"
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
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "39"
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TMARCHE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TNOMEN-ERR SECTION.
           use after error procedure on tnomen.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnomen
           when "39"
                set errori to true
                display message "File [TNOMEN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNOMEN] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TNOMEN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TCLA1ART-ERR SECTION.
           use after error procedure on tcla1art.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcla1art
           when "39"
                set errori to true
                display message "File [TCLA1ART] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCLA1ART] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TCLA1ART] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "39"
                set errori to true
                display message "File [TMAGAZ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMAGAZ] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TMAGAZ] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PRODENER-ERR SECTION.
           use after error procedure on prodener.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-prodener
           when "39"
                set errori to true
                display message "File [PRODENER] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PRODENER] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [PRODENER] inesistente"
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
      *     move 0,9  to passo.
           move 0,52  to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.


      ***---
       OPEN-FILES.
           open input articoli 
                      progmag 
                      tcla1art 
                      tnomen 
                      tmarche 
                      tmagaz
                      prodener
                      tudm 
                      tsetmerc 
                      tivaese  
                      timbalqta 
                      timballi 
                      clienti
                      destinif.

      ***---
       ELABORAZIONE.
           move link-articolo to art-codice.
           read articoli no lock 
                invalid set errori to true
           end-read.
           if tutto-ok
              move "IV"           to tbliv-codice1
              move art-codice-iva to tbliv-codice2
              read tivaese no lock invalid continue end-read

              set cli-tipo-F to true
              move art-cod-fornitore to cli-codice
                                        desf-codice
              read clienti no lock 
                 invalid 
                    continue 
              end-read
              move art-cod-desf-forn  to desf-prog
              read destinif no lock
                 invalid
                    continue
              end-read

              move art-settore-merceologico to sme-codice
              read tsetmerc no lock invalid continue end-read

              move art-imballo-standard to imq-codice
              read timbalqta no lock
                   invalid continue
               not invalid
                   move imq-tipo to imb-codice
                   read timballi no lock invalid continue end-read
                   move imq-qta-imb to imballi-ed
                   initialize r-imballo
                   call "C$JUSTIFY" using imballi-ed, "L"
                   inspect imb-descrizione replacing trailing
                                           spaces by low-value
                   string  imb-descrizione delimited by low-value
                           " da "          delimited by size
                           imballi-ed      delimited by spaces
                           into r-imballo
                   end-string
              end-read

              move art-unita-di-misura to udm-codice
              read tudm no lock invalid continue end-read

              move art-marca-prodotto to mar-codice
              read tmarche no lock invalid continue end-read

              move art-classe-1 to cl1-codice
              read tcla1art no lock invalid continue end-read
                                                             
              move art-cod-doganale to nom-codice
              read tnomen no lock invalid continue end-read

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

              move titolo               to spl-nome-job
              set  spl-apertura         to true
              set  spl-horizontal       to true
              set  WFDEVICE-WIN-PRINTER to true
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

              move 0,0  to save-riga 
              move 1,0  to spl-colonna
              move CourierNew10B   to spl-hfont

              move art-codice      to r-art-codice
              move art-descrizione to rc-art-descrizione
              move r-codice        to spl-riga-stampa
              perform SCRIVI

      *        move 1,55 to save-riga
              move 1,35 to save-riga
              perform STAMPA-LINEA

              move 0,9 to save-riga

              if art-collegato = 0
                 move spaces to rc-art-des-coll
              else
                 move art-collegato to art-codice
                 read articoli no lock invalid continue end-read
                 move art-descrizione to rc-art-des-coll
                 move link-articolo to art-codice
                 read articoli no lock
              end-if

              move art-collegato   to r-art-coll
              move r-collegato     to spl-riga-stampa
              perform SCRIVI

              move 1,5 to save-riga

              move art-descrizione   to r-art-descrizione
              move art-descrizione-2 to r-art-descrizione-2

              move r1 to spl-riga-stampa
              perform SCRIVI

              move 2,1 to save-riga

              move art-settore-merceologico 
              to r-art-settore-merceologico
              move sme-descrizione    to r-sme-descrizione
              move art-codice-iva     to r-art-codice-iva
              move tbliv-descrizione1 to r-tbliv-descrizione1
              move r2 to spl-riga-stampa
              perform SCRIVI

              move 2,7   to save-riga
                                  
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 3,15  to spl-riga
              move 4,25  to spl-riga-fine

              perform STAMPA-FRAME

              move art-marca-prodotto to r-art-marca-prodotto
              move mar-descrizione    to r-mar-descrizione

              move art-mag-std to r-mag-codice mag-codice
              read tmagaz no lock invalid continue end-read
              move mag-descrizione to r-mag-descrizione

              if art-limite-scorta not = 0
                 move "Mx Scorta pz"    to r-tit-limite-scorta
                 move art-limite-scorta to r-limite-scorta
              end-if

              move r3 to spl-riga-stampa
              perform SCRIVI

              move cl1-descrizione    to r-cl1-descrizione
              move art-classe-1       to r-art-classe-1
              move art-classe-2       to r-art-classe-2
              move art-classe-3       to r-art-classe-3
              move art-classe-4       to r-art-classe-4
              move r4 to spl-riga-stampa
              perform SCRIVI      

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 4,40  to spl-riga
              move 5,05  to spl-riga-fine

              perform STAMPA-FRAME

              move art-unita-di-misura  to r-unita-di-misura
              move udm-descrizione      to r-udm-descrizione
              move art-imballo-standard to r-art-imballo-standard
              move art-udm-imballo      to r-art-udm-imballo
              move art-scorta           to r-art-scorta
              move r5 to spl-riga-stampa
              perform SCRIVI      

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 5,15  to spl-riga
              move 7,25  to spl-riga-fine

              perform STAMPA-FRAME

              move art-cod-doganale(1:4) to r-art-cod-doganale
              move nom-descrizione       to r-nom-descrizione
              evaluate true
              when art-si-utf move "Soggetto"     to r-tratt-utf
              when art-misto  move "Misto"        to r-tratt-utf
              when art-no-utf move "Non Soggetto" to r-tratt-utf
              end-evaluate
              move art-peso-utf     to r-art-peso-utf
              move art-peso-non-utf to r-art-peso-non-utf
              move r6 to spl-riga-stampa
              perform SCRIVI

              move art-cod-prodener   to r-art-prod-ener
                                         pen-codice
              read prodener
                 invalid
                    initialize pen-dati
              end-read
              initialize r-prod-ener-descrizione
              string "CPA: "      delimited size
                     pen-cpa      delimited size
                     " - NC: "    delimited size
                     pen-nc       delimited size
                     " - TARIC: " delimited size
                     pen-taric    delimited size
                     " - DAC:"    delimited size
                     pen-dac      delimited size
                     into r-prod-ener-descrizione

              evaluate true
              when art-confezionato
                   move "Confezionato"   to r-tipo-stoc
              when art-sfuso
                   move "Sfuso"          to r-tipo-stoc
              end-evaluate

              move r6-bis to spl-riga-stampa
              perform SCRIVI

              if art-si-imposte
                 move "X" to r-sogg-imposte
              end-if
              move art-perce-imposte to r-art-perce-imposte
              move art-perce-cou     to r-art-perce-cou
              if art-peso-reale not = 0
                 move art-peso-reale to r-art-peso-reale
                 move "Kg Reale"     to label-peso-reale
              end-if
              move r7 to spl-riga-stampa
              perform SCRIVI

              if art-si-cobat
                 move "X" to r-sogg-cobat
                 if art-auto-cobat
                    move "AUTO" to r-tipo-cobat
                 else
                    move "MOTO" to r-tipo-cobat
                 end-if
              end-if
              move art-amperaggio   to r-art-amperaggio
              move art-litri        to r-litri
              move r8 to spl-riga-stampa
              perform SCRIVI      

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 7,4  to spl-riga
              move 8,5  to spl-riga-fine

              perform STAMPA-FRAME

              move art-prezzo-acquisto to r-art-prezzo-acquisto
              move art-perce-sconto-acquisto 
              to r-art-perce-sconto-acquisto
              move art-prezzo-vendita to r-art-prezzo-vendita
              move art-perce-sconto-agente
              to r-art-perce-sconto-agente
              move r9 to spl-riga-stampa
              perform SCRIVI
              
              move art-cod-art-frn  to r-cod-art-frn
              move art-prz-min-vend to r-prz-min-vend
              move art-prezzo-banco to r-prz-banco
              move r10 to spl-riga-stampa
              perform SCRIVI      

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 8,65   to spl-riga
              move 10,8   to spl-riga-fine

              perform STAMPA-FRAME
                                  
              if art-codice-ean-1 = 0
                 move "            0"  to r-art-codice-ean-1
              else
                 move art-codice-ean-1 to r-art-codice-ean-1
              end-if        

              if art-codice-ean-2 = 0
                 move "            0"  to r-art-codice-ean-2
              else
                 move art-codice-ean-2 to r-art-codice-ean-2
              end-if
              
              if art-limite-scorta not = 0
                 move "Mx Scorta pz"    to r-tit-limite-scorta
                 move art-limite-scorta to r-limite-scorta
              end-if

              move r11 to spl-riga-stampa
              perform SCRIVI

              if art-codice-ean-3 = 0
                 move "            0"  to r-art-codice-ean-3
              else
                 move art-codice-ean-3 to r-art-codice-ean-3
              end-if

              if art-codice-ean-4 = 0
                 move "            0"  to r-art-codice-ean-4
              else
                 move art-codice-ean-4 to r-art-codice-ean-4
              end-if
              move r12 to spl-riga-stampa
              perform SCRIVI
                    

              if art-codice-ean-5 = 0
                 move "            0"  to r-art-codice-ean-5
              else
                 move art-codice-ean-5 to r-art-codice-ean-5
              end-if
              move art-cod-fornitore   to r-art-cod-fornitore
              move cli-ragsoc-1        to r-cli-ragsoc-1
              move art-cod-desf-forn   to r-art-cod-dest
              move desf-ragsoc-1       to r-dest-ragsoc-1


              move r13 to spl-riga-stampa
              perform SCRIVI

              move r13-bis to spl-riga-stampa
              perform SCRIVI

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 20,0  to spl-colonna-fine

              move 10,95  to spl-riga
              move 11,95  to spl-riga-fine

              perform STAMPA-FRAME
                                  
              move 20,1   to spl-colonna
              move 27,9   to spl-colonna-fine

              move 10,95  to spl-riga
              move 13,2   to spl-riga-fine

              perform STAMPA-FRAME

              move art-altezza     to r-art-altezza
              move art-larghezza   to r-art-larghezza
              move art-profondita  to r-art-profondita
              if art-si-ala        move "X"    to r-ala
              else                 move spaces to r-ala
              end-if
      *****        if art-si-gruppi     move "X"    to r-gruppi
      *****        else                 move spaces to r-gruppi
      *****        end-if
              if art-si-specialist move "X"    to r-specialist
              else                 move spaces to r-specialist
              end-if
              if art-si-do         move "X"    to r-do
              else                 move spaces to r-do
              end-if
      *****        if art-web-si        move "X"    to r-web
      *****        else                 move spaces to r-web
      *****        end-if                                
              if art-si-gda     move "X"    to r-gda
              else              move spaces to r-gda
              end-if            

      *****        if art-SHARK-si   move "X"    to r-CEPSA
      *****        else              move spaces to r-CEPSA
      *****        end-if              

              move r14 to spl-riga-stampa
              perform SCRIVI

              move art-qta-epal to r-art-qta-epal
              move art-qta-std  to r-art-qta-std    
      *****        if art-si-agenti  move "X"    to r-agenti
      *****        else              move spaces to r-agenti
      *****        end-if
      *****        if art-si-estero  move "X"    to r-estero
      *****        else              move spaces to r-estero
      *****        end-if
      *****        if art-si-gds     move "X"    to r-gds
      *****        else              move spaces to r-gds
      *****        end-if
      *****        if art-SPI-si     move "X"    to r-spi
      *****        else              move spaces to r-spi
      *****        end-if
              if art-AT-si      move "X"    to r-at
              else              move spaces to r-at
              end-if   
              if art-TEXACO-si  move "X"    to r-TEXACO
              else              move spaces to r-TEXACO
              end-if
              move r15 to spl-riga-stampa
              perform SCRIVI

              add 78-distanza to save-riga
                                  
              move 0,8   to spl-colonna
              move 20,0  to spl-colonna-fine

              move 12,1   to spl-riga
              move 13,2   to spl-riga-fine

              perform STAMPA-FRAME

              move art-altezza-pz     to r-art-altezza-pz    
              move art-larghezza-pz   to r-art-larghezza-pz  
              move art-profondita-pz  to r-art-profondita-prz 

              move r14-bis to spl-riga-stampa
              perform SCRIVI

              move art-conf-cartone   to r-art-conf-cart
              move art-cartone-UDC    to r-art-UDC 
              move art-adr            to r-adr 

              move r15-bis to spl-riga-stampa
              perform SCRIVI

              add 78-distanza to save-riga

              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 13,35   to spl-riga
              move 16,05   to spl-riga-fine

              perform STAMPA-FRAME

              move art-foto to r-art-foto
              move r16 to spl-riga-stampa
              perform SCRIVI

              move art-scheda-tecnica to r-art-scheda-tecnica
              evaluate true
              when art-attivo    move "ATTIVO"    to r-stato
              when art-bloccato  move "BLOCCATO"  to r-stato
              when art-disattivo move "SOSPESO"   to r-stato
              end-evaluate
              move r17 to spl-riga-stampa
              perform SCRIVI

              move art-tossicologica  to r-art-tossi
              move r17-bis to spl-riga-stampa
              perform SCRIVI

              move art-logo-brand  to r-art-brand
              move r17-tris to spl-riga-stampa
              perform SCRIVI

              move art-note-agg     to r-art-note-agg
              move r18 to spl-riga-stampa
              perform SCRIVI
              
              move 0,8   to spl-colonna
              move 27,9  to spl-colonna-fine

              move 16,2    to spl-riga
              move 19,85   to spl-riga-fine

              perform STAMPA-FRAME
                           
              add 78-distanza to save-riga

              move 0 to tot-char idx-note
              move 1 to num-riga 
              inspect art-note2 replacing trailing spaces by low-value
              inspect art-note2 
                      tallying tot-char for characters before low-value
              perform varying idx from 1 by 1 
                        until idx > tot-char
                 if num-riga > 7
                    exit perform
                 end-if
                 if art-note2(idx:1) = x"0D"
                    perform SCRIVI-NOTA
                    add 1 to idx
                 else
                    add 1 to idx-note
                    move art-note2(idx:1) to r-art-note(idx-note:1)
                    if idx-note = 78-NoteCrtRiga
                       perform SCRIVI-NOTA
                    end-if
                 end-if
              end-perform
              if idx-note > 0 and num-riga < 7
                 perform SCRIVI-NOTA
              end-if

              perform STAMPA-PROGRESSIVI

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       SCRIVI-NOTA.
           if num-riga = 1 
              move "Note" to r19filler
           else
              move spaces to r19filler
           end-if.
           move r19 to spl-riga-stampa.
           perform SCRIVI.
           add 1 to num-riga.
           initialize r-art-note.
           move 0 to idx-note.

      ***---
       STAMPA-PROGRESSIVI.
           perform SALTO-PAGINA.
           move 1 to save-riga.
           move "** PROGRESSIVI DI MAGAZZINO COLLEGATI **" to r-titolo.
           move r-titolo   to spl-riga-stampa
           move 58,5       to spl-tipo-colonna.
           move 5,7        to spl-colonna.
           move Verdana20B to spl-hfont.
           perform SCRIVI.
      
           move low-value  to prg-chiave.
           move art-codice to prg-cod-articolo.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 move Verdana16B to spl-hfont
                 move 2,4 to save-riga
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo not = art-codice
                       exit perform
                    end-if
                    if prg-cod-magazzino not = spaces and
                       prg-tipo-imballo  not = spaces and
                       prg-peso          not = 0
                       move prg-peso to r-peso-utf
                       initialize r-titolo
                       string "Magazzino: "     delimited size
                              prg-cod-magazzino delimited size
                              " - "             delimited size
                              "Imballo: "       delimited size
                              prg-tipo-imballo  delimited size
                              " - "             delimited size
                              "Peso: "          delimited size
                              r-peso-utf        delimited size
                              into r-titolo
                       end-string
                       move r-titolo to spl-riga-stampa
                       move 6,7      to spl-colonna
                       perform SCRIVI
                       perform STAMPA-LINEA-PROGRESSIVI
                    end-if
                 end-perform
           end-start.

      ***---
       STAMPA-FRAME.
           move 4     to spl-pen-with.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.            

           move 1                 to spl-colonna.
            
      ***---
       STAMPA-LINEA.
           move 8                  to spl-pen-with.
           move 1,0                to spl-colonna.
           move 27,7               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-LINEA-PROGRESSIVI.
           move 8                  to spl-pen-with.
           move 6,3                to spl-colonna.
           move 23,4               to spl-colonna-fine.
           add 0,60 to save-riga   giving spl-riga spl-riga-fine.
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
      * Courier New 10B
           initialize wfont-data CourierNew10B.
           move 10 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, CourierNew10B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 20B
           initialize wfont-data Verdana20B.
           move 20 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana20B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 16B
           initialize wfont-data Verdana16B.
           move 16 to wfont-size.
           move "Courier New"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana16B, wfont-data
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
           close articoli 
                 progmag 
                 tcla1art 
                 tnomen 
                 tmarche 
                 tmagaz
                 prodener
                 tudm 
                 tsetmerc 
                 tivaese  
                 timbalqta 
                 timballi 
                 clienti
                 destinif.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy CourierNew10B.
           destroy Verdana16B.
           destroy Verdana20B.

           cancel "spooler".
           initialize spooler-link.
           goback.
