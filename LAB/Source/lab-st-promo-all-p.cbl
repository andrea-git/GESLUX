       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-st-promo-all-p.
       AUTHOR.                          Andrea.
       REMARKS. i files tmp "2" sono per l'anno corrente.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "tgrupgdo.sl".
           copy "articoli.sl". 
           copy "blister.sl".

           copy "tmp-st-promo.sl".
           COPY "tmp-st-promo.sl"
                REPLACING ==tmp-st-promo== BY ==tmp-st-promo2==,
                   ==STATUS-tmp-st-promo== BY ==STATUS-tmp-st-promo2==.

           copy "tmp-st-promo-r.sl".
           COPY "tmp-st-promo-r.sl"
             REPLACING ==tmp-st-promo-r== BY ==tmp-st-promo-r2==,
                ==STATUS-tmp-st-promo-r== BY ==STATUS-tmp-st-promo-r2==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "tgrupgdo.fd".
           copy "articoli.fd". 
           copy "blister.fd".

           copy "tmp-st-promo.fd".
           copy "tmp-st-promo.fd"
             REPLACING ==tmp-st-promo== BY ==tmp-st-promo2==,
                ==STATUS-tmp-st-promo== BY ==STATUS-tmp-st-promo2==.

           copy "tmp-st-promo-r.fd".
           copy "tmp-st-promo-r.fd"
             REPLACING ==tmp-st-promo-r== BY ==tmp-st-promo-r2==,
                ==STATUS-tmp-st-promo-r== BY ==STATUS-tmp-st-promo-r2==.

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tordini          pic xx.
       77  status-rordini          pic xx.
       77  status-tpromo           pic xx.
       77  status-rpromo           pic xx.
       77  status-tgrupgdo         pic xx.
       77  status-articoli         pic xx.
       77  status-blister          pic xx.
       77  status-tmp-st-promo     pic xx.
       77  status-tmp-st-promo2    pic xx.
       77  status-tmp-st-promo-r   pic xx.
       77  status-tmp-st-promo-r2  pic xx.
       77  path-tmp-st-promo       pic x(256).
       77  path-tmp-st-promo2      pic x(256).
       77  path-tmp-st-promo-r     pic x(256).
       77  path-tmp-st-promo-r2    pic x(256).

      * COSTANTI
       78  titolo                value "GESLUX - Stampa Promozioni".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 87.

      * RIGHE PER LA STAMPA
       01  r-titolo.
           05 tit-prec           pic x(35).
           05 tit-corr           pic x(35).

       01  r-data-ini            pic x(10).

       01  r-data-fine           pic x(10).

       01  r-intesta.
           05 filler             pic x(8)  value "Art.".
           05 filler             pic x(50) value "Descrizione".
           05 filler             pic x(7)  value "Prz V".
           05 filler             pic x(7)  value "Prz A".
           05 filler             pic x(7)  value "Qtà".
           05 filler             pic x(7)  value "Qtà B".
           05 filler             pic x(5)  value "Prz F".

       01  r-riga.
           05 r-articolo-1       pic z(6)       blank zero.
           05 r-descr-1          pic x(30).
           05 r-prz-vend-1       pic zzz.zz9,99 blank zero.
           05 r-prz-acq-1        pic zzz.zz9,99 blank zero.
           05 r-qta-1            pic zzz.zz9    blank zero.
           05 r-qta-b-1          pic zzz.zz9    blank zero.
           05 r-prz-fatt-1       pic zzz.zz9,99 blank zero.

           05 r-articolo         pic z(6)       blank zero.
           05 r-descr            pic x(30).
           05 r-prz-vend         pic zzz.zz9,99 blank zero.
           05 r-prz-acq          pic zzz.zz9,99 blank zero.
           05 r-qta              pic zzz.zz9    blank zero.
           05 r-qta-b            pic zzz.zz9    blank zero.
           05 r-prz-fatt         pic zzz.zz9,99 blank zero.

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 riga-blister       value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       77  filler                pic 9.
           88 LeggiPrec          value 1, false 0.
       77  filler                pic 9.
           88 LeggiCorr          value 1, false 0.
      ***** 77  filler                pic 9.
      *****     88 CambioGDO-prec     value 1, false 0.
      ***** 77  filler                pic 9.
      *****     88 CambioGDO-corr     value 1, false 0.
       77  filler                pic 9.
           88 LeggiPrecT         value 1, false 0.
       77  filler                pic 9.
           88 LeggiCorrT         value 1, false 0.
       77  filler                pic 9.
           88 trovato-ordine     value 1, false 0.
       77  filler                pic 9.
           88 anno-corrente      value 1.
           88 anno-precedente    value 2.

      * VARIABILI
       01  save-chiave.
           05 filler             pic 9(2).
           05 filler             pic 9(6).
           05 filler             pic 9(6).
           
       01  occ-ordini            occurs 9999 indexed by ord-idx.
           05 el-numero          pic 9(8).

      ***** 77  SaveGDO               pic x(5).
       77  num-righe             pic 9(5).
       77  bordo                 pic 99v99 value 0.
       77  anno-corr             pic 9(4).
       77  anno-prec             pic 9(4).
       77  idx                   pic 9(4).
       77  tot-idx               pic 9(4).
       77  SaveQta               pic 9(8).   
       77  PrzF                  pic 9(6)v99.
       
       77  tot-fatt-corr         pic 9(6)v99.
       77  tot-fatt-prec         pic 9(6)v99.

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana6BI            handle of font.
       77  Verdana6B             handle of font.
       77  Verdana6              handle of font.
       77  passo                 pic 99v99.
       77  store-riga            pic 9(7)v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  save-colonna          pic s9(7)v99.
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
           move link-data-from(1:4) to anno-corr.
           subtract 1 from anno-corr giving anno-prec.
           move 0        to counter counter2.
           accept como-data from century-date.
           accept como-ora  from time.
           move 0   to num-righe.
           move 0,3 to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok    to true.
           set RecLocked   to false.
           set trovato     to false.
           set prima-volta to true.
           initialize  path-tmp-st-promo.
           accept  path-tmp-st-promo from environment "PATH_ST".
           inspect path-tmp-st-promo replacing trailing 
                                     spaces by low-value.

           string  path-tmp-st-promo delimited low-value
                   "TMP_ST_PROMO_"   delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".tmp"            delimited size
                   into path-tmp-st-promo
           end-string.
           initialize  path-tmp-st-promo2.
           accept  path-tmp-st-promo2 from environment "PATH_ST".
           inspect path-tmp-st-promo2 replacing trailing 
                                      spaces by low-value.

           string  path-tmp-st-promo2 delimited low-value
                   "TMP_ST_PROMO2_"   delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-st-promo2
           end-string.

           initialize  path-tmp-st-promo-r.
           accept  path-tmp-st-promo-r from environment "PATH_ST".
           inspect path-tmp-st-promo-r replacing trailing 
                                       spaces by low-value.

           string  path-tmp-st-promo-r delimited low-value
                   "TMP_ST_PROMO-R_"   delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-tmp-st-promo-r
           end-string.

           initialize  path-tmp-st-promo-r2.
           accept  path-tmp-st-promo-r2 from environment "PATH_ST".
           inspect path-tmp-st-promo-r2 replacing trailing 
                                        spaces by low-value.

           string  path-tmp-st-promo-r2 delimited low-value
                   "TMP_ST_PROMO-R2_"   delimited size
                   como-data            delimited size
                   "_"                  delimited size
                   como-ora             delimited size
                   ".tmp"               delimited size
                   into path-tmp-st-promo-r2
           end-string.

      ***---
       OPEN-FILES.
           open input  tpromo rpromo articoli tgrupgdo 
                       blister tordini rordini.
           open output tmp-st-promo tmp-st-promo-r.
           close       tmp-st-promo tmp-st-promo-r.
           open i-o    tmp-st-promo tmp-st-promo-r.

      *     open output tmp-st-promo2 tmp-st-promo-r2.
      *     close       tmp-st-promo2 tmp-st-promo-r2.
           open i-o    tmp-st-promo2 tmp-st-promo-r2.

      ***---
       ELABORAZIONE.
           move low-value to tpr-rec.
           move link-data-from to tpr-ini-volantino.
                          
           set anno-corrente to true.
           start tpromo key >= tpr-chiave-volantino
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-ini-volantino > link-data-to
                       exit perform
                    end-if

                    if tpr-ini-volantino >= link-data-from and
                       tpr-ini-volantino <= link-data-to

                       if link-gdo = spaces or
                          link-gdo = tpr-gdo
                    
                          move anno-corr
                            to tmp-st-anno      of tmp-st-promo2
                          move tpr-gdo             
                            to tmp-st-gdo       of tmp-st-promo2
                          move tpr-ini-volantino   
                            to tmp-st-ini-vol   of tmp-st-promo2
                          move tpr-fine-volantino  
                            to tmp-st-fine-vol  of tmp-st-promo2
                          move tpr-codice          
                            to tmp-st-codice    of tmp-st-promo2
                          move tpr-tipo            
                            to tmp-st-tipo      of tmp-st-promo2
                          move tpr-descrizione     
                            to tmp-st-vol-descr of tmp-st-promo2
                          move tpr-ini-dpo         
                            to tmp-st-ini-dpo   of tmp-st-promo2
                          move tpr-fine-dpo        
                            to tmp-st-fine-dpo  of tmp-st-promo2
                          write tmp-st-rec of tmp-st-promo2
                                invalid continue 
                          end-write

                          perform LOOP-RIGHE

                       end-if

                    end-if

                 end-perform
           end-start.
                             
           move low-value      to tpr-rec.
           move anno-prec      to link-data-from(1:4).
           move anno-prec      to link-data-to(1:4).
           move link-data-from to tpr-ini-volantino.

           set anno-precedente to true.
           start tpromo key >= tpr-chiave-volantino
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-ini-volantino > link-data-to
                       exit perform
                    end-if

                    if tpr-ini-volantino >= link-data-from and
                       tpr-ini-volantino <= link-data-to

                       if link-gdo = spaces or
                          link-gdo = tpr-gdo
                          move anno-prec
                            to tmp-st-anno      of tmp-st-promo
                          move tpr-gdo             
                            to tmp-st-gdo       of tmp-st-promo
                          move tpr-ini-volantino   
                             to tmp-st-ini-vol   of tmp-st-promo
                          move tpr-fine-volantino  
                            to tmp-st-fine-vol  of tmp-st-promo
                          move tpr-codice          
                            to tmp-st-codice    of tmp-st-promo
                          move tpr-tipo            
                            to tmp-st-tipo      of tmp-st-promo
                          move tpr-descrizione     
                            to tmp-st-vol-descr of tmp-st-promo
                          move tpr-ini-dpo         
                            to tmp-st-ini-dpo   of tmp-st-promo
                          move tpr-fine-dpo        
                            to tmp-st-fine-dpo  of tmp-st-promo
                          write tmp-st-rec of tmp-st-promo
                                invalid continue 
                          end-write

                          perform LOOP-RIGHE
                       end-if
                    end-if
                 end-perform
           end-start.

           if trovato
              perform CONTA-RIGHE
              move 1 to pagina
              move 0 to store-riga
      *****        set CambioGDO-prec to false
      *****        set CambioGDO-corr to false

              perform until 1 = 2

                 set LeggiPrecT     to true
                 set LeggiCorrT     to true
      *****           set CambioGDO-prec to false
      *****           set CambioGDO-corr to false

                 move anno-prec to tmp-st-rec of tmp-st-promo
                 start tmp-st-promo key >= tmp-st-chiave of tmp-st-promo
                       invalid 
                       set LeggiPrecT to false
                   not invalid
                       read tmp-st-promo next no lock
                            at end
                            set LeggiPrecT to false
                        not at end
                            if tmp-st-anno of tmp-st-promo not = 
                               anno-prec
                               set LeggiPrecT to false
                            end-if
      *****                      if SaveGDO not = spaces and
      *****                         SaveGDO not = tmp-st-gdo of tmp-st-promo
      *****                         set CambioGDO-prec to true
      *****                      end-if
                       end-read
                 end-start

                 if not LeggiPrecT
                    initialize tmp-st-rec of tmp-st-promo
                        replacing numeric data by zeroes
                             alphanumeric data by spaces
                 end-if
                               
                 move anno-corr to tmp-st-rec of tmp-st-promo2
                 start tmp-st-promo2 key >= tmp-st-chiave 
                                        of tmp-st-promo2
                       invalid set LeggiCorrT to false
                   not invalid
                       read tmp-st-promo2 next no lock
                            at end
                            set LeggiCorrT to false
                        not at end
                            if tmp-st-anno of tmp-st-promo2 not = 
                               anno-corr
                               set LeggiCorrT to false
                            end-if
      *****                      if SaveGDO not = spaces and
      *****                         SaveGDO not = tmp-st-gdo of tmp-st-promo2
      *****                         set CambioGDO-corr to true
      *****                      end-if
                       end-read
                 end-start

                 if not LeggiCorrT
                    initialize tmp-st-rec of tmp-st-promo2
                        replacing numeric data by zeroes
                             alphanumeric data by spaces
                 end-if

                 if not LeggiPrecT and not LeggiCorrT
                    exit perform
                 end-if

      *****           if CambioGDO-prec and CambioGDO-corr
      *****              perform SALTO-PAGINA
      *****              perform FINCATURE
      *****              set CambioGDO-prec to false
      *****              set CambioGDO-corr to false
      *****           end-if

                 set LeggiPrec to false
                 set LeggiCorr to false

                 if not LeggiPrecT
                    set LeggiCorr to true
                 else
                    |Inizia prima il precedente per cui è
                    |il punto di riferimento come periodo
                    if ( tmp-st-ini-vol of tmp-st-promo(5:) <= 
                         tmp-st-ini-vol of tmp-st-promo2(5:) and
                         tmp-st-gdo     of tmp-st-promo <=
                         tmp-st-gdo     of tmp-st-promo2 )   or
                       ( tmp-st-gdo     of tmp-st-promo <
                         tmp-st-gdo     of tmp-st-promo2 )   
                       set LeggiPrec to true
      *****                 move tmp-st-gdo of tmp-st-promo to SaveGDO

                       if tmp-st-ini-vol  of tmp-st-promo2(5:) >= 
                          tmp-st-ini-vol  of tmp-st-promo(5:) and
                          tmp-st-ini-vol  of tmp-st-promo2(5:) <= 
                          tmp-st-fine-vol of tmp-st-promo(5:)
                          if tmp-st-gdo   of tmp-st-promo =
                             tmp-st-gdo   of tmp-st-promo2
                             set LeggiCorr to true
      *****                    else
      *****                       set CambioGDO-corr to true
                          end-if
                       else
                          set LeggiCorr to false
                       end-if
                    end-if
                 end-if

                 if not LeggiCorrT
                    set LeggiPrec to true
                 else
                   |Inizia prima il corrente per cui è
                   |il punto di riferimento come periodo
                    if ( tmp-st-ini-vol of tmp-st-promo2(5:) <= 
                         tmp-st-ini-vol of tmp-st-promo(5:)  and
                         tmp-st-gdo     of tmp-st-promo2 <=
                         tmp-st-gdo     of tmp-st-promo )    or
                       ( tmp-st-gdo     of tmp-st-promo2 <
                         tmp-st-gdo     of tmp-st-promo )
                       set LeggiCorr to true
      *****                 move tmp-st-gdo of tmp-st-promo2 to SaveGDO
              
                       if tmp-st-ini-vol  of tmp-st-promo(5:)   >=
                          tmp-st-ini-vol  of tmp-st-promo2(5:) and
                          tmp-st-ini-vol  of tmp-st-promo(5:)   <=
                          tmp-st-fine-vol of tmp-st-promo2(5:)
                          if tmp-st-gdo   of tmp-st-promo2 =
                             tmp-st-gdo   of tmp-st-promo
                             set LeggiPrec to true
      *****                    else
      *****                       set CambioGDO-prec to true
                          end-if
                       else
                          set LeggiPrec to false
                       end-if
                    end-if
                 end-if

                 if prima-volta
                    perform APRI-STAMPA

                    if errori
                       exit perform
                    end-if

                    perform FINCATURE
                    add 0,05 to save-riga
                 else
                    if num-righe < 78-MaxRows and
                       num-righe > 5
                       add passo 0,1 to save-riga
                       perform STAMPA-LINEA
                       subtract passo from save-riga
                       add 0,04 to save-riga
                    end-if
                 end-if

                 if num-righe >= 78-MaxRows - 5
                    perform SALTO-PAGINA
                    perform FINCATURE
                 end-if
                       
                 move 0 to tot-fatt-prec tot-fatt-corr
                 perform SCRIVI-INTESTAZIONE

                 move low-value to tmpr-st-rec of tmp-st-promo-r
                 move tmp-st-codice  of tmp-st-promo 
                   to tmpr-st-codice of tmp-st-promo-r

                 start tmp-st-promo-r key >= tmpr-st-chiave
                                          of tmp-st-promo-r
                       invalid continue
                 end-start
                          

                 move low-value to tmpr-st-rec of tmp-st-promo-r2
                 move tmp-st-codice  of tmp-st-promo2
                   to tmpr-st-codice of tmp-st-promo-r2

                 start tmp-st-promo-r2 key >= tmpr-st-chiave 
                                           of tmp-st-promo-r2
                       invalid continue
                 end-start

                 perform until 1 = 2

                    if LeggiPrec
                       read tmp-st-promo-r next no lock
                            at end set LeggiPrec to false
                       end-read
                       if tmpr-st-codice of tmp-st-promo-r not =
                          tmp-st-codice  of tmp-st-promo
                          set LeggiPrec to false
                          initialize tmpr-st-rec of tmp-st-promo-r
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                       end-if
                    else
                       initialize tmpr-st-rec of tmp-st-promo-r
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                    end-if

                    if LeggiCorr
                       read tmp-st-promo-r2 next no lock
                            at end set LeggiCorr to false
                       end-read
                       if tmpr-st-codice of tmp-st-promo-r2 not =
                          tmp-st-codice  of tmp-st-promo2
                          set LeggiCorr to false
                          initialize tmpr-st-rec of tmp-st-promo-r2
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                       end-if
                    else
                       initialize tmpr-st-rec of tmp-st-promo-r2
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                    end-if

                    if not LeggiCorr and not LeggiPrec
                       exit perform
                    end-if

                    perform SCRIVI-RIGA

                    add tmpr-st-prz-fatt of tmp-st-promo-r 
                                         to tot-fatt-prec

                    add tmpr-st-prz-fatt of tmp-st-promo-r2
                                         to tot-fatt-corr

                 end-perform

                 if tot-fatt-prec not = 0 or tot-fatt-corr not = 0
                    if num-righe < 78-MaxRows
                       perform STAMPA-TOT-FATT
                    end-if
                 end-if

              end-perform
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
       SCRIVI-RIGA.
           move tmpr-st-art-descrizione 
             of tmp-st-promo-r to r-descr-1.
           move tmpr-st-art-codice      
             of tmp-st-promo-r to r-articolo-1.
           move tmpr-st-prz-ven         
             of tmp-st-promo-r to r-prz-vend-1.
           move tmpr-st-prz-acq         
             of tmp-st-promo-r to r-prz-acq-1.
           move tmpr-st-qta             
             of tmp-st-promo-r to r-qta-1.     
           move tmpr-st-qta-b           
             of tmp-st-promo-r to r-qta-b-1.   
           move tmpr-st-prz-fatt        
             of tmp-st-promo-r to r-prz-fatt-1.

           move tmpr-st-art-descrizione 
             of tmp-st-promo-r2 to r-descr.
           move tmpr-st-art-codice      
             of tmp-st-promo-r2 to r-articolo.   
           move tmpr-st-prz-ven
             of tmp-st-promo-r2 to r-prz-vend.
           move tmpr-st-prz-acq         
             of tmp-st-promo-r2 to r-prz-acq.
           move tmpr-st-qta             
             of tmp-st-promo-r2 to r-qta.     
           move tmpr-st-qta-b
             of tmp-st-promo-r2 to r-qta-b.   
           move tmpr-st-prz-fatt
             of tmp-st-promo-r2 to r-prz-fatt.

           move Verdana6 to spl-hfont.
           move r-riga   to spl-riga-stampa.
           set spl-nero  to true.
           move 83,7     to spl-tipo-colonna.
           perform SCRIVI.
           add 1 to num-righe.
           if num-righe >= 78-MaxRows
              perform SALTO-PAGINA
              perform FINCATURE
              move 0,5 to save-riga
           end-if.

      ***---
       STAMPA-TOT-FATT.
           move Verdana6 to spl-hfont.
           set spl-blu   to true.
           move spaces   to r-riga.

           if tot-fatt-prec not = 0
              move "TOTALE FATTURATO" to r-descr-1 
              move tot-fatt-prec      to r-prz-fatt-1
           end-if.

           if tot-fatt-corr not = 0
              move "TOTALE FATTURATO" to r-descr
              move tot-fatt-corr      to r-prz-fatt
           end-if.
           move r-riga to spl-riga-stampa.
           move 83,7   to spl-tipo-colonna.
           perform SCRIVI.
           add 1 to num-righe.

      ***---
       APRI-STAMPA.
           set prima-volta to false.
           initialize spooler-link.
           call   "selprint" using selprint-linkage.
           cancel "selprint".
      
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
           else
              set spl-sta-annu to true
              set errori to true
           end-if.

      ***---
       LOOP-RIGHE.
           move tpr-codice to rpr-codice.
           move low-value  to rpr-articolo.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next at end exit perform end-read

                    if rpr-codice not = tpr-codice
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

                    set riga-blister to false
                    set trovato to true
                    initialize art-rec bli-rec
                    move rpr-articolo to art-codice
                    read articoli no lock 
                         invalid
                         move art-codice to bli-codice
                         read blister no lock
                              invalid continue
                          not invalid
                              move bli-descrizione to art-descrizione
                              set riga-blister to true
                              move 0 to SaveQta
                         end-read
                    end-read
 
                    perform CERCA-ORDINI-PROMO

                    if anno-precedente
                       move rpr-codice to tmpr-st-codice 
                                       of tmp-st-promo-r
                       move art-codice          
                         to tmpr-st-art-codice of tmp-st-promo-r
                       move art-descrizione     
                         to tmpr-st-art-descrizione of tmp-st-promo-r
                       move rpr-prz-ven         
                         to tmpr-st-prz-ven    of tmp-st-promo-r
                       move rpr-prz-acq         
                         to tmpr-st-prz-acq    of tmp-st-promo-r
                       move rpr-qta             
                         to tmpr-st-qta        of tmp-st-promo-r
                       write tmpr-st-rec       of tmp-st-promo-r
                             invalid continue 
                       end-write
                    else
                       move rpr-codice to tmpr-st-codice 
                                       of tmp-st-promo-r2
                       move art-codice          
                         to tmpr-st-art-codice of tmp-st-promo-r2
                       move art-descrizione     
                         to tmpr-st-art-descrizione of tmp-st-promo-r2
                       move rpr-prz-ven         
                         to tmpr-st-prz-ven    of tmp-st-promo-r2
                       move rpr-prz-acq         
                         to tmpr-st-prz-acq    of tmp-st-promo-r2
                       move rpr-qta             
                         to tmpr-st-qta        of tmp-st-promo-r2
                       write tmpr-st-rec       of tmp-st-promo-r2
                             invalid continue 
                       end-write
                    end-if

                 end-perform
                       
           end-start.

      ***---
       CERCA-ORDINI-PROMO.                       
           move 0 to tmpr-st-qta-b      of tmp-st-promo-r.
           move 0 to tmpr-st-qta-b      of tmp-st-promo-r2.
           move 0 to tmpr-st-prz-fatt   of tmp-st-promo-r.
           move 0 to tmpr-st-prz-fatt   of tmp-st-promo-r2.

           move low-value  to ror-rec.
           move tpr-codice to ror-promo.
           start rordini key >= ror-k-promo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-promo not = tpr-codice
                       exit perform
                    end-if                                        
                    move ror-chiave to tor-chiave
                    read tordini no lock 
                         invalid continue
                     not invalid
                         if tor-data-fattura not = 0 and
                            tor-num-fattura  not = 0
       
                            if riga-blister
                               if ror-bli-codice = bli-codice
                                  perform SOMMA-QTA-PRZ
                               end-if
                            else
                               if ror-cod-articolo = rpr-articolo and 
                                  not ror-si-blister
                                  perform SOMMA-QTA-PRZ
                               end-if
                            end-if
                         end-if
                    end-read
                 end-perform
           end-start.

      ***---
       SOMMA-QTA-PRZ.
           if riga-blister
      ***     primo elemento del blister e mi salvo la quantita
      ***     che si trova su quello, negli altri elementi viene 
      ***     moltiplicata e ho il numero degli elementi
              if ror-num-colli not = 0 
                 compute SaveQta = SaveQta + ror-qta - ror-qta-omaggi
              end-if
              if anno-corrente
                 compute tmpr-st-qta-b of tmp-st-promo-r2 = SaveQta
                 compute tmpr-st-prz-fatt of tmp-st-promo-r2 =
                         tmpr-st-prz-fatt of tmp-st-promo-r2 +
                       ( ror-prz-unitario * 
                       ( ror-qta - ror-qta-omaggi ))
              else
                 compute tmpr-st-qta-b of tmp-st-promo-r = SaveQta
                 compute tmpr-st-prz-fatt of tmp-st-promo-r =
                         tmpr-st-prz-fatt of tmp-st-promo-r +
                       ( ror-prz-unitario * 
                       ( ror-qta - ror-qta-omaggi ))
              end-if
           else
              if anno-corrente
                 compute tmpr-st-qta-b of tmp-st-promo-r2 = 
                         tmpr-st-qta-b of tmp-st-promo-r2 +
                         ror-qta - ror-qta-omaggi
                 compute tmpr-st-prz-fatt of tmp-st-promo-r2 =
                         tmpr-st-prz-fatt of tmp-st-promo-r2 +
                       ( ror-prz-unitario  * 
                       ( ror-qta - ror-qta-omaggi ))
              else
                 compute tmpr-st-qta-b of tmp-st-promo-r = 
                         tmpr-st-qta-b of tmp-st-promo-r +
                         ror-qta - ror-qta-omaggi
                 compute tmpr-st-prz-fatt of tmp-st-promo-r =
                         tmpr-st-prz-fatt of tmp-st-promo-r +
                       ( ror-prz-unitario * 
                       ( ror-qta - ror-qta-omaggi ))
              end-if
           end-if.
      ***---
       FINCATURE.
           if LeggiPrecT
              move tmp-st-gdo of tmp-st-promo to gdo-codice
              read tgrupgdo no lock invalid continue end-read
           else
              move tmp-st-gdo of tmp-st-promo2 to gdo-codice
              read tgrupgdo no lock invalid continue end-read
           end-if.

           move Verdana6BI to spl-hfont.
           string "Promo dal "        delimited size
                  link-data-from(7:2) delimited size
                  "/"                 delimited size
                  link-data-from(5:2) delimited size
                  "/"                 delimited size
                  anno-prec           delimited size
                  " al "              delimited size
                  link-data-to(7:2)   delimited size
                  "/"                 delimited size
                  link-data-to(5:2)   delimited size
                  "/"                 delimited size
                  anno-prec           delimited size
                  into tit-prec
           end-string.
           string "Promo dal "        delimited size
                  link-data-from(7:2) delimited size
                  "/"                 delimited size
                  link-data-from(5:2) delimited size
                  "/"                 delimited size
                  anno-corr           delimited size
                  " al "              delimited size
                  link-data-to(7:2)   delimited size
                  "/"                 delimited size
                  link-data-to(5:2)   delimited size
                  "/"                 delimited size
                  anno-corr           delimited size
                  into tit-corr
           end-string.
           move r-titolo to spl-riga-stampa.
           set spl-blu to true.
           move 0 to save-riga.
           move 83,8 to spl-tipo-colonna.
           perform SCRIVI.

           set spl-nero to true.

           move 0,6 to save-riga.
           perform STAMPA-LINEA.

           move 0,4 to save-colonna.
           perform STAMPA-LINEA-VERTICALE.

           move 10,0 to save-colonna.
           perform STAMPA-LINEA-VERTICALE.

           move 19,5 to save-colonna.
           perform STAMPA-LINEA-VERTICALE.

           move 27,3 to save-riga.
           perform STAMPA-LINEA.

           move 0,3 to save-riga.
           move 0   to num-righe.

      ***---
       SCRIVI-INTESTAZIONE.
           add 5     to num-righe.
           move 0    to spl-tipo-colonna.
           move 0,1  to bordo. 
           perform STAMPA-FRAME.
           add bordo to save-riga.
           move save-riga to store-riga.
           move 0,4 to passo.
           if LeggiPrec
              move tmp-st-gdo of tmp-st-promo to gdo-codice
              read tgrupgdo no lock invalid continue end-read

              move store-riga to save-riga
              move 0          to spl-tipo-colonna
              move -0,3       to save-colonna
              set spl-rosso   to true
              move Verdana6BI to spl-hfont
              add 0,9         to save-colonna giving spl-colonna
              move "Cliente"  to spl-riga-stampa
              perform SCRIVI

              set spl-blu to true
              subtract passo from save-riga
              add 2,2 to save-colonna giving spl-colonna
              move gdo-intestazione       to spl-riga-stampa
              perform SCRIVI
              set spl-rosso to true

              add 0,9 to save-colonna giving spl-colonna
              move "Volantino" to spl-riga-stampa
              subtract 0,1  from save-riga
              perform SCRIVI

              subtract passo from save-riga
              add 2,2 to save-colonna giving spl-colonna
              move tmp-st-vol-descr of tmp-st-promo to spl-riga-stampa
              perform SCRIVI

              if tmp-st-locale   of tmp-st-promo
                 set spl-blu     to true
                 subtract 0,20 from save-riga
                 move Verdana6B  to spl-hfont
                 add 2,2 to save-colonna giving spl-colonna
                 move "*LOCALE*" to spl-riga-stampa
                 perform SCRIVI
                 move Verdana6BI to spl-hfont
                 set spl-rosso   to true
                 subtract 0,20 from save-riga
              end-if

      *****     add 0,1 to save-riga.
           
              move "Vol. dal"             to spl-riga-stampa
              add 0,9 to save-colonna giving spl-colonna
              perform SCRIVI

              string tmp-st-ini-vol of tmp-st-promo(7:2) delimited size
                     "/"                                 delimited size
                     tmp-st-ini-vol of tmp-st-promo(5:2) delimited size
                     "/"                                 delimited size
                     tmp-st-ini-vol of tmp-st-promo(1:4) delimited size
                     into r-data-ini
              end-string
              subtract passo from save-riga
              move r-data-ini to spl-riga-stampa
              add 2,2 to save-colonna giving spl-colonna
              perform SCRIVI

              move "-" to spl-riga-stampa
              add 3,8 to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI
                                                                        
              string tmp-st-fine-vol of tmp-st-promo(7:2) delimited size
                     "/"                                  delimited size
                     tmp-st-fine-vol of tmp-st-promo(5:2) delimited size
                     "/"                                  delimited size
                     tmp-st-fine-vol of tmp-st-promo(1:4) delimited size
                     into r-data-fine
              end-string
              move r-data-fine to spl-riga-stampa
              add 3,95         to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move "DPO dal"              to spl-riga-stampa
              add 5,8 to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              string tmp-st-ini-dpo  of tmp-st-promo(7:2) delimited size
                     "/"                                  delimited size
                     tmp-st-ini-dpo  of tmp-st-promo(5:2) delimited size
                     "/"                                  delimited size
                     tmp-st-ini-dpo  of tmp-st-promo(1:4) delimited size
                     into r-data-ini
              end-string
              move r-data-ini  to spl-riga-stampa
              add 6,8          to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move "-"                    to spl-riga-stampa
              add 8,4 to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              string tmp-st-fine-dpo of tmp-st-promo(7:2) delimited size
                     "/"                                  delimited size
                     tmp-st-fine-dpo of tmp-st-promo(5:2) delimited size
                     "/"                                  delimited size
                     tmp-st-fine-dpo of tmp-st-promo(1:4) delimited size
                     into r-data-fine
              end-string
              move r-data-fine             to spl-riga-stampa
              add 8,55 to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move 83,5      to spl-tipo-colonna
              move r-intesta to spl-riga-stampa
              subtract 0,1 from save-riga
              perform SCRIVI
              if LeggiPrecT
                 delete tmp-st-promo record 
                        invalid continue
                 end-delete
              end-if
           end-if.

           if LeggiCorr
              move tmp-st-gdo of tmp-st-promo2 to gdo-codice
              read tgrupgdo no lock invalid continue end-read

              |SECONDA PARTE
              move 0          to spl-tipo-colonna
              move store-riga to save-riga

              move 9,3        to save-colonna
              set  spl-rosso  to true
              move Verdana6BI to spl-hfont
              add 0,9         to save-colonna giving spl-colonna
              move "Cliente"  to spl-riga-stampa
              perform SCRIVI

              set spl-blu to true
              subtract passo from save-riga
              add 2,2 to save-colonna giving spl-colonna
              move gdo-intestazione to spl-riga-stampa
              perform SCRIVI
              set spl-rosso to true

              add 0,9          to save-colonna giving spl-colonna
              move "Volantino" to spl-riga-stampa
              subtract 0,1   from save-riga
              perform SCRIVI

              subtract passo from save-riga
              add 2,2 to save-colonna giving spl-colonna
              move tmp-st-vol-descr of tmp-st-promo2 to spl-riga-stampa
              perform SCRIVI

              if tmp-st-locale of tmp-st-promo2
                 set spl-blu     to true
                 subtract 0,20 from save-riga
                 move Verdana6B  to spl-hfont
                 add 2,2 to save-colonna giving spl-colonna
                 move "  *LOCALE*"  to spl-riga-stampa
                 perform SCRIVI
                 move Verdana6BI to spl-hfont
                 set spl-rosso   to true
                 subtract 0,20 from save-riga
              end-if

      *****     add 0,1 to save-riga.
           
              move "Vol. dal" to spl-riga-stampa
              add 0,9 to save-colonna giving spl-colonna
              perform SCRIVI
                                                                        
              string tmp-st-ini-vol  of tmp-st-promo2(7:2)delimited size
                     "/"                                  delimited size
                     tmp-st-ini-vol  of tmp-st-promo2(5:2)delimited size
                     "/"                                  delimited size
                     tmp-st-ini-vol  of tmp-st-promo2(1:4)delimited size
                     into r-data-ini
              end-string
              subtract passo from save-riga
              move r-data-ini  to spl-riga-stampa
              add 2,2 to save-colonna giving spl-colonna
              perform SCRIVI

              move "-" to spl-riga-stampa
              add 3,8  to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI
                                                                        
              string tmp-st-fine-vol of tmp-st-promo2(7:2)delimited size
                     "/"                                  delimited size
                     tmp-st-fine-vol of tmp-st-promo2(5:2)delimited size
                     "/"                                  delimited size
                     tmp-st-fine-vol of tmp-st-promo2(1:4)delimited size
                     into r-data-fine
              end-string
              move r-data-fine to spl-riga-stampa
              add 3,95         to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move "DPO dal"   to spl-riga-stampa
              add 5,8          to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI
                                                                        
              string tmp-st-ini-dpo  of tmp-st-promo2(7:2)delimited size
                     "/"                                  delimited size
                     tmp-st-ini-dpo  of tmp-st-promo2(5:2)delimited size
                     "/"                                  delimited size
                     tmp-st-ini-dpo  of tmp-st-promo2(1:4)delimited size
                     into r-data-ini
              end-string
              move r-data-ini  to spl-riga-stampa
              add 6,8          to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move "-" to spl-riga-stampa
              add 8,4  to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI
                                                                        
              string tmp-st-fine-dpo of tmp-st-promo2(7:2)delimited size
                     "/"                                  delimited size
                     tmp-st-fine-dpo of tmp-st-promo2(5:2)delimited size
                     "/"                                  delimited size
                     tmp-st-fine-dpo of tmp-st-promo2(1:4)delimited size
                     into r-data-fine
              end-string
              move r-data-fine to spl-riga-stampa
              add 8,55         to save-colonna giving spl-colonna
              subtract passo from save-riga
              perform SCRIVI

              move 83,6      to spl-tipo-colonna
              move r-intesta to spl-riga-stampa
              subtract 0,1 from save-riga
              perform SCRIVI
              if LeggiCorrT
                 delete tmp-st-promo2 record 
                        invalid continue
                 end-delete
              end-if
           end-if.
           add  0,2 to save-riga.
           move 0,3 to passo.

      ***---
       STAMPA-FRAME.
           move 8     to spl-pen-with.
           move 00,5  to spl-colonna.
           move  9,9  to spl-colonna-fine.

           add  0,4   to save-riga giving spl-riga.
           add  1,8   to save-riga giving spl-riga-fine.

           set  spl-oggetto    to true.
           set  spl-rettangolo to true.
           set  spl-grigio     to true.
           set  spl-brush-null to true.
           call "spooler"      using spooler-link.

           move 8     to spl-pen-with.
           move 10,1  to spl-colonna.
           move 19,40 to spl-colonna-fine.

           add  0,4   to save-riga giving spl-riga.
           add  1,8   to save-riga giving spl-riga-fine.

           set  spl-oggetto    to true.
           set  spl-rettangolo to true.
           set  spl-grigio     to true.
           set  spl-brush-null to true.
           call "spooler"   using spooler-link.
            
      ***---
       STAMPA-LINEA.
           set spl-nero         to true.
           move 4               to spl-pen-with.
           move 0,4             to spl-colonna.
           move 19,5            to spl-colonna-fine.
           move save-riga       to spl-riga spl-riga-fine.
           set  spl-oggetto     to true.
           set  spl-linea       to true.
           set  spl-pen-solid   to true.
           call "spooler"    using spooler-link.
           add 1 to num-righe.
            
      ***---
       STAMPA-LINEA-VERTICALE.
           set spl-nero         to true.
           move 4               to spl-pen-with.
           move save-colonna    to spl-colonna spl-colonna-fine.
           move 0,6             to spl-riga.
           move 27,3            to spl-riga-fine.
           set  spl-oggetto     to true.
           set  spl-linea       to true.
           set  spl-pen-solid   to true.
           call "spooler"    using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           perform PIE-DI-PAGINA.
           add 1                to pagina.
           set spl-salto-pagina to true.
           call "spooler"    using spooler-link.
           move 0 to num-righe.
           move 0 to save-riga.

      ***---
       CONTA-RIGHE.
      ******     move 0 to SavePrg SaveMese pagine-mesi.
      ******     move low-value to tmp-st-rec.
      ******     start tmp-st-promo key >= tmp-st-chiave
      ******           invalid continue
      ******       not invalid
      ******           perform until 1 = 2
      ******              read tmp-st-promo next at end exit perform end-read
      ******              if SavePrg = 0
      ******                 add 5 to num-righe
      ******                 move tmp-st-prg  to SavePrg
      ******                 move tmp-st-mese to SaveMese
      ******              end-if
      ******              if tmp-st-prg not = SavePrg
      ******                 move tmp-st-prg to SavePrg
      ******                 add 2 to num-righe
      ******                 add 5 to num-righe
      ******                 if tmp-st-mese not = SaveMese
      ******
      ******                    |divide num-righe + 1 by 78-MaxRows  DA PROVARE
      ******                    divide num-righe by 78-MaxRows 
      ******                    giving tot-pag   remainder resto
      ******                    if resto > 0 add 1 to tot-pag end-if
      ******                    add resto to num-righe
      ******                    move tmp-st-mese to SaveMese
      ******                 end-if
      ******              end-if
      ******              add 1 to num-righe
      ******              
      ******           end-perform
      ******     end-start.
      ******
      ******     if num-righe > 1
      ******        move 1 to pagina
      ******        evaluate true
      ******        when num-righe < 78-MaxRows
      ******             move 1 to tot-pag
      ******        when num-righe = 78-MaxRows
      ******             move 2 to tot-pag
      ******        when other
      ******             divide num-righe by 78-MaxRows 
      ******             giving tot-pag   remainder resto
      ******             if resto > 0 add 1 to tot-pag end-if
      ******        end-evaluate
      ******     end-if.
      ******     move 0 to num-righe.

      ***---
       CARICA-FONT.

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
                        giving WFONT-STATUS.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
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
           close tpromo rpromo articoli tgrupgdo 
                 blister  tordini rordini
                 tmp-st-promo2 tmp-st-promo-r
                 tmp-st-promo  tmp-st-promo-r2.
                  
           delete file tmp-st-promo  tmp-st-promo-r.
           delete file tmp-st-promo2 tmp-st-promo-r2.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana6BI.
           destroy Verdana6B.
           destroy Verdana6.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 
           display "                                                   "
              upon link-handle at column 22 line 10.
           goback.

      ***---
       PARAGRAFO-COPY.
      *****     copy "pie-di-pagina.cpy".      ***---
       PIE-DI-PAGINA.
           if font-pie-pagina = 0
      *       Verdana 6I
              initialize wfont-data font-pie-pagina
              move 6 to wfont-size
              move "Verdana"            to wfont-name
              set  wfcharset-dont-care  to true
              set  wfont-bold           to false
              set  wfont-italic         to true
              set  wfont-underline      to false
              set  wfont-strikeout      to false
              set  wfont-fixed-pitch    to false
              move 0                    to wfont-char-set
              set  wfdevice-win-printer to true |E' un carattere per la stampante
              call "W$FONT" using wfont-get-font, font-pie-pagina, 
                                  wfont-data
                           giving WFONT-STATUS

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
              if wfont-status not = 1
                 set errori to true
                 perform MESSAGGIO-ERR-FONT
                 exit paragraph
              end-if 
           end-if.

           if mese = spaces
              evaluate como-data(5:2)
              when "01" move "Gennaio"   to mese
              when "02" move "Febbraio"  to mese
              when "03" move "Marzo"     to mese
              when "04" move "Aprile"    to mese
              when "05" move "Maggio"    to mese
              when "06" move "Giugno"    to mese
              when "07" move "Luglio"    to mese
              when "08" move "Agosto"    to mese
              when "09" move "Settembre" to mese
              when "10" move "Ottobre"   to mese
              when "11" move "Novembre"  to mese
              when "12" move "Dicembre"  to mese
              end-evaluate
              inspect mese replacing trailing spaces by low-value
           end-if.
           if giorno = spaces
              accept giorno from day-of-week
              evaluate giorno
              when "1" move "Lunedì"    to giorno
              when "2" move "Martedì"   to giorno
              when "3" move "Mercoledì" to giorno
              when "4" move "Giovedì"   to giorno
              when "5" move "Venerdì"   to giorno
              when "6" move "Sabato"    to giorno
              when "7" move "Domenica"  to giorno
              end-evaluate
              inspect giorno replacing trailing spaces by low-value
           end-if.

           move save-altezza-pagina to save-riga.

           set  spl-grigio       to true.
           move 10               to spl-pen-with.
           move 2                to spl-colonna.
           move 18,5             to spl-colonna-fine.
           move save-riga        to spl-riga spl-riga-fine.
           set  spl-oggetto      to true.
           set  spl-linea        to true.
           set  spl-pen-solid    to true.
           call "spooler"    using spooler-link.

           subtract 0,2 from save-riga.
           initialize data-stampa.
           string giorno         delimited low-value
                  " "            delimited size
                  como-data(7:2) delimited size
                  " "            delimited size
                  mese           delimited low-value
                  " "            delimited size
                  como-data(1:4) delimited size
                  into data-stampa
           end-string.
           move    tot-pag to tot-pag-x.
           inspect tot-pag-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using tot-pag-x, "L".
           inspect tot-pag-x replacing trailing spaces by low-value.
           move pagina to pagina-x.
           inspect pagina-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using pagina-x, "L".
           inspect pagina-x replacing trailing spaces by low-value.
           
           initialize pagina-di.

           string "Pagina"  delimited size
                  " "       delimited size
                  pagina-x  delimited low-value
                  into pagina-di
           end-string.
           set  spl-nero  to true.
           move font-pie-pagina to spl-hfont.
           move 57        to spl-tipo-colonna.
           move p-riga    to spl-riga-stampa.
           perform SCRIVI.
           move 0         to save-riga.
