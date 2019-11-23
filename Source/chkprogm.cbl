       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      chkprogm.
       AUTHOR.                          Andrea.
       REMARKS. Controlli sul file dei progressivi di magazzino:
                1. presenza di padri con relativi figli
                2. congruenza di valori tra padri e somma dei figli
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           COPY "progmag.sl"
                REPLACING ==progmag== BY ==progmag2==,
                          ==STATUS-progmag== BY ==STATUS-progmag2==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".
           COPY "progmag.fd"
                REPLACING ==progmag== BY ==progmag2==,
                          ==STATUS-progmag== BY ==STATUS-progmag2==.

       WORKING-STORAGE SECTION.
       78  78-clear              value 
           "                                                          ".
       77  status-progmag        pic xx.
       77  status-progmag2       pic xx.
       77  SaveArticolo          pic 9(6).
       77  articolo-edit         pic z(6).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  como-ora              pic 9(8).
       77  como-ora-edit         pic x(9).
       77  riga-d                pic x(40).
       77  riga-c                pic x(40).
       77  Ordinato-1Figli       pic s9(8).
       77  Ordinato-2Figli       pic s9(8).
       77  Ordinato-3Figli       pic s9(8).
       77  Ordinato-4Figli       pic s9(8).
       77  Ordinato-5Figli       pic s9(8).
       77  Ordinato-6Figli       pic s9(8).
       77  ImpegnatoFigli        pic s9(8).
       77  ImpegnatoMFigli       pic s9(8).
       77  GiacenzaFigli         pic s9(8).
       77  GiacenzaFigliC        pic s9(8).
       77  VenditeFigli          pic s9(9)v99.
       77  AcquistiFigli         pic s9(9)v99.
       77  InizialiFigli         pic s9(9)v99.

       77  GiacenzaFigli-ed      pic ---.---.--9.
       77  GiacenzaFigliC-ed     pic ---.---.--9.
       77  VenditeFigli-ed       pic ----.---.--9,99.
       77  AcquistiFigli-ed      pic ----.---.--9,99.
       77  InizialiFigli-ed      pic ----.---.--9,99.
       77  ImpegnatoFigli-ed     pic ----.---.--9.
       77  ImpegnatoMFigli-ed    pic ----.---.--9.
       77  Ordinato-1Figli-ed    pic ----.---.--9.
       77  Ordinato-2Figli-ed    pic ----.---.--9.
       77  Ordinato-3Figli-ed    pic ----.---.--9.
       77  Ordinato-4Figli-ed    pic ----.---.--9.
       77  Ordinato-5Figli-ed    pic ----.---.--9.
       77  Ordinato-6Figli-ed    pic ----.---.--9.

       77  GiacenzaPadre-ed      pic ---.---.--9.
       77  GiacenzaPadreC-ed     pic ---.---.--9.
       77  VenditePadre-ed       pic ----.---.--9,99.
       77  AcquistiPadre-ed      pic ----.---.--9,99.
       77  InizialiPadre-ed      pic ----.---.--9,99.
       77  Ordinato-1Padre-ed    pic ----.---.--9.
       77  Ordinato-2Padre-ed    pic ----.---.--9.
       77  Ordinato-3Padre-ed    pic ----.---.--9.
       77  Ordinato-4Padre-ed    pic ----.---.--9.
       77  Ordinato-5Padre-ed    pic ----.---.--9.
       77  Ordinato-6Padre-ed    pic ----.---.--9.
       77  ImpegnatoPadre-ed     pic ----.---.--9.
       77  ImpegnatoMPadre-ed    pic ----.---.--9.

       01  filler                pic 9.
         88 trovato              value 1, false 0.

       01  filler                pic 9.
         88 errore-c             value 1, false 0.

       01  filler                pic 9.
         88 errore-d             value 1, false 0.

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

      ***---
       PROCEDURE DIVISION.
              
       DECLARATIVES.
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message box        "Impossibile procedere."
            x"0d0a""File progressivi di magazzino [PROGMAG] inesistente"
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                           icon 3
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
           move 0 to counter counter2.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input progmag progmag2.

      ***---
       ELABORAZIONE.
           move low-value to prg-rec of progmag.
           start progmag key is >= prg-chiave of progmag
                 invalid set errori to true
           end-start.
           if tutto-ok

              perform until 1 = 2
                 set trovato to false
                 read progmag next at end exit perform end-read

                 if prg-cod-magazzino of progmag = spaces and
                    prg-tipo-imballo  of progmag = spaces and
                    prg-peso          of progmag = 0
                    move prg-cod-articolo of progmag to SaveArticolo
                    read progmag next 
                         at end move 0 to prg-cod-articolo of progmag 
                    end-read
                    if prg-cod-articolo of progmag not = SaveArticolo
                       set trovato to false
                    else
                       set trovato to true
                       move high-value   to prg-chiave of progmag 
                       move SaveArticolo to prg-cod-articolo of progmag 
                       start progmag key is >= prg-chiave of progmag 
                             invalid exit perform
                       end-start
                    end-if
                    if not trovato
                       move SaveArticolo to articolo-edit
                       display message "Articolo BASE " articolo-edit
                                       " senza progressivi relativi"
                                x"0d0a""Contattare amministrazione"
                                 title "** ERRORE **"
                                  icon 2
                       set errori to true
                       exit perform
                    end-if
                 end-if
              end-perform
           end-if.


           move low-value to prg-rec of progmag.
           start progmag key is >= prg-chiave of progmag
                 invalid set errori to true
           end-start.
           if tutto-ok

              perform until 1 = 2
                 set trovato to false
                 read progmag next at end exit perform end-read

                 if prg-cod-magazzino of progmag = spaces and
                    prg-tipo-imballo  of progmag = spaces and
                    prg-peso          of progmag = 0
                    move prg-cod-articolo of progmag to SaveArticolo
                    move low-value to prg-chiave of progmag2
                    move SaveArticolo to prg-cod-articolo of progmag2
                    move 0 to Ordinato-1Figli
                    move 0 to Ordinato-2Figli
                    move 0 to Ordinato-3Figli
                    move 0 to Ordinato-4Figli
                    move 0 to Ordinato-5Figli
                    move 0 to Ordinato-6Figli
                    move 0 to ImpegnatoFigli
                    move 0 to ImpegnatoMFigli
                    move 0 to GiacenzaFigli
                    move 0 to GiacenzaFigliC
                    move 0 to VenditeFigli
                    move 0 to InizialiFigli
                    move 0 to AcquistiFigli

                    start progmag2 key >= prg-chiave of progmag2
                          invalid continue 
                    end-start

                    perform until 1 = 2
                       read progmag2 next at end exit perform end-read
                       if prg-cod-articolo of progmag2 <> SaveArticolo
                          exit perform
                       end-if
                       if prg-peso of progmag2 > 0
                          add prg-impegnato  of progmag2  
                                             to ImpegnatoFigli
                          add prg-imp-master of progmag2  
                                             to ImpegnatoMFigli
                          add prg-giacenza   of progmag2  
                                             to GiacenzaFigli
                          add prg-ordinato-1 of progmag2  
                                             to Ordinato-1Figli
                          add prg-ordinato-2 of progmag2  
                                             to Ordinato-2Figli
                          add prg-ordinato-3 of progmag2  
                                             to Ordinato-3Figli
                          add prg-ordinato-4 of progmag2  
                                             to Ordinato-4Figli
                          add prg-ordinato-5 of progmag2  
                                             to Ordinato-5Figli
                          add prg-ordinato-6 of progmag2  
                                             to Ordinato-6Figli
                          add prg-giacenza-udm   of progmag2  
                                             to GiacenzaFigliC
                          add prg-ven-valore of progmag2 
                                             to VenditeFigli
                          add prg-acq-valore of progmag2 
                                             to AcquistiFigli
                          add prg-ini-valore of progmag2 
                                             to InizialiFigli
                       end-if
                    end-perform

                    set errore-d to false
                    set errore-c to false

                    if prg-giacenza     of progmag <> GiacenzaFigli   or
                       prg-impegnato    of progmag <> ImpegnatoFigli  or
                       prg-imp-master   of progmag <> ImpegnatoMFigli or
      *****                 prg-ordinato-1    of progmag <> OrdinatoFigli  or
                       prg-ven-valore   of progmag <> VenditeFigli    or
                       prg-acq-valore   of progmag <> AcquistiFigli   or
                       prg-giacenza-udm of progmag <> GiacenzaFigliC  or
                       prg-ini-valore   of progmag <> InizialiFigli   or
                       prg-ordinato-1   of progmag <> Ordinato-1Figli or
                       prg-ordinato-2   of progmag <> Ordinato-2Figli or
                       prg-ordinato-3   of progmag <> Ordinato-3Figli or
                       prg-ordinato-4   of progmag <> Ordinato-4Figli or
                       prg-ordinato-5   of progmag <> Ordinato-5Figli or
                       prg-ordinato-6   of progmag <> Ordinato-6Figli
   
                       if prg-giacenza   of progmag <> GiacenzaFigli
                       or prg-impegnato  of progmag <> ImpegnatoFigli
                       or prg-imp-master of progmag <> ImpegnatoMFigli
                       or prg-ordinato-1 of progmag <> Ordinato-1Figli
                       or prg-ordinato-2 of progmag <> Ordinato-2Figli
                       or prg-ordinato-3 of progmag <> Ordinato-3Figli
                       or prg-ordinato-4 of progmag <> Ordinato-4Figli
                       or prg-ordinato-5 of progmag <> Ordinato-5Figli
                       or prg-ordinato-6 of progmag <> Ordinato-6Figli
                          set errore-d to true
                       end-if

                       if prg-ven-valore   of progmag <> 
                          VenditeFigli     or
                          prg-acq-valore   of progmag <> 
                          AcquistiFigli    or
                          prg-giacenza-udm of progmag <> 
                          GiacenzaFigliC   or
                          prg-ini-valore   of progmag <> 
                          InizialiFigli
                          set errore-c to true
                       end-if

                       move SaveArticolo to articolo-edit
                       perform COMPONI-MSG
                       set errori to true
                    end-if
                 end-if
              end-perform
           end-if.

      ***---
       COMPONI-MSG.
           move GiacenzaFigli    to GiacenzaFigli-ed.
           move GiacenzaFigliC   to GiacenzaFigliC-ed.
           move VenditeFigli     to VenditeFigli-ed.
           move AcquistiFigli    to AcquistiFigli-ed.
           move InizialiFigli    to InizialiFigli-ed.
           move Ordinato-1Figli  to Ordinato-1Figli-ed.
           move Ordinato-2Figli  to Ordinato-2Figli-ed.
           move Ordinato-3Figli  to Ordinato-3Figli-ed.
           move Ordinato-4Figli  to Ordinato-4Figli-ed.
           move Ordinato-5Figli  to Ordinato-5Figli-ed.
           move Ordinato-6Figli  to Ordinato-6Figli-ed.
           move ImpegnatoFigli   to ImpegnatoFigli-ed.
           move ImpegnatoMFigli  to ImpegnatoMFigli-ed.

           move prg-giacenza     of progmag to GiacenzaPadre-ed.
           move prg-giacenza-udm of progmag to GiacenzaPadreC-ed.
           move prg-ven-valore   of progmag to VenditePadre-ed.
           move prg-acq-valore   of progmag to AcquistiPadre-ed.
           move prg-ini-valore   of progmag to InizialiPadre-ed.
           move prg-ordinato-1   of progmag to Ordinato-1Padre-ed.
           move prg-ordinato-2   of progmag to Ordinato-2Padre-ed.
           move prg-ordinato-3   of progmag to Ordinato-3Padre-ed.
           move prg-ordinato-4   of progmag to Ordinato-4Padre-ed.
           move prg-ordinato-5   of progmag to Ordinato-5Padre-ed.
           move prg-ordinato-6   of progmag to Ordinato-6Padre-ed.
           move prg-impegnato    of progmag to ImpegnatoPadre-ed.
           move prg-imp-master   of progmag to ImpegnatoMPadre-ed.

           if errore-d
              move "!!!!!!!!!!  DINAMICI KO !!!!!!!!!!!!" to riga-d
           else
              move "**********  DINAMICI OK ************" to riga-d
           end-if.

           if errore-c
              move "!!!!!!!!!!  CONSOLIDATI KO !!!!!!!!!!!!" to riga-c
           else
              move "**********  CONSOLIDATI OK ************" to riga-c
           end-if.
           display message "Articolo " articolo-edit
                           " con valori non congrui:"
                    x"0d0a"riga-d
                    x"0d0a""Giacenza Padre: "  GiacenzaPadre-ed
                           " - Giacenza Figli: " GiacenzaFigli-ed
                    x"0d0a""Impegnato Padre: "  ImpegnatoPadre-ed
                           " - Impegnato Figli: " ImpegnatoFigli-ed 
                    x"0d0a""Impegnato M Padre: "  ImpegnatoMPadre-ed
                           " - Impegnato M Figli: " ImpegnatoMFigli-ed 
                    x"0d0a""Ordinato Padre: "  Ordinato-1Padre-ed
                           " - Ordinato 1 Figli: " Ordinato-1Figli-ed
                    x"0d0a""Ordinato Padre: "  Ordinato-2Padre-ed
                           " - Ordinato 2 Figli: " Ordinato-2Figli-ed
                    x"0d0a""Ordinato Padre: "  Ordinato-3Padre-ed
                           " - Ordinato 3 Figli: " Ordinato-3Figli-ed
                    x"0d0a""Ordinato Padre: "  Ordinato-4Padre-ed
                           " - Ordinato 4 Figli: " Ordinato-4Figli-ed
                    x"0d0a""Ordinato Padre: "  Ordinato-5Padre-ed
                           " - Ordinato 5 Figli: " Ordinato-5Figli-ed
                    x"0d0a""Ordinato Padre: "  Ordinato-6Padre-ed
                           " - Ordinato 6 Figli: " Ordinato-6Figli-ed
                    x"0d0a"" "
                    x"0d0a"riga-c
                    x"0d0a""Giacenza C Padre: "  GiacenzaPadreC-ed
                           " - Giacenza C Figli: " GiacenzaFigliC-ed
                    x"0d0a""Vendite Padre: "   VenditePadre-ed
                           " - Vendite Figli: "  VenditeFigli-ed
                    x"0d0a""Acquisti Padre: "  AcquistiPadre-ed
                           " - Acquisti Figli: " AcquistiFigli-ed
                    x"0d0a""Iniziali Padre: "  InizialiPadre-ed
                           " - Iniziali Figli: " InizialiFigli-ed
                     title "** ERRORE **"
                      icon 2.

      ***---
       CLOSE-FILES.
           close progmag progmag2.

      ***---
       ACCEPT-FORMAT-TIME.
           accept como-ora from time.
           initialize como-ora-edit.
           string como-ora(1:2) delimited size
                  ":"           delimited size
                  como-ora(3:2) delimited size
                  "'"           delimited size
                  como-ora(5:2) delimited size
                  x"22"         delimited size
                  into como-ora-edit
           end-string.

      ***---
       EXIT-PGM.
           if tutto-ok
              display message "Nessun errore riscontrato"
                        title "Controllo progressivi"
           end-if.
           goback.
