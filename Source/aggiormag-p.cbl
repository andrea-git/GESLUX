       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggiormag-p.
       AUTHOR.                          Andrea.
       REMARKS. Estrazione e calcolo scritture ausiliarie
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tmagaz.sl".
           copy "tcaumag.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "tnomen.sl".
           copy "giormag.sl".
           copy "tmp-giormese.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tmagaz.fd".
           copy "tcaumag.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "tnomen.fd".
           copy "giormag.fd".
           copy "tmp-giormese.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * FILE STATUS 
       77  status-tparamge       pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tmagaz         pic xx.
       77  status-tcaumag        pic xx.
       77  status-progmag        pic xx.
       77  status-articoli       pic xx.
       77  status-tnomen         pic xx.
       77  status-giormag        pic xx.
       77  status-tmp-giormese   pic xx.
       77  path-tmp              pic x(256).

      * COSTANTI
       78  titolo                value "Scritture ausiliarie NON UTF".
       78  OgniQuanti            value 50.
       
      * VARIABILI 
       77  scelta                pic 9(8).
       77  como-valore           pic 9(8).
       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).
       77  giorno                pic 9(2).
       77  mese                  pic 9(2).  
       77  anno                  pic 9(4).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  check-data            pic 9(8).
       77  mese-esteso           pic x(12).
       77  msg                   pic x(25).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 record-ok          value 1, false 0.

       LINKAGE SECTION.
       77  link-user             pic x(10). 
       77  link-data             pic 9(8).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION USING  link-user 
                                 link-data 
                                 link-handle.

       DECLARATIVES.
      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "39"
                set errori to true
                display message "File [TPARAMGE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
              x"0d0a""File testata di magazzino [TPARAMGE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set errori to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
              x"0d0a""File testata di magazzino [TMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set errori to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
              x"0d0a""File [RMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set errori to true
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
           when "99" set errori to true
           end-evaluate.
                       
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
              x"0d0a""File [TCAUMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set errori to true
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
            x"0d0a""File progressivi di magazzino [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set errori to true
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
           when "99" set errori to true
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
           when "99" set errori to true
           end-evaluate.         
                       
      ***---
       GIORMAG-ERR SECTION.
           use after error procedure on giormag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-giormag
           when "39"
                set errori to true
                display message "File [GIORMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[GIORMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
              x"0d0a""File [GIORMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.         
                       
      ***---
       TMP-GIORMESE-ERR SECTION.
           use after error procedure on tmp-giormese.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-giormese
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-GIORMESE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-GIORMESE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-GIORMESE] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           move 0 to counter counter2.
           set tutto-ok        to true.
      *****     set trovato         to false.     
           initialize path-tmp.
           accept  path-tmp    from environment "PATH-ST".
           accept  como-data   from century-date.
           accept  como-ora    from time.
           inspect path-tmp    replacing trailing spaces by low-value.
           string  path-tmp    delimited low-value
                   "aggiormag" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".tmp"      delimited size
                   into path-tmp
           end-string.

      ***---
       OPEN-FILES.
           open input tmovmag
                      rmovmag
                      tcaumag
                      articoli
                      progmag
                      tnomen
                      tmagaz
                      tparamge.
           if tutto-ok
              perform OPEN-CREATE-GIORMAG-LOCK
              if tutto-ok
                 perform OPEN-OUTPUT-TMP-GIORMESE
                 if errori
                    close tmovmag
                          rmovmag
                          tcaumag
                          articoli
                          progmag
                          tnomen
                          giormag
                          tmagaz
                          tparamge
                 end-if
              else
                 close tmovmag
                       rmovmag
                       tcaumag
                       articoli
                       progmag
                       tnomen
                       tmagaz
                       tparamge
              end-if
           end-if.

           if errori goback end-if.

      ***---
       OPEN-CREATE-GIORMAG-LOCK.
           initialize geslock-linkage.
           move "giormag" to geslock-nome-file.

           set tutto-ok  to true.
           set RecLocked to false.
           open output giormag.
           if RecLocked
              move "Operazione già in corso su altro terminale." 
                to geslock-messaggio
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-CREATE-GIORMAG-LOCK
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           else
              close giormag
              open i-o giormag allowing readers
           end-if.                 

      ***---
       OPEN-OUTPUT-TMP-GIORMESE.
           initialize geslock-linkage.
           move "File TMP" to geslock-nome-file.

           set tutto-ok  to true.
           set RecLocked to false.
           open output tmp-giormese.
           if RecLocked
              move "Operazione già in corso su altro terminale." 
                to geslock-messaggio
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-OUTPUT-TMP-GIORMESE
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           else
              close    tmp-giormese
              open i-o tmp-giormese allowing readers
           end-if.

      ***---
       ELABORAZIONE.
           perform CREA-GIACENZE-INIZIO-ANNO.
           perform CREA-MOVIMENTAZIONI.

      ***---
       CREA-GIACENZE-INIZIO-ANNO.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                                                   "
              upon link-handle at column 01 line 03.
      *    FASE 1: Creo le giacenze iniziali per
      *            Gennaio per tutti i progmag FIGLI
           move low-value to prg-rec.
           start progmag key >= prg-chiave
                 invalid display message "NO DATA ON PROGMAG!"
                                    title titolo
                                     icon 3
             not invalid
                 perform until 1 = 2


                    add 1 to counter
                    add 1 to counter2
                    if counter2 = OgniQuanti
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 33
                                                line 03
                       move 0 to counter2
                       if counter = OgniQuanti
                          display "GIAC INIZIALI TMP"
                             upon link-handle at column 13
                                                   line 03
                       end-if
                    end-if


                    read progmag next no lock at end exit perform
                    end-read
                    set record-ok to false
                    if prg-cod-magazzino not = spaces and
                       prg-tipo-imballo  not = spaces and
                       prg-peso          not = 0
LUBEXX                 move prg-cod-magazzino to mag-codice
LUBEXX                 read tmagaz no lock 
LUBEXX                      invalid
LUBEXX                      display message "Magazzino "
LUBEXX                                      prg-cod-magazzino,
LUBEXX                                      " NON valido"
LUBEXX                                title titolo
LUBEXX                                 icon 3
LUBEXX                  not invalid
LUBEXX                      if mag-solo-no-utf or mag-tutte
LUBEXX                         move prg-cod-articolo to art-codice
LUBEXX                         read articoli no lock
LUBEXX                              invalid 
LUBEXX                              display message 
LUBEXX                                      "Progressivo magazzino "
LUBEXX                                      prg-cod-articolo
LUBEXX                                      "non presente in anagrafica"
LUBEXX                                        title titolo
LUBEXX                                         icon 3
LUBEXX                          not invalid
LUBEXX                              if mag-solo-no-utf
LUBEXX                                 move art-cod-doganale 
LUBEXX                                   to nom-codice
LUBEXX                                 read tnomen no lock
LUBEXX                                      invalid 
LUBEXX                                      display message 
LUBEXX                                      "Nomenclatura articolo"
LUBEXX                                      " non valida"
LUBEXX                                                title titolo
LUBEXX                                                 icon 3
LUBEXX                                  not invalid
LUBEXX                                      if nom-no-utf
LUBEXX                                         set record-ok to true
LUBEXX                                      end-if
LUBEXX                                 end-read
LUBEXX                              else
LUBEXX                                 set record-ok to true
LUBEXX                              end-if
LUBEXX                         end-read
LUBEXX                      end-if
LUBEXX                 end-read
LUBEXX              end-if

LUBEXX*****                       move prg-cod-articolo to art-codice
LUBEXX*****                       read articoli no lock
LUBEXX*****                            invalid 
LUBEXX*****                            display message "Progressivo magazzino "
LUBEXX*****                                            prg-cod-articolo
LUBEXX*****                                            "non presente in anagrafica"
LUBEXX*****                                      title titolo
LUBEXX*****                                       icon 3
LUBEXX*****                        not invalid
LUBEXX*****                            move art-cod-doganale to nom-codice
LUBEXX*****                            read tnomen no lock
LUBEXX*****                                 invalid 
LUBEXX*****                                 display message "Nomenclatura articolo"
LUBEXX*****                                                 " non valida"
LUBEXX*****                                           title titolo
LUBEXX*****                                            icon 3
LUBEXX*****                             not invalid
LUBEXX*****                                 if nom-no-utf
LUBEXX*****                                    move prg-cod-magazzino to mag-codice
LUBEXX*****                                    read tmagaz no lock
LUBEXX*****                                         invalid
LUBEXX*****                                         display message 
LUBEXX*****                                                 "Magazzino "
LUBEXX*****                                                 prg-cod-magazzino,
LUBEXX*****                                                 " NON valido"
LUBEXX*****                                                   title titolo
LUBEXX*****                                                    icon 3
LUBEXX*****                                     not invalid
LUBEXX*****                                         if mag-si-scritture
LUBEXX*****                                            set record-ok to true
LUBEXX*****                                         end-if
LUBEXX*****                                    end-read
LUBEXX*****                                 end-if
LUBEXX*****                            end-read
LUBEXX*****                       end-read
LUBEXX*****                    end-if
                    if record-ok
                       |Raggruppo per magazzino/mese
                       initialize tmg-rec 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move 01         to tmg-mese
                       move mag-codice to tmg-mag
                       move art-codice to tmg-articolo
                       read tmp-giormese no lock
                            invalid continue
                       end-read
                       move art-unita-di-misura to tmg-um
                       add  prg-ini-udm         to tmg-qta
                       write tmg-rec invalid rewrite tmg-rec end-write
                    end-if
                 end-perform
           end-start.

           set tutto-ok to true.
           move spaces to tge-codice.
           read tparamge no lock 
                invalid display message "NO DATA ON TPARAMGE"
                                   title titolo
                                    icon 3
                        set errori to true
           end-read.

           if tutto-ok
              move 0 to counter counter2
              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                                                "
                 upon link-handle at column 01 line 03
              |Travaso il tutto all'interno del file effettivo
              move low-value to tmg-rec
              start tmp-giormese key >= tmg-chiave
                    invalid 
                    display message "NO DATA ON TMP-GIORMESE!"
                              title titolo
                               icon 3
                not invalid
                    perform until 1 = 2

                       add 1 to counter
                       add 1 to counter2
                       if counter2 = OgniQuanti
                          move counter to counter-edit
                          display counter-edit
                             upon link-handle at column 33
                                                   line 03
                          move 0 to counter2
                          if counter = OgniQuanti
                             display "GIAC INIZIALI EFF"
                                upon link-handle at column 13
                                                      line 03
                          end-if
                       end-if

                       read tmp-giormese next 
                            at end exit perform 
                       end-read
                       initialize gio-rec 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move tmg-mag      to gio-mag
                       move tmg-articolo to gio-art
                       string link-data(1:4) delimited size
                              "0101"         delimited size
                              into gio-data
                       end-string
                       move 1                    to gio-prog
                       set  gio-iniziale         to true
                       move tmg-um               to gio-um
                       move tmg-qta              to gio-qta
                       set  gio-tipo-C           to true
                       move tge-cliente-corrisp  to gio-cod-clifor
                       perform VALORIZZA-DATI-COMUNI
                       write gio-rec 
                             invalid
                             display message "Giac. iniziale articolo "
                                            tmg-articolo," già presente"
                                             " su file GIORMAG!"
                                       title titolo
                                        icon 3
                       end-write
                    end-perform
              end-start
           end-if.

      ***---
      * FASE 2: Creo le movimentazioni giornaliere in base ai movmag
       CREA-MOVIMENTAZIONI.
           move 0 to mese.
           perform until mese = 12
              add 1 to mese
              string link-data(1:4) delimited size
                     mese           delimited size
                     "01"           delimited size
                     into check-data
              end-string
              if check-data > link-data exit perform end-if
              move check-data to tmo-data-movim
              move low-value  to tmo-numero
              start tmovmag key >= k-data
                    invalid continue
                not invalid
                    perform CREA-MOVIMENTAZIONI-MESE
              end-start
              perform CREA-MOVIMENTAZIONE-FINE-MESE-INIZIO-MESE-DOPO
           end-perform.

      ***---
       CREA-MOVIMENTAZIONI-MESE.                    
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                                                   "
              upon link-handle at column 01 line 03.
           evaluate mese
           when 01  move "GENNAIO"   to mese-esteso
           when 02  move "FEBBRAIO"  to mese-esteso
           when 03  move "MARZO"     to mese-esteso
           when 04  move "APRILE"    to mese-esteso
           when 05  move "MAGGIO"    to mese-esteso
           when 06  move "GIUGNO"    to mese-esteso
           when 07  move "LUGLIO"    to mese-esteso
           when 08  move "AGOSTO"    to mese-esteso
           when 09  move "SETTEMBRE" to mese-esteso
           when 10  move "OTTOBRE"   to mese-esteso
           when 11  move "NOVEMBRE"  to mese-esteso
           when 12  move "DICEMBRE"  to mese-esteso
           end-evaluate.
           inspect mese-esteso replacing trailing spaces by low-value.
           initialize msg.
           string "MOVIM. MESE " delimited size
                  mese-esteso    delimited low-value
                  into msg
           end-string.
           perform until 1 = 2

              add 1 to counter
              add 1 to counter2
              if counter2 = OgniQuanti
                 move counter to counter-edit
                 display counter-edit upon link-handle at column 33
                                                            line 03
                 move 0 to counter2
                 if counter = OgniQuanti
                    display msg upon link-handle at column 10
                                                      line 03
                 end-if
              end-if
                      
              initialize tca-rec mag-rec 
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              read tmovmag next at end          exit perform end-read
              if tmo-data-movim(5:2) not = mese exit perform end-if
              move tmo-causale to tca-codice
              read tcaumag no lock 
                   invalid 
                   display message "Causale " tca-codice " su movimento"
                                   " #" tmo-numero " non presente su"
                                   " file TCAUMAG"
                             title titolo
                              icon 3
              end-read
              if tca-movim-giac-periodo-pos or
                 tca-movim-giac-periodo-neg
                 move tca-cod-magaz to mag-codice
                 read tmagaz  no lock 
                      invalid
                      display message "Magazzino " mag-codice " su "
                                      "movimento #" tmo-numero " non "
                                      "presente su file TCAUMAG"
                                title titolo
                                 icon 3
                 end-read
LUBEXX*****                 if mag-si-scritture
LUBEXX           if mag-tutte or mag-solo-no-utf
                    move tmo-anno   to rmo-anno
                    move tmo-numero to rmo-movim
                    move low-value  to rmo-riga
                    start rmovmag key >= rmo-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             set record-ok to false
                             read rmovmag next 
                                  at end exit perform 
                             end-read
                             if rmo-anno  not = tmo-anno or
                                rmo-movim not = tmo-numero
                                exit perform 
                             end-if
                             if rmo-qta > 0
                                move rmo-articolo to art-codice
                                read articoli no lock 
                                     invalid 
                                     display message 
                                     "Articolo " art-codice, " su riga "
                                     "movimento #" rmo-movim, "non "
                                     "presente su anagrafica"
                                               title titolo
                                                icon 3
                                 not invalid
LUBEXX                               if mag-solo-no-utf      
                                        move art-cod-doganale 
                                          to nom-codice
                                        read tnomen no lock 
                                             invalid 
                                             display message
                                             "Nomenclatura NON valida"
                                             " su anagrafica"
                                                       title titolo
                                                        icon 3
                                         not invalid
                                             if nom-no-utf
                                                set record-ok to true
                                             end-if
                                        end-read
LUBEXX                               else
LUBEXX                                  set record-ok to true
LUBEXX                               end-if
                                end-read
                             end-if
                             if record-ok
                                perform ALIMENTA-GIORMAG
                             end-if
                          end-perform
                    end-start
                 end-if
              end-if
           end-perform.

      ***---
       ALIMENTA-GIORMAG.
           move mag-codice     to gio-mag.
           move rmo-articolo   to gio-art.
           move tmo-data-movim to gio-data.
           move 1              to gio-prog.
           perform until 1 = 2
              read giormag no lock 
                  |Ho trovato il primo prog libero
                   invalid exit perform
               not invalid
                   add 1 to gio-prog
              end-read
           end-perform.
           |SOMMO LA QTA AL TMP PER CALCOLARE LA GIACENZA FINALE
           initialize tmg-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move mese    to tmg-mese.
           move gio-mag to tmg-mag.
           move gio-art to tmg-articolo.
           read tmp-giormese no lock 
                invalid continue
           end-read.

           if tca-movim-giac-periodo-pos add      rmo-qta   to tmg-qta
           else                          subtract rmo-qta from tmg-qta
           end-if.

           move art-unita-di-misura  to tmg-um.
           rewrite tmg-rec invalid write tmg-rec end-rewrite.
           |SCRIVO LA MOVIMENTAZIONE
           initialize gio-dati replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if tca-movim-giac-periodo-pos
              set gio-entrata to true
           else
              set gio-uscita  to true
           end-if.
           move tca-codice           to gio-causale.
           move art-unita-di-misura  to gio-um.
           move rmo-qta              to gio-qta.
           move tmo-numdoc-clifor    to gio-num-doc.
           move tmo-tipo             to gio-tipo-CF.
           move tmo-cod-clifor       to gio-cod-clifor.
           perform VALORIZZA-DATI-COMUNI.
           write gio-rec
                 invalid display message 
                                 "Magazzino " gio-mag
                          x"0d0a""Articolo "  gio-art
                          x"0d0a""Data "      gio-data
                          x"0d0a""Prog. "     gio-prog
                          x"0d0a""già presente su GIORMAG"
                                   title titolo
                                    icon 3
           end-write.

      ***---
      * FASE 3: Creo le movimentazioni per fine mese e quella 
      *         iniziale per il mese successivo coi medesimi valori
       CREA-MOVIMENTAZIONE-FINE-MESE-INIZIO-MESE-DOPO.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                                                   "
              upon link-handle at column 01 line 03.

           initialize msg.
           string "GIAC. FINALI " delimited size
                  mese-esteso     delimited low-value
                  into msg
           end-string.

           move low-value to tmg-rec.
           move mese      to tmg-mese.
           start tmp-giormese key >= tmg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                     add 1 to counter
                     add 1 to counter2
                     if counter2 = OgniQuanti
                        move counter to counter-edit
                        display counter-edit
                           upon link-handle at column 33
                                                 line 03
                        move 0 to counter2
                        if counter = OgniQuanti
                           display msg
                              upon link-handle at column 10
                                                    line 03
                        end-if
                     end-if

                    read tmp-giormese next at end exit perform end-read
                    if tmg-mese not = mese exit perform end-if
                    |Record fine mese
                    initialize gio-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move tmg-mag      to gio-mag
                    move tmg-articolo to gio-art
                    perform VALORIZZA-GIORNO
                    string link-data(1:4) delimited size
                           mese           delimited size
                           giorno         delimited size
                           into gio-data
                    end-string
                    move all "9"    to gio-prog
                    set  gio-finale to true
                    move tmg-um     to gio-um
                    move tmg-qta    to gio-qta
                    set  gio-tipo-C to true
                    move tge-cliente-corrisp  to gio-cod-clifor
                    perform VALORIZZA-DATI-COMUNI
                    write gio-rec  invalid continue end-write
                    |Record inizio mese successivo...
                    add   1 to mese giving gio-mm
                    move  1 to gio-gg
                    |... se all'interno del periodo da considerarsi
                    |altrimenti non mi serve
                    if gio-data <= link-data
                       set gio-iniziale to true
                       move 1 to gio-prog
                       write gio-rec
                             invalid
                             display message "Giac. iniziale mese "
                                             "successivo " mese, 
                                             " già presente su GIORMAG"
                                       title titolo
                                        icon 3
                       end-write
                       add 1 to mese giving tmg-mese
                       write tmg-rec
                             invalid
                             display message "Giac. iniziale mese "
                                             "successivo " mese, 
                                             " già presente su TMP"
                                       title titolo
                                        icon 3
                       end-write
                       move mese to tmg-mese
                       start tmp-giormese key = tmg-chiave end-start
                       read  tmp-giormese next
                    end-if
                 end-perform
           end-start.

      ***---
       VALORIZZA-GIORNO.
           evaluate tmg-mese
           when 01  move 31 to giorno
           when 02  move link-data(1:4)  to anno
                    divide   anno        by 4 giving como-valore
                    multiply como-valore by 4 giving como-valore
                    if gio-aaaa = como-valore
                       move 29 to giorno
                    else
                       move 28 to giorno
                    end-if
           when 03  move 31 to giorno
           when 04  move 30 to giorno
           when 05  move 31 to giorno
           when 06  move 30 to giorno
           when 07  move 31 to giorno
           when 08  move 31 to giorno
           when 09  move 30 to giorno
           when 10  move 31 to giorno
           when 11  move 30 to giorno
           when 12  move 31 to giorno
           end-evaluate.

      ***---
       VALORIZZA-DATI-COMUNI.
           move   link-user          to gio-utente-creazione.
           accept gio-data-creazione from century-date.
           accept gio-ora-creazione  from time.

      ***--
       CLOSE-FILES.
           close tmovmag
                 rmovmag
                 tcaumag
                 articoli
                 progmag
                 tnomen
                 tmagaz
                 giormag
                 tmp-giormese
                 tparamge.

      ***---
       EXIT-PGM.
           delete file tmp-giormese.
           goback.
