       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      extmov-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl". 
           copy "rmovmag.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "tmagaz.sl".
           copy "tnomen.sl".
           copy "tmovtrat.sl".
           copy "movutf.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".
      *     copy "tedi.sl".
      *     copy "redi.sl".
      *     copy "tmp-redi.sl".
      *     copy "prodener.sl".
      *     copy "paramedi.sl".
      *     copy "tnazioni.sl".
      *     copy "anautf.sl".

      *****************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd". 
           copy "rmovmag.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "tmagaz.fd".
           copy "tnomen.fd".
           copy "tmovtrat.fd".
           copy "movutf.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
      *     copy "tedi.fd".
      *     copy "redi.fd".
      *     copy "tmp-redi.fd".
      *     copy "prodener.fd".
      *     copy "paramedi.fd".
      *     copy "tnazioni.fd".
      *     copy "anautf.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       77  status-tmovmag          pic x(2).
       77  status-rmovmag          pic x(2).
       77  status-articoli         pic x(2).
       77  status-tcaumag          pic x(2).
       77  status-tmagaz           pic x(2).
       77  status-tnomen           pic x(2).
       77  status-tmovtrat         pic x(2).
       77  status-movutf           pic x(2).
       77  status-clienti          pic x(2).
       77  status-ttipocli         pic x(2).
      * 77  status-tedi             pic x(2).
      * 77  status-redi             pic x(2).
      * 77  status-tmp-redi         pic x(2).
      * 77  status-prodener         pic x(2).
      * 77  status-paramedi         pic x(2).
      * 77  status-tnazioni         pic x(2).
      * 77  status-anautf           pic x(2).

       77  path-tmp-redi           pic x(256).

       77  como-valore             pic s9(15)v999.

       01  tmp-occurs              occurs 999 indexed by occ-idx.
           05 el-reg-col.
              10 el-num-reg        pic 9(8).
              10 el-col            pic x.
           05 el-kg                pic s9(12)v999.
           05 el-prog              pic 9(10).

       01  como-reg-col.
           05 como-num-reg         pic 9(8).
           05 como-col             pic x.

       01  PrimaChiave.
           05 primo-anno           pic 9(4).
           05 primo-numero         pic 9(8).

       78  titolo    value "Estrazione movimenti UTF".

       01  filler                  pic 9.
         88 errore-tmovtrat        value 1, false 0.

       01  filler                  pic 9.
         88 trasferisci            value 1, false 0.

       01  filler                  pic 9.
         88 trovato-registro       value 1, false 0.

       01  filler                  pic 9.
         88 record-ok              value 1, false 0.

       77  counter                 pic 9(10).
       77  counter2                pic 9(10).
       77  counter-edit            pic z(10).

       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).

       01                          pic 9.
           88 peso-mod             value zero false 1.

       77  peso-calcolato          pic 9(11)V9(3).
       77  dif-peso                pic s9(11)V9(3).
       77  discordanza             pic s9(11)V9(3).

       01  como-rmo-chiave.
           10 como-rmo-anno         PIC  9(4).
           10 como-rmo-movim        PIC  9(8).
           10 como-rmo-riga         PIC  9(5).

       01                          pic 9.
           88 si-edi               value 1 false zero.
           88 no-edi               value zero false 1.

       01  tipo-ope                pic 9.
           88 controllo-peso       value 1.
           88 valorizza-edi        value 2.

       01                          pic 9.
           88 SI-MOVUTF            value 1 false zero.

       01                          pic 9.
           88 fornitore-estero     value 1 false zero.

       77  tmo-numdoc-clifor-ed    pic z(18).

       77  num-righe               pic 9(5).

       77  como-peso               PIC  9(5)v999.
       77  como-peso-imp           PIC  9(5)v999.

       77  como-imposta            pic 9(9)V9(3).
       77  como-imposta-2dec       pic 9(9)V9(2).

       LINKAGE SECTION.
       copy "link-extmov.def".

      ******************************************************************
       PROCEDURE DIVISION using extmov-linkage.

       DECLARATIVES.

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "File [TMOVMAG] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "File [RMOVMAG] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "File [TCAUMAG] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed [ARTICOLI] file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "File [CLIENTI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed [CLIENTI] file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TTIPOCLI-ERR SECTION.
           use after error procedure on TTIPOCLI.
           set tutto-ok  to true.
           evaluate status-TTIPOCLI
           when "35"
                display message "File [TTIPOCLI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed [TTIPOCLI] file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "35"
                display message "File [TMAGAZ] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TNOMEN-ERR SECTION.
           use after error procedure on tnomen.
           set tutto-ok  to true.
           evaluate status-tnomen
           when "35"
                display message "File [TNOMEN] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TMOVTRAT-ERR SECTION.
           use after error procedure on tmovtrat.
           set tutto-ok  to true.
           evaluate status-tmovtrat
           when "35"
                display message "File [TMOVTRAT] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TMOVTRAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVTRAT] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "TMOVTRAT"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o tmovtrat
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99"
                initialize geslock-messaggio
                string "Il record per l'anno: " tra-anno " risulta"
                x"0d0a""già in uso su altro terminale."   delimited size
                x"0d0a""Questo comporta l'impossibilità"  delimited size
                x"0d0a""ad aggiornare la tabella l'ultimo"delimited size
                x"0d0a""movimento elaborato."             delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "TMOVTRAT"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     read tmovtrat lock
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set tutto-ok  to true.
           evaluate status-movutf
           when "35"
                display message "File [MOVUTF] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TMOVUTF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVUTF] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "MOVUTF"     to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o movutf allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      * TEDI-ERR SECTION.
      *     use after error procedure on tedi.
      *     set tutto-ok  to true.
      *     evaluate status-tedi
      *     when "35"
      *          display message "File [TEDI] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [TEDI] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[TEDI] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *          initialize geslock-messaggio
      *          string   "File già in uso!"
      *            x"0d0a""Impossibile procedere!" delimited size
      *                into geslock-messaggio
      *          end-string
      *          move 1 to geslock-v-riprova
      *          move 0 to geslock-v-ignora
      *          move 1 to geslock-v-termina
      *          move   "TEDI"    to geslock-nome-file
      *          call   "geslock" using geslock-linkage
      *          cancel "geslock"
      *          evaluate true
      *          when riprova
      *               open i-o tedi allowing readers
      *          when termina
      *               set errori to true
      *               display message "Operazione interrotta!"
      *                         title titolo
      *                          icon 2
      *          end-evaluate
      *     when "99"
      *          initialize geslock-messaggio
      *          string "Il record per l'anno: " tedi-anno 
      *                 " mese: "   tedi-mese                
      *          " risulta"
      *          x"0d0a""già in uso su altro terminale."   delimited size
      *          x"0d0a""Questo comporta l'impossibilità"  delimited size
      *          x"0d0a""ad aggiornare i dati EDI."        delimited size
      *                into geslock-messaggio
      *          end-string
      *          move 1 to geslock-v-riprova
      *          move 0 to geslock-v-ignora
      *          move 1 to geslock-v-termina
      *          move   "TEDI"   to geslock-nome-file
      *          call   "geslock" using geslock-linkage
      *          cancel "geslock"
      *          evaluate true
      *          when riprova
      *               read tedi lock
      *          when termina
      *               set errori to true
      *               display message "Operazione interrotta!"
      *                         title titolo
      *                          icon 2
      *          end-evaluate
      *     end-evaluate.
      *
      * REDI-ERR SECTION.
      *     use after error procedure on redi.
      *     set tutto-ok  to true.
      *     evaluate status-redi
      *     when "35"
      *          display message "File [REDI] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [REDI] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[REDI] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 
      *
      * TMP-REDI-ERR SECTION.
      *     use after error procedure on tmp-redi.
      *     set tutto-ok  to true.
      *     evaluate status-tmp-redi
      *     when "35"
      *          display message "File [TMP-REDI] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [TMP-REDI] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[TMP-REDI] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 

      * PRODENER-ERR SECTION.
      *     use after error procedure on prodener.
      *     set tutto-ok  to true.
      *     evaluate status-prodener
      *     when "35"
      *          display message "File [PRODENER] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [PRODENER] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[PRODENER] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 

      * PARAMEDI-ERR SECTION.
      *     use after error procedure on paramedi.
      *     set tutto-ok  to true.
      *     evaluate status-paramedi
      *     when "35"
      *          display message "File [PARAMEDI] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [PARAMEDI] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[PARAMEDI] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 

      * TNAZIONI-ERR SECTION.
      *     use after error procedure on TNAZIONI.
      *     set tutto-ok  to true.
      *     evaluate status-TNAZIONI
      *     when "35"
      *          display message "File [TNAZIONI] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [TNAZIONI] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[TNAZIONI] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 

      * ANAUTF-ERR SECTION.
      *     use after error procedure on ANAUTF.
      *     set tutto-ok  to true.
      *     evaluate status-ANAUTF
      *     when "35"
      *          display message "File [ANAUTF] not found!"
      *                    title titolo
      *                     icon 3
      *          set errori to true
      *     when "39"
      *          set errori to true
      *          display message "File [ANAUTF] mismatch size!"
      *                    title titolo
      *                     icon 3
      *     when "98"
      *          set errori to true
      *          display message "[ANAUTF] Indexed file corrupt!"
      *                    title titolo
      *                     icon 3 
      *     when "93"
      *     when "99"
      *          set RecLocked to true
      *          set errori    to true
      *     end-evaluate. 

       END DECLARATIVES.

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
           move 0              to counter counter2.
           set RecLocked       to false.
           set tutto-ok        to true.
           set trovato         to false.
           set errore-tmovtrat to false.
           accept esercizio-x  from environment "ESERCIZIO".
           move   esercizio-x  to esercizio.

           accept path-tmp-redi from environment "PATH_ST".
           inspect path-tmp-redi   replacing trailing space by low-value
           accept como-data  from century-date
           accept como-ora   from time.

           string path-tmp-redi delimited by low-value
                  "tmp_redi_"   delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  into path-tmp-redi
           inspect path-tmp-redi  replacing trailing low-value by space.


      ***---
       OPEN-FILES.
           perform OPEN-IO-MOVUTF-LOCK.
      *     if tutto-ok
      *        perform OPEN-IO-TEDI
      *     end-if

           if tutto-ok
              perform OPEN-IO-TMOVTRAT
      *        perform OPEN-IO-REDI
      *        perform OPEN-IO-TMP-REDI
              if tutto-ok
                 open input tmovmag
                            rmovmag
                            articoli
                            tcaumag
                            tmagaz
                            tnomen
                            clienti
                            ttipocli
      *                      prodener
      *                      paramedi
      *                      tnazioni
      *                      anautf
      *                      timposte
                 if errori
                    close movutf tmovtrat
                 end-if
              else
                 close movutf
              end-if
           end-if.

      ***---
       OPEN-IO-MOVUTF-LOCK.
           open i-o movutf allowing readers.

      ***---
       OPEN-IO-TMOVTRAT.
           open i-o tmovtrat.

      ****---
      * OPEN-IO-TEDI.
      *     open i-o tedi allowing readers.

      ****---
      * OPEN-IO-REDI.
      *     open i-o redi.

      ****---
      * OPEN-IO-TMP-REDI.
      *     open output tmp-redi
      *     close tmp-redi.
      *     open i-o tmp-redi.

      ***---
       ELABORAZIONE. 
      *    metto in linea i parametri generali
      *     move space  to pae-codice
      *     read PARAMEDI
      *        invalid
      *           continue
      *     end-read

           move esercizio to tra-anno
           read tmovtrat  no lock 
                invalid   set errori          to true 
                          set errore-tmovtrat to true 
           end-read.
           if tutto-ok
              move low-value       to tmo-chiave
              move tra-anno        to tmo-anno
              move tra-ult-mov-utf to tmo-numero
              start tmovmag key is >  tmo-chiave
                 invalid 
                    set errori to true
              end-start
           end-if.

           if tutto-ok
              perform until 1 = 2
                 set  record-ok  to false
                 read tmovmag next at end    
                    exit perform 
                 end-read
                 if tmo-anno not = esercizio 
                    exit perform 
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon extmov-handle at column 11
                                               line 03
                    move 0 to counter2
                 end-if

                 if extmov-mov-elab-da = 0
                    move tmo-numero to extmov-mov-elab-da
                 end-if
                 move tmo-numero to extmov-mov-elab-a

                 if tmo-anno  not = tra-anno exit perform end-if
                 move tmo-causale to tca-codice
                 read tcaumag no  lock
                    invalid 
                       continue
                    not invalid
                       if tca-si-utf
                          if tca-movim-giac-pos or
                             tca-movim-giac-neg
                             set record-ok to true
                          end-if
                       end-if
                 end-read
                 if record-ok
                    if tmo-cliente
                       |Recupero la tipologia clienti per sapere
                       |a che serie di bolle appartiene
                       set cli-tipo-C to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock invalid continue end-read

                       move cli-tipo to tcl-codice
                       read ttipocli no lock invalid continue end-read
                    else
                       set cli-tipo-F to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock invalid continue end-read
                      
                       |Come da indicazione di Mori (19/01/09). 
                       |I movimenti di scarico fornitore (ad esempio 
                       |resi) devono essere trattati come bolle serie 1.
                       move 1 to tcl-serie-bolle
                    end-if
      *    
                    perform LOOP-RIGHE
                 end-if
              end-perform
           end-if.

           if not trovato
              if errore-tmovtrat
                 display message "Tabella movimenti da trasferire"
                          x"0d0a""non presente per l'anno utilizzato!"
                           title titolo
                            icon 2
              else
                 display message "Nessun movimento da elaborare!"
                           title titolo
                            icon 2
              end-if
           else
              perform READ-TMOVTRAT-LOCK
              if tutto-ok
                 move extmov-mov-elab-a to tra-ult-mov-utf
                 rewrite tra-rec invalid continue end-rewrite
              end-if
           end-if.

      ***---
       LOOP-RIGHE.
           set  SI-MOVUTF    to false
           set  tutto-ok     to true.
           move tmo-anno     to rmo-anno.
           move tmo-numero   to rmo-movim.
           move low-value    to rmo-riga.
           start rmovmag   key is >= rmo-chiave
              invalid   
                 set errori to true
           end-start.
           if tutto-ok
              perform INITIALIZE-OCCURS
              perform until 1 = 2
                 set  record-ok  to false
                 read rmovmag  next 
                    at end 
                       exit perform 
                 end-read
                 if   rmo-anno  not = tmo-anno or 
                      rmo-movim not = tmo-numero
                      exit perform
                 end-if
                 move rmo-codmag  to mag-codice
                 read tmagaz no lock
                    invalid 
                       continue
                    not invalid
                       if mag-si-utf
                          set record-ok to true
                       end-if
                 end-read
                 if record-ok
                    set record-ok     to false
                    move rmo-articolo to art-codice
                    read articoli no lock
                         invalid  continue
                     not invalid
                         move art-cod-doganale to nom-codice
                         read tnomen  no  lock
                              invalid continue
                          not invalid
                              if nom-si-utf
                                 set record-ok to true
                              end-if
                         end-read
                    end-read
                 end-if
                 if record-ok
LUBEXX              set trasferisci to false
LUBEXX              if tca-movim-giac-pos
LUBEXX                 if tmo-peso-utf not = 0
LUBEXX                    set trasferisci to true
LUBEXX                 end-if
LUBEXX              else
LUBEXX                 if rmo-qta not = 0
LUBEXX                    set trasferisci to true
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX
LUBEXX              if trasferisci

LUBEXX                 |MOVIMENTO IN ENTRATA (CARICO DI MAGAZZINO)
LUBEXX                 if tca-movim-giac-pos
LUBEXX                    compute como-valore = tmo-peso-utf
LUBEXX                 else
LUBEXX                 |MOVIMENTO IN USCITA (SCARICO DI MAGAZZINO)
LUBEXX                    compute como-valore = rmo-peso-tot-utf
LUBEXX                 end-if
                                                       
                       |MOVIMENTO IN ENTRATA (CARICO DI MAGAZZINO)
LUBEXX                 if tca-movim-giac-pos
                          |Uso il registro di carico
                          move nom-num-reg-c to como-num-reg
                       else
                          evaluate tcl-serie-bolle
                          when 1 move nom-num-reg-s1 to como-num-reg
                          when 2 move nom-num-reg-s2 to como-num-reg
                          when 3 move nom-num-reg-s3 to como-num-reg
                          end-evaluate
                       end-if

                       move nom-colonna to como-col
                       perform SEARCH-ELEMENT
                       if trovato-registro
LUBEXX                    |MOVIMENTO IN ENTRATA (CARICO DI MAGAZZINO)
LUBEXX                    |Faccio la MOVE perchè il peso
LUBEXX                    |c'è in testata è cumulativo
LUBEXX                    if tca-movim-giac-pos
LUBEXX                       move como-valore to el-kg(occ-idx)
LUBEXX                    |MOVIMENTO IN USCITA (SCARICO DI MAGAZZINO)
LUBEXX                    |Faccio la somma delle righe
LUBEXX                    else
LUBEXX                       add  como-valore to el-kg(occ-idx)
LUBEXX                    end-if
                       else
                          perform RECUPERA-PROG
                          add  1            to idx
                          move como-num-reg to el-num-reg(idx)
                          move nom-colonna  to el-col(idx)
                          move mov-prog-reg to el-prog(idx)
LUBEXX                    |MOVIMENTO IN ENTRATA (CARICO DI MAGAZZINO)
LUBEXX                    |Faccio la MOVE perchè il peso
LUBEXX                    |c'è in testata è cumulativo
LUBEXX                    if tca-movim-giac-pos
LUBEXX                       move como-valore to el-kg(idx)
LUBEXX                    |MOVIMENTO IN USCITA (SCARICO DI MAGAZZINO)
LUBEXX                    |Faccio la somma delle righe
LUBEXX                    else
LUBEXX                       add  como-valore to el-kg(idx)
LUBEXX                    end-if
                       end-if
                    end-if
LUBEXX           end-if
                 
              end-perform
              if idx > 0
                 perform varying occ-idx from 1 by 1 
                           until occ-idx > idx
                    initialize mov-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move esercizio           to mov-anno
                    move el-num-reg(occ-idx) to mov-num-reg
                    move el-prog(occ-idx)    to mov-prog-reg
                    if tca-movim-giac-pos
                       set mov-entrata to true
                    else
                       set mov-uscita  to true
                    end-if
                    move el-col(occ-idx)   to mov-col-reg-utf
                    move el-kg(occ-idx)    to mov-kg
                    move tmo-tipo          to mov-tipo-CF
                    move tmo-cod-clifor    to mov-cod-clifor
                    move tmo-numdoc-clifor to mov-num-doc
                    move tmo-data-movim    to mov-data
                    move tmo-numero        to mov-num-movim
                    accept mov-data-creazione from century-date
                    accept mov-ora-creazione  from time
                    move   extmov-user        to mov-utente-creazione
                    write mov-rec
                          invalid continue 
                      not invalid
                          set trovato to true
      *****                    if extmov-mov-elab-da = 0
      *****                       move tmo-numero to extmov-mov-elab-da
      *****                    end-if
      *****                    move tmo-numero to extmov-mov-elab-a
                          add 1 to extmov-tot-mov-elab

                          add 1 to extmov-tot-mov-gen

LUBEXX                    if mov-entrata
LUBEXX                       add 1 to extmov-tot-ent
LUBEXX                    end-if

                    end-write
                    set SI-MOVUTF  to true
                 end-perform
              end-if
           end-if.

      **    faccio il nuovo giro sulle righe per vedere se devo trasferire
      **    i dati verso l'EDI 
      *     perform VAL-EDI.

      ***---
       RECUPERA-PROG.
           move esercizio    to mov-anno.
           move como-num-reg to mov-num-reg.
           move high-value   to mov-prog-reg.
           start movutf key <= mov-chiave
                 invalid move 1 to mov-prog-reg
             not invalid
                 read movutf previous
                 if mov-anno = esercizio
                    if mov-num-reg = como-num-reg
                       add  1 to mov-prog-reg
                    else
                       move 1 to mov-prog-reg
                    end-if
                 else
                    move 1 to mov-prog-reg
                 end-if
           end-start.

      ***---
       INITIALIZE-OCCURS.
           move 0 to occ-idx idx.
           perform 999 times
              add  1     to occ-idx
              move 0     to el-num-reg(occ-idx)
              move space to el-col(occ-idx)
              move 0     to el-kg(occ-idx)
           end-perform.

      ***---
       SEARCH-ELEMENT.
           set trovato-registro to false.
           set occ-idx to 1.
           search tmp-occurs
           when el-reg-col(occ-idx) = como-reg-col
                set trovato-registro to true
           end-search.

      ****---
       READ-TMOVTRAT-LOCK.
           set RecLocked to false.
           set tutto-ok  to true.
           read tmovtrat with lock invalid set errori to true end-read.

      ***---
       CLOSE-FILES.
           close tmovmag
                 rmovmag
                 articoli
                 tcaumag
                 tmagaz
                 tnomen
                 tmovtrat
                 movutf
                 clienti
                 ttipocli.
      *           tedi
      *           redi
      *           tmp-redi
      *           prodener
      *           paramedi
      *           tnazioni
      *           anautf
      *           timposte.

      *     delete file tmp-redi.

      ***---
       EXIT-PGM.
           goback.

      ****---
      * PARAGRAFO-COPY.
      *     copy "UTF-EDI.cpy".
