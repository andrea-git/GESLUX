       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      extmovedi-p.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl". 
           copy "rmovmag.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "tmagaz.sl".
           copy "clienti.sl".
           copy "tedi.sl".
           copy "redi.sl".
           copy "tmp-redi.sl".
           copy "prodener.sl".
           copy "paramedi.sl".
           copy "tnazioni.sl".
           copy "timposte.sl".
           copy "tmp-art-no-prodener.sl".
           copy "lineseq.sl".

      *****************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd". 
           copy "rmovmag.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "tmagaz.fd".
           copy "clienti.fd".
           copy "tedi.fd".
           copy "redi.fd".
           copy "tmp-redi.fd".
           copy "prodener.fd".
           copy "paramedi.fd".
           copy "tnazioni.fd".
           copy "timposte.fd".
           copy "tmp-art-no-prodener.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       77  status-tmovmag              pic x(2).
       77  status-rmovmag              pic x(2).
       77  status-articoli             pic x(2).
       77  status-tcaumag              pic x(2).
       77  status-tmagaz               pic x(2).
       77  status-clienti              pic x(2).
       77  status-tedi                 pic x(2).
       77  status-redi                 pic x(2).
       77  status-tmp-redi             pic x(2).
       77  status-prodener             pic x(2).
       77  status-paramedi             pic x(2).
       77  status-tnazioni             pic x(2).
       77  status-timposte             pic x(2).
       77  status-tmp-art-no-prodener  pic x(2).
       77  status-lineseq              pic x(2).

       77  path-tmp-redi               pic x(256).
       77  path-tmp-art-no-prodener    pic x(256).
       77  wstampa                     pic x(256).

       78  titolo    value "Estrazione movimenti UTF telematici".

       01  filler                  pic 9.
         88 trasferisci            value 1, false 0.

       01  filler                  pic 9.
         88 trovato-registro       value 1, false 0.

       01  filler                  pic 9.
         88 record-ok              value 1, false 0.

       01  filler                  pic 9.
         88 problemi-articoli      value 1, false 0.

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
           88 controllo-3403       value 3.


       01  tipo-ope2               pic 9.
           88 ciclo-controllo      value 1.
           88 ciclo-elaborazione   value 2.

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

       01 mov-rec.
           05 mov-chiave.
               10 mov-anno         PIC  9(4).
               10 mov-num-reg      PIC  9(8).
               10 mov-prog-reg     PIC  9(10).
           05 mov-dati.
               10 mov-data         PIC  9(8).
               10 mov-tipo         PIC  x.
                   88 mov-entrata VALUE IS "+". 
                   88 mov-uscita VALUE IS "-". 
               10 mov-kg           PIC  s9(9)v999.
               10 mov-tipo-CF      PIC  x.
                   88 mov-cliente VALUE IS "C". 
                   88 mov-fornitore VALUE IS "F". 
               10 mov-cod-clifor   PIC  9(5).
               10 mov-num-doc      PIC  9(10).

       01  riga-report.
           05 rr-codice   PIC  x(8).
           05 filler      pic x(2) value space.
           05 rr-descr    PIC  x(40).
           05 filler      pic x(2) value space.
           05 rr-problema PIC  x(50).
       77  art-ed         pic z(8).

       LINKAGE SECTION.
       copy "link-extmovedi.def".

      ******************************************************************
       PROCEDURE DIVISION using extmovedi-linkage.

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

       TEDI-ERR SECTION.
           use after error procedure on tedi.
           set tutto-ok  to true.
           evaluate status-tedi
           when "35"
                display message "File [TEDI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TEDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TEDI] Indexed file corrupt!"
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
                move   "TEDI"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o tedi allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99"
                initialize geslock-messaggio
                string "Il record per l'anno: " tedi-anno 
                       " mese: "   tedi-mese                
                " risulta"
                x"0d0a""già in uso su altro terminale."   delimited size
                x"0d0a""Questo comporta l'impossibilità"  delimited size
                x"0d0a""ad aggiornare i dati EDI."        delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "TEDI"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     read tedi lock
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       REDI-ERR SECTION.
           use after error procedure on redi.
           set tutto-ok  to true.
           evaluate status-redi
           when "35"
                display message "File [REDI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [REDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[REDI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TMP-REDI-ERR SECTION.
           use after error procedure on tmp-redi.
           set tutto-ok  to true.
           evaluate status-tmp-redi
           when "35"
                display message "File [TMP-REDI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TMP-REDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-REDI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TMP-ART-NO-PRODENER-ERR SECTION.
           use after error procedure on TMP-ART-NO-PRODENER.
           set tutto-ok  to true.
           evaluate status-TMP-ART-NO-PRODENER
           when "35"
                display message "File [TMP-ART-NO-PRODENER] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
             display message "File [TMP-ART-NO-PRODENER] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
           display message "[TMP-ART-NO-PRODENER] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       PRODENER-ERR SECTION.
           use after error procedure on prodener.
           set tutto-ok  to true.
           evaluate status-prodener
           when "35"
                display message "File [PRODENER] not found!"
                          title titolo
                           icon 3
                set errori to true
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       PARAMEDI-ERR SECTION.
           use after error procedure on paramedi.
           set tutto-ok  to true.
           evaluate status-paramedi
           when "35"
                display message "File [PARAMEDI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [PARAMEDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PARAMEDI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TNAZIONI-ERR SECTION.
           use after error procedure on TNAZIONI.
           set tutto-ok  to true.
           evaluate status-TNAZIONI
           when "35"
                display message "File [TNAZIONI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TNAZIONI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNAZIONI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TIMPOSTE-ERR SECTION.
           use after error procedure on TIMPOSTE.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                display message "File [TIMPOSTE] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TIMPOSTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

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
           set extmovedi-problemi-art to false
           move 0                     to counter counter2.
           set RecLocked              to false.
           set tutto-ok               to true.
           set trovato                to false.
           set problemi-articoli      to false.
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

           accept path-tmp-art-no-prodener from environment "PATH_ST".
           inspect path-tmp-art-no-prodener   
                                replacing trailing space by low-value

           string path-tmp-art-no-prodener  delimited by low-value
                  "tmp_art_no_prodener"     delimited by size
                  como-data                 delimited by size
                  "_"                       delimited by size
                  como-ora                  delimited by size
                  into path-tmp-art-no-prodener
           inspect path-tmp-art-no-prodener
                                replacing trailing low-value by space.

      ***---
       OPEN-FILES.
           if tutto-ok
              perform OPEN-IO-TEDI
           end-if

           if tutto-ok
              perform OPEN-IO-REDI
              perform OPEN-IO-TMP-REDI
              perform OPEN-IO-TMP-ART-NO-PRODENER
              if tutto-ok
                 open input tmovmag
                            rmovmag
                            articoli
                            tcaumag
                            tmagaz
                            clienti
                            prodener
                            paramedi
                            tnazioni
                            timposte
              end-if
           end-if.

      ***---
       OPEN-IO-TEDI.
           open i-o tedi allowing readers.

      ***---
       OPEN-IO-REDI.
           open i-o redi.

      ***---
       OPEN-IO-TMP-REDI.
           open output tmp-redi
           close       tmp-redi.
           open i-o    tmp-redi.

      ***---
       OPEN-IO-TMP-ART-NO-PRODENER.
           open output tmp-art-no-prodener
           close       tmp-art-no-prodener.
           open i-o    tmp-art-no-prodener.

      ***---
       ELABORAZIONE. 
      *    metto in linea i parametri generali
           move spaces to pae-codice.
           read PARAMEDI invalid continue end-read.

           set ciclo-controllo to true.
           perform SCORRI-TMOVMAG.

           if problemi-articoli
              perform CREA-REPORT
           else
              set ciclo-elaborazione to true
              perform SCORRI-TMOVMAG
              if not trovato
                 display message "Nessun movimento da elaborare!"
                         title titolo
                        icon 2
              end-if
           end-if.   

      ***---
       SCORRI-TMOVMAG.
           move extmovedi-mov-gen-da to tmo-data-movim.
           move low-value            to tmo-numero.

           start tmovmag key >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set  record-ok  to false
                 read tmovmag next at end exit perform end-read
                 if tmo-data-movim >  extmovedi-mov-gen-a
                    exit perform 
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon extmovedi-handle at column 11
                                                  line 03
                    move 0 to counter2
                 end-if

                 move tmo-causale to tca-codice
                 read tcaumag no  lock
                      invalid continue
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
                       read clienti no  lock 
                            invalid continue 
                       end-read
                    else
                       set cli-tipo-F to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no  lock 
                            invalid continue 
                       end-read
                    end-if
      *    
                    evaluate true
                    when ciclo-elaborazione perform LOOP-RIGHE
                    when ciclo-controllo    perform LOOP-RIGHE-CONTROLLO
                    end-evaluate
                 end-if
              end-perform
           end-if.

      ***---
       LOOP-RIGHE.
           set  SI-MOVUTF    to false
           set  tutto-ok     to true.

      *    faccio il nuovo giro sulle righe per vedere se devo trasferire
      *    i dati verso l'EDI, come da richiesta di Walter del 1/3/2010
      *    se il peso utf della testa è a zero (forzato anche manualmente
      *    non devo prendere in considerazione il movimento
      *     if tmo-peso-utf not = zero
      *        perform VAL-EDI
      *     end-if.

      
      *    Nuovo codice se un movimento ha peso utf valorizzato esporto 
      *    direttamente, se non ha peso faccio una verifica per 
      *    controllare se ho delle righe da esportare comunque, ad 
      *    esempio gli articoli 3403 per i fornitori esteri.
           
           if tmo-peso-utf = zero
              perform VERIFICA-MOVIMENTO
              if tutto-ok
                 perform VAL-EDI
              end-if
           else
              perform VAL-EDI
           end-if.                                   

      ***---
       CLOSE-FILES.
           close tmovmag
                 rmovmag
                 articoli
                 tcaumag
                 tmagaz
                 clienti
                 tedi
                 redi
                 tmp-redi
                 tmp-art-no-prodener
                 prodener
                 paramedi
                 tnazioni
                 timposte.

           delete file tmp-redi.
           delete file tmp-art-no-prodener.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "UTF-EDI.cpy".
