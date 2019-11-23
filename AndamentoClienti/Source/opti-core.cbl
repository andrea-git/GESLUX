       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      opti-core.
       AUTHOR.                          Andrea.
       REMARKS.  Motore di estrazione righe ordini per reportistica
                 andamento clienti. Viene generato un txt raggruppato
                 per codice articolo. Vengono scartati i documenti che 
                 non hanno impostato il flag  "STATISTICHE VEN/ACQ".
                 Parametri passati in chaining.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "andamento.sl".
           copy "ttipocli.sl".
           copy "tgrupgdo.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tmarche.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "andamento.fd".
           copy "ttipocli.fd".
           copy "tgrupgdo.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tmarche.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COSTANTI
       78  titolo                  value "Andamento Clienti".

      * FILE STATUS AND VARIABLES
       77  status-andamento        pic x(2).
       77  status-ttipocli         pic x(2).
       77  status-tgrupgdo         pic x(2).
       77  status-clienti          pic x(2).
       77  status-destini          pic x(2).
       77  status-tmarche          pic x(2).
       77  status-tordini          pic x(2).
       77  status-rordini          pic x(2).
       77  status-tnotacr          pic x(2).
       77  status-rnotacr          pic x(2).
       77  status-articoli         pic x(2).
       77  status-tcaumag          pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).
       77  path-andamento          pic x(256).

      * FLAGS
       01  filler                  pic 9.
         88 rotta                  value 1, false 0.

       01  filler                  pic 9.
         88 trovato                value 1, false 0.

       01  filler                  pic 9.
         88 record-ok              value 1, false 0.

       01  controlli               pic xx.
         88 tutto-ok               value "OK".
         88 errori                 value "ER".

       01 filler                   pic 9.
         88 TutteTipologie         value 1, false 0.

       01 filler                   pic 9.
         88 TuttiGDO               value 1, false 0.

       01 filler                   pic 9.
         88 TuttiClienti           value 1, false 0.

       01 filler                   pic 9.
         88 TuttiDestini           value 1, false 0.

       01 filler                   pic 9.
         88 TutteMarche            value 1, false 0.

       01 filler                   pic 9.
         88 inserisci              value 1.
         88 cancella               value 2.

       77  como-qta                pic s9(8).
       77  como-imponib-merce      pic s9(12)v99.
       77  como-imp-consumo        pic s9(12)v99.
       77  como-add-piombo         pic s9(12)v99.
       77  como-imp-cou-cobat      pic s9(12)v99.
       77  como-riga               pic x(80).
       77  last-riga               pic x(200).

       77  tipo-doc                pic x.
       77  tot-documento           pic 9(12)v99.
       77  num-doc                 pic 9(8).
       77  data-doc                pic 9(8).

       77  save-data-ini           pic 9(8).
       77  save-data-fine          pic 9(8).

       77  como-ora                pic 9(8).
       77  como-data               pic 9(8).
       77  como-anno               pic 9(4).
       77  comodo                  pic x(8).

       77  start-data-rotta        pic 9(8).
       77  end-data-rotta          pic 9(8).

       77  ciclo                   pic 9.

       77  testate-elaborate       pic 9(5).
       77  testate-nc-elaborate    pic 9(5).
       77  testate-elaborate-edit  pic zz.zz9.

       77  righe-elaborate         pic 9(5).
       77  righe-nc-elaborate      pic 9(5).
       77  righe-elaborate-edit    pic zz.zz9.

       77  imponibile-pos          pic 9(12)v99 occurs 3.
       77  segno                   pic x        occurs 3.

       77  qta-pos                 pic 9(10)    occurs 3.
       77  segno2                  pic x        occurs 3.

       77  idx                     pic 9.

       77  start-secondi           pic 9(18).
       77  end-secondi             pic 9(18).
       77  tot-secondi             pic 9(18).
       77  hh                      pic 99.
       77  mm                      pic 99.
       77  ss                      pic 99.

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.               
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

      * VARIABILI DI CHAINING
       77  tipo-pgm                pic x.
         88 principale             value "X".
         88 non-venduto            value "N".
         88 fatturato              value "F".

       77  tipo-cli                pic x(2).     |" 1" GDO "XX" TUTTI
       77  gruppo-gdo              pic x(5).     
       77  cliente                 pic 9(5).
       77  destino                 pic 9(5).
       77  marca                   pic 9(4).
       77  data-ini                pic 9(8).
       77  data-fine               pic 9(8).
       77  flag-omaggio            pic x.
         88 flag-si                value "S".
         88 flasg-no               value "N".
       77  num-cicli               pic 9.
       77  path-fisso-txt          pic x(256).

      ******************************************************************

       PROCEDURE DIVISION CHAINING tipo-pgm
                                   tipo-cli
                                   gruppo-gdo
                                   cliente
                                   destino
                                   marca
                                   data-ini
                                   data-fine
                                   flag-omaggio
                                   num-cicli
                                   path-fisso-txt.

       DECLARATIVES.
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] record locked!" delimited size
                       " P-KEY: " tor-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] record locked!" delimited size
                       " P-KEY: " ror-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TNOTACR] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TNOTACR] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TNOTACR] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TNOTACR] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TNOTACR] record locked!" delimited size
                       " P-KEY: " tor-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RNOTACR] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RNOTACR] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RNOTACR] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RNOTACR] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RNOTACR] record locked!" delimited size
                       " P-KEY: " ror-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [CLIENTI] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] indexed file corrupt!"
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [CLIENTI]"                delimited size
                       " P-KEY: " cli-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.  

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [DESTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [DESTINI] record locked!" delimited size
                       " P-KEY: " des-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       ANDAMENTO-ERR SECTION.
           use after error procedure on andamento.
           set tutto-ok  to true.
           evaluate status-andamento
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TMP [ADNAMENTO] non trovato!"delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TMP [ANDAMENTO] mismatch size!"
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TMP [ANDAMENTO] indexed file corrupt!"
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TMP [ANDAMENTO] File locked!"delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "File [ANDAMENTO] record locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [LINESEQ] non trovato!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [LINESEQ] mismatch size!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [LINESEQ] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [LINESEQ] File locked!" delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
                display message 
                        "File di transizione bloccato."
                 x"0d0a""Lasciare trascorrere un istante e ritentare!"
                          title "Thin-Client"
                           icon 3
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [LINESEQ] record locked!" delimited size
                       " P-KEY: " tor-chiave           delimited size
                       into como-riga
                end-string
                move "*" to como-riga(1:1)
                display como-riga upon syserr
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.

      *****     open i-o tordini rordini tnotacr rnotacr.
      *****
      *****     move 2005 to tor-anno.
      *****     move    0 to tor-numero.
      *****
      *****     start tordini key is >= tor-chiave.
      *****     perform until 1 = 2
      *****        read tordini next at end exit perform end-read
      *****        if tor-anno > 2005       exit perform end-if
      *****        move tor-anno   to ror-anno
      *****        move tor-numero to ror-num-ordine
      *****        move low-value  to ror-num-riga
      *****        start rordini key is >= ror-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rordini next at end exit perform end-read
      *****                 if ror-anno       not = tor-anno or
      *****                    ror-num-ordine not = tor-numero
      *****                    exit perform
      *****                 end-if
      *****                 delete rordini record
      *****              end-perform
      *****        end-start
      *****        delete tordini record
      *****     end-perform.
      *****
      *****     move 2006 to tor-anno.
      *****     move    0 to tor-numero.
      *****
      *****     start tordini key is >= tor-chiave.
      *****     perform until 1 = 2
      *****        read tordini next at end exit perform end-read
      *****        if tor-anno > 2006       exit perform end-if
      *****        move tor-anno   to ror-anno
      *****        move tor-numero to ror-num-ordine
      *****        move low-value  to ror-num-riga
      *****        start rordini key is >= ror-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rordini next at end exit perform end-read
      *****                 if ror-anno       not = tor-anno or
      *****                    ror-num-ordine not = tor-numero
      *****                    exit perform
      *****                 end-if
      *****                 move 2005 to ror-anno
      *****                 write ror-rec
      *****                       invalid display message "INV WRITE R"
      *****                 end-write
      *****              end-perform
      *****        end-start
      *****        move 2005 to tor-anno
      *****        move 2005 to tor-data-ordine(1:4)
      *****        if tor-data-passaggio-ordine not = 0
      *****           move 2005 to tor-data-passaggio-ordine(1:4)
      *****        end-if
      *****        if tor-data-note1 not = 0
      *****           move 2005 to tor-data-note1(1:4)
      *****        end-if
      *****        if tor-anno-bolla not = 0
      *****           move 2005 to tor-anno-bolla
      *****        end-if
      *****        if tor-data-bolla not = 0
      *****           move 2005 to tor-data-bolla(1:4)
      *****        end-if
      *****        if tor-anno-fattura not = 0
      *****           move 2005 to tor-anno-fattura
      *****        end-if
      *****        if tor-data-fattura not = 0
      *****           move 2005 to tor-data-fattura(1:4)
      *****        end-if
      *****        write tor-rec 
      *****              invalid display message "INV WRITE T"
      *****        end-write
      *****     end-perform.
      *****
      *****     move 2005 to tno-anno.
      *****     move    0 to tno-numero.
      *****
      *****     start tnotacr key is >= tno-chiave.
      *****     perform until 1 = 2
      *****        read tnotacr next at end exit perform end-read
      *****        if tno-anno > 2005       exit perform end-if
      *****        move tno-anno   to rno-anno
      *****        move tno-numero to rno-numero
      *****        move low-value  to rno-num-riga
      *****        start rnotacr key is >= rno-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rnotacr  next at end exit perform end-read
      *****                 if rno-anno   not = tno-anno or
      *****                    rno-numero not = tno-numero
      *****                    exit perform
      *****                 end-if
      *****                 delete rnotacr record
      *****              end-perform
      *****        end-start
      *****        delete tnotacr record
      *****     end-perform.
      *****
      *****     move 2006 to tno-anno.
      *****     move    0 to tno-numero.
      *****
      *****     start tnotacr key is >= tno-chiave.
      *****     perform until 1 = 2
      *****        read tnotacr next at end exit perform end-read
      *****        if tno-anno > 2006       exit perform end-if
      *****        move tno-anno   to rno-anno
      *****        move tno-numero to rno-numero
      *****        move low-value  to rno-num-riga
      *****        start rnotacr key is >= rno-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rnotacr  next at end exit perform end-read
      *****                 if rno-anno   not = tno-anno or
      *****                    rno-numero not = tno-numero
      *****                    exit perform
      *****                 end-if
      *****                 move 2005 to rno-anno
      *****                 write rno-rec
      *****                       invalid display message "INV WRITE R"
      *****                 end-write
      *****              end-perform
      *****        end-start
      *****        move 2005 to tno-anno
      *****        if tno-data-passaggio-ordine not = 0
      *****           move 2005 to tno-data-passaggio-ordine(1:4)
      *****        end-if
      *****        if tno-anno-fattura not = 0
      *****           move 2005 to tno-anno-fattura
      *****        end-if
      *****        if tno-data-fattura not = 0
      *****           move 2005 to tno-data-fattura(1:4)
      *****        end-if
      *****        write tno-rec 
      *****              invalid display message "INV WRITE T"
      *****        end-write
      *****     end-perform.
      *****            
      *****     display message "FINE!".
      *****     goback.

           if tutto-ok

              evaluate true
              when principale
                   perform ELABORAZIONE-PRINCIPALE num-cicli times
              when non-venduto
                   perform ELABORAZIONE-NON-VENDUTO
                   move low-value to and-rec
                   start andamento key >= and-chiave
                         invalid set trovato to false
                     not invalid set trovato to true
                   end-start
              when fatturato
                   perform ELABORAZIONE-FATTURATO
              end-evaluate

              if trovato
                 evaluate true
                 when principale  perform CREA-TXT-PRINCIPALE
                 when non-venduto perform CREA-TXT-NON-VENDUTO
                 end-evaluate
              end-if

              perform CLOSE-FILES

           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move data-ini  to save-data-ini.
           move data-fine to save-data-fine.

           if num-cicli = 3
              move 2 to num-cicli
              set rotta to true
           else
              set rotta to false
           end-if.
           move 0 to righe-elaborate   righe-nc-elaborate ciclo
                     testate-elaborate testate-nc-elaborate.
           move path-fisso-txt to wstampa.
           set tutto-ok        to true.
           set trovato         to false.

           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           
           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           initialize path-andamento.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-andamento from environment "PATH_ST".
           inspect path-andamento replacing trailing spaces by low-value
           string  path-andamento delimited low-value
                   "ANDAMENTO_"   delimited size
                   como-data      delimited size
                   "_"            delimited size
                   como-ora       delimited size
                   ".tmp"         delimited size
                   into path-andamento
           end-string.
           set TutteTipologie to false.
           set TuttiGDO       to false.
           set TuttiClienti   to false.
           set TuttiDestini   to false.
           set TutteMarche    to false.
           if tipo-cli   = "XX"
              set TutteTipologie to true
           end-if.
           if gruppo-gdo = "XXXXX"
              set TuttiGDO       to true
           end-if.
           if cliente    = 99999
              move 0 to cliente
              set TuttiClienti   to true
           end-if.
           if destino    = 99999
              move 0 to destino
              set TuttiDestini   to true
           end-if.
           if marca      = 9999
              set TutteMarche    to true
           end-if.

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

      ***---
       OPEN-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                        delimited size
                  "APERTURA FILES "               delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           open input tordini
                      rordini
                      clienti
                      destini
                      ttipocli
                      tgrupgdo
                      tmarche
                      tnotacr
                      rnotacr
                      articoli
                      tcaumag.

           if tutto-ok
              open output andamento
              close       andamento
              open i-o    andamento
              call "C$DELETE" using path-fisso-txt, "S"
              open output lineseq
              if errori
                 close tordini
                       rordini
                       clienti
                       destini
                       ttipocli
                       tgrupgdo
                       tmarche
                       tnotacr
                       rnotacr
                       articoli
                       tcaumag
              end-if
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                    delimited size
                  "APERTURA FILES ESEGUITA"   delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       ELABORAZIONE-PRINCIPALE.
           add 1 to ciclo.
           evaluate ciclo
           |Ciclo sulle date che mi vengono passate
           when 1
                initialize como-riga
                move data-ini(1:4)  to como-anno
                move data-fine(1:4) to como-anno
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "PRIMO CICLO DI ELABORAZIONE" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           |Ciclo sulle date che mi vengono passate ma nell'anno precedente
           when 2
                initialize como-riga
                subtract 1 from como-anno
                move como-anno to data-ini(1:4)
                move como-anno to data-fine(1:4)
               |Ciclo sul periodo di rotta ossia tutto il
               |mese dell'anno precedente relativo al periodo passato.
               |Non sottraggo 1 all'anno perchè già fatto nel passaggio prima
               |Metto 99 nel giorno di fine periodo x' cmq il primo di
               |Febbraio è successivo al 99 di Gennaio. Evito così di fare
               |i calcoli sul quale sia l'ultimo giorno e sul bisestile
                if rotta
                   move data-ini       to start-data-rotta
                   move "01"           to start-data-rotta(7:2)
                   move data-fine      to end-data-rotta
                   move data-fine(5:2) to end-data-rotta(5:2)
                   move "99"           to end-data-rotta(7:2)
                end-if
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "SECONDO CICLO DI ELABORAZIONE" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
           end-evaluate.
           perform ELABORA-TORDINI-PRINCIPALE.
           perform ELABORA-TNOTACR-PRINCIPALE.

      ***---
       ELABORA-TORDINI-PRINCIPALE.
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           evaluate ciclo
           when 1          move data-ini         to tor-data-fattura
           when 2 if rotta move start-data-rotta to tor-data-fattura
                  else     move data-ini         to tor-data-fattura
                  end-if
           end-evaluate.

           move cliente  to tor-cod-cli.
           move destino  to tor-prg-destino.
           set  tor-si-agg-contab to true.

           set tutto-ok to true.
           evaluate true also true
           when TuttiClienti also TuttiDestini
                start tordini key is >= k-andamento-data
                      invalid set errori to true
                end-start

           when not TuttiClienti also TuttiDestini
                start tordini key is >= k-andamento-cliente
                      invalid set errori to true
                end-start

           when not TuttiClienti also not TuttiDestini
                start tordini key is >= k-andamento-clides
                      invalid set errori to true
                end-start
           end-evaluate.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 evaluate ciclo
                 when 1
                      if tor-data-fattura > data-fine
                         exit perform
                      end-if
                 when 2
                      if rotta
                         if tor-data-fattura > end-data-rotta
                            exit perform
                         end-if
                      else
                         if tor-data-fattura > data-fine
                            exit perform
                         end-if
                      end-if
                 end-evaluate

                 if tor-no-agg-contab
                    exit perform
                 end-if

                 if not TuttiClienti
                    if tor-cod-cli not = cliente
                       exit perform
                    end-if
                 end-if

                 if not TuttiDestini
                    if tor-prg-destino not = destino
                       exit perform
                    end-if
                 end-if

                 set record-ok to true
                 perform VALIDA-ORDINE
                 if record-ok
                    add 1 to testate-elaborate
                    perform LOOP-RORDINI-PRINCIPALE
                 end-if
              end-perform
           end-if.

      ***---
       ELABORA-TNOTACR-PRINCIPALE.
           initialize tno-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           evaluate ciclo
           when 1          move data-ini         to tno-data-fattura
           when 2 if rotta move start-data-rotta to tno-data-fattura
                  else     move data-ini         to tno-data-fattura
                  end-if
           end-evaluate.
           move cliente  to tno-cod-cli.
           move destino  to tno-prg-destino.
           set  tno-si-agg-contab to true.

           set tutto-ok to true.
           evaluate true also true
           when TuttiClienti also TuttiDestini
                start tnotacr key is >= k-andamento-data
                      invalid set errori to true
                end-start

           when not TuttiClienti also TuttiDestini
                start tnotacr key is >= k-andamento-cliente
                      invalid set errori to true
                end-start

           when not TuttiClienti also not TuttiDestini
                start tnotacr key is >= k-andamento-clides
                      invalid set errori to true
                end-start
           end-evaluate.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next at end exit perform end-read
                 evaluate ciclo
                 when 1
                      if tno-data-fattura > data-fine
                         exit perform
                      end-if
                 when 2
                      if rotta
                         if tno-data-fattura > end-data-rotta
                            exit perform
                         end-if
                      else
                         if tno-data-fattura > data-fine
                            exit perform
                         end-if
                      end-if
                 end-evaluate

                 if tno-no-agg-contab
                    exit perform
                 end-if

                 if not TuttiClienti
                    if tno-cod-cli not = cliente
                       exit perform
                    end-if
                 end-if

                 if not TuttiDestini
                    if tno-prg-destino not = destino
                       exit perform
                    end-if
                 end-if

                 set record-ok to true
                 move tno-causale to tca-codice
                 read tcaumag no lock invalid continue end-read
                 if tca-tipo-nota-prz or
                    tca-tipo-nota-reso
                    move tno-cod-cli     to tor-cod-cli
                    move tno-prg-destino to tor-prg-destino
                    move tno-causale     to tor-causale
                    perform VALIDA-ORDINE
                    if record-ok
                       add 1 to testate-nc-elaborate
                       perform LOOP-RNOTACR-PRINCIPALE
                    end-if
                 end-if

              end-perform
           end-if.

      ***---
       VALIDA-ORDINE.
           if record-ok
              set cli-tipo-C   to true
              move tor-cod-cli to cli-codice
              read clienti no lock
                   invalid set record-ok to false
               not invalid
                   |231009 MODIFICA TEMPORANEA PER NORAUTO
                   if cli-codice = 899
                      move "1 " to cli-tipo
                   end-if
                   if not TutteTipologie
                      if tipo-cli = "1 " or tipo-cli = "11"
                         if not ( cli-tipo = "1 " or cli-tipo = "11" )
                            set record-ok to false
                         end-if
                      else
                         if cli-tipo not = tipo-cli
                            set record-ok to false
                         end-if
                      end-if
                   end-if
                   if not TuttiGDO and record-ok
                      if cli-gdo not = gruppo-gdo
                         set record-ok to false
                      end-if
                   end-if
              end-read
           end-if.

           if record-ok
              move tor-causale to tca-codice
              read tcaumag no lock 
                   invalid continue 
               not invalid
                   if tca-no-stat set record-ok to false end-if
              end-read
           end-if.

      ***---
       LOOP-RORDINI-PRINCIPALE.
           move low-value  to ror-rec.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    set record-ok to true
                    move ror-cod-articolo to art-codice
                    read articoli no lock
                         invalid set record-ok to false
                     not invalid
                         if not TutteMarche
                            if art-marca-prodotto not = marca
                               set record-ok to false
                            end-if
                         end-if
                    end-read

                    if flag-omaggio = "S"
                       if ror-no-omaggio 
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok
                       move tor-prg-destino   to des-prog
                       set trovato            to true
                       add 1 to righe-elaborate
                       move ror-imponib-merce to como-imponib-merce
                       move ror-imp-consumo   to como-imp-consumo  
                       move ror-imp-cou-cobat to como-imp-cou-cobat
                       move ror-add-piombo    to como-add-piombo
                                                                    
                       move ror-qta to como-qta
OMAGGI                 subtract ror-qta-omaggi from ror-qta
                       if ror-qta = 0 move 1       to ror-qta end-if

                       perform SCRIVI-RECORD
                    end-if

                 end-perform
           end-start.

      ***---
       LOOP-RNOTACR-PRINCIPALE.
           move low-value  to rno-rec.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr  next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    set record-ok to true
                    move rno-cod-articolo to art-codice
                    read articoli no lock
                         invalid set record-ok to false
                     not invalid
                         if not TutteMarche
                            if art-marca-prodotto not = marca
                               set record-ok to false
                            end-if
                         end-if
                    end-read

                    if flag-omaggio = "S"
                       if rno-prz-unitario not = 0
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok
                       move tno-prg-destino to des-prog
                       set trovato to true
                       add 1 to righe-nc-elaborate
                       evaluate tno-causale 
                       when "NCPZ"
                       when "NPEX"                    
                            if rno-qta = 0 move 1       to ror-qta
                            else           move rno-qta to ror-qta
                            end-if
                            |Non agisce sulla quatità rientrata
                            move 0 to rno-qta como-qta
                       when other
                            move rno-qta to ror-qta
                            compute como-qta = rno-qta - (rno-qta * 2)
                       end-evaluate

                       move tno-data-fattura to tor-data-fattura

                       
                       |Così la somma va già in sottrazione
                       compute como-imponib-merce  =
                               rno-prz-unitario    -
                             ( rno-prz-unitario *  2 )
                       compute como-imp-consumo    =
                               rno-imp-consumo     -
                             ( rno-imp-consumo  *  2 )
                       compute como-imp-cou-cobat  =
                               rno-imp-cou-cobat   -
                             ( rno-imp-cou-cobat * 2 )
                       compute como-add-piombo     =
                               rno-add-piombo    -
                             ( rno-add-piombo    * 2 )
                       perform SCRIVI-RECORD
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI-RECORD.
           initialize and-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move art-codice              to and-articolo.
           move cli-codice              to and-cliente.
           move des-prog                to and-prg-destino.
           read andamento       no lock invalid continue end-read.
           move cli-tipo                to and-tipocli.
           |231009 MODIFICA TEMPORANEA PER NORAUTO
           if cli-codice = 899
              move "NORAU" to cli-gdo
           end-if.
           move cli-gdo                 to and-gdo.
           move cli-ragsoc-1            to and-cli-ragsoc-1.
           move art-marca-prodotto      to and-marca.
           move art-descrizione         to and-art-descrizione.
           move art-unita-di-misura     to and-art-um.
           move art-prezzo-acquisto     to and-art-prz-acq.

           evaluate ciclo
           when 1
                compute and-tot-imponibile(1)  =
                        and-tot-imponibile(1)  + ( ror-qta *
                          ( como-imponib-merce +
                            como-imp-consumo   +
                            como-imp-cou-cobat +
                            como-add-piombo   ))
                compute and-qta(1) = and-qta(1) + como-qta
           when 2
                if rotta
                   if tor-data-fattura >= data-ini and
                      tor-data-fattura <= data-fine
                      compute and-tot-imponibile(2)  =
                              and-tot-imponibile(2)  + ( ror-qta * 
                                ( como-imponib-merce +
                                  como-imp-consumo   +
                                  como-imp-cou-cobat +
                                  como-add-piombo   ))
                      compute and-qta(2) = and-qta(2) + como-qta

                      compute and-tot-imponibile(3)  =
                              and-tot-imponibile(3)  + ( ror-qta *
                                ( como-imponib-merce +
                                  como-imp-consumo   +
                                  como-imp-cou-cobat +
                                  como-add-piombo   ))
                      compute and-qta(3) = and-qta(3) + como-qta

                   else

                      compute and-tot-imponibile(3)  =
                              and-tot-imponibile(3)  + ( ror-qta *
                                ( como-imponib-merce +
                                  como-imp-consumo   +
                                  como-imp-cou-cobat +
                                  como-add-piombo   ))
                      compute and-qta(3) = and-qta(3) + como-qta

                   end-if
                else
                   compute and-tot-imponibile(2)  =
                           and-tot-imponibile(2)  + ( ror-qta *
                             ( como-imponib-merce +
                               como-imp-consumo   +
                               como-imp-cou-cobat +
                               como-add-piombo   ))
                   compute and-qta(2) = and-qta(2) + como-qta
                end-if
           end-evaluate.

           write and-rec invalid rewrite and-rec end-write.

      ***---
       CREA-TXT-PRINCIPALE.
           move low-value to and-rec.
           start andamento key >= and-chiave invalid continue end-start.
           perform until 1 = 2
              read andamento next at end exit perform end-read
              move and-tipocli     to tcl-codice
              move and-cliente     to des-codice
              move and-prg-destino to des-prog
              if des-prog not = 0
                 read destini  no lock
                      invalid
                      initialize des-ragsoc-1 des-localita
                      string "** DESTINO "     delimited size
                             des-prog          delimited size
                             " NON TROVATO **" delimited size
                             into des-ragsoc-1
                      end-string
                 end-read
              else
                 initialize des-ragsoc-1 des-localita
              end-if

              read ttipocli no lock
                   invalid
                   initialize tcl-descrizione
                   string "** TIPOLOGIA "   delimited size
                          tcl-codice        delimited size
                          " NON TROVATA **" delimited size
                          into tcl-descrizione
                   end-string
              end-read

              move and-gdo to gdo-codice
              if gdo-codice not = spaces
                 read tgrupgdo no lock
                      invalid
                      initialize gdo-intestazione
                      string "** GDO "         delimited size
                             gdo-codice        delimited size
                             " NON TROVATO **" delimited size
                             into gdo-intestazione
                      end-string
                 end-read
              else
                 initialize gdo-intestazione
              end-if

              move and-marca to mar-codice
              read tmarche no lock
                   invalid
                   initialize mar-descrizione
                   string "** MARCA "       delimited size
                          mar-codice        delimited size
                          " NON TROVATA **" delimited size
                          into mar-descrizione
                   end-string
              end-read

              initialize line-riga          
              move 0 to idx
              perform 3 times
                 add 1 to idx
                 if and-tot-imponibile(idx) < 0 move "-" to segno(idx)
                 else                           move "+" to segno(idx)
                 end-if
                 move and-tot-imponibile(idx) to imponibile-pos(idx)
              end-perform

              move 0 to idx
              perform 3 times
                 add 1 to idx
                 if and-qta(idx) < 0 move "-" to segno2(idx)
                 else                move "+" to segno2(idx)
                 end-if
                 move and-qta(idx) to qta-pos(idx)
              end-perform

              string and-cliente          delimited size
                     and-cli-ragsoc-1     delimited size
                     and-prg-destino      delimited size
                     des-ragsoc-1         delimited size
                     des-localita         delimited size
                     and-articolo         delimited size
                     and-art-descrizione  delimited size
                     and-art-um           delimited size
                     and-art-prz-acq      delimited size
                     tcl-descrizione      delimited size
                     gdo-codice           delimited size
                     gdo-intestazione     delimited size
                     mar-codice           delimited size
                     mar-descrizione      delimited size
                     segno(1)             delimited size
                     imponibile-pos(1)    delimited size
                     segno2(1)            delimited size
                     qta-pos(1)           delimited size
                     segno(2)             delimited size
                     imponibile-pos(2)    delimited size
                     segno2(2)            delimited size
                     qta-pos(2)           delimited size
                     segno(3)             delimited size
                     imponibile-pos(3)    delimited size
                     segno2(3)            delimited size
                     qta-pos(3)           delimited size
                     into line-riga |PRINCIPALE
              end-string
              write line-riga
           end-perform.

      ***---
       ELABORAZIONE-NON-VENDUTO.
           move 0 to testate-elaborate.
           |CICLO DALL'INIZIO DELL'ANNO FINO ALLA DATA D'INIZIO LIMITE
           move low-value     to tor-rec.
           set tor-si-agg-contab to true.
           string data-ini(1:4)  delimited size
                  "01"           delimited size
                  "01"           delimited size
                  into tor-data-fattura
           end-string.
           start tordini key >= k-andamento-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-no-agg-contab exit perform end-if
                    if tor-data-fattura >= data-ini exit perform end-if
                    move tor-causale to tca-codice
                    read tcaumag no lock 
                         invalid continue 
                     not invalid
                         if tca-si-stat
                            set inserisci to true
                            add 1 to testate-elaborate
                            perform LOOP-RORDINI-NON-VENDUTO
                         end-if
                    end-read
                 end-perform
           end-start. 

           |CICLO DALLA DATA DI FINE LIMITE + 1 FINO A FINE ANNO
           move low-value         to tor-rec.
           set tor-si-agg-contab  to true.
           add 1 to data-fine giving tor-data-fattura.
           start tordini key >= k-andamento-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-no-agg-contab     exit perform end-if
                    move tor-causale to tca-codice
                    read tcaumag no lock 
                         invalid continue 
                     not invalid
                         if tca-si-stat
                            set inserisci to true
                            add 1 to testate-elaborate
                            perform LOOP-RORDINI-NON-VENDUTO
                         end-if
                    end-read
                 end-perform
           end-start. 

           |CICLO NEL PERIODO DI RIFERIMENTO
           move low-value         to tor-rec.
           set tor-si-agg-contab  to true.
           move data-ini          to tor-data-fattura.
           start tordini key >= k-andamento-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-no-agg-contab     exit perform end-if
                    if tor-data-fattura > data-fine exit perform end-if
                    move tor-causale to tca-codice
                    read tcaumag no lock 
                         invalid continue 
                     not invalid
                         if tca-si-stat
                            set cancella to true
                            add 1 to testate-elaborate
                            perform LOOP-RORDINI-NON-VENDUTO
                         end-if
                    end-read
                 end-perform
           end-start.
           
      ***---
       LOOP-RORDINI-NON-VENDUTO.
           move low-value  to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    add 1 to righe-elaborate
                    |Riempio la chiave fittizia tranne l'articolo
                    move spaces           to and-cliente
                    move spaces           to and-prg-destino
                    move ror-cod-articolo to and-articolo
                    if inserisci
                       |Posizioni l'articolo per indicare che è stato
                       |trattato al di fuori del periodo richiesto
                       write and-rec 
                             invalid continue 
                       end-write
                    else
                       |Elimino l'articolo trattato nel periodo fuori
                       |(inserito nei cicli precedenti) ma presente
                       |all'interno del periodo richiesto
                       delete andamento record 
                              invalid continue 
                       end-delete
                    end-if
                 end-perform
           end-start.

      ***---
       CREA-TXT-NON-VENDUTO.
           perform until 1 = 2
              read andamento next at end exit perform end-read
              move and-articolo to art-codice
              read articoli no lock
                   invalid initialize art-rec
              end-read
              move art-marca-prodotto to mar-codice
              read tmarche no lock
                   invalid initialize mar-rec
              end-read
              string art-codice         delimited size
                     art-descrizione    delimited size
                     art-marca-prodotto delimited size
                     mar-descrizione    delimited size
                     into line-riga |NON VENDUTO
              end-string
              write line-riga
           end-perform.

      ***---
       ELABORAZIONE-FATTURATO.
           perform ELABORA-TORDINI-FATTURATO.
           perform ELABORA-TNOTACR-FATTURATO.

      ***---
       ELABORA-TORDINI-FATTURATO.
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move data-ini to tor-data-fattura.

           move cliente  to tor-cod-cli.
           move destino  to tor-prg-destino.
           set  tor-si-agg-contab to true.

           set tutto-ok to true.
           evaluate true also true
           when TuttiClienti also TuttiDestini
                start tordini key is >= k-andamento-data
                      invalid set errori to true
                end-start

           when not TuttiClienti also TuttiDestini
                start tordini key is >= k-andamento-cliente
                      invalid set errori to true
                end-start

           when not TuttiClienti also not TuttiDestini
                start tordini key is >= k-andamento-clides
                      invalid set errori to true
                end-start
           end-evaluate.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end        exit perform end-read
                 if tor-data-fattura > data-fine exit perform end-if
                 
                 if tor-no-agg-contab            exit perform end-if

                 if not TuttiClienti
                    if tor-cod-cli not = cliente
                       exit perform
                    end-if
                 end-if

                 if not TuttiDestini
                    if tor-prg-destino not = destino
                       exit perform
                    end-if
                 end-if

                 set record-ok to true
                 perform VALIDA-ORDINE
                 if record-ok
                    move 0 to tot-documento
                    add  1 to testate-elaborate
                    perform LOOP-RORDINI-FATTURATO
                    move "F" to tipo-doc
                    move tor-num-fattura  to num-doc
                    move tor-data-fattura to data-doc
                    perform SCRIVI-TXT-FATTURATO
                 end-if
              end-perform
           end-if.

      ***---
       ELABORA-TNOTACR-FATTURATO.
           initialize tno-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move data-ini to tno-data-fattura.
           move cliente  to tno-cod-cli.
           move destino  to tno-prg-destino.
           set  tno-si-agg-contab to true.

           set tutto-ok to true.
           evaluate true also true
           when TuttiClienti also TuttiDestini
                start tnotacr key is >= k-andamento-data
                      invalid set errori to true
                end-start

           when not TuttiClienti also TuttiDestini
                start tnotacr key is >= k-andamento-cliente
                      invalid set errori to true
                end-start

           when not TuttiClienti also not TuttiDestini
                start tnotacr key is >= k-andamento-clides
                      invalid set errori to true
                end-start
           end-evaluate.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next at end        exit perform end-read
                 if tno-data-fattura > data-fine exit perform end-if

                 if tno-no-agg-contab
                    exit perform
                 end-if

                 if not TuttiClienti
                    if tno-cod-cli not = cliente
                       exit perform
                    end-if
                 end-if

                 if not TuttiDestini
                    if tno-prg-destino not = destino
                       exit perform
                    end-if
                 end-if

                 set record-ok to true

                 move tno-cod-cli     to tor-cod-cli
                 move tno-prg-destino to tor-prg-destino
                 move tno-causale     to tor-causale

                 perform VALIDA-ORDINE
                 if record-ok
                    move 0 to tot-documento
                    add  1 to testate-nc-elaborate
                    perform LOOP-RNOTACR-FATTURATO
                    move "N" to tipo-doc
                    move tno-num-fattura  to num-doc
                    move tno-data-fattura to data-doc
                    perform SCRIVI-TXT-FATTURATO
                 end-if

              end-perform
           end-if.

      ***---
       LOOP-RORDINI-FATTURATO.
           set  trovato    to true.
           move low-value  to ror-rec.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read

                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    move tor-prg-destino   to des-prog
                    add 1 to righe-elaborate

OMAGGI              subtract ror-qta-omaggi from ror-qta

                    if ror-qta = 0 move 1 to ror-qta end-if
                    compute tot-documento     =
                            tot-documento     +
                         (( ror-imponib-merce +
                            ror-imp-consumo   +
                            ror-imp-cou-cobat +
                            ror-add-piombo  ) * ror-qta)

                 end-perform
           end-start.

      ***---
       LOOP-RNOTACR-FATTURATO.
           set  trovato    to true.
           move low-value  to rno-rec.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr  next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if

                    if rno-qta = 0 move 1 to rno-qta end-if

                    move tno-prg-destino to des-prog
                    set trovato to true
                    add 1 to righe-nc-elaborate
                    compute tot-documento     =
                            tot-documento     +
                         (( rno-prz-unitario  +
                            rno-imp-consumo   +
                            rno-imp-cou-cobat +
                            rno-add-piombo  ) * rno-qta)

                 end-perform
           end-start.

      ***---
       SCRIVI-TXT-FATTURATO.
           initialize line-riga.
           move cli-tipo   to tcl-codice.
           move cli-codice to des-codice.
           if des-prog not = 0
              read destini  no lock
                   invalid
                   initialize des-ragsoc-1 des-localita
                   string "** DESTINO "     delimited size
                          des-prog          delimited size
                          " NON TROVATO **" delimited size
                          into des-ragsoc-1
                   end-string
              end-read
           else
              initialize des-ragsoc-1 des-localita
           end-if.

           read ttipocli no lock
                invalid
                initialize tcl-descrizione
                string "** TIPOLOGIA "   delimited size
                       tcl-codice        delimited size
                       " NON TROVATA **" delimited size
                       into tcl-descrizione
                end-string
           end-read.

           if cli-gdo not = spaces
              move cli-gdo to gdo-codice
              read tgrupgdo no lock
                   invalid
                   initialize gdo-intestazione
                   string "** GDO "         delimited size
                          gdo-codice        delimited size
                          " NON TROVATO **" delimited size
                          into gdo-intestazione
                   end-string
              end-read
           else
              initialize gdo-intestazione
           end-if.

           string cli-codice           delimited size
                  cli-ragsoc-1         delimited size
                  des-prog             delimited size
                  des-ragsoc-1         delimited size
                  des-localita         delimited size
                  tcl-descrizione      delimited size
                  gdo-codice           delimited size
                  gdo-intestazione     delimited size
                  tipo-doc             delimited size
                  num-doc              delimited size
                  data-doc             delimited size
                  tot-documento        delimited size
                  into line-riga |FATTURATO
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio          delimited size
                  "CHIUSURA FILES " delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.
           if not trovato
              |Richiesta di Sergio: se non c'è nessun record
              |valido per l'elaborazione devo creare comunque 
              |il file con una riga vuota
              write line-riga from spaces
           end-if.
           close tordini
                 rordini
                 clienti
                 destini
                 andamento
                 ttipocli
                 tgrupgdo
                 tmarche
                 tnotacr
                 rnotacr
                 articoli
                 tcaumag
                 lineseq.
           delete file andamento.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.
           initialize como-riga.
           move testate-elaborate to testate-elaborate-edit.
           move righe-elaborate   to righe-elaborate-edit.
           if testate-elaborate > 1 move " TESTATE" to comodo
           else                     move " TESTATA" to comodo
           end-if.
           string r-inizio                 delimited size
                  "ORDINI : ELABORATE "    delimited size
                  testate-elaborate-edit   delimited size
                  comodo                   delimited size
                  " E "                    delimited size
                  righe-elaborate-edit     delimited size
                  " RIGHE"                 delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           initialize como-riga.
           move testate-nc-elaborate to testate-elaborate-edit.
           move righe-nc-elaborate   to righe-elaborate-edit.
           if testate-nc-elaborate > 1 move " TESTATE" to comodo
           else                        move " TESTATA" to comodo
           end-if.
           string r-inizio                 delimited size
                  "NOTE CR: ELABORATE "    delimited size
                  testate-elaborate-edit   delimited size
                  comodo                   delimited size
                  " E "                    delimited size
                  righe-elaborate-edit     delimited size
                  " RIGHE"                 delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           initialize como-riga.

           if tot-secondi < 60
              move tot-secondi to ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                     ss                            delimited size
                     " SECONDI"                    delimited size
                     into como-riga
              end-string
           else
              divide tot-secondi by 60 giving mm remainder ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                     mm                            delimited size
                     " MINUTI E "                  delimited size
                     ss                            delimited size
                     " SECONDI"                    delimited size
                     into como-riga
              end-string
           end-if.
           display como-riga upon syserr.

           initialize last-riga.
           if rotta move 3 to num-cicli end-if.
           if destino = 0 move 99999 to destino end-if.
           if cliente = 0 move 99999 to cliente end-if.
           string "PROGRAMMA: "       delimited size
                  tipo-pgm            delimited size
                  " - TIPO CLI: "     delimited size
                  tipo-cli            delimited size
                  " - GRUPPO GDO: "   delimited size
                  gruppo-gdo          delimited size
                  " - CLIENTE: "      delimited size
                  cliente             delimited size
                  " - DESTINO: "      delimited size
                  destino             delimited size
                  " - MARCA: "        delimited size
                  marca               delimited size
                  " - DAL: "          delimited size
                  save-data-ini(7:2)  delimited size
                  "/"                 delimited size
                  save-data-ini(5:2)  delimited size
                  "/"                 delimited size
                  save-data-ini(1:4)  delimited size
                  " AL: "             delimited size
                  save-data-fine(7:2) delimited size
                  "/"                 delimited size
                  save-data-fine(5:2) delimited size
                  "/"                 delimited size
                  save-data-fine(1:4) delimited size
                  " OMAGGIO: "        delimited size
                  flag-omaggio        delimited size
                  " - CICLI "         delimited size
                  num-cicli           delimited size
                  into last-riga
           end-string.
           display last-riga upon syserr.
           goback.
