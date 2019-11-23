       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-volantini.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl". 
           copy "mrordini.sl".
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "tpromo.sl".
           copy "tparamge.sl".
           copy "lineseq.sl".
           copy "tgrupgdo.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "articoli.sl".
           copy "tescons.sl".
           copy "tvettori.sl".
           copy "tgruppi.sl".

       SELECT tmp-vol-master
           ASSIGN       TO path-tmp-vol-master
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-vol-master
           RECORD KEY   IS tvm-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".           
           copy "mtordini.fd". 
           copy "mrordini.fd".
           copy "tordini.fd". 
           copy "rordini.fd".
           copy "tpromo.fd".  
           copy "tparamge.fd".
           copy "lineseq.fd".
           copy "tgrupgdo.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "articoli.fd".
           copy "tescons.fd".
           copy "tvettori.fd".
           copy "tgruppi.fd".

       FD  tmp-vol-master.
       01 tvm-rec.
           05 tvm-chiave.
               10 tvm-tpr-codice       PIC  9(15).
           05 tvm-dati.
              10 tvm-gdo               pic x(5).
              10 tvm-tpr-descrizione   pic x(50).
              10 tvm-mto-chiave        occurs 999.
                 15 tvm-mto-anno       pic 9(4).
                 15 tvm-mto-numero     pic 9(8).

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "comune.def".
           copy "mail.def".

       78  titolo                value "Mail volantini".

       77  status-mtordini       pic xx.  
       77  status-mrordini       pic xx.    
       77  status-tsetinvio      pic xx.
       77  status-tordini        pic xx.  
       77  status-rordini        pic xx.
       77  status-tpromo         pic xx.
       77  status-tparamge       pic xx.
       77  status-tmp-vol-master pic xx.
       77  status-lineseq        pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-articoli       pic xx.
       77  status-tescons        pic xx.
       77  status-tvettori       pic xx.
       77  status-tgruppi        pic xx.

       77  path-tmp-vol-master   pic x(256).
       77  wstampa               pic x(256).

       77  como-data             pic 9(8).
       77  data-start            pic 9(8).
       77  como-ora              pic 9(8).
       77  des-prog-x            pic x(5).
       77  tpr-codice-x          pic x(18).
       77  mto-numero-x          pic x(8).
       77  como-bloc             pic x(10).
       77  como-stato            pic x(20).
       77  art-codice-x          pic x(6).
       77  mro-qta-x             pic x(8).

       01  dati-evasione. |(comodo)
           05  ror-qta-x             pic x(8).
           05  tor-numero-x          pic x(8).
           05  tor-num-bolla-x       pic x(8).
           05  tor-num-fattura-x     pic x(8).
           05  como-data-ordine      pic x(10).
           05  como-data-bolla       pic x(10).
           05  como-data-fattura     pic x(10).
           05  como-esito            pic x(40).

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini 
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "39"
                set errori to true
                display message "File [RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini 
           when "39"
                set errori to true
                display message "File [MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[mTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [MTORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "39"
                set errori to true
                display message "File [MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
           set RecLocked to false.
           set tutto-ok  to true.
           accept separatore from environment "SEPARATORE".

      ***---
       OPEN-FILES.
           open input mtordini mrordini tordini tescons
                      rordini tpromo tparamge tgrupgdo
                      clienti destini articoli tvettori
                      tgruppi.

           initialize path-tmp-vol-master.
           accept como-ora  from time.
           accept como-data from century-date.
           accept  path-tmp-vol-master from environment "PATH_ST".
           inspect path-tmp-vol-master replacing trailing spaces by 
           low-value.
           string  path-tmp-vol-master    delimited low-value
                   "tmp-vol-master_"      delimited size
                   como-data              delimited size
                   "_"                    delimited size
                   como-ora               delimited size
                   ".tmp"                 delimited size
                   into path-tmp-vol-master
           end-string.                                            
           inspect path-tmp-vol-master replacing trailing low-value by 
           spaces.
           open output tmp-vol-master.                            
           close       tmp-vol-master.
           open i-o    tmp-vol-master.
      
      ***---
       ELABORAZIONE.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           accept como-data from century-date.
           compute como-data = function integer-of-date (como-data).
           subtract tge-gg-mail-volantino from como-data.
           compute data-start = function date-of-integer(como-data).
           move low-value to tpr-rec.
           move data-start to tpr-ini-volantino.
           start tpromo key >= tpr-chiave-volantino
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-ini-volantino not = data-start
                       exit perform
                    end-if                                       
                    initialize tvm-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move tpr-codice      to tvm-tpr-codice
                    move tpr-descrizione to tvm-tpr-descrizione
                    move tpr-gdo         to tvm-gdo
                    write tvm-rec

                    move 0 to mto-chiave idx
                    move low-value  to mro-rec
                    move tpr-codice to mro-promo
                    start mrordini key >= mro-k-promo 
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read mrordini next 
                                  at end exit perform 
                             end-read
                             if mro-promo not = tpr-codice
                                exit perform
                             end-if
                             if mro-chiave-testa not = mto-chiave  
                                add 1 to idx
                                move mro-chiave-testa to mto-chiave
                                                     tvm-mto-chiave(idx)
                                rewrite tvm-rec
                             end-if
                          end-perform
                    end-start
                    if idx = 0
                       delete tmp-vol-master record
                    end-if   
                 end-perform
           end-start.        

           move low-value to tvm-rec.
           start tmp-vol-master key >= tvm-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-vol-master next 
                         at end exit perform 
                    end-read                                        
                    perform CREA-CSV               
                 end-perform
           end-start.
           
      ***---
       CREA-CSV.
           initialize wstampa.
           accept como-ora  from time.
           accept como-data from century-date.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           inspect tvm-gdo replacing trailing spaces by low-value.
           inspect tvm-tpr-descrizione 
                   replacing trailing spaces by low-value.
           string  wstampa delimited low-value
                   tvm-gdo             delimited low-value
                   "_"                 delimited size
                   tvm-tpr-descrizione delimited low-value
                   "_"                 delimited size
                   como-data           delimited size
                   ".csv"              delimited size
                   into wstampa
           end-string.
           open output lineseq.                    
                                                                       
           move tvm-tpr-codice to tpr-codice-x.
           inspect tpr-codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using tpr-codice-x, "L".
           inspect tpr-codice-x replacing trailing spaces by low-value.
                                      
           initialize line-riga
           string "VOLANTINO:"        delimited size
                  separatore          delimited size
                  tpr-codice-x        delimited size
                  separatore          delimited size
                  tvm-tpr-descrizione delimited size
                  into line-riga
           end-string
           write line-riga      
           move tvm-gdo to gdo-codice
           read tgrupgdo 
                invalid move spaces to gdo-intestazione
           end-read
           initialize line-riga
           string "GRUPPO:"           delimited size
                  separatore          delimited size
                  tvm-gdo             delimited size
                  separatore          delimited size
                  gdo-intestazione    delimited size
                  into line-riga
           end-string
           write line-riga
           write line-riga from spaces
           initialize line-riga
           string "DAL:"                 delimited size
                  separatore             delimited size
                  tpr-ini-volantino(7:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(5:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(1:4) delimited size
                  into line-riga
           end-string
           write line-riga            
           initialize line-riga
           string "AL:"                   delimited size
                  separatore              delimited size
                  tpr-fine-volantino(7:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(5:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(1:4) delimited size
                  into line-riga
           end-string
           write line-riga                                
           perform varying idx from 1 by 1 until idx > 9999
              if tvm-mto-anno(idx)   = 0 and
                 tvm-mto-numero(idx) = 0
                 exit perform
              end-if
              write line-riga from spaces
              
              move tvm-mto-chiave(idx) to mto-chiave
              read mtordini no lock     

              initialize line-riga
              string "ANNO"            delimited size
                     separatore        delimited size
                     "MASTER"          delimited size
                     separatore        delimited size
                     "DESTINO"         delimited size
                     separatore        delimited size
                     "CLIENTE"         delimited size
                     separatore        delimited size
                     "NR. ORDINE"      delimited size
                     separatore        delimited size
                     "DATA ORDINE"     delimited size
                     separatore        delimited size
                     "STATO"           delimited size
                     separatore        delimited size
                     "BLOCCATO"        delimited size
                     separatore        delimited size
                     "DATA CREAZIONE"  delimited size
                     separatore        delimited size
                     separatore        delimited size
                     "DATA CONSEGNA"   delimited size
                     separatore        delimited size
                     into line-riga
              end-string
              write line-riga
              set cli-tipo-C to true
              move mto-cod-cli to cli-codice des-codice
              read clienti no lock 
                   invalid move "NON TROVATO" to cli-ragsoc-1
              end-read

              if mto-prg-destino not = 0
                 move mto-prg-destino to des-prog
                 read destini no lock invalid continue end-read
              else
                 move cli-ragsoc-1 to des-ragsoc-1
                 move cli-localita to des-localita
              end-if
              
              inspect des-localita 
                      replacing trailing spaces by low-value
              inspect des-ragsoc-1
                      replacing trailing spaces by low-value
              move des-prog to des-prog-x
              inspect des-prog-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using des-prog-x, "L"
              inspect des-prog-x replacing trailing spaces by low-value
              move mto-numero to mto-numero-x
              inspect mto-numero-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using mto-numero-x, "L"
              inspect mto-numero-x 
                      replacing trailing spaces by low-value                
              move spaces to como-bloc
              if mto-bloccato            
                 evaluate true
                 when mto-causale-blocco-manuale 
                      move "MANUALE" to como-bloc
                 when mto-causale-blocco-prezzo  
                      move "PREZZO"  to como-bloc
                 when mto-causale-blocco-fido    
                      move "FIDO"    to como-bloc
                 end-evaluate                                               
              end-if

              evaluate true
              when mto-registrato     move "REGISTRATO"    to como-stato 
              when mto-in-lavorazione move "IN LAVORAZIONE"to como-stato 
              when mto-sped-parz      move "SPEDITO PARZ"  to como-stato 
              when mto-sped-tot       move "SPEDITO TOT"   to como-stato
              when mto-chiuso         move "CHIUSO"        to como-stato
              end-evaluate

              initialize line-riga
              string mto-anno                delimited size
                     separatore              delimited size
                     mto-numero              delimited low-value
                     separatore              delimited size
                     des-ragsoc-1            delimited low-value
                     " - "                   delimited size
                     des-prog-x              delimited low-value
                     " - "                   delimited size
                     des-localita            delimited low-value
                     separatore              delimited size
                     cli-ragsoc-1            delimited low-value
                     separatore              delimited size
                     mto-num-ord-cli         delimited size     
                     separatore              delimited size        
                     mto-data-ordine(7:2)    delimited size
                     "/"                     delimited size
                     mto-data-ordine(5:2)    delimited size
                     "/"                     delimited size
                     mto-data-ordine(1:4)    delimited size     
                     separatore              delimited size
                     como-stato              delimited size     
                     separatore              delimited size
                     como-bloc               delimited size     
                     separatore              delimited size                       
                     mto-data-creazione(7:2) delimited size
                     "/"                     delimited size
                     mto-data-creazione(5:2) delimited size
                     "/"                     delimited size
                     mto-data-creazione(1:4) delimited size     
                     separatore              delimited size
                     separatore              delimited size                       
                     mto-data-note1(7:2)     delimited size
                     "/"                     delimited size
                     mto-data-note1(5:2)     delimited size
                     "/"                     delimited size
                     mto-data-note1(1:4)     delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces

              initialize line-riga
              string separatore       delimited size
                     separatore       delimited size
                     "ARTICOLO"       delimited size
                     separatore       delimited size
                     "PROMO"          delimited size
                     separatore       delimited size
                     "ORDINATA"       delimited size
                     separatore       delimited size
                     "EVASA"          delimited size
                     separatore       delimited size
                     "STATO"          delimited size
                     separatore       delimited size
                     "NR. EVASIONE"   delimited size
                     separatore       delimited size
                     "DATA EVASIONE"  delimited size
                     separatore       delimited size
                     "BOLLA"          delimited size
                     separatore       delimited size
                     "DATA BOLLA"     delimited size
                     separatore       delimited size
                     "FATTURA"        delimited size
                     separatore       delimited size
                     "DATA FATTURA"   delimited size
                     separatore       delimited size
                     "STATO CONSEGNA" delimited size
                     into line-riga
              end-string
              write line-riga

              move low-value  to mro-rec
              move mto-chiave to mro-chiave-testa
              start mrordini key >= mro-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read mrordini next at end exit perform end-read
                       if mro-chiave-testa not = mto-chiave
                          exit perform
                       end-if  
                       move mro-cod-articolo to art-codice art-codice-x
                       read articoli no lock
                                                                       
                       inspect art-codice-x
                               replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using art-codice-x, "L"
                       inspect art-codice-x
                               replacing trailing spaces by low-value
                       
                       if mro-promo = 0
                          move spaces to tpr-codice tpr-descrizione
                       else    
                          move mro-promo to tpr-codice
                          read tpromo no lock invalid continue end-read
                       end-if                 
                       move tpr-codice to tpr-codice-x  
                       inspect tpr-codice-x
                               replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using tpr-codice-x, "L"
                       inspect tpr-codice-x
                               replacing trailing spaces by low-value
                       inspect tpr-descrizione
                               replacing trailing spaces by low-value
                       move mro-qta to mro-qta-x  
                       inspect mro-qta-x
                               replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using mro-qta-x, "L"
                       inspect mro-qta-x
                               replacing trailing spaces by low-value

                       evaluate true
                       when mro-registrato   
                            move "REGISTRATO"     to como-stato
                       when mro-in-lavorazione             
                            move "IN LAVORAZIONE" to como-stato
                       when mro-sped-parz                  
                            move "SPEDITO PARZ"   to como-stato
                       when mro-sped-tot                   
                            move "SPEDITO TOT"    to como-stato
                       when mro-chiuso                     
                            move "CHIUSO"         to como-stato
                       end-evaluate
                                               
                       initialize dati-evasione
                       perform RICERCA-EVASIONI

                       if not trovato
                          perform SCRIVI-RIGA
                       else
                          if mro-qta > mro-qta-e
                             perform SCRIVI-RIGA-SALDO
                          end-if
                       end-if             

                    end-perform
              end-start
              write line-riga from spaces
              write line-riga from spaces
           end-perform.
           close lineseq.
                                 
           move tvm-gdo to gdo-codice
           read tgrupgdo 
                invalid continue
           end-read.

           move gdo-gruppo-invio to tgr-codice.
           read tgruppi no lock 
                invalid continue
            not invalid
                move tgr-email to LinkAddress
                accept LinkSubject  from environment "MAIL_VOL_SUBJECT"
                accept LinkBody     from environment "MAIL_VOL_BODY"   
                accept LinkAddressFrom from environment "MAIL_VOL_FROM"
                move wstampa to LinkAttach
                move "mail-volantini" to NomeProgramma
                perform SEND-MAIL
           end-read.

      ***---
       RICERCA-EVASIONI.
           set trovato to false.
           move low-value to ror-rec.
           move mro-chiave-testa to ror-chiave-ordine-testa.
           move mro-progr        to ror-progr-master.
           start rordini key >= ror-k-master
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-chiave-ordine-testa not = mro-chiave-testa or
                       ror-progr-master        not = mro-progr
                       exit perform
                    end-if
                    add 1 to idx
                    move ror-anno       to tor-anno
                    move ror-num-ordine to tor-numero
                    read tordini no lock
                    move ror-qta to ror-qta-x
                    inspect ror-qta-x
                            replacing leading x"30" by x"20"
                    call "C$JUSTIFY" using ror-qta-x, "L"
                    inspect ror-qta-x
                            replacing trailing spaces by low-value
                                                                     
                    move tor-numero to tor-numero-x  
                    inspect tor-numero-x
                            replacing leading x"30" by x"20"
                    call "C$JUSTIFY" using tor-numero-x, "L"
                    inspect tor-numero-x
                            replacing trailing spaces by low-value

                    string tor-data-ordine(7:2)     delimited size
                           "/"                      delimited size
                           tor-data-ordine(5:2)     delimited size
                           "/"                      delimited size
                           tor-data-ordine(1:4)     delimited size
                           into como-data-ordine
                    end-string

                    if tor-data-bolla not = 0
                       string tor-data-bolla(7:2)     delimited size
                              "/"                     delimited size
                              tor-data-bolla(5:2)     delimited size
                              "/"                     delimited size
                              tor-data-bolla(1:4)     delimited size
                              into como-data-bolla
                       end-string

                       move tor-num-bolla to tor-num-bolla-x
                       inspect tor-num-bolla-x
                               replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using tor-num-bolla-x, "L"
                       inspect tor-num-bolla-x
                               replacing trailing spaces by low-value
                    end-if

                    if tor-data-fattura not = 0
                       string tor-data-fattura(7:2)   delimited size
                              "/"                     delimited size
                              tor-data-fattura(5:2)   delimited size
                              "/"                     delimited size
                              tor-data-fattura(1:4)   delimited size
                              into como-data-fattura
                       end-string
        
                       move tor-num-fattura to tor-num-fattura-x  
                       inspect tor-num-fattura-x
                               replacing leading x"30" by x"20"
                       call "C$JUSTIFY" using tor-num-fattura-x, "L"
                       inspect tor-num-fattura-x
                               replacing trailing spaces by low-value
                    end-if
                    if tor-vettore = 0
                       move spaces to como-esito
                    else
                       move tor-vettore to vet-codice
                       read tvettori no lock

                       if tor-esito-consegna not = space
                          move tor-esito-consegna to tec-codice
                          read tescons
                               invalid
                               move "ESITO INESITENT" to tec-descrizione
                          end-read
                          string vet-sigla       delimited size
                                 " - "           delimited size
                                 tec-descrizione delimited size
                                 into como-esito
                          end-string
                       else
                          move vet-sigla to como-esito
                       end-if
                    end-if

                    if not trovato
                       set trovato to true
                    end-if

                    if trovato
                       perform SCRIVI-RIGA    
                       initialize art-codice-x
                                  art-descrizione
                                  tpr-descrizione
                                  mro-qta-x
                    end-if

                 end-perform
           end-start.
                  
      ***---
       SCRIVI-RIGA.               
           initialize line-riga.       
           string separatore          delimited size
                  separatore          delimited size
                  art-codice-x        delimited low-value
                  " - "               delimited size
                  art-descrizione     delimited size
                  separatore          delimited size
                  tpr-descrizione     delimited low-value
                  separatore          delimited size
                  mro-qta-x           delimited low-value
                  separatore          delimited size
                  ror-qta-x           delimited low-value
                  separatore          delimited size
                  como-stato          delimited low-value
                  separatore          delimited size
                  tor-numero-x        delimited low-value
                  separatore          delimited size    
                  como-data-ordine    delimited size
                  separatore          delimited size
                  tor-num-bolla-x     delimited low-value
                  separatore          delimited size
                  como-data-bolla     delimited size
                  separatore          delimited size
                  tor-num-fattura-x   delimited low-value
                  separatore          delimited size
                  como-data-fattura   delimited size
                  separatore          delimited size
                  como-esito          delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       SCRIVI-RIGA-SALDO.                                            
           subtract mro-qta-e from mro-qta.
           move mro-qta to mro-qta-x.
           inspect mro-qta-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using mro-qta-x, "L".
           inspect mro-qta-x replacing trailing spaces by low-value.

           evaluate true
           when mto-bloccato
                move "BLOCC"  to como-stato
           when mto-chiuso
                move "CHIUSO" to como-stato
           when other
                if mro-chiuso
                   move "CHIUSO" to como-stato
                else
                   move "REG"    to como-stato
                end-if
           end-evaluate.

           initialize line-riga.       
           string separatore          delimited size
                  separatore          delimited size
                  separatore          delimited size
                  separatore          delimited size
                  mro-qta-x           delimited low-value
                  separatore          delimited size
                  separatore          delimited size
                  como-stato          delimited low-value
                  into line-riga
           end-string.
           write line-riga.


      ***---
       CLOSE-FILES.
           close tordini rordini mtordini clienti destini articoli
                mrordini tpromo tparamge tgrupgdo tescons tvettori
                 tgruppi.
           close       tmp-vol-master.
           delete file tmp-vol-master.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
