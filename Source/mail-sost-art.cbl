       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-sost-art.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl". 
           copy "destini.sl".
           copy "clienti.sl".
           copy "tgrupgdo.sl".
           copy "articoli.sl".
           copy "tmp-sost-art.sl".
           copy "ttipocli.sl".
           COPY "lineseq.sl".
           COPY "lineseq.sl"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "tsetinvio.fd".
           COPY "mtordini.fd".
           copy "destini.fd".
           copy "clienti.fd".
           copy "tgrupgdo.fd".
           copy "articoli.fd".
           copy "tmp-sost-art.fd".
           copy "ttipocli.fd".
           COPY "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

       WORKING-STORAGE SECTION.
           copy "comune.def".
           copy "mail.def".

       77  status-mtordini      pic xx.  
       77  status-tsetinvio     pic xx.
       77  status-destini       pic xx.
       77  status-clienti       pic xx.
       77  status-tgrupgdo      pic xx.
       77  status-articoli      pic xx.
       77  status-tmp-sost-art  pic xx.
       77  status-ttipocli      pic xx.
       77  STATUS-lineseq       PIC  X(2).
       77  STATUS-lineseq1      PIC  X(2).
       77  wstampa              PIC  X(256).
       77  path-tmp-sost-art    PIC  X(256).


       78  titolo    
           value "Invio Mail sostituzione articoli negli ordini master".

       77  tentativi      pic 99.
       77  num-edit       pic z(8).
       77  num-edit-2     pic z(8).
       77  cont           pic 9(4).
       77  cont2          pic 9(4).
       77  cont3          pic 9(4).
       77  como-linkbody  pic x(3000).
       77  sostituto      pic x(50).
       77  da-sostituire  pic x(2).
       77  sost-articoli  pic x.

       01  r-art-orig.
           05 filler pic x(5) value "Cod: ".
           05 rao-art  pic z(6).
           05 filler   pic x(16) value " - Descrizione: ".
           05 rao-art-descr  pic x(20).
           05 filler   pic x(6) value " Qta: ".
           05 rao-qta  pic z(8).

       01  r-art-dest.
           05 filler pic x(5) value "Cod: ".
           05 rad-art  pic z(6).
           05 filler   pic x(16) value " - Descrizione: ".
           05 rad-art-descr  pic x(20).
           05 filler   pic x(6) value " Qta: ".
           05 rad-qta  pic z(8).

       LINKAGE SECTION.
           copy "link-mail-sost-art.def".

      ******************************************************************


       PROCEDURE DIVISION using mail-sost-art-linkage.

       DECLARATIVES.
       mtordini-ERR SECTION.
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
                display message "[MTORDINI] Indexed file corrupt!"
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
      *                     
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
                  x"0d0a""File delle righe [DESTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
      
       CLIENTI-ERR SECTION.
           use after error procedure on CLIENTI.
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
                  x"0d0a""File delle righe [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

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
                  x"0d0a""File delle righe [TGRUPGDO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

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
                  x"0d0a""File delle righe [ARTICOLI] inesistente"
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
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked     to false.
           set tutto-ok      to true.
           move msa-path-tmp to path-tmp-sost-art.

      ***---
       OPEN-FILES.
           open input mtordini.
           open input destini
           open input clienti.
           open input tgrupgdo.
           open input articoli.
           open input tmp-sost-art.
           open input ttipocli.

      ***---
       ELABORAZIONE.

           move msa-mto-chiave to mto-chiave
           read mtordini no lock
              invalid
                 continue
           end-read.

           perform INVIO.

      ***---
       INVIO.
           perform PREPARA-INDIRIZZO.
           perform PREPARA-INDIRIZZO-CC.
           if LinkAddress       = spaces and
              LinkAddressCC not = spaces
              move LinkAddressCC to LinkAddress
              move spaces        to LinkAddressCC
           end-if.
           if LinkAddress = space and LinkAddressCC = space
              exit paragraph
           end-if.
           perform PREPARA-SUBJECT.
           perform PREPARA-CORPO.
           perform PREPARA-FROM.
           move space   to LinkAttach.

           set errori to true.
           move 0 to tentativi.
           move "mail-sost-art" to NomeProgramma.
           perform 5 times
              add 1 to tentativi
              perform SEND-MAIL
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1
           end-perform.

      ***---
       PREPARA-SUBJECT.
           accept LinkSubject from environment "CATART_SUBJECT".

      ***---
       PREPARA-CORPO.
           initialize linkBody

           perform PREPARA-DATI-ORDINE.
           perform PREPARA-SOSTITUZIONI.


      ***---
       PREPARA-DATI-ORDINE.
           move "DATI ORDINE:"  to linkBody
           perform DIVISORIO


      *     if mto-gdo not = space
      *        move mto-gdo   to gdo-codice
      *        read tgrupgdo
      *           invalid
      *              initialize gdo-intestazione
      *        end-read
      *        inspect gdo-codice replacing trailing space by low-value
      *        string linkbody         delimited by low-value
      *               "GRUPPO GDO: "   delimited by size
      *               gdo-codice       delimited by low-value
      *               " - "            delimited by size
      *               gdo-intestazione delimited by size
      *               x"0D0A"          delimited by size
      *               into linkBody
      *     end-if.

           set cli-tipo-c    to true
           move mto-cod-cli  to cli-codice
           read clienti
              invalid
                 initialize cli-dati
           end-read
           inspect cli-ragsoc-1 replacing trailing space by low-value
           inspect cli-ragsoc-2 replacing trailing space by low-value
           inspect cli-localita replacing trailing space by low-value
           inspect linkBody replacing trailing space by low-value
           string linkBody      delimited by low-value
                  "CLIENTE: "   delimited by size
                  cli-ragsoc-1  delimited by low-value
                  " "           delimited by size
                  cli-ragsoc-2  delimited by low-value
      *            " "           delimited by size
      *            cli-localita  delimited by low-value
                  x"0D0A"       delimited by size
                  into linkBody

           if mto-prg-destino not = zero
              move mto-cod-cli     to des-codice
              move mto-prg-destino to des-prog
              read destini
                 invalid
                    initialize des-dati
              end-read
              inspect des-ragsoc-1 replacing trailing space by low-value
              inspect des-ragsoc-2 replacing trailing space by low-value
              inspect des-localita replacing trailing space by low-value
              inspect linkBody replacing trailing space by low-value
              string linkBody      delimited by low-value
                     "DESTINO: "   delimited by size
                     des-ragsoc-1  delimited by low-value
                     " "           delimited by size
                     des-ragsoc-2  delimited by low-value
                     " "           delimited by size
                     des-localita  delimited by low-value
                     x"0D0A"       delimited by size
                     into linkBody
           else
              string linkBody      delimited by low-value
                     "DESTINO: "   delimited by size
                     cli-ragsoc-1  delimited by low-value
                     " "           delimited by size
                     cli-ragsoc-2  delimited by low-value
                     " "           delimited by size
                     cli-localita  delimited by low-value
                     x"0D0A"       delimited by size
                     into linkBody
           end-if.


           inspect linkBody replacing trailing space by low-value

           move mto-numero   to num-edit
           call "C$JUSTIFY" using num-edit, "L"
           inspect num-edit replacing trailing space by low-value


           string linkBody      delimited by low-value
                  "N. MASTER: "
                  mto-anno      delimited by size
                  "/"           delimited by size
                  num-edit      delimited by low-value
                  into linkBody

           if mto-num-ord-cli not = space
              inspect linkBody replacing trailing space by low-value
              inspect mto-num-ord-cli
                                replacing trailing space by low-value
              string linkBody         delimited by low-value
                     "NR. ORDINE CLIENTE: " delimited by size
                     mto-num-ord-cli  delimited by low-value
                     into linkBody
           end-if           

           inspect linkBody replacing trailing space by low-value
           string linkBody                      delimited by low-value
                  " del "                            delimited by size
                  mto-data-ordine(7:2)               delimited by size
                  "/"                                delimited by size
                  mto-data-ordine(5:2)               delimited by size
                  "/"                                delimited by size
                  mto-data-ordine(1:4)               delimited by size
                  into linkBody.

      ***---
       PREPARA-SOSTITUZIONI.
           inspect linkBody replacing trailing space by low-value
           string linkBody                      delimited by low-value
                  x"0D0A"                            delimited by size
                  x"0D0A"                            delimited by size
                  x"0D0A"                            delimited by size
                  "LISTA SOSTITUZIONI EFFETTUATE:"   delimited by size
                  into linkBody.

           perform DIVISORIO.
      
           move low-value to tmp-sar-chiave
           start tmp-sost-art key not < tmp-sar-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-sost-art next no lock
                       at end
                          exit perform
                    end-read
                    perform PREPARA-ARTICOLO
                 end-perform
           end-start.

      ***---
       PREPARA-ARTICOLO.
      *
      *                05 tmp-sar-chiave.
      *         10 tmp-sar-codice-orig          PIC  9(6).
      *         10 tmp-sar-imb-orig PIC  x(3).
      *         10 tmp-sar-ean-orig PIC  9(13).
      *         10 tmp-sar-codice-dest          PIC  9(6).
      *         10 tmp-sar-imb-dest PIC  x(3).
      *         10 tmp-sar-ean-dest PIC  9(13).



           move tmp-sar-codice-orig   to art-codice
                                         rao-art
           read articoli no lock
              invalid
                 continue
           end-read

           move art-descrizione to rao-art-descr
      *    metto i puntini
           if art-descrizione(21:2) not = space
              move "..."  to rao-art-descr(18:3)
           end-if.

           move tmp-sar-qta   to rao-qta
           call "C$JUSTIFY" using rao-qta, "L"




           move tmp-sar-codice-dest   to art-codice
                                         rad-art
           read articoli no lock
              invalid
                 continue
           end-read

           move art-descrizione to rad-art-descr
      *    metto i puntini
           if art-descrizione(21:2) not = space
              move "..."  to rad-art-descr(18:3)
           end-if.

           move tmp-sar-qta   to rad-qta
           call "C$JUSTIFY" using rad-qta, "L"


           inspect linkbody replacing trailing space by low-value

           string linkbody            delimited by low-value
                  x"0D0A"             delimited by size
                  "Articolo;"         delimited by size
                  x"0D0A"             delimited by size
                  r-art-orig          delimited by size       
                  x"0D0A"             delimited by size
                  r-art-dest          delimited by size       
                  x"0D0A"             delimited by size
                  "Prezzo invariato." delimited by size
                  into linkbody.

           perform DIVISORIO.

      *****************************************

      *     move tmp-sar-codice-orig   to art-codice
      *                                   num-edit
      *     read articoli no lock
      *        invalid
      *           continue
      *     end-read
      *
      *
      *     call "C$JUSTIFY" using num-edit, "L"
      *     inspect num-edit replacing trailing space by low-value
      *     move tmp-sar-qta-imb-orig  to num-edit-2
      *     call "C$JUSTIFY" using num-edit-2, "L"
      *     inspect num-edit-2 replacing trailing space by low-value
      *     inspect linkbody replacing trailing space by low-value
      *     inspect art-descrizione replacing trailing space by low-value
      *     inspect tmp-sar-descr-imb-orig 
      *                             replacing trailing space by low-value
      *
      *     string linkbody               delimited by low-value
      *            "L'articolo "          delimited by size
      *            num-edit               delimited by low-value
      *            " - "                  delimited by size
      *            tmp-sar-descr-imb-orig delimited by low-value
      *            " da "                 delimited by size
      *            num-edit-2             delimited by low-value
      *            " è stato sostituito dall'articolo " delimited by size
      *            into linkbody
      *
      *     move tmp-sar-codice-dest   to art-codice
      *                                   num-edit
      *     read articoli no lock
      *        invalid
      *           continue
      *     end-read
      *
      *     call "C$JUSTIFY" using num-edit, "L"
      *     inspect num-edit replacing trailing space by low-value
      *     move tmp-sar-qta-imb-dest  to num-edit-2
      *     call "C$JUSTIFY" using num-edit-2, "L"
      *     inspect num-edit-2 replacing trailing space by low-value
      *     inspect linkbody replacing trailing space by low-value
      *     inspect art-descrizione replacing trailing space by low-value
      *     inspect tmp-sar-descr-imb-dest 
      *                             replacing trailing space by low-value
      *
      *     string linkbody               delimited by low-value
      *            "L'articolo "          delimited by size
      *            num-edit               delimited by low-value
      *            " - "                  delimited by size
      *            tmp-sar-descr-imb-dest delimited by low-value
      *            " da "                 delimited by size
      *            num-edit-2             delimited by low-value
      *            ". Prezzo invarito"    delimited by size
      *            x"0D0A"                delimited by size
      *            x"0D0A"                delimited by size
      *            into linkbody.

      ***---
       DIVISORIO.
           inspect linkBody replacing trailing space by low-value
           string linkBody   delimited by low-value
                 x"0D0A"     delimited by size
                  "_______________________________________" 
                             delimited by size
                 x"0D0A"     delimited by size
                  into linkBody.



      ***---
       PREPARA-INDIRIZZO. 
           accept sost-articoli from environment "SOST_ARTICOLI".
           evaluate sost-articoli
           when "1"
                move mto-cod-cli  to cli-codice
                set cli-tipo-c to true
                read clienti no lock
                     invalid
                     move spaces to cli-email
                end-read
                move cli-email   to LinkAddress
           when "2"
                move mto-cod-cli to cli-codice
                set cli-tipo-c   to true
                read clienti no lock
                     invalid move spaces to tcl-mail-comm
                 not invalid
                     move cli-tipo to tcl-codice
                     read ttipocli no lock
                          invalid move spaces to tcl-mail-comm
                     end-read
                end-read
                move tcl-mail-comm to LinkAddress
           end-evaluate.

      ***---
       PREPARA-INDIRIZZO-CC.
      *     accept LinkAddressCC from environment "CATART_ADDRESSES_CC".
           if sost-articoli = "1"
              move cli-tipo to tcl-codice
              read ttipocli no lock
                   invalid move spaces to tcl-mail-comm
              end-read
              move tcl-mail-comm to LinkAddress
           end-if.

           initialize cont
           inspect LinkAddressCC replacing trailing space by low-value
           inspect LinkAddressCC 
                       tallying cont for characters before low-value
           if cont not = zero
              if LinkAddressCC(cont:1) = ";"
                 move low-value to LinkAddressCC(cont:1)
              end-if
           end-if
           inspect LinkAddressCC replacing trailing low-value by space.

      ***---
       PREPARA-FROM.
           accept LinkAddressFrom 
                  from environment "CATART_ADDRESSES_FROM"
           if LinkAddressFrom = space
              move "info@lubex.it" to LinkAddressFrom
           end-if.

      ***---
       CLOSE-FILES.
           close mtordini
                 clienti
                 tgrupgdo
                 articoli
                 destini
                 tmp-sost-art
                 ttipocli.

      ***---
       EXIT-PGM.
           goback.

      ***---
           copy "mail.cpy".


