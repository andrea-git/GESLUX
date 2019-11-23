       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-catart.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "articoli.sl". 
           copy "catart.sl".
           copy "clienti.sl".
           copy "progmag.sl".
           copy "timbalqta.sl".
           copy "timballi.sl".
           COPY "lineseq.sl".
           COPY "lineseq.sl"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           COPY "articoli.fd".
           copy "catart.fd".
           copy "clienti.fd".
           copy "progmag.fd".
           copy "timbalqta.fd".
           copy "timballi.fd".
           COPY "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

       WORKING-STORAGE SECTION.
           copy "comune.def".
           copy "mail.def".

       77  status-articoli   pic xx.  
       77  status-tsetinvio  pic xx.
       77  status-catart     pic xx.
       77  status-clienti    pic xx.
       77  status-progmag    pic xx.
       77  status-timbalqta  pic xx.
       77  status-timballi   pic xx.
       77  STATUS-lineseq    PIC  X(2).
       77  STATUS-lineseq1   PIC  X(2).
       77  wstampa           PIC  X(256).

       78  titolo    value "Invio Mail variazione catena articoli".

       01                    pic 9.
           88 invia          value 1 false zero.
       77  tentativi        PIC  99.

       77  cont           pic 9(4).
       77  cont2          pic 9(4).
       77  cont3          pic 9(4).
       77  como-linkbody  pic x(3000).

       77  stringa-art    pic x(1000).
       77  stringa-art-1  pic x(1000).
       77  stringa-art-2  pic x(1000).

       77  art-1          pic 9(6).
       77  art-2          pic 9(6).
       77  como-articolo  pic 9(6).
       77  art-ed         pic z(6).
       77  como-ean       pic 9(13).

       01  como-prg-chiave.
           10 como-prg-cod-articolo   PIC  9(6).
           10 como-prg-cod-magazzino  PIC  X(3).
           10 como-prg-tipo-imballo   PIC  X(3).
           10 como-prg-peso           PIC  9(5)V9(3).

       77  como-giacenza     pic 9(8).
       77  giacenza-max      pic s9(8).

       77  imballi-ed        pic z(4).
       77  peso-ed           PIC  zz.zz9,9(3).
       LINKAGE SECTION.
       77  link-articolo     pic 9(6).

      ******************************************************************
       PROCEDURE DIVISION using link-articolo.

       DECLARATIVES.
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
                  x"0d0a""File delle testate [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       CATART-ERR SECTION.
           use after error procedure on catart.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-catart
           when "39"
                set errori to true
                display message "File [CATART] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CATART] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [CATART] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       PROGMAG-ERR SECTION.
           use after error procedure on PROGMAG.
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
                  x"0d0a""File delle righe [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TIMBALQTA-ERR SECTION.
           use after error procedure on TIMBALQTA.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TIMBALQTA
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
                  x"0d0a""File delle righe [TIMBALQTA] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TIMBALLI-ERR SECTION.
           use after error procedure on TIMBALLI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TIMBALLI
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
                  x"0d0a""File delle righe [TIMBALLI] inesistente"
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
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           open input articoli
           open input catart
           open input clienti.
           open input progmag.
           open input timbalqta.
           open input timballi.

      ***---
       ELABORAZIONE.
           perform PREPARA-PARTI-MAIL.
      *    i controlli li devo sempre fare riferiti all'ultimo articolo
      *    della catena
           move art-2   to art-codice
           read articoli
              invalid
                 continue
           end-read.

           set cli-tipo-C to true
           move low-value to cli-codice 
           start clienti key not < cli-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read clienti next no lock
                       at end
                          exit perform
                    end-read
                    if not cli-tipo-C
                       exit perform
                    end-if
                    if not cli-sost-no
                       perform ELABORA-RECORD
                    end-if
                 end-perform
           end-start.
                          
      ***---
       ELABORA-RECORD.
           set invia   to false

           evaluate true also true
           when cli-tipo-art-gruppi      also art-si-gruppi
           when cli-tipo-art-specialist  also art-si-specialist
           when cli-tipo-art-DO          also art-si-do
           when cli-tipo-art-GDA         also art-si-gda
           when cli-tipo-art-GDS         also art-si-gds
           when cli-tipo-art-ESTERO      also art-si-estero
                set invia   to true
           end-evaluate.

           if cli-email = space
              set invia   to false
           end-if

           if invia
              perform INVIO
           end-if.

      ***---
       PREPARA-PARTI-MAIL.
           perform PREPARA-SUBJECT.
           perform PREPARA-CORPO.
           perform PREPARA-INDIRIZZO-CC.
           perform PREPARA-FROM.
           move space   to LinkAttach.

      ***---
       INVIO.
      *****     perform PREPARA-INDIRIZZO.
      *****
      *****     set errori to true.
      *****     move 0 to tentativi.
      *****     move "mail-catart" to NomeProgramma.
      *****     perform 5 times
      *****        add 1 to tentativi
      *****        perform SEND-MAIL
      *****
      *****        open input lineseq1
      *****        read  lineseq1 next
      *****        if line-riga of lineseq1 = "True"
      *****           set tutto-ok to true
      *****           close lineseq1
      *****           exit perform
      *****        end-if
      *****        close lineseq1
      *****     end-perform.

      ***---
       PREPARA-SUBJECT.
           accept LinkSubject from environment "CATART_SUBJECT"
           if LinkSubject = space
              move "Sostituzione Articoli"  to LinkSubject
           end-if.

      ***---
       PREPARA-INDIRIZZO. |05/07: Non mandare nulla al cliente
      *****     move cli-email   to LinkAddress.

      ***---
       PREPARA-INDIRIZZO-CC.
           accept LinkAddressCC from environment "CATART_ADDRESSES_CC".

      ***---
       PREPARA-FROM.
           accept LinkAddressFrom 
                               from environment "CATART_ADDRESSES_FROM".
           if LinkAddressFrom = space
              move "info@lubex.it" to LinkAddressFrom
           end-if.

      ***---
       PREPARA-CORPO.
           perform ARTICOLI-CATENA
           initialize linkBody

           perform PREPARA-ART-1
           perform PREPARA-ART-2

           accept linkBody from environment "CATART_MASTER_BODY"
      *    controllo di dover inserire i dati dell'articolo 1
           initialize cont
           inspect linkBody tallying cont for all "$1"
           if cont not = zero
              perform cont times
                 initialize cont2
                 inspect linkBody tallying cont2 
                                         for characters before "$1"
                 add 3 to cont2 giving cont3
                 initialize como-linkbody
                 if cont2 = zero
                    string stringa-art-1       delimited by low-value
                           linkbody(cont3:)    delimited by size
                          into como-linkbody
                 else
                    string linkbody(1:cont2)   delimited by size
                           stringa-art-1       delimited by low-value
                           linkbody(cont3:)    delimited by size
                           into como-linkbody
                 end-if
                 move como-linkbody   to linkbody
              end-perform
           end-if

      *    controllo di dover inserire l'anno dell'ordine
           initialize cont
           inspect linkBody tallying cont for all "$2"
           if cont not = zero
              perform cont times
                 initialize cont2
                 inspect linkBody tallying cont2 
                                         for characters before "$2"
                 add 3 to cont2 giving cont3
                 initialize como-linkbody
                 if cont2 = zero
                    string stringa-art-2       delimited by low-value
                           linkbody(cont3:)    delimited by size
                           into como-linkbody
                 else
                    string linkbody(1:cont2)   delimited by size
                           stringa-art-2       delimited by low-value
                           linkbody(cont3:)    delimited by size
                           into como-linkbody
                 end-if
                 move como-linkbody   to linkbody
              end-perform
           end-if.

      ***---
       PREPARA-ART-1.
           move art-1  to como-articolo
           perform PREPARA-ART
           move stringa-art  to stringa-art-1.

      ***---
       PREPARA-ART-2.
           move art-2  to como-articolo
           perform PREPARA-ART
           move stringa-art  to stringa-art-2.

      ***---
       PREPARA-ART.
           initialize stringa-art.

           move como-articolo   to art-ed
                                   art-codice
           call "C$JUSTIFY" using art-ed, "L"
           inspect art-ed replacing trailing space by low-value

           read articoli 
              invalid
                 continue
           end-read.

           string x"0D0A"          delimited by size
                  x"0D0A"          delimited by size
                  art-ed           delimited by low-value
                  " - "            delimited by size
                  art-descrizione  delimited by size
                  into stringa-art

           move zero   to como-ean
           if art-codice-ean-1 not = zero
              move art-codice-ean-1   to como-ean
           end-if
           if art-codice-ean-2 not = zero
              move art-codice-ean-2   to como-ean
           end-if
           if art-codice-ean-3 not = zero
              move art-codice-ean-3   to como-ean
           end-if
           if art-codice-ean-4 not = zero
              move art-codice-ean-4   to como-ean
           end-if
           if art-codice-ean-5 not = zero
              move art-codice-ean-5   to como-ean
           end-if

           if como-ean not = zero
              inspect stringa-art replacing trailing space by low-value
              string stringa-art      delimited by low-value
                     x"0D0A"          delimited by size
                     "Codice EAN: "   delimited by size
                     como-ean         delimited by size
                     into stringa-art
           end-if.

           perform FIND-PROGMAG.


           move prg-tipo-imballo   to imq-codice
           read timbalqta
              invalid
                 continue
           end-read
           move imq-tipo  to imb-codice
           read timballi
              invalid
                 continue
           end-read

           move imq-qta-imb  to imballi-ed
           call "C$JUSTIFY"  using imballi-ed, "L"
           inspect imballi-ed replacing trailing space by low-value
           move prg-peso  to peso-ed
           call "C$JUSTIFY"  using peso-ed, "L"
           inspect peso-ed replacing trailing space by low-value

           inspect stringa-art replacing trailing space by low-value

           inspect imb-descrizione replacing trailing spaces 
                                          by low-value.

           inspect art-udm-imballo replacing trailing spaces 
                                          by low-value.

           string stringa-art      delimited by low-value
                 x"0D0A"           delimited by size
                 imb-descrizione   delimited low-value
                 " da "            delimited size
                 imballi-ed        delimited by low-value
                 " x "             delimited size
                 art-udm-imballo   delimited by low-value
                 " Peso: "         delimited by size
                 peso-ed           delimited by low-value
                 x"0D0A"           delimited by size
                 x"0D0A"           delimited by size
                 into stringa-art
           end-string.

      ***---
       FIND-PROGMAG.
           initialize como-prg-chiave
           move -1              to giacenza-max
           move como-articolo   to prg-cod-articolo
           move low-value       to prg-cod-magazzino
                                   prg-tipo-imballo
                                   prg-peso
           start progmag key not < prg-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read progmag next no lock
                       at end
                          exit perform
                    end-read
                    if como-articolo not = prg-cod-articolo
                       exit perform
                    end-if

                    if prg-cod-magazzino not = space

                       if prg-giacenza < zero
                          multiply prg-giacenza by -1 
                                               giving como-giacenza
                       else
                          move prg-giacenza to como-giacenza
                       end-if
                       if como-giacenza > giacenza-max
                          move como-giacenza   to giacenza-max
                          move prg-chiave      to como-prg-chiave
                       end-if
                    end-if
                 end-perform
           end-start.

           move como-prg-chiave to prg-chiave
           read progmag
              invalid
                 continue
           end-read.
              
      ***---
       ARTICOLI-CATENA.
           move zero   to art-1
                          art-2
           move link-articolo   to cat-codice
           move low-value       to cat-princ
           start catart key not < cat-chiave
              invalid
                 continue
              not invalid
                 read catart next no lock
                    at end
                       continue
                    not at end
                       if link-articolo = cat-codice
                          if cat-princ not = zero
                             move cat-princ to cat-codice
                             move zero      to cat-princ
                             read catart
                                invalid
                                   continue
                             end-read
                          end-if
                          if cat-num-el-catena = 1
                             move cat-codice      to art-1
                          else
                             move cat-collegato(cat-num-el-catena - 1)  
                                                  to art-1
                          end-if
                          move cat-collegato(cat-num-el-catena)  
                                                  to art-2
                       end-if
                 end-read
           end-start.

      ***---
       CLOSE-FILES.
           close articoli 
                 clienti
                 progmag
                 timbalqta
                 timballi
                 catart.

      ***---
       EXIT-PGM.
           goback.

      ***---
           copy "mail.cpy".

