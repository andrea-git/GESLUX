       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-chiu-master.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl". 
           copy "destini.sl".
           copy "clienti.sl".
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
           COPY "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

       WORKING-STORAGE SECTION.
           copy "comune.def".
           copy "mail.def".

       77  status-mtordini   pic xx.  
       77  status-tsetinvio  pic xx.
       77  status-destini    pic xx.
       77  status-clienti    pic xx.
       77  STATUS-lineseq    PIC  X(2).
       77  STATUS-lineseq1   PIC  X(2).
       77  wstampa           PIC  X(256).


       78  titolo    value "Invio Mail chiusura ordine master".

       77 tentativi        PIC  99.
       77  num-edit       pic z(8).
       77  cont           pic 9(4).
       77  cont2          pic 9(4).
       77  cont3          pic 9(4).
       77  como-linkbody  pic x(3000).
       77  sostituto      pic x(50).
       77  da-sostituire  pic x(2).


       LINKAGE SECTION.
       01  link-mto-chiave.
           05 link-mto-anno         PIC  9(4).
           05 link-mto-numero       PIC  9(8).

      ******************************************************************


       PROCEDURE DIVISION using link-mto-chiave.

       DECLARATIVES.
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
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           open input mtordini.
           open input destini
           open input clienti.

      ***---
       ELABORAZIONE.
           move link-mto-chiave to mto-chiave
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
           if LinkAddress = spaces and LinkAddressCC = space
              exit paragraph
           end-if.
           perform PREPARA-SUBJECT.
           perform PREPARA-CORPO.
           perform PREPARA-FROM.
           move space   to LinkAttach.

           set errori to true.
           move 0 to tentativi.
           move "mail-chiu-master" to NomeProgramma.
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
           if errori
              display message line-riga of lineseq1
                        title titolo
                         icon 2
           end-if.

      ***---
       PREPARA-SUBJECT.
           accept LinkSubject 
                             from environment "CHIU_ORD_MASTER_SUBJECT".

      ***---
       PREPARA-CORPO.
           initialize linkBody.

      *     move mto-numero    to num-edit
      *     call "C$JUSTIFY" using num-edit, "L"
      *     inspect num-edit replacing trailing space by low-value

           accept linkBody from environment "CHIU_ORD_MASTER_BODY".

      *    controllo di dover inserire il numero d'ordine
           move mto-numero    to num-edit.
           call "C$JUSTIFY" using num-edit, "L".
           move "$1"   to da-sostituire.
           move num-edit  to sostituto.
           perform SOSTITUZIONE.


      *     initialize cont
      *     inspect linkBody tallying cont for all "$1"
      *     if cont not = zero
      *        perform cont times
      *           initialize cont2
      *           inspect linkBody tallying cont2 
      *                                   for characters before "$1"
      *           add 3 to cont2 giving cont3
      *           initialize como-linkbody
      *           if cont2 = zero
      *              string num-edit            delimited by low-value
      *                     linkbody(cont3:)    delimited by size
      *                    into como-linkbody
      *           else
      *              string linkbody(1:cont2)   delimited by size
      *                     num-edit            delimited by low-value
      *                     linkbody(cont3:)    delimited by size
      *                     into como-linkbody
      *           end-if
      *           move como-linkbody   to linkbody
      *        end-perform
      *     end-if
      *

      *    controllo di dover inserire l'anno dell'ordine
           move "$2"   to da-sostituire
           move mto-anno  to sostituto
           perform SOSTITUZIONE


      *     initialize cont
      *     inspect linkBody tallying cont for all "$2"
      *     if cont not = zero
      *        perform cont times
      *           initialize cont2
      *           inspect linkBody tallying cont2 
      *                                   for characters before "$2"
      *           add 3 to cont2 giving cont3
      *           initialize como-linkbody
      *           if cont2 = zero
      *              string mto-anno            delimited by size
      *                     linkbody(cont3:)    delimited by size
      *                     into como-linkbody
      *           else
      *              string linkbody(1:cont2)   delimited by size
      *                     mto-anno            delimited by size
      *                     linkbody(cont3:)    delimited by size
      *                     into como-linkbody
      *           end-if
      *           move como-linkbody   to linkbody
      *        end-perform
      *     end-if.

      *    controllo di dover inserire il numero del cliente
           move "$3"   to da-sostituire
           move mto-num-ord-cli to sostituto
           perform SOSTITUZIONE


      *    controllo di dover inserire la data dell'ordine
           move "$4"   to da-sostituire
           initialize sostituto
           move mto-data-ordine(7:2)  to sostituto(1:2)
           move "/"                   to sostituto(3:1)
           move mto-data-ordine(5:2)  to sostituto(4:2)
           move "/"                   to sostituto(6:1)
           move mto-data-ordine(1:4)  to sostituto(7:4)
           perform SOSTITUZIONE.

      ***---
       SOSTITUZIONE.
           inspect sostituto replacing trailing space by low-value
           initialize cont
           inspect linkBody tallying cont for all da-sostituire
           if cont not = zero
              perform cont times
                 initialize cont2
                 inspect linkBody tallying cont2 
                                     for characters before da-sostituire
                 add 3 to cont2 giving cont3
                 initialize como-linkbody
                 if cont2 = zero
                    string sostituto           delimited by low-value
                           linkbody(cont3:)    delimited by size
                           into como-linkbody
                 else
                    string linkbody(1:cont2)   delimited by size
                           sostituto           delimited by low-value
                           linkbody(cont3:)    delimited by size
                           into como-linkbody
                 end-if
                 move como-linkbody   to linkbody
              end-perform
           end-if.

      ***---
       PREPARA-INDIRIZZO.
           if mto-prg-destino = zero
              move mto-cod-cli  to cli-codice
              set cli-tipo-c to true
              read clienti no lock
                 invalid
                    move space     to cli-email
              end-read
              move cli-email       to LinkAddress
           else
              move mto-cod-cli     to des-codice 
              move mto-prg-destino to des-prog  
              read destini no lock
                 invalid
                    move space  to des-mail
              end-read
              move des-mail  to LinkAddress
           end-if.

      ***---
       PREPARA-INDIRIZZO-CC.
           accept LinkAddressCC 
                 from environment "CHIU_ORD_MASTER_ADDRESSES_CC".
           initialize cont.
           inspect LinkAddressCC replacing trailing space by low-value.
           inspect LinkAddressCC 
                       tallying cont for characters before low-value.
           if cont not = zero
              if LinkAddressCC(cont:1) = ";"
                 move low-value to LinkAddressCC(cont:1)
              end-if
           end-if.
           inspect LinkAddressCC replacing trailing low-value by space.

      ***---
       PREPARA-FROM.
           accept LinkAddressFrom 
                       from environment "CHIU_ORD_MASTER_ADDRESSES_FROM"
           if LinkAddressFrom = space
              move "info@lubex.it" to LinkAddressFrom
           end-if.

      ***---
       CLOSE-FILES.
           close mtordini
                 clienti
                 destini.

      ***---
       EXIT-PGM.
           goback.

      ***---
           copy "mail.cpy".
