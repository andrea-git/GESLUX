       PROGRAM-ID. get-mail-mod-bozze.
       AUTHOR.     Luciano.

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "paramget.sl".
           copy "tmp-mod-rordini.sl".
           copy "lineseq.sl".
       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.



       FILE SECTION.           
           copy "tsetinvio.fd".
           copy "paramget.fd".
           copy "tmp-mod-rordini.fd".
       FD  lineseq.
       01 line-riga        PIC  x(32000).
       FD  lineseq1.
       01 line-riga        PIC  x(32000).

       WORKING-STORAGE SECTION.
           COPY "comune.def".
           COPY "mail.DEF".
       77  status-paramget         pic xx.
       77  status-tsetinvio        pic xx.
       77  status-tmp-mod-rordini  pic xx.
       77  status-lineseq          pic xx.
      
      * OTHER DATA             
       77  como-data                pic 9(8)   value zero.
       77  como-ora                 pic 9(8)   value zero.

       77  titolo                   pic x(256).

       77  wstampa                 pic x(256).
       77  path-tmp-mod-rordini    pic x(256).

       77  num-edit       pic z(8).
       77  cont           pic 9(4).
       77  cont2          pic 9(4).
       77  cont3          pic 9(4).
       77  como-linkbody  pic x(3000).
       77  sostituto      pic x(50).
       77  da-sostituire  pic x(2).

      *
       77  mail           pic x(200).
       77  tentativi             pic 99.
       01  riga-art.
           05 filler   pic x(10) value "articolo: ".
           05 ra-art   pic z(6).
           05 filler   pic x value "-".
           05 ra-imb   PIC  X(3).
           05 filler   pic x(12) value " Quantità :".
           05 ra-qta   pic zz.zzz.zz9.



       LINKAGE SECTION.
           copy "link-mail-mod-bozze.def".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING mail-mod-bozze-linkage.

       DECLARATIVES.
       PARAMget-ERR SECTION.
           use after error procedure on PARAMget.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-PARAMget 
           when "39"
                set errori to true
                display message "File [PARAMget] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PARAMget] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [PARAMget] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TMP-MOD-RORDINI-ERR SECTION.
           use after error procedure on TMP-MOD-RORDINI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TMP-MOD-RORDINI 
           when "39"
                set errori to true
                display message "File [TMP-MOD-RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                             "[TMP-MOD-RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a"
                    "File delle testate [TMP-MOD-RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  


       END DECLARATIVES.


      ***---
       MAIN-PARAGRAPH.
           perform INIT.
           if tutto-ok 
              perform OPEN-FILES 
           end-if.

           move space  to get-codice
           read paramget
              invalid
                 continue
           end-read

           perform PREPARA-INDIRIZZO.
           perform PREPARA-INDIRIZZO-CC.

           evaluate true
           when mmb-cancellazione
                perform CANCELLA
           when mmb-modifica
                perform MODIFICA
           end-evaluate.


           perform CLOSE-FILES
           perform EXIT-PGM.

      ***---
       INIT.
           move  "GESLUX - Mail modifica bozze ordini clienti get" 
                                            to titolo.
      
           set tutto-ok            to true.
           accept como-ora from time.
           accept como-data from century-date.
           move mmb-path  to path-tmp-mod-rordini.

      ***---
       OPEN-FILES.
           open input paramget.

      ***---
       CLOSE-FILES.
           close paramget.

      ***---
       CANCELLA.
           perform PREPARA-SUBJECT-CANCELLA
           perform PREPARA-CORPO-CANCELLA.
           move space  to LinkAttach.
           perform INVIO-MAIL.

      ***---
       MODIFICA.
           perform CONTROLLA-MOD

           if tutto-OK
              close LINESEQ
              perform PREPARA-SUBJECT-MODIFICA
              perform PREPARA-CORPO-MODIFICA
              move wstampa   to LinkAttach
              perform INVIO-MAIL
           end-if.

      ***---
       CONTROLLA-MOD.
           set errori  to true
           open input TMP-MOD-RORDINI.

           move low-value to tmp-mror-mod
                             tmp-mror-chiave.
           start tmp-mod-rordini key not < tmp-mror-k-tipo
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read TMP-MOD-RORDINI next
                       at end
                          exit perform
                    end-read

                    evaluate true
                    when tmp-mror-modifica
                         perform SCRIVI-MOD
                    when tmp-mror-inserita
                         perform SCRIVI-INS
                    when tmp-mror-cancellata
                         perform SCRIVI-CANC
                    end-evaluate
                 end-perform
           end-start.

      ***---
       SCRIVI-MOD.
           if tmp-mror-orig not = tmp-mror-mod
              if errori
                 set tutto-ok   to true
                 perform APRI-TXT-MOD
              end-if

              initialize line-riga of lineseq 
              write line-riga of lineseq

              move tmp-mor-articolo-orig to ra-art
              move tmp-mror-imb-orig     to ra-imb
              move tmp-qta-orig          to ra-qta
              move "La riga" to line-riga of lineseq
              write line-riga of lineseq
              move riga-art  to line-riga of lineseq
              write line-riga of lineseq
       
              move tmp-mor-articolo-mod  to ra-art
              move tmp-mror-imb-mod      to ra-imb
              move tmp-qta-mod           to ra-qta  

              move "è stata modificata nel seguente modo"  
                                               to line-riga of lineseq
              write line-riga of lineseq
              move riga-art  to line-riga of lineseq
              write line-riga of lineseq
           end-if.

      ***---
       SCRIVI-INS.
           if errori
              set tutto-ok   to true
              perform APRI-TXT-MOD
           end-if.

           initialize line-riga of lineseq 
           write line-riga of lineseq
           move tmp-mor-articolo-mod  to ra-art
           move tmp-mror-imb-mod      to ra-imb
           move tmp-qta-mod           to ra-qta

           move "Aggiunta la riga"    to line-riga of lineseq
           write line-riga of lineseq.
           move riga-art  to line-riga of lineseq
           write line-riga of lineseq.

      ***---
       SCRIVI-CANC.
           if errori
              set tutto-ok   to true
              perform APRI-TXT-MOD
           end-if.

           initialize line-riga of lineseq 
           write line-riga of lineseq
           move tmp-mor-articolo-orig to ra-art
           move tmp-mror-imb-orig     to ra-imb
           move tmp-qta-orig          to ra-qta

           move "Cancellata la riga"  to line-riga of lineseq.
           write line-riga of lineseq
           move riga-art  to line-riga of lineseq
           write line-riga of lineseq.

      ***---
       APRI-TXT-MOD.
           move mmb-tor-numero  to num-edit
           call "C$JUSTIFY" using num-edit, "L".
           inspect num-edit replacing trailing space by low-value


           accept wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing space by low-value
           string wstampa delimited by low-value
                  "Elenco modifiche ordine "   delimited by size
                  num-edit                     delimited by low-value
                  "-"                          delimited by size
                  mmb-tor-anno                 delimited by size
                  ".txt"                       delimited by size
                  into wstampa
           inspect wstampa replacing trailing space by low-value

           open output lineseq

           initialize line-riga of lineseq
           string "Elenco modifiche apportate all'ordine "
                                               delimited by size
                  num-edit                     delimited by low-value
                  "-"                          delimited by size
                  mmb-tor-anno                 delimited by size
                  into line-riga of lineseq
           write line-riga of lineseq

           initialize line-riga of lineseq
           string "apportate in data  "  delimited by size
                  como-data(7:2)         delimited by size
                  "/"                    delimited by size
                  como-data(5:2)         delimited by size
                  "/"                    delimited by size
                  como-data(1:4)         delimited by size
                  " alle ore "           delimited by size
                  como-ora(1:2)          delimited by size
                  ":"                    delimited by size
                  como-ora(3:2)          delimited by size
                  into line-riga of lineseq
           write line-riga of lineseq.

      ***---
       INVIO-MAIL.
           set errori to true.
           move 0 to tentativi.
           move "GET-mail-mod-bozze" to NomeProgramma.
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
              display message box 
                    "Errore durante l'invio della mail riepologativa"
                       title titolo
                       icon 2
           end-if.

      ***---
       PREPARA-SUBJECT-CANCELLA.
           move mmb-tor-numero           to num-edit
           move get-oggetto-canc-bozze   to linkBody

      *    controllo di dover inserire il numero d'ordine
           call "C$JUSTIFY" using num-edit, "L"
           move "$1"   to da-sostituire
           move num-edit  to sostituto
           perform SOSTITUZIONE.

      *    controllo di dover inserire l'anno dell'ordine
           move "$2"         to da-sostituire
           move mmb-tor-anno to sostituto
           perform SOSTITUZIONE
           move linkBody     to LinkSubject.

      ***---
       PREPARA-SUBJECT-MODIFICA.
           move mmb-tor-numero           to num-edit
           move get-oggetto-mod-bozze    to linkBody

      *    controllo di dover inserire il numero d'ordine
           call "C$JUSTIFY" using num-edit, "L"
           move "$1"   to da-sostituire
           move num-edit  to sostituto
           perform SOSTITUZIONE.

      *    controllo di dover inserire l'anno dell'ordine
           move "$2"         to da-sostituire
           move mmb-tor-anno to sostituto
           perform SOSTITUZIONE
           move linkBody     to LinkSubject.

      ***---
       PREPARA-CORPO-CANCELLA.
           move mmb-tor-numero  to num-edit
           move get-corpo-canc-bozze  to linkBody

      *    controllo di dover inserire il numero d'ordine
           call "C$JUSTIFY" using num-edit, "L"
           move "$1"   to da-sostituire
           move num-edit  to sostituto
           perform SOSTITUZIONE.

      *    controllo di dover inserire l'anno dell'ordine
           move "$2"         to da-sostituire
           move mmb-tor-anno to sostituto
           perform SOSTITUZIONE.

      ***---
       PREPARA-CORPO-MODIFICA.
           move mmb-tor-numero  to num-edit
           move get-corpo-mod-bozze  to linkBody

      *    controllo di dover inserire il numero d'ordine
           call "C$JUSTIFY" using num-edit, "L"
           move "$1"   to da-sostituire
           move num-edit  to sostituto
           perform SOSTITUZIONE.

      *    controllo di dover inserire l'anno dell'ordine
           move "$2"         to da-sostituire
           move mmb-tor-anno to sostituto
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
           move get-mail-mod-bozze to mail
           initialize LinkAddress
           inspect mail replacing trailing space by low-value
           initialize cont 
           inspect mail tallying cont for characters before low-value
           if mail(cont:1) = ";"
              move low-value to mail(cont:1)
           end-if
           inspect mail replacing trailing low-value by space
           move mail   to LinkAddress.

      ***---
       PREPARA-INDIRIZZO-cc.
           move get-mail-mod-bozze to mail
           initialize LinkAddressCC
           inspect mail replacing trailing space by low-value
           initialize cont 
           inspect mail tallying cont for characters before low-value
           if mail(cont:1) = ";"
              move low-value to mail(cont:1)
           end-if
           inspect mail replacing trailing low-value by space
           move mail   to LinkAddressCC.

      ***---
       EXIT-PGM.
           goback.

      **---
       PARAGRAFO-COPY.
           copy "mail.cpy".
