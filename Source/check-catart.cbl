       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-catart.
       AUTHOR.                          Luciano.
      ******************************************************************
       SPECIAL-NAMES. 
           decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

       WORKING-STORAGE SECTION. 
           copy "acucobol.def".
           copy "acugui.def".
           copy "comune.def".

       78  titolo value "Controllo catena articoli sostituzioni".
                                     
       77  status-articoli   pic xx.

       77  como-data            pic 9(8).

       01  filler   pic 9 value 0.
         88 si-msg  value 0.
         88 no-msg  value 1.

       77  CallingPgm  pic x(20).

       77  val-ini  pic 99/99/9999.
       77  val-fine pic 99/99/9999.

      * 01              pic x.
      *     88 tutto-ok value "O".
      *     88 errori   value "E".
           
       LINKAGE SECTION.
           copy "link-check-catart.def".

      ******************************************************************
       PROCEDURE DIVISION using check-catart-linkage.

       DECLARATIVES.
       articoli-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                if si-msg
                   display message "Impossibile procedere."
                     x"0d0a""File [articoli] inesistente"
                             title titolo
                              icon 2
                end-if
                set errori to true
           when "39"
                if si-msg
                   display message "File [articoli] Mismatch size!"
                             title titolo
                              icon 3
                end-if
                set errori to true
           when "98"
                if si-msg
                   display message "[articoli] Indexed file corrupt!"
                             title titolo
                              icon 3
                end-if
                set errori to true
           end-evaluate.
       END DECLARATIVES.


      ***--- 
       MAIN-PRG.
           call "C$CALLEDBY" using CallingPgm.
           if CallingPgm = "imp-articoli"
              set no-msg to true
           end-if.
           perform OPEN-FILES
           perform ELABORAZIONE
           perform CLOSE-FILES
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input articoli.

      ***---
       ELABORAZIONE.
           set chk-ca-errore to false


      * 01  check-articoli-linkage.
      *     05 chk-ca-art        pic 9(6).
      *     05 chk-ca-collegato  pic 9(6).
      *     05 filler            pic 9.
      *        88 chk-ca-errore  value 1 false zero.


      *    verifico per prima che il codice non sia già collegato ad un
      *    altro articolo
      *    (codice presente originariamente in garticoli

           move chk-ca-collegato   to art-collegato 

           start ARTICOLI key not < art-collegato 
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read articoli next no lock
                       at end
                          exit perform
                    end-read
                    if art-collegato not = chk-ca-collegato
                       exit perform
                    end-if

                    if chk-ca-art not = art-codice 
                       set chk-ca-errore to true
                       if si-msg
                          display message "Articolo " chk-ca-collegato
                                          " già collegato all'articolo "
                                          art-codice 
                                  title tit-err
                                  icon MB-WARNING-ICON
                       end-if
                       exit perform
                    end-if
                 end-perform
           end-start.


      *    controllo che l'ariticolo che ho collegato non colleghi me stesso
      *    esempio: articolo 1 ha collegato l'articolo 2, impedisco che 
      *    l'articolo 2 colleghi l'articolo 1
           if not chk-ca-errore
              move chk-ca-collegato   to art-codice
              read articoli
                 invalid
                    continue
              end-read
              if art-collegato  = chk-ca-art
                 set chk-ca-errore to true 
                 if si-msg 
                    display message "Articolo " chk-ca-collegato 
                                    " e articolo " chk-ca-art
                                    " già collegati tra loro "
                              title tit-err
                               icon MB-WARNING-ICON
                 end-if
              end-if
           end-if.

      *    controllo che l'articolo che cerco di collegare non abbia sia
      *    già inserito nella mia catena di articoli
           if not chk-ca-errore
              perform until 1 = 2
                 move art-collegato   to art-codice
                 read articoli
                    invalid
                       exit perform
                 end-read
                 if art-collegato = zero
                    exit perform
                 end-if
                 if art-collegato = chk-ca-art
                    set chk-ca-errore to true
                    if si-msg
                       display message "Articolo " chk-ca-collegato 
                                    " e articolo " chk-ca-art
                                    " già collegati tra loro"
                                 title tit-err
                                  icon MB-WARNING-ICON
                    end-if
                 end-if

              end-perform
           end-if.


      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.


