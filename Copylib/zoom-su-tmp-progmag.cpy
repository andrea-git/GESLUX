      ***---
       ZOOM-SU-PROGMAG-ARTICOLO.
           set filtro-articoli to true.
           perform COMPONI-TMP.
           if trovato
              perform POSITION-ON-MAJOR-GIACENZA
              move path-tmp-progmag-zoom to ext-file
              move "tmp-progmag-zoom"    to como-file
              call "zoom-gt"          using como-file, tmp-prg-z-rec
                                     giving stato-zoom
              end-call
              cancel "zoom-gt"
           else
              move 1 to stato-zoom
           end-if.
           delete file tmp-progmag-zoom.
           if stato-zoom = 0
              move tmp-prg-z-chiave to prg-chiave HiddenKey
              read progmag no lock invalid continue end-read
              move prg-peso-utf     to hid-peso-utf
              move prg-peso-non-utf to hid-peso-non-utf
              move prg-cod-articolo to ef-art-buf
              display ef-art
      *****        set CheckAfterZoom to true
              perform CANCELLA-COLORE
              perform CONTROLLO
      *****        set CheckAfterZoom to false
           else
              set errori to true
              move 4     to accept-control
           end-if.

      ***---
       ZOOM-SU-PROGMAG.           
           move 0 to num-articoli.
           inquire ef-art, value in art-codice.
           read articoli no lock
                invalid move spaces to art-descrizione
           end-read.
           move "articoli"    to como-file.
           call "zoom-gt"  using como-file, art-rec
                          giving stato-zoom
           end-call.
           cancel "zoom-gt".
           move art-codice to ef-art-buf.
           display ef-art.
           if stato-zoom = 0
              set filtro-articoli to true
              perform COMPONI-TMP
              if trovato
                 if num-articoli > 1
                    perform POSITION-ON-MAJOR-GIACENZA
                    move path-tmp-progmag-zoom to ext-file
                    move "tmp-progmag-zoom"    to como-file
                    call "zoom-gt"          using como-file, 
                                                  tmp-prg-z-rec
                                           giving stato-zoom
                    end-call
                    cancel "zoom-gt"
                    if stato-zoom = 0 
                       move tmp-prg-z-chiave to prg-chiave HiddenKey
                       read progmag no lock invalid continue end-read
                       move prg-cod-articolo to ef-art-buf
                       display ef-art
      *****                 set CheckAfterZoom to true
                       perform CANCELLA-COLORE
                       perform CONTROLLO
      *****                 set CheckAfterZoom to false
                       if tutto-ok
      *                    move 78-ID-ef-qta  to control-id
                          move 4             to accept-control
                       end-if
                    end-if
                 else
                    perform UN-SOLO-ARTICOLO-SU-PROGMAG
                 end-if
              else
                 display message "Articolo NON valido"
                           title tit-err
                            icon 2
                 set errori to true
                 move 1 to stato-zoom
              end-if
              delete file tmp-progmag-zoom
           end-if.

      ***---
       POSITION-ON-FIRST-RECORD.
           |Mi posiziono sul PRIMO record
           open input tmp-progmag-zoom.
           move low-value to tmp-prg-z-rec.
           start tmp-progmag-zoom key is >= key-des
                 invalid continue
           end-start.
           read tmp-progmag-zoom next end-read.
           close tmp-progmag-zoom.

      ***---
       UN-SOLO-ARTICOLO-SU-PROGMAG.
           |Se c'è un solo rec. su progmag con quel codice articolo
           |simulo la scelta di progmag da zoom con il rec. in linea
           move GiacenzaKey   to prg-chiave HiddenKey.
           read progmag no  lock invalid continue end-read.
           move prg-cod-articolo to ef-art-buf.
           display ef-art.
      *****     set CheckAfterZoom to true.
           move 78-ID-ef-art  to control-id.
           perform CANCELLA-COLORE.
           perform CONTROLLO.
      *****     set CheckAfterZoom to false.

      ***---
       COMPONI-TMP.
           move 0 to num-articoli.
           call "W$MOUSE" using set-mouse-shape, wait-pointer.
           set record-ok to true.
           inquire ef-art, value in SaveArticolo.
      *     tca-cod-magaz.
           accept  path-tmp-progmag-zoom from environment "PATH_ST".
           accept  como-data             from century-date.
           accept  como-ora              from time.
           inspect path-tmp-progmag-zoom replacing trailing
                                         spaces by low-value.
           string path-tmp-progmag-zoom  delimited by low-value
                  "tmp-progmag-zoom"     delimited by size
                  "_"                    delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-progmag-zoom
           end-string.

           set trovato to false.
           open output tmp-progmag-zoom.
           initialize prg-rec.
           if filtro-articoli
              move SaveArticolo to prg-cod-articolo
           end-if.
           move tca-cod-magaz to prg-cod-magazzino.
           start progmag key  is >= prg-chiave
                 invalid continue 
             not invalid perform CICLO-READ
           end-start.

           close tmp-progmag-zoom.
           call "W$MOUSE" using set-mouse-shape, arrow-pointer.

      ***---
       CICLO-READ.
           perform until 1 = 2
              read progmag next no lock at end exit perform end-read
              if filtro-articoli
                 if prg-cod-articolo not = SaveArticolo
                    exit perform
                 end-if
              end-if
              set record-ok to false
              if prg-cod-magazzino not = spaces and
                 prg-tipo-imballo  not = spaces and
                 prg-peso          not = 0      and
                 prg-cod-magazzino     = tca-cod-magaz
                 move prg-cod-articolo  to art-codice
                 read articoli no lock 
                      invalid continue 
                  not invalid set record-ok to true
                 end-read

                 if record-ok

                    move prg-tipo-imballo to imb-codice
                                             imq-codice
                    read timballi  no lock invalid continue end-read
                    read timbalqta no lock invalid continue end-read

                    move imq-qta-imb      to hid-imballi imballi-ed
                    move imq-tipo         to imb-codice
                    read timballi no lock
                         invalid  initialize imb-descrizione
                    end-read
                    inspect imb-descrizione replacing trailing spaces
                                                         by low-value
                    move imq-qta-imb    to imballi-ed
                    call "C$JUSTIFY" using imballi-ed, "L"
                    initialize imballo-descrizione
                    string  imb-descrizione delimited by low-value
                            " da "          delimited by size
                            imballi-ed      delimited by spaces
                            " x "           delimited by size
                            art-udm-imballo delimited by size
                            into imballo-descrizione
                    end-string

                    move art-codice          to tmp-prg-z-cod-articolo
                    move art-descrizione     to tmp-prg-z-art-des
                    move tca-cod-magaz       to tmp-prg-z-cod-magazzino
                    move StoreDesMagazzino   to tmp-prg-z-mag-des
                    move prg-tipo-imballo    to tmp-prg-z-tipo-imballo
                    move imballo-descrizione to tmp-prg-z-imb-des
                    move prg-peso            to tmp-prg-z-peso
                    move prg-giacenza        to tmp-prg-z-giacenza
                    move prg-impegnato       to tmp-prg-z-impegnato
                    move prg-ordinato-1      to tmp-prg-z-ordinato

                    write tmp-prg-z-rec invalid continue end-write
                    set trovato to true
                    add 1 to num-articoli
                    if num-articoli = 1
                       move prg-chiave to GiacenzaKey
                    end-if
                 end-if

              end-if

           end-perform.

      ***---
       POSITION-ON-MAJOR-GIACENZA.
           open input tmp-progmag-zoom.
           move low-value to tmp-prg-z-rec.
           inquire ef-art,   value in tmp-prg-z-cod-articolo
           move tmp-prg-z-cod-articolo  to como-articolo.
           move tca-cod-magaz           to tmp-prg-z-cod-magazzino 
                                           como-magazzino.
           move 0      to como-giacenza.
           move spaces to como-record.
           start tmp-progmag-zoom key is >=  key-des
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-progmag-zoom next at end exit perform 
                    end-read
                    if tmp-prg-z-cod-articolo  not = como-articolo or
                       tmp-prg-z-cod-magazzino not = como-magazzino
                       exit perform
                    end-if
                    if tmp-prg-z-giacenza > como-giacenza or
                                            como-giacenza = 0
                       move tmp-prg-z-giacenza to como-giacenza
                       move tmp-prg-z-rec      to como-record
                    end-if
                 end-perform
           end-start.
           if como-record not = spaces
              move como-record to tmp-prg-z-rec
           end-if.
           close tmp-progmag-zoom.
