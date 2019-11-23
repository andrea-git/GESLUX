      ***---
       RIEMPI-TMP-DISTINTEB.
           set tutto-ok to true.
           initialize path-zoom-distinteb.
           accept  path-zoom-distinteb from environment "PATH_ST".
           accept  como-data from century-date.
           accept  como-ora  from time.
           inspect path-zoom-distinteb replacing trailing 
                                       spaces by low-values.
           string  path-zoom-distinteb delimited low-value
                   "zoom-distinteb_"   delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-zoom-distinteb
           end-string.

           open output zoom-distinteb.
           move low-value      to dis-rec.
           move como-articolo  to dis-articolo-finale.
           start distinteb key is >= k-articolo
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read distinteb next no lock at end exit perform 
                 end-read
                 if como-articolo not = dis-articolo-finale and
                    como-articolo not = 0
                    exit perform
                 end-if
                 initialize zoom-dis-rec replacing numeric data by zeros
                                              alphanumeric data by space
                 move dis-codice            to zoom-dis-codice
                 move dis-articolo-finale   to zoom-dis-articolo
                                               art-codice
                 read articoli no lock 
                      invalid move spaces to art-rec
                 end-read
                 move art-descrizione       to zoom-dis-art-descrizione
                 move dis-magazzino-finale  to zoom-dis-magazzino
                                               mag-codice
                 read tmagaz no lock 
                      invalid move spaces to mag-descrizione 
                 end-read
                 move mag-descrizione       to zoom-dis-mag-descrizione

                 move dis-imballo-finale    to zoom-dis-imballo 
                                               imq-codice
                 read timbalqta no lock
                      invalid continue
                  not invalid
                      move imq-tipo to imb-codice
                      read timballi no lock
                           invalid  continue
                       not invalid
                           move imq-qta-imb        to imballi-ed
                           call "C$JUSTIFY" using imballi-ed, "L"
                           inspect imb-descrizione replacing trailing
                                                   spaces by low-value
                           string  imb-descrizione delimited low-value
                                   " da "          delimited size
                                   imballi-ed      delimited spaces
                                   " x "           delimited size
                                   art-udm-imballo delimited size
                              into zoom-imb-descrizione
                          end-string
                      end-read
                 end-read
                 move dis-peso-finale       to zoom-dis-peso
                 move dis-peso-utf          to zoom-dis-peso-utf
                 move dis-prezzo            to zoom-dis-prezzo
                 write zoom-dis-rec invalid continue end-write
              end-perform
           end-if.
           close zoom-distinteb.
