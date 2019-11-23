      ***---
       PIE-DI-PAGINA.
           if font-pie-pagina = 0
      *       Verdana 6I
              initialize wfont-data font-pie-pagina
              move 6 to wfont-size
              move "Verdana"            to wfont-name
              set  wfcharset-dont-care  to true
              set  wfont-bold           to false
              set  wfont-italic         to true
              set  wfont-underline      to false
              set  wfont-strikeout      to false
              set  wfont-fixed-pitch    to false
              move 0                    to wfont-char-set
              set  wfdevice-win-printer to true |E' un carattere per la stampante
              call "W$FONT" using wfont-get-font, font-pie-pagina, 
                                  wfont-data
                           giving WFONT-STATUS

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
              if wfont-status not = 1
                 set errori to true
                 perform MESSAGGIO-ERR-FONT
                 exit paragraph
              end-if 
           end-if.

           if mese = spaces
              evaluate como-data(5:2)
              when "01" move "Gennaio"   to mese
              when "02" move "Febbraio"  to mese
              when "03" move "Marzo"     to mese
              when "04" move "Aprile"    to mese
              when "05" move "Maggio"    to mese
              when "06" move "Giugno"    to mese
              when "07" move "Luglio"    to mese
              when "08" move "Agosto"    to mese
              when "09" move "Settembre" to mese
              when "10" move "Ottobre"   to mese
              when "11" move "Novembre"  to mese
              when "12" move "Dicembre"  to mese
              end-evaluate
              inspect mese replacing trailing spaces by low-value
           end-if.
           if giorno = spaces
              accept giorno from day-of-week
              evaluate giorno
              when "1" move "Lunedì"    to giorno
              when "2" move "Martedì"   to giorno
              when "3" move "Mercoledì" to giorno
              when "4" move "Giovedì"   to giorno
              when "5" move "Venerdì"   to giorno
              when "6" move "Sabato"    to giorno
              when "7" move "Domenica"  to giorno
              end-evaluate
              inspect giorno replacing trailing spaces by low-value
           end-if.

           subtract 1,5 from save-altezza-pagina giving save-riga.

           set  spl-grigio       to true.
           move 10               to spl-pen-with.
           move 2                to spl-colonna.
           move 18,5             to spl-colonna-fine.
           move save-riga        to spl-riga spl-riga-fine.
           set  spl-oggetto      to true.
           set  spl-linea        to true.
           set  spl-pen-solid    to true.
           call "spooler"    using spooler-link.

           subtract 0,2 from save-riga.
           initialize data-stampa.
           string giorno         delimited low-value
                  " "            delimited size
                  como-data(7:2) delimited size
                  " "            delimited size
                  mese           delimited low-value
                  " "            delimited size
                  como-data(1:4) delimited size
                  into data-stampa
           end-string.
           move    tot-pag to tot-pag-x.
           inspect tot-pag-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using tot-pag-x, "L".
           inspect tot-pag-x replacing trailing spaces by low-value.
           move pagina to pagina-x.
           inspect pagina-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using pagina-x, "L".
           inspect pagina-x replacing trailing spaces by low-value.
           
           initialize pagina-di.

           string "Pagina"  delimited size
                  " "       delimited size
                  pagina-x  delimited low-value
                  " di "    delimited size
                  tot-pag-x delimited low-value
                  into pagina-di
           end-string.
           set  spl-nero  to true.
           move font-pie-pagina to spl-hfont.
           move 57        to spl-tipo-colonna.
           move p-riga    to spl-riga-stampa.
           perform SCRIVI.
           move 0         to save-riga.
