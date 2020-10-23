      ***---
       STAMPA-TESTA-GRAF.
           if PrimaVolta
              set PrimaVolta to false
              perform APRI-STAMPA-GRAF
           else
              if calling-program = "stdoccsv" 
                 if stampa--segue
                    call "W$BITMAP" using wbitmap-destroy, 
                                          BitmapSfondoHandle
                 else
                    perform SALTO-PAGINA-GRAF
                 end-if
              end-if
           end-if
                                
           move tor-cod-cli to cli-codice.
           set cli-tipo-C   to true.
           read clienti     no lock invalid continue end-read.

           move cli-tipo to tcl-codice.
           read ttipocli.

           perform STAMPA-BITMAP.

           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move 1               to spl-colonna
           move 4,0             to spl-riga
           move 1               to spl-tipo-colonna

           move courier-10      to spl-hfont

           move tor-cod-cli     to des-codice.
           move tor-prg-destino to des-prog.
           read destini no lock invalid continue end-read.

           move cli-ragsoc-1   to cli-riga-titolo-1.
           if des-ragsoc-1 not = space
              move des-ragsoc-1   to des-riga-titolo-1
           else
              move cli-ragsoc-1   to des-riga-titolo-1
           end-if
           initialize line-riga.
           move st-riga-titolo-1 to line-riga.
           perform STAMPA-RIGA-GRAF.

           move cli-indirizzo  to cli-riga-titolo-2.
           if des-indirizzo not = space
              move des-indirizzo  to des-riga-titolo-2
           else
              move cli-indirizzo  to des-riga-titolo-2
           end-if
           initialize line-riga.
           move st-riga-titolo-2 to line-riga.
           perform STAMPA-RIGA-GRAF.

           move cli-cap      to cli-st-cap.
           move cli-localita to cli-st-localita.
           if des-localita not = space
              move des-localita to des-st-localita
           else
              move cli-localita to des-st-localita
           end-if
           initialize line-riga.
           move st-riga-titolo-3 to line-riga.
           perform STAMPA-RIGA-GRAF.
                  
           if des-cap not = spaces
              move des-cap to des-st-cap
           else
              move cli-cap to des-st-cap
           end-if.
           if des-prov not = space
              move des-prov     to des-st-prov
           else
              move cli-prov     to des-st-prov
           end-if.
           initialize line-riga.
           move st-riga-titolo-4 to line-riga.

           move 2               to spl-tipo-colonna

           perform STAMPA-RIGA-GRAF.

           add 0,3  to spl-riga

           move tor-vettore      to vet-codice.
           read tvettori 
                invalid move spaces               to st-vettore
                                                     st-vettore-estesa-1
                                                     st-vettore-estesa-2
            not invalid
                move vet-descrizione to st-vet-ragsoc

                string "P.IVA: "        delimited size
                       vet-piva         delimited size
                       " - ISCR.ALBO: " delimited size
                       vet-n-albo       delimited size
                       into st-vet-piva-albo
                end-string
                move vet-sigla            to st-vettore
                move vet-indirizzo(1:40)  to st-vet-ind
                move vet-indirizzo(41:40) to st-vet-ind2
           end-read.

           move tor-numero     to st-numord.
           call "C$JUSTIFY" using st-numord, "R".

           initialize line-riga.
           move st-vettore               to st-vettore-gv
           move st-numord                to st-numord-gv
           move st-riga-vettore-gordcvar to line-riga

           move courier-g to spl-hfont
           perform STAMPA-RIGA-GRAF.

           move courier to spl-hfont

           initialize line-riga.

           set stampa--segue to false.

           move tor-num-bolla  to st-num-bolla.
           call "C$JUSTIFY" using st-num-bolla, "R".

           if stbolle-stampa
              move stbolle-data to tor-data-bolla
              accept tor-data-bolla-effettiva from century-date
           end-if.
           
           move all "/"             to st-data-bolla.
           move tor-data-bolla(1:4) to st-data-bolla(7:4).
           move tor-data-bolla(5:2) to st-data-bolla(4:2).
           move tor-data-bolla(7:2) to st-data-bolla(1:2).
           initialize line-riga.
           move st-num-data to line-riga.

           move 3   to spl-tipo-colonna
           add 0,9  to spl-riga
           perform STAMPA-RIGA-GRAF.
           
           move tor-cod-cli     to st-cod-cli .
           call "C$JUSTIFY"  using st-cod-cli, "R".

           move tor-num-ord-cli to st-num-ord-cli.
           call "C$JUSTIFY"  using st-num-ord-cli, "R".

           if tor-data-ordine not = 0
              move all "/"              to st-data-ordine
              move tor-data-ordine(3:2) to st-data-ordine(7:2)
              move tor-data-ordine(5:2) to st-data-ordine(4:2)
              move tor-data-ordine(7:2) to st-data-ordine(1:2)
           end-if.

           add 0,4  to spl-riga
           move tca-caus-trasporto  to st-causale.
           move 4   to spl-tipo-colonna

           initialize st-collegate.
      *     if stbolle-stampa
      *        if CreatoSplit
      *           perform NUMERI-BOLLA-COLLEGATI-GRAF
      *        else
      *           move spaces to line-riga
      *           perform STAMPA-RIGA-GRAF
      *           initialize st-collegate
      *           string "N.REG.PILE " delimited by size
      *                  tge-reg-PILE  delimited by size
      *                  into st-collegate
      *        end-if
      *     else
      *        move tor-ordine-testa to rlt-chiave
      *        read reltor no lock 
      *             invalid
      *             move spaces to line-riga
      *             perform STAMPA-RIGA-GRAF
      *             initialize st-collegate
      *             string "N.REG.PILE " delimited by size
      *                    tge-reg-PILE  delimited by size
      *                    into st-collegate
      *             end-string
      *         not invalid
      *             perform varying tot-coll from 1 by 1 
      *                       until tot-coll > 15 
      *                if rlt-numero-s(tot-coll) = 0
      *                   exit perform
      *                end-if
      *             end-perform
      *             perform NUMERI-BOLLA-COLLEGATI-GRAF
      *        end-read
      *     end-if.

           initialize line-riga.
           move st-testa to line-riga.
           perform STAMPA-RIGA-GRAF.

      ***---
       NUMERI-BOLLA-COLLEGATI-GRAF.    
           move st-x to line-riga
           perform STAMPA-RIGA-GRAF

           add rlt-num-bolla to tot-coll 
                    giving como-bolla.
           subtract 1 from como-bolla.

           move rlt-num-bolla to num-bolla-da.
           move como-bolla    to num-bolla-a.
           inspect num-bolla-da replacing leading x"30" by x"20".
           inspect num-bolla-a  replacing leading x"30" by x"20".
           call "C$JUSTIFY" using num-bolla-da, "L".
           call "C$JUSTIFY" using num-bolla-a,  "L".
           inspect num-bolla-da replacing leading x"20" by low-value.
           inspect num-bolla-a  replacing leading x"20" by low-value.
           string "Da n. "     delimited size
                  num-bolla-da delimited low-value
                  "a n. "      delimited size
                  num-bolla-a  delimited low-value
                  into st-collegate
           end-string.


      ***---
       SALTO-PAGINA-GRAF.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
           set spl-stringa to true
           move 0      to spl-riga
           move 0      to spl-tipo-colonna
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       SETTA-FONT.
           initialize wfont-data.
           move 8                   to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 10                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier-10, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 7                   to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier-7, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 6                   to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier-7g, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           initialize wfont-data.
           move 12                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               courier-g, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.


      ***---
       APRI-STAMPA-GRAF.
           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      
              set spl-vertical        to true
              set spl-apertura        to true
              move 1                  to spl-margine-sinistro
              move 1                  to spl-margine-destro
              move 1                  to spl-margine-inf
              if calling-program = "stdoccsv"
                 move "Stampa Elenco bolle da csv"  to spl-nome-job
              else
                 move "Stampa Bolle"                to spl-nome-job
              end-if
              call "spooler"        using spooler-link
           else
              set spl-sta-annu to true
           end-if.
      
           if spl-sta-annu 
              cancel "spooler"
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move courier      to spl-hfont
              call "spooler" using spooler-link
           end-if.
      *
      ***---
       CHIUDI-STAMPA-GRAF.
           set spl-chiusura to true.
           call "spooler" using spooler-link.
           cancel "spooler".
           call "W$BITMAP" using wbitmap-destroy, BitmapSfondoHandle.
           perform DISTRUGGI-FONT.

      ***---
       STAMPA-RIGA-GRAF.
           set spl-stringa   to true.
           add 0,3           to spl-riga
           move line-riga    to spl-riga-stampa
           call "spooler" using spooler-link.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       STAMPA-CORPO-GRAF.
           add 0,6  to spl-riga
           move tor-anno   to ror-anno of rordini.
           move tor-numero to ror-num-ordine of rordini.
           move low-value  to ror-num-riga of rordini
                              ror-chiave-ordine OF rordini

           start rordini key is >= ror-k-stbolle of rordini
                 invalid set errori to true
           end-start.

           initialize old-ror-chiave-ordine-testa
           move 0 to tot-colli
                     tot-peso-utf
                     tot-peso-non-utf
                     tot-peso-tot
                     WrittenRows.
                                  
           perform until 1 = 2

              read rordini next  at end exit perform end-read
              if ror-anno       of rordini not = tor-anno    or 
                 ror-num-ordine of rordini not = tor-numero
                 exit perform
              end-if

              |STATO ORDINE XXX
              if stbolle-stampa
                 if ror-anno-master   of rordini not = 0 and
                    ror-numero-master of rordini not = 0
                    set trovato-master to false
                    set idx-master to 1
                    search el-ordine-m
                    when el-ordine-m(idx-master) = 
                         ror-chiave-ordine-testa of rordini
                         set trovato-master to true
                    end-search
                    if not trovato-master
                       add 1 to tot-master
                       move ror-chiave-ordine-testa of rordini
                         to el-ordine-m(tot-master)
                    end-if
                 end-if
              end-if
              |XXX

              set stampa-corpo  to true

              perform SALTO-PAGINA-MODULO-CONTINUO-GRAF

              set stampa-corpo  to false

              if ror-chiave-ordine-testa of rordini not = 
             old-ror-chiave-ordine-testa
                 if ror-numero-master of rordini not = 0
                    perform DIVISORIO-MASTER-GRAF
                 end-if
              end-if

              initialize st-dati replacing numeric data by zeroes 
                                      alphanumeric data by spaces
              move ror-cod-articolo  of rordini to st-cod-art art-codice
              read articoli no lock invalid continue end-read
              if art-cod-doganale = 0
                 move "*" to st-cod-dog
              else
                 move art-cod-doganale to nom-codice
                 read tnomen no lock
                      invalid move all "*" to st-cod-dog
                  not invalid
                      if nom-si-olio
                         if art-cod-prodener not = spaces
                            move art-cod-prodener to pen-codice
                            read prodener  no lock 
                                 invalid move art-cod-doganale(1:4) 
                                           to st-cod-dog
                             not invalid move pen-nc to st-cod-dog
                            end-read
                         else
                            move art-cod-doganale(1:4) to st-cod-dog
                         end-if        
                      else                 
                         if nom-si-cobat
                            accept st-cod-dog 
                            from environment "CODICE_SICUREZZA_BATTERIE"
                         else
                            move all "*" to st-cod-dog
                         end-if
                      end-if
                 end-read
              end-if        
           
              set trovato-art to false
              move 0 to idx
              perform 20 times
                 add 1 to idx
                 if art-codice = art-no-colli(idx)
                    set trovato-art to true
                    exit perform
                 end-if
              end-perform

              if trovato-art
                 add  0 to tot-colli
                 move 0 to st-colli
              else
                 add  ror-num-colli of rordini to tot-colli
                 move ror-num-colli of rordini to st-colli
              end-if

              perform DESCRIZIONE-IMBALLO
              move art-descrizione      to st-des-art
              if ror-si-omaggio of rordini
                 move "OMAGGIO"  to st-codart-cli
              else
      *****           perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
      *****           move asc-cod-articolo-per-cliente to st-codart-cli
                 move cli-tipo to tcl-codice
                 read ttipocli no lock invalid continue end-read
                 if tcl-gdo-si or tcl-gdo-opz
      *****           if cli-gdo not = spaces
                    perform TROVA-CODICE-ARTICOLO-ON-LISTINI
                 end-if
              end-if
              move ror-peso-utf of rordini to st-peso-utf
              compute tot-peso-utf =
                      tot-peso-utf + 
                    ( ror-peso-utf of rordini * ror-qta of rordini )
              compute tot-peso-non-utf =
                      tot-peso-non-utf + 
                    ( ror-peso-non-utf of rordini * ror-qta of rordini )
              compute tot-peso-tot =
                      tot-peso-tot +
                   (( ror-peso-utf of rordini +
                      ror-peso-non-utf of rordini ) *
                      ror-qta of rordini )
              move art-unita-di-misura  to st-udm
              move ror-qta of rordini   to ror-qta-z st-qta

              evaluate ror-qta-z(1:1)
              when 1 move "A" to st-dec-qta(1:1)
              when 2 move "E" to st-dec-qta(1:1)
              when 3 move "G" to st-dec-qta(1:1)
              when 4 move "H" to st-dec-qta(1:1)
              when 5 move "M" to st-dec-qta(1:1)
              when 6 move "P" to st-dec-qta(1:1)
              when 7 move "S" to st-dec-qta(1:1)
              when 8 move "T" to st-dec-qta(1:1)
              when 9 move "K" to st-dec-qta(1:1)
              end-evaluate

              evaluate ror-qta-z(2:1)
              when 0 move "Z" to st-dec-qta(2:1)
              when 1 move "A" to st-dec-qta(2:1)              
              when 2 move "E" to st-dec-qta(2:1)
              when 3 move "G" to st-dec-qta(2:1)
              when 4 move "H" to st-dec-qta(2:1)
              when 5 move "M" to st-dec-qta(2:1)
              when 6 move "P" to st-dec-qta(2:1)
              when 7 move "S" to st-dec-qta(2:1)
              when 8 move "T" to st-dec-qta(2:1)
              when 9 move "K" to st-dec-qta(2:1)
              end-evaLuate

              evaluate ror-qta-z(3:1)
              when 0 move "Z" to st-dec-qta(3:1)
              when 1 move "A" to st-dec-qta(3:1)               
              when 2 move "E" to st-dec-qta(3:1)
              when 3 move "G" to st-dec-qta(3:1)
              when 4 move "H" to st-dec-qta(3:1)
              when 5 move "M" to st-dec-qta(3:1)
              when 6 move "P" to st-dec-qta(3:1)
              when 7 move "S" to st-dec-qta(3:1)
              when 8 move "T" to st-dec-qta(3:1)
              when 9 move "K" to st-dec-qta(3:1)
              end-evaluate

              evaluate ror-qta-z(3:1)
              when 0 move "Z" to st-dec-qta(4:1)
              when 1 move "A" to st-dec-qta(4:1)               
              when 2 move "E" to st-dec-qta(4:1)
              when 3 move "G" to st-dec-qta(4:1)
              when 4 move "H" to st-dec-qta(4:1)
              when 5 move "M" to st-dec-qta(4:1)
              when 6 move "P" to st-dec-qta(4:1)
              when 7 move "S" to st-dec-qta(4:1)
              when 8 move "T" to st-dec-qta(4:1)
              when 9 move "K" to st-dec-qta(4:1)
              end-evaluate

              evaluate ror-qta-z(5:1)
              when 0 move "Z" to st-dec-qta(5:1)
              when 1 move "A" to st-dec-qta(5:1)               
              when 2 move "E" to st-dec-qta(5:1)
              when 3 move "G" to st-dec-qta(5:1)
              when 4 move "H" to st-dec-qta(5:1)
              when 5 move "M" to st-dec-qta(5:1)
              when 6 move "P" to st-dec-qta(5:1)
              when 7 move "S" to st-dec-qta(5:1)
              when 8 move "T" to st-dec-qta(5:1)
              when 9 move "K" to st-dec-qta(5:1)
              end-evaluate

              evaluate ror-qta-z(6:1)
              when 0 move "Z" to st-dec-qta(6:1)
              when 1 move "A" to st-dec-qta(6:1)               
              when 2 move "E" to st-dec-qta(6:1)
              when 3 move "G" to st-dec-qta(6:1)
              when 4 move "H" to st-dec-qta(6:1)
              when 5 move "M" to st-dec-qta(6:1)
              when 6 move "P" to st-dec-qta(6:1)
              when 7 move "S" to st-dec-qta(6:1)
              when 8 move "T" to st-dec-qta(6:1)
              when 9 move "K" to st-dec-qta(6:1)
              end-evaluate

              evaluate ror-qta-z(7:1)
              when 0 move "Z" to st-dec-qta(7:1)
              when 1 move "A" to st-dec-qta(7:1)               
              when 2 move "E" to st-dec-qta(7:1)
              when 3 move "G" to st-dec-qta(7:1)
              when 4 move "H" to st-dec-qta(7:1)
              when 5 move "M" to st-dec-qta(7:1)
              when 6 move "P" to st-dec-qta(7:1)
              when 7 move "S" to st-dec-qta(7:1)
              when 8 move "T" to st-dec-qta(7:1)
              when 9 move "K" to st-dec-qta(7:1)
              end-evaluate

              evaluate ror-qta-z(8:1)
              when 0 move "Z" to st-dec-qta(8:1)
              when 1 move "A" to st-dec-qta(8:1)               
              when 2 move "E" to st-dec-qta(8:1)
              when 3 move "G" to st-dec-qta(8:1)
              when 4 move "H" to st-dec-qta(8:1)
              when 5 move "M" to st-dec-qta(8:1)
              when 6 move "P" to st-dec-qta(8:1)
              when 7 move "S" to st-dec-qta(8:1)
              when 8 move "T" to st-dec-qta(8:1)
              when 9 move "K" to st-dec-qta(8:1)
              end-evaluate

              call "C$JUSTIFY" using st-dec-qta, "R"

              initialize line-riga
              move courier-7 to spl-hfont
              move struttura-stampa to line-riga
              move 5   to spl-tipo-colonna
              perform STAMPA-RIGA-GRAF
              add 1 to WrittenRows
 
OMAGGI        if ror-qta-omaggi of rordini not = 0
OMAGGI           perform SALTO-PAGINA-MODULO-CONTINUO
OMAGGI           move ror-qta-omaggi of rordini to st-qta-oma
OMAGGI           move st-riga-omaggi to line-riga
OMAGGI           perform STAMPA-RIGA-GRAF
OMAGGI           add 1 to WrittenRows
OMAGGI        end-if

              if stbolle-stampa 
                 perform AGGIORNA-PROGMAG 
              end-if
           end-perform.

      ***---
       DIVISORIO-MASTER-GRAF.
           if (WrittenRows + 2) >= RowsPerPage
              perform SALTO-PAGINA-GRAF
           end-if

           add 0,3  to spl-riga
           add 1 to WrittenRows.

           move ror-anno-master   of rordini to st-om-anno.
           move ror-numero-master of rordini to st-om-numero.
           call "C$JUSTIFY" using st-om-numero "L".

           move ror-anno-master   of rordini to mto-anno.
           move ror-numero-master of rordini to mto-numero.

           read mtordini no lock invalid continue end-read.

           move mto-num-ord-cli to st-om-numordcli.

           string mto-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(1:4) delimited size
                  into st-om-datacli
           end-string.

           initialize line-riga
           move st-ordine-master   to line-riga
           move 7   to spl-tipo-colonna
           move courier   to spl-hfont
           perform STAMPA-RIGA-GRAF
           add 1 to WrittenRows.
           move ror-chiave-ordine-testa of rordini 
                                   to old-ror-chiave-ordine-testa.

      ***---
       STAMPA-NOTE-TOTALI-GRAF.
           subtract WrittenRows from RowsPerPage giving n-vuote.
           compute spl-riga = spl-riga + (n-vuote * 0,3)
           move zero   to spl-tipo-colonna
              
           move 0 to st-tot-peso-utf st-tot-peso-non-utf st-tot-peso-tot

           initialize line-riga.
           move PagePerBolla to PagePerBollaEdit.
           string "*Tot. Pag. "    delimited size
                  PagePerBollaEdit delimited size
                  " - FINE*"       delimited size
                  into line-riga
           end-string.

           move 55 to limite
           move line-riga(1:21) to line-riga(limite:21)
           move  all"-" to line-riga(1:limite - 1)
           move 6   to spl-tipo-colonna
           perform STAMPA-RIGA-GRAF.

           if esiste-note
              move  tor-note1           to st-note-1
              if tor-data-note1 not = 0
                 move  tor-data-note1(1:4) to st-note-data(7:4)
                 move  "/"                 to st-note-data(6:1)
                 move  tor-data-note1(5:2) to st-note-data(4:2)
                 move  "/"                 to st-note-data(3:1)
                 move  tor-data-note1(7:2) to st-note-data(1:2)
              else
                 move spaces to st-note-data
              end-if

              initialize line-riga
              move st-riga-piede-1 to line-riga
              perform STAMPA-RIGA-GRAF
           end-if.             

           move  tot-colli to st-tot-colli.
           move  tor-note2 to st-note-2.
           initialize line-riga.
           move st-riga-piede-2 to line-riga.
           perform STAMPA-RIGA-GRAF.

           if esiste-note
              move  tor-note3 to st-note-3
              initialize line-riga
              move st-riga-piede-3 to line-riga
              perform STAMPA-RIGA-GRAF

              move  tor-note4 to st-note-4
              initialize line-riga
              move st-riga-piede-4 to line-riga
              perform STAMPA-RIGA-GRAF

              move  tor-note-bolla-1 to st-note-5
              initialize line-riga
              move st-riga-piede-5 to line-riga
              perform STAMPA-RIGA-GRAF
      
              move  tor-note-bolla-2 to st-note-6
              initialize line-riga
              move st-riga-piede-6 to line-riga
              perform STAMPA-RIGA-GRAF

           end-if.

           move spaces to dicitura-vett.
           if tor-vettore not = 0
              set riga-vettore to true
           else
              set riga-vettore to false
           end-if.

      *     move zero to spl-tipo-colonna
      *     initialize line-riga.
      *     move flag-vettore to line-riga.
      *     move 19,25   to spl-riga
      *     move 7,1    to spl-colonna
      *     perform STAMPA-RIGA-GRAF.

           move 6   to spl-tipo-colonna
           move 22,00   to spl-riga
           move  tot-peso-utf to st-tot-peso-utf.
           initialize line-riga.
           subtract 0,1   from spl-riga
           move st-riga-utf to line-riga
           perform STAMPA-RIGA-GRAF.

           add 0,2  to spl-riga
           move  tot-peso-non-utf to st-tot-peso-non-utf.
           initialize line-riga.
           move st-riga-non-utf to line-riga
           perform STAMPA-RIGA-GRAF.

           add 0,1  to spl-riga
           move  tot-peso-tot to st-tot-peso-tot.
           initialize line-riga.
              move st-riga-tot to line-riga
           perform STAMPA-RIGA-GRAF.

           subtract 1,1  from spl-riga

           move 7   to spl-tipo-colonna
           initialize line-riga.
           move st-vettore-estesa-1 to line-riga.
           perform STAMPA-RIGA-GRAF.

           initialize line-riga.
           move st-vettore-estesa-1b to line-riga.
           perform STAMPA-RIGA-GRAF.
                                     
           initialize line-riga.    
           move st-vettore-estesa-2 to line-riga.
           perform STAMPA-RIGA-GRAF.
                                     
           initialize line-riga.    
           move st-vettore-estesa-2b to line-riga.
           perform STAMPA-RIGA-GRAF.

      ***---
       SALTO-PAGINA-MODULO-CONTINUO-GRAF.
           if WrittenRows = RowsPerPage
              perform STAMPA-SEGUE-GRAF

              perform SALTO-PAGINA-GRAF
              perform STAMPA-TESTA-GRAF

              move 0 to WrittenRows
              add  1 to PagePerBolla

      *    luciano
              if stampa-corpo
                 if ror-numero-master of rordini not = 0
                    perform DIVISORIO-MASTER-GRAF
                 end-if
              end-if
      *    luciano fine
              move 8,3  to spl-riga
           end-if.

      ***---
       STAMPA-SEGUE-GRAF.
           move 6   to spl-tipo-colonna.
           set stampa--segue to true.
           initialize line-riga.

           move all "-" to line-riga(1:130).
           perform STAMPA-RIGA-GRAF.

           move PagePerBolla  to st-num-page.

           move st-num-page            to st-num-page-gv.
           move st-riga-segue-gordcvar to line-riga.
           perform STAMPA-RIGA-GRAF.

      ***---
       STAMPA-BITMAP.   
           call "W$BITMAP" using WBITMAP-LOAD, tcl-path-sfondo-bolle,
                              giving BitmapSfondoHandle

      *    scrittura fasulla per settare la posizione
           set spl-stringa   to true.
           move courier-7G   to spl-hfont.
           move " "  to spl-riga-stampa.
           move 5,0  to spl-riga.
           move 3,0  to spl-colonna.
           call "spooler" using spooler-link.

           set spl-bitmap to true.
           move 5,0 to spl-colonna.
           move 3,0 to spl-riga.

           move BitmapSfondoHandle to spl-hbitmap.
           move 25,7 to spl-bitmap-height.
           move 18,0 to spl-bitmap-width.

           call "spooler" using spooler-link.


           perform SCRITTE-FINCATURA.

      ***---
       SCRITTE-FINCATURA.
           set spl-stringa   to true.
           move courier-7G   to spl-hfont.
           move "DESTINATARIO"  to spl-riga-stampa.
           move 3,9   to spl-riga.
           move 1,2   to spl-colonna.
           call "spooler" using spooler-link.

           move "DESTINAZIONE MERCE" to spl-riga-stampa.
           move 9,7 to spl-colonna.
           call "spooler" using spooler-link.

           move "N. BOLLA"  to spl-riga-stampa.
           move 6,70 to spl-riga.
           move 12,8 to spl-colonna.
           call "spooler" using spooler-link.
       
           move "DEL"  to spl-riga-stampa.
           move 14,5   to spl-colonna.
           call "spooler" using spooler-link.

           move "COD. CLIENTE"  to spl-riga-stampa
           move 7,39 to spl-riga.
           move 1,2  to spl-colonna.
           call "spooler" using spooler-link.

           move "ORDINE N." to spl-riga-stampa.
           move 3,3         to spl-colonna.

           call "spooler" using spooler-link.

           move "DATA"  to spl-riga-stampa.
           move 6,6     to spl-colonna.
           call "spooler" using spooler-link.

           move "CAUSALE DEL TRASPORTO"  to spl-riga-stampa.
           move 8,0                      to spl-colonna.
           call "spooler" using spooler-link.

           move "CODICE ARTICOLO"  to spl-riga-stampa.
           move 8,20 to spl-riga.
           move 1,05 to spl-colonna.
           call "spooler" using spooler-link.

           move "COLLI"  to spl-riga-stampa.
           move 3,3      to spl-colonna.
           call "spooler" using spooler-link.

           move "ASP. EST. DEI BENI" to spl-riga-stampa.
           move 4,15                 to spl-colonna.
           call "spooler" using spooler-link.

           move "DESCRIZIONE ARTICOLO - BENI (NATURA A QUALITÀ)"  
                                         to spl-riga-stampa
           move 6,55 to spl-colonna.
           call "spooler" using spooler-link.

           move "U.M."  to spl-riga-stampa.
           move 14,26   to spl-colonna.
           call "spooler" using spooler-link.

           move "QUANTITÀ"   to spl-riga-stampa.
           move 16,10        to spl-colonna.
           call "spooler" using spooler-link.

           move 
             "VETTORE: DITTA, RESIDENZA O DOMICILIO (COMUNE - VIA - N.)"
                                                     to spl-riga-stampa
           move 22,00 to spl-riga.
           move 1,1   to spl-colonna.
           call "spooler" using spooler-link.

           move 
              "PRODOTTI SOGGETTI AD IMPOSTA DI CONSUMO KG.:"
                                                     to spl-riga-stampa
           move 22,20 to spl-riga.
           move 9,8   to spl-colonna.
           call "spooler" using spooler-link.

           move "PRODOTTI NON SOGGETTI AD IMPOSTA KG.:"
                                                     to spl-riga-stampa.
           move 22,70 to spl-riga.
           call "spooler" using spooler-link.

           move "TOTALE PESO KG.:" to spl-riga-stampa.

           move 23,20 to spl-riga.
           call "spooler" using spooler-link.

      ***---
       DISTRUGGI-FONT.
           Destroy courier.
           Destroy courier-10.
           Destroy courier-7.
           Destroy courier-7g.
           Destroy courier-g.
