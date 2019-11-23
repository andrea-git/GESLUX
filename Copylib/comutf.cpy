      ***---
       INIT.
           set tutto-ok to true.
           set trovato  to false.

      ***---
       OPEN-FILES.
           open i-o tmovtrat.
           open input tmovmag tcaumag tparamge clienti destinif tordini
                      destini rordini.

      ***---
       SCRIVI-TESTATA.
           perform CARICA-FONT.

           move 0 to num-righe.
           move 1 to pagina.
           perform CREA-PDF.
           if settaPDF-OK
              accept selprint-stampante from environment "STAMPANTE_UTF"
           else
              exit paragraph
           end-if.

           initialize spooler-link.
           move "GESLUX - Comunicazione UTF CARICO" to spl-nome-job.

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      *    NUOVA SELEZIONE
              set spl-apertura   to true
              set spl-horizontal to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
           end-if.       
           move Courier8 to spl-hfont. 
                       
           perform SCRIVI-PIEDE.       

           move tge-ditta-comunicazione-utf to r-dest r-int7.

           move 1,5 to spl-riga.
           move intestazione1  to spl-riga-stampa.
           perform SCRIVI.
           add 0,3 to spl-riga.
           move intestazione2  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,3 to spl-riga.
           move intestazione3  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,3 to spl-riga.
           move intestazione4  to spl-riga-stampa.
           perform SCRIVI.     
           add 1,5 to spl-riga.
           move intestazione5  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,3 to spl-riga.
           move intestazione6  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,3 to spl-riga.
           move intestazione7  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,9 to spl-riga.
           move intestazione8a  to spl-riga-stampa.
           perform SCRIVI.    
           add 0,3 to spl-riga.
           move intestazione8b  to spl-riga-stampa.
           perform SCRIVI.    
           add 0,9 to spl-riga.
           move intestazione8c  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,3 to spl-riga.
           move intestazione8d  to spl-riga-stampa.
           perform SCRIVI.     
           add 0,9 to spl-riga.                 
           move intestazione9  to spl-riga-stampa.
           perform SCRIVI.   
           add 0,5 to spl-riga.

      ***---
       SCRIVI-RIGA-COMUNE.
           if selprint-stampante = spaces exit paragraph end-if.
           if pagina = 1
              move 36 to max-righe 
           else
              move 60 to max-righe
           end-if.

           if num-righe >= max-righe
              move 0 to num-righe
              add  1 to pagina
              set spl-salto-pagina to true
              call "spooler" using spooler-link
              perform SCRIVI-PIEDE
              move 1,1 to spl-riga
           end-if.
                                         
           move r-riga to spl-riga-stampa.
           perform SCRIVI.
           add 0,3 to spl-riga.
           add 1 to num-righe.

      ***---
       SCRIVI.
           set spl-stringa to true.
           call "spooler"  using spooler-link.
           initialize spl-riga-stampa.    

      ***---
       SCRIVI-PIEDE.
           if selprint-stampante = spaces exit paragraph end-if.
           move 19,6   to spl-riga.
           move pagina to r-pag.
           move pie-di-pagina to spl-riga-stampa.
           perform SCRIVI.    

      ***--
       CLOSE-FILES.
           close  tmovmag
                  tparamge
                  tcaumag
                  clienti
                  destinif
                  destini
                  tordini
                  rordini.

      ***---
       CREA-PDF.
           accept DestFile from environment "COMUTF_PATH".
      ******    tolgo l'eventuale barra finale
      *****     inspect DestFile replacing trailing spaces by low-value.
      *****     initialize cont.
      *****     inspect DestFile tallying cont
      *****             for characters before low-value.
      *****     if DestFile(cont:1) = "\" 
      *****        move low-value  to DestFile(cont:1)
      *****     end-if.
      *****     inspect DestFile replacing trailing low-value by spaces.

           accept como-data from century-date.
           accept como-ora  from time.                  
           string  "COMUNICAZIONE_AGENZIA_DELLE_DOGANE" delimited size
                   78-tipo-doc-parte2                   delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size   
                  into NomeFile
           end-string.

           set settaPDF-setta to true.

           move NomeFile  to settaPDF-nome-file.
           move DestFile  to settaPDF-percorso.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".

      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     string NomeFile   delimited low-value
      *****            ".pdf"     delimited size
      *****            into NomeFile
      *****            
      *****     inspect DestFile replacing trailing spaces by low-value.
               
           initialize link-path.       
      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     else
      *****        string DestFile   delimited by low-value
      *****               "\"        delimited by size
      *****               NomeFile   delimited by low-value
      *****               into link-path
      *****     end-if. 

      ***--- 
       CARICA-FONT.
      * Courier 8
           initialize wfont-data Courier8.
           move 8 to wfont-size.
           move "Courier"        to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Courier8, wfont-data
                        giving WFONT-STATUS. 

      ***---
       ASPETTA-PDF.
           set settaPDF-resetta   to true.
           call   "settaPDF2" using settaPDF-linkage.
           cancel "settaPDF2".
                         
      ***---
       FINE-PGM-COMUNE.                           
           if settaPDF-OK                         
              set spl-chiusura to true
              call   "spooler" using spooler-link
              cancel "spooler"   
              
              perform ASPETTA-PDF

              if settaPDF-ok
                 move settaPDF-nome-file to link-path
                 move link-path to LinkAttach        

                 set errori to true
                 move 0 to tentativi
                 perform 5 times
                    add 1 to tentativi
                    perform SEND-MAIL

      *        call "C$DELETE" using FileDest
                    open input lineseq1
                    read  lineseq1 next
                    if line-riga of lineseq1 = "True"
                       set tutto-ok to true
                       close lineseq1
                       exit perform
                    end-if
                    close lineseq1
         
                  end-perform

                  if trovato
                     perform AGGIORNA-CONTATORE
                  end-if

               end-if
           end-if.

      ***---                                 
       EXIT-PGM.  
           close tmovtrat.
           destroy Courier8.
           goback.                     

      ***---
       AGGIORNA-CONTATORE.
           rewrite tra-rec invalid continue end-rewrite.
           unlock tmovtrat all record.
