       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rice-clienti.
       AUTHOR.                          Andrea.
       REMARKS. Riceve in input i parametri di ricerca e riempie il file tmp
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmp-ricerca-clienti.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmp-ricerca-clienti.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       77  status-tmp-ricerca-clienti  pic xx.
       77  status-clienti              pic xx.
       77  path-tmp-ricerca-clienti    pic x(200).
       77  n-rec                       pic 9(5) value 0.
       77  como-data                   pic 9(8).
       77  como-ora                    pic 9(8).
       77  como-file                   pic x(20).
       77  stato-zoom                  signed-long.

           copy "externals.def".

       LINKAGE SECTION.
       copy "link-rice-clienti.def".

      ******************************************************************
       PROCEDURE DIVISION USING rice-clienti-linkage.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.      
           move 0 to link-rc-annulla.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp-ricerca-clienti
           accept  path-tmp-ricerca-clienti from environment "PATH_ST".
           inspect path-tmp-ricerca-clienti 
                   replacing trailing spaces by low-value.
           string  path-tmp-ricerca-clienti delimited low-value
                   "TMP-RICERCA-CLIENTI_"   delimited size
                   como-data                delimited size
                   "_"                      delimited size
                   como-ora                 delimited size
                   ".tmp"                   delimited size
              into path-tmp-ricerca-clienti
           end-string.
           inspect path-tmp-ricerca-clienti 
                   replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.
           open output tmp-ricerca-clienti.
           close       tmp-ricerca-clienti.
           open i-o    tmp-ricerca-clienti.
           open input clienti.

      ***---
       ELABORAZIONE.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    if cli-piva not = link-rc-piva
                       exit perform cycle
                    end-if
                    if n-rec = 0
                       move cli-codice to link-rc-codice
                    end-if
                    add 1 to n-rec
                    move cli-codice    to trc-cli-codice   
                    move cli-ragsoc-1  to trc-cli-ragsoc-1 
                    move cli-ragsoc-2  to trc-cli-ragsoc-2 
                    move cli-indirizzo to trc-cli-indirizzo
                    move cli-cap       to trc-cli-cap      
                    move cli-localita  to trc-cli-localita 
                    move cli-prov      to trc-cli-prov     
                    move cli-nazione   to trc-cli-nazione  
                    move cli-codfis    to trc-cli-codfis   
                    move cli-piva      to trc-cli-piva     
                    move cli-tipo      to trc-cli-tipo     
                    write trc-cli-rec
                 end-perform
           end-start.             
           if n-rec > 1
              move 0 to link-rc-codice
              move path-tmp-ricerca-clienti to ext-file
              move   "tmp-ricerca-clienti" to como-file
              call   "zoom-gt"          using como-file, trc-cli-rec
                                       giving stato-zoom
              cancel "zoom-gt"
              if stato-zoom = 0
                 move trc-cli-codice to link-rc-codice
              else
                 move 1 to link-rc-annulla
              end-if   
           end-if. 

      ***---
       CLOSE-FILES.
           close tmp-ricerca-clienti clienti.

      ***---
       EXIT-PGM.
           goback.

                  
