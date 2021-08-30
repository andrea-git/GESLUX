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

       LINKAGE SECTION.
       copy "link-rice-clienti.def".

      ******************************************************************
       PROCEDURE DIVISION USING rice-clienti-linkage.

      ***---
       MAIN-PRG.
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
           open output tmp-ricerca-clienti.
           close       tmp-ricerca-clienti.
           open i-o    tmp-ricerca-clienti.
           open input clienti.
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
           set cli-tipo-c to true.
           move   "clienti-all"  to como-file.
           call   "zoom-gt"   using como-file, cli-rec
                             giving stato-zoom.
           cancel "zoom-gt".
      
           if stato-zoom = 0
              if old-cli-chiave  not =  cli-chiave
                 move cli-chiave   to  save-chiave
                 move cli-ragsoc-1 to  save-ragsoc-K1
                 perform SALV-MOD
                 if tutto-ok
                    move save-ragsoc-K1 to cli-ragsoc-1
                    move save-chiave    to cli-chiave
                    move cli-codice     to codice-ed
                    move codice-ed      to ef-codice-buf   
                    call "C$JUSTIFY" using ef-codice-buf, "L"
                    move cli-ragsoc-1   to ef-ragsoc-1-buf 
                                             lab-des-buf
                    display ef-codice lab-des ef-ragsoc-1
                    set     ReadSecca    to true 
                    perform CANCELLA-COLORE
                    perform CURRENT-RECORD
                    move 78-ID-ef-codice to control-id
                    move 4 to accept-control
                 end-if
              end-if   
           end-if. 
           close tmp-ricerca-clienti clienti.

                  
