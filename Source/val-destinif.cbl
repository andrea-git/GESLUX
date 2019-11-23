       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-destinif.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destinif.sl".
           copy "nforn.sl".
           copy "nforn-dest.sl".



      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "clienti.fd".
           copy "destinif.fd".
           copy "nforn.fd".
           copy "nforn-dest.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-clienti    pic X(2).
       77  status-destinif   pic X(2).
       77  status-nforn      pic X(2).
       77  status-nforn-dest pic X(2).

       77  CONT              PIC 9(3).
       77  CONT-ED           PIC Z(3).
       77  scelta            pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     clienti
                     destinif
                     nforn
                     nforn-dest
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
      *     display message box 
      *                    "Confermi la conversione del file clienti?"
      *                    x"0D0A"
      *                    "Prima di procedere con la conversione"
      *                    x"0D0A"
      *                    "rinominare il file "
      *                    x"22"
      *                    "clienti"
      *                    x"22"
      *                    " in "
      *                    x"22"
      *                    "clienti-old"
      *                    x"22"
      *                    "."
      *                    type mb-yes-no
      *                    default mb-no
      *                    giving scelta
      *                    icon 2
      *     if scelta = mb-yes
              perform CONVERSIONE
      *     end-if.

           goback.


      ***---
       CONVERSIONE.
           move zero   to cont

           open input clienti.
           open i-o destinif
           open input nforn.
           open i-o nforn-dest.

           move low-value to cli-chiave.
           set cli-tipo-F to true.
           
           start clienti key >= cli-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read clienti next 
                       at end 
                          exit perform 
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close clienti
                 destinif
                 nforn
                 nforn-dest.


           move cont   to cont-ed
           display message box "Aggiunti " cont-ed " destini.".

      ***---
       MUOVI-RECORD.
           move cli-codice   to desf-codice
           move 1            to desf-prog
           read destinif
              invalid
                 add 1 to cont
                 move cli-ragsoc-1          to desf-ragsoc-1 
                 move cli-ragsoc-2          to desf-ragsoc-2 
                 move cli-indirizzo         to desf-indirizzo
                 move cli-cap               to desf-cap      
                 move cli-localita          to desf-localita 
                 move cli-prov              to desf-prov     
                 move cli-nazione           to desf-nazione  
                 move cli-tel-1             to desf-telef-1  
                 move cli-tel-2             to desf-telef-2  
                 move cli-fax               to desf-fax    
                 move cli-email             to desf-mail   
                 move cli-superamento-500   to desf-superamento-500
                 move cli-stato             to desf-stato
                 move cli-referente         to desf-referente
                 move cli-vettore           to desf-vettore
                 move cli-utf               to desf-depostio-UTF
                 move cli-referente-ord     to desf-referente-ord        
                 move cli-tel-dir-ref-ord   to desf-tel-dir-ref-ord      
                 move cli-mail-ref-ord      to desf-mail-ref-ord         
                 move cli-dati-comuni       to desf-dati-comuni
                 write desf-rec 
                    invalid 
                       continue 
                 end-write
           end-read.

           move cli-chiave   to nfor-chiave-forn.
           move low-value    to nfor-prog

           start nforn key not < nfor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read nforn next
                       at end
                          exit perform
                    end-read
                    if cli-chiave not = nfor-chiave-forn
                       exit perform
                    end-if

                    move nfor-codice  to nfod-codice
                    move 1            to nfod-dest
                    move nfor-prog    to nfod-prog

                    move nfor-nota    to nfod-nota
                    move nfor-dati-comuni   to nfod-dati-comuni

                    write nfod-rec 
                       invalid 
                          continue
                    end-write
                 end-perform
           end-start.
