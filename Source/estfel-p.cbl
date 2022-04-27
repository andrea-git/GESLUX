       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      estfel-p.
       AUTHOR.                          Andrea.
       REMARKS. Estrazione e calcolo fatture elettroniche
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.           
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "lineseq.sl".
           copy "articoli.sl".       
           copy "blister.sl".
           copy "paramfel.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tcontat.sl".
           copy "tcaumag.sl".       
           copy "tvettori.sl".
           copy "tivaese.sl".
           copy "tcodpag.sl".
           copy "CLI.sl".
           copy "eordini.sl".
           copy "tnazioni.sl".
           copy "recapiti.sl".
           copy "edi-clides.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.    
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "lineseq.fd".
           copy "articoli.fd".       
           copy "blister.fd".
           copy "paramfel.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tcontat.fd".
           copy "tcaumag.fd".       
           copy "tvettori.fd".
           copy "tivaese.fd".
           copy "tcodpag.fd".
           copy "CLI.fd".
           copy "eordini.fd". 
           copy "tnazioni.fd".        
           copy "recapiti.fd".  
           copy "edi-clides.fd". 
           copy "ttipocli.fd". 

       WORKING-STORAGE SECTION.      
       copy "varsca".
       copy "link-geslock.def".

      * FILE STATUS                       
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-articoli       pic xx.
       77  status-blister        pic xx.
       77  status-paramfel       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tcontat        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tvettori       pic xx.
       77  status-tivaese        pic xx.
       77  status-tcodpag        pic xx.
       77  status-CLI            pic xx.
       77  status-lineseq        pic xx.    
       77  status-eordini        pic xx.
       77  status-tnazioni       pic xx.
       77  status-recapiti       pic xx.
       77  status-edi-clides     pic xx.
       77  status-ttipocli       pic xx.
       77  cod-pag               pic x(3).
       77  data-doc              pic 9(8).
       77  num-doc               pic 9(8).
       77  tipo-doc              pic x.
       77  wstampa               pic x(256).
       77  como-importo          pic 9(4)v99.

      * COSTANTI
       78  titolo                value "Fatturazione elettronica".
       78  OgniQuanti            value 50.
       78  78-spazi              value "  ".
       78  78-max-doc            value 250.
       
      * VARIABILI                           
       01  abilitazioni-nazione.       
         05 cli-naz-fe           pic x.
         05 des-naz-fe           pic x.
       77  prg-invio             pic 9(5).
       77  como-des              pic x(80).
       77  como-note             pic x(500).
       77  data-cons             pic 9(8).
       77  ora-cons              pic 9(8). 
       77  ult-doc               pic 9(8).
       77  scad-importo          pic 9(8)v99.
       77  scad-data             pic x(10).
       77  tot-fattura           pic 9(9)v99. 
       77  tot-iva               pic 9(10)v99.
       77  como-iva              pic 9(8)v999.
       77  como-iva-2dec         pic 9(8)v99.   
       77  scelta                pic 9(8).
       77  como-valore           pic 9(8).
       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).
       77  giorno                pic 9(2).
       77  mese                  pic 9(2).  
       77  anno                  pic 9(4).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).   
       77  CodPag                pic x(4).
       77  idx                   pic 9(9) value 0.
       77  como-prz              pic 9(8)v99.
       77  como-prz-tot          pic 9(8)v99.
       77  prg-riga              pic 9(4).
       77  codice-SDI            pic x(7).
       77  num-doc-from          pic 9(8).
       77  data-doc-from         pic 9(8).
       77  como-numero           pic x(15).
       77  como-ean              pic 9(13).

       01 tab-iva.
         03 el-iva               occurs 3.
            05 el-importo        pic 9(9)v99.
            05 el-perce-iva      pic 9(3)v99.
            05 el-cod-iva        pic x(3).
            05 el-des-iva        pic x(60).  
            05 el-natura-iva     pic x(10).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".      
       77  filler                pic 9.
           88 record-ok          value 1, false 0.

       LINKAGE SECTION.
           copy "link-estfel.def".

      ******************************************************************
       PROCEDURE DIVISION USING lfel-linkage.

       DECLARATIVES.          
       PARAMFEL-ERR SECTION.
           use after error procedure on paramfel.
           continue.          
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           continue.
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           continue.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.   
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                                     
           move 0 to counter counter2 lfel-status.
           set tutto-ok        to true.

      ***---
       OPEN-FILES.            
           open input paramfel.
           if status-paramfel not = "00"
              move -1 to lfel-status
              display message "Generazione impossibile."
                       x"0d0a""Parametri inesistenti"
                        title titolo
                         icon 2
              goback
           end-if.

           perform OPEN-TCONTAT-LOCK.
           if errori
              close paramfel
              goback
           end-if.

           open input 
           tordini
           rordini
           tnotacr
           rnotacr
           articoli
           blister 
           clienti   
           destini
           tcaumag       
           tvettori
           tivaese
           tcodpag
           CLI
           eordini
           tnazioni
           recapiti
           edi-clides
           ttipocli.

           if errori goback end-if.


      ***---
       OPEN-TCONTAT-LOCK.
           initialize geslock-linkage.
           move "tcontat" to geslock-nome-file.
      
           set tutto-ok  to true.
           open i-o tcontat allowing readers.
           if status-tcontat = "93"
              move "Tabella contatori in uso su altro terminale." 
                to geslock-messaggio
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-TCONTAT-LOCK
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           set errori to true
              end-evaluate         
           end-if.                 

      ***---
       ELABORAZIONE.                          
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           move spaces to pfel-chiave.
           read paramfel no lock
                invalid
                display message "Generazione impossibile."
                         x"0d0a""Parametri inesistenti"
                          title titolo
                           icon 2
                move -1 to lfel-status
                exit paragraph
           end-read.                
                               
           move lfel-anno to con-anno.
           read tcontat no lock.

           if lfel-f
              perform ELABORA-FATTURE
           else
              perform ELABORA-NOTECR
           end-if.       

      ***---
       ELABORA-FATTURE.
           move lfel-anno to tor-anno-fattura.
           move lfel-da   to tor-num-fattura.
           start tordini  key >= k-fattura
                 invalid  continue
             not invalid 
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-num-fattura > lfel-a
                       exit perform
                    end-if 
                    move tor-num-fattura to ult-doc
                    perform COUNTER-VIDEO
                    set cli-tipo-C to true
                    move tor-cod-cli     to cli-codice des-codice
                    move tor-prg-destino to des-prog
                    read clienti no lock
                         invalid initialize cli-rec
                    end-read

                    move cli-nazione to naz-codice
                    read tnazioni no lock 
                         invalid  move space to naz-fe
                    end-read
                    move naz-fe to cli-naz-fe

                    read destini no lock
                         invalid 
                         initialize des-rec
                         move cli-nazione to des-nazione
                    end-read

                    move des-nazione to naz-codice
                    read tnazioni no lock 
                         invalid  move space to naz-fe
                    end-read
                    move naz-fe to des-naz-fe

                    move cli-tipo to tcl-codice
                    read ttipocli
                    move cli-codice to rec-codice
                    read recapiti no lock
                         invalid initialize rec-rec
                    end-read
                    if tor-num-fattura < 14950 exit perform cycle end-if
                    if cli-naz-fe = "S" or 
                       des-naz-fe = "S"
      *****              if cli-nazione(1:2) = "IT" or 
      *****                 des-nazione(1:2) = "IT"
                       move cli-codice to cli-codice-G2
                       read CLI no lock
                            invalid initialize record-cli
                       end-read
                       move tor-vettore to vet-codice
                       read tvettori no lock
                            invalid initialize vet-rec
                       end-read   
                       move tor-causale to tca-codice
                       read tcaumag no lock
                            invalid initialize tca-rec
                       end-read              
                       move "PA"              to tblpa-codice1
                       move tor-cod-pagamento to tblpa-codice2
                       read tcodpag no lock
                            invalid continue
                       end-read
                       move tor-data-fattura  to data-doc
                       move tor-num-fattura   to num-doc
                       move tor-cod-pagamento to cod-pag
                       perform ELABORA-DOCUMENTO
                    end-if
                 end-perform
           end-start.                      

           if lfel-gen
              move ult-doc to con-ult-num-fel-f
           end-if.
           perform AGGIORNA-CONTATORI.

      ***---
       ELABORA-NOTECR.
           move lfel-anno to tno-anno-fattura.
           move lfel-da   to tno-num-fattura.
           start tnotacr key >= k-fattura
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                    if tno-num-fattura > lfel-a
                       exit perform
                    end-if    

                    if tno-fattura-from-numero not = 0 and 
                       tno-fattura-from-data   not = 0
                       move tno-fattura-from-data(1:4) 
                         to tor-anno-fattura
                       move tno-fattura-from-numero    
                         to tor-num-fattura
                       read tordini key k-fattura
                            invalid
                            move 0 to tno-fattura-from-numero
                            display message "Fattura non trovata"
                                      title titolo
                                       icon 2
                       end-read
                    else    
                       move spaces to tor-num-ord-cli
                    end-if

                    move tno-num-fattura to ult-doc
                    perform COUNTER-VIDEO
                    set cli-tipo-C to true
                    move tno-cod-cli     to cli-codice des-codice
                    move tno-prg-destino to des-prog
                    read clienti no lock
                         invalid initialize cli-rec
                    end-read
                    read destini no lock
                         invalid initialize des-rec
                    end-read    

                    move cli-nazione to naz-codice
                    read tnazioni no lock 
                         invalid  move space to naz-fe
                    end-read
                    move naz-fe to cli-naz-fe

                    move cli-tipo to tcl-codice
                    read ttipocli
      *****              if cli-nazione(1:2) = "IT"
                    if cli-naz-fe = "S"
                       move cli-codice to cli-codice-G2
                       read CLI no lock
                            invalid initialize record-cli
                       end-read   
                       move tno-causale to tca-codice
                       read tcaumag no lock
                            invalid initialize tca-rec
                       end-read              
                       move "PA"              to tblpa-codice1
                       move tno-cod-pagamento to tblpa-codice2
                       read tcodpag no lock
                            invalid continue
                       end-read                 
                       move tno-data-fattura to data-doc
                       move tno-num-fattura  to num-doc 
                       move tno-cod-pagamento to cod-pag
                       perform ELABORA-DOCUMENTO
                    end-if
                 end-perform
           end-start.               

           if lfel-gen
              move ult-doc to con-ult-num-fel-nc
           end-if.        
           perform AGGIORNA-CONTATORI.

      ***---
      * Siccome il progressivo di invio è sempre incrementale e 
      * dev'essere lo stesso sul nome del file e nel tag ProgressivoInvio
      * l'elaborazione deve PER FORZA scrivere il contatore altrimenti
      * si perde l'allineamento
       AGGIORNA-CONTATORI.
           perform until 1 = 2
              rewrite con-rec
              if status-tcontat = "00"
                 exit perform
              end-if
           end-perform.       

      ***---
       CALCOLA-TOTALE.      
           initialize tab-iva como-prz como-prz-tot
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           if lfel-f
              move 0 to prg-riga
              move low-value   to ror-rec
              move tor-anno    to ror-anno
              move tor-numero  to ror-num-ordine
              start rordini key > ror-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordini next at end exit perform end-read
                       if ror-anno       not = tor-anno or
                          ror-num-ordine not = tor-numero
                          exit perform
                       end-if          
                       move "IV"        to tbliv-codice1
                       move ror-cod-iva to tbliv-codice2
                       read tivaese no lock
                            invalid continue
                       end-read       
                       perform LEGGI-RIGA-DOCUMENTO
                    end-perform
              end-start
           else
              move 0 to prg-riga
              move low-value   to rno-rec
              move tno-anno    to rno-anno
              move tno-numero  to rno-numero
              start rnotacr key > rno-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rnotacr next at end exit perform end-read
                       if rno-anno   not = tno-anno or
                          rno-numero not = tno-numero
                          exit perform
                       end-if          
                       move "IV"        to tbliv-codice1
                       move rno-cod-iva to tbliv-codice2 ror-cod-iva
                       read tivaese no lock
                            invalid continue
                       end-read
                       move rno-des-libera    to ror-des-libera 
                       move rno-qta           to ror-qta
                       move rno-imp-consumo   to ror-imp-consumo
                       move rno-imp-cou-cobat to ror-imp-cou-cobat
                       move rno-prz-unitario  to ror-imponib-merce
                       move rno-add-piombo    to ror-add-piombo
                       perform LEGGI-RIGA-DOCUMENTO
                    end-perform
              end-start
           end-if.      
           move 0 to tot-fattura.
           move 0 to tot-iva.           
           perform varying idx from 1 by 1 until idx > 3
              if el-cod-iva(idx) not = spaces
                 move 0 to como-iva
                 compute como-iva = 
                   ( ( el-importo(idx) * el-perce-iva(idx) ) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec 
  
                 move el-perce-iva(idx) to como-prz
                 move el-importo(idx)   to como-prz-tot
                                       
                 compute tot-fattura = tot-fattura  +
                                       como-prz-tot +
                                       como-iva-2dec
              end-if
           end-perform.

      ***---
       LEGGI-RIGA-DOCUMENTO.
           compute como-prz = ror-imp-consumo   + 
                              ror-imp-cou-cobat +
                              ror-add-piombo    +
                              ror-imponib-merce  
           if ror-qta = 0
              move como-prz to como-prz-tot
           else
              compute como-prz-tot = como-prz * ror-qta
           end-if.                    
           inspect tbliv-descrizione1 
                   replacing trailing spaces by low-value.
           perform varying idx from 1 by 1 until idx > 3
              if el-cod-iva(idx) = tbliv-codice2 or
                 el-cod-iva(idx) = spaces
                 move tbliv-percentuale to el-perce-iva(idx)
                 move tbliv-codice2     to el-cod-iva(idx)
                 add como-prz-tot       to el-importo(idx)
                 string tbliv-descrizione1 delimited low-value
                        tbliv-descrizione2 delimited size
                    into el-des-iva(idx) 
                 end-string
                 move tbliv-natura-iva to el-natura-iva(idx)
                 exit perform
              end-if
           end-perform.          


      ***---
       ELABORA-DOCUMENTO.
           perform CALCOLA-TOTALE.
           add 1 to con-ult-prg-invio-fel.
           move con-ult-prg-invio-fel to prg-invio.
           perform OPEN-FILE-XML.
           initialize tab-iva como-prz como-prz-tot
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           if status-lineseq not = "00"
              exit paragraph
           end-if.
           initialize line-riga.
           string 78-spazi 
                  "<FatturaElettronicaHeader>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  "<DatiTrasmissione>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<IdTrasmittente>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  78-spazi
                  "<IdPaese>IT</IdPaese>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           inspect pfel-IdCodice replacing trailing spaces by low-value
           string 78-spazi 
                  78-spazi
                  78-spazi 
                  78-spazi
                  "<IdCodice>"
                  pfel-CodiceFiscale delimited low-value
                  "</IdCodice>" 
      *            HVQWPH73P42H501Y
             into line-riga.
           write line-riga.  
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "</IdTrasmittente>" 
             into line-riga.
           write line-riga.  
           initialize line-riga. 
                                         
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<ProgressivoInvio>"
                  prg-invio
                  "</ProgressivoInvio>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<FormatoTrasmissione>FPR12</FormatoTrasmissione>" 
             into line-riga.
           write line-riga.                       
           initialize line-riga.
           move cli-codice-SDi to codice-SDI.
           if codice-SDI = spaces
              move all "0" to codice-SDI
           end-if.
           inspect cli-codice-SDI replacing trailing spaces by low-value
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<CodiceDestinatario>"
                  codice-SDI  delimited low-value
                  "</CodiceDestinatario>" 
             into line-riga.
           write line-riga.
           initialize line-riga.  
           if cli-pec not = spaces
              inspect cli-pec replacing trailing spaces by low-value
              string 78-spazi 
                     78-spazi
                     78-spazi
                     "<PECDestinatario>"
                     cli-pec delimited low-value
                     "</PECDestinatario>" 
                into line-riga
              write line-riga
           end-if.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  "</DatiTrasmissione>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  "<CedentePrestatore>" 
             into line-riga.
           write line-riga.  
           initialize line-riga.
           string 78-spazi         
                  78-spazi
                  78-spazi
                  "<DatiAnagrafici>" 
             into line-riga.
           write line-riga.  
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi  
                  78-spazi
                  "<IdFiscaleIVA>" 
             into line-riga.
           write line-riga.  
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi 
                  78-spazi
                  78-spazi
                  "<IdPaese>IT</IdPaese>" 
             into line-riga.
           write line-riga.   
           initialize line-riga.
           inspect pfel-IdCodice replacing trailing spaces by low-value.
           string 78-spazi 
                  78-spazi
                  78-spazi 
                  78-spazi
                  78-spazi
                  "<IdCodice>"
                  pfel-IdCodice delimited low-value
                  "</IdCodice>" 
             into line-riga.
           write line-riga.   
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "</IdFiscaleIVA>" 
             into line-riga.
           write line-riga. 
           initialize line-riga.     
           initialize line-riga.
           string 78-spazi   
                  78-spazi     
                  78-spazi
                  78-spazi
                  "<Anagrafica>" 
             into line-riga.
           write line-riga.
           initialize line-riga.                 
           inspect pfel-Denominazione 
                  replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi
                  78-spazi     
                  78-spazi
                  78-spazi
                  "<Denominazione>"
                  pfel-Denominazione delimited low-value
                  "</Denominazione>" 
             into line-riga.
           write line-riga.     
           initialize line-riga.
           string 78-spazi   
                  78-spazi     
                  78-spazi
                  78-spazi
                  "</Anagrafica>" 
             into line-riga.
           write line-riga.       
           initialize line-riga. 
           string 78-spazi   
                  78-spazi
                  78-spazi     
                  78-spazi      
                  "<RegimeFiscale>"
                  pfel-RegimeFiscale
                  "</RegimeFiscale>" 
             into line-riga.
           write line-riga.       
           initialize line-riga.
           string 78-spazi         
                  78-spazi
                  78-spazi
                  "</DatiAnagrafici>" 
             into line-riga.
           write line-riga.      
           initialize line-riga. 
           string 78-spazi         
                  78-spazi
                  78-spazi
                  "<Sede>" 
             into line-riga.
           write line-riga.        
           initialize line-riga. 
           move pfel-indirizzo to como-des.
           perform NORMALIZZA-DES.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<Indirizzo>"
                  como-des delimited low-value
                  "</Indirizzo>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           inspect pfel-NumeroCivico 
                   replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<NumeroCivico>"
                  pfel-NumeroCivico delimited low-value
                  "</NumeroCivico>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<CAP>"
                  pfel-CAP
                  "</CAP>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.   
           inspect pfel-Comune
                   replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<Comune>"
                  pfel-Comune delimited low-value
                  "</Comune>" 
             into line-riga.
           write line-riga.          
           initialize line-riga.  
           inspect pfel-Provincia
                   replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<Provincia>"    
                  pfel-Provincia
                  "</Provincia>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           inspect pfel-Nazione
                   replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<Nazione>"
                  pfel-Nazione 
                  "</Nazione>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  78-spazi 
                  "</Sede>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi   
                  78-spazi       
                  "</CedentePrestatore>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  "<CessionarioCommittente>" 
             into line-riga.
           write line-riga.        
           initialize line-riga.
           string 78-spazi      
                  78-spazi   
                  78-spazi   
                  "<DatiAnagrafici>" 
             into line-riga.
           write line-riga.               
           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  78-spazi   
                  78-spazi       
                  "<IdFiscaleIVA>"               
             into line-riga
           end-string.
           write line-riga.     
     
           move cli-nazione to naz-codice.
           read tnazioni no lock 
                invalid move cli-nazione to naz-cod-edi
           end-read.
           inspect naz-cod-edi replacing trailing spaces by low-value.

           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  78-spazi           
                  78-spazi       
                  78-spazi       
                  "<IdPaese>"    
                  naz-cod-edi delimited low-value
                  "</IdPaese>"   
             into line-riga
           end-string.
           write line-riga. 

      * per evitare l’errore 00305 1.4.1.1.2. Questo è un errore molto comune. 
      * Viene sollevato quando per un anagrafica avente codice fiscale numerico, 
      * questo viene anche inviato come partita IVA. Questa problematica è molto 
      * comune con le anagrafiche delle associazioni, che hanno il codice fiscale
      * numerico che inizia normalmente con il numero 9. In questo caso, se questo
      * codice fiscale venisse anche inviato come partita IVA, la fattura verrà 
      * scartata appunto con l’errore 00305.

      *Soluzione generale
      * Per risolvere il problema basta rettificare la fattura, eliminando 
      * la partita IVA, ed inviando il dato solo come codice fiscale.

      * fonte: https://www.newsoftit.net/err-fatt-sdi-00305
 
           if cli-piva(1:1) = "9"
              move spaces to cli-piva
           end-if.
   
           if cli-piva not = spaces
              inspect cli-piva replacing trailing spaces by low-value
              initialize line-riga
              string 78-spazi   
                     78-spazi   
                     78-spazi             
                     78-spazi 
                     78-spazi       
                     "<IdCodice>"
                     cli-piva delimited low-value
                     "</IdCodice>" 
                into line-riga
              end-string
              write line-riga
           else
              initialize line-riga
              string 78-spazi   
                     78-spazi   
                     78-spazi             
                     78-spazi 
                     78-spazi       
                     "<IdCodice>"
                     "00000000000" delimited low-value
                     "</IdCodice>" 
                into line-riga
              end-string
              write line-riga
      *****     else
      *****        if cli-codfis not = spaces           
      *****           inspect cli-codfis 
      *****                   replacing trailing spaces by low-value
      *****           initialize line-riga
      *****           string 78-spazi   
      *****                  78-spazi   
      *****                  78-spazi   
      *****                  78-spazi   
      *****                  78-spazi     
      *****                  "<IdCodice>"
      *****                  cli-codfis delimited low-value
      *****                  "</IdCodice>" 
      *****             into line-riga
      *****           end-string
      *****           write line-riga
      *****        end-if
           end-if.     

           initialize line-riga.
           string 78-spazi   
                  78-spazi   
                  78-spazi   
                  78-spazi       
                  "</IdFiscaleIVA>"               
             into line-riga
           write line-riga. 
                            
           if cli-piva = spaces and cli-codfis not = spaces   
              inspect cli-codfis replacing trailing spaces by low-value
              initialize line-riga
              string 78-spazi   
                     78-spazi   
                     78-spazi   
                     78-spazi       
                     "<CodiceFiscale>"
                     cli-codfis delimited low-value
                     "</CodiceFiscale>" 
                into line-riga
              end-string
              write line-riga
           end-if.

      *****     if cli-codfis not = spaces   
      *****        inspect cli-codfis replacing trailing spaces by low-value

      *****        initialize line-riga
      *****        string 78-spazi   
      *****               78-spazi   
      *****               78-spazi   
      *****               78-spazi       
      *****               "<CodiceFiscale>"
      *****               cli-codfis delimited low-value
      *****               "</CodiceFiscale>" 
      *****          into line-riga
      *****        end-string
      *****        write line-riga
      *****     else
      *****        if cli-piva not = spaces           
      *****           inspect cli-piva 
      *****                   replacing trailing spaces by low-value
      *****           initialize line-riga
      *****           string 78-spazi   
      *****                  78-spazi   
      *****                  78-spazi   
      *****                  78-spazi       
      *****                  "<CodiceFiscale>"
      *****                  cli-piva delimited low-value
      *****                  "</CodiceFiscale>" 
      *****             into line-riga
      *****           end-string
      *****           write line-riga
      *****        end-if
      *****     end-if.

           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "<Anagrafica>" 
             into line-riga.
           write line-riga.                              
           initialize como-des.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.
           inspect cli-ragsoc-2 replacing trailing spaces by low-value.
           string  cli-ragsoc-1 delimited low-value
                   cli-ragsoc-2 delimited low-value
               into como-des
           end-string.
           perform NORMALIZZA-DES.
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "<Denominazione>" 
                  como-des delimited low-value
                  "</Denominazione>" 
             into line-riga.
           write line-riga.     
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "</Anagrafica>" 
             into line-riga.
           write line-riga.     
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi     
                  "</DatiAnagrafici>" 
             into line-riga.
           write line-riga.     
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi     
                  "<Sede>" 
             into line-riga.
           write line-riga.   
           move cli-indirizzo to como-des.
           perform NORMALIZZA-DES.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Indirizzo>" 
                  como-des delimited low-value
                  "</Indirizzo>"
             into line-riga.
           write line-riga.
           if cli-cap of clienti = "EE" or spaces
              move "00000" to cli-cap of clienti
           end-if.
           inspect cli-cap of clienti
                   replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<CAP>" 
                  cli-cap of clienti delimited low-value
                  "</CAP>"
             into line-riga
           write line-riga.
           move cli-localita to como-des.
           perform NORMALIZZA-DES.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Comune>" 
                  como-des delimited low-value
                  "</Comune>"
             into line-riga.
           write line-riga.   
           inspect cli-prov replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Provincia>" 
                  cli-prov delimited low-value
                  "</Provincia>"
             into line-riga.
           write line-riga.      

           move cli-nazione to naz-codice.
           read tnazioni no lock 
                invalid move cli-nazione to naz-cod-edi
           end-read.
           inspect naz-cod-edi replacing trailing spaces by low-value.

           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Nazione>" 
                  naz-cod-edi delimited low-value
                  "</Nazione>"
             into line-riga.
           write line-riga.   
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi     
                  "</Sede>" 
             into line-riga.
           write line-riga.   
           initialize line-riga.
           string 78-spazi 
                  78-spazi     
                  "</CessionarioCommittente>" 
             into line-riga.
           write line-riga.  

           initialize line-riga.
           string 78-spazi 
                  "</FatturaElettronicaHeader>" 
             into line-riga.
           write line-riga.

           initialize line-riga.
           string 78-spazi 
                  "<FatturaElettronicaBody>" 
             into line-riga.
           write line-riga.                
           initialize line-riga.           
           string 78-spazi 
                  78-spazi 
                  "<DatiGenerali>" 
             into line-riga.
           write line-riga.                
           initialize line-riga. 
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  "<DatiGeneraliDocumento>" 
             into line-riga.
           write line-riga.                
           initialize line-riga. 
           if lfel-f
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<TipoDocumento>TD01</TipoDocumento>" 
                into line-riga
           else
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<TipoDocumento>TD04</TipoDocumento>" 
                into line-riga
           end-if
           write line-riga.                
           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi 
                  78-spazi 
                  "<Divisa>EUR</Divisa>" 
             into line-riga.
           write line-riga.               
           initialize line-riga. 
           if lfel-f            
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Data>"
                     tor-data-fattura(1:4)
                     "-"
                     tor-data-fattura(5:2)
                     "-"
                     tor-data-fattura(7:2)
                     "</Data>"
                into line-riga
              write line-riga
           else                      
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Data>"
                     tno-data-fattura(1:4)
                     "-"
                     tno-data-fattura(5:2)
                     "-"
                     tno-data-fattura(7:2)
                     "</Data>"
                into line-riga
              write line-riga
           end-if.                      
           initialize line-riga.  
           if lfel-f      
              move tor-num-fattura to como-numero
              perform EDIT-NUMERO
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Numero>"
                     como-numero delimited low-value
                     "</Numero>"
                into line-riga
               write line-riga    
           else                                     
              move tno-num-fattura to como-numero
              perform EDIT-NUMERO
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Numero>"
                     como-numero delimited low-value
                     "</Numero>"
                into line-riga
               write line-riga    
           end-if.                           
           move tot-fattura(1:9) to como-numero.
           perform EDIT-NUMERO.
           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi 
                  78-spazi 
                  "<ImportoTotaleDocumento>"
                  como-numero       delimited low-value
                  "."               delimited size
                  tot-fattura(10:2) delimited size
                  "</ImportoTotaleDocumento>"
             into line-riga.
           write line-riga.
           if lfel-nc
              move tno-note to tor-note
           end-if.
           inspect tca-descrizione 
                   replacing trailing spaces by low-value. 
           inspect tor-note
                   replacing trailing spaces by low-value.
           initialize line-riga.                           
           move tor-note to como-note.
           perform NORMALIZZA-NOTE.
           string 78-spazi 
                  78-spazi       
                  78-spazi 
                  78-spazi 
                  "<Causale>"
                  tca-descrizione delimited low-value
                  " "
                  como-note       delimited low-value
                  "</Causale>"
             into line-riga
           write line-riga.

           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi                  
                  "</DatiGeneraliDocumento>"
             into line-riga.
           write line-riga.           

           if tor-num-ord-cli not = spaces or des-cig not = spaces
              initialize line-riga
              string 78-spazi 
                     78-spazi              
                     78-spazi 
                     "<DatiOrdineAcquisto>"
                into line-riga
              write line-riga        
    
              if lfel-f
                 move low-value   to ror-rec
                 move tor-anno    to ror-anno
                 move tor-numero  to ror-num-ordine
                 start rordini key > ror-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rordini next at end exit perform end-read
                          if ror-anno       not = tor-anno or
                             ror-num-ordine not = tor-numero
                             exit perform
                          end-if          
                          move ror-num-riga to como-numero
                          perform EDIT-NUMERO
                          initialize line-riga
                          string 78-spazi
                                 78-spazi
                                 78-spazi
                                 78-spazi
                                 "<RiferimentoNumeroLinea>"
                                 como-numero delimited low-value
                                 "</RiferimentoNumeroLinea>"
                            into line-riga
                          write line-riga
                       end-perform
                 end-start
              else        
                 move low-value   to rno-rec
                 move tno-anno    to rno-anno
                 move tno-numero  to rno-numero
                 start rnotacr key > rno-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read rnotacr next at end exit perform end-read
                          if rno-anno   not = tno-anno or
                             rno-numero not = tno-numero
                             exit perform
                          end-if          
                          move rno-num-riga to como-numero
                          perform EDIT-NUMERO
                          initialize line-riga
                          string 78-spazi
                                 78-spazi
                                 78-spazi
                                 78-spazi
                                 "<RiferimentoNumeroLinea>"
                                 como-numero delimited low-value
                                 "</RiferimentoNumeroLinea>"
                            into line-riga
                          write line-riga
                       end-perform        
                 end-start
              end-if                      
              
              if tor-num-ord-cli = spaces    
                 initialize line-riga
                 string 78-spazi 
                        78-spazi       
                        78-spazi 
                        78-spazi 
                        "<IdDocumento>"
                        "." delimited low-value
                        "</IdDocumento>"
                   into line-riga
                 write line-riga
              else
                 inspect tor-num-ord-cli
                         replacing trailing spaces by low-value
                 initialize line-riga
                 string 78-spazi 
                        78-spazi       
                        78-spazi 
                        78-spazi 
                        "<IdDocumento>"
                        tor-num-ord-cli(1:20) delimited low-value
                        "</IdDocumento>"
                   into line-riga
                 write line-riga
              end-if
           
              if des-cig not = spaces
                 inspect des-CIG
                         replacing trailing spaces by low-value
                 initialize line-riga
                 string 78-spazi 
                        78-spazi       
                        78-spazi 
                        78-spazi 
                        "<CodiceCIG>"
                        des-CIG delimited low-value
                        "</CodiceCIG>"
                   into line-riga
                 write line-riga    
              end-if   
              initialize line-riga
              string 78-spazi
                     78-spazi
                     78-spazi
                     "</DatiOrdineAcquisto>"
                into line-riga
              write line-riga
           end-if.

           if lfel-nc and tno-fattura-from-numero not = 0
              initialize line-riga
              string 78-spazi
                     78-spazi
                     78-spazi
                     "<DatiFattureCollegate>"
                into line-riga
              write line-riga

              move tno-fattura-from-numero to como-numero
              perform EDIT-NUMERO

              initialize line-riga
              string 78-spazi                
                     78-spazi                
                     78-spazi                
                     78-spazi                
                     "<IdDocumento>" 
                     como-numero             delimited low-value
                     "</IdDocumento>" 
                into line-riga
              write line-riga

              initialize line-riga
              string 78-spazi
                     78-spazi
                     78-spazi
                     "</DatiFattureCollegate>"
                into line-riga
              write line-riga
           end-if.

           if lfel-f or des-prog > 0
              if tor-num-bolla   > 0 or
                 tor-data-bolla  > 0
                 perform TAG-DDT
              end-if
              perform TAG-TRASPORTO
           end-if.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  "</DatiGenerali>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  "<DatiBeniServizi>"
             into line-riga.
           write line-riga.

           if lfel-f
              move 0 to prg-riga
              move low-value   to ror-rec
              move tor-anno    to ror-anno
              move tor-numero  to ror-num-ordine
              start rordini key > ror-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordini next at end exit perform end-read
                       if ror-anno       not = tor-anno or
                          ror-num-ordine not = tor-numero
                          exit perform
                       end-if
                       move ror-cod-articolo to art-codice
                       read articoli no lock
                            invalid
                            move ror-cod-articolo to bli-codice
                            read blister no lock
                                 invalid initialize art-descrizione
                             not invalid move bli-descrizione
                                           to art-descrizione
                            end-read
                       end-read
                       move "IV"        to tbliv-codice1
                       move ror-cod-iva to tbliv-codice2
                       read tivaese no lock
                            invalid continue
                       end-read
                       perform SCRIVI-RIGA-DOCUMENTO
                    end-perform
              end-start
           else
              move 0 to prg-riga
              move low-value   to rno-rec
              move tno-anno    to rno-anno
              move tno-numero  to rno-numero
              start rnotacr key > rno-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rnotacr next at end exit perform end-read
                       if rno-anno   not = tno-anno or
                          rno-numero not = tno-numero
                          exit perform
                       end-if
                       move rno-cod-articolo to art-codice
                            ror-cod-articolo
                       read articoli no lock
                            invalid
                            move rno-cod-articolo to bli-codice
                            read blister no lock
                                 invalid initialize art-descrizione
                             not invalid move bli-descrizione
                                           to art-descrizione
                            end-read
                       end-read
                       move "IV"        to tbliv-codice1
                       move rno-cod-iva to tbliv-codice2 ror-cod-iva
                       read tivaese no lock
                            invalid continue
                       end-read
                       move rno-des-libera    to ror-des-libera
                       move rno-qta           to ror-qta
                       move rno-imp-consumo   to ror-imp-consumo
                       move rno-imp-cou-cobat to ror-imp-cou-cobat
                       move rno-prz-unitario  to ror-imponib-merce
                       move rno-add-piombo    to ror-add-piombo
                       perform SCRIVI-RIGA-DOCUMENTO
                    end-perform
              end-start
           end-if.
           perform SCRIVI-RIEPILOGO.
           move "</p:FatturaElettronica>" to line-riga.
           write line-riga.
           close lineseq.

      ***---
       TAG-DDT.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  "<DatiDDT>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           move tor-num-bolla to como-numero.
           perform EDIT-NUMERO.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<NumeroDDT>"
                  como-numero delimited low-value
                  "</NumeroDDT>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<DataDDT>"
                  tor-data-bolla(1:4) delimited size
                  "-"                 delimited size
                  tor-data-bolla(5:2) delimited size
                  "-"                 delimited size
                  tor-data-bolla(7:2) delimited size
                  "</DataDDT>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  "</DatiDDT>"
             into line-riga.
           write line-riga. 
  
      ***---
       TAG-TRASPORTO.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  "<DatiTrasporto>"
             into line-riga.
           write line-riga.
           if lfel-f
              perform TAGS-VETTORE
           end-if.
           if des-prog > 0
              perform TAG-DESTINO
           end-if.
           if lfel-f
              move 0 to data-cons ora-cons
              move tor-chiave to eor-tor-chiave
              move high-value to eor-num-riga
              start eordini key <= eor-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read eordini previous
                            at end exit perform
                       end-read
                       if eor-tor-chiave not = tor-chiave
                          exit perform
                       end-if
                       if eor-dt-consegna > data-cons
                          move eor-dt-consegna  to data-cons
                          move eor-ora-consegna to ora-cons
                       end-if
                    end-perform
              end-start
              if data-cons > 0
                 initialize line-riga
                 string 78-spazi
                        78-spazi
                        78-spazi
                        78-spazi
                        "<DataOraConsegna>"
                        data-cons(1:4)
                        "-"
                        data-cons(5:2)
                        "-"
                        data-cons(7:2)
                        "T"
                        ora-cons(1:2)
                        ":"
                        ora-cons(3:2)
                        ":"
                        ora-cons(5:2)
                        "</DataOraConsegna>"
                   into line-riga
                 write line-riga
              end-if
           end-if.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  "</DatiTrasporto>"
             into line-riga.
           write line-riga.

      ***---
       TAGS-VETTORE.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<DatiAnagraficiVettore>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<IdFiscaleIVA>"
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi                 
                  78-spazi        
                  "<IdPaese>IT</IdPaese>"
             into line-riga.
           write line-riga.            
           initialize line-riga.    
           if vet-piva not = spaces
              inspect vet-piva replacing trailing spaces by low-value
              string 78-spazi 
                     78-spazi       
                     78-spazi       
                     78-spazi                 
                     78-spazi                 
                     78-spazi        
                     "<IdCodice>"
                     vet-piva delimited low-value
                     "</IdCodice>"
                into line-riga
              write line-riga
           else
              inspect vet-piva replacing trailing spaces by low-value
              string 78-spazi 
                     78-spazi       
                     78-spazi       
                     78-spazi                 
                     78-spazi                 
                     78-spazi        
                     "<IdCodice>"
                     "00000000000" delimited size
                     "</IdCodice>"
                into line-riga
              write line-riga
           end-if
           initialize line-riga.               
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi        
                  "</IdFiscaleIVA>"
             into line-riga.
           write line-riga.           
           initialize line-riga.                             
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi        
                  "<Anagrafica>"
             into line-riga.
           write line-riga.     
           if vet-descrizione not = spaces
              move vet-descrizione to como-des
              perform NORMALIZZA-DES
              initialize line-riga
              string 78-spazi 
                     78-spazi       
                     78-spazi       
                     78-spazi                 
                     78-spazi                 
                     78-spazi        
                     "<Denominazione>"
                     como-des delimited low-value
                     "</Denominazione>"
                into line-riga
              write line-riga
           else
              inspect vet-descrizione 
                      replacing trailing spaces by low-value
              initialize line-riga
              string 78-spazi 
                     78-spazi       
                     78-spazi       
                     78-spazi                 
                     78-spazi                 
                     78-spazi        
                     "<Denominazione>"
                     "DIRETTO"         delimited low-value
                     "</Denominazione>"
                into line-riga
              write line-riga
           end-if.
           initialize line-riga.                              
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi        
                  "</Anagrafica>"
             into line-riga.
           write line-riga.                
           initialize line-riga.           
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "</DatiAnagraficiVettore>"
             into line-riga.
           write line-riga. 

      ***---
       TAG-DESTINO.     
           move cli-codice to des-codice.
           move des-prog   to des-prog.
           read destini no lock
                invalid initialize des-dati
           end-read.                 
           initialize line-riga.               
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<IndirizzoResa>"
             into line-riga.
           write line-riga.                                             
           inspect des-indirizzo replacing trailing spaces by low-value.
           inspect des-localita  replacing trailing spaces by low-value.
           initialize line-riga.  
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<Indirizzo>"             
                  des-indirizzo delimited low-value
                  "</Indirizzo>"
             into line-riga.

           write line-riga.     
                                          
           if des-cap = "EE" or des-cap = spaces
              move "00000" to des-cap
           end-if.   
           inspect des-cap replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<CAP>"             
                  des-cap delimited low-value
                  "</CAP>"
             into line-riga
           write line-riga.
           initialize line-riga.  
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<Comune>"             
                  des-localita delimited low-value
                  "</Comune>"
             into line-riga.
           write line-riga.           
           initialize line-riga.    
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<Provincia>"             
                  des-prov
                  "</Provincia>"
             into line-riga.
           write line-riga.           
           initialize line-riga.    

           move des-nazione to naz-codice.
           read tnazioni no lock 
                invalid  move des-nazione to naz-cod-edi
           end-read.                               
           inspect naz-cod-edi replacing trailing spaces by low-value.
           
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "<Nazione>"             
                  naz-cod-edi delimited low-value
                  "</Nazione>"
             into line-riga.
           write line-riga.           
           initialize line-riga.               
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                  
                  "</IndirizzoResa>"
             into line-riga.
           write line-riga. 

      ***---      
       SCRIVI-RIGA-DOCUMENTO.             
           initialize line-riga.       
           string 78-spazi   
                  78-spazi   
                  78-spazi                  
                  "<DettaglioLinee>"
             into line-riga.
           write line-riga.
           add 1 to prg-riga.
           move prg-riga to como-numero.
           perform EDIT-NUMERO.
           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<NumeroLinea>"
                  como-numero delimited low-value
                  "</NumeroLinea>"
             into line-riga.
           write line-riga. 

           move 0 to como-ean.
           if art-codice-ean-1 > 0 
              move art-codice-ean-1 to como-ean
           else               
              if art-codice-ean-2 > 0 
                 move art-codice-ean-2 to como-ean
              else
                 if art-codice-ean-3 > 0 
                    move art-codice-ean-3 to como-ean
                 else
                    if art-codice-ean-4 > 0 
                       move art-codice-ean-4 to como-ean
                    else
                       if art-codice-ean-5 > 0 
                          move art-codice-ean-5 to como-ean
                       end-if
                    end-if
                 end-if
              end-if
           end-if.   
 
           if rec-escludi-int-no or como-ean = 0   
              initialize line-riga
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceArticolo>"
                into line-riga
              write line-riga

              initialize line-riga  
              string 78-spazi
                     78-spazi 
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceTipo>"
                     "INT"         
                     "</CodiceTipo>"
                into line-riga
              write line-riga     
              initialize line-riga 
              move art-codice to como-numero
              perform EDIT-NUMERO
              string 78-spazi
                     78-spazi 
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceValore>"
                     como-numero delimited low-value
                     "</CodiceValore>"
                into line-riga
              write line-riga     
              initialize line-riga  
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "</CodiceArticolo>"
                into line-riga

              write line-riga
           end-if.

           if como-ean > 0      
              initialize line-riga
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceArticolo>"
                into line-riga
              write line-riga     
              initialize line-riga  
              string 78-spazi
                     78-spazi 
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceTipo>"
                     "EAN"         
                     "</CodiceTipo>"
                into line-riga
              write line-riga     
              initialize line-riga 
              string 78-spazi
                     78-spazi 
                     78-spazi
                     78-spazi
                     78-spazi
                     "<CodiceValore>"
                     como-ean
                     "</CodiceValore>"
                into line-riga
              write line-riga     
              initialize line-riga  
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "</CodiceArticolo>"
                into line-riga
              write line-riga 
           end-if.
       
           initialize line-riga.
           if ror-cod-articolo = 0 |FM                      
              move ror-des-libera to como-des
              perform NORMALIZZA-DES
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<Descrizione>"
                     como-des   delimited low-value
                     "</Descrizione>"
                into line-riga
           else
              move art-descrizione to como-des
              perform NORMALIZZA-DES
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<Descrizione>"
                     como-des delimited low-value
                     "</Descrizione>"
                into line-riga
           end-if.
           write line-riga.  
           initialize line-riga.     
           if ror-qta not = 0
              move ror-qta to como-numero
              perform EDIT-NUMERO
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<Quantita>"
                     como-numero delimited low-value
                     ".00"
                     "</Quantita>"
                into line-riga
           else               
              string 78-spazi
                     78-spazi
                     78-spazi
                     78-spazi
                     "<Quantita>"
                     "0000001.00"
                     "</Quantita>"
                into line-riga
           end-if.
           write line-riga.                        
           initialize line-riga. 
           compute como-prz = ror-imp-consumo   + 
                              ror-imp-cou-cobat +
                              ror-add-piombo    +
                              ror-imponib-merce  
           if ror-qta = 0
              move como-prz to como-prz-tot
           else
              compute como-prz-tot = como-prz * ror-qta
           end-if.                    
           inspect tbliv-descrizione1 
                   replacing trailing spaces by low-value.
           perform varying idx from 1 by 1 until idx > 3
              if el-cod-iva(idx) = tbliv-codice2 or
                 el-cod-iva(idx) = spaces
                 move tbliv-percentuale to el-perce-iva(idx)
                 move tbliv-codice2     to el-cod-iva(idx)
                 add como-prz-tot       to el-importo(idx)
                 string tbliv-descrizione1 delimited low-value
                        tbliv-descrizione2 delimited size
                    into el-des-iva(idx) 
                 end-string
                 move tbliv-natura-iva to el-natura-iva(idx)
                 exit perform
              end-if
           end-perform.                                          
                   
           move ror-imponib-merce(1:9) to como-numero.
           perform EDIT-NUMERO.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<PrezzoUnitario>"    
                  como-numero delimited low-value
                  "."                   
                  ror-imponib-merce(10:2)
                  "</PrezzoUnitario>"
             into line-riga.
           write line-riga.                          

           if ror-imp-consumo   > 0 or
              ror-imp-cou-cobat > 0 or
              ror-add-piombo    > 0          
              if ror-imp-consumo > 0  
                 move ror-imp-consumo to como-importo
                 perform SCRIVI-MAGGIORAZIONE
              end-if          
              if ror-imp-cou-cobat > 0 
                 move ror-imp-cou-cobat to como-importo
                 perform SCRIVI-MAGGIORAZIONE
              end-if
              if ror-add-piombo > 0  
                 move ror-add-piombo to como-importo
                 perform SCRIVI-MAGGIORAZIONE
              end-if             
           end-if.
                                
           move como-prz-tot(1:8) to como-numero.
           perform EDIT-NUMERO.

           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<PrezzoTotale>"  
                  como-numero delimited low-value
                  "."
                  como-prz-tot(9:2)
                  "</PrezzoTotale>"
             into line-riga.
           write line-riga.       
      
           initialize line-riga. 
           if tbliv-percentuale = 0
              string 78-spazi    
                     78-spazi
                     78-spazi
                     78-spazi
                     "<AliquotaIVA>00.00</AliquotaIVA>"
                into line-riga
           else                    
              if tbliv-percentuale(2:1) = "0"
                 string 78-spazi    
                        78-spazi
                        78-spazi
                        78-spazi
                        "<AliquotaIVA>"
                        tbliv-percentuale(3:1)
                        "."                   
                        tbliv-percentuale(4:2)
                        "</AliquotaIVA>"
                   into line-riga
              else
                 string 78-spazi             
                        78-spazi
                        78-spazi
                        78-spazi
                        "<AliquotaIVA>"
                        tbliv-percentuale(2:2)
                        "."                   
                        tbliv-percentuale(4:2)
                        "</AliquotaIVA>"
                   into line-riga
              end-if
           end-if.
           write line-riga.      

           if tbliv-percentuale = 0
              inspect tbliv-natura-iva 
                      replacing trailing spaces by low-value
              initialize line-riga
              string 78-spazi    
                     78-spazi
                     78-spazi
                     78-spazi
                     "<Natura>"
                     tbliv-natura-iva delimited low-value
                     "</Natura>"
                     into line-riga
              write line-riga
           end-if.     

           if tno-fattura-from-numero not = 0 and lfel-nc
              move tno-fattura-from-numero to como-numero
              perform EDIT-NUMERO
              initialize line-riga
              string 78-spazi    
                     78-spazi
                     78-spazi
                     78-spazi
                     "<RiferimentoAmministrazione>"     
                     como-numero delimited low-value
                     "</RiferimentoAmministrazione>"
                     into line-riga
              write line-riga
           end-if.

           if des-prog > 0
              perform SCRIVI-ALTRI-DATI-GESTIONALI
           end-if.

           initialize line-riga.       
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "</DettaglioLinee>"
             into line-riga.     
           write line-riga.      

      ***---
       SCRIVI-MAGGIORAZIONE.
           initialize line-riga.
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<ScontoMaggiorazione>"
             into line-riga
           write line-riga
           initialize line-riga
           string 78-spazi
                  78-spazi
                  78-spazi    
                  78-spazi

                  78-spazi
                  "<Tipo>"
                  "MG"
                  "</Tipo>"
             into line-riga
           write line-riga    
           move como-importo(1:4) to como-numero.
           perform EDIT-NUMERO.
           initialize line-riga
           string 78-spazi
                  78-spazi
                  78-spazi    
                  78-spazi
                  78-spazi
                  "<Importo>"      
                  como-numero delimited low-value
                  "."                   
                  como-importo(5:2)
                  "</Importo>"
             into line-riga
           write line-riga   
           initialize line-riga
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "</ScontoMaggiorazione>"
             into line-riga
           write line-riga.

      ***---
       SCRIVI-ALTRI-DATI-GESTIONALI. 
           initialize line-riga.
           string 78-spazi 
                  78-spazi     
                  78-spazi
                  78-spazi
                  "<AltriDatiGestionali>"
             into line-riga.
           write line-riga.

           initialize line-riga.
           string 78-spazi 
                  78-spazi     
                  78-spazi           
                  78-spazi
                  78-spazi
                  "<TipoDato>RSD</TipoDato>"
             into line-riga.
           write line-riga.  
      
           initialize como-des.
           inspect des-ragsoc-1 replacing trailing spaces by low-value.
           inspect des-ragsoc-2 replacing trailing spaces by low-value.
           string  des-ragsoc-1 delimited low-value
                   des-ragsoc-2 delimited low-value
               into como-des
           end-string.
           perform NORMALIZZA-DES.
      
           initialize line-riga.
           string 78-spazi                 delimited size
                  78-spazi                 delimited size
                  78-spazi                 delimited size
                  78-spazi                 delimited size
                  78-spazi                 delimited size
                  "<RiferimentoTesto>"     delimited size
                  como-des(1:60)           delimited low-value
                  "</RiferimentoTesto>"    delimited size
             into line-riga.
           write line-riga. 

           initialize line-riga.
           string 78-spazi 
                  78-spazi     
                  78-spazi
                  78-spazi
                  "</AltriDatiGestionali>"
             into line-riga.
           write line-riga.
           
           initialize ecd-cod-consegna.
           move des-codice to ecd-cli-codice.
           move des-prog   to ecd-prg-destino.
           read edi-clides no lock invalid continue end-read.
           if ecd-cod-consegna not = spaces

              initialize line-riga
              string 78-spazi 
                     78-spazi     
                     78-spazi
                     78-spazi
                     "<AltriDatiGestionali>"
                into line-riga
              write line-riga

              initialize line-riga
              string 78-spazi 
                     78-spazi     
                     78-spazi           
                     78-spazi
                     78-spazi
                     "<TipoDato>DP</TipoDato>"
                into line-riga
              write line-riga
              inspect ecd-cod-consegna 
                      replacing trailing spaces by low-value
              initialize line-riga
              string 78-spazi                 delimited size
                     78-spazi                 delimited size
                     78-spazi                 delimited size
                     78-spazi                 delimited size
                     78-spazi                 delimited size
                     "<RiferimentoTesto>"     delimited size
                     ecd-cod-consegna         delimited low-value
                     "</RiferimentoTesto>"    delimited size
                into line-riga
              write line-riga       

              initialize line-riga
              string 78-spazi 
                     78-spazi     
                     78-spazi
                     78-spazi
                     "</AltriDatiGestionali>"
                into line-riga
              write line-riga
           end-if.

      ***---
       SCRIVI-RIEPILOGO.        
           move 0 to tot-fattura.
           move 0 to tot-iva.           
           perform varying idx from 1 by 1 until idx > 3
              if el-cod-iva(idx) not = spaces
                 initialize line-riga
                 string 78-spazi 
                        78-spazi
                        78-spazi
                        "<DatiRiepilogo>"
                   into line-riga
                 write line-riga

                 move 0 to como-iva
                 compute como-iva = 
                   ( ( el-importo(idx) * el-perce-iva(idx) ) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec 
  
                 move el-perce-iva(idx) to como-prz
                 move el-importo(idx)   to como-prz-tot
                                       
                 compute tot-fattura = tot-fattura  +
                                       como-prz-tot +
                                       como-iva-2dec

                 add como-iva-2dec to tot-iva

                 move como-prz(7:2) to como-numero
                 perform EDIT-NUMERO
                 initialize line-riga
                 if como-prz = 0 
                    string 78-spazi 
                           78-spazi
                           78-spazi
                           78-spazi       
                           "<AliquotaIVA>00.00</AliquotaIVA>"
                      into line-riga
                 else
                    string 78-spazi 
                           78-spazi
                           78-spazi
                           78-spazi       
                           "<AliquotaIVA>"
                           como-numero delimited low-value
                           "."
                           como-prz(9:2)
                           "</AliquotaIVA>"
                      into line-riga
                 end-if
                 write line-riga      
                 if el-perce-iva(idx) = 0
                    inspect el-natura-iva(idx)
                            replacing trailing spaces by low-value
                    initialize line-riga
                    string 78-spazi    
                           78-spazi
                           78-spazi
                           78-spazi
                           "<Natura>"
                           el-natura-iva(idx) delimited low-value
                           "</Natura>"
                           into line-riga
                    write line-riga
                 end-if
                 move como-prz-tot(1:8) to como-numero
                 perform EDIT-NUMERO
                 initialize line-riga     
                 string 78-spazi 
                        78-spazi
                        78-spazi
                        78-spazi       
                        "<ImponibileImporto>"
                        como-numero delimited low-value
                        "."
                        como-prz-tot(9:2)
                        "</ImponibileImporto>"
                   into line-riga     
                 write line-riga      
                 move como-iva-2dec(1:8) to como-numero
                 perform EDIT-NUMERO
                 initialize line-riga
                 if como-iva-2dec = 0         
                    string 78-spazi 
                           78-spazi
                           78-spazi
                           78-spazi       
                           "<Imposta>00.00</Imposta>"
                      into line-riga
                 else          
                    string 78-spazi 
                           78-spazi
                           78-spazi
                           78-spazi       
                           "<Imposta>"
                           como-numero delimited low-value
                           "."
                           como-iva-2dec(9:2)
                           "</Imposta>"
                      into line-riga     
                 end-if
                 write line-riga            
                 initialize line-riga 
                 string 78-spazi 
                        78-spazi
                        78-spazi
                        78-spazi       
                        "<EsigibilitaIVA>"
                        cli-esigibilita-iva
                        "</EsigibilitaIVA>"
                   into line-riga     
                 write line-riga       
                 if el-perce-iva(idx) = 0
                    inspect el-des-iva(idx) 
                            replacing trailing spaces by low-value     
                    initialize line-riga 
                    string 78-spazi 
                           78-spazi
                           78-spazi
                           78-spazi       
                           "<RiferimentoNormativo>"
                           el-des-iva(idx) delimited low-value
                           "</RiferimentoNormativo>"
                      into line-riga     
                    write line-riga       
                 end-if      
                 initialize line-riga
                 string 78-spazi 
                        78-spazi
                        78-spazi
                        "</DatiRiepilogo>"
                   into line-riga
                 write line-riga     
              end-if
           end-perform.
           initialize line-riga.      
           string 78-spazi 
                  78-spazi        
                  "</DatiBeniServizi>"
             into line-riga.
           write line-riga.               

           initialize line-riga. 
           string 78-spazi 
                  78-spazi   
                  "<DatiPagamento>"
             into line-riga.
           write line-riga.

           perform varying idx from 1 by 1 
                     until idx > 36
              if tblpa-codice-tr(idx) = spaces
                 exit perform 
              end-if
           end-perform.

           if idx > 1
               move "TP01" to CodPag
           else
               move "TP02" to CodPag
           end-if.                  
           initialize line-riga.  
           string 78-spazi 
                  78-spazi          
                  78-spazi
                  "<CondizioniPagamento>"
                  CodPag                 
                  "</CondizioniPagamento>"
             into line-riga.
           write line-riga.  
           initialize variabili-varsca replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move cod-pag           to sca-codice-pa.
           move data-doc          to sca-data-fattura.
           move data-doc          to sca-data-conteggio.
           move tot-fattura       to sca-importo-fattura.
           move tot-fattura       to sca-importo-fattura-va.
           move tot-iva           to sca-iva.
           move tot-iva           to sca-iva-va.
           
           move cli-mese1         to sca-mese1.
           move cli-giorno1       to sca-giorno1.
           move cli-mese2         to sca-mese2.
           move cli-giorno2       to sca-giorno2.
      
           move cli-escluso-dal-giorno1 to sca-escluso-dal-giorno1.
           move cli-escluso-dal-giorno2 to sca-escluso-dal-giorno2.
      
           call   "calsca" using variabili-varsca.
           cancel "calsca".   
       
           |12 è il numero massimo delle scadenze STAMPABILI
           perform varying idx from 1 by 1 until idx > 13
              if sca-importo(idx) = 0 exit perform end-if      
              move sca-importo(idx)   to scad-importo             

              initialize line-riga
              string 78-spazi          
                     78-spazi
                     78-spazi
                     "<DettaglioPagamento>"
                into line-riga
              write line-riga
              
              evaluate tblpa-codice-tr(idx)   
              when "Z"
              when "B"   
              when "E"   move "MP05" to CodPag
              when "D"   move "MP01" to CodPag
              when "W"   move "MP12" to CodPag
              when "Z"   move "MP05" to CodPag
              when other move spaces to CodPag
              end-evaluate

              initialize line-riga
              string 78-spazi 
                     78-spazi   
                     78-spazi
                     78-spazi
                     "<ModalitaPagamento>"
                     CodPag               
                     "</ModalitaPagamento>"
                into line-riga
              write line-riga

              if sca-a-vista(idx) = "S"
                 if lfel-f                                        
                    move tor-data-fattura(1:4)   to scad-data(1:4)
                    move "-"                     to scad-data(5:1)
                    move tor-data-fattura(5:2)   to scad-data(6:2)
                    move "-"                     to scad-data(8:1)
                    move tor-data-fattura(7:2)   to scad-data(9:2)
                 else                                             
                    move tno-data-fattura(1:4)   to scad-data(1:4)
                    move "-"                     to scad-data(5:1)
                    move tno-data-fattura(5:2)   to scad-data(6:2)
                    move "-"                     to scad-data(8:1)
                    move tno-data-fattura(7:2)   to scad-data(9:2)
                 end-if
              else                                         
                 move aa of sca-data(idx) to scad-data(1:4)
                 move "-"                 to scad-data(5:1)
                 move mm of sca-data(idx) to scad-data(6:2)
                 move "-"                 to scad-data(8:1)
                 move gg of sca-data(idx) to scad-data(9:2)
              end-if                                        
              initialize line-riga
              string 78-spazi       
                     78-spazi   
                     78-spazi
                     78-spazi
                     "<DataScadenzaPagamento>"
                     scad-data
                     "</DataScadenzaPagamento>"
                into line-riga
              write line-riga          
              move scad-importo(1:8) to como-numero
              perform EDIT-NUMERO  
              initialize line-riga
              string 78-spazi 
                     78-spazi   
                     78-spazi
                     78-spazi
                     "<ImportoPagamento>"
                     como-numero delimited low-value
                     "."
                     scad-importo(9:2)
                     "</ImportoPagamento>"
                into line-riga
              write line-riga 
              if tcl-iban not = spaces
                 inspect tcl-iban replacing trailing spaces by low-value
                 initialize line-riga
                 string 78-spazi 
                        78-spazi   
                        78-spazi
                        78-spazi
                        "<IBAN>"
                        tcl-iban delimited low-value
                        "</IBAN>"
                   into line-riga
                 write line-riga          
              end-if         
              initialize line-riga
              string 78-spazi 
                     78-spazi   
                     78-spazi  
                     "</DettaglioPagamento>"
                into line-riga
              write line-riga

           end-perform.               
           initialize line-riga. 
           string 78-spazi 
                  78-spazi   
                  "</DatiPagamento>"
             into line-riga.
           write line-riga.             
           initialize line-riga. 
           string 78-spazi   
                  "</FatturaElettronicaBody>"
             into line-riga.
           write line-riga.                     


      ********---
      ***** VALORIZZA-GIORNO.
      *****     evaluate tmg-mese
      *****     when 01  move 31 to giorno
      *****     when 02  move link-data(1:4)  to anno
      *****              divide   anno        by 4 giving como-valore
      *****              multiply como-valore by 4 giving como-valore
      *****              if gio-aaaa = como-valore
      *****                 move 29 to giorno
      *****              else
      *****                 move 28 to giorno
      *****              end-if
      *****     when 03  move 31 to giorno
      *****     when 04  move 30 to giorno
      *****     when 05  move 31 to giorno
      *****     when 06  move 30 to giorno
      *****     when 07  move 31 to giorno
      *****     when 08  move 31 to giorno
      *****     when 09  move 30 to giorno
      *****     when 10  move 31 to giorno
      *****     when 11  move 30 to giorno
      *****     when 12  move 31 to giorno

      *****     end-evaluate. 
      
      ***---
       EDIT-NUMERO.       
           inspect como-numero replacing leading x"30" by x"20".
           call "C$JUSTIFY" using como-numero, "L".
           if como-numero = spaces
              move "0" to como-numero                                   
           end-if.                                 
           inspect como-numero replacing trailing spaces by low-value.

      ***---
       OPEN-FILE-XML.
           accept como-data from century-date.
           accept como-ora from time.
           initialize wstampa.
           inspect pfel-path replacing trailing spaces by low-value.
           inspect pfel-CodiceFiscale 
                   replacing trailing spaces by low-value.
           if lfel-f
              move "F" to tipo-doc
           else
              move "N" to tipo-doc
           end-if.
           string  pfel-path          delimited low-value
                   "IT"               delimited size
                   pfel-CodiceFiscale delimited low-value
                   "__"               delimited size
                   prg-invio          delimited size
                   "_"                delimited size
                   tipo-doc           delimited size
                   "-"                delimited size
                   num-doc            delimited size
                   ".xml"
              into wstampa
           end-string.  
           open output lineseq.
           if status-lineseq not = "00"
              move -1 to lfel-status
              display message "Generazione impossibile."
                       x"0d0a""Impossibile creare file xml in"
                       x"0d0a"wstampa
                        title titolo
                         icon 2
              exit paragraph
           end-if.
           if pfel-riga-1 not = spaces
              move pfel-riga-1 to line-riga
              write line-riga
           end-if.
           if pfel-riga-2 not = spaces
              move pfel-riga-2 to line-riga
              write line-riga
           end-if.            
           if pfel-riga-3 not = spaces
              move pfel-riga-3 to line-riga
              write line-riga
           end-if.            
           if pfel-riga-4 not = spaces
              move pfel-riga-4 to line-riga
              write line-riga
           end-if.            
           if pfel-riga-5 not = spaces
              move pfel-riga-5 to line-riga
              write line-riga
           end-if.
           if pfel-riga-6 not = spaces
              move pfel-riga-6 to line-riga
              write line-riga
           end-if.

      ***---
       NORMALIZZA-DES.
           inspect como-des replacing trailing spaces by low-value.
           inspect como-des replacing all "&"   by "e".
           inspect como-des replacing all "€"   by "E".
           inspect como-des replacing all "°"   by " ".
           inspect como-des replacing all "à"   by "a".
           inspect como-des replacing all "è"   by "e".
           inspect como-des replacing all "é"   by "e".
                                                   
           inspect como-des replacing all "/"   by " ".
           inspect como-des replacing all "."   by " ".
           inspect como-des replacing all "@"   by " ".
           inspect como-des replacing all "#"   by " ".
           inspect como-des replacing all "'"   by " ".
           inspect como-des replacing all "\"   by " ".
           inspect como-des replacing all "È"   by " ".
           inspect como-des replacing all "Ò"   by " ".
           inspect como-des replacing all "À"   by " ".
           inspect como-des replacing all "Ù"   by " ".
           inspect como-des replacing all "Ì"   by " ".
           inspect como-des replacing all "%"   by " ".
           inspect como-des replacing all "ø"   by " ". 
           inspect como-des replacing all "<"   by " ".
           inspect como-des replacing all ">"   by " ".
           inspect como-des replacing all x"22" by " ".
           inspect como-des replacing all x"27" by " ".
                                                         
           inspect como-des replacing all x"0d" by x"20".
           inspect como-des replacing all x"0a" by x"20".

      ***---
       NORMALIZZA-NOTE.
           inspect como-note replacing trailing spaces by low-value.
           inspect como-note replacing all "&"   by "e".
           inspect como-note replacing all "€"   by "E".
           inspect como-note replacing all "°"   by " ".
           inspect como-note replacing all "à"   by "a".
           inspect como-note replacing all "è"   by "e".
           inspect como-note replacing all "é"   by "e".
                                                     
           inspect como-note replacing all "/"   by " ".
           inspect como-note replacing all "."   by " ".
           inspect como-note replacing all "@"   by " ".
           inspect como-note replacing all "#"   by " ".
           inspect como-note replacing all "'"   by " ".
           inspect como-note replacing all "\"   by " ".
           inspect como-note replacing all "È"   by " ".
           inspect como-note replacing all "Ò"   by " ".
           inspect como-note replacing all "À"   by " ".
           inspect como-note replacing all "Ù"   by " ".
           inspect como-note replacing all "Ì"   by " ".
           inspect como-note replacing all "%"   by " ".
           inspect como-note replacing all "ø"   by " ".
           inspect como-note replacing all "<"   by " ".
           inspect como-note replacing all ">"   by " ".
           inspect como-note replacing all x"22" by " ".
           inspect como-note replacing all x"27" by " ".
                                                         
           inspect como-note replacing all x"0d" by x"20".
           inspect como-note replacing all x"0a" by x"20".

      ***---
       COUNTER-VIDEO.
           add 1 to counter.
           add 1 to counter2.
           if counter2 = 10
              move counter to counter-edit  
              display counter-edit
                 upon lfel-handle at column 37
                                       line 8
              move 0 to counter2
           end-if.
                              

      ***--
       CLOSE-FILES.
           display "                      "
                 upon lfel-handle at column 37 line 8.
           close      
           tordini
           rordini
           tnotacr
           rnotacr
           lineseq
           articoli
           blister
           paramfel
           clienti
           destini
           tcontat
           tcaumag       
           tvettori
           tivaese
           tcodpag
           CLI
           eordini
           tnazioni
           recapiti
           edi-clides
           ttipocli.

      ***---
       EXIT-PGM.                                                        
           goback.
