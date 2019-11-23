       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      estfel-p.
       AUTHOR.                          Andrea.
       REMARKS. Estrazione e calcolo scritture ausiliarie
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
           copy "tcontat.sl".
           copy "tcaumag.sl".       
           copy "tvettori.sl".
           copy "tivaese.sl".
           copy "tcodpag.sl".

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
           copy "tcontat.fd".
           copy "tcaumag.fd".       
           copy "tvettori.fd".
           copy "tivaese.fd".
           copy "tcodpag.fd".

       WORKING-STORAGE SECTION.      

      * FILE STATUS                       
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-articoli       pic xx.
       77  status-blister        pic xx.
       77  status-paramfel       pic xx.
       77  status-clienti        pic xx.
       77  status-tcontat        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tvettori       pic xx.
       77  status-tivaese        pic xx.
       77  status-tcodpag        pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).

      * COSTANTI
       78  titolo                value "Fatturazione elettronica".
       78  OgniQuanti            value 50.
       78  78-spazi              value "  ".
       78  78-max-doc            value 250.
       
      * VARIABILI 
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
       77  CodPag                pic xxx.
       77  idx                   pic 9(9) value 0.
       77  como-prz              pic 9(7)v999.
       77  como-prz-tot          pic 9(7)v999.
       77  prg-riga              pic 9(4).
       77  num-doc               pic 9(5).

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
           move 0 to counter counter2.
           set tutto-ok        to true.
      *****     set trovato         to false.     

      ***---
       OPEN-FILES.
           open input 
           tordini
           rordini
           tnotacr
           rnotacr
           articoli
           blister
           paramfel
           clienti
           tcontat
           tcaumag       
           tvettori
           tivaese
           tcodpag.
           if errori goback end-if.

      ********---
      ***** OPEN-CREATE-GIORMAG-LOCK.
      *****     initialize geslock-linkage.
      *****     move "giormag" to geslock-nome-file.
      *****
      *****     set tutto-ok  to true.
      *****     set RecLocked to false.
      *****     open output giormag.
      *****     if RecLocked
      *****        move "Operazione già in corso su altro terminale." 
      *****          to geslock-messaggio
      *****        set errori to true
      *****        move 1     to geslock-v-termina
      *****        move 1     to geslock-v-riprova
      *****        move 0     to geslock-v-ignora
      *****        call   "geslock" using geslock-linkage
      *****        cancel "geslock"
      *****        
      *****        evaluate true
      *****        when riprova perform OPEN-CREATE-GIORMAG-LOCK
      *****        when other   display message "Operazione interrotta!"
      *****                               title titolo
      *****                                icon 2
      *****        end-evaluate
      *****     else
      *****        close giormag
      *****        open i-o giormag allowing readers
      *****     end-if.                 

      ***---
       ELABORAZIONE.                          
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                                                   "
              upon lfel-handle at column 01 line 03.
           move spaces to pfel-chiave.
           read paramfel no lock.
           move 0 to num-doc.
           perform OPEN-FILE-XML.
                               
           move lfel-anno to con-anno.
           read tcontat no lock.

           move lfel-anno to tor-anno-fattura.
           move lfel-da   to tor-num-fattura.
           start tordini key >= k-fattura
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-num-fattura > lfel-a
                       exit perform
                    end-if 
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 10
                       move counter to counter-edit
                       display counter-edit
                          upon lfel-handle at column 13
                                                line 03
                       move 0 to counter2
                    end-if
                    set cli-tipo-C to true
                    move tor-cod-cli to cli-codice
                    read clienti no lock
                         invalid initialize cli-rec
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
                    perform SCRIVI-FATTURA
                 end-perform
           end-start.               
           move "</p:FatturaElettronica>" to line-riga.
           write line-riga.

      ***---
       SCRIVI-FATTURA.
           if num-doc > 78-max-doc
              move "</p:FatturaElettronica>" to line-riga
              write line-riga
              close lineseq
              move 0 to num-doc
              perform OPEN-FILE-XML
           end-if.

           add 1 to num-doc.       
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
           string 78-spazi 
                  78-spazi
                  78-spazi 
                  78-spazi
                  "<IdCodice>CodiceLBX</IdCodice>" 
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
           if lfel-f
              string 78-spazi 
                     78-spazi
                     78-spazi
                     "<ProgressivoInvio>"
                     con-ult-num-fel-f
                     "</ProgressivoInvio>" 
                into line-riga
              write line-riga
           else              
              string 78-spazi 
                     78-spazi
                     78-spazi
                     "<ProgressivoInvio>"
                     con-ult-num-fel-nc
                     "</ProgressivoInvio>" 
                into line-riga
              write line-riga
           end-if.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<FormatoTrasmissione>FPR12</FormatoTrasmissione>" 
             into line-riga.
           write line-riga.                       
           initialize line-riga.
           inspect cli-codice-SDI replacing trailing spaces by low-value
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<CodiceDestinatario>"
                  cli-codice-SDI  delimited low-value
                  "</CodiceDestinatario>" 
             into line-riga.
           write line-riga.
           initialize line-riga.
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<PECDestinatario>CLIENTE@pec.it</PECDestinatario>" 
             into line-riga.
           write line-riga.  
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
           inspect pfel-Indirizzo
                   replacing trailing spaces by low-value.
           string 78-spazi   
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<Indirizzo>"
                  pfel-Indirizzo delimited low-value
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
                  pfel-provincia
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
           if cli-codfis not = spaces           
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
           else                 
              if cli-piva not = spaces   
                 inspect cli-piva replacing trailing spaces by low-value
                 initialize line-riga
                 string 78-spazi   
                        78-spazi   
                        78-spazi   
                        78-spazi       
                        "<CodiceFiscale>"
                        cli-piva delimited low-value
                        "</CodiceFiscale>" 
                   into line-riga
                 end-string
                 write line-riga
              end-if
           end-if.
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "<Anagrafica>" 
             into line-riga.
           write line-riga.                        
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.
           inspect cli-ragsoc-2 replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi 
                  78-spazi
                  "<Denominazione>" 
                  cli-ragsoc-1 delimited low-value
                  cli-ragsoc-2 delimited low-value
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
           inspect cli-indirizzo replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Indirizzo>" 
                  cli-indirizzo delimited low-value
                  "</Indirizzo>"
             into line-riga.
           write line-riga.   
           inspect cli-cap replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<CAP>" 
                  cli-cap delimited low-value
                  "</CAP>"
             into line-riga.
           write line-riga.   
           inspect cli-localita replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Comune>" 
                  cli-localita delimited low-value
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
           inspect cli-nazione replacing trailing spaces by low-value.
           initialize line-riga.
           string 78-spazi 
                  78-spazi   
                  78-spazi   
                  78-spazi     
                  "<Nazione>" 
                  cli-nazione delimited low-value
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
           string 78-spazi 
                  78-spazi       
                  78-spazi 
                  78-spazi 
                  "<TipoDocumento>TD01</TipoDocumento>" 
             into line-riga.
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
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Numero>"
                     tor-num-fattura
                     "</Numero>"
                into line-riga
               write line-riga
           else               
              string 78-spazi 
                     78-spazi       
                     78-spazi 
                     78-spazi 
                     "<Numero>"
                     tno-num-fattura
                     "</Numero>"
                into line-riga
               write line-riga
           end-if.
           inspect tca-descrizione 
                   replacing trailing spaces by low-value.
                                   
           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi 
                  78-spazi 
                  "<Causale>"
                  tca-descrizione delimited low-value
                  "</Causale>"
             into line-riga.
           write line-riga            
           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi                  
                  "</DatiGeneraliDocumento>"
             into line-riga.
           write line-riga.           
           initialize line-riga.     
           string 78-spazi 
                  78-spazi       
                  78-spazi                  
                  "<DatiTrasporto>"
             into line-riga.
           write line-riga.           
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
                  "<IdPaese>IT></IdPaese>"
             into line-riga.
           write line-riga.            
           initialize line-riga.    
           inspect vet-piva replacing trailing spaces by low-value.
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi                 
                  78-spazi        
                  "<IdCodice>"
                  vet-piva delimited low-value
                  "</IdCodice>"
             into line-riga.
           write line-riga.            
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
           inspect vet-descrizione 
                   replacing trailing spaces by low-value.          
           initialize line-riga. 
           string 78-spazi 
                  78-spazi       
                  78-spazi       
                  78-spazi                 
                  78-spazi                 
                  78-spazi        
                  "<Denominazione>"
                  vet-descrizione delimited low-value
                  "</Denominazione>"
             into line-riga.
           write line-riga.          
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
           if lfel-f                    
              initialize line-riga
              string 78-spazi 
                     78-spazi       
                     78-spazi       
                     78-spazi                  
                     "<DataOraConsegna>"
                     tor-data-note1(1:4)
                     "-"                 
                     tor-data-note1(5:2)
                     "-"
                     tor-data-note1(7:2)
                     "</DataOraConsegna>"
                into line-riga    
              write line-riga
           end-if.
           initialize line-riga.   
           string 78-spazi 
                  78-spazi       
                  78-spazi                  
                  "</DatiTrasporto>"
             into line-riga.
           write line-riga.            
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
                       perform SCRIVI-RIGA-F
                    end-perform
              end-start
           end-if.   
           perform SCRIVI-RIEPILOGO.     

      ***---      
       SCRIVI-RIGA-F.             
           initialize line-riga.       
           string 78-spazi   
                  78-spazi   
                  78-spazi                  
                  "<DettaglioLinee>"
             into line-riga.
           write line-riga.
           add 1 to prg-riga.
           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<NumeroLinea>"
                  prg-riga
                  "</NumeroLinea>"
             into line-riga.
           write line-riga.           
           inspect art-descrizione 
                   replacing trailing spaces by low-value.
           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<Descrizione>"
                  art-descrizione   delimited low-value
                  "</Descrizione>"
             into line-riga.
           write line-riga.           
           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<Quantita>"
                  ror-qta
                  "</Quantita>"
             into line-riga.
           write line-riga.          
           initialize line-riga. 
           if lfel-f          
              compute como-prz = ror-imp-consumo   + 
                                 ror-imp-cou-cobat +
                                 ror-add-piombo    +
                                 ror-imponib-merce  
              compute como-prz-tot = como-prz * ror-qta
           end-if.     
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<PrezzoUnitario>"
                  como-prz(1:7)
                  "."
                  como-prz(8:3)
                  "</PrezzoUnitario>"
             into line-riga.
           write line-riga.            
           initialize line-riga. 
           string 78-spazi
                  78-spazi
                  78-spazi
                  78-spazi
                  "<PrezzoTotale>"    
                  como-prz-tot(1:7)
                  "."
                  como-prz-tot(8:3)
                  "</PrezzoTotale>"
             into line-riga.
           write line-riga.             
           initialize line-riga. 
           string 78-spazi    
                  78-spazi
                  78-spazi
                  78-spazi
                  "<AliquotaIVA>"
                  tbliv-percentuale(2:2)
                  "."                   
                  tbliv-percentuale(4:2)
                  "</AliquotaIVA>"
             into line-riga.
           write line-riga.             
           initialize line-riga.       
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "</DettaglioLinee>"
             into line-riga.     
           write line-riga.      

      ***---
       SCRIVI-RIEPILOGO.               
           initialize line-riga. 
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "<DatiRiepilogo>"
             into line-riga.     
           write line-riga.           
           initialize line-riga.      
           string 78-spazi 
                  78-spazi
                  78-spazi
                  78-spazi       
                  "<AliquotaIva>"
                  "</AliquotaIva>"
             into line-riga.     
           write line-riga.            
           initialize line-riga.     
           string 78-spazi 
                  78-spazi
                  78-spazi
                  78-spazi       
                  "<ImponibileImporto>"
                  "</ImponibileImporto>"
             into line-riga.     
           write line-riga.            
           initialize line-riga.          
           string 78-spazi 
                  78-spazi
                  78-spazi
                  78-spazi       
                  "<Imposta>"
                  "</Imposta>"
             into line-riga.     
           write line-riga.            
           initialize line-riga. 
           string 78-spazi 
                  78-spazi
                  78-spazi
                  78-spazi       
                  "<EsigibilitaIva>"
                  "</EsigibilitaIva>"
             into line-riga.     
           write line-riga.           
           initialize line-riga.           
           string 78-spazi 
                  78-spazi
                  78-spazi
                  "</DatiRiepilogo>"
             into line-riga.     
           write line-riga.           
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
           initialize line-riga.      
           string 78-spazi          
                  78-spazi
                  78-spazi
                  "<DettaglioPagamento>"
             into line-riga.
           write line-riga.    

           
      *****     initialize variabili-varsca replacing numeric data by zeroes
      *****                                      alphanumeric data by spaces.
      *****     move tor-cod-pagamento to sca-codice-pa.
      *****     move tor-data-fattura  to sca-data-fattura.
      *****     move tor-data-fattura  to sca-data-conteggio.
      *****     move tot-fattura       to sca-importo-fattura.
      *****     move tot-fattura       to sca-importo-fattura-va.
      *****     move tot-iva           to sca-iva.
      *****     move tot-iva           to sca-iva-va.
      *****     
      *****     move cli-mese1         to sca-mese1.
      *****     move cli-giorno1       to sca-giorno1.
      *****     move cli-mese2         to sca-mese2.
      *****     move cli-giorno2       to sca-giorno2.
      *****
      *****     move cli-escluso-dal-giorno1 to sca-escluso-dal-giorno1.
      *****     move cli-escluso-dal-giorno2 to sca-escluso-dal-giorno2.
      *****
      *****     call   "calsca" using variabili-varsca.
      *****     cancel "calsca".
      *****
      *****

           evaluate tblpa-codice-tr(1)   
           when "B" move "MP05" to CodPag
           when "D" move "MP01" to CodPag
           when "W" move "MP12" to CodPag
           when other move spaces to CodPag
           end-evaluate.             
           initialize line-riga. 
           string 78-spazi 
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<ModalitaPagamento>"
                  CodPag               
                  "</ModalitaPagamento>"
             into line-riga.
           write line-riga.           
           initialize line-riga.                 
           string 78-spazi       
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<DataScadenzaPagamento>"
                  "</DataScadenzaPagamento>"
             into line-riga.
           write line-riga.           
           initialize line-riga. 
           string 78-spazi 
                  78-spazi   
                  78-spazi
                  78-spazi
                  "<ImportoPagamento>"
                  "</ImportoPagamento>"
             into line-riga.
           write line-riga.            
           initialize line-riga.  
           string 78-spazi 
                  78-spazi   
                  78-spazi  
                  "</DettaglioPagamento>"
             into line-riga.
           write line-riga.           
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
       OPEN-FILE-XML.
           accept como-data from century-date.
           accept como-ora from time.
           initialize wstampa.
           inspect pfel-path replacing trailing spaces by low-value.
           inspect pfel-CodiceFiscale 
                   replacing trailing spaces by low-value.
           string  pfel-path delimited low-value
                   "IT"     delimited size
                   pfel-CodiceFiscale delimited low-value
                   "_"       delimited size
                   como-data delimited size
                   "_"       delimited size
                   como-ora  delimited size
                   ".xml"
              into wstampa
           end-string.  
           open output lineseq.
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
                              

      ***--
       CLOSE-FILES.
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
           tcontat
           tcaumag       
           tvettori
           tivaese
           tcodpag.

      ***---
       EXIT-PGM.
           goback.
