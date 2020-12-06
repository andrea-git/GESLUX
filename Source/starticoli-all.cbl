       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      starticoli-all.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".        
           copy "articoli.sl".       
           copy "tmarche.sl".        
           copy "tsetmerc.sl".
           copy "tivaese.sl".
           copy "tmagaz.sl".
           copy "tscorte.sl".
           copy "tcla1art.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "tnomen.sl".
           copy "prodener.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
       FD  lineseq.
       01 line-riga        PIC  x(9000).

           copy "articoli.fd".       
           copy "tmarche.fd".        
           copy "tsetmerc.fd".
           copy "tivaese.fd".
           copy "tmagaz.fd".
           copy "tscorte.fd".
           copy "tcla1art.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "tnomen.fd".
           copy "prodener.fd".

       WORKING-STORAGE SECTION.
       copy "acugui.def".
       copy "common-excel.def".

      * COSTANTI
       78  titolo value "Stampa Articoli (TUTTI)".

      * FILE STATUS         
       77 status-lineseq        pic xx.
       77 status-articoli       pic xx.
       77 status-tmarche        pic xx.
       77 status-tsetmerc       pic xx.
       77 status-tivaese        pic xx.
       77 status-tmagaz         pic xx.
       77 status-tscorte        pic xx.
       77 status-tcla1art       pic xx.
       77 status-timballi       pic xx. 
       77 status-timbalqta      pic xx.
       77 status-tnomen         pic xx.
       77 status-prodener       pic xx.

       77  wstampa                    pic x(256).

      * VARIABILI
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  user-codi                  pic x(10).

       77  r-art-peso-utf              PIC zz9,999.
       77  r-art-peso-non-utf          PIC zz9,999.
       77  r-art-peso-standard         PIC z.zz9,999.
       77  r-art-prezzo-vendita        PIC zzz.zzz.zz9,99.
       77  r-art-perce-sconto-agente   PIC z9,99.
       77  r-art-prezzo-acquisto       PIC zzz.zzz.zz9,99.
       77  r-art-perce-sconto-acquisto PIC z9,99.
       77  r-art-perce-imposte         PIC zz9,999.
       77  r-art-perce-cou             PIC zz9,999.
       77  r-art-prezzo-banco          PIC zzz.zz9,99.
       77  r-art-prz-min-vend          PIC zz.zz9,99.
       77  r-art-peso-reale            PIC zz9,999.   
       77  r-art-altezza               PIC zzz.zz9,99.
       77  r-art-larghezza             PIC zzz.zz9,99.
       77  r-art-profondita            PIC zzz.zz9,99.
       77  r-art-altezza-pz            PIC zzz.zz9,99.
       77  r-art-larghezza-pz          PIC zzz.zz9,99.
       77  r-art-profondita-pz         PIC zzz.zz9,99.
       77  r-art-peso-SHI              PIC zzz.zz9,999.
       77  r-art-peso-GET              PIC zzz.zz9,999.    
       77  prod-ener-completo          pic x(30).

      * FLAGS               
       01  controllo                  pic xx.
         88 errori                    value "ER".
         88 tutto-ok                  value "OK".
       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.

      ******************************************************************
       LINKAGE SECTION.
       77  link-user                 pic x(20).

       PROCEDURE DIVISION USING link-user.

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
      *-
           move link-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.       
           set tutto-ok       to true.
           set prima-volta    to true.
           accept  wstampa      from environment "PATH-ST".
           accept  como-data    from century-date.
           accept  como-ora     from time.
           inspect wstampa      replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           string  wstampa                   delimited low-value
                   "LISTA_COMPLETA_ARTICOLI" delimited size
                   "_"                       delimited size
                   user-codi                 delimited low-value
                   "_"                       delimited size
                   como-data                 delimited size
                   "_"                       delimited size
                   como-ora                  delimited size
                   ".csv"                    delimited size
                   into wstampa
           end-string    
           set prima-volta to true.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input articoli tsetmerc tmarche tivaese tmagaz 
                         tscorte
                         tcla1art timballi timbalqta tnomen prodener
           end-if.
           if errori goback end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.                                     

      ***---
       ELABORAZIONE.
           move  low-value to art-codice.
           start articoli key >= art-chiave
                 invalid  set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read articoli next at end exit perform end-read
                 perform GENERA-FILE-EXCEL
              end-perform
              perform CALL-EXCEL  
           end-if.                

      ***---
       GENERA-FILE-EXCEL.
           set tutto-ok    to true.
           if prima-volta
              perform ACCETTA-SEPARATORE
              perform SCRIVI-INTESTAZIONE
              set prima-volta to false
           end-if.

           move art-settore-merceologico to sme-codice.
           read tsetmerc no lock invalid continue end-read.
           
           move art-marca-prodotto to mar-codice.
           read tmarche no lock invalid continue end-read.

           move "IV"           to tbliv-codice1.
           move art-codice-iva to tbliv-codice2.
           read tivaese no lock invalid continue end-read.

           move art-mag-std to mag-codice.
           read tmagaz   no lock invalid continue end-read.

           move art-scorta to sco-codice.
           read tscorte   no lock invalid continue end-read.
                      
           move art-classe-1 to cl1-codice.
           read tcla1art no lock invalid continue end-read.
                                         
           move art-imballo-standard to imq-codice.
           read timbalqta no lock invalid continue end-read.
           move imq-tipo  to imb-codice.
           read timballi  no lock invalid continue end-read.       

           move art-cod-doganale to nom-codice.
           read tnomen no lock invalid continue end-read.
                                                       
           initialize prod-ener-completo.
           move art-cod-prodener to pen-codice.
           read prodener no lock 
                invalid continue
            not invalid
                string "CPA: "      delimited size
                       pen-cpa      delimited size
                       " - NC: "    delimited size
                       pen-nc       delimited size
                       " - TARIC: " delimited size
                       pen-taric    delimited size
                       " - DAC:"    delimited size
                       pen-dac      delimited size
                       into prod-ener-completo
                end-string
           end-read.

           perform SISTEMA-NUMERICI.

           initialize line-riga.
           string art-codice       
                  separatore
                  art-descrizione               
                  separatore
                  art-descrizione-2 
                  separatore
                  art-settore-merceologico       
                  separatore
                  sme-descrizione
                  separatore
                  art-marca-prodotto             
                  separatore
                  mar-descrizione
                  separatore
                  art-classe-1                   
                  separatore
                  cl1-descrizione
                  separatore
                  art-classe-2                   
                  separatore
                  art-classe-3                   
                  separatore
                  art-classe-4                   
                  separatore
                  art-unita-di-misura            
                  separatore       
                  art-gestione-utf               
                  separatore
                  r-art-peso-utf                   
                  separatore
                  r-art-peso-non-utf               
                  separatore
                  r-art-peso-standard              
                  separatore
                  art-imballo-standard           
                  separatore
                  imb-descrizione
                  separatore
                  art-udm-imballo                
                  separatore
                  art-codice-iva                 
                  separatore
                  tbliv-descrizione1
                  separatore
                  r-art-prezzo-vendita             
                  separatore
                  r-art-perce-sconto-agente        
                  separatore
                  r-art-prezzo-acquisto            
                  separatore
                  r-art-perce-sconto-acquisto       
                  separatore
                  art-cod-doganale                
                  separatore
                  nom-descrizione
                  separatore
                  art-soggetto-imposte            
                  separatore
                  r-art-perce-imposte               
                  separatore
                  r-art-perce-cou                   
                  separatore
                  art-soggetto-cobat              
                  separatore
                  art-amperaggio                  
                  separatore
                  art-auto-moto-per-cobat         
                  separatore
                  art-note                        
                  separatore
                  art-codice-ean-1                
                  separatore
                  art-codice-ean-2                
                  separatore
                  art-codice-ean-3                
                  separatore
                  art-codice-ean-4                
                  separatore
                  art-codice-ean-5                
                  separatore
                  art-foto                        
                  separatore
                  art-note-agg                    
                  separatore
                  art-scheda-tecnica              
                  separatore
                  art-tossicologica               
                  separatore
                  art-qta-epal                    
                  separatore
                  art-qta-std                     
                  separatore
                  r-art-altezza                     
                  separatore
                  r-art-larghezza                   
                  separatore
                  r-art-profondita                  
                  separatore
                  art-scorta                      
                  separatore
                  sco-descrizione
                  separatore
                  art-moq
                  separatore
                  r-art-prezzo-banco                
                  separatore
                  r-art-prz-min-vend                
                  separatore
                  r-art-peso-reale                  
                  separatore
                  art-cod-art-frn                 
                  separatore
                  art-mag-std                     
                  separatore
                  mag-descrizione
                  separatore
                  art-collegato                   
                  separatore   
                  art-cod-prodener                
                  separatore
                  prod-ener-completo
                  separatore
                  pen-descrizione
                  separatore
                  art-tipo-stoc                   
                  separatore
                  art-logo-brand                  
                  separatore
                  art-conf-cartone                
                  separatore
                  art-cartone-UDC                 
                  separatore
                  r-art-altezza-pz                  
                  separatore
                  r-art-larghezza-pz                
                  separatore
                  r-art-profondita-pz               
                  separatore
                  art-stato                       
                  separatore
                  separatore
                  art-gruppi                      
                  separatore
                  art-gda                         
                  separatore
                  art-agenti                      
                  separatore
                  art-specialist                  
                  separatore
                  art-estero                      
                  separatore
                  art-gds                         
                  separatore
                  art-do                          
                  separatore
                  art-web                         
                  separatore
                  art-AT                          
                  separatore
                  art-SPI                         
                  separatore
                  art-T1                          
                  separatore
                  art-T2                          
                  separatore
                  art-T3                          
                  separatore
                  art-adr                         
                  separatore
                  art-data-creazione              
                  separatore
                  art-ora-creazione               
                  separatore
                  art-utente-creazione            
                  separatore
                  art-data-ultima-modifica        
                  separatore
                  art-ora-ultima-modifica         
                  separatore
                  art-utente-ultima-modifica       
                  separatore
                  r-art-peso-SHI                     
                  separatore
                  r-art-peso-GET                     
                  separatore
                  into line-riga
           end-string.
           write line-riga.

      ***---
       SISTEMA-NUMERICI.
           move art-peso-utf              to r-art-peso-utf.
           move art-peso-non-utf          to r-art-peso-non-utf.
           move art-peso-standard         to r-art-peso-standard.
           move art-prezzo-vendita        to r-art-prezzo-vendita.
           move art-perce-sconto-agente   to r-art-perce-sconto-agente.
           move art-prezzo-acquisto       to r-art-prezzo-acquisto.
           move art-perce-sconto-acquisto to r-art-perce-sconto-acquisto
           move art-perce-imposte         to r-art-perce-imposte.
           move art-perce-cou             to r-art-perce-cou.
           move art-prezzo-banco          to r-art-prezzo-banco.
           move art-prz-min-vend          to r-art-prz-min-vend.
           move art-peso-reale            to r-art-peso-reale.   
           move art-altezza               to r-art-altezza.
           move art-larghezza             to r-art-larghezza.
           move art-profondita            to r-art-profondita.
           move art-altezza-pz            to r-art-altezza-pz.
           move art-larghezza-pz          to r-art-larghezza-pz.
           move art-profondita-pz         to r-art-profondita-pz.
           move art-peso-SHI              to r-art-peso-SHI.
           move art-peso-GET              to r-art-peso-GET.


      ***---
       SCRIVI-INTESTAZIONE.
           initialize line-riga.
           string separatore                   delimited size
                  "** "                        delimited size
                  "LUBEX S.p.A."               delimited size
                  " - "                        delimited size
                  "Lista completa articoli "   delimited size
                  " **"                        delimited size
                  into line-riga
           end-string.
           write line-riga.

           write line-riga from spaces.

           initialize line-riga.
           string "Codice"
                  separatore
                  "Descrizione"
                  separatore     
                  "Descrizione 2"
                  separatore
                  "Sett. Merc."
                  separatore
                  "Descrizione"
                  separatore
                  "Marca"
                  separatore
                  "Descrizione"
                  separatore
                  "Classe 1"
                  separatore
                  "Descrizione"
                  separatore
                  "Classe 2"
                  separatore
                  "Classe 3"
                  separatore
                  "Classe 4"
                  separatore
                  "UDM"
                  separatore       
                  "UTF"
                  separatore
                  "Peso UTF"
                  separatore
                  "Peso NON UTF"
                  separatore
                  "Peso STD"
                  separatore
                  "Imballo"
                  separatore
                  "Descrizione"
                  separatore
                  "UDM imballo"
                  separatore
                  "Cod. IVA"
                  separatore
                  "Descrizione"
                  separatore
                  "Prz. Vendita"
                  separatore
                  "% Sc. Agente"
                  separatore
                  "Prz. Acquisto"
                  separatore
                  "% Sc. Acquisto"
                  separatore
                  "Cod. Doganale"
                  separatore
                  "Descrizione"
                  separatore
                  "Sogg. Imposte"
                  separatore
                  "% Imposte"
                  separatore
                  "% COU"
                  separatore
                  "% COBAT"
                  separatore
                  "Amperaggio"
                  separatore
                  "COBAT A/M"
                  separatore
                  "Note"
                  separatore
                  "EAN 1"
                  separatore                     
                  "EAN 2" 
                  separatore                     
                  "EAN 3" 
                  separatore                     
                  "EAN 4" 
                  separatore                     
                  "EAN 5" 
                  separatore
                  "Foto"
                  separatore
                  "Note Aggiuntive"
                  separatore       
                  "Scheda Tecnica"
                  separatore
                  "Scheda Tossicologica"
                  separatore     
                  "Qta EPAL"
                  separatore
                  "Qta STD"
                  separatore
                  "Altezza"
                  separatore
                  "Larghezza"
                  separatore
                  "Profondità"
                  separatore
                  "Scorta"
                  separatore
                  "Descrizione"
                  separatore
                  "Limite Scorta"
                  separatore
                  "Prz. Banco"
                  separatore
                  "Prz. Min. Vend."
                  separatore
                  "Peso Reale"
                  separatore
                  "Cod Art Frn"
                  separatore
                  "Mag STD"
                  separatore
                  "Descrizione"
                  separatore
                  "Collegato"
                  separatore   
                  "Prod. Ener."
                  separatore
                  separatore
                  "Descrizione"
                  separatore
                  "Tipo Stoc"
                  separatore
                  "Logo Brand"
                  separatore
                  "Conf cartone"
                  separatore
                  "Cartone UDC"
                  separatore
                  "Altezza pz"
                  separatore
                  "Larghezza pz"
                  separatore
                  "Profondità pz"
                  separatore
                  "Stato"
                  separatore
                  "Diretti"
                  separatore
                  "Gruppi"
                  separatore
                  "GDA"
                  separatore
                  "Agenti"
                  separatore
                  "Specialist"
                  separatore
                  "Estero"
                  separatore
                  "GDS"
                  separatore
                  "DO"
                  separatore
                  "WEB"
                  separatore
                  "AT"
                  separatore
                  "SPI"
                  separatore
                  "T1"
                  separatore
                  "T2"
                  separatore
                  "T3"
                  separatore
                  "ADR"
                  separatore
                  "Data Creazione"
                  separatore
                  "Ora Creazione"
                  separatore
                  "Utente Creazione"
                  separatore    
                  "Data Modifica"
                  separatore
                  "Ora Modifica"
                  separatore
                  "Utente Modifica"
                  separatore
                  "Peso SHI"
                  separatore
                  "Peso GET"
                  separatore
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           close articoli tsetmerc tmarche tivaese tmagaz tscorte
                 tcla1art timballi timbalqta tnomen prodener lineseq.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".                 
