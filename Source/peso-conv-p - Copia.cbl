       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      peso-conv-p.
       AUTHOR.                          Andrea.

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".
           copy "rordini.sl".
           copy "mrordini.sl".
           copy "rnotacr.sl".
           copy "rordforn.sl".
           copy "listini.sl".
           copy "rmovmag.sl".
           copy "progmagric.sl".
           copy "distinteb.sl".
           copy "provvig.sl".
           copy "brnotacr.sl".
           copy "ordfor2.sl".
           copy "reva.sl".
           copy "art-ordforn.sl".
           copy "cli-prg.sl".   
           copy "STO-rordini.sl".
           copy "STO-mrordini.sl".     
           copy "STO-rmovmag.sl".
           copy "STO-rnotacr.sl".
           copy "STO-rordforn.sl".
           copy "STO-provvig.sl".
           copy "STO-reva.sl".
           copy "STO-brnotacr.sl". 
           
           copy "OLD-articoli.sl".
           copy "OLD-progmag.sl".
           copy "OLD-rordini.sl".
           copy "OLD-mrordini.sl".
           copy "OLD-rnotacr.sl".
           copy "OLD-rordforn.sl".
           copy "OLD-listini.sl".
           copy "OLD-rmovmag.sl".
           copy "OLD-progmagric.sl".
           copy "OLD-distinteb.sl".
           copy "OLD-provvig.sl".
           copy "OLD-brnotacr.sl".
           copy "OLD-ordfor2.sl".
           copy "OLD-reva.sl".
           copy "OLD-art-ordforn.sl".
           copy "OLD-cli-prg.sl".   
           copy "OLD-STO-rordini.sl".
           copy "OLD-STO-mrordini.sl".     
           copy "OLD-STO-rmovmag.sl".
           copy "OLD-STO-rnotacr.sl".
           copy "OLD-STO-rordforn.sl".
           copy "OLD-STO-provvig.sl".
           copy "OLD-STO-reva.sl".
           copy "OLD-STO-brnotacr.sl". 


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd". 
           copy "progmag.fd".  
           copy "rordini.fd".
           copy "mrordini.fd".
           copy "rnotacr.fd".
           copy "rordforn.fd".
           copy "listini.fd".
           copy "rmovmag.fd".
           copy "progmagric.fd".
           copy "distinteb.fd".
           copy "provvig.fd".
           copy "brnotacr.fd".
           copy "ordfor2.fd".
           copy "reva.fd".
           copy "art-ordforn.fd".
           copy "cli-prg.fd".   
           copy "STO-rordini.fd".
           copy "STO-mrordini.fd".     
           copy "STO-rmovmag.fd". 
           copy "STO-rnotacr.fd".
           copy "STO-rordforn.fd".
           copy "STO-provvig.fd".
           copy "STO-reva.fd".
           copy "STO-brnotacr.fd". 
           
           copy "OLD-articoli.fd".
           copy "OLD-progmag.fd".   
           copy "OLD-rordini.fd".
           copy "OLD-mrordini.fd".
           copy "OLD-rnotacr.fd".
           copy "OLD-rordforn.fd".
           copy "OLD-listini.fd".
           copy "OLD-rmovmag.fd".
           copy "OLD-progmagric.fd".
           copy "OLD-distinteb.fd".
           copy "OLD-provvig.fd".
           copy "OLD-brnotacr.fd".
           copy "OLD-ordfor2.fd".
           copy "OLD-reva.fd".
           copy "OLD-art-ordforn.fd".
           copy "OLD-cli-prg.fd".   
           copy "OLD-STO-rordini.fd".
           copy "OLD-STO-mrordini.fd".     
           copy "OLD-STO-rmovmag.fd". 
           copy "OLD-STO-rnotacr.fd".
           copy "OLD-STO-rordforn.fd".
           copy "OLD-STO-provvig.fd".
           copy "OLD-STO-reva.fd".
           copy "OLD-STO-brnotacr.fd".

       WORKING-STORAGE SECTION.    
       77  status-articoli          pic xx.
       77  status-progmag           pic xx.
       77  status-rordini           pic xx.
       77  status-mrordini          pic xx.
       77  status-rnotacr           pic xx.
       77  status-rordforn          pic xx.
       77  status-listini           pic xx.
       77  status-rmovmag           pic xx.
       77  status-progmagric        pic xx.
       77  status-distinteb         pic xx.
       77  status-provvig           pic xx.
       77  status-brnotacr          pic xx.
       77  status-ordfor2           pic xx.
       77  status-reva              pic xx.
       77  status-art-ordforn       pic xx.
       77  status-cli-prg           pic xx.
       77  status-STO-rordini       pic xx.
       77  status-STO-mrordini      pic xx.
       77  status-STO-tmovmag       pic xx.
       77  status-STO-rmovmag       pic xx.
       77  status-STO-rnotacr       pic xx.
       77  status-STO-rordforn      pic xx.
       77  status-STO-provvig       pic xx.
       77  status-STO-reva          pic xx.
       77  status-STO-brnotacr      pic xx.
       
       77  status-OLD-articoli          pic xx.
       77  status-OLD-progmag           pic xx.
       77  status-OLD-rordini           pic xx.
       77  status-OLD-mrordini          pic xx.
       77  status-OLD-rnotacr           pic xx.
       77  status-OLD-rordforn          pic xx.
       77  status-OLD-listini           pic xx.
       77  status-OLD-rmovmag           pic xx.
       77  status-OLD-progmagric        pic xx.
       77  status-OLD-distinteb         pic xx.
       77  status-OLD-provvig           pic xx.
       77  status-OLD-brnotacr          pic xx.
       77  status-OLD-ordfor2           pic xx.
       77  status-OLD-reva              pic xx.
       77  status-OLD-art-ordforn       pic xx.
       77  status-OLD-cli-prg           pic xx.
       77  status-OLD-STO-rordini       pic xx.
       77  status-OLD-STO-mrordini      pic xx.
       77  status-OLD-STO-tmovmag       pic xx.
       77  status-OLD-STO-rmovmag       pic xx.
       77  status-OLD-STO-rnotacr       pic xx.
       77  status-OLD-STO-rordforn      pic xx.
       77  status-OLD-STO-provvig       pic xx.
       77  status-OLD-STO-reva          pic xx.
       77  status-OLD-STO-brnotacr      pic xx.
                                                  
       77  path-archivi                 pic x(50).
       77  path-archivi-sto             pic x(50).

       77  path-source                  pic x(50).
       77  path-dest                    pic x(50).
                                                  
       77  line-file                    pic 99v99.
       77  nome-file                    pic x(15).
       77  nome-file-upper              pic x(15).
       77  I                            pic 999.

       77  counter             pic 9(12).
       77  counter2            pic 9(12).
       77  counter-edit        pic z(12).

       78  interlinea          value 2.
       78  titolo    value "GESLUX - Aumento peso".

       LINKAGE SECTION.                                         
       77 peso-conv-handle USAGE IS HANDLE OF WINDOW.


      **********************************************************
       PROCEDURE DIVISION USING peso-conv-handle.
      ***---
       MAIN.                                                   
           accept path-archivi      from environment "PATH_ARCHIVI".
           accept path-archivi-sto  from environment "PATH_ARCHIVI_STO".
                                                       
           inspect path-archivi     replacing trailing 
                                    spaces by low-value.
           inspect path-archivi-sto replacing trailing 
                                    spaces by low-value.

           perform ELABORA-ARCHIVI.

           goback.

      ***---
       ELABORA-ARCHIVI.        
           move 6               to line-file.

           move "articoli" to nome-file.
           perform CREA-RENAME.

           open input old-articoli.
      *    open output    articoli.

           move low-value to old-art-rec.
           start old-articoli key >= old-art-chiave.
           perform until 1 = 2         
              perform CONTATORE-SCREEN 
              exit perform
              read old-articoli next at end exit perform end-read
              move OLD-art-codice                 to art-codice                
              move OLD-art-descrizione            to art-descrizione           
              move OLD-art-settore-merceologico   
                to art-settore-merceologico  
              move OLD-art-marca-prodotto         to art-marca-prodotto        
              move OLD-art-classe-1               to art-classe-1              
              move OLD-art-classe-2               to art-classe-2              
              move OLD-art-classe-3               to art-classe-3              
              move OLD-art-classe-4               to art-classe-4              
              move OLD-art-unita-di-misura        to art-unita-di-misura       
              move OLD-art-cod-fornitore          to art-cod-fornitore         
              move OLD-art-gestione-utf           to art-gestione-utf          
              move OLD-art-peso-utf               to art-peso-utf              
              move OLD-art-peso-non-utf           to art-peso-non-utf          
              move OLD-art-peso-standard          to art-peso-standard         
              move OLD-art-imballo-standard       
                to art-imballo-standard      
              move OLD-art-udm-imballo            to art-udm-imballo           
              move OLD-art-codice-iva             to art-codice-iva            
              move OLD-art-prezzo-vendita         to art-prezzo-vendita        
              move OLD-art-perce-sconto-agente    
                to art-perce-sconto-agente   
              move OLD-art-prezzo-acquisto        to art-prezzo-acquisto       
              move OLD-art-perce-sconto-acquisto 
                to art-perce-sconto-acquisto 
              move OLD-art-cod-doganale           to art-cod-doganale          
              move OLD-art-soggetto-imposte      to art-soggetto-imposte      
              move OLD-art-perce-imposte          to art-perce-imposte         
              move OLD-art-perce-cou              to art-perce-cou             
              move OLD-art-soggetto-cobat         to art-soggetto-cobat        
              move OLD-art-amperaggio             to art-amperaggio            
              move OLD-art-auto-moto-per-cobat
                to art-auto-moto-per-cobat   
              move OLD-art-note                   to art-note                  
              move OLD-art-codice-ean-1           to art-codice-ean-1          
              move OLD-art-codice-ean-2           to art-codice-ean-2          
              move OLD-art-codice-ean-3           to art-codice-ean-3          
              move OLD-art-codice-ean-4           to art-codice-ean-4          
              move OLD-art-codice-ean-5           to art-codice-ean-5          
              move OLD-art-foto                   to art-foto                  
              move OLD-art-note-agg               to art-note-agg              
              move OLD-art-scheda-tecnica         to art-scheda-tecnica        
              move OLD-art-tossicologica          to art-tossicologica         
              move OLD-art-descrizione-2          to art-descrizione-2         
              move OLD-art-qta-epal               to art-qta-epal              
              move OLD-art-qta-std                to art-qta-std               
              move OLD-art-altezza                to art-altezza               
              move OLD-art-larghezza              to art-larghezza             
              move OLD-art-profondita             to art-profondita            
              move OLD-art-stato                  to art-stato                 
              move OLD-art-diretti                to art-diretti               
              move OLD-art-gruppi                 to art-gruppi                
              move OLD-art-gda                    to art-gda                   
              move OLD-art-agenti                 to art-agenti                
              move OLD-art-specialist             to art-specialist            
              move OLD-art-estero                 to art-estero                
              move OLD-art-gds                    to art-gds                   
              move OLD-art-scorta                 to art-scorta                
              move OLD-art-prezzo-banco           to art-prezzo-banco          
              move OLD-art-prz-min-vend           to art-prz-min-vend          
              move OLD-art-limite-scorta          to art-limite-scorta         
              move OLD-art-peso-reale             to art-peso-reale            
              move OLD-art-do                     to art-do                    
              move OLD-art-cod-art-frn            to art-cod-art-frn           
              move OLD-art-mag-std                to art-mag-std               
              move OLD-art-collegato              to art-collegato             
              move OLD-art-cod-desf-forn          to art-cod-desf-forn         
              move OLD-art-cod-prodener           to art-cod-prodener          
              move OLD-art-tipo-stoc              to art-tipo-stoc             
              move OLD-art-logo-brand             to art-logo-brand            
              move OLD-art-conf-cartone           to art-conf-cartone          
              move OLD-art-cartone-UDC            to art-cartone-UDC           
              move OLD-art-altezza-pz             to art-altezza-pz            
              move OLD-art-larghezza-pz           to art-larghezza-pz          
              move OLD-art-profondita-pz          to art-profondita-pz         
              move OLD-art-web                    to art-web                   
              move OLD-art-adr                    to art-adr                   
              move OLD-art-data-creazione         to art-data-creazione        
              move OLD-art-ora-creazione          to art-ora-creazione         
              move OLD-art-utente-creazione       
                to art-utente-creazione      
              move OLD-art-data-ultima-modifica   
                to art-data-ultima-modifica  
              move OLD-art-ora-ultima-modifica    
                to art-ora-ultima-modifica   
              move OLD-art-utente-ultima-modifica 
                to art-utente-ultima-modifica
              move OLD-art-peso-SHI               to art-peso-SHI              
              move OLD-art-SPI                    to art-SPI                   
              move OLD-art-T1                     to art-T1                    
              move OLD-art-T2                     to art-T2                    
              move OLD-art-T3                     to art-T3                    
              move OLD-art-peso-GET               to art-peso-GET              
              move OLD-art-AT                     to art-AT                    
              move OLD-art-TEXACO                 to art-TEXACO                
              move OLD-art-num-vuoto-2            to art-num-vuoto-2           
              move OLD-art-num-vuoto-3            to art-num-vuoto-3           
              move OLD-art-alfa-vuoto             to art-alfa-vuoto  
              write art-rec
           end-perform.
                   
           close       old-articoli.
      *    delete file old-articoli.
      *     close articoli.



           move "progmag" to nome-file.
           perform CREA-RENAME.

           open input OLD-progmag.
      *    open output    progmag.

           move low-value to OLD-prg-rec.
           start old-progmag key >= old-prg-chiave.
           perform until 1 = 2      
              perform CONTATORE-SCREEN
              exit perform
              read old-progmag next at end exit perform end-read
              move OLD-prg-cod-articolo        to prg-cod-articolo       
              move OLD-prg-cod-magazzino       to prg-cod-magazzino      
              move OLD-prg-tipo-imballo        to prg-tipo-imballo       
              move OLD-prg-peso                to prg-peso               
              move OLD-prg-peso-utf            to prg-peso-utf           
              move OLD-prg-peso-non-utf        to prg-peso-non-utf       
                                                                       
              move OLD-prg-sezione-dinamici    
                to prg-sezione-dinamici   
              move OLD-prg-sezione-consolidati 
                to prg-sezione-consolidati
              move OLD-prg-giac-day            to prg-giac-day           
              move OLD-prg-stato               to prg-stato              
              move OLD-prg-dati-comuni         to prg-dati-comuni        
              move OLD-prg-vuoti               to prg-vuoti              
              write prg-rec

           end-perform.
           
           close       old-progmag.
      *    delete file old-progmag.
      *     close progmag.



           move "rordini" to nome-file.
           perform CREA-RENAME.

           open input OLD-rordini.
      *    open output    rordini.

           move low-value to old-ror-rec.
           start old-rordini key >= old-ror-chiave.
           perform until 1 = 2    
              perform CONTATORE-SCREEN
              exit perform
              read old-rordini next at end exit perform end-read

              move OLD-ror-chiave            to ror-chiave           
              move OLD-ror-cod-articolo      to ror-cod-articolo     
              move OLD-ror-des-libera        to ror-des-libera       
              move OLD-ror-qta               to ror-qta              
              move OLD-ror-prz-unitario      to ror-prz-unitario     
              move OLD-ror-imp-consumo       to ror-imp-consumo      
              move OLD-ror-imp-cou-cobat     to ror-imp-cou-cobat    
              move OLD-ror-imponib-merce     to ror-imponib-merce    
              move OLD-ror-perce-sconto      to ror-perce-sconto     
              move OLD-ror-omaggio           to ror-omaggio          
              move OLD-ror-peso-utf          to ror-peso-utf         
              move OLD-ror-peso-non-utf      to ror-peso-non-utf     
              move OLD-ror-num-colli         to ror-num-colli        
              move OLD-ror-cod-imballo       to ror-cod-imballo      
              move OLD-ror-des-imballo       to ror-des-imballo      
              move OLD-ror-qta-imballi       to ror-qta-imballi      
              move OLD-ror-cod-art-cli       to ror-cod-art-cli      
              move OLD-ror-cod-iva           to ror-cod-iva          
              move OLD-ror-prz-commle        to ror-prz-commle       
              move OLD-ror-prg-cod-articolo  to ror-prg-cod-articolo 
              move OLD-ror-prg-cod-magazzino to ror-prg-cod-magazzino
              move OLD-ror-prg-tipo-imballo  to ror-prg-tipo-imballo 
              move OLD-ror-prg-peso          to ror-prg-peso         
              move OLD-ror-stato             to ror-stato              
              move OLD-ror-dati-comuni       to ror-dati-comuni      
              move OLD-ror-vuoti             to ror-vuoti            
              write ror-rec

           end-perform.
           close       old-rordini.
      *    delete file old-rordini.
      *     close rordini.



           move "mrordini" to nome-file.
           perform CREA-RENAME.

           open input OLD-mrordini.
      *    open output    mrordini.

           move low-value to old-mro-rec.
           start OLD-mrordini key >= OLD-mro-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-mrordini next at end exit perform end-read

              move OLD-mro-chiave            to mro-chiave           
              move OLD-mro-cod-articolo      to mro-cod-articolo     
              move OLD-mro-qta               to mro-qta              
              move OLD-mro-qta-e             to mro-qta-e            
              move OLD-mro-qta-b             to mro-qta-b            
              move OLD-mro-qta-f             to mro-qta-f            
              move OLD-mro-qta-omaggi        to mro-qta-omaggi       
              move OLD-mro-prz-unitario      to mro-prz-unitario     
              move OLD-mro-imp-consumo       to mro-imp-consumo      
              move OLD-mro-imp-cou-cobat     to mro-imp-cou-cobat    
              move OLD-mro-add-piombo        to mro-add-piombo       
              move OLD-mro-imponib-merce     to mro-imponib-merce    
              move OLD-mro-perce-sconto      to mro-perce-sconto     
              move OLD-mro-omaggio           to mro-omaggio          
              move OLD-mro-peso-utf          to mro-peso-utf         
              move OLD-mro-peso-non-utf      to mro-peso-non-utf     
              move OLD-mro-num-colli         to mro-num-colli        
              move OLD-mro-cod-imballo       to mro-cod-imballo      
              move OLD-mro-des-imballo       to mro-des-imballo      
              move OLD-mro-qta-imballi       to mro-qta-imballi      
              move OLD-mro-cod-art-cli       to mro-cod-art-cli      
              move OLD-mro-cod-iva           to mro-cod-iva          
              move OLD-mro-prz-commle        to mro-prz-commle       
              move OLD-mro-prg-cod-articolo  to mro-prg-cod-articolo 
              move OLD-mro-prg-cod-magazzino to mro-prg-cod-magazzino
              move OLD-mro-prg-tipo-imballo  to mro-prg-tipo-imballo 
              move OLD-mro-prg-peso          to mro-prg-peso         
              move OLD-mro-dati-blister      to mro-dati-blister               
              move OLD-mro-promo             to mro-promo            
              move OLD-mro-flag-cancellato   to mro-flag-cancellato               
              move OLD-mro-prz-promo         to mro-prz-promo                 
              move OLD-mro-progr             to mro-progr            
              move OLD-mro-evadi-dal         to mro-evadi-dal        
              move OLD-mro-dati-comuni       to mro-dati-comuni      
              move OLD-mro-vuoti             to mro-vuoti            

              write mro-rec
           end-perform.
           close       old-mrordini.
      *    delete file old-mrordini.
      *     close mrordini.



           move "rnotacr" to nome-file.
           perform CREA-RENAME.

           open input OLD-rnotacr.
      *    open output    rnotacr.

           move low-value to OLD-rno-rec.
           start OLD-rnotacr key >= OLD-rno-chiave.
           perform until 1 = 2         
              perform CONTATORE-SCREEN
              exit perform
              read OLD-rnotacr next at end exit perform end-read

              move OLD-rno-chiave            to rno-chiave           
              move OLD-rno-cod-articolo      to rno-cod-articolo     
              move OLD-rno-qta               to rno-qta              
              move OLD-rno-prz-unitario      to rno-prz-unitario     
              move OLD-rno-imp-consumo       to rno-imp-consumo      
              move OLD-rno-imp-cou-cobat     to rno-imp-cou-cobat    
              move OLD-rno-cod-iva           to rno-cod-iva          
              move OLD-rno-des-libera        to rno-des-libera       
              move OLD-rno-perce-sconto      to rno-perce-sconto     
              move OLD-rno-peso-utf          to rno-peso-utf         
              move OLD-rno-peso-non-utf      to rno-peso-non-utf     
              move OLD-rno-prg-cod-articolo  to rno-prg-cod-articolo 
              move OLD-rno-prg-cod-magazzino to rno-prg-cod-magazzino
              move OLD-rno-prg-tipo-imballo  to rno-prg-tipo-imballo 
              move OLD-rno-prg-peso          to rno-prg-peso         
              move OLD-rno-stato             to rno-stato                
              move OLD-rno-dati-comuni       to rno-dati-comuni      
              move OLD-rno-vuoti             to rno-vuoti            

              write rno-rec
           end-perform.

           close       OLD-rnotacr.
      *    delete file OLD-rnotacr.
      *     close rnotacr.



           move "rordforn" to nome-file.
           perform CREA-RENAME.

           open input OLD-rordforn.
      *    open output    rordforn.

           move low-value to OLD-rof-rec.
           start OLD-rordforn key >= OLD-rof-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-rordforn next at end exit perform end-read 

              move OLD-rof-chiave           to rof-chiave          
              move OLD-rof-cod-articolo     to rof-cod-articolo    
              move OLD-rof-cod-magazzino    to rof-cod-magazzino   
              move OLD-rof-tipo-imballo     to rof-tipo-imballo    
              move OLD-rof-peso             to rof-peso            
              move OLD-rof-imb-ordinato     to rof-imb-ordinato    
              move OLD-rof-qta-ord          to rof-qta-ord         
              move OLD-rof-qta-evasa        to rof-qta-evasa       
              move OLD-rof-prz-unitario     to rof-prz-unitario    
              move OLD-rof-sconto-1         to rof-sconto-1        
              move OLD-rof-sconto-2         to rof-sconto-2        
              move OLD-rof-sconto-3         to rof-sconto-3        
              move OLD-rof-sconto-4         to rof-sconto-4        
              move OLD-rof-sconto-5         to rof-sconto-5        
              move OLD-rof-imponib-merce    to rof-imponib-merce   
              move OLD-rof-imp-consumo      to rof-imp-consumo     
              move OLD-rof-imp-cou-cobat    to rof-imp-cou-cobat   
              move OLD-rof-add-piombo       to rof-add-piombo      
              move OLD-rof-costi-aggiuntivi to rof-costi-aggiuntivi
              move OLD-rof-cod-iva          to rof-cod-iva         
              move OLD-rof-peso-utf         to rof-peso-utf        
              move OLD-rof-peso-non-utf     to rof-peso-non-utf    
              move OLD-rof-cod-imballo      to rof-cod-imballo     
              move OLD-rof-qta-imballi      to rof-qta-imballi             
              move OLD-rof-promo            to rof-promo                   
              move OLD-rof-cod-listino      to rof-cod-listino     
              move OLD-rof-ddt              to rof-ddt             
              move OLD-rof-dati-carico      to rof-dati-carico     
              move OLD-rof-dati-comuni      to rof-dati-comuni     
              move OLD-rof-vuoti            to rof-vuoti           

              write rof-rec
           end-perform.

           close       OLD-rordforn.
      *    delete file OLD-rordforn.
      *     close rordforn.



           move "listini" to nome-file.
           perform CREA-RENAME.

           open input OLD-listini.
      *    open output    listini.
           move low-value to OLD-lst-rec.
           start OLD-listini key >= OLD-lst-chiave.
           perform until 1 = 2       
              perform CONTATORE-SCREEN
              exit perform
              read OLD-listini next at end exit perform end-read

              move OLD-lst-chiave            to lst-chiave           
              move OLD-lst-dati              to lst-dati             
              move OLD-lst-num-vuoto-1       to lst-num-vuoto-1      
              move OLD-lst-num-vuoto-2       to lst-num-vuoto-2      
              move OLD-lst-num-vuoto-3       to lst-num-vuoto-3      
              move OLD-lst-prg-cod-articolo  to lst-prg-cod-articolo   
              move OLD-lst-prg-cod-magazzino to lst-prg-cod-magazzino 
              move OLD-lst-prg-tipo-imballo  to lst-prg-tipo-imballo  
              move OLD-lst-prg-peso          to lst-prg-peso         
              move OLD-lst-alfa-vuoto-1      to lst-alfa-vuoto-1     
              move OLD-lst-alfa-vuoto-2      to lst-alfa-vuoto-2     
              move OLD-lst-alfa-vuoto-3      to lst-alfa-vuoto-3     

              write lst-rec
           end-perform.

           close       OLD-listini.
      *    delete file OLD-listini.
      *     close listini.



           move "rmovmag" to nome-file.
           perform CREA-RENAME.

           open input OLD-rmovmag.
      *    open output    rmovmag.

           move low-value to OLD-rmo-rec.
           start OLD-rmovmag key >= OLD-rmo-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-rmovmag next at end exit perform end-read
                        
              move OLD-rmo-chiave         to rmo-chiave        
              move OLD-rmo-dati-ricerca   to rmo-dati-ricerca  
              move OLD-rmo-destino        to rmo-destino            
              move OLD-rmo-articolo       to rmo-articolo      
              move OLD-rmo-codmag         to rmo-codmag        
              move OLD-rmo-imballo        to rmo-imballo       
              move OLD-rmo-peso           to rmo-peso          
              move OLD-rmo-udm            to rmo-udm           
              move OLD-rmo-qta            to rmo-qta           
              move OLD-rmo-peso-udm       to rmo-peso-udm      
              move OLD-rmo-peso-tot-utf   to rmo-peso-tot-utf  
              move OLD-rmo-peso-tot       to rmo-peso-tot      
              move OLD-rmo-listino        to rmo-listino       
              move OLD-rmo-sconto         to rmo-sconto        
              move OLD-rmo-netto          to rmo-netto         
              move OLD-rmo-imp-cons       to rmo-imp-cons      
              move OLD-rmo-coubat         to rmo-coubat        
              move OLD-rmo-marca-prodotto to rmo-marca-prodotto
              move OLD-rmo-stato          to rmo-stato                          
              move OLD-rmo-dati-comuni    to rmo-dati-comuni   
              move OLD-rmo-vuoti          to rmo-vuoti         
     
              write rmo-rec
           end-perform.

           close       OLD-rmovmag.
      *    delete file OLD-rmovmag.
      *     close rmovmag.

           

           move "progmagric" to nome-file.
           perform CREA-RENAME.

           open input OLD-progmagric.
           open input     progmagric.

           move low-value to OLD-prr-rec.
           start OLD-progmagric key >= OLD-prr-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-progmagric next at end exit perform end-read

              move OLD-prr-cod-articolo  to prr-cod-articolo 
              move OLD-prr-cod-magazzino to prr-cod-magazzino
              move OLD-prr-tipo-imballo  to prr-tipo-imballo 
              move OLD-prr-peso          to prr-peso         
              move OLD-prr-peso-utf      to prr-peso-utf     
              move OLD-prr-peso-non-utf  to prr-peso-non-utf 
              move OLD-prr-dinamici      to prr-dinamici     
              move OLD-prr-consolidati   to prr-consolidati  
              move OLD-prr-dati-comuni   to prr-dati-comuni  
              move OLD-prr-vuoti         to prr-vuoti        

              write prr-rec
           end-perform.
           close       OLD-progmagric.
      *    delete file OLD-progmagric.
      *     close progmagric.



           move "distinteb" to nome-file.
           perform CREA-RENAME.

           open input OLD-distinteb.
      *    open output    distinteb.

           move low-value to OLD-dis-rec.
           start OLD-distinteb key >= OLD-dis-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-distinteb next at end exit perform end-read
              
              move OLD-dis-codice           to dis-codice               
              move OLD-dis-articolo-finale  to dis-articolo-finale 
              move OLD-dis-magazzino-finale to dis-magazzino-finale
              move OLD-dis-imballo-finale   to dis-imballo-finale  
              move OLD-dis-peso-finale      to dis-peso-finale     
              move OLD-dis-peso-utf         to dis-peso-utf        
              move OLD-dis-peso-non-utf     to dis-peso-non-utf    
              move OLD-dis-prezzo           to dis-prezzo          
                                            
              perform varying I from 1 by 1 
                        until I > 10         
                 move OLD-dis-articolo(I)      to dis-articolo(I)

                 move OLD-dis-magazzino(I)     to dis-magazzino(I)       
                 move OLD-dis-imballo(I)       to dis-imballo(I)         
                 move OLD-dis-peso(I)          to dis-peso(I)            
                 move OLD-dis-qta(I)           to dis-qta(I)             
                 move OLD-dis-oneri(I)         to dis-oneri(I)           
                 move OLD-dis-somma-peso(I)    to dis-somma-peso(I)
              end-perform
                                                                   
              move OLD-dis-dati-comuni      to dis-dati-comuni                  
              move OLD-dis-vuoti            to dis-vuoti           

              write dis-rec
           end-perform.

           close       OLD-distinteb.
      *    delete file OLD-distinteb.
      *     close distinteb.



           move "provvig" to nome-file.
           perform CREA-RENAME.

           open input OLD-provvig.
      *    open output    provvig.

           move low-value to OLD-pvv-rec.
           start OLD-provvig key >= OLD-pvv-chiave.
           perform until 1 = 2    
              perform CONTATORE-SCREEN
              exit perform
              read OLD-provvig next at end exit perform end-read

              move OLD-pvv-chiave              to pvv-chiave             
              move OLD-pvv-data-fat            to pvv-data-fat           
              move OLD-pvv-agente              to pvv-agente             
              move OLD-pvv-cliente             to pvv-cliente            
              move OLD-pvv-articolo            to pvv-articolo           
              move OLD-pvv-marca               to pvv-marca              
              move OLD-pvv-um                  to pvv-um                 
              move OLD-pvv-qta-vend            to pvv-qta-vend           
              move OLD-pvv-peso-um             to pvv-peso-um            
              move OLD-pvv-prezzo-unit-vend    to pvv-prezzo-unit-vend   
              move OLD-pvv-prezzo-netto-agente 
                to pvv-prezzo-netto-agente
              move OLD-pvv-sconto-listino      to pvv-sconto-listino     
              move OLD-pvv-num-listino         to pvv-num-listino        
              move OLD-pvv-tipo-vend           to pvv-tipo-vend          
              move OLD-pvv-val-provvig         to pvv-val-provvig        
              move OLD-pvv-data-liq            to pvv-data-liq           
              move OLD-pvv-dati-comuni         to pvv-dati-comuni        
              move OLD-pvv-vuoti               to pvv-vuoti              

              write pvv-rec
           end-perform.
           close       OLD-provvig.
      *    delete file OLD-provvig.
      *     close provvig.



           move "brnotacr" to nome-file.
           perform CREA-RENAME.

      *    open output OLD-brnotacr.
           open input      brnotacr.

           move low-value to OLD-brno-rec.
           start OLD-brnotacr key >= OLD-brno-chiave.
           perform until 1 = 2     
              perform CONTATORE-SCREEN
              exit perform
              read OLD-brnotacr next at end exit perform end-read

              move OLD-brno-chiave            to brno-chiave           
              move OLD-brno-cod-articolo      to brno-cod-articolo     
              move OLD-brno-qta               to brno-qta              
              move OLD-brno-imponib-merce     to brno-imponib-merce    
              move OLD-brno-imp-consumo       to brno-imp-consumo      
              move OLD-brno-imp-cou-cobat     to brno-imp-cou-cobat    
              move OLD-brno-add-piombo        to brno-add-piombo       
              move OLD-brno-prz-unitario      to brno-prz-unitario     
              move OLD-brno-cod-iva           to brno-cod-iva          
              move OLD-brno-des-libera        to brno-des-libera       
              move OLD-brno-perce-sconto      to brno-perce-sconto     
              move OLD-brno-peso-utf          to brno-peso-utf         
              move OLD-brno-peso-non-utf      to brno-peso-non-utf     
              move OLD-brno-prg-cod-articolo  to brno-prg-cod-articolo 
              move OLD-brno-prg-cod-magazzino to brno-prg-cod-magazzino
              move OLD-brno-prg-tipo-imballo  to brno-prg-tipo-imballo 
              move OLD-brno-prg-peso          to brno-prg-peso         
              move OLD-brno-stato             to brno-stato            
              move OLD-brno-tipo-riga         to brno-tipo-riga        
              move OLD-brno-dati-comuni       to brno-dati-comuni      
              move OLD-brno-vuoti             to brno-vuoti            

              write brno-rec
           end-perform.
           close       OLD-brnotacr.
      *    delete file OLD-brnotacr.
      *     close brnotacr.



           move "ordfor2" to nome-file.
           perform CREA-RENAME.

           open input OLD-ordfor2.
      *    open output    ordfor2.

           move low-value to OLD-ord2-rec.
           start OLD-ordfor2 key >= OLD-ord2-chiave.
           perform until 1 = 2                              
              perform CONTATORE-SCREEN
              exit perform
              read OLD-ordfor2 next at end exit perform end-read

              move OLD-ord2-chiave          to ord2-chiave                   
              move OLD-ord2-art-descrizione to ord2-art-descrizione          
              move OLD-ord2-marca           to ord2-marca                    
              move OLD-ord2-qta-imb         to ord2-qta-imb                  
              move OLD-ord2-imballo         to ord2-imballo                  
              move OLD-ord2-peso            to ord2-peso                     
              move OLD-ord2-scost           to ord2-scost                    
              move OLD-ord2-lead-time       to ord2-lead-time                
              move OLD-ord2-lead-time-f     to ord2-lead-time-f              
              move OLD-ord2-giac            to ord2-giac                     
              move OLD-ord2-scorta          to ord2-scorta                   
              move OLD-ord2-impegnato       to ord2-impegnato                
              move OLD-ord2-qta-vendita-anno-corrente 
                to ord2-qta-vendita-anno-corrente
              move OLD-ord2-qta-vendita-anno-passsato 
                to ord2-qta-vendita-anno-passsato
              move OLD-ord2-media-vend      to ord2-media-vend               
              move OLD-ord2-consegna        to ord2-consegna                 
              move OLD-ord2-riordino        to ord2-riordino                 
              move OLD-ord2-fabbisogno-prox-6-mesi    
                to ord2-fabbisogno-prox-6-mesi   
              move OLD-ord2-qta-promo       to ord2-qta-promo                
              move OLD-ord2-promo           to ord2-promo                    
              move OLD-ord2-promo-mesi      to ord2-promo-mesi
              move OLD-ord2-mese-rif        to ord2-mese-rif                 
              move OLD-ord2-mese-scelto     to ord2-mese-scelto              
              move OLD-ord2-qta-ord         to ord2-qta-ord                  
              move OLD-ord2-costo-mp        to ord2-costo-mp                 
              move OLD-ord2-fabbisogno      to ord2-fabbisogno               
              move OLD-ord2-urgente         to ord2-urgente                  
              move OLD-ord2-conferma        to ord2-conferma                 
              move OLD-ord2-dati-comuni     to ord2-dati-comuni              
              move OLD-ord2-vuoti           to ord2-vuoti                    

              write ord2-rec
           end-perform.

           close       OLD-ordfor2.
      *    delete file OLD-ordfor2.
      *     close ordfor2.



           move "reva" to nome-file.
           perform CREA-RENAME.

           open input OLD-reva.
      *    open output    reva.

           move low-value to OLD-reva-rec.
           start OLD-reva key >= OLD-reva-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-reva next at end exit perform end-read

              move OLD-reva-chiave      to reva-chiave     
              move OLD-reva-articolo    to reva-articolo   
              move OLD-reva-codmag      to reva-codmag     
              move OLD-reva-imballo     to reva-imballo    
              move OLD-reva-peso        to reva-peso       
              move OLD-reva-qta         to reva-qta        
              move OLD-reva-prz-unit    to reva-prz-unit   
              move OLD-reva-netto       to reva-netto      
              move OLD-reva-imp-cons    to reva-imp-cons   
              move OLD-reva-coubat      to reva-coubat     
              move OLD-reva-add-pb      to reva-add-pb     
              move OLD-reva-chiave-ordf to reva-chiave-ordf
              move OLD-reva-stato       to reva-stato      
              move OLD-reva-dati-comuni to reva-dati-comuni
              move OLD-reva-vuoti       to reva-vuoti      

              write reva-rec
           end-perform.

           close       OLD-reva.
      *    delete file OLD-reva.
      *     close reva.



           move "art-ordforn" to nome-file.
           perform CREA-RENAME.

           open input OLD-art-ordforn.
      *    open output    art-ordforn.

           move low-value to OLD-aor-rec.
           start OLD-art-ordforn key >= OLD-aor-chiave.
           perform until 1 = 2
              perform CONTATORE-SCREEN
              exit perform
              read OLD-art-ordforn next at end exit perform end-read
                                                              
              move OLD-aor-chiave       to aor-chiave     
              move OLD-aor-articolo     to aor-articolo   
              move OLD-aor-codmag       to aor-codmag     
              move OLD-aor-imballo      to aor-imballo    
              move OLD-aor-peso         to aor-peso       
              move OLD-aor-descrizione  to aor-descrizione
              move OLD-aor-qta          to aor-qta        
              move OLD-aor-prz-unit     to aor-prz-unit   
              move OLD-aor-netto        to aor-netto      
              move OLD-aor-imp-cons     to aor-imp-cons   
              move OLD-aor-coubat       to aor-coubat     
              move OLD-aor-add-pb       to aor-add-pb     
              move OLD-aor-dati-comuni  to aor-dati-comuni
              move OLD-aor-vuoti        to aor-vuoti      

              write aor-rec
           end-perform.

           close       OLD-art-ordforn.
      *    delete file OLD-art-ordforn.
      *     close art-ordforn.



           move "cli-prg" to nome-file.
           perform CREA-RENAME.

           open input OLD-cli-prg.
      *    open output    cli-prg.

           move low-value to OLD-cp-rec.
           start OLD-cli-prg key >= OLD-cp-chiave.
           perform until 1 = 2    
              perform CONTATORE-SCREEN
              exit perform
              read OLD-cli-prg next at end exit perform end-read
              
              move OLD-cp-chiave        to cp-chiave       
              move OLD-cp-cod-articolo  to cp-cod-articolo 
              move OLD-cp-cod-magazzino to cp-cod-magazzino
              move OLD-cp-tipo-imballo  to cp-tipo-imballo 
              move OLD-cp-peso          to cp-peso         
              move OLD-cp-filler        to cp-filler       
              move OLD-cp-vuoti         to cp-vuoti        

              write cp-rec
           end-perform.

           close       OLD-cli-prg.
      *    delete file OLD-cli-prg.
      *     close cli-prg.


                                  
           set environment "FILE_PREFIX" to path-archivi-sto.

           move "STO-rordini" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-rordini.
      *    open output    STO-rordini.

           move low-value to OLD-STO-ror-rec.
           start OLD-STO-rordini key >= OLD-STO-ror-chiave.
           perform until 1 = 2     
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-rordini next at end exit perform end-read
                                                              
              move OLD-STO-ror-chiave           to STO-ror-chiave           
              move OLD-STO-ror-cod-articolo     to STO-ror-cod-articolo      
              move OLD-STO-ror-des-libera       to STO-ror-des-libera        
              move OLD-STO-ror-qta              to STO-ror-qta               
              move OLD-STO-ror-prz-unitario     to STO-ror-prz-unitario      
              move OLD-STO-ror-imp-consumo      to STO-ror-imp-consumo       
              move OLD-STO-ror-imp-cou-cobat    to STO-ror-imp-cou-cobat     
              move OLD-STO-ror-imponib-merce    to STO-ror-imponib-merce     
              move OLD-STO-ror-perce-sconto     to STO-ror-perce-sconto      
              move OLD-STO-ror-omaggio          to STO-ror-omaggio                     
              move OLD-STO-ror-peso-utf         to STO-ror-peso-utf          
              move OLD-STO-ror-peso-non-utf     to STO-ror-peso-non-utf      
              move OLD-STO-ror-num-colli        to STO-ror-num-colli         
              move OLD-STO-ror-cod-imballo      to STO-ror-cod-imballo       
              move OLD-STO-ror-des-imballo      to STO-ror-des-imballo       
              move OLD-STO-ror-qta-imballi      to STO-ror-qta-imballi       
              move OLD-STO-ror-cod-art-cli      to STO-ror-cod-art-cli       
              move OLD-STO-ror-cod-iva          to STO-ror-cod-iva           
              move OLD-STO-ror-prz-commle       to STO-ror-prz-commle        
              move OLD-STO-ror-prg-cod-articolo 
                to STO-ror-prg-cod-articolo  
              move OLD-STO-ror-prg-cod-magazzino 
                to STO-ror-prg-cod-magazzino 
              move OLD-STO-ror-prg-tipo-imballo  
                to STO-ror-prg-tipo-imballo  
              move OLD-STO-ror-prg-peso         to STO-ror-prg-peso         
              move OLD-STO-ror-stato            to STO-ror-stato                
              move OLD-STO-ror-dati-comuni      to STO-ror-dati-comuni            
              move OLD-STO-ror-vuoti            to STO-ror-vuoti            
           
              write STO-ror-rec
           end-perform.

           close       OLD-STO-rordini.
      *    delete file OLD-STO-rordini.
      *     close STO-rordini.



           move "STO-mrordini" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-mrordini.
      *    open output    STO-mrordini.

           move low-value to OLD-STO-mro-rec.
           start OLD-STO-mrordini key >= OLD-STO-mro-chiave.
           perform until 1 = 2      
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-mrordini next at end exit perform end-read

              move OLD-STO-mro-chiave         to STO-mro-chiave           
              move OLD-STO-mro-cod-articolo   to STO-mro-cod-articolo     
              move OLD-STO-mro-qta            to STO-mro-qta              
              move OLD-STO-mro-qta-e          to STO-mro-qta-e            
              move OLD-STO-mro-qta-b          to STO-mro-qta-b            
              move OLD-STO-mro-qta-f          to STO-mro-qta-f            
              move OLD-STO-mro-qta-omaggi     to STO-mro-qta-omaggi       
              move OLD-STO-mro-prz-unitario   to STO-mro-prz-unitario     
              move OLD-STO-mro-imp-consumo    to STO-mro-imp-consumo      
              move OLD-STO-mro-imp-cou-cobat  to STO-mro-imp-cou-cobat    
              move OLD-STO-mro-add-piombo     to STO-mro-add-piombo       
              move OLD-STO-mro-imponib-merce  to STO-mro-imponib-merce    
              move OLD-STO-mro-perce-sconto   to STO-mro-perce-sconto     
              move OLD-STO-mro-omaggio        to STO-mro-omaggio          
              move OLD-STO-mro-peso-utf       to STO-mro-peso-utf         
              move OLD-STO-mro-peso-non-utf   to STO-mro-peso-non-utf     
              move OLD-STO-mro-num-colli      to STO-mro-num-colli        
              move OLD-STO-mro-cod-imballo    to STO-mro-cod-imballo      
              move OLD-STO-mro-des-imballo    to STO-mro-des-imballo      
              move OLD-STO-mro-qta-imballi    to STO-mro-qta-imballi      
              move OLD-STO-mro-cod-art-cli    to STO-mro-cod-art-cli      
              move OLD-STO-mro-cod-iva        to STO-mro-cod-iva          
              move OLD-STO-mro-prz-commle     to STO-mro-prz-commle       
              move OLD-STO-mro-prg-cod-articolo  
                to STO-mro-prg-cod-articolo 
              move OLD-STO-mro-prg-cod-magazzino 
                to STO-mro-prg-cod-magazzino
              move OLD-STO-mro-prg-tipo-imballo  
                to STO-mro-prg-tipo-imballo 
              move OLD-STO-mro-prg-peso         to STO-mro-prg-peso         
              move OLD-STO-mro-dati-blister     to STO-mro-dati-blister     
              move OLD-STO-mro-promo            to STO-mro-promo            
              move OLD-STO-mro-flag-cancellato  
                to STO-mro-flag-cancellato  
              move OLD-STO-mro-prz-promo        to STO-mro-prz-promo        
              move OLD-STO-mro-progr            to STO-mro-progr            
              move OLD-STO-mro-evadi-dal        to STO-mro-evadi-dal        
              move OLD-STO-mro-dati-comuni      to STO-mro-dati-comuni      
              move OLD-STO-mro-vuoti            to STO-mro-vuoti            

              write STO-mro-rec
           end-perform.

           close       OLD-STO-mrordini.
      *    delete file OLD-STO-mrordini.
      *     close STO-mrordini.
           
           

           move "STO-rmovmag" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-rmovmag.
      *    open output    STO-rmovmag.

           move low-value to OLD-STO-rmo-rec.
           start OLD-STO-rmovmag key >= OLD-STO-rmo-chiave.
           perform until 1 = 2       
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-rmovmag next at end exit perform end-read
                                                             
              move OLD-STO-rmo-chiave       to   STO-rmo-chiave                               
              move OLD-STO-rmo-dati-ricerca to   STO-rmo-dati-ricerca      
              move OLD-STO-rmo-destino      to   STO-rmo-destino     
              move OLD-STO-rmo-articolo     to   STO-rmo-articolo    
              move OLD-STO-rmo-codmag       to   STO-rmo-codmag      
              move OLD-STO-rmo-imballo      to   STO-rmo-imballo     
              move OLD-STO-rmo-peso         to   STO-rmo-peso        
              move OLD-STO-rmo-udm          to   STO-rmo-udm         
              move OLD-STO-rmo-qta          to   STO-rmo-qta         
              move OLD-STO-rmo-peso-udm     to   STO-rmo-peso-udm    
              move OLD-STO-rmo-peso-tot-utf to   STO-rmo-peso-tot-utf
              move OLD-STO-rmo-peso-tot     to   STO-rmo-peso-tot    
              move OLD-STO-rmo-listino      to   STO-rmo-listino     
              move OLD-STO-rmo-sconto       to   STO-rmo-sconto      
              move OLD-STO-rmo-netto        to   STO-rmo-netto       
              move OLD-STO-rmo-imp-cons     to   STO-rmo-imp-cons    
              move OLD-STO-rmo-coubat       to   STO-rmo-coubat      
              move OLD-STO-rmo-marca-prodotto to STO-rmo-marca-prodotto
              move OLD-STO-rmo-stato        to   STO-rmo-stato       
              move OLD-STO-rmo-dati-comuni  to   STO-rmo-dati-comuni 
              move OLD-STO-rmo-vuoti        to   STO-rmo-vuoti       

              write STO-rmo-rec
           end-perform.

           close       OLD-STO-rmovmag.
      *    delete file OLD-STO-rmovmag.
      *     close STO-rmovmag.
           
           

           move "STO-rnotacr" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-rnotacr.
      *    open output    STO-rnotacr.

           move low-value to OLD-STO-rno-rec.
           start OLD-STO-rnotacr key >= OLD-STO-rno-chiave.
           perform until 1 = 2              
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-rnotacr next at end exit perform end-read

              move OLD-STO-rno-chiave         to STO-rno-chiave           
              move OLD-STO-rno-cod-articolo   to STO-rno-cod-articolo     
              move OLD-STO-rno-qta            to STO-rno-qta              
              move OLD-STO-rno-prz-unitario   to STO-rno-prz-unitario     
              move OLD-STO-rno-imp-consumo    to STO-rno-imp-consumo      
              move OLD-STO-rno-imp-cou-cobat  to STO-rno-imp-cou-cobat    
              move OLD-STO-rno-cod-iva        to STO-rno-cod-iva          
              move OLD-STO-rno-des-libera     to STO-rno-des-libera       
              move OLD-STO-rno-perce-sconto   to STO-rno-perce-sconto     
              move OLD-STO-rno-peso-utf       to STO-rno-peso-utf         
              move OLD-STO-rno-peso-non-utf   to STO-rno-peso-non-utf     
              move OLD-STO-rno-prg-cod-articolo  
                to STO-rno-prg-cod-articolo 
              move OLD-STO-rno-prg-cod-magazzino 
                to STO-rno-prg-cod-magazzino
              move OLD-STO-rno-prg-tipo-imballo  
                to STO-rno-prg-tipo-imballo 
              move OLD-STO-rno-prg-peso          to STO-rno-prg-peso         
              move OLD-STO-rno-stato             to STO-rno-stato            
              move OLD-STO-rno-dati-comuni       to STO-rno-dati-comuni      
              move OLD-STO-rno-vuoti             to STO-rno-vuoti            

              write STO-rno-rec
           end-perform.

           close       OLD-STO-rnotacr.
      *    delete file OLD-STO-rnotacr.
      *     close STO-rnotacr. 
           
           

           move "STO-rordforn" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-rordforn.
      *    open output    STO-rordforn.

           move low-value to OLD-STO-rof-rec.
           start OLD-STO-rordforn key >= OLD-STO-rof-chiave.
           perform until 1 = 2              
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-rordforn next at end exit perform end-read
                                             
              move OLD-STO-rof-chiave           to STO-rof-chiave          
              move OLD-STO-rof-cod-articolo     to STO-rof-cod-articolo    
              move OLD-STO-rof-cod-magazzino    to STO-rof-cod-magazzino   
              move OLD-STO-rof-tipo-imballo     to STO-rof-tipo-imballo    
              move OLD-STO-rof-peso             to STO-rof-peso            
              move OLD-STO-rof-imb-ordinato     to STO-rof-imb-ordinato    
              move OLD-STO-rof-qta-ord          to STO-rof-qta-ord         
              move OLD-STO-rof-qta-evasa        to STO-rof-qta-evasa       
              move OLD-STO-rof-prz-unitario     to STO-rof-prz-unitario    
              move OLD-STO-rof-sconto-1         to STO-rof-sconto-1        
              move OLD-STO-rof-sconto-2         to STO-rof-sconto-2        
              move OLD-STO-rof-sconto-3         to STO-rof-sconto-3        
              move OLD-STO-rof-sconto-4         to STO-rof-sconto-4        
              move OLD-STO-rof-sconto-5         to STO-rof-sconto-5        
              move OLD-STO-rof-imponib-merce    to STO-rof-imponib-merce   
              move OLD-STO-rof-imp-consumo      to STO-rof-imp-consumo     
              move OLD-STO-rof-imp-cou-cobat    to STO-rof-imp-cou-cobat   
              move OLD-STO-rof-add-piombo       to STO-rof-add-piombo      
              move OLD-STO-rof-costi-aggiuntivi 
                to STO-rof-costi-aggiuntivi
              move OLD-STO-rof-cod-iva          to STO-rof-cod-iva         
              move OLD-STO-rof-peso-utf         to STO-rof-peso-utf        
              move OLD-STO-rof-peso-non-utf     to STO-rof-peso-non-utf    
              move OLD-STO-rof-cod-imballo      to STO-rof-cod-imballo     
              move OLD-STO-rof-qta-imballi      to STO-rof-qta-imballi      
              move OLD-STO-rof-promo            to STO-rof-promo                       
              move OLD-STO-rof-cod-listino      to STO-rof-cod-listino     
              move OLD-STO-rof-ddt              to STO-rof-ddt             
              move OLD-STO-rof-dati-carico      to STO-rof-dati-carico     
              move OLD-STO-rof-dati-comuni      to STO-rof-dati-comuni     
              move OLD-STO-rof-vuoti            to STO-rof-vuoti           

              write STO-rof-rec
           end-perform.

           close       OLD-STO-rordforn.
      *    delete file OLD-STO-rordforn.
      *     close STO-rordforn.
           
           

           move "STO-provvig" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-provvig.
      *    open output    STO-provvig.

           move low-value to OLD-STO-pvv-rec.
           start OLD-STO-provvig key >= OLD-STO-pvv-chiave.
           perform until 1 = 2            
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-provvig next at end exit perform end-read
                            
              move OLD-STO-pvv-chiave         to STO-pvv-chiave             
              move OLD-STO-pvv-data-fat       to STO-pvv-data-fat           
              move OLD-STO-pvv-agente         to STO-pvv-agente             
              move OLD-STO-pvv-cliente        to STO-pvv-cliente            
              move OLD-STO-pvv-articolo       to STO-pvv-articolo           
              move OLD-STO-pvv-marca          to STO-pvv-marca              
              move OLD-STO-pvv-um             to STO-pvv-um                 
              move OLD-STO-pvv-qta-vend       to STO-pvv-qta-vend           
              move OLD-STO-pvv-peso-um        to STO-pvv-peso-um            
              move OLD-STO-pvv-prezzo-unit-vend    
                to STO-pvv-prezzo-unit-vend   
              move OLD-STO-pvv-prezzo-netto-agente 
                to STO-pvv-prezzo-netto-agente
              move OLD-STO-pvv-sconto-listino      
                to STO-pvv-sconto-listino     
              move OLD-STO-pvv-num-listino    to STO-pvv-num-listino        
              move OLD-STO-pvv-tipo-vend      to STO-pvv-tipo-vend          
              move OLD-STO-pvv-val-provvig    to STO-pvv-val-provvig        
              move OLD-STO-pvv-data-liq       to STO-pvv-data-liq           
              move OLD-STO-pvv-dati-comuni    to STO-pvv-dati-comuni        
              move OLD-STO-pvv-vuoti          to STO-pvv-vuoti              
              

              write STO-pvv-rec
           end-perform.

           close       OLD-STO-provvig.
      *    delete file OLD-STO-provvig.
      *     close STO-provvig.
           
           

           move "STO-reva" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-reva.
      *    open output    STO-reva.

           move low-value to OLD-STO-reva-rec.
           start OLD-STO-reva key >= OLD-STO-reva-chiave.
           perform until 1 = 2          
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-reva next at end exit perform end-read
                                                              
              move OLD-STO-reva-chiave       to STO-reva-chiave       
              move OLD-STO-reva-articolo     to STO-reva-articolo   
              move OLD-STO-reva-codmag       to STO-reva-codmag     
              move OLD-STO-reva-imballo      to STO-reva-imballo    
              move OLD-STO-reva-peso         to STO-reva-peso       
              move OLD-STO-reva-qta          to STO-reva-qta        
              move OLD-STO-reva-prz-unit     to STO-reva-prz-unit   
              move OLD-STO-reva-netto        to STO-reva-netto      
              move OLD-STO-reva-imp-cons     to STO-reva-imp-cons   
              move OLD-STO-reva-coubat       to STO-reva-coubat     
              move OLD-STO-reva-add-pb       to STO-reva-add-pb     
              move OLD-STO-reva-chiave-ordf  to STO-reva-chiave-ordf
              move OLD-STO-reva-stato        to STO-reva-stato        
              move OLD-STO-reva-dati-comuni  to STO-reva-dati-comuni
              move OLD-STO-reva-vuoti        to STO-reva-vuoti      

              write STO-reva-rec
           end-perform.

           close       OLD-STO-reva.
      *    delete file OLD-STO-reva.
      *     close STO-reva.
           
           

           move "STO-brnotacr" to nome-file.
           perform CREA-RENAME-STO.
           
           open input OLD-STO-brnotacr.
      *    open output    STO-brnotacr.

           move low-value to OLD-STO-brno-rec.
           start OLD-STO-brnotacr key >= OLD-STO-brno-chiave.
           perform until 1 = 2          
              perform CONTATORE-SCREEN
              exit perform
              read OLD-STO-brnotacr next at end exit perform end-read
                                                              
              move OLD-STO-brno-chiave         to STO-brno-chiave           
              move OLD-STO-brno-cod-articolo   to STO-brno-cod-articolo     
              move OLD-STO-brno-qta            to STO-brno-qta              
              move OLD-STO-brno-imponib-merce  to STO-brno-imponib-merce    
              move OLD-STO-brno-imp-consumo    to STO-brno-imp-consumo      
              move OLD-STO-brno-imp-cou-cobat  to STO-brno-imp-cou-cobat    
              move OLD-STO-brno-add-piombo     to STO-brno-add-piombo       
              move OLD-STO-brno-prz-unitario   to STO-brno-prz-unitario     
              move OLD-STO-brno-cod-iva        to STO-brno-cod-iva          
              move OLD-STO-brno-des-libera     to STO-brno-des-libera       
              move OLD-STO-brno-perce-sconto   to STO-brno-perce-sconto     
              move OLD-STO-brno-peso-utf       to STO-brno-peso-utf         
              move OLD-STO-brno-peso-non-utf   to STO-brno-peso-non-utf     
              move OLD-STO-brno-prg-cod-articolo  
                to STO-brno-prg-cod-articolo 
              move OLD-STO-brno-prg-cod-magazzino 
                to STO-brno-prg-cod-magazzino 
              move OLD-STO-brno-prg-tipo-imballo  
                to STO-brno-prg-tipo-imballo 
              move OLD-STO-brno-prg-peso       to STO-brno-prg-peso         
              move OLD-STO-brno-stato          to STO-brno-stato              
              move OLD-STO-brno-tipo-riga      to STO-brno-tipo-riga        
              move OLD-STO-brno-dati-comuni    to STO-brno-dati-comuni      
              move OLD-STO-brno-vuoti          to STO-brno-vuoti            

              write STO-brno-rec
           end-perform.

           close       OLD-STO-brnotacr.
      *    delete file OLD-STO-brnotacr.
      *     close STO-brnotacr.



      ***---
       CREA-RENAME.                          
           perform NOME-FILE-UPPERCASE.
           inspect nome-file replacing trailing spaces by low-value.
           add  interlinea      to line-file.

           move 0 to counter counter2.               
           initialize path-dest path-source.
           string path-archivi delimited low-value
                  nome-file    delimited low-value
                  into path-source
           end-string.
           string path-archivi delimited low-value
                  "OLD-"       delimited size
                  nome-file    delimited low-value
                  into path-dest
           end-string.
           call "RENAME" using path-source path-dest.
           initialize path-dest path-source.
           string path-archivi delimited low-value
                  nome-file    delimited low-value
                  ".vix"       delimited size
                  into path-source
           end-string.
           string path-archivi delimited low-value
                  "OLD-"       delimited size
                  nome-file    delimited low-value
                  ".vix"       delimited size
                  into path-dest
           end-string.
           call "RENAME" using path-source path-dest.

      ***---
       CREA-RENAME-STO.                      
           perform NOME-FILE-UPPERCASE.
           inspect nome-file replacing trailing spaces by low-value.                  
           add  interlinea      to line-file.

           move 0 to counter counter2.            
           initialize path-dest path-source.
           string path-archivi-sto delimited low-value
                  nome-file        delimited low-value
                  into path-source
           end-string.
           string path-archivi-sto delimited low-value
                  "OLD-"           delimited size
                  nome-file        delimited low-value
                  into path-dest
           end-string.
           call "RENAME" using path-source path-dest.
           initialize path-dest path-source.
           string path-archivi-sto delimited low-value
                  nome-file        delimited low-value
                  ".vix"           delimited size
                  into path-source
           end-string.
           string path-archivi-sto delimited low-value
                  "OLD-"           delimited size
                  nome-file        delimited low-value
                  ".vix"           delimited size
                  into path-dest
           end-string.
           call "RENAME" using path-source path-dest.

      ***---
       NOME-FILE-UPPERCASE.
           move nome-file to nome-file-upper.
           call "C$TOUPPER" using nome-file-upper, value 15.

      ***---            
       CONTATORE-SCREEN.
           add 1 to counter
           add 1 to counter2
           move 50 to counter2.
           if counter2 = 50
              move counter to counter-edit                   
              display nome-file-upper
                      upon peso-conv-handle at column 3,00
                                               line line-file
              display counter-edit 
                      upon peso-conv-handle at column 31,50
                                               line line-file
              move 0 to counter2
           end-if.
