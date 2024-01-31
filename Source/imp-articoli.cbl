       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-articoli.
       AUTHOR.                          Andrea.
       REMARKS.
           Specifiche file CSV:
           - i valori numerici devono avere la , come separatore dei decimali,
             per quanto riguarda il resto del formato non ci sono problemi
           - I valori del codice ean vengono considerati dal primo numerico
           - Non ci devono essere ; all'interno di alcun campo
           - deve chiamarsi articoli.csv
           - deve risiedere nella dir degli archivi
           - deve avere la prima riga di intestazione

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT file-err
           ASSIGN       TO  path-file-err
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-file-err.

           copy "lineseq.sl". 
           copy "articoli.sl".
           COPY "articoli.sl"
                REPLACING ==articoli== BY ==articoli1==,
                          ==STATUS-articoli== BY ==STATUS-articoli1==.
           copy "progmag.sl".
           copy "tcla1art.sl".
           copy "tnomen.sl".
           copy "tmarche.sl".
           copy "prodener.sl".
           copy "clienti.sl".
           copy "destinif.sl".    
           copy "tscorte.sl".
           copy "tsetmerc.sl".
           copy "tivaese.sl".
           copy "tudm.sl".
           copy "timbalqta.sl".
           copy "tmagaz.sl".
           copy "blister.sl".
      *****     copy "catart.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  file-err.
       01 ferr-riga        PIC  x(900).

           copy "lineseq.fd". 
           copy "articoli.fd". 
           COPY "articoli.fd"
                REPLACING ==articoli== BY ==articoli1==,
                          ==STATUS-articoli== BY ==STATUS-articoli1==.
           copy "progmag.fd".
           copy "tcla1art.fd".
           copy "tnomen.fd".
           copy "tmarche.fd".
           copy "prodener.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "tscorte.fd". 
           copy "tsetmerc.fd". 
           copy "tivaese.fd".
           copy "tudm.fd".
           copy "timbalqta.fd".
           copy "tmagaz.fd". 
           copy "blister.fd".
      *****     copy "catart.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "link-G2Agg.def".
      *****     copy "link-check-catart.def".
           copy "tratta-numerico.def".

      * COSTANTI
       78  titolo value "Importazione articoli". 
       78  78-comma              value ",".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-articoli       pic xx.
       77  status-articoli1      pic xx.
       77  status-progmag        pic xx.
       77  status-tcla1art       pic xx.
       77  status-tnomen         pic xx.
       77  status-tmarche        pic xx.
       77  status-prodener       pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx. 
       77  status-tscorte        pic xx.
       77  status-tsetmerc       pic xx. 
       77  status-tivaese        pic xx.
       77  status-tudm           pic xx.
       77  status-timbalqta      pic xx.
       77  status-tmagaz         pic xx.   
       77  status-blister        pic xx.
       77  status-file-err       pic xx.
      ***** 77  status-catart         pic xx.
       77  wstampa               pic x(256).
       77  path-file-err         pic x(256) value spaces.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * Occurs errori                                                 
       77  idx                  pic 9(4) value 0.
       01  el-no-descr          pic 9(4) occurs 9999 
                                         indexed by idx-no-descr. 
       01  el-no-merce          pic 9(4) occurs 9999 
                                         indexed by idx-no-merce. 
       01  el-no-marca          pic 9(4) occurs 9999 
                                         indexed by idx-no-marca. 
       01  el-no-magaz          pic 9(4) occurs 9999 
                                         indexed by idx-no-magaz. 
       01  el-no-iva            pic 9(4) occurs 9999 
                                         indexed by idx-no-iva. 
       01  el-iva-no-perce      pic 9(4) occurs 9999 
                                         indexed by idx-iva-no-perce. 
       01  el-no-classe         pic 9(4) occurs 9999 
                                         indexed by idx-no-classe. 
       01  el-no-udm            pic 9(4) occurs 9999 
                                         indexed by idx-no-udm. 
       01  el-no-imballo        pic 9(4) occurs 9999 
                                         indexed by idx-no-imballo. 
       01  el-no-udm-imballo    pic 9(4) occurs 9999 
                                         indexed by idx-no-udm-imballo. 
       01  el-ass-imp-errato    pic 9(4) occurs 9999 
                                         indexed by idx-ass-imp-errato. 
       01  el-no-scorta         pic 9(4) occurs 9999 
                                         indexed by idx-no-scorta. 
       01  el-forn-non-valido   pic 9(4) occurs 9999 
                                         indexed by idx-forn-non-valido. 
       01  el-desf-non-valido   pic 9(4) occurs 9999 
                                         indexed by idx-desf-non-valido. 
       01  el-no-prodener       pic 9(4) occurs 9999 
                                         indexed by idx-no-prodener. 
       01  el-prodener-non-comp pic 9(4) occurs 9999 
                                       indexed by idx-prodener-non-comp. 
       01  el-no-altezza        pic 9(4) occurs 9999 
                                         indexed by idx-no-altezza. 
       01  el-no-larghezza      pic 9(4) occurs 9999 
                                         indexed by idx-no-larghezza. 
       01  el-no-profondita     pic 9(4) occurs 9999 
                                         indexed by idx-no-profondita. 
       01  el-no-qta-epal       pic 9(4) occurs 9999 
                                         indexed by idx-no-qta-epal. 
      ***** 01  el-colleg-non-trovato pic 9(4) occurs 9999 
      *****                           indexed by idx-colleg-non-trovato.  
      ***** 01  el-colleg-no-scorta   pic 9(4) occurs 9999 
      *****                           indexed by idx-colleg-no-scorta.  
      ***** 01  el-colleg-non-valido  pic 9(4) occurs 9999 
      *****                           indexed by idx-colleg-non-valido.
       01  el-no-dogana          pic 9(4) occurs 9999 
                                       indexed by idx-no-dogana.
       01  el-no-limite          pic 9(4) occurs 9999 
                                       indexed by idx-no-limite.
       01  el-no-reale           pic 9(4) occurs 9999 
                                       indexed by idx-no-reale. 
       01  el-ass-cou-errato  pic 9(4) occurs 9999 
                                       indexed by idx-ass-cou-errato. 
       01  el-ass-cob-errato  pic 9(4) occurs 9999 
                                       indexed by idx-ass-cob-errato. 
       01  el-no-peso-utf     pic 9(4) occurs 9999 
                                       indexed by idx-no-peso-utf.
       01  el-no-peso-non-utf pic 9(4) occurs 9999 
                                       indexed by idx-no-peso-non-utf.
       01  el-si-perce-imp    pic 9(4) occurs 9999 
                                       indexed by idx-si-perce-imp.
       01  el-no-perce-imp    pic 9(4) occurs 9999 
                                       indexed by idx-no-perce-imp.
       01  el-si-amperaggio   pic 9(4) occurs 9999 
                                       indexed by idx-si-amperaggio.
       01  el-no-amperaggio   pic 9(4) occurs 9999 
                                       indexed by idx-no-amperaggio.
       01  el-no-scorta-9     pic 9(4) occurs 9999 
                                       indexed by idx-no-scorta-9.
       01  el-err-peso-utf    pic 9(4) occurs 9999 
                                       indexed by idx-err-peso-utf.    
       01  el-err-peso-non-utf  pic 9(4) occurs 9999 
                                       indexed by idx-err-peso-non-utf.
       01  el-err-pesi          pic 9(4) occurs 9999 
                                       indexed by idx-err-pesi.

      * VARIABILI                          
       77  last-art-codice       pic 9(6)   value 0.
       77  como-data             pic 9(8)   value 0.
       77  como-ora              pic 9(8)   value 0.
       77  riga                  pic 9(6)   value 0.
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0. 
       77  como-peso             pic 9(6)v9(4).
       77  peso-inserito         pic 9(6)v9(4).     
       77  last-codice           pic 9(6)   value 0.
       77  ultimo-elemento       pic 9(6)   value 0.
       77  old-last-codice       pic 9(6)   value 0.
       77  old-ultimo-elemento   pic 9(6)   value 0.

       77 como-peso-utf                pic x(20).
       77 como-peso-non-utf            pic x(20).
       77 como-peso-standard           pic x(20).
       77 como-prezzo-vendita          pic x(20).
       77 como-perce-sconto-agente     pic x(20).
       77 como-prezzo-acquisto         pic x(20).
       77 como-perce-sconto-acquisto   pic x(20).
       77 como-perce-imposte           pic x(20).
       77 como-perce-cou               pic x(20).
       77 como-prezzo-banco            pic x(20).
       77 como-prz-min-vend            pic x(20).
       77 como-moq                     pic x(20).
       77 como-codice-ean-1            pic x(20).
       77 como-codice-ean-2            pic x(20).
       77 como-codice-ean-3            pic x(20).
       77 como-codice-ean-4            pic x(20).
       77 como-codice-ean-5            pic x(20).
       77 como-ean                     pic x(13).
    
       01  tipo-errore           pic 99.
         88 no-merce             value 1.
         88 no-marca             value 2.
         88 no-magaz             value 3.
         88 no-iva               value 4.
         88 iva-no-perce         value 5.
         88 no-classe            value 6.
         88 no-udm               value 7.
         88 no-imballo           value 8.
         88 no-udm-imballo       value 9.
         88 ass-imp-errato       value 10.
         88 no-scorta            value 11.
         88 forn-non-valido      value 12.
         88 desf-non-valido      value 13.
         88 no-prodener          value 14.
         88 prodener-non-comp    value 15.
         88 no-altezza           value 16.
         88 no-larghezza         value 17.
         88 no-profondita        value 18.
         88 no-qta-epal          value 19.
         88 no-descr             value 20.
      *****   88 colleg-non-trovato   value 21.
      *****   88 colleg-no-scorta     value 22.
      *****   88 colleg-non-valido    value 23.
         88 no-dogana            value 24.
         88 no-limite            value 25.
         88 no-reale             value 26. 
         88 ass-cou-errato       value 27.
         88 ass-cob-errato       value 28.
         88 no-peso-utf          value 29. 
         88 no-peso-non-utf      value 30.
         88 si-perce-imp         value 31.
         88 no-perce-imp         value 32.
         88 si-amperaggio        value 33.
         88 no-amperaggio        value 34.
         88 no-scorta-9          value 35.
         88 err-peso-utf         value 36.       
         88 err-peso-non-utf     value 37.
         88 err-pesi             value 38.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [LINESEQ] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

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
           set tutto-ok to true.
           move "articoli.csv" to wstampa.
           move 0 to idx-no-descr
                     idx-no-merce        
                     idx-no-marca        
                     idx-no-magaz        
                     idx-no-iva          
                     idx-iva-no-perce    
                     idx-no-classe       
                     idx-no-udm          
                     idx-no-imballo      
                     idx-no-udm-imballo  
                     idx-ass-imp-errato  
                     idx-no-scorta       
                     idx-forn-non-valido 
                     idx-desf-non-valido 
                     idx-no-prodener     
                     idx-prodener-non-comp 
                     idx-no-altezza      
                     idx-no-larghezza    
                     idx-no-profondita   
                     idx-no-qta-epal
                     idx-no-dogana       
                     idx-no-limite        
                     idx-no-reale         
                     idx-ass-cou-errato   
                     idx-ass-cob-errato   
                     idx-no-peso-utf      
                     idx-no-peso-non-utf  
                     idx-si-perce-imp     
                     idx-no-perce-imp     
                     idx-si-amperaggio    
                     idx-no-amperaggio    
                     idx-no-scorta-9
                     idx-err-peso-utf
                     idx-err-peso-non-utf
                     idx-err-pesi.

      ***---
       OPEN-FILES.
           open i-o   articoli.
           open input lineseq.
           open i-o   progmag.
           open input tcla1art tnomen tmarche prodener clienti 
                      destinif tscorte tsetmerc timbalqta tudm tivaese
                      tmagaz articoli1 blister. |catart.

      ***---
       ELABORAZIONE.
           move 0 to last-art-codice.
           move high-value to art-codice of articoli.
           start articoli key <= art-codice of articoli
                 invalid continue
             not invalid
                 read articoli previous
                 move art-codice of articoli to last-art-codice
           end-start.
           initialize line-riga.
           read lineseq next. |Salto l'intestazione
           move 1 to riga.
           perform until 1 = 2
              initialize line-riga art-rec of articoli
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              read lineseq next at end exit perform end-read
              add 1 to riga          
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into art-descrizione1           of articoli
                            art-descrizione2           of articoli
                            art-settore-merceologico   of articoli
                            art-marca-prodotto         of articoli      
                            art-classe-1               of articoli
                            art-classe-2               of articoli
                            art-classe-3               of articoli
                            art-classe-4               of articoli
                            art-cod-fornitore          of articoli     
                            art-gestione-utf           of articoli

                            como-peso-utf
                            como-peso-non-utf          
                            como-peso-standard         
                            art-imballo-standard       of articoli    
                            art-udm-imballo            of articoli
                            como-prezzo-vendita           
                            como-perce-sconto-agente  
                            como-prezzo-acquisto      
                            como-perce-sconto-acquisto
                            art-cod-doganale           of articoli

                            art-soggetto-imposte       of articoli
                            como-perce-imposte       
                            como-perce-cou        
                            art-soggetto-cobat         of articoli
                            art-amperaggio             of articoli
                            art-auto-moto-per-cobat    of articoli
                            art-note                   of articoli
                            como-codice-ean-1        
                            como-codice-ean-2        
                            como-codice-ean-3        

                            como-codice-ean-4        
                            como-codice-ean-5        
                            art-descrizione-2          of articoli
                            art-qta-epal               of articoli
                            art-qta-std                of articoli
                            art-altezza                of articoli
                            art-larghezza              of articoli
                            art-profondita             of articoli
                            art-ALA                    of articoli
                            art-gda                    of articoli

                            art-agenti                 of articoli
                            art-specialist             of articoli
                            art-estero                 of articoli
                            art-scorta                 of articoli 
                            como-prezzo-banco   
                            como-prz-min-vend   
                            como-moq
                            art-do                     of articoli
                            art-cod-art-frn            of articoli
                            art-mag-std                of articoli
                            art-cod-desf-forn          of articoli
                            art-cod-prodener           of articoli
                            art-SPI                    of articoli
                            art-T1                     of articoli
                            art-T2                     of articoli
                            art-T3                     of articoli
                            art-AT                     of articoli
                            art-TEXACO                 of articoli
                            art-SHARK                  of articoli
                            art-litri                  of articoli
                            art-note2                  of articoli
                            art-des-ita                of articoli
                            art-des-spa                of articoli
              end-unstring 
              move "PZ"  to art-unita-di-misura of articoli
              move "022" to art-codice-iva      of articoli
              move "C"   to art-tipo-stoc       of articoli

              perform TRATTA-NUMERICI
              
              perform CONTROLLI

              if errori
                 perform AGGIUNGI-ERRATO
              else             
                                    
                 accept art-ora-creazione of articoli  from time
                 accept art-data-creazione of articoli from century-date
                 move "IMPORT"  to art-utente-creazione of articoli  

                 add 1 to num-rec-ok
                 perform until 1 = 2
                    add 1 to last-art-codice
                    move last-art-codice to art-codice of articoli
                    set art-attivo of articoli to true
                    write art-rec of articoli
                          invalid continue
                      not invalid exit perform
                    end-write         
                 end-perform

                 perform SCRIVI-PROGMAG    

                 |Allineare anche i progressivi
                 close      progmag
                 open i-o   progmag
                 initialize prg-chiave replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move art-codice of articoli to prg-cod-articolo
                 start progmag key >= prg-chiave
                       invalid continue 
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo not = 
                             art-codice of articoli
                             exit perform
                          end-if
      
                          if RecLocked |IGNORA
                             exit perform
                          end-if
                                                    
                          move art-stato of articoli to prg-stato

                          rewrite prg-rec 
                                  invalid continue 
                          end-rewrite
                       end-perform
                 end-start       
                
                 |non ci sono articoli collegati
      *****           if art-collegato of articoli not = 0
      *****              perform AGGIORNA-CATENA
      *****           end-if  
         
                 initialize G2Agg-linkage
                 set G2Agg-art   to true
                 move art-codice of articoli to G2Agg-articolo
                 set G2Agg-insert to true
                 call   "G2Agg" using G2Agg-linkage
                 cancel "G2Agg"
LUBEXX        end-if
           end-perform.
                   
           if path-file-err = spaces
              display message "Operazione terminata!"
                       x"0d0a""IMPORTATI: ", num-rec,   
                              " ARTICOLI CORRETTAMENTE"
                        title titolo
                         icon 2
           else
              move "Elenco righe non importate" to ferr-riga
              write ferr-riga
              write ferr-riga from spaces
              perform varying idx from 1 by 1 
                        until idx > idx-no-descr
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-descr(idx)         delimited size
                        " - Descrizione assente" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-merce 
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-merce(idx)         delimited size
                        " - Settore merceologico non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-marca
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-marca(idx)         delimited size
                        " - Codice marca non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform          
              perform varying idx from 1 by 1 
                        until idx > idx-no-magaz
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-magaz(idx)         delimited size
                        " - Codice magazzino non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-iva
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-iva(idx)           delimited size
                        " - Codice IVA non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-merce
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-merce(idx)         delimited size
                        " - Codice IVA senza aliquota" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-classe
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-classe(idx)        delimited size
                        " - Classe non valida"   delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-udm   
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-udm(idx)           delimited size
                        " - Unità di misura non valida" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga                 
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-imballo
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-imballo(idx)       delimited size
                        " - Codice imballo non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-udm-imballo
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-udm-imballo(idx)   delimited size
                        " - U.d.m. per imballo non valida" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-ass-imp-errato
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-ass-imp-errato(idx)   delimited size
                        " - Assoggettamento imposte non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-scorta
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-scorta(idx)        delimited size
                        " - Codice scorta non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-forn-non-valido
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-forn-non-valido(idx)  delimited size
                        " - Codice fornitore non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-desf-non-valido
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-desf-non-valido(idx)  delimited size
                        " - Destino fornitore non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-prodener
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-prodener(idx)  delimited size
                        " - Prodotto energetico non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-prodener-non-comp
                 initialize ferr-riga
                 string "RIGA: "                  delimited size
                        el-prodener-non-comp(idx) delimited size
                        " - Prodotto energetico non compatibile" 
                                                  delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-altezza
                 initialize ferr-riga
                 string "RIGA: "            delimited size
                        el-no-altezza(idx)  delimited size
                        " - Altezza non valida"  delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-larghezza
                 initialize ferr-riga
                 string "RIGA: "              delimited size
                        el-no-larghezza(idx)  delimited size
                        " - Larghezza non valida" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform        
              perform varying idx from 1 by 1 
                        until idx > idx-no-profondita
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-profondita(idx)    delimited size
                        " - Profondita non valida" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-qta-epal
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-qta-epal(idx)      delimited size
                        " - Qta EPAL non valida" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
      *****        perform varying idx from 1 by 1 
      *****                  until idx > idx-colleg-non-trovato
      *****           initialize ferr-riga
      *****           string "RIGA: "                   delimited size
      *****                  el-colleg-non-trovato(idx) delimited size
      *****                  " - Articolo collegato non valido"
      *****                                             delimited size
      *****             into ferr-riga
      *****           end-string
      *****           write ferr-riga
      *****        end-perform   
      *****        perform varying idx from 1 by 1 
      *****                  until idx > idx-colleg-no-scorta
      *****           initialize ferr-riga
      *****           string "RIGA: "                 delimited size
      *****                  el-colleg-no-scorta(idx) delimited size
      *****                  " - Articolo collegato non valido (scorta)" 
      *****                                           delimited size
      *****             into ferr-riga
      *****           end-string
      *****           write ferr-riga
      *****        end-perform   
      *****        perform varying idx from 1 by 1 
      *****                  until idx > idx-colleg-non-valido
      *****           initialize ferr-riga
      *****           string "RIGA: "                  delimited size
      *****                  el-colleg-non-valido(idx) delimited size
      *****                  " - Articolo già collegato" 
      *****                                           delimited size
      *****             into ferr-riga
      *****           end-string
      *****           write ferr-riga
      *****        end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-dogana
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-dogana(idx)        delimited size
                        " - Codice doganale non valido" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-limite
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-limite(idx)        delimited size
                        " - Limite scorta obbligatorio" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-reale
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-reale(idx)         delimited size
                        " - Peso reale obbligatorio" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-ass-cou-errato
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-ass-cou-errato(idx) delimited size
                        " - Assoggettamento COU errato" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-ass-cob-errato
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-ass-cob-errato(idx)   delimited size
                        " - Assoggettamento COBAT errato" 
                                                 delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-peso-utf
                 initialize ferr-riga
                 string "RIGA: "             delimited size
                        el-no-peso-utf(idx)  delimited size
                        " - Peso UTF non valido" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform   
              perform varying idx from 1 by 1 
                        until idx > idx-no-peso-non-utf
                 initialize ferr-riga
                 string "RIGA: "                 delimited size
                        el-no-peso-non-utf(idx)  delimited size
                        " - Peso NON UTF non valido" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-perce-imp
                 initialize ferr-riga
                 string "RIGA: "               delimited size
                        el-no-perce-imp(idx)   delimited size
                        " - % imposte non valorizzato" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-si-perce-imp
                 initialize ferr-riga
                 string "RIGA: "               delimited size
                        el-si-perce-imp(idx)   delimited size
                        " - % imposte valorizzato" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-si-amperaggio
                 initialize ferr-riga
                 string "RIGA: "               delimited size
                        el-si-amperaggio(idx)  delimited size
                        " - Amperaggio valorizzato" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-amperaggio
                 initialize ferr-riga
                 string "RIGA: "               delimited size
                        el-no-amperaggio(idx)  delimited size
                        " - Amperaggio non valorizzato" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-no-scorta-9
                 initialize ferr-riga
                 string "RIGA: "             delimited size
                        el-no-scorta-9(idx)  delimited size
                        " - Scorta non impostata a 9" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-err-peso-utf
                 initialize ferr-riga
                 string "RIGA: "              delimited size
                        el-err-peso-utf(idx)  delimited size
                        " - Peso UTF non valido" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-err-peso-non-utf
                 initialize ferr-riga
                 string "RIGA: "                  delimited size
                        el-err-peso-non-utf(idx)  delimited size
                        " - Peso non UTF non valido" delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform
              perform varying idx from 1 by 1 
                        until idx > idx-err-pesi
                 initialize ferr-riga
                 string "RIGA: "          delimited size
                        el-err-pesi(idx)  delimited size
                        " - Peso UTF e NON UTF valorizzati" 
                                          delimited size
                   into ferr-riga
                 end-string
                 write ferr-riga
              end-perform

              display message "Operazione terminata!"
                       x"0d0a""TOTALE RIGHE: ", num-rec, 
                       x"0d0a""IMPORTATI: ", num-rec-ok,   
                       x"0d0a""NON IMPORTATI: ", num-rec-ko
                       x"0d0a"
                       "Consultare il report per ulteriori dettagli"
                       x"0d0a"path-file-err
                        title titolo
                         icon 2
           end-if.

      ***---
       CONTROLLI.
           set tutto-ok to true.
           perform CONTROLLO-CAMPI.

LUBEXX     if tutto-ok
              if art-scorta of articoli = 5
                 perform CONTROLLA-PESO
                 if errori
                    exit paragraph
                 end-if
              end-if   

              if art-peso-utf     of articoli not = 0 and
                 art-peso-non-utf of articoli not = 0
                 set err-pesi to true
                 set errori   to true
                 exit paragraph
              end-if

              if art-peso-utf     of articoli = 0 and
                 art-peso-non-utf of articoli = 0
                 set err-pesi to true
                 set errori   to true
                 exit paragraph
              end-if

              if art-peso-utf     of articoli is not numeric or
                 art-peso-non-utf of articoli is not numeric
                 set err-pesi to true
                 set errori   to true
                 exit paragraph
              end-if
                                   
              evaluate true
              when art-si-utf of articoli    
                   move 0 to art-peso-non-utf of articoli
              when art-no-utf of articoli
                   move 0 to art-peso-utf of articoli
              end-evaluate            
              add art-peso-utf of articoli  
               to art-peso-non-utf of articoli
                            giving art-peso-standard of articoli
                                     
              if art-no-imposte    of articoli and 
               ( art-perce-imposte of articoli not = 0 or
                 art-perce-cou     of articoli not = 0 )
                 set si-perce-imp to true
                 set errori to true
                 exit paragraph
              end-if
                                     
              if art-si-imposte    of articoli and 
               ( art-perce-imposte of articoli = 0 or
                 art-perce-cou     of articoli = 0 )
                 set no-perce-imp to true
                 set errori to true
                 exit paragraph
              end-if

              if art-no-cobat   of articoli and 
                 art-amperaggio of articoli not = 0
                 set si-amperaggio to true
                 set errori to true
                 exit paragraph
              end-if

              if art-si-cobat   of articoli and 
                 art-amperaggio of articoli = 0
                 set no-amperaggio to true
                 set errori to true
                 exit paragraph
              end-if

           end-if.

      ***---
       TRATTA-NUMERICI.
           move como-peso-utf               to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-peso-utf of articoli
           move como-peso-non-utf           to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-peso-non-utf of articoli
           move como-peso-standard          to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-peso-standard of articoli
           move como-prezzo-vendita         to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-prezzo-vendita of articoli
           move como-perce-sconto-agente    to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-perce-sconto-agente of articoli
           move como-prezzo-acquisto        to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-prezzo-acquisto of articoli
           move como-perce-sconto-acquisto  to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-perce-sconto-acquisto of articoli
           move como-perce-imposte          to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-perce-imposte of articoli
           move como-perce-cou              to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-perce-cou of articoli
           move como-prezzo-banco           to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-prezzo-banco of articoli
           move como-prz-min-vend           to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-prz-min-vend of articoli
           move como-moq            to NumericEdi
           perform TRATTA-NUMERICO
           move como-numero to art-moq of articoli

           |Il primo carattere dev'essere un valore da scartare ed
           |essere riempiti completamente di 13 caratteri successivi
           perform varying IdxChar from 1 by 1 
                     until IdxChar > 20
              if como-codice-ean-1(IdxChar:1) is numeric
                 move como-codice-ean-1(IdxChar:) to como-ean
                 exit perform
              end-if
           end-perform.

           move como-ean to art-codice-ean-1 of articoli.   

           |Il primo carattere dev'essere un valore da scartare ed
           |essere riempiti completamente di 13 caratteri successivi
           perform varying IdxChar from 1 by 1 
                     until IdxChar > 20
              if como-codice-ean-2(IdxChar:1) is numeric
                 move como-codice-ean-2(IdxChar:) to como-ean
                 exit perform
              end-if
           end-perform.

           move como-ean to art-codice-ean-2 of articoli.

           |Il primo carattere dev'essere un valore da scartare ed
           |essere riempiti completamente di 13 caratteri successivi
           perform varying IdxChar from 1 by 1 
                     until IdxChar > 20
              if como-codice-ean-3(IdxChar:1) is numeric
                 move como-codice-ean-3(IdxChar:) to como-ean
                 exit perform
              end-if
           end-perform.
           move como-ean to art-codice-ean-3 of articoli.

           |Il primo carattere dev'essere un valore da scartare ed
           |essere riempiti completamente di 13 caratteri successivi
           perform varying IdxChar from 1 by 1 
                     until IdxChar > 20
              if como-codice-ean-4(IdxChar:1) is numeric
                 move como-codice-ean-4(IdxChar:) to como-ean
                 exit perform
              end-if
           end-perform.
           move como-ean to art-codice-ean-4 of articoli.

           |Il primo carattere dev'essere un valore da scartare ed
           |essere riempiti completamente di 13 caratteri successivi
           perform varying IdxChar from 1 by 1 
                     until IdxChar > 20
              if como-codice-ean-5(IdxChar:1) is numeric
                 move como-codice-ean-5(IdxChar:) to como-ean
                 exit perform
              end-if
           end-perform.
           move como-ean to art-codice-ean-5 of articoli.

           call "C$JUSTIFY" using art-codice-iva of articoli, "R".
           inspect art-codice-iva of articoli 
                   replacing leading x"20" by x"30".

      ***---
       CONTROLLO-CAMPI.
           set tutto-ok to true.

           if art-descrizione1 of articoli = spaces
              set errori   to true  
              set no-descr to true
              exit paragraph
           end-if.

           move art-settore-merceologico of articoli to sme-codice.
           read tsetmerc no lock
                invalid
                set errori   to true
                set no-merce to true
                exit paragraph
           end-read.

           move art-marca-prodotto of articoli to mar-codice.
           read tmarche no lock
                invalid
                set errori   to true
                set no-marca to true
                exit paragraph
           end-read.

           move art-mag-std of articoli to mag-codice.
           read tmagaz no lock
                invalid
                set errori   to true
                set no-magaz to true
                exit paragraph
           end-read.

           move "IV"           to tbliv-codice1.
           move art-codice-iva of articoli to tbliv-codice2.
           read tivaese no lock
                invalid
                set errori to true
                set no-iva to true
                exit paragraph
            not invalid
                if tbliv-percentuale = 0    
                   set errori to true
                   set iva-no-perce to true
                   exit paragraph
                end-if
           end-read.

           move art-classe-1 of articoli to cl1-codice.
           read tcla1art no lock
                invalid
                set errori to true
                set no-classe to true
                exit paragraph
           end-read.

           move art-unita-di-misura of articoli to udm-codice.
           read tudm no lock
                invalid
                set errori to true
                set no-udm to true
                exit paragraph
           end-read.

           move art-imballo-standard of articoli to imq-codice.
           read timbalqta no lock
                invalid
                set errori to true
                set no-imballo to true
                exit paragraph
           end-read. 

           if art-udm-imballo of articoli = spaces
              set errori to true
              set no-udm-imballo to true
              exit paragraph
           end-if    

           move art-cod-doganale of articoli to nom-codice.
           read tnomen no lock
                invalid
                set errori to true
                set no-dogana to true
                exit paragraph                          
           end-read.

           if art-peso-utf of articoli = 0 and 
              art-si-utf   of articoli
              set errori to true
              set no-peso-utf to true
              exit paragraph
           end-if.

           if art-peso-non-utf of articoli = 0 and 
              art-no-utf       of articoli
              set errori to true
              set no-peso-non-utf to true
              exit paragraph
           end-if.

           set errori to true
           evaluate true 
           when nom-uguale
                if art-perce-imposte of articoli = nom-perce
                   set tutto-ok to true
                end-if
           when nom-maggiore
                if art-perce-imposte of articoli > nom-perce
                   set tutto-ok to true
                end-if
           when nom-minore
                if art-perce-imposte of articoli < nom-perce
                   set tutto-ok to true
                end-if
           when nom-mag-u
                if art-perce-imposte of articoli >= nom-perce
                   set tutto-ok to true
                end-if
           when nom-min-u
                if art-perce-imposte of articoli <= nom-perce
                   set tutto-ok to true
                end-if
           end-evaluate
           if errori
              set ass-imp-errato to true
              exit paragraph
           end-if. 
                                  
           if art-perce-cou of articoli = 0 and nom-si-ic-cou
              set ass-cou-errato to true
              set errori to true
              exit paragraph
           end-if. 
                                  
           if art-amperaggio of articoli = 0 and nom-si-cobat
              set ass-cob-errato to true
              set errori to true
              exit paragraph
           end-if.
                    
           move art-scorta of articoli to sco-codice.
           read tscorte no lock
                invalid
                set errori    to true
                set no-scorta to true
                exit paragraph
           end-read.     

      *****     if art-collegato of articoli not = 0
      *****        perform CONTROLLO-CODICE-COLLEGATO
      *****        if errori
      *****           exit paragraph
      *****        end-if
      *****     end-if. 

           if art-scorta of articoli = 5 or 
              art-scorta of articoli = 7 and
              art-moq    of articoli = 0
              set errori    to true
              set no-limite to true
              exit paragraph
           end-if.
                     
           if art-scorta     of articoli = 9 and
              art-peso-reale of articoli = 0
              set errori   to true
              set no-reale to true
              exit paragraph
           end-if.

           if art-cod-fornitore of articoli not = 0
              set cli-tipo-F to true
              move art-cod-fornitore of articoli to cli-codice
              read clienti no lock
                   invalid
                   set errori to true
                   set forn-non-valido to true
                   exit paragraph
              end-read
           end-if.

           if art-cod-desf-forn of articoli not = 0
              move art-cod-fornitore of articoli to desf-codice
              move art-cod-desf-forn of articoli to desf-prog
              read destinif no lock
                   invalid
                   set errori to true
                   set desf-non-valido to true
                   exit paragraph
              end-read
           end-if.

           move art-cod-prodener of articoli to pen-codice
           read prodener no lock
                invalid 
                if nom-si-ic-cou
                   set no-prodener to true
                   set errori to true
                   exit paragraph
                end-if
            not invalid   
                evaluate true
                when art-no-utf of articoli
                     if not pen-si-sdoppia-riga
                        set errori to true
                        set prodener-non-comp to true
                        exit paragraph
                     end-if
                when other
                     if pen-si-sdoppia-riga
                        set errori to true
                        set prodener-non-comp to true
                        exit paragraph
                     end-if
                end-evaluate                   
           end-read.

           if art-scorta  of articoli = 5 and 
              art-altezza of articoli = 0
              set errori to true
              set no-altezza to true
              exit paragraph
           end-if      

           if art-scorta    of articoli = 5 and 
              art-larghezza of articoli = 0
              set errori to true
              set no-larghezza to true
              exit paragraph
           end-if 

           if art-scorta     of articoli = 5 and 
              art-profondita of articoli = 0
              set errori to true
              set no-profondita to true
              exit paragraph
           end-if 
                  
           if art-qta-EPAL of articoli = 0 and 
              art-qta-STD  of articoli = 0
              set errori to true
              set no-qta-epal to true
              exit paragraph
           end-if.

      ***---
       SCRIVI-PROGMAG.
           set link-batch to true.
           move "IMPORT"  to link-user.
           move art-codice            of articoli to link-articolo.
           move art-descrizione       of articoli to link-des-articolo.
           move art-imballo-standard  of articoli to link-imballo
           move art-mag-std           of articoli to link-magazzino.
           move art-peso-utf          of articoli to link-utf.
           move art-peso-non-utf      of articoli to link-non-utf.
           add link-utf to link-non-utf giving link-peso.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".
           if art-mag-std of articoli not = "LBX"
              move "LBX" to link-magazzino
              call   "wprogmag" using link-wprogmag
              cancel "wprogmag"
           end-if.

      ********---
      ***** AGGIORNA-CATENA.   
      ******    per prima cosa controllo qual'era l'ultimo elemento della cartena
      *****     perform LAST-EL-CATENA.
      *****     move ultimo-elemento to old-ultimo-elemento
      *****     move last-codice     to old-last-codice
      *****
      ******    ricalcolo la catena 
      *****     call   "car-catart".
      *****     cancel "car-catart".
      *****
      ******    controllo se ci sono stati cambiamenti signigicativi nella 
      ******    catena, in tal caso spedisco la mail
      *****     perform LAST-EL-CATENA.
      *****     if ultimo-elemento > old-ultimo-elemento
      *****        if last-codice not = old-last-codice
      *****           call   "mail-catart" using art-codice of articoli
      *****           cancel "mail-catart"
      *****        end-if
      *****     end-if.
                            

      ********---
      ***** LAST-EL-CATENA.
      *****     move 0 to last-codice.
      *****     open input catart.
      *****     move art-codice of articoli to cat-codice.
      *****     move low-value  to cat-princ.
      *****     start catart key >= cat-chiave
      *****           invalid move 0 to ultimo-elemento
      *****        not invalid
      *****           read catart next no lock
      *****                at end move 0 to ultimo-elemento
      *****            not at end
      *****                if art-codice of articoli not = cat-codice
      *****                   move 0 to ultimo-elemento
      *****                else
      *****                   if cat-princ not = 0
      *****                      move cat-princ to cat-codice
      *****                      move 0         to cat-princ
      *****                      read catart
      *****                           invalid move 0 to cat-num-el-catena
      *****                      end-read
      *****                   end-if
      *****                   move cat-num-el-catena to ultimo-elemento
      *****                   if cat-num-el-catena not = 0
      *****                      move cat-collegato(cat-num-el-catena)  
      *****                                           to last-codice
      *****                   end-if
      *****                end-if
      *****           end-read
      *****     end-start.
      *****     close catart.

      ***---
       CONTROLLA-PESO.
           if art-qta-EPAL of articoli = 0
              move art-qta-STD of articoli to art-qta-EPAL of articoli
           end-if.

           compute peso-inserito =  art-peso-utf     of articoli + 
                                    art-peso-non-utf of articoli.

           compute como-peso =
               ( ( art-altezza    of articoli * 
                   art-larghezza  of articoli * 
                   art-profondita of articoli ) / 
                   1000000 * 150 ) / art-qta-EPAL of articoli.

           if como-peso > peso-inserito
              set errori      to true
              set no-scorta-9 to true
              exit paragraph
              if art-peso-utf of articoli > 0
                 if art-peso-utf of articoli not = como-peso
                    set errori   to true
                    set err-peso-utf to true
                    exit paragraph
                 end-if
              end-if   
              if art-peso-non-utf of articoli > 0
                 if art-peso-non-utf of articoli not = como-peso
                    set errori   to true
                    set err-peso-non-utf to true
                    exit paragraph
                 end-if
              end-if
           end-if.

      ********---
      ***** CONTROLLO-CODICE-COLLEGATO.
      *****     if art-collegato of articoli not = 0
      *****        move art-collegato of articoli to art-codice of articoli1
      *****        read articoli1 no lock
      *****             invalid
      *****             move art-codice of articoli1 to bli-codice
      *****             read blister no lock
      *****                  invalid
      *****                  set colleg-non-trovato to true
      *****                  set errori to true
      *****                  exit paragraph
      *****             end-read
      *****        end-read                
      *****        
      *****        if art-codice of articoli1 <= art-codice of articoli
      *****           if sco-permetti-sost-no
      *****              set errori to true  
      *****              set colleg-no-scorta to true
      *****              exit paragraph
      *****           end-if
      *****        end-if              
      *****
      *****        move art-codice of articoli  to chk-ca-art
      *****        move art-codice of articoli1 to chk-ca-collegato
      *****        call   "check-catart" using check-catart-linkage
      *****        cancel "check-catart"
      *****        if chk-ca-errore
      *****           set colleg-non-valido to true
      *****           set errori to true
      *****        end-if   
      *****     end-if.   

      ***---
       AGGIUNGI-ERRATO.
           add 1 to num-rec-ko.
           if path-file-err = spaces
              accept como-data from century-date
              accept como-ora  from time
              accept  path-file-err from environment "PATH_ST"
              inspect path-file-err replacing trailing spaces 
                                    by low-value
              string  path-file-err           delimited low-value
                      "ERR-LOG-IMP-ARTICOLI_" delimited size
                      como-data               delimited size
                      "_"                     delimited size
                      como-ora                delimited size
                      ".log"                  delimited size
                 into path-file-err
              end-string
              inspect path-file-err replacing trailing low-value
                                    by spaces
              open output file-err
           end-if.

           evaluate true
           when no-merce
                add 1 to idx-no-merce
                move riga to el-no-merce(idx-no-merce)
           when no-marca                              
                add 1 to idx-no-marca
                move riga to el-no-marca(idx-no-marca)
           when no-magaz                          
                add 1 to idx-no-magaz
                move riga to el-no-magaz(idx-no-magaz)
           when no-iva           
                add 1 to idx-no-iva
                move riga to el-no-iva(idx-no-iva)
           when iva-no-perce                     
                add 1 to idx-iva-no-perce
                move riga to el-no-merce(idx-iva-no-perce)
           when no-classe                                
                add 1 to idx-no-classe
                move riga to el-no-classe(idx-no-classe)
           when no-udm                                 
                add 1 to idx-no-udm
                move riga to el-no-udm(idx-no-udm)
           when no-imballo                       
                add 1 to idx-no-imballo
                move riga to el-no-imballo(idx-no-imballo)
           when no-udm-imballo            
                add 1 to idx-no-udm-imballo
                move riga to el-no-udm-imballo(idx-no-udm-imballo)
           when ass-imp-errato                                   
                add 1 to idx-ass-imp-errato
                move riga to el-ass-imp-errato(idx-ass-imp-errato)
           when ass-cou-errato                                   
                add 1 to idx-ass-cou-errato
                move riga to el-ass-cou-errato(idx-ass-cou-errato)
           when ass-cob-errato                                   
                add 1 to idx-ass-cob-errato
                move riga to el-ass-cob-errato(idx-ass-cob-errato)
           when no-scorta                                        
                add 1 to idx-no-scorta
                move riga to el-no-scorta(idx-no-scorta)
           when forn-non-valido                        
                add 1 to idx-forn-non-valido
                move riga to el-forn-non-valido(idx-forn-non-valido)
           when desf-non-valido  
                add 1 to idx-desf-non-valido
                move riga to el-desf-non-valido(idx-desf-non-valido)
           when no-prodener                                        
                add 1 to idx-no-prodener
                move riga to el-no-prodener(idx-no-prodener)
           when prodener-non-comp                          
                add 1 to idx-prodener-non-comp
                move riga to el-no-merce(idx-prodener-non-comp)
           when no-altezza                                    
                add 1 to idx-no-altezza
                move riga to el-no-altezza(idx-no-altezza)
           when no-larghezza                             
                add 1 to idx-no-larghezza
                move riga to el-no-larghezza(idx-no-larghezza)
           when no-profondita    
                add 1 to idx-no-profondita
                move riga to el-no-profondita(idx-no-profondita)
           when no-qta-epal                                    
                add 1 to idx-no-qta-epal
                move riga to el-no-qta-epal(idx-no-qta-epal)
           when no-descr                                   
                add 1 to idx-no-descr
                move riga to el-no-descr(idx-no-descr)
      *****     when colleg-non-trovato 
      *****          add 1 to idx-colleg-non-trovato
      *****          move riga 
      *****            to el-colleg-non-trovato(idx-colleg-non-trovato)
      *****     when colleg-no-scorta 
      *****          add 1 to idx-colleg-no-scorta
      *****          move riga 
      *****            to el-colleg-no-scorta(idx-colleg-no-scorta) 
      *****     when colleg-non-valido 
      *****          add 1 to idx-colleg-non-valido
      *****          move riga 
      *****            to el-colleg-non-valido(idx-colleg-non-valido)
           when no-dogana
                add 1 to idx-no-dogana
                move riga to el-no-dogana(idx-no-dogana)
           when si-perce-imp
                add 1 to idx-si-perce-imp
                move riga to el-si-perce-imp(idx-si-perce-imp)
           when no-perce-imp
                add 1 to idx-no-perce-imp
                move riga to el-no-perce-imp(idx-no-perce-imp)
           when si-amperaggio
                add 1 to idx-si-amperaggio
                move riga to el-si-amperaggio(idx-si-amperaggio)
           when no-amperaggio
                add 1 to idx-no-amperaggio
                move riga to el-no-amperaggio(idx-no-amperaggio)
           when no-limite
                add 1 to idx-no-limite
                move riga to el-no-limite(idx-no-limite)
           when no-reale
                add 1 to idx-no-reale
                move riga to el-no-reale(idx-no-reale) 
           when no-peso-utf                                   
                add 1 to idx-no-peso-utf
                move riga to el-no-peso-utf(idx-no-peso-utf)
           when no-peso-non-utf                                   
                add 1 to idx-no-peso-non-utf
                move riga to el-no-peso-non-utf(idx-no-peso-non-utf)
           when no-scorta-9
                add 1 to idx-no-scorta-9
                move riga to el-no-scorta-9(idx-no-scorta-9) 
           when err-peso-utf
                add 1 to idx-err-peso-utf
                move riga to el-err-peso-utf(idx-err-peso-utf)
           when err-peso-non-utf
                add 1 to idx-err-peso-non-utf
                move riga to el-err-peso-non-utf(idx-err-peso-non-utf)
           when err-pesi
                add 1 to idx-err-pesi
                move riga to el-err-pesi(idx-err-pesi)
           end-evaluate.

      ***---
       CLOSE-FILES.
           close articoli lineseq progmag.
           close tcla1art tnomen tmarche prodener clienti 
                 destinif tscorte tsetmerc tudm timbalqta tivaese
                 tmagaz articoli1 blister. |catart.
           if path-file-err not = spaces
              close file-err
           end-if.

      ***---
       EXIT-PGM.
           goback.

           copy "tratta-numerico.cpy".
