       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      comutfc.
       AUTHOR.                          Andrea.
       REMARKS. Estrazione e riepilogo su CSV dei movimenti UTF carico
                - partenza dal numero indicato in tmovtrat + 1
                - aventi causale LBX con movim giacenza positiva

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tmovtrat.sl".
           copy "tmovmag.sl".
           copy "tcaumag.sl".
           copy "tparamge.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "destini.sl".            
           copy "tordini.sl".
           copy "rordini.sl".
           copy "lineseq-mail.sl".
       SELECT logfile
           ASSIGN       TO logfile-path
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "tmovtrat.fd".
           copy "tmovmag.fd".
           copy "tcaumag.fd".
           copy "tparamge.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "destini.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "lineseq-mail.fd".
       FD  logfile.
       01 logfile-riga        PIC  x(1000).

       WORKING-STORAGE SECTION.   
       78  nomePgm               value "CARICO".
       78  titolo                value "COMUNICAZIONE UTF CARICO".
       78  78-tipo-doc-parte2    value "_DI_MILANO_3_CARICO_DEL".
       78  78-col-kg             value "      RICEVUTO KG".       
           copy "comutf.def".   
                                    

      ******************************************************************
       PROCEDURE DIVISION.       
       DECLARATIVES.
       copy "mail-decl.cpy".
      ***---  
       TMOVTRAT-ERR SECTION.
           use after error procedure on tmovtrat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovtrat
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

      ***---                   
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform LEGGI-PARAMETRI
              if tutto-ok
                 perform ELABORAZIONE
              end-if
              perform CLOSE-FILES
           end-if.                                           
           if trovato       
              accept LinkSubject
                     from environment "COMUTFC_SUBJECT"
              accept LinkBody
                     from environment "COMUTFC_BODY"
              accept LinkAddress
                     from environment "COMUTFC_ADDRESS"
              accept LinkAddressCC                    
                     from environment "COMUTFC_ADDRESS_CC"

              perform FINE-PGM-COMUNE
           end-if.
           if aggiornaContatore
              initialize como-riga
              string "Scrittura ultimo movimento trattato: " 
                                                      delimited size
                     tra-ult-mov-comun-utf-c          delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              perform AGGIORNA-CONTATORE
           end-if.
           perform EXIT-PGM.                                    
      
      ***---
       LEGGI-PARAMETRI.
           move space to tge-chiave.
           read tparamge  no lock invalid continue end-read.
           move tge-anno  to tra-anno.
           read tmovtrat  lock 
                invalid   set errori to true
            not invalid   add 1 to tra-ult-mov-comun-utf-c
           end-read.
           if RecLocked        
              move "ELABORAZIONE INTERROTTA: file movtrat lock"
                to como-riga
              perform SCRIVI-RIGA-LOG
              set errori to true
           end-if.

      ***---
       ELABORAZIONE.
           initialize como-riga.
           string "Partenza da movimento numero: " delimited size
                  tra-ult-mov-comun-utf-c          delimited size
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           move tra-anno                to tmo-anno.
           move tra-ult-mov-comun-utf-c to tmo-numero.
           start tmovmag key is >= tmo-chiave
                 invalid
                 move "NESSUN MOVIMENTO TROVATO" to como-riga
                 perform SCRIVI-RIGA-LOG
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tmovmag next no lock at end exit perform end-read
                 if tmo-anno not = tge-anno
                    exit perform 
                 end-if   
                 move tmo-numero to ult-num-movim

                 if tmo-fornitore
                    move tmo-causale to tca-codice
                    read tcaumag no lock
                         invalid continue
                     not invalid
                         if tca-movim-giac-pos and
                            tca-si-utf         and
                            tca-cod-magaz = "LBX"      
                            if tmo-peso-utf > 0    

                               initialize como-riga
                               string "Movimento n. " delimited size
                                      tmo-anno        delimited size
                                      "-"             delimited size
                                      tmo-numero      delimited size
                                      ": TROVATO PESO UTF "
                                                      delimited size
                                      tmo-peso-utf    delimited size
                                 into como-riga
                               end-string
                               perform SCRIVI-RIGA-LOG

                               if not trovato
                                  set trovato to true
                                  perform SCRIVI-TESTATA
                               end-if
                               perform SCRIVI-RIGA
                            else

                               initialize como-riga
                               string "Movimento n. " delimited size
                                      tmo-anno        delimited size
                                      "-"             delimited size
                                      tmo-numero      delimited size
                                      ": PESO UTF 0"  delimited size
                                 into como-riga
                               end-string
                               perform SCRIVI-RIGA-LOG
                            end-if
                            set aggiornaContatore to true
                            move ult-num-movim 
                              to tra-ult-mov-comun-utf-c
                         else
                            initialize como-riga
                            string "Movimento n. " delimited size
                                   tmo-anno        delimited size
                                   "-"             delimited size
                                   tmo-numero      delimited size
                                   ": SCARTATO"    delimited size
                              into como-riga
                            end-string
                            perform SCRIVI-RIGA-LOG
                         end-if
                    end-read
                 end-if

              end-perform

           end-if.                      
           
      ***---
       SCRIVI-RIGA.             
           move "SCRITTURA RIGA" to como-riga.
           perform SCRIVI-RIGA-LOG.

           if tmo-data-movim not = 0
              move tmo-data-movim(7:2) to data-x10(1:2)
              move "/"                 to data-x10(3:1)
              move tmo-data-movim(5:2) to data-x10(4:2)
              move "/"                 to data-x10(6:1)
              move tmo-data-movim(1:4) to data-x10(7:4)
           else
              move spaces to data-x10
           end-if.
           
           set cli-tipo-F     to true
           move tmo-cod-clifor to cli-codice
           read clienti no lock 
                invalid  move spaces to cli-ragsoc-1 
                         move spaces to cli-localita 
                         move spaces to cli-cap      
                         move spaces to cli-prov     
           end-read.
           move cli-codice  to desf-codice.
           move tmo-destino to desf-prog.
           read destinif no lock 
                invalid  move cli-ragsoc-1 to desf-ragsoc-1
                         move cli-localita to desf-localita
                         move cli-cap      to desf-cap
                         move cli-prov     to desf-prov
           end-read.                
           move tmo-peso-utf to tot-kg-edit.
           if tmo-numdoc-clifor not = 0
              move "BOLLA " to tipo-doc
              move tmo-numdoc-clifor to num-doc
              inspect num-doc replacing leading x"30" by x"20"
              call "C$JUSTIFY" using num-doc, "L"
           else
              move spaces   to tipo-doc
              move spaces   to num-doc
           end-if.     
           
           move data-x10 to r-data.
           initialize r-doc.
           string tipo-doc delimited size
                  num-doc  delimited size
                  into r-doc
           end-string.
                                       
           move desf-ragsoc-1 to r-ragsoc.
           move desf-localita to r-localita.
           move desf-cap      to r-cap.
           move desf-prov     to r-prov.                                      
           move tot-kg-edit to r-kg.  

           perform SCRIVI-RIGA-COMUNE.           

      ***---
       PARAGRAFO-COPY.
       copy "mail.cpy".
       copy "comutf.cpy".
