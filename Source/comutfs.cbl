       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      comutfs.
       AUTHOR.                          Andrea.
       REMARKS. Estrazione e riepilogo su CSV delle bolle UTF scarico
                - partenza dal numero indicato in tmovtrat + 1
                - aventi causale LBX con movim giacenza negativa

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tmovmag.sl".
           copy "tmovtrat.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tcaumag.sl".
           copy "tparamge.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "destinif.sl".
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
           copy "tmovmag.fd".
           copy "tmovtrat.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tcaumag.fd".
           copy "tparamge.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "destinif.fd".
           copy "lineseq-mail.fd".

       FD  logfile.
       01 logfile-riga        PIC  x(1000).

       WORKING-STORAGE SECTION.     
       78  nomePgm               value "SCARICO".
       78  titolo                value "COMUNICAZIONE UTF SCARICO".
       78  78-tipo-doc-parte2    value "_DI_MILANO_3_SCARICO_DEL".
       78  78-col-kg             value "       SPEDITO KG".
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
                     from environment "COMUTFS_SUBJECT"
              accept LinkBody
                     from environment "COMUTFS_BODY"
              accept LinkAddress
                     from environment "COMUTFS_ADDRESS"
              accept LinkAddressCC                    
                     from environment "COMUTFS_ADDRESS_CC"

              perform FINE-PGM-COMUNE
           end-if.   
           if aggiornaContatore 
              initialize como-riga
              string "Scrittiura ultima bolla S1 trattata: " 
                                                     delimited size
                     tra-bolla-comun-utf-S1          delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              initialize como-riga
              string "Scrittiura ultima bolla S2 trattata: " 
                                                     delimited size
                     tra-bolla-comun-utf-S2          delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              initialize como-riga
              string "Scrittiura ultima bolla S3 trattata: " 
                                                     delimited size
                     tra-bolla-comun-utf-S3          delimited size
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
            not invalid   move tra-bolla-comun-UTF-S1 
                            to ult-numero-bolla-S1
                          move tra-bolla-comun-UTF-S2
                            to ult-numero-bolla-S2
                          move tra-bolla-comun-UTF-S3
                            to ult-numero-bolla-S3
                          add 1 to tra-bolla-comun-UTF-S1
                          add 1 to tra-bolla-comun-UTF-S2
                          add 1 to tra-bolla-comun-UTF-S3
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
           string "Partenza da bolla S1 numero: " delimited size
                  tra-bolla-comun-utf-s1          delimited size
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           set tutto-ok                to true.
           move tra-anno               to tor-anno-bolla.
           move tra-bolla-comun-utf-s1 to tor-num-bolla.
           move 500001                 to max-numero.
           start tordini key is >= k-bolla
                 invalid
                 move "NESSUNA BOLLA S1 TROVATA" to como-riga
                 perform SCRIVI-RIGA-LOG
                 set errori to true
           end-start.

           if tutto-ok
              move 1 to SerieBolle
              perform LOOP-BOLLE
              move ult-numero-bolla-S1 to tra-bolla-comun-utf-s1  
           end-if.     
                 
           initialize como-riga.
           string "Partenza da bolla S2 numero: " delimited size
                  tra-bolla-comun-utf-s2          delimited size
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.
                        
           set tutto-ok                to true.
           move tra-anno               to tor-anno-bolla.
           move tra-bolla-comun-utf-s2 to tor-num-bolla.
           move 99999999               to max-numero.
           start tordini key is >= k-bolla
                 invalid             
                 move "NESSUNA BOLLA S2 TROVATA" to como-riga
                 perform SCRIVI-RIGA-LOG
                 set errori to true
           end-start.

           if tutto-ok
              move 2 to SerieBolle
              perform LOOP-BOLLE
              move ult-numero-bolla-S2 to tra-bolla-comun-utf-S2 

           end-if.   
                 
           initialize como-riga.
           string "Partenza da bolla S3 numero: " delimited size
                  tra-bolla-comun-utf-s3          delimited size
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.
                              
           set tutto-ok                to true.
           move tra-anno               to tor-anno-bolla.
           move tra-bolla-comun-utf-s3 to tor-num-bolla.
           move 900001                 to max-numero.
           start tordini key is >= k-bolla
                 invalid             
                 move "NESSUNA BOLLA S3 TROVATA" to como-riga
                 perform SCRIVI-RIGA-LOG
                 set errori to true
           end-start.

           if tutto-ok
              move 3 to SerieBolle
              perform LOOP-BOLLE
              move ult-numero-bolla-S3 to tra-bolla-comun-utf-S3  
           end-if.

      ***---
       LOOP-BOLLE.
           perform until 1 = 2
              read tordini next no lock at end exit perform end-read
              if tor-anno-bolla not = tge-anno or
                 tor-num-bolla >= max-numero
                 exit perform
              end-if 

              move tor-causale to tca-codice
              read tcaumag no lock
                   invalid continue
               not invalid
                   if tca-movim-giac-neg and 
                      tca-si-utf         and
                      tca-cod-magaz = "LBX"
                      perform LOOP-RIGHE-BOLLA
                      if tot-kg > 0   
                      
                         initialize como-riga
                         string "Bolla S"             delimited size
                                SerieBolle            delimited size
                                " n. "                delimited size
                                tor-anno-bolla        delimited size
                                "-"                   delimited size
                                tor-num-bolla         delimited size
                                ": TROVATO PESO UTF " delimited size
                                tot-kg                delimited size
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
                         string "Bolla S"       delimited size
                                SerieBolle      delimited size
                                " n. "          delimited size
                                tor-anno-bolla  delimited size
                                "-"             delimited size
                                tor-num-bolla   delimited size
                                ": PESO UTF 0"  delimited size
                                tot-kg          delimited size
                           into como-riga
                         end-string
                         perform SCRIVI-RIGA-LOG
                      end-if
  
                      set aggiornaContatore to true

                      evaluate SerieBolle
                      when 1
                           move tor-num-bolla to ult-numero-bolla-S1
                      when 2
                           move tor-num-bolla to ult-numero-bolla-S2
                      when 3
                           move tor-num-bolla to ult-numero-bolla-S3
                      end-evaluate
              
                   else
                      initialize como-riga
                      string "Bolla S"       delimited size
                             SerieBolle      delimited size
                             " n. "          delimited size
                             tor-anno-bolla  delimited size
                             "-"             delimited size
                             tor-num-bolla   delimited size
                             ": SCARTATA"    delimited size
                        into como-riga
                      end-string
                      perform SCRIVI-RIGA-LOG
                   end-if
              end-read

           end-perform.

      ***---
       LOOP-RIGHE-BOLLA.    
           move 0 to tot-kg.
           move low-value    to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rordini  next at end exit perform end-read
                 if ror-anno       not = tor-anno  or
                    ror-num-ordine not = tor-numero
                    exit perform
                 end-if
                 compute tot-kg = tot-kg + ror-qta * ror-peso-utf
                 
              end-perform
           end-if.                    

      ***---
       SCRIVI-RIGA.   
           move "SCRITTURA RIGA" to como-riga.
           perform SCRIVI-RIGA-LOG.

           if tor-data-bolla not = 0
              move tor-data-bolla(7:2) to data-x10(1:2)
              move "/"                 to data-x10(3:1)
              move tor-data-bolla(5:2) to data-x10(4:2)
              move "/"                 to data-x10(6:1)
              move tor-data-bolla(1:4) to data-x10(7:4)
           else
              move spaces to data-x10
           end-if.
           
           set cli-tipo-C   to true
           move tor-cod-cli to cli-codice
           read clienti no lock 
                invalid  move spaces to cli-ragsoc-1 
                         move spaces to cli-localita 
                         move spaces to cli-cap      
                         move spaces to cli-prov     
           end-read.
           move cli-codice      to des-codice.
           move tor-prg-destino to des-prog.
           read destini  no lock
                invalid  move cli-ragsoc-1 to des-ragsoc-1
                         move cli-localita to des-localita
                         move cli-cap      to des-cap
                         move cli-prov     to des-prov
           end-read.
           move tot-kg        to tot-kg-edit.
           move "BOLLA "      to tipo-doc.
           move tor-num-bolla to num-doc.
           inspect num-doc replacing leading x"30" by x"20".
           call "C$JUSTIFY" using num-doc, "L". 

           move data-x10 to r-data.
           initialize r-doc.
           string tipo-doc delimited size
                  num-doc  delimited size
                  into r-doc
           end-string.

           move des-ragsoc-1 to r-ragsoc.
           move des-localita to r-localita.
           move des-cap      to r-cap.
           move des-prov     to r-prov.
           move tot-kg-edit  to r-kg.

           perform SCRIVI-RIGA-COMUNE.

      ***---
       PARAGRAFO-COPY.
       copy "mail.cpy".
       copy "comutf.cpy".
