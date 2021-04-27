       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      situacont-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "tordini.sl". 
           copy "PNT.sl".
           copy "PNR.sl".
           copy "PAR.sl".
           copy "PAS.sl".  
           copy "tcodpag.sl".
           copy "TBLCO.sl".
           copy "TBLDO.sl".
           copy "TBLVA.sl".
           copy "G2.sl".
           copy "tsetinvio.sl".

       SELECT iniFtp
           ASSIGN       TO  iniFtpPath
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-iniFtp.

           copy "lineseq.sl".
           COPY "lineseq.sl"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "tordini.fd".
           copy "PNT.fd".
           copy "PNR.fd".
           copy "PAR.fd".
           copy "PAS.fd". 
           copy "tcodpag.fd".
           copy "TBLCO.fd".
           copy "TBLDO.fd".
           copy "TBLVA.fd".
           copy "G2.fd".
           copy "tsetinvio.fd".

       FD  iniFtp.
       01 iniFtp-riga        PIC  x(1000).

           copy "lineseq.fd".
           COPY "lineseq.fd"
                REPLACING ==lineseq== BY ==lineseq1==,
                          ==STATUS-lineseq== BY ==STATUS-lineseq1==.

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".
           copy "mail.def".        
           copy "acucobol.def".

       78  titolo    value "GESLUX - Sitauzione contabile".
       78  78-clear              value 
           "                                                          ".

       77  status-clienti   pic xx.
       77  status-ttipocli  pic xx.
       77  status-tordini   pic xx.  
       77  status-pnt       pic xx.
       77  status-pnr       pic xx.
       77  status-par       pic xx.
       77  status-pas       pic xx.
       77  status-tcodpag   pic xx.
       77  status-tblco     pic xx.
       77  status-tbldo     pic xx.
       77  status-tblva     pic xx.
       77  status-G2        pic xx.
       77  status-tsetinvio pic xx.
       77  status-lineseq   pic xx.
       77  status-lineseq1  pic xx.
       77  status-iniFtp    pic xx.
       77  wstampa          pic x(256). 
       77  iniFtpPath       pic x(256). 
       77  pattern          pic x(10).
       77  dir-handle       handle.

       77  path-st          pic x(256).

       77  counter          pic 9(10) value 0.
       77  counter2         pic 9(10) value 0.
       77  counter-edit     pic z(10).
                                       
       77  file-name        pic x(50). 
       77  file-backup      pic x(50).       
       77  situacont-path-fileseq pic x(200).
       77  situacont-path-backup  pic x(200).
       77  cmd              pic x(200).
                  
      * Esposto senza separatore di migliaia
       77  importo          pic ------------9,99.
       77  como-importo     pic s9(12)v99.
       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).
       77  como-anno        pic 9(4).
       77  div              pic 9(4).
       77  como-mese        pic 9(2).
       77  invio            pic 9.

       01  ftp-export.
         03 ftp-server      pic x(50).
         03 ftp-port        pic x(4).
         03 ftp-user        pic x(100).
         03 ftp-password    pic x(100).
         03 ftp-remote-dir  pic x(100).

       77  user-codi        pic x(10).
       77  tipo-documento   pic x(6).
       77  wpar-progressivo pic 9(8).
       77  narg             pic 99 comp-1.

       77  PathInvioFTP     pic x(256).
       77  StatusInvioFTP   pic s9.
       77  PathFile         pic x(256).
       77  FileName1        pic x(256).
       77  FileName2        pic x(256).

       01 como-linkage.
           03 data-from     PIC  9(8).
           03 data-to       PIC  9(8).
           03 cliente       PIC  9(8).

       01  filler           pic 9 value 0.
         88 trovato-cli     value 1, false 0.
       77  tot-clienti      pic 9(4) value 0.
       01  el-cliente       pic 9(6) occurs 9999 indexed by idx-cliente.
                                      
       01  filler           pic 9 value 0.
         88 trovato         value 1, false 0.
       01  filler           pic 9 value 0.
         88 FromBatch       value 1, false 0.
       01  filler           pic 9 value 0.
         88 trovato-par-pas value 1, false 0.

       01  controlli        pic xx.
         88 tutto-ok        value "OK".
         88 errori          value "ER".

       LINKAGE SECTION.
       copy "link-situacont.def".

      ******************************************************************
       PROCEDURE DIVISION USING sc-linkage.
       DECLARATIVES.    

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                string   "Chiudere file Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move   "File CSV"   to geslock-nome-file
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       END DECLARATIVES.

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
           accept  path-st from environment "PATH_ST".
           inspect path-st replacing trailing spaces by low-value.
           set trovato  to false.
           set tutto-ok to true.
           CALL "C$NARG" USING narg.
           if narg = 0
              set FromBatch to true
              accept como-data from century-date
              move como-data(5:2) to como-mese
              if como-mese = 1
                 move 12 to como-mese
                 move como-data(1:4) to como-anno
                 subtract 1 from como-anno
                 move como-anno to como-data(1:4)
              else
                 subtract 1 from como-mese
              end-if
              move como-mese to como-data(5:2)

              evaluate como-mese
              when 04
              when 06
              when 09
              when 11
                   move 30 to como-data(7:2)

              when 02
                   move como-data(1:4) to como-anno
                   compute div = como-anno / 4
                   compute div = div * 4
                   if div = como-anno
                      move 29 to como-data(7:2)
                   else
                      move 28 to como-data(7:2)
                   end-if
              when other
                   move 31 to como-data(7:2)

              end-evaluate

              move como-data to data-to
              move como-data to data-from
              compute data-from =
                      function integer-of-date(data-from)
              subtract 365 from data-from                      
              compute data-from =
                      function date-of-integer(data-from)

              move 0 to cliente
           else
              set FromBatch to false
              move sc-user to user-codi
              move sc-data-from to data-from
              move sc-data-to   to data-to
              move sc-cliente   to cliente
           end-if.

      ***---
       OPEN-FILES.
           initialize wstampa.
           accept  como-ora  from time.
           accept  como-data from century-date.
           string  path-st   delimited low-value
                   "M32092"       delimited size
                   como-data(1:4) delimited size
                   como-data(5:2) delimited size
                   ".csv"         delimited size
                   into wstampa
           end-string.
           inspect wstampa replacing trailing low-value by spaces.

           initialize FileName1.
           string  "M32092"       delimited size
                   como-data(1:4) delimited size
                   como-data(5:2) delimited size
                   ".csv"       delimited size
                   into FileName1
           end-string.

           open output lineseq.
           if tutto-ok
              open input tordini pnt pnr pas par tblco tcodpag tblva
                         tbldo G2 clienti ttipocli
           end-if.
      
      ***---
       ELABORAZIONE.
           move spaces to G2-chiave.
           read G2 no lock invalid continue end-read.

           move "DO"           to tbldo-codice1.
           move G2-cod-fatture to tbldo-codice2.
           read tbldo  no lock invalid continue end-read.

           move "CO"            to tblco-codice1.
           move tbldo-codice-co to tblco-codice2.
           read tblco  no lock invalid continue end-read.
           move tblco-tipo-documento to tipo-documento.

           initialize tor-rec.
           move data-from(1:4) to tor-anno-fattura.
           move data-from      to tor-data-fattura.
           start tordini key >= k4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura > data-to(1:4) or
                       tor-data-fattura > data-to
                       exit perform
                    end-if
                    perform COUNTER-VIDEO
                    if tor-num-fattura not = 0
                       if cliente = 0
                          perform CERCA-FATTURA-IN-G2
                       else
                          if cliente = tor-cod-cli
                             perform CERCA-FATTURA-IN-G2
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CERCA-FATTURA-IN-G2.                                          
           set cli-tipo-C to true.
           move tor-cod-cli to cli-codice.
           read clienti no lock invalid initialize cli-rec end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid initialize tcl-rec end-read.
           
           move low-value        to pnt-codice.
           move tor-num-fattura  to pnt-numero-documento.
           move tipo-documento   to pnt-tipo-documento.
           move tor-data-fattura to pnt-data-documento.
           start PNT key >= pnt-codice3
                 invalid continue
             not invalid
                 read PNT next
                 if pnt-numero-documento = tor-num-fattura and
                    pnt-tipo-documento   = tipo-documento  and
                    pnt-data-documento   = tor-data-fattura
                    move low-value       to pnr-riga
                    move pnt-progressivo to pnr-progressivo 
                    start PNR key >= pnr-codice
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read PNR next at end exit perform end-read
                             if pnr-progressivo not = pnt-progressivo
                                exit perform
                             end-if

                             perform COUNTER-VIDEO
                             move low-value  to record-par
                             move pnr-codice to par-codice-pnr
                             start PAR key >= par-codice1
                                   invalid continue
                               not invalid
                                   read PAR next
                                   |TROVO IL PROGRESSIVO DEL PAR E
                                   |DEVO LEGGERE PER CHIAVE PRIMARIA
                                   if par-codice-pnr not = pnr-codice
                                      move 0 to wpar-progressivo
                                   else
                                      move par-progressivo to
                                          wpar-progressivo
                                   end-if
                             end-start
                             if wpar-progressivo not = 0
                                move low-value to par-riga
                                start PAR key >= par-codice
                                      invalid continue
                                  not invalid
                                      perform until 1 = 2
                                         read PAR next
                                              at end exit perform
                                         end-read
                                         if par-progressivo not =
                                           wpar-progressivo
                                            exit perform
                                         end-if
                                         perform SCRIVI-SCADENZE
                                      end-perform
                                end-start
                             end-if
                             |SOLO UN PNR MOVIMENTA, QUINDI TROVATO
                             |UNO POSSO USCIRE DAL CICLO DI PNR
                             if trovato-par-pas
                                set trovato-par-pas to false
                                exit perform
                             end-if
                          end-perform
                    end-start
                 end-if
           end-start.

      ***---
       SCRIVI-SCADENZE. 
           set trovato-par-pas to true.
           if not trovato
              perform ACCETTA-SEPARATORE
              set trovato to true
              initialize line-riga of lineseq
              string "Cod.Cliente"              delimited size
                     separatore                 delimited size
                     "Ragione sociale"          delimited size
                     separatore                 delimited size
                     "Tipo pag."                delimited size
                     separatore                 delimited size
                     "Causale"                  delimited size
                     separatore                 delimited size
                     "Descrizione"              delimited size
                     separatore                 delimited size
                     "Anno documento"           delimited size
                     separatore                 delimited size
                     "Num.Documento"            delimited size
                     separatore                 delimited size
                     "Num.Riferimento"          delimited size
                     separatore                 delimited size
                     "Data registrazione"       delimited size
                     separatore                 delimited size
                     "Data documento"           delimited size
                     separatore                 delimited size
                     "Data scadenza"            delimited size
                     separatore                 delimited size
                     "Valuta"                   delimited size
                     separatore                 delimited size
                     "Importo +/-(dare/avere)"  delimited size
                     separatore                 delimited size
                     "Condizioni pagamento"     delimited size
                     separatore                 delimited size
                     "Causale movimento"        delimited size
                     separatore                 delimited size
                     "Tipologia cliente"        delimited size
                     separatore                 delimited size
                into line-riga of lineseq
              end-string
              write line-riga of lineseq
           end-if.
                      
      *****     fattura 39302 - 3 righe PAR 2 PAS
      *****     PAS PROGRESSIVO 480166
      *****     Sulla prima riga di PAR non ho il riferimento a PAS, infatti
      *****     il 480166 riga 1 non ha valorizzato PAR-CODICE-PAS, se uso
      *****     il progressivo ne vedrei solo 2 (ripetute poi per ogni 
      *****     lettura in questo caso ne ho 6, con la medesima descrizione
      *****     ripetuta a coppia).
      *****     Se uso la riga di PAR non ho la scadenza ed in più
      *****     come faccio a trovare eventuali piu scadenze per il pagamento


      *****     Se non no il PAS vado a leggere sul PAS ed espongo la
      *****     scadenza più alta dopodiché espongo i PAR 
                                                                           
           inspect par-descrizione1 
                   replacing trailing spaces by low-value.
           inspect par-descrizione2 
                   replacing trailing spaces by low-value.
                                                  
           move par-codice-pas to pas-codice.
           read PAS no lock
                invalid perform CERCA-SCADENZA
           end-read.

           move par-codice-pnr to pnr-codice.
           read PNR no lock 
                invalid move spaces to pnr-codice-co
           end-read.

           if PAR-DARE-AVERE-D
              move par-importo to como-importo
           else
              compute como-importo = par-importo * -1
           end-if.                            
           move como-importo to importo.

           move "PA"              to tblpa-codice1.
           move tor-cod-pagamento to tblpa-codice2.
           read tcodpag no lock invalid initialize record-tblpa end-read

           move "VA"          to tblva-codice1.
           move par-codice-va to tblva-codice2.
           read tblva  no lock invalid initialize record-tblva end-read.

           initialize line-riga of lineseq.
           string tor-cod-cli                 delimited size
                  separatore                  delimited size
                  cli-ragsoc-1                delimited size
                  separatore                  delimited size
                  tblpa-codice-tr(1)          delimited size
                  separatore                  delimited size
                  pnr-codice-co               delimited size
                  separatore                  delimited size
                  par-descrizione1            delimited low-value
                  " "                         delimited size
                  par-descrizione2            delimited low-value
                  separatore                  delimited size
      *****            aa of par-data-documento    delimited size
                  tor-data-fattura(1:4)       delimited size
                  separatore                  delimited size
                  par-numero-documento        delimited size
                  separatore                  delimited size
                  tor-num-fattura             delimited size
                  separatore                  delimited size
                  par-data-registrazione(7:2) delimited size  
                  "/"                         delimited size
                  par-data-registrazione(5:2) delimited size
                  "/"                         delimited size
                  par-data-registrazione(1:4) delimited size
                  separatore                  delimited size
                  par-data-documento(7:2)     delimited size  
                  "/"                         delimited size
                  par-data-documento(5:2)     delimited size
                  "/"                         delimited size
                  par-data-documento(1:4)     delimited size
                  separatore                  delimited size
                  pas-data-scadenza(7:2)      delimited size  
                  "/"                         delimited size
                  pas-data-scadenza(5:2)      delimited size
                  "/"                         delimited size
                  pas-data-scadenza(1:4)      delimited size
                  separatore                  delimited size
                  tblva-descrizione1          delimited size
                  separatore                  delimited size
                  importo                     delimited size
                  separatore                  delimited size
                  tblpa-descrizione1          delimited size
                  separatore                  delimited size
                  pnr-descrizione1            delimited size
                  separatore                  delimited size
                  tcl-descrizione             delimited size
                  separatore                  delimited size
                  into line-riga of lineseq
           end-string.
           write line-riga of lineseq.   

           set trovato-cli to false.
           set idx-cliente to 1.
           search el-cliente
           when el-cliente(idx-cliente) = tor-cod-cli
                set trovato-cli to true
           end-search.

           if not trovato-cli
              add 1 to tot-clienti
              move tor-cod-cli to el-cliente(tot-clienti)
           end-if.


      ***--- 
       CERCA-SCADENZA.
           move 0 to como-data.
           move par-progressivo to pas-progressivo.
           move low-value       to pas-riga.
           start PAS key >= pas-codice
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read PAS next at end exit perform end-read
                    if pas-progressivo not = par-progressivo
                       exit perform
                    end-if
                    if pas-data-scadenza > como-data
                       move pas-data-scadenza to como-data
                    end-if
                 end-perform
           end-start.
           move como-data to pas-data-scadenza.

      ***---
       COUNTER-VIDEO.
           if FromBatch exit paragraph end-if.
           add 1 to counter.
           add 1 to counter2.
           if counter2 = 500
              if counter = 500
                 display "SC"
                    upon sc-handle at column 18
                                        line 05
             end-if
              move counter to counter-edit
              display counter-edit
                 upon sc-handle at column 21
                                     line 05
              move 0 to counter2
           end-if.

      ***---
       CLOSE-FILES.
           close tordini pnt pnr par pas tblco tbldo lineseq G2 clienti
                 tcodpag tblva ttipocli.

      ***---
       EXIT-PGM.
           if trovato
              if not FromBatch
                 perform CALL-EXCEL
              end-if
              perform EXPORT-CLIENTI
              if not FromBatch
      *****           perform CALL-EXCEL
                 continue
              else
      *****           move 1 to invio
      *****           perform EXPORT-FTP
      *****           move 2 to invio
                 perform EXPORT-FTP
              end-if
           else
              delete file lineseq
              move spaces to wstampa
              move spaces to FileName1
           end-if.                  

           goback.

      ***---
       EXPORT-CLIENTI.        
           move "M*.csv"  to pattern
           perform ACCODA-FILE.
           initialize wstampa.
           accept  como-data from century-date.
           accept  como-ora  from time.
           initialize wstampa.
           string  path-st        delimited low-value
                   "C32092"       delimited size
                   como-data(1:4) delimited size
                   como-data(5:2) delimited size
                   ".csv"         delimited size
                   into wstampa
           end-string.    

           initialize FileName2.
           string "C32092"        delimited size
                   como-data(1:4) delimited size
                   como-data(5:2) delimited size
                  ".csv"          delimited size
             into FileName2
           end-string.

           inspect wstampa replacing trailing low-value by spaces.
           open output lineseq.
           open input  clienti tcodpag ttipocli.
                                                    
           perform varying idx-cliente from 1 by 1 
                     until idx-cliente > tot-clienti       
              move low-value to cli-rec
              move el-cliente(idx-cliente) to cli-codice
              set cli-tipo-C to true
              read clienti no lock
                   invalid initialize cli-rec
              end-read
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid initialize tcl-rec end-read
              perform COUNTER-VIDEO
              perform AGGIUNGI-CLIENTE
           end-perform.  
           close       clienti tcodpag ttipocli.
           close       lineseq.
           move "C*.csv"  to pattern
           perform ACCODA-FILE.  

      ***---
       ACCODA-FILE.
           accept situacont-path-fileseq 
                  from environment "SITUACONT_PATH_FILESEQ".
           inspect situacont-path-fileseq
                   replacing trailing spaces by low-value.
      * CONTROLLO L'ESISTENZA DELLE CARTELLE              

      *    cartella di export
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         situacont-path-fileseq,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle > 0
              perform until 1 = 2
                 call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                               Dir-Handle,
                                               File-Name

                 if File-Name = "." or = ".." or = ".DS_Store"
                    exit perform cycle
                 end-if
                 
                 if File-Name = spaces
                    exit perform
                 end-if

                 |ACCODO IL FILES
                 open extend lineseq
                 inspect file-name 
                         replacing trailing spaces by low-value
                 string situacont-path-fileseq delimited low-value
                        file-name              delimited low-value
                   into wstampa
                 end-string
                 open input lineseq1
                 perform until 1 = 2
                    read lineseq1 next at end exit perform end-read
                    move line-riga  of lineseq1
                      to line-riga  of lineseq
                    write line-riga of lineseq
                 end-perform       
                 close lineseq lineseq1                   
                 
                 |LO SPOSTO NEI BACKUP
                 accept como-data from century-date
                 accept como-ora  from time
                 accept situacont-path-backup
                        from environment "SITUACONT_PATH_BACKUP"
                 inspect situacont-path-backup
                         replacing trailing spaces by low-value
                 initialize file-backup
                 string situacont-path-backup delimited low-value
                        file-name             delimited low-value
                        "_"                   delimited size
                        como-data             delimited size
                        "_"                   delimited size
                        como-ora              delimited size
                   into file-backup
                 end-string                        
                 inspect file-backup
                         replacing trailing spaces by low-value
                 inspect wstampa
                         replacing trailing spaces by low-value
                 initialize cmd
                 string "move "     delimited size
                       x"22"        delimited size         
                        wstampa     delimited low-value
                       x"22"        delimited size     
                        " "         delimited size     
                       x"22"        delimited size     
                        file-backup delimited low-value
                       x"22"        delimited size     
                  into cmd
                 end-string  
                 move 0 to return-code
                 call "C$SYSTEM" using cmd, 225
                                giving return-code
                 exit perform

              end-perform
           end-if.
           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle.

      ***---
       AGGIUNGI-CLIENTE.            
           move "PA"    to tblpa-codice1.
           move cli-pag to tblpa-codice2.
           read tcodpag invalid 
                initialize tblpa-descrizione1 
           end-read.
           if idx-cliente = 1
              initialize line-riga of lineseq
              string "Codice"          delimited size
                     separatore        delimited size
                     "Ragione Sociale" delimited size
                     separatore        delimited size
                     "Indirizzo"       delimited size
                     separatore        delimited size
                     "CAP"             delimited size
                     separatore        delimited size
                     "Località"        delimited size
                     separatore        delimited size
                     "Provincia"       delimited size
                     separatore        delimited size
                     "Nazione"         delimited size
                     separatore        delimited size
                     "Codice Fiscale"  delimited size
                     separatore        delimited size
                     "Partiva IVA"     delimited size
                     separatore        delimited size
                     "Pagamento"       delimited size
                     separatore        delimited size
                     "Fido"            delimited size
                     separatore        delimited size
                     "Tipologia cliente"  delimited size
                     separatore        delimited size
                into line-riga of lineseq
              end-string
              write line-riga of lineseq
           end-if.
           move cli-fido to importo.
           initialize line-riga of lineseq.
           string cli-codice         delimited size
                  separatore         delimited size
                  cli-ragsoc-1       delimited size
                  separatore         delimited size
                  cli-indirizzo      delimited size
                  separatore         delimited size
                  cli-cap            delimited size
                  separatore         delimited size
                  cli-localita       delimited size
                  separatore         delimited size
                  cli-prov           delimited size
                  separatore         delimited size
                  cli-nazione        delimited size
                  separatore         delimited size
                  cli-codfis         delimited size
                  separatore         delimited size
                  cli-piva           delimited size
                  separatore         delimited size
                  tblpa-descrizione1 delimited size
                  separatore         delimited size
                  importo            delimited size
                  separatore         delimited size
                  tcl-descrizione    delimited size
                  separatore         delimited size
              into line-riga of lineseq
           end-string.
           write line-riga of lineseq.

      ***---
       EXPORT-FTP.     
           initialize iniFtpPath.
           accept  iniFtpPath from environment "PATH_INI_FTP".    
           open output iniFtp.

           accept ftp-server
                  from environment "SITUACONT_FTP_SERVER"
           accept ftp-port
                  from environment "SITUACONT_FTP_PORT"
           accept ftp-user
                  from environment "SITUACONT_FTP_USER"
           accept ftp-password
                  from environment "SITUACONT_FTP_PASSWORD"
           accept ftp-remote-dir
                  from environment "SITUACONT_FTP_REMOTE_DIR"
                                                             

           inspect ftp-server     replacing trailing spaces by low-value
           inspect ftp-user       replacing trailing spaces by low-value
           inspect ftp-port       replacing trailing spaces by low-value
           inspect ftp-password   replacing trailing spaces by low-value
           inspect ftp-remote-dir replacing trailing spaces by low-value
           inspect FileName1      replacing trailing spaces by low-value
           inspect FileName2      replacing trailing spaces by low-value
                                
           initialize iniFtp-riga.
           string "open ftp://" delimited size
                  ftp-user      delimited low-value
                  ":"           delimited size
                  ftp-password  delimited low-value
                  "@"           delimited size
                  ftp-server    delimited low-value
                  ":"           delimited size
                  ftp-port      delimited low-value
                  " -explicit"  delimited size
             into iniFtp-riga
           end-string.
           write iniFtp-riga.
                             
           initialize iniFtp-riga.
           string "put "         delimited size
                  path-st        delimited low-value
                  FileName1      delimited low-value
                  " "            delimited size
                  ftp-remote-dir delimited low-value
                  FileName1      delimited low-value
             into iniFtp-riga
           end-string.
           write iniFtp-riga.

           initialize iniFtp-riga.
           string "put "         delimited size
                  path-st        delimited low-value
                  FileName2      delimited low-value
                  " "            delimited size
                  ftp-remote-dir delimited low-value
                  FileName2      delimited low-value
             into iniFtp-riga
           end-string.
           write iniFtp-riga.

           move "exit" to iniFtp-riga.
           write iniFtp-riga.

           close iniFtp.

      *****     if invio = 1
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_SERVER_1"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_PORT_1"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_USER_1"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_PASSWORD_1"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_REMOTE_DIR_1"
      *****     else
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_SERVER_2"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_PORT_2"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_USER_2"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_PASSWORD_2"
      *****        write line-riga  of lineseq
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_FTP_REMOTE_DIR_2"
      *****     end-if.
      *****     write line-riga of lineseq.
      *****     move FileName1  to line-riga of lineseq.
      *****     write line-riga of lineseq.
      *****     move FileName2  to line-riga of lineseq.
      *****     write line-riga of lineseq.
      *****     move PathFile   to line-riga of lineseq.
      *****     write line-riga of lineseq.                         
      *****     if invio = 1
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_CERTIFICATE_1"
      *****     else
      *****        accept line-riga of lineseq 
      *****               from environment "SITUACONT_CERTIFICATE_2"
      *****     end-if.
      *****     write line-riga of lineseq.

      *****     close lineseq.

           accept PathInvioFTP from environment "PATH_INVIO_FTP".
           move 0 to StatusInvioFTP.
           call "C$SYSTEM" using PathInvioFTP
                          giving StatusInvioFTP.

           initialize LinkBody.
           move "INVIO FTP SITUAZIONE CONTABILE" to LinkSubject.

           move "In allegato dettaglio invio ftp. " to LinkBody.

           accept LinkAddress from environment "INVIO_FTP_ADDRESSES".

           if LinkAddress not = spaces

              move wstampa to LinkAttach
                               
              move "situacont-p" to NomeProgramma
              set trovato to false
              perform 10 times
                 perform SEND-MAIL
                 open input lineseq1
                 read lineseq1 next
                 if line-riga of lineseq1 = "True"
                    set trovato to true
                    close lineseq1
                    exit perform 
                 end-if
                 close lineseq1
              end-perform
              call "C$DELETE" using wstampa, "S"

           end-if.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
           copy "mail.cpy".
