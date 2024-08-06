       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      fattpag-p.
       AUTHOR.                          Andrea.
       REMARKS. il programma può essere chiamato 
                anche come batch notturno da server.
                In questo caso deve lavorare su tutti i clienti 
                per un anno sia su fatture che su note credito
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tordini.sl". 
           copy "tnotacr.sl".
           copy "PAT.sl".
           copy "PAR.sl".
           copy "PAS.sl".  
           copy "PNT.sl".
           copy "tcodpag.sl".
           copy "TBLCO.sl".
           copy "TBLDO.sl".   
           copy "G2.sl".
           copy "lineseq.sl".

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.



      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tordini.fd".
           copy "tnotacr.fd".
           copy "PAS.fd".
           copy "PAR.fd".
           copy "PAT.fd".
           copy "PNT.fd".
           copy "tcodpag.fd".
           copy "TBLCO.fd".
           copy "TBLDO.fd".   
           copy "G2.fd".
           copy "lineseq.fd".    

       FD  logfile.
       01 log-riga        PIC  x(900). 


       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def". 

       78  titolo    value "GESLUX - Sitauzione pagamento fatture".
       78  78-clear  value 
           "                                                          ".
                                    
       77  status-logfile    pic xx.
       77  status-clienti    pic xx.
       77  status-tordini    pic xx.
       77  status-tnotacr    pic xx.  
       77  status-pnt        pic xx.
       77  status-pnr        pic xx.
       77  status-par        pic xx.
       77  status-pas        pic xx.
       77  status-pat        pic xx.
       77  status-tcodpag    pic xx.
       77  status-tblco      pic xx.
       77  status-tbldo      pic xx.
       77  status-tblva      pic xx.
       77  status-G2         pic xx.
       77  status-lineseq    pic xx.
       77  wstampa           pic x(256). 
       77  path-logfile      pic x(256).   
       77  como-riga         pic x(200).
       
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  whh                   pic 99.
       77  wmm                   pic 9(5).
       77  wss                   pic 99.  

       77  n-rec                 pic 9(6).

       01  r-inizio              pic x(25).

       77  counter           pic 9(10) value 0.
       77  counter2          pic 9(10) value 0.
       77  counter-edit      pic z(10).
                             
       77  idx               pic 9(3). 
       77  como-numero-rif   pic x(12).
       77  cli-codice-x      pic x(5).
       77  num-fattura-x     pic x(8).
       77  num-fattura-x6    pic x(6).
       77  como-num-doc      pic z(8).
       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).
       77  como-stato        pic x(20).  
       77  tot-importo-dare  pic 9(9)v99.
       77  tot-importo-avere pic 9(9)v99.
       77  data-scadenza     pic 9(8).
       77  ultima-data-scad  pic 9(8).
       77  ultima-data-inc   pic 9(8).
       77  ultimo-importo    pic s9(12)v99.
       
       77  tipo-documento-tblco pic x(6).
       77  ultimo-codice-co     pic x(3).
                                              
       01  filler            pic 9 value 0.
         88 trovato          value 1, false 0.

       01  controlli         pic xx.
         88 tutto-ok         value "OK".
         88 errori           value "ER".        
       01 filler             pic  9.
           88 PagamentoBancario value 1, false 0.
       01 filler             pic 9.
           88 trovato-pagamento value 1, false 0.
       01 filler             pic 9.
           88 trovato-insoluto value 1, false 0.
       01 filler             pic x.
           88 pagato-totale   value "T".
           88 pagato-parziale value "P".  

       77  wk-data-from        pic 9(8).
       77  wk-data-to          pic 9(8).
       77  wk-cliente          pic 9(6).

       77  importo-insoluto    pic s9(9)v99.
       77  importo-insoluto-ed pic zzz.zz.zz9,99.

       77  user-codi           pic x(10).
       77  CallingPgm          pic x(20).   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.
           
       LINKAGE SECTION.
       copy "link-batch.def".
       copy "link-situacont.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage, sc-linkage.
       DECLARATIVES.    

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                if RichiamoSchedulato
                   move "FILESEQ NON TROVATO" to como-riga
                   perform SCRIVI-RIGA-LOG
                else
                   display message "File not found!"
                             x"0d0a"wstampa
                             title titolo
                              icon 3
                end-if
           when "93"
                if RichiamoSchedulato
                   initialize como-riga
                   inspect wstampa replacing trailing spaces 
                                             by low-value
                   string wstampa       delimited low-value
                          "GIA' IN USO" delimited size
                     into como-riga
                   end-string       
                   perform SCRIVI-RIGA-LOG
                   move -1 to batch-status
                   set termina to true
                else   
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
                end-if
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok                    
              perform ELABORAZIONE-FATTURE
              perform ELABORAZIONE-NOTECR
              perform CLOSE-FILES
           end-if.               
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           accept data-scadenza from century-date.
           call "C$CALLEDBY" using CallingPgm.
           if CallingPgm not = "fattpag"
              set RichiamoSchedulato to true
              move 0 to batch-status
           end-if.

      ***---
       OPEN-FILES.   
           if RichiamoSchedulato                                        
              perform OPEN-LOGFILE
              move "APERURA FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           open input tordini tnotacr pat pas par tblco pnt
                      tcodpag tbldo G2 clienti.

      ***---
       OPEN-LOGFILE.
           accept como-data  from century-date.
           accept como-ora   from time.
           initialize path-logfile.
           accept  path-logfile from environment "SCHEDULER_PATH_LOG".
           inspect path-logfile replacing trailing spaces by low-value.
           string path-logfile  delimited low-value
                  "FATTPAG_"    delimited size
                  como-data     delimited size
                  "_"           delimited size
                  como-ora      delimited size
                  ".log"        delimited size
                  into path-logfile
           end-string.
           open output logfile.
                                            
           move   como-ora(1:2) to whh.
           move   como-ora(3:2) to wmm.
           move   como-ora(5:2) to wss.

           compute start-secondi = ( whh * 3600 ) + ( wmm * 60 ) + wss.


      ***---
       COMPONI-NOME-FATTURE.
           initialize wstampa.
           if not RichiamoSchedulato
              move sc-user      to user-codi
              move sc-data-from to wk-data-from    
              move sc-data-to   to wk-data-to
              move sc-cliente   to wk-cliente  
              inspect sc-user replacing trailing spaces by low-value
              accept  wstampa   from environment "PATH_ST"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa    delimited low-value
                      "FATTPAG_" delimited size
                      sc-user    delimited low-value
                      ".csv"     delimited size
                      into wstampa
              end-string
           else                               
              accept wk-data-to from century-date

              move wk-data-to to wk-data-from
              compute wk-data-from =
                      function integer-of-date(wk-data-from)
              subtract 1825 from wk-data-from                      
              compute wk-data-from =
                      function date-of-integer(wk-data-from)

              move 0 to wk-cliente 
              accept  wstampa from environment "PATH_FATTPAG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa       delimited low-value
                      "FATTPAG.csv" delimited size
                      into wstampa
              end-string
           end-if.
           inspect wstampa replacing trailing low-value by spaces.
      
      ***---
       ELABORAZIONE-FATTURE.      
           move spaces to geslock-scelta.
           if RichiamoSchedulato
              move "ELABORAZIONE FATTURE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           set trovato  to false.
           perform COMPONI-NOME-FATTURE.
           open output lineseq.
           if termina exit paragraph end-if.

           move spaces to G2-chiave.
           read G2 no lock invalid continue end-read.

           move "DO"           to tbldo-codice1.
           move G2-cod-fatture to tbldo-codice2.
           read tbldo  no lock invalid continue end-read.

           move "CO"            to tblco-codice1.
           move tbldo-codice-co to tblco-codice2.
           read tblco  no lock invalid continue end-read.
           move tblco-tipo-documento to tipo-documento-tblco.

           move 0 to n-rec.
           initialize tor-rec.
           move wk-data-from(1:4) to tor-anno-fattura.
           move wk-data-from      to tor-data-fattura.
           start tordini key >= k4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura > wk-data-to(1:4) or
                       tor-data-fattura > wk-data-to
                       exit perform
                    end-if
                    perform COUNTER-VIDEO
                    if tor-num-fattura not = 0
                       if wk-cliente = 0
                          perform PAGAMENTO-FATTURA
                          add 1 to n-rec
                       else
                          if wk-cliente = tor-cod-cli
                             perform PAGAMENTO-FATTURA
                             add 1 to n-rec
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.     
           close lineseq.
           if trovato
              if CallingPgm = "fattpag"
                 perform CALL-EXCEL
              end-if
              if RichiamoSchedulato
                 initialize como-riga
                 string "ELABORATE " delimited size
                        n-rec        delimited size
                        " FATTURE"   delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if
           else
              if RichiamoSchedulato
                 move "NESSUNA FATTURA ELABORATA" to como-riga
                 perform SCRIVI-RIGA-LOG
              end-if
              delete file lineseq
           end-if.    

      ***---
       COMPONI-NOME-NOTECR.
           initialize wstampa.
           if not RichiamoSchedulato
              move sc-user      to user-codi
              move sc-data-from to wk-data-from    
              move sc-data-to   to wk-data-to
              move sc-cliente   to wk-cliente  
              inspect sc-user replacing trailing spaces by low-value
              accept  wstampa   from environment "PATH_ST"
              inspect wstampa   replacing trailing spaces by low-value
              string  wstampa   delimited low-value
                      "NOTECR_" delimited size
                      sc-user   delimited low-value
                      ".csv"    delimited size
                      into wstampa
              end-string
           else
              accept wk-data-to from century-date

              move wk-data-to to wk-data-from
              compute wk-data-from =
                      function integer-of-date(wk-data-from)
              subtract 3650 from wk-data-from                      
              compute wk-data-from =
                      function date-of-integer(wk-data-from)

              move 0 to wk-cliente 
              accept  wstampa from environment "PATH_FATTPAG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa      delimited low-value
                      "NOTECR.csv" delimited size
                      into wstampa
              end-string
           end-if.
           inspect wstampa replacing trailing low-value by spaces.
      
      ***---
       ELABORAZIONE-NOTECR.    
           move spaces to geslock-scelta.
           if RichiamoSchedulato
              move "ELABORAZIONE NOTE CR" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           set trovato  to false.
           perform COMPONI-NOME-NOTECR.
           open output lineseq.
           if termina exit paragraph end-if.

           move spaces to G2-chiave.
           read G2 no lock invalid continue end-read.

           move "DO"      to tbldo-codice1.
           move G2-cod-nc to tbldo-codice2.
           read tbldo  no lock invalid continue end-read.

           move "CO"            to tblco-codice1.
           move tbldo-codice-co to tblco-codice2.
           read tblco  no lock invalid continue end-read.
           move tblco-tipo-documento to tipo-documento-tblco.

           move 0 to n-rec.
           initialize tno-rec.
           move wk-data-from(1:4) to tno-anno-fattura.
           move wk-data-from      to tno-data-fattura.
           start tnotacr key >= k4
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read
                    if tno-anno-fattura > wk-data-to(1:4) or
                       tno-data-fattura > wk-data-to
                       exit perform
                    end-if
                    perform COUNTER-VIDEO
                    if tno-num-fattura not = 0
                       if wk-cliente = 0
                          perform PAGAMENTO-NOTA
                          add 1 to n-rec
                       else
                          if wk-cliente = tor-cod-cli
                             perform PAGAMENTO-NOTA
                             add 1 to n-rec
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.   
           close lineseq.
           if trovato
              if CallingPgm = "fattpag"
                 perform CALL-EXCEL
              end-if         
              if RichiamoSchedulato
                 initialize como-riga
                 string "ELABORATE " delimited size
                        n-rec        delimited size
                        " NOTE CR"   delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if
           else                  
              if RichiamoSchedulato
                 move "NESSUNA FATTURA ELABORATA" to como-riga
                 perform SCRIVI-RIGA-LOG
              end-if
              delete file lineseq
           end-if.    

      ***---
       PAGAMENTO-FATTURA.
           set cli-tipo-C to true.
           move tor-cod-cli to cli-codice.
           read clienti no lock invalid move spaces to cli-rec end-read.

           move "PA"              to tblpa-codice1.
           move tor-cod-pagamento to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.
           inspect tblpa-descrizione1 
                   replacing trailing spaces by low-value.
                                              
           set PagamentoBancario to false.
           perform varying idx from 1 by 1 until idx > 36
              if tblpa-codice-tr(idx) = "W"
                 set PagamentoBancario to true
                 exit perform
              end-if
           end-perform.
           
           |DEVO FARLO COMUNQUE PER RISALIRE ALLA SCADENZA         
           move tor-num-fattura   to como-num-doc.
           move tor-data-fattura  to como-data.   

           move tor-cod-cli       to cli-codice-x.   
           perform CONTROLLA-PAGAMENTO.

           if PagamentoBancario
              move "RI.BA." to como-stato
           end-if.
           perform SCRIVI-RIGA-FATTURA.

      ***---
       PAGAMENTO-NOTA.
           set cli-tipo-C to true.
           move tno-cod-cli to cli-codice.
           read clienti no lock invalid move spaces to cli-rec end-read.

           move "PA"              to tblpa-codice1.
           move tno-cod-pagamento to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.
           inspect tblpa-descrizione1 
                   replacing trailing spaces by low-value.
                                              
           set PagamentoBancario to false.
           perform varying idx from 1 by 1 until idx > 36
              if tblpa-codice-tr(idx) = "W"
                 set PagamentoBancario to true
                 exit perform
              end-if
           end-perform.
           
           |DEVO FARLO COMUNQUE PER RISALIRE ALLA SCADENZA         
           move tno-num-fattura   to como-num-doc.
           move tno-data-fattura  to como-data.

           move tno-cod-cli       to cli-codice-x.
           perform CONTROLLA-PAGAMENTO.

           if PagamentoBancario
              move "RI.BA." to como-stato
           end-if.
           perform SCRIVI-RIGA-NOTA.

      ***---
       CONTROLLA-PAGAMENTO.     
           set trovato-pagamento to false.
           set trovato-insoluto  to false.
           move como-num-doc             to num-fattura-x.
           inspect num-fattura-x  replacing leading x"30" by x"20".
           call "C$JUSTIFY"           using num-fattura-x, "L".
           move num-fattura-x            to num-fattura-x6.
           call "C$JUSTIFY"           using num-fattura-x6, "R".
           inspect num-fattura-x6 replacing leading x"20" by x"30".
           
           initialize como-numero-rif.
           string num-fattura-x6       delimited size
                  tipo-documento-tblco delimited size
                  into como-numero-rif
           end-string.
           set pat-tipo-cfm-cli to true.
           call "C$JUSTIFY"         using cli-codice-x, "R".
           inspect cli-codice-x replacing leading x"20" by x"30".
           move cli-codice-x    to pat-codice-cfm.
           move como-data       to pat-data-riferimento.
           move como-numero-rif to pat-numero-riferimento.
           move low-value       to pat-codice.
           start pat key >= pat-codice1
                 invalid move 0 to pat-importo-saldo
             not invalid
                 read pat next 
                      at end continue
                  not at end
                      if pat-numero-riferimento = como-numero-rif and
                         pat-data-riferimento   = como-data       and
                         pat-codice-cfm         = cli-codice-x    and
                         pat-tipo-cfm-cli
                         set trovato-pagamento to true
                         if pat-importo-saldo = 0
                            set pagato-totale  to true
                         else
                            set pagato-parziale to true
                         end-if
                      end-if
                 end-read
           end-start.

           if trovato-pagamento
              move 0 to tot-importo-dare
              move 0 to tot-importo-avere  

              move pat-progressivo to pas-progressivo
              move 0               to pas-riga ultima-data-scad
              start PAS key >= pas-codice
                    invalid continue
              end-start
              perform until 1 = 2
                 read PAS next at end exit perform end-read
                 if pas-progressivo not = pat-progressivo
                    exit perform
                 end-if
                 if PAS-DATA-SCADENZA > ultima-data-scad
                    move pas-data-scadenza to ultima-data-scad
                 end-if
              end-perform

              |1. trovo l'ultimo incasso
              move pat-progressivo to par-progressivo
              move 0               to par-riga ultima-data-inc
              move spaces          to ultimo-codice-co
              start PAR key >= par-codice
                    invalid continue
              end-start
              perform until 1 = 2
                 read PAR next at end exit perform end-read
                 if par-progressivo not = pat-progressivo
                    exit perform
                 end-if
                 if par-data-registrazione > ultima-data-inc 
                    move par-codice-pnr-progressivo 
                      to pnt-progressivo
                    read pnt no lock 
                         invalid continue
                     not invalid
                         move "CO"          to tblco-codice1
                         move pnt-codice-co to tblco-codice2
                         read tblco no lock
                              invalid continue
                          not invalid       
                              if par-dare-avere-a
                                 move pnt-codice-co to ultimo-codice-co
                                 move par-data-registrazione 
                                   to ultima-data-inc
                                 move par-importo   to ultimo-importo
                              end-if
                         end-read
                    end-read
                 end-if
              end-perform

      *       |2. calcolo il totale del documento
              move pat-progressivo to par-progressivo
              move 0               to par-riga    
              move 0               to importo-insoluto
              start PAR key >= par-codice
                    invalid continue
              end-start
              perform until 1 = 2
                 read PAR next at end exit perform end-read
                 if par-progressivo not = pat-progressivo
                    exit perform
                 end-if
                 move par-codice-pnr-progressivo 
                   to pnt-progressivo
                 read pnt no lock 
                      invalid continue
                  not invalid
                      move "CO"          to tblco-codice1
                      move pnt-codice-co to tblco-codice2
                      read tblco no lock
                           invalid continue
                       not invalid
                           if par-dare-avere-d and 
                              pnt-codice-co = "INS"
                              add par-importo to importo-insoluto
                              set trovato-insoluto to true
                           end-if
                           if par-dare-avere-a and 
                            ( pnt-codice-co = "IN3" or
                              pnt-codice-co = "IN4" or
                              pnt-codice-co = "INC" or
                              pnt-codice-co = "PP"  or
                              pnt-codice-co = "PAG" or
                              pnt-codice-co = "AFC")
                              subtract par-importo 
                                  from importo-insoluto
                           end-if
                      end-read
                 end-read
              end-perform
              
              if not PagamentoBancario
                 if pagato-totale
                    move "COMPLETO" to como-stato
                 else
                    move pat-progressivo to pas-progressivo
                    move 0               to pas-riga
                    start PAS key >= pas-codice
                          invalid continue
                    end-start
                    perform until 1 = 2
                       read PAS next at end exit perform end-read
                       if pas-progressivo not = pat-progressivo
                          exit perform
                       end-if
                       if PAS-DATA-SCADENZA > data-scadenza
                          exit perform
                       end-if
                       add pas-importo-dare  to tot-importo-dare
                       add pas-importo-avere to tot-importo-avere
                    end-perform
                    if tot-importo-dare  = 0 and
                       tot-importo-avere = 0
                       move "NON SCADUTO" to como-stato
                    else
                       move "SCADUTO"     to como-stato
                    end-if
                 end-if
              end-if
           else
              move "NON TROVATO" to como-stato 
           end-if.
                          
      ***---
       SCRIVI-RIGA-FATTURA. 
           if not trovato
              perform ACCETTA-SEPARATORE
              set trovato to true
              initialize line-riga
              string "Data fattura"     delimited size
                     separatore         delimited size
                     "Num. fattura"     delimited size
                     separatore         delimited size
                     "Cod.Cliente"      delimited size
                     separatore         delimited size
                     "Ragione Sociale"  delimited size
                     separatore         delimited size
                     "Tipo pag."        delimited size
                     separatore         delimited size
                     "Descrizione"      delimited size
                     separatore         delimited size
                     "Stato pagamento"  delimited size
                     separatore         delimited size
                     "Ultima scadenza"  delimited size
                     separatore         delimited size
                     "Ultimo incasso"   delimited size
                     separatore         delimited size
                     "INSOLUTO"         delimited size
                     separatore         delimited size
                into line-riga
              end-string
              write line-riga
           end-if.  
                                                            
           if importo-insoluto > 0
              move "INSOLUTO" to como-stato
           else
              move 0 to importo-insoluto
              if pat-importo-saldo not = 0 and not trovato-insoluto
                 move pat-importo-saldo to importo-insoluto
              end-if
           end-if.
           move importo-insoluto to importo-insoluto-ed.

           initialize line-riga.                      
           string tor-data-fattura(7:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(5:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(1:4) delimited size
                  separatore            delimited size
                  tor-num-fattura       delimited size
                  separatore            delimited size
                  tor-cod-cli           delimited size
                  separatore            delimited size
                  cli-ragsoc-1          delimited size
                  separatore            delimited size
                  tor-cod-pagamento     delimited size
                  separatore            delimited size     
                  tblpa-descrizione1    delimited low-value
                  tblpa-descrizione2    delimited size
                  separatore            delimited size
                  como-stato            delimited size
                  separatore            delimited size
                  ultima-data-scad(7:2) delimited size
                  "/"                   delimited size
                  ultima-data-scad(5:2) delimited size
                  "/"                   delimited size
                  ultima-data-scad(1:4) delimited size
                  separatore            delimited size
                  ultima-data-inc(7:2)  delimited size
                  "/"                   delimited size
                  ultima-data-inc(5:2)  delimited size
                  "/"                   delimited size
                  ultima-data-inc(1:4)  delimited size
                  separatore            delimited size
                  importo-insoluto-ed   delimited size
                  separatore            delimited size
             into line-riga
           end-string.
           write line-riga.

      ***---
       SCRIVI-RIGA-NOTA.
           if not trovato
              perform ACCETTA-SEPARATORE
              set trovato to true
              initialize line-riga
              string "Data N.C."        delimited size
                     separatore         delimited size
                     "Num. N.C."        delimited size
                     separatore         delimited size
                     "Cod.Cliente"      delimited size
                     separatore         delimited size
                     "Ragione Sociale"  delimited size
                     separatore         delimited size
                     "Tipo pag."        delimited size
                     separatore         delimited size
                     "Descrizione"      delimited size
                     separatore         delimited size
                     "Stato pagamento"  delimited size
                     separatore         delimited size
                     "Ultima scadenza"  delimited size
                     separatore         delimited size
                     "Ultimo incasso"   delimited size
                     separatore         delimited size
      *****               "INSOLUTO"         delimited size
      *****               separatore         delimited size
                into line-riga
              end-string
              write line-riga
           end-if.  

      *****     move importo-insoluto to importo-insoluto-ed.
           if importo-insoluto not = 0
              move "INSOLUTO" to como-stato
           end-if.
           initialize line-riga.                      
           string tno-data-fattura(7:2) delimited size
                  "/"                   delimited size
                  tno-data-fattura(5:2) delimited size
                  "/"                   delimited size
                  tno-data-fattura(1:4) delimited size
                  separatore            delimited size
                  tno-num-fattura       delimited size
                  separatore            delimited size
                  tno-cod-cli           delimited size
                  separatore            delimited size
                  cli-ragsoc-1          delimited size
                  separatore            delimited size
                  tno-cod-pagamento     delimited size
                  separatore            delimited size     
                  tblpa-descrizione1    delimited low-value
                  tblpa-descrizione2    delimited size
                  separatore            delimited size
                  como-stato            delimited size
                  separatore            delimited size
                  ultima-data-scad(7:2) delimited size
                  "/"                   delimited size
                  ultima-data-scad(5:2) delimited size
                  "/"                   delimited size
                  ultima-data-scad(1:4) delimited size
                  separatore            delimited size
                  ultima-data-inc(7:2)  delimited size
                  "/"                   delimited size
                  ultima-data-inc(5:2)  delimited size
                  "/"                   delimited size
                  ultima-data-inc(1:4)  delimited size
                  separatore            delimited size
      *****            importo-insoluto-ed   delimited size
      *****            separatore            delimited size
             into line-riga
           end-string.
           write line-riga.

      ***---
       COUNTER-VIDEO.
           add 1 to counter.
           add 1 to counter2.
           if counter2 = 500
              move counter to counter-edit

              if RichiamoSchedulato
                 display counter-edit
                    upon batch-win-handle
                    line 25,00
                  column 38,00
              else
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
              end-if
              move 0 to counter2
           end-if.

      ***---
       CLOSE-FILES.
           if RichiamoSchedulato
              move "CHIUSURA FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           close tordini tnotacr pas tblco tbldo pnt
                 G2 clienti tcodpag pat par.

      ***---
       EXIT-PGM.
           if RichiamoSchedulato

              move 0 to tot-secondi
              accept como-ora from time
              move como-ora(1:2) to whh
              move como-ora(3:2) to wmm
              move como-ora(5:2) to wss

              compute end-secondi = (whh * 3600) + (wmm * 60) + wss
              compute tot-secondi = end-secondi - start-secondi

              if tot-secondi < 60
                 move tot-secondi to wss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                        wss, " SECONDI" delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              else
                 divide tot-secondi by 60 giving wmm remainder wss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                         wmm, " MINUTI E ", wss, " SECONDI" 
                         delimited size
                    into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if

              close logfile
           end-if.

           goback.                 

      ***---
       SCRIVI-RIGA-LOG.
           if path-logfile = spaces exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize log-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into log-riga
           end-string.
           write log-riga.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
           copy "setta-inizio-riga.cpy".
