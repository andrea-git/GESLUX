       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      sttendenza-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.        

       select logFile
           assign       to logFilePath
           organization is line sequential
           access mode  is sequential
           file status  is status-logFile.

           copy "lineseq.sl".
           copy "statsett.sl".
           copy "tmarche.sl".
           copy "ttipocli.sl".
           copy "tmp-tendenza.sl".
           copy "tparamge.sl".
           copy "tcontat.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.  

       FD  logFile.
       01 logFile-riga     PIC  x(900).

           copy "lineseq.fd". 
           copy "statsett.fd".
           copy "tmarche.fd".
           copy "ttipocli.fd".
           copy "tmp-tendenza.fd".
           copy "tparamge.fd".
           copy "tcontat.fd".

       WORKING-STORAGE SECTION.     

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".   
       77  nargs                 pic 99 comp-1 value 0.
       77  como-anno            pic 9(4).

       copy "statsett.def".      
       77  logFilePath          pic x(256).
       77  status-logFile       pic xx.
       77  status-tmp-tendenza  pic xx.
       77  status-tparamge      pic xx.
       77  status-tcontat       pic xx.
       77  path-tmp-tendenza    pic x(256). 
       77  riga-log             pic x(200).

      * COSTANTI
       78  titolo       value "DIREZIONALE: Statistica giornaliera NEW".

       01  filler               pic 9.
           88 RicalcoloNotturno value 1, false 0.

       01  filler               pic 9.
           88 trovato-contab    value 1, false 0.

       01 statraff-tipo         pic  x.
           88 statraff-mensile  value is "M". 
           88 statraff-cumulato value is "C". 

       77  statraff-mese-a  pic 99.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.   

       LINKAGE SECTION.
       77  link-mese        pic 99.
       77  link-data        pic 9(8).

       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING link-mese link-data batch-linkage.

       DECLARATIVES.

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                set errori to true
           when "39"
      *****          display message "File [TMARCHE] Mismatch size!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           when "98"
      *****          display message "[TMARCHE] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           end-evaluate.
      
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
      *****          display message "Impossibile procedere."
      *****            x"0d0a""File tipologie clienti [TTIPOCLI] inesistente"
      *****                    title titolo
      *****                     icon 2
                set errori to true
           when "39"
      ****          display message "File [TTIPOCLI] Mismatch size!"
      ****                    title titolo
      ****                     icon 3
                set errori to true
           when "98"
      *****          display message "[TTIPOCLI] Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           end-evaluate.  

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
      *****          display message "File Mismatch size!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           when "98"
      *****          display message "Indexed file corrupt!"
      *****                    title titolo
      *****                     icon 3
                set errori to true
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "Lineseq"    to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
      *****               perform OPEN-OUTPUT-LINESEQ
      *****          when termina
                     set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.

       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
      *****          display message "File [STATSETT] not found!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "39"
      *****          display message "File [STATSETT] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "98"
      *****          display message "[STATSETT] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "statsett"   to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
      *****               open input statsett
      *****          when termina
                     set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate 
       END DECLARATIVES.

      ***--- 
       MAIN-PRG.    
           |Mi serve solamente per recuperare la data di consolid. mag.
           open  input tparamge.
           move  spaces to tge-chiave.
           read  tparamge.
           close tparamge.
           set RichiamoSchedulato to false.
           call "C$NARG" using nargs. 
           if nargs > 2
              accept como-data from century-date
              accept como-ora  from time
              set RichiamoSchedulato to true
              move 0 to batch-status  
              accept logFilePath from environment "SCHEDULER_PATH_LOG"
              inspect logFilePath replacing trailing spaces by low-value
              string  logFilePath    delimited low-value
                      "ST-TENDENZA_" delimited size
                      "_"            delimited size
                      como-data      delimited size
                      "_"            delimited size
                      como-ora       delimited size
                      ".log"         delimited size
                      into logFilePath
              end-string
              open output logFile
              move "Inizio programma" to riga-log
              perform RIGA-LOG
           end-if.

           set statraff-mensile  to true.
           set RicalcoloNotturno to true.
           perform INIT.
           perform OPEN-FILES.
           open input tparamge tcontat.
           if tutto-ok        

              initialize riga-log
              string "Creata stampa statisiche mese "
                     link-mese
                     " in: "
                     wstampa
                into riga-log
              end-string
              perform RIGA-LOG

              move "Controllo presenza contabilizzazione" to riga-log
              perform RIGA-LOG

              perform CONTROLLA-PRESENZA-CONTAB
              if trovato-contab    
                 move "Contabilizzazione trovata" to riga-log
                 perform RIGA-LOG
                 perform ELABORAZIONE
              else
                 move "Contabilizzazione NON trovata" to riga-log
                 perform RIGA-LOG
                 move 1 to batch-status
              end-if
              perform CLOSE-FILES
              close       tmp-tendenza
              delete file tmp-tendenza
              close tparamge tcontat
           end-if.

           perform EXIT-PGM.
                      

      ***---
       ELABORAZIONE.
           call   "caldelta-p" using path-tmp-tendenza, 
                                     link-mese,
                                     link-data.
           cancel "caldelta-p".
           open input tmp-tendenza.

           set statraff-mensile to true.
           move  low-value to sts-rec.
           move  link-mese to sts-mese.
           start statsett key is >= k-ord
                 invalid set errori to true
           end-start.

           if tutto-ok

              perform until 1 = 2
                 read statsett next no lock
                      at end   perform SCRIVI-TOTALI-MESE
                               perform SCRIVI-TOTALI-PERIODO
                               perform SCRIVI-TOTALI-GENERALI
                               exit perform
                 end-read

                 if sts-mese not = link-mese
                    perform SCRIVI-TOTALI-MESE
                    perform SCRIVI-TOTALI-PERIODO
                    perform SCRIVI-TOTALI-GENERALI
                    exit perform
                 end-if
                 
                 if sts-tipocli not = SaveTipo
                    if SaveTipo not = spaces
                       perform SCRIVI-TOTALI-MESE
                       perform SCRIVI-TOTALI-PERIODO
                       move sts-tipocli to SaveTipo
                       perform SALTO-PAGINA
                       perform SCRIVI-INTESTAZIONE
                    else
                       move sts-tipocli to SaveTipo
                    end-if
                 end-if
                 
                 if prima-volta
                    perform SCRIVI-INTESTAZIONE
                    set prima-volta to false
                 end-if

                 perform VALORIZZA-OCCURS

                 move sts-marca    to tmdt-marca
                 move sts-tipocli  to tmdt-tipocli
                 read tmp-tendenza no lock 
                      invalid
                      move 0 to sts-adeguam-corr
                  not invalid
                      move tmdt-tendenza to sts-adeguam-corr
                 end-read

                 if sts-fat-corr      not = 0 or
                    sts-csm-corr      not = 0 or
                    sts-kg-corr       not = 0 or
                    sts-adeguam-corr  not = 0 or
                    sts-fat-past      not = 0 or
                    sts-csm-past      not = 0 or
                    sts-kg-past       not = 0 or
                    sts-adeguam-past  not = 0
                    move sts-fat-corr to sts-csm-corr
                    perform SCRIVI-RIGA
                 end-if

              end-perform

           end-if.

      *****     if not trovato
      *****        display message "Nessun dato per il mese richiesto!"
      *****                  title titolo
      *****                   icon 2
      *****     end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           move "Ricalcolo del: "            to fil-ric.
           move " Ultima Fatturazione del: " to fil-fatt.
           move 0 to con-ult-stampa-fatt. 
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno to con-anno.
           read tcontat  no lock invalid continue end-read.
           move con-ult-stampa-fatt(7:2) to st-data-fatt(1:2).
           move "/"                      to st-data-fatt(3:1).
           move con-ult-stampa-fatt(5:2) to st-data-fatt(4:2).
           move "/"                      to st-data-fatt(6:1).
           move con-ult-stampa-fatt(1:4) to st-data-fatt(7:4).
           move link-data(7:2)           to st-data-rical(1:2).
           move "/"                      to st-data-rical(3:1).
           move link-data(5:2)           to st-data-rical(4:2).
           move "/"                      to st-data-rical(6:1).
           move link-data(1:4)           to st-data-rical(7:4).

           move "(Dati mese RICALCOLATI)"    to tit-tipo-stampa.
           perform SCRIVI-INTESTAZIONE-COMMON.  

      ***---
       RIGA-LOG.
           if not RichiamoSchedulato exit paragraph end-if.
           initialize logFile-riga.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  riga-log  delimited size
             into logFile-riga
           end-string.   
           write logFile-riga.

      ***---
       EXIT-PGM.
           move "Fine programma" to riga-log
           perform RIGA-LOG
           if RichiamoSchedulato
              close logFile
           end-if.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "statsett.cpy".
           copy "controlla-presenza-contab.cpy".
           copy "setta-inizio-riga.cpy".
