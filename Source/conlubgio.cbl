       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conlubgio.
       AUTHOR.                          Andrea.
       REMARKS. Ricezione in automatico ogni gg un csv.
           il geslux deve:
           - associare numero bolla e data bolla all'
             evasione riportata nel csv
           - se tutti i controlli vanno a buon fine:
      * prenotare l'evasione per la fatturazione
           - se non vanno a buon fine:
      * mandare una mail con riportati i dettagli di evasione/bolla che non è riuscito a prenotare
           OGGETTO DELLA MAIL: ERRORE PRENOTAZIONE BOLLE SINTECO
           NEL TESTO RIPORTARE LA LISTA
           terminata l'elaborazione il file va tagliato 
           ed incollato in una cartella di backup.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          
           copy "lineseq.sl".    
           copy "tordini.sl".    
           copy "rordini.sl".     
           copy "tsetinvio.sl".   

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.    
       
       select file-log
           assign       to wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-file-log.
       
       select fittizio
           assign       to wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-fittizio.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "lineseq.fd".       
           copy "tordini.fd".       
           copy "rordini.fd".   
           copy "tsetinvio.fd".   

       FD  lineseq1.
       01 line-riga        pic x(900).  

       FD  file-log.
       01 log-riga         pic x(900).

       FD  fittizio.
       01 fittizio-riga    pic x.

       WORKING-STORAGE SECTION.
           copy "mail.def".    
           copy "acucobol.def". 
           copy "link-bprenf.def". 
           copy "link-wprogmag.def".

       01 tipo-errore      PIC  x.
           88 errore-prezzo VALUE IS "P". 
           88 errore-omaggio VALUE IS "O". 
           88 errore-iva-no-E15 VALUE IS "E". 
           88 errore-imposte VALUE IS "I". 
           88 errore-contatore VALUE IS "C". 
           88 errore-mese-bolla VALUE IS "M". 
           88 errore-prezzo-master VALUE IS "R". 
           88 errore-prog-master VALUE IS "X". 
           88 errore-E15-no-zero VALUE IS "W". 
           88 errore-iva-020 VALUE IS "V". 
           88 errore-iva-021 VALUE IS "Y". 
           88 errore-totale-0 VALUE IS "0". 
                                         
       77  status-tordini       pic xx.  
       77  status-rordini       pic xx.     
       77  status-tsetinvio     pic xx.     
       77  status-fittizio      pic xx.     
       77  status-lineseq       pic xx.
       77  status-file-log      pic xx.
       77  wstampa              pic x(256).   
                                           
       77  num-bolla            pic 9(8).
       77  data-bolla           pic 9(8).
       77  nargs                pic 99  comp-1 value 0.   

       77  cmd                  pic x(600).
       77  riga                 pic 9(5).      
                                                 
       77  n-gia-bolla          pic 9(5) value 0.
       77  n-no-ordine          pic 9(5) value 0.
       77  n-no-bolla           pic 9(5) value 0.
       77  n-associate          pic 9(5) value 0.
       77  n-prenotate          pic 9(5) value 0.
                                             
       77  status-call          signed-short.

       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler              pic x(2)  value "] ".

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.       
       77  tentativi             pic 99.  

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).
                                            
       77  como-riga             pic x(300).
       77  como-messaggio        pic x(300).
       77  msg-err-pren          pic x(100).
                                            
       77  path-import           pic x(200).
       77  path-backup           pic x(200).
       77  nome-file             pic x(200).
       77  file-backup           pic x(200).

       01  LUREC.
         05 LUNBOL               pic x(20).
         05 LUDBOL               pic x(20).
         05 LUAORD               pic x(20).
         05 LUNORD               pic x(20).
         05 LURFOR               pic x(20).
         05 LUDORD               pic x(20).
         05 LUFLAG               pic x(20).

       77  filler                pic 9.
           88  corretto                value 0.
           88  ordf-not-found          value 1.
           88  teva-not-found          value 2.
           88  prog-rmo-rof-diversi    value 3.
           88  prog-rmo-reva-diversi   value 4.
           88  prog-reva-rof-diversi   value 5.
           88  qta-rmo-reva-diversi    value 6.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".         

       77  filler                pic 9.
           88 errori-elab        value 1, false 0.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.      

       77  pattern               pic x(50)   value spaces.
       77  dir-Handle            handle.
       77  MSG-Folder-Name       pic x(256)  value spaces.

       77  n-file                pic 9(5) value 0.
       77  idx-err               pic 9(5) value 0.
       01  body-mess.
         05 body-mess-riga       occurs 99.
            10 el-body-mess      pic x(300). 
            10 filler            pic x(5) value x"0d0a".

       LINKAGE SECTION.
           copy "link-batch.def".
      

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                move "File [TORDINI] inesistente!" to como-messaggio
                perform COMPONI-RIGA-LOG
                set errori to true
                move -1 to batch-status
           when "39"
                move "File [TORDINI] mismatch size!" to como-messaggio
                perform COMPONI-RIGA-LOG
                set errori to true      
                move -1 to batch-status
           when "98"
                move "[TORDINI] indexed file corrupt!" to como-messaggio
                perform COMPONI-RIGA-LOG
                set errori to true      
                move -1 to batch-status
           when "93" 
                move "FILE TORDINI IN USO" to como-messaggio
                perform COMPONI-RIGA-LOG
                move -1 to batch-status
                set errori to true
           when "99" 
                initialize como-messaggio
                string tor-anno                  delimited size
                       " - "                     delimited size
                       tor-numero                delimited size
                       ": RECORD TORDINI IN USO" delimited size
                  into como-messaggio
                end-string
                perform COMPONI-RIGA-LOG
                move 1 to batch-status
                set errori to true
           end-evaluate. 

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              if tutto-ok
                 perform ELABORAZIONE
                 perform CLOSE-FILES
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa       delimited low-value
                      "CONLUBGIO_"  delimited size
                      como-data     delimited size
                      "_"           delimited size
                      como-ora      delimited size
                      ".log"        delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output file-log
      *****     else
      *****        display "Conferme LBX giornaliere in corso..."
           end-if.

           set tutto-ok    to true.
           accept como-ora from time.

           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           move "INIZIO ELABORAZIONE" to como-messaggio.
           perform COMPONI-RIGA-LOG.
                                                                       
           accept path-import from environment "CONLUBGIO_PATH_IMPORT".
           if path-import = spaces  
              move "PATH IMPORT NON IMPOSTATO: CONLUBGIO_PATH_IMPORT" 
                to como-messaggio
              perform COMPONI-RIGA-LOG
              move -1 to batch-status
              set errori to true
           end-if.
                                                                       
           accept path-backup from environment "CONLUBGIO_PATH_BACKUP".
           if path-backup = spaces  
              move "PATH BACKUP NON IMPOSTATO: CONLUBGIO_PATH_BACKUP" 
                to como-messaggio
              perform COMPONI-RIGA-LOG
              move -1 to batch-status
              set errori to true
           end-if.
           
           accept LinkAddress from environment "CONLUBGIO_ADDRESSES".
           if LinkAddress = spaces  
              move "DESTINI POSTA NON IMPOSTATO: CONLUBGIO_ADDRESSES" 
                to como-messaggio
              perform COMPONI-RIGA-LOG
              move -1 to batch-status
              set errori to true
           end-if.

      ***---
       OPEN-FILES.            
           move "APERTURA FILES" to como-messaggio.
           perform COMPONI-RIGA-LOG.
           open i-o tordini.
           if tutto-ok
              open input tsetinvio
           end-if.
           open input rordini.

      ***---
       ELABORAZIONE.
           set errori-elab to false.
           perform CHECK-CARTELLE.
           move "ELABORAZIONE CARTELLA IMPORT" to como-messaggio.
           perform COMPONI-RIGA-LOG.                 

           perform until 1 = 2
              call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                            dir-handle,
                                            nome-file

              if nome-file = spaces exit perform end-if

              if nome-file not = "."      and
                           not = ".."     and
                           not = "Backup" and
                           not = ".DS_Store"
                 
                 add 1 to n-file
                 initialize como-messaggio 
                 string "ELABORAZIONE FILE: " delimited size
                        nome-file             delimited size
                   into como-messaggio
                 end-string
                 perform COMPONI-RIGA-LOG
                 perform ELABORA-FILE
                 perform SPOSTA-IN-BACKUP
              end-if                    
           end-perform.                 
           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, dir-handle.
           initialize como-messaggio. 
           string "ELABORATI " delimited size
                  n-file       delimited size
                  " FILES CSV" delimited size
             into como-messaggio
           end-string.
           perform COMPONI-RIGA-LOG.
           initialize como-messaggio. 
           string "FATTURE PRENOTATE "              delimited size
                  n-prenotate                       delimited size
                  " - BOLLE ASSOCIATE: "            delimited size
                  n-associate                       delimited size
                  " - RIGHE CON BOLLE NON VALIDE: " delimited size
                  n-no-bolla                        delimited size            
                  " - ORDINI NON TROVATI: "         delimited size
                  n-no-ordine                       delimited size
                  " - ORDINI GIA' BOLLETTATI: "     delimited size
                  n-gia-bolla                       delimited size
                  " FILES CSV"                      delimited size
             into como-messaggio
           end-string.      
           perform COMPONI-RIGA-LOG.

      ***---
       ELABORA-FILE.
           move 0 to riga.
           initialize wstampa.
           inspect path-import replacing trailing spaces by low-value.
           inspect nome-file   replacing trailing spaces by low-value.
           string  path-import delimited low-value
                   nome-file   delimited low-value
              into wstampa
           end-string.
           open input lineseq.
           read lineseq next. |Salto intestazione
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to riga
              perform ELABORA-ORDINE
           end-perform.
           close lineseq.

      ***---
       ELABORA-ORDINE.
           unstring line-riga of lineseq delimited by ";"
               into LUNBOL
                    LUDBOL
                    LUAORD
                    LUNORD
                    LURFOR
                    LUDORD
                    LUFLAG.

           move 0 to num-bolla data-bolla.
           
           move lunbol to num-bolla convert.
           move LUDBOL(6:2) to LUDBOL(8:2).
           move "20"        to LUDBOL(6:2).
           move LUDBOL(6:4) to data-bolla(1:4) convert.
           move LUDBOL(4:2) to data-bolla(5:2) convert.
           move LUDBOL(2:2) to data-bolla(7:2) convert.

           if num-bolla  = 0 or
              data-bolla = 0                    
              initialize como-messaggio     
              string "FILE: "                       delimited size
                     nome-file                      delimited low-value
                     " || RIGA: "                   delimited size
                     riga                           delimited size
                     " - RIFERIMENTI BOLLA NON PRESENTI" 
                                                    delimited size
                into como-messaggio
              end-string           
              move 1 to batch-status
              perform COMPONI-RIGA-LOG        
              add 1 to n-no-bolla
           else
              move num-bolla       to tor-num-bolla
              move data-bolla(1:4) to tor-anno-bolla
              read tordini no lock key k-bolla
                   invalid            
                   move LURFOR(2:4) to tor-anno   convert
                   move LURFOR(6:)  to tor-numero convert
                   read tordini lock
                        invalid                                 
                        initialize como-messaggio
                        string "FILE: "                 delimited size
                               nome-file           delimited low-value
                               " || RIGA: "             delimited size
                               riga                     delimited size
                               " - ANNO EVASIONE: "     delimited size
                               tor-anno                 delimited size
                               " || N. EVASIONE: "      delimited size
                               tor-numero               delimited size
                               " - ORDINE NON TROVATO." delimited size
                          into como-messaggio
                        end-string
                        perform COMPONI-RIGA-LOG
                        move 1 to batch-status 
                        add 1 to n-no-ordine
                    not invalid
                        if tor-anno-bolla not = 0 or
                           tor-data-bolla not = 0 or
                           tor-num-bolla  not = 0               
                           initialize como-messaggio
                           string "FILE: "             delimited size
                                  nome-file        delimited low-value
                                  " || RIGA: "         delimited size
                                  riga                 delimited size
                                  " - ANNO EVASIONE: " delimited size
                                  tor-anno             delimited size
                                  " || N. EVASIONE: "  delimited size
                                  tor-numero           delimited size
                                  " . GIA' BOLLETTATO" 
                                                 delimited size
                             into como-messaggio
                           end-string           
                           move 1 to batch-status
                           perform COMPONI-RIGA-LOG 
                           add 1 to n-gia-bolla
                        else   
                           perform QTA-BOLLA
                           move num-bolla       to tor-num-bolla
                           move data-bolla(1:4) to tor-anno-bolla
                           move data-bolla      to tor-data-bolla   
                           set tor-da-inviare-no to true
                           rewrite tor-rec       
                           add 1 to n-associate
                           initialize como-messaggio            
                           string "FILE: "              delimited size
                                  nome-file        delimited low-value
                                  " || RIGA: "          delimited size
                                  riga                  delimited size
                                  " - ANNO EVASIONE: "  delimited size
                                  tor-anno              delimited size
                                  " || N. EVASIONE: "   delimited size
                                  tor-numero            delimited size
                                  " - ANNO BOLLA: "     delimited size
                                  tor-anno-bolla        delimited size
                                  " || N. BOLLA: "      delimited size
                                  tor-num-bolla         delimited size
                                  " || DEL: "           delimited size
                                  tor-data-bolla(7:2)   delimited size
                                  "/"                   delimited size
                                  tor-data-bolla(5:2)   delimited size
                                  "/"                   delimited size
                                  tor-data-bolla(1:4)   delimited size
                                  " - BOLLA ASSOCIATA CORRETTAMENTE."
                                                        delimited size
                             into como-messaggio
                           end-string                    
                           perform COMPONI-RIGA-LOG    
                           perform PRENOTAZIONE-FATTURA
                        end-if
                   end-read
               not invalid
                   initialize como-messaggio             
                   string "FILE: "              delimited size
                          nome-file             delimited low-value
                          " || RIGA: "          delimited size
                          riga                  delimited size
                          " - ANNO EVASIONE: "  delimited size
                          tor-anno              delimited size
                          " || N. EVASIONE: "   delimited size
                          tor-numero            delimited size
                          " - ANNO BOLLA: "     delimited size
                          tor-anno-bolla        delimited size
                          " || N. BOLLA: "      delimited size
                          tor-num-bolla         delimited size
                          " || DEL: "           delimited size
                          tor-data-bolla(7:2)   delimited size
                          "/"                   delimited size
                          tor-data-bolla(5:2)   delimited size
                          "/"                   delimited size
                          tor-data-bolla(1:4)   delimited size
                          " - RIFERIMENTI BOLLA GIA' PRESENTI."
                                                delimited size
                     into como-messaggio
                   end-string                    
                   move 1 to batch-status
                   perform COMPONI-RIGA-LOG
              end-read                  
           end-if.

      ***---
       QTA-BOLLA.
           move tor-anno       to ror-anno.
           move tor-numero     to ror-num-ordine.
           move low-value      to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    move 0       to link-impegnato
                    move ror-qta to link-valore
                    set link-update         to true
                    move ror-prg-chiave     to link-key
                    move tor-causale        to link-causale
                    set link-update-um      to true
                    set link-update-peso    to false
                    set link-update-valore  to false
                    move "0000000000000000" to link-array
                    move  1                 to multiplyer(1)
                    move -1                 to multiplyer(2)
                    move "BATCH"            to link-user 
                    call   "wprogmag" using link-wprogmag
                    cancel "wprogmag"
                 end-perform
           end-start.

      ***---
       PRENOTAZIONE-FATTURA.
           move "BATCH"       to lprenf-user-codi.
           move tor-anno      to lprenf-anno.
           move tor-numero    to lprenf-numero.
           call   "bprenf" using link-bprenf.
           cancel "bprenf".
           if lprenf-errore = space                  
              set tor-fatt-si-prenotata to true 
              set tor-da-inviare-no to true
              rewrite tor-rec
              unlock tordini all records
              if status-tordini = "00"
                 initialize como-messaggio              
                 string "FILE: "              delimited size
                        nome-file        delimited low-value
                        " || RIGA: "          delimited size
                        riga                  delimited size
                        " - ANNO EVASIONE: "  delimited size
                        tor-anno              delimited size
                        " || N. EVASIONE: "   delimited size
                        tor-numero            delimited size
                        " - ANNO BOLLA: "     delimited size
                        tor-anno-bolla        delimited size
                        " || N. BOLLA: "      delimited size
                        tor-num-bolla         delimited size
                        " || DEL: "           delimited size
                        tor-data-bolla(7:2)   delimited size
                        "/"                   delimited size
                        tor-data-bolla(5:2)   delimited size
                        "/"                   delimited size
                        tor-data-bolla(1:4)   delimited size
                        " - FATTURA PRENOTATA CORRETTAMENTE."
                                              delimited size
                   into como-messaggio
                 end-string                    
              else
                 initialize como-messaggio              
                 string "FILE: "              delimited size
                        nome-file        delimited low-value
                        " || RIGA: "          delimited size
                        riga                  delimited size
                        " - ANNO EVASIONE: "  delimited size
                        tor-anno              delimited size
                        " || N. EVASIONE: "   delimited size
                        tor-numero            delimited size
                        " - ANNO BOLLA: "     delimited size
                        tor-anno-bolla        delimited size
                        " || N. BOLLA: "      delimited size
                        tor-num-bolla         delimited size
                        " || DEL: "           delimited size
                        tor-data-bolla(7:2)   delimited size
                        "/"                   delimited size
                        tor-data-bolla(5:2)   delimited size
                        "/"                   delimited size
                        tor-data-bolla(1:4)   delimited size
                        " - ERRORE IN PRENOTAZIONE: "
                                              delimited size
                        status-tordini        delimited size
                   into como-messaggio
                 end-string                 
              end-if
              perform COMPONI-RIGA-LOG
              add 1 to n-prenotate
           else
              move lprenf-errore to tipo-errore
              perform MSG-ERRORE-PRENF
           end-if.

      ***---
       MSG-ERRORE-PRENF.
LUBEXX     evaluate true
LUBEXX     when errore-contatore
LUBEXX          move "Contatore anno esercizio inesistente!" 
                  to msg-err-pren

LUBEXX     when errore-prezzo
LUBEXX          move "Prezzo incoerente!!!"
                  to msg-err-pren
           when errore-omaggio
                move "Prenotazione impossibile!!!"
                  to msg-err-pren
           when errore-iva-no-E15
                move "Iva E15 non presente"
                  to msg-err-pren
           when errore-imposte
                move "Ricalcolare le imposte ripassando la riga"
                  to msg-err-pren
           when errore-prezzo-master
                move "Il prezzo non coincide con l'ordine master"
                  to msg-err-pren
           when errore-prog-master
                move "L'articolo non coincide con l'ordine master"
                  to msg-err-pren
           when errore-E15-no-zero
                move "Iva esente con valori monetari"
                  to msg-err-pren
           when errore-iva-020
                move "Codice IVA 20"
                  to msg-err-pren
           when errore-iva-021
                move "Prenotazione impossibile!!!"
                  to msg-err-pren
           when errore-totale-0
                move "Importo totale 0"
                  to msg-err-pren
           end-evaluate.
           initialize como-messaggio.
           string "FILE: "                       delimited size
                  nome-file                      delimited low-value
                  " || RIGA: "                   delimited size
                  riga                           delimited size
                  " - ANNO EVASIONE: "           delimited size
                  tor-anno                       delimited size
                  " || N. EVASIONE: "            delimited size
                  tor-numero                     delimited size
                  " || RIGA EVASIONE: "          delimited size
                  lprenf-riga                    delimited size
                  " - ANNO BOLLA: "              delimited size
                  tor-anno-bolla                 delimited size
                  " || N. BOLLA: "               delimited size
                  tor-num-bolla                  delimited size
                  " || DEL: "                    delimited size
                  tor-data-bolla(7:2)            delimited size
                  "/"                            delimited size
                  tor-data-bolla(5:2)            delimited size
                  "/"                            delimited size
                  tor-data-bolla(1:4)            delimited size
                  " - PRENOTAZIONE NON RIUSCITA" delimited size
                  " - "                          delimited size
                  msg-err-pren                   delimited size
             into como-messaggio
           end-string.           
           set errori-elab to true
           add 1 to idx-err.
           move como-messaggio to el-body-mess(idx-err).
           perform COMPONI-RIGA-LOG. 

      ***---
       SPOSTA-IN-BACKUP.
           inspect path-backup replacing trailing spaces by low-value.
           initialize file-backup
           string  path-backup delimited low-value
                   nome-file   delimited low-value
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
              into file-backup
           end-string.
           inspect wstampa   replacing trailing spaces 
                             by low-value.
           initialize cmd.
           string "move "     delimited size
                  wstampa     delimited low-value
                  " "         delimited size
                  file-backup delimited size
             into cmd
           end-string.                
                                                            
           initialize como-messaggio.
           string "ESEGUO BACKUP: " delimited size
                  cmd               delimited size
             into como-messaggio
           end-string
           perform COMPONI-RIGA-LOG.

           move 0 to status-call.
           call "C$SYSTEM" using cmd, 225
                          giving status-call.
           if status-call not = 0
              move "COMANDO BACKUP NON RIUSCITO" to como-messaggio
              perform COMPONI-RIGA-LOG
              if RichiamoSchedulato
                 move 1 to batch-status
              end-if  
           else                   
              initialize como-messaggio
              string "BACKUP RIUSCITO: " delimited size
                     file-backup         delimited low-value
                into como-messaggio
              end-string
              perform COMPONI-RIGA-LOG
           end-if.

      ***---
       CHECK-CARTELLE.
           move "VERIFICA CARTELLA BACKUP" to como-messaggio.
           perform COMPONI-RIGA-LOG.

           move "*.*"  to pattern.
      *    cartella di export
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-backup,
                                         pattern.

           move return-code to dir-handle.

           if dir-handle = 0
              set errori         to true
              move path-backup   to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              move "VERIFICA CARTELLA BACKUP OK" to como-messaggio
              perform COMPONI-RIGA-LOG   
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, dir-handle
           end-if.

           if tutto-ok
              move "VERIFICA CARTELLA IMPORT" to como-messaggio
              perform COMPONI-RIGA-LOG
      *       cartella di backup
              move "*.*" to pattern
              call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                            path-import,
                                            pattern
              move return-code     to dir-handle
              if dir-handle = 0
                 set errori        to true
                 move path-import  to MSG-Folder-Name
                 perform MSG-DIR-ERR
              else
                 move "VERIFICA CARTELLA IMPORT OK" to como-messaggio
                 perform COMPONI-RIGA-LOG
              end-if
           end-if.

      ***---
       MSG-DIR-ERR.
           inspect como-messaggio replacing trailing space by low-value.
           initialize como-messaggio.
           string "IMPOSSIBILE APRIRE DIRECTORY: "  delimited size
                   MSG-Folder-Name                  delimited size
              into como-messaggio.
           perform COMPONI-RIGA-LOG. 
           move -1 to batch-status.

      ***--
       CLOSE-FILES.
           close tordini tsetinvio rordini.
           move "CHIUSURA FILES" to como-messaggio.
           perform COMPONI-RIGA-LOG.

      ***---
       EXIT-PGM.                    
           accept como-ora from time.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.

           if tot-secondi < 60
              if RichiamoSchedulato
                 move tot-secondi to ss
                 initialize como-messaggio
                 string "ELABORAZIONE TERMINATA IN: ",
                        ss, " SECONDI"
                        into como-messaggio
                 end-string
                 perform COMPONI-RIGA-LOG
              end-if
           else
              divide tot-secondi by 60 giving mm remainder ss
              if RichiamoSchedulato
                 initialize como-messaggio
                 string "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI"
                         into como-messaggio
                 end-string
                 perform COMPONI-RIGA-LOG
              end-if
           end-if.

           if RichiamoSchedulato   
      *****        display "                                             "
      *****           upon batch-win-handle
      *****             line 25,00
      *****           column 35,00    

              if errori-elab
                 perform INVIO-MAIL
              end-if               
              close file-log
           end-if.

           goback.

      ***---
       INVIO-MAIL.
      *****     display "Invio mail in corso...".

           move "INVIO MAIL IN CORSO..." to como-messaggio.
           perform COMPONI-RIGA-LOG.



           initialize LinkBody.
           move "RIEPILOGO ERRORI CONFERME LBX GIORNALIERE" 
             to LinkSubject.
                         
           move body-mess to LinkBody. 
      
           set errori to true.
           move 0 to tentativi.
           move "conlubgio" to NomeProgramma.
                                            
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa    delimited low-value
                   "FITTIZIO" delimited size
                   into wstampa
           end-string.
           open output fittizio.
           close       fittizio.
                  
           initialize LinkAttach.
           string wstampa delimited low-value
             into LinkAttach
           end-string.
      
           perform 10 times
              add 1 to tentativi
              perform SEND-MAIL
              
              initialize como-messaggio
              if StatusInvioMail = -1
                 string "TENTATIVO N. "               delimited size
                        tentativi                     delimited size
                        ": "                          delimited size
                        "Chiamata InvioMail fallita!" delimited size
                        " STATUS -1"                  delimited size
                        into como-messaggio
                 end-string
              else
                 string "TENTATIVO N. "                delimited size
                        tentativi                      delimited size
                        ": "                           delimited size
                        "Chiamata InvioMail riuscita!" delimited size
                        into como-messaggio
                 end-string
              end-if
              perform COMPONI-RIGA-LOG

              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1
      
              initialize como-messaggio
              string "TENTATIVO N. "       delimited size
                     tentativi             delimited size
                     " - ESITO: "          delimited size
                     line-riga of lineseq1 delimited size
                     into como-messaggio
              end-string
              perform COMPONI-RIGA-LOG
      
           end-perform.

           delete file fittizio.

      ***---
       COMPONI-RIGA-LOG.
           initialize como-riga.
           perform SETTA-INIZIO-RIGA.
           inspect como-messaggio replacing trailing spaces by low-value
           string r-inizio        delimited size
                  como-messaggio  delimited low-value
             into como-riga
           end-string.
           perform RIGA-LOG.        

      ***---
       RIGA-LOG.
           if RichiamoSchedulato
              initialize log-riga
              write log-riga from como-riga
           else
              display como-riga upon syserr
           end-if.         

      ***---
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa. 
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
