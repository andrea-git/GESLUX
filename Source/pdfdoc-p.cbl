       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      pdfdoc-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".
           copy "tordini.sl".
           copy "tnotacr.sl".
           copy "tcontat.sl".

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".
           copy "tordini.fd".
           copy "tnotacr.fd".
           copy "tcontat.fd".
      *(( XFD FILE = logfile ))
       FD  logfile.
       01 log-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "acugui.def".      
           copy "acucobol.def".

       78  titolo    value "GESLUX - Stampa documenti pdf batch".
       78  78-clear              value 
           "                                                          ".

       77  status-tparamge  pic xx.
       77  status-tordini   pic xx.
       77  status-tnotacr   pic xx.
       77  status-tcontat   pic xx.
       77  status-logfile   pic xx.
       77  path-logfile     pic x(256).  
       77  dir-handle       handle.
       77  pgm-status       pic s9 value 0.
       77  como-num-x       pic x(8).
       77  como-num         pic 9(8).
       77  tot-doc          pic 9(10) value 0.

       77  counter          pic 9(10) value 0.
       77  counter2         pic 9(10) value 0.
       77  counter-edit     pic z(10).      
       77  como-riga             pic x(200).
       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).
                                   
       77  file-name        pic x(50). 
       77  file-backup      pic x(50).       
       77  path-doc         pic x(200).      
       77  path-doc-f       pic x(200).      
       77  path-doc-nc      pic x(200).      
       77  path-docum       pic x(200).
       77  docum            pic x(200). 
       77  cmd              pic x(200).  
       77  t                pic 9 value 0.
       
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  whh                   pic 99.
       77  wmm                   pic 9(5).
       77  wss                   pic 99.   
                                   
       01  r-inizio              pic x(25).


       01  filler           pic 9 value 0.
         88 RichiamoSchedulato value 1, false 0.     
       77  nargs                 pic 99  comp-1 value 0.

       01  controlli        pic xx.
         88 tutto-ok        value "OK".
         88 errori          value "ER".

       01  filler           pic 9 value 0.
         88 RecLocked       value 1, false 0.
           
       copy "link-stfatt.def".

       LINKAGE SECTION.      
       77  status-pgm       pic s9.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING status-pgm, batch-linkage.
       DECLARATIVES.    

      ***---
       logfile-ERR SECTION.
           use after error procedure on logfile.
           set tutto-ok  to true.
           evaluate status-logfile
           when "35"
                set errori to true
                if not RichiamoSchedulato
                   display message "File LOG not found!"
                             title titolo
                              icon 3
                end-if
           when "39"
                set errori to true
                if not RichiamoSchedulato
                   display message "File mismatch size!"
                             title titolo
                              icon 3
                end-if
           when "98"
                set errori to true
                if not RichiamoSchedulato
                   display message "Indexed file corrupt!"
                             title titolo
                              icon 3 
                end-if
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked  to false.
           evaluate status-tparamge
           when "93"
           when "99"
                set RecLocked to true
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
           accept como-ora  from time.
           accept como-data from century-date.
           move   como-ora(1:2) to whh.
           move   como-ora(3:2) to wmm.
           move   como-ora(5:2) to wss.

           compute start-secondi = ( whh * 3600 ) + ( wmm * 60 ) + wss.
      
           set tutto-ok to true.
           call "C$NARG" using nargs.
           if nargs > 1
              set RichiamoSchedulato to true
           end-if.

      ***---
       OPEN-FILES.           
           initialize path-logfile.
           if RichiamoSchedulato
              accept path-logfile from environment "SCHEDULER_PATH_LOG"
           else                                                      
              accept path-logfile from environment "PATH_ST"
           end-if.
           accept como-ora  from time.
           inspect path-logfile replacing trailing spaces by low-value.
           string path-logfile     delimited low-value
                  "LOG_PDFDOC_"    delimited size
                  como-data        delimited size
                  "_"              delimited size
                  como-ora         delimited size
                  ".log"           delimited size
                  into path-logfile
           end-string.
           open output logfile.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SCRIVI-RIGA-LOG.
           open i-o tparamge.
           if RecLocked
              move "TPARAMGE LOCKED. IMPOSSIBILE PROCEDERE" to como-riga
              perform SCRIVI-RIGA-LOG
              close logfile
              set errori to true
              exit paragraph
           end-if.

           open input tordini tnotacr tcontat.

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
       CHECK-CARTELLE.            
      * CONTROLLO L'ESISTENZA DELLE CARTELLE              
           if fatture
              accept  path-doc from environment "PATH_F_PDF"
              if path-doc = spaces
                 move -1 to pgm-status
                 move 
                 "PATH CARTELLA FATTURE (PATH_F_PDF) NON VALORIZZATO"
                   to como-riga
                 perform SCRIVI-RIGA-LOG
              else                                           
                 inspect path-doc replacing trailing spaces by low-value
                 perform CHECK-CARTELLA
                 if pgm-status = 0
                    move path-doc to path-doc-f
                 end-if
              end-if
           else
              accept  path-doc from environment "PATH_NC_PDF" 
              if path-doc = spaces
                 move -1 to pgm-status
                 move 
                "PATH CARTELLA FATTURE (PATH_NC_PDF) NON VALORIZZATO"
                   to como-riga
                 perform SCRIVI-RIGA-LOG
              else
                 inspect path-doc 
                         replacing trailing spaces by low-value
                 perform CHECK-CARTELLA
                 move path-doc to path-doc-nc
              end-if
           end-if.

      ***---
       CHECK-CARTELLA.
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-doc,
                                         "*"

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle = 0 or null
              call "c$makedir" using path-doc giving return-code
              if return-code not = 0
                 move -1 to pgm-status
                 inspect path-doc replacing trailing space by low-value
                 initialize como-riga
                 string "Errore nella creazione della cartella: " 
                        delimited size
                        path-doc delimited size
                   into como-riga
                 end-string     
              end-if
           end-if.
           if pgm-status = 0
              string path-doc delimited low-value
                     con-anno delimited size
                     "\"      delimited size
                into path-doc
              end-string     
              call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                            path-doc,
                                            "*"

              move RETURN-CODE        to Dir-Handle
              if dir-handle = 0 or null
                 call "c$makedir" using path-doc giving return-code
                 if return-code not = 0
                    move -1 to pgm-status
                    inspect path-doc 
                            replacing trailing space by low-value
                    initialize como-riga
                    string "Errore nella creazione della cartella: " 
                           delimited size
                           path-doc delimited size
                      into como-riga
                    end-string     
                 end-if
              end-if
           end-if.

      ***---
       ELABORAZIONE.   
           move 0 to status-pgm.
           perform ELABORA-FATTURE.
           if status-pgm = 0
              perform ELABORA-NC 
           end-if.

      ***---
       ELABORA-FATTURE.
           move 0 to tot-doc.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno-f-pdf to con-anno.
           read tcontat no lock
                invalid
                move -1 to batch-status
                move "CONTATORE ANNO FATTURE NON TROVATO" to como-riga
                perform SCRIVI-RIGA-LOG
            not invalid                         
                set fatture to true
                perform CHECK-CARTELLE
                if pgm-status = 0 
                   initialize como-riga
                   string "CONTATORE FATTURE: " delimited size
                          tge-num-f-pdf         delimited size
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
                   move tge-anno-f-pdf to tor-anno-fattura
                   add 1 to tge-num-f-pdf
                   perform varying tor-num-fattura
                              from tge-num-f-pdf by 1 
                             until tor-num-fattura > con-num-fatt
                      move tor-num-fattura  to como-num-x como-num
                      inspect como-num-x 
                              replacing leading x"30" by x"20"
                      call "C$JUSTIFY" using como-num-x, "L"
                      inspect como-num-x 
                              replacing trailing spaces by low-value
                      read tordini key k-fattura
                           invalid
                           move 1 to pgm-status
                           initialize como-riga
                           string "** FATTURA: "  delimited size
                                  como-num-x     delimited low-value
                                  " NON TROVATA"  delimited size
                             into como-riga
                           end-string
                           perform SCRIVI-RIGA-LOG
                       not invalid                           
                           initialize path-docum
                           move path-doc-f to stfatt-path-doc
                           string path-doc-f      delimited low-value
                                  como-num-x     delimited low-value
                                  ".pdf"          delimited size
                             into path-docum
                           end-string                             
                           initialize docum     
                           string como-num-x delimited low-value
                                  ".pdf"      delimited size
                             into docum
                           end-string      
                           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                                         path-doc-f,
                                                         docum
                           move RETURN-CODE        to Dir-Handle
                           if not (dir-handle = 0 or null)
                              initialize como-riga
                              inspect path-docum replacing trailing 
                                                 spaces by low-value
                              string path-docum delimited low-value
                                     " GIA' PRESENTE. SOVRASCRITTURA"
                                     delimited size
                                into como-riga
                              end-string
                              perform SCRIVI-RIGA-LOG
                           end-if
                           initialize como-riga
                           string "ELABORAZIONE FATTURA: " 
                                                  delimited size
                                  como-num-x     delimited size
                                  into como-riga
                           end-string
                           perform SCRIVI-RIGA-LOG  
                           perform STAMPA-EFFETTIVA
                           if stfatt-status = 1
                              move -1 to status-pgm
                              exit perform
                           end-if
                      end-read
                   end-perform
                end-if
           end-read.                          

      ***---
       ELABORA-NC.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno-nc-pdf to con-anno.
           read tcontat no lock
                invalid
                move -1 to batch-status
                move "CONTATORE ANNO NOTE CREDITO NON TROVATO" 
                  to como-riga
                perform SCRIVI-RIGA-LOG
            not invalid                
                set NoteCredito to true
                perform CHECK-CARTELLE
                if pgm-status = 0     
                   initialize como-riga
                   string "CONTATORE NC: " delimited size
                          tge-num-NC-pdf   delimited size
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
                   move tge-anno-NC-pdf to tno-anno-fattura
                   add 1 to tge-num-nc-pdf
                   perform varying tno-num-fattura
                              from tge-num-nc-pdf by 1 
                             until tno-num-fattura > con-ult-num-nc-fisc
                      move tno-num-fattura  to como-num como-num-x
                      inspect como-num-x 
                              replacing leading x"30" by x"20"
                      call "C$JUSTIFY" using como-num-x, "L"
                      inspect como-num-x 
                              replacing trailing spaces by low-value
                      read tnotacr key k-fattura
                           invalid
                           move 1 to pgm-status
                           initialize como-riga
                           string "** NOTA CREDITO: "  delimited size
                                  como-num-x     delimited low-value
                                  " NON TROVATA"  delimited size
                             into como-riga
                           end-string
                           perform SCRIVI-RIGA-LOG
                       not invalid
                           move path-doc-nc to stfatt-path-doc
                           initialize path-docum
                           string path-doc-nc     delimited low-value
                                  como-num-x     delimited low-value
                                  ".pdf"          delimited size
                             into path-docum
                           end-string      
                           initialize docum     
                           string como-num-x delimited low-value
                                  ".pdf"      delimited size
                             into docum
                           end-string      
                           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                                         path-doc-nc,
                                                         docum
                           move RETURN-CODE        to Dir-Handle
                           if dir-handle > 0 or null
                              initialize como-riga
                              inspect path-docum replacing trailing 
                                                 spaces by low-value
                              string path-docum delimited low-value
                                     " GIA' PRESENTE. SOVRASCRITTURA"
                                     delimited size
                                into como-riga
                              end-string
                              perform SCRIVI-RIGA-LOG
                           end-if
                           initialize como-riga
                           string "ELABORAZIONE NOTA CREDITO: " 
                                                   delimited size
                                  como-num-x      delimited size
                                  into como-riga
                           end-string
                           perform SCRIVI-RIGA-LOG             
                           perform STAMPA-EFFETTIVA
                           if stfatt-status = 1
                              move -1 to status-pgm
                              exit perform
                           end-if
                      end-read
                   end-perform
                end-if
           end-read.
           if tot-doc = 0
              initialize como-riga
              string "NESSUN DOCUMENTO DA STAMPARE" delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           end-if.
             
      ***---
       STAMPA-EFFETTIVA.
      *****     perform varying t from 1 by 1 
      *****               until t > 5
              if RichiamoSchedulato
                 set SoloPDFBatch to true
              else
                 set SoloPDF to true
              end-if
              move 1 to LinkElab
              move 1 to num-copie
              move con-anno to LinkAnno
              move como-num to num-a num-da
              initialize como-riga
      *****        string "TENTATIVO " delimited size
      *****               t            delimited size
      *****          into como-riga
      *****        end-string
      *****        perform SCRIVI-RIGA-LOG
              call   "stfatt-p" using stfatt-linkage
              cancel "stfatt-p"
              |lo cancello altrimenti ne crea uno per documento
              if stfatt-path-log not = spaces
                 call "C$DELETE" using stfatt-path-log
              end-if
              evaluate stfatt-status
              when 1
                   move 1 to pgm-status
                   move "** STAMPA PDF KO" to como-riga
              when 2
                   continue
              when other               
                   add 1 to tot-doc
                   move "STAMPA PDF OK" to como-riga
                   perform AGGIORNA-CONTATORE
                   move 0 to pgm-status
      *****           exit perform
              end-evaluate
              perform SCRIVI-RIGA-LOG
      *****     end-perform.
           .

      ***---
       AGGIORNA-CONTATORE.
           move spaces to tge-chiave.
           perform until 1 = 2  
              set RecLocked to false
              read tparamge lock
              if not RecLocked             
                 if fatture
                    move tor-num-fattura to tge-num-f-pdf
                 else
                    move tno-num-fattura to tge-num-nc-pdf
                 end-if
                 rewrite tge-rec
                 unlock tparamge all records
                 read tparamge no lock
                 exit perform
              end-if          
           end-perform.      

      ***---
       COUNTER-VIDEO.
           if not RichiamoSchedulato exit paragraph end-if.
           add 1 to counter.
           add 1 to counter2.
           if counter2 = 500
              if counter = 500
                 display "PDF"
                    upon batch-win-handle at column 18
                                               line 05
             end-if
              move counter to counter-edit
              display counter-edit
                 upon batch-win-handle at column 21
                                     line 05
              move 0 to counter2
           end-if.

      ***---
       CLOSE-FILES.                    
           move "CHIUSURA FILES" to como-riga.
           perform SCRIVI-RIGA-LOG.
           close tcontat tparamge tordini tnotacr.     


      ***---
       EXIT-PGM.                          
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

           close logfile.
                    
           if RichiamoSchedulato
              move pgm-status to batch-status
           end-if.
           
           goback.  

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
