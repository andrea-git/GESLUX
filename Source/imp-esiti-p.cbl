       PROGRAM-ID. imp-esiti-p.
       AUTHOR.     Luciano.

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tordini.sl".
           copy "eordini.sl".
           copy "tvettori.sl".
           copy "vettel.sl".
           copy "tesconsvet.sl".
           copy "lineseq.sl".
           copy "tparamge.sl".

       SELECT er-flusso
           ASSIGN       TO  PATH-ER-FLUSSO
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-er-flusso.

       SELECT flusso-rep
           ASSIGN       TO  PATH-flusso-rep
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-flusso-rep.

       FILE SECTION.          
           copy "tordini.fd".
           copy "eordini.fd".
           copy "tvettori.fd".
           copy "vettel.fd".
           copy "tesconsvet.fd".
           copy "lineseq.fd".
           copy "tparamge.fd".

       FD  er-flusso.
       01  er-f-rec         PIC  x(32000).

       FD  flusso-rep.
       01  rec-flusso-rep   PIC  x(80).

       WORKING-STORAGE SECTION.
           copy "tipo-data.def".
           copy "tipo-ora.def".
                                    
       01  como-record.
           05 como-rif-lubex       pic 9(8).
           05 como-anno-rif-lubex  pic 9(4).
           05 como-vettore         PIC 9(5).
           05 como-esito           PIC x(20).
           05 como-dt-consegna     PIC 9(8).
           05 como-ora-consegna    PIC 9(8).
           05 como-dt-avviso       PIC 9(8).
           05 como-dt-ap-giacenza  PIC 9(8).

      * FILE STATUS
       77  status-tordini          pic xx.
       77  status-eordini          pic xx.
       77  status-tvettori         pic xx.
       77  status-vettel           pic xx.
       77  status-lineseq          pic xx.
       77  status-er-flusso        pic xx.
       77  status-flusso-rep       pic xx.
       77  status-tescons          pic xx.
       77  status-tesconsvet       pic xx.
       77  status-tparamge         pic xx.

      * TESTATA ASCOLIST
       01  testa-report.   
           05 filler            pic x     value space.
           05 filler            pic x(10) value "Esito".
           05 filler            pic x(2)  value space.
           05 filler            pic x(4)  value "Anno".
           05 filler            pic x(2)  value space.
           05 filler            pic x(8)  value "Ordine".
                                          
       01  riga-report.
           05 filler            pic x     value space.
           05 r-esito           pic x(10).
           05 filler            pic x(2)  value space.
           05 r-anno            pic 9(4).
           05 filler            pic x(2)  value space.
           05 r-ordine          pic z(8).

       01  Riga-Cod-cli-Ragsoc.
           05 filler                pic x(01)      value spaces.
           05 filler                pic X(10)      value "Fornitore:".
           05 r-codforn             pic z(5)       value spaces.
           05 filler                pic x(3)       value " - ".
           05 r-ragsoc              pic X(50)      value spaces.

       01  Riga-file.
           05 filler                pic x(01)      value spaces.
           05 filler                pic x(10)      value "File.....:".
           05 r-file                pic x(50)      value spaces.

      * SWITCHES
       01  filler                   pic 9.
           88 Prima-volta           value 1, false 0.

       01  controlli                pic xx.
           88 tutto-ok              value "OK".
           88 errori                value "ER".

       01  FlagApertura             pic   9.  
           88 NonAncoraAperto       value 0.
           88 FileAperto            value 1.

       01  FlagTestata              pic   9.  
           88 TestataGiaFatta       value 1, false 0.

       01  FlagTrovato              pic   9.
           88 trovato               value 1, false 0.

       01  FlagTrovatiErrori        pic   9.
           88 TrovatiErrori         value 1, false 0.

       01  errori-flusso                    pic x.
           88 no-esito                      value "1". 
           88 no-bolla                      value "2".
           88 no-evasione                   value "4".
           88 ordine-bloccato               value "3".
           88 lineseq-non-aperto            value "a".
           88 ordini-non-aperto             value "b".
           88 ordini-locked                 value "c".
           88 FLUSSO-OFFSET-INESISTENTE     value "d".
           88 flusso-offset-non-valorizzato value "e".
           88 lineseq-VUOTO                 value "f".
           88 lineseq-INCOERENTE            value "g".
           88 FLUSSO-NOT-DESCR-ART          value "h".
           88 FLUSSO-NOT-COD-ART            value "i".

      * OTHER DATA             
       77  como-data                pic 9(8)   value zero.
       77  como-ora                 pic 9(8)   value zero.
       77  SaveCodice               pic x(15).
       77  save-colli               pic 9(18).
       77  save-numsped             pic x(15).
       77  rename-status            pic 9(9)   comp-4.
       77  como-peso-edit           pic z.zz9,999.
       77  como-prg                 pic z(5).
       77  WFlagAttivo              pic x.
         88 FlagAttivo              value "S", false "N".


       01  filler                   pic 9.
           88 RecLocked             value 1 false 0.
       77  scelta                   pic 9.
       77  titolo                   pic x(256).

       77  sav-trailing             pic x(5) value "0".
       77  env-cod-prodotto         pic x(2).       
       77  env-cod-servizio         pic x(3).          

       77  sorta-colli              pic x value space.
           88 si-sorta              value "S".

       77  COMANDO-UNIX             PIC X(256).

       01  EXTEND-STAT.
           03 PRI-ERR               pic XX.
           03 SEC-ERR               pic X(10).

       77  TEXT-MESSAGE             pic X.
       77  File-name-err            pic X(15).
       77  status-of-file           pic XX.

       77  Calling-Program          pic X(20) value spaces.

070904 77  msg-error                pic x(200).

       77  wstampa                  pic x(256).
       77  path-flusso-rep          pic x(256).
       77  path-er-flusso           pic x(256).

      * 77  path-imp-esiti-p             pic x(256).

       77  como-peso-int-x          pic x(15).
       77  como-peso-dec-x          pic x(3).       
       77  como-numerico-x          pic x(18).       
     
       77  como-peso-int            pic 9(4).
       77  como-peso-dec            pic 9(3).

       77  status-code              pic 9.
       01  file-info.
           02 file-size             pic x(8) usage is comp-x.
           02 file-date             pic 9(8) usage is comp-x.
           02 file-time             pic 9(8) usage is comp-x.
       77  PathFile                 pic x.

       01  como-occurs.
           05 como-occurs-unstring  pic x(200)  occurs 100.

       77  idx                      pic 9(3).
       77  cont                     unsigned-int.

       77  record-counter           pic 9(5).
       77  rec-counter-ed           pic z(5).

       77  cont-2                   pic 9(5).
       77  como-data-2              pic 9(8).
       77  como-ora-2               pic 9(8).

       01                           pic 9.
           88 anno-auto             value 1 false zero.

       linkage section.
       copy "link-imp-esiti-p.def".

      *-----------------------------------------------------------------*
       procedure division using imp-esiti-p-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use error  procedure on lineseq.
           set errori to true.

           if status-lineseq = "35"
              set lineseq-non-aperto to true
              perform SCRIVI-FILE-ERRORI-SEQ
              move -2 to imp-esiti-p-status
           end-if.

       TORDINI-ERR SECTION.
           use error  procedure on tordini.
           set errori to true.

           evaluate status-tordini
           when "35" 
                set ordini-non-aperto to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -2 to imp-esiti-p-status
           when "99"
                set ordini-locked  to true
                set RecLocked      to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-esiti-p-status
           when "9D"
           when other
                move -1 to imp-esiti-p-status
                set recLocked to false
                set ordini-locked   to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-esiti-p-status
      **
      **          call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
070904**          initialize msg-error
070904**          string "Programma: imp-esiti-p " delimited by size
070904**                 "File error "        delimited by size
070904**                 EXTEND-STAT          delimited by size
070904**                 " on [ORDINI]"     delimited by size
070904**                 into msg-error
070904**          end-string
070904**          display msg-error upon syserr
      *****     when "9d" set RecLocked to false
           end-evaluate.

       VETTEL-ERR SECTION.
           use error  procedure on vettel.
           set errori to true.

           evaluate status-vettel
           when "9D"
           when other
                move -1 to imp-esiti-p-status
                set recLocked to false
                set ordine-bloccato   to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-esiti-p-status
           end-evaluate.

       END DECLARATIVES.


      ***---
       MAIN-PARAGRAPH.
           perform INIT.

           if tutto-ok 
              perform OPEN-FILES 
           end-if.

           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move  "GESLUX - Importazioni Esiti consegna" to titolo

           set tutto-ok            to true.

           initialize errori-flusso. 
           move zero      to imp-esiti-p-status, imp-esiti-p-num-imp.
           set RecLocked  to false.
           set Prima-volta     to true.
           
           accept como-ora from time.
           accept como-data from century-date.

      *     open input tparamge
      *     move space  to tge-codice.
      *     read tparamge.
      *     close tparamge.


           open input tvettori

           move imp-esiti-p-vettore   to r-codforn
                                         vet-codice.
           read tvettori
           move vet-descrizione       to r-ragsoc.
           close tvettori.

           perform OPEN-VETTEL.

      ***---
       OPEN-FILES.
           perform OPEN-TORDINI.
           if tutto-ok
              perform OPEN-EORDINI
              if tutto-ok 
                 perform OPEN-INPUT-lineseq
                 if tutto-ok
                    perform OPEN-OTHER-FILE
                 end-if
              end-if
           end-if.

      ***---  
       OPEN-OTHER-FILE.
           open input tvettori
                      tparamge
                      tesconsvet.

           move space  to tge-codice.
           read tparamge invalid continue end-read.

      ***---
       OPEN-TORDINI.
           set RecLocked to false.
           open i-o tordini.
           
           if status-tordini = "9D"
              call "C$RERR" using extend-stat, text-message
              display message box "Errore ",  extend-stat,
                                  " sul file TORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-esiti-p-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-esiti-p-status
           end-if.

      ***---
       OPEN-EORDINI.
           set RecLocked to false.
           open i-o eordini.
           
           if status-eordini = "9D"
              call "C$RERR" using extend-stat, text-message
              display message box "Errore ",  extend-stat,
                                  " sul file EORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-esiti-p-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-esiti-p-status
           end-if.

      ***---
       OPEN-VETTEL.
           open input vettel.

           move imp-esiti-p-vettore  to vtt-codice.
           read vettel no lock 
                invalid set errori to true 
           end-read.

           if errori  
      *        set ElaboraBolle              to true
              set flusso-offset-inesistente to true
              move -3 to imp-esiti-p-status  
              perform SCRIVI-FILE-ERRORI-SEQ
              perform CLOSE-ER-FLUSSO
           else
              if vtt-rif-lubex   = zero or 
                 vtt-esito = zero
                 set errori to true
                 set flusso-offset-non-valorizzato to true
              end-if
              if vtt-anno-rif-lubex = zero
                 set anno-auto  to true
              else
                 set anno-auto  to false
              end-if
              perform PREPARA-PERCORSI
              if errori
                 move -3 to imp-esiti-p-status
                 perform SCRIVI-FILE-ERRORI-SEQ
                 perform CLOSE-ER-FLUSSO
              end-if
           end-if.

      ***---
       PREPARA-PERCORSI.
           inspect vtt-path-environment  replacing trailing space 
                                                         by low-value
           inspect vtt-path-environment-client  replacing trailing space 
                                                         by low-value
           inspect vtt-path-suff-tmp     replacing trailing space 
                                                         by low-value
           inspect vtt-path-suff-err    replacing trailing space 
                                                         by low-value
           inspect vtt-path-suff-log    replacing trailing space 
                                                         by low-value

           initialize path-flusso-rep.
           string vtt-path-environment         delimited by low-value
                  "/"                          delimited by size
                  vtt-path-suff-log            delimited by low-value
                  "/rep-imp-esiti-p_"          delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  ".txt"                       delimited by size
                  into path-flusso-rep.
           move path-flusso-rep to imp-esiti-p-path-esito
           initialize imp-esiti-p-path-esito-c.
           string vtt-path-environment-client  delimited by low-value
                  "/"                          delimited by size
                  vtt-path-suff-log            delimited by low-value
                  "/rep-imp-esiti-p_"          delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  ".txt"                       delimited by size
                  into imp-esiti-p-path-esito-c.

           initialize path-er-flusso.
           string vtt-path-environment         delimited by low-value
                  "/"                          delimited by size
                  vtt-path-suff-err            delimited by low-value
                  "/imp-esiti-p-err_"          delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  ".txt"                       delimited by size
                  into path-er-flusso.
           move path-er-flusso  to imp-esiti-p-path-err

           initialize imp-esiti-p-path-err-c.
           string vtt-path-environment-client  delimited by low-value
                  "/"                          delimited by size
                  vtt-path-suff-err            delimited by low-value
                  "/imp-esiti-p-err_"          delimited by size
                  como-data                    delimited by size
                  "_"                          delimited by size
                  como-ora                     delimited by size
                  ".txt"                       delimited by size
                  into imp-esiti-p-path-err-c

           initialize wstampa
           string imp-esiti-p-path-orig  delimited by low-value
                  "/"                    delimited by size
                  imp-esiti-p-file-name  delimited by size
                  into wstampa.

      ***---
       OPEN-INPUT-LINESEQ.
           set tutto-ok to true.
           open input lineseq.

           if tutto-ok
              initialize line-riga
              read lineseq next
                 at end
                    set errori       to true
                    set lineseq-vuoto to true
                    move -3 to imp-esiti-p-status
                    perform SCRIVI-FILE-ERRORI-SEQ
                    perform CLOSE-ER-FLUSSO
                 not at end
      *    se ho l'intestazione provo a leggere un record in più
                    if vtt-si-riga-int
                       read lineseq next
                          at end
                             set errori       to true
                             set lineseq-vuoto to true
                             move -3 to imp-esiti-p-status
                             perform SCRIVI-FILE-ERRORI-SEQ
                             perform CLOSE-ER-FLUSSO
                       end-read
                    end-if
                    if tutto-ok                       
                       perform MOVE-TO-STRUTTURA
                    end-if        
              end-read
              close lineseq
              open input lineseq
           end-if.

      ***---
      * Creo il tmp contenente la chiave dei file errati
       ELABORAZIONE.
           move zero   to record-counter
      *    elimino l'eventuale record di intestazione
           if vtt-si-riga-int
              initialize line-riga
              read lineseq next at end continue end-read
           end-if
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
090604        if line-riga(1:1) = x"1A" 
090604           exit perform
090604        end-if
              add 1 to record-counter
              set tutto-ok   to true
              perform MOVE-TO-STRUTTURA
      *        perform CHECK-DATI
              if errori
                 perform SCRIVI-FILE-ERRORI-SEQ    
              else
                 perform VALIDA-RECORD
                 if tutto-ok
                    perform VALORIZZA-RECORD
                 end-if
              end-if
           end-perform.
           close lineseq.
            
      ***---
       VALORIZZA-RECORD.
      *    decodifico la causale vettore
           evaluate vtt-siglia-pers
           when "PERS-BARTOLINI"
                if como-dt-ap-giacenza not = zero
                   move "GIAC"        to tev-esito
                else
                   move vet-codice    to tev-vet
                   move como-esito    to tev-codice
                   read tesconsvet
                      invalid
                         set no-esito to true
                         perform SCRIVI-FILE-ERRORI-SEQ
                         move -2 to imp-esiti-p-status
                         exit paragraph
                   end-read
                end-if
           when other
                move vet-codice   to tev-vet
                move como-esito   to tev-codice
                read tesconsvet
                   invalid
                      set no-esito to true
                       perform SCRIVI-FILE-ERRORI-SEQ
                       move -2 to imp-esiti-p-status
                       exit paragraph
                end-read
           end-evaluate

      *     move como-rif-lubex        to tor-numero
           if vtt-evasione-si  
              move como-rif-lubex    to tor-numero
              if anno-auto
                 move tge-anno  to tor-anno
                 read tordini no lock
                    invalid
                       subtract 1 from tor-anno
                       read tordini no lock
                          invalid
                             set no-evasione to true
                             perform SCRIVI-FILE-ERRORI-SEQ
                             exit paragraph
                       end-read
                 end-read
              else
                 move como-anno-rif-lubex   to tor-anno
                 read tordini no lock
                    invalid
                       set no-evasione to true
                       perform SCRIVI-FILE-ERRORI-SEQ
                       exit paragraph
                 end-read
              end-if
           else
              move como-rif-lubex        to tor-num-bolla
              if anno-auto
                 move tge-anno  to tor-anno-bolla
                 read tordini no lock key k-bolla
                    invalid
                       subtract 1 from tor-anno-bolla
                       read tordini no lock key k-bolla
                          invalid
                             set no-bolla to true
                             perform SCRIVI-FILE-ERRORI-SEQ
                             exit paragraph
                       end-read
                 end-read
              else
                 move como-anno-rif-lubex   to tor-anno-bolla
                 read tordini no lock key k-bolla
                    invalid
                       set no-bolla to true
                       perform SCRIVI-FILE-ERRORI-SEQ
                       exit paragraph
                 end-read
              end-if
           end-if.

           read tordini lock
                invalid continue
           end-read

           if not reclocked
              move tev-esito        to tor-esito-consegna
      *****        move como-data        to tor-data-ultima-modifica
      *****        move como-ora         to tor-ora-ultima-modifica
      *****        move imp-esiti-p-user to tor-utente-ultima-modifica

              rewrite tor-rec invalid continue end-rewrite
              unlock tordini all record
           else
              set RecLocked  to false
           end-if


      *     initialize art-rec.
           perform RECUPERA-NUM-ESITO.

           move como-vettore         to eor-vettore.
           move tev-esito            to eor-esito.

           if como-dt-consegna = zero
              accept eor-dt-consegna from century-date
           else
              move como-dt-consegna  to eor-dt-consegna
           end-if.
           move como-ora-consegna    to eor-ora-consegna.
           move como-dt-avviso       to eor-dt-avviso.
           move como-dt-ap-giacenza  to eor-dt-ap-giacenza.

           move como-data            to eor-data-creazione
                                        eor-data-ultima-modifica.
           move como-ora             to eor-ora-creazione
                                        eor-ora-ultima-modifica.
           move imp-esiti-p-user     to eor-utente-creazione
                                        eor-utente-ultima-modifica.

           write eor-rec invalid continue end-write.

           perform SCRIVI-LIST.
           add 1 to imp-esiti-p-num-imp.

      ***---
       VALIDA-RECORD.
           evaluate true
           when vtt-import-csv
                if vtt-import-includi-da not = zero
                   if como-occurs-unstring(vtt-import-includi-da) 
                          not = vtt-import-includi
                      set errori to true
                   end-if
                end-if

                if vtt-import-escludi-da not = zero
                   if como-occurs-unstring(vtt-import-escludi-da) = 
                      vtt-import-escludi
                      set errori to true
                   end-if
                end-if
           when vtt-import-txt
                if vtt-import-includi-da not = zero
                   if line-riga(vtt-import-includi-da:
                                     vtt-import-includi-per) not = 
                      vtt-import-includi(vtt-import-includi-da:
                                     vtt-import-includi-per)
                      set errori to true
                   end-if    
                end-if

                if vtt-import-escludi-da not = zero
                   if line-riga(vtt-import-escludi-da:
                                     vtt-import-escludi-per) = 
                      vtt-import-escludi(vtt-import-escludi-da:
                                     vtt-import-escludi-per)
                      set errori   to true
                   end-if
                end-if
           end-evaluate.

      ***---
       RECUPERA-NUM-ESITO.
           move tor-chiave   to eor-tor-chiave
           move high-value   to eor-num-riga
           start eordini key not > eor-chiave
              invalid
                 move zero   to eor-num-riga
              not invalid
                 read eordini previous no lock
                    at end
                       move zero   to eor-num-riga
                    not at end
                       if tor-chiave not = eor-tor-chiave
                          move zero   to eor-num-riga
                       end-if
                 end-read
           end-start
           add 1 to eor-num-riga
           move tor-chiave   to eor-tor-chiave
           initialize eor-dati.

      ***---
       MOVE-TO-STRUTTURA.
           initialize como-record replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           evaluate true
           when vtt-import-txt
                perform MOVE-TO-STRUTTURA-TXT
           when vtt-import-csv
                perform MOVE-TO-STRUTTURA-CSV
           end-evaluate.

      ***---
       MOVE-TO-STRUTTURA-TXT.
           move line-riga(vtt-rif-lubex:vtt-rif-lubex-per)
                       to como-rif-lubex convert.
           if vtt-anno-rif-lubex not = zero
              move line-riga(vtt-anno-rif-lubex:vtt-anno-rif-lubex-per)
                       to como-anno-rif-lubex convert
           end-if.
           move imp-esiti-p-vettore to como-vettore.

           move line-riga(vtt-esito:vtt-esito-per) to como-esito.

           if vtt-data-cons not = zero and 
              vtt-data-cons-per not = zero
              move line-riga(vtt-data-cons:vtt-data-cons-per)
                       to como-tipo-data convert
              move vtt-data-cons-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-consegna convert
           end-if.


           if vtt-ora-cons not = zero and 
              vtt-ora-cons-per not = zero
              move line-riga(vtt-ora-cons:vtt-ora-cons-per)
                       to como-tipo-ora convert
              move vtt-ora-cons-tipo to tipo-ora
              perform SETTA-ORA
              move como-tipo-ora  to como-ora-consegna convert
           end-if.


           if vtt-data-avviso not = zero and 
              vtt-data-avviso-per not = zero
              move line-riga(vtt-data-avviso:vtt-data-avviso-per)
                       to como-tipo-data convert
              move vtt-data-avviso-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-avviso
           end-if.

           if vtt-data-ap-giac not = zero and 
              vtt-data-ap-giac-per not = zero
              move line-riga(vtt-data-ap-giac:vtt-data-ap-giac-per)
                       to como-tipo-data convert
              move vtt-data-ap-giac-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-ap-giacenza
           end-if.

      ***---
       MOVE-TO-STRUTTURA-CSV.
           initialize como-occurs
           unstring line-riga delimited by vtt-delim-csv
                    into como-occurs-unstring(1)
                         como-occurs-unstring(2)
                         como-occurs-unstring(3)
                         como-occurs-unstring(4)
                         como-occurs-unstring(5)
                         como-occurs-unstring(6)
                         como-occurs-unstring(7)
                         como-occurs-unstring(8)
                         como-occurs-unstring(9)
                         como-occurs-unstring(10)
                         como-occurs-unstring(11)
                         como-occurs-unstring(12)
                         como-occurs-unstring(13)
                         como-occurs-unstring(14)
                         como-occurs-unstring(15)
                         como-occurs-unstring(16)
                         como-occurs-unstring(17)
                         como-occurs-unstring(18)
                         como-occurs-unstring(19)
                         como-occurs-unstring(20)
                         como-occurs-unstring(21)
                         como-occurs-unstring(22)
                         como-occurs-unstring(23)
                         como-occurs-unstring(24)
                         como-occurs-unstring(25)
                         como-occurs-unstring(26)
                         como-occurs-unstring(27)
                         como-occurs-unstring(28)
                         como-occurs-unstring(29)
                         como-occurs-unstring(30)
                         como-occurs-unstring(31)
                         como-occurs-unstring(32)
                         como-occurs-unstring(33)
                         como-occurs-unstring(34)
                         como-occurs-unstring(35)
                         como-occurs-unstring(36)
                         como-occurs-unstring(37)
                         como-occurs-unstring(38)
                         como-occurs-unstring(39)
                         como-occurs-unstring(40)
                         como-occurs-unstring(41)
                         como-occurs-unstring(42)
                         como-occurs-unstring(43)
                         como-occurs-unstring(44)
                         como-occurs-unstring(45)
                         como-occurs-unstring(46)
                         como-occurs-unstring(47)
                         como-occurs-unstring(48)
                         como-occurs-unstring(49)
                         como-occurs-unstring(50)
                         como-occurs-unstring(51)
                         como-occurs-unstring(52)
                         como-occurs-unstring(53)
                         como-occurs-unstring(54)
                         como-occurs-unstring(55)
                         como-occurs-unstring(56)
                         como-occurs-unstring(57)
                         como-occurs-unstring(58)
                         como-occurs-unstring(59)
                         como-occurs-unstring(60)
                         como-occurs-unstring(61)
                         como-occurs-unstring(62)
                         como-occurs-unstring(63)
                         como-occurs-unstring(64)
                         como-occurs-unstring(65)
                         como-occurs-unstring(66)
                         como-occurs-unstring(67)
                         como-occurs-unstring(68)
                         como-occurs-unstring(69)
                         como-occurs-unstring(70)
                         como-occurs-unstring(71)
                         como-occurs-unstring(72)
                         como-occurs-unstring(73)
                         como-occurs-unstring(74)
                         como-occurs-unstring(75)
                         como-occurs-unstring(76)
                         como-occurs-unstring(77)
                         como-occurs-unstring(78)
                         como-occurs-unstring(79)
                         como-occurs-unstring(80)
                         como-occurs-unstring(81)
                         como-occurs-unstring(82)
                         como-occurs-unstring(83)
                         como-occurs-unstring(84)
                         como-occurs-unstring(85)
                         como-occurs-unstring(86)
                         como-occurs-unstring(87)
                         como-occurs-unstring(88)
                         como-occurs-unstring(89)
                         como-occurs-unstring(90)
                         como-occurs-unstring(91)
                         como-occurs-unstring(92)
                         como-occurs-unstring(93)
                         como-occurs-unstring(94)
                         como-occurs-unstring(95)
                         como-occurs-unstring(96)
                         como-occurs-unstring(97)
                         como-occurs-unstring(98)
                         como-occurs-unstring(99)
                         como-occurs-unstring(100).

      *    se sono presenti i doppi apici faccio il ciclo per toglierli
           if vtt-delim-x-csv not = space
              perform varying cont from 1 by 1 until cont > 100
                 if como-occurs-unstring(cont)(1:1) = vtt-delim-x-csv
                    initialize cont-2
                    inspect como-occurs-unstring(cont) 
                          replacing trailing space by low-value
                    inspect como-occurs-unstring(cont) tallying cont-2
                             for characters before low-value
      *              add 1 to cont-2
                    move " " to como-occurs-unstring(cont)(cont-2:1)
                                como-occurs-unstring(cont)(1:1)
                    inspect como-occurs-unstring(cont) 
                          replacing trailing low-value by space
                    call "c$justify" using como-occurs-unstring(cont) 
                                           "L"
                 end-if
              end-perform
           end-if

           move como-occurs-unstring(vtt-rif-lubex)
                       to como-rif-lubex convert
           if vtt-anno-rif-lubex not = zero
              move como-occurs-unstring(vtt-anno-rif-lubex)
                       to como-anno-rif-lubex convert
           end-if
           move imp-esiti-p-vettore   to como-vettore

           move como-occurs-unstring(vtt-esito)   to como-esito 

           if vtt-data-cons not = zero 
              move como-occurs-unstring(vtt-data-cons)
                                            to como-tipo-data convert
              move vtt-data-cons-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-consegna convert
           end-if. 

           if vtt-ora-cons not = zero 
              move como-occurs-unstring(vtt-ora-cons)
                                            to como-tipo-ora convert
              move vtt-ora-cons-tipo to tipo-ora
              perform SETTA-ORA
              move como-tipo-ora  to como-ora-consegna convert
           end-if.  

           if vtt-data-avviso not = zero 
              move como-occurs-unstring(vtt-data-avviso)
                       to como-tipo-data convert
              move vtt-data-avviso-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-avviso
           end-if.

           if vtt-data-ap-giac not = zero 
              move como-occurs-unstring(vtt-data-ap-giac)
                                            to como-tipo-data convert
              move vtt-data-ap-giac-tipo to tipo-data
              perform SETTA-DATA
              move como-tipo-data  to como-dt-ap-giacenza
           end-if.


      ***---
       SCRIVI-LIST.
           if not TestataGiaFatta
              set TestataGiaFatta   to true
                          
              open output flusso-rep
              initialize rec-flusso-rep
              move spaces to rec-flusso-rep
              write rec-flusso-rep

              write rec-flusso-rep from riga-cod-cli-ragsoc
              move  imp-esiti-p-file-name   to r-file
              write rec-flusso-rep from Riga-file
              write rec-flusso-rep from SPACES

              string " - Lista riepilogativa degli"  delimited by size
                     " esiti importati in data: "    delimited by size
                     como-data(7:2)                  delimited by size
                     "/"                             delimited by size
                     como-data(5:2)                  delimited by size
                     "/"                             delimited by size
                     como-data(1:4)                  delimited by size
                     " -"                            delimited by size
                    into rec-flusso-rep
              end-string
              call "c$justify" using rec-flusso-rep, "C"
              write rec-flusso-rep              

              write rec-flusso-rep from space

       
              write rec-flusso-rep from testa-report
              move all "-" to rec-flusso-rep(2:)
              write rec-flusso-rep
              move spaces to rec-flusso-rep
           end-if.    

           move eor-anno        to r-anno
           move eor-num-ordine  to r-ordine
           move eor-esito       to r-esito.
           write rec-flusso-rep from riga-report.

      ****---
      * CHECK-DATI.
      *     set tutto-ok to true.
      *     initialize errori-flusso.
      *
      *     if tutto-ok and como-descrizione = spaces
      *        set errori to true
      *        set flusso-not-descr-art to true
      *     end-if                
      *
      *     if tutto-ok and como-cod-art-cli = spaces
      *        set errori to true
      *        set flusso-not-cod-art to true
      *     end-if.

      ***---
       SCRIVI-FILE-ERRORI-SEQ.
           if NonAncoraAperto
              set FileAperto to true                            
              open output er-flusso          
           end-if.
      
           initialize er-f-rec.             
      
           if prima-volta
              set prima-volta to false
              write er-f-rec from riga-cod-cli-ragsoc after 1
              move  imp-esiti-p-file-name   to r-file
              write er-f-rec from Riga-file           after 1
              write er-f-rec from SPACES              after 1
           end-if.

           initialize er-f-rec
           move record-counter   to rec-counter-ed
           evaluate true
           when no-esito
                initialize er-f-rec
                string "record "      delimited by size
                       rec-counter-ed delimited by size
                       ": Tipologia Esito sconosciuta!" 
                                      delimited by size
                       into er-f-rec
           when no-bolla
                string "record "              delimited by size
                       rec-counter-ed         delimited by size
                       ": Bolla inesistente!" delimited by size
                       into er-f-rec

           when ordine-bloccato
           when ordini-locked
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Ordine Bloccato da altro utente!" 
                                               delimited by size
                       "Non aggiornato l'esito sull'ordine"
                                               delimited by size
                       into er-f-rec

           when lineseq-non-aperto
                move "Impossibile aprire il flusso di import!"   
                                                        to er-f-rec
           when ordini-non-aperto
                move "Impossibile aprire il file TORDINI!"  to er-f-rec
           when flusso-offset-inesistente
                move "Dati telematici del cliente inesistenti"  
                 to er-f-rec
           when flusso-offset-non-valorizzato
                move "Dati telematici degli articoli non valorizzati"
                    to er-f-rec
           when lineseq-vuoto
                move "Flusso di import vuoto!" to er-f-rec
           when lineseq-incoerente
                move "Flusso di import non valido!" to er-f-rec
           end-evaluate.

           write er-f-rec.
           move -1 to imp-esiti-p-status.

      ***---
       CLOSE-FILES.
           perform CLOSE-ER-FLUSSO.
           perform CLOSE-LIST.

           close tordini
                 eordini
                 tvettori
                 vettel
                 tparamge
                 tesconsvet.


      ***---
       CLOSE-ER-FLUSSO.
           if FileAperto
              move  spaces to er-f-rec

              write er-f-rec
              move "-------------- End of file --------------" 
                to er-f-rec                       
              write ER-F-REC

              write er-f-rec from space
              close er-flusso
           end-if.

      ***---
       CLOSE-LIST.
           if TestataGiaFatta
              write rec-flusso-rep from space
              move "-------------- End of file --------------"
                    to rec-flusso-rep
              write rec-flusso-rep
              write rec-flusso-rep from space
              close flusso-rep
      *        move path-ascolist to LinkPathList
           end-if.

      ***---
       SETTA-DATA.
           evaluate true
           when AAAAMMGG
                continue
           when GGMMAAAA
                move como-tipo-data(1:2) to como-data-2(7:2)
                move como-tipo-data(3:2) to como-data-2(5:2)
                move como-tipo-data(5:4) to como-data-2(1:4)
                move como-data-2         to como-tipo-data

           when AAMMGG
                move como-tipo-data(1:6) to como-tipo-data(3:6)
                move "20"                to como-tipo-data(1:2)
           when GGMMAA
                move como-tipo-data(1:2) to como-data-2(7:2)
                move como-tipo-data(3:2) to como-data-2(5:2)
                move como-tipo-data(5:2) to como-data-2(3:2)
                move "20"                to como-data-2(1:2)
                move como-data-2         to como-tipo-data
           end-evaluate.

      ***---
       SETTA-ORA.
           evaluate true
           when HHMMSSCC
                continue
           when HHMMSS
                move como-tipo-ora(1:6)  to como-ora-2(1:6)
                move "00"                to como-ora-2(7:2)
                move como-ora-2          to como-tipo-ora
           when HHMM
                move como-tipo-ora(1:4)  to como-ora-2(1:4)
                move "0000"              to como-ora-2(5:4)
                move como-ora-2          to como-tipo-ora
           end-evaluate.

      ***---
       EXIT-PGM.
      *     call "c$delete" using path-imp-esiti-p.

           goback.
