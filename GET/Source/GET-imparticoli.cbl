       PROGRAM-ID. get-imp-articoli.
       AUTHOR.     Luciano.

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "articoli.sl".
           copy "paramget.sl".
           copy "fileseq.sl".
           copy "lineseq.sl".

       FILE SECTION.          
           copy "articoli.fd".
           copy "paramget.fd".
           copy "fileseq.fd".

       FD  lineseq.
       01 line-riga        PIC  x(32000).

       WORKING-STORAGE SECTION.
           copy "imp-ws.def".

       01  exp-art-rec.
           05 HART_COM_CODICE            pic x(15).   
           05 HART_CODICE                pic x(25).   
           05 HART_ESTENSIONE            pic x(10).   
           05 HART_DESCRIZIONE           pic x(60).   
           05 HART_CLASSI                pic x(10).   
           05 HART_PZCF                  pic 9(11)v9(3).   
           05 HART_CFCA                  pic 9(11)v9(3).   
           05 HART_CAUDC                 pic 9(11)v9(3).   
           05 HART_CNT_CODICE_UDC        pic x(10).   
           05 HART_CNT_CODICE_UDS        pic x(10).   
           05 HART_SEQ_TRASF_IN          pic x(80).   
           05 HART_SEQ_TRASF_OUT         pic x(80).   
           05 HART_PZ_LARGHEZZA          pic 9(11)v9(3).
           05 HART_PZ_LUNGHEZZA          pic 9(13)v9(3).   
           05 HART_PZ_ALTEZZA            pic 9(11)v9(3).   
           05 HART_PZ_PESO               pic 9(11)v9(3).   
           05 HART_CF_LARGHEZZA          pic 9(11)v9(3).  
           05 HART_CF_LUNGHEZZA          pic 9(13)v9(3).  
           05 HART_CF_ALTEZZA            pic 9(11)v9(3).  
           05 HART_CF_PESO               pic 9(11)v9(3).  
           05 HART_SEQ_ELAB              pic x(80).   
           05 HART_VPK_NUMERO            pic 9(5).    
           05 HART_UDF1                  pic x(200).  
           05 HART_UDF2                  pic x(200).  
           05 HART_UDF3                  pic x(200).  
           05 HART_UDF4                  pic x(200).  
           05 HART_UDF5                  pic x(200).  
           05 HART_UDF6                  pic x(200).  
           05 HART_UDF7                  pic x(200).  
           05 HART_UDF8                  pic x(200).  
           05 HART_UDF9                  pic x(200).  
           05 HART_UDF10                 pic x(200).  
           05 HART_GEST_LOTTO            pic x(1).  
           05 HART_GEST_DSCADENZA        pic x(1).  
           05 HART_GEST_PESO             pic x(1).  
           05 HART_UOM_CODICE_UNITA      pic x(10).  
           05 HART_UOM_CODICE_PESO       pic x(10).  
           05 HART_FAM_CODICE            pic x(10).   
           05 HART_GEST_MATRICOLA        pic x(1).    
           05 HART_GIORNI_VITA           pic 9(3).    
           05 HART_GIORNI_PRE_DSCADENZA  pic 9(3).    
           05 HART_PESO_TOLLERANZA       pic 9(2).    
           05 HART_CALCVOL_PZ            pic x(1).    
           05 HART_CALCVOL_CF            pic x(1).    
           05 HART_CALCVOL_POSIZ         pic x(1).    
           05 HART_CTRL_QUALITA          pic x(1).    
           05 HART_ORE_QUARANTENA        pic 9(3).    
           05 HART_ARTDET_CODICE         pic x(25).   
           05 HART_TIPOIVA               pic x(4).   
           05 HART_B_ANIDRI-PRE          pic 9.              
           05 HART_B_ANIDRI              pic 9(15)v9(3).
           05 HART_D_ANIDRI-PRE          pic 9.              
           05 HART_D_ANIDRI              pic 9(15)v9(3).
           05 HART_B_IDRATI-pre          pic 9.
           05 HART_B_IDRATI              pic 9(15)v9(3).
           05 HART_D_IDRATI-pre          pic 9.
           05 HART_D_IDRATI              pic 9(15)v9(3).
           05 HART_ADR                   pic x(15).   
           05 HART_PZ_PESO_NETTO         pic 9(11)v9(3).   
           05 HART_UOM_ALTERNATIVA       pic x(10).   
           05 HART_UOM_CONV_NUMERATORE   pic 9(5).   
           05 HART_UOM_CONV_DENOMINATORE pic 9(5).   
           05 HART_UOM_VOLUME            pic x(10).   
           05 HART_FLASHP                pic 9(11)v9(3).
           05 HART_IMBALLO               pic x(1).   
           05 HART_PERC_UTIF             pic 9(5)v9(3).   
           05 HART_QTA_TIMBRA_EST        pic 9(6).    


      * FILE STATUS
       77  status-articoli         pic xx.
       77  status-paramget         pic xx.
       77  status-lineseq          pic xx.

       01  controlli                pic xx.
           88 tutto-ok              value "OK".
           88 errori                value "ER".

      * OTHER DATA             
       77  como-data                pic 9(8)   value zero.
       77  como-ora                 pic 9(8)   value zero.

       01  filler                   pic 9.
           88 RecLocked             value 1 false 0.
       77  titolo                   pic x(256).

       01  EXTEND-STAT.
           03 PRI-ERR               pic XX.
           03 SEC-ERR               pic X(10).

       77  TEXT-MESSAGE             pic X.

       77  wstampa                 pic x(256).
       77  path-er-flusso          pic x(256).

       77  record-counter pic 9(5).
       77  rec-counter-ed pic z(5).

       78  barra          value "\".

       77  rec-non-elab      pic 9(5).
       77  rec-elab          pic 9(5).

       LINKAGE SECTION.
           copy "link-imp.def".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING imp-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "93"
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.



       ARTICOLI-ERR SECTION.
           use error  procedure on articoli.
           set errori to true.

           evaluate status-articoli
           when "35"
                move 
            "Impossibile procedere. File [ARTICOLI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "99"
                move record-counter   to rec-counter-ed
                initialize como-messaggio
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Articolo Bloccato da altro utente!" 
                                               delimited by size
                       "Non aggiornati i dati sull'articolo"
                                               delimited by size
                       into como-messaggio
                set errore-bloccante  to false
                perform SCRIVI-ERRORE
                set RecLocked      to true
           when "9D"
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file ARTICOLI."   delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           end-evaluate.


      ***---
       PARAMget-ERR SECTION.
           use after error procedure on paramget.
           set tutto-ok  to true.
           evaluate status-paramget
           when "35"
                move 
            "Impossibile procedere. File vettori [PARAMget] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when "39"
                move "File [PARAMget] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
           when other
                initialize como-messaggio
                call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
                string "Errore "               delimited size
                       EXTEND-STAT             delimited size
                       " sul file PROGMAG."    delimited size
                       " Contattare l'amministratore del Sistema."
                                               delimited size
                       into como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERRORE
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
           perform INITIALIZE-FLAG,
           set imp-articoli  to true.

           move  "GESLUX - Importazioni Dati Articoli" to titolo

           set tutto-ok            to true.

           move zero      to imp-status.
           set RecLocked        to false.
           
           accept como-ora from time.
           accept como-data from century-date.

           move imp-path-log to path-er-flusso

           perform OPEN-PARAMget.
           move zero      to rec-non-elab
                             rec-elab.

      ***---
       OPEN-FILES.
           perform OPEN-articoli.
           if tutto-ok
              perform OPEN-INPUT-lineseq
           end-if.

      ***---
       OPEN-articoli.
           set RecLocked to false.
           open i-o articoli.
           
      ***---
       OPEN-paramget.
           open input paramget.

           move space  to get-codice
           read paramget no lock 
              invalid 
                 set errori to true 
           end-read.

           if get-path-imp   = space or
              get-file-articoli-imp = space
              set errori to true

              set errore-bloccante to true
              move "Dati telematici degli articoli non valorizzati"
                    to como-messaggio
              perform SCRIVI-ERRORE
           end-if
           
           if tutto-ok
              perform PREPARA-PERCORSI
           end-if.

      ***---
       PREPARA-PERCORSI.
           inspect get-path-imp  replacing trailing space by low-value
           initialize wstampa
           string get-path-imp              delimited by low-value
                  barra                     delimited by size
                  get-file-articoli-imp     delimited by size
                  into wstampa.

      ***---
       OPEN-INPUT-lineseq.
           set tutto-ok to true.
           open input lineseq.

           if tutto-ok
              initialize line-riga
              read lineseq next
                 at end
                    set errori       to true
                    move "Flusso di import vuoto!" to como-messaggio
                    perform SCRIVI-ERRORE
              end-read
              close lineseq
              open input lineseq
           end-if.

      ***---
       ELABORAZIONE.
           move zero   to record-counter
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga(1:1) = x"1A" 
                 exit perform
              end-if
              add 1 to record-counter
              set tutto-ok   to true
              perform MOVE-TO-STRUTTURA
              perform VALORIZZA-RECORD
           end-perform.
           close lineseq.

      *    messaggi riepilogativi
           initialize como-messaggio.
           move record-counter     to rec-counter-ed
           string "Letti         " delimited by size
                  rec-counter-ed   delimited by size 
                  " record"        delimited by size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO.

           if rec-non-elab not = zero
              initialize como-messaggio
              move rec-non-elab  to rec-counter-ed
              string "Non elaborati " delimited by size
                     rec-counter-ed   delimited by size 
                     " record"        delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if
           
           if rec-elab not = zero
              initialize como-messaggio
              move rec-elab           to rec-counter-ed
              string "Elaborati     " delimited by size
                     rec-counter-ed   delimited by size 
                     " record"        delimited by size
                     into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.
           
      ***---
       VALORIZZA-RECORD.
           move HART_CODICE(1:6)   to art-codice convert

           read articoli no lock 
              invalid
                 move record-counter  to rec-counter-ed
                 initialize como-messaggio
                 set errore-bloccante to false
                 string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Articolo inesistente!" delimited by size
                       into como-messaggio
                 perform SCRIVI-ERRORE
                 add 1 to rec-non-elab
                 exit paragraph
           end-read

           read articoli lock
              invalid
                 continue
           end-read

           if not reclocked
              move HART_CFCA    to art-conf-cartone
              move HART_CAUDC   to art-cartone-UDC
              move HART_PZ_LARGHEZZA  to art-larghezza-pz
      *    divido per ottenere i cm visto che mi passano i mm
              compute art-larghezza-pz = art-larghezza-pz / 10

              move HART_PZ_ALTEZZA to art-altezza-pz
      *    divido per ottenere i cm visto che mi passano i mm
              compute art-altezza-pz = art-altezza-pz / 10

              move HART_PZ_LUNGHEZZA to art-profondita-pz
      *    divido per ottenere i cm visto che mi passano i mm
              compute art-profondita-pz = art-profondita-pz / 10

              move HART_PZ_PESO to art-peso-get
      *    divido per ottenere i kg visto che mi passano i grammi
              compute art-peso-get = art-peso-get * 1000

              move HART_ADR           to art-adr

              move como-data    to art-data-ultima-modifica
              move como-ora     to art-ora-ultima-modifica
              move imp-user     to art-utente-ultima-modifica

              rewrite art-rec 
                 invalid 
                    continue 
              end-rewrite
              unlock articoli all record
              add 1 to rec-elab
           else
              set RecLocked  to false
              add 1 to rec-non-elab
           end-if.

      ***---
       MOVE-TO-STRUTTURA.
           initialize exp-art-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move line-riga to exp-art-rec.

      ***---
       CLOSE-FILES.
           close articoli
                 paramget.


      ****---
           COPY "imp-procedure.cpy".
