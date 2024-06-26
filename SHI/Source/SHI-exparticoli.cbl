       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-exparticoli.
       AUTHOR.                          Luciano.
       REMARKS. EXPORT dei articoli
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "paramshi.sl".
           copy "progmag.sl".
           copy "timbalqta.sl".
           copy "tivaese.sl".
           copy "prodener.sl".
           copy "lineseq.sl".
           copy "fileseq.sl".
       SELECT lineseq2
           ASSIGN       TO  wstampa2
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq2.
       SELECT lineseq3
           ASSIGN       TO  wstampa3
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq3.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "paramshi.fd".
           copy "progmag.fd".
           copy "timbalqta.fd".
           copy "tivaese.fd".
           copy "prodener.fd".
           copy "fileseq.fd".
       FD  lineseq.
       01  line-riga         pic  x(10000).
       fd  lineseq2.
       01  line-riga2        pic  x(900).
       fd  lineseq3.
       01  line-riga3        pic  x(900).

       WORKING-STORAGE SECTION.
       copy "exp-ws.def".

       78  titolo                value "Export articoli".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  status-lineseq        pic xx.
       77  status-lineseq2       pic xx.
       77  status-lineseq3       pic xx.
       77  status-articoli       pic xx.
       77  status-paramshi       pic xx.
       77  status-progmag        pic xx.
       77  status-timbalqta      pic xx.
      * 77  status-tmarche        pic xx.
      * 77  status-tsetmerc       pic xx.
      * 77  status-tcla1art       pic xx.
       77  status-tivaese        pic xx.
       77  status-prodener       pic xx.
       77  wstampa               pic x(256).
       77  wstampa2              pic x(256).
       77  wstampa3              pic x(256).

       77  WrittenRows           pic 99 value 0.

       77  path-txt              pic x(256).

       01  filler                pic 9.
         88 si-imballi           value 1, false 0.

       01  tabella-imballi.
           05 imballo-articolo  pic x(3) occurs 200.

       01  tabella-prodener.
           05 prodener-el  pic x(100) occurs 10000.

       77  idx  pic 9(5).

       77  exp-prg-peso-utf     PIC  9(5)v9(3).
       77  exp-prg-peso-non-utf PIC  9(5)v9(3).

       77  cont-ean                pic 9.
       77  como-aliquota           pic 9(4).

       77  num-rec-prodener        pic 9(10).
       77  num-rec-ean             pic 9(10).


       01  exp-art-det-rec.
           05 HARTDET_ART_COM_CODICE  pic x(15).
           05 HARTDET_ART_CODICE      pic x(25).
           05 HARTDET_QTAEAN          pic 9(7)v9(3).
           05 HARTDET_CODICE          pic x(25).
           05 HARTDET_UOM_CODICE      pic x(2 ).

       01  exp-art-rec.
           05 HART_COM_CODICE            pic x(15).   
           05 HART_CODICE                pic x(25).   
           05 HART_ESTENSIONE            pic x(10).   
           05 HART_DESCRIZIONE           pic x(60).   
           05 HART_CLASSI                pic x(10).     |120
           05 HART_PZCF                  pic 9(11)v9(3).   
           05 HART_CFCA                  pic 9(11)v9(3).   
           05 HART_CAUDC                 pic 9(11)v9(3).  |162 
           05 HART_CNT_CODICE_UDC        pic x(10).   
           05 HART_CNT_CODICE_UDS        pic x(10).   
           05 HART_SEQ_TRASF_IN          pic x(80).   
           05 HART_SEQ_TRASF_OUT         pic x(80).       |342
           05 HART_PZ_LARGHEZZA          pic 9(11)v9(3).
           05 HART_PZ_LUNGHEZZA          pic 9(13)v9(3).   
           05 HART_PZ_ALTEZZA            pic 9(11)v9(3).   
           05 HART_PZ_PESO               pic 9(11)v9(3).   |398
           05 HART_CF_LARGHEZZA          pic 9(11)v9(3).   
           05 HART_CF_LUNGHEZZA          pic 9(13)v9(3).  
           05 HART_CF_ALTEZZA            pic 9(11)v9(3).  
           05 HART_CF_PESO               pic 9(11)v9(3).   |454
           05 HART_SEQ_ELAB              pic x(80).   
           05 HART_VPK_NUMERO            pic 9(5).         |539
           05 HART_UDF1                  pic x(200).  
           05 HART_UDF2                  pic x(200).  
           05 HART_UDF3                  pic x(200).  
           05 HART_UDF4                  pic x(200).  
           05 HART_UDF5                  pic x(200).      |1539
           05 HART_UDF6                  pic x(200).  
           05 HART_UDF7                  pic x(200).  
           05 HART_UDF8                  pic x(200).  
           05 HART_UDF9                  pic x(200).  
           05 HART_UDF10                 pic x(200).      |2539
           05 HART_GEST_LOTTO            pic x(1).  
           05 HART_GEST_DSCADENZA        pic x(1).  
           05 HART_GEST_PESO             pic x(1).  
           05 HART_UOM_CODICE_UNITA      pic x(10).  
           05 HART_UOM_CODICE_PESO       pic x(10).  
           05 HART_FAM_CODICE            pic x(10).   
           05 HART_GEST_MATRICOLA        pic x(1).        |2573
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
           05 HART_B_ANIDRI-PRE          pic 9.           |2618   
           05 HART_B_ANIDRI              pic 9(15)v9(3).
           05 HART_D_ANIDRI-PRE          pic 9.              
           05 HART_D_ANIDRI              pic 9(15)v9(3).
           05 HART_B_IDRATI-pre          pic 9.
           05 HART_B_IDRATI              pic 9(15)v9(3).
           05 HART_D_IDRATI-pre          pic 9.
           05 HART_D_IDRATI              pic 9(15)v9(3).
           05 HART_ADR                   pic x(15).      |2708
           05 HART_PZ_PESO_NETTO         pic 9(11)v9(3).   
           05 HART_UOM_ALTERNATIVA       pic x(10).   
           05 HART_UOM_CONV_NUMERATORE   pic 9(5).   
           05 HART_UOM_CONV_DENOMINATORE pic 9(5).   
           05 HART_UOM_VOLUME            pic x(10).       |2752
           05 HART_FLASHP                pic 9(11)v9(3).
           05 HART_IMBALLO               pic x(1).        |2767
           05 HART_PERC_UTIF             pic 9(5)v9(3).   
           05 HART_QTA_TIMBRA_EST        pic 9(6).    

       01  exp-pen-rec.
           05 exp-pen-codice       PIC  x(10).
           05 exp-pen-descrizione  PIC  x(50).
           05 exp-pen-cpa          PIC  x(4).
           05 exp-pen-nc           PIC  9(8).
           05 exp-pen-taric        PIC  9(2).
           05 exp-pen-dac          PIC  x(4).

       77  cont                    pic 9(5).

       LINKAGE SECTION.
           copy "link-exp.def".

      ******************************************************************
       PROCEDURE DIVISION using exp-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
                move "File gi� in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

       LINESEQ2-ERR SECTION.
           use after error procedure on lineseq2.
           set tutto-ok  to true.
           evaluate status-lineseq2
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
                move "File gi� in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

       LINESEQ3-ERR SECTION.
           use after error procedure on lineseq3.
           set tutto-ok  to true.
           evaluate status-lineseq3
           when "35"
                move "Impossibile procedere. File [LINESEQ] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [LINESEQ] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[LINESEQ] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "93"
                move "File gi� in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       articoli-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                move 
            "Impossibile procedere. File vettori [ARTICOLI] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [ARTICOLI] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[ARTICOLI] Indexed file corrupt!" 
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       PARAMSHI-ERR SECTION.
           use after error procedure on paramshi.
           set tutto-ok  to true.
           evaluate status-paramshi
           when "35"
                move 
           "Impossibile procedere. File paramshi [PARAMSHI] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [PARAMSHI] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[PARAMSHI] Indexed file corrupt!"
                                                  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                move 
           "Impossibile procedere. File progmag [PROGMAG] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [PROGMAG] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[PROGMAG] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on TIMBALQTA.
           set tutto-ok  to true.
           evaluate status-TIMBALQTA
           when "35"
                move 
                 "Impossibile procedere. File [TIMBALQTA] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TIMABLQTA] Mismatch size!" to como-messaggio
                perform SCRIVI-ERORRE
           when "98"
                move "[TIMABLQTA] Indexed file corrupt!"
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on TIVAESE.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                move 
           "Impossibile procedere. File tivaese [TIVAESE] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TIVAESE] Mismatch size!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TIVAESE] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       PRODENER-ERR SECTION.
           use after error procedure on PRODENER.
           set tutto-ok  to true.
           evaluate status-prodener
           when "35"
                move 
           "Impossibile procedere. File vettori [PRODENER] inesistente"
                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [PRODENER] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[PRODENER] Indexed file corrupt!"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
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
           accept como-data from century-date.
           accept como-ora  from time.
           perform INITIALIZE-FLAG
           move zero   to num-rec-prodener
                          num-rec-ean

           set exp-articoli     to true.
           set tutto-ok         to true.
           open input paramshi.
           move space  to shi-codice
           read paramshi invalid continue end-read.
           close paramshi 
           inspect shi-path-elab-exp
                   replacing trailing space by low-value
           inspect shi-path-exp
                   replacing trailing space by low-value
           inspect shi-file-articoli
                   replacing trailing space by low-value
           inspect shi-file-ean
                   replacing trailing space by low-value
           inspect shi-file-prodener
                   replacing trailing space by low-value

           string shi-path-elab-exp delimited low-value
                  "\"               delimited size
                  shi-file-articoli delimited size
             into wstampa.
              
           move wstampa to exp-shi-articoli-file-articoli.

           string shi-path-elab-exp delimited low-value
                  "\"               delimited size
                  shi-file-ean      delimited size
             into wstampa2.  
           move wstampa2 to exp-shi-articoli-file-ean.              

           inspect shi-file-prodener  replacing trailing spaces 
                                      by low-value.
           string shi-path-elab-exp delimited low-value
                  "\"               delimited size
                  shi-file-prodener delimited low-value
                  "_"               delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".txt"            delimited size
             into wstampa3.
           move wstampa3 to exp-shi-articoli-file-prodener.
           initialize tabella-prodener.

      ***---
       OPEN-FILES.
           open output lineseq.
           open output lineseq2.
           open output lineseq3.
           if tutto-ok
              open input articoli
              open input progmag
              open input timbalqta
              open input tivaese
              open input prodener
           end-if.

      ***---
       ELABORAZIONE.
           move low-value to art-codice.    
           start articoli key >= art-chiave
                 invalid  set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read articoli next at end exit perform end-read
                 perform GENERA-FILE
              end-perform
           end-if.
           perform GENERA-PRODENER.
           perform SCRIVI-RIEPILOGO.

      ***---
       SCRIVI-RIEPILOGO.       
           initialize como-messaggio
           move num-rec-exp  to num-rec-ed
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "     delimited size
                   num-rec-ed      delimited low-value
                   " articoli."    delimited size
                   into como-messaggio
           perform SCRIVI-MESSAGGIO

           if num-rec-no-exp not = zero
              move num-rec-no-exp  to num-rec-ed
              initialize como-messaggio
              call "C$justify" using num-rec-ed, "L"
              inspect num-rec-ed replacing trailing space by low-value
              string "Non esportati " delimited size
                     num-rec-ed       delimited low-value
                     " articoli in quanto privi di " delimited by size
                     "progressivi sul magazzino selezionato." 
                                      delimited size
                   into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

           move num-rec-ean  to num-rec-ed
           initialize como-messaggio
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati " delimited size
                  num-rec-ed   delimited low-value
                  " Ean."      delimited size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO

           move num-rec-prodener  to num-rec-ed
           initialize como-messaggio
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "  delimited size
                  num-rec-ed    delimited low-value
                  " Prodotti energetici." 
                                delimited size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO.


      ***---
       GENERA-FILE.
           perform CICLO-IMBALLI.
           if si-imballi
              perform CICLO-PRODENER

              perform GEN-ART-PARTE-FISSA

              perform varying cont from 1 by 1 until cont > 200
                 if imballo-articolo(cont) = space
                    exit perform
                 end-if
                 perform GEN-ART-PARTE-VARIABILE
              end-perform
              add 1 to num-rec-exp
           else
              add 1 to num-rec-no-exp
           end-if.

      ***---
       CICLO-IMBALLI.
           move zero   to exp-prg-peso-utf
                          exp-prg-peso-non-utf.

           set si-imballi  to false.
           initialize tabella-imballi.
           move art-codice to prg-cod-articolo.

           if shi-mag-SHI-1 = space
              move low-value     to prg-cod-magazzino
           else
              move shi-mag-SHI-1 to prg-cod-magazzino
           end-if.
           move low-value    to prg-tipo-imballo
                                prg-peso.

           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if art-codice not = prg-cod-articolo
                       exit perform
                    end-if
                    if shi-mag-SHI-1 not = spaces and
                       shi-mag-SHI-1 not = prg-cod-magazzino
                       exit perform
                    end-if
                    if prg-tipo-imballo not = spaces and prg-attivo
                       perform varying cont from 1 by 1 until cont > 200
                          if imballo-articolo(cont) = prg-tipo-imballo
                             exit perform
                          end-if

                          if imballo-articolo(cont) = space
                             move prg-tipo-imballo   
                               to imballo-articolo(cont)

                             if cont = 1
                                move prg-peso-utf 
                                  to exp-prg-peso-utf
                                move prg-peso-non-utf   
                                  to exp-prg-peso-non-utf
                             end-if

                             set si-imballi to true
                             exit perform
                          end-if
                       end-perform
                    end-if
                 end-perform
           end-start.
                                            
           if not si-imballi      
              move art-codice to prg-cod-articolo
              if shi-mag-SHI-2 = space
                 move low-value     to prg-cod-magazzino
              else
                 move shi-mag-SHI-2 to prg-cod-magazzino
              end-if
              move low-value    to prg-tipo-imballo
                                   prg-peso
              
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if art-codice not = prg-cod-articolo
                          exit perform
                       end-if
                       if shi-mag-SHI-2 not = spaces and
                          shi-mag-SHI-2 not = prg-cod-magazzino
                          exit perform
                       end-if
                       if prg-tipo-imballo not = spaces and prg-attivo
                          perform varying cont from 1 by 1 
                                    until cont > 200
                             if imballo-articolo(cont) = 
                                prg-tipo-imballo
                                exit perform
                             end-if
                             if imballo-articolo(cont) = space
                                move prg-tipo-imballo   
                                  to imballo-articolo(cont)
              
                                if cont = 1
                                   move prg-peso-utf 
                                     to exp-prg-peso-utf
                                   move prg-peso-non-utf   
                                     to exp-prg-peso-non-utf
                                end-if
              
                                set si-imballi to true
                                exit perform
                             end-if
                          end-perform
                       end-if
                    end-perform
              end-start
           end-if.                   

      ***---
       CLOSE-FILES.
           close articoli 
                 progmag
                 timbalqta
                 tivaese
                 prodener
                 lineseq
                 lineseq2
                 lineseq3.
  
      ***---
           COPY "exp-procedure.cpy".
           copy "exparticoli-procedure.cpy".
