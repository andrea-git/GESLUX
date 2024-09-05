       PROGRAM-ID. SHI-impordini.
       AUTHOR.     Luciano.

       SPECIAL-NAMES. 
           DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "paramshi.sl".
           copy "tmp-imp-ror.sl".
           copy "lineseq.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "tagli.sl".
           copy "tcaumag.sl".
           copy "tscorte.sl".
           copy "clienti.sl".
           copy "tparamge.sl".
           copy "param.sl".
           copy "ttipocli.sl".

       SELECT lineseq2
           ASSIGN       TO  wstampa2
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq2.

       SELECT er-flusso
           ASSIGN       TO  PATH-ER-FLUSSO
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-er-flusso.

       FILE SECTION.          
           copy "tordini.fd".
           copy "rordini.fd".
           copy "paramshi.fd".
           copy "tmp-imp-ror.fd".
           copy "lineseq.fd". 
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "tagli.fd".
           copy "tcaumag.fd".
           copy "tscorte.fd".
           copy "clienti.fd".
           copy "tparamge.fd".
           copy "param.fd".
           copy "ttipocli.fd".

       FD  lineseq2.
       01 line-riga2        PIC  x(32000).
       FD  er-flusso.
       01  er-f-rec         PIC  x(32000).

       WORKING-STORAGE SECTION.
           copy "link-wprogmag.def".
           copy "aggiorna-stato-master.def".
           copy "link-geslock.def".
           copy "versione-evasione.def".

       01  imp-tordini. |1400 crt tot (circa)
           05 HUSCTES_COM_CODICE             PIC  X(15).   
           05 HUSCTES_ORDINE                 PIC  X(15).   
           05 HUSCTES_ORDUSCTIP_CODICE       PIC  X(15).   
           05 HUSCTES_ORD2                   PIC  x(15).   
           05 HUSCTES_COMUSCTIP_CODICE       PIC  x(15).   
           05 HUSCTES_COMCAU_CODICE          PIC  x(10).   
           05 HUSCTES_COMDES_CODICE          PIC  X(10).   
           05 HUSCTES_PRIORITA               PIC  9(2).   
           05 HUSCTES_USCTES_ID              PIC  9(10).   
           05 HUSCTES_STATO                  PIC  x(1).   
           05 HUSCTES_BAT_ID                 PIC  x(10).   
           05 HUSCTES_DATA                   PIC  x(10).   
           05 HUSCTES_DATA_AVANZAMENTO       PIC  X(10).   
           05 HUSCTES_CNTGRUTES_CODICE       PIC  X(10).   
           05 HUSCTES_UDF1                   PIC  X(60).   
           05 HUSCTES_UDF2                   PIC  x(60).   
           05 HUSCTES_UDF3                   PIC  x(60).   
           05 HUSCTES_UDF4                   PIC  x(60).   
           05 HUSCTES_UDF5                   PIC  X(60).   
           05 HUSCTES_UDF6                   PIC  X(60).   
           05 HUSCTES_UDF7                   PIC  X(60).   
           05 HUSCTES_UDF8                   PIC  x(60).   
           05 HUSCTES_UDF9                   PIC  x(60).   
           05 HUSCTES_UDF10                  PIC  x(60).   
           05 HUSCTES_VAN_CODICE_DEST        PIC  X(8).   
           05 HUSCTES_ELABORAZIONE_DATA      PIC  X(10).   
           05 HUSCTES_ELABORAZIONE_PERC      PIC  X(7).   
           05 HUSCTES_ELABORAZIONE_PERC_MIN  PIC  x(7).   
           05 HUSCTES_ELABORAZIONE_PESO      PIC  x(13).   
           05 HUSCTES_ELABORAZIONE_PESO_MIN  PIC  x(13).   
           05 HUSCTES_ELABORAZIONE_RISULTATO PIC  X(120).
           05 HUSCTES_PREPARAZIONE_DATA      PIC  X(10).   
           05 HUSCTES_PREPARAZIONE_RISULTATO PIC  X(120).  
           05 HUSCTES_ELABORAZIONE_AUTO      PIC  x(1).   
           05 HUSCTES_PREPARAZIONE_AUTO      PIC  x(1).   
           05 HUSCTES_COR_CODICE             PIC  x(15).   
           05 HUSCTES_COR_CODICE_TP          PIC  X(15).   
           05 HUSCTES_DES_CODICE             PIC  X(15).   
           05 HUSCTES_DES_DESCRIZIONE        PIC  X(60).   
           05 HUSCTES_DES_INDIRIZZO          PIC  x(60).   
           05 HUSCTES_DES_CAP                PIC  X(10).   
           05 HUSCTES_DES_LOCALITA           PIC  X(60).   
           05 HUSCTES_DES_PROVINCIA          PIC  x(10).   
           05 HUSCTES_DATA_SCHEDULAZIONE     PIC  x(10).   
           05 HUSCTES_DATA_LIMITE            PIC  x(10).   
           05 HUSCTES_AGGREGAZIONE_TIPO      PIC  X(1).   
           05 HUSCTES_MERCE_VIAGGIANTE       PIC  X(1).   
           05 HUSCTES_TX                     PIC  X(1).   
           05 HUSCTES_LAVORAZ_SEQ            PIC  x(20).   
           05 HUSCTES_CAU_CODICE_KILL        PIC  x(10).   
           05 HUSCTES_SPE_ID                 PIC  x(10).   
           05 HUSCTES_PESO_TOT               PIC  X(16).   
           05 HUSCTES_TIPO_SPED              PIC  X(4).   
           05 HUSCTES_NCOL                   PIC  X(4).   

       01  imp-rordini. |2444 crt totali (circa)
           05 HUSCRIG_COM_CODICE             PIC  x(15). 
           05 HUSCRIG_ORDINE                 PIC  x(15). 
           05 HUSCRIG_RIGA                   PIC  9(5). 
           05 HUSCRIG_ART_COM_CODICE         PIC  X(15). 
           05 HUSCRIG_ART_CODICE             PIC  X(25). 
           05 HUSCRIG_ART_ESTENSIONE         PIC  X(10). 
           05 HUSCRIG_COMMESSA               PIC  x(10). 
           05 HUSCRIG_STATOMERCE             PIC  x(10). 
           05 HUSCRIG_LOTTO                  PIC  9(7)v9(3).
           05 HUSCRIG_DSCADENZA              PIC  x(10). 
           05 HUSCRIG_QTA_RICHIESTA          PIC  9(11)v9(3).
           05 HUSCRIG_QTA_MODIFICATA         PIC  X(14). 
           05 HUSCRIG_QTA_EVASA              PIC  9(11)v9(3).
           05 HUSCRIG_UDF1                   PIC  x(25). 
           05 HUSCRIG_UDF2                   PIC  x(25). 
           05 HUSCRIG_UDF3                   PIC  x(25). 
           05 HUSCRIG_UDF4                   PIC  x(25). 
           05 HUSCRIG_UDF5                   PIC  X(25). 
           05 HUSCRIG_UDF6                   PIC  X(25). 
           05 HUSCRIG_UDF7                   PIC  X(25). 
           05 HUSCRIG_UDF8                   PIC  x(25). 
           05 HUSCRIG_UDF9                   PIC  x(25). 
           05 HUSCRIG_UDF10                  PIC  x(25). 
           05 HUSCRIG_SEQ_ELAB               PIC  x(80). 
           05 HUSCRIG_UDC_COMMITTENTE        PIC  x(20). 
           05 HUSCRIG_ELABORAZIONE_RISULTATO PIC  x(500).
           05 HUSCRIG_PESO_EVASO             PIC  9(14). 
           05 HUSCRIG_RIGA_HOST              PIC  X(10). 
           05 HUSCRIG_USCTES_ID              PIC  X(10). 

      * FILE STATUS
       77  status-tordini          pic xx.
       77  status-rordini          pic xx.
       77  status-paramshi         pic xx.
       77  status-tmp-imp-ror      pic xx.
       77  status-mtordini         pic xx.
       77  status-mrordini         pic xx.
       77  status-articoli         pic xx.
       77  status-progmag          pic xx.
       77  status-tpromo           pic xx.
       77  status-rpromo           pic xx.
       77  status-timposte         pic xx.
       77  status-tmarche          pic xx.
       77  status-tagli            pic xx.
       77  status-tcaumag          pic xx.
       77  status-tscorte          pic xx.
       77  status-clienti          pic xx.
       77  status-tparamge         pic xx.
       77  status-param            pic xx.
       77  status-lineseq          pic xx.
       77  status-lineseq2         pic xx.
       77  status-er-flusso        pic xx.
       77  status-ttipocli         pic xx.

       copy "trova-parametro.def".
       77  user-codi               pic x(20).
       77  nome-file               pic x(50).

       77  path-tmp-imp-ror        pic x(256).

       77  tot-idx-m pic 999.
       01 tab-master.
           05 el-chiave-m
                      OCCURS 9999 TIMES
                      INDEXED  idx-m.
               10 el-anno-m        PIC  9(4).
               10 el-numero-m      PIC  9(8).

       01  Riga-file.
           05 filler                pic X(01)      value SPACES.
           05 filler                pic X(10)      value "File.....:".
           05 r-file                pic X(50)      value SPACES.

      * SWITCHES
       01  filler                   pic XX.
           88 Prima-volta           value "SI", false "NO".

       01  controlli                pic xx.
           88 tutto-ok              value "OK".
           88 errori                value "ER".

       01  FlagApertura             pic   9.  
           88 NonAncoraAperto       value 0.
           88 FileAperto            value 1.

       01  FlagTestata              pic   9.  
           88 TestataGiaFatta       value 1, false 0.

       01  errori-flusso                    pic   xx.
           88 no-ordine                     value "NO".
           88 ordine-bolla                  value "OB".
           88 no-riga-ordine                value "NR".
           88 ordine-locked                 value "OL".
           88 ordine-non-aperto             value "ON".
           88 rordine-locked                value "RL".
           88 rordine-non-aperto            value "RA".
           88 lineseq-non-aperto            value "LN".
           88 flusso-offset-non-valorizzato value "FO".
           88 lineseq-VUOTO                 value "LV".
           88 lineseq-INCOERENTE            value "LI".

      * OTHER DATA             
       77  como-data                pic 9(8)   value zero.
       77  como-ora                 pic 9(8)   value zero.
       77  como-num-bolla           pic x(60).
       77  como-num-bolla-8         pic x(8).

       01  filler                   pic 9.
           88 RecLocked             value 1 false 0.
       77  titolo                   pic x(256).

       01  EXTEND-STAT.
           03 PRI-ERR               pic XX.
           03 SEC-ERR               pic X(10).

       77  TEXT-MESSAGE             pic X.

       77  wstampa                 pic x(256).
       77  wstampa2                pic x(256).
       77  path-er-flusso          pic x(256).

       77  record-counter pic 9(5).
       77  rec-counter-ed pic z(5).

       78  barra          value "\".

       LINKAGE SECTION.
           copy "link-imp.def".
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING imp-linkage.

       DECLARATIVES.
       lineseq-ERR SECTION.
           use error  procedure on lineseq.
           set errori to true.

           if status-lineseq = "35"
              set lineseq-non-aperto to true
              perform SCRIVI-FILE-ERRORI-SEQ
              move -2 to imp-status
           end-if.

       lineseq2-ERR SECTION.
           use error  procedure on lineseq2.
           set errori to true.

           if status-lineseq2 = "35"
              set lineseq-non-aperto to true
              perform SCRIVI-FILE-ERRORI-SEQ
              move -2 to imp-status
           end-if.

       TORDINI-ERR SECTION.
           use error  procedure on tordini.
           set errori to true.

           evaluate status-tordini
           when "35" 
                set ordine-non-aperto to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -2 to imp-status
           when "99"
                set ordine-locked  to true
                set RecLocked      to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-status
           when "9D"
           when other
                move -1 to imp-status
                set recLocked to false
                set ordine-locked   to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-status
           end-evaluate.

       RORDINI-ERR SECTION.
           use error  procedure on rordini.
           set errori to true.

           evaluate status-rordini
           when "35" 
                set rordine-non-aperto to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -2 to imp-status
           when "99"
                set rordine-locked  to true
                set RecLocked      to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-status
           when "9D"
           when other
                move -1 to imp-status
                set recLocked to false
                set rordine-locked   to true
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-status
           end-evaluate.

       paramshi-ERR SECTION.
           use error  procedure on paramshi.
           set errori to true.

           evaluate status-paramshi
           when "9D"
           when other
                move -1 to imp-status
                set recLocked to false
                perform SCRIVI-FILE-ERRORI-SEQ
                move -1 to imp-status
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
           move  "GESLUX - Importazioni Ordini" to titolo
           accept versione-evasione from environment "VERSIONE_EVASIONE"

           set tutto-ok            to true.
           set NonAncoraAperto     to true

           initialize errori-flusso. 
           move zero      to imp-status.
           set RecLocked  to false.
           set Prima-volta     to true.
           
           accept como-ora from time.
           accept como-data from century-date.

           move imp-path-log to path-er-flusso

           accept  path-tmp-imp-ror from environment "PATH-ST"
           inspect path-tmp-imp-ror 
                                replacing trailing space by low-value
           string path-tmp-imp-ror delimited by low-value
                  "tmp-imp-ror_"   delimited by size
                  como-data        delimited by size
                  "_"              delimited by size
                  como-ora         delimited by size
                  into path-tmp-imp-ror

           perform OPEN-PARAMSHI.

      ***---
       OPEN-FILES.
           open input articoli progmag  tscorte 
                      timposte tmarche  tagli tcaumag
                      clienti  tparamge param ttipocli.
           open i-o tpromo rpromo.
           perform OPEN-MTORDINI.
           if tutto-ok
              perform OPEN-MRORDINI
           end-if.
           if tutto-ok
              perform OPEN-TORDINI
           end-if.
           if tutto-ok
              perform OPEN-RORDINI
           end-if.
           if tutto-ok
              perform OPEN-TMP-IMP-ROR
           end-if.
           if tutto-ok
              perform OPEN-INPUT-LINESEQ
           end-if.
           if tutto-ok
              perform OPEN-INPUT-LINESEQ2
           end-if.

      ***---
       OPEN-MTORDINI.
           set RecLocked to false.
           open i-o mtordini.
           
           if status-mtordini = "9D"
              call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
              display message box "Errore ",  EXTEND-STAT,
                                  " sul file MTORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-MRORDINI.
           set RecLocked to false.
           open i-o mrordini.
           
           if status-mrordini = "9D"
              call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
              display message box "Errore ",  EXTEND-STAT,
                                  " sul file MRORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-TORDINI.
           set RecLocked to false.
           open i-o tordini.
           
           if status-tordini = "9D"
              call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
              display message box "Errore ",  EXTEND-STAT,
                                  " sul file TORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-RORDINI.
           set RecLocked to false.
           open i-o rordini.
           
           if status-rordini = "9D"
              call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
              display message box "Errore ",  EXTEND-STAT,
                                  " sul file RORDINI.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.

      ***---
       OPEN-TMP-IMP-ROR.
           set RecLocked to false.
           open output tmp-imp-ror.
           
           if status-tmp-imp-ror = "9D"
              call "C$RERR" using EXTEND-STAT, TEXT-MESSAGE
              display message box "Errore ",  EXTEND-STAT,
                                  " sul file TEMPORANEO.", X"0A",
                              "Contattare l'amministratore del Sistema."
                         title titolo
              move -1 to imp-status
           end-if.

           if RecLocked
              set errori to true
              move -5 to imp-status
           end-if.
           close tmp-imp-ror.
           open i-o tmp-imp-ror.

      ***---
       OPEN-PARAMSHI.
           open input paramshi.

           move space  to shi-codice
           read paramshi no lock 
              invalid 
                 set errori to true 
           end-read.

           if shi-path-imp   = space or
              shi-file-tordini-imp = space or
              shi-file-rordini-imp = space
              set errori to true
              set flusso-offset-non-valorizzato to true
              perform SCRIVI-FILE-ERRORI-SEQ
              move -1  to imp-status
           end-if
           
           if tutto-ok
              perform PREPARA-PERCORSI
              if errori
                 move -3 to imp-status
                 perform SCRIVI-FILE-ERRORI-SEQ
                 perform CLOSE-ER-FLUSSO
              end-if
           end-if.

      ***---
       PREPARA-PERCORSI.
           inspect shi-path-imp  replacing trailing space by low-value.
           initialize wstampa.
           string shi-path-imp           delimited by low-value
                  barra                  delimited by size
                  shi-file-tordini-imp   delimited by size
                  into wstampa.
           initialize wstampa2.
           string shi-path-imp           delimited by low-value
                  barra                  delimited by size
                  shi-file-rordini-imp   delimited by size
                  into wstampa2.

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
                    move -3 to imp-status
                    perform SCRIVI-FILE-ERRORI-SEQ
                    perform CLOSE-ER-FLUSSO
                 not at end
                    perform MOVE-TO-STRUTTURA
              end-read
              close lineseq
              open input lineseq
           end-if.

      ***---
       OPEN-INPUT-LINESEQ2.
           set tutto-ok to true.
           open input lineseq2.

           if tutto-ok
              initialize line-riga2
              read lineseq2 next
                 at end
                    set errori       to true
                    set lineseq-vuoto to true
                    move -3 to imp-status
                    perform SCRIVI-FILE-ERRORI-SEQ
                    perform CLOSE-ER-FLUSSO
                 not at end
                    perform MOVE-TO-STRUTTURA-R
              end-read
              close lineseq2
              open input lineseq2
           end-if.

      ***---
       ELABORAZIONE.
           perform PREELABORA-RIGHE.

           move 0 to record-counter tot-idx-m.
           move shi-file-tordini-imp to nome-file.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga(1:1) = x"1A" 
                 exit perform
              end-if
              add 1 to record-counter
              set tutto-ok   to true
              perform MOVE-TO-STRUTTURA
              if errori
                 perform SCRIVI-FILE-ERRORI-SEQ    
              else
                 perform VALORIZZA-RECORD
              end-if
           end-perform.
           close lineseq.

           move imp-user to user-codi.
           perform varying idx-m from 1 by 1 
                     until idx-m > tot-idx-m
              move el-chiave-m(idx-m) to mto-chiave
              perform AGGIORNA-STATO-MASTER
           end-perform.
            
      ***---
       VALORIZZA-RECORD.
           move HUSCTES_ORDINE(1:4) to tor-anno   convert.
           move HUSCTES_ORDINE(5:)  to tor-numero convert.
           
           read tordini no lock 
                invalid
                set no-ordine to true
                perform SCRIVI-FILE-ERRORI-SEQ
                exit paragraph  
            not invalid
                if tor-data-bolla not = 0 or tor-num-bolla not = 0
                   set ordine-bolla to true
                   perform SCRIVI-FILE-ERRORI-SEQ
                   exit paragraph
                end-if
           end-read

           read tordini lock invalid continue end-read.

           if not RecLocked
      *     05 HUSCTES_ORDINE                 PIC  X(15).   
      *     05 HUSCTES_ORDUSCTIP_CODICE       PIC  X(15).   
      *     05 HUSCTES_ORD2                   PIC  x(15).   
      *     05 HUSCTES_COMUSCTIP_CODICE       PIC  x(15).   
      *     05 HUSCTES_COMCAU_CODICE          PIC  x(10).   
      *     05 HUSCTES_COMDES_CODICE          PIC  X(10).   
      *     05 HUSCTES_PRIORITA               PIC  9(2).   
      *     05 HUSCTES_USCTES_ID              PIC  9(10).   
      *     05 HUSCTES_STATO                  PIC  x(1).   
      *     05 HUSCTES_BAT_ID                 PIC  x(10).   
      *     05 HUSCTES_DATA                   PIC  x(10).   
      *     05 HUSCTES_DATA_AVANZAMENTO       PIC  X(10).   
      *     05 HUSCTES_CNTGRUTES_CODICE       PIC  X(10).   
      *     05 HUSCTES_UDF1                   PIC  X(60).   
      *     05 HUSCTES_UDF2                   PIC  x(60).   
      *     05 HUSCTES_UDF3                   PIC  x(60).   
      *     05 HUSCTES_UDF4                   PIC  x(60).   
      *     05 HUSCTES_UDF5                   PIC  X(60).   
      *     05 HUSCTES_UDF6                   PIC  X(60).   
      *     05 HUSCTES_UDF7                   PIC  X(60).   
      *     05 HUSCTES_UDF8                   PIC  x(60).   
      *     05 HUSCTES_UDF9                   PIC  x(60).   
      *     05 HUSCTES_UDF10                  PIC  x(60).   
      *     05 HUSCTES_VAN_CODICE_DEST        PIC  X(8).   
      *     05 HUSCTES_ELABORAZIONE_DATA      PIC  X(10).   
      *     05 HUSCTES_ELABORAZIONE_PERC      PIC  X(7).   
      *     05 HUSCTES_ELABORAZIONE_PERC_MIN  PIC  x(7).   
      *     05 HUSCTES_ELABORAZIONE_PESO      PIC  x(13).   
      *     05 HUSCTES_ELABORAZIONE_PESO_MIN  PIC  x(13).   
      *     05 HUSCTES_ELABORAZIONE_RISULTATO PIC  X(120).
      *     05 HUSCTES_PREPARAZIONE_DATA      PIC  X(10).   
      *     05 HUSCTES_PREPARAZIONE_RISULTATO PIC  X(120).  
      *     05 HUSCTES_ELABORAZIONE_AUTO      PIC  x(1).   
      *     05 HUSCTES_PREPARAZIONE_AUTO      PIC  x(1).   
      *     05 HUSCTES_COR_CODICE             PIC  x(15).   
      *     05 HUSCTES_COR_CODICE_TP          PIC  X(15).   
      *     05 HUSCTES_DES_CODICE             PIC  X(15).   
      *     05 HUSCTES_DES_DESCRIZIONE        PIC  X(60).   
      *     05 HUSCTES_DES_INDIRIZZO          PIC  x(60).   
      *     05 HUSCTES_DES_CAP                PIC  X(10).   
      *     05 HUSCTES_DES_LOCALITA           PIC  X(60).   
      *     05 HUSCTES_DES_PROVINCIA          PIC  x(10).   
      *     05 HUSCTES_DATA_SCHEDULAZIONE     PIC  x(10).   
      *     05 HUSCTES_DATA_LIMITE            PIC  x(10).   
      *     05 HUSCTES_AGGREGAZIONE_TIPO      PIC  X(1).   
      *     05 HUSCTES_MERCE_VIAGGIANTE       PIC  X(1).   
      *     05 HUSCTES_TX                     PIC  X(1).   
      *     05 HUSCTES_LAVORAZ_SEQ            PIC  x(20).   
      *     05 HUSCTES_CAU_CODICE_KILL        PIC  x(10).   
      *     05 HUSCTES_SPE_ID                 PIC  x(10).   
      *     05 HUSCTES_PESO_TOT               PIC  X(16).   
      *     05 HUSCTES_TIPO_SPED              PIC  X(4).   
      *     05 HUSCTES_NCOL                   PIC  X(4).   
      *
              move como-data              to tor-data-ultima-modifica
              move como-ora               to tor-ora-ultima-modifica
              move imp-user               to tor-utente-ultima-modifica
              move HUSCTES_UDF5           to como-num-bolla
              call "C$JUSTIFY" using como-num-bolla, "L"
              inspect como-num-bolla replacing leading x"30" by x"20"
              call "C$JUSTIFY" using como-num-bolla, "L"
              move como-num-bolla         to como-num-bolla-8
              call "C$JUSTIFY" using como-num-bolla-8, "R"
              inspect como-num-bolla-8 replacing leading x"20" by x"30"
              move como-num-bolla-8       to tor-num-bolla
              move HUSCTES_UDF6(1:8)      to tor-data-bolla
              move HUSCTES_UDF6(1:4)      to tor-anno-bolla
              set  tor-bolla-si-prenotata to true
              if HUSCTES_COR_CODICE not = spaces
                 move HUSCTES_COR_CODICE to tor-vettore
              end-if

              perform TRATTA-RIGHE
      *
              rewrite tor-rec 
                      invalid continue 
              end-rewrite
              unlock tordini all record
           else
              set RecLocked  to false
           end-if.

      ***---
       TRATTA-RIGHE.
           move tor-anno   to tmp-iror-anno.
           move tor-numero to tmp-iror-num-ordine.
           move low-value  to tmp-iror-num-riga.
           perform STORNO-QUANTITA.

           start tmp-imp-ror key >= tmp-iror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read TMP-IMP-ROR next no lock
                       at end
                          exit perform
                    end-read
                    if tor-anno   not = tmp-iror-anno or 
                       tor-numero not = tmp-iror-num-ordine
                       exit perform
                    end-if
                    perform TRATTA-RIGA
                 end-perform
           end-start.
           perform RIPRISTINO-QUANTITA.

      ***---
       STORNO-QUANTITA.
           move low-value to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read rordini next  at end exit perform end-read
              if ror-anno       not = tor-anno or
                 ror-num-ordine not = tor-numero
                 exit perform 
              end-if
              |DIMINUISCO L'IMPEGNATO
              set  link-update    to true
              set  link-update-um to true
              move 0              to link-impegnato
              move ror-qta        to link-valore
              move tor-causale    to link-causale
              move ror-prg-chiave to link-key
              move "0000000000000000"   to link-array
              |Devo fare il contrario di quanto dice la
              |causale perciò per aumentare metto il -1
              move -1                to multiplyer(2)
              move -1                to multiplyer(15)
              move imp-user to link-user of link-wprogmag
              call   "wprogmag" using link-wprogmag
              cancel "wprogmag"
           end-perform.

      ***---
       TRATTA-RIGA.
           move tmp-iror-chiave to ror-chiave
           read RORDINI no lock
                invalid continue
           end-read.

           move tmp-iror-ART-CODICE to prg-cod-articolo art-codice.
           move tmp-iror-MAG-CODICE to prg-cod-magazzino.
           move tmp-iror-IMBALLO    to prg-tipo-imballo.
           move tmp-iror-PESO       to prg-peso.

           read progmag no lock
                invalid
                close    progmag
                open i-o progmag
                read articoli no lock
                initialize prg-dati replacing numeric data by zeroes
                                         alphanumeric data by spaces
                move tor-data-creazione   to prg-data-creazione
                move tor-ora-creazione    to prg-ora-creazione
                move tor-utente-creazione to prg-utente-creazione
                set  prg-attivo      to true
                if art-si-utf
                   move prg-peso to prg-peso-utf
                else
                   move prg-peso to prg-peso-non-utf
                end-if
                write prg-rec
                close      progmag
                open input progmag
           end-read.
           move prg-chiave to ror-prg-chiave.

           move tmp-iror-QTA-EVASA to ror-qta.
           move tor-dati-comuni    to ror-dati-comuni.
           set  ror-evasa-SHI-si   to true.
           rewrite ror-rec invalid continue end-rewrite.
           if ror-anno-master   not = 0 and
              ror-numero-master not = 0
              set idx-m to 1
              search el-chiave-m
              at end
                 add 1 to tot-idx-m
                 move ror-chiave-ordine to el-chiave-m(tot-idx-m)
              when el-chiave-m(idx-m) = ror-chiave-ordine
                  continue |MI serve solo aggiungere
              end-search
           end-if.

      ***---
       RIPRISTINO-QUANTITA.
           move low-value to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read rordini next  at end exit perform end-read
              if ror-anno       not = tor-anno or
                 ror-num-ordine not = tor-numero
                 exit perform 
              end-if
              if ror-evasa-SHI-si
                |DIMINUISCO LA GIACENZA
                 set  link-update    to true
                 set  link-update-um to true
                 move 0              to link-impegnato
                 move ror-qta        to link-valore
                 move tor-causale    to link-causale
                 move ror-prg-chiave to link-key
                 move "0000000000000000"   to link-array
                 move 1                 to multiplyer(1)
                 move 1                 to multiplyer(15)
                 move imp-user to link-user of link-wprogmag
                 call   "wprogmag" using link-wprogmag
                 cancel "wprogmag"
              else
                 delete rordini record
              end-if
           end-perform.

      ***---
       MOVE-TO-STRUTTURA.
           initialize imp-tordini replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move line-riga to imp-tordini.

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
              open extend er-flusso          
           end-if.
      
           initialize er-f-rec.             
      
           if r-file not = nome-file
              set prima-volta to false
              move  nome-file to r-file
              write er-f-rec from Riga-file after 2
              write er-f-rec from SPACES    after 1
           end-if.

           initialize ER-F-REC
           move record-counter   to rec-counter-ed
           evaluate true
           when no-ordine
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": ordine inesistente!" delimited by size
                       into ER-F-REC
           when ordine-bolla
                string "record "                  delimited by size
                       rec-counter-ed             delimited by size
                       ": ordine già bollettato!" delimited by size
                       into ER-F-REC
           when no-riga-ordine
                string "record "                     delimited by size
                       rec-counter-ed                delimited by size
                       ": riga ordine inesistente!"  delimited by size
                       into ER-F-REC
           when ordine-locked
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Ordine Bloccato da altro utente!" 
                                               delimited by size
                       "Non aggiornati i dati sull'ordine"
                                               delimited by size
                       into ER-F-REC

           when rordine-locked
                string "record "               delimited by size
                       rec-counter-ed          delimited by size
                       ": Riga Ordine Bloccata da altro utente!" 
                                               delimited by size
                       "Non aggiornati i dati sull'ordine"
                                               delimited by size
                       into ER-F-REC

           when lineseq-non-aperto
                move "Impossibile aprire il flusso di import!"   
                                                        to ER-F-REC
           when ordine-non-aperto
                move "Impossibile aprire il file TORDINI!"  to ER-F-REC
           when rordine-non-aperto
                move "Impossibile aprire il file RORDINI!"  to ER-F-REC
           when flusso-offset-non-valorizzato
                move "Dati telematici degli ordini non valorizzati"
                    to ER-F-REC
           when lineseq-VUOTO
                move "Flusso di import vuoto!" to ER-F-REC
           when lineseq-INCOERENTE
                move "Flusso di import non valido!" to ER-F-REC
           end-evaluate.

           write ER-F-REC.
           move -1 to imp-status.

      ***---
       CLOSE-FILES.
           perform CLOSE-ER-FLUSSO.
           close tordini
                 rordini
                 paramshi
                 tmp-imp-ror.
           delete file tmp-imp-ror.
           close mtordini mrordini articoli progmag tpromo rpromo
                 timposte tmarche  tagli    tcaumag tscorte
                 clienti  tparamge param ttipocli.

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
       EXIT-PGM.
           goback.

      ***---
       PREELABORA-RIGHE.
           move shi-file-rordini-imp to nome-file.
           move zero   to record-counter
           perform until 1 = 2
              initialize line-riga2
              read lineseq2 next at end exit perform end-read
              if line-riga2(1:1) = x"1A" 
                 exit perform
              end-if
              add 1 to record-counter
              set tutto-ok   to true
              perform MOVE-TO-STRUTTURA-R
              if errori
                 perform SCRIVI-FILE-ERRORI-SEQ    
              else
                 perform VALORIZZA-RECORD-R
              end-if
           end-perform.
           close lineseq2.

      ***---
       MOVE-TO-STRUTTURA-R.
           initialize imp-rordini replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move line-riga2 to imp-rordini.


      ***---
       VALORIZZA-RECORD-R.
           move HUSCRIG_ORDINE(1:4)   to tor-anno convert
           move HUSCRIG_ORDINE(5:)    to tor-numero convert
           read tordini no lock 
                invalid
                set no-ordine to true
                perform SCRIVI-FILE-ERRORI-SEQ
                exit paragraph
            not invalid
                if tor-data-bolla not = 0 or tor-num-bolla not = 0
                   set ordine-bolla to true
                   perform SCRIVI-FILE-ERRORI-SEQ
                   exit paragraph
                end-if
           end-read

           move tor-anno  to ror-anno
           move tor-numero   to ror-num-ordine
           move HUSCRIG_RIGA to ror-num-riga

           read rordini no lock 
              invalid
                 set no-riga-ordine to true
                 perform SCRIVI-FILE-ERRORI-SEQ
                 exit paragraph
           end-read.

           move ror-chiave   to tmp-iror-chiave.

           move HUSCRIG_ART_CODICE(1:6)  to tmp-iror-ART-CODICE.
           move "SHI"                    to tmp-iror-MAG-CODICE.
           move HUSCRIG_ART_CODICE(8:3)  to tmp-iror-IMBALLO.

           move HUSCRIG_LOTTO            to tmp-iror-PESO.

           move HUSCRIG_QTA_RICHIESTA to tmp-iror-QTA-RICHIESTA.
           move HUSCRIG_QTA_EVASA     to tmp-iror-QTA-EVASA.
           move HUSCRIG_UDF1          to tmp-iror-UDF1.
           move HUSCRIG_UDF2          to tmp-iror-UDF2.
           move HUSCRIG_UDF3          to tmp-iror-UDF3.
           move HUSCRIG_UDF4          to tmp-iror-UDF4.
           move HUSCRIG_UDF5          to tmp-iror-UDF5.
           move HUSCRIG_UDF6          to tmp-iror-UDF6.
           move HUSCRIG_UDF7          to tmp-iror-UDF7.
           move HUSCRIG_UDF8          to tmp-iror-UDF8.
           move HUSCRIG_UDF9          to tmp-iror-UDF9.
           move HUSCRIG_UDF10         to tmp-iror-UDF10.
           move HUSCRIG_SEQ_ELAB      to tmp-iror-SEQ-ELAB.
           move HUSCRIG_UDC_COMMITTENTE  
                                      to tmp-iror-UDC-COMMITTENTE.
           move HUSCRIG_ELABORAZIONE_RISULTATO 
                                      to tmp-iror-ELABORAZIONE-RISULTATO
           move HUSCRIG_PESO_EVASO    to tmp-iror-PESO-EVASO.
           move HUSCRIG_RIGA_HOST     to tmp-iror-RIGA-HOST.
           move HUSCRIG_USCTES_ID     to tmp-iror-USCTES-ID.

           if tmp-iror-QTA-EVASA not = 0
              write tmp-iror-rec
                    invalid continue
              end-write
           end-if.

      ***---
       PARAGRAFO-COPY.
           copy "aggiorna-stato-master.cpy".
           copy "trova-parametro.cpy".

      ***--- 
       DIREZIONA-IMPEGNATO.
           |DUMMY: non intervengo sull'impegnato
