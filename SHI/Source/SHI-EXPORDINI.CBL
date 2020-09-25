       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-expordini.
       AUTHOR.                          Luciano.
       REMARKS. EXPORT delle bozze ordini
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "paramshi.sl".
           copy "tordini.sl".
           copy "mtordini.sl".
           copy "rordini.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "destini.sl".
           copy "tcaumag.sl".
           copy "tnazioni.sl".
           copy "articoli.sl".
           copy "art-exp-shi.sl".
           copy "tivaese.sl".
           copy "timbalqta.sl".
           copy "prodener.sl".
           copy "progmag.sl".
           copy "tmp-tordini-exp.sl".

           copy "fileseq.sl".

           copy "lineseq.sl". |articoli
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
       SELECT lineseq4
           ASSIGN       TO  wstampa4
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq4.
       SELECT lineseq5
           ASSIGN       TO  wstampa5
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq5.
       SELECT lineseq6
           ASSIGN       TO  wstampa6
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq6.
       SELECT riepilogo-c
           ASSIGN       TO  path-riepilogo-c
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-riepilogo-c.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "paramshi.fd".
           copy "tordini.fd".
           copy "mtordini.fd".
           copy "rordini.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "destini.fd".
           copy "tcaumag.fd".
           copy "tnazioni.fd".
           copy "articoli.fd".
           copy "art-exp-shi.fd".
           copy "tivaese.fd".
           copy "timbalqta.fd".
           copy "prodener.fd".
           copy "progmag.fd".
           copy "tmp-tordini-exp.fd".

           copy "fileseq.fd".
       FD  lineseq.
       01 line-riga        PIC  x(10000).
       FD  lineseq2.
       01  line-riga2      PIC  x(1000).
       FD  lineseq3.
       01  line-riga3      PIC  x(1000).
       FD  lineseq4.
       01  line-riga4      PIC  x(1000).
       FD  lineseq5.
       01  line-riga5      PIC  x(1000).  
       FD  lineseq6.
       01  line-riga6      PIC  x(1000).
       FD  riepilogo-c.
       01  line-riepilogo-c PIC  x(1000).

       
       WORKING-STORAGE SECTION.
       copy "exp-ws.def".
       copy "exparticoli.def".

       77  como-mag             pic x(3).
       78  titolo               value "Export Vettori".

       77  status-paramshi      pic xx.
       77  status-tordini       pic xx.
       77  status-mtordini      pic xx.
       77  status-rordini       pic xx.
       77  status-clienti       pic xx.
       77  status-destini       pic xx.
       77  status-ttipocli      pic xx.
       77  status-tcaumag       pic xx.
       77  status-tnazioni      pic xx.
       77  status-articoli      pic xx.
       77  status-art-exp-shi   pic xx.
       77  status-tivaese       pic xx.
       77  status-timbalqta     pic xx.
       77  status-prodener      pic xx.
       77  status-progmag       pic xx.
       77  status-riepilogo-c   pic xx.
       77  status-tmp-tordini-exp  pic xx.  

       77  status-lineseq       pic xx.
       77  status-lineseq2      pic xx.
       77  status-lineseq3      pic xx.
       77  status-lineseq4      pic xx.
       77  status-lineseq5      pic xx.
       77  status-lineseq6      pic xx.
       77  wstampa              pic x(256).
       77  wstampa2             pic x(256).
       77  wstampa3             pic x(256).
       77  wstampa4             pic x(256).
       77  wstampa5             pic x(256).
       77  wstampa6             pic x(256).
       77  path-tmp-tordini-exp pic x(256).
       77  path-riepilogo-c     pic x(256).

       77  primo-numero         pic 9(8) value 0.
       77  ultimo-numero        pic 9(8) value 0.
       01  bozze-elab.
         05 el-bozza-elab       pic 9(8) occurs 20000.
       77  idx-elab             pic 9(5) value 0.

       78  78-max-bozze-in-line value 6.
       01  bozze-in-line.                
         05 filler              pic x(3) value " | ".
         03 filler              occurs 78-max-bozze-in-line.
         05 bozza-in-line       pic 9(8) blank zero.
         05 filler              pic x(3) value " | ".
       77  cnt-bozze            pic 99 value 0.
       78  78-line-div          value " +-------------------------------
      -                           "----------------------------------+".
       78  78-tit-bozze         value " |           Trasmissione Sinteco
      -                           " - Numero bozze inviate           |".
       78  78-tit-colli         value 
                              " |                           Numero colli
      -                         "                          |".

       01  tab-colli            occurs 1000 indexed by idx-colli.
         05 el-collo-tipocli    pic xx.
         05 el-collo-num        pic 9(9).
       77  tot-idx-colli        pic 9(5) value 0.
       01  riga-colli.
         05 filler              pic x(3) value " | ".
         05 tipocli-c           pic xx.
         05 filler              pic x(3) value " | ".
         05 tipocli-d           pic x(44).
         05 filler              pic x(3) value " | ".
         05 num-colli           pic zzz.zzz.zz9.      
         05 filler              pic x(2) value " |".

       01                 pic 9.
           88 prima-volta value 1 false zero. 

       77  tor-numero-ed     pic z(8).

       77  num-rec-righe     pic 9(10).
       77  num-rec-note      pic 9(10).
       77  num-rec-art       pic 9(10).

       77  como-utente       pic 9(10).

       77  como-data         pic 9(8).
       77  como-ora          pic 9(8).

       01  exp-tordini.
1        05 HUSCTES_COM_CODICE         PIC  X(15).
2        05 HUSCTES_ORDINE             PIC  X(15).
3        05 HUSCTES_ORDUSCTIP_CODICE   PIC  X(15).
4        05 HUSCTES_COMCAU_CODICE      PIC  x(15).
5        05 HUSCTES_COMDES_CODICE      PIC  x(15).
6        05 HUSCTES_PRIORITA           PIC  x(2). 
7        05 HUSCTES_DATA               PIC  X(10).
8        05 HUSCTES_DATA_PASS          PIC  X(10).
9        05 HUSCTES_UDF1               PIC  X(15).
10       05 HUSCTES_UDF2               PIC  X(25).
11       05 HUSCTES_UDF3               PIC  x(25).
12       05 HUSCTES_UDF4               PIC  x(25).
13       05 HUSCTES_UDF5               PIC  x(25).
14       05 HUSCTES_UDF6               PIC  X(25).
15       05 HUSCTES_UDF7               PIC  X(25).
16       05 HUSCTES_UDF8               PIC  X(25).
17       05 HUSCTES_UDF9               PIC  x(25).
18       05 HUSCTES_UDF10              PIC  x(25).
19       05 HUSCTES_COR_CODICE         PIC  x(15).
20       05 HUSCTES_COR_CODICE_TP      PIC  X(15).
21       05 HTES_DATA_SCHED            PIC  X(10).
22       05 HUSCTES_DATA_LIMITE        PIC  X(10).
23       05 HUSCTES_MERCE_VIAGGIANTE   PIC  x(1).
24       05 HUSCTES_COMDES_DESCRIZIONE PIC  x(60).
25       05 HUSCTES_COMDES_INDIRIZZO   PIC  x(60).
26       05 HUSCTES_COMDES_CAP         PIC  X(10).
27       05 HUSCTES_COMDES_LOCALITA    PIC  X(60).
28       05 HUSCTES_COMDES_PROVINCIA   PIC  X(10).
29       05 HUSCTES_COMDES_NAZIONE     PIC  x(10).
30       05 HUSCTES_DES_DESCRIZIONE    PIC  x(60).
31       05 HUSCTES_DES_INDIRIZZO      PIC  x(60).
32       05 HUSCTES_DES_CAP            PIC  X(10).
33       05 HUSCTES_DES_LOCALITA       PIC  X(60).
34       05 HUSCTES_DES_PROVINCIA      PIC  X(10).
35       05 HUSCTES_DES_NAZIONE        PIC  x(10).
36       05 HUSCTES_DES_CODICE         PIC  x(15).
37       05 HUSCTES_TIPO_SPED          PIC  x(4).
38       05 HUSCTES_RIF_ORDINE         PIC  X(35).
39       05 HUSCTES_RIF_DORDINE        PIC  X(10).
40       05 HUSCTES_UDF11              PIC  X(60).
41       05 HUSCTES_UDF12              PIC  x(60).

       01  exp-rordini.
1        05 HUSCRIG_COM_CODICE         PIC  x(15).
2        05 HUSCRIG_ORDINE             PIC  x(15).
3        05 HUSCRIG_RIGA               PIC  9(5).
4        05 HUSCRIG_ART_COM_CODICE     PIC  X(15).
5        05 HUSCRIG_ART_CODICE         PIC  X(25).
6        05 HUSCRIG_ART_ESTENSIONE     PIC  X(10).
7        05 HUSCRIG_COMMESSA           PIC  x(10).
8        05 HUSCRIG_STATOMERCE         PIC  x(10).
9        05 HUSCRIG_LOTTO              PIC  9(7)v9(3).
10       05 HUSCRIG_DSCADENZA          PIC  x(10).
11       05 HUSCRIG_QTA_RICHIESTA      PIC  9(11)v9(3).
12       05 HUSCRIG_UDF1               PIC  x(25).
13       05 HUSCRIG_UDF2               PIC  X(25).
14       05 HUSCRIG_UDF3               PIC  x(25).
15       05 HUSCRIG_UDF4               PIC  x(25).
16       05 HUSCRIG_UDF5               PIC  x(25).
17       05 HUSCRIG_UDF6               PIC  x(25).
18       05 HUSCRIG_PESO               PIC  9(5)v999.
19       05 HUSCRIG_UDF8               PIC  X(25).
20       05 HUSCRIG_UDF9               PIC  X(25).
21       05 HUSCRIG_UDF10              PIC  x(25).
22       05 HUSCRIG_SEQ_ELAB           PIC  x(80).
23       05 HUSCRIG_UDC_COMMITTENTE    PIC  x(20).
24       05 HUSCRIG_RIGA_HOST          PIC  x(10).

       01  exp-note.
1        05 HUSCNOT_COM_CODICE         PIC  x(15).
2        05 HUSCNOT_ORDINE             PIC  x(15).
3        05 HUSCNOT_TIPO               PIC  x(1).
4        05 HUSCNOT_RIGA               PIC  9(5).
5        05 HUSCNOT_CODICE             PIC  9(3).
6        05 HUSCNOT_DESCRIZIONE        PIC  X(200).
7        05 HUSCNOT_USO                PIC  x(10).

       LINKAGE SECTION.
           copy "link-exp.def".
           copy "link-expordini.def".

      ******************************************************************
       PROCEDURE DIVISION using exp-linkage, expordini-linkage.

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
                move "File già in uso! Impossibile procedere! Operazione 
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
                move "File già in uso! Impossibile procedere! Operazione 
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
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

       LINESEQ4-ERR SECTION.
           use after error procedure on lineseq4.
           set tutto-ok  to true.
           evaluate status-lineseq4
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
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

       LINESEQ5-ERR SECTION.
           use after error procedure on lineseq5.
           set tutto-ok  to true.
           evaluate status-lineseq5
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
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

       LINESEQ6-ERR SECTION.
           use after error procedure on lineseq6.
           set tutto-ok  to true.
           evaluate status-lineseq6
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
                move "File già in uso! Impossibile procedere! Operazione 
      -              " interrotta!" 
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
            "Impossibile procedere. File vettori [PARAMSHI] inesistente"
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
       tordini-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                move 
             "Impossibile procedere. File vettori [tordini] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [tordini] Mismatch size!"   to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[tordini] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                move 
            "Impossibile procedere. File vettori [mtordini] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [mtordini] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[mtordini] Indexed file corrupt!"
                                                     to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       rordini-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                move 
            "Impossibile procedere. File vettori [rordini] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [rordini] Mismatch size!"   to como-messaggio
                perform SCRIVI-ERORRE
                set errore-bloccante  to true
           when "98"
                move "[rordini] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                move 
            "Impossibile procedere. File vettori [CLIENTI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [CLIENTI] Mismatch size!"   to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[CLIENTI] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                move 
            "Impossibile procedere. File vettori [DESTINI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [DESTINI] Mismatch size!"   to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[DESTINI] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                move 
            "Impossibile procedere. File vettori [TCAUMAG] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TCAUMAG] Mismatch size!"   to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TCAUMAG] Indexed file corrupt!" to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TNAZIONI-ERR SECTION.
           use after error procedure on tnazioni.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                move 
            "Impossibile procedere. File vettori [TNAZIONI] inesistente"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TNAZIONI] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TNAIZONI] Indexed file corrupt!"
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
       ART-EXP-SHI-ERR SECTION.
           use after error procedure on art-exp-shi.
           set tutto-ok  to true.
           evaluate status-art-exp-shi
           when "35"
                move                            
                "Impossibile procedere. File [ART-EXP-SHI] inesistente"
                          to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [ART-EXP-SHI] Mismatch size!"  
                                                  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[ART-EXP-SHI] Indexed file corrupt!" 
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
       PRODENER-ERR SECTION.
           use after error procedure on PRODENER.
           set tutto-ok  to true.
           evaluate status-prodener
           when "35"
                move 
           "Impossibile procedere. File [PRODENER] inesistente"
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

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on PROGMAG.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                move 
           "Impossibile procedere. File [PROGMAG] inesistente"
                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [PROGMAG] Mismatch size!"  to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[PROGMAG] Indexed file corrupt!"
                                                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           end-evaluate.

      ***---
       TMP-TODINI-EXP-ERR SECTION.
           use after error procedure on TMP-TORDINI-EXP.
           set tutto-ok  to true.
           evaluate status-TMP-TORDINI-EXP
           when "35"
                move 
           "Impossibile procedere. File [TMP-TORDINI-EXP] inesistente"
                       to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "39"
                move "File [TMP-TORDINI-EXP] Mismatch size!"  
                                         to como-messaggio
                set errore-bloccante  to true
                perform SCRIVI-ERORRE
           when "98"
                move "[TMP-TORDINI-EXP] Indexed file corrupt!"
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
           accept path-tmp-tordini-exp from environment "PATH_ST".
           inspect path-tmp-tordini-exp 
                                replacing trailing space by low-value.
           string path-tmp-tordini-exp delimited by low-value
                  "tmp-tordini-exp_"  delimited by size
                  como-data           delimited by size
                  "_"                 delimited by size
                  como-ora            delimited by size
                  into path-tmp-tordini-exp.
           inspect path-tmp-tordini-exp 
                                replacing trailing low-value by space.
           move path-tmp-tordini-exp to exp-ord-path.

           accept como-utente from environment "USER_IMPORT_ARTICOLI".
           perform INITIALIZE-FLAG.
           move zero   to num-rec-righe
                          num-rec-note
                          num-rec-prodener
                          num-rec-ean
                          num-rec-art.

           set exp-ordini       to true.
           set prima-volta      to true
           set tutto-ok         to true.

           open input paramshi.
           move spaces to shi-codice.
           read  paramshi invalid continue end-read.
           close paramshi.
           inspect shi-path-exp replacing trailing space by low-value.

           string shi-path-exp        delimited by low-value
                  "\"                 delimited by size
                  shi-file-articoli   delimited by size
                  into wstampa.

           string shi-path-exp        delimited by low-value
                  "\"                 delimited by size
                  shi-file-ean        delimited by size
                  into wstampa2.

           string shi-path-exp        delimited by low-value
                  "\"                 delimited by size
                  shi-file-prodener   delimited by size
                  into wstampa3.
           initialize tabella-prodener.

           inspect shi-file-tordini 
                   replacing trailing spaces by low-value.
           string shi-path-exp     delimited low-value
                  "\"              delimited size
                  shi-file-tordini delimited low-value
                  "_"              delimited size   
                  como-data        delimited size
                  "_"              delimited size   
                  como-ora         delimited size
                  ".txt"           delimited size 
                  into wstampa4.
           
           inspect shi-file-rordini 
                   replacing trailing spaces by low-value.
           string shi-path-exp     delimited low-value
                  "\"              delimited size
                  shi-file-rordini delimited low-value
                  "_"              delimited size   
                  como-data        delimited size
                  "_"              delimited size   
                  como-ora         delimited size     
                  ".txt"           delimited size 
                  into wstampa5.
                             
           inspect shi-file-note-ordini 
                   replacing trailing spaces by low-value.
           string shi-path-exp         delimited low-value
                  "\"                  delimited size
                  shi-file-note-ordini delimited low-value
                  "_"                  delimited size   
                  como-data            delimited size
                  "_"                  delimited size   
                  como-ora             delimited size 
                  ".txt"               delimited size 
                  into wstampa6.

      ***---
       OPEN-FILES.
           open input tordini.
           open i-o   art-exp-shi.
           open input mtordini.
           open input rordini.
           open input clienti.
           open input destini.
           open input tcaumag.
           open input tnazioni.
           open input articoli.
           open input tivaese.
           open input timbalqta.
           open input prodener.
           open input progmag.
           open input ttipocli.

           open output tmp-tordini-exp
           close       tmp-tordini-exp
           open i-o    tmp-tordini-exp.

      ***---
       ELABORAZIONE.
           initialize bozze-elab replacing numeric data by zeroes.
           
           move low-value to tor-anno-bolla tor-num-bolla.
 
           start tordini key >= k-bolla
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tordini next no lock
                   at end exit perform 
                 end-read        
                 if tor-anno-bolla not = 0 or tor-num-bolla not = 0
                    exit perform
                 end-if
                 if primo-numero = 0
                    move tor-numero to primo-numero
                 end-if
                 move tor-numero to ultimo-numero
                                       
                 perform VALIDA-ORDINE
                 if tutto-ok
                    add 1 to idx-elab
                    move tor-numero to el-bozza-elab(idx-elab)
                    perform GENERA-FILE
                 end-if
              end-perform
           end-if.

           perform GENERA-PRODENER.

           perform SCRIVI-RIEPILOGO.

           if idx-elab > 0
              perform SCRIVI-RIEPILOGO-COLLI
           end-if.

      ***---
       SCRIVI-RIEPILOGO-COLLI.
           initialize path-riepilogo-c.                  
           move shi-path-backup-exp to path-riepilogo-c.
           inspect path-riepilogo-c 
                   replacing trailing spaces by low-value.
           string path-riepilogo-c    delimited low-value
                  "\RIEPILOGO_COLLI_" delimited size
                  como-data           delimited size
                  "_"                 delimited size
                  como-ora            delimited size
                  ".txt"              delimited size
             into path-riepilogo-c
           end-string.          
           inspect path-riepilogo-c 
                   replacing trailing low-value by spaces.
           open output riepilogo-c.           
           write line-riepilogo-c from spaces.
           write line-riepilogo-c from 78-line-div.
           write line-riepilogo-c from 78-tit-bozze.
           write line-riepilogo-c from 78-line-div.
           move 0 to cnt-bozze.
           perform varying idx-elab from 1 by 1 
                     until idx-elab > 999999
              if el-bozza-elab(idx-elab) = 0
                 move bozze-in-line to line-riepilogo-c
                 write line-riepilogo-c
                 exit perform
              end-if
              add 1 to cnt-bozze
              move el-bozza-elab(idx-elab) to bozza-in-line(cnt-bozze)
              if cnt-bozze = 78-max-bozze-in-line
                 move bozze-in-line to line-riepilogo-c
                 write line-riepilogo-c
                 move 0 to cnt-bozze   
                 perform 78-max-bozze-in-line times
                    add  1 to cnt-bozze
                    move 0 to bozza-in-line(cnt-bozze)
                 end-perform
                 move 0 to cnt-bozze
              end-if
           end-perform.
           write line-riepilogo-c from 78-line-div.
           write line-riepilogo-c from spaces.     
           write line-riepilogo-c from 78-line-div.
           write line-riepilogo-c from 78-tit-colli.  
           write line-riepilogo-c from 78-line-div.
           perform varying idx-colli from 1 by 1 
                     until idx-colli > tot-idx-colli
              move el-collo-tipocli(idx-colli) to tcl-codice
              read ttipocli no lock 
                   invalid move "*** NON TROVATA **" to tcl-descrizione
              end-read
              move tcl-codice      to tipocli-c
              move tcl-descrizione to tipocli-d
              move el-collo-num(idx-colli) to num-colli
              write line-riepilogo-c from riga-colli
           end-perform.                            
           write line-riepilogo-c from 78-line-div.
           close lineseq.
           move path-riepilogo-c to exp-ord-path-riepilogo-c.

      ***---
       SCRIVI-RIEPILOGO.       
           initialize como-messaggio.
           move num-rec-exp    to num-rec-ed.
           call "C$justify" using num-rec-ed, "L".
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "   delimited size
                   num-rec-ed    delimited low-value
                   " ordini."    delimited size
                   into como-messaggio.
           perform SCRIVI-MESSAGGIO.

           if num-rec-no-exp not = 0
              move num-rec-no-exp  to num-rec-ed
              initialize como-messaggio
              call "C$justify" using num-rec-ed, "L"
              inspect num-rec-ed replacing trailing space by low-value
              string "Non esportati " delimited size
                     num-rec-ed       delimited low-value
                     " ordini."       delimited size
                   into como-messaggio
              perform SCRIVI-MESSAGGIO
           end-if.

           move num-rec-righe  to num-rec-ed.
           initialize como-messaggio.
           call "C$justify" using num-rec-ed, "L".
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportate "     delimited size
                  num-rec-ed       delimited low-value
                  " righe ordine." delimited size
                  into como-messaggio.
           perform SCRIVI-MESSAGGIO.

           move num-rec-note to num-rec-ed
           initialize como-messaggio
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportate "     delimited size
                  num-rec-ed       delimited low-value
                  " note ordine."  delimited size
                  into como-messaggio.
           perform SCRIVI-MESSAGGIO.

           move num-rec-art  to num-rec-ed.
           initialize como-messaggio.
           call "C$justify" using num-rec-ed, "L".
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati "  delimited by size
                  num-rec-ed    delimited by low-value
                  " Articoli."  delimited by size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO

           move num-rec-ean  to num-rec-ed
           initialize como-messaggio
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati " delimited by size
                  num-rec-ed      delimited by low-value
                  " Ean."         delimited by size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO

           move num-rec-prodener  to num-rec-ed
           initialize como-messaggio
           call "C$justify" using num-rec-ed, "L"
           inspect num-rec-ed replacing trailing space by low-value
           string "Esportati " delimited by size
                  num-rec-ed      delimited by low-value
                  " Prodotti energetici." 
                                  delimited by size
                  into como-messaggio
           perform SCRIVI-MESSAGGIO.
      
      ***---
       GENERA-FILE.                          
      *    per prima cosa apro i file di export
           if prima-volta
              set prima-volta   to false
              perform APRI-EXP
              if errori
                 exit paragraph
              end-if
           end-if.

           perform EXP-ORDINE.

      ***---
       EXP-ORDINE.
           move tor-cod-cli     to cli-codice
           set cli-tipo-C       to true
           read clienti invalid continue end-read.

           if tor-prg-destino = 0
              move cli-ragsoc-1  to des-ragsoc-1
              move cli-ragsoc-2  to des-ragsoc-2
              move cli-indirizzo to des-indirizzo
              move cli-cap       to des-cap      
              move cli-localita  to des-localita 
              move cli-prov      to des-prov     
              move cli-nazione   to des-nazione  
           else
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog  
              read destini invalid continue end-read
           end-if.

           move tor-causale     to tca-codice
           read tcaumag
              invalid
                 initialize tca-caus-trasporto
           end-read

           initialize exp-tordini.
      *****     move shi-cod-SHI        to HUSCTES_COM_CODICE.
           move como-mag      to HUSCTES_COM_CODICE.
           string tor-anno         delimited by size
                  tor-numero(3:6)  delimited by size
                  into HUSCTES_ORDINE.

           if tca-si-zero
              move "VO"            to HUSCTES_ORDUSCTIP_CODICE
           else
              move "VS"            to HUSCTES_ORDUSCTIP_CODICE
           end-if.

           initialize HUSCTES_COMCAU_CODICE
           move tor-cod-cli        to HUSCTES_COMDES_CODICE
           move "00"               to HUSCTES_PRIORITA
           move tor-data-ordine    to HUSCTES_DATA
           move tor-data-passaggio-ordine to HUSCTES_DATA_PASS
           move "44"               to HUSCTES_UDF2
           move space              to HUSCTES_UDF3

           if cli-nazione = "ITA"
              move "3690"          to HUSCTES_UDF4
           else
              move "3691"          to HUSCTES_UDF4
           end-if.

           move space              to HUSCTES_UDF5
           move tca-caus-trasporto to HUSCTES_UDF6
           move space              to HUSCTES_UDF7
           move space              to HUSCTES_UDF8
           move space              to HUSCTES_UDF9

           |move tor-numero         to HUSCTES_UDF10
           move space              to HUSCTES_UDF10.
           if tor-vettore not = zero
              move "L"                to HUSCTES_COR_CODICE(1:1)
              move tor-vettore(4:2)   to HUSCTES_COR_CODICE(2:2)
           else
              move space           to HUSCTES_COR_CODICE
           end-if.
           move space              to HUSCTES_COR_CODICE_TP.

      *     move tor-data-note1(7:2)   to HTES_DATA_SCHED(1:2)
      *     move "/"                   to HTES_DATA_SCHED(3:1)
      *     move tor-data-note1(5:2)   to HTES_DATA_SCHED(4:2)
      *     move "/"                   to HTES_DATA_SCHED(6:1)
      *     move tor-data-note1(1:4)   to HTES_DATA_SCHED(7:4)
           move space                 to HTES_DATA_SCHED.
           move tor-data-note1        to HUSCTES_DATA_LIMITE.
           move space                 to HUSCTES_MERCE_VIAGGIANTE.
           inspect cli-ragsoc-1 replacing trailing space by low-value.
           string cli-ragsoc-1  delimited by low-value
                  " "           delimited by size
                  cli-ragsoc-2  delimited by size
                  into HUSCTES_COMDES_DESCRIZIONE.

           move cli-indirizzo         to HUSCTES_COMDES_INDIRIZZO.
           move cli-cap               to HUSCTES_COMDES_CAP.
           move cli-localita          to HUSCTES_COMDES_LOCALITA.
           move cli-prov              to HUSCTES_COMDES_PROVINCIA.
           move cli-nazione           to naz-codice
           read tnazioni invalid continue end-read.
           move naz-cod-EDI           to HUSCTES_COMDES_NAZIONE.


           inspect des-ragsoc-1 replacing trailing space by low-value.
           string  des-ragsoc-1  delimited by low-value
                   " "           delimited by size
                   des-ragsoc-2  delimited by size
                   into HUSCTES_DES_DESCRIZIONE.

           move des-indirizzo         to HUSCTES_DES_INDIRIZZO.
           move des-cap               to HUSCTES_DES_CAP.
           move des-localita          to HUSCTES_DES_LOCALITA.
           move des-prov              to HUSCTES_DES_PROVINCIA.
           move des-nazione           to naz-codice.
           read tnazioni invalid continue end-read.
           move naz-cod-EDI           to HUSCTES_DES_NAZIONE.

           string tor-cod-cli         delimited by size
                  "-"                 delimited by size
                  tor-prg-destino     delimited by size 
                  into HUSCTES_DES_CODICE.

           if tor-vettore = 0
              move "M"                to HUSCTES_TIPO_SPED
           else
              move "D"                to HUSCTES_TIPO_SPED
           end-if.

           move tor-num-ord-cli       to HUSCTES_RIF_ORDINE.
           move tor-data-ordine       to HUSCTES_RIF_DORDINE.

           move cli-tipo              to HUSCTES_UDF11.
           move cli-piva              to HUSCTES_UDF12.

           move exp-tordini   to line-riga4.
           write line-riga4.

           add 1 to num-rec-exp.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           move low-value    to ror-num-riga.
           start rordini key not < ror-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read rordini next
                       at end
                          exit perform
                    end-read
                    if tor-anno   not = ror-anno or
                       tor-numero NOT = ror-num-ordine
                       exit perform
                    end-if
                    move 1 to idx-colli
                    search tab-colli varying idx-colli
                      at end 
                         add 1 to tot-idx-colli
                         move ror-num-colli 
                           to el-collo-tipocli(tot-idx-colli)
                         move cli-tipo 
                           to el-collo-tipocli(tot-idx-colli)
                         move ror-num-colli 
                           to el-collo-num(tot-idx-colli)
                    when el-collo-tipocli(idx-colli) = cli-tipo      
                         add ror-num-colli to el-collo-num(idx-colli)
                    end-search
      *              if not ror-si-blister
                       perform EXP-RIGA
      *              end-if
                 end-perform
           end-start.

           perform EXP-NOTE.

           move  tor-chiave  to tmp-tor-chiave
           write TMP-TOR-REC invalid continue end-write.

      ***---
       EXP-RIGA.
           initialize exp-rordini.
                                                           
      *****     move shi-cod-SHI     to HUSCRIG_COM_CODICE.
           move como-mag        to HUSCRIG_COM_CODICE.
           move HUSCTES_ORDINE  to HUSCRIG_ORDINE.
           move ror-num-riga    to HUSCRIG_RIGA.
           move "S750"          to HUSCRIG_ART_COM_CODICE.

           string ror-cod-articolo       delimited by size
                  "-"                    delimited by size
                  ror-prg-tipo-imballo   delimited by size
                  into HUSCRIG_ART_CODICE.

           move space           to HUSCRIG_ART_ESTENSIONE.
           move space           to HUSCRIG_COMMESSA.
           move space           to HUSCRIG_STATOMERCE.

           move ror-prg-peso    to HUSCRIG_LOTTO.

           move space           to HUSCRIG_DSCADENZA.
           move ror-qta         to HUSCRIG_QTA_RICHIESTA.
           move "PZ"            to HUSCRIG_UDF1.
           move space           to HUSCRIG_UDF2.

           if ror-numero-master not = 0
              move ror-numero-master  to HUSCRIG_UDF3
              move ror-anno-master    to HUSCRIG_UDF4
              move ror-chiave-ordine-testa  to mto-chiave
              read mtordini invalid continue end-read
              move mto-num-ord-cli    to HUSCRIG_UDF5
              move mto-data-ordine    to HUSCRIG_UDF6
           else
              move space              to HUSCRIG_UDF3
              move space              to HUSCRIG_UDF4
              move space              to HUSCRIG_UDF5
              move space              to HUSCRIG_UDF6
           end-if.

           move ror-prg-peso    to HUSCRIG_PESO.
           move space           to HUSCRIG_UDF8.
           move space           to HUSCRIG_UDF9.
           move space           to HUSCRIG_UDF10.
           move space           to HUSCRIG_SEQ_ELAB.
           move space           to HUSCRIG_UDC_COMMITTENTE.
           move space           to HUSCRIG_RIGA_HOST.

           move exp-rordini   to line-riga5.
           write line-riga5.
           add 1 to num-rec-righe.

           perform EXP-ARTICOLO.

      ***---
       EXP-ARTICOLO.
           move ror-cod-articolo      to aes-articolo
                                         art-codice.
           move ror-prg-tipo-imballo  to aes-imballo.
                                         

           read ART-EXP-SHI no lock
                invalid continue
            not invalid exit paragraph
           end-read.

           read articoli no lock
                invalid  continue
           end-read

      *    Luciano
           perform DETERMINA-PESO.
      *    Luciano fine

           perform CICLO-PRODENER.
           perform GEN-ART-PARTE-FISSA.

           move 1   to cont.
           move ror-prg-tipo-imballo  to imballo-articolo(cont).
           perform GEN-ART-PARTE-VARIABILE..

           add 1 to num-rec-art.

           move como-utente  to aes-utente-creazione
                                aes-utente-ultima-modifica.
           accept aes-data-creazione  from century-date.
           move   aes-data-creazione  to aes-data-ultima-modifica.
           accept aes-ora-creazione   from time.
           move   aes-ora-creazione   to aes-ora-ultima-modifica.
           write AES-REC invalid continue end-write.

      ***---
       EXP-NOTE.
           initialize exp-note.

      *****     move shi-cod-SHI     to HUSCNOT_COM_CODICE.
           move como-mag        to HUSCNOT_COM_CODICE.
           
           move HUSCTES_ORDINE  to HUSCNOT_ORDINE.
           move "A"             to HUSCNOT_TIPO.
           move zero            to HUSCNOT_RIGA.
           move 5               to HUSCNOT_CODICE.
           move space           to HUSCNOT_USO.

           if tor-note1   not = space
              if tor-data-note1 = 0
                 move tor-note1 to HUSCNOT_DESCRIZIONE
              else
                 inspect tor-note1 replacing trailing space by low-value
                 string tor-note1           delimited by low-value
                        " "                 delimited by size
                        tor-data-note1(7:2) delimited by size
                        "/"                 delimited by size
                        tor-data-note1(5:2) delimited by size
                        "/"                 delimited by size
                        tor-data-note1(1:4) delimited by size
                        into HUSCNOT_DESCRIZIONE
              end-if

              move 3            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
              move 5            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
           end-if.

           if tor-note2   not = space
              move tor-note2    to HUSCNOT_DESCRIZIONE
              move 3            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
              move 5            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
           end-if.

           if tor-note3   not = space
              move tor-note3    to HUSCNOT_DESCRIZIONE
              move 3            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
              move 5            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
           end-if.

           if tor-note4   not = space
              move tor-note4    to HUSCNOT_DESCRIZIONE
              move 3            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              add 1             to num-rec-note
              write line-riga6
              move 5            to HUSCNOT_CODICE
              move exp-note     to line-riga6
              write line-riga6
              add 1             to num-rec-note
           end-if.

      ***---
       APRI-EXP.
           open output lineseq.
           open output lineseq2.
           open output lineseq3.
           open output lineseq4.
           open output lineseq5.
           open output lineseq6.


      ***---
       CLOSE-FILES.
           close lineseq.
           close lineseq2.
           close lineseq3.
           close lineseq4.
           close lineseq5.
           close lineseq6.
           close tordini.
           close mtordini.
           close rordini.
           close clienti.
           close destini.
           close tcaumag.
           close tnazioni.
           close articoli.
           close art-exp-shi.
           close tivaese.
           close timbalqta.
           close prodener.
           close progmag.
           close tmp-tordini-exp.
           close ttipocli.
  
      ***---
       VALIDA-ORDINE.
           set tutto-ok to true.
           if shi-mag-SHI-1 not = space or
              shi-mag-SHI-2 not = space
              move tor-causale     to tca-codice
              read tcaumag
                   invalid initialize tca-cod-magaz
              end-read
              if tca-cod-magaz = shi-mag-SHI-1
                 move tca-cod-magaz to como-mag
              else
                 if tca-cod-magaz = shi-mag-SHI-2
                    move tca-cod-magaz to como-mag
                 else
                    set errori  to true
                    add 1 to num-rec-no-exp
                  
                    move tor-numero   to tor-numero-ed
                    call "C$JUSTIFY" using tor-numero-ed, "L"
                    inspect tor-numero-ed 
                                   replacing trailing space by low-value
                    initialize como-messaggio
                    string "Ordine "              delimited by size
                           tor-anno               delimited by size
                           "/"                    delimited by size
                           tor-numero-ed          delimited by low-value
                           " non esportato "      delimited by size
                           "in quanto assegnato " delimited by size
                           "ad un magazzino "     delimited by size
                           "diverso da "          delimited by size
                           "quello selezionato"   delimited by size
                           into como-messaggio
                    set errore-bloccante  to false
                    perform SCRIVI-ERORRE
                 end-if
              end-if
           end-if.
           if tutto-ok
              if tor-cod-pagamento = "R04"
                 set errori  to true
                 add 1 to num-rec-no-exp
                 
                 move tor-numero   to tor-numero-ed
                 call "C$JUSTIFY" using tor-numero-ed, "L"
                 inspect tor-numero-ed 
                                replacing trailing space by low-value
                 initialize como-messaggio
                 string "Ordine "              delimited by size
                        tor-anno               delimited by size
                        "/"                    delimited by size
                        tor-numero-ed          delimited by low-value
                        " non esportato "      delimited by size
                        "in quanto codice "    delimited by size
                        "pagamento R04 "       delimited by size
                        into como-messaggio
                 set errore-bloccante  to false
                 perform SCRIVI-ERORRE
              end-if
           end-if.
           if tutto-ok
              if tor-num-fattura  not = 0 or
                 tor-data-fattura not = 0
                 set errori  to true
                 add 1 to num-rec-no-exp
                 
                 move tor-numero     to tor-numero-ed
                 call "C$JUSTIFY" using tor-numero-ed, "L"
                 inspect tor-numero-ed 
                                replacing trailing space by low-value
                 initialize como-messaggio
                 string "Ordine "                   delimited size
                        tor-anno                    delimited size
                        "/"                         delimited size
                        tor-numero-ed               delimited low-value
                        " non esportato "           delimited size
                        "in quanto già fatturato. " delimited size
                        "N. fattura "               delimited size
                        tor-num-fattura             delimited size
                        " - Anno "                  delimited size
                        tor-anno-fattura            delimited size
                        " - "                       delimited size
                        tor-data-fattura            delimited size
                        into como-messaggio
                 set errore-bloccante  to false
                 perform SCRIVI-ERORRE
              end-if
           end-if.

      ***---
       DETERMINA-PESO.
           move 0 to exp-prg-peso-utf
                     exp-prg-peso-non-utf.

           move art-codice to prg-cod-articolo.

      *****     if shi-mag-SHI = space
      *****        move low-value    to prg-cod-magazzino
      *****     else
      *****        move shi-mag-SHI  to prg-cod-magazzino
      *****     end-if.
           move como-mag  to prg-cod-magazzino.
           move low-value to prg-tipo-imballo prg-peso.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next
                         at end exit perform
                    end-read
                    if art-codice not = prg-cod-articolo
                       exit perform
                    end-if
                    if prg-cod-magazzino not = como-mag
                       exit perform
                    end-if
                    if prg-tipo-imballo not = space
                       move prg-peso-utf     to exp-prg-peso-utf
                       move prg-peso-non-utf to exp-prg-peso-non-utf
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
           COPY "exp-procedure.cpy".
           copy "Exparticoli-Procedure.cpy".
