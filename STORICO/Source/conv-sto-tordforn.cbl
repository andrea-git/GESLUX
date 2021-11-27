       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-sto-tordforn.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "STO-tordforn.sl".  
       SELECT STO-tordforn-old
           ASSIGN       TO "tordforn-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-sto-tordforn-old
           RECORD KEY   IS old-STO-tof-chiave
           ALTERNATE RECORD KEY IS old-STO-tof-k-causale = 
           old-STO-tof-causale, 
           old-STO-tof-chiave
           ALTERNATE RECORD KEY IS old-STO-tof-k-stato = 
           old-STO-tof-stato, 
           old-STO-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fornitore = 
           old-STO-tof-cod-forn, 
           old-STO-tof-destino, old-STO-tof-stato, old-STO-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-STO-tof-k-data = 
           old-STO-tof-data-ordine, old-STO-tof-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "STO-tordforn.fd". 

       FD  sto-tordforn-old.
       01 old-sto-tof-rec.
           05 old-sto-tof-chiave.
               10 old-sto-tof-anno     PIC  9(4).
               10 old-sto-tof-numero   PIC  9(8).
           05 old-sto-tof-dati.
               10 old-sto-tof-causale  PIC  x(4).
               10 old-sto-tof-data-ordine          PIC  9(8).
               10 FILLER           PIC  9(8).
               10 old-sto-tof-cod-forn PIC  9(5).
               10 old-sto-tof-destino  PIC  9(5).
               10 old-sto-tof-dati-forn.
                   15 old-sto-tof-referente            PIC  x(40).
                   15 old-sto-tof-tel-dir  PIC  x(20).
                   15 old-sto-tof-fax      PIC  x(20).
                   15 old-sto-tof-email    PIC  x(100).
               10 FILLER           PIC  9(12).
               10 old-sto-tof-cliente  PIC  9(5).
               10 old-sto-tof-data-listino         PIC  9(8).
               10 old-sto-tof-cod-pagamento        PIC  x(3).
      *(( XFD NAME = old-sto-tof-cod-pagame ))
               10 old-sto-tof-cod-ese-iva          PIC  x(3).
               10 old-sto-tof-tipo-scarico         PIC  x.
                   88 old-sto-tof-urgente VALUE IS "U". 
               10 old-sto-tof-data-consegna        PIC  9(8).
               10 old-sto-tof-mese-rif PIC  99.
               10 old-sto-tof-pz-tot   PIC  9(8).
               10 old-sto-tof-pz-arrivati          PIC  9(8).
               10 old-sto-tof-tipo-creazione       PIC  x.
                   88 old-sto-tof-manuale VALUE IS "M". 
                   88 old-sto-tof-automatico VALUE IS "A". 
               10 old-sto-tof-stato    PIC  x.
                   88 old-sto-tof-inserito VALUE IS "I". 
                   88 old-sto-tof-inviato VALUE IS "S". 
                   88 old-sto-tof-in-lavorazione VALUE IS "L". 
                   88 old-sto-tof-chiuso VALUE IS "C". 
               10 old-sto-tof-stato-evasione       PIC  x.
                   88 old-sto-tof-inevaso VALUE IS "I". 
                   88 old-sto-tof-evas-parz VALUE IS "P". 
                   88 old-sto-tof-evas-tot VALUE IS "T". 
               10 old-sto-tof-da-confermare        PIC  9(1).
                   88 old-sto-tof-da-confermare-si VALUE IS 1. 
                   88 old-sto-tof-da-confermare-no VALUE IS 0. 
               10 old-sto-tof-rivisto  PIC  9(1).
                   88 old-sto-tof-riviold-sto-si VALUE IS 1. 
                   88 old-sto-tof-riviold-sto-no VALUE IS 0. 
               10 old-sto-tof-programmazione       PIC  9.
                   88 old-sto-tof-programmazione-si VALUE IS 1. 
                   88 old-sto-tof-programmazione-no VALUE IS 0. 
               10 old-sto-tof-tipo-invio           PIC  x.
                   88 old-sto-tof-invio-man VALUE IS "M". 
                   88 old-sto-tof-invio-fax VALUE IS "F". 
                   88 old-sto-tof-invio-mail VALUE IS "E". 
               10 old-sto-tof-dati-invio.
                   15 old-sto-tof-data-invio           PIC  9(8).
                   15 old-sto-tof-ora-invio            PIC  9(8).
                   15 old-sto-tof-utente-invio         PIC  X(10).
               10 old-sto-tof-tipo-chiusura        PIC  x.
                   88 old-sto-tof-chiusura-man VALUE IS "M". 
                   88 old-sto-tof-chiusura-auto VALUE IS "A". 
               10 old-sto-tof-dati-chiusura.
                   15 old-sto-tof-nota-chiusura        PIC  x(50).
                   15 old-sto-tof-data-chiusura        PIC  9(8).
                   15 old-sto-tof-ora-chiusura         PIC  9(8).
                   15 old-sto-tof-utente-chiusura      PIC  X(10).
               10 old-sto-tof-dati-comuni.
                   15 old-sto-tof-data-creazione       PIC  9(8).
                   15 old-sto-tof-ora-creazione        PIC  9(8).
                   15 old-sto-tof-utente-creazione     PIC  X(10).
                   15 old-sto-tof-data-ultima-modifica PIC  9(8).
                   15 old-sto-tof-ora-ultima-modifica  PIC  9(8).
                   15 old-sto-tof-utente-ultima-modifica           PIC  
           X(10).
               10 old-sto-tof-vuoti.
                   15 old-sto-tof-destino-c            PIC  9(5).
                   15 old-sto-tof-promo    PIC  9(15).
                   15 old-sto-tof-franco-part          PIC  9.
                       88 old-sto-tof-franco-part-si VALUE IS 1. 
                       88 old-sto-tof-franco-part-no VALUE IS 0. 
      *(( XFD NAME = old-sto-tof-num-vuoto- ))
                   15 old-sto-tof-aperto   PIC  9(1).
                       88 old-sto-tof-aperto-si VALUE IS 1. 
                       88 old-sto-tof-aperto-no VALUE IS 0. 
                   15 old-sto-tof-st-dati-fatt         PIC  9.
                       88 old-sto-tof-st-dati-fatt-si VALUE IS 1. 
                       88 old-sto-tof-st-dati-fatt-no VALUE IS 0. 
                   15 old-sto-tof-num-vuoto-2          PIC  9(13).
                   15 old-sto-tof-num-vuoto-3          PIC  9(18).
                   15 old-sto-tof-alfa-vuoto-1         PIC  X(20).
                   15 old-sto-tof-alfa-vuoto-2         PIC  X(20).
                   15 old-sto-tof-stampante            PIC  X.
                       88 old-sto-tof-stampante-A VALUE IS " ". 
                       88 old-sto-tof-stampante-L VALUE IS "L". 
                       88 old-sto-tof-stampante-M VALUE IS "M". 
                   15 old-sto-tof-alfa-vuoto-3         PIC  X(19).  

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".

       77  status-sto-tordforn       pic X(2).
       77  status-sto-tordforn-old   pic X(2).

       77  CONT                 PIC 9(6).
       77  CONT-ED              PIC Z(6).
       77  scelta               pic 9.
       77  path-archivi         pic x(1200).
       77  PATH-STO-tordforn    pic x(1200).
       77  filePrefix           pic x(1000).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     STO-tordforn
                     sto-tordforn-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.         
           accept filePrefix from environment "FILE_PREFIX".
           accept path-archivi from environment "PATH_ARCHIVI_STO".
           set environment "FILE_PREFIX" to path-archivi.
           inspect path-archivi replacing trailing spaces by low-value.
           string path-archivi delimited low-value
                  "tordforn"    delimited size
                  into path-sto-tordforn
           end-string.

           display message box
                   "Confermi la conversione del file STORICO tordforn?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tordforn"
                          x"22"
                          " in "
                          x"22"
                          "tordforn-old"
                          x"22"
                          "nella cartella Archivi-STO"
                          x"22"
                          "."
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.
                  
           set environment "FILE_PREFIX" to filePrefix.
           goback.


      ***---
       CONVERSIONE.
           move zero   to cont.

           open input  sto-tordforn-old.
           open output sto-tordforn.

           move low-value to OLD-STO-tof-chiave.

           start STO-tordforn-old key >= old-STO-tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read STO-tordforn-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close STO-tordforn
                 sto-tordforn-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize STO-tof-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.      

           move old-STO-tof-rec to STO-tof-rec.

           write STO-tof-rec. 
