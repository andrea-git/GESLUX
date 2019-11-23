       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convbtnotacr.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "btnotacr.sl".
       SELECT btnotacr-old
           ASSIGN       TO  "btnotacr-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-btnotacr-old
           RECORD KEY   IS old-btno-chiave
           ALTERNATE RECORD KEY IS old-k1 = old-btno-cod-cli, 
           old-btno-prg-destino, 
           old-btno-anno, old-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-data = old-btno-data, 
           old-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fattura = old-btno-anno-fatt, 
           old-btno-num-fatt
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-reso = old-btno-anno, 
           old-btno-num-reso
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-bolla = old-btno-anno-bolla, 
           old-btno-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-vettore = old-btno-vettore, 
           old-btno-anno, 
           old-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-nota = old-btno-anno-nc, 
           old-btno-num-nc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fatman = 
           old-btno-anno-fm, old-btno-num-fm
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "btnotacr.fd".
       FD  btnotacr-old.
       01 old-btno-rec.
           05 old-btno-chiave.
               10 old-btno-anno        PIC  9(4).
               10 old-btno-numero      PIC  9(8).
           05 old-btno-dati.
               10 old-btno-causale     PIC  x(4).
               10 old-btno-cod-cli     PIC  9(5).
               10 old-btno-prg-destino PIC  9(5).
               10 old-btno-data        PIC  9(8).
               10 old-btno-cod-pag     PIC  x(3).
               10 old-btno-vettore     PIC  9(5).
               10 old-btno-dati-fm.
                   15 old-btno-fm-tipo     PIC  x.
                       88 old-btno-fm-vettore VALUE IS "V". 
                       88 old-btno-fm-cliente VALUE IS "C". 
                   15 old-btno-cod-cli-fm  PIC  9(5).
                   15 old-btno-prg-destino-fm          PIC  9(5).
      *(( XFD NAME = btno-vettore_1 ))
                   15 old-btno-vettore-fm  PIC  9(5).
               10 old-btno-note        PIC  X(300).
               10 old-btno-bolla.
                   15 old-btno-data-bolla  PIC  9(8).
                   15 old-btno-num-bolla   PIC  x(8).
               10 old-btno-fatt-rif.
                   15 old-btno-data-fatt   PIC  9(8).
                   15 old-btno-num-fatt    PIC  9(8).
               10 old-btno-rif-fm.
                   15 old-btno-data-fm     PIC  9(8).
                   15 old-btno-num-fm      PIC  9(8).
               10 old-btno-rif-nc.
                   15 old-btno-data-nc     PIC  9(8).
                   15 old-btno-num-nc      PIC  9(8).
               10 old-btno-motivo-cont PIC  x(200).
               10 old-btno-errore-colpa            PIC  x.
               10 old-btno-stato       PIC  X(1). 
               10 old-btno-dati-comuni.
                   15 old-btno-data-creazione          PIC  9(8).
                   15 old-btno-ora-creazione           PIC  9(8).
                   15 old-btno-utente-creazione        PIC  X(10).
                   15 old-btno-data-modifica           PIC  9(8).
                   15 old-btno-ora-modifica            PIC  9(8).
                   15 old-btno-utente-modifica         PIC  X(10).
               10 old-btno-vuoti.
                   15 old-btno-num-reso    PIC  9(8).
                   15 old-btno-anno-fatt   PIC  9(4).
                   15 old-btno-anno-bolla  PIC  9(4).
                   15 old-btno-anno-nc     PIC  9(4).
                   15 old-btno-anno-fm     PIC  9(4).
                   15 old-btno-num-vuoto-2 PIC  9(6).
                   15 old-btno-num-vuoto-3 PIC  9(15).
                   15 old-btno-alfa-vuoto-1            PIC  X(20).
                   15 old-btno-alfa-vuoto-2            PIC  X(20).
                   15 old-btno-alfa-vuoto-3            PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-btnotacr      pic X(2).
       77  status-btnotacr-old  pic X(2).

       77  cont                 PIC 9(6).
       77  cont-ed              PIC zzz.zz9.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     btnotacr
                     btnotacr-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file btnotacr?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "btnotacr"
                          x"22"
                          " in "
                          x"22"
                          "btnotacr-old"
                          x"22"
                          "."
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.

           goback.


      ***---
       CONVERSIONE.
           move 0 to cont

           open input  btnotacr-old.
           open output btnotacr.


           move low-value to old-btno-chiave.

           start btnotacr-old key >= old-btno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read btnotacr-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close btnotacr
                 btnotacr-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize btno-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move old-btno-chiave       to btno-chiave
           move old-btno-causale      to btno-causale
           move old-btno-cod-cli      to btno-cod-cli
           move old-btno-prg-destino  to btno-prg-destino
           move old-btno-data         to btno-data
           move old-btno-cod-pag      to btno-cod-pag
           move old-btno-vettore      to btno-vettore
           move old-btno-dati-fm      to btno-dati-fm
           move old-btno-note         to btno-note
           move old-btno-bolla        to btno-bolla
           move old-btno-fatt-rif     to btno-fatt-rif
           move old-btno-rif-fm       to btno-rif-fm
           move old-btno-rif-nc       to btno-rif-nc
           move old-btno-motivo-cont  to btno-motivo-cont
           move old-btno-errore-colpa to btno-errore-colpa
           move old-btno-stato        to btno-stato
           move old-btno-dati-comuni  to btno-dati-comuni
           move old-btno-vuoti        to btno-vuoti.
       
           write btno-rec.
