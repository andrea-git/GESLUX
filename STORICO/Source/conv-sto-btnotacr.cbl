       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-sto-btnotacr.
       remarks. Per allineamento storico
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "STO-btnotacr.sl".

       SELECT old-STO-btnotacr
           ASSIGN       TO  "btnotacr-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-old-sto-btnotacr
           RECORD KEY   IS old-STO-btno-chiave
           ALTERNATE RECORD KEY IS old-k1 = old-STO-btno-cod-cli, 
           old-STO-btno-prg-destino, 
           old-STO-btno-anno, old-STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-data = old-STO-btno-data, 
           old-STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fattura = 
           old-STO-btno-anno-fatt, 
           old-STO-btno-num-fatt
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-reso = old-STO-btno-anno, 
           old-STO-btno-num-reso
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-bolla = 
           old-STO-btno-anno-bolla, 
           old-STO-btno-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-vettore = old-STO-btno-vettore, 
           old-STO-btno-anno, old-STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-nota = old-STO-btno-anno-nc, 
           old-STO-btno-num-nc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fatman = old-STO-btno-anno-fm, 
           old-STO-btno-num-fm
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "STO-btnotacr.fd".

      *(( XFD FILE = OLD-sto-btnotacr ))
       FD  OLD-sto-btnotacr.
       01 OLD-sto-btno-rec.
           05 OLD-sto-btno-chiave.
               10 OLD-sto-btno-anno    PIC  9(4).
               10 OLD-sto-btno-numero  PIC  9(8).
           05 OLD-sto-btno-dati.
               10 OLD-sto-btno-causale PIC  x(4).
               10 OLD-sto-btno-cod-cli PIC  9(5).
               10 OLD-sto-btno-prg-destino         PIC  9(5).
               10 OLD-sto-btno-data    PIC  9(8).
               10 OLD-sto-btno-cod-pag PIC  x(3).
               10 OLD-sto-btno-vettore PIC  9(5).
               10 OLD-sto-btno-dati-fm.
                   15 OLD-sto-btno-fm-tipo PIC  x.
                       88 OLD-sto-btno-fm-vettore VALUE IS "V". 
                       88 OLD-sto-btno-fm-cliente VALUE IS "C". 
                   15 OLD-sto-btno-cod-cli-fm          PIC  9(5).
                   15 OLD-sto-btno-prg-destino-fm      PIC  9(5).
      *(( XFD NAME = OLD-sto-btno-vettore_1 ))
                   15 OLD-sto-btno-vettore-fm          PIC  9(5).
               10 OLD-sto-btno-note    PIC  X(300).
               10 OLD-sto-btno-bolla.
                   15 OLD-sto-btno-data-bolla          PIC  9(8).
                   15 OLD-sto-btno-num-bolla           PIC  x(8).
               10 OLD-sto-btno-fatt-rif.
                   15 OLD-sto-btno-data-fatt           PIC  9(8).
                   15 OLD-sto-btno-num-fatt            PIC  9(8).
               10 OLD-sto-btno-rif-fm.
                   15 OLD-sto-btno-data-fm PIC  9(8).
                   15 OLD-sto-btno-num-fm  PIC  9(8).
               10 OLD-sto-btno-rif-nc.
                   15 OLD-sto-btno-data-nc PIC  9(8).
                   15 OLD-sto-btno-num-nc  PIC  9(8).
               10 OLD-sto-btno-motivo-cont         PIC  x(200).
               10 OLD-sto-btno-errore-colpa        PIC  x.
                   88 OLD-sto-btno-corriere VALUE IS "C". 
                   88 OLD-sto-btno-magazzino VALUE IS "M". 
                   88 OLD-sto-btno-fornitore VALUE IS "F". 
                   88 OLD-sto-btno-cliente VALUE IS "K". 
                   88 OLD-sto-btno-ufficio VALUE IS "U". 
               10 OLD-sto-btno-stato   PIC  X(1).
                   88 OLD-sto-btno-attivo VALUE IS "A". 
                   88 OLD-sto-btno-disattivo VALUE IS "D". 
                   88 OLD-sto-btno-bloccato VALUE IS "B". 
               10 OLD-sto-btno-dati-comuni.
                   15 OLD-sto-btno-data-creazione      PIC  9(8).
                   15 OLD-sto-btno-ora-creazione       PIC  9(8).
                   15 OLD-sto-btno-utente-creazione    PIC  X(10).
                   15 OLD-sto-btno-data-modifica       PIC  9(8).
                   15 OLD-sto-btno-ora-modifica        PIC  9(8).
                   15 OLD-sto-btno-utente-modifica     PIC  X(10).
               10 OLD-sto-btno-vuoti.
                   15 OLD-sto-btno-num-reso            PIC  9(8).
                   15 OLD-sto-btno-anno-fatt           PIC  9(4).
                   15 OLD-sto-btno-anno-bolla          PIC  9(4).
                   15 OLD-sto-btno-anno-nc PIC  9(4).
                   15 OLD-sto-btno-anno-fm PIC  9(4).
                   15 OLD-sto-btno-num-vuoto-2         PIC  9(6).
                   15 OLD-sto-btno-num-vuoto-3         PIC  9(15).
                   15 OLD-sto-btno-alfa-vuoto-1        PIC  X(20).
                   15 OLD-sto-btno-alfa-vuoto-2        PIC  X(20).
                   15 OLD-sto-btno-alfa-vuoto-3        PIC  X(20).


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-sto-btnotacr       pic X(2).
       77  status-old-sto-btnotacr   pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.
       77  path-archivi         pic x(1200).
       77  PATH-STO-btnotacr    pic x(1200).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     STO-btnotacr
                 old-sto-btnotacr
                     .
           COPY "DECLXER2".

       MAIN-PRG.         
           accept path-archivi from environment "PATH_ARCHIVI_STO".
           set environment "FILE_PREFIX" to path-archivi.
           inspect path-archivi replacing trailing spaces by low-value.
           string path-archivi delimited low-value
                  "btnotacr"   delimited size
                  into path-sto-btnotacr
           end-string.

           display message box
                        "Confermi la conversione del file STO-btnotacr?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "sto-btnotacr"
                          x"22"
                          " in "
                          x"22"
                          "sto-btnotacr-old"
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
           move zero   to cont.

           open input  old-sto-btnotacr.
           open output sto-btnotacr.

           move low-value to OLD-STO-btno-chiave.

           start old-STO-btnotacr key >= old-STO-btno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    read old-STO-btnotacr next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close STO-btnotacr
                 old-sto-btnotacr.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           move old-STO-btno-anno         to STO-btno-anno        
           move old-STO-btno-numero       to STO-btno-numero      
           move old-STO-btno-causale      to STO-btno-causale     
           move old-STO-btno-cod-cli      to STO-btno-cod-cli     
           move old-STO-btno-prg-destino  to STO-btno-prg-destino 
           move old-STO-btno-data         to STO-btno-data        
           move old-STO-btno-cod-pag      to STO-btno-cod-pag     
           move old-STO-btno-vettore      to STO-btno-vettore     
           move old-STO-btno-dati-fm      to STO-btno-dati-fm     
           move old-STO-btno-note         to STO-btno-note        
           move old-STO-btno-bolla        to STO-btno-bolla       
           move old-STO-btno-fatt-rif     to STO-btno-fatt-rif    
           move old-STO-btno-rif-fm       to STO-btno-rif-fm      
           move old-STO-btno-rif-nc       to STO-btno-rif-nc      
           move old-STO-btno-motivo-cont  to STO-btno-motivo-cont  
           move old-STO-btno-errore-colpa to STO-btno-errore-colpa 
           move old-STO-btno-stato        to STO-btno-stato       
           move old-STO-btno-dati-comuni  to STO-btno-dati-comuni 
           move old-STO-btno-num-reso     to STO-btno-num-reso     
           move old-STO-btno-anno-fatt    to STO-btno-anno-fatt    
           move old-STO-btno-anno-bolla   to STO-btno-anno-bolla   
           move old-STO-btno-anno-nc      to STO-btno-anno-nc     
           move old-STO-btno-anno-fm      to STO-btno-anno-fm     

           write STO-btno-rec. 
