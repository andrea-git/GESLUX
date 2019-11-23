       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      impTBL.
       AUTHOR.                          Andrea.
       REMARKS. Merge del file TBL creato da Lubex con quello std SSI.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcodpag.sl".

       SELECT lub-tbl
           ASSIGN       TO RANDOM wstampa
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS status-lub-tbl
           RECORD KEY   IS lub-TBLPA-CODICE
           ALTERNATE RECORD KEY IS 
           lub-TBL-CODICE-01 = lub-TBLPA-CODICE1, 
            lub-TBLPA-DESCRIZIONE1, lub-TBLPA-CODICE2
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcodpag.fd".

      *(( XFD FILE = tcodpag ))
       FD  lub-tbl.
      *$XFD  WHEN TBLPA-CODICE1="PA" TABLENAME=TBLPA
       01 lub-RECORD-TBLPA.
           05 lub-TBLPA-CODICE.
               10 lub-TBLPA-CODICE1    PIC  X(02).
               10 lub-TBLPA-CODICE2    PIC  X(20).
           05 lub-RESTO-RECORD-TBLPA.
               10 lub-TBLPA-DESCRIZIONE1           PIC  X(30).
               10 lub-TBLPA-DESCRIZIONE2           PIC  X(30).
               10 lub-TBLPA-CODICE-SC  PIC  X(03).
               10 lub-TBLPA-CODICE-SC-MAS          PIC  X(08).
               10 lub-TBLPA-PAGAMENTO-IMMEDIATO.
                   15 lub-TBLPA-IMMEDIATO  PIC  X(01).
                       88 lub-TBLPA-IMMEDIATO-88 VALUE IS "S" " ". 
                   15 lub-TBLPA-CODICE-MAS PIC  X(08).
                   15 lub-TBLPA-CODICE-CO  PIC  X(03).
               10 lub-TBLPA-NUMERO-SCADENZE        PIC  9(02).
                   88 lub-TBLPA-NUMERO-SCADENZE-88 VALUE IS 1 THRU 36. 
               10 lub-TBLPA-MESI-ESCLUSI.
                   15 lub-TBLPA-MESE1      PIC  9(02).
                       88 lub-TBLPA-MESE1-88 VALUE IS 0 THRU 12. 
                   15 lub-TBLPA-GIORNO1    PIC  9(02).
                   15 lub-TBLPA-MESE2      PIC  9(02).
                       88 lub-TBLPA-MESE2-88 VALUE IS 0 THRU 12. 
                   15 lub-TBLPA-GIORNO2    PIC  9(02).
               10 lub-TBLPA-IVA        PIC  X(01).
                   88 lub-TBLPA-IVA-88 VALUE IS "A" " ". 
                   88 lub-TBLPA-IVA-88-o VALUE IS " ". 
                   88 lub-TBLPA-IVA-88-a VALUE IS "A". 
                   88 lub-TBLPA-iva-88-g VALUE IS "A" " ". 
      *
      *Anticipato - Posticipato - prima rata solo di IVA
      *
               10 lub-TBLPA-TABELLA-X.
                   15 lub-TBLPA-TABELLA
                              OCCURS 36 TIMES.
                       20 lub-TBLPA-CODICE-TR  PIC  X(01).
                       20 lub-TBLPA-INIZIO-CONTEGGIO       PIC  9(02).
                       20 lub-TBLPA-TIPO-IMPORTI           PIC  X(01).
                       88 lub-TBLPA-TIPO-IMPORTI-88 VALUE IS "P" "A" "C"
           . 
                           88 lub-TBLPA-TIPO-IMPORTI-88-P VALUE IS "P". 
                           88 lub-TBLPA-TIPO-IMPORTI-88-A VALUE IS "A". 
                           88 lub-TBLPA-TIPO-IMPORTI-88-C VALUE IS "C". 
                       20 lub-TBLPA-TIPO-SCADENZE          PIC  X(01).
                           88 lub-TBLPA-TIPO-SCADENZE-88-i VALUE IS "I". 
                           88 lub-TBLPA-TIPO-SCADENZE-88-d VALUE IS "D". 
                         88 lub-TBLPA-TIPO-SCADENZE-88 VALUE IS "I" "D". 
                       20 lub-TBLPA-IMPORTO    PIC  S9(15)V9(03)
                                  SIGN IS TRAILING SEPARATE CHARACTER.
                       20 lub-TBLPA-SCADENZA   PIC  9(08).
               10 lub-TBLPA-CODICE-ART PIC  X(15).
               10 lub-TBLPA-SLITTAMENTO            PIC  X(01).
                   88 lub-TBLPA-SLITTAMENTO-88 VALUE IS " " "S". 
               10 lub-TBLPA-CONTRASSEGNO-79        PIC  X(01).
                   88 lub-TBLPA-CONTRASSEGNO-79-88 VALUE IS " " "S". 
               10 lub-TBLPA-DETRAZIONE-X.
                   15 lub-TBLPA-detrazione
                              OCCURS 36 TIMES.
                       20 lub-TBLPA-DETRAZIONE-det         PIC  X(01).
                        88 lub-TBLPA-DETRAZIONE-det-88 VALUE IS "S" " ". 
                         88 lub-TBLPA-DETRAZIONE-det-yes VALUE IS "S". 
                          88 lub-TBLPA-DETRAZIONE-det-no VALUE IS " ". 
      *(( XFD NAME = TBLPA-ESCL-GG1 ))
               10 lub-TBLPA-ESCLUSO-DAL-GIORNO1    PIC  9(02).
      *(( XFD NAME = TBLPA-ESCL-GG2 ))
               10 lub-TBLPA-ESCLUSO-DAL-GIORNO2    PIC  9(02).
               10 lub-TBLPA-FILLER     PIC  X(15).
               10 lub-TBLPA-FILLER-RIS PIC  X(40).

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Merge TBL".

      * FILE-STATUS
       77  status-tcodpag       pic xx.
       77  status-lub-tbl       pic xx.
       77  wstampa              pic x(256) value "C:\xtbl".

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILE.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILE
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILE.
           open i-o tcodpag.
           open input lub-tbl.

      ***---
       ELABORAZIONE.
           move low-value to lub-record-tblpa.
           start lub-tbl key is >= lub-TBLPA-CODICE
                 invalid continue
           end-start.
           perform until 1 = 2
              read lub-tbl next at end exit perform end-read
              move lub-record-tblpa to record-tblpa
              write record-tblpa invalid rewrite record-tblpa end-write
           end-perform.
           display message "FINE!".

      ***---
       CLOSE-FILE.
           close tcodpag lub-tbl.

      ***---
       EXIT-PGM.
           goback.
