       IDENTIFICATION DIVISION.
       PROGRAM-ID. blockpgm.
      *
      *    Verifica se un programma può essere lanciato o se é già
      *    in esecuzione (per la stessa sessione di lavoro).
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       input-output section.
       file-control.
      *
      *   Tabella blocchi attivi sui programmi 
      *
       SELECT FBLOCK
           ASSIGN       TO DISK "FBLOCK"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS AUTOMATIC WITH LOCK ON RECORD 
           FILE STATUS  IS STATO-IO
           RECORD KEY   IS FB-PRI-KEY.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FBLOCK
           LABEL RECORD IS STANDARD.
       01  REC-FBLOCK.
           05 FB-PRI-KEY.
              10 FB-PROG-ID    PIC  X(15).
              10 FB-DATA       PIC  9(8).
              10 FB-ORA        PIC  9(8).
           05 FB-HND-WIN       PIC S9(9).

       WORKING-STORAGE SECTION.
      *
       COPY "ACUGUI.DEF".
       77  STATO-IO             PIC XX.
      *
       LINKAGE SECTION.
      *
           copy "blockpgm.lks".
      *
      ******************************************************************
      *    PROCEDURE DIVISION                                          *
      ******************************************************************
      *
       PROCEDURE DIVISION USING LK-BLOCKPGM.

       DECLARATIVES.
       FBLOCK-ERR SECTION.
           use after error procedure on fblock.
           evaluate stato-io
           when "93"
           when "99" continue
           end-evaluate.
       END DECLARATIVES.

       INIZIO-PROC.
           |Richiamo da menu acu (richiamato da menu visual).
           |Non essendoci nulla in questa linkage non deve fare niente
           if LK-BL-PROG-ID = spaces exit paragraph end-if

           OPEN I-O FBLOCK.

           EVALUATE TRUE
              WHEN LK-BL-LETTURA                   
                   PERFORM LETTURA
              WHEN LK-BL-SCRITTURA                   
                   PERFORM SCRITTURA
              WHEN LK-BL-CANCELLAZIONE                   
                   PERFORM CANCELLAZIONE
           END-EVALUATE.
      *
           GO TO CHIUSURA.

       CHIUSURA.
           CLOSE FBLOCK.
           EXIT PROGRAM.
      *************
       LETTURA.
           MOVE LK-BL-PROG-ID  TO FB-PROG-ID.
           MOVE LK-BL-DATA     TO FB-DATA.
           MOVE LK-BL-ORA      TO FB-ORA.
           READ FBLOCK
                   INVALID 
                        INITIALIZE LK-BL-USO
               NOT INVALID 
                        SET LK-BL-IN-USO TO TRUE
                        MOVE FB-HND-WIN  TO LK-HND-WIN
           END-READ.


      *************
       SCRITTURA.
           MOVE LK-BL-PROG-ID  TO FB-PROG-ID.
           MOVE LK-BL-DATA     TO FB-DATA.
           MOVE LK-BL-ORA      TO FB-ORA.
           MOVE LK-HND-WIN     TO FB-HND-WIN.
           WRITE REC-FBLOCK
                   INVALID REWRITE REC-FBLOCK
           END-WRITE.
           if stato-io = "99"
              perform 50 times
                 add 1 to lk-bl-ora
                 write rec-fblock
                         invalid rewrite rec-fblock
                 end-write
                 if stato-io = "00" exit perform end-if
              end-perform
           end-if.

      *************
       CANCELLAZIONE.
           MOVE LK-BL-PROG-ID  TO FB-PROG-ID.
           MOVE LK-BL-DATA     TO FB-DATA.
           MOVE LK-BL-ORA      TO FB-ORA.
           DELETE FBLOCK
                   INVALID CONTINUE
           END-DELETE.
           if stato-io = "99" add 1 to lk-bl-ora end-if.
