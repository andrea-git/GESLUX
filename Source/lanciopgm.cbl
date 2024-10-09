       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lanciopgm.

      ******************************************************************
       ENVIRONMENT DIVISION.

       WORKING-STORAGE SECTION.
           copy "link-gpgmexe.def".
       78 titolo1 VALUE IS "Geslux - Menu Principale". 
       77  pgmChiamante pic x(10).

       LINKAGE SECTION.        
       77  session-id  pic x(50).
           copy "common-linkage.def".

      ******************************************************************
       PROCEDURE DIVISION using LK-BLOCKPGM,
                                USER-CODI,
                                LIVELLO-ABIL.
       
       Main Section.
           call "C$CALLEDBY" using pgmChiamante.
      *    come prima cosa verifico che non ci siano blocchi al lancio 
      *    dei programmi, attualmente solamente EVACLI
           move LK-BL-PROG-ID   to pgme-pgm
           move user-codi       to pgme-utente
           set pgme-ingresso    to true

           |Verrà deciso dopo non in ingresso, in base all'evasione scelta
           if pgme-pgm = "evacli" and pgmChiamante not = "evacli" 
              set pgme-ok to true
           else
              call   "gpgmexe" using gpgmexe-linkage
              cancel "gpgmexe"
           end-if

      *    se è tutto ok lancio il pgm
           if pgme-ok
              call LK-BL-PROG-ID  using lk-blockpgm
                                        user-codi
                                        LIVELLO-ABIL 
                 on overflow
                    display message "Il programma non esiste!"
                             title Titolo1
                    set lk-bl-cancellazione to true
                    perform I-O-BLOCCO
              end-call
              cancel LK-BL-PROG-ID
      *       chiamata della cancellazione
              set pgme-uscita   to true
              call   "gpgmexe" using gpgmexe-linkage
              cancel "gpgmexe"
           else
              set lk-bl-cancellazione to true
              perform I-O-BLOCCO
           end-if.

           goback.


      ***---
       I-O-BLOCCO.
      *     MOVE LK-BL-PROG-ID   TO LK-BL-PROG-ID.
      *     MOVE COMO-DATA       TO LK-BL-DATA.
      *     if lk-bl-ora = 0
      *        MOVE COMO-ORA     TO LK-BL-ORA
      *     end-if.
           CALL   "BLOCKPGM"  USING LK-BLOCKPGM.
           CANCEL "BLOCKPGM".

