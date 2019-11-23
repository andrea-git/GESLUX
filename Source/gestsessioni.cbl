       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      gestsessioni.

      ******************************************************************
       ENVIRONMENT DIVISION.

       WORKING-STORAGE SECTION.
           copy "link-gpgmexe.def".
           copy "comune.def".
      * 78 titolo1 VALUE IS "Geslux - Menu Principale". 

       LINKAGE SECTION.
       77  session-id  pic x(50).
           copy "common-linkage.def".

      ******************************************************************
       PROCEDURE DIVISION using LK-BLOCKPGM,
                                USER-CODI,
                                LIVELLO-ABIL.
       
       Main Section.
      *    come prima cosa verifico che non ci siano blocchi al lancio 
      *    dei programmi, attualmente solamente EVACLI
           move LK-BL-PROG-ID   to pgme-pgm
           move user-codi       to pgme-utente
           set pgme-ingresso    to true

           move user-codi to pgme-utente
           set pgme-cancellazione  to true

      *     05 pgme-stato              pic x.
      *        88 pgme-ok              value "O".
      *        88 pgme-evacli-attivo   value "E".
      *        88 pgme-pgm-attivi      value "A".
      *
      *
           set environment "BLOCK_GESTSESSIONI" to LK-BLOCKPGM


           call "gpgmexe" using gpgmexe-linkage
                  on exception display message "Pgm. " "gpgmexe"
                                               " inesistente!"
                                       title tit-err
           end-call.              
           cancel "gpgmexe".

           goback.
