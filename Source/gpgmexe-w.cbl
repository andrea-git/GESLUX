       program-id.                      gpgmexe-w.
       author.                          Andrea.
       remarks. 
           Wrapper per chiamata e gpgmexe da menu.
       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.  
       copy "link-gpgmexe.def".

       LINKAGE SECTION.
       copy "common-linkage.def".
                          
       PROCEDURE DIVISION USING LK-BLOCKPGM USER-CODI LIVELLO-ABIL.

       MAIN.      
           set  lk-bl-scrittura to true.
           move "gpgmexe-w" to lk-bl-prog-id
           move spaces      to lk-hnd-win.
           call   "blockpgm" using lk-blockpgm.
           cancel "blockpgm".
           move user-codi   to pgme-utente.
           set pgme-cancellazione  to true.
           call "gpgmexe" using gpgmexe-linkage
                  on exception continue
           end-call.
           cancel "gpgmexe".
           set lk-bl-cancellazione to true.
           move "gpgmexe-w"   to lk-bl-prog-id.
           call   "blockpgm" using lk-blockpgm.
           cancel "blockpgm".
           goback.
