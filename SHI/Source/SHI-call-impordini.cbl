       program-id.                      SHI-call-impordini.
       author.                          Andrea.
       remarks. Richiamo stralcio SHI in notturna.
       special-names. decimal-point is comma.

       working-storage section.
      * VARIABILI DI CHAINING
       copy "common-linkage.def".

       procedure division.
       MAIN.
           move "desktop"    to LK-BL-PROG-ID.
           accept LK-BL-DATA from century-date.
           accept LK-BL-ORA  from time.
           move "BATCH"      to USER-CODI.
           move 1            to LIVELLO-ABIL.

           call   "SHI-imp" using LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL
           cancel "SHI-imp".

           call   "ricalimp-bat".
           cancel "ricalimp-bat".
           goback.
