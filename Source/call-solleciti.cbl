       program-id.                      call-solleciti.
       author.                          Andrea.
       remarks. Richiamo solleciti da desktop.
       special-names. decimal-point is comma.

       working-storage section.
      * VARIABILI DI CHAINING
       copy "common-linkage.def".


       procedure division.
       MAIN.
      *    luciano inizio: mod per gestione sessioni
           call "getpid"
           cancel "getpid"
      *    luciano fine

      *    luciano inizio: mod per gestione sessioni
      *     move "desktop"    to LK-BL-PROG-ID.
           move "solleciti"    to LK-BL-PROG-ID.
      *    luciano fine
           accept LK-BL-DATA from century-date.
           accept LK-BL-ORA  from time.
           move "desktop"    to USER-CODI.
           move 1            to LIVELLO-ABIL.

           call   "colors".
           cancel "colors".

      *    luciano inizio: mod per gestione sessioni
      *     call   "solleciti" using LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL
      *     cancel "solleciti"
           call "lanciopgm" using LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL
           cancel "lanciopgm"
      *    luciano fine

           goback.
