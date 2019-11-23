       program-id.                      call-pordini.
       author.                          Andrea.
       remarks. Richiamo programma ordini in notturna.
       special-names. decimal-point is comma.

       working-storage section.
      * VARIABILI DI CHAINING
       copy "common-linkage.def".
       77  user-cod pic x(10).
       77  nargs    pic 99 comp-1.  

       01  filler           pic 9.
           88 RichiamoSchedulato    value 1, false 0.

       linkage section.
           copy "link-batch.def".

       procedure division using batch-linkage.
       MAIN.                                 
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.

           move "desktop"    to LK-BL-PROG-ID.
           accept LK-BL-DATA from century-date.
           accept LK-BL-ORA  from time.
           move "AUTOMATICI" to USER-CODI.
           move 1            to LIVELLO-ABIL.

           move "BOSS" to user-cod.
           set environment "USER_CODI" to user-cod.
           call   "pordini" using LK-BLOCKPGM, USER-CODI, LIVELLO-ABIL
           cancel "pordini".

           move spaces to user-cod.
           set environment "USER_CODI" to user-cod.

           if RichiamoSchedulato
              move  0 to batch-status
           end-if.

           goback.
