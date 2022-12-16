       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ql.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                   

       working-storage section.        
       77  nomepgm pic x(20).      
       copy "common-linkage.def".

       procedure division chaining nomepgm.
           move nomepgm to LK-BL-PROG-ID.
           accept LK-BL-DATA from century-date.
           accept lK-BL-ORA  from time
           set LK-BL-CANCELLAZIONE to true
           move 0 to LK-BL-USO LK-HND-WIN
           move "BOSS" to USER-CODI.
           move 3      to livello-abil.

           call nomepgm using lk-blockpgm, user-codi, livello-abil.

           goback.
