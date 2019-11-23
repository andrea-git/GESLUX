       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      menuvc.
       AUTHOR.                          Andrea.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                   

      *****************************************************************
       DATA DIVISION.            
       FILE SECTION.                   

       WORKING-STORAGE SECTION.
      * VARIABILI DI CHAINING
       77 pgm-nome   pic x(20).
           copy "common-linkage.def".

       LINKAGE SECTION.
      ******************************************************************
       PROCEDURE DIVISION CHAINING pgm-nome user-codi livello-abil.

      ***---
       MAIN-PRG.                       
           move spaces to lk-blockpgm.
           call   pgm-nome using lk-blockpgm 
                                 user-codi 
                                 livello-abil.
           cancel pgm-nome.
           goback.
