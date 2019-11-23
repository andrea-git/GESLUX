       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      nome-pgm.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                          

       working-storage section.               
       linkage section.
       77  NomePgm   pic x(30).

       procedure division using NomePgm.

      ***---
       MAIN.
           call "C$CALLEDBY" using NomePgm.
           goback.
