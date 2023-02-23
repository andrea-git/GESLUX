       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rand.

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                          

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         

       WORKING-STORAGE SECTION.
       77  n   pic 99.
       78  val value 10.
       

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           perform 2000 times
              compute n = function random * (val) |da 0 a [val -1]
              display n
           end-perform.
           stop "K"
           goback.
