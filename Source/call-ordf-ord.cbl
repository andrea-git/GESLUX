       program-id.                      call-ordf-ord.
       author.                          Andrea.
       special-names. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.  
       77  a pic 9(8).
       77  b pic 9(8).
                          
       PROCEDURE DIVISION.  stop "K"
           call   "ordf-ord" using 2024 a b
           cancel "ordf-ord".
           goback.
