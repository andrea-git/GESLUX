       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      call-caltras.
       AUTHOR.                          Andrea.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION. 
       77  tot-mov-from-tras      pic 9(5) value 0.
       77  ult-num-mov            pic 9(8) value 0.
       77  link-user              pic x(20) value "BOSS".
       77  link-result            pic 9 value 0.
       77  scr-oper-handle        handle of window.

      ******************************************************************
       PROCEDURE DIVISION.                                          
           call   "caltras" using tot-mov-from-tras
                                  ult-num-mov      
                                  link-user        
                                  link-result      
                                  scr-oper-handle.
           cancel "caltras".
           goback.
