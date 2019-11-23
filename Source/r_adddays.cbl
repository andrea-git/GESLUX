       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      r_adddays.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  date-format   pic 9.  
         88 AAAAMMGG     value 1.
         88 AAMMGG       value 2.
       77  como-data     pic 9(8).
       77  como-anno     pic 9(4).
       77  AAAA          pic 9(4).

       LINKAGE SECTION.          
       copy "adddays.clk".

      ******************************************************************
       PROCEDURE DIVISION USING madd-link-area. 

       MAIN-PRG.
           if madd-in-date <= 999999
              set AAMMGG   to true
              move madd-in-date(1:4) to como-anno
              if como-anno > 50 
                 move 1900 to AAAA
              else
                 move 2000 to AAAA
              end-if
              add como-anno to AAAA
              move AAAA to como-anno
              move como-anno to madd-in-date(1:4)
           else
              set AAAAMMGG to true
           end-if.

           compute como-data =  function integer-of-date (madd-in-date).
           add madd-in-days to como-data.
           compute madd-out-date = function date-of-integer (como-data).

           if AAMMGG
              move 0 to madd-out-date(1:2)
           end-if.

           goback.
