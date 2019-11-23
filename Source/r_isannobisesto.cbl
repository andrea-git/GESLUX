       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      r_annobisesto.
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

       77  como-data      pic 9(8).
       77  como-anno      pic 9(4).
       77  b              pic 9(4).
       77  c              pic 9(4).
       77  isannobisesto  unsigned-int.

       LINKAGE SECTION.          
       77  link-data                   pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING link-data. 

       MAIN-PRG.
           move link-data      to como-data.
           if como-data = 0
              accept como-data from century-date
           end-if.
           move como-data(1:4) to como-anno.
           compute b = como-anno / 4
           compute c = b * 4
           if c = como-anno
              move 1 to isannobisesto
           else
              move 0 to isannobisesto
           end-if.
           goback giving isannobisesto.
