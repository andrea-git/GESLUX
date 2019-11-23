       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      r_finemese.
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

       77  como-data     pic 9(8).
       77  como-anno     pic 9(4).
       77  como-mese     pic 9(2).
       77  como-giorno   pic 9(2).
       77  b             pic 9(4).
       77  c             pic 9(4).
       77  ws-fine-mese  pic 9(8).

       LINKAGE SECTION.          
       77  link-data                   unsigned-int.

      ******************************************************************
       PROCEDURE DIVISION USING link-data. 

       MAIN-PRG.
           move link-data      to como-data.
           if como-data = 0
              accept como-data from century-date
           end-if.
           move como-data(5:2) to como-mese.
           move como-data(1:4) to como-anno.
           evaluate como-mese
           when 01 move 31 to como-giorno
           when 02
                compute b = como-anno / 4
                compute c = b * 4
                if c = como-anno
                   move 29 to como-giorno
                else
                   move 28 to como-giorno
                end-if
           when 03 move 31 to como-giorno
           when 04 move 30 to como-giorno
           when 05 move 31 to como-giorno
           when 06 move 30 to como-giorno
           when 07 move 31 to como-giorno
           when 08 move 31 to como-giorno
           when 09 move 30 to como-giorno
           when 10 move 31 to como-giorno
           when 11 move 30 to como-giorno
           when 12 move 31 to como-giorno
           end-evaluate.
           string como-anno   delimited size
                  como-mese   delimited size
                  como-giorno delimited size
             into como-data
           end-string.
           move como-data to ws-fine-mese. 
           goback giving ws-fine-mese.
