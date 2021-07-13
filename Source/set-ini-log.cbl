       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-ini-log.
       AUTHOR.                          Andrea.
       REMARKS.
           Usato da :
           - macrobatch

           solamente per settare l'inizio della riga con data e ora
           Nessun input, solo output della stringa ottenuta

      ******************************************************************
       SPECIAL-NAMES. decimal-point is comma.
       
       WORKING-STORAGE      SECTION.
       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data-i.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler              pic x(2)  value "] ".
         
       77  como-data            pic 9(8).
       77  como-ora             pic 9(8).

       LINKAGE SECTION.
       77  r-output             pic x(25).

      ******************************************************************
       PROCEDURE DIVISION USING r-output.
       MAIN.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa.
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

           move r-inizio to r-output.

           goback.
