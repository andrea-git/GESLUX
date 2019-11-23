      ***--- 
      * Controlla il formato della data
       DATE-FORMAT. 
           set gg-mm-aaaa to true.
           call "utydata" using test-validita,
                                formato-data,
                                como-data
           end-call.

      ***---
      * Adatta il formato della data per il file (After buf-to-fld).
       DATE-TO-FILE.              
           call "utydata" using conversione,
                                5,
                                como-data,
                                1,
                                como-data
           end-call.

      ***---
      * Adatta il formato della data per la screen (After fld-to-buf).
       DATE-TO-SCREEN.            
           call "utydata" using conversione,
                                1,
                                como-data,
                                5,
                                como-data
           end-call.
