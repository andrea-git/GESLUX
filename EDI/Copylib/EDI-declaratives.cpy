      ***---
       PRODENER-ERR SECTION.
           use after error procedure on prodener.
           evaluate status-prodener
           when "35" continue
           when "39"
                display message "File [PRODENER] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PRODENER] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       TEDI-ERR SECTION.
           use after error procedure on tedi.
           evaluate status-tedi
           when "35" continue
           when "39"
                display message "File [TEDI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TEDI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       REDI-ERR SECTION.
           use after error procedure on redi.
           evaluate status-redi
           when "35" continue
           when "39"
                display message "File [REDI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[REDI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       PARAMEDI-ERR SECTION.
           use after error procedure on paramedi.
           evaluate status-paramedi
           when "35" continue
           when "39"
                display message "File [PARAMEDI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAMEDI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.                     

      ***---
       EDI-PARAM-ERR SECTION.
           use after error procedure on edi-param.
           evaluate status-edi-param
           when "35" continue
           when "39"
                display message "File [EDI-PARAM] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EDI-PARAM] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.


      ***---
       EDI-TIVA-ERR SECTION.
           use after error procedure on edi-tiva.
           evaluate status-edi-tiva
           when "35" continue
           when "39"
                display message "File [EDI-TIVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EDI-TIVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       EDI-CLIDES-ERR SECTION.
           use after error procedure on edi-clides.
           evaluate status-edi-clides
           when "35" continue
           when "39"
                display message "File [EDI-CLIDES] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EDI-CLIDES] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       EDI-MTORDINI-ERR SECTION.
           use after error procedure on edi-mtordini.
           evaluate status-edi-mtordini
           when "35" continue
           when "39"
                display message "File [EDI-MTORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EDI-MTORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.

      ***---
       EDI-MRORDINI-ERR SECTION.
           use after error procedure on edi-mrordini.
           evaluate status-edi-mrordini
           when "35" continue
           when "39"
                display message "File [EDI-MRORDINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[EDI-MRORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.
