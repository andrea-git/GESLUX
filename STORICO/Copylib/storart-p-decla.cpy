      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "39"
                set errori to true
                display message "File [RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "39"
                set errori to true
                display message "File [MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [MTORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                                       
      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "39"
                set errori to true
                display message "File [MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.     
          
      ***---
       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-trasporti
           when "39"
                set errori to true
                display message "File [TRASPORTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TRASPORTI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TRASPORTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.    
           
      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
           
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                 
               
      ***---
       GIORMAG-ERR SECTION.
           use after error procedure on giormag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-giormag
           when "39"
                set errori to true
                display message "File [GIORMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[GIORMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [GIORMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
           
           
      ***---
       TEDI-ERR SECTION.
           use after error procedure on tedi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tedi
           when "39"
                set errori to true
                display message "File [TEDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TEDI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TEDI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
                                           
           
      ***---
       REDI-ERR SECTION.
           use after error procedure on redi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-redi
           when "39"
                set errori to true
                display message "File [REDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[REDI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [REDI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.        
           
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "39"
                set errori to true
                display message "File [TNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.     
           
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "39"
                set errori to true
                display message "File [RNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.         
           
      ***---
       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-movutf
           when "39"
                set errori to true
                display message "File [MOVUTF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MOVUTF] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [MOVUTF] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.               
           
      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "39"
                set errori to true
                display message "File [TORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
           
      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "39"
                set errori to true
                display message "File [RORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.         

      ***---
       NORDFORN-ERR SECTION.
           use after error procedure on nordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-nordforn
           when "39"
                set errori to true
                display message "File [NORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[NORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [NORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
           
      ***---
       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-provvig
           when "39"
                set errori to true
                display message "File [PROVVIG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROVVIG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [PROVVIG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
           
      ***---
       TEVA-ERR SECTION.
           use after error procedure on teva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-teva
           when "39"
                set errori to true
                display message "File [TEVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TEVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TEVA] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       REVA-ERR SECTION.
           use after error procedure on reva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-reva
           when "39"
                set errori to true
                display message "File [REVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[REVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [REVA] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       BTNOTACR-ERR SECTION.
           use after error procedure on btnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-btnotacr
           when "39"
                set errori to true
                display message "File [BTNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[BTNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [BTNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       BRNOTACR-ERR SECTION.
           use after error procedure on brnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-brnotacr
           when "39"
                set errori to true
                display message "File [BRNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[BRNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [BRNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                   
           
      ***---
       SORDFORN-ERR SECTION.
           use after error procedure on sordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sordforn
           when "39"
                set errori to true
                display message "File [SORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[SORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [SORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                   
           
      ***---
       CONTESTAZIONI-ERR SECTION.
           use after error procedure on contestazioni.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-contestazioni
           when "39"
                set errori to true
                display message "File [CONTESTAZIONI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CONTESTAZIONI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [CONTESTAZIONI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                
           
      ***---
       TAGLI-ERR SECTION.
           use after error procedure on tagli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tagli
           when "39"
                set errori to true
                display message "File [TAGLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TAGLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TAGLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                            
           
      ***---
       EDI-MTORDINI-ERR SECTION.
           use after error procedure on edi-mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-edi-mtordini
           when "39"
                set errori to true
                display message "File [EDI-MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                        "[EDI-MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                          
           
      ***---
       EDI-MRORDINI-ERR SECTION.
           use after error procedure on edi-mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-edi-mrordini
           when "39"
                set errori to true
                display message "File [EDI-MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                        "[EDI-MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       STO-TORDINI-ERR SECTION.
           use after error procedure on sto-tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-tordini
           when "39"
                set errori to true
                display message "File [STO-TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       STO-RORDINI-ERR SECTION.
           use after error procedure on sto-rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-rordini
           when "39"
                 CALL "C$RERR"
                      USING EXTEND-STAT, TEXT-MESSAGE
                set errori to true
                display message "File [STO-RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.        

      ***---
       STO-MTORDINI-ERR SECTION.
           use after error procedure on sto-mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-mtordini
           when "39"
                set errori to true
                display message "File [STO-MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   

      ***---
       STO-MRORDINI-ERR SECTION.
           use after error procedure on sto-mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-mrordini
           when "39"
                set errori to true
                display message "File [STO-MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.     

      ***---
       STO-TRASPORTI-ERR SECTION.
           use after error procedure on sto-trasporti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-trasporti
           when "39"
                set errori to true
                display message "File [STO-TRASPORTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TRASPORTI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                

      ***---
       STO-TMOVMAG-ERR SECTION.
           use after error procedure on sto-tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-tmovmag
           when "39"
                set errori to true
                display message "File [STO-TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   

      ***---
       STO-RMOVMAG-ERR SECTION.
           use after error procedure on sto-rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-sto-rmovmag
           when "39"
                set errori to true
                display message "File [STO-RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                 
               
      ***---
       STO-GIORMAG-ERR SECTION.
           use after error procedure on STO-giormag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-giormag
           when "39"
                set errori to true
                display message "File [STO-GIORMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-GIORMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
           
           
      ***---
       STO-TEDI-ERR SECTION.
           use after error procedure on STO-tedi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-tedi
           when "39"
                set errori to true
                display message "File [STO-TEDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TEDI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
                                           
           
      ***---
       STO-REDI-ERR SECTION.
           use after error procedure on STO-redi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-redi
           when "39"
                set errori to true
                display message "File [STO-REDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-REDI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.        
           
      ***---
       STO-TNOTACR-ERR SECTION.
           use after error procedure on STO-tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-tnotacr
           when "39"
                set errori to true
                display message "File [STO-TNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.     
           
      ***---
       STO-RNOTACR-ERR SECTION.
           use after error procedure on STO-rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-rnotacr
           when "39"
                set errori to true
                display message "File [STO-RNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.         
           
      ***---
       STO-MOVUTF-ERR SECTION.
           use after error procedure on STO-movutf.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-movutf
           when "39"
                set errori to true
                display message "File [STO-MOVUTF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-MOVUTF] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.               
           
      ***---
       STO-TORDFORN-ERR SECTION.
           use after error procedure on STO-tordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-tordforn
           when "39"
                set errori to true
                display message "File [STO-TORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
           
      ***---
       STO-RORDFORN-ERR SECTION.
           use after error procedure on STO-rordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-rordforn
           when "39"
                set errori to true    
                display message "File [STO-RORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-RORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.         

      ***---
       STO-NORDFORN-ERR SECTION.
           use after error procedure on STO-nordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-nordforn
           when "39"
                set errori to true
                display message "File [STO-NORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-NORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
           
      ***---
       STO-PROVVIG-ERR SECTION.
           use after error procedure on STO-provvig.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-provvig
           when "39"
                set errori to true
                display message "File [STO-PROVVIG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-PROVVIG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
           
      ***---
       STO-TEVA-ERR SECTION.
           use after error procedure on STO-teva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-teva
           when "39"
                set errori to true
                display message "File [STO-TEVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TEVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       STO-REVA-ERR SECTION.
           use after error procedure on STO-reva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-reva
           when "39"
                set errori to true
                display message "File [STO-REVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-REVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       STO-BTNOTACR-ERR SECTION.
           use after error procedure on STO-btnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-btnotacr
           when "39"
                set errori to true     
                display message "File [STO-BTNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-BTNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                   
           
      ***---
       STO-BRNOTACR-ERR SECTION.
           use after error procedure on STO-brnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-brnotacr
           when "39"
                set errori to true     
                display message "File [STO-BRNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-BRNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                   
           
      ***---
       STO-SORDFORN-ERR SECTION.
           use after error procedure on STO-sordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-sordforn
           when "39"
                set errori to true
                display message "File [STO-SORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-SORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                   
           
      ***---
       STO-CONTESTAZIONI-ERR SECTION.
           use after error procedure on STO-contestazioni.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-contestazioni
           when "39"
                set errori to true
                display message 
                        "File [STO-CONTESTAZIONI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                        "[STO-CONTESTAZIONI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"    
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                
           
      ***---
       STO-TAGLI-ERR SECTION.
           use after error procedure on STO-tagli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-tagli
           when "39"
                set errori to true
                display message "File [STO-TAGLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STO-TAGLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                
           
      ***---
       STO-EDI-MTORDINI-ERR SECTION.
           use after error procedure on STO-edi-mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-edi-mtordini
           when "39"
                set errori to true
                display message "File [STO-EDI-MTORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                        "[STO-EDI-MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                                
           
      ***---
       STO-EDI-MRORDINI-ERR SECTION.
           use after error procedure on STO-edi-mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-STO-edi-mrordini
           when "39"
                set errori to true
                display message "File [STO-EDI-MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message 
                        "[STO-EDI-MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"               
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.    
