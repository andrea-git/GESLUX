      ***---
       RIEMPI-COMBO-INVIO.
           Modify cbo-invio,        item-to-add "Manuale".
      *****     Modify cbo-invio,        item-to-add "Postel".
           Modify cbo-invio,        item-to-add "EDI".
           Modify cbo-invio,        item-to-add "Nessuno".
           
      ***---
       CARICA-COMBO-INVIO.
           evaluate true            
           when invio-manuale move "Manuale"  to cbo-invio-buf
           when invio-EDI     move "EDI"      to cbo-invio-buf
           when invio-nessuno move "Nessuno"  to cbo-invio-buf
      *****     when invio-postel  move "Postel"   to cbo-invio-buf
           end-evaluate.
           modify cbo-invio,      value cbo-invio-buf.
           
      ***---
       SCARICA-COMBO-INVIO.     
           inquire cbo-invio,      value cbo-invio-buf.
           evaluate cbo-invio-buf
           when "Manuale"       set invio-manuale       to true
           when "EDI"           set invio-EDI           to true
           when "Nessuno"       set invio-nessuno       to true
      *****     when "Postel"        set invio-postel        to true
           end-evaluate.  
