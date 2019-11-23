      ***---
       RIEMPI-COMBO-STATO.
           Modify cbo-stato,  item-to-add "Attivo".
           Modify cbo-stato,  item-to-add "Bloccato".
           Modify cbo-stato,  item-to-add "Chiuso".
           
      ***---
       CARICA-COMBO-STATO.
           evaluate true
           when attivo         move "Attivo"    to cbo-stato-buf
           when bloccato       move "Bloccato"  to cbo-stato-buf
           when chiuso         move "Chiuso"    to cbo-stato-buf
           end-evaluate.
           modify cbo-stato,  value cbo-stato-buf.
           
      ***---
       SCARICA-COMBO-STATO.     
           inquire cbo-stato, value cbo-stato-buf.
           evaluate cbo-stato-buf               
           when "Attivo"      set attivo        to true
           when "Bloccato"    set bloccato      to true
           when "Chiuso"      set chiuso        to true
           end-evaluate.  
