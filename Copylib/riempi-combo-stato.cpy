      ***---
       RIEMPI-COMBO-STATO.
           Modify cbo-stato,  item-to-add "Disattivo".
           Modify cbo-stato,  item-to-add "Attivo".
           Modify cbo-stato,  item-to-add "Bloccato".
           
      ***---
       CARICA-COMBO-STATO.
           evaluate true
           when disattivo     move "Disattivo"  to cbo-stato-buf
           when attivo        move "Attivo"     to cbo-stato-buf
           when bloccato      move "Bloccato"   to cbo-stato-buf
           end-evaluate.
           modify cbo-stato,  value cbo-stato-buf.
           
      ***---
       SCARICA-COMBO-STATO.     
           inquire cbo-stato, value cbo-stato-buf.
           evaluate cbo-stato-buf               
           when "Disattivo"   set disattivo       to true
           when "Attivo"      set attivo          to true              
           when "Bloccato"    set bloccato        to true             
           end-evaluate.  
