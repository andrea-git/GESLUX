      ***---
       RIEMPI-COMBO-COLPA.
           modify cbo-colpa,  item-to-add "Cliente".
           modify cbo-colpa,  item-to-add "Corriere".
           modify cbo-colpa,  item-to-add "Fornitore".
           modify cbo-colpa,  item-to-add "Magazzino".
           modify cbo-colpa,  item-to-add "Ufficio".
           
      ***---
       CARICA-COMBO-COLPA.
           evaluate true
           when cliente     move "Cliente"   to cbo-colpa-buf
           when corriere    move "Corriere"  to cbo-colpa-buf
           when fornitore   move "Fornitore" to cbo-colpa-buf
           when magazzino   move "Magazzino" to cbo-colpa-buf
           when ufficio     move "Ufficio"   to cbo-colpa-buf
           end-evaluate.
           modify cbo-colpa,  value cbo-colpa-buf.
           
      ***---
       SCARICA-COMBO-COLPA.
           inquire cbo-colpa, value cbo-colpa-buf.
           evaluate cbo-colpa-buf
           when "Cliente"    set cliente    to true
           when "Corriere"   set corriere   to true
           when "Fornitore"  set fornitore  to true
           when "Magazzino"  set magazzino  to true
           when "Ufficio"    set ufficio    to true
           end-evaluate.
