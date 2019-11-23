      ***---
       RIEMPI-COMBO-STATO-NOTA.
           Modify cbo-stato-nota, item-to-add 78-NCNC.
           Modify cbo-stato-nota, item-to-add 78-NCPZ.
           Modify cbo-stato-nota, item-to-add 78-NCRE.
           Modify cbo-stato-nota, item-to-add 78-TUTTE.

      ***---
       CARICA-COMBO-STATO-NOTA.
           evaluate true
           when NCNC  move 78-NCNC  to cbo-stato-nota-buf
           when NCPZ  move 78-NCPZ  to cbo-stato-nota-buf
           when NCRE  move 78-NCRE  to cbo-stato-nota-buf
           when TUTTE move 78-TUTTE to cbo-stato-nota-buf
           end-evaluate.
           modify cbo-stato-nota,   value cbo-stato-nota-buf.
           
      ***---
       SCARICA-COMBO-STATO-NOTA.     
           inquire cbo-stato-nota, value cbo-stato-nota-buf.
           evaluate cbo-stato-nota-buf
           when 78-NCNC  set NCNC   to true
           when 78-NCPZ  set NCPZ   to true
           when 78-NCRE  set NCRE   to true
           when 78-TUTTE set TUTTE  to true
           end-evaluate.
