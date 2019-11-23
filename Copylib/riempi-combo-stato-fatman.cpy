      ***---
       RIEMPI-COMBO-STATO-FATMAN.                       
           Modify cbo-stato-fatman, item-to-add 78-FTTR.
           Modify cbo-stato-fatman, item-to-add 78-FTND.
           Modify cbo-stato-fatman, item-to-add 78-FTMA.
           Modify cbo-stato-fatman, item-to-add 78-TUTTE.

      ***---
       CARICA-COMBO-STATO-FATMAN.
           evaluate true
           when FTMA  move 78-FTMA     to  cbo-stato-fatman-buf
           when FTND  move 78-FTND     to  cbo-stato-fatman-buf
           when FTTR  move 78-FTTR     to  cbo-stato-fatman-buf
           when TUTTE move 78-TUTTE    to  cbo-stato-fatman-buf
           end-evaluate.
           modify cbo-stato-fatman, value  cbo-stato-fatman-buf.
           
      ***---
       SCARICA-COMBO-STATO-FATMAN.     
           inquire cbo-stato-fatman, value cbo-stato-fatman-buf.
           evaluate cbo-stato-fatman-buf
           when 78-FTND  set FTND   to true
           when 78-FTMA  set FTMA   to true
           when 78-FTTR  set FTTR   to true
           when 78-TUTTE set TUTTE  to true
           end-evaluate.                   
