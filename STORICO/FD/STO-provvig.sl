       SELECT STO-provvig
           ASSIGN       TO  path-sto-provvig
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-provvig
           RECORD KEY   IS STO-pvv-chiave
           ALTERNATE RECORD KEY IS k-data-fat = STO-pvv-data-fat, 
           STO-pvv-agente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente = STO-pvv-agente, 
           STO-pvv-anno-fat, STO-pvv-num-fat, STO-pvv-riga-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-marca = STO-pvv-agente, 
           STO-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = STO-pvv-data-liq, 
           STO-pvv-data-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cliente = STO-pvv-cliente, 
           STO-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente-data = STO-pvv-agente, 
           STO-pvv-data-fat
           WITH DUPLICATES .
