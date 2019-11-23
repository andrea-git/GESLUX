       SELECT OLD-STO-provvig
           ASSIGN       TO  path-OLD-STO-provvig
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-STO-provvig
           RECORD KEY   IS OLD-STO-pvv-chiave
           ALTERNATE RECORD KEY IS k-data-fat = OLD-STO-pvv-data-fat, 
           OLD-STO-pvv-agente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente = OLD-STO-pvv-agente, 
           OLD-STO-pvv-anno-fat, OLD-STO-pvv-num-fat, 
           OLD-STO-pvv-riga-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-marca = OLD-STO-pvv-agente, 
           OLD-STO-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = OLD-STO-pvv-data-liq, 
           OLD-STO-pvv-data-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cliente = OLD-STO-pvv-cliente, 
           OLD-STO-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente-data = OLD-STO-pvv-agente, 
           OLD-STO-pvv-data-fat
           WITH DUPLICATES .
