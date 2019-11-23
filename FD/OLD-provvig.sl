       SELECT OLD-provvig
           ASSIGN       TO  "OLD-provvig"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-provvig
           RECORD KEY   IS OLD-pvv-chiave
           ALTERNATE RECORD KEY IS k-data-fat = OLD-pvv-data-fat, 
           OLD-pvv-agente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente = OLD-pvv-agente, 
           OLD-pvv-anno-fat, 
           OLD-pvv-num-fat, OLD-pvv-riga-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-marca = OLD-pvv-agente, 
           OLD-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = OLD-pvv-data-liq, 
           OLD-pvv-data-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cliente = OLD-pvv-cliente, 
           OLD-pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente-data = OLD-pvv-agente, 
           OLD-pvv-data-fat
           WITH DUPLICATES .
