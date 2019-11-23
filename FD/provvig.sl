       SELECT provvig
           ASSIGN       TO  "provvig"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-provvig
           RECORD KEY   IS pvv-chiave
           ALTERNATE RECORD KEY IS k-data-fat = pvv-data-fat, pvv-agente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente = pvv-agente, pvv-anno-fat, 
           pvv-num-fat, pvv-riga-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-marca = pvv-agente, pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = pvv-data-liq, pvv-data-fat
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-cliente = pvv-cliente, pvv-marca
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agente-data = pvv-agente, 
           pvv-data-fat
           WITH DUPLICATES .
