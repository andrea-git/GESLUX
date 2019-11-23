       SELECT btnotacr
           ASSIGN       TO  "btnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-btnotacr
           RECORD KEY   IS btno-chiave
           ALTERNATE RECORD KEY IS k1 = btno-cod-cli, btno-prg-destino, 
           btno-anno, btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = btno-data, btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = btno-anno-fatt, 
           btno-num-fatt
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-reso = btno-anno, btno-num-reso
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = btno-anno-bolla, 
           btno-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-vettore = btno-vettore, btno-anno, 
           btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-nota = btno-anno-nc, btno-num-nc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fatman = btno-anno-fm, btno-num-fm
           WITH DUPLICATES .
