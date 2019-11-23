       SELECT STO-btnotacr
           ASSIGN       TO  "btnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-sto-btnotacr
           RECORD KEY   IS STO-btno-chiave
           ALTERNATE RECORD KEY IS k1 = STO-btno-cod-cli, 
           STO-btno-prg-destino, STO-btno-anno, STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = STO-btno-data, 
           STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = STO-btno-anno-fatt, 
           STO-btno-num-fatt
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-reso = STO-btno-anno, 
           STO-btno-num-reso
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = STO-btno-anno-bolla, 
           STO-btno-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-vettore = STO-btno-vettore, 
           STO-btno-anno, STO-btno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-nota = STO-btno-anno-nc, 
           STO-btno-num-nc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fatman = STO-btno-anno-fm, 
           STO-btno-num-fm
           WITH DUPLICATES .
