       SELECT STO-tnotacr
           ASSIGN       TO  path-sto-tnotacr
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH LOCK ON MULTIPLE RECORDS 
           FILE STATUS  IS STATUS-sto-tnotacr
           RECORD KEY   IS STO-tno-chiave
           ALTERNATE RECORD KEY IS k-causale = STO-tno-causale, 
           STO-tno-anno, STO-tno-numero
           ALTERNATE RECORD KEY IS k1 = STO-tno-cod-cli, 
           STO-tno-prg-destino, STO-tno-anno, STO-tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 = STO-tno-data-passaggio-ordine, 
           STO-tno-anno, STO-tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = STO-tno-anno-fattura, 
           STO-tno-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = STO-tno-anno-fattura, 
           STO-tno-data-fattura, STO-tno-num-fattura, 
           STO-tno-num-prenot, STO-tno-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = STO-tno-agg-contab, 
           STO-tno-anno-fattura, STO-tno-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = STO-tno-data, STO-tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = STO-tno-anno-fattura, 
           STO-tno-data-fattura, STO-tno-num-fattura, 
           STO-tno-num-prenot, STO-tno-fatt-prenotata, STO-tno-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = 
           STO-tno-agg-contab, STO-tno-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = 
           STO-tno-cod-cli, STO-tno-agg-contab, STO-tno-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = 
           STO-tno-cod-cli, STO-tno-prg-destino, STO-tno-agg-contab, 
           STO-tno-data-fattura
           WITH DUPLICATES .
