       SELECT STO-contestazioni
           ASSIGN       TO  path-sto-contestazioni
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-contestazioni
           RECORD KEY   IS STO-cnt-chiave
           ALTERNATE RECORD KEY IS k1 = STO-cnt-cod-cli, 
           STO-cnt-prg-destino, STO-cnt-tipo, STO-cnt-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = STO-cnt-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-nota = STO-cnt-nota-cr
           WITH DUPLICATES .
