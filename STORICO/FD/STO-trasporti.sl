       SELECT STO-trasporti
           ASSIGN       TO  PATH-STO-TRASPORTI
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-STO-trasporti
           RECORD KEY   IS STO-trs-chiave
           ALTERNATE RECORD KEY IS k-data-bolla = STO-trs-data-bolla, 
           STO-trs-num-bolla, STO-trs-prog-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-fattura = 
           STO-trs-data-fattura, STO-trs-data-bolla, STO-trs-num-bolla, 
           STO-trs-prog-bolla
           WITH DUPLICATES .
