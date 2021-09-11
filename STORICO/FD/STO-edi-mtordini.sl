       SELECT STO-edi-mtordini
           ASSIGN       TO  path-sto-EDI-mtordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-STO-EDI-mtordini
           RECORD KEY   IS sto-emto-chiave
           ALTERNATE RECORD KEY IS emto-k-ord-cli = sto-emto-anno, 
           sto-emto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-stato = sto-emto-stato, 
           sto-emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-data = sto-emto-data-ordine, 
           sto-emto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-emto-clides = sto-emto-cod-cli, 
           sto-emto-prg-destino, sto-emto-chiave
           WITH DUPLICATES .
