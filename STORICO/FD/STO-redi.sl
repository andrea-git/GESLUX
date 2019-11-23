       SELECT STO-redi
           ASSIGN       TO  path-sto-redi
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-sto-redi
           RECORD KEY   IS STO-redi-chiave of STO-redi
           ALTERNATE RECORD KEY IS STO-redi-k-tipo-mov of STO-redi = 
           STO-redi-tedi-chiave of STO-redi, STO-redi-tipo-movim of 
           STO-redi, STO-redi-chiave-mov of STO-redi
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-redi-k-dt-mov of STO-redi = 
           STO-redi-tedi-chiave of STO-redi, STO-redi-tipo-movim of 
           STO-redi, STO-redi-data-movimento of STO-redi, 
           STO-redi-tmo-chiave of STO-redi
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS STO-redi-tmo-chiave of STO-redi
           WITH DUPLICATES .
