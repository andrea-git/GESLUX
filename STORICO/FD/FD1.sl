       SELECT FD1
           ASSIGN       TO  path-sto-EDI-mrordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-sto-EDI-mrordini
           RECORD KEY   IS sto-emro-chiave
           ALTERNATE RECORD KEY IS emro-k-articolo = 
           sto-emro-cod-articolo, sto-emro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS emro-k-stato = sto-emro-stato, 
           sto-emro-chiave
           WITH DUPLICATES .
