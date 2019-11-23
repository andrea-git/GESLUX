      *stato invio mail su evasioni
       SELECT stato-invio
           ASSIGN       TO  "STATO-INVIO"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-stato-invio
           RECORD KEY   IS sto-chiave-ev
           ALTERNATE RECORD KEY IS sto-k-invio = sto-invio, 
           sto-chiave-ev
           WITH DUPLICATES .
