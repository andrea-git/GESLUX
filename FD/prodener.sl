       SELECT prodener
           ASSIGN       TO  "prodener"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-prodener
           RECORD KEY   IS pen-chiave OF prodener
           ALTERNATE RECORD KEY IS pen-descrizione OF prodener
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS pen-k-nc of prodener = pen-nc OF 
           prodener, pen-cpa OF prodener, pen-taric OF prodener, 
           pen-dac OF prodener
           WITH DUPLICATES .
