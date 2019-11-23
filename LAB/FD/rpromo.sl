       SELECT rpromo
           ASSIGN       TO  "rpromo"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rpromo
           RECORD KEY   IS rpr-chiave
           ALTERNATE RECORD KEY IS k-stampa = rpr-codice, 
           rpr-data-creazione, rpr-ora-creazione
           WITH DUPLICATES .
