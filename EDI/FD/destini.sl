       SELECT destini
           ASSIGN       TO  "destini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destini
           RECORD KEY   IS des-chiave
           ALTERNATE RECORD KEY IS K1 = des-ragsoc-1, des-codice, 
           des-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-localita = des-localita
           WITH DUPLICATES .
