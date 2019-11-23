       SELECT rlistini
           ASSIGN       TO  "rlistini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rlistini
           RECORD KEY   IS rlis-chiave OF rlistini
           ALTERNATE RECORD KEY IS rlis-k-art of rlistini = 
           rlis-articolo OF rlistini, rlis-chiave-ricerca OF rlistini
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rlis-k-descr of rlistini = 
           rlis-codice OF rlistini, rlis-des-libera OF rlistini
           WITH DUPLICATES .
