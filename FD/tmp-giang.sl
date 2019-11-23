       SELECT tmp-giang
           ASSIGN       TO  path-tmp-giang
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-giang
           RECORD KEY   IS tgia-chiave
           ALTERNATE RECORD KEY IS tgia-k-articolo = tgia-tipo-rec, 
           tgia-cod-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tgia-k-numero = tgia-tipo-rec, 
           tgia-testa-ordf, tgia-cod-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tgia-k-forn = tgia-tipo-rec, 
           tgia-ragsoc, tgia-cod-articolo
           WITH DUPLICATES .
