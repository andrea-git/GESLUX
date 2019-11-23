      ** Testa e righe tutte su un file unico *
      *A
      *
       SELECT tmp-eva
           ASSIGN       TO  path-tmp-eva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-eva
           RECORD KEY   IS tmp-eva-chiave
      *Per sapere se devo scrivere qualcosa
      *
      *
           ALTERNATE RECORD KEY IS k-prog = tmp-eva-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-qta = tmp-eva-qta
           WITH DUPLICATES .
