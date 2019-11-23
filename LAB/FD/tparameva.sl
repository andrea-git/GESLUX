      ** File contenente i parametri che utilizzerà l'evasione
       SELECT tparameva
           ASSIGN       TO  "tparameva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tparameva
           RECORD KEY   IS tpa-chiave
           ALTERNATE RECORD KEY IS tpa-descrizione
           WITH DUPLICATES .
