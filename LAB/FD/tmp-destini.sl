      *** Riempimento di TUTTI i destini per un codice GDO (utilizzato per i promo locali) **
      *o
       SELECT tmp-destini
           ASSIGN       TO  path-tmp-destini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-destini
           RECORD KEY   IS tmd-chiave
           ALTERNATE RECORD KEY IS k-cli-ragsoc = tmd-cli-ragsoc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-des-ragsoc = tmd-des-ragsoc
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-des-localita = tmd-localita
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-des-indirizzo = tmd-indirizzo
           WITH DUPLICATES 
      *** Serve per posizionarmi correttamente su zoom-gt **
      *m
      * 
           ALTERNATE RECORD KEY IS k-des-ord-zoom = tmd-des-ragsoc, 
           tmd-localita, tmd-indirizzo
           WITH DUPLICATES .
