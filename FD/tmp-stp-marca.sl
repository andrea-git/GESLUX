       SELECT tmp-stp-marca
           ASSIGN       TO  path-tmp-stp-marca
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-stp-marca
           RECORD KEY   IS tmp-stp-chiave
           ALTERNATE RECORD KEY IS k-des-marca = tmp-stp-agente, 
           tmp-stp-mar-descrizione
           WITH DUPLICATES .
