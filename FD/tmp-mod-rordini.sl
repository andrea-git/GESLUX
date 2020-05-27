      ** Usato per inviare la mail di notifica in caso di variazione di un ordine SHI/GET
      *O
       SELECT tmp-mod-rordini
           ASSIGN       TO  path-tmp-mod-rordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tmp-mod-rordini
           RECORD KEY   IS tmp-mror-chiave OF tmp-mod-rordini
           ALTERNATE RECORD KEY IS tmp-mror-k-tipo = tmp-mror-mod OF 
           tmp-mod-rordini, tmp-mror-chiave OF tmp-mod-rordini
           WITH DUPLICATES .
