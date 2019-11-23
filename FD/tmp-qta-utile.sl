      ** Usato dall'evasione clienti per fare i calcoli sulla quantità da assegnare
       SELECT tmp-qta-utile
           ASSIGN       TO  path-tmp-qta-utile
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-qta-utile
           RECORD KEY   IS tqu-chiave.
