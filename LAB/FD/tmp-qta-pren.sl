      ** Usato dall'evasione clienti per fare i calcoli sulla quantità prenotata da assegnare
       SELECT tmp-qta-pren
           ASSIGN       TO  path-tmp-qta-pren
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-qta-pren
           RECORD KEY   IS tqp-chiave.
