      ** Evasione Clienti: contiene la somma di tutte le quanti� prenotate nei diversi listini per lo stesso articolo, la quanti� max e la giacenza utile da scalare
       SELECT tmp-qta-eva
           ASSIGN       TO  path-tmp-qta-eva
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-qta-eva
           RECORD KEY   IS tqe-chiave.
