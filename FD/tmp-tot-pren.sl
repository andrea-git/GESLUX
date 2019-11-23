      ** Evasione Clienti: contiene la somma di tutte le quantià prenotate nei diversi listini per lo stesso articolo
       SELECT tmp-tot-pren
           ASSIGN       TO  path-tmp-tot-pren
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tot-pren
           RECORD KEY   IS ttp-chiave.
