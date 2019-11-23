      ** Collegamento tra cliente/destino e le evasioni da considerare. 
       SELECT evaclides
           ASSIGN       TO  "evaclides"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-evaclides
           RECORD KEY   IS ecd-chiave-OLD.
