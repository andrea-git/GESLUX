      *Contiene tendenza e scostamento (delta) così come espresso nella stampa "delta". Serve per conservare i valori per future consultazioni senza dover effettuare di nuovo il ricalcolo
      *F
       SELECT tendelta
           ASSIGN       TO  "tendelta"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tendelta
           RECORD KEY   IS ten-chiave.
