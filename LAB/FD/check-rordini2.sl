      ** Sommatoria degli articoli usati con quantità, importo, numero righe. Situazione appena DOPO il salvataggio di una modifica. Usato in st-ordine *
      *
      *F
       SELECT check-rordini2
           ASSIGN       TO  path-check-rordini2
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-check-rordini2
           RECORD KEY   IS cror-chiave2.
