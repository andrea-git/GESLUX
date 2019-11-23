      ** Sommatoria degli articoli usati con quantità, importo, numero righe. Situazione appena PRIMA di caricare la griglia *
      *
       SELECT chec-rordini2
           ASSIGN       TO  path-check-rordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-check-rordini
           RECORD KEY   IS cror-chiave.
