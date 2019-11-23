      ** Sommatoria degli articoli usati con quantità, importo, numero righe. Situazione appena PRIMA di caricare la griglia. Usato in gordcvar e in st-ordine *
      *\
       SELECT check-rordini
           ASSIGN       TO  path-check-rordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-check-rordini
           RECORD KEY   IS cror-chiave.
