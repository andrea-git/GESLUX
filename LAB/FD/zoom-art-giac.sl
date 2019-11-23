      ** Zoom su articoli con giacenza LBX
       SELECT zoom-art-giac
           ASSIGN       TO  path-zoom-art-giac
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-zoom-art-giac
           RECORD KEY   IS zag-chiave
           ALTERNATE RECORD KEY IS zag-art-descrizione
           WITH DUPLICATES .
