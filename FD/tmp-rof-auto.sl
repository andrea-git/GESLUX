      ** TMP righe ordini F automatici: controllo congruenza finale
       SELECT tmp-rof-auto
           ASSIGN       TO  path-tmp-rof-auto
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-rof-auto
           RECORD KEY   IS tra-chiave.
