      ** Usato nei solleciti per aprire la lista di ordini fornitore aperti per un determinato articolo
      *2
       SELECT tmp-ordf-art
           ASSIGN       TO  path-tmp-ordf-art
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ordf-art
           RECORD KEY   IS toa-chiave.
