      *Estrazione movimenti di magazzino  fornitori (vmovmagf)
       SELECT tmp-movf
           ASSIGN       TO  path-tmp-movf
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-movf
           RECORD KEY   IS tmf-chiave.
