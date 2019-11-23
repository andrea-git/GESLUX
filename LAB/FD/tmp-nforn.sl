       SELECT tmp-nforn
           ASSIGN       TO  path-tmp-nforn
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-nforn
           RECORD KEY   IS tmp-nfor-chiave OF tmp-nforn.
