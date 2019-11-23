       SELECT tmp-nforn-dest
           ASSIGN       TO  path-tmp-nforn-dest
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-nforn-dest
           RECORD KEY   IS tmp-nfod-chiave OF tmp-nforn-dest.
