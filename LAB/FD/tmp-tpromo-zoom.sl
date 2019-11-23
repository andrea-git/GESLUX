       SELECT tmp-tpromo-zoom
           ASSIGN       TO  path-tmp-tpromo-zoom
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-tpromo-zoom
           RECORD KEY   IS tmp-tpr-chiave OF tmp-tpromo-zoom
           ALTERNATE RECORD KEY IS tmp-tpr-z-k1 = tmp-tpr-z-gdo OF 
           tmp-tpromo-zoom, tmp-tpr-z-ini-volantino OF tmp-tpromo-zoom
           WITH DUPLICATES .
