      *file di sort per stampare i master collegati in stordcp
       SELECT tmp-ord-mast
           ASSIGN       TO DISC path-tmp-ord-mast
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ord-mast
           RECORD KEY   IS tmp-om-chiave
           ALTERNATE RECORD KEY IS tmp-om-chiave-master
           WITH DUPLICATES .
