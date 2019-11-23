      *File di relazioni ordini. Serve per manternere le relazioni create al momento dello split della bolla.
      *z
       SELECT reltor
           ASSIGN       TO  "reltor"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-reltor
           RECORD KEY   IS rlt-chiave
           ALTERNATE RECORD KEY IS k-bolla = rlt-chiave-bolla
           WITH DUPLICATES .
