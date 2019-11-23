       SELECT teva
           ASSIGN       TO  "teva"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-teva
           RECORD KEY   IS teva-chiave OF teva
           ALTERNATE RECORD KEY IS teva-stato of teva = teva-stato OF 
           teva, teva-chiave OF teva
           WITH DUPLICATES .
