       SELECT log4mas
           ASSIGN       TO  "log4mas"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-log4mas
           RECORD KEY   IS l4m-chiave
           ALTERNATE RECORD KEY IS k-l4m-art = l4m-articolo, l4m-prog
           WITH DUPLICATES .
