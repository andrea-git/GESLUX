       SELECT giormag
           ASSIGN       TO  "giormag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-giormag
           RECORD KEY   IS gio-chiave
           ALTERNATE RECORD KEY IS k2 = gio-mag, gio-art, gio-mm, 
           gio-tipo-movim
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = gio-aaaa, gio-mm, gio-mag, 
           gio-art, gio-gg, gio-prog
           WITH DUPLICATES .
