       SELECT STO-giormag
           ASSIGN       TO  path-sto-giormag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-giormag
           RECORD KEY   IS STO-gio-chiave
           ALTERNATE RECORD KEY IS k2 = STO-gio-mag, STO-gio-art, 
           STO-gio-mm, STO-gio-tipo-movim
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = STO-gio-aaaa, STO-gio-mm, 
           STO-gio-mag, STO-gio-art, STO-gio-gg, STO-gio-prog
           WITH DUPLICATES .
