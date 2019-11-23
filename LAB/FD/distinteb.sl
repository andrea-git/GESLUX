       SELECT distinteb
           ASSIGN       TO  "distinteb"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-distinteb
           RECORD KEY   IS dis-chiave
           ALTERNATE RECORD KEY IS k-articolo = dis-articolo-finale, 
           dis-chiave
           ALTERNATE RECORD KEY IS k-progmag = dis-chiave-progmag
           WITH DUPLICATES .
