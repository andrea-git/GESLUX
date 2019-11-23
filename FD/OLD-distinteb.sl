       SELECT OLD-distinteb
           ASSIGN       TO  "OLD-distinteb"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-distinteb
           RECORD KEY   IS OLD-dis-chiave
           ALTERNATE RECORD KEY IS k-articolo = OLD-dis-articolo-finale, 
           OLD-dis-chiave
           ALTERNATE RECORD KEY IS k-progmag = OLD-dis-chiave-progmag
           WITH DUPLICATES .
