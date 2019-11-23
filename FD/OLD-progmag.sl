       SELECT OLD-progmag
           ASSIGN       TO  "OLD-progmag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-progmag
           RECORD KEY   IS OLD-prg-chiave OF OLD-progmag
           ALTERNATE RECORD KEY IS key01 = OLD-prg-cod-magazzino OF 
           OLD-progmag, OLD-prg-cod-articolo OF OLD-progmag, 
           OLD-prg-tipo-imballo OF 
           OLD-progmag, OLD-prg-peso OF OLD-progmag
           WITH DUPLICATES .
