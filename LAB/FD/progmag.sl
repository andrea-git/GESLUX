       SELECT progmag
           ASSIGN       TO  "progmag"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-progmag
           RECORD KEY   IS prg-chiave OF progmag
           ALTERNATE RECORD KEY IS key01 = prg-cod-magazzino OF 
           progmag, prg-cod-articolo OF progmag, prg-tipo-imballo OF 
           progmag, prg-peso OF progmag
           WITH DUPLICATES .
