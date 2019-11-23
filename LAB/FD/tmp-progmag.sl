       SELECT tmp-progmag
           ASSIGN       TO  path-tmp-progmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-progmag
           RECORD KEY   IS tmp-prg-chiave
           ALTERNATE RECORD KEY IS key-des = tmp-prg-art-des, 
           tmp-prg-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS key-costo = tmp-prg-costo-medio, 
           tmp-prg-art-des
           WITH DUPLICATES .
