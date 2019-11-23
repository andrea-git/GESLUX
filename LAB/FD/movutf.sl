       SELECT movutf
           ASSIGN       TO  "movutf"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-movutf
           RECORD KEY   IS mov-chiave
           ALTERNATE RECORD KEY IS k-data = mov-anno, mov-num-reg, 
           mov-data, mov-tipo, mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-bolla = mov-anno, 
           mov-num-reg, mov-data, mov-num-doc, mov-tipo, mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-reg = mov-anno, mov-data, 
           mov-num-reg, mov-num-doc, mov-tipo, mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-movim = mov-anno, mov-data, 
           mov-tipo, mov-num-movim, mov-num-reg, mov-prog-reg
           WITH DUPLICATES .
