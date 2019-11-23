       SELECT STO-movutf
           ASSIGN       TO  path-sto-movutf
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sto-movutf
           RECORD KEY   IS STO-mov-chiave
           ALTERNATE RECORD KEY IS k-data = STO-mov-anno, 
           STO-mov-num-reg, STO-mov-data, STO-mov-tipo, 
           STO-mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-bolla = STO-mov-anno, 
           STO-mov-num-reg, STO-mov-data, STO-mov-num-doc, 
           STO-mov-tipo, STO-mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-reg = STO-mov-anno, 
           STO-mov-data, STO-mov-num-reg, STO-mov-num-doc, 
           STO-mov-tipo, STO-mov-prog-reg
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data-movim = STO-mov-anno, 
           STO-mov-data, STO-mov-tipo, STO-mov-num-movim, 
           STO-mov-num-reg, STO-mov-prog-reg
           WITH DUPLICATES .
