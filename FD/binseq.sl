      *Usato per convertire le stampe txt in anteprima, convertendo "-" che causano lentezza in apertura
       SELECT binseq
           ASSIGN       TO  path-binseq
           ORGANIZATION IS BINARY SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-binseq.
