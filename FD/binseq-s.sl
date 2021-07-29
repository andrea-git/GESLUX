      *Usato per convertire le stampe txt in anteprima, convertendo "-" che causano lentezza in apertura.
      *S (source), per aprire il file e con una read sola portarlo in -d per scrivere quello nuovo
       SELECT binseq-s
           ASSIGN       TO  path-binseq-s
           ORGANIZATION IS BINARY SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-binseq-s.
