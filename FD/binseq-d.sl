      *Usato per convertire le stampe txt in anteprima, convertendo "-" che causano lentezza in apertura. Riceve da binseq-s, converte e scrive il nuovo, che verrà poi aperto in maniera line seq
       SELECT binseq-d
           ASSIGN       TO  path-binseq-d
           ORGANIZATION IS BINARY SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-binseq-d.
