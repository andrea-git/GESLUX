      *File che contiene solamente le qta vendute per l'anno in corso e i due precedenti. Serve per la colonna "andamento" del programma ordini. Simile a ordfor riempito con la stessa logica da un batch notturno
      *L
      *d
       SELECT qta-vend
           ASSIGN       TO  "qta-vend"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-qta-vend
           RECORD KEY   IS qta-chiave.
