      *Contiene le quantità bollettate per mettere il limite rispetto a quelle totali (lab-sellout-p e sos-ordini)
      *\
       SELECT tmp-sellout-qta
           ASSIGN       TO  path-tmp-sellout-qta
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-sellout-qta
           RECORD KEY   IS tsq-chiave.
