      *** File contenente i risultati delle sommatorie delle righe tra fatture e note credito differenziate tra STAMPA FATTURE, POSTEL, CONTABILITA' G2 **
      *i
       SELECT tmp-contab
           ASSIGN       TO  path-tmp-contab
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-contab
           RECORD KEY   IS tmcont-chiave.
