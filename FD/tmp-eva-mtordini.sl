      ** File contenenti le chiavi dei master (teste) filtrati dal primo giro dell'evasione clienti
       SELECT tmp-eva-mtordini
           ASSIGN       TO  path-tmp-eva-mtordini
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tmp-eva-mtordini
           RECORD KEY   IS tmp-mto-chiave.
