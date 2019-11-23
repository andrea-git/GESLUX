      *file tmp che serve a memorizzare i valori degli ordini per cliente quando faccio l'attivazione
       SELECT tmp-fido
           ASSIGN       TO  path-tmp-fido
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-fido
           RECORD KEY   IS tfid-chiave.
