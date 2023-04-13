      *Usato da:
      *- pordini
      *
      *per indicare chi tiene in blocco il file
       SELECT lockname
           ASSIGN       TO  "lockname"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-lockname
           RECORD KEY   IS lckn-chiave
           ALTERNATE RECORD KEY IS lckn-k-ute = lckn-chiave, lckn-utente
           WITH DUPLICATES .
