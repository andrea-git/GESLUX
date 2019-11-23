      **** NON USATO ****
       SELECT classi
           ASSIGN       TO  "classi"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-classi
           RECORD KEY   IS cla-chiave
           ALTERNATE RECORD KEY IS cla-livello-classe
           WITH DUPLICATES .
