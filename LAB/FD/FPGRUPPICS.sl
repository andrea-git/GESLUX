       SELECT FPGRUPPICS
           ASSIGN       TO  "FPGRUPPICS"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-fpgruppics
           RECORD KEY   IS fpgruppics-key
           ALTERNATE RECORD KEY IS key01 = fpgruppics-codice-cs, 
           fpgruppics-key
           WITH DUPLICATES .
