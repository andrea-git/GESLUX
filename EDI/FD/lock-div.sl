      *Usato in evasioni ordini EDI, per bloccare l'ingresso multiplo per le divisioni. Chi per primo entra, seleziona le divisioni su cui vuole lavorare. Nessun altro può accedere alle stesse
       SELECT lock-div
           ASSIGN       TO  "lock-div"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-lock-div
           RECORD KEY   IS ld-chiave
           ALTERNATE RECORD KEY IS ld-tipocli.
