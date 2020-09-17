      *Usato dall invio mail per avere piu di 900 caratteri disponibili (per il corpo della mail nelle conferme d'ordine ad esempio). L ho duplicato in quanto lineseq viene usato da tutte le procedure
       SELECT lineseq-mail
           ASSIGN       TO  wstampa-mail
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq-mail.
