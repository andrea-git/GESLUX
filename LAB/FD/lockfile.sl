      *Utilizzato per bloccare i programmi a livello di funzione ad un solo utente.
      *Un utente per volta può fare NUOVO su GCLIENTI.
      *Un utente per volta può entrare nella funzione di ARTICOLI DA CONFERMARE da GORDFORN(VAR).
      *Un utente alla volta può entrare nella funzione di EVAART e EVAART-GDO e mentre da quest'ultimo vengono create evasioni, non possono 
      *venire create da nessun'altra parte in modo da garantire la sequenzialità dei numeri di evasioni create. In caso di crash controlli di lock all'ingresso dei programmi di evasione e al momento del salvataggio GORDC un sistema di cancellazione gestisce in autmatico il record di controllo.
      *In ogni caso, alla peggio, chiudere tutte le sessioni e cancellare manualmente il file.
      *
       SELECT lockfile
           ASSIGN       TO  "lockfile"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-lockfile
           RECORD KEY   IS lck-chiave.
