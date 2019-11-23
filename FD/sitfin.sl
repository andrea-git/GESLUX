      *Dati per il ricalcolo notturno delle situazioni finanziarie. Usato poi per il recupero ad ogni richiamo (clienti, ordini gruppi GDO). La chiave è composta da due campi ma ne verrà usato solo uno. O gruppo GDO o codice cliente se <> da GDO.
      *La prima riga (vuota) contiene le stringhe da usare per le lables.
       SELECT sitfin
           ASSIGN       TO  "sitfin"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-sitfin
           RECORD KEY   IS sf-chiave.
