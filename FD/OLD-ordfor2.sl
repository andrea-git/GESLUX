      *Generazione automatica ordini a fornitori
      *Per ora mese-rif e mese-scelto hanno lo stesso significato
      *Invece OLD-ord2-lead-time è il valore std (usato per il calcolo del fabbisogno), mente OLD-ord2-lead-time-f è quello scelto per l'associazione dei fornitori.
      *
       SELECT OLD-ordfor2
           ASSIGN       TO  "OLD-ordfor2"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-OLD-ordfor2
           RECORD KEY   IS OLD-ord2-chiave
           ALTERNATE RECORD KEY IS k-ord = OLD-ord2-marca, 
           OLD-ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-descr = OLD-ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-scorta = OLD-ord2-scorta, 
           OLD-ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ok = OLD-ord2-conferma, 
           OLD-ord2-art-descrizione
           WITH DUPLICATES .
