      *Generazione automatica ordini a fornitori
      *Per ora mese-rif e mese-scelto hanno lo stesso significato
      *Invece ord2-lead-time è il valore std (usato per il calcolo del fabbisogno), mente ord2-lead-time-f è quello scelto per l'associazione dei fornitori.
      *
       SELECT ordfor2
           ASSIGN       TO  "ordfor2"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-ordfor2
           RECORD KEY   IS ord2-chiave
           ALTERNATE RECORD KEY IS k-ord = ord2-marca, 
           ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-descr = ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-scorta = ord2-scorta, 
           ord2-art-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ok = ord2-conferma, 
           ord2-art-descrizione
           WITH DUPLICATES .
