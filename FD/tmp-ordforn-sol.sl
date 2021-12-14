      ** per pgm ordforn-sol
       SELECT tmp-ordforn-sol
           ASSIGN       TO  path-tmp-ordforn-sol
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-ordforn-sol
           RECORD KEY   IS tos-chiave
           ALTERNATE RECORD KEY IS k-mag = tos-mag-codice, 
           tos-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-frn-ragsoc = tos-frn-ragsoc, 
           tos-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-data = tos-data-ordine, 
           tos-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art = tos-cod-articolo, 
           tos-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-art-des = tos-art-descrizione, 
           tos-tof-chiave
           WITH DUPLICATES .
