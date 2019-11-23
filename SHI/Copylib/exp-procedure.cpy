      ***---
       SCRIVI-ERORRE.
           set errori to true

           if errore-bloccante
              set exp-err-bloccante   to true
           else
              if exp-ok
                 set exp-err          to true
              end-if
           end-if.


           perform APRI-REPORT.
           if primo-errore
              set primo-errore  to false
              move "ELENCO ERRORI" to rec-stampa
              write rec-stampa after 2
           end-if.

           write rec-stampa from como-messaggio after 2.

      ***---
       SCRIVI-MESSAGGIO.
           perform APRI-REPORT.
           write rec-stampa from como-messaggio after 2.

      ***---
       APRI-REPORT.
           if not report-aperto
              set report-aperto to true
              move exp-path-log to path-fileseq
              open extend fileseq

              evaluate true
              when exp-articoli
                   move "ESPORTAZIONE ARTICOLI"         to rec-stampa
              when exp-classi
                   move "ESPORTAZIONE CLASSI"           to rec-stampa
              when exp-fornitori
                   move "ESPORTAZIONE FORNITORI"        to rec-stampa
              when exp-ordini
                   move "ESPORTAZIONE ORDINI CLIENTI"   to rec-stampa
              when exp-vettori
                   move "ESPORTAZIONE VETTORI"          to rec-stampa
              end-evaluate
              write rec-stampa after 2
           end-if.

      ***---
       INITIALIZE-FLAG.
           move zero            to exp-status.
           set exp-ok           to true

           set primo-errore     to true.
           set report-aperto    to false.
           move zero            to num-rec-exp
                                   num-rec-no-exp.

      ***---
       EXIT-PGM.
           if report-aperto
              initialize rec-stampa
              move all "-" to rec-stampa(1:80)
              write rec-stampa after 2
              close fileseq
           end-if.
           goback.


