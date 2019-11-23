      ***---
       SCRIVI-ERRORE.
           set errori to true

           if errore-bloccante
              set imp-err-bloccante   to true
           else
              if imp-ok
                 set imp-err          to true
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
              move imp-path-log to path-fileseq
              open extend fileseq

              evaluate true
              when imp-articoli
                   move "IMPORTAZIONE ARTICOLI"         to rec-stampa
              when imp-ordini
                   move "IMPORTAZIONE ORDINI CLIENTI"   to rec-stampa
              when imp-carichi
                   move "IMPORTAZIONE CARICHI"          to rec-stampa
              end-evaluate
              write rec-stampa after 2
           end-if.

      ***---
       INITIALIZE-FLAG.
           move zero            to imp-status.
           set imp-ok           to true

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


