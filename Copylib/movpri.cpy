      *
      *aggiorna progressivi CLI FRN MAS da movimenti contabili
      *deve avere in linea PNT-RECORD e PNR-RECORD (testata e righe primanota)
      *condizionare la scrittura dei file PNT e PNR al valore "S" della
      *   variabile external CONTABILITA
      *ISW-PRI=0  aggiorna progressivi normalmente
      *ISW-PNR=1  moltiplica importo per -1.
      *
      * i-o esterno su CLI                                                                               | mxm 14/01/2005 11.57
      * i-o esterno su FRN                                                                               | mxm 14/01/2005 11.57
      *
       MOVPRI.
           move pnr-importo   to importo-pri.
           move "CO"          to tblco-codice1.
           move pnr-codice-co to tblco-codice2.
           read tblco no lock
           evaluate pnr-tipo-cfm
           when "C"
                move pnr-codice-cfm to cli-codice-G2 clz-codice
                read cli no lock invalid continue end-read
                read clz
                     invalid
                     initialize resto-record-clz
                     write record-clz
                     read clz end-read
                end-read
                if RecLocked
                   go MOVPRI
                end-if
                if pnr-dare-avere = "D"
                   add importo-pri to clz-dare
                else
                   add importo-pri to clz-avere
                end-if
                evaluate tblco-bilancio
                when "A"
                     if pnr-dare-avere = "D"
                        add importo-pri to clz-dare-apertura
                     else
                        add importo-pri to clz-avere-apertura
                     end-if
                when "C"
                     if pnr-dare-avere = "D"
                        add importo-pri to clz-dare-chiusura
                     else
                        add importo-pri to clz-avere-chiusura
                     end-if
                end-evaluate
                if tblco-insoluti = "S"
                   if pnr-dare-avere = "D"
                      if importo-pri > 0
                         add 1           to clz-numero-insoluti
                         add importo-pri to clz-importo-insoluti
                      end-if
                      if importo-pri < 0
                         subtract 1 from clz-numero-insoluti
                         add importo-pri to clz-importo-insoluti
                      end-if
                   end-if
                end-if
                rewrite record-clz
           when "F"
                move pnr-codice-cfm to frn-codice frz-codice
                read frn no lock invalid continue end-read
                read frz
                     invalid
                     initialize resto-record-frz
                     write record-frz
                     read frz end-read
               end-read
               if RecLocked
                  go MOVPRI
               end-if
               if pnr-dare-avere = "D"
                  add importo-pri to frz-dare
               else
                  add importo-pri to frz-avere
               end-if
               evaluate tblco-bilancio
               when "A"
                    if pnr-dare-avere = "D"
                       add importo-pri to frz-dare-apertura
                    else
                       add importo-pri to frz-avere-apertura
                    end-if
               when "C"
                    if pnr-dare-avere = "D"
                       add importo-pri to frz-dare-chiusura
                    else
                       add importo-pri to frz-avere-chiusura
                    end-if
               end-evaluate
               rewrite record-frz
           when "M"
                move pnr-codice-cfm to mas-codice maz-codice
                read mas no lock invalid continue end-read
                read maz
                     invalid
                     initialize resto-record-maz
                     write record-maz
                     read maz end-read
                end-read
                if RecLocked
                   go MOVPRI
                end-if
                if pnr-dare-avere = "D"
                   add importo-pri to maz-dare
                else
                   add importo-pri to maz-avere
                end-if
                evaluate tblco-bilancio
                when "A"
                     if pnr-dare-avere = "D"
                        add importo-pri to maz-dare-apertura
                     else
                        add importo-pri to maz-avere-apertura
                     end-if
                when "C"
                     if pnr-dare-avere = "D"
                        add importo-pri to maz-dare-chiusura
                     else
                        add importo-pri to maz-avere-chiusura
                     end-if
                end-evaluate
                rewrite record-maz
           end-evaluate.
