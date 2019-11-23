      ***---
       SCADENZIARIO.
           move high-value to  pat-codice.
           start pat key is <= pat-codice
                 invalid move 0 to contatore-PAT
             not invalid read pat previous no lock
                         move pat-progressivo to contatore-PAT
           end-start.
           move low-value to    scad-chiave.
           start scad key is >= scad-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read scad next no lock at end exit perform end-read
                 if PrimaVolta
                    perform READ-DOCCN-SCAD
                    set PrimaVolta to false
                 end-if
                 if scad-cliente
                    move scad-codice-clifor to cli-codice-G2
                    read CLI no lock invalid continue end-read
                 else
                    move scad-codice-clifor to frn-codice
                    read FRN no lock invalid continue end-read
                 end-if
                 add  1 to contatore-pat
                 move 0 to pas-riga par-riga
                 perform CHECK-SCADENZE
                 perform CREA-PAR righe-par times
                 perform CREA-PAT
                 perform AGGIORNA-CONTATORE-PAT
              end-perform
           end-if.

      ***---
       READ-DOCCN-SCAD.
           move "CN" to doccn-codice1.
           move ap-cf-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "26"   to doccn-tipo.
           read doccn with lock 
                invalid perform CREA-CONTATORE-SCAD
           end-read.
           if contatore-PAT > doccn-contatore
              move contatore-PAT   to doccn-contatore 
           else
              move doccn-contatore to contatore-PAT
           end-if.

      ***---
       CREA-CONTATORE-SCAD.
           initialize record-doccn  replacing numeric data by zeroes
                                         alphanumeric data by spaces.
           move "CN"                to doccn-codice1.
           move ap-cf-codice-login  to doccn-ditta.
           move spaces              to doccn-esercizio.
           move "26"                to doccn-tipo.
           move "Scadenziari"       to doccn-descrizione.
           move scad-data-documento to doccn-data.
           write record-doccn  invalid continue end-write.
           read doccn lock     invalid continue end-read.

      ***---
       CHECK-SCADENZE.
           |Conto le scadenze
           move 0 to idx.
           perform varying idx from 1 by 1
                     until idx > 36
              if scad-data-scadenza(idx) = 0 or
                 scad-importo(idx)       = 0
                 exit perform
              end-if
           end-perform.
           subtract 1 from idx giving tot-scadenze.
      
           move 0 to tot-saldo-dare tot-saldo-avere.
           move 1 to righe-par.
           move spaces to PrimoSegno.
           perform varying idx from 1 by 1
                     until idx > tot-scadenze
              if scad-dare(idx)
                 add scad-importo(idx) to tot-saldo-dare
              else
                 add scad-importo(idx) to tot-saldo-avere
              end-if
              if righe-par = 1
                 if PrimoSegno = spaces
                    move scad-segno(idx) to PrimoSegno
                 else
                    if PrimoSegno not = scad-segno(idx)
                       move scad-segno(idx) to SecondoSegno
                       move 2 to righe-par
                    end-if
                 end-if
              end-if
              perform CREA-PAS
           end-perform.
      
      ***---
       CREA-PAS.
           move contatore-pat          to pas-progressivo.
           add 1 to pas-riga.
           initialize resto-record-pas replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move scad-tipo-CF            to pas-tipo-cfm.
           move scad-codice-clifor      to pas-codice-cfm.
           move scad-data-documento     to pas-data-riferimento.
           move scad-numero-riferimento to pas-numero-riferimento.
           move scad-descrizione(1:30)  to pas-descrizione1.
           move scad-descrizione(31:)   to pas-descrizione2.
           move scad-data-scadenza(idx) to pas-data-scadenza.
           move scad-codice-tr(idx)     to pas-codice-tr.
           if idx = tot-scadenze
              set pas-saldo-acconto-88-s to true
           else
              set pas-saldo-acconto-88-a to true
           end-if.
           move scad-a-vista(idx) to pas-a-vista.
           if scad-dare(idx)
              move scad-importo(idx)     to pas-importo-dare 
                                            pas-importo-dare-va
              move scad-data-documento   to pas-data-registrazione-d
              move scad-numero-documento to pas-numero-protocollo-d
                                            pas-numero-documento-d
              move scad-data-documento   to pas-data-documento-d
              move scad-numerazione      to pas-num-documento-d
           else
              move scad-importo(idx)     to pas-importo-avere
                                            pas-importo-avere-va
              move scad-data-documento   to pas-data-registrazione-a
              move scad-numero-documento to pas-numero-protocollo-a
                                            pas-numero-documento-a
              move scad-data-documento   to pas-data-documento-a
              move scad-numerazione      to pas-num-documento-a
           end-if.
           move scad-insoluti(idx)       to pas-numero-insoluti.
           move scad-solleciti(idx)      to pas-numero-solleciti.
           if scad-cliente
              move cli-codice-ba to pas-codice-ba
           else
              move frn-codice-ba to pas-codice-ba
           end-if.
           set pas-situazione-88-0 to true.
           move scad-scadenza-insoluta(idx) to pas-scadenza-insoluta.
           write record-pas invalid continue end-write.
      
      ***---
       CREA-PAR.
           move contatore-pat          to par-progressivo.
           add 1 to par-riga.
           initialize resto-record-par replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move scad-data-documento   to par-data-registrazione.
           move scad-numero-documento to par-numero-protocollo
                                         par-numero-documento.
           move scad-data-documento   to par-data-documento.
           move scad-descrizione(1:30)to par-descrizione1.
           move scad-descrizione(31:) to par-descrizione2.
           if par-riga = 1 move PrimoSegno   to par-dare-avere
           else            move SecondoSegno to par-dare-avere
           end-if.
           if par-dare-avere-d
              move tot-saldo-dare  to par-importo par-importo-va
           else
              move tot-saldo-avere to par-importo par-importo-va
           end-if.
           move scad-numerazione      to par-num-documento.
           write record-par invalid continue end-write.
      
      ***---
       CREA-PAT.
           move contatore-pat          to pat-progressivo.
           initialize resto-record-pat replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move scad-tipo-CF            to pat-tipo-cfm.
           move scad-codice-clifor      to pat-codice-cfm.
           move scad-data-documento     to pat-data-riferimento.
           move scad-numero-riferimento to pat-numero-riferimento.
           move tot-saldo-dare          to pat-importo-dare.
           move tot-saldo-avere         to pat-importo-avere.
           if tot-saldo-dare > tot-saldo-avere
              compute pat-importo-saldo =
                      tot-saldo-dare - tot-saldo-avere
           else
              compute pat-importo-saldo =
                      tot-saldo-avere - tot-saldo-dare
           end-if.
           move scad-agente           to pat-codice-ag.
           move scad-pagamento        to pat-codice-pa.
           move par-riga              to pat-ultima-riga-par.
           move pas-riga              to pat-ultima-riga-pas.
           move scad-data-documento   to pat-data-registrazione
                                         pat-data-documento.
           move scad-numero-documento to pat-numero-protocollo
                                         pat-numero-documento.
           move scad-numerazione      to pat-num-documento.
           write record-pat invalid continue end-write.
      
      ***---
       AGGIORNA-CONTATORE-PAT.
           move "CN" to doccn-codice1.
           move ap-cf-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "26"   to doccn-tipo.
           read doccn invalid continue end-read.
           move contatore-pat to doccn-contatore.
           rewrite record-doccn invalid continue end-rewrite.
