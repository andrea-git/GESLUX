       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricaltras-p.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo trasporti da data a data.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "trasporti.sl".
           copy "tvettori.sl".
           copy "tarifvet.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "trasporti.fd".
           copy "tvettori.fd".
           copy "tarifvet.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo              value "RICALCOLO TRASPORTI".

      *FILE-STATUS
       77  status-trasporti      pic xx.
       77  status-tvettori       pic xx.
       77  status-tarifvet       pic xx.

      * VARIABILI
       77  num-rec-ok            pic 9(10) value 0.
       77  num-rec-ko            pic 9(10) value 0.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
LUBEXX 77  filler                pic 9.
LUBEXX     88 trovata-tariffa    value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.

       77  tot-mov-from-tras     pic 9(5)  value 0.
       77  ult-num-mov           pic 9(8)  value 0.
       77  link-result           pic 9     value 0.   
       77  num-rec               pic 9(18) value 0.

       77  tot-trs-qta-kg        pic 9(15)v999 value 0.
       77  tot-trs-qta-arrot     pic 9(15)v999 value 0.
       77  tot-trs-tariffa       pic 9(15)v99  value 0.

       LINKAGE SECTION.
       77  como-data-from        pic 9(8).
       77  como-data-to          pic 9(8).
       77  link-vettore          pic 9(5).
       77  link-user             pic x(20).
       77  link-handle           handle of window.
       77  link-completo         pic 9.

      ******************************************************************
       PROCEDURE DIVISION using como-data-from 
                                como-data-to   
                                link-vettore
                                link-user      
                                link-handle
                                link-completo.

       DECLARATIVES.
      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TVETTORI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TARIFVET-ERR SECTION.
           use after error procedure on tarifvet.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tarifvet
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Tariffari vettori [TARIFVET] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TARIFVET] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TARIFVET] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           set tutto-ok  to true.
           evaluate status-trasporti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File trasporti [TRASPORTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TRASPORTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TRASPORTI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "trasporti"  to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o trasporti allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to counter counter2.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           |Quando calcolo i trasporti nessuno deve lavorare
           |dentro al file TRASPORTI stesso, mentre il lock per tmovtrat
           |(per la scritura dell'ultimo numero di fattura) sarà
           |gestito attraverso lock per record
           perform OPEN-IO-TRASPORTI-LOCK.
           if tutto-ok
              open input tvettori
                         tarifvet
              if errori
                 close trasporti
              end-if
           end-if.

      ***---
       OPEN-IO-TRASPORTI-LOCK.
           open i-o trasporti allowing readers.

      ***---
       ELABORAZIONE.
           move low-value      to trs-rec.
           if link-completo = 1
              move como-data-from to trs-data-bolla
              start trasporti key >= k-data-bolla
                    invalid set errori to true
              end-start
           else
              move como-data-from to trs-data-fattura
              start trasporti key >= k-data-fattura
                    invalid set errori to true
              end-start
           end-if.
      
           if tutto-ok

              perform until 1 = 2      
                 read trasporti next at end exit perform end-read

                 if link-completo = 1
                    if trs-data-bolla > como-data-to
                       exit perform
                    end-if
                 else
                    if trs-data-fattura > como-data-to
                       exit perform
                    end-if
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    move counter to counter-edit
                    display counter-edit 
                       upon link-handle at column 22,00 line 12,00
                    move 0 to counter2
                 end-if
                 
                 if link-vettore not = 0 and
                    link-vettore not = trs-vettore
                    exit perform cycle
                 end-if

                 if link-completo = 1
                    add 1 to num-rec

                    add trs-qta-kg    to tot-trs-qta-kg   
                    add trs-qta-arrot to tot-trs-qta-arrot
                    add trs-tariffa   to tot-trs-tariffa  
                    delete trasporti record 
                           invalid continue 
                    end-delete
                 else
                    move trs-vettore to vet-codice
                    read tvettori no lock 
                         invalid 
                         add 1 to num-rec-ko
                     not invalid
                         perform TROVA-TARIFFA-E-VALORIZZA-CAMPO
                         perform TROVA-TARIFFA-E-VALORIZZA-CAMPO-SHI
                         perform TROVA-TARIFFA-E-VALORIZZA-CAMPO-GET
                         perform VALORIZZA-DATI-COMUNI
                         rewrite trs-rec invalid continue end-rewrite
                         add 1 to num-rec-ok
                    end-read
                 end-if
                 
              end-perform                                    
              display message "RECORD CANCELLATI: " num-rec
                       x"0d0a""QTA KG: "    tot-trs-qta-kg   
                       x"0d0a""QTA ARROT: " tot-trs-qta-arrot
                       x"0d0a""TARIFFA: "   tot-trs-tariffa  
              if link-completo = 1     
                 close      trasporti
                 open input trasporti
                 call   "caltras" using tot-mov-from-tras
                                        ult-num-mov
                                        link-user
                                        link-result
                                        link-handle
                                        como-data-from
                                        como-data-to
                                        link-vettore
                 cancel "caltras"
                 move tot-mov-from-tras to num-rec-ok   
                 display message "RECORD DA CALCOLO: " num-rec-ok
                 move low-value to trs-rec
                 move como-data-from to trs-data-bolla
                 start trasporti key >= k-data-bolla
                       invalid set errori to true
                 end-start
                 move 0 to num-rec                        
                           tot-trs-qta-kg   
                           tot-trs-qta-arrot
                           tot-trs-tariffa  
                 perform until 1 = 2
                    read trasporti next at end exit perform end-read
                   
                    if trs-data-bolla > como-data-to
                       exit perform
                    end-if             
                 
                    if link-vettore not = 0 and
                       link-vettore not = trs-vettore
                       exit perform cycle
                    end-if

                    add 1 to num-rec
                    add trs-qta-kg    to tot-trs-qta-kg   
                    add trs-qta-arrot to tot-trs-qta-arrot
                    add trs-tariffa   to tot-trs-tariffa  
                 end-perform
                 display message "RECORD RITROVATI: " num-rec  
                          x"0d0a""QTA KG: "    tot-trs-qta-kg   
                          x"0d0a""QTA ARROT: " tot-trs-qta-arrot
                          x"0d0a""TARIFFA: "   tot-trs-tariffa  
              end-if

              if num-rec-ok = 0
                 display message "Nessuna rettifica effettuata!"
                           title titolo
                            icon 2
              else                                      
                 if num-rec-ko = 0
                    display message 
                            "Rettificati " num-rec-ok, " trasporti"
                              title titolo
                               icon 2
                 else
                    display message 
                    "Rettificati " num-rec-ok, " trasporti"
              x"0d0a"num-rec-ko, " non rettificati vettore non valido"
                              title titolo
                               icon 2
              end-if
           end-if.

      ***---
       VALORIZZA-DATI-COMUNI.
           if trs-data-creazione = 0
              accept trs-data-creazione from century-date
           end-if.
           if trs-ora-creazione = 0
              accept trs-ora-creazione from time
           end-if.
           if trs-utente-creazione = space
              move link-user to trs-utente-creazione
           end-if.

           accept trs-data-ultima-modifica from century-date.
           accept trs-ora-ultima-modifica  from time.
           move link-user to trs-utente-ultima-modifica.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO.
           move 0          to trs-tariffa.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if trs-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if trs-provincia = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if trs-cliente = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if trs-cliente = tfv-campo1 and
LUBEXX                      trs-destino = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot >= tfv-qli-da and
                          trs-qta-arrot <= tfv-qli-a
                          move tfv-euro to trs-tariffa
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO-SHI.
           move 0          to trs-tariffa-SHI.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if trs-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if trs-provincia = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if trs-cliente = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if trs-cliente = tfv-campo1 and
LUBEXX                      trs-destino = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot-SHI >= tfv-qli-da and
                          trs-qta-arrot-SHI <= tfv-qli-a
                          move tfv-euro to trs-tariffa-SHI
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO-GET.
           move 0          to trs-tariffa-GET.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if trs-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if trs-provincia = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if trs-cliente = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if trs-cliente = tfv-campo1 and
LUBEXX                      trs-destino = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot-GET >= tfv-qli-da and
                          trs-qta-arrot-GET <= tfv-qli-a
                          move tfv-euro to trs-tariffa-GET
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           unlock trasporti all records.
           close  trasporti
                  tarifvet.
      
      ***---
       EXIT-PGM.
           display "                                                   "
              upon link-handle at column 22,00 line 12,00.
           goback.
