      ***---
       SEND-MAIL.
           accept PathInvioMail from environment "PATH_INVIO_MAIL".
           accept wstampa       from environment "PATH_INI_MAIL".

           call "nomepgm" using NomeProgramma.

           open input tsetinvio.
           move NomeProgramma to tsi-codice.
           read tsetinvio no lock
                invalid
                move spaces to tsi-codice
                read tsetinvio no lock
                     invalid display message "Parametri assenti"
                end-read
           end-read.
           close tsetinvio.

           |Questi programmi recuperano l'indirizzo del mittente
           |diversamente da quanto scritto in tabella
           evaluate NomeProgramma 
           when "conford"      
           when "invioordforn" 
           when "invio-sol"    move LinkAddressFrom to tsi-user-from
           end-evaluate.

           open output lineseq.
           if status-lineseq = "00"
              |Prima riga vuota per bypassare i caratteri strani
              move spaces to line-riga of lineseq
              write line-riga of lineseq
              |1. ADRESSES (Separati da ";").
              move LinkAddress to line-riga of lineseq
              write line-riga of lineseq
              |2. SUBJECT
              move LinkSubject to line-riga of lineseq
              write line-riga of lineseq
              |3. BODY adesso posso mettere tutte le righe che voglio
              move LinkBody to line-riga of lineseq
              write line-riga of lineseq
              |4. chiusura BODY
              move "###"  to line-riga of lineseq
              write line-riga of lineseq
              |5. ATTACH
              inspect LinkAttach replacing trailing low-value by spaces
              move LinkAttach to line-riga of lineseq
              write line-riga of lineseq
              |6. FROM
              move tsi-user-from to line-riga of lineseq
              write line-riga of lineseq
              |7. SMTP SERVER 
              move tsi-smtp to line-riga of lineseq
              write  line-riga of lineseq
              |8. ADDRESS CC
              move LinkAddressCC to line-riga of lineseq
              write line-riga of lineseq
              |9. RIGA PORTA
              move tsi-porta to line-riga of lineseq
              write  line-riga of lineseq
              |10. RIGA UTILIZZO SSL
              move tsi-SSL to line-riga of lineseq
              write  line-riga of lineseq
              |11. RIGA UTENTE            
              if tsi-user = spaces |(CONVENZIONE INTERNA)
                 move "#" to tsi-user
              end-if
              move tsi-user to line-riga of lineseq
              write  line-riga of lineseq
              |12. RIGA PASSWORD
              move tsi-pass to line-riga of lineseq
              write  line-riga of lineseq
              |13. RIGA VUOTA FINALE
              move spaces to line-riga of lineseq
              write line-riga of lineseq
              
              close lineseq
              move 0 to StatusInvioMail
              call "C$SYSTEM" using PathInvioMail
                             giving StatusInvioMail
           end-if.
