      ***---
       SEND-MAIL.
           accept PathInvioMail     from environment "PATH_INVIO_MAIL".
           accept path-lineseq-mail from environment "PATH_INI_MAIL".

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

           open output lineseq-mail.
           if status-lineseq-mail = "93"
              display message 
                     "File di configurazione invio mail già in uso."
              x"0d0a""Attendere qualche secondo e ritentare l'invio"
                        title "Invio mail"
                         icon 2
              exit paragraph
           end-if.
           if status-lineseq-mail not = "00"
              display message "Errore: " status-lineseq-mail
                              " sul file di configurazione invio mail"
                       x"0d0a""Contattare assistenza"
                        title "Invio mail"
                         icon 2
              exit paragraph
           end-if

           |Prima riga vuota per bypassare i caratteri strani
           move spaces to line-riga-mail
           write line-riga-mail
           |1. ADRESSES (Separati da ";").
           move LinkAddress to line-riga-mail
           write line-riga-mail
           |2. SUBJECT
           move LinkSubject to line-riga-mail
           write line-riga-mail
           |3. BODY adesso posso mettere tutte le righe che voglio
           move LinkBody to line-riga-mail
           write line-riga-mail
           |4. chiusura BODY
           move "###"  to line-riga-mail
           write line-riga-mail
           |5. ATTACH
           inspect LinkAttach replacing trailing low-value by spaces
           move LinkAttach to line-riga-mail
           write line-riga-mail
           |6. FROM
           move tsi-user-from to line-riga-mail
           write line-riga-mail
           |7. SMTP SERVER 
           move tsi-smtp to line-riga-mail
           write  line-riga-mail
           |8. ADDRESS CC
           move LinkAddressCC to line-riga-mail
           write line-riga-mail
           |9. RIGA PORTA
           move tsi-porta to line-riga-mail
           write line-riga-mail
           |10. RIGA UTILIZZO SSL
           move tsi-SSL to line-riga-mail
           write line-riga-mail
           |11. RIGA UTENTE            
           if tsi-user = spaces |(CONVENZIONE INTERNA)
              move "#" to tsi-user
           end-if
           move tsi-user to line-riga-mail
           write line-riga-mail
           |12. RIGA PASSWORD
           move tsi-pass to line-riga-mail
           write line-riga-mail
           |13. RIGA VUOTA FINALE
           move spaces to line-riga-mail
           write line-riga-mail
           
           close lineseq-mail
           move 0 to StatusInvioMail
           call "C$SYSTEM" using PathInvioMail
                          giving StatusInvioMail.
