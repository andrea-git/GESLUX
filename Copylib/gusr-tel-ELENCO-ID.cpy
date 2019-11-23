      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-ind-from è l'ID del control ef-ind-from
           when 78-ID-ef-ind-from
                inquire ef-ind-from, value in ef-ind-from-buf

           |78-ID-ef-SMTP è l'ID del control ef-SMTP
           when 78-ID-ef-SMTP
                inquire ef-SMTP, value in ef-SMTP-buf

           |78-ID-ef-porta-smtp è l'ID del control ef-porta-smtp
           when 78-ID-ef-porta-smtp
                inquire ef-porta-smtp, value in ef-porta-smtp-buf

           |78-ID-ef-POP3 è l'ID del control ef-POP3
           when 78-ID-ef-POP3
                inquire ef-POP3, value in ef-POP3-buf

           |78-ID-ef-porta-pop3 è l'ID del control ef-porta-pop3
           when 78-ID-ef-porta-pop3
                inquire ef-porta-pop3, value in ef-porta-pop3-buf

           |78-ID-ef-utente è l'ID del control ef-utente
           when 78-ID-ef-utente
                inquire ef-utente, value in ef-utente-buf

           |78-ID-ef-pwd è l'ID del control ef-pwd
           when 78-ID-ef-pwd
                inquire ef-pwd, value in ef-pwd-buf

           end-evaluate.

