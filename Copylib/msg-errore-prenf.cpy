      ***---
       MSG-ERRORE-PRENF.
LUBEXX     evaluate true
LUBEXX     when errore-contatore
LUBEXX          display message "Prenotazione impossibile!!!"
LUBEXX                   x"0d0a""Contatore anno esercizio inesistente!"
LUBEXX                    title tit-err
LUBEXX                     icon 2
LUBEXX*****     when errore-mese-bolla
LUBEXX*****          display message "Prenotazione impossibile!!!"
LUBEXX*****                 x"0d0a""Mese di bollettazione fatturabile: " mese
LUBEXX*****                    title tit-err
LUBEXX*****                     icon 2

LUBEXX     when errore-prezzo
LUBEXX          display message "Prenotazione impossibile!!!"
LUBEXX                   x"0d0a""Prezzo incoerente!!!"
LUBEXX                    title tit-err
LUBEXX                     icon 2
           when errore-omaggio
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Omaggio incoerente!!!"
                          title tit-err
                           icon 2
           when errore-iva-no-E15
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Iva E15 non presente"
                          title tit-err
                           icon 2
           when errore-imposte
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Ricalcolare le imposte ripassando"
                         x"0d0a""sulla riga in modifica ordini"
                          title tit-err
                           icon 2
           when errore-prezzo-master
                display message "Prenotazione impossibile!!!"
                    x"0d0a""Il prezzo non coincide con l'ordine master"
                          title tit-err
                           icon 2
           when errore-prog-master
                display message "Prenotazione impossibile!!!"
                    x"0d0a""L'articolo non coincide con l'ordine master"
                          title tit-err
                           icon 2
           when errore-E15-no-zero
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Iva esente con valori monetari"
                          title tit-err
                           icon 2
           when errore-iva-020
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Codice IVA 20"
                          title tit-err
                           icon 2
           when errore-iva-021
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Codice IVA 21"
                          title tit-err
                           icon 2
           when errore-totale-0
                display message "Prenotazione impossibile!!!"
                         x"0d0a""Importo totale 0"
                          title tit-err
                           icon 2
           end-evaluate.
