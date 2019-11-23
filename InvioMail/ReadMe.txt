ISTRUZIONI PER IL FUNZIONAMENTO DEL PROGRAMMA:

Per utilizzare il programma occorre innanzitutto un server di posta senza
richiesta di autenticazione "mail.cs.interbusiness.it" "smtp.tiscali.it".
Va richiamato tramite "C$SYSTEM" e i parametri vanno indicati come segue:

- Prima riga vuota per bypassare caratteri strani
1. ADRESSES (Separati da ";" tranne per l'ultimo) (*)
2. SUBJECT
3. BODY (Inserire alla fine del body una riga con ### per indicare la fine)
4. ATTACH
5. FROM
6. SMTP SERVER (*)
- Ultima riga vuota finale

(*) = Da file di configurazione

Questi parametri vanno scritti all'interno del file "InvioMail.ini" che
DEVE trovarsi all'interno della stessa cartella in cui risiede l'eseguibile.
Questo file INI dev'essere creato da nuovo ad ogni richiamo e chiuso prima
di effettuare la call SYSTEM.
Al ritorno della "SYSTEM" l'eseguibile lascia una traccia all'interno del file ini:
Se la string "TRUE" allora la mail è stata inviata altrimenti viene scritta
la causa del mancato/errato invio.
La chiamata "SYSTEM" dev'essere fatta semplice ossia asincrona.
Il path COMPLETO dell'exe si trova nel cblconfi così come il path COMPLETO dell'ini.
