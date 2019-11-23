       identification division.
       program-id.     utydata.
       environment division.
       data division.
       working-storage section.
       
       78  anno_minimo                         value 1900.
       78  anno_massimo                        value 2099.
       78  std_century                         value 19.
       78  rottura_secolo                      value 50.
       
       01  formato_secolo                      pic 9.
           88 secolo_normale                   value 6.
           88 secolo_esteso                    value 8.
       
       01  formato_data                        pic 9.
           88 aaaa_mm_gg                       value 1.
           88 aaaa_gg_mm                       value 2.
           88 mm_gg_aaaa                       value 3.
           88 mm_aaaa_gg                       value 4.
           88 gg_mm_aaaa                       value 5.
           88 gg_aaaa_mm                       value 6.
       
       01  error_level                         pic 99 comp-x.
           88 e_no_errors                      value 0.
           88 e_invalid_date                   value 1.
           88 e_missing_parms                  value 2.
       
       01  red_1                               pic x(24) value
                                           "312831303130313130313031".
       01  gg_nel_mese redefines red_1         pic 99 occurs 12.
       
       01  oggi.
           03 aa_oggi                          pic 9999.
           03 redefines aa_oggi.
              05 a1_oggi                       pic 99.
              05 a2_oggi                       pic 99.
           03 mm_oggi                          pic 99.
           03 gg_oggi                          pic 99.
       
       01  prog_giorno.
           03                                  pic 99.
           03 giorni_da_inizio                 pic 999.
       
       01  seriale                             pic 9(8).
       01  seriale1                            pic 9(8).
       
       01  g_settimana                         pic 9.
       01  sett_da_inizio                      pic 99.
       
       01  primo_giorno                        pic 9.
       
       01  campo_data.
           03 aa                               pic 9999.
           03 redefines aa.
              05 a1                            pic 99.
              05 a2                            pic 99.
           03 mm                               pic 99.
           03 gg                               pic 99.
       
       01  campo_data1                         pic 9(8).
       
       01  anno_bisestile                      pic 9.
           88 bisestile                        value 0.
       
       01  validita_data                       pic 9.
           88 data_valida                      value 1 false 0.
       
       01  num_parametri                       pic 99 comp-1.
       01  par_size                            pic 9(4) comp occurs 3.
       
       77  counter                             pic 9999 comp-x.
       77  temp-var                            pic 9(8).

      *DARWIN
       01  CALLING-PROGRAM                     pic x(25) value spaces.
       01  CALL-STATUS                         pic s99   value zero.
       01  giorno_settimana                    pic 9.
       01  RISULTATO                           pic 9.
      *DARWIN fine
      
       linkage section.
       01  tipo_routine                        pic x.
           88 agg_giorni                       value "1".
           88 sottr_giorni                     value "2".
           88 test_validita                    value "3".
           88 conv_in_data                     value "4".
           88 conv_in_seriale                  value "5".
           88 sottr_date                       value "6".
           88 dom_lun                          value "7".
           88 settimane                        value "8".
           88 giorni_da_data                   value "9".
           88 data_da_giorni                   value "A".
           88 conversione                      value "B".
       
       01  formato                             pic 9.
       01  par_1                               pic 9(8).
       01  par_2                               pic 9(8).
       01  par_3                               pic 9(8).
       
       procedure division using tipo_routine formato par_1 par_2 par_3.
       
       inizio.
       
      ***---
      ***--- Controlla che ci siano almeno tre parametri
      ***---
       
           call "c$narg" using num_parametri.
           if num_parametri < 3
              set e_missing_parms to true
              exit program error_level
              stop run error_level
           end-if.
       
      ***---
      * Rileva la dimensione in bytes dei parametri (salta i primi 2 in
      * quanto fissi di un byte)
      ***---
       
           perform test after varying counter from 3 by 1
                   until counter = num_parametri
              call "c$paramsize" using counter
                           giving par_size(counter - 2)
           end-perform.
           subtract 2 from num_parametri.
       
      ***---
      * Controlla la validita' dei parametri
      ***---
           perform controlli.
       
      ***---
      * Azzera il codice di ritorno (viene cambiato solo se si verifica
      * un errore)
      ***---
           set e_no_errors to true.
       
      ***---
      * Esegue solo se la data inserita e' valida o se e' richiesta una
      * conversione di formato
      ***---
       
           if data_valida
       
      ***---
      * Esegue solo se la routine chiamata ha il n. corretto di
      * parametri, altrimenti ritorna l'errore e_missing_parms
      ***---
              evaluate true
              when agg_giorni       and num_parametri = 2
       
      ***---
      *                    [NOTA VALIDA PER TUTTE LE ROUTINES]
      ***---
      ***---
      * Tutte le operazioni sulle date vengono effettuate sul numero
      * seriale corrispondente alla data stessa,
      * (n. seriale = n. di giorni dal primo giorno dell'anno minimo di
      *  riferimento)
      ***---
      ***---
      * Prima di manipolare la data, questa viene portata in formato
      * aa/mm/gg e viene aggiunto eventualmente il secolo all'inizio
      * dell'anno
      ***---
      * Es.: 311294 viene prima convertito in 941231 e poi in 19941231
      ***---
      * In uscita dalla routine la data viene riportata nel formato
      * originario.
      * La data dell'esempio precedente viene restituita come 311294
      ***---
       
                   perform date_2_serial
                   move par_2(1:par_size(2)) to temp_var
      ***---
      * Dopo aver serializzato la data, aggiunge i giorni desiderati al
      * n. seriale che poi viene riportato in formato data.
      ***---
                   add temp_var to seriale
                   perform serial_2_date
                   move par_size(1) to formato_secolo
                   perform sconv_secolo_esteso
                   move formato to formato_data
                   perform converti_in
                   move campo_data to par_1(1:par_size(1))
       
              when sottr_giorni     and num_parametri = 2
                   perform date_2_serial
                   move par_2(1:par_size(2)) to temp_var
      ***---
      * Dopo aver serializzato la data, sottrae i giorni desiderati dal
      * n. seriale dopo aver verificato che l'operazione non generi una
      * data inferiore alla data minima. Il numero viene poi riportato
      * in formato data.
      ***---
                   if temp_var < seriale
                      subtract temp_var from seriale
                      perform serial_2_date
                      move par_size(1) to formato_secolo
                      perform sconv_secolo_esteso
                      move formato to formato_data
                      perform converti_in
                      move campo_data to par_1(1:par_size(1))
                   else
                      set data_valida to false
                   end-if
                                
              when test_validita    and num_parametri = 1
      ***---
      * Fa solo il test di validita' della data. In realta' il programma
      * esegue le istruzioni seguenti solo se la data e' valida,
      * pertanto le righe potrebbero essere rimosse e sostituite con un
      * verbo CONTINUE (per non attivare il WHEN OTHER)
      ***---
                   move par_1(1:par_size(1)) to campo_data
                   move par_size(1) to formato_secolo
                   move formato to formato_data
                   perform converti_da
                   perform conv_secolo_esteso
                   perform validita
       
              when conv_in_data     and num_parametri = 2
      ***---
      ***--- Dato un numero seriale, lo porta in formato data
      ***---
                   move par_1(1:par_size(1)) to seriale
                   perform serial_2_date
                   move par_size(2) to formato_secolo
                   perform sconv_secolo_esteso
                   move formato to formato_data
                   perform converti_in
                   move campo_data to par_2(1:par_size(2))
       
              when conv_in_seriale  and num_parametri = 2
      ***---
      ***--- Serializza la data passata
      ***---
                   perform date_2_serial
                   move seriale(8 - par_size(2) + 1:) to
                                                   par_2(1:par_size(2))
       
              when sottr_date       and num_parametri = 3
      ***---
      * Per eseguire la sottrazione fra due date, le serializza e
      * sottrae il n. seriale minore da quello maggiore
      ***---
                   perform data_-_data
                   move seriale1(8 - par_size(3) + 1:) to
                                                   par_3(1:par_size(3))
       
              when dom_lun          and num_parametri = 2
      ***---
      * Per stabilire il giorno della settimana della data passata
      * esegue una perform che, per motivi di semplicita' restituisce un
      * numero che assume un valore compreso tra 0 (domenica) e
      * 6 (sabato). Lo zero viene trasformato in 7 per mantenere la
      * stessa codifica della istruzione ACCEPT FROM DAY-OF-WEEK.
      ***---
                   perform giorno_settimana
                   if g_settimana = 0  move 7 to g_settimana end_if
                   move g_settimana(1:) to par_2(par_size(2)- 1 + 1:1)
       
              when settimane        and num_parametri = 2
      ***---
      * Viene chiamata una perform che rilascia il numero di settimane
      * trascorse dall'inizio dell'anno al giorno dato.
      * Convenzioni: La settimana inizia da Domenica e viene considerata
      *              come settimana n. 1 la prima intera.
      ***---
       
                   perform prog_settimana
                   move sett_da_inizio(1:) to
                                           par_2(par_size(2) - 2 + 1:2)
       
              when giorni_da_data   and num_parametri = 2
      ***---
      * Viene chiamata una perform che rilascia il numero di giorni
      * trascorsi dall'inizio dell'anno al giorno dato.
      ***---
       
                   perform date_2_days
                   move giorni_da_inizio(1:) to
                                           par_2(par_size(2) - 3 + 1:3)
       
              when data_da_giorni   and num_parametri = 2
      ***---
      * Viene chiamata una perform che rilascia la data corrispondente
      * ai giorni passati dall'inizio dell'anno.
      ***---
                   move par_2(1:par_size(2)) to campo_data
                   move par_size(2) to formato_secolo
                   move formato to formato_data
                   perform converti_da
                   perform conv_secolo_esteso
                   move par_1(1:par_size(1)) to seriale
                   perform days_2_date
                   perform sconv_secolo_esteso
                   perform converti_in
                   move campo_data to par_2(1:par_size(2))
       
              when conversione      and num_parametri = 3
      ***---
      ***--- Converte una data da un formato ad un altro.
      ***--- I formati disponibili sono: aa/mm/gg, aa/gg/mm, mm/gg/aa,
      ***---                             mm/aa/gg, gg/mm/aa e gg/aa/mm.
      ***---
       
                   move par_1(1:par_size(1)) to campo_data
                   move par_size(1) to formato_secolo
                   move formato to formato_data
                   perform converti_da
                   perform conv_secolo_esteso
                   perform validita
                   if data_valida
                      move par_size(3) to formato_secolo
                      move par_2(1:par_size(2)) to formato_data
                      perform sconv_secolo_esteso
                      perform converti_in
                   end_if
                   move campo_data to par_3(1:par_size(3))
       
              when other
                   set e_missing_parms to true
       
              end-evaluate
           end_if.
       
           if not data_valida  set e_invalid_date to true.
       
           exit program error_level.
           stop run error_level.
       
      ***---
       date_2_serial.
       
      ***---
      ***--- Inizia a contare partendo da 0!!!
      ***---
           move 0 to seriale.
      ***---
      * Incrementa un contatore dall'anno minimo a quello desiderato,
      * aggiungendo al numero seriale 365 o 366 a seconda che l'anno sia
      * bisestile o no
      ***---
           perform varying counter from anno_minimo by 1
                   until counter = aa
              divide counter by 4 giving temp_var
                           remainder anno_bisestile
              if bisestile  add 366 to seriale
                 else       add 365 to seriale
              end-if
           end_perform.
       
      ***---
      * Se l'ultimo anno e' bisestile considera febbraio di 29 giorni.
      ***---
       
           divide aa by 4 giving temp_var remainder anno_bisestile.
           if bisestile  move "29" to gg_nel_mese(2)
              else       move "28" to gg_nel_mese(2).
       
      ***---
      * Incrementa un contatore da 1 (gennaio) al numero corrispondente
      * al mese precedente a quello dato e aggiunge al numero seriale i
      * giorni di ogni mese
      ***---
       
           perform varying counter from 1 by 1 until counter = mm
              add gg_nel_mese(counter) to seriale
           end_perform.
       
      ***---
      * Infine aggiunge al numero seriale i giorni della data passata
      * che fanno parte del mese non considerato nel ciclo precedente.
      ***---
       
           add gg to seriale.
       
      ***---
       serial-2-date.
       
      ***---
      * Converte un numero seriale in data.
      * Partendo dall'anno minimo, sottrae 365 (o 366) al numero seriale
      * finche' e' possibile, incrementando intanto l'anno.
      ***---
       
           divide anno_minimo by 4 giving temp_var
                   remainder anno_bisestile.
           perform varying aa from anno_minimo by 1 until 1 = 2
              divide aa by 4 giving temp_var remainder anno_bisestile
              if bisestile
                 if seriale > 366
                    subtract 366 from seriale
                 else
                    exit perform
                 end-if
              else
                 if seriale > 365
                    subtract 365 from seriale
                 else
                    exit perform
                 end-if
              end-if
           end_perform.
       
      ***---
      * Se l'ultimo anno e' bisestile considera febbraio di 29 giorni.
      ***---
       
           divide aa by 4 giving temp_var remainder anno_bisestile.
           if bisestile  move "29" to gg_nel_mese(2)
              else       move "28" to gg_nel_mese(2).
       
      ***---
      * Incrementa il mese sottraendo al numero seriale i giorni di ogni
      * mese
      ***---
       
           perform varying mm from 1 by 1
                   until seriale <= gg_nel_mese(mm)
              subtract gg_nel_mese(mm) from seriale
           end_perform.
       
      ***---
      * Il numero che rimane corrisponde ai giorni del mese non
      * considerato nel ciclo precedente.
      ***---
       
           move seriale to gg.
       
      ***---
       date_2_days.
       
      ***---
      * Serializza la data partendo dall'anno corrente invece che
      * dall'anno minimo (Vedere sopra per il sistema)
      ***---
       
           move 0 to giorni_da_inizio.
           divide aa by 4 giving temp_var remainder anno_bisestile.
           if bisestile  move "29" to gg_nel_mese(2)
              else       move "28" to gg_nel_mese(2).
       
           perform varying counter from 1 by 1 until counter = mm
              add gg_nel_mese(counter) to giorni_da_inizio
           end_perform.
           add gg to giorni_da_inizio.
       
      ***---
       days_2_date.
       
      ***---
      * Converte un numero seriale in data patenda dall'anno corrente.
      ***---
      * Se l'ultimo anno e' bisestile considera febbraio di 29 giorni.
      ***---
       
           divide aa by 4 giving temp_var remainder anno_bisestile.
           if bisestile  move "29" to gg_nel_mese(2)
              else       move "28" to gg_nel_mese(2).
       
      ***---
      * Incrementa il mese sottraendo al numero seriale i giorni di ogni
      * mese
      ***---
       
           perform varying mm from 1 by 1
                   until seriale <= gg_nel_mese(mm)
              subtract gg_nel_mese(mm) from seriale
           end_perform.
       
      ***---
      * Il numero che rimane corrisponde ai giorni del mese non
      * considerato nel ciclo precedente.
      ***---
       
           move seriale to gg.
       
      ***---
       giorno_settimana.
       
      ***---
      * Serializza la data corrente e ricava il giorno della settimana
      * del primo giorno dell'anno minimo di riferimento
      * (78, anno_minimo)
      ***---
           move campo_data to oggi.
       
           accept campo_data   from century-date.
           accept g_settimana  from day-of-week.
           perform date_2_serial.
       
           compute seriale = seriale + 7 - g_settimana - 1
           divide seriale by 7 giving counter remainder primo_giorno.
           if primo_giorno > 0  compute primo_giorno = 7 - primo_giorno.
       
      ***---
      ***--- Serializza la data
      ***---
       
           move oggi to campo_data.
           perform date_2_serial.
       
      ***---
      * Sottrae al risultato il valore del primo giorno del primo anno
      * (vedere inizio programma) in modo da avere il numero di giorni
      * passati dalla prima domenica dell'anno minimo.
      ***---
           compute seriale = seriale - primo_giorno - 1.
      ***---
      ***--- Divide per sette ed il resto e' il giorno della settimana
      ***---
           divide seriale by 7 giving temp_var remainder g_settimana.
       
      ***---
       prog_settimana.
       
      ***---
      ***--- Calcola il giorno della settimana
      ***---
           perform giorno_settimana.
      ***---
      ***--- Calcola quanti giorni sono passati dall'inizio dell'anno
      ***---
           perform date_2_days.
      ***---
      * Toglie il giorno della settimana dai giorni passati dall'inizio
      * in modo da avere i giorni passati fino alla prima domenica.
      * Divide per sette ed il risultato intero e' il numero di
      * settimane complete da inizio anno.
      ***---
           compute sett_da_inizio =
                           ((giorni_da_inizio + 7) - g_settimana) / 7.
       
      ***---
       data_-_data.
      ***---
      ***--- Serializza la data 1
      ***---
           perform date_2_serial.
           move seriale to seriale1.
           move campo_data1 to campo_data
      ***---
      ***--- Serializza la data 2
      ***---
           perform date_2_serial.
      ***---
      ***--- Fa la differenza fra i due numeri seriali ottenuti.
      ***---
           if seriale1 >= seriale compute seriale1 = seriale1 - seriale
              else                compute seriale1 = seriale - seriale1.
       
      ***---
       converti_da.
       
      ***---
      * A seconda del formato di partenza fa in modo che la data risulti
      * in formato aa/mm/gg, tenendo conto del formato anno (aaaa o aa)
      ***---
           evaluate true
           when aaaa_mm_gg
                move campo_data to oggi
           when aaaa_gg_mm
                if secolo_esteso
                   move a1 to a1_oggi
                   move a2 to a2_oggi
                   move gg to mm_oggi
                   move mm to gg_oggi
                else
                   move a1 to a1_oggi
                   move mm to a2_oggi
                   move a2 to mm_oggi
                end_if
           when mm_gg_aaaa
                if secolo_esteso
                   move mm to a1_oggi
                   move gg to a2_oggi
                   move a1 to mm_oggi
                   move a2 to gg_oggi
                else
                   move mm to a1_oggi
                   move a1 to a2_oggi
                   move a2 to mm_oggi
                end_if
           when mm_aaaa_gg
                if secolo_esteso
                   move a2 to a1_oggi
                   move mm to a2_oggi
                   move a1 to mm_oggi
                   move gg to gg_oggi
                else
                   move a2 to a1_oggi
                   move a1 to a2_oggi
                   move mm to mm_oggi
                end_if
           when gg_mm_aaaa
                if secolo_esteso
                   move mm to a1_oggi
                   move gg to a2_oggi
                   move a2 to mm_oggi
                   move a1 to gg_oggi
                else
                   move mm to a1_oggi
                   move a2 to a2_oggi
                   move a1 to mm_oggi
                end_if
           when gg_aaaa_mm
                if secolo_esteso
                   move a2 to a1_oggi
                   move mm to a2_oggi
                   move gg to mm_oggi
                   move a1 to gg_oggi
                else
                   move a2 to a1_oggi
                   move mm to a2_oggi
                   move a1 to mm_oggi
                end_if
           end_evaluate.
           move oggi to campo_data.
       
      ***---
       converti_in.
       
      ***---
      * Converte una data nel formato richiesto partendo dal formato
      * aa/mm/gg, tenendo conto del formato anno (aaaa o aa)
      ***---
           evaluate true
           when aaaa_mm_gg
                move campo_data to oggi
           when aaaa_gg_mm
                if secolo_esteso
                   move a1 to a1_oggi
                   move a2 to a2_oggi
                   move gg to mm_oggi
                   move mm to gg_oggi
                else
                   move a1 to a1_oggi
                   move mm to a2_oggi
                   move a2 to mm_oggi
                end_if
           when mm_gg_aaaa
                if secolo_esteso
                   move mm to a1_oggi
                   move gg to a2_oggi
                   move a1 to mm_oggi
                   move a2 to gg_oggi
                else
                   move a2 to a1_oggi
                   move mm to a2_oggi
                   move a1 to mm_oggi
                end_if
           when mm_aaaa_gg
                if secolo_esteso
                   move mm to a1_oggi
                   move a1 to a2_oggi
                   move a2 to mm_oggi
                   move gg to gg_oggi
                else
                   move a2 to a1_oggi
                   move a1 to a2_oggi
                   move mm to mm_oggi
                end_if
           when gg_mm_aaaa
                if secolo_esteso
                   move gg to a1_oggi
                   move mm to a2_oggi
                   move a1 to mm_oggi
                   move a2 to gg_oggi
                else
                   move mm to a1_oggi
                   move a2 to a2_oggi
                   move a1 to mm_oggi
                end_if
           when gg_aaaa_mm
                if secolo_esteso
                   move gg to a1_oggi
                   move a1 to a2_oggi
                   move a2 to mm_oggi
                   move mm to gg_oggi
                else
                   move mm to a1_oggi
                   move a1 to a2_oggi
                   move a2 to mm_oggi
                end_if
           end-evaluate.
           move oggi to campo_data.
       
      ***---
       conv_secolo_esteso.
       
      ***---
      * Se vede che la data e' un 9(6) la converte in 9(8) utilizzando
      * il secolo standard (std_century) se l'anno e' superiore all'anno
      * di rottura (rottura_secolo) o, in caso contrario, std_century+1.
      ***---
           if secolo_normale
              move mm to gg
              move a2 to mm
              move a1 to a2
              move std_century to a1
              if a2 < rottura_secolo  add 1 to a1  end-if
           end-if.
       
      ***---
       sconv_secolo_esteso.
       
      ***---
      * Se vede che la data era un 9(6) la riporta nello stesso formato
      * troncando il secolo.
      ***---
           if secolo_normale
              move a2 to a1
              move mm to a2
              move gg to mm
           end-if.
       
      ***---
       validita.
       
      ***--
      * Mette a falso la variabile di validita': se non viene
      * soddisfatta una qualsiasi condizione e' sufficiente uscire senza
      * cambiare nulla.
      ***--
           set data_valida to false.
           if aa < anno_minimo   exit paragraph.
           if aa > anno_massimo  exit paragraph.
           divide aa by 4 giving temp_var remainder anno_bisestile.
           if bisestile  move "29" to gg_nel_mese(2)
              else       move "28" to gg_nel_mese(2).
       
           if mm > 12  exit paragraph.
           if mm <  1  exit paragraph.
           if gg <  1  exit paragraph.
           if gg > gg_nel_mese(mm)  exit paragraph.
       
      ***---
      * Se la data supera tutti i controlli vuol dire che e' valida
      * (ma va'!?)
      ***---
       
           set data_valida to true.
       
      ***---
       controlli.
       
           set data_valida to true.
       
      ***---
      * Controlla la validita' della data passata in linkage
      * Se il primo parametro non e' una data oppure e' una data in
      * formato non standard il controllo viene saltato.
      ***---
       
           if not conv_in_data and not conversione and
              not data_da_giorni
              move par_1(1:par_size(1)) to campo_data
              move par_size(1) to formato_secolo
              move formato to formato_data
      *DARWIN                 
              if test_validita and gg_mm_aaaa
                 perform correggi-data
              end-if
      *DARWIN fine
              perform converti_da
              perform conv_secolo_esteso
              perform validita
           end_if.
      *DARWIN                 
              if test_validita and
                 gg_mm_aaaa    and
                 not data_valida
                 perform chiama-seldata
              end-if
              if test_validita and
                 aaaa_mm_gg    and
                 not data_valida                          
                 move 99999999 to par_1(1:par_size(1))
              end-if
      *DARWIN fine
       
      ***---
      * Controlla se il numero seriale da convertire in data e' minore
      * del numero seriale corrispondente alla data massima.
      ***---
       
           if conv_in_data
              move 31 to gg
              move 12 to mm
              move anno_massimo to aa
              perform date_2_serial
              move par_1(1:par_size(1)) to temp_var
              if temp_var > seriale  set data_valida to false
                 else                set data_valida to true
                 end_if
           end_if.
       
      ***---
      * Se e' richiesta una sottrazione fra due date e la prima e'
      * valida controlla anche la seconda
      ***---
       
           if sottr_date and data_valida
              move campo_data to campo_data1
              move par_2(1:par_size(2)) to campo_data
              move par_size(2) to formato_secolo
              move formato to formato_data
              perform converti_da
              perform conv_secolo_esteso
              perform validita
           end_if.
       
       ultima.
       
       correggi-data.
      * Aggiunta per DARWIN:
      * SOLO SE SI FA IL TEST DI VALIDITA',
      *    se data = ZERO, mettere 01/01/1900;
      *    se data < 100, aggiungere mese ed anno correnti;
      *    se data < 10000, aggiungere anno corrente;
           accept oggi from century-date.
      * N.B.: oggi è in formato aaaa/mm/gg;
      *       campo_data è in formato gg/mm/aaaa e DEVE restare così
           if campo_data < "00000100"
              then move gg      to a1
                   move mm_oggi to a2                                   
                   move a1_oggi to mm
                   move a2_oggi to gg
                   move campo_data to par_1(1:8)
              else if campo_data < "00010000"
                      then move mm to a1
                           move gg to a2
                           move a1_oggi to mm
                           move a2_oggi to gg
                           move campo_data to par_1(1:8)
                      else if campo_data < "01000000"
                              then move a2 to a1
                                   move mm to a2
                                   move std_century to mm
                                   if gg < rottura_secolo
                                      then add 1 to mm
                                   end-if
                                   move campo_data to par_1(1:8).
       chiama-seldata.
           SET ENVIRONMENT "RECURSION" TO 1.
           CALL "C$CALLEDBY" USING CALLING-PROGRAM GIVING CALL-STATUS.
           CALL "C$TOUPPER" USING CALLING-PROGRAM, VALUE 25.
           if CALLING-PROGRAM = "SELDATA" OR "SELDATA1" OR "SELDATA2"
              then exit paragraph.
           MOVE campo_data to oggi.
           ACCEPT campo_data from century-date.
           CALL "seldata" using campo_data, giorno_settimana
                   GIVING RISULTATO.
           SET ENVIRONMENT "RECURSION" TO ZERO.           
      * Se l'utente annulla l'operazione, rimetto la data sbagliata
           IF RISULTATO = ZERO
              THEN MOVE oggi TO campo_data
              ELSE move gg to par_1(1:2)
                   move mm to par_1(3:2)
                   move aa to par_1(5:4)
                   set data_valida to true.
      

