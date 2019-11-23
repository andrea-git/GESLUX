      *******************************************************************************
       IDENTIFICATION DIVISION.
      *******************************************************************************

       PROGRAM-ID.     CALSCA.
      *
      *calcolo scadenze da condizioni pagamento
      *
      *******************************************************************************
       ENVIRONMENT DIVISION.
      *******************************************************************************
      * se la 1^ rata e' uguale a zero, l'eventuale arrotondamento viene fatto
      * sull'ultima rata - ds 16.09.03
      *
      * redirect disable                                                                                 | mxm 12/01/2005
      *
      * se la 1^ rata e' in valore assoluto, l'eventuale arrotondamento viene fatto
      * sull'ultima rata - ds 18.04.05

      * nel caso di scadenze con giorni > 3000, non calcolava correttamente la scadenza
      * - ds 27.06.05
      *
      *   12.04.06 - S.R. nr. 1168 - Aggiunta inizializzazione record tblpa nel caso                     |pm 12/04/2006 17.37
      *                              di codice pagamento inesistente                                     |pm 12/04/2006 17.38
      *----------------------------------------------------------------------------------------------------------------------
      * tk.1973 -  in caso di codice pagamento inesistente segnala e abortisce                           | mxm 25/05/2009 13.00
      *----------------------------------------------------------------------------------------------------------------------
      * tk.2053 -  errore in caso di scadenza calcolata al 31 di un mese con 30 giorni; anziche' andare  |ds 06/11/2009 12.24
      *            allo 01 del mese successivo si fermava al 30 del mese.
      *----------------------------------------------------------------------------------------------------------------------
      * tk.2599 -  errore in caso di pagamento fine mese con scadenza anno successivo.                   |ds 23/12/2009 12.41
      *----------------------------------------------------------------------------------------------------------------------
      * tk.2613 -  errore in caso di scadenza 29 o 30 febbraio : anziche' andare a 01 o 02 marzo metteva |ds 18/01/2010 11.15
      *            28 febbraio.
      *----------------------------------------------------------------------------------------------------------------------
      * tk.2224 -  errore "abend missing..." in gestione primanota errore simile segnalato con ticket 2103 | rososv 26/07/2011
      * Corretto il seguente errore:
      * - Se in fase di aggiornamento scadenziario non esisteva il codice pagamento 
      *   sulla tabella, veniva generato l'errore irreversibile "abend missing..."
      * Soluzione:
      * - Ora in caso di codice pagamento inesistente viene accettato ugualmente con 
      *   l'impostazione dei valori di default (gli stessi proposti anche in gestione di 
      *   nuovo codice di pagamento).
      *   Se si tratta di programma interattivo viene anche fornito un messaggio a video 
      *   di codice di pagamento non esistente.
      *----------------------------------------------------------------------------------------------------------------------
      * tk.3852 - Slittamento scadenze non gestisce correttamente fine mese                                | rososv 02/11/2012
      * Corretto il seguente errore:
      * - Si imposta un codice di pagamento che prevede l'esclusione di in mese con slittamento della rata 
      *   al mese successivo, e l'attivazione del flag per lo slittamento di un mese di tutte le scadenze successive. 
      *   Il problema si presenta quando incorriamo nello slittamento a un mese di 30 giorni (se slitto agosto e passo a settembre, per es). 
      *   Le successive scadenze vengono emesse al 30 (anziché al 31, esempio 30/10) anche se il codice pagamento ha impostato FINE MESE.
      *-----------------------------------------------------------------------------------------------------------------------
      * tk.4771 - Agroalimentari: disposizioni art 62 D.L.1 del 24/01/2012 (beni deteriorabili e non deteriorabili)
      *                                                                                                    | rososv 05/11/2012
      *
      * - Aggiunto calcolo scadenze per giorni effettivi anzichè commerciali, in base al nuovo  
      *   radio-button presente sulla tabella pagamenti.
      *------------------------------------------------------------------------------------------------------------------------
      * tk.4918 - ticket interno - aperto per correzione problemi riscontrati durante il collaudo "agroalimentare" 
      *                                                                                                    | rososv 05/02/2013
      * Corretto il seguente errore:
      * - Se stampa provvisoria di documento con data documento a zero e codice pagamento con calcolo
      *   scadenze a calendario (non per mesi commerciali) il programma di stampa segnalava il messaggio
      *   "Valore non corretto".
      *-------------------------------------------------------------------------------------------------------------------  
      *
       CONFIGURATION SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA
                       CRT STATUS F1
                       EVENT STATUS EVENT-STATUS
                       SCREEN CONTROL SC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY SELROU.
      
      *******************************************************************************
       DATA DIVISION.
      *******************************************************************************

       FILE SECTION.
       COPY ROU.

       WORKING-STORAGE SECTION.

           copy     "system.def".                               |ds 06/11/2009
           copy     "file.def".
           copy     "tbl.def".
      ***       copy     "tblpa.dpb" replacing  == dpb-tblpa == by == dpb-tblpa redefines record-tbl ==.   | rososv 26/07/2011 10:00:22
           copy     "tblva.dpb" 
                     replacing  == dpb-tblva == by 
                      == dpb-tblva redefines record-tbl ==.
           copy     "tblpa.dab".                                                                         | rososv 26/07/2011 10:00:38
           copy     "adddays.clk".                                                                       | rososv 05/11/2012
       
           COPY VARIAB.
          05  AAAA                                PIC  9(04).
          05  AAAAAA                              PIC  9(04)V9(02).
          05  SLITTAMENTO                         PIC  X(01).
          05  MESI-SLITTAMENTO                    PIC  9(01).
          05  IMPORTO-DETRAZIONE                  PIC S9(13)V9(02).
          05  IMPORTO-DETRAZIONE-VA               PIC S9(15)V9(03).
          05  CAMBIO-LIRE                         PIC  9(01).
              88  CAMBIO-LIRE-88                      VALUE 1.
          05  IMPORTO-FATTURA-EURO                PIC S9(13)V9(02).
          05  DECIMALI                            PIC  9(01).
          05  IMPORTO0                            PIC S9(15).
          05  IMPORTO1                            PIC S9(15)V9(01).
          05  IMPORTO2                            PIC S9(15)V9(02).
          05  IMPORTO3                            PIC S9(15)V9(03).
          05  ws-resto                            pic s9(6).
          05  ws-fine-mese                        pic  9(08).                     |ds 06/11/2009
          05  filler redefines ws-fine-mese.                                      |ds 06/11/2009
              10 ws-fine-aa                       pic  9(04).                     |ds 06/11/2009
              10 ws-fine-mm                       pic  99.                        |ds 06/11/2009
              10 ws-fine-gg                       pic  99.                        |ds 06/11/2009
       77  isannobisesto                    unsigned-int.                                                |ds 18/01/2010 11.48
       77  ws-data-num                            pic  9(08).                                            |ds 18/01/2010 12.11

       77  ws-program-batch                       pic x(20).                                               | rososv 26/07/2011 10:21:43
           88  ws-program-batch-ok          value "CARPRI02" "carpri02".                             | rososv 26/07/2011 10:11:07
 
       77  ws-tblpa-tipo-calcolo-gg               pic 9.                                                   | rososv 05/11/2012
           88  ws-tblpa-tipo-calcolo-gg-comm      value 1.                                                 | rososv 05/11/2012
           88  ws-tblpa-tipo-calcolo-gg-eff       value 2.                                                 | rososv 05/11/2012
       77  ws-data-inizio-calcolo                 pic 9(8).                                                | rososv 05/11/2012
     
       COPY VARSCA.
       01  LIN-VARIABILI PIC X(11000).

       PROCEDURE DIVISION.
      *******************************************************************************

       DECLARATIVES.
       COPY DECROU.
       END DECLARATIVES.

       PROG SECTION.
       INIZIO.
           set  sw-my-redirect-disable-on  to  true.                                                      | mxm 12/01/2005
       
           CALL "C$CALLEDBY" USING COMODO-CODICE.
           MOVE 00 TO TIPO-PROCEDURA.
           MOVE "CALSCA" TO NOME-PROGRAMMA.
           MOVE 99 TO W-S W-L.

           move ext-mainprogram        to ws-program-batch.                                                 | rososv 26/07/2011 10:22:10
    
           PERFORM LEGGEPAR.
           MOVE LIN-VARIABILI TO VARIABILI-VARSCA.

           IF NOT DIVISA-LIRE
              COMPUTE IMPORTO-FATTURA-EURO ROUNDED = 
              SCA-IMPORTO-FATTURA-VA / 1936,27
             IF IMPORTO-FATTURA-EURO = SCA-IMPORTO-FATTURA
               MOVE 1 TO CAMBIO-LIRE
             END-IF
           END-IF.

           MOVE "VA" TO TBL-CODICE  MOVE SCA-TBLVA-CODICE TO TBL-CODICE2
           perform  fm_tbl-read-random
           if  not file-ok
             MOVE 0 TO DECIMALI
           else
               MOVE TBLVA-DECIMALI TO DECIMALI.

      ***    MOVE "PA" TO TBL-CODICE  MOVE  SCA-CODICE-PA TO TBL-CODICE2.                                  | rososv 26/07/2011 10:00:58
      ***    perform  fm_tbl-read-random.                                                                  | rososv 26/07/2011 10:00:59

           move sca-codice-pa   to tblpa-codice2.                                                           | rososv 26/07/2011 10:01:30
           perform  al-tblpa-read-random                                                                    | rososv 26/07/2011 10:02:07
           if not file-ok                                                         |pm 12/04/2006 17.38
              perform  abort-tblpa-missing                                                                | mxm 25/05/2009 13.07
      ***        initialize resto-record-tblpa                                      |pm 12/04/2006 17.38 | mxm 25/05/2009 12.58
               initialize resto-record-tblpa                                                                 | rososv 26/07/2011 16:35:21
           end-if.                                                                |pm 12/04/2006 17.38
    
           move tblpa-tipo-calcolo-gg    to ws-tblpa-tipo-calcolo-gg.                                       | rososv 05/11/2012    
    
           MOVE TBLPA-SLITTAMENTO TO SLITTAMENTO.
           MOVE TBLPA-NUMERO-SCADENZE TO SCA-RATE.
           IF SCA-MESI-EFF NOT = "00000000"
             MOVE SCA-MESE1 TO TBLPA-MESE1
             MOVE SCA-GIORNO1 TO TBLPA-GIORNO1
             MOVE SCA-ESCLUSO-DAL-GIORNO1 TO TBLPA-ESCLUSO-DAL-GIORNO1
             IF TBLPA-ESCLUSO-DAL-GIORNO1 NOT NUMERIC
               MOVE 1 TO TBLPA-ESCLUSO-DAL-GIORNO1
             END-IF
             MOVE SCA-MESE2 TO TBLPA-MESE2
             MOVE SCA-GIORNO2 TO TBLPA-GIORNO2
             MOVE SCA-ESCLUSO-DAL-GIORNO2 TO TBLPA-ESCLUSO-DAL-GIORNO2
             IF TBLPA-ESCLUSO-DAL-GIORNO2 NOT NUMERIC
               MOVE 1 TO TBLPA-ESCLUSO-DAL-GIORNO2
             END-IF
           END-IF.
           IF NOT (SCA-TIPO-IMPORTI (01) = SPACE AND 
              SCA-TIPO-SCADENZE (01) = SPACE)
             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
               IF NOT (SCA-TIPO-IMPORTI (J) = SPACE  AND
                   SCA-TIPO-SCADENZE (J) = SPACE)
                 MOVE J TO TBLPA-NUMERO-SCADENZE SCA-RATE
               END-IF
               MOVE SCA-CODICE-TR-PA (J) TO TBLPA-CODICE-TR (J)
               MOVE SCA-INIZIO-CONTEGGIO(J) TO TBLPA-INIZIO-CONTEGGIO(J)
               MOVE SCA-TIPO-IMPORTI (J) TO TBLPA-TIPO-IMPORTI (J)
               MOVE SCA-TIPO-SCADENZE (J) TO TBLPA-TIPO-SCADENZE (J)
               MOVE SCA-VALORE (J) TO TBLPA-IMPORTO (J)
               MOVE SCA-SCADENZA (J) TO TBLPA-SCADENZA (J)
               MOVE SCA-DETRAZIONE (J) TO TBLPA-DETRAZIONE (J)
             END-PERFORM
           END-IF.

           INITIALIZE SCA-TABELLA.
           MOVE SCA-DATA-FATTURA TO SCA-DATA-DOCUMENTO.
           IF SCA-DATA-FATTURA = ZERO
             MOVE "00010101" TO SCA-DATA-DOCUMENTO
           END-IF.
           IF SCA-DATA-CONTEGGIO NOT = ZERO
             MOVE SCA-DATA-CONTEGGIO TO SCA-DATA-DOCUMENTO
           END-IF.
           IF TBLPA-IVA = "A"
             SUBTRACT SCA-IVA FROM SCA-IMPORTO-FATTURA
             SUBTRACT SCA-IVA-VA FROM SCA-IMPORTO-FATTURA-VA
           END-IF.
           SUBTRACT SCA-SPESE FROM SCA-IMPORTO-FATTURA.
           SUBTRACT SCA-SPESE-VA FROM SCA-IMPORTO-FATTURA-VA.

           PERFORM SCA-TABELLA THRU FINE-SCA-TABELLA VARYING SCA-J
             FROM 1 BY 1 UNTIL SCA-J > TBLPA-NUMERO-SCADENZE.  
      
           MOVE 0 TO SCA-TOTALE SCA-TOTALE-VA.
           IF TBLPA-IVA = "A"
             ADD SCA-IVA TO SCA-IMPORTO (1) SCA-IMPORTO-FATTURA
             ADD SCA-IVA-VA TO SCA-IMPORTO-VA (1) SCA-IMPORTO-FATTURA-VA
           END-IF.
           ADD SCA-SPESE TO SCA-IMPORTO (1) SCA-IMPORTO-FATTURA.
           ADD SCA-SPESE-VA TO SCA-IMPORTO-VA (1) SCA-IMPORTO-FATTURA-VA.
           PERFORM SCA-SOMMA VARYING SCA-J FROM 1 BY 1 UNTIL SCA-J
             > TBLPA-NUMERO-SCADENZE.
           ADD IMPORTO-DETRAZIONE TO SCA-IMPORTO-FATTURA.
           ADD IMPORTO-DETRAZIONE-VA TO SCA-IMPORTO-FATTURA-VA.
           IF SCA-IMPORTO (01) NOT = 0 AND TBLPA-IMPORTO (1) NOT = 0       |ds 09.03.04
            AND TBLPA-TIPO-IMPORTI (1) NOT = "A"                                        |DS 18/04/2005 11.19
             COMPUTE SCA-IMPORTO (01) =                                    |ds 16.09.03
               SCA-IMPORTO (01) + SCA-IMPORTO-FATTURA - SCA-TOTALE         |ds 16.09.03
             COMPUTE SCA-IMPORTO-VA (01) =                                 |ds 16.09.03
               SCA-IMPORTO-VA (01) + 
               SCA-IMPORTO-FATTURA-VA - SCA-TOTALE-VA |ds 16.09.03
           ELSE                                                            |ds 16.09.03
             COMPUTE SCA-IMPORTO (TBLPA-NUMERO-SCADENZE) =                 |ds 16.09.03
               SCA-IMPORTO (TBLPA-NUMERO-SCADENZE) +                       |ds 16.09.03
                 SCA-IMPORTO-FATTURA - SCA-TOTALE                          |ds 16.09.03
             COMPUTE SCA-IMPORTO-VA (TBLPA-NUMERO-SCADENZE) =               |ds 16.09.03
               SCA-IMPORTO-VA (TBLPA-NUMERO-SCADENZE) +                    |ds 16.09.03
                 SCA-IMPORTO-FATTURA-VA - SCA-TOTALE-VA                    |ds 16.09.03
           END-IF.                                                         |ds 16.09.03
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 36
             IF SCA-IMPORTO-FATTURA = SCA-IMPORTO-FATTURA-VA
               MOVE SCA-IMPORTO (J) TO SCA-IMPORTO-VA (J)
             END-IF
             IF J > TBLPA-NUMERO-SCADENZE
               INITIALIZE SCA-TAB (J)
               MOVE ZERO TO SCA-DATA (J)
             END-IF
           END-PERFORM.
           GO FINE-CALSCA.

       SCA-TABELLA.
           IF TBLPA-INIZIO-CONTEGGIO (SCA-J) = 99
             MOVE "S" TO SCA-A-VISTA (SCA-J)
           END-IF.
           MOVE TBLPA-CODICE-TR (SCA-J) TO SCA-CODICE-TR (SCA-J).
           IF TBLPA-TIPO-SCADENZE (SCA-J) = "D"
             MOVE TBLPA-SCADENZA (SCA-J) TO DATA-GO
             MOVE CORR DATA-GO TO SCA-DATA (SCA-J)
             GO SCA-CALCOLO-IMPORTO
           END-IF.
           IF TBLPA-INIZIO-CONTEGGIO (SCA-J) = 0  OR
               TBLPA-INIZIO-CONTEGGIO (SCA-J) = 99  OR
               TBLPA-INIZIO-CONTEGGIO (SCA-J) > 30
             GO SCA-CALCOLO-SCADENZA
           END-IF.
           IF TBLPA-INIZIO-CONTEGGIO (SCA-J) < GIORNO
             ADD 1 TO MESE
             IF MESE > 12
               ADD 1 TO ANNO
               MOVE 1 TO MESE
             END-IF
           END-IF.
           MOVE TBLPA-INIZIO-CONTEGGIO (SCA-J) TO GIORNO.

       SCA-CALCOLO-SCADENZA.

           if ws-tblpa-tipo-calcolo-gg-eff                                                                  | rososv 05/11/2012
              perform sca-calcolo-scadenza-eff                                                              | rososv 05/11/2012
              go sca-calcolo-importo                                                                        | rososv 05/11/2012
           end-if.                                                                                          | rososv 05/11/2012

            MOVE ANNO TO AA IN SCA-DATA (SCA-J).
      *    IF TBLPA-SCADENZA (SCA-J) > 29
      *      DIVIDE TBLPA-SCADENZA (SCA-J) BY 30 GIVING SCA-COMODO-MESE
      *      ADD SCA-COMODO-MESE MESE GIVING MM IN SCA-DATA (SCA-J)
      *      PERFORM SCA-CTRL-MESE THRU FINE-SCA-CTRL-MESE
      *      COMPUTE SCA-COMODO-GIORNO =
      *        TBLPA-SCADENZA (SCA-J) - SCA-COMODO-MESE * 30
      *      ADD SCA-COMODO-GIORNO GIORNO GIVING GG IN SCA-DATA (SCA-J)
      *      PERFORM SCA-CONTROLLO-GIORNO1
      *    ELSE

      *      DIVIDE TBLPA-SCADENZA (SCA-J) BY 30 GIVING SCA-COMODO-MESE           |DS 27/06/2005 12.16
      *      ADD SCA-COMODO-MESE MESE GIVING MM IN SCA-DATA (SCA-J)               |DS 27/06/2005 12.16
      *      COMPUTE SCA-COMODO-GIORNO =                                          |DS 27/06/2005 12.16
      *        TBLPA-SCADENZA (SCA-J) - SCA-COMODO-MESE * 30                      |DS 27/06/2005 12.16
      *      ADD SCA-COMODO-GIORNO GIORNO GIVING GG IN SCA-DATA (SCA-J)           |DS 27/06/2005 12.16
             DIVIDE TBLPA-SCADENZA (SCA-J) BY 30 GIVING SCA-COMODO-MESE1.                     |DS 27/06/2005 12.16
             COMPUTE SCA-COMODO-GIORNO =                                                   |DS 27/06/2005 12.16
               TBLPA-SCADENZA (SCA-J) - SCA-COMODO-MESE1 * 30                              |DS 27/06/2005 12.16
            DIVIDE SCA-COMODO-MESE1 BY 12 GIVING SCA-COMODO-ANNO.                            |DS 27/06/2005 12.16
            COMPUTE SCA-COMODO-MESE1 =                                                    |DS 27/06/2005 12.16
              SCA-COMODO-MESE1 - SCA-COMODO-ANNO * 12.                                       |DS 27/06/2005 12.16
            ADD SCA-COMODO-MESE1 MESE GIVING MM IN SCA-DATA (SCA-J)                     |DS 27/06/2005 12.16
            ADD SCA-COMODO-GIORNO GIORNO GIVING GG IN SCA-DATA (SCA-J)                  |DS 27/06/2005 12.16
            ADD SCA-COMODO-ANNO TO AA IN SCA-DATA (SCA-J)                               |DS 27/06/2005 12.16

            IF TBLPA-SCADENZA (SCA-J) > 29                                                |DS 27/06/2005 12.16
              PERFORM SCA-CONTROLLO-GIORNO1
            ELSE
             PERFORM SCA-CONTROLLO-GIORNO
            END-IF.

           IF TBLPA-INIZIO-CONTEGGIO (SCA-J) = 31
      *        MOVE 31 TO GG IN SCA-DATA (SCA-J)                                                         |ds 06/11/2009
                perform sca-ctrl-mese thru fine-sca-ctrl-mese                                             |ds 23/12/2009 12.42|ds 06/11/2009 12.19
                move sca-data(sca-j) to num-unsigned-int  convert                                         |ds 06/11/2009
                call "r_finemese" using num-unsigned-int                                                  |ds 06/11/2009
                                  giving ws-fine-mese                                                     |ds 06/11/2009
                if gg in sca-data(sca-j) <= ws-fine-gg                                                    |ds 06/11/2009
                   move ws-fine-mese       to sca-data(sca-j)                                             |ds 06/11/2009
      *** osv      else                                                                                     |ds 23/12/2009
      ***            if mm in sca-data(sca-j) < 12                                                          |ds 23/12/2009
      ***                  add 1 to          mm in sca-data(sca-j)                                          |ds 23/12/2009
      ***               else                                                                                |ds 23/12/2009
      ***                  move 1 to         mm in sca-data(sca-j)                                          |ds 23/12/2009
      ***                  add  1 to         aa in sca-data(sca-j)                                          |ds 23/12/2009
      ***            end-if                                                                                 |ds 23/12/2009
      ***            move sca-data(sca-j) to num-unsigned-int  convert                                      |ds 23/12/2009
      ***            call "r_finemese" using num-unsigned-int                                               |ds 23/12/2009
      ***                             giving ws-fine-mese                                                   |ds 23/12/2009
      ***  osv       move ws-fine-mese       to sca-data(sca-j)                                             |ds 23/12/2009
               end-if                                                                                    |ds 06/11/2009
           END-IF.
           IF TBLPA-INIZIO-CONTEGGIO (SCA-J) > 31  AND
              TBLPA-INIZIO-CONTEGGIO (SCA-J) < 60
             COMPUTE GG IN SCA-DATA (SCA-J) = 
             TBLPA-INIZIO-CONTEGGIO (SCA-J) - 31
             ADD 1 TO MM IN SCA-DATA (SCA-J)
           END-IF.
           IF SLITTAMENTO = "S"
             ADD MESI-SLITTAMENTO TO MM IN SCA-DATA (SCA-J)

      *** se addiziono il mese di slittamento e ho la scedenze a fine mese
      *** occorre riverificare quale la fine mese (se sono capitato su un mese di 31 da un mese di 30
      *** giorni devo spostare la scadenza al 31 del mese).
            if tblpa-inizio-conteggio (sca-j) = 31                                                         | rososv 02/11/2012
               move sca-data(sca-j) to num-unsigned-int  convert                                           | rososv 02/11/2012
               call "r_finemese" using num-unsigned-int                                                    | rososv 02/11/2012
                                giving ws-fine-mese                                                       | rososv 02/11/2012
               if gg in sca-data(sca-j) <= ws-fine-gg                                                      | rososv 02/11/2012
                  move ws-fine-mese       to sca-data(sca-j)                                               | rososv 02/11/2012
               end-if                                                                                      | rososv 02/11/2012
             end-if                                                                                        | rososv 02/11/2012
           END-IF.

      *** Se non è scadenza fine mese e se non è scadenza 30 giorni data fattura (commerciale)
      *** controllo data scadenza e se superiore alla fine reale del mese (esempio 31 febbraio)
      *** sposto il giorno al mese successivo
           if tblpa-inizio-conteggio(sca-j) not = 31                                           |ds 18/01/2010
                divide tblpa-scadenza(sca-j) by 30 giving importo0                                |ds 18/01/2010
                remainder ws-resto                                                         |ds 18/01/2010
             if ws-resto not = 0                                                               |ds 18/01/2010
                perform sca-ctrl-mese thru fine-sca-ctrl-mese                               |ds 18/01/2010
                move sca-data(sca-j) to num-unsigned-int  convert                           |ds 18/01/2010
                call "r_finemese" using num-unsigned-int                                    |ds 18/01/2010
                                giving ws-fine-mese                                         |ds 18/01/2010
                if sca-data(sca-j) > ws-fine-mese                                           |ds 18/01/2010
                   compute  gg in sca-data(sca-j) = 
                            gg in sca-data(sca-j) - ws-fine-gg     |ds 18/01/2010
                      if mm in sca-data(sca-j) < 12                                           |ds 18/01/2010
                         add 1 to   mm in sca-data(sca-j)                    |ds 18/01/2010
                       else                                                                   |ds 18/01/2010
                         add   1 to   aa in sca-data(sca-j)                    |ds 18/01/2010
                         move  1 to   mm in sca-data(sca-j)                    |ds 18/01/2010
                      end-if                                                                  |ds 18/01/2010
                  end-if                                                                      |ds 18/01/2010
            end-if                                                                            |ds 18/01/2010
           end-if.                                                                            |ds 18/01/2010

           PERFORM SCA-CONTROLLO-MESE.

       SCA-CALCOLO-IMPORTO.
           IF TBLPA-TIPO-IMPORTI (SCA-J) = "A"
             MOVE TBLPA-IMPORTO (SCA-J) TO SCA-IMPORTO (SCA-J)
             MOVE TBLPA-IMPORTO (SCA-J) TO SCA-IMPORTO-VA (SCA-J)
             IF SCA-TIPO-IMPORTI (1) = "A" AND 
                SCA-VALORE-EURO (SCA-J) NOT = 0 |ds 16.03.04
               MOVE SCA-VALORE-EURO (SCA-J) TO SCA-IMPORTO (SCA-J)
             END-IF
             PERFORM ASSEGNA-CAMBIO-LIRE
             GO FINE-SCA-CALCOLO-IMPORTO
           END-IF.
           IF TBLPA-TIPO-IMPORTI (SCA-J) = "C"
             IF DIVISA-LIRE
               COMPUTE IMPORTO-DIVISA =
                 SCA-IMPORTO-FATTURA / TBLPA-NUMERO-SCADENZE
                 ON SIZE ERROR MOVE 0 TO SCA-IMPORTO (SCA-J)
               END-COMPUTE
               MOVE IMPORTO-DIVISA TO SCA-IMPORTO (SCA-J)
             ELSE
               COMPUTE SCA-IMPORTO (SCA-J) =
                 SCA-IMPORTO-FATTURA / TBLPA-NUMERO-SCADENZE
                 ON SIZE ERROR MOVE 0 TO SCA-IMPORTO (SCA-J)
               END-COMPUTE
             END-IF
             COMPUTE SCA-IMPORTO-VA (SCA-J) =
                 SCA-IMPORTO-FATTURA-VA / TBLPA-NUMERO-SCADENZE
                   ON SIZE ERROR MOVE 0 TO SCA-IMPORTO-VA (SCA-J)
             END-COMPUTE
             PERFORM ASSEGNA-CAMBIO-LIRE
             GO FINE-SCA-CALCOLO-IMPORTO
           END-IF.
           IF DIVISA-LIRE
             COMPUTE IMPORTO-DIVISA = SCA-IMPORTO-FATTURA *
                 TBLPA-IMPORTO (SCA-J) / 100
             MOVE IMPORTO-DIVISA TO SCA-IMPORTO (SCA-J)
           ELSE
             COMPUTE SCA-IMPORTO (SCA-J) = SCA-IMPORTO-FATTURA *
                 TBLPA-IMPORTO (SCA-J) / 100
           END-IF.
           COMPUTE SCA-IMPORTO-VA (SCA-J) = SCA-IMPORTO-FATTURA-VA *
               TBLPA-IMPORTO (SCA-J) / 100
               ON SIZE ERROR MOVE 0 TO SCA-IMPORTO-VA (SCA-J)
           END-COMPUTE.
           PERFORM ASSEGNA-CAMBIO-LIRE.
       FINE-SCA-CALCOLO-IMPORTO.
           EVALUATE DECIMALI
           WHEN 0
             MOVE SCA-IMPORTO-VA (SCA-J) TO IMPORTO0
             MOVE IMPORTO0 TO SCA-IMPORTO-VA (SCA-J)
           WHEN 1
             MOVE SCA-IMPORTO-VA (SCA-J) TO IMPORTO1
             MOVE IMPORTO1 TO SCA-IMPORTO-VA (SCA-J)
           WHEN 2
             MOVE SCA-IMPORTO-VA (SCA-J) TO IMPORTO2
             MOVE IMPORTO2 TO SCA-IMPORTO-VA (SCA-J)
           WHEN 3
             MOVE SCA-IMPORTO-VA (SCA-J) TO IMPORTO3
             MOVE IMPORTO3 TO SCA-IMPORTO-VA (SCA-J)
           END-EVALUATE.
           IF TBLPA-DETRAZIONE (SCA-J) = "S"
             SUBTRACT SCA-IMPORTO (SCA-J) FROM SCA-IMPORTO-FATTURA
             ADD SCA-IMPORTO (SCA-J) TO IMPORTO-DETRAZIONE
             SUBTRACT SCA-IMPORTO-VA (SCA-J) FROM SCA-IMPORTO-FATTURA-VA
             ADD SCA-IMPORTO-VA (SCA-J) TO IMPORTO-DETRAZIONE-VA
           END-IF.
       FINE-SCA-TABELLA.

       SCA-CTRL-MESE.
           IF MM IN SCA-DATA (SCA-J) > 12
             ADD 1 TO AA IN SCA-DATA (SCA-J)
             SUBTRACT 12 FROM MM IN SCA-DATA (SCA-J)
             GO SCA-CTRL-MESE
           END-IF.
       FINE-SCA-CTRL-MESE.
       
       SCA-CONTROLLO-GIORNO1.
           IF GG IN SCA-DATA (SCA-J) > 31
             SUBTRACT 30 FROM GG IN SCA-DATA (SCA-J)
             ADD 1 TO MM IN SCA-DATA (SCA-J)
           END-IF.

       SCA-CONTROLLO-GIORNO.
      *** osv   move sca-data(sca-j) to num-unsigned-int  convert                                   |ds 23/12/2009
      ***    call "r_finemese" using num-unsigned-int                                               |ds 23/12/2009
      ***                        giving ws-fine-mese                                                |ds 23/12/2009
      *** osv   IF GG IN SCA-DATA (SCA-J) > ws-fine-gg                                              |ds 23/12/2009
           if mm in sca-data (sca-j) = 02 and 
              gg in sca-data (sca-j) > 28                                 |ds 18/01/2010 11.41
             move sca-data (sca-j) to ws-data-num                                                         |ds 18/01/2010 12.11
             call "r_isannobisesto"  using  ws-data-num                                                   |ds 18/01/2010 11.41
                                    giving isannobisesto                                                 |ds 18/01/2010 11.41
           end-if                                                                                         |ds 18/01/2010 11.41
           IF GG IN SCA-DATA (SCA-J) > 31
             or (gg in sca-data (sca-j) > 28 and 
                 mm in sca-data (sca-j) = 02 and 
                 isannobisesto = k-false )|ds 18/01/2010 11.30
             or (gg in sca-data (sca-j) > 29 and 
                 mm in sca-data (sca-j) = 02)                             |ds 18/01/2010 11.30

             IF MM IN SCA-DATA(SCA-J) > 12
               SUBTRACT 12 FROM MM IN SCA-DATA(SCA-J)
               ADD 1 TO AA IN SCA-DATA(SCA-J)
             END-IF

             IF MM IN SCA-DATA (SCA-J) = 01 OR 03 OR 05 OR 07 
                                            OR 08 OR 10 OR 12
               SUBTRACT 31 FROM GG IN SCA-DATA (SCA-J)
               ADD 1 TO MM IN SCA-DATA (SCA-J)
             ELSE
               IF MM IN SCA-DATA (SCA-J) = 04  OR  06  OR  09  OR  11
                 SUBTRACT 30 FROM GG IN SCA-DATA (SCA-J)
                 ADD 1 TO MM IN SCA-DATA (SCA-J)
               ELSE
                 IF MM IN SCA-DATA (SCA-J) = 02
                   IF AA IN SCA-DATA (SCA-J) = 2004  OR  2008  OR  
                                               2012  OR  2016
                     SUBTRACT 29 FROM GG IN SCA-DATA (SCA-J)
                     ADD 1 TO MM IN SCA-DATA (SCA-J)
                   ELSE
                     SUBTRACT 28 FROM GG IN SCA-DATA (SCA-J)
                     ADD 1 TO MM IN SCA-DATA (SCA-J)
                   END-IF
                 END-IF
               END-IF
             END-IF
      *
      *      SUBTRACT 30 FROM GG IN SCA-DATA (SCA-J)
      *      ADD 1 TO MM IN SCA-DATA (SCA-J)
      *
           END-IF.

       SCA-CONTROLLO-MESE.
           PERFORM SCA-DIVIDI-MESE 
                   UNTIL MM IN SCA-DATA (SCA-J) NOT > 12.
           IF MM IN SCA-DATA (SCA-J) = TBLPA-MESE1
             IF GG IN SCA-DATA (SCA-J) NOT < TBLPA-ESCLUSO-DAL-GIORNO1
               ADD 1 TO MM IN SCA-DATA (SCA-J) MESI-SLITTAMENTO
               MOVE TBLPA-GIORNO1 TO GG IN SCA-DATA (SCA-J)
             END-IF
           END-IF.
           IF MM IN SCA-DATA (SCA-J) = TBLPA-MESE2
             IF GG IN SCA-DATA (SCA-J) NOT < TBLPA-ESCLUSO-DAL-GIORNO2
               ADD 1 TO MM IN SCA-DATA (SCA-J) MESI-SLITTAMENTO
               MOVE TBLPA-GIORNO2 TO GG IN SCA-DATA (SCA-J)
             END-IF
           END-IF.
           PERFORM SCA-DIVIDI-MESE 
                   UNTIL MM IN SCA-DATA (SCA-J) NOT > 12.
           IF MM IN SCA-DATA (SCA-J) = TBLPA-MESE1
             IF GG IN SCA-DATA (SCA-J) NOT < TBLPA-ESCLUSO-DAL-GIORNO1
               ADD 1 TO MM IN SCA-DATA (SCA-J) MESI-SLITTAMENTO
               MOVE TBLPA-GIORNO1 TO GG IN SCA-DATA (SCA-J)
             END-IF
           END-IF.
           IF MM IN SCA-DATA (SCA-J) = TBLPA-MESE2
             IF GG IN SCA-DATA (SCA-J) NOT < TBLPA-ESCLUSO-DAL-GIORNO2
               ADD 1 TO MM IN SCA-DATA (SCA-J) MESI-SLITTAMENTO
               MOVE TBLPA-GIORNO2 TO GG IN SCA-DATA (SCA-J)
             END-IF
           END-IF.
           PERFORM SCA-DIVIDI-MESE 
                   UNTIL MM IN SCA-DATA (SCA-J) NOT > 12.
           IF GG IN SCA-DATA (SCA-J) > 30
             IF (MM IN SCA-DATA (SCA-J) = 4  OR  
                 MM IN SCA-DATA (SCA-J) = 6  OR
                 MM IN SCA-DATA (SCA-J) = 9  OR  
                 MM IN SCA-DATA (SCA-J) = 11)
              IF TBLPA-SCADENZA (SCA-J) > 29                                                              |ds 18/01/2010 12.24
                MOVE 30 TO GG IN SCA-DATA (SCA-J)                                                         |ds 18/01/2010 12.24
              ELSE                                                                                        |ds 18/01/2010 12.24
      *        MOVE 30 TO GG IN SCA-DATA (SCA-J)                                                         |ds 06/11/2009 12.34
              IF TBLPA-INIZIO-CONTEGGIO (SCA-J) = 0                                                      |ds 06/11/2009 9.49
                 MOVE 01 TO GG IN SCA-DATA (SCA-J)                                                        |ds 06/11/2009 12.34
                 ADD 1 TO MM IN SCA-DATA (SCA-J)                                                          |ds 06/11/2009 12.34
               ELSE                                                                                       |ds 06/11/2009 10.22
                 IF TBLPA-INIZIO-CONTEGGIO (SCA-J) = 31                                                   |ds 06/11/2009 9.49
                   MOVE 31 TO GG IN SCA-DATA (SCA-J)                                                      |ds 06/11/2009 12.34
                   ADD 1 TO MM IN SCA-DATA (SCA-J)                                                        |ds 06/11/2009 12.34
                 ELSE                                                                                     |ds 06/11/2009 11.10
                   MOVE 30 TO GG IN SCA-DATA (SCA-J)                                                      |ds 06/11/2009 12.34
                END-IF                                                                                   |ds 06/12/2009 11.10
               END-IF                                                                                     |ds 06/11/2009 9.50
              END-IF                                                                                      |ds 18/01/2010 12.24
             END-IF
           END-IF.
           IF GG IN SCA-DATA (SCA-J) > 28
             IF MM IN SCA-DATA (SCA-J) = 2
               MOVE 28 TO GG IN SCA-DATA (SCA-J)
      * verifica anno bisestile
               MOVE AA IN SCA-DATA (SCA-J) TO AAAA AAAAAA
               COMPUTE AAAA = AAAA / 4  END-COMPUTE
               COMPUTE AAAAAA = AAAAAA / 4  END-COMPUTE
               IF AAAA = AAAAAA
                 MOVE 29 TO GG IN SCA-DATA (SCA-J)
               END-IF
             END-IF
           END-IF.

       SCA-DIVIDI-MESE.
           SUBTRACT 12 FROM MM IN SCA-DATA (SCA-J).
           ADD 1 TO AA IN SCA-DATA (SCA-J).

       SCA-SOMMA.
           ADD SCA-IMPORTO (SCA-J) TO SCA-TOTALE.
           ADD SCA-IMPORTO-VA (SCA-J) TO SCA-TOTALE-VA.

       FINE-CALSCA.
           MOVE VARIABILI-VARSCA TO LIN-VARIABILI.
           GO FINE.
       ASSEGNA-CAMBIO-LIRE.
           IF CAMBIO-LIRE-88
             COMPUTE IMPORTO-DIVISA ROUNDED = SCA-IMPORTO-VA (SCA-J)
             MOVE IMPORTO-DIVISA TO SCA-IMPORTO-VA (SCA-J)
           END-IF.

      *** calcolo scadenze se tipo scadenza con giorni effettivi (non commerciali: un mese = 30 giorni)    
       sca-calcolo-scadenza-eff.                                                                            | rososv 05/11/2012
           if tblpa-inizio-conteggio(sca-j) = 31                                               |    pagamento fine mese
                move sca-data-documento to num-unsigned-int  convert                           |        sposto data di inizio calcolo a fine mese
                call "r_finemese" using num-unsigned-int                                       |
                                  giving ws-fine-mese                                          |
                move ws-fine-mese        to ws-data-inizio-calcolo                             |
            else                                                                               |    pagamento data fattura o a vista
                move sca-data-documento  to ws-data-inizio-calcolo                             |        data documento = data inizio calcolo
           end-if.                                                                             |
       
           if ws-data-inizio-calcolo = 0                                                                    | rososv 05/02/2013
              exit paragraph                                                                                | rososv 05/02/2013
           end-if.                                                                                          | rososv 05/02/2013
    
           move ws-data-inizio-calcolo           to madd-in-date.
           move tblpa-scadenza(sca-j)            to madd-in-days
           perform madd-adddays.
           move madd-out-date                    to sca-data(sca-j). 
    
           if tblpa-inizio-conteggio(sca-j) > 31  and
              tblpa-inizio-conteggio(sca-j) < 60
              compute gg in sca-data(sca-j) = 
              tblpa-inizio-conteggio(sca-j) - 31
              add 1 to mm in sca-data(sca-j)
           end-if.
           if slittamento = "S"
              add mesi-slittamento to mm in sca-data(sca-j)
           end-if.       

           perform sca-controllo-mese.    
    
      * COPY ROUTINES.
      *
      *
      *
      * routine richiamata da FINE
      *
      *
       fine-programma.
      *
      *
       abort-tblpa-missing.                                                                              | mxm 25/05/2009 13.07
      ***      set      msg-information-type          to  true                                             | rososv 26/07/2011 16:31:59
      ***      set      msg-ok-type                   to  true                                             | rososv 26/07/2011 16:32:00
      ***      set      msg-default-1                 to  true                                             | rososv 26/07/2011 16:32:00
      ***      move     spaces                        to  msg-text                                         | rososv 26/07/2011 16:32:00
      ***      move     "Codice pagamento mancante:"  to  msg-text-1                                       | rososv 26/07/2011 16:32:00
      ***      move     SCA-CODICE-PA    to  msg-text-2                                                    | rososv 26/07/2011 16:32:00
      ***      perform  msg-send-custom-message.                                                           | rososv 26/07/2011 16:32:01
      ***      call     "r_printstacktrace".                                                               | rososv 26/07/2011 10:03:06
      
      *
           copy     "al_tblpa.cpr".                                                                        | rososv 26/07/2011 10:02:55
           copy     "adddays.cpr".                                                                         | rososv 05/11/2012
      *
      *
