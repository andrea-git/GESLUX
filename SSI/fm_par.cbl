*----------------------------------------------------------------------------
* fm_par - componente server i-o fisico par
*----------------------------------------------------------------------------
* Ticket 1460 - Eliminato "punto niente"                                                           |ds 23/01/2009 11.14
*               Inizializzato nuovo campo PAR-CODICE-IVS                                           |DS 23/01/2009 11.55
*----------------------------------------------------------------------------
* tk.2116 - Cruscotti amministrativi NGS                                                           |mc 29/09/2009 17.13
*           sistemazioni di TUTTI i campi numerici per valori blank                                |mc 29/09/2009 17.13
*----------------------------------------------------------------------------
*---------------------------------------------
* tk 3875 - Alcuni movimenti nel dettaglio scadenze del cuscotto                                     | scamar 11/05/2011 09:49:53
*           dello scadenzario non espongono il progressivo di primanota,
*           nonostante la scadenza sia generata da un movimento contabile
*           . modifiche per nuova chiave par-codice2
*           . rollback
*           . modifiche per obsoletaggio copy selpar
*
*----------------------------------
*
*
identification division.
program-id. fm_par.

environment division.
configuration section.                                                                                                                           

source-computer. Lebedev_MESM.
object-computer. Lebedev_MESM.
special-names.

input-output section.                                                                                                                                    
file-control.

***    copy     "selpar"                                                                             | scamar 11/05/2011 12:25:36
***             replacing  == "PAR" == by == ws-file-name ==                                         | scamar 11/05/2011 12:25:37
***                        == stato == by == file-status                                             | scamar 11/05/2011 12:25:37
***                        lock is manual with lock on multiple records ==.                          | scamar 11/05/2011 12:25:38         

    copy     "selxrollback.def".                                                                     | scamar 11/05/2011 12:28:16
             record key             Key-0 = par-codice      of data-record                           | scamar 11/05/2011 12:28:16
             alternate record  key  key-1 = par-codice-pnr  of data-record                           | scamar 11/05/2011 13:52:57
                                            par-codice      of data-record                           | scamar 11/05/2011 12:28:17
             alternate record  key  key-2 = par-codice-pas  of data-record                           | scamar 11/05/2011 13:52:58
                                            par-codice      of data-record.                          | scamar 11/05/2011 12:28:17

                                                                                                                                                   



data division.
file section.                                                                       

$xfd file=par                                                                                                                                     

***fd par.                                                  |ds 23/01/2009 11.26                     | scamar 11/05/2011 13:48:35


    copy     "fdx.def".                                                                              | scamar 11/05/2011 13:49:29
    
    copy     "par.dpb"                                                                             |DS 23/01/2009 11.26
             replacing     == dpb-par == by == data-record ==                                        | scamar 11/05/2011 13:50:14
***             replacing  == resto-record-par == by == data-data ==.                                | scamar 11/05/2011 13:50:06
                           == resto-record-par == by == data-data ==.                                | scamar 11/05/2011 13:50:18

*----------------------------------------------------------------
*
*
working-storage section.
*
*
01  work-area.                                                                                                              
    05  ws-record-id     pic x(03) value "  ".
*
*
    copy     "buffer.def" replacing == ws-buffer == by == new-data-record ==.
*
*
*
    copy     "fm.skl"
***             replacing  == ws-file-key == by == par-codice ==                                     | scamar 11/05/2011 13:51:33
             replacing  == ws-file-key == by == par-codice of data-record ==.                        | scamar 11/05/2011 13:53:17
***                        == data-record == by == dpb-par ==       |DS 23/01/2009 11.27             | scamar 11/05/2011 13:51:27
***                        == data-file ==   by == par ==                                            | scamar 11/05/2011 13:51:27
***                        == key-1 ==       by == par-codice ==.                                    | scamar 11/05/2011 13:51:28
*
*
*
*----------------------------------------------------------------
*
* custom routines
*
set-file-name.
    move     "fm_par"                to  ws-prog-id.
    set      entity-par              to  true.
    move     "par"                   to  ws-file-name.
main-start-not-less.
***    if       file-start-index     equal  1   start    par  key not less  par-codice else          | scamar 11/05/2011 12:31:10
***    if       file-start-index     equal  2   start    par  key not less  par-codice1.             | scamar 11/05/2011 12:31:10
    
    if       file-start-index     equal  1   start    data-file  key not less  key-0 else                  | scamar 11/05/2011 12:33:25
    if       file-start-index     equal  2   start    data-file  key not less  key-1 else                  | scamar 11/05/2011 12:33:25
    if       file-start-index     equal  3   start    data-file  key not less  key-2.                      | scamar 11/05/2011 12:33:25
    
main-start-greater.                                                               
***    if       file-start-index     equal  1   start    par  key greater  par-codice else           | scamar 11/05/2011 12:34:05
***    if       file-start-index     equal  2   start    par  key greater  par-codice1.              | scamar 11/05/2011 12:34:05
    
    if       file-start-index     equal  1   start    data-file  key greater  key-0 else
    if       file-start-index     equal  2   start    data-file  key greater  key-1 else
    if       file-start-index     equal  3   start    data-file  key greater  key-2.
    
 main-start-not-greater.                                                            
***    if       file-start-index     equal  1   start    par  key not greater  par-codice else       | scamar 11/05/2011 12:35:30
***    if       file-start-index     equal  2   start    par  key not greater  par-codice1.          | scamar 11/05/2011 12:35:31
    
    if       file-start-index     equal  1   start    data-file  key not greater  key-0 else               | scamar 11/05/2011 12:35:33
    if       file-start-index     equal  2   start    data-file  key not greater  key-1 else               | scamar 11/05/2011 12:35:33
    if       file-start-index     equal  3   start    data-file  key not greater  key-2.                   | scamar 11/05/2011 12:35:33
        
    
 main-start-less.
***    if       file-start-index     equal  1   start    par  key less  par-codice else              | scamar 11/05/2011 12:35:49
***    if       file-start-index     equal  2   start    par  key less  par-codice1.                 | scamar 11/05/2011 12:35:49
    
    if       file-start-index     equal  1   start    data-file  key less  key-0 else                      | scamar 11/05/2011 12:36:28
    if       file-start-index     equal  2   start    data-file  key less  key-1 else                      | scamar 11/05/2011 12:36:28
    if       file-start-index     equal  3   start    data-file  key less  key-2.                          | scamar 11/05/2011 12:36:29
*
*
set-record-id.
check-record-type.
data-patch-in.
data-patch-out.
    if par-codice-ivs not numeric                                                                  |DS 23/01/2009 11.56
       move 0 to par-codice-ivs                                                                    |DS 23/01/2009 11.56
    end-if.                                                                                        |DS 23/01/2009 11.56
*

    if    par-data-registrazione-num is not numeric                                                |mc 29/09/2009 17.15
          move  zero      to par-data-registrazione-num.                                           |mc 29/09/2009 17.15
*
    if    par-numero-protocollo is not numeric                                                     |mc 29/09/2009 17.15
          move  zero      to par-numero-protocollo.                                                |mc 29/09/2009 17.15
*
    if    par-data-documento-num is not numeric                                                    |mc 29/09/2009 17.15
          move  zero      to par-data-documento-num.                                               |mc 29/09/2009 17.15
*
    if    par-numero-documento is not numeric                                                      |mc 29/09/2009 17.15
          move  zero      to par-numero-documento.                                                 |mc 29/09/2009 17.15
*
    if    par-codice-ivs is not numeric                                                            |mc 29/09/2009 17.15
          move  zero      to par-codice-ivs.                                                       |mc 29/09/2009 17.15
*
    if    par-importo is not numeric                                                               |mc 29/09/2009 17.15
          move  zero      to par-importo.                                                          |mc 29/09/2009 17.15
*
    if    par-importo-va is not numeric                                                            |mc 29/09/2009 17.15
          move  zero      to par-importo-va.                                                       |mc 29/09/2009 17.15
*

set-lookup-key.
    move     file-lookup-key      to  par-codice.

move-lookup-desc.
    move     spaces               to  file-lookup-desc.
    string   par-descrizione1     delimited by "  "
             " "
             par-descrizione2     delimited by size
                                into  file-lookup-desc.
create-views.
drop-views.

*----------------------------------------------------------------


