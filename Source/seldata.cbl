       identification division.
       program-id.     seldata2 is initial.
      ************************
       working-storage section.
      ************************
           copy "acugui.def".
           copy "utydata.def".
      *    copy "crtvars.def".

      *MODIFICA(ANDREA)
       01  EVENT-STATUS
           IS SPECIAL-NAMES EVENT STATUS.
           03  EVENT-TYPE                      PIC X(4) COMP-X.
           03  EVENT-WINDOW-HANDLE             HANDLE OF WINDOW.
           03  EVENT-CONTROL-HANDLE            HANDLE.
           03  EVENT-CONTROL-ID                PIC XX COMP-X.
           03  EVENT-DATA-1                    SIGNED-SHORT.
           03  EVENT-DATA-2                    SIGNED-LONG.
           03  EVENT-ACTION                    PIC X COMP-X.

       01  SCREEN-CONTROL
           IS SPECIAL-NAMES SCREEN CONTROL.
           03  ACCEPT-CONTROL                  PIC 9.
           03  CONTROL-VALUE                   PIC 999.
           03  CONTROL-HANDLE                  HANDLE.
           03  CONTROL-ID                      PIC XX COMP-X.
      *FINE MODIFICA

       01  crt_status
           is special-names crt status pic 999.
           88 k_tab                    value  9.
           88 k_ret                    value 13.
           88 k_esc                    value 27.
           88 k_event                  value W-EVENT.

       01  status_val                  pic 99 comp-1.
       01  num_param                   pic 99 comp-1.
       01  calling_program             pic x(25).

       01  seldata_window              handle of window.
       01  small_font                  handle of font.

       01  nuovo_giorno                pic xx.

       01  giorno_inizio_mese          pic 9.
       01  giorni_mese                 pic 99.

       01  idx                         pic 99.
       01  idx1                        pic 99.

       01  nome_mese                   pic x(15).
       01  anno                        pic 9(4).
       01  mesi.
           03                          pic x(15) value "Gennaio".
           03                          pic x(15) value "Febbraio".
           03                          pic x(15) value "Marzo".
           03                          pic x(15) value "Aprile".
           03                          pic x(15) value "Maggio".
           03                          pic x(15) value "Giugno".
           03                          pic x(15) value "Luglio".
           03                          pic x(15) value "Agosto".
           03                          pic x(15) value "Settembre".
           03                          pic x(15) value "Ottobre".
           03                          pic x(15) value "Novembre".
           03                          pic x(15) value "Dicembre".
       01  mese redefines mesi         pic x(15) occurs 12.

       01  ws_date.
           03 cur_year                 pic 9999.
           03 cur_month                pic 99.
           03 cur_day                  pic 99.
       01  tmp_date.
           03 tmp_year                 pic 9999.
           03 tmp_month                pic 99.
           03 tmp_day                  pic 99.

       01                              pic 9 value 0.
           88 dentro_giorni            value 1, false 0.
       01                              pic 9 value 0.
           88 ProgrammaPrincipale      value 1, false 0.

      ************************
       linkage section.
      ************************
       01  cur_date                    pic x(8).
       01  giorno_settimana            pic 9.
      ************************
       screen section.
      ************************
       01  seldata_screen.
           03 frame            line            1.5
                               col             2
                               size            30      cells
                               lines           15      cells
                               title           "&Data"
                               engraved
                               .
           03 cb_mese
              combo-box        drop-list
                               unsorted
                               notify-selchange
                               line            3
                               col             3
                               size            19      cells
                               lines           13
                               value           nome_mese
                               event           CB-MESE-EVENT
                               .
           03 ef_anno
              entry-field      line            3
                               col             23
                               size            8       cells
                               lines           1.4     cells
                               value           anno
                               notify-change
                               auto-spin
                               min-val         1900
                               max-val         2099
                               event           EF-ANNO-EVENT
                               .
           03 gr_giorni
              grid             line            5
                               col             3
                               size            28      cells
                               lines           7
                               num-rows        7
                               vpadding        50
                               alignment       ("C", "C", "C", "C",
                                                "C", "C", "C")
                               column-dividers (0, 0, 0, 0, 0, 0)
                               row-dividers    0
                               display-columns (1, 5, 9, 13, 17, 21, 25)
                               cursor-color    80
                               cursor-frame-width      -1
                               column-headings
                               heading-color   80
                               before          GR-GIORNI-BEFORE
                               after           GR-GIORNI-AFTER
                               event           GR-GIORNI-EVENT
                               .
           03 btn_1
              push-button      line            17
                               col             11
                               size            10      cells
                               ok-button
                               .
           03 btn_2
              push-button      col             + 2
                               size            10      cells
                               cancel-button
                               title           "Annulla"
                               .

      ************************
       procedure division using cur_date, giorno_settimana.
      ************************
       MAIN-LOGIC.
           perform SETUP-TASTIERA.

           call "C$NARG" using num_param.
           if num_param = 0  set address of cur_date 
                               to address of ws_date
              else           move cur_date to ws_date
           end-if.
           set aaaa_mm_gg to true.
           call "utydata" using  test_validita formato_data ws_date
                          giving error_level.
           if e_invalid_date  accept ws_date from century-date  end-if.

           move cur_year        to anno.
           move mese(cur_month) to nome_mese.
           accept small_font from object "SMALL_FONT".

           accept seldata_window from window handle.
           if seldata_window = null
              set ProgrammaPrincipale to true
              display standard window background-low visible 0
           else
              set ProgrammaPrincipale to false
           end-if.
      
           display floating graphical window
                   color 257 background-low
                   title "Proprietà - Data"
                   control font small_font
                   lines 18 size 32
                   handle in seldata_window.

           display seldata_screen.
      *DARWIN
           modify btn_2, enabled = 0.
      *DARWIN fine

           if ProgrammaPrincipale
              modify btn_1, visible = 0
              modify btn_2, title   = "Chiudi"
           end-if.

           perform CARICA-MESI.
           perform INIZIALIZZA-GRIGLIA.
           perform AGGIORNA-GRIGLIA.

           perform until 1 = 2
              move 1  to accept_control
              accept seldata_screen  on exception continue  end-accept
              if k_event
                 if event-type = MSG-BEGIN-ENTRY
                    set k_ret to true
                 end-if
              end-if
              evaluate true
      *DARWIN
      * Disabilitato tasto annulla:
      *       when k_esc  exit perform
      *DARWIN fine
              when k_ret  move ws_date to cur_date
                          if num_param > 1
                             inquire gr_giorni,
                                     cursor-x in giorno_settimana
                          end-if
                          exit perform
              when k_tab  move 1 to accept_control
                          move 1 to control_value
              when other  move 1 to accept_control
              end-evaluate
           end-perform.

           perform RIPRISTINA-TASTIERA.
           destroy seldata_screen.
           destroy seldata_window.

           cancel "utydata".
           if k_ret  goback 1
              else   goback 0
           end-if.

      ***---
       CARICA-MESI.
           modify cb_mese, reset-list = 1.
           perform test after varying idx from 1 by 1 until idx = 12
              modify cb_mese, item-to-add = mese (idx)
           end-perform.
           modify cb_mese, value = nome_mese.

      ***---
       INIZIALIZZA-GRIGLIA.
           modify gr_giorni (1, 1), cell-data = "L".
           modify gr_giorni (1, 2), cell-data = "M".
           modify gr_giorni (1, 3), cell-data = "M".
           modify gr_giorni (1, 4), cell-data = "G".
           modify gr_giorni (1, 5), cell-data = "V".
           modify gr_giorni (1, 6), cell-data = "S".
           modify gr_giorni (1, 7), cell-data = "D".

      ***---
       AGGIORNA-GRIGLIA.
           set aaaa_mm_gg to true.
           move anno to cur_year.
           move ws_date to tmp_date.
           move 1 to tmp_day.
           call "utydata" using  dom_lun formato_data tmp_date
                                 giorno_inizio_mese
                          giving error_level.
           perform test after varying idx  from 2 by 1 until idx  = 7
                                after idx1 from 1 by 1 until idx1 = 7
              modify gr_giorni (idx, idx1), cell-data  = spaces
                                            cell-color = 513
           end-perform.
           if e_invalid_date  modify gr_giorni, enabled = 0
                              exit paragraph
              else            modify gr_giorni, enabled = 1
           end-if
           if tmp_month < 12  add  1 to tmp_month
                              call "utydata" using  sottr_giorni
                                                    formato_data
                                                    tmp_date
                                                    1
                                             giving error_level
                              move tmp_day to giorni_mese
              else            move 31      to giorni_mese
           end-if.
           if cur_day > giorni_mese
              move giorni_mese to cur_day
           end-if.

           move 2 to idx1.
           perform test after varying idx from 1 by 1 
                              until idx = giorni_mese
              modify gr_giorni (idx1, giorno_inizio_mese),
                     cell-data = idx
              if idx = cur_day
                 modify gr_giorni, cursor-y = idx1
                                   cursor-x = giorno_inizio_mese
              end-if
              if giorno_inizio_mese < 7  add  1 to giorno_inizio_mese
                 else                    move 1 to giorno_inizio_mese
                                         add  1 to idx1
              end-if
           end-perform.
           perform GR-GIORNI-AFTER.

      ***---
       CB-MESE-EVENT.
           evaluate event_type
           when NTF-SELCHANGE  move event_data_1 to cur_month
                               perform AGGIORNA-GRIGLIA
           end-evaluate.

      ***---
       EF-ANNO-EVENT.
           evaluate event_type
           when NTF-CHANGED  inquire ef_anno, value in anno
                             perform AGGIORNA-GRIGLIA
                             move event-action-continue to event-action
           end-evaluate.

      ***---
       GR-GIORNI-EVENT.
           evaluate event_type
           when MSG-BEGIN-ENTRY
                set event-action to event-action-terminate
           when MSG-GOTO-CELL
           when MSG-GOTO-CELL-MOUSE
           when MSG-GOTO-CELL-DRAG
                if not dentro_giorni
                   perform GR-GIORNI-BEFORE
                end-if
                inquire gr_giorni (event_data_2, event_data_1),
                                  cell_data in nuovo_giorno
                if nuovo_giorno = spaces
                   move event-action-fail to event-action
                else
                   move nuovo_giorno to cur_day convert
                end-if
           end-evaluate.

      ***---
       GR-GIORNI-BEFORE.
           set dentro_giorni to true.
           inquire gr_giorni, cursor-y in idx
                              cursor-x in idx1.
           modify  gr_giorni (idx, idx1), cell-color = 513.

      ***---
       GR-GIORNI-AFTER.
           set dentro_giorni to false.
           inquire gr_giorni, cursor-y in idx
                              cursor-x in idx1.
           modify  gr_giorni (idx, idx1), cell-color = 80.

      ***---
       SETUP-TASTIERA.
           call "C$KEYMAP" using 1, status-val.
           if status_val = 0
              display message box
                      "Impossibile rimappare la tastiera"
                      type is MB-OK
                      icon is MB-ERROR-ICON
              goback
           end-if.
           set environment "KEYSTROKE" to "Terminate=13 ^M"
                           "KEYSTROKE" to "Edit=Next Terminate=9 ^I"
                           "KEYSTROKE" to "Exception=27 ^[".

      ***---
       RIPRISTINA-TASTIERA.
           call "C$KEYMAP" using 0.

