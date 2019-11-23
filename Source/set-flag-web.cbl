       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-flag-web.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare il flag WEB a false per certi articoli

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag WEB".

      * FILE-STATUS
       77  status-articoli           pic xx.

      * VARIABILI

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                set errori to true
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
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
                move   "articoli"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o articoli allowing readers
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
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open i-o articoli.

      ***---
       ELABORAZIONE.
           move low-value to art-rec.
           perform until 1 = 2 
              read articoli next at end exit perform end-read
              if art-codice = 72     or =
                              80     or =
                              116    or =
                              117    or =
                              118    or =
                              119    or =
                              121    or =
                              122    or =
                              123    or =
                              124    or =
                              126    or =
                              127    or =
                              130    or =
                              134    or =
                              136    or =
                              137    or =
                              140    or =
                              142    or =
                              143    or =
                              146    or =
                              149    or =
                              151    or = 
                              153    or = 
                              155    or = 
                              160    or = 
                              162    or = 
                              163    or = 
                              169    or = 
                              170    or = 
                              172    or = 
                              176    or = 
                              177    or = 
                              178    or = 
                              179    or = 
                              181    or = 
                              182    or = 
                              183    or = 
                              185    or = 
                              186    or = 
                              187    or = 
                              193    or = 
                              196    or = 
                              197    or = 
                              202    or = 
                              208    or = 
                              217    or = 
                              229    or = 
                              234    or = 
                              235    or = 
                              240    or = 
                              241    or = 
                              248    or = 
                              249    or = 
                              250    or = 
                              251    or = 
                              252    or = 
                              254    or = 
                              255    or = 
                              256    or = 
                              257    or = 
                              258    or = 
                              259    or = 
                              260    or = 
                              261    or = 
                              262    or = 
                              263    or = 
                              297    or = 
                              306    or = 
                              323    or = 
                              330    or = 
                              333    or = 
                              368    or = 
                              369    or = 
                              371    or = 
                              373    or = 
                              377    or = 
                              379    or = 
                              393    or = 
                              394    or = 
                              396    or = 
                              397    or = 
                              399    or = 
                              401    or = 
                              402    or = 
                              416    or = 
                              420    or = 
                              448    or = 
                              456    or = 
                              460    or = 
                              465    or = 
                              471    or = 
                              477    or = 
                              481    or = 
                              488    or = 
                              492    or = 
                              498    or = 
                              547    or = 
                              574    or = 
                              613    or = 
                              616    or = 
                              617    or = 
                              618    or = 
                              624    or = 
                              625    or = 
                              626    or = 
                              629    or = 
                              634    or = 
                              635    or = 
                              636    or = 
                              642    or = 
                              643    or = 
                              644    or = 
                              645    or = 
                              647    or = 
                              659    or = 
                              662    or = 
                              665    or = 
                              666    or = 
                              667    or = 
                              670    or = 
                              685    or = 
                              691    or = 
                              695    or = 
                              696    or = 
                              697    or = 
                              698    or = 
                              699    or = 
                              701    or = 
                              710    or = 
                              719    or = 
                              747    or = 
                              758    or = 
                              760    or = 
                              844    or = 
                              850    or = 
                              852    or = 
                              855    or = 
                              865    or = 
                              868    or = 
                              869    or = 
                              883    or = 
                              885    or = 
                              887    or = 
                              893    or = 
                              894    or = 
                              916    or = 
                              1139   or = 
                              1140   or = 
                              1141   or = 
                              1142   or = 
                              1143   or = 
                              1144   or = 
                              1146   or = 
                              1148   or = 
                              1149   or = 
                              1150   or = 
                              1158   or = 
                              1174   or = 
                              1175   or = 
                              1176   or = 
                              1177   or = 
                              1179   or = 
                              1402   or = 
                              1580   or = 
                              1594   or = 
                              1596   or = 
                              1598   or = 
                              1599   or = 
                              1601   or = 
                              1602   or = 
                              1607   or = 
                              1618   or = 
                              1646   or = 
                              1654   or = 
                              1657   or = 
                              1658   or = 
                              1672   or = 
                              1673   or = 
                              1676   or = 
                              1700   or = 
                              1754   or = 
                              1759   or = 
                              1760   or = 
                              1776   or = 
                              1783   or = 
                              1788   or = 
                              1791   or = 
                              1792   or = 
                              1793   or = 
                              1794   or = 
                              1797   or = 
                              1801   or = 
                              1802   or = 
                              1803   or = 
                              1804   or = 
                              1806   or = 
                              1807   or = 
                              1808   or = 
                              1809   or = 
                              1811   or = 
                              1812   or = 
                              1813   or = 
                              1814   or = 
                              1815   or = 
                              1816   or = 
                              1817   or = 
                              1819   or = 
                              1820   or = 
                              1822   or = 
                              1823   or = 
                              1824   or = 
                              1825   or = 
                              1830   or = 
                              1831   or = 
                              1834   or = 
                              1836   or = 
                              1840   or = 
                              1842   or = 
                              1845   or = 
                              1848   or = 
                              1850   or = 
                              1853   or = 
                              1855   or = 
                              1857   or = 
                              1858   or = 
                              1863   or = 
                              1868   or = 
                              1873   or = 
                              1874   or = 
                              1879   or = 
                              1882   or = 
                              1884   or = 
                              1886   or = 
                              1893   or = 
                              1901   or = 
                              1902   or = 
                              1903   or = 
                              1931   or = 
                              1961   or = 
                              2039   or = 
                              2041   or = 
                              2059   or = 
                              2071   or = 
                              2077   or = 
                              2082   or = 
                              2116   or = 
                              2138   or = 
                              2149   or = 
                              2150   or = 
                              2156   or = 
                              2158   or = 
                              2159   or = 
                              2160   or = 
                              2171   or = 
                              2173   or = 
                              2175   or = 
                              2195   or = 
                              2196   or = 
                              2197   or = 
                              2200   or = 
                              2232   or = 
                              2253   or = 
                              2254   or = 
                              2309   or = 
                              2323   or = 
                              2432   or = 
                              2481   or = 
                              2527   or = 
                              2529   or = 
                              2532   or = 
                              2539   or = 
                              2540   or = 
                              2542   or = 
                              2543   or = 
                              2544   or = 
                              2545   or = 
                              2546   or = 
                              2548   or = 
                              2550   or = 
                              2568   or = 
                              2579   or = 
                              2592   or = 
                              2639   or = 
                              2640   or = 
                              2716   or = 
                              2717   or = 
                              2787   or = 
                              2822   or = 
                              2823   or = 
                              2952   or = 
                              3169   or = 
                              3174   or = 
                              3189   or = 
                              3197   or = 
                              3455   or = 
                              3458   or = 
                              3459   or = 
                              3460   or = 
                              3472   or = 
                              3602   or = 
                              3884   or = 
                              3885   or = 
                              3887   or = 
                              3890   or = 
                              4094   or = 
                              4095   or = 
                              4146   or = 
                              4154   or = 
                              4167   or = 
                              4279   or = 
                              4407   or = 
                              4408   or = 
                              4410   or = 
                              4481   or = 
                              4608   or = 
                              4626   or = 
                              4684   or = 
                              4693   or = 
                              4697   or = 
                              4699   or = 
                              4743   or = 
                              4744   or = 
                              4771   or = 
                              4785   or = 
                              4791   or = 
                              4792   or = 
                              4794   or = 
                              4796   or = 
                              4808   or = 
                              4865   or = 
                              4866   or = 
                              4867   or = 
                              4868   or = 
                              4869   or = 
                              4870   or = 
                              4871   or = 
                              4877
                 set art-web-no to true    
                 rewrite art-rec invalid continue end-rewrite
              end-if
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.
