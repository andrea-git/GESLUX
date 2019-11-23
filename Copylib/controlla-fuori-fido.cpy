
      ***---
       CONTROLLA-FUORI-FIDO.
LUBEXX     if tutto-ok
LUBEXX        initialize calfido-linkage 
LUBEXX                   replacing numeric data by zeroes
LUBEXX                        alphanumeric data by spaces
LUBEXX        move cli-codice to link-cli-codice
LUBEXX        call   "calfido" using calfido-linkage
LUBEXX        cancel "calfido"
LUBEXX        compute scoperto = saldo + Sum     +
LUBEXX                           effetti-rischio + ordini-in-essere
              if cli-gestione-fido-si
      *****           move cli-piva to sf-piva
      *****           read sitfin no lock                   
      *****                invalid move 0 to sf-fido-max
      *****           end-read                   
                 if tcl-fido-nuovo-si            
                    compute fido-tmp = cli-fido |sf-lince
                 else
                    compute tot-fido = cli-fido |sf-lince
                 end-if
              else
                 if tcl-fido-nuovo-si
                    compute fido-tmp = cli-fido
                 else
                    compute tot-fido = cli-fido + 
                                       cli-pfa  + 
                                       cli-fidejussione
                 end-if
              end-if  
              if tcl-fido-nuovo-si  
                 move 0 to fido-usato
                 if cli-fidejussione > 0

                    if cli-grade > 0
                       move spaces to gra-codice
                       read grade no lock
                            invalid move 0 to Sum
                        not invalid
                            perform varying idx from 1 by 1 
                                      until idx > 20
                               if gra-da(idx) <= cli-grade and
                                  gra-a(idx)  >= cli-grade
                                  move gra-perce(idx) to Sum
                                  exit perform
                               end-if
                            end-perform
                        end-read
                        if Sum > 0
                           compute cli-fidejussione = 
                                   cli-fidejussione * Sum / 100
                        end-if
                    end-if

                    compute fido-usato = 
                            cli-fidejussione +
                            cli-fido-extra   +
                            cli-pfa
                 else
                    if tge-blocco-fido < cli-fido
                       compute fido-usato = tge-blocco-fido + cli-pfa
                    else
                       compute fido-usato = fido-tmp + cli-pfa
                    end-if
                 end-if
                 if scoperto > fido-usato
                    subtract fido-usato from scoperto giving como-numero
LUBEXX              move como-numero to como-edit
                    set errori to true
LUBEXX              display message
LUBEXX                      "ATTENZIONE!!!!"
LUBEXX               x"0d0a""CLIENTE FUORI FIDO DI:  " como-edit
LUBEXX               x"0d0a""IMPOSSIBILE REGISTRARE L'ORDINE!"
LUBEXX                        title tit-err
LUBEXX                         icon 2
LUBEXX           end-if
              else
LUBEXX           if scoperto > tot-fido
                    if Sum <= cli-fido-extra
LUBEXX                 close    clienti
LUBEXX                 open i-o clienti
LUBEXX                 read clienti no lock invalid continue end-read
LUBEXX                 subtract Sum from cli-fido-extra
LUBEXX                 rewrite cli-rec invalid continue end-rewrite
LUBEXX                 close clienti
LUBEXX                 open input clienti
LUBEXX                 read clienti no lock invalid continue end-read
                    else
                       compute como-numero = scoperto - tot-fido
                       if como-numero > cli-fido-extra
LUBEXX                    set errori to true
                          subtract cli-fido-extra from como-numero
LUBEXX                    move como-numero to como-edit
LUBEXX                    display message
LUBEXX                            "ATTENZIONE!!!!"
LUBEXX                     x"0d0a""CLIENTE FUORI FIDO DI:  " como-edit
LUBEXX                     x"0d0a""IMPOSSIBILE REGISTRARE L'ORDINE!"
LUBEXX                              title tit-err
LUBEXX                               icon 2
LUBEXX                 else
LUBEXX                    close    clienti
LUBEXX                    open i-o clienti
LUBEXX                    read clienti no lock invalid continue end-read
LUBEXX                    subtract como-numero from cli-fido-extra
LUBEXX                    rewrite cli-rec invalid continue end-rewrite
LUBEXX                    close clienti
LUBEXX                    open input clienti
LUBEXX                    read clienti no lock invalid continue end-read
                       end-if
LUBEXX              end-if
LUBEXX           end-if
              end-if
LUBEXX     end-if. 
