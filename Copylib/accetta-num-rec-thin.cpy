      ***---
       ACCETTA-NUM-REC-THIN.
           accept num-rec-thin-clientx
                  from environment "NUM_REC_THIN_CLIENT".
           move num-rec-thin-clientx to num-rec-thin-clientz convert.
           inspect num-rec-thin-clientz
                   replacing leading x"20" by x"30".
           move num-rec-thin-clientz to num-rec-thin-client.
