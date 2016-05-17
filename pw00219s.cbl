      $set sourceformat"free"

      *>===================================================================================
       identification division.
       program-id.  PW00219S.
      *>===================================================================================

      *>
      *>            --------------------------------------------------------
      *>                                 Versão do Sistema
      *>            --------------------------------------------------------
      *>

      *>===================================================================================
       working-storage section.
       01  ws-working-storage.
           03 ws-gp-versao-promax.
              05 filler                 pic x(05).
              05 ws-versao-promax       pic x(11).
              05 filler                 pic x(05).

      *>===================================================================================
       linkage section.
       01  lnk-versao.
           03 lnk-versao-sistema        pic x(11).

      *>===================================================================================
       procedure division using lnk-versao.

      *>===================================================================================
       0000-controle section.
       0000.
            move "=====11.23.00.00====="to ws-gp-versao-promax.
            move ws-versao-promax       to lnk-versao-sistema.
            move "=====11.23.00.00====="to ws-gp-versao-promax.
            move ws-versao-promax       to lnk-versao-sistema.
            move "=====11.23.00.00====="to ws-gp-versao-promax.
            move ws-versao-promax       to lnk-versao-sistema.
            move "=====11.23.00.00====="to ws-gp-versao-promax.
            move ws-versao-promax       to lnk-versao-sistema.
            move "=====11.23.00.00====="to ws-gp-versao-promax.
            move ws-versao-promax       to lnk-versao-sistema.

       0000-saida.
            exit program
            stop run.
