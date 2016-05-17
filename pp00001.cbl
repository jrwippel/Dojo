      $set preprocess (htmlpp) endp
      $set sourceformat"free"

      *>===================================================================================
       identification division.
       program-id. PP00001.
      *>===================================================================================

      *>
      *>            --------------------------------------------------------
      *>                            Carrega/Atualização Linkage
      *>            --------------------------------------------------------
      *>

      *>===================================================================================
        environment division.
            special-names. decimal-point is comma.
      *>===================================================================================

       copy PCS00400.CPY.               *> NF
       copy PCS02202.CPY.               *> Cotacoes
       copy PCS09400.CPY.               *> Controle de Projetos
       copy PCS18700.CPY replacing ==(F187)== BY ==F187==. *> Controle Fechamento Estoque
       copy PCS26800.CPY.               *> Empresa Corporativa
       copy PCS30800.CPY.               *> Usuário-Perfil
       copy PCS33200.CPY.               *> Perfil-Módulo
       copy PCS38300.CPY.               *> Fechamento Financeiro
       copy PCS42200.CPY.               *> Fechamento Financeiro
       copy PCS89700.CPY.               *> Log divergencias
       copy PCS99800.CPY.               *> Interface 2V
       copy PCSI00400.CPY.              *> Índice NF

            select promax-ini assign to disk wid-promax-ini
                   organization is line sequential
                   access mode is sequential
                   file status is ws-resultado-acesso
                   lock mode is manual.

            select lep-log-erro-parametros assign to disk wid-log-erro-parametros
                   organization is line sequential
                   access mode is sequential
                   file status is ws-resultado-acesso.

            select logon assign to disk wid-logon
                   organization is indexed
                   access mode is dynamic                                                                                      *>T500
                   record key is lgn-session-id
                   alternate key is lgn-usuario with duplicates
                   file status is ws-resultado-acesso
                   lock mode is manual.

            select sublogon assign to disk wid-sublogon
                   organization is indexed
                   access mode is dynamic
                   record key is lns-sub-session-id
                   alternate key is lns-chave-2 = lns-session-id
                                                  lns-sub-session-id
                   file status is ws-resultado-acesso
                   lock mode is manual.

      *>===================================================================================
       data division.

       copy PCF00400.CPY.               *> NF
       copy PCF02202.CPY.               *> Cotacoes
       copy PCF09400.CPY.               *> Controle de Projetos
       copy PCF18700.CPY replacing ==(F187)== BY ==F187==. *> Controle Fechamento Estoque
       copy PCF26800.CPY.               *> Empresa Corporativa
       copy PCF30800.CPY.               *> Usuário-Perfil
       copy PCF33200.CPY.               *> Perfil-Módulo
       copy PCF38300.CPY.               *> Fechamento Financeiro
       copy PCF42200.CPY.               *> Interface 2V
       copy PCF89700.CPY.               *> Log divergencias
       copy PCF99800.CPY.               *> Parametros
       copy PCFI00400.CPY.              *> Índice NF
       fd   promax-ini.
       01   reg-promaxini               pic x(200).

       fd   lep-log-erro-parametros.
       01   lep-registro.
            03 lep-data                 pic x(010).
            03 lep-filler1              pic x(001).
            03 lep-hora                 pic x(008).
            03 lep-filler2              pic x(001).
            03 lep-descricao            pic x(200).

       fd   logon.
       01   lgn-registro.
            03 lgn-session-id           pic x(50).
            03 lgn-usuario              pic x(20).
            03 lgn-linkage              pic x(10000).

       fd   sublogon.
       01   lns-registro.
            03 lns-sub-session-id       pic x(50).
            03 lns-session-id           pic x(50).
            03 lns-linkage              pic x(10000).

      *>===================================================================================
       working-storage section.
       78   versao                      value "v".

       78  implantacao                             value "IMPLANTACAO".
       78  on-off-hide-url-sso                     value "hideurl.sso".
       78  on-off-vincula-ip                       value "ip-link.sso".

       copy PCW900.CPY.
       copy PCW9126.CPY.

       01  ws-campos-de-trabalho.
           03  ws-mensagem-aux                     pic x(200) value spaces.
           03  ws-dias-fechamento                  pic 9(03) value zeros.
           03  ws-dias-fechamento-z                pic z(03) value spaces.
           03  ws-contador                         pic 9(04) value zeros.
           03  ws-dia-limite-reorg                 pic 9(02) value zeros.
           03  ws-dias-anterior                    pic 9(09) value zeros.
           03  ws-dias                             pic s9(09) value 0.
           03  ws-dias-movimento                   pic 9(09) value 0.
           03  ws-dt-entrega-nf                    pic 9(08) value zeros.
           03  ws-data-inv-aux                     pic 9(08) value zeros.
           03  filler redefines ws-data-inv-aux.
               05  ws-ano-inv-aux                  pic 9(04).
               05  ws-mes-inv-aux                  pic 9(02).
               05  ws-dia-inv-aux                  pic 9(02).
           03  ws-chave-log                        pic 9(08) comp-x.
           03  ws-chave                            pic x(30) value spaces.
           03  ws-valor                            pic x(60) value spaces.
           03  ws-directory-name                   pic x(200) value spaces.
           03  ws-acesso-fecha-financ              pic 9(01) value zeros.
               88  acessa-fecha-financ                 value 1.
           03  ws-acesso-fecha-estoque             pic 9(01) value zeros.
               88  acessa-fecha-estoque                value 1.
           03  ws-numero-dias-fec-fin              pic 9(09) value zeros.
           03  ws-numero-dias-fec-est              pic 9(09) value zeros.
           03  ws-estado-revenda                   pic x(02) value spaces.
           03  ws-id-ctrl-aber-efd004              pic 9(01) value zeros.
               88  ws-ctrl-pd00400                     value 1.
           03  ws-id-ctrl-aber-efdi04              pic 9(01) value zeros.
               88  ws-ctrl-pdi0400                     value 1.
           03  ws-id-ctrl-aber-efd383              pic 9(01) value zeros.
               88  ws-ctrl-pd38300                     value 1.
           03  ws-nr-dias-retencao-2v              pic 9(08) value zeros.
           03  ws-nr-dias-retencao                 pic s9(08) value zeros.
           03  ws-qt-nr-mapas-exportados           pic 9(03) value zeros.
           03  ws-qt-nr-mapas-exportados-z         pic z(03) value spaces.
           03  ws-ip-cliente                       pic x(15) value spaces.
           03  ws-user-cliente                     pic x(15) value spaces.

       copy PCW0007.CPY.                                                   *>NB310804:23125

       01   cgi-input is external-form.
            copy PCW902.CPY.

      *>===================================================================================
       linkage section.
       copy PCW990.CPY.

      *>===================================================================================
       procedure division using lnk-par.

      *>===================================================================================
       0000-controle section.
       0000.
            perform 1000-inicializacao
            perform 2000-processamento
            perform 3000-finalizacao.
       0000-saida.
            exit program
            stop run.

      *>===================================================================================
       1000-inicializacao section.
       1000.
            if   lnk-inicio equal 7
                 perform 2999-output-logon-sublogon
                 exit section
            end-if
            accept ws-data-inv   from date yyyymmdd
            move ws-dia-inv            to lnk-dia-cpu
            move ws-mes-inv            to lnk-mes-cpu
            move ws-ano-inv            to lnk-ano-cpu
            move spaces                            to whs-mensagem
            perform 1050-carrega-ini
            if   not processamento-sem-erro
                 move whs-mensagem                 to lnk-auxiliar
                 perform 9000-mensagem-log
            end-if
            if   lnk-inicio equal 8  *>carregar SOMENTE, mas SOMENTE mesmo o INI
                 exit section
            end-if
            .
       1000-exit.
            exit.

      *>===================================================================================
       2000-processamento section.
       2000.
            evaluate lnk-inicio
               when 0 *> lnk-carrega-linkage
                   perform 2100-carrega-linkage
               when 1 *> lnk-criar-logon
               when 2 *> lnk-criar-logon *>Sem mensagens
                   perform 2200-criar-logon
               when 3 *> lnk-chama-rotina
                   perform 2300-chama-rotina
               when 5 *> lnk-carrega-linkage e DESTROI (logoff)
                   perform 2100-carrega-linkage
               when 6 *> lnk-carregar-parametros                            *>NB020304:12645
                   perform 2900-carrega-parametros                          *>NB020304:12645
               when 7 thru 8
                   continue
               when 9 *> lnk-criar-logon - animate
                   perform 2200-criar-logon
            end-evaluate.
       2000-exit.
            exit.

      *>===================================================================================
       2100-carrega-linkage section.
       2100.
           move 1                                  to lnk-status
           if  lnk-subsessionid > spaces
               perform 2110-ler-subsession
           else
               perform 2120-ler-session
           end-if
           if  lnk-sso-vincula-ip
               perform 2101-ler-ip-cliente
               if  lnk-sso2-ip-client <> ws-ip-cliente
                   move 1                          to lnk-status
               end-if
           end-if
           if  lnk-sso-vincula-user-ad
               perform 2102-ler-user-cliente
               if  lnk-sso2-user-ad <> ws-user-cliente
                   move 1                          to lnk-status
               end-if
           end-if
           if  lnk-animate = "1"
               move lnk-animpath to lnk-cgipath
           end-if
           .
       2100-exit.
           exit.

      *>===================================================================================
       2101-ler-ip-cliente section.
       2101.
           move spaces                             to ws-ip-cliente
           display "REMOTE_ADDR" upon environment-name
               on exception
                   add 4 to zero giving return-code
           end-display
           accept ws-ip-cliente from environment-value
               on exception add 4 to zero giving return-code
           end-accept
           .
       2101-exit.
           exit.

      *>===================================================================================
       2102-ler-user-cliente section.
       2102.
           move spaces                         to ws-user-cliente
           display "REMOTE_USERNAME" upon environment-name
               on exception
                   add 4                       to zero giving return-code
           end-display
           accept ws-user-cliente from environment-value
               on exception
                   add 4                       to zero giving return-code
           end-accept
           .
       2102-exit.
           exit.

      *>===================================================================================
       2110-ler-subsession section.
       2110.
            perform 9000-abrir-io-sublogon
            move lnk-subsessionid       to lns-sub-session-id
            read sublogon key is lns-sub-session-id
            if   not ws-operacao-ok
                 perform 2120-ler-session
                 exit section
            else
                 move zeros             to lnk-status
                 *>move lns-session-id    to lnk-sessionid
                 if  lnk-sessionid <> lns-session-id
                 and not lnk-unb-alterada
                     *>session inválida!
                     move 1             to lnk-status
                 end-if
                 move lns-linkage       to lnk-retorno
            end-if
            if   lnk-inicio equal 5  *>LOGOFF
                 delete sublogon
            end-if
            close sublogon.
       2110-exit.
            exit.

      *>===================================================================================
       2120-ler-session section.
       2120.
            perform 9000-abrir-io-logon

            move lnk-sessionid          to lgn-session-id
            read logon
            if   not ws-operacao-ok
                 continue
            else
                 move zeros             to lnk-status
                 move lgn-linkage       to lnk-retorno
            end-if
            if   lnk-inicio equal 5  *>LOGOFF
                 perform 9000-abrir-io-sublogon
                 delete logon
                 perform 2250-excluir-subsession
                 if   lnk-id-usuario <> spaces
                      initialize                   lgn-registro
                      move lnk-id-usuario          to lgn-usuario
                      start logon key is not less lgn-usuario
                      read logon next
                      perform until not ws-operacao-ok
                      or   lgn-usuario <> lnk-id-usuario
                           delete logon
                           perform 2250-excluir-subsession
                           read logon next
                      end-perform
                 end-if
                 close sublogon
            end-if
            close logon.
       2120-exit.
            exit.

      *>===================================================================================
       2200-criar-logon section.
       2200.
            perform 9000-abrir-io-logon
            perform 9000-abrir-io-sublogon
            if   lnk-opcao not numeric
                 move zeros             to lnk-opcao
            end-if
            if   lnk-opcao <> 2
                 initialize             lgn-registro
                 move lnk-id-usuario    to lgn-usuario
                 start logon key is not less lgn-usuario
                 read logon next
                 perform until not ws-operacao-ok
                                or lgn-usuario <> lnk-id-usuario
                      delete logon
                      perform 2250-excluir-subsession
                      read logon next
                 end-perform
            end-if

            perform 2210-config-seguranca

            move lnk-sessionID          to lgn-session-id
            move lnk-id-usuario         to lgn-usuario
            move spaces                 to lnk-rotina
            if   lnk-inicio = 9
                 move "1"               to lnk-animate
                 move lnk-animpath      to lnk-cgipath
            else
                 move "0"               to lnk-animate
            end-if
            *>move ws-ip                  to lnk-sso2-ip-client

            if   function upper-case( lnk-id-usuario ) <> implantacao
                 perform 2900-carrega-parametros
            end-if

            accept lnk-hora-login     from time

            move lnk-retorno            to lgn-linkage
            write lgn-registro.
            if   not ws-operacao-ok
                 continue
            else
                 if   lnk-grava-log = "S"
                      continue
                 end-if
            end-if.
       2200-exit.
            exit.

      *>===================================================================================
       2210-config-seguranca section.
       2210.
           close promax-ini

           move on-off-hide-url-sso                to wid-promax-ini
           open input promax-ini
           if  ws-operacao-ok
               close promax-ini
               set lnk-sso-hide-url                to true
           end-if

           move on-off-vincula-ip                  to wid-promax-ini
           open input promax-ini
           if  ws-operacao-ok
               close promax-ini
               set lnk-sso-vincula-ip              to true
           end-if

        *> Esses itens precisam do SSO habilitado, e por isso o PP00100 é quem faz a verificação, e "marca" ou não os controles,
        *> porém, o PP0001 é quem "controlará" a section baseado nessas informações
        *>
        *> move on-off-vincula-user                to wid-promax-ini
        *> open input promax-ini
        *> if  ws-operacao-ok
        *>     close promax-ini
        *>     set lnk-sso-vincula-user-ad         to true
        *> end-if
        *>
        *> move on-off-upromax-uad                 to wid-promax-ini
        *> open input promax-ini
        *> if  ws-operacao-ok
        *>     close promax-ini
        *>     set lnk-sso-login-only-uad          to true
        *> end-if
           .
       2210-exit.
           exit.

      *>===================================================================================
       2250-excluir-subsession section.
       2250.
            initialize                  lns-registro
            move lgn-session-id         to lns-session-id
            start sublogon key is not less lns-chave-2
            read sublogon next
            perform until not ws-operacao-ok
                           or lns-session-id <> lgn-session-id
                 delete sublogon
                 if   not ws-operacao-ok
                      continue
                 end-if
                 read sublogon next
            end-perform.
       2250-exit.
            exit.

      *>===================================================================================
       2300-chama-rotina section.
       2300.
            perform 9000-abrir-i-logon
            perform 9000-abrir-io-sublogon
            initialize                  lgn-registro
                                        lns-registro
            move lnk-sessionid          to lgn-session-id
            read logon
                 invalid key
                     perform 9000-monta-status
                     string "Erro LOGON - status: ", ws-status into whs-mensagem
                     perform 8000-mensagem
                     exit section
            end-read
            move lnk-subsessionid       to lns-sub-session-id
            move lgn-session-id         to lns-session-id
            move lnk-retorno            to lns-linkage
            write lns-registro
                  invalid key
                      rewrite lns-registro
            end-write

            if   not ws-operacao-ok
                 perform 9000-monta-status
                 if   ws-status = " 9041"
                      close sublogon
                      open output sublogon
                      close sublogon
                      open i-o sublogon
                      move lnk-subsessionid       to lns-sub-session-id
                      move lgn-session-id         to lns-session-id
                      move lnk-retorno            to lns-linkage
                      write lns-registro
                            invalid key
                                rewrite lns-registro
                      end-write
                 end-if
            end-if

            if   not ws-operacao-ok
                 perform 9000-monta-status
                 exec html
                   <br>PP00001.cbl Erro ao gravar SUBLOGON (2300-chama-rotina) <br>
                   status=:ws-status  reg=:lns-sub-session-id <br>
                 end-exec
            end-if.
       2300-exit.
            exit.

      *>===================================================================================
       2900-carrega-parametros section.
       2900.
            perform 9000-abrir-i-pd02202
            perform 9000-abrir-io-pd09400
            perform 9000-abrir-i-pd26800
            perform 9000-abrir-i-pd99800

            move lnk-cd-empresa         to fpare-cd-empresa
            move lnk-cd-filial          to fpare-cd-filial
            move 1                      to fpare-codigo-registro
            move 0                      to fpare-sequencia
            perform 9000-ler-pd99800-ran
            if   not ws-operacao-ok
                 string "Erro leitura (CR=1, SEQ=0) arq. ", wid-pd99800,
                        into whs-mensagem
                 perform 9000-mensagem-inteligente
                 exit section
            end-if
            move fpare-tipo-cdd         to lnk-ind-tipo-cdd
            move fpare-nome-revenda     to lnk-nome-revenda    *> Abaixo será atualizado pela seção 2904-reestruturacao
            move fpare-software-erp     to lnk-software-erp
            move fpare-estoque          to lnk-estoque
            move fpare-emissao-nota     to lnk-emissao-nota
            move fpare-livros-fiscais   to lnk-livros-fiscais
            move fpare-comissoes        to lnk-comissoes
            move fpare-expiracao        to lnk-data-expiracao
            move fpare-numero-serie     to lnk-numero-serie-cd
            move spaces                 to lnk-filler-2
            move fpare-dia-limite-reorg to ws-dia-limite-reorg
            move fpare-estado-revenda   to ws-estado-revenda

            accept ws-data-inv from date yyyymmdd
            move ws-dia-inv             to ws-dia
            move ws-mes-inv             to ws-mes
            move ws-ano-inv             to ws-ano
            move ws-data                to lnk-data-cpu

            move fpare-data-atual       to lnk-data-movimento

            if   lnk-data-movimento not equal zeros
                 move lnk-data-movimento to data-inicial
                 move zeros             to numero-dias
                 move 1                 to opcao
                 perform 9000-controle-de-datas
                 move numero-dias       to ws-dias-anterior
            end-if

       *> Lendo Atualizacao CDD
            move lnk-cd-empresa                    to fpare-cd-empresa-17-1
            move lnk-cd-filial                     to fpare-cd-filial-17-1
            move 17                                to fpare-cd-registro-17-1
            move 1                                 to fpare-sequencia-17-1
            perform 9000-ler-pd99800-ran
            if   ws-operacao-ok
                 move fpare-dt-anterior-atualiz     to lnk-dt-anterior-atualizacao
                 move fpare-dt-atual-atualiz        to lnk-dt-atual-atualizacao
                 move fpare-prx-dt-atualiz          to lnk-prx-dt-atualizacao
                 move fpare-avanca-dt-pw04003e      to lnk-avanca-data-PW04003E
                 move fpare-avanca-dt-2-pw04003e    to lnk-avanca-data2-PW04003E
                 move fpare-avanca-domingo-pw04003e to lnk-avanca-domingo-PW04003E
                 move fpare-avanca-feriado-pw04003e to lnk-avanca-feriado-PW04003E
            end-if

      *> Busca o nome da Unidade no arquivo de Empresa Corporativa (Neste ponto já tem a data de movimento)
            perform 2904-reestruturacao

       *> Verificando UNB-CDD
            move lnk-cd-empresa         to fpare-cd-empresa
            move lnk-cd-filial          to fpare-cd-filial
            move 2                      to fpare-codigo-registro
            move 2                      to fpare-sequencia
            perform 9000-ler-pd99800-ran
            if   not ws-operacao-ok
                 string "Erro leitura (CR=2, SEQ=2) arq. ", wid-pd99800
                        into whs-mensagem
                 perform 9000-mensagem-inteligente
                 exit section
            end-if

       *> Lendo DATA CFO
            move lnk-cd-empresa         to fpare-cd-empresa
            move lnk-cd-filial          to fpare-cd-filial
            move 1                      to fpare-codigo-registro
            move 1                      to fpare-sequencia
            perform 9000-ler-pd99800-ran
            if   not ws-operacao-ok
                 string "Erro leitura (CR=1, SEQ=1) arq. ", wid-pd99800
                        into whs-mensagem
                 perform 9000-mensagem-inteligente
                 exit section
            end-if
            move fpare-data-limite-cfo  to lnk-data-limite-cfo.

       *> Lendo CR=2, SEQ=0
            move lnk-cd-empresa         to fpare-cd-empresa
            move lnk-cd-filial          to fpare-cd-filial
            move 2                      to fpare-codigo-registro
            move zeros                  to fpare-sequencia
            perform 9000-ler-pd99800-ran
            if   not ws-operacao-ok
                 string "Erro leitura (CR=2, SEQ=0) arq. ", wid-pd99800
                        into whs-mensagem
                 perform 9000-mensagem-inteligente
                 exit section
            end-if
            perform 2902-verifica-projeto-adf
            if   not ws-prj-novo-adf-escalonado
                 move fpare-ind-robin-hood   to lnk-utilizar-robin-hood
            end-if
            move fpare-ind-utiliza-monitoramto
                                        to lnk-ind-utiliza-monitoramento.

       *> Verif Projeto 86 MTFiscal Habilitado nas Revendas
            if   lnk-revenda
                 perform 2903-verifica-projeto-mtfiscal
            end-if.

       *> Carrega linkage de projetos
            initialize f9400-dados-projeto
            move lnk-cd-empresa          to f9400-cd-empresa
            move lnk-cd-filial           to f9400-cd-filial
            perform 9000-str-pd09400-nlss
            perform 9000-ler-pd09400-nex
            perform until not ws-operacao-ok
            or   f9400-cd-empresa        <> lnk-cd-empresa
            or   f9400-cd-filial         <> lnk-cd-filial
                 move f9400-status-projeto to lnk-tb-status-projeto(f9400-codigo-projeto)
                 perform 9000-ler-pd09400-nex
            end-perform

       *>ler f2202-cotacoes( lnk-taxa-mensal )
            move spaces                 to f2202-cotacoes
            initialize                     f2202-cotacoes
            move lnk-cd-empresa         to f2202-cd-empresa
            move lnk-cd-filial          to f2202-cd-filial
            move 1                      to f2202-codigo-moeda
            move 1                      to f2202-dia-cotacao
            move lnk-mes-movimento      to f2202-mes-cotacao
            move lnk-ano-movimento      to f2202-ano-cotacao
            perform 9000-ler-pd02202-ran
            if   not ws-operacao-ok
                 move zeros             to f2202-cotacao
            end-if
            if   f2202-cotacao equal zeros
                 string "Taxa Mensal de Juros não informada \n",
                        "Erro leitura (CR=2, SEQ=0) arq. ", wid-pd99800 delimited by "  ",
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
                 move spaces            to whs-mensagem
                                           lnk-auxiliar

            end-if
            move f2202-cotacao          to lnk-taxa-mensal

            if   (lnk-cdd or lnk-revenda)
            and  lnk-execucao-normal
                 perform 2901-primeira-nota
            end-if

            if   lnk-efetua-reorganizacao
                 if   lnk-dia-movimento greater 09
                 and  lnk-dia-movimento not greater ws-dia-limite-reorg
                 and  lnk-revenda
                      perform 8000-msg-01
                 else
                      if   not lnk-cdd                                                   *>AJ030709:74003
                           if   lnk-dia-movimento greater ws-dia-limite-reorg
                                move "Sistema INOPERANTE por falta de REORGANIZAÇÃO" to whs-mensagem
                                perform 9000-mensagem-inteligente
                                exit section
                           end-if
                      end-if                                                             *>AJ030709:74003
                 end-if
            end-if

            if  (lnk-usuario-gerencial
            or   lnk-permissao-especial)
            and  lnk-dia-movimento > ( rdia-do-mes ( lnk-mes-movimento ) - 4 )
                 perform 2902-verifica-projeto-sap-log                     *>NB310804:23125
                 if   not ws-operacao-ok                                   *>NB310804:23125
                 and  not ws-registro-inexistente                          *>NB310804:23125
                      exit section                                         *>NB310804:23125
                 end-if                                                    *>NB310804:23125
                 if   lnk-revenda                                          *>NB310804:23125
                 or  (lnk-cdd                                              *>NB310804:23125
                 and  not ws-prj-sap-logistica)                            *>NB310804:23125
                      perform 8000-msg-02
                 end-if
            end-if

            if   lnk-revenda
                 if   lnk-usuario-gerencial
                 and  lnk-permissao-especial
                      move 1            to ws-acesso-fecha-financ
                 else
                      perform 9000-abrir-i-pd30800
                      perform 9000-abrir-i-pd33200

                      move zeros        to ws-acesso-fecha-financ
                      perform varying ws-ind from 1 by 1
                              until ws-ind greater 20
                              or    lnk-cd-perfil ( ws-ind ) not numeric
                              or    ( ws-ind                 greater 1
                              and     lnk-cd-perfil ( ws-ind ) equal zeros )
                           initialize                    f30800-usuario-perfil
                           move lnk-id-usuario           to f30800-id-usuario
                           move lnk-cd-empresa           to f30800-cd-empresa
                           move lnk-cd-filial            to f30800-cd-filial
                           move 3                        to f30800-cd-sistema   *> Revenda = 3
                           move lnk-cd-perfil ( ws-ind ) to f30800-cd-perfil
                           perform 9000-ler-pd30800-ran-1
                           if  ws-operacao-ok
                           and f30800-perfil-normal
                               initialize             f33200-perfil-modulo
                               move f30800-cd-sistema to f33200-cd-sistema
                               move f30800-cd-perfil  to f33200-cd-perfil
                               move 1200000000        to f33200-cd-modulo
                               perform 9000-ler-pd33200-ran-1
                               if   ws-operacao-ok
                                    move 1 to ws-acesso-fecha-financ
                                    exit perform
                               end-if
                           end-if
                      end-perform

                      close pd30800
                      close pd33200
                 end-if
                 if   acessa-fecha-financ
                      perform 9000-verifica-status-pd38300
                      if   not ws-ctrl-pd38300
                           perform 9000-abrir-i-pd38300
                      end-if
                      move zeros          to data-inicial
                      perform 9000-leitura-fech-financeiro
                      move ws9126-ano-fechamento to ano-inicial
                      move ws9126-mes-fechamento to mes-inicial
                      move ws9126-dia-fechamento to dia-inicial

                      initialize                  fparv-incorporacao-revenda
                      move lnk-cd-empresa         to fparv-cd-empresa
                      move lnk-cd-filial          to fparv-cd-filial
                      move 81                     to fparv-cd-registro
                      move 0                      to fparv-cd-sequencia
                      perform 9000-ler-pd99800-ran
                      if   not ws-operacao-ok
                           move zeros             to fparv-nr-dias-fech-financ
                      end-if

                      if   fparv-nr-dias-fech-estoque is not numeric
                           move zeros to fparv-nr-dias-fech-financ
                      end-if

                      close pd38300

                      move zeros        to ws-resultado-acesso
                      move 1            to opcao
                      perform 9000-controle-de-datas
                      move numero-dias  to ws-numero-dias-fec-fin
                      move lnk-dia-cpu  to dia-inicial
                      move lnk-mes-cpu  to mes-inicial
                      move lnk-ano-cpu  to ano-inicial
                      move 1            to opcao
                      perform 9000-controle-de-datas
                      compute ws-dias-fechamento = numero-dias - ws-numero-dias-fec-fin
                      if   ws-dias-fechamento > fparv-nr-dias-fech-financ
                           compute ws-dias-fechamento = ws-dias-fechamento - 1
                           move ws-dias-fechamento to ws-dias-fechamento-z
                           string "Fechamento Financeiro não realizado há "
                                  ws-dias-fechamento-z
                                  " dias"
                           into whs-mensagem

                           perform 9000-mensagem-inteligente
                           move spaces  to whs-mensagem
                                           lnk-auxiliar
                      end-if
                 end-if
            end-if

            if   lnk-revenda
                 if   lnk-usuario-gerencial
                 and  lnk-permissao-especial
                      move 1            to ws-acesso-fecha-estoque
                 else
                      perform 9000-abrir-i-pd30800
                      perform 9000-abrir-i-pd33200

                      move zeros        to ws-acesso-fecha-estoque
                      perform varying ws-ind from 1 by 1
                              until ws-ind greater 20
                              or    lnk-cd-perfil ( ws-ind ) not numeric
                              or    ( ws-ind                 greater 1
                              and     lnk-cd-perfil ( ws-ind ) equal zeros )
                           initialize                    f30800-usuario-perfil
                           move lnk-id-usuario           to f30800-id-usuario
                           move lnk-cd-empresa           to f30800-cd-empresa
                           move lnk-cd-filial            to f30800-cd-filial
                           move 3                        to f30800-cd-sistema   *> Revenda = 3 Isso só vai funcionar sem o prj Segregação habilitado
                           move lnk-cd-perfil ( ws-ind ) to f30800-cd-perfil    *> quando for habilitado o codigo do sistema será 1
                           perform 9000-ler-pd30800-ran-1
                           if  ws-operacao-ok
                           and f30800-perfil-normal
                               initialize             f33200-perfil-modulo
                               move f30800-cd-sistema to f33200-cd-sistema
                               move f30800-cd-perfil  to f33200-cd-perfil
                               move 1200000000        to f33200-cd-modulo
                               perform 9000-ler-pd33200-ran-1
                               if   ws-operacao-ok
                                    move 1 to ws-acesso-fecha-estoque
                                    exit perform
                               end-if
                               initialize             f33200-perfil-modulo
                               move f30800-cd-sistema to f33200-cd-sistema
                               move f30800-cd-perfil  to f33200-cd-perfil
                               move 0200000000        to f33200-cd-modulo
                               perform 9000-ler-pd33200-ran-1
                               if   ws-operacao-ok
                                    move 1 to ws-acesso-fecha-estoque
                                    exit perform
                               end-if
                           end-if
                      end-perform

                      close pd30800
                      close pd33200
                 end-if
                 if   acessa-fecha-estoque
                      perform 9000-abrir-i-pd18700
                      initialize             f18700-fechamento-estoque
                      move lnk-cd-empresa    to f18700-cd-empresa
                      move lnk-cd-filial     to f18700-cd-filial
                      move 99999999          to f18700-dt-fechamento
                      perform 9000-str-pd18700-lss
                      perform 9000-ler-pd18700-pre
                      if   not ws-operacao-ok
                      or   f18700-cd-empresa not equal lnk-cd-empresa
                      or   f18700-cd-filial  not equal lnk-cd-filial
                           initialize        f18700-fechamento-estoque
                      end-if
                      close pd18700
                      move F18700-dt-fechamento to ws-data-inv
                      move ws-dia-inv   to dia-inicial
                      move ws-mes-inv   to mes-inicial
                      move ws-ano-inv   to ano-inicial

                      initialize                  fparv-incorporacao-revenda
                      move lnk-cd-empresa         to fparv-cd-empresa
                      move lnk-cd-filial          to fparv-cd-filial
                      move 81                     to fparv-cd-registro
                      move 0                      to fparv-cd-sequencia
                      perform 9000-ler-pd99800-ran
                      if   not ws-operacao-ok
                           move zeros             to fparv-nr-dias-fech-estoque
                      end-if

                      if   fparv-nr-dias-fech-estoque is not numeric
                           move zeros to fparv-nr-dias-fech-estoque
                      end-if

                      move zeros        to ws-resultado-acesso
                      move 1            to opcao
                      perform 9000-controle-de-datas
                      move numero-dias  to ws-numero-dias-fec-est
                      move lnk-dia-cpu  to dia-inicial
                      move lnk-mes-cpu  to mes-inicial
                      move lnk-ano-cpu  to ano-inicial
                      move 1            to opcao
                      perform 9000-controle-de-datas
                      compute ws-dias-fechamento = numero-dias - ws-numero-dias-fec-est
                      if   ws-dias-fechamento > fparv-nr-dias-fech-estoque
                           compute ws-dias-fechamento = ws-dias-fechamento - 1
                           move ws-dias-fechamento to ws-dias-fechamento-z
                           string "Fechamento do Estoque não realizado há "
                                  ws-dias-fechamento-z
                                  " dias"
                              into whs-mensagem
                           perform 9000-mensagem-inteligente
                           move spaces  to whs-mensagem
                                           lnk-auxiliar
                      end-if
                 end-if
            end-if

            if   lnk-revenda
            and  lnk-usuario-gerencial
            and  lnk-permissao-especial
                 string lnk-dtbpath delimited by " " "/EFD422.CAD" delimited by size into wid-pd42200
                 open input pd42200
                 if   ws-arquivo-inexistente
                      close pd42200
                      open output pd42200
                 end-if
                 initialize                        f42200-recebimento-2v
                 move lnk-cd-empresa               to f42200-cd-empresa
                 move lnk-cd-filial                to f42200-cd-filial
                 perform 9000-ler-pd42200-ran
      *>         Quando exisitr uma data, significa que o projeto entrou na revenda e já foi atualizado o dia
                 if   f42200-dt-retencao(1) greater zeros
                      move f42200-dt-retencao-dia(1) to dia-inicial
                      move f42200-dt-retencao-mes(1) to mes-inicial
                      move f42200-dt-retencao-ano(1) to ano-inicial
                      move 1                       to opcao
                      perform 9000-controle-de-datas
                      move numero-dias             to ws-nr-dias-retencao-2v

                      move lnk-dia-cpu             to dia-inicial
                      move lnk-mes-cpu             to mes-inicial
                      move lnk-ano-cpu             to ano-inicial
                      move 1                       to opcao
                      perform 9000-controle-de-datas
                      compute ws-nr-dias-retencao = numero-dias - ws-nr-dias-retencao-2v
                      subtract 5                   from ws-nr-dias-retencao
                      if   ws-nr-dias-retencao greater zeros
                           perform varying wx-index-1 from 1 by 1 until wx-index-1 greater ws-nr-dias-retencao
                           or   wx-index-1 greater 10
                           or   f42200-dt-retencao(wx-index-1) is not numeric
                           or   f42200-dt-retencao(wx-index-1) equal zeros
                                add f42200-qt-mapas-pendentes(wx-index-1) to ws-qt-nr-mapas-exportados
                           end-perform
                           if   ws-qt-nr-mapas-exportados greater zeros
                                move ws-qt-nr-mapas-exportados to ws-qt-nr-mapas-exportados-z
                                string "2V - Existem "ws-qt-nr-mapas-exportados-z" mapa(s) pendente(s) a mais de 5 dias" into whs-mensagem
                                perform 9000-mensagem-inteligente
                                move spaces        to whs-mensagem
                                                      lnk-auxiliar
                           end-if
                      end-if
                 end-if
            end-if

            if   lnk-revenda
            and  lnk-usuario-gerencial
            and  lnk-permissao-especial
                 initialize fparg-parametros-gerais
                 move lnk-cd-empresa               to fparg-cd-empresa
                 move lnk-cd-filial                to fparg-cd-filial
                 move 2                            to fparg-cd-registro
                 move 4                            to fparg-cd-sequencia
                 perform 9000-ler-pd99800-ran
                 if   ws-operacao-ok
                      if   fparg-sem-vinculo-foto-seav
                           string "Fotografia SEAV \n"
                                  "Existem Vendedores do perfil SEGMENTADO \n"
                                  "que não possuem PERFIL FOTOGRAFIA SEAV vinculado. \n"
                                  "Efetue o cadastro no módulo 01.02.24 - Setores."
                                                 into whs-mensagem
                           perform 9000-mensagem-inteligente
                           move spaces             to whs-mensagem
                                                      lnk-auxiliar
                      end-if
                 end-if
            end-if

            if   lnk-revenda
                 string lnk-dtbpath delimited by " " "/EFD897.CAD" delimited by size into wid-pd89700
                 open input pd89700
                 if   ws-arquivo-inexistente
                      close pd89700
                      open output pd89700
                      close pd89700
                      open input pd89700
                 end-if

                 initialize f89700-hist-diverg-promax-sefaz
                 move lnk-cd-empresa               to f89700-cd-empresa
                 move lnk-cd-filial                to f89700-cd-filial
                 move 2                            to f89700-id-situacao-promax
                 move ws-dt-entrega-nf             to f89700-dt-entrega
                 perform 9000-str-pd89700-nlss-2
                 perform 9000-ler-pd89700-nex
                 if   ws-operacao-ok
                 and  f89700-cd-empresa          equal lnk-cd-empresa
                 and  f89700-cd-filial           equal lnk-cd-filial
                 and  f89700-id-situacao-promax  equal 2
                 and  f89700-dt-entrega          >= ws-dt-entrega-nf
                      string "Existem NFs a partir do dia " f89700-dt-entrega-dia "/" f89700-dt-entrega-mes "/" f89700-dt-entrega-ano " com divergências.\n\n"
                             "Verificar no módulo 07.22.23-Rel. de Diverg. Promax x SEFAZ, proceder\n"
                             "a correção e executar a rotina 07.22.22-Verificação de Divergências."
                                            into whs-mensagem
                      perform 9000-mensagem-inteligente
                      move spaces                  to whs-mensagem
                                                      lnk-auxiliar
                 end-if
                 close pd89700
            end-if

            close pd02202
            close pd09400                                                  *>NB310804:23125
            close pd26800
            close pd99800.
       2900-exit.
            exit.

      *>===================================================================================
       2901-primeira-nota section.
       2901.
            perform 9000-verifica-status-pd00400

            perform 9000-verifica-status-pdi00400

            if   not ws-ctrl-pdi0400
                 perform 9000-abrir-i-pdi00400
            end-if

            if   not ws-ctrl-pd00400
                 perform 9000-abrir-i-pd00400
            end-if
            move "N"                    to lnk-ind-reorganizacao
            initialize                  fifrd-registro-data
            move lnk-cd-empresa         to fifrd-cd-empresa
            move lnk-cd-filial          to fifrd-cd-filial
            move 01                     to fifrd-codigo-registro
            perform 9000-str-pdi00400-gtr
            perform 9000-ler-pdi00400-nex
            perform until not ws-operacao-ok
                           or fifrd-cd-empresa      <> lnk-cd-empresa
                           or fifrd-cd-filial       <> lnk-cd-filial
                           or fifrd-codigo-registro <> 01

                 move lnk-cd-empresa    to fcfmf-cd-empresa
                 move lnk-cd-filial     to fcfmf-cd-filial
                 move fifrd-tipo-nota   to fcfmf-tipo-nota
                 move fifrd-codigo-des  to fcfmf-codigo-des
                 move fifrd-numero-doc  to fcfmf-numero-doc
                 move fifrd-serie-doc   to fcfmf-serie-doc
                 move 1                 to fcfmf-codigo-registro
                 move zeros             to fcfmf-sequencia
                 perform 9000-ler-pd00400-ran
                 if   not ws-operacao-ok
                      exit perform
                 end-if
                 if   fcfmf-tipo-movimento = 51
                      move lnk-dia-movimento  to ws-dia-inv-aux
                      move lnk-mes-movimento  to ws-mes-inv-aux
                      move lnk-ano-movimento  to ws-ano-inv-aux
                      move fcfmf-data-emissao to ws-data
                      move ws-dia             to ws-dia-inv
                      move ws-mes             to ws-mes-inv
                      move ws-ano             to ws-ano-inv
                      move ws-data-inv        to ws-dt-entrega-nf
                      compute ws-dias = function integer-of-date( ws-data-inv-aux ) -
                                        function integer-of-date( ws-data-inv )
                      if   lnk-cdd and ws-dias > 62
                           move "S"     to lnk-ind-reorganizacao
                      end-if

                      if   lnk-revenda and ws-dias > 92
                           move "S"     to lnk-ind-reorganizacao
                      end-if

                      exit perform
                 end-if

                 perform 9000-ler-pdi00400-nex
            end-perform
            close pdi00400
            close pd00400.
       2901-exit.
            exit.

      *>===================================================================================
       2902-verifica-projeto-sap-log section.
       2902.
            move "N"                       to ws-st-projeto-5
            initialize                     f9400-dados-projeto
            move lnk-cd-empresa            to f9400-cd-empresa
            move lnk-cd-filial             to f9400-cd-filial
            move 05                        to f9400-codigo-projeto
            read pd09400 with ignore lock
            if   not ws-operacao-ok
            and  not ws-registro-inexistente
                 perform 9000-monta-status
                 string "Erro de VERIFICACAO Projeto: ", f9400-codigo-projeto,
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
            else
                 move f9400-status-projeto to ws-st-projeto-5
            end-if.
       2902-exit.
            exit.

      *>===================================================================================
       2902-verifica-projeto-adf section.
       2902.
            move "N"                       to ws-st-projeto-5
            initialize                     f9400-dados-projeto
            move lnk-cd-empresa            to f9400-cd-empresa
            move lnk-cd-filial             to f9400-cd-filial
            move 84                        to f9400-codigo-projeto
            read pd09400 with ignore lock
            if   not ws-operacao-ok
            and  not ws-registro-inexistente
                 perform 9000-monta-status
                 string "Erro de VERIFICACAO Projeto: ", f9400-codigo-projeto,
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
            else
                 move f9400-status-projeto to ws-st-projeto-84
            end-if.
       2902-exit.
            exit.

      *>===================================================================================
       2903-verifica-projeto-mtfiscal section.
       2903.
            move lnk-data-movimento  to data-inicial
            move 1                   to opcao
            perform 9000-controle-de-datas

            if   (numero-dias greater 733057)   *>numero dias para 01/01/2008  fim da verificação
                 exit section
            end-if

            if   (ws-estado-revenda equal "PR" or "SP" or "RJ")
            and  (numero-dias less 732944)   *>numero dias para 10/09/2007
                 exit section
            end-if

            if   (ws-estado-revenda equal "MG" or "RS" or "SC")
            and  (numero-dias less 732954)   *>numero dias para 20/09/2007
                 exit section
            end-if

            if   (ws-estado-revenda equal "AC" or "AM" or "DF" or "GO" or "MS" or "MT" or "RO"
            or "RR" or "TO" or "ES") and (numero-dias less 732974)   *>numero dias para 10/10/2007
                 exit section
            end-if

            if   (ws-estado-revenda equal "BA" or "PI" or "AL" or "CE" or "PB" or "PE" or "RN"
            or "SE" or "AP" or "MA" or "PA") and (numero-dias less 732984)   *>numero dias para 20/10/2007
                 exit section
            end-if

            *> verifica se projeto 86 está habilitado (se não habilita)
            initialize                     f9400-dados-projeto
            move lnk-cd-empresa            to f9400-cd-empresa
            move lnk-cd-filial             to f9400-cd-filial
            move 86                        to f9400-codigo-projeto
            read pd09400 with ignore lock
            if   not ws-operacao-ok
                 perform 9000-monta-status
                 string "Erro de VERIFICACAO Projeto: ", f9400-codigo-projeto,
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
                 exit section
            end-if.

            if   f9400-habilitado
                 exit section
            end-if.

            move lnk-data-movimento     to f9400-data-projeto
            accept ws-horas             from time
            move ws-hh-mm               to f9400-hora-projeto
            move "S"                    to f9400-status-projeto ws-st-projeto-86
            move "hbsis"                to f9400-id-usuario-projeto
            rewrite f9400-dados-projeto
            if   not ws-operacao-ok
                 perform 9000-monta-status
                 string "Erro de REGRAVACAO Projeto: ", f9400-codigo-projeto,
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
            end-if.
       2903-exit.
            exit.

      *>===================================================================================
       2904-reestruturacao section.
       2904.
            move lnk-dia-movimento                 to ws-dia-inv
            move lnk-mes-movimento                 to ws-mes-inv
            move lnk-ano-movimento                 to ws-ano-inv

            initialize                             f26800-empresa-corporativa
            move lnk-cd-empresa                    to f26800-cd-empresa
            move lnk-cd-filial                     to f26800-cd-filial
            move all "9"                           to f26800-dt-vigencia-i
            perform 9000-str-pd26800-ngtr
            perform 9000-ler-pd26800-pre
            perform until not ws-operacao-ok
            or lnk-cd-empresa not equal f26800-cd-empresa
            or lnk-cd-filial  not equal f26800-cd-filial
                 if   ws-data-inv not less f26800-dt-vigencia-i *>(se está dentro vigencia)
                      move f26800-nome-revenda     to lnk-nome-revenda
                      exit perform
                 end-if
                 perform 9000-ler-pd26800-pre
            end-perform.
       2904-exit.
            exit.

      *>===================================================================================
       2905-verifica-projeto-24x7 section.
       2905.
            move "N"                               to ws-st-projeto-109
            initialize                             f9400-dados-projeto
            move lnk-cd-empresa                    to f9400-cd-empresa
            move lnk-cd-filial                     to f9400-cd-filial
            move 109                               to f9400-codigo-projeto
            read pd09400 with ignore lock
            if   not ws-operacao-ok
            and  not ws-registro-inexistente
                 perform 9000-monta-status
                 string "Erro de VERIFICACAO Projeto: ", f9400-codigo-projeto,
                        " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-inteligente
            else
                 move f9400-status-projeto         to ws-st-projeto-109
            end-if.
       2905-exit.
            exit.

      *>===================================================================================
       2999-output-logon-sublogon section.
       2999.
            close logon
            close sublogon
            move spaces                 to wid-logon
                                           wid-sublogon
            string lnk-dtbPath delimited by " ", "/LOGON"
                   into wid-logon
            string lnk-dtbPath delimited by " ", "/SUBLOGON"
                   into wid-sublogon
            perform 2999-output-sublogon
            perform 2999-output-logon
            close logon
            close sublogon.
       2999-exit.
            exit.

      *>===================================================================================
       2999-output-logon section.
       2999.
            move 9                      to lnk-status-erro
            perform varying ws-contador from 1 by 1 until ws-contador > 1000
                 open output logon
                 if   ws-operacao-ok
                      move zeros        to lnk-status-erro
                      exit perform
                 end-if
            end-perform

            if   lnk-status-erro <> zeros
                 open i-o logon
                 initialize                  lgn-registro
                 start logon key is not less lgn-session-id
                 read logon next
                 perform until not ws-operacao-ok
                      delete logon
                      read logon next
                 end-perform
            end-if.
       2999-exit.
            exit.

      *>===================================================================================
       2999-output-sublogon section.
       2999.
            move 9                      to lnk-status-erro
            perform varying ws-contador from 1 by 1 until ws-contador > 1000
                 open output sublogon
                 if   ws-operacao-ok
                      move zeros        to lnk-status-erro
                      exit perform
                 end-if
            end-perform

            if   lnk-status-erro <> zeros
                 open i-o sublogon
                 initialize                  lns-registro
                 start sublogon key is not less lns-chave-2
                 read sublogon next
                 perform until not ws-operacao-ok
                      delete sublogon
                      read sublogon next
                 end-perform
            end-if.
       2999-exit.
            exit.

      *>===================================================================================
       3000-finalizacao section.
       3000.
            close logon
            close sublogon.
       3000-exit.
            exit.

      *>===================================================================================
      *>HTML
       copy PCP8000.cpy.

      *>===================================================================================
       8000-msg-01 section.
       8000.
            if   lnk-execucao-em-background
            or   lnk-inicio = 2
                 exit section
            end-if
            exec html
               <SCRIPT>
                   linha1 = 'O dia 15 de cada mes é o prazo LIMITE para a REORGANIZAÇÃO, após esse prazo ';
                   linha2 = 'o sistema ficara INOPERANTE, portanto, sabendo como proceder execute-a, ';
                   linha3 = 'e em caso de dúvida contate o suporte:';
                   linha4 = '\nHBSIS Informatica Ltda fone (047) 2123-5400';
                   alert( linha1 + linha2 + linha3 + linha4 );
               </SCRIPT>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
       8000-msg-02 section.
       8000.
            if   lnk-execucao-em-background
            or   lnk-inicio = 2
                 exit section
            end-if
            exec html
               <SCRIPT>
                   linha1 = 'O final do mês está próximo. NÃO ESQUEÇA de emitir ';
                   linha2 = 'a Nota de Entrada referente às Trocas efetuadas no mês.';
                   linha3 = 'Não sabendo como proceder, entre em contato com o suporte:';
                   linha4 = '\nHBSIS Informática Ltda fone (047) 2123-5400.';
                   linha5 = '\n\nVocê foi AVISADO!';
                   alert( linha1 + linha2 + linha3 + linha4 + linha5 );
               </SCRIPT>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
      *>Rotinas Padrões
       copy PCP9000.cpy.

      *>===================================================================================
       9000-abrir-io-logon section.
       9000.
            move spaces to wid-logon
            string lnk-dtbPath delimited by " ", "/LOGON"
                   into wid-LOGON
            open i-o logon
            if   ws-resultado-acesso = 35 or 05 or 39
                 close logon
                 open output logon
                 close logon
                 open i-o logon
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-abrir-i-logon section.
       9000.
            string lnk-dtbPath delimited by " ", "/LOGON"
                   into wid-LOGON
            open input logon
            if   ws-resultado-acesso = 35 or 05 or 39
                 close logon
                 open output logon
                 close logon
                 open i-o logon
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-abrir-io-sublogon section.
       9000.
            close SUBLOGON
            move spaces to wid-sublogon
            string lnk-dtbPath , "/SUBLOGON" delimited by spaces into wid-SUBLOGON
            OPEN I-O SUBLOGON
            if  ws-resultado-acesso = 35 or 05 or 39
                close SUBlogon
                open output SUBlogon
                close SUBlogon
                open i-o SUBlogon
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-mensagem-inteligente section.
       9000.
            if   lnk-inicio = 2
                 exit section
            end-if
            if   not ws-operacao-ok
                 move whs-mensagem      to ws-mensagem-aux
                 move spaces            to whs-mensagem
                 perform 9000-monta-status
                 string ws-mensagem-aux delimited by "  ", " - Status: ", ws-status
                        into whs-mensagem
            end-if
            move whs-mensagem           to lnk-auxiliar
            if   lnk-execucao-em-background
                 perform 9000-mensagem-log
            else
                 perform 8000-mensagem
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-mensagem-log section.
       9000.
            perform 9000-abrir-ext-lep
            initialize                  lep-registro
            accept ws-data-inv        from date yyyymmdd
            accept ws-horas           from time
            string ws-dia-inv, "/", ws-mes-inv, "/", ws-ano-inv into lep-data
            string ws-hora, ":", ws-minutos, ":", ws-segundos into lep-hora
            move whs-mensagem           to lep-descricao
            write lep-registro
            if   not ws-operacao-ok
                 perform 9000-monta-status
                 move spaces            to lnk-linha-comando
                 string "Erro ao gerar LOG - status", ws-status
                         into lnk-linha-comando
            end-if
            close lep-log-erro-parametros.
       9000-exit.
            exit.

      *>===================================================================================
       9000-verifica-status-pd00400 section.
       9000.
            move 0                      to ws-id-ctrl-aber-efd004
            move spaces                 to wid-pd00400
            if   lnk-modulo-1 equal 09
                 move lnk-mes-his       to wid-mes-004h
                 move lnk-ano-his       to wid-ano-004h
                 move lnk-cd-filial     to wid-unb-004h
                 string lnk-dtbpath delimited by " ",
                        "/", wid-efd004h into wid-pd00400
            else
                 string lnk-dtbpath delimited by " ",
                        "/EFD004.CAD" into wid-pd00400
            end-if
            open input pd00400.
            move space to whs-arquivo
            move "pd00400-mestre" to whs-arquivo
            if   ws-resultado-acesso = 39
                 move 1           to ws-id-ctrl-aber-efd004
            end-if
            close pd00400.

       9000-exit.
            exit.

      *>===================================================================================
       9000-verifica-status-pdi00400 section.
       9000.
            move 0                      to ws-id-ctrl-aber-efdi04
            move spaces                 to wid-pdi00400
            if   lnk-modulo-1 equal 09
                 move lnk-mes-his       to wid-mes-i04h
                 move lnk-ano-his       to wid-ano-i04h
                 move lnk-cd-filial     to wid-unb-i04h
                 string lnk-dtbpath delimited by " ",
                        "/", wid-efdi04h into wid-pdi00400
            else
                 string lnk-dtbpath delimited by " ",
                        "/EFDI04.IND" into wid-pdi00400
            end-if
            open input pdi00400.
            if   ws-resultado-acesso = 39
                 move 1           to ws-id-ctrl-aber-efdi04
            end-if
            close pdi00400.

       9000-exit.
            exit.


      *>===================================================================================
       9000-verifica-status-pd38300 section.
       9000.
            move 0                      to ws-id-ctrl-aber-efd383
            move spaces                 to wid-pd38300
            string lnk-dtbpath delimited by " "
                   "/EFD383.CAD" delimited by size
                   into wid-pd38300
            open input pd38300
            if   ws-resultado-acesso = 39
                 move 1           to ws-id-ctrl-aber-efd383
            end-if
            close pd38300.
       9000-exit.
            exit.

      *>===================================================================================
       9000-abrir-ext-lep section.
       9000.
            move spaces                            to wid-log-erro-parametros
            if   lnk-dtbPath equal spaces
                  move "PP00001.LOG"               to wid-log-erro-parametros
            else
                  string lnk-dtbPath delimited by "  ", "/PP00001.LOG"
                         into wid-log-erro-parametros

            end-if
            open extend lep-log-erro-parametros
            if   not ws-operacao-ok
            and  ws-resultado-acesso <> "05"
                 perform 9000-monta-status
                 move spaces                       to lnk-linha-comando
                 string "Erro ao abrir PP00001.LOG - status", ws-status
                         into lnk-linha-comando
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       copy PCP0066.CPY.                *>(ANTIGO LOADINI - CARREGA PROMAX.CFG)
       copy PCP9126.CPY.

      *>===================================================================================
      *>Leituras
       copy PCL00400.CPY.               *> NF
       copy PCL02202.CPY.               *> Cotacoes
       copy PCL09400.CPY.               *> Controle de Projetos
       copy PCL18700.CPY replacing ==(F187)== BY ==F187==. *> Controle Fechamento Estoque
       copy PCL26800.CPY.               *> Empresa Corporativa
       copy PCL30800.CPY.               *> Usuário-Perfil
       copy PCL33200.CPY.               *> Perfil-Módulo
       copy PCL38300.CPY.               *> Fechamento Financeiro
       copy PCL42200.CPY.               *> Interface 2V
       copy PCL89700.CPY.               *> Log divergencias
       copy PCL99800.CPY.               *> Parametros
       copy PCLI00400.CPY.              *> Índice NF
*>

      