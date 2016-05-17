      $set preprocess (htmlpp) endp
      $set sourceformat"free"
      $set ms(2)

      *>===================================================================================
       identification division.
       program-id.    PWSTB003.                                              *> Gerenciador
      *>===================================================================================

      *>
      *>            --------------------------------------------------------
      *>                    STB - Sistema de Transferência de Base
      *>            --------------------------------------------------------
      *>

      *>
      *>    Mensagens gravadas no log devem possuir a seguinte formatação:
      *>
      *>        a) Programa (que gerou a ocorrência);
      *>        b) Arquivo (sendo trabalhado);
      *>        c) Origem (Empresa/Filial);
      *>        d) Destino (Empresa/Filial);
      *>        e) Ocorrência (LOG/Chave);
      *>        f) Lidos (Registros lidos);
      *>        g) Gravados (Registros gravados)
      *>
      *>    Exemplo: "PWSTB003;EFDPAR;081/0171;081/0796;Arquivo de origem locado;2300;2300"
      *>

      *>===================================================================================
       environment division.
            special-names. decimal-point is comma.
      *>===================================================================================

       copy PCS99800.CPY.
       copy PCS99800.CPY replacing leading ==fpa== by ==xpa==
                                       ==pd99800== by ==px99800==
                                   ==wid-pd99800== by ==wid-px99800==.

           select arqwor assign to disk wid-arqwor
                  organization is indexed
                  access mode is dynamic
                  record key is rw-chave = rw-cd-registro
                  lock mode is manual
                  file status is ws-resultado-acesso.

           select stb-cfg assign to disk wid-stb-cfg
                   organization is line sequential
                   access mode is sequential
                   file status is ws-resultado-acesso.

      *>===================================================================================
       data division.

       copy PCF99800.CPY.
       copy PCF99800.CPY replacing leading ==fpa== by ==xpa==
                                       ==pd99800== by ==px99800==.

       fd   arqwor.

       01   rw-empresa-filial.
            03 rw-cd-registro           pic 9(01).
            03 rw-id-diagnostico        pic x(01).
            03 rw-de-path-para-path     pic 9(01).
            03 rw-tabela-emp-fil        pic x(196).
            03 rw-dir-aux-escolhido     pic x(40).
            03 rw-ds-diretorio-auxiliar pic x(100).

       fd   stb-cfg.

       01   sc-registro-stb-cfg.
            03 sc-cd-empresa            pic 9(03).
            03 sc-cd-filial             pic 9(04).
            03 sc-ds-unb                pic x(20).

      *>===================================================================================
       working-storage section.

       78   versao                      value "n".

       78   ws-limite-filiais           value 14.

       copy PCW900.CPY.
       copy PCW991.CPY.
       copy PCW904.CPY.
       copy PCW0007.CPY.
       copy PCW0002.CPY replacing ==(LP)== BY ==lp==.

       01   lk-empresa-filial.
            03 lk-id-diagnostico        pic 9(01).
               88 diagnostico               value 1.
            03 lk-de-path-para-path     pic 9(01).
               88 database-dataux           value 1.
            03 lk-tabela-emp-fil        pic x(196).
            03 lk-tabela-emp-fil-1 occurs ws-limite-filiais redefines lk-tabela-emp-fil.
               05 lk-cd-empresa-origem  pic 9(03).
               05 lk-cd-filial-origem   pic 9(04).
               05 lk-cd-empresa-destino pic 9(03).
               05 lk-cd-filial-destino  pic 9(04).

       01   ld-diretorios.
            03 ld-database              pic x(40).
            03 ld-auxiliar              pic x(40).

       01   ws-campos.
            03 ws-acao-pwstb003         pic x(15) value spaces.
            03 ws-ind-lock              pic 9(05) value zeros.
            03 ws-ctrl-etapa            pic x(99).
            03 ws-indice-exe            pic 9(02) value zeros.
            03 ws-ds-arquivo            pic x(15) value spaces.
            03 ws-registros-lidos       pic 9(10) value zeros.
            03 ws-registros-gravados    pic 9(10) value zeros.
            03 ws-cd-empresa-origem-log  pic 9(03) value zeros.
            03 ws-cd-filial-origem-log   pic 9(04) value zeros.
            03 ws-cd-empresa-destino-log pic 9(03) value zeros.
            03 ws-cd-filial-destino-log  pic 9(04) value zeros.
            03 ws-diretorio-origem      pic x(256) value spaces.
            03 ws-diretorio-destino     pic x(256) value spaces.
            03 ws-valor-combo-dir       pic x(40) value spaces.

       01   wt-tabela-execucao.
            03 filler                   pic x(08) value "PWSTB004".
            03 filler                   pic x(08) value "PWSTB005".
            03 filler                   pic x(08) value "PWSTB006".
            03 filler                   pic x(08) value "PWSTB007".
            03 filler                   pic x(08) value "PWSTB008".
            03 filler                   pic x(08) value "PWSTB009".
            03 filler                   pic x(08) value "PWSTB010".
            03 filler                   pic x(08) value "PWSTB011".
            03 filler                   pic x(08) value "PWSTB012".
            03 filler                   pic x(08) value "PWSTB019".
            03 filler                   pic x(08) value "PWSTB020".
            03 filler                   pic x(08) value "PWSTB013".
            03 filler                   pic x(08) value "PWSTB014".
            03 filler-finalizador       pic x(08) value spaces.

       01   filler redefines wt-tabela-execucao.
            03 filler occurs 99 times.
               05 wt-nome-programa      pic x(08).

       01   cgi-input is external-form.
            copy PCW902.CPY.
            03 f-campos-formulario.
               05 f-origem               pic 9(01) identified by "origem".
               05 f-id-diagnostico       pic x(01) identified by "idDiagnostico".
                  88 somente-diagnostico     value "S".
               05 f-unidade-resultado    pic x(196) identified by "unidadeResultado".
               05 f-dir-aux-escolhido    pic x(40) identified by "dirAuxEscolhido".
               05 f-ds-diretorio-auxiliar pic x(100) identified by "dsDiretorioAuxiliar".

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
            move spaces                 to whs-mensagem
            move spaces                 to lnk-id-label-log
            move zeros                  to ws-padrao-display
            perform 1100-divisao-processamento.
       1000-exit.
            exit.

      *>===================================================================================
       2000-processamento section.
       2000.
            if   f-opcao = 6
                 perform 2110-carrega-efdpar
                 perform 8000-finaliza-controle
            else
                 evaluate f-frame
                     when 0
                         perform 2000-controle-programa
                     when 9
                         perform 2999-controle-frame
                     when other
                         move "Frame inválido" to whs-mensagem
                         perform 8000-mensagem
                 end-evaluate
            end-if.
       2000-exit.
            exit.

      *>===================================================================================
       2000-controle-programa section.
       2000.
            evaluate f-opcao
                when 0
                    perform 8000-tela
                when other
                    move "Opção inválida" to whs-mensagem
                    perform 8000-mensagem
            end-evaluate.
       2000-exit.
            exit.

      *>===================================================================================
       2100-controles-adicionais section.
       2100.
            perform 2105-carrega-dir-auxiliares
            perform 2110-carrega-efdpar
            perform 8500-ctrl-tela-inicial.
       2100-exit.
            exit.

      *>===================================================================================
       2100-verifica-execucao section.
       2100.
       2100-exit.
            exit.

      *>===================================================================================
       2105-carrega-dir-auxiliares section.
       2105.
            inspect lnk-integpath replacing all "\" by "/"

            move "dirAuxEscolhido"      to ws-nome-combo
            move "dirAuxEscolhido"      to ws-nome-objeto

            move spaces                 to ws-valor-combo-dir
            string lnk-integpath delimited by " " "dataux"
                   into ws-valor-combo-dir
            move "Dataux (Raíz)"        to ws-option-text
            perform 8500-adiciona-combo-dir

            move spaces                 to wid-stb-cfg
            string lnk-integpath delimited by " " "dataux"
                   "/STB.CFG"
                   into wid-stb-cfg
            close stb-cfg
            open input stb-cfg
            if   ws-operacao-ok
                 initialize             sc-registro-stb-cfg
                 read stb-cfg next
                 perform until not ws-operacao-ok
                      move spaces       to ws-valor-combo-dir
                      string lnk-integpath delimited by " "
                             "dataux/"
                             sc-cd-filial
                             into ws-valor-combo-dir
                      close pd99800
                      move spaces       to wid-pd99800
                      string ws-valor-combo-dir delimited by " "
                             "/EFDPAR.CAD"
                             into wid-pd99800
                      open input pd99800
                      if   ws-operacao-ok
                           move spaces  to ws-option-text
                           string sc-cd-empresa "."
                                  sc-cd-filial " - "
                                  sc-ds-unb delimited by "  "
                                  into ws-option-text
                           perform 8500-adiciona-combo-dir
                      end-if
                      read stb-cfg next
                 end-perform
            end-if

            close stb-cfg.
       2105-exit.
            exit.

      *>===================================================================================
       2110-carrega-efdpar section.
       2110.
            move "unidade"              to ws-nome-combo
            move "unidade"              to ws-nome-objeto

            if   f-origem = 2
                 perform 2120-monta-label-efdpar-dataux
            else
                 perform 2120-monta-label-efdpar-databas
            end-if

            close pd99800
            open input pd99800
            if   ws-operacao-ok
                 initialize             fpare-revenda
                 perform 9000-str-pd99800-nlss
                 perform 9000-ler-pd99800-nex
                 perform until not ws-operacao-ok
                      if   fpare-codigo-registro = 1
                      and  fpare-sequencia       = 1
                           move spaces  to ws-option-value
                           string fpare-cd-empresa fpare-cd-filial
                                  into ws-option-value
                           move spaces  to ws-option-text
                           string fpare-cd-empresa "." fpare-cd-filial " - " fpare-dsc-abrev-filial
                                  into ws-option-text
                           perform 8100-adiciona-combo
                      end-if
                      perform 9000-ler-pd99800-nex
                 end-perform
            end-if
            close pd99800.
       2110-exit.
            exit.

      *>===================================================================================
       2120-monta-label-efdpar-databas section.
       2120.
            move spaces                 to wid-pd99800
            string lnk-dtbpath delimited by " "
                   "/EFDPAR.CAD" delimited by size
                   into wid-pd99800.
       2120-exit.
            exit.

      *>===================================================================================
       2120-monta-label-efdpar-dataux section.
       2120.
            move spaces                 to wid-pd99800
            string f-ds-diretorio-auxiliar delimited by " "
                   "/EFDPAR.CAD" delimited by size
                   into wid-pd99800.
       2120-exit.
            exit.

      *>===================================================================================
       2200-ctrl-parametros section.
       2200.
            if   not lnk-rotina-configuracao
                 move "Execução somente permitida pelas rotinas especiais" to whs-mensagem
                 perform 8000-fecha-janela-rotina
            end-if

            perform 9000-monta-label-arquivo-temp
            move wid-temporario         to wid-arqwor
            close arqwor
            open output arqwor
            if   not ws-operacao-ok
            and  ws-resultado-acesso <> "05"
                 perform 9000-monta-status
                 move spaces            to whs-mensagem
                 string "Erro ao abrir i-o arquivo temporário - Status: " ws-status into whs-mensagem
                 perform 8000-fecha-janela-rotina
            end-if
            initialize                  rw-empresa-filial
            move 1                      to rw-cd-registro
            read arqwor
            move f-origem               to rw-de-path-para-path
            move f-id-diagnostico       to rw-id-diagnostico
            move f-unidade-resultado    to rw-tabela-emp-fil
            move f-dir-aux-escolhido    to rw-dir-aux-escolhido
            move f-ds-diretorio-auxiliar to rw-ds-diretorio-auxiliar
            if   ws-operacao-ok
                 rewrite rw-empresa-filial
                 if   not ws-operacao-ok
                      perform 9000-monta-status
                      move spaces       to whs-mensagem
                      string "Erro ao atualizar arquivo temporário - Status: " ws-status into whs-mensagem
                      perform 8000-fecha-janela-rotina
                 end-if
            else
                 write rw-empresa-filial
                 if   not ws-operacao-ok
                      perform 9000-monta-status
                      move spaces       to whs-mensagem
                      string "Erro ao gravar arquivo temporário - Status: " ws-status into whs-mensagem
                      perform 8000-fecha-janela-rotina
                 end-if
            end-if
            close arqwor
            move spaces                 to whs-mensagem.
       2200-exit.
            exit.

      *>===================================================================================
       6000-processamento-background section.
       6000.
            perform 6100-abre-arquivos
            perform 6200-processo
            perform 6300-fecha-arquivos.
       6000-exit.
            exit.

      *>===================================================================================
       6100-abre-arquivos section.
       6100.
       6100-exit.
            exit.

      *>===================================================================================
       6200-processo section.
       6200.
            perform 9000-inicio-processo

            move 0                      to lnk-status-erro

            if   lnk-sem-erro
                 perform 6201-carrega-variaveis-tela
                 close arqwor
            end-if

            if   lnk-sem-erro
                 perform 6202-carrega-linkage-diretorios
            end-if

            if   lnk-sem-erro
                 perform 6250-diagnostico-efdpar
            end-if

            if   not lnk-com-erro
                 perform 6203-executa-programas
            end-if

            if   not lnk-com-erro
            and  not somente-diagnostico
                 perform 6251-conversao-efdpar
            end-if

            if   somente-diagnostico
                 move atencao-processo    to lnk-status-processo
            else
                 if   not lnk-com-erro
                      move processo-ok       to lnk-status-processo
                 else
                      move erro-processo     to lnk-status-processo
                 end-if
            end-if

            perform 9000-finaliza-processo.
       6200-exit.
            exit.

      *>===================================================================================
       6201-carrega-variaveis-tela section.
       6201.
            move spaces            to lnk-detalhe-processo
            move spaces            to lnk-etapa-processo
            move "PWSTB003;;;;Carregando informações para linkage;;" to lnk-etapa-processo
            perform 9000-display-loop

            perform 9000-monta-label-arquivo-temp
            move wid-temporario         to wid-arqwor
            close arqwor
            open input arqwor
            if   not ws-operacao-ok
                 move "PWSTB003;temporário;;;Erro ao abrir arquivo;;" to lnk-ocorrencia-log
                 perform 9000-log-erro-parar-processo
                 exit section
            end-if

            initialize                  rw-empresa-filial
            move 1                      to rw-cd-registro
            start arqwor key is not less rw-chave
            read arqwor next
            if   not ws-operacao-ok
            or   rw-cd-registro <> 1
                 move "PWSTB003;temporário;;;Erro ao ler arquivo;;" to lnk-ocorrencia-log
                 perform 9000-log-erro-parar-processo
                 exit section
            end-if

            move rw-id-diagnostico      to f-id-diagnostico
            move rw-de-path-para-path   to f-origem
            move rw-tabela-emp-fil      to f-unidade-resultado
            move rw-dir-aux-escolhido   to f-dir-aux-escolhido
            move rw-ds-diretorio-auxiliar to f-ds-diretorio-auxiliar.
       6201-exit.
            exit.

      *>===================================================================================
       6202-carrega-linkage-diretorios section.
       6202.
            move lnk-dtbPath            to ld-database
            move f-ds-diretorio-auxiliar to ld-auxiliar

            if   ld-auxiliar equal spaces
                  string lnk-integpath delimited by " " "dataux" into ld-auxiliar
            end-if

            move 998                    to ws-contador-display
            move spaces                 to lnk-detalhe-processo
            move spaces                 to lnk-etapa-processo
            string "PWSTB003;;;;Diretório database: " ld-database delimited by "  " ";;"
                   into lnk-etapa-processo
            perform 9000-display-loop

            move 998                    to ws-contador-display
            move spaces                 to lnk-detalhe-processo
            move spaces                 to lnk-etapa-processo
            string "PWSTB003;;;;Diretório auxiliar: " ld-auxiliar delimited by "  " ";;"
                   into lnk-etapa-processo
            perform 9000-display-loop

            move ld-diretorios          to lnk-linha-comando.
       6202-exit.
            exit.

      *>===================================================================================
       6203-executa-programas section.
       6203.
            if   somente-diagnostico
                 move "Diagnóstico"     to ws-acao-pwstb003
            else
                 move "Conversão"       to ws-acao-pwstb003
            end-if

            move 998                    to ws-contador-display
            move spaces                 to lnk-detalhe-processo
            move spaces                 to lnk-etapa-processo
            string "PWSTB003;;;;Iniciando - " ws-acao-pwstb003 delimited by "  " " dos arquivos do Sistema;;;"
                   into lnk-etapa-processo
            perform 9000-display-loop

            move f-origem               to lk-de-path-para-path
            if   somente-diagnostico
                 move 1                 to lk-id-diagnostico
            else
                 move 0                 to lk-id-diagnostico
            end-if
            move f-unidade-resultado    to lk-tabela-emp-fil

            move lk-empresa-filial      to lnk-auxiliar

            perform 6204-processa-tabela-execucao

            if   lnk-com-erro
                 move spaces            to lnk-detalhe-processo
                 move spaces            to lnk-etapa-processo
                 string "PWSTB003;;;;Processado com ERRO - " ws-acao-pwstb003 delimited by "  " " dos arquivos do Sistema;;"
                        into lnk-etapa-processo
                 perform 9000-display-loop
            else
                 if   lnk-sem-erro
                      move spaces            to lnk-detalhe-processo
                      move spaces            to lnk-etapa-processo
                      string "PWSTB003;;;;Processado com SUCESSO - " ws-acao-pwstb003 delimited by "  " " dos arquivos do Sistema;;"
                             into lnk-etapa-processo
                      perform 9000-display-loop
                 else
                      move spaces            to lnk-detalhe-processo
                      move spaces            to lnk-etapa-processo
                      string "PWSTB003;;;;Processado com ADVERTÊNCIA - " ws-acao-pwstb003 delimited by "  " " dos arquivos do Sistema;;"
                             into lnk-etapa-processo
                      perform 9000-display-loop
                 end-if
            end-if.
       6203-exit.
            exit.

      *>===================================================================================
       6204-processa-tabela-execucao section.
       6204.
            perform varying ws-indice-exe from 1 by 1 until wt-nome-programa( ws-indice-exe ) = spaces
                                                      or    lnk-com-erro

                 move spaces            to lnk-detalhe-processo
                 move spaces            to lnk-etapa-processo
                 string wt-nome-programa( ws-indice-exe ) ";;;;Iniciando;;"
                        into lnk-etapa-processo
                 perform 9000-display-loop

                 call wt-nome-programa( ws-indice-exe ) using lnk-par lnk-complementar-processo
                 cancel wt-nome-programa( ws-indice-exe )

                 if   somente-diagnostico
                      move spaces            to lnk-detalhe-processo
                      move spaces            to lnk-etapa-processo
                      string wt-nome-programa( ws-indice-exe ) ";;;;Processado;;"
                             into lnk-etapa-processo
                      perform 9000-display-loop
                 else
                      if   lnk-com-erro
                           move spaces            to lnk-detalhe-processo
                           move spaces            to lnk-etapa-processo
                           string wt-nome-programa( ws-indice-exe ) ";;;;Processado com ERRO;;"
                                  into lnk-etapa-processo
                           perform 9000-display-loop
                      else
                           if   lnk-sem-erro
                                move spaces            to lnk-detalhe-processo
                                move spaces            to lnk-etapa-processo
                                string wt-nome-programa( ws-indice-exe ) ";;;;Processado com SUCESSO;;"
                                       into lnk-etapa-processo
                                perform 9000-display-loop
                           else
                                move spaces            to lnk-detalhe-processo
                                move spaces            to lnk-etapa-processo
                                string wt-nome-programa( ws-indice-exe ) ";;;;Processado com ADVERTÊNCIA;;"
                                       into lnk-etapa-processo
                                perform 9000-display-loop
                           end-if
                      end-if
                 end-if
            end-perform.
       6204-exit.
            exit.

      *>===================================================================================
       6250-diagnostico-efdpar section.
       6250.
            move f-origem               to lk-de-path-para-path
            move 1                      to lk-id-diagnostico
            move f-unidade-resultado    to lk-tabela-emp-fil
            move "Diagnóstico"          to ws-acao-pwstb003
            perform 6255-efdpar.
       6250-exit.
            exit.

      *>===================================================================================
       6251-conversao-efdpar section.
       6251.
            move f-origem               to lk-de-path-para-path
            move f-unidade-resultado    to lk-tabela-emp-fil
            move 0                      to lk-id-diagnostico
            move "Conversão"            to ws-acao-pwstb003
            perform 6255-efdpar.
       6251-exit.
            exit.

      *>===================================================================================
       6255-efdpar section.
       6255.
            move "EFDPAR"               to ws-ds-arquivo

            move spaces                 to lnk-detalhe-processo
            move spaces                 to lnk-etapa-processo
            string lnk-rotina delimited by " " ";"
                   ws-ds-arquivo delimited by " " ";;;"
                   "Iniciando - " ws-acao-pwstb003 delimited by "  " ";;"
                   into lnk-etapa-processo
            perform 9000-display-loop

            if   database-dataux
                 move ld-database       to ws-diretorio-origem
                 move ld-auxiliar       to ws-diretorio-destino
            else
                 move ld-auxiliar       to ws-diretorio-origem
                 move ld-database       to ws-diretorio-destino
            end-if

            move spaces                 to lnk-detalhe-processo
            move spaces                 to lnk-etapa-processo
            string lnk-rotina delimited by " " ";"
                   ws-ds-arquivo delimited by " " ";;;"
                   "Executando - " ws-acao-pwstb003 delimited by "  " ";;"
                   into lnk-etapa-processo
            perform 9000-display-loop

            close pd99800 px99800

            move zeros                  to ws-registros-lidos ws-registros-gravados
            move 0                      to lnk-status-erro
            perform 6255-1-efdpar-consiste-origem
            if   lnk-sem-erro
                 perform 6255-2-efdpar-consiste-destino
            end-if
            if   lnk-sem-erro
            and  not diagnostico
                 perform 6255-3-efdpar-atualiza-destino
                 if   lnk-sem-erro
                      perform 6255-4-efdpar-finaliza-execucao
                 end-if
            end-if

            close pd99800 px99800

            if   lnk-com-erro
                 move spaces            to lnk-detalhe-processo
                 move spaces            to lnk-etapa-processo
                 string lnk-rotina delimited by " " ";"
                        ws-ds-arquivo delimited by " " ";;;"
                        "Processado com ERRO - " ws-acao-pwstb003 delimited by "  " ";;"
                        into lnk-etapa-processo
                 perform 9000-display-loop
            else
                 if   lnk-sem-erro
                      move spaces            to lnk-detalhe-processo
                      move spaces            to lnk-etapa-processo
                      string lnk-rotina delimited by " " ";"
                             ws-ds-arquivo delimited by " " ";;;"
                             "Processado com SUCESSO - " ws-acao-pwstb003 delimited by "  " ";;"
                             into lnk-etapa-processo
                      perform 9000-display-loop
                 else
                      move spaces            to lnk-detalhe-processo
                      move spaces            to lnk-etapa-processo
                      string lnk-rotina delimited by " " ";"
                             ws-ds-arquivo delimited by " " ";;;"
                             "Processado com ADVERTÊNCIA - " ws-acao-pwstb003 delimited by "  " ";;"
                             into lnk-etapa-processo
                      perform 9000-display-loop
                 end-if
            end-if.
       6255-exit.
            exit.

      *>===================================================================================
       6255-1-efdpar-consiste-origem section.
       6255-1.
            string ws-diretorio-origem delimited by  " " "/EFDPAR.CAD"
                   into wid-pd99800
            open input pd99800
            if   ws-arquivo-inexistente
                 move "Arquivo de origem inexistente" to lnk-ocorrencia-log
                 perform 9000-grava-log-warning-2
            end-if
            if   lnk-sem-erro
                 perform 9000-verifica-status-arquivo-2
            end-if
            if   not lnk-sem-erro
                 exit section
            end-if
            initialize fparm-mestre
            perform 9000-str-pd99800-nlss
            perform 9000-ler-pd99800-nex
            if   not ws-operacao-ok
            and  ws-resultado-acesso <> 46
                 move "Erro na leitura do arquivo de origem" to lnk-ocorrencia-log
                 perform 9000-grava-erro-log-2
            end-if.
       6255-1-exit.
            exit.

      *>===================================================================================
       6255-2-efdpar-consiste-destino section.
       6255-2.
            string ws-diretorio-destino delimited by " " "/EFDPAR.CAD"
                   into wid-px99800
            if   diagnostico
                 open input px99800
                 if   not ws-operacao-ok
                 and  not ws-arquivo-inexistente
                      move "Erro na abertura input do arquivo de destino" to lnk-ocorrencia-log
                      perform 9000-grava-erro-log-2
                 end-if
                 if   ws-arquivo-inexistente
                      exit section
                 end-if
            else
                 open i-o px99800
                 if   ws-resultado-acesso = "05"
                      close px99800
                      open output px99800
                 end-if
                 if   not ws-operacao-ok
                      move "Erro na abertura i-o do arquivo de destino" to lnk-ocorrencia-log
                      perform 9000-grava-erro-log-2
                 end-if
            end-if
            if   not lnk-sem-erro
                 exit section
            end-if
            perform varying wx-index-1 from 1 by 1
            until wx-index-1 > ws-limite-filiais
            or   lk-cd-empresa-destino (wx-index-1) = 0
                 initialize xparm-mestre
                 move lk-cd-empresa-destino (wx-index-1) to xparm-cd-empresa
                 move lk-cd-filial-destino (wx-index-1)  to xparm-cd-filial
                 start px99800 key is not less xparm-chave
                 read px99800 next
                 if   ws-operacao-ok
                 and  lk-cd-empresa-destino (wx-index-1) = xparm-cd-empresa
                 and  lk-cd-filial-destino (wx-index-1)  = xparm-cd-filial
                      move "Empresa/Filial já existe no arquivo de destino" to lnk-ocorrencia-log
                      perform 9000-grava-erro-log
                 end-if
            end-perform.
       6255-2-exit.
            exit.

      *>===================================================================================
       6255-3-efdpar-atualiza-destino section.
       6255-3.
            initialize fparm-mestre
            perform 9000-ler-pd99800-ran
            if   ws-operacao-ok
                 move fparm-mestre      to xparm-mestre
                 write xparm-mestre
                 if   not ws-operacao-ok
                 and  not ws-registro-existente
                      add 1             to ws-registros-lidos
                      perform 9000-verif-sts-arq-convertido
                 end-if
            end-if
            perform varying wx-index-1 from 1 by 1
            until wx-index-1 > ws-limite-filiais
            or   lk-cd-empresa-origem (wx-index-1) = 0
                 initialize fparm-mestre
                 move lk-cd-empresa-origem (wx-index-1) to fparm-cd-empresa
                 move lk-cd-filial-origem (wx-index-1)  to fparm-cd-filial
                 perform 9000-str-pd99800-nlss
                 perform 9000-ler-pd99800-nex
                 perform until not ws-operacao-ok
                         or lk-cd-empresa-origem (wx-index-1) <> fparm-cd-empresa
                         or lk-cd-filial-origem (wx-index-1)  <> fparm-cd-filial
                         or lnk-com-erro
                     perform 6255-3-1-efdpar-converte
                     perform 9000-ler-pd99800-nex
                 end-perform
                 if   not lnk-sem-erro
                      exit perform
                 end-if
            end-perform.
       6255-3-exit.
            exit.

      *>===================================================================================
       6255-3-1-efdpar-converte section.
       6255-3-1.
            move fparm-chave            to lnk-chave-processo
            perform 9000-display-loop
            add 1                                   to ws-registros-lidos
            move fparm-mestre                       to xparm-mestre
            move lk-cd-empresa-destino (wx-index-1) to xparm-cd-empresa
            move lk-cd-filial-destino (wx-index-1)  to xparm-cd-filial
            write xparm-mestre
            perform 9000-verif-sts-arq-convertido.
       6255-3-1-exit.
            exit.

      *>===================================================================================
       6255-4-efdpar-finaliza-execucao section.
       6255-4.
            close px99800
            open input px99800
            if   not ws-operacao-ok
                 move "Erro na abertura input do arquivo de destino gerado" to lnk-ocorrencia-log
                 perform 9000-grava-erro-log-2
            end-if
            if   lnk-sem-erro
                 initialize xparm-mestre
                 start px99800 key is not less xparm-chave
                 read px99800 next
                 if   not ws-operacao-ok
                 and  ws-resultado-acesso <> "46"
                      move "Arquivo de destino convertido com erro" to lnk-ocorrencia-log
                      perform 9000-grava-erro-log-2
                 else
                      if   ws-resultado-acesso = "46"
                           move "Arquivo de destino convertido com sucesso - vazio" to lnk-ocorrencia-log
                           perform 9000-grava-log-warning-2
                      else
                           move "Arquivo de destino convertido com sucesso" to lnk-ocorrencia-log
                           perform 9000-grava-log-warning-2
                      end-if
                 end-if
            end-if.
       6255-4-exit.
            exit.

      *>===================================================================================
       6300-fecha-arquivos section.
       6300.
       6300-exit.
            exit.

      *>===================================================================================
       3000-finalizacao section.
       3000.
            perform 8000-oculta-gif.
       3000-exit.
            exit.

      *>===================================================================================

      *>===================================================================================
       8000-scripts-adicionais section.
       8000.
            perform 8000-script-funcao-senha
            exec html
               <SCRIPT>
     //            relatorio = 1; //para excluir arquivo temporário ao fechar a janela
                   function Confirmar(){
                       if( document.all.unidadeResultadoLst.length < 1 ) {
                           alert( 'Nenhum resultado foi selecionado' );
                           document.all.unidade.focus();
                           return false;
                       }
                       if( ! VerificaDuplicidadeLista( 'origem' ) ) {
                           return false;
                       }
                       if( ! VerificaDuplicidadeLista( 'destino' ) ) {
                           return false;
                       }
                       if( !Senha997OK() ){
                           return false;
                       }
                       CarregaLista();
                       IniciarProcesso();
                   }
                   function CarregaLista() {
                       var tex = '';
                       var empO = '';
                       var filO = '';
                       var empD = '';
                       var filD = '';
                       var res = '';
                       for( i = 0; i < document.all.unidadeResultadoLst.length; i++ ){
                           tex = document.all.unidadeResultadoLst[i].value;
                           empO = tex.substring( 0, 3 );
                           filO = tex.substring( 4, 8 );
                           empD = tex.substring( 11, 14 );
                           filD = tex.substring( 15, 19 );
                           res += empO + filO + empD + filD;
                       }
                       document.all.unidadeResultado.value = res + Replicate( '0', 196 - res.length );
                       return true;
                   }
                   function VerificaDuplicidadeLista( ind ) {
                       var itexto = '';
                       var iempresa = '000';
                       var ifilial = '0000';
                       var jtexto = '';
                       var jempresa = '000';
                       var jfilial = '0000';
                       var posEmpIni = 0;
                       var posFilIni = 0;
                       var posEmpFim = 0;
                       var posFilFim = 0;
                       if( ind == 'origem' ) {
                           posEmpIni = 0;
                           posEmpFim = posEmpIni + 3;
                           posFilIni = posEmpFim + 1;
                           posFilFim = posFilIni + 4;
                       }
                       else {
                           posEmpIni = 11;
                           posEmpFim = posEmpIni + 3;
                           posFilIni = posEmpFim + 1;
                           posFilFim = posFilIni + 4;
                       }
                       for( i = 0; i < document.all.unidadeResultadoLst.length; i++ ){
                           itexto = document.all.unidadeResultadoLst[i].value;
                           iempresa = document.all.unidadeResultadoLst[i].value.substring( posEmpIni, posEmpFim );
                           ifilial = document.all.unidadeResultadoLst[i].value.substring( posFilIni, posFilFim );
                           for( j = 0; j < document.all.unidadeResultadoLst.length; j++ ){
                               jtexto = document.all.unidadeResultadoLst[j].value;
                               jempresa = document.all.unidadeResultadoLst[j].value.substring( posEmpIni, posEmpFim );
                               jfilial = document.all.unidadeResultadoLst[j].value.substring( posFilIni, posFilFim );
                               if( i != j ) {
                                   if( iempresa == jempresa && ifilial == jfilial ) {
                                      alert( 'Foi verificada duplicidade de ocorrências da empresa/filial ' + ind + ' ' + jempresa + '/' + jfilial );
                                      document.all.unidade.focus();
                                      return false;
                                   }
                               }
                           }
                       }
                       return true;
                   }
                   function AlteraCombo( desc1, desc2 ){
                       if( document.all (desc1).value == 1 ){
                           document.all.dirAuxEscolhido.selectedIndex = 0;
                           document.all.dirAuxEscolhido.disabled = true;
                           document.all (desc2).value = 2;
                           document.all.dsDiretorioAuxiliar.value = "";
                           document.all.dsDiretorioAuxiliar.disabled = true;
                       }
                       else{
                           document.all.dirAuxEscolhido.disabled = false;
                           document.all (desc2).value = 1;
                           document.all.dsDiretorioAuxiliar.disabled = false;
                           document.all.dsDiretorioAuxiliar.value = document.all.dirAuxEscolhido.value;
                       }
                       AlteraComboUnidades();
                   }
                   function IncluiItem( cod ){
                       oOption = document.createElement( 'OPTION' );
                       oOption.text = cod;
                       oOption.value = cod;
                       document.all.unidadeResultadoLst.add( oOption );
                       document.all.unidadeResultadoLst.size = document.all.unidadeResultadoLst.size + 1;
                       oOption = null;
                       document.all.unidade.value = "";
                       document.all.cdEmpresaDestino.value = "";
                       document.all.cdFilialDestino.value = "";
                   }
                   function AdicionaItem(){
                      var nMaxElementos = 14;
                      if( document.all.unidadeResultadoLst.length >= nMaxElementos
                      && document.all.controleMensagem.value != 1 ){
                          alert( 'Todos os itens já foram adicionados' );
                          document.all.controleMensagem.value = 1;
                          return false;
                      }
                      else{
                          document.all.controleMensagem.value = 2;
                      }
                      if( ! ValidaCampos( 'obr1' ) ){
                          return false;
                      }
                      if( document.all.origem.value == '2' ) {
                          if( document.all.dirAuxEscolhido.selectedIndex > 0 ) {
                              if( document.all.unidade( document.all.unidade.selectedIndex ).innerHTML.substring( 0, 8 ) != document.all.dirAuxEscolhido( document.all.dirAuxEscolhido.selectedIndex ).innerHTML.substring( 0, 8 ) ) {
                                  alert( 'Empresa auxiliar/Filial do diretório auxiliar disponível difere da Empresa/Filial escolhida para origem' );
                                  document.all.dirAuxEscolhido.focus();
                                  return false;
                              }
                          }
                      }
                      var resultado = '';
                      var empO = document.all.unidade.value.substring( 0,3 );
                      var filO = document.all.unidade.value.substring( 3,8 );
                      var empD = document.all.cdEmpresaDestino.value;
                      var filD = document.all.cdFilialDestino.value;
                      var nome = document.all.unidade[ document.all.unidade.selectedIndex ].innerHTML.substring( 11, 31 );
                      resultado += empO;
                      resultado += '/';
                      resultado += filO;
                      resultado += ' - ';
                      resultado += Replicate( '0', 3 - empD.length ) + empD;
                      resultado += '/';
                      resultado += Replicate( '0', 4 - filD.length ) + filD;
                      resultado += ' - ';
                      resultado += nome;
                      IncluiItem( resultado );
                   }
                   function RemoverItem(){
                      var xRemover = document.all.unidadeResultadoLst.value;
                      if( document.all.unidadeResultadoLst.selectedIndex < 0 ){
                          alert( 'Nenhum item selecionado' );
                          document.all.unidadeResultadoLst.focus();
                          return false;
                      }
                      for( i = 0; i < document.all.unidadeResultadoLst.length; i++ ){
                          if( document.all.unidadeResultadoLst[i].value == xRemover ) {
                              nIndice = i;
                              break;
                          }
                      }
                      document.all.unidadeResultadoLst.remove( nIndice );
                      if( document.all.unidadeResultadoLst.length != nIndice ){
                          document.all.unidadeResultadoLst.selectedIndex = nIndice;
                      }
                      else{
                          document.all.unidadeResultadoLst.selectedIndex = nIndice - 1;
                      }
                      if( document.all.unidadeResultadoLst.length <= 0 ){
                          document.all.unidade.focus();
                      }
                   }
                   function CarregaDestino() {
                      if( parseInt( document.all.unidade[ document.all.unidade.selectedIndex ].innerHTML.substring( 0, 3 ), 10 ) > 0 ) {
                          document.all.cdEmpresaDestino.value = document.all.unidade[ document.all.unidade.selectedIndex ].innerHTML.substring( 0, 3 );
                          document.all.cdFilialDestino.value = document.all.unidade[ document.all.unidade.selectedIndex ].innerHTML.substring( 4, 8 );
                      }
                      else {
                          document.all.cdEmpresaDestino.value = '';
                          document.all.cdFilialDestino.value = '';
                      }
                   }
                   function AlteraComboUnidades() {
                       var tam = document.all.unidade.length + 1;
                       for( i=1; i <= tam; i++ ) {
                           document.all.unidade.remove( 1 );
                       }
                       document.all.cdEmpresaDestino.value = '';
                       document.all.cdFilialDestino.value = '';
                       var tam2 = document.all.unidadeResultadoLst.length + 1;
                       for( i=0; i <= tam2; i++ ) {
                           document.all.unidadeResultadoLst.remove( 0 );
                       }
                       document.all.opcao.value = 6;
                       EnviarFormulario();
                   }
               </SCRIPT>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
       8000-tela section.
       8000.
            perform 8000-scripts-tela
            exec html
               <INPUT type=hidden name=controleMensagem>
               <INPUT type=hidden name=unidadeResultado>
               <TABLE width=100%>
                  <TR>
                     <TD>
               <TABLE rules=none width=100% :tabela-padrao>
                  <TR>
                     <TD colspan=2>&nbsp;
                  <TR>
                     <TD colspan=2 class=titulocampo><DIV align=center>:lnk-rotina [:versao]&nbsp;&nbsp;-&nbsp;&nbsp;STB - Sistema de Transferência de Bases</DIV>
                  <TR>
                     <TD colspan=2>&nbsp;
                  <TR>
                     <TD colspan=2>
                        <TABLE border=0 width=100% rules=none :tabela-padrao>
                           <TR>
                              <TD width=10% class=titulocampo>Origem&nbsp;
                              <TD width=15%><SELECT tabindex=1 name=origem class=combo onChange="AlteraCombo( 'origem', 'destino' )">
                                     <OPTION selected value=1>Database</OPTION>
                                     <OPTION value=2>Auxiliar</OPTION>
                                  </SELECT>

                              <TD width=25% class=titulocampo noWrap>Diretórios auxiliares&nbsp;
                              <TD width=25%><SELECT tabindex=1 name=dirAuxEscolhido class=combo disabled onChange="AlteraComboUnidades()"></SELECT>

                              <TD width=10%><DIV align=right><INPUT type=checkbox tabindex=3 name=idDiagnostico value="S" checked>
                              <TD width=15% class=titulocampo><DIV align=left>Diagnóstico</DIV>

                           <TR>
                              <TD class=titulocampo>Destino&nbsp;
                              <TD colspan=5><SELECT tabIndex=2 name=destino class=combo onChange="AlteraCombo( 'destino', 'origem' )" disabled>
                                     <OPTION value=1>Database</OPTION>
                                     <OPTION selected value=2>Auxiliar</option>
                                  </SELECT>
                  <TR>
                              <TD class=titulocampo>Diretório&nbsp;
                              <TD colspan=6><INPUT type=text tabIndex=4 name=dsDiretorioAuxiliar size=100 maxLength=100 class=campo disabled onBlur="AlteraComboUnidades()">
                        </TABLE>
                  <TR>
                     <TD colspan=2><HR></HR>
                  <TR>
                     <TD colspan=2>
                        <TABLE border=0 width=100% rules=none :tabela-padrao>
                           <TR>
                              <TD width=12%>&nbsp;
                              <TD width=27% class=titulocampo><DIV align=center>Origem</DIV>
                              <TD width=20% class=titulocampo><DIV align=center>Destino</DIV>
                              <TD class=titulocampo><DIV align=center>Resultado</DIV>
                           <TR>
                              <TD class=titulocampo>Empresa/Filial&nbsp;
                              <TD nowrap><SELECT name=unidade tabindex=4 class=combo style="width:195;" obr1=1 onChange="CarregaDestino();">
                                            <OPTION value="">--Selecionar--</OPTION>
                                         </SELECT>
                              <TD><INPUT type=text tabIndex=5 name=cdEmpresaDestino size=5 maxLength=03 class=campo onKeyPress="return SomenteNumeros()" obr1=1><label class=titulocampo>&nbsp;/</label>
                                  <INPUT type=text tabIndex=6 name=cdFilialDestino size=5 maxLength=04 class=campo onKeyPress="return SomenteNumeros()" obr1=1>
                                  &nbsp;<BUTTON name=BotAdic style="height:20;" tabIndex=7 onClick="AdicionaItem();" title="Adiciona Item"><LABEL>&nbsp;>&nbsp;</LABEL></BUTTON>
                              <TD rowspan=3>
                                  <SELECT tabIndex=8 name=unidadeResultadoLst class="combo" size=4 style="width:300; height=63"
                                   multiple onKeyDown="VerificaTeclaPress( '46', 'RemoverItem()' ); VerificaTeclaPress( 13,'event.keyCode=9;' )">
                                  </SELECT>
                           <TR>
                              <TD colspan=3 rowspan=2>&nbsp;
                        </TABLE>
                  <TR>
                     <TD colspan=2><HR></HR>
                  <TR>
                     <TD colspan=2>&nbsp;
                  <TR>
                     <TD colspan=2>
                        <TABLE width=100%>
                           <TR>
                              <TD class=titulocampo>Etapa&nbsp;
                              <TD colspan=3><INPUT type=text name=etapa size=50 class=clsDisabled DISABLED>
                           <TR>
                              <TD class=titulocampo>Processo&nbsp;
                              <TD><INPUT type=text name=displayLoop size=30 class=clsDisabled DISABLED>
                              <TD class=titulocampo>Última atividade&nbsp;
                              <TD><INPUT type=text name=dataHora size=20 class=clsDisabled DISABLED>
                           <TR>
                              <TD colspan=4>&nbsp;
                           <TR>
                              <TD class=titulocampo>LOG do Processo&nbsp;
                              <TD><INPUT type=text name=logFile size=50 class=clsDisabled DISABLED>
                              <TD class=titulocampo>Status&nbsp;
                              <TD><INPUT type=text name=statusProcesso size=20 class=clsDisabled DISABLED>
                        </TABLE>
                  <TR>
                     <TD colspan=2>&nbsp;
               </TABLE>
            end-exec
            perform 8000-botao-confirmar
            exec html
               <SCRIPT>
                  document.all.DivBtReservado.innerHTML = '<BUTTON name=BotExibirLog AccessKEY="L" onClick="ExibirLog();" class="botao" tabindex=102> <LABEL>Exibir <U>L</U>og</LABEL></BUTTON>';
               </SCRIPT>
            end-exec
            perform 2001-carrega-tela-inicial.
       8000-exit.
            exit.

      *>===================================================================================
       8500-display-carregando section.
       8500.
            exec html
               <INPUT type=hidden id=hidden:ws-nome-combo>
               <SCRIPT>
                   if( :objs-rotina ( ':ws-nome-combo' ).options(0) == null ){
                       document.all ( 'hidden:ws-nome-combo' ).value = 'NaoExisteOpcaoInicial';
                       oOption = document.createElement( "OPTION" );
                       oOption.value = 'CCC';
                       oOption.text = ' Carregando ... ';
                       :objs-rotina ( ':ws-nome-combo' ).add( oOption );
                       oOption = null;
                   }
                   else {
                       document.all ( 'hidden:ws-nome-combo' ).value = :objs-rotina ( ':ws-nome-combo' ).options(0).text;
                       :objs-rotina ( ':ws-nome-combo' ).options(0).text = ' Carregando ... ';
                   }
               </SCRIPT>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       8500-remove-carregando section.
       8500.
            exec html
               <SCRIPT>
                   if( document.all ( 'hidden:ws-nome-combo' ).value == 'NaoExisteOpcaoInicial' ){
                       :objs-rotina ( ':ws-nome-combo' ).remove( 0 );
                   }
                   else {
                       :objs-rotina ( ':ws-nome-combo' ).options(0).text = document.all ( 'hidden:ws-nome-combo' ).value;
                   }
                   :objs-rotina ( ':ws-nome-combo' ).disabled = false;
               </SCRIPT>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       8500-esvazia-combo-unidade section.
       8500.
            exec html
               <SCRIPT>
                   :objs-rotina ( 'unidade' ).innerHTML = '<OPTION value="">--Selecionar--</OPTION>';
               </SCRIPT>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       8500-ctrl-combo-unidade section.
       8500.
            exec html
               <SCRIPT>
                   :frame-rotina .CarregaDestino();
               </SCRIPT>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       8500-ctrl-tela-inicial section.
       8500.
            move 1                      to lnk-alt lnk-inc lnk-exc
            move "origem"               to ws-nome-objeto
            move "BotConfirmar"         to ws-botao-permissao
            perform 8100-controles-iniciais.
       8500-exit.
            exit.

      *>===================================================================================
       8500-adiciona-combo-dir section.
       8500.
            exec html
               <SCRIPT>
                   oOption = document.createElement( "OPTION" );
                   oOption.value = ':ws-valor-combo-dir';
                   oOption.text = ":ws-option-text";
                   :objs-rotina ( ":ws-nome-combo" ).add( oOption );
                   oOption = null;
               </SCRIPT>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
      *> Rotinas genéricas

       copy PCP8000.CPY.
       copy PCP8004.CPY.                *> Controle de Display (Loop)
       copy PCP9000.CPY.

      *>===================================================================================
       9000-log-erro-parar-processo section.
       9000.
            move 1                      to lnk-status-erro
            move ws-resultado-acesso    to lnk-resultado-acesso-prc
            move gravar-log             to lnk-controle-processo
            call controle-processo using lnk-par lnk-complementar-processo
            cancel controle-processo.
       9000-exit.
            exit.

      *>===================================================================================
       9000-ctrl-abertura section.
       9000.
       9000-exit.
            exit.

      *>===================================================================================
       9000-verifica-status-arquivo-2 section.
       9000.
            if   ws-arquivo-locado
                 move "Arquivo de origem locado" to lnk-ocorrencia-log
                 perform 9000-grava-erro-log-2
                 exit section
            end-if
            if   not ws-operacao-ok
                 move "Erro na abertura do arquivo de origem" to lnk-ocorrencia-log
                 perform 9000-grava-erro-log-2
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-verif-sts-arq-convertido section.
       9000.
            if   ws-resultado-acesso = "28"
                 move "Falta de espaço em disco"    to lnk-ocorrencia-log
                 perform 9000-grava-erro-log
                 exit section
            end-if
            if   not ws-operacao-ok
                 move "Erro na gravação do arquivo de destino" to lnk-ocorrencia-log
                 perform 9000-grava-erro-log
                 exit section
            end-if
            add 1                                   to ws-registros-gravados.
       9000-exit.
            exit.

      *>===================================================================================
       9000-grava-erro-log section.
       9000.
            move lk-cd-empresa-origem  (wx-index-1) to ws-cd-empresa-origem-log
            move lk-cd-filial-origem   (wx-index-1) to ws-cd-filial-origem-log
            move lk-cd-empresa-destino (wx-index-1) to ws-cd-empresa-destino-log
            move lk-cd-filial-destino  (wx-index-1) to ws-cd-filial-destino-log
            perform 9000-trata-ocorrencia-log
            move 1                 to lnk-status-erro
            perform 9000-log-erro.
       9000-exit.
            exit.

      *>===================================================================================
       9000-grava-erro-log-2 section.
       9000.
            move zeros                  to ws-cd-empresa-origem-log
            move zeros                  to ws-cd-filial-origem-log
            move zeros                  to ws-cd-empresa-destino-log
            move zeros                  to ws-cd-filial-destino-log
            perform 9000-trata-ocorrencia-log
            move 1                 to lnk-status-erro
            perform 9000-log-erro.
       9000-exit.
            exit.

      *>===================================================================================
       9000-grava-log-warning-2 section.
       9000.
            move zeros                  to ws-cd-empresa-origem-log
            move zeros                  to ws-cd-filial-origem-log
            move zeros                  to ws-cd-empresa-destino-log
            move zeros                  to ws-cd-filial-destino-log
            perform 9000-trata-ocorrencia-log
            move 2                 to lnk-status-erro
            perform 9000-log-erro.
       9000-exit.
            exit.

      *>===================================================================================
       9000-log-erro section.
       9000.
            move ws-resultado-acesso    to lnk-resultado-acesso-prc
            move gravar-log             to lnk-controle-processo
            call controle-processo using lnk-par lnk-complementar-processo
            cancel controle-processo.
       9000-exit.
            exit.

      *>===================================================================================
       9000-trata-ocorrencia-log section.
       9000.
            move spaces                 to whs-mensagem

            string lnk-rotina delimited by " " ";"
                   ws-ds-arquivo delimited by " " ";"
                   ws-cd-empresa-origem-log "/"
                   ws-cd-filial-origem-log ";"
                   ws-cd-empresa-destino-log "/"
                   ws-cd-filial-destino-log ";"
                   lnk-ocorrencia-log delimited by "  " ";"
                   into whs-mensagem
            if   diagnostico
                 string whs-mensagem ";"
                        into lnk-ocorrencia-log
            else
                 string whs-mensagem delimited by "  "
                        ws-registros-lidos ";"
                        ws-registros-gravados
                        into lnk-ocorrencia-log
            end-if.
       9000-exit.
            exit.

      *>===================================================================================
       9000-monta-label-arquivo-temp section.
       9000.
            move spaces                 to wid-temporario
            string lnk-dtbpath delimited by " ",
                   "/../tmp/work/t_",
                   lnk-id-usuario delimited by " ",
                   "_",
                   lnk-cd-empresa delimited by " ",
                   "_",
                   lnk-cd-filial delimited by " ",
                   "_",
                   lnk-rotina delimited by " ",
                   ".WRK"
                   into wid-temporario.
       9000-exit.
            exit.

      *>===================================================================================
      *> Leituras

       copy PCL99800.CPY.
      *>
