      $set preprocess (htmlpp) endp
      $set sourceformat"free"

       identification division.
       program-id.    PP00005.

      *>
      *>              Controle de execução de ROTINAS ESPECIAIS.
      *>

       environment division.
       special-names. decimal-point is comma.

       copy PCS05701.CPY replacing ==(f57)== by ==f57==.
       copy PCS09400.CPY replacing ==(f94)== by ==f94==.
       copy PCS99800.CPY replacing ==(fpa)== by ==fpa==.

       data division.

       copy PCF05701.CPY replacing ==(f57)== by ==f57==.
       copy PCF09400.CPY replacing ==(f94)== by ==f94==.
       copy PCF99800.CPY replacing ==(fpa)== by ==fpa==.

       working-storage section.
       78   versao                       value "s".
       78   conversor-novo-comodato-ag   value "PWPE0196".
       78   conv-novo-comod-litrao-300ml value "PWPE0271".
       78   conversor-gestao-vale        value "PWPE0412".
       78   atualiza-class-pis-cofins    value "PW00733S".
       copy PCW900.CPY.                 *>Padrão

       01   ws-campos-de-trabalho.
            03 ws-nome-programa         pic x(08)   value spaces.
            03 ws-tipo-cdd              pic x(01).
               88 ws-cdd                value "I" "E".
               88 ws-revenda            value "R".

       01   wt-tabela-prog-executados.
       *> Esta chamada deve ser nesta posicao, pois altera o nome arquivos de Mapas e Pedidos
            03 filler                   pic x(15)   value "PWC0902F000 AC ".   *> Caracter
            03 filler                   pic x(15)   value "PWC0902A000 AC ".   *> PWC0902J (Caracter) no lugar do PWC0902A
            03 filler                   pic x(15)   value "PWC0902I000 AV ".
            03 filler                   pic x(15)   value "PWC0902C000 AV ".
            03 filler                   pic x(15)   value "PWC0902D000 AV ".
            03 filler                   pic x(15)   value "PWC0902E000 AV ".
            03 filler                   pic x(15)   value "PWC0903 000 AC ".
            03 filler                   pic x(15)   value "PWC0904 000 AC ".
            03 filler                   pic x(15)   value "PWC0904J000 AV ".             *>MP22012004:10585
            03 filler                   pic x(15)   value "PWC0904K000 AV ".             *>RF100204:11313
            03 filler                   pic x(15)   value "PWC0905 000 AV ".
            03 filler                   pic x(15)   value "PWPA5128000 AV ".             *>IS310304:14036
            03 filler                   pic x(15)   value "PWPA5132000 AV ".             *>RRMP1404:15230
            03 filler                   pic x(15)   value "PWPA5146000 RV ".             *>RRMP1405:16766
            03 filler                   pic x(15)   value "PWC0906 000 AV ".             *>RRMP210504:16702
            03 filler                   pic x(15)   value "PWC0906B000 AV ".             *>RRMP210504:16702
            03 filler                   pic x(15)   value "PWC0907 000 AV ".             *>RRMP310704:19133
            03 filler                   pic x(15)   value "PWC0907A000 AV ".             *>RRMP030904:23569
            03 filler                   pic x(15)   value "PWC0908 000 AV ".             *>RRMP240904:24582
            03 filler                   pic x(15)   value "PWC0908A000 AV ".             *>RRMP181004:25964
            03 filler                   pic x(15)   value "PWC0908B000 AV ".             *>RRMP281004:26459
            03 filler                   pic x(15)   value "PWC0908C000 AV ".             *>RRMP111104:27060
            03 filler                   pic x(15)   value "PWC0908D000 AV ".             *>RRMP241104:27648
            03 filler                   pic x(15)   value "PWC0908E000 AV ".             *>RRMP171204:28340
            03 filler                   pic x(15)   value "PWC0908F000 AV ".             *>RRMP070205:30749
            03 filler                   pic x(15)   value "PWC0908G000 AV ".             *>RRMP210205:30976
            03 filler                   pic x(15)   value "PWC0908H000 AV ".             *>RRMP080305:32023
            03 filler                   pic x(15)   value "PWC0908I000 AV ".             *>RRMP220305:32834
            03 filler                   pic x(15)   value "PWC0908J000 AV ".             *>NB280405:34574
            03 filler                   pic x(15)   value "PWC0909 000 AV ".             *>RRMP050405:32849
            03 filler                   pic x(15)   value "PWC0909A000 AV ".             *>RRMP230505:35984
            03 filler                   pic x(15)   value "PWC0910 000 AV ".             *>RRMP030605:35956
            03 filler                   pic x(15)   value "PWC0910A000 AV ".             *>RRMP040705:37986
            03 filler                   pic x(15)   value "PWC0910B000 AV ".             *>RRMP080805:39446
            03 filler                   pic x(15)   value "PWC0911 000 AV ".             *>RRMP230805:40114
            03 filler                   pic x(15)   value "PWC0911A000 AV ".             *>RRMP070905:40725
            03 filler                   pic x(15)   value "PWPA5331000 RV ".             *>RRMP281005:42978
            03 filler                   pic x(15)   value "PWC0912 000 AV ".             *>RRMP221005:42595
            03 filler                   pic x(15)   value "PWC0912A000 AV ".             *>RRMP171105:43986
            03 filler                   pic x(15)   value "PWC0912B000 AV ".             *>RRMP100106:46652
            03 filler                   pic x(15)   value "PWC0912C000 AV ".             *>RRMP100106:46652
            03 filler                   pic x(15)   value "PWC0912D000 AV ".             *>RRMP141205:45934
            03 filler                   pic x(15)   value "PWC0912E000 AV ".             *>RRMP310106:47788
            03 filler                   pic x(15)   value "PWC0912F000 AV ".             *>RRMP120206:48538
            03 filler                   pic x(15)   value "PWC0912G000 AV ".             *>RRMP120206:48538
            03 filler                   pic x(15)   value "PWC0912H000 AV ".             *>RRMP130306:50717
            03 filler                   pic x(15)   value "PWC0913 000 AV ".             *>RRMP020406:51847
            03 filler                   pic x(15)   value "PWC0913A000 AV ".             *>RRMP030506:53530
            03 filler                   pic x(15)   value "PWC0913B000 AV ".             *>RRMP030506:53530
            03 filler                   pic x(15)   value "PWC0913C000 AV ".             *>JRWP220506:54531
            03 filler                   pic x(15)   value "PWC0913D000 AV ".             *>JRWP200606:55241
            03 filler                   pic x(15)   value "PWC0913E000 AV ".             *>JRWP200607:55241
            03 filler                   pic x(15)   value "PWC0914 000 AV ".             *>JRWP200607:56528
            03 filler                   pic x(15)   value "PWC0914A000 AV ".             *>JRWP200608:57492
            03 filler                   pic x(15)   value "PWC0914B000 AV ".             *>JRWP200608:58201
            03 filler                   pic x(15)   value "PWC0914C000 AV ".             *>JRWP200609:58899
            03 filler                   pic x(15)   value "PWC0915 000 AV ".             *>JRWP200610:59554
            03 filler                   pic x(15)   value "PWC0915A000 AV ".             *>JRWP200611:60692
            03 filler                   pic x(15)   value "PWC0915B000 AV ".             *>JRWP200612:62170
            03 filler                   pic x(15)   value "PWC0916 000 AV ".             *>JRWP200106:62622
            03 filler                   pic x(15)   value "PWC0916A000 AV ".             *>JRWP200106:62622
            03 filler                   pic x(15)   value "PWC0917 000 AV ".             *>JRWP200704
            03 filler                   pic x(15)   value "PWC0917A000 AV ".             *>JRWP200705
            03 filler                   pic x(15)   value "PWC0917B000 AV ".             *>JRWP200706
            03 filler                   pic x(15)   value "PWC0917C000 AV ".             *>JRWP200707
            03 filler                   pic x(15)   value "PWC0917D000 AV ".             *>JRWP200707
            03 filler                   pic x(15)   value "PWC0918 000 AV ".             *>JRWP200708
            03 filler                   pic x(15)   value "PWC0918A000 AV ".             *>JRWP200708
            03 filler                   pic x(15)   value "PWC0919 000 AV ".             *>JRWP200710
            03 filler                   pic x(15)   value "PWC1000A000 AV ".             *>JRWP200712
            03 filler                   pic x(15)   value "PWC1000B000 AV ".             *>JRWP200802
            03 filler                   pic x(15)   value "PWC1000C000 AV ".             *>JRWP200803
            03 filler                   pic x(15)   value "PWPE0196102 AVS".             *>JRWP200803
            03 filler                   pic x(15)   value "PWC1001 000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1001A000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1001B000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1002 000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1002A000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1003 000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1004 000 AV ".             *>JRWP200804
            03 filler                   pic x(15)   value "PWC1005 000 AV ".             *>JRWP200810
            03 filler                   pic x(15)   value "PWC1005A000 CV ".             *>JRWP200810
            03 filler                   pic x(15)   value "PWC1005C000 CV ".             *>JRWP200810
            03 filler                   pic x(15)   value "PWC1006 000 RV ".             *>JRWP200811
            03 FILLER                   PIC X(15)   VALUE "PWC1006A000 RV ".             *>JRWP200811
            03 FILLER                   PIC X(15)   VALUE "PWC1006B000 RV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1007 000 RV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1007A000 RV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1007B000 RV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1007C000 RV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1008 000 AV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1008A000 AV ".             *>JRWP200811
            03 filler                   pic x(15)   value "PWC1008B000 AV ".             *>RRMP200904
            03 filler                   pic x(15)   value "PWC1009 000 AV ".             *>JRW200905
            03 filler                   pic x(15)   value "PWC1010 000 AV ".             *>JRW200907
            03 filler                   pic x(15)   value "PWC1011 000 AV ".             *>JRW200908
            03 filler                   pic x(15)   value "PWC1011A000 AV ".             *>JRW200909
            03 filler                   pic x(15)   value "PWC1012 000 AV ".             *>JRW200909
            03 filler                   pic x(15)   value "PWC1012A000 AV ".             *>RRMP201001
            03 filler                   pic x(15)   value "PWC1013 000 AV ".             *>JRW201001
            03 filler                   pic x(15)   value "PWC1013A000 AV ".             *>RRMP201002
            03 filler                   pic x(15)   value "PWC1013B000 AV ".             *>JRW201002
            03 filler                   pic x(15)   value "PWC1013C000 RV ".             *>JRW201002
            03 filler                   pic x(15)   value "PWC1014 000 AV ".             *>JRW201002
            03 filler                   pic x(15)   value "PWC1014A000 AV ".             *>LLDD201005
            03 filler                   pic x(15)   value "PWC1014B000 AV ".             *>LLDD201005
            03 filler                   pic x(15)   value "PWC1015 000 AV ".             *>LLDD201005
            03 filler                   pic x(15)   value "PWC1015A000 AV ".             *>LLDD201005
            03 filler                   pic x(15)   value "PWC1015B000 AV ".             *>LLDD201005
            03 filler                   pic x(15)   value "PWPE0271120 AVS".             *>LLDD201006
            03 filler                   pic x(15)   value "PWC1016 000 AV ".             *>JRW102010
            03 filler                   pic x(15)   value "PWC1016A000 AV ".             *>JRW122010
            03 filler                   pic x(15)   value "PWC1016B000 AV ".             *>SE13012011
            03 filler                   pic x(15)   value "PWC1017 000 AV ".             *>RRMP29012011
            03 filler                   pic x(15)   value "PWC1017A000 RV ".             *>RRMP29012011
            03 filler                   pic x(15)   value "PWC1018 000 AV ".             *>JRW15042011
            03 filler                   pic x(15)   value "PWC1018A000 AV ".             *>JRW13052011
            03 filler                   pic x(15)   value "PWC1018B000 AV ".             *>JRW13052011
            03 filler                   pic x(15)   value "PWC1018C000 RV ".             *>JRW13052011
            03 filler                   pic x(15)   value "PWC1019 000 AV ".             *>JRW13052011
            03 filler                   pic x(15)   value "PWC1020 000 AV ".             *>JRW13082011
            03 filler                   pic x(15)   value "PWC1020A000 AV ".             *>JRW13082011
            03 filler                   pic x(15)   value "PWC1020B000 AV ".             *>JRW22092011
            03 filler                   pic x(15)   value "PWC1021 000 AV ".             *>JRW10102011
            03 filler                   pic x(15)   value "PWC1021A000 AV ".             *>LLDD21102011
            03 filler                   pic x(15)   value "PWC1021B000 AV ".             *>JRW15122011
            03 filler                   pic x(15)   value "PWC1022 000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1022A000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1022B000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1022C000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1023 000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1023A000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1023B000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1024 000 AV ".             *>JRW03012012
            03 filler                   pic x(15)   value "PWC1024A000 AV ".             *>JRW02082012
            03 filler                   pic x(15)   value "PWC1024B000 AV ".             *>JRW02082012
            03 filler                   pic x(15)   value "PWC1025 000 AV ".             *>JRW00310012
            03 filler                   pic x(15)   value "PWC1025A000 AV ".             *>JRW30112012
            03 filler                   pic x(15)   value "PWC1025B000 AV ".             *>JRW30112012
            03 filler                   pic x(15)   value "PWC1026 000 AV ".             *>JRW25022013
            03 filler                   pic x(15)   value "PWC1026A000 AV ".             *>JRW14032013
            03 filler                   pic x(15)   value "PWC1026B000 AV ".             *>JRW09042013
            03 filler                   pic x(15)   value "PWC1026C000 AV ".             *>JRW29042013
            03 filler                   pic x(15)   value "PWC1100 000 AV ".             *>JRW17052013
            03 filler                   pic x(15)   value "PWC1101 000 AV ".             *>JRW21062013
            03 filler                   pic x(15)   value "PWC1102 000 AV ".             *>JRW11072013
            03 filler                   pic x(15)   value "PWC1103 000 AV ".             *>JRW16082013
            03 filler                   pic x(15)   value "PWC1103A000 AV ".             *>JRW16082013
            03 filler                   pic x(15)   value "PWC1104 000 AV ".             *>JRW16082013
            03 filler                   pic x(15)   value "PWC1104A000 AV ".             *>JRW16082013
            03 filler                   pic x(15)   value "PWC1104B000 AV ".             *>JRW04112013
            03 filler                   pic x(15)   value "PWC1105 000 AV ".             *>JRW04112013
            03 filler                   pic x(15)   value "PWC1105A000 AV ".             *>JRW04112013
            03 filler                   pic x(15)   value "PWC1106 000 AV ".             *>JRW11042014
            03 filler                   pic x(15)   value "PWC1107 000 AV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1107A000 AV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1108 000 AV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1108A000 RV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1109 000 CV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1110 000 CV ".             *>JRW16052014
            03 filler                   pic x(15)   value "PWC1111 000 AV ".             *>JRW02102014
            03 filler                   pic x(15)   value "PWC1111A000 AV ".             *>JRW02102014
            03 filler                   pic x(15)   value "PWC1112 000 AV ".             *>JRW19122014
            03 filler                   pic x(15)   value "PW00733S000 RVS".
            03 filler                   pic x(15)   value "PWC1112B000 AV ".             *>JRP23022015
            03 filler                   pic x(15)   value "PWC1112C000 AV ".             *>JRP17032015
            03 filler                   pic x(15)   value "PWC1113 000 AV ".             *>JRP06032015
            03 filler                   pic x(15)   value "PWC1113A000 AV ".             *>JRP01042015
            03 filler                   pic x(15)   value "PWC1113B000 AV ".             *>JRP01042015
            03 filler                   pic x(15)   value "PWC1113C000 AV ".             *>JRW17042015
            03 filler                   pic x(15)   value "PWC1113E000 RV ".             *>JRP19062015
            03 filler                   pic x(15)   value "PWC1114 000 AV ".             *>JRP19062015
            03 filler                   pic x(15)   value "PWC1114A000 AV ".             *>JRP19062015
            03 filler                   pic x(15)   value "PWC1115 000 AV ".             *>JRP19062015
            03 filler                   pic x(15)   value "PWC1115A000 AV ".             *>JRP29062015
            03 filler                   pic x(15)   value "PWC1116 000 AV ".             *>JRP08072015
            03 filler                   pic x(15)   value "PWC1116A000 AV ".             *>JRP23072015
            03 filler                   pic x(15)   value "PWC1116B000 AV ".             *>JRP10082015
            03 filler                   pic x(15)   value "PWC1117 000 AV ".             *>JRP12082015
            03 filler                   pic x(15)   value "PWC1117A000 AV ".             *>JRP04092015
            03 filler                   pic x(15)   value "PWC1118 000 AV ".             *>JRP18092015
            03 filler                   pic x(15)   value "PWC1119 000 AV ".             *>JRP14102015
            03 filler                   pic x(15)   value "PWC1119A000 AV ".             *>AJ04112015
            03 filler                   pic x(15)   value "PWC1120 000 AV ".             *>JRP13112015
            03 filler                   pic x(15)   value "PWC1121 000 AV ".             *>JRP13112015
       *> Esta linha em branco serve p/ sinalizar o FIM da tabela, eliminando assim    *>MP11268
       *> o controle via variável limite-tabela-prog-times. Adicionar novos programas  *>MP11268
       *> acima deste comentário.                                                      *>MP11268
            03 filler                   pic x(15)   value "               ".             *>JRWP200803
       01   filler redefines wt-tabela-prog-executados.
            03 filler occurs 200 times.                                                 *>MP11268
               05 wt-nome-programa      pic x(8).
               05 wt-codigo-projeto     pic 9(3).
                  88 wt-projeto-vinculado           value 1 thru 999.
               05 wt-modo-execucao      pic x(1).
                  88 wt-execucao-automatica         value "A".
               05 wt-ind-tipo-cdd       pic x(1).
                  88 wt-cdd                         value "C".
                  88 wt-revenda                     value "R".
                  88 wt-ambos                       value "A".   *> cdd e revenda
               05 wt-ind-tipo-programa  pic x(1).
                  88 wt-visual                      value "V".
                  88 wt-caracter                    value "C".
               05 wt-ind-execucao       pic x(1).
                  88 wt-por-unidade                 value "S".
       01   cgi-input is external-form.
            copy pcw902.cpy.

       linkage section.
       copy PCW990.CPY.
       copy PCW0002.CPY replacing ==(lp)== by ==lp==.

       procedure division using lnk-par
                                lp-par-programa.

       0000-controle section.
       0000.
            perform 1000-inicializacao
            perform 2000-processamento
            perform 3000-finalizacao.
       0000-saida.
            exit program
            stop run.


       *>---------------------------------------------------------------------------------------------
       1000-inicializacao section.
       1000.
            *>perform 9000-abrir-io-pd05701
            string lnk-dtbpath delimited by " " "/EFD057.CAD" into wid-pd05701
            open i-o pd05701
            move "pd05701-execucao" to whs-arquivo
            perform 9000-trata-abertura

            *>perform 9000-abrir-i-pd09400
            string lnk-dtbpath delimited by " " "/EFD094.CAD" into wid-pd09400
            open input pd09400
            move "pd09400-dados-projeto" to whs-arquivo
            perform 9000-trata-abertura

            *>perform 9000-abrir-i-pd99800
            string lnk-dtbpath delimited by " " "/EFDPAR.CAD" into wid-pd99800
            open input pd99800
            move "pd99800-parametros" to whs-arquivo
            perform 9000-trata-abertura

            if   lnk-rotina <> "PW99922F"
            and  lnk-rotina <> "PWC0912B"
                 if   lp-opcao-prog = "SC"
                      perform 1100-carrega-empresa-filial
                 else
                      perform 1100-carrega-parametros
                 end-if
            end-if.

       1000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       1100-carrega-empresa-filial section.
       1100.
            initialize                  fpare-revenda
            perform 9000-str-pd99800-gtr
            perform 9000-ler-pd99800-nex
            perform until not ws-operacao-ok
                if   fpare-codigo-registro = 1
                and  fpare-sequencia = 0
                     move fpare-cd-empresa      to lp-cd-empresa
                     move fpare-cd-filial       to lp-cd-filial
                     move fpare-tipo-cdd        to ws-tipo-cdd
                     exit perform
                end-if
                perform 9000-ler-pd99800-nex
            end-perform.

       1000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       1100-carrega-parametros section.
       1100.
            initialize                  fpare-revenda
            move lnk-cd-empresa         to fpare-cd-empresa
            move lnk-cd-filial          to fpare-cd-filial
            move 1                      to fpare-codigo-registro
            perform 9000-ler-pd99800-ran
            if   not ws-operacao-ok
                 move spaces                   to whs-mensagem
                 perform 9000-monta-status
                 string "Empresa não homologada - Empresa:", lnk-cd-empresa, " Filial: ", lnk-cd-filial, " - Status: " ws-status into whs-mensagem
                 perform 9000-mensagem-especial
                 perform 3000-finalizacao
                 perform 0000-saida
            end-if
            move fpare-tipo-cdd         to ws-tipo-cdd
            move fpare-data-atual       to ws-data
            close pd99800.

       1000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2000-processamento section.
       2000.
            evaluate  lp-opcao-prog
                when  "SC"                           *> lp-verifica-todos-prog
                      perform 2100-verifica-execucoes
                when  "TT"                           *> lp-verifica-todos-prog
                      perform 2100-verifica-execucoes
                when  "PR"                           *> lp-verifica-prog-unico
                      if   lp-abandona-usuario
                           move spaces  to lp-status-usuario
                           exit section
                      end-if
                      if   not lp-usuario-verificado
                           perform 2500-solicita-usuario
                           go to 2000
                      end-if
                      perform 2200-verifica-execucao-unica
                      if   processamento-sem-erro
                           perform 2300-verifica-ordem-execucoes
                      end-if
                when  "AP"                           *> lp-atualiza-programa
                      perform 2400-atualiza-programa
                when  "CO"                           *> lp-consulta-execucoes
                      move spaces to ws-nome-programa
                      perform 2600-consulta-programas
                when "57"                            *> lp-atualiza-efd057
                     perform 2700-atualiza-efd057
            end-evaluate.
       2000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2100-verifica-execucoes section.
       2100.
            *>ws-data recebeu fpare-data-atual
            move ws-ano                            to ws-ano-inv
            move ws-mes                            to ws-mes-inv
            move ws-dia                            to ws-dia-inv

            perform varying wx-index-1 from 1 by 1
              until wt-nome-programa (wx-index-1) equal spaces                         *>MP11268

              if   wt-nome-programa (wx-index-1) = atualiza-class-pis-cofins
              and  ws-data-inv <= 20150430 *>Só consiste após o dia 30/04/2015
                   exit perform cycle
              end-if

              initialize  f5701-execucao
              if   wt-por-unidade (wx-index-1)
                   if   lp-cd-empresa = zeros
                   and  lp-cd-filial  = zeros
                        move lnk-cd-empresa to lp-cd-empresa
                        move lnk-cd-filial  to lp-cd-filial
                   end-if
                   move lp-cd-empresa to f5701-cd-empresa
                   move lp-cd-filial  to f5701-cd-filial
              end-if
              move wt-nome-programa (wx-index-1) to f5701-nome-programa
              perform 9000-ler-pd05701-ran
              if   not ws-operacao-ok
                   if   wt-ambos (wx-index-1)
                   or  (ws-cdd     and wt-cdd (wx-index-1))
                   or  (ws-revenda and wt-revenda (wx-index-1))
                        if   wt-projeto-vinculado (wx-index-1)
                             initialize            f9400-dados-projeto
                             move lp-cd-empresa to f9400-cd-empresa
                             move lp-cd-filial  to f9400-cd-filial
                             move wt-codigo-projeto (wx-index-1) to
                                  f9400-codigo-projeto
                             perform 9000-ler-pd09400-ran
                             if   ws-operacao-ok
                             and  f9400-habilitado
                                  if   wt-caracter (wx-index-1)
                                       string "Programa " wt-nome-programa (wx-index-1)
                                               " em caracter não executado"
                                               into whs-mensagem
                                  else
                                       string "Programa " wt-nome-programa (wx-index-1)
                                               " não executado"
                                               into whs-mensagem
                                  end-if
                                  move "  "  to lp-status-prog
                                  move whs-mensagem    to lp-mensagem
                                  exit perform
                             end-if
                        else
                             if   wt-execucao-automatica (wx-index-1)
                                  call wt-nome-programa (wx-index-1)
                                  cancel wt-nome-programa (wx-index-1)
                                  move wt-nome-programa (wx-index-1) to
                                       lp-nome-programa
                                  move "automatica"  to lp-usuario-prog
                                  perform 2400-atualiza-programa
                             else
                                  if    wt-nome-programa (wx-index-1) = "PWC0902A"
                                        string "Programa PWC0902J (Caracter) não executado"
                                               into whs-mensagem
                                  else
                                        if   wt-caracter (wx-index-1)
                                            string "Programa " wt-nome-programa (wx-index-1)
                                                    " em caracter não executado"
                                                    into whs-mensagem
                                        else
                                            string "Programa " wt-nome-programa (wx-index-1)
                                                    " não executado"
                                                    into whs-mensagem
                                        end-if
                                  end-if
                                  move "  "       to lp-status-prog
                                  move whs-mensagem    to lp-mensagem
                                  exit perform
                             end-if
                        end-if
                   else
                        move "OK"       to lp-status-prog
                   end-if
              else
                   move "OK"            to lp-status-prog
              end-if
            end-perform.

       2100-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2200-verifica-execucao-unica section.
       2200.
            if   not lp-usuario-verificado
                 exit section
            end-if
            initialize                     f5701-execucao
            move lp-nome-programa       to f5701-nome-programa
            perform 9000-ler-pd05701-ran
            if   ws-operacao-ok
                 move "Programa          já executado" to
                      whs-mensagem
                 move lp-nome-programa  to whs-mensagem (10:8)
                 move whs-mensagem      to lp-mensagem
            else
                 move "OK"              to lp-status-prog
            end-if.

       2200-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2300-verifica-ordem-execucoes section.
       2300.
            move "OK"                   to lp-status-prog
            perform varying wx-index-1 from 1 by 1
              until wt-nome-programa (wx-index-1) equal spaces
              or    wt-nome-programa (wx-index-1) equal lp-nome-programa
              or    not lp-execucao-valida
              if   wt-ambos (wx-index-1)
              or  (ws-cdd     and wt-cdd (wx-index-1))
              or  (ws-revenda and wt-revenda (wx-index-1))
                   initialize  f5701-execucao
                   if   wt-por-unidade (wx-index-1)
                        if   lp-cd-empresa = zeros
                        and  lp-cd-filial  = zeros
                             move lnk-cd-empresa to lp-cd-empresa
                             move lnk-cd-filial  to lp-cd-filial
                        end-if
                        move lp-cd-empresa to f5701-cd-empresa
                        move lp-cd-filial  to f5701-cd-filial
                   end-if

                   if   wt-nome-programa (wx-index-1) = atualiza-class-pis-cofins
                   and  ws-data-inv <= 20150430 *>Só consiste após o dia 30/04/2015
                        exit perform cycle
                   end-if

                   move wt-nome-programa (wx-index-1) to f5701-nome-programa
                   perform 9000-ler-pd05701-ran
                   if   not ws-operacao-ok
                        if   wt-projeto-vinculado (wx-index-1)
                             initialize            f9400-dados-projeto
                             move lp-cd-empresa to f9400-cd-empresa
                             move lp-cd-filial  to f9400-cd-filial
                             move wt-codigo-projeto (wx-index-1) to
                                  f9400-codigo-projeto
                             perform 9000-ler-pd09400-ran
                             if   ws-operacao-ok
                             and  f9400-habilitado
                                  if   wt-caracter (wx-index-1)
                                       string "Programa " wt-nome-programa (wx-index-1)
                                               " em caracter não executado"
                                               into whs-mensagem
                                  else
                                       string "Programa " wt-nome-programa (wx-index-1)
                                               " não executado"
                                               into whs-mensagem
                                  end-if
                             end-if
                        else
                             if   wt-nome-programa (wx-index-1) = "PWC0902A"
                                  string "Programa PWC0902J (Caracter) não executado"
                                          into whs-mensagem
                             else
                                  if   wt-caracter (wx-index-1)
                                       string "Programa " wt-nome-programa (wx-index-1)
                                              " em caracter não executado"
                                              into whs-mensagem
                                  else
                                       string "Programa " wt-nome-programa (wx-index-1)
                                              " não executado"
                                              into whs-mensagem
                                  end-if
                             end-if
                        end-if
                        if   whs-mensagem <> spaces
                             move "AN"              to lp-status-prog
                             move whs-mensagem      to lp-mensagem
                             perform 9000-mensagem-especial
                             exit section
                        end-if
                   end-if
              end-if
            end-perform.

       2100-exit.
            exit.


       *>---------------------------------------------------------------------------------------------
       2400-atualiza-programa section.
       2400.
            if   lp-nome-programa = spaces
                 move "Falta nome programa" to whs-mensagem
                 move whs-mensagem      to lp-mensagem
                 exit section.
            if   lp-usuario-prog = spaces
                 move "Falta usuário"   to whs-mensagem
                 move whs-mensagem      to lp-mensagem
                 exit section.
            initialize                     f5701-execucao
            if   conversor-novo-comodato-ag   = lnk-rotina
            or   conv-novo-comod-litrao-300ml = lnk-rotina
            or   conversor-gestao-vale        = lnk-rotina
                 move lp-cd-empresa     to f5701-cd-empresa
                 move lp-cd-filial      to f5701-cd-filial
            end-if
            move lp-nome-programa       to f5701-nome-programa
            perform 9000-ler-pd05701-ran
            if   ws-operacao-ok
                 exit section.
            move lp-usuario-prog        to f5701-id-usuario
            accept ws-data-inv          from date
            move ws-dia-inv             to ws-dia
            move ws-mes-inv             to ws-mes
            move ws-ano-inv             to ws-ano
            if   ws-ano-inv less 1900
                 add 2000               to ws-ano.
            accept ws-horas             from time
            move ws-data                to f5701-data-execucao
            move ws-horas               to f5701-hora-execucao
            write f5701-execucao
            if   not ws-operacao-ok
                 move "2400-erro de atualizacao efd057.cad !!!" to
                      whs-mensagem
                 move ws-resultado-acesso to ws-status
                 move whs-mensagem      to lp-mensagem
                 perform 3000-finalizacao
                 perform 0000-saida.
            move "OK"                   to lp-status-prog.
       2400-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2500-solicita-usuario section.
       2500.
       *>     exec html
       *>        <script>
       *>          function SolicitaUsuario(){
       *>              var lRetorno = window.showModalDialog( ':ws-href-pesquisa&call=PW00026S&programa=:lnk-rotina&SubSessionID=' + :frame-rotina .getNewSessionID(),
       *>                                                     null, "dialogHeight:8; dialogWidth:12; center: yes; help: no; resizable: no; scroll: no; status: no;" );
       *>              return( lRetorno );
       *>          }
       *>        </script>
       *>   end-exec.
            move "UV"  to lp-status-usuario.
       2500-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2600-consulta-programas section.
       2600.

       2600-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       2700-atualiza-efd057 section.
       2700.
            move spaces                 to lp-mensagem
            move 1                      to wx-index-2
            perform varying wx-index-1 from 1 by 1 until wt-nome-programa (wx-index-1) equal spaces  *>MP11268
                 initialize                f5701-execucao
                 move wt-nome-programa (wx-index-1) to f5701-nome-programa
                 perform 9000-ler-pd05701-ran
                 if   not  ws-operacao-ok
                      move lp-usuario-prog to f5701-id-usuario
                      accept ws-data-inv from date yyyymmdd
                      move ws-dia-inv      to ws-dia
                      move ws-mes-inv      to ws-mes
                      move ws-ano-inv      to ws-ano
                      accept ws-horas    from time
                      move ws-data         to f5701-data-execucao
                      move ws-horas        to f5701-hora-execucao
                      write f5701-execucao
                      if   not ws-operacao-ok
                           perform 9000-monta-status
                           string "Erro write EFD057 [", f5701-nome-programa delimited by " ",
                                  "] - status: ", ws-status, "\n"
                                  into lp-mensagem pointer wx-index-2
                      end-if
                 end-if
            end-perform.
       2700-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       3000-finalizacao section.
       3000.
            close pd09400
            close pd05701
            close pd99800.
       3000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       9000-trata-abertura section.
       9000.
            if   not ws-operacao-ok
                 move spaces            to whs-mensagem
                 perform 9000-monta-status
                 string "Erro abertura arquivo ", whs-arquivo , " - Status: ", ws-status into whs-mensagem
                 perform 9000-mensagem-especial
                 perform 3000-finalizacao
                 perform 0000-saida
            end-if.
       9000-exit.
            exit.

       *>---------------------------------------------------------------------------------------------
       9000-mensagem-especial section.
       9000.
            if   lnk-execucao-em-background
                 move whs-mensagem      to lp-mensagem
            else
                 perform 8000-mensagem
            end-if.

       9000-exit.
            exit.

       copy PCP8000.CPY.
       copy PCP9000.CPY.
       copy PCL05701.CPY replacing ==(f57)== by ==f57==.
       copy PCL09400.CPY replacing ==(f94)== by ==f94==.
       copy PCL99800.CPY replacing ==(fpa)== by ==fpa==.
*>
        