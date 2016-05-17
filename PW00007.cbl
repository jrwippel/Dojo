      $set preprocess (htmlpp) endp
      $set sourceformat"free"

      *>===================================================================================
       identification division.
       program-id.  PW00007.                                                *>OPC: 00.00.00
      *>===================================================================================

      *>
      *>            --------------------------------------------------------
      *>                              Função do programa
      *>            --------------------------------------------------------
      *>

      *>===================================================================================
       environment division.
            special-names. decimal-point is comma.
      *>===================================================================================

       copy PCS99704.CPY.               *> Spooling Manager


      *>===================================================================================
       data division.

       copy PCF99704.CPY.               *> Spooling Manager


      *>===================================================================================
       working-storage section.
       78   versao             value "c".

       copy PCW900.CPY.
       copy PCW904.CPY.                 *>carrega-combo


       01   ws-campos-de-trabalho.
            03 ws-tabelas.
               05 ws-selecionados       pic x(2000) occurs 100.
               05 ws-campos             pic x(1900) occurs 4.
            03 ws-index                 pic 9(07) value zeros.
            03 ws-inv-key               pic 9(01) value zeros.

       01   cgi-input is external-form.
            copy PCW902.CPY.
            03  f-campos-formulario.
                05  f-cod-lote          pic 9(03) identified by "lote".
                05  f-dsc-lote          pic x(50) identified by "dscLote".
            03  f-executa               pic x(20) identified by "executa".
            03  f-Selecionados       pic x(50000) identified by "hSelecionados".


      *>===================================================================================
       linkage section.
       copy PCW990.CPY.

      *>===================================================================================
       procedure division using lnk-par.

      *>===================================================================================
       0000-controle section.
       0000.
            perform 1000-inicializacao.
            perform 2000-processamento.
            perform 3000-finalizacao.
       0000-saida.
            exit program
            stop run.

      *>===================================================================================
       1000-inicializacao section.
       1000.
            perform 9000-validar-sessao
            perform 8000-cabecalho-padrao
            perform 9000-abrir-io-pd99704.
       1000-exit.
            exit.

      *>===================================================================================
       2000-processamento section.
       2000.
            evaluate f-frame
                when 0
                    perform 2000-controle-programa
                when 9
                    perform 2999-controle-frame
                when other
                    move "Frame Inválido !" to whs-mensagem
                    perform 8000-mensagem
            end-evaluate.
       2000-exit.
            exit.

      *>===================================================================================
       2000-controle-programa section.
       2000.
            evaluate f-opcao
                when 0
                    perform 8000-tela
                when 1
                    perform 2200-carrega-lote
                    perform 8000-tela-execucao
                when other
                    move "Opção Inválida!" to whs-mensagem
                    perform 8000-mensagem
            end-evaluate.
       2000-exit.
            exit.

      *>===================================================================================
       2200-carrega-lote section.
       2200.
            perform 2201-posiciona-registro
            if   ws-operacao-ok
                 move 0 to ws-inv-key
                 perform 2250-move-dados-f-formulario
            else
                 move 1 to ws-inv-key
                 perform 8100-limpa-tela
                 perform 8500-controles-tela
            end-if.
       2200-exit.
            exit.

      *>===================================================================================
       2201-posiciona-registro section.
       2201.
            move spaces                 to f99704-registro
            initialize                  f99704-registro
            move lnk-cd-empresa         to f99704-cd-empresa
            move lnk-cd-filial          to f99704-cd-filial
            move f-cod-lote             to f99704-lote
            perform 9000-ler-pd99704-ran.
       2201-exit.
            exit.

      *>===================================================================================
       2250-move-dados-f-formulario section.
       2250.
            move f99704-lote           to f-cod-lote
            move f99704-descricao      to f-dsc-lote

            move spaces to f99704-registro
            initialize f99704-registro f-Selecionados
            move lnk-cd-empresa to f99704-cd-empresa
            move lnk-cd-filial  to f99704-cd-filial
            move f-cod-lote     to f99704-lote
            move 1              to f99704-codigo-registro
            move zeros          to f99704-seq
            move 1              to ws-index
            perform 9000-str-pd99704-nlss
            perform 9000-ler-pd99704-nex
            perform until
                    not ws-operacao-ok
                    or  f99704-cd-empresa      not equal lnk-cd-empresa
                    or  f99704-cd-filial       not equal lnk-cd-filial
                    or  f99704-lote            not equal f-cod-lote
                    or  f99704-codigo-registro not equal 1
                        string f99704-seq  delimited by size,
                               "¶" delimited by size,
                               f99704-descricao delimited by "  ",
                               "¶" delimited by size,
                               f99704-parametros delimited by "  ",
                               "¶" delimited by size,
                               f99704-nome-programa delimited by "  ",
                               delimitador delimited by size
                               into f-Selecionados
                               pointer ws-index
                        perform 9000-ler-pd99704-nex
            end-perform
            perform 8500-move-dados-para-tela.
       2250-exit.
            exit.

      *>===================================================================================
       2600-salvar section.
       2600.
            perform 2201-posiciona-registro
            perform 2650-move-formulario-arquivo
            if   ws-operacao-ok
                 rewrite f99704-registro
                 if  not ws-operacao-ok
                     perform 2601-status-salvar
                 else
                     perform 2610-salvar-selecionados
                     perform 2601-status-salvar
                 end-if
            else
                 write f99704-registro
                 perform 2601-status-salvar
                 perform 2610-salvar-selecionados
            end-if.
       2600-exit.
            exit.

      *>===================================================================================
       2601-status-salvar section.
       2601.
            if   ws-operacao-ok
                 perform 8100-limpa-tela
            else
                 call ws-PP00001X using wid-pd99704 "RW" ws-resultado-acesso
                 cancel ws-PP00001X
            end-if.
       2601-exit.
            exit.

      *>===================================================================================
       2610-salvar-selecionados section.
       2160.
            move spaces to f99704-registro
            initialize f99704-registro
            move lnk-cd-empresa to f99704-cd-empresa
            move lnk-cd-filial  to f99704-cd-filial
            move 1              to f99704-codigo-registro
            move f-cod-lote     to f99704-lote
            move zeros          to f99704-seq
            perform 9000-str-pd99704-nlss
            perform 9000-ler-pd99704-nex
            perform until
                    not ws-operacao-ok
                    or  f99704-cd-empresa      not equal lnk-cd-empresa
                    or  f99704-cd-filial       not equal lnk-cd-filial
                    or  f99704-lote            not equal f-cod-lote
                    or  f99704-codigo-registro not equal 1
                        delete pd99704
                        if  not ws-operacao-ok
                            call ws-PP00001X using wid-pd99704 "DL" ws-resultado-acesso
                            cancel ws-PP00001X
                        end-if
                        perform 9000-ler-pd99704-nex
            end-perform

            unstring f-Selecionados delimited by delimitador into
                     ws-selecionados(01) ws-selecionados(02) ws-selecionados(03) ws-selecionados(04) ws-selecionados(05) ws-selecionados(06) ws-selecionados(07) ws-selecionados(09) ws-selecionados(09) ws-selecionados(10)
                     ws-selecionados(11) ws-selecionados(12) ws-selecionados(13) ws-selecionados(14) ws-selecionados(15) ws-selecionados(16) ws-selecionados(17) ws-selecionados(19) ws-selecionados(19) ws-selecionados(20)
                     ws-selecionados(21) ws-selecionados(22) ws-selecionados(23) ws-selecionados(24) ws-selecionados(25) ws-selecionados(26) ws-selecionados(27) ws-selecionados(29) ws-selecionados(09) ws-selecionados(30)
                     ws-selecionados(31) ws-selecionados(32) ws-selecionados(33) ws-selecionados(34) ws-selecionados(35) ws-selecionados(36) ws-selecionados(37) ws-selecionados(39) ws-selecionados(19) ws-selecionados(40)
                     ws-selecionados(41) ws-selecionados(42) ws-selecionados(43) ws-selecionados(44) ws-selecionados(45) ws-selecionados(46) ws-selecionados(47) ws-selecionados(49) ws-selecionados(09) ws-selecionados(50)
                     ws-selecionados(51) ws-selecionados(52) ws-selecionados(53) ws-selecionados(54) ws-selecionados(55) ws-selecionados(56) ws-selecionados(57) ws-selecionados(59) ws-selecionados(19) ws-selecionados(60)
                     ws-selecionados(61) ws-selecionados(62) ws-selecionados(63) ws-selecionados(64) ws-selecionados(65) ws-selecionados(66) ws-selecionados(67) ws-selecionados(69) ws-selecionados(09) ws-selecionados(70)
                     ws-selecionados(71) ws-selecionados(72) ws-selecionados(73) ws-selecionados(74) ws-selecionados(75) ws-selecionados(76) ws-selecionados(77) ws-selecionados(79) ws-selecionados(19) ws-selecionados(80)
                     ws-selecionados(81) ws-selecionados(82) ws-selecionados(83) ws-selecionados(84) ws-selecionados(85) ws-selecionados(86) ws-selecionados(87) ws-selecionados(89) ws-selecionados(09) ws-selecionados(90)
                     ws-selecionados(91) ws-selecionados(92) ws-selecionados(93) ws-selecionados(94) ws-selecionados(95) ws-selecionados(96) ws-selecionados(97) ws-selecionados(99) ws-selecionados(19) ws-selecionados(100)

            move spaces to f99704-registro
            initialize f99704-registro
            move lnk-cd-empresa to f99704-cd-empresa
            move lnk-cd-filial  to f99704-cd-filial
            move f-cod-lote     to f99704-lote
            move 1              to f99704-codigo-registro
            move zeros          to f99704-seq

            perform varying ws-index from 1 by 1
                    until ws-index > 100
                    move spaces to ws-campos(1) ws-campos(2) ws-campos(3) ws-campos(4)
                    unstring ws-selecionados(ws-index) delimited by "¶" into
                             ws-campos(1) ws-campos(2) ws-campos(3) ws-campos(4)
                    if   ws-campos(1) <> spaces and <> "undefined"
                         add 1             to f99704-seq
                         move ws-campos(2) to f99704-descricao
                         move ws-campos(3) to f99704-parametros
                         move ws-campos(4) to f99704-nome-programa
                         write f99704-registro
                         if   not ws-operacao-ok
                              call ws-PP00001X using wid-pd99704 "DL" ws-resultado-acesso
                              cancel ws-PP00001X
                              perform 3000-finalizacao
                              stop run
                         end-if
                    end-if
            end-perform.

       2610-exit.
            exit.

      *>===================================================================================
       2650-move-formulario-arquivo section.
       2650.
            move f-cod-lote       to f99704-lote
            move f-dsc-lote       to f99704-descricao.
       2650-exit.
            exit.

      *>===================================================================================
       2700-excluir section.
       2700.
            perform 2201-posiciona-registro
            if   ws-operacao-ok
                 delete pd99704
                 if   not ws-operacao-ok
                      call ws-PP00001X using wid-pd99704 "EX" ws-resultado-acesso
                      cancel ws-PP00001X
                 else
                      perform 2710-excluir-selecionados
                 end-if
            else
                 call ws-PP00001X using wid-pd99704 "RR" ws-resultado-acesso
                 cancel ws-PP00001X
            end-if
            perform 8100-limpa-tela.
       2700-exit.
            exit.

      *>===================================================================================
       2710-excluir-selecionados section.
       2710.
            move spaces to f99704-registro
            initialize f99704-registro
            move lnk-cd-empresa to f99704-cd-empresa
            move lnk-cd-filial  to f99704-cd-filial
            move 1              to f99704-codigo-registro
            move f-cod-lote     to f99704-lote
            move zeros          to f99704-seq
            perform 9000-str-pd99704-nlss
            perform 9000-ler-pd99704-nex
            perform until
                    not ws-operacao-ok
                    or  f99704-cd-empresa      not equal lnk-cd-empresa
                    or  f99704-cd-filial       not equal lnk-cd-filial
                    or  f99704-lote            not equal f-cod-lote
                    or  f99704-codigo-registro not equal 1
                        delete pd99704
                        if  not ws-operacao-ok
                            call ws-PP00001X using wid-pd99704 "DL" ws-resultado-acesso
                            cancel ws-PP00001X
                        end-if
                        perform 9000-ler-pd99704-nex
            end-perform.
       2710-exit.
            exit.

      *>===================================================================================
       2900-validacoes section.
       2900.
            move spaces                 to whs-mensagem.

       *>     if   processamento-sem-erro
       *>          ...
       *>     end-if

       2900-exit.
            exit.

      *>===================================================================================
       2999-controle-frame section.
       2999.
            evaluate f-opcao
                when 1
                    move 20                     to lnk-combo
                    move "Relatorios"           to lnk-nome-combo
                    call carrega-combo using lnk-par lnk-parametros-combo-box
                    cancel carrega-combo
                    perform 8500-desabilita-combo
                    move "lote" to ws-nome-objeto
                    perform 8100-controles-iniciais
                when 2
                    perform 2200-carrega-lote
                    move 21                     to lnk-combo
                    move "Selecionados"         to lnk-nome-combo
                    move f-cod-lote             to campoint
                    perform 9000-int2char
                    move campoChar              to lnk-selecionado
                    call carrega-combo using lnk-par lnk-parametros-combo-box
                    cancel carrega-combo
                when 6
                    perform 2900-validacoes
                    perform 2600-salvar
                when 7
                    perform 2700-excluir
                when 9
                    perform 2200-carrega-lote
                when 10
                    move 24                 to lnk-combo
                    move "Rotina"           to lnk-nome-combo
                    move f-cod-lote         to lnk-selecionado
                    call carrega-combo using lnk-par lnk-parametros-combo-box
                    cancel carrega-combo
                    move "Rotina"   to ws-nome-objeto
                    perform 8100-controles-iniciais
                when other
                    move "Controle Inválido" to whs-mensagem
                    perform 8000-mensagem
            end-evaluate
            if  f-executa =  spaces
                perform 8000-finaliza-controle
            else
                exec html
                  <!-- end program -->
                end-exec
            end-if.
       2999-exit.
            exit.

      *>===================================================================================
       3000-finalizacao section.
       3000.
            close   pd99704
            perform 8000-oculta-gif.
       3000-exit.
            exit.

      *>-----------------------------------------------------------------------------------
      *>                                        HTML
      *>-----------------------------------------------------------------------------------
       copy PCP8000.CPY.

      *>===================================================================================
       8000-scripts-tela section.
       8000.
            exec html
               <script>
                   var ret='';
                   var nomeJob='';
                   var arrayJob = new Array();
                   arrayJob[0] = new Array();    //Código job
                   arrayJob[1] = new Array();    //Nome job
                   arrayJob[2] = new Array();    //configuracao
                   arrayJob[3] = new Array();    //Nome do programa a ser executado
                   var indexJob=0;
                   var seqJob=1;

                   function CarregarLote(){
                       Cancelar();
                       for (var i=0; i < seqJob; i++) {
                           arrayJob[0][i] = '';
                           arrayJob[1][i] = '';
                           arrayJob[2][i] = '';
                           arrayJob[3][i] = '';
                       }
                       indexJob=0;
                       seqJob=1;
                       MostraArray();
                       document.all.opcao.value = 2;

                       document.parentWindow.frameMostra.document.all.divTmp.innerHTML = "";

                       EnviarFormulario();
       //                :href-oculto = document.all.urlSubmit.value + "&opcao=2" +
       //                               "&lote=" + document.all.lote.value;
                   }
                   function Excluir(){
                       document.all.BotExcluir.disabled = true;
                       if( confirm( 'Confirma Exclusão ?' ) ) {
                       document.all.opcao.value = 7;
                       EnviarFormulario();
       //                    :href-oculto = document.all.urlSubmit.value + "&opcao=7" +
       //                                   "&lote=" + document.all.lote.value;
                       }
                       else {
                           document.all.BotExcluir.disabled = false;
                       }
                   }
                   function Salvar(){
                       if( document.all.lote.value == 0 ) {
                          alert( 'Código da Rotina Inválido !' );
                          return false;
                       }
                       if( document.all.dscLote.value == '' ) {
                          alert( 'Descrição Inválida !' );
                          return false;
                       }
                       if( document.all.Selecionados.length == 0 ){
                          alert( 'Nenhum Job informado !' );
                          return false;
                       }

                       var Selecionados='';
                       for( var i=0; i < indexJob; i++ ) {
                           Selecionados = Selecionados + arrayJob [0][i] + "¶" + arrayJob [1][i] + "¶" + arrayJob [2][i] + "¶" + arrayJob [3][i] + ":delimitador";
                       }
                       document.all.hSelecionados.value = Selecionados;
                       document.all.opcao.value = 6;
                       EnviarFormulario();
                       indexJob=0;
                   }
                   function Destripa() {
                       var Selecionados= document.all.hSelecionados.value;
                       indexJob = 0;
                       seqJob=1;
                       var myArray = Selecionados.split(':delimitador');
                       for( var i=0; i < myArray.length -1; i++ ){
                           var myCampos = myArray[i].split( '¶' );
                           arrayJob[0][i] = myCampos[0];
                           arrayJob[1][i] = myCampos[1];
                           arrayJob[2][i] = myCampos[2];
                           arrayJob[3][i] = myCampos[3];
                           seqJob = parseInt(arrayJob[0][i],10) + 1;
                           indexJob = i +1;
                       }
                       if( isNaN(indexJob) ){indexJob = 0;}
                       MostraArray();
                   }
                   function Cancelar(){
                       document.all.Relatorios.disabled = true;
                       document.all.btAdicionar.disabled = true;
                       for( var i = (document.all.Selecionados.length -1); i > -1; i-- ) {
                          document.all.Selecionados.options.remove(i);
                       }
                       document.all.Selecionados.disabled = true;
                       document.all.lote.disabled = false;
                       document.all.lote.className = 'campo';
                       document.all.BotExcluir.disabled = true;
                       document.all.BotExecutar.disabled = true;
                       document.parentWindow.frameMostra.document.all.divTmp.innerHTML = "";
                       document.all.lote.focus();
                   }
                   function Adicionar(){
                       if( AbrirRotina( ':ws-href-pesquisa', document.all.Relatorios.value, 0, 0, 0, 0,[''],[''],'no','') )
                           {
                           for (var i=0; i < indexJob; i++) {
                                if(  nomeJob == arrayJob [1][i] ){
                                   alert ('Nome da rotina já existente!');
                                   return false;
                                }
                                if( (document.all.Relatorios.value == arrayJob[3][i]) && (ret == arrayJob [2][i]) ){
                                   alert ('Este relatório já foi parametrizado com o nome de [' + arrayJob [1][i] + ']');
                                   return false;
                                }
                           }
                           arrayJob [0][indexJob] = seqJob;
                           arrayJob [1][indexJob] = nomeJob;
                           arrayJob [2][indexJob] = ret;
                           arrayJob [3][indexJob] = document.all.Relatorios.value;
                           indexJob++;
                           seqJob++;

                           oOption = document.createElement( "OPTION" );
                           oOption.text = nomeJob; //document.all.Relatorios.options[document.all.Relatorios.options.selectedIndex].text;
                           oOption.value = document.all.Relatorios.value;
                           document.all.Selecionados.add( oOption );
                           oOption = null;
                           document.all.BotExecutar.disabled = true;
                           MostraArray();
                           }
                   }
                   function MostraArray(){
                      //var x='';
                      //for (var i=0; i <= indexJob; i++) {
                      //    x = x + arrayJob [0][i] + " + " + arrayJob [1][i] + " + " + arrayJob [3][i] + "<br>";
                      //}
                      //document.all.mostra.innerHTML = x + "<br>" + indexJob + "-" + seqJob ;
                   }


                   function MostraSelecao(){

                      var nIndice = '';
                      for( var i = 0; i < document.all.Selecionados.length; i++ ){
                          if( document.all.Selecionados[i].value == document.all.Selecionados.value ) {
                              nIndice = i;
                              break;
                          }
                      }
                      var sel = arrayJob [2][nIndice];
                      var ln=2;
                      var x;
                      x = '<table border=0 width=100% cellspacing=1 cellpadding=2><tr><td class=listar_titulo>Opção [' + arrayJob [3][nIndice] + '] ' + document.all.Selecionados.value + '-' + nIndice + '<td class=listar_titulo>Valor</tr>'
                      var myArray = sel.split('&');
                      for( var i=0; i < myArray.length -1; i++ ){
                           var myCampos = myArray[i].split( '=' );
                           if( ln > 2 ) {ln = 1;}
                           x = x + "<tr><td class=listar_corpo_" + ln + ">" + myCampos[0] + "</td><td class=listar_corpo_" + ln + ">" + myCampos[1] + "</td>";
                           ln++;
                       }
                      x = x + "</table></html>";

                      document.parentWindow.frameMostra.document.all.divTmp.innerHTML = x;
                   }




                   function AbrirRotina( href_pesquisa, rotina, top, left, width, height, aEntrada, aSaida, scrollbars, status, toolbar, menubar, location, resizable, sParm ){
                       topo = 220;
                       esquerda = 166;
                       largura = 618;
                       altura = 260;
                       screenParametros = "";
                       ParametrosIN = "";
                       ParametrosOUT = "";

                       scrollbars = "yes";
                       if( status != "yes" && status != "no" )
                           status = "no";
                       if( toolbar != "yes" && toolbar != "no" )
                           toolbar = "no";
                       if( menubar != "yes" && menubar != "no" )
                           menubar = "no";
                       if( location != "yes" && location != "no" )
                           location = "no";
                       if( resizable != "yes" && resizable != "no" )
                           resizable = "yes";

                       screenParametros = "scrollbars=" + scrollbars + ", " +
                                  "status=" + status + ", " +
                                  "toolbar=" + toolbar + ", " +
                                  "menubar=" + menubar + ", " +
                                  "location=" + location + ", " +
                                  "resizable=" + resizable;

                       if( top > 0 )
                           topo = top;
                       if( left > 0 )
                           esquerda = left;
                       if( width > 0 )
                           largura = width;
                       if( height > 0 )
                           altura = height;

                       for( Count = 0; Count < aEntrada.length; Count++ ) {
                           cTMP = Count + 1;
                           ParametrosIN = ParametrosIN + "cE" + cTMP + "=" + aEntrada[Count] + "&";
                       }
                       for( Count = 0; Count < aSaida.length; Count++ ) {
                           cTMP = Count + 1;
                           ParametrosOUT = ParametrosOUT + "cS" + cTMP + "=" + aSaida[Count] + "&";
                       }
                       nomeArq = prompt ('Nome do arquivo de saída','rel'+ seqJob);
                       if(  (nomeArq != null) ){
                            //var strURL = href_pesquisa + '&call=' + rotina + '&SubSessionID=' + getNewSessionID() + "&sm=1&nomeArquivoSM=" + nomeArq + "&" + ParametrosIN + ParametrosOUT + sParm;
                            var strURL = href_pesquisa + '&call=' + rotina + '&SubSessionID=' + getNewSessionID() + '&parMultiFrame=' + replace( ParametrosIN + ParametrosOUT + 'sm=1&nomeArquivoSM=' + nomeArq, '&', '¤' );

                            var opcoes = "dialogTop:91px; dialogHeight: 480px; dialogWidth: 780px;center: Yes; help: No; resizable: No; status: No;";
                            ret = window.showModalDialog(strURL,'rotina',opcoes);
                            if (ret != undefined) {
                               nomeJob = prompt ('Nome da rotina',document.all.Relatorios.options[document.all.Relatorios.options.selectedIndex].text);
                               if( (nomeJob != null) ) {
                                  return true;}
                               else{
                                  return false;
                               }
                            }
                            return false;
                      }
                   }

                  function ChecaTecla(){
                      var Tecla = event.keyCode;
                      if( Tecla == 46 ){
                          if( confirm( 'Confirma Exclusão deste Job?' ) ) {
                             Remove( document.all.Selecionados.value );
                          }
                      }
                  }

                  function Remove( Selecionado ){
                      var nIndice = '';
                      for( var i = 0; i < document.all.Selecionados.length; i++ ){
                          if( document.all.Selecionados[i].value == Selecionado ) {
                              nIndice = i;
                              break;
                          }
                      }
                      for( var i=0; i < indexJob; i++ ){
                          if( arrayJob [0][i] == Selecionado ){
                             arrayJob[0][i] = '';
                             arrayJob[1][i] = '';
                             arrayJob[2][i] = '';
                             arrayJob[3][i] = '';
                             document.all.Selecionados.remove( nIndice );
                             document.all.BotExecutar.disabled = true;
                             break;
                          }
                      }
                      MostraArray();
                  }



                   function ExecutaJob(){
                      var selecionado = document.all.Selecionados.value;
                      for (var i=0; i < indexJob; i++) {
                          if( arrayJob [0][i] == selecionado ){
                            var strURL = ':ws-href-pesquisa' + '&call=' + arrayJob [3][i] + '&SubSessionID=' + getNewSessionID() + "&sm=0&opcao=1&" + arrayJob [2][i];
                            docWindow = window.open( strURL, arrayJob [3][i], "top=220, left=166, width=618, height=260,scrollbars=yes,toolbar=no, menubar=no, location=no, resizable=no" );
                            docWindow.focus();
                            break;
                          }
                      }
                   }

                   function Executa(){
                      document.all.opcao.value = 1;
                      document.all.lote.disabled = false;
                      document.form1.submit();
                   }
                   function PesquisaLote(){
                       AbrirPesquisa( ':ws-href-pesquisa', 'PW00007P', 50, 200, 400, 430, ['SUBMIT-FRAME', 'CarregarLote()'], ['lote'],'no' );
                   }
               </script>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
       8000-tela section.
       8000.
            perform 8000-scripts-tela
            exec html
               <TABLE width=100% border=0>
                  <TR>
                     <TD>
               <TABLE rules=none width=100% :tabela-padrao border=0>
                  <TR>
                     <TD class=titulocampo width=10%>Rotina
                     <TD nowrap colspan=4>&nbsp;<input name=lote size=5 maxlength=5 value="" onChange="CarregarLote();" onKeyDown="AltPesquisa( 'PesquisaLote()' );" class="campo" DISABLED>
                                  <IMG title="Pesquisar Lote" style="cursor: hand;" onClick="PesquisaLote();"  src=":lnk-imgPath/busca.gif">
                                  <IMG title="Carregar Lote"  style="cursor: hand;" onClick="CarregarLote(1);" src=":lnk-imgPath/check.jpg">
                  <TR>
                     <TD class=titulocampo>Descrição
                     <TD nowrap colspan=4>&nbsp;<input name=dscLote size=40 maxlength=40 value="" class="campo">

                  <TR>
                     <TD nowrap colspan=5>&nbsp;
                  <TR>
                     <TD class=titulocampo valign=top>Relatorios
                     <TD nowrap valign=top width=10>
                         <table border=0 width=100% height=100%>
                          <tr nowrap>
                             <td><SELECT name=Relatorios disabled style="width=300" onKeyDown="DigitaCombo( this );"></SELECT>
                                 <button name=btAdicionar class=botao accesskey="A" style="width=60" onclick="Adicionar()" disabled><label><U>A</U>dicionar</button>
                          <tr>
                              <td><SELECT name=Selecionados disabled  size=20 style="WIDTH: 361;" onKeyDown="ChecaTecla(1);" onChange="MostraSelecao();"></SELECT></TD>
                         </table>
                     <TD class=titulocampo valign=top colspan=3>&nbsp;
                           <!--            <div id=mostra align=left width=99% >
                                           </div> -->
                                           <iframe style="height=312" id=frameMostra src="../scripts/vazio.html" frameborder=0></iframe>&nbsp;&nbsp;&nbsp;&nbsp;
                  </TR>
                  <tr>
                     <TD colspan=5>&nbsp;
                  </tr>
               </TABLE>
               </TABLE>
               <input type=hidden xpto=1 name=hSelecionados value=''>
               <!--
               <input type=hidden name=frame value=0>
               -->
            end-exec
            perform 8000-botoes-padroes
            exec html
            <script>
             document.all.DivBtReservado.innerHTML = '<BUTTON name=BotExecutar AccessKEY="E" onClick="Executa();" class="botao" disabled> <LABEL><U>E</U>xecutar</LABEL></BUTTON>';
            </script>
            end-exec
            perform 2001-carrega-tela-inicial.
       8000-exit.
            exit.


      *>===================================================================================
       8000-scripts-tela-execucao section.
       8000.
            exec html
               <script>
                   var arrayJob = new Array();
                   arrayJob[0] = new Array();    //Código job
                   arrayJob[1] = new Array();    //Nome job
                   arrayJob[2] = new Array();    //configuracao
                   arrayJob[3] = new Array();    //Nome do programa a ser executado
                   indexJob=0;
                   seqJob=0;

                   function CarregaLote(exec_procedure){
                       for (var i=0; i < seqJob; i++) {
                           arrayJob[0][i] = '';
                           arrayJob[1][i] = '';
                           arrayJob[2][i] = '';
                           arrayJob[3][i] = '';
                       }
                       indexJob=0;
                       seqJob=1;
                       MostraArray();
                       document.all.opcao.value = 9;
                       document.all.executa.value = exec_procedure;
                       document.all.lote.value = document.all.Rotina.value;
                       EnviarFormulario();
                   }


                   function Destripa() {
                       var Selecionados= document.all.hSelecionados.value;
                       indexJob = 0;
                       seqJob=0;
                       var myArray = Selecionados.split(':delimitador');
                       for( var i=0; i < myArray.length -1; i++ ){
                           var myCampos = myArray[i].split( '¶' );
                           arrayJob[0][i] = myCampos[0];
                           arrayJob[1][i] = myCampos[1];
                           arrayJob[2][i] = myCampos[2];
                           arrayJob[3][i] = myCampos[3];
                           indexJob = i +1;
                       }
                       if( isNaN(indexJob) ){indexJob = 0;}
                       MostraArray();
                   }

                   function MostraArray(){
                      var tipo = 1;     // Muda a cor de cada linha na tela
                      var tmp = "<table border=0 width=100% cellspacing=1 cellpadding=2>"
                      tmp = tmp + "<tr>" +
                                  "<td align=center class=listar_titulo nowrap>Status</td>" +
                                  "<td align=center class=listar_titulo nowrap>Job</td>" +
                                  "<td align=center class=listar_titulo nowrap>Hora Inicial</td>" +
                                  "<td align=center class=listar_titulo nowrap>Hora Final</td></tr>"
                      for (var i=0; i < indexJob; i++) {
                          tmp = tmp + "<tr>" +
                                  "<td class=listar_corpo_" + tipo + " align=center nowrap><input type=checkbox name=rot" + parseInt(arrayJob [0][i]) + " onclick='return false;' disabled style='height:10'></td>"+
                                  "<td class=listar_corpo_" + tipo + " nowrap>" +arrayJob [1][i] + " </td>"+
                                  "<td class=listar_corpo_" + tipo + " width=10% nowrap><div id=tempoini" + parseInt(arrayJob [0][i]) + "></div></td>" +
                                  "<td class=listar_corpo_" + tipo + " width=10% nowrap><div id=tempofim" + parseInt(arrayJob [0][i]) + "></div></td></tr>"
                          if( tipo == 1 ){
                              tipo =2;}
                          else{
                              tipo=1;}
                      }
                      tmp = tmp + "</table>"
                      document.all.mostra.innerHTML = tmp;
                   }
                   function now(){
                      var s = "";
                      var aux = "";
                      var d = new Date();
                      var c = ":";
                      d = new Date();
                      aux = d.getHours();
                      if( aux < 10) {aux =  "0" + aux;}
                      s += aux + c;
                      aux = d.getMinutes();
                      if( aux < 10) {aux =  "0" + aux;}
                      s += aux + c;
                      aux = d.getSeconds();
                      if( aux < 10) {aux =  "0" + aux;}
                      s += aux + c;
                      aux = d.getMilliseconds();
                      if( aux < 10) {aux =  "0" + aux;}
                      s += aux;
                      return s;
                   }

                   function IniciarMacro(){
                       document.all.imgWait.style.display = '';
                       document.all.Rotina.disabled = true;
                       document.all.botExecutar.disabled = true;
                       CarregaLote('Executa();');
                   }

                   function Executa(){
                      if( seqJob < indexJob ){
                         try {
                          document.all('tempofim' + parseInt(seqJob)).innerHTML = now();
                         }
                         catch(e){
                         }
                         document.all('tempoini' + parseInt(seqJob + 1)).innerHTML = now();
                         document.all('rot' + parseInt(seqJob + 1)).disabled = false;
                         document.all('rot' + parseInt(seqJob + 1)).checked = true;
                         document.all.iframe1.src = ':ws-href-pesquisa' + '&call=' + arrayJob[3][seqJob] + '&SubSessionID=' + getNewSessionID() + "&sm=1&opcao=1&opcaorelat=4&" + arrayJob [2][seqJob];
                         seqJob++;}
                      else{
                         document.all('tempofim' + parseInt(seqJob)).innerHTML = now();
                         seqJob=0;
                         document.all.iframe1.src = '';
                         document.all.Rotina.disabled = false;
                         document.all.botExecutar.disabled = false;
                         document.all.imgWait.style.display = 'none';
                      }

                   }

               </script>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
       8000-tela-execucao section.
       8000.
            perform 8000-scripts-tela-execucao
            inspect f-Selecionados replacing all '"' by "'"
            exec html
               <TABLE width=100% border=0>
                  <TR>
                     <TD>
               <TABLE rules=none width=100% :tabela-padrao border=0>
                  <TR>
                    <TD class=titulocampo width=10%>Rotina
                    <TD class=titulocampo width=100%><div align=left><SELECT name=Rotina disabled style="width=300" onChange="CarregaLote('');" onKeyDown="DigitaCombo( this );"></SELECT>
                    &nbsp;<button name=botExecutar accessKey="E" class=botao onclick="IniciarMacro(); "><label><U>E</U>xecutar</label></button>
                    <div style="display:none"><button name=botExecutarNext accessKey="E" class=botao onclick="Executa(); "><label><U>E</U>xecutar Mesmo</label></button></div></div>
               </TABLE>
               <BR><br>
                  <div style="display:none">
                    <iframe id=iframe1 src="">
                    </iframe>
                  </div>
                  <div id=mostra></div>

               </TABLE>
             <input type=hidden name=hSelecionados value=":f-Selecionados">
             <input type=hidden name=lote value=''>
             <input type=hidden name=executa value=''>
             <!--
             <input type=hidden name=frame value=0>
             -->
            end-exec
            perform 2002-carrega-tela-executar
            exec html
              <script>
               Destripa();
              </script>
            end-exec.
       8000-exit.
            exit.

      *>===================================================================================
       8500-move-dados-para-tela section.
       8500.
            if   f-frame = 9
                 inspect f-Selecionados replacing all '"' by "'"
                 inspect f-dsc-lote replacing all '"' by "'"
                 inspect f-executa replacing all '"' by "'"
                 exec html
                     <script>
                        try {
                         :objs-rotina ( 'lote' ).value = ':f-cod-lote';
                         :objs-rotina ( 'dscLote' ).value = ":f-dsc-lote";
                         :objs-rotina ( 'BotSalvar' ).disabled = false;
                         :objs-rotina ( 'BotExcluir' ).disabled = false;
                        }
                        catch(e) {
                        }
                         :objs-rotina ( 'hSelecionados' ).value = ":f-Selecionados";
                         :frame-rotina .Destripa();
                         :frame-rotina .eval( ":f-executa" );
                     </script>
                 end-exec
                 perform 8500-controles-tela   *>opcional
            end-if.
       8500-exit.
            exit.

      *>===================================================================================
       8500-controles-tela section.
       8500.
            exec html
               <script>

                   function HabilitaCodigo( NomeBotao ){
                       :objs-rotina ( 'lote' ).disabled = false;
                       :objs-rotina ( 'lote' ).value = '';
                       :objs-rotina ( 'lote' ).className = 'campo';
                       :objs-rotina ( 'Bot' + NomeBotao ).disabled = false;
                       :objs-rotina ( 'lote' ).focus();
                   }

//    *            Quando Carrega Cliente
                   if( :f-opcao == 02 ){
                       :objs-rotina ( 'lote' ).disabled = true;
                       :objs-rotina ( 'lote' ).className = 'clsDisabled';
                       :objs-rotina ( 'lote' ).value = ':f-cod-lote';
                       :objs-rotina ( 'Relatorios' ).disabled = false;
                       :objs-rotina ( 'btAdicionar' ).disabled = false;
                       :objs-rotina ( 'Selecionados' ).disabled = false;
                       if( :ws-inv-key == 0 ) {
                          :objs-rotina ( 'BotExcluir' ).disabled = false;
                          :objs-rotina ( 'BotExecutar' ).disabled = false;}
                       else {
                          :objs-rotina ( 'BotExcluir' ).disabled = true;
                          :objs-rotina ( 'BotExecutar' ).disabled = true;
                       }
                   }
//    *            Quando Salvar
                   if( :f-opcao == 06 ){
                       HabilitaCodigo( 'Salvar' );
                   }
//    *            Quando Excluir
                   if( :f-opcao == 07 ){
                       HabilitaCodigo( 'Excluir' );
                   }

               </script>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       8500-desabilita-combo section.
       8500.
            exec html
               <script>
                  :objs-rotina ( 'Relatorios' ).disabled = true;
                  :objs-rotina ( 'BotSalvar' ).disabled = false;
                  :objs-rotina ( 'BotCancelar' ).disabled = false;
                  :objs-rotina ( 'BotExcluir' ).disabled = true;
                  :objs-rotina ( 'BotExecutar' ).disabled = true;
               </script>
            end-exec.
       8500-exit.
            exit.

      *>===================================================================================
       copy PCP9000.CPY.                *>Rotinas Padrões

      *>===================================================================================
       2002-carrega-tela-executar section.
       2002.
            move 1                      to wx-index-1
            move spaces                 to ws-controle-oculto
            string ws-href-padrao delimited by "  ", "&ppopcao=6&call=", lnk-rotina delimited by "  ",
                   "&frame=9&opcao=10&lote=", f-cod-lote
                   into ws-controle-oculto pointer wx-index-1
            perform 8100-controle-oculto.
       2002-exit.
            exit.


      *>===================================================================================
      *>===================================================================================
      *>Leituras

       copy PCL99704.CPY.               *> Spooling Manager

