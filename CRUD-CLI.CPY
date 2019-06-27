      *--------------------------
      *  CRUD DO CADASTRO DE CLIENTES
      *---------------------------
       S-INCLUIR SECTION.
       INCLUIR-1.
           PERFORM UNTIL WCODIGO = 9999999
           DISPLAY TELA-CLI
           MOVE '=> 9999999 PARA SAIR ' TO WMENSAGEM
           DISPLAY TELA-MENSAGEM
           ACCEPT TELA-CODIGO
           IF WCODIGO = 9999999
               CONTINUE
           ELSE
              PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
              IF WCODIGO NOT = ZEROES
                 PERFORM INCLUIR-2 THRU INC-FIM
              END-IF
           END-IF
           END-PERFORM.
       INCLUIR-1-EXIT.
           EXIT.

       INCLUIR-2.
           MOVE WCODIGO TO CODIGO
           PERFORM LER-CLIENTE
           IF FS-CLI  NOT = '00'
              CONTINUE
           ELSE
              MOVE 'CLIENTES JÁ EXISTE ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
           END-IF.
           INITIALIZE TELA-MENSAGEM.
           DISPLAY TELA-MENSAGEM.
           MOVE 1 TO WSAIDA.
       INCLUIR-3.
           PERFORM UNTIL WSAIDA = 1
           ACCEPT ACC-TELA-CLI
           IF WCNPJ = 99999999999999
               MOVE 1 TO WSAIDA
           END-IF
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM GRAVACAO-1 THRU GRAVACAO-EXIT
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE GRAVAÇÃO ST '  TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
           END-IF
           END-PERFORM.
       INC-FIM.
             EXIT.

       S-ALTERAR SECTION.
       ALTERAR-1.
           PERFORM UNTIL WCODIGO = 9999999
           DISPLAY TELA-CLI
           MOVE '=> 9999999 PARA SAIR ' TO WTXT
           DISPLAY TELA-MENSAGEM

           ACCEPT TELA-CODIGO
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO NOT = ZEROES
              PERFORM ALTERAR-2 THRU ALT-FIM
           END-IF
           END-PERFORM.

       ALTERAR-2.
           MOVE WCODIGO TO CODIGO
           PERFORM LER-CLIENTE
           IF FS-CLI NOT = '00'
              MOVE 'CLIENTES NÃO EXISTE ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
           END-IF.
           INITIALIZE TELA-MENSAGEM.
           DISPLAY TELA-MENSAGEM.

       ALTERAR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-CLI
           ACCEPT TELA-CLI
           MOVE 'J'     TO WCGCCPF-1
           MOVE WCNPJ   TO WCGCCPF-2
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM REGRAVACAO-1 THRU REGRAVACAO-EXIT
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE REGRAVAÇÃO ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
           END-IF
               END-PERFORM.

       ALT-FIM.
             EXIT.

       S-EXCLUIR SECTION.
       EXCLUIR-1.
           PERFORM UNTIL WCODIGO = 9999999
           MOVE '=> 9999999 PARA SAIR ' TO WTXT
           DISPLAY TELA-MENSAGEM
           DISPLAY TELA-CLI
           ACCEPT TELA-CODIGO
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO NOT = ZEROES
              PERFORM EXCLUIR-2 THRU EXCL-FIM
           END-IF
               END-PERFORM.
       EXCLUIR-2.
           MOVE WCODIGO TO CODIGO
           PERFORM LER-CLIENTE
           IF FS-CLI NOT = '00'
              MOVE 'CLIENTES NÃO EXISTE ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
           END-IF.
           INITIALIZE TELA-MENSAGEM.
           DISPLAY TELA-MENSAGEM.

       EXCLUIR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-CLI
           PERFORM S-DELETA
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE EXCLUSÃO ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO
           END-IF
           END-PERFORM.
       EXCL-FIM.
           EXIT.

       S-GRAVAR SECTION.
       GRAVACAO-1.
           MOVE 'CONFIRMA ?...' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              PERFORM MOVE-DADOS
              WRITE REGISTRO-CLIENTES
           END-IF.
       GRAVACAO-EXIT.
           EXIT.

       S-REGRAVAR SECTION.
       REGRAVACAO-1.
           MOVE 'CONFIRMA ?...' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              PERFORM MOVE-DADOS
              REWRITE REGISTRO-CLIENTES
           END-IF.
       REGRAVACAO-EXIT.
           EXIT.

       S-DELETA SECTION.
       DELETA-1.
           MOVE 'CONFIRMA EXCLUSÃO ?...' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              DELETE CLIENTES
           END-IF.
       DELETA-1-EXIT.
           EXIT.

       S-MOVER-DADOS SECTION.
       MOVE-DADOS.
           MOVE WCNPJ      TO CNPJ
           MOVE WRAZAO     TO RAZAO
           MOVE WLATITUDE  TO LATITUDE
           MOVE WLONGITUDE TO LONGITUDE.

       MOVE-DADOS-2.
           MOVE CNPJ      TO WCNPJ
           MOVE RAZAO     TO WRAZAO
           MOVE LATITUDE  TO WLATITUDE
           MOVE LONGITUDE TO WLONGITUDE.
