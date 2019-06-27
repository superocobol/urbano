       S-INCLUIR SECTION.
       INCLUIR-1.
           PERFORM UNTIL WCODIGO-VEN = 999
      *    MOVE 1 TO WSAIDA
           DISPLAY TELA-VEN
           MOVE '=> 999 PARA SAIR '  TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT TELA-CODIGO
           IF WCODIGO-VEN = 999
               CONTINUE
           ELSE
              PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
              IF WCODIGO-VEN NOT = ZEROES
                 PERFORM INCLUIR-2 THRU INC-FIM
              END-IF
           END-IF
           END-PERFORM.
       INCLUIR-1-EXIT.
           EXIT.

       INCLUIR-2.
           MOVE WCODIGO-VEN TO CODIGO-VEN
           PERFORM LER-VEN
           IF FS-VEN = '00'
              MOVE 'VENDEDOR JÁ EXISTE ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO-VEN
              EXIT
           END-IF.
           INITIALIZE WMENSAGEM.

       INCLUIR-3.
           PERFORM UNTIL WSAIDA = 0
           ACCEPT TELA-VEN
           MOVE 'f'     TO WCGCCPF-1
           MOVE WCGCCPF TO LINK-CPFCGC
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM GRAVACAO-1 THRU GRAVACAO-EXIT
           IF FS-VEN NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE GRAVAÇÃO ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO-VEN
           END-IF
           END-PERFORM.
       INC-FIM.
             EXIT.

       S-ALTERAR SECTION.
       ALTERAR-1.
           PERFORM UNTIL WCODIGO-VEN= 999
           MOVE '=> 999 PARA SAIR '  TO WTXT
           DISPLAY TELA-MENSAGEM
           DISPLAY TELA-VEN
           ACCEPT TELA-CODIGO
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO-VEN NOT = ZEROES
              PERFORM ALTERAR-2 THRU ALT-FIM
           END-IF
           END-PERFORM.

       ALTERAR-2.
           MOVE WCODIGO-VEN TO CODIGO-VEN
           PERFORM LER-VEN
           IF FS-VEN NOT = '00'
              MOVE 'VENDEDOR NÃO EXISTE ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO-VEN
           END-IF.
           INITIALIZE WMENSAGEM.
           DISPLAY TELA-MENSAGEM.

       ALTERAR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-VEN
           ACCEPT TELA-VEN
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM REGRAVACAO-1 THRU REGRAVACAO-EXIT
           IF FS-VEN NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE REGRAVAÇÃO ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO-VEN
           END-IF
           END-PERFORM.

       ALT-FIM.
             EXIT.

       S-EXCLUIR SECTION.
       EXCLUIR-1.
           PERFORM UNTIL WCODIGO-VEN= 999
           MOVE '=> 999 PARA SAIR '  TO WTXT
           DISPLAY TELA-MENSAGEM
           DISPLAY TELA-VEN
           ACCEPT TELA-CODIGO
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO-VEN NOT = ZEROES
              PERFORM EXCLUIR-2 THRU EXCL-FIM
           END-IF
           END-PERFORM.
       EXCLUIR-2.
           MOVE WCODIGO-VEN TO CODIGO-VEN
           PERFORM LER-VEN
           IF FS-VEN NOT = '00'
              MOVE 'VENDEDOR NÃO EXISTE ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO
           END-IF.
           INITIALIZE WMENSAGEM.
           DISPLAY TELA-MENSAGEM.

       EXCLUIR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-VEN
           PERFORM S-DELETAR
           IF FS-VEN NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE EXCLUSÃO ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *        SETFOCUS WCODIGO
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
              WRITE REGISTRO-VENDEDORES
           END-IF.
       GRAVACAO-EXIT.
           EXIT.

       S-REGRAVAR SECTION.
       REGRAVACAO-1.
           INITIALIZE WMENSAGEM.
           MOVE 'CONFIRMA ?...' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              PERFORM MOVE-DADOS
              REWRITE REGISTRO-VENDEDORES
           END-IF.
       REGRAVACAO-EXIT.
           EXIT.

       S-DELETAR SECTION.
       DELETA-1.
           MOVE 'CONFIRMA EXCLUSÃO ?...' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              DELETE VENDEDORES
           END-IF.
       DELETA-1-EXIT.
           EXIT.

       S-MOVER-DADOS SECTION.
       MOVE-DADOS.
           MOVE WCPF-VEN        TO CPF-VEN
           MOVE WNOME-VEN      TO NOME-VEN
           MOVE WLATITUDE-VEN  TO LATITUDE-VEN
           MOVE WLONGITUDE-VEN TO LONGITUDE-VEN.

       MOVE-DADOS-2.
           MOVE CPF-VEN        TO WCPF-VEN
           MOVE NOME-VEN       TO WNOME-VEN
           MOVE LATITUDE-VEN  TO WLATITUDE-VEN
           MOVE LONGITUDE-VEN TO WLONGITUDE-VEN.
