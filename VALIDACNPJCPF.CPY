      *-------------------------------------
      * ROTINA DE VALIDACAO DE CPF CNPJ
      *-------------------------------------
       VALIDA-CNPJ.
           CALL   'CNPJCPF' USING 01  LINK-CPFCGC.
           CANCEL 'CNPJCPF'
           IF LINK-CPFCGC(20:1) = 'I'
                 IF LINK-CPFCGC(1:1) = 'F'
                   MOVE 'CPF invalido...' TO WTXT
                 ELSE
                   MOVE 'CNPJ invalido...' TO WTXT
                 END-IF
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
      *          SETFOCUS WCNPJ
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
           END-IF.
       VALIDA-CNPJ-EXIT.
           EXIT.
