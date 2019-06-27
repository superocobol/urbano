      *-------------------------------------
      * TELA DO RELATORIOO DE DISTRIBUIÇÃO
      *-------------------------------------
       SCREEN SECTION.
       01 TELA-OPCAO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE
          "SISTEMA EXEMPLO - RELATORIO DE DISTRIBUICAO".

       01 TELA-MENSAGEM.
          02 LINE 25 COL 10 VALUE "MENSAGEM.....:".
          02 LINE 25 COL 27 PIC X(50) USING WMENSAGEM.
