      *-------------------------------------
      * TELA DO CADASTRO DE VENDEDORES
      *-------------------------------------
       SCREEN SECTION.
       01 TELA-OPCAO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "SISTEMA EXEMPLO - VENDEDORES".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 4 COL 33 VALUE "SELECIONE A OPCAO".
          02 LINE 7 COL 10 VALUE "[1] INCLUSAO".
          02 LINE 8 COL 10 VALUE "[2] ALTERACCAO".
          02 LINE 9 COL 10 VALUE "[3] EXCLUSAO".
          02 LINE 10 COL 10 VALUE "[4] IMPORTACAO".
          02 LINE 11 COL 10 VALUE "[5] SAIR".
          02 LINE 15 COL 10 VALUE "DIGITE A OPCAO DESEJADA[.]".
          02 LINE 15 COL 34 PIC 9 USING WOPCAO-V AUTO.

       01 TELA-VEN.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "SISTEMA EXEMPLO - VENDEDORES".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 4 COL 33 VALUE "CADASTRO DE VENDEDORES".
          02 LINE 7 COL 10 VALUE "Codigo Vendedor.:".
          02 LINE 8 COL 10 VALUE "CPF.............:".
          02 LINE 9 COL 10 VALUE "Nome............:".
          02 LINE 10 COL 10 VALUE "Latitude........:".
          02 LINE 11 COL 10 VALUE "Longitude.......:".
          02 LINE 08 COL 27 PIC  9(14) USING WCPF-VEN.
          02 LINE 09 COL 27 PIC  X(040) USING WNOME-VEN.
          02 LINE 10 COL 27 PIC S9(003)v9(008) USING WLATITUDE-VEN.
          02 LINE 11 COL 27 PIC S9(003)v9(008) USING WLONGITUDE-VEN.

       01 TELA-CODIGO.
          02 LINE 07 COL 27 PIC  ZZZ USING WCODIGO-VEN.

       01 TELA-MENSAGEM.
          02 LINE 25 COL 10 VALUE "MENSAGEM.....:".
          02 LINE 25 COL 27 PIC X(50) USING WMENSAGEM.

       01 TELA-NOME-ARIMPORT.
          02 LINE 26 COL 10 PIC  X(50) USING WID-ARQ-IMPORT-1.
