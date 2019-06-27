      *-------------------------------------
      * ROTINA DE PROCESSAMENTO DOS DADOS DE DISTRIBUIÇÃO
      *-------------------------------------
       S-PROCESSA-DADOS SECTION.
       R-PROC-DADOS.
           CLOSE DISTRIBUICAO.
           OPEN OUTPUT DISTRIBUICAO.
           PERFORM LER-VEND.
           CLOSE DISTRIBUICAO.
           EXIT-PROGRAM.
       R-PROC-DADOS-EXIT.
           EXIT.

       LER-VEND SECTION.
       R-LER-VEND1.
      *    +-----------+     +-----------+
      *    +VENDEDOR(1)| ==> | CLIENTE(n)|
      *    +-----------+     *-----------+
           MOVE ZEROES TO CHAVE-VEN
           START VENDEDORES KEY IS NOT LESS THAN CHAVE-VEN
           IF FS-VEN NOT = '0'
                 MOVE 'ERRO START VENDEDORES ST ' TO WTXT
                 MOVE FS-VEN TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF.

       R-LER-VEND2.
           PERFORM UNTIL FS-VEN NOT = '00'
           READ VENDEDORES NEXT
           MOVE CODIGO-VEN    TO CODIGO-VEN-DIS
           MOVE LONGITUDE-VEN TO WDLO-VEN
           MOVE LATITUDE-VEN  TO WDLA-VEN

           MOVE ZEROES TO CHAVE-CLI
           START CLIENTES KEY IS NOT LESS THAN CHAVE-CLI
           IF FS-VEN NOT = '0'
                 MOVE 'ERRO START CLIENTES ST ' TO WTXT
                 MOVE FS-CLI TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           PERFORM R-LER-CLI UNTIL FS-CLI NOT = '00'

           PERFORM R-GERA-DISTR
           PERFORM R-GRAVA-DISTR
           END-PERFORM.

       R-LER-CLI.
           READ CLIENTES NEXT.
           MOVE CODIGO TO CODIGO-CLI-DIS.
           MOVE LONGITUDE TO WDLO-CLI.
           MOVE LATITUDE  TO WDLA-CLI.

       R-GERA-DISTR.
      *----CALCULO DLO
           COMPUTE WRESULT1 = (WDLO-VEN - WDLO-CLI).
           COMPUTE WRESULT2 = (WRESULT1 / 60).
           COMPUTE WRESULT-FINAL = (WRESULT2 * 1852).
           MOVE WRESULT-FINAL TO LONGITUDE-DIS.
           INITIALIZE WRESULT1 WRESULT2 WRESULT-FINAL.

      *----CALCULO DLA
           COMPUTE WRESULT1 = (WDLA-VEN - WDLA-CLI).
           COMPUTE WRESULT2 = (WRESULT1 / 60).
           COMPUTE WRESULT-FINAL = (WRESULT2 * 1852).
           MOVE WRESULT-FINAL TO LATITUDE-DIS.

       R-GRAVA-DISTR.
           WRITE REGISTRO-DISTRIBUICAO.
           IF FS-VEN NOT = '0'
              MOVE 'ERRO GRAVACAO DISTRIBUICAO ST ' TO WTXT
              MOVE FS-DIS TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
              MOVE '10' TO FS-VEN
              EXIT
           END-IF.
