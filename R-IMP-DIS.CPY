      *-------------------------------------
      * ROTINA DE IMPRESSAO DO RELATORIOO DE DISTRIBUIÇÃO
      *-------------------------------------
       S-MPR-DISTRIBUICAO SECTION.
       R-IMPR-DISTRIBUICAO-1.
           OPEN I-O DISTRIBUICAO.
           IF FS-DIS NOT = '0'
                 MOVE 'ERRO OOPEN DISTRIBUIÇÃO ST ' TO WTXT
                 MOVE FS-DIS TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF

           START DISTRIBUICAO KEY IS NOT LESS THAN LONGITUDE-DIS
           IF FS-DIS NOT = '0'
                 MOVE 'ERRO START DISTRIBUIÇÃO ST ' TO WTXT
                 MOVE FS-DIS TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           MOVE 'RELDIS1.CSV' TO WID-ARQ-REL
           OPEN OUTPUT ARQREL
           PERFORM S-IMP-CAB
           PERFORM S-IMPRIME UNTIL FS-DIS NOT = '00'.
       R-GERA-DISTRIBUICAO-1-EXIT.
           EXIT.

       S-IMPRIME SECTION.
       R-IMPRIME-1.
           READ DISTRIBUICAO NEXT.
           MOVE CODIGO-VEN-DIS TO CODIGO-VEN
           READ VENDEDORES
           IF FS-VEN = '00'
              MOVE NOME-VEN TO RL-NOME-VEN
              MOVE CODIGO-VEN-DIS TO RL-CODIGO-VEN
              MOVE LONGITUDE-DIS  TO RL-DISTANCIA-VEN
           END-IF
           MOVE CODIGO-CLI-DIS TO CODIGO
           READ CLIENTES
           IF FS-CLI = '00'
              MOVE RAZAO TO RL-RAZAO-CLI
              MOVE CODIGO TO RL-CODIGO-CLI
              IF WCODIGO-CLI-ANT = ZEROES
                 MOVE CODIGO TO WCODIGO-CLI-ANT
              END-IF
           END-IF
           PERFORM S-IMP-DET.
       R-IMP-REL-VEN-1-EXIT.
           EXIT.

       S-IMP-CAB SECTION.
       R-IMP-CAB.
           MOVE CAB01 TO ARQREL-DATA-01
           WRITE ARQREL-REC
           MOVE CAB02 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
           MOVE CAB03 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
       R-IMP-CAB-EXIT.
           EXIT.

       S-IMP-DET SECTION.
       R-IMP-DET.
           IF WCODIGO-CLI-ANT NOT = CODIGO
              MOVE CODIGO TO WCODIGO-CLI-ANT
              MOVE CAB03 TO ARQREL-DATA-01
              WRITE ARQREL-REC
           END-IF
           MOVE DET01 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
       R-IMP-DET-EXIT.
           EXIT.
