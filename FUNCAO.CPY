      *--------------------------
      *  FUNCOES DE TECLAS
      *---------------------------
       01  back-space             pic 9(003) value zeros.
       01  filler redefines back-space pic 9(006) binary.
           88 tecla-pf1           value 3211520.
           88 tecla-pf2           value 3211776.
           88 tecla-pf3           value 3212032.
           88 tecla-pf4           value 3212288.
           88 tecla-pf5           value 3212544.
           88 tecla-pf6           value 3212800.
           88 tecla-pf7           value 3213056.
           88 tecla-pf8           value 3213312.
           88 tecla-pf9           value 3213568.
           88 tecla-pf10          value 3213824.
           88 tecla-avanca        value 3279616.
           88 tecla-retorna       value 3279872.
           88 tecla-setas         value 3277824, 3277568.
           88 tecla-seta-baixo    value 3279616.
           88 tecla-seta-acima    value 3279872.
           88 tecla-seta-direita  value 3277824.
           88 tecla-seta-esquerda value 3277568.
           88 tecla-lateral       value 3278848, 3279104.
           88 tecla-escape        value 3211291, 3279360.
           88 tecla-pgup          value 3224832.
           88 tecla-pgdw          value 3225088.
           88 tecla-enter         value 3279629.
       01  back-space-r redefines back-space pic 9(006) binary.
       01  filler redefines back-space.
           05  ws-primeiro        pic 9(002) binary.
               88 tecla-funcao               value 49.
               88 tecla-controle             value 50.
               88 tecla-page-up              value 57.
           05  ws-segunda         pic 9(002) binary.
               88 esc                        value 00.
               88 pf1                        value 01.
               88 pf2                        value 02.
               88 pf3                        value 03.
               88 pf4                        value 04.
               88 pf5                        value 05.
               88 pf6                        value 06.
               88 pf7                        value 07.
               88 pf8                        value 08.
               88 pf9                        value 09.
               88 pf10                       value 10.
               88 seta-cima                  value 12.
               88 seta-baixo                 value 48.
           05  ws-tecla redefines ws-segunda pic x(001).
               88 ws-alg-tecla               value "A" THRU "Z",
                                                   "a" THRU "z".
           05  ws-terceiro        pic 9(002) binary.
