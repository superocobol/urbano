      *--------------------------
      * LAYOUT DO RELATORIOO DE CLIENTES
      *---------------------------
       01  CAB01.
           02 FILLER            PIC X(36) VALUE IS ALL '-'.
           02 FILLER            PIC X(25) VALUE
            '  RELATORIO DE CLIENTES '.
           02 FILLER            PIC X(36) VALUE IS ALL '-'.

       01  CAB02.
           03 FILLER PIC X(09) VALUE 'COD.   '.
           03 FILLER PIC X(01) VALUE SPACES.
           03 FILLER PIC X(12) VALUE 'RAZAO SOCIAL'.
           03 FILLER PIC X(30) VALUE SPACES.
           03 FILLER PIC X(04) VALUE 'CNPJ'.
           03 FILLER PIC X(16) VALUE SPACES.
           03 FILLER PIC X(08) VALUE 'LATITUDE'.
           03 FILLER PIC X(06) VALUE SPACES.
           03 FILLER PIC X(09) VALUE 'LONGITUDE'.

       01  CAB03.
           03 FILLER PIC X(94) VALUE IS ALL '-'.

       01  DET01.
           03 RL-CODIGO    PIC  9(007) VALUE ZEROES.
           03 FILLER PIC X(01)  VALUE SPACES.
           03 RL-RAZAO     PIC  X(040) VALUE SPACES.
           03 FILLER PIC X(01)  VALUE SPACES.
           03 RL-CNPJ      PIC  9(14) VALUE ZEROS.
           03 FILLER PIC X(01)  VALUE SPACES.
           03 RL-LATITUDE  PIC -999,99999999 VALUE ZEROS.
           03 FILLER PIC X(01)  VALUE SPACES.
           03 RL-LONGITUDE PIC -999,99999999 VALUE ZEROS.
           03 FILLER PIC X(01)  VALUE SPACES.
