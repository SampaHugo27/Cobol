       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX03.
       AUTHOR. HUGO DA SILVA SAMPAIO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 08/09/2013.
       DATE-COMPILED.
       SECURITY.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADENT ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELSAI ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD CADENT 
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADENT.DAT".

       01 REG-ENT.
              02 CODIGO         PIC 9(7).
              02 NOME           PIC X(30).
              02 SALARIO        PIC 9(5)V9(2).
              02 SEXO           PIC X(1).

       FD  RELSAI 
           LABEL        RECORD         IS          OMITTED.

       01  REG-ATR                  PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ     PIC 9(01) VALUE 0.
       77 CT-LIN      PIC 9(02) VALUE 21.
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       77 TOTAL-CLI   PIC 9(05) VALUE 0.
       77 TOTAL-SAL   PIC 9(9)V9(2) VALUE 0.

       01 CAB-01.
          02 FILLER    PIC X(69) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 999.
          02 FILLER    PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(21) VALUE SPACES.
          02 FILLER    PIC X(38)
                    VALUE "LISTAGEM DOS CLIENTES DO SEXO FEMININO".
          02 FILLER    PIC X(21) VALUE SPACES.

       01 CAB-03.
          02 FILLER    PIC X(23) VALUE SPACES.
          02 FILLER    PIC X(34)
                    VALUE "COM SALARIOS SUPERIORES A 5.000,00".
          02 FILLER    PIC X(23) VALUE SPACES.
       
       01 CAB-04.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(06) VALUE "CODIGO".
          02 FILLER    PIC X(16) VALUE SPACES.
          02 FILLER    PIC X(04) VALUE "NOME".
          02 FILLER    PIC X(34) VALUE SPACES.
          02 FILLER    PIC X(07) VALUE "SALARIO".
          02 FILLER    PIC X(08)  VALUE SPACES.

       01 CAB-05.
          02 FILLER      PIC X(05)  VALUE SPACES.
          02 FILLER      PIC X(28) VALUE "Total de Clientes impressos:".
          02 FILLER      PIC X(05) VALUE SPACES.
          02 MASC-TOTAL-CLI   PIC ZZ.999.
          02 FILLER      PIC X(36) VALUE SPACES.

       01 CAB-06.
          02 FILLER      PIC X(05)  VALUE SPACES.
          02 FILLER      PIC X(17) VALUE "Total de Sal??rio:".
          02 FILLER      PIC X(08) VALUE SPACES.
          02 MASC-TOTAL-SAL   PIC ZZZ.ZZZ.ZZ9,99.
          02 FILLER      PIC X(36) VALUE SPACES.

       01 DETALHE.
          02 FILLER      PIC X(05).
          02 CODIGO-CLI  PIC 9(07).
          02 FILLER      PIC X(11) VALUE SPACES.
          02 NOME-CLI    PIC X(30).
          02 FILLER      PIC X(11) VALUE SPACES.
          02 SALARIO-CLI PIC ZZ.999,99.
          02 FILLER      PIC X(07) VALUE SPACES.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM CABECALHO-FINAL.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADENT OUTPUT RELSAI.
              PERFORM VERIFICA-FIM.
       VERIFICA-FIM.
              READ CADENT AT END MOVE 1 TO FIM-ARQ.
       IMPRESSAO.
              IF CT-LIN GREATER THAN 20
                     PERFORM CABECALHO.
              PERFORM IMPDET.
       TERMINO.
              CLOSE CADENT RELSAI.
       PRINCIPAL.
              IF (SEXO EQUAL "F" OR SEXO EQUAL "f") AND 
                 (SALARIO NOT LESS THAN 5000)
                     PERFORM IMPRESSAO.
              PERFORM VERIFICA-FIM.
       IMPDET.
              MOVE NOME    TO NOME-CLI.
              MOVE CODIGO  TO CODIGO-CLI.
              MOVE SALARIO TO SALARIO-CLI.
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              ADD 1 TO TOTAL-CLI.
              ADD SALARIO TO TOTAL-SAL.
       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 1 LINES.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 1 LINES.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 3 LINES.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING 1 LINES.
              MOVE 1 TO CT-LIN.
       CABECALHO-FINAL.
              MOVE TOTAL-CLI TO MASC-TOTAL-CLI.
              MOVE TOTAL-SAL TO MASC-TOTAL-SAL.
              WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-06 AFTER ADVANCING 1 LINES.