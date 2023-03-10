       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX01.
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
       SELECT CADSOC1  ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT CADSOC2  ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELSOCIO ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD CADSOC1
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADSOC1.DAT".

       01 REG-ENT.
              02 CODIGO-PAGAMENTO1  PIC 9(2).
              02 NUMERO-SOCIO1      PIC 9(6).
              02 NOME-SOCIO1        PIC X(30).
              02 VALOR-PAGAMENTO1   PIC 9(09)V9(02).

       FD CADSOC2
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADSOC2.DAT".

       01 REG-SAI.
              02 NUMERO-SOCIO2      PIC 9(6).
              02 NOME-SOCIO2        PIC X(30).
              02 VALOR-PAGAMENTO2   PIC 9(09)V9(02).

       FD  RELSOCIO
           LABEL        RECORD         IS          OMITTED.

       01  REG-ATR                  PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ     PIC 9(01) VALUE 0.
       77 CT-LIN      PIC 9(02) VALUE 31.
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       77 VALOR-TOTAL PIC 9(12)V9(02) VALUE ZEROES.

       01 CAB-01.
          02 FILLER      PIC X(70) VALUE SPACES.
          02 FILLER      PIC X(05) VALUE "PAG. ".
          02 VAR-PAG     PIC 99.
          02 FILLER      PIC X(03) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(26) VALUE SPACES.
          02 FILLER    PIC X(27) VALUE "RELAÇÃO DE SÓCIOS ATRASADOS".
          02 FILLER    PIC X(27) VALUE SPACES.
       
       01 CAB-03.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(15) VALUE "NÚMERO DO SÓCIO".
          02 FILLER    PIC X(10) VALUE SPACES.
          02 FILLER    PIC X(13) VALUE "NOME DO SÓCIO".
          02 FILLER    PIC X(15) VALUE SPACES.
          02 FILLER    PIC X(22) VALUE "VALOR DO PAGAMENTO".


       01 CAB-04.
          02 FILLER      PIC X(05) VALUE SPACES.
          02 FILLER      PIC X(28) VALUE "Total de Sócios atrasados : ".
          02 TOTAL-SOCIO PIC 9(05) VALUE ZEROES.
          02 FILLER      PIC X(42) VALUE SPACES.

       01 CAB-05.
          02 FILLER           PIC X(05) VALUE SPACES.
          02 FILLER           PIC X(23) VALUE "Valor Total atrasado : ".
          02 MASC-VALOR-TOTAL PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
          02 FILLER           PIC X(34)  VALUE SPACES.

       01 DETALHE.
          02 FILLER    PIC X(09) VALUE SPACES.
          02 NUM       PIC 999.999.
          02 FILLER    PIC X(06) VALUE SPACES.
          02 NOM       PIC X(30).
          02 FILLER    PIC X(08) VALUE SPACES.
          02 VAL       PIC ZZZ.ZZZ.ZZ9,99.
          02 FILLER    PIC X(06) VALUE SPACES.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM CABECALHO-FINAL.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADSOC1 OUTPUT CADSOC2 RELSOCIO.
              PERFORM VERIFICA-FIM.
       VERIFICA-FIM.
              READ CADSOC1 AT END MOVE 1 TO FIM-ARQ.
       COPIA.
              MOVE NUMERO-SOCIO1     TO NUMERO-SOCIO2.
              MOVE NOME-SOCIO1       TO NOME-SOCIO2.
              MOVE VALOR-PAGAMENTO1  TO VALOR-PAGAMENTO2.
              WRITE REG-SAI.
       IMPRESSAO.
              IF CODIGO-PAGAMENTO1 EQUAL 1
                     PERFORM COPIA
              ELSE
                     IF CT-LIN GREATER THAN 30
                            PERFORM CABECALHO
                     END-IF
                     PERFORM IMPDET
              END-IF.
       TERMINO.
              CLOSE CADSOC1 CADSOC2 RELSOCIO.
       PRINCIPAL.
              PERFORM IMPRESSAO.
              PERFORM VERIFICA-FIM.
       IMPDET.
              MOVE NUMERO-SOCIO1    TO NUM.
              MOVE NOME-SOCIO1      TO NOM.
              MOVE VALOR-PAGAMENTO1 TO VAL.
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              ADD 1 TO TOTAL-SOCIO.
              ADD VALOR-PAGAMENTO1 TO VALOR-TOTAL.

       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 2 LINES.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING 1 LINE.
              MOVE 1 TO CT-LIN.

       CABECALHO-FINAL.
              MOVE SPACES TO REG-ATR.
              MOVE VALOR-TOTAL TO MASC-VALOR-TOTAL.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 2 LINES.
              WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 1 LINE.