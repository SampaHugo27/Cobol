       IDENTIFICATION DIVISION.

       PROGRAM-ID. EX04.
       AUTHOR. HUGO DA SILVA SAMPAIO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 09/09/2013.
       DATE-COMPILED.
       SECURITY.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADESTOQ ASSIGN TO DISK ORGANIZATION IS LINE SEQUENTIAL.
       SELECT RELESTOQ ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       FD CADESTOQ
          LABEL         RECORD         ARE         STANDARD
          VALUE       OF FILE-ID       IS        "CADESTOQ.DAT".

       01 REG-ENT.
              02 CODIGO         PIC 9(05).
              02 NOME           PIC X(15).
              02 QTD-ESTOQUE    PIC 9(05).
              02 PRECO-UNITARIO PIC 9(06)V99.

       FD  RELESTOQ
           LABEL        RECORD         IS          OMITTED.

       01  REG-ATR                  PIC X(80).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ     PIC 9(01) VALUE 0.
       77 CT-LIN      PIC 9(02) VALUE 26.
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       77 CUSTO-TOTAL PIC 9(07)V9(2) VALUE 0.

       77 QUTD-TOTAL  PIC 9(07) VALUE 0.
       77 PRECO-MEDIO PIC 9(06)V9(2) VALUE 0.
       77 PRECO-TOTAL PIC 9(08)V9(2) VALUE 0.  

       01 CAB-01.
          02 FILLER    PIC X(06) VALUE "DATA: ".
          02 DIA       PIC 9(02) VALUE 26.
          02 FILLER    PIC X(01) VALUE "/".
          02 MES       PIC 9(02) VALUE 09.
          02 FILLER    PIC X(01) VALUE "/".
          02 ANO       PIC 9(04) VALUE 2013.
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(32)
                    VALUE "RELATÓRIO DE MATERIAL EM ESTOQUE".
          02 FILLER    PIC X(18) VALUE SPACES.
          02 FILLER    PIC X(05) VALUE "PAG. ".
          02 VAR-PAG   PIC 9(03).
          02 FILLER    PIC X(02) VALUE SPACES.

       01 CAB-02.
          02 FILLER    PIC X(06) VALUE "CODIGO".
          02 FILLER    PIC X(05) VALUE SPACES.
          02 FILLER    PIC X(04) VALUE "NOME".
          02 FILLER    PIC X(09) VALUE SPACES.
          02 FILLER    PIC X(07) VALUE "QTDE DO".
          02 FILLER    PIC X(04) VALUE SPACES.
          02 FILLER    PIC X(14) VALUE "CUSTO UNITARIO".
          02 FILLER    PIC X(03) VALUE SPACES.
          02 FILLER    PIC X(11) VALUE "CUSTO TOTAL".
          02 FILLER    PIC X(07) VALUE SPACES.

       01 CAB-03.
          02 FILLER    PIC X(24) VALUE SPACES.
          02 FILLER    PIC X(34) VALUE "ESTOQUE".
          02 FILLER    PIC X(22) VALUE SPACES.

       01 CAB-04.
          02 FILLER    PIC X(16) VALUE "QUANTIDADE TOTAL".
          02 FILLER    PIC X(10) VALUE SPACES.
          02 FILLER    PIC X(11) VALUE "PREÇO MÉDIO".
          02 FILLER    PIC X(06) VALUE SPACES.
          02 FILLER    PIC X(11) VALUE "PREÇO TOTAL".
          02 FILLER    PIC X(26) VALUE SPACES.

       01 CAB-05.
          02 FILLER           PIC X(03).
          02 QUTD-TOTAL-REL   PIC Z.ZZZ.ZZ9.
          02 FILLER           PIC X(12) VALUE SPACES.
          02 PRECO-MEDIO-REL  PIC ZZZ.ZZ9,99.
          02 FILLER           PIC X(08) VALUE SPACES.
          02 PRECO-TOTAL-REL  PIC ZZ.ZZZ.ZZ9,99.
       
       01 DETALHE.
          02 CODIGO-REL        PIC 9(05).
          02 FILLER            PIC X(02) VALUE SPACES.
          02 NOME-REL          PIC X(15).
          02 FILLER            PIC X(02) VALUE SPACES.
          02 QTD-ESTOQUE-REL   PIC ZZ.ZZ9.
          02 FILLER            PIC X(05) VALUE SPACES.
          02 CUSTO-UNIT-REL    PIC ZZZ.ZZ9,99.
          02 FILLER            PIC X(05) VALUE SPACES.
          02 CUSTO-TOTAL-REL   PIC Z.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.

       PROGRAMA05.

       PERFORM INICIO.
       PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL 1.
       PERFORM FOLHA-SOMATORIA-GERAL.
       PERFORM TERMINO.

       STOP RUN.

       INICIO.
              OPEN INPUT CADESTOQ OUTPUT RELESTOQ.
              PERFORM VERIFICA-FIM.
       VERIFICA-FIM.
              READ CADESTOQ AT END MOVE 1 TO FIM-ARQ.
       IMPRESSAO.
              IF CT-LIN GREATER THAN 25
                     PERFORM CABECALHO.
              PERFORM IMPDET.
       TERMINO.
              CLOSE CADESTOQ RELESTOQ.
       PRINCIPAL.
              PERFORM IMPRESSAO.
              PERFORM VERIFICA-FIM.
       IMPDET.
              MOVE NOME    TO NOME-REL.
              MOVE CODIGO  TO CODIGO-REL.
              MOVE QTD-ESTOQUE TO QTD-ESTOQUE-REL.
              MOVE PRECO-UNITARIO TO CUSTO-UNIT-REL.
              MULTIPLY QTD-ESTOQUE BY PRECO-UNITARIO
                                      GIVING CUSTO-TOTAL.
              MOVE CUSTO-TOTAL TO CUSTO-TOTAL-REL.
              WRITE REG-ATR FROM DETALHE AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.
              ADD QTD-ESTOQUE TO QUTD-TOTAL.
              ADD CUSTO-TOTAL TO PRECO-TOTAL.
       CABECALHO.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-02 AFTER ADVANCING 3 LINES.
              WRITE REG-ATR FROM CAB-03 AFTER ADVANCING 1 LINE.
              MOVE 1 TO CT-LIN.
       FOLHA-SOMATORIA-GERAL.
              DIVIDE PRECO-TOTAL BY QUTD-TOTAL GIVING PRECO-MEDIO.
              MOVE QUTD-TOTAL  TO QUTD-TOTAL-REL.
              MOVE PRECO-MEDIO TO PRECO-MEDIO-REL.
              MOVE PRECO-TOTAL TO PRECO-TOTAL-REL.
              MOVE SPACES TO REG-ATR.
              WRITE REG-ATR AFTER ADVANCING PAGE.
              WRITE REG-ATR FROM CAB-01 AFTER ADVANCING 1 LINE.
              WRITE REG-ATR FROM CAB-04 AFTER ADVANCING 3 LINES.
              WRITE REG-ATR FROM CAB-05 AFTER ADVANCING 2 LINES.