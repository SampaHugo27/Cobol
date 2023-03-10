       IDENTIFICATION  DIVISION.
       PROGRAM-ID. EXEMPLO05.
       AUTHOR. ANGELA/HIRO.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 21/09/2004.
       DATE-COMPILED.
       SECURITY. APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS.  FAZ O SORT DO ARQUIVO DE ENTRADA.
      *          USANDO USING E GIVING.
      *          GERANDO COMO SAIDA UM ARQUIVO CLASSIFICADO.

       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT    SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN  TO DISK
           ORGANIZATION   IS LINE SEQUENTIAL.
           SELECT TRAB    ASSIGN  TO DISK.
           SELECT SAIDA   ASSIGN  TO DISK
           ORGANIZATION   IS LINE SEQUENTIAL.

       DATA            DIVISION.
       FILE            SECTION.
       FD  ENTRADA
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "ENT.DAT".
       01  REG-ENT.
           02 COD-ENT  PIC 9(04).
           02 NOME-ENT PIC X(30).

       SD  TRAB.
       01  REG-TRAB.
           02 COD-TRAB  PIC 9(04).
           02 NOME-TRAB PIC X(30).

       FD  SAIDA
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "SAI.DAT".
       01  REG-SAI.
           03 COD-SAI   PIC 9(04).
           03 NOME-SAI  PIC X(30).

       WORKING-STORAGE SECTION.
       77  FIM-ARQ     PIC X(03) VALUE "NAO".

       PROCEDURE       DIVISION.
       
       PGM-EX05.
          SORT         TRAB
                       ASCENDING  KEY  COD-TRAB
                       USING      ENTRADA
                       GIVING     SAIDA
          STOP         RUN.