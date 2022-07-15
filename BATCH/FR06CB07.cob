       IDENTIFICATION                  DIVISION.                        00010000
       PROGRAM-ID. FR06CB07.                                            00020000
      *================================================================*00030000
      *     AUTOR   : JOEI LORENTI                                      00040000
      *     EMPRESA : FOURSYS                                           00050000
      *     DATA    : 20/04/2022                                        00060004
      *     OBJETIVO:   IMPRIMIR NA SYSOUT CAMPOS DE UM REGISTRO        00070000
      *                UTILIZANDO REDEFINES E FILLER                    00071000
      *================================================================*00080000
      *ENVIRONMENT                      DIVISION.                       00081000
      *================================================================*00081100
      *================================================================*00081200
       DATA                             DIVISION.                       00081300
      *================================================================*00081400
      *================================================================*00081500
       WORKING-STORAGE                  SECTION.                        00081600
      *================================================================*00081700
       01 WRK-REGISTRO    PIC X(45)        VALUE SPACES.                00081800
       01 WRK-NOME-RDF    REDEFINES        WRK-REGISTRO.                00081900
          05 WRK-NOME     PIC X(15).                                    00082101
          05 FILLER       PIC X(30).                                    00082203
       01 WRK-ENDERECO    REDEFINES        WRK-REGISTRO.                00082300
          05 FILLER       PIC X(15).                                    00082403
          05 WRK-RUA      PIC X(15).                                    00082501
          05 WRK-NUM      PIC 9(05).                                    00082601
          05 WRK-BAIRRO   PIC X(10).                                    00082701
                                                                        00082800
      *================================================================*00082900
       PROCEDURE                        DIVISION.                       00083000
      *================================================================*00083100
           ACCEPT WRK-REGISTRO          FROM SYSIN.                     00083200
           DISPLAY 'REGISTRO COMPLETO.: ' WRK-REGISTRO.                 00083300
           DISPLAY 'APENAS NOME.......: ' WRK-NOME.                     00083503
           DISPLAY 'ENDEREÇO COMPLETO.: ' WRK-RUA WRK-NUM WRK-BAIRRO    00083603
           STOP RUN.                                                    00084000
