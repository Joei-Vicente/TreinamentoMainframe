      *================================================================*00001001
       IDENTIFICATION                            DIVISION.              00002001
      *================================================================J00003001
                                                                        00004001
       PROGRAM-ID.  FR06CB31.                                           00005001
                                                                        00006001
      *================================================================*00007001
      *     AUTOR.......: JOEI LORENTI                                  00008001
      *     ANALISTA....: IVAN SANCHES                                  00008101
      *     EMPRESA.....: FOURSYS                                       00008201
      *     DATA........: 04/05/2022                                    00008311
      *----------------------------------------------------------------*00008401
      *                                                                 00008521
      *     OBJETIVO: RECEBER ARQUIVO (ARQPROD) E GRAVAR UM ARQUIVO     00008620
      *               MENOR (SAIARQ).                                   00008720
      *                                                                 00008801
      *----------------------------------------------------------------*00008901
      *     INPUT:                                                      00009001
      *     DDNAME           I/O                                        00009101
      *     ARQPROD           I                                         00009220
      *     SAIARQ            O                                         00009320
      *                                                                 00009401
      *================================================================*00009501
                                                                        00009601
      *================================================================*00009701
       ENVIRONMENT                               DIVISION.              00009801
      *================================================================*00009901
                                                                        00010001
      *----------------------------------------------------------------*00010101
       CONFIGURATION                             SECTION.               00011001
      *----------------------------------------------------------------*00020001
                                                                        00021001
       SPECIAL-NAMES.                                                   00022001
           DECIMAL-POINT IS COMMA.                                      00023001
                                                                        00024001
      *----------------------------------------------------------------*00025001
       INPUT-OUTPUT                              SECTION.               00026001
      *----------------------------------------------------------------*00026101
                                                                        00026201
       FILE-CONTROL.                                                    00026301
             SELECT ARQPROD            ASSIGN TO ARQPROD                00026414
                FILE STATUS            IS WRK-FS-ARQPROD.               00026510
             SELECT SAIARQ             ASSIGN TO SAIARQ                 00026607
                FILE STATUS            IS WRK-FS-SAIARQ.                00026707
                                                                        00026806
      *================================================================*00026906
       DATA                                      DIVISION.              00027006
      *================================================================*00027106
                                                                        00027206
      *----------------------------------------------------------------*00027306
       FILE                                      SECTION.               00027406
      *----------------------------------------------------------------*00027506
                                                                        00027606
       FD ARQPROD                                                       00027707
           RECORDING MODE IS F                                          00027806
           BLOCK CONTAINS 0 RECORDS.                                    00027906
       01 FD-ARQPROD PIC X(70).                                         00028007
                                                                        00028107
       FD SAIARQ                                                        00028206
           RECORDING MODE IS F                                          00028306
           BLOCK CONTAINS 0 RECORDS.                                    00028406
                                                                        00028506
       01 FD-SAIARQ PIC X(40).                                          00028606
                                                                        00028701
      *----------------------------------------------------------------*00028801
       WORKING-STORAGE                           SECTION.               00028901
      *----------------------------------------------------------------*00029001
                                                                        00029107
      *----------------------------------------------------------------*00029208
      * AREA AQUIVO PRODUTO                                             00029308
      *----------------------------------------------------------------*00029408
                                                                        00029508
        COPY 'B#PROD'.                                                  00029609
                                                                        00029709
      *01 WRK-REG-PRODUTO.                                              00029809
      *   05 WRK-REG-COD               PIC 9(05)    VALUE ZEROS.        00029909
      *   05 WRK-REG-PROD              PIC X(20)    VALUE SPACES.       00030009
      *   05 WRK-REG-FORN              PIC X(15)    VALUE SPACES.       00030109
      *   05 WRK-REG-VALOR             PIC 9(08)V99 VALUE ZEROS.        00030209
      *   05 WRK-REG-ESTOQUE           PIC 9(05)    VALUE ZEROS.        00030309
      *   05 FILLER                    PIC X9(15)   VALUE SPACES.       00030409
                                                                        00030508
      *----------------------------------------------------------------*00030607
       01 FILLER                       PIC X(50)               VALUE    00030707
            '**** AREA DE VARIAVEIS DA FILE SEC ****'.                  00030814
      *----------------------------------------------------------------*00030907
                                                                        00031010
       77 WRK-FS-ARQPROD               PIC X(02) VALUE SPACES.          00031110
       77 WRK-FS-SAIARQ                PIC X(02) VALUE SPACES.          00031201
       77 WRK-DADOS                    PIC X(40) VALUE SPACES.          00031502
                                                                        00031718
      *----------------------------------------------------------------*00031801
       01 FILLER                       PIC X(50)               VALUE    00031907
            '**** AREA DE AUXILIARES ****'.                             00032014
      *----------------------------------------------------------------*00032101
                                                                        00032201
       77 WRK-MSG                      PIC X(50) VALUE SPACES.          00032309
       77 WRK-FIMARQ                   PIC X(30) VALUE SPACES.          00032419
       77 WRK-FIM-ARQ                  PIC 9(01) VALUE ZEROS.           00032519
       01 WRK-SAIARQ                   PIC X(40) VALUE SPACES.          00032619
                                                                        00032701
      *----------------------------------------------------------------*00032807
       01 FILLER                       PIC X(50)               VALUE    00032907
            '****  AREA DE REDEFINES ****'.                             00033014
      *----------------------------------------------------------------*00033107
                                                                        00033201
      *================================================================*00033301
       PROCEDURE DIVISION.                                              00033401
      *================================================================*00033501
                                                                        00033601
      *----------------------------------------------------------------*00033701
       0000-PRINCIPAL                     SECTION.                      00033801
      *----------------------------------------------------------------*00033901
                                                                        00034001
           PERFORM 1000-INICIAR.                                        00035001
                                                                        00037401
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-ARQPROD EQUAL '10'.      00037515
                                                                        00037608
           PERFORM 3000-FINALIZAR.                                      00037708
                                                                        00037808
      *----------------------------------------------------------------*00037901
       0000-99-FIM.            EXIT.                                    00038008
      *----------------------------------------------------------------*00038101
                                                                        00039001
      *----------------------------------------------------------------*00040008
       1000-INICIAR                       SECTION.                      00040108
      *----------------------------------------------------------------*00040208
            OPEN INPUT ARQPROD                                          00040308
                 OUTPUT SAIARQ.                                         00040412
            PERFORM 1100-TESTAR-FILE-STATUS.                            00040508
                READ ARQPROD INTO WRK-REG-PRODUTO.                      00040608
                                                                        00040708
      *----------------------------------------------------------------*00040808
       1000-99-FIM.            EXIT.                                    00040908
      *----------------------------------------------------------------*00041008
      *----------------------------------------------------------------*00041108
       1100-TESTAR-FILE-STATUS            SECTION.                      00041208
      *----------------------------------------------------------------*00041308
                                                                        00041408
            IF WRK-FS-ARQPROD NOT EQUAL ZEROS                           00041508
                 MOVE ' ERRO ABERTURA ARQPROD ' TO WRK-MSG              00041608
                     PERFORM 9000-TRATAR-ERROS                          00041708
            END-IF.                                                     00041808
                                                                        00041908
            IF WRK-FS-SAIARQ NOT EQUAL ZEROS                            00042008
                 MOVE ' ERRO ABERTURA SAIARQ '  TO WRK-MSG              00042108
                     PERFORM 9000-TRATAR-ERROS                          00042208
            END-IF.                                                     00042308
                                                                        00042408
      *----------------------------------------------------------------*00042508
       1100-99-FIM.            EXIT.                                    00042608
      *----------------------------------------------------------------*00042708
                                                                        00042808
      *----------------------------------------------------------------*00042908
       2000-PROCESSAR                     SECTION.                      00043008
      *----------------------------------------------------------------*00043108
                                                                        00043208
            IF WRK-FS-ARQPROD EQUAL ZEROS                               00043310
                 MOVE FD-ARQPROD(1:40) TO WRK-SAIARQ                    00043419
                                                                        00043508
                 WRITE FD-SAIARQ FROM WRK-SAIARQ                        00043617
            ELSE                                                        00043708
                MOVE ' FIM DE ARQUIVO ' TO WRK-MSG                      00043808
            END-IF.                                                     00043908
                                                                        00044008
                READ ARQPROD INTO WRK-REG-PRODUTO.                      00044108
                                                                        00044208
      *----------------------------------------------------------------*00044308
       2000-99-FIM.            EXIT.                                    00044408
      *----------------------------------------------------------------*00044508
      *----------------------------------------------------------------*00044601
       3000-FINALIZAR                     SECTION.                      00044701
      *----------------------------------------------------------------*00044801
                                                                        00044901
            CLOSE SAIARQ.                                               00045004
               IF WRK-FS-SAIARQ NOT EQUAL ZEROS                         00045104
                 MOVE ' STATUS NO CLOSE ' TO WRK-MSG                    00045201
                 PERFORM 9000-TRATAR-ERROS                              00045301
               END-IF.                                                  00045401
            STOP RUN.                                                   00045516
      *----------------------------------------------------------------*00045601
       3000-99-FIM.            EXIT.                                    00045701
      *----------------------------------------------------------------*00045801
                                                                        00045901
      *----------------------------------------------------------------*00046001
       9000-TRATAR-ERROS                  SECTION.                      00046101
      *----------------------------------------------------------------*00046201
             DISPLAY '-------------------STATUS-----------------------'.00046301
             DISPLAY '  MENSAGEM        '  WRK-MSG.                     00046401
             DISPLAY '  FILE STATUS     '  WRK-FS-SAIARQ.               00046505
             DISPLAY '------------------------------------------------'.00046601
      *----------------------------------------------------------------*00046701
       9000-99-FIM.            EXIT.                                    00046801
      *----------------------------------------------------------------*00046901
                                                                        00047001