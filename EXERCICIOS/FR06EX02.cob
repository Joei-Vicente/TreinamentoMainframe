      *=============================================================*   00001001
       IDENTIFICATION                            DIVISION.              00002001
      *=============================================================*   00003001
                                                                        00004001
       PROGRAM-ID. FR06EX02.                                            00005001
                                                                        00006001
      *=============================================================*   00007001
      *   AUTOR...........: JOEI LORENTI                            *   00008001
      *   ANALISTA........: IVAN SANCHES                            *   00008101
      *   DATA ...........: 24/05/2022                              *   00009001
      *-------------------------------------------------------------*   00009101
      *                                                             *   00009201
      *   OBJETIVO........: ESTE PROGRAMA TEM A FINALIDADE DE       *   00010001
      *                     RECEBER DADOS  DOS ARQUIVOS DE ENTRADA  *   00010101
      *                     FUNC3 E PROJ3, FAZER A RELAÇÃO (BALANCO)*   00010201
      *                     ENTRE AS CHAVES E GRAVAR  NO ARQUIVO DE *   00010301
      *                     SAIDA  FUNPROJ3.                        *   00010401
      *                                                             *   00010501
      *-------------------------------------------------------------*   00010601
      *                                                             *   00010701
      *    ARQUIVOS....:                                            *   00010801
      *      DDNAME            I/O                  INCLUDE/BOOK    *   00010901
      *     FUNC3               I                      --------     *   00011001
      *     PROJ3               I                      --------     *   00011101
      *     FUNPROJ3            O                      --------     *   00011201
      *     EXCECAO             O                               *       00011303
      *-------------------------------------------------------------*   00011401
      *                                                             *   00011502
      *   MODULOS.....:                                             *   00011602
      *     #GRVLOG                         GRAVAR LOG DE ERROS     *   00011702
      *                                                             *   00011802
      *                                                             *   00011902
      *=============================================================*   00012301
                                                                        00012401
      *=============================================================*   00012701
       ENVIRONMENT                     DIVISION.                        00012802
      *=============================================================*   00013001
                                                                        00014001
      *-------------------------------------------------------------*   00015001
       CONFIGURATION                   SECTION.                         00015102
      *-------------------------------------------------------------*   00015201
       SPECIAL-NAMES.                                                   00015301
           DECIMAL-POINT IS COMMA.                                      00015401
                                                                        00015501
       INPUT-OUTPUT                    SECTION.                         00015602
                                                                        00015702
       FILE-CONTROL.                                                    00015801
            SELECT FUNC3               ASSIGN TO  FUNC3                 00015902
                 FILE STATUS IS WRK-FS-FUNC3.                           00016001
                                                                        00016201
            SELECT PROJ3               ASSIGN TO  PROJ3                 00016302
                 FILE STATUS IS WRK-FS-PROJ3.                           00016401
                                                                        00016501
            SELECT FUNPROJ3            ASSIGN TO FUNPROJ3               00016602
                 FILE STATUS IS WRK-FS-FUNPROJ3.                        00016701
                                                                        00016803
            SELECT EXCECAO             ASSIGN TO EXCECAO                00016903
                 FILE STATUS IS WRK-FS-EXC.                             00017004
                                                                        00017103
      *=============================================================*   00017203
       DATA                            DIVISION.                        00017303
      *=============================================================*   00017403
      *-------------------------------------------------------------*   00017503
       FILE                            SECTION.                         00017603
      *-------------------------------------------------------------*   00017703
       FD FUNC3                                                         00017803
           RECORDING MODE IS F                                          00017903
           LABEL RECORD IS STANDARD                                     00018003
           BLOCK CONTAINS 0 RECORDS.                                    00018103
      *------------------ LRECL 39                                      00018203
       01 FD-FUNC3.                                                     00018303
          05 FD-FUNC3-ID               PIC X(05).                       00018403
          05 FD-FUNC3-NOME             PIC X(30).                       00018503
          05 FD-FUNC3-SETOR            PIC 9(04).                       00018603
                                                                        00018703
                                                                        00018901
       FD PROJ3                                                         00019001
           RECORDING MODE IS F                                          00019101
           LABEL RECORD IS STANDARD                                     00019201
           BLOCK CONTAINS 0 RECORDS.                                    00020001
      *------------------ LRECL 28                                      00020101
       01 FD-PROJ3.                                                     00020201
          05 FD-PROJ3-ID               PIC X(05).                       00020402
          05 FD-PROJ3-PROJETO          PIC X(20).                       00020502
          05 FD-PROJ3-QTHORAS          PIC 9(03).                       00020602
                                                                        00020801
       FD FUNPROJ3                                                      00021001
           RECORDING MODE IS F.                                         00021104
      *------------------ LRECL 58                                      00021203
       01 FD-FUNPROJ3 PIC X(58).                                        00021301
                                                                        00021401
       FD EXCECAO                                                       00021503
           RECORDING MODE IS F.                                         00021604
      *------------------ LRECL 58                                      00021703
       01 FD-EXCECAO PIC X(35).                                         00021804
                                                                        00021903
      *-------------------------------------------------------------*   00022001
       WORKING-STORAGE                 SECTION.                         00022101
      *-------------------------------------------------------------*   00022201
       01 WRK-SAIDA.                                                    00022302
         05 WRK-IDFUNC                 PIC 9(05).                       00022402
         05 WRK-NOMEFUNC               PIC X(30).                       00022502
         05 WRK-PROJETO                PIC X(20).                       00022602
         05 WRK-QTHORAS                PIC 9(03).                       00022702
                                                                        00022801
      *-------------------------------------------------------------*   00022903
      *                    AREA DE VARIAVEIS DE FS                  *   00023003
      *-------------------------------------------------------------*   00023103
                                                                        00023201
       77 WRK-FS-FUNC3                 PIC 9(02).                       00023301
       77 WRK-FS-PROJ3                 PIC 9(02).                       00023401
       77 WRK-FS-FUNPROJ3              PIC 9(02).                       00023501
       77 WRK-FS-EXC                   PIC 9(02).                       00023604
                                                                        00023703
      *-------------------------------------------------------------*   00023803
      *                    AREA DE ACUMULADORES                     *   00023903
      *-------------------------------------------------------------*   00024003
                                                                        00024103
       77 WRK-ACU-LIDOS-FUN            PIC 9(03) VALUE ZEROS.           00024203
       77 WRK-ACU-LIDOS-PRO            PIC 9(03) VALUE ZEROS.           00024303
       77 WRK-ACU-GRAVADOS             PIC 9(03) VALUE ZEROS.           00024403
       77 WRK-ACU-GRAV-EXC             PIC 9(03) VALUE ZEROS.           00024503
                                                                        00024604
                                                                        00024704
      *-------------------------------------------------------------*   00024804
      *                    AREA DE AUXILIARES                       *   00024904
      *-------------------------------------------------------------*   00025004
                                                                        00025104
       77 WRK-AUX-EXC                  PIC 9(02) VALUE ZEROS.           00025204
       01 WRK-EXCECAO.                                                  00025304
          05 WRK-EXC-ID                PIC X(05).                       00025404
          05 WRK-EXC-NOME              PIC X(30).                       00025504
                                                                        00025604
      *-------------------------------------------------------------*   00025704
      *                    AREA BOOK GRVLOG - GRAVAR ERROS          *   00025804
      *-------------------------------------------------------------*   00025904
                                                                        00026004
            COPY '#GRVLOG'.                                             00026104
                                                                        00026204
      *=============================================================*   00026304
       PROCEDURE                       DIVISION.                        00026404
      *=============================================================*   00026504
      *----------------------------------------------------------------*00026604
       0000-PRINCIPAL                  SECTION.                         00026704
      *----------------------------------------------------------------*00026804
                                                                        00026904
            PERFORM 1000-INICIAR.                                       00027004
                                                                        00027104
            PERFORM 2000-VERIFICAR-VAZIO.                               00027204
                                                                        00027304
            PERFORM 3000-PROCESSAR     UNTIL WRK-FS-FUNC3 EQUAL 10      00027404
                                       AND   WRK-FS-PROJ3 EQUAL 10.     00027504
      * -----------ACU-LIDOS-FUNC                                       00027604
      *         ADD 1                 TO WRK-ACU-LIDOS-FUN              00027704
            PERFORM 4000-FINALIZAR.                                     00027804
                                                                        00027904
            STOP RUN.                                                   00028004
                                                                        00028104
                                                                        00028204
      *----------------------------------------------------------------*00028304
       1000-INICIAR                    SECTION.                         00028404
      *----------------------------------------------------------------*00028504
             OPEN INPUT  FUNC3                                          00028604
                         PROJ3                                          00028704
                  OUTPUT FUNPROJ3                                       00028804
                         EXCECAO.                                       00028904
                                                                        00029004
               PERFORM 1300-TESTARSTATUS.                               00029104
                                                                        00029204
      *----------------------------------------------------------------*00029304
       1300-TESTARSTATUS               SECTION.                         00029404
      *----------------------------------------------------------------*00029504
                                                                        00029604
                 PERFORM 1310-TESTARSTATUS-FUNC3.                       00029704
                 PERFORM 1320-TESTARSTATUS-PROJ3.                       00029804
                 PERFORM 1330-TESTARSTATUS-FUNPROJ3.                    00029904
                                                                        00030004
      *----------------------------------------------------------------*00030104
       1310-TESTARSTATUS-FUNC3         SECTION.                         00030204
      *----------------------------------------------------------------*00030304
                                                                        00030404
               IF WRK-FS-FUNC3         NOT EQUAL 00                     00030504
                 MOVE 'FR06EX02'               TO WRK-PROGRAMA          00030604
                 MOVE 'ERRO NO OPEN FUNC3 '    TO WRK-MSG-ERRO          00030704
                 MOVE '1300'                   TO WRK-SECTION           00030804
                 MOVE WRK-FS-FUNC3             TO WRK-STATUS            00030904
                  PERFORM 9000-TRATAERROS                               00031004
               END-IF.                                                  00031104
                                                                        00031204
      *----------------------------------------------------------------*00031304
       1320-TESTARSTATUS-PROJ3         SECTION.                         00031404
      *----------------------------------------------------------------*00031504
               IF WRK-FS-PROJ3 NOT EQUAL 00                             00031604
                 MOVE 'FR06EX02'               TO WRK-PROGRAMA          00031704
                 MOVE 'ERRO NO OPEN PROJ3'     TO WRK-MSG-ERRO          00031804
                 MOVE '1300'                   TO WRK-SECTION           00031904
                 MOVE WRK-FS-PROJ3             TO WRK-STATUS            00032004
                  PERFORM 9000-TRATAERROS                               00032104
               END-IF.                                                  00032204
                                                                        00032304
      *----------------------------------------------------------------*00032404
       1330-TESTARSTATUS-FUNPROJ3      SECTION.                         00032504
      *----------------------------------------------------------------*00032604
               IF WRK-FS-FUNPROJ3  NOT EQUAL 00                         00032704
                 MOVE 'FR06EX02'               TO WRK-PROGRAMA          00032804
                 MOVE 'ERRO NO OPEN FUNPROJ3 ' TO WRK-MSG-ERRO          00032904
                 MOVE '1300'                   TO WRK-SECTION           00033004
                 MOVE WRK-FS-FUNPROJ3          TO WRK-STATUS            00033104
                  PERFORM 9000-TRATAERROS                               00033204
               END-IF.                                                  00033304
                                                                        00033404
      *----------------------------------------------------------------*00033504
       2000-VERIFICAR-VAZIO            SECTION.                         00033604
      *----------------------------------------------------------------*00033704
                                                                        00033804
                 READ FUNC3.                                            00033904
                 READ PROJ3.                                            00034004
                                                                        00034104
      *----------------------------------------------------------------*00034204
       3000-PROCESSAR                  SECTION.                         00034304
      *----------------------------------------------------------------*00034404
                                                                        00034504
            EVALUATE TRUE                                               00034604
                                                                        00034704
                                                                        00034804
             WHEN FD-FUNC3-ID          EQUAL FD-PROJ3-ID                00034904
                   PERFORM 3005-MOVER                                   00035004
                   PERFORM 3100-GRAVAR                                  00035104
                   PERFORM 3200-LER-PROJ3                               00035204
                   MOVE 1              TO WRK-AUX-EXC                   00035304
                                                                        00035404
             WHEN FD-FUNC3-ID LESS FD-PROJ3-ID                          00035504
                 IF WRK-AUX-EXC        EQUAL ZERO                       00035604
                    MOVE  FD-FUNC3-ID   TO  WRK-EXC-ID                  00035704
                    MOVE  FD-FUNC3-NOME TO  WRK-EXC-NOME                00035804
                    WRITE FD-EXCECAO   FROM WRK-EXCECAO                 00035904
                   ELSE                                                 00036004
                     MOVE 0 TO WRK-AUX-EXC                              00036104
                 END-IF                                                 00036204
                   PERFORM 3300-LER-FUNC3                               00036304
                                                                        00036404
                                                                        00036504
             WHEN OTHER                                                 00036604
                DISPLAY 'CHAVE ERRADA'                                  00036704
                    PERFORM 3200-LER-PROJ3                              00036804
                                                                        00036904
            END-EVALUATE.                                               00037004
                                                                        00037104
      *----------------------------------------------------------------*00037204
       3005-MOVER                      SECTION.                         00037304
      *----------------------------------------------------------------*00037404
                                                                        00037504
                MOVE FD-FUNC3-ID       TO WRK-IDFUNC.                   00037604
                MOVE FD-FUNC3-NOME     TO WRK-NOMEFUNC.                 00037704
                MOVE FD-PROJ3-PROJETO  TO WRK-PROJETO.                  00037804
                MOVE FD-PROJ3-QTHORAS  TO WRK-QTHORAS.                  00037904
                                                                        00038004
      *----------------------------------------------------------------*00038104
       3100-GRAVAR                     SECTION.                         00038204
      *----------------------------------------------------------------*00038304
                                                                        00038404
                 WRITE FD-FUNPROJ3 FROM WRK-SAIDA.                      00038504
      *------------ACUM-GRAVADOS                                        00038604
               ADD 1            TO WRK-ACU-GRAVADOS.                    00038704
                                                                        00038804
      *----------------------------------------------------------------*00038904
       3200-LER-PROJ3                  SECTION.                         00039004
      *----------------------------------------------------------------*00039104
                READ PROJ3                                              00039204
                 IF WRK-FS-PROJ3 EQUAL 10                               00039304
                   MOVE HIGH-VALUES TO FD-PROJ3-ID                      00039404
                 END-IF.                                                00039504
               ADD 1              TO WRK-ACU-LIDOS-PRO.                 00039604
                                                                        00039704
      *----------------------------------------------------------------*00039804
       3300-LER-FUNC3                  SECTION.                         00039904
      *----------------------------------------------------------------*00040004
                READ FUNC3                                              00040104
               ADD 1              TO WRK-ACU-LIDOS-FUN.                 00040804
                                                                        00040904
      *----------------------------------------------------------------*00041004
       4000-FINALIZAR                  SECTION.                         00041104
      *----------------------------------------------------------------*00041204
             CLOSE FUNC3                                                00041304
                   PROJ3                                                00041404
                   FUNPROJ3                                             00041504
                   EXCECAO.                                             00041604
                                                                        00041704
             DISPLAY ' TOTAL LIDOS FUNC: ' WRK-ACU-LIDOS-FUN            00041804
             DISPLAY ' TOTAL LIDOS PROJ: ' WRK-ACU-LIDOS-PRO            00041904
             DISPLAY ' TOTAL GRAVADOS..: ' WRK-ACU-GRAVADOS             00042004
               PERFORM 1300-TESTARSTATUS.                               00042104
                                                                        00042204
                                                                        00042304
      *----------------------------------------------------------------*00042404
       9000-TRATAERROS                 SECTION.                         00042504
      *----------------------------------------------------------------*00042604
           CALL 'GRAVALOG' USING WRK-LOG.                               00043001
           STOP RUN.                                                    00050002
