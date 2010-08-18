       IDENTIFICATION           DIVISION.
       PROGRAM-ID.      SHIRITORI.
       ENVIRONMENT              DIVISION.
       INPUT-OUTPUT             SECTION.
       FILE-CONTROL.
        SELECT OPTIONAL S-FILE ASSIGN TO "G:\COBOL\SHIRITORI.TXT"
          ORGANIZATION IS RELATIVE
          ACCESS MODE  IS DYNAMIC
          RELATIVE KEY IS W-NUM.
       
        SELECT ALLOW-LIST ASSIGN "..\04 SHIRITORI_COMMON\ALLOW-LIST.TXT"
          ORGANIZATION LINE SEQUENTIAL.
       DATA                     DIVISION.
       FILE                     SECTION.
       FD S-FILE.
       COPY "S-FILE.CBF".

       FD ALLOW-LIST.
       COPY "ALLOW-LIST.CBF".
       WORKING-STORAGE          SECTION.
       01 IN-STR        PIC X(50).
       01 MY-NAME       PIC X(20).

       78 DEFAULT-NAME  VALUE "名無し".
       
       01 EOF-FLG       PIC X VALUE LOW-VALUE.
        88 EOF                VALUE HIGH-VALUE.

       01 ERR-FLG       PIC X VALUE LOW-VALUE.
        88 ERR                VALUE HIGH-VALUE.
       
       01 W-NUM         PIC 9(04) VALUE 1.
       
       01 R             PIC 9(02).
       
      * 文字列分割用のテーブル
       01 STR-TMP.
           02 C         PIC X(02) OCCURS 25 INDEXED BY P. *> C は Character の C

       01 STR-TMP2.
           02 D         PIC X(02) OCCURS 25 INDEXED BY Q. *> C の次なので D
       
       01 LOG-TBL.
           02 L         OCCURS 10000 INDEXED BY I. *>しりとりが1万語以上続くとおかしくなります(^^;
             03 L-WORD  PIC X(50).
             03 L-NAME  PIC X(20).

       01 ALLOW-TBL.
           02 A         PIC X(02) OCCURS 100 INDEXED PA. *> 使用可能なひらがな
           02 B         PIC X(02) OCCURS 100 INDEXED PB. *> 使用可能ではあるものの、語頭には使えない文字
       PROCEDURE                DIVISION.
       MAIN.
           PERFORM INIT
           
           PERFORM INPUT-NAME
           
           OPEN INPUT S-FILE
           PERFORM F-READ
           CLOSE S-FILE
           
           PERFORM INPUT-WORD
           PERFORM UNTIL IN-STR = "END" OR "end" OR "おわり" OR "終わり"
             IF IN-STR NOT = SPACE
             THEN
               PERFORM CHECK-WORD
               
               IF NOT ERR THEN
                 OPEN I-O S-FILE
                 PERFORM F-WRITE
                 IF ERR THEN
                   PERFORM F-READ
                 END-IF
                 CLOSE S-FILE
               END-IF
             ELSE
               OPEN INPUT S-FILE
               PERFORM F-READ
               CLOSE S-FILE
             END-IF
             PERFORM INPUT-WORD
           END-PERFORM
           STOP RUN.
       
       INIT.
      * =========================================================
      * =                       初期化                          =
      * =========================================================
           INITIALIZE LOG-TBL
           MOVE 1 TO W-NUM    *> W-NUMには常に、次に書き込む位置が入るようにする
           PERFORM ALLOW-INIT.
       
       ALLOW-INIT.
      * =========================================================
      * =              使用可能な文字一覧を読み込む             =
      * =========================================================
           OPEN INPUT ALLOW-LIST
           READ ALLOW-LIST INTO ALLOW-TBL
           CLOSE ALLOW-LIST.
       
       INPUT-NAME.
           DISPLAY "名前を入力してください。"
           ACCEPT MY-NAME
           IF MY-NAME = SPACE THEN
             MOVE DEFAULT-NAME TO MY-NAME
             DISPLAY "デフォルトの名前「" DEFAULT-NAME "」
      -                                             "に設定されました。"
           END-IF.

       INPUT-WORD.
           PERFORM DSP-WORD
           ACCEPT IN-STR.
       
       CHECK-WORD.
           MOVE LOW-VALUE TO ERR-FLG *> フラグ初期化
           
           IF W-NUM NOT = 1 THEN
             MOVE L-WORD(W-NUM - 1) TO STR-TMP
           END-IF
           MOVE IN-STR TO STR-TMP2
           
           PERFORM CHECK-INVALID-CHAR
           
           IF W-NUM NOT = 1 THEN
             PERFORM CHECK-START-WITH *> 二回目以降の入力の場合のみ
           END-IF
           
           PERFORM CHECK-END-WITH
           PERFORM CHECK-CONTAINS.
       
       CHECK-INVALID-CHAR.
       *> ＠ CHECK-WORD内でのチェック処理の一部です
       *> ---------------------------------------------------------
       *> - 入力された文字に、使用できない文字が含まれていないか  -
       *> -   ※ ひらがなと一部の記号以外は使用できない           -
       *> ---------------------------------------------------------
           PERFORM VARYING Q FROM 1 BY 1 UNTIL D(Q) = SPACE
             SET PA TO 1
             SEARCH A
              AT END
             *> まだBに使用できる文字として格納されている可能性があるため、すぐにはエラーを表示しない
               SET ERR TO TRUE
              WHEN A(PA) = D(Q)
               CONTINUE
             END-SEARCH
             
             IF ERR THEN
               MOVE LOW-VALUE TO ERR-FLG *> まだエラーではない可能性があるため
               SET PB TO 1
               SEARCH B
                AT END
                 SET ERR TO TRUE
                 DISPLAY "!! 「" D(Q) "」は使用できない文字らしいです"
                 EXIT PERFORM
                WHEN B(PB) = D(Q)
                 CONTINUE
               END-SEARCH
             END-IF
           END-PERFORM.
       
       CHECK-START-WITH.
       *> ＠ CHECK-WORD内でのチェック処理の一部です
       *> ------------------------------------------------------------
       *> - 入力された単語が、前の単語の最後の文字から始まっているか -
       *> ------------------------------------------------------------
           PERFORM FIND-CHAR
           IF C(P) NOT = D(1) THEN
             SET ERR TO TRUE
             DISPLAY "!! 「" C(P) "」から始まる単語を入力してください"
           END-IF.
       
       CHECK-END-WITH.
       *> ＠ CHECK-WORD内でのチェック処理の一部です
       *> ---------------------------------------------------------
       *> -       入力された単語が「ん」で終わっていないか        -
       *> ---------------------------------------------------------
           PERFORM FIND-CHAR2
           IF D(Q) = "ん" THEN
             SET ERR TO TRUE
             DISPLAY "!! 入力された単語が「ん」で終わっています"
           END-IF.
       
       CHECK-CONTAINS.
       *> ＠ CHECK-WORD内でのチェック処理の一部です
       *> ---------------------------------------------------------
       *> -        入力された単語がすでに使われていないか         -
       *> ---------------------------------------------------------
           SET I TO 1
           SEARCH L
            AT END CONTINUE
            WHEN L-WORD(I) = SPACE  CONTINUE  *> SPACE以降にはデータが無い
            WHEN L-WORD(I) = IN-STR
             SET ERR TO TRUE
             DISPLAY "!! その単語はもう使われているらしいですよ"
           END-SEARCH.
       
       FIND-CHAR.
       *> =========================================================
       *> =          STR-TMP内の最後の文字の位置を探す            =
       *> =========================================================
           PERFORM VARYING P FROM 1 BY 1 UNTIL C(P) = SPACE
             CONTINUE
           END-PERFORM
           SET P DOWN BY 1
           
         *> 最後の文字が「ー」「。」「、」などであれば巻き戻す
           PERFORM VARYING P FROM P BY -1 UNTIL P = 0
             SET PB TO 1
             SEARCH B
              AT END
               EXIT PERFORM            *> 巻き戻す文字がなければループを抜ける
              WHEN B(PB) = C(P)
               CONTINUE                *> こっちは何もしないでループを続ける
             END-SEARCH
           END-PERFORM.
       
       FIND-CHAR2.
       *> =========================================================
       *> =         STR-TMP2内の最後の文字の位置を探す            =
       *> =========================================================
           PERFORM VARYING Q FROM 1 BY 1 UNTIL D(Q) = SPACE
             CONTINUE
           END-PERFORM
           SET Q DOWN BY 1
           
         *> 最後の文字が「ー」「。」「、」などであれば巻き戻す
           PERFORM VARYING Q FROM Q BY -1 UNTIL Q = 0
             SET PB TO 1
             SEARCH B
              AT END
               EXIT PERFORM            *> 巻き戻す文字がなければループを抜ける
              WHEN B(PB) = D(Q)
               CONTINUE                *> こっちは何もしないでループを続ける
             END-SEARCH
           END-PERFORM.
       
       DSP-WORD.
           IF W-NUM = 1
           THEN
             DISPLAY SPACE
             DISPLAY "まだ誰も単語を入力していません。"
           ELSE
             DISPLAY SPACE
             DISPLAY "*** 直前に送られた単語５つ ***"
             
      *       直前の5つまでを表示する
             SET I TO W-NUM
             SET I DOWN BY 5
             IF I < 1 THEN
               SET I TO 1
             END-IF
             PERFORM VARYING I FROM I BY 1 UNTIL I = W-NUM
               DISPLAY L-WORD(I) SPACE "(" L-NAME(I) ")"
             END-PERFORM
             
             SUBTRACT 1 FROM W-NUM
             DISPLAY "ここまで " W-NUM " 個の単語が入力されました。"
             ADD 1 TO W-NUM
             
             MOVE L-WORD(W-NUM - 1) TO STR-TMP
             PERFORM FIND-CHAR
             DISPLAY SPACE
             DISPLAY "「" C(P) "」から始まる単語を入力してください。"
           END-IF.
       
       F-READ.
      *     前回読み込んだ場所から続きを読み込めば良いため、
      *     MOVE 1 TO W-NUM は不要となる。
           MOVE LOW-VALUE TO EOF-FLG
           PERFORM UNTIL EOF
             READ S-FILE
               INVALID KEY  SET EOF TO TRUE
               NOT INVALID KEY
                 MOVE S-WORD TO L-WORD(W-NUM)
                 MOVE S-NAME TO L-NAME(W-NUM)
                 ADD 1 TO W-NUM
             END-READ
           END-PERFORM.
       
       F-WRITE.
           MOVE LOW-VALUE TO ERR-FLG
           SET I TO 1
           SEARCH L
             AT END
               MOVE IN-STR  TO S-WORD L-WORD(W-NUM)
               MOVE MY-NAME TO S-NAME L-NAME(W-NUM)
               WRITE S-REC
                 INVALID KEY
                   DISPLAY "!! 誰かが先に書き込んでしまったようです。"
                   SET ERR TO TRUE
                 NOT INVALID KEY
                   ADD 1 TO W-NUM
               END-WRITE
             WHEN L-WORD(I) = IN-STR
               DISPLAY "!! その単語は既に使われています。"
               SET ERR TO TRUE
           END-SEARCH.

