;;; slippi parsing example
(import (rnrs records syntactic (6))
        (rnrs io ports)
        (srfi :1)
        (parmesan))

(define in-file "/home/liamp/projects/slippi_parser/test/raw.slp")
(define (read-file in-file)
  (let ([tx (make-transcoder (latin-1-codec))])
    (call-with-port (open-file-input-port in-file (file-options) (buffer-mode block) tx)
      (lambda (p) (get-string-all p)))))


(define event-payload-size/p
  (bind (char/p #\5) (lambda (x) uint8/p)))
(define other-event-payload/p
  (and/p take1 uint16/p))
(define (payload-sizes-map/p n)
  (repeat n other-event-payload/p))
(define all-payload-sizes/p
  (bind event-payload-size/p
        (lambda (size)
          (payload-sizes-map/p (/ (- size 1) 3)))))

(define-record-type version (fields major minor build unused))
(define-record-type game-start
  (fields
   version
   game-block-info
   random-seed
   dashback-fix
   shield-drop-fix
   nametags
   pal
   frozen-ps
   minor-scene
   major-scene
   display-names
   connect-codes
   slippi-uids
   language-option))

(define version/p
  (lift (repeat 4 uint8/p)
        (lambda (x) (apply make-version x))))
(define game-info-block/p
  (take/p 312))

(define (game-start/p v)
  (lift
   (all-of/p
    game-info-block/p
    uint32/p
    (repeat 4 uint32/p)
    (repeat 4 uint32/p)
    (repeat 4 (take/p 16))
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    (repeat 4 (take/p 31))
    (repeat 4 (take/p 10))
    (if (and (>= (version-major v) 3)
             (>= (version-minor v) 11))
        (repeat 4 (take/p 29))
        (return '()))
    (if (and (>= (version-major v) 3)
             (>= (version-minor v) 12))
        uint8/p
        (return '()))
    )
   (lambda (l) (apply make-game-start (cons v l)))))

(define-record-type pre-frame-update
  (fields
   frame-number
   player-index
   is-follower
   random-seed
   action-state-id
   x-position
   y-position
   facing-direction
   joystick-x
   joystick-y
   c-stick-x
   c-stick-y
   trigger
   processed-buttons
   physical-buttons
   physical-l-trigger
   physical-r-trigger
   x-analog-ucf
   percent))

(define pre-frame-update/p
  (lift (all-of/p
         int32/p
         int8/p
         int8/p
         int32/p
         int16/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         int32/p
         int16/p
         float32/p
         float32/p
         int8/p
         float32/p
         ) (lambda (l) (apply make-pre-frame-update l))))

(define-record-type post-frame-update
  (fields
   frame-number
   player-index
   is-follower
   internal-character-id
   action-state-id
   x-position
   y-position
   facing-direction
   percent
   shield-size
   last-hitting-attack-id
   current-combo-count
   last-hit-by
   stocks-remaining
   action-state-frame-counter
   state-bit-flags-1
   state-bit-flags-2
   state-bit-flags-3
   state-bit-flags-4
   state-bit-flags-5
   misc-as
   ground-air-state
   last-ground-id
   jumps-remaining
   l-cancel-status
   hurtbox-collision-state
   self-induced-air-x-speed
   self-induced-air-y-speed
   attack-based-x-speed
   attack-based-y-speed
   self-induced-ground-x-speed
   hitlag-frames-remaining
   animation-index))

(define (post-frame-update/p v)
  (lift (all-of/p
         int32/p
         uint8/p
         uint8/p
         uint8/p
         uint16/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         uint8/p
         uint8/p
         uint8/p
         uint8/p
         float32/p
         uint8/p
         uint8/p
         uint8/p
         uint8/p
         uint8/p
         float32/p
         uint8/p
         uint16/p
         uint8/p
         uint8/p
         uint8/p
         float32/p
         float32/p
         float32/p
         float32/p
         float32/p
         (if (and (>= (version-major v) 3)
                  (>= (version-minor v) 8))
             float32/p
             (return '()))
         (if (and (>= (version-major v) 3)
                  (>= (version-minor v) 11))
             uint32/p
             (return '())))
        (lambda (l) (apply make-post-frame-update l))))

(define-record-type game-end (fields game-end-method lras-initiator))
(define game-end/p (lift (all-of/p uint8/p int8/p) (lambda (l) (apply make-game-end l))))

(define-record-type frame-start
  (fields
   frame-number
   random-seed
   scene-frame-counter))

(define (frame-number r)
  (cond
   ([frame-start? r] (frame-start-frame-number r))
   ([pre-frame-update? r] (pre-frame-update-frame-number r))
   ([post-frame-update? r] (post-frame-update-frame-number r))
   (#t (format #t "Not a valid record\n"))))
(define (player-number r)
  (cond
   ([pre-frame-update? r] (pre-frame-update-player-index r))
   ([post-frame-update? r] (post-frame-update-player-index r))))
(define (x-position r)
  (cond
   ([pre-frame-update? r] (pre-frame-update-x-position r))
   ([post-frame-update? r] (post-frame-update-x-position r))))
(define (y-position r)
  (cond
   ([pre-frame-update? r] (pre-frame-update-y-position r))
   ([post-frame-update? r] (post-frame-update-y-position r))))
(define (action-state r)
  (cond
   ([post-frame-update? r] (post-frame-update-action-state r))))

(define (frame-start/p v)
  (lift (all-of/p int32/p uint32/p
                  (if (and (>= (version-major v) 3)
                           (>= (version-minor v) 10))
                      uint32/p
                      (return '())))
        (lambda (l) (apply make-frame-start l))))

(define-record-type frame-bookend (fields frame-number latest-finalized-frame))
(define frame-bookend/p (lift (all-of/p int32/p uint32/p) (lambda (l) (apply make-frame-bookend l))))

(define slippi-raw/p
  (bind
   all-payload-sizes/p
   (lambda (payloads)
     (let ([current-v (make-version 0 0 0 0)]
           [event/p
            (bind
             take1
             (lambda (s)
               (if (= 0 (string-length s))
                   fail
                   (begin
                     (case (string-ref s 0)
                       ;; cases 054 -> 061 and 016
                       [(#\6) (bind version/p (lambda (v) (set! current-v v) (game-start/p v)))]
                       [(#\7) pre-frame-update/p]
                       [(#\8) (post-frame-update/p current-v) ]
                       [(#\9) game-end/p ]
                       [(#\:) (frame-start/p current-v)]
                       ;; [(#\;)  ]
                       ;; [(#\<)  ]
                       ;; [(#\=)  ]
                       ;; [(#\x10)]
                       [else  (let ([c (assoc s payloads)])
                                (if c
                                    (take/p (cdr (assoc s payloads)))
                                    fail))])))))])
       (many/p event/p)))))

(define (slice l offset n)
  (take (drop l offset) n))

(define slippi/p
  (bind peek1
        (lambda (c)
          (cond
           [(char=? #\{ c)
            (and/p (take/p 15) slippi-raw/p)]
           [else slippi-raw/p]))))

(define (partition-by-player postframes)
  (partition (lambda (x) (= (post-frame-update-player-index x) 1)) postframes))

(define (run-parser p str)
  (p (string->list str)
     (lambda (v s) v)
     (lambda () (format #t "Parser failed.~%"))))

(define in (read-file  "/home/liamp/Slippi/Game_20221005T181051.slp"))
(define out-frames (filter post-frame-update? (run-parser slippi/p in)))

(define (get-distances slp)
  (define postframes (filter post-frame-update? slp))
  (define-values (g1 g2)
    (partition-by-player postframes))
  (define (sqr x) (expt x 2))
  (define distances
    (map
     (lambda (p1 p2)
       (sqrt (+
              (sqr
               (- (post-frame-update-y-position p1)
                  (post-frame-update-y-position p2)))
              (sqr
               (- (post-frame-update-x-position p1)
                  (post-frame-update-x-position p2)))))) g1 g2))
  distances)


(define (action-state-damaged? postframe)
  (or (and (>= (post-frame-update-action-state-id postframe) 75)
           (<= (post-frame-update-action-state-id postframe) 91))
      (= (post-frame-update-action-state-id postframe) 38)))
(define (action-state-grabbed? postframe)
  (or (and (>= (post-frame-update-action-state-id postframe) 223)
           (<= (post-frame-update-action-state-id postframe) 232))))
(define (action-state-hit? postframe)
  (or (action-state-grabbed? postframe)
      (action-state-damaged? postframe)))
;; TODO: command grabs



(define (get-damaged-states postframes)
  (define damaged-postframes
    (filter action-state-damaged? postframes))
  (define-values (p1 p2) (partition-by-player damaged-postframes))
  (define p1-frame-numbers (map post-frame-update-frame-number p1))
  p1-frame-numbers
  )

(define (combo-strings postframes)
  (define clean-postframes (delete-duplicates (sort < damaged-postframes)))
  (define (skip-while-one postframes)
    (if (and (not (null? postframes)) (= (first postframes) 1))
        (skip-while-one (cdr postframes))
        postframes))

  (define (helper postframes cur)
    '())

  (helper clean-postframes cur))


(define distances (get-distances out-frames))
