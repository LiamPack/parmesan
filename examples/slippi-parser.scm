
;;; slippi parsing example
(import (rnrs records syntactic (6))
        (rnrs io ports)
        (parmesan))

(define in-file "/home/liamp/projects/slippi_parser/test/raw.slp")
(define (read-file in-file)
  (let ([tx (make-transcoder (latin-1-codec))])
    (call-with-port (open-file-input-port in-file (file-options) (buffer-mode block) tx)
      (lambda (p) (get-string-all p)))))

(define (run-parser p str)
  (p (string->list str)
     (lambda (v s) (format #t "Parser ran successfully.~%") v)
     (lambda () (format #t "Parser failed.~%"))))

(define event-payload-size/p (bind (char/p #\5) (lambda (x) uint8/p)))
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
  (take 312))

(define game-start/p
  (lift
   (all-of/p
    version/p
    game-info-block/p
    uint32/p
    (repeat 4 uint32/p)
    (repeat 4 uint32/p)
    (repeat 4 (take 16))
    uint8/p
    uint8/p
    uint8/p
    uint8/p
    (repeat 4 (take 31))
    (repeat 4 (take 10))
    (return '()) ;; (repeat 4 (take 29)) TODO: version-dependent parse..
    (return '()) ;; uint8/p
    )
   (lambda (l) (apply make-game-start l))))

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

(define post-frame-update/p
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
    float32/p
    (return '()))
        (lambda (l) (apply make-post-frame-update l))))

(define-record-type game-end (fields game-end-method lras-initiator))
(define game-end/p (lift (all-of/p uint8/p int8/p) (lambda (l) (apply make-game-end l))))

(define-record-type frame-start
  (fields
   frame-number
   random-seed
   scene-frame-counter))
(define frame-start/p
  (lift (all-of/p int32/p uint32/p (return '()))
        (lambda (l) (apply make-frame-start l))))

(define-record-type frame-bookend (fields frame-number latest-finalized-frame))
(define frame-bookend/p (lift (all-of/p int32/p uint32/p) (lambda (l) (apply make-frame-bookend l))))

(define slippi/p
  (bind
   all-payload-sizes/p
   (lambda (payloads)
     (let ([event/p
            (bind
             take1
             (lambda (s)
               (if (= 0 (string-length s))
                   fail
                   (case (string-ref s 0)
                     ;; cases 054 -> 061 and 016
                     [(#\6) game-start/p]
                     [(#\7) pre-frame-update/p]
                     [(#\8) post-frame-update/p ]
                     [(#\9) game-end/p ]
                     ;; [(#\:)  ]
                     ;; [(#\;)  ]
                     ;; [(#\<)  ]
                     ;; [(#\=)  ]
                     ;; [(#\x10)]
                     [else  (take (cdr (assoc s payloads)))]))))])
       (many/p event/p)))))


(define (get-distances slp)
  (define postframes (filter (lambda (x) (post-frame-update? x)) slp))
  (define-values (g1 g2)
    (partition (lambda (x) (= (post-frame-update-player-index x) 1)) postframes))
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

(define in (read-file in-file))
(define out (run-parser slippi/p in))
(define distances (get-distances out))