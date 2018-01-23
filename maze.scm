(define (list-set lis i val) (append (take lis i) (cons val (drop lis (++ i)))))

(define (maze-ref maze x y) (list-ref (list-ref maze x) y))
(define (maze-set maze x y val)
  (list-set maze x (list-set (list-ref maze x) y val)))

;; 迷路の大きさ
(define XMAX 86) (define YMAX 124)

;;;;;
(define outer-wall (build-list (+ YMAX 1) (lambda (y) 1)))
(define field
  (append '(1 1 1) (append (build-list (- YMAX 5) (lambda (y) 0)) '(1 1 1))))
(define maze0
  (append (list outer-wall outer-wall outer-wall)
          (append (build-list (- XMAX 5) (lambda (x) field))
                  (list outer-wall outer-wall outer-wall))))
;; 迷路の初期状態
(define maze (maze-set (maze-set maze0 2 3 0) (- XMAX 2) (- YMAX 3) 0))

;;;;;
(define width 1080) (define height 1560)
(define thickness (min (/ width (- XMAX 2)) (/ height (- YMAX 2))))
(define x-margin (/ (- width (* thickness XMAX)) 2))
(define y-margin (/ (- height (* thickness YMAX)) 2))
;; 描画のための手続き
(define (place-wall x y scn)
  (place-image (square thickness "solid" "black")
               (+ (* x thickness) x-margin) (+ (* y thickness) y-margin)
               scn))

;;;;;
;; 画面の初期状態
(define scn
  (letrec ((f (lambda (x y)
                (cond ((> y (- YMAX 2)) (empty-scene))
                      ((> x (- XMAX 2)) (f 2 (++ y)))
                      ((= (maze-ref maze x y) 0) (f (++ x) y))
                      (else (place-wall x y (f (++ x) y)))))))
    (f 2 2)))

;;;;;
(define-struct pos (x y))
(define dps
  (list (make-pos 2 0) (make-pos 0 2) (make-pos -2 0) (make-pos 0 -2)))
(define sites0 (build-list (/ (- XMAX 8) 2)
                           (lambda (i) (make-pos (+ (* i 2) 4) 2))))
(define sites1 (build-list (/ (- XMAX 8) 2)
                           (lambda (i) (make-pos (+ (* i 2) 4) (- YMAX 2)))))
(define sites2 (build-list (/ (- YMAX 8) 2)
                           (lambda (j) (make-pos 2 (+ (* j 2) 4)))))
(define sites3 (build-list (/ (- YMAX 8) 2)
                           (lambda (j) (make-pos (- XMAX 2) (+ (* j 2) 4)))))
;; 壁を延ばせる場所のリスト
(define sites (foldr append '() (list sites0 sites1 sites2 sites3)))

;;;;;
(define (push-site sites x y) (cons (make-pos x y) sites))
(define (cut-sites sites)
  (let ((r (random (length sites)))) (append (drop sites r) (take sites r))))
(define dirtable '((0 1 2 3) (1 0 2 3) (1 2 0 3) (1 2 3 0)
                   (0 2 1 3) (2 0 1 3) (2 1 0 3) (2 1 3 0)
                   (0 2 3 1) (2 0 3 1) (2 3 0 1) (2 3 1 0)
                   (0 1 3 2) (1 0 3 2) (1 3 0 2) (1 3 2 0)
                   (0 3 1 2) (3 0 1 2) (3 1 0 2) (3 1 2 0)
                   (0 3 2 1) (3 0 2 1) (3 2 0 1) (3 2 1 0)))
(define (select-direction maze x y)
  (letrec ((lp (lambda (is)
                 (if (null? is) (values #f #f)
                   (let* ((dp (list-ref dps (car is)))
                          (dx (pos-x dp)) (dy (pos-y dp)))
                     (let ((x1 (+ x dx)) (y1 (+ y dy)))
                       (if (= (maze-ref maze x1 y1) 0)
                         (values x1 y1)
                         (lp (cdr is)))))))))
    (lp (list-ref dirtable (random 24)))))

;;;;;
(define-struct maze-status (maze scn sites))

(define (extend-wall w)
  (let ((maze (maze-status-maze w)) (scn (maze-status-scn w))
        (sites (maze-status-sites w)))
    (if (null? sites) w
      (let ((x (pos-x (car sites))) (y (pos-y (car sites))))
        (let-values (((x1 y1) (select-direction maze x y)))
          (if x1
            (let ((sites (push-site sites x1 y1))
                  (x0 (/ (+ x x1) 2)) (y0 (/ (+ y y1) 2)))
              (make-maze-status (maze-set (maze-set maze x0 y0 1) x1 y1 1)
                                (place-wall x1 y1 (place-wall x0 y0 scn))
                                sites))
            (extend-wall (make-maze-status maze scn
					   (cut-sites (cdr sites))))))))))

(big-bang (make-maze-status maze scn (cut-sites sites))
          (on-draw (lambda (w) (maze-status-scn w)))
          (on-tick extend-wall))
