#lang racket
(require 2htdp/image 2htdp/universe)
(define min-decay (make-parameter 0))
(define max-decay (make-parameter 3))
(define max-hbias (make-parameter 2))
(define max-vbias (make-parameter 1))
(define apply-bias (make-parameter +))
(define palette
  (list (make-color 7 7 7)
        (make-color 31 7 7)
        (make-color 47 15 7)
        (make-color 71 15 7)
        (make-color 87 23 7)
        (make-color 103 31 7)
        (make-color 119 31 7)
        (make-color 143 39 7)
        (make-color 159 47 7)
        (make-color 175 63 7)
        (make-color 191 71 7)
        (make-color 199 71 7)
        (make-color 223 79 7)
        (make-color 223 87 7)
        (make-color 223 87 7)
        (make-color 215 95 7)
        (make-color 215 95 7)
        (make-color 215 103 15)
        (make-color 207 111 15)
        (make-color 207 119 15)
        (make-color 207 127 15)
        (make-color 207 135 23)
        (make-color 199 135 23)
        (make-color 199 143 23)
        (make-color 199 151 31)
        (make-color 191 159 31)
        (make-color 191 159 31)
        (make-color 191 167 39)
        (make-color 191 167 39)
        (make-color 191 175 47)
        (make-color 183 175 47)
        (make-color 183 183 47)
        (make-color 183 183 55)
        (make-color 207 207 111)
        (make-color 223 223 159)
        (make-color 239 239 199)
        (make-color 255 255 255)))

(define (make-fire w h)
  (append (make-list (sub1 h) (make-list w 0))
          (list (make-list w (sub1 (length palette))))))
(define (fire-spread lst i [decay (λ () (+ (min-decay) (random (max-decay))))] [hbias (λ () (random (max-hbias)))] [vbias (λ () (add1 (random (max-vbias))))])
  (let ([x (min (+ i (vbias)) (sub1 (length lst)))])
    (for/fold ([res lst]) ([j (in-range (length (list-ref lst i)))])
      (list-update res i (λ (k) (list-set k (min (max 0 ((apply-bias) j (hbias))) (sub1 (length k)))
                                          (max 0 (- (list-ref (list-ref res x) j) (decay)))))))))
(define (render-fire lst [sz 1])
  (apply above (append (map (λ (row) (apply beside (map (λ (x) (square sz "solid" (list-ref palette x))) row))) lst)
                       (list (beside (text (format "Decay (~a, ~a) " (min-decay) (max-decay)) 12 "black")
                                     (text (format "Vertical Bias (1, ~a) " (max-vbias)) 12 "black")
                                     (text (format "Horizontal Bias (0, ~a)" (max-hbias)) 12 "black"))))))
;#|
(define line (make-parameter (* 2 (length palette))))
(big-bang (make-fire 100 (* 2 (length palette)))
  [on-tick (λ (lst) (foldl (λ (i res) (fire-spread res i)) lst (range (* 2 (sub1 (length palette))) -1 -1))) 1/24]
  [to-draw (λ (lst) (render-fire lst 5))]
  ;[record? #t]
  [on-key (λ (lst ev)
            (cond [(key=? ev "up") (max-decay (add1 (max-decay)))]
                  [(key=? ev "down") (max-decay (max 1 (sub1 (max-decay))))]
                  [(key=? ev "right") (max-hbias (add1 (max-hbias)))]
                  [(key=? ev "left") (max-hbias (max 1 (sub1 (max-hbias))))]
                  [(key=? ev "prior") (max-vbias (add1 (max-vbias)))]
                  [(key=? ev "next") (max-vbias (max 1 (sub1 (max-vbias))))]
                  [(key=? ev "\b") (min-decay (max 0 (sub1 (min-decay))))]
                  [(key=? ev " ") (min-decay (min (add1 (min-decay)) (sub1 (max-decay))))]
                  [(or (key=? ev "+") (key=? ev "add")) (apply-bias +)]
                  [(or (key=? ev "-") (key=? ev "subtract")) (apply-bias -)]) lst)])
;|#  
