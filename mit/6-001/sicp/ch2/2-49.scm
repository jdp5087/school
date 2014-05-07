###2.49###
PROMPT:
------------------------------------------------------------
Exercise 2.49.  Use segments->painter to define the following primitive painters:

a.  The painter that draws the outline of the designated frame.

b.  The painter that draws an ``X'' by connecting opposite corners of the frame.

c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

d.  The wave painter.
------------------------------------------------------------

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define outline
  (let ((bottom-left (make-vect 0 0))
	(bottom-right (make-vect 1 0))
	(top-left (make-vect 0 1))
	(top-right (make-vect 1 1)))
    (segments->painter
     (list (make-segment bottom-left bottom-right)
	   (make-segment bottom-right top-right)
	   (make-segment bottom-left top-left)
	   (make-segment top-left top-right)))))

(define x-painter
  (let ((bottom-left (make-vect 0 0))
	(bottom-right (make-vect 1 0))
	(top-left (make-vect 0 1))
	(top-right (make-vect 1 1)))
    (segments->painter
     (list (make-segment bottom-left top-right)
	   (make-segment bottom-right top-left)))))

(define diamond-painter
  (let ((bottom (make-vect 0.5 0))
	(left (make-vect 0 0.5))
	(right (make-vect 1 0.5))
	(top (make-vect 0.5 1)))
    (segments->painter
     (list (make-segment bottom left)
	   (make-segment bottom right)
	   (make-segment left top)
	   (make-segment right top)))))


(define wave-painter
  (let ((ll-bot-o (make-vect 0.25 0))
	(ll-bot-i (make-vect 0.35 0))
	(rl-bot-i (make-vect 0.55 0))
	(rl-bot-o (make-vect 0.75 0))
	(crotch (make-vect 0.5 0.25))
	(l-armpit (make-vect 0.3 0.6))
	(l-shoulder-b (make-vect 0.27 0.63))
	(l-shoulder-t (make-vect 0.27 0.65))
	(l-elbow-b (make-vect 0.2 0.5))
	(l-elbow-t (make-vect 0.2 0.62))
	(l-hand-b (make-vect 0 0.65))
	(l-hand-t (make-vect 0 0.70))
	(r-armpit (make-vect 0.6 0.6))
	(r-shoulder (make-vect 0.7 0.65))
	(r-hand-b (make-vect 1 0.23))
	(r-hand-t (make-vect 1 0.33))
	(l-neck (make-vect 0.40 0.65))
	(r-neck (make-vect 0.60 0.65))
	(l-head-middle (make-vect 0.3 0.80))
	(r-head-middle (make-vect 0.63 0.80))
	(l-head-top (make-vect 0.40 1))
	(r-head-top (make-vect 0.60 1)))
    (segments->painter
     (list (make-segment ll-bot-o l-armpit)
	   (make-segment ll-bot-i crotch)
	   (make-segment rl-bot-i crotch)
	   (make-segment rl-bot-o r-armpit)
	   (make-segment l-armpit l-shoulder-b)
	   (make-segment l-shoulder-b l-elbow-b)
	   (make-segment l-elbow-b l-hand-b)
	   (make-segment l-shoulder-t l-elbow-t)
	   (make-segment l-elbow-t l-hand-t)
	   (make-segment l-shoulder-t l-neck)
	   (make-segment r-armpit r-hand-b)
	   (make-segment r-shoulder r-hand-t)
	   (make-segment r-shoulder r-neck)
	   (make-segment l-neck l-head-middle)
	   (make-segment r-neck r-head-middle)
	   (make-segment l-head-middle l-head-top)
	   (make-segment r-head-middle r-head-top)))))
		       






