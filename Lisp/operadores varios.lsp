;***************************************************** VECTORES****************************************************************
; un vector es en este caso básicamente un punto conseguido con el comando GETPOINT

;***DIFERENCIA DE VECTORES***
;diferencia de vectores, en otras palabras también sería otro vector que va desde la cabeza de uno de
;ellos hasta el otro
(defun VDIF (p2 p1 / x1 y1 z1)
(setq x1 (- (car p2) (car p1)))
(setq y1 (- (cadr p2) (cadr p1)))
(setq z1 (- (caddr p2) (caddr p1)))
(list x1 y1 z1)
)
;***FIN DE DIFERENCIA DE VECTORES***

;***SUMA DE VECTORES***
;suma de vectores, en otras palabras también sería la resultante de ambos vectores
(defun VSUM (p1 p2 / x1 y1 z1)
(setq x1 (+ (car p2) (car p1)))
(setq y1 (+ (cadr p2) (cadr p1)))
(setq z1 (+ (caddr p2) (caddr p1)))
(list x1 y1 z1)
)
;***FIN DE SUMA DE VECTORES***

;***PROMEDIO DE VECTORES***
(defun VAVG (p1 p2 / x1 y1 z1)
(setq x1 (+ (car p2) (car p1)))
(setq y1 (+ (cadr p2) (cadr p1)))
(setq z1 (+ (caddr p2) (caddr p1)))
(list (/ x1 2) (/ y1 2) (/ z1 2))
)
;***FIN DE PROMEDIO DE VECTORES***

;***VECTOR UNITARIO***
;devuelve el vector unitario de un vector, es decir un vector con la misma dirección pero de magnitud 1
(defun Vunit (p1 / p2 x1 y1 z1)
(setq p2 '(0 0 0))
(setq x1 (/ (car p1) (distance p1 p2)))
(setq y1 (/ (cadr p1) (distance p1 p2)))
(setq z1 (/ (caddr p1) (distance p1 p2)))
(list x1 y1 z1)
)
;***FIN DE VECTOR UNITARIO***

;***Multiplicación escalar de vectores***
;si p1 =(x1, y1, z1) y p2 =(x2, y2, z2), la multiplicación escalar es p1.p2=x1.x2+y1.y2+z1.z2
(defun Vmesc (p1 p2 / x1 y1 z1 x2 y2 z2)
(setq x1 (car p1))
(setq y1 (cadr p1))
(setq z1 (caddr p1))
(setq x2 (car p2))
(setq y2 (cadr p2))
(setq z2 (caddr p2))
(+ (* x1 x2) (* y1 y2) (* z1 z2))
)
;****FIN DE multiplicación escalar de vectores****

;***Multiplicación de un escalar por un vetor***
;si p1 =(x1, y1, z1) entonces a.p1=(a.x1, ay1, az1)
(defun Vprod (p1 a)
(list (* (car p1) a) (* (cadr p1) a) (* (caddr p1) a))
  )
;***FIN de multiplicación de un escalar por un vector***

;***ángulo que forma un vector con la horizontal***
;solo vectores en 2D
(defun Vang (p1)
  	(if (/= (car p1) 0)
		(cond ((and (> (car p1) 0) (>= (cadr p1) 0 ))(atang (/ (cadr p1) (car p1))));primer cuadrante
		      ((and (< (car p1) 0) (>= (cadr p1) 0 ))(+ 180 (atang (/ (cadr p1) (car p1)))));segundo cuadrante
		      ((and (< (car p1) 0) (<= (cadr p1) 0 ))(+ 180 (atang (/ (cadr p1) (car p1)))));tercer cuadrante
		      ((and (> (car p1) 0) (<= (cadr p1) 0 ))(+ 360 (atang (/ (cadr p1) (car p1)))));cuarto cuadrante
		)
	    (if (> (cadr p1) 0 ) 90 (if (< (cadr p1) 0) 270)) 
	)
)
;***FIN ángulo que forma un vector***
;******** FIN DE VECTORES*********

;*******REDONDEAR NÚMEROS********* falta mejorar no cumple la regla del dígito par
(defun round (numero decimal)
(read (rtos  numero 2  decimal))
)
;*******redondear números*********

;*******TRUNCAR NÚMEROS********* trunca el número a la cantidad de digitos especificado
(defun RoundMin (numero decimal)
(/ (fix(* numero (expt 10.0 decimal))) (expt 10.0 decimal))
)
;*******redondear números*********

;**************************************INGENIERÍA*********************************************************************

;*********************ÁREA COMERCIAL EN cm2 DE UN BARILLA DE DIÁMETRO COMERCIAL *************************
(defun dc2cm2 (diam);diam será un texto del tipo "3/4''" representando un díametro comercial de acero, o el número que lo represente en pulgadas ejem: en vez de 3/4 puede ser 0.75
(cond ((or (= diam "1/4''") (= diam 0.250)) 0.32)
      ((= diam "8mm") 0.50)
      ((or (= diam "3/8''") (= diam 0.375)) 0.71)
      ((or (= diam "1/2''") (= diam 0.500)) 1.29)
      ((or (= diam "5/8''") (= diam 0.625)) 2.00)
      ((or (= diam "3/4''") (= diam 0.750)) 2.84)
      ((or (= diam "7/8''") (= diam 0.875)) 3.87)
      ((or (= diam "1''") (= diam 1.000)) 5.10)
      (t (prompt "no se a ingresado ningún diámetro comercial") (terpri)))
)

;;*********************FIN ÁREA COMERCIAL EN cm2 DE UN BARILLA DE DIÁMETRO COMERCIAL*************************

;*********************SEPARACIÓN DE REFUERZO DE ACERO DE UN DIÁMETRO COMERCIAL TENIENDO UNA RELACIÓN Av/S *************************
(defun dcR2S (diam Avs);diam será un texto del tipo "3/4''" representando un díametro comercial de acero, o el número que lo represente en pulgadas ejem: en vez de 3/4 puede ser 0.75; Avs será la relación Av/S (Área de la varilla sobre separación)
(/ (fix (/ (dc2cm2 diam) Avs)) 100.00)
)

;;*********************FIN ÁREA COMERCIAL EN cm2 DE UN BARILLA DE DIÁMETRO COMERCIAL*************************