;***************************************************** VECTORES****************************************************************
; un vector es en este caso b�sicamente un punto conseguido con el comando GETPOINT

;***DIFERENCIA DE VECTORES***
;diferencia de vectores, en otras palabras tambi�n ser�a otro vector que va desde la cabeza de uno de
;ellos hasta el otro
(defun VDIF (p2 p1 / x1 y1 z1)
(setq x1 (- (car p2) (car p1)))
(setq y1 (- (cadr p2) (cadr p1)))
(setq z1 (- (caddr p2) (caddr p1)))
(list x1 y1 z1)
)
;***FIN DE DIFERENCIA DE VECTORES***

;***SUMA DE VECTORES***
;suma de vectores, en otras palabras tambi�n ser�a la resultante de ambos vectores
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
;devuelve el vector unitario de un vector, es decir un vector con la misma direcci�n pero de magnitud 1
(defun Vunit (p1 / p2 x1 y1 z1)
(setq p2 '(0 0 0))
(setq x1 (/ (car p1) (distance p1 p2)))
(setq y1 (/ (cadr p1) (distance p1 p2)))
(setq z1 (/ (caddr p1) (distance p1 p2)))
(list x1 y1 z1)
)
;***FIN DE VECTOR UNITARIO***

;***Multiplicaci�n escalar de vectores***
;si p1 =(x1, y1, z1) y p2 =(x2, y2, z2), la multiplicaci�n escalar es p1.p2=x1.x2+y1.y2+z1.z2
(defun Vmesc (p1 p2 / x1 y1 z1 x2 y2 z2)
(setq x1 (car p1))
(setq y1 (cadr p1))
(setq z1 (caddr p1))
(setq x2 (car p2))
(setq y2 (cadr p2))
(setq z2 (caddr p2))
(+ (* x1 x2) (* y1 y2) (* z1 z2))
)
;****FIN DE multiplicaci�n escalar de vectores****

;***Multiplicaci�n de un escalar por un vetor***
;si p1 =(x1, y1, z1) entonces a.p1=(a.x1, ay1, az1)
(defun Vprod (p1 a)
(list (* (car p1) a) (* (cadr p1) a) (* (caddr p1) a))
  )
;***FIN de multiplicaci�n de un escalar por un vector***

;***�ngulo que forma un vector con la horizontal***
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
;***FIN �ngulo que forma un vector***
;******** FIN DE VECTORES*********

;*******REDONDEAR N�MEROS********* falta mejorar no cumple la regla del d�gito par
(defun round (numero decimal)
(read (rtos  numero 2  decimal))
)
;*******redondear n�meros*********

;*******TRUNCAR N�MEROS********* trunca el n�mero a la cantidad de digitos especificado
(defun RoundMin (numero decimal)
(/ (fix(* numero (expt 10.0 decimal))) (expt 10.0 decimal))
)
;*******redondear n�meros*********

;**************************************INGENIER�A*********************************************************************

;*********************�REA COMERCIAL EN cm2 DE UN BARILLA DE DI�METRO COMERCIAL *************************
(defun dc2cm2 (diam);diam ser� un texto del tipo "3/4''" representando un d�ametro comercial de acero, o el n�mero que lo represente en pulgadas ejem: en vez de 3/4 puede ser 0.75
(cond ((or (= diam "1/4''") (= diam 0.250)) 0.32)
      ((= diam "8mm") 0.50)
      ((or (= diam "3/8''") (= diam 0.375)) 0.71)
      ((or (= diam "1/2''") (= diam 0.500)) 1.29)
      ((or (= diam "5/8''") (= diam 0.625)) 2.00)
      ((or (= diam "3/4''") (= diam 0.750)) 2.84)
      ((or (= diam "7/8''") (= diam 0.875)) 3.87)
      ((or (= diam "1''") (= diam 1.000)) 5.10)
      (t (prompt "no se a ingresado ning�n di�metro comercial") (terpri)))
)

;;*********************FIN �REA COMERCIAL EN cm2 DE UN BARILLA DE DI�METRO COMERCIAL*************************

;*********************SEPARACI�N DE REFUERZO DE ACERO DE UN DI�METRO COMERCIAL TENIENDO UNA RELACI�N Av/S *************************
(defun dcR2S (diam Avs);diam ser� un texto del tipo "3/4''" representando un d�ametro comercial de acero, o el n�mero que lo represente en pulgadas ejem: en vez de 3/4 puede ser 0.75; Avs ser� la relaci�n Av/S (�rea de la varilla sobre separaci�n)
(/ (fix (/ (dc2cm2 diam) Avs)) 100.00)
)

;;*********************FIN �REA COMERCIAL EN cm2 DE UN BARILLA DE DI�METRO COMERCIAL*************************