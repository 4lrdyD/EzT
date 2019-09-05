
;***********************************************************+
;;;recibe ang en radianes y lo devuelve en grados
(defun grados (rad)
     (* (/ rad pi) 180.0)
);_fin de defun
;***********************************************************

;;;recibe ang en grados y lo devuelve en radianes
(defun radianes (grd)
     (* (/ grd 180.0) pi)
);_fin de defun
;***********************************************************

;;;recibe �ngulo en radianes y devuelve la tangente del mismo
(defun tan (rad)
     (/ (sin rad) (cos rad))
);_fin de defun
;***********************************************************

;;;recibe �ngulo en grados sexagesimales y devuelve la tangente del mismo
(defun tang (grd)
     (/ (sin (radianes grd)) (cos (radianes grd)));la funci�n "radianes" fue definida previamente
);_fin de defun
;******************************************************************************************************

;;;recibe �ngulo en grados sexagesimales y devuelve el seno del mismo
(defun seng (grd)
     (sin (radianes grd));la funci�n "radianes" fue definida previamente
);_fin de defun
;*****************************************************************************************************

;;;recibe �ngulo en grados sexagesimales y devuelve el coseno del mismo
(defun cosg (grd)
     (cos (radianes grd));la funci�n "radianes" fue definida previamente
);_fin de defun

;*****************************************************************************************************
;;;devuelve un �ngulo en sexagesimales cuya tangente sea el n�mero especificado
(defun atang (f)
     (* 180 (/ (atan f) pi))
);_fin de defun
;*****************************************************************************************************
(defun aseng(seno) ;funci�n arcoseno para devolver directamente el �ngulo en sexagesimales
(* 180 (/ (atan (/ seno (sqrt (- 1 (* seno seno))))) pi))
  )
;End deFun
;*****************************************************************************************************

(defun acosg(coseno) ;funci�n arcocoseno para devolver directamente el �ngulo en sexagesimales
(* 180 (/ (atan (/ (sqrt (- 1 (* coseno coseno))) coseno)) pi))
  )
;End deFun
;*****************************************************************************************************
;;;selecciona cualquier objeto de la capa seleccionada 
(defun SelSameLay (nombre)
     (ssget "x" (list (cons 8 nombre)))  
);_fin de defun
