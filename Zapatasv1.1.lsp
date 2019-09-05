								;FUNCI�N
								;-------

(defun est_cota_actual(/ a b);guarda el nombre del estilo de cota actual
(command "_dimlinear" '(1 1 0) '(1 2 0) '(2 1 0));dibuja una cota lineal
(setq a (entget (ssname (ssget "L") 0)));obtiene la lista de la entidad dibujada
(command "_erase" "L" "");borra la entidad dibujada
(setq b (cdr (assoc 3 a)));guarda el nombre del estilo de cota
)

								;FUNCI�N
								;-------
(defun est_texto_actual(/ a b);guarda el nombre del estilo de texto actual
(command "_mtext" '(1 1 0) '(1 2 0) "blabla" "");dibuja un texto
(setq a (entget (ssname (ssget "L") 0)));obtiene la lista de la entidad dibujada
(command "_erase" "L" "");borra la entidad dibujada
(setq b (cdr (assoc 7 a)));guarda el nombre del estilo de texto
)

								;FUNCI�N
								;-------
(defun txtRefDiam (desc key keytxt / D);obtiene un di�metro de refuerzo con descripci�n "desc" o devuelve la palabra clave "key"
  (initget (strcat "8mm 1-4'' 3-8'' 1-2'' 5-8'' 3-4'' 7-8'' 1'' " key))(setq D (getkword (strcat "\n Di�metro del refuerzo " desc "[1-4''/8mm/3-8''/1-2''/5-8''/3-4''/7-8''/1''" keytxt "]: ")))
  	(cond ((= D \n)(setq D "1/2''"))
		((= D "1-4''")(setq D "1/4''"))
		((= D "3-8''")(setq D "3/8''"))
		((= D "1-2''")(setq D "1/2''"))
	        ((= D "5-8''")(setq D "5/8''"))
	      	((= D "3-4''")(setq D "3/4''"))
	      	((= D "7-8''")(setq D "7/8''")))
  D)
								;FUNCI�N
								;-------
(defun txtRefSep (desc key keytxt / S);obtiene una separaci�n del refuerzo en metros con descripci�n "desc" o devuelve la palabra clave "key"
(initget 7 key)(setq S (getreal (strcat "\n Separaci�n del refuerzo " desc"(m): [" keytxt "]")))
  S)

								;FUNCI�N
								;-------
(defun Cant2Sep (n L / S nc pot);distribuye una cantidad de elementos ingresados en una longitud, y devolver� la separaci�n existente entre elementos
(setq S (roundmin (/ L (- n 1)) 2));de preferencia la separaci�n debe truncarse a 2 d�gitos, pero la separaci�n por dicho truncamiento puede contradecir la cantidad de elementos
  					;roundmin es una funci�n personalizada, ver archivo "operadores varios"
(setq nc (+ 1 (fix (/ L S))))
(setq pot 3)  
(while (/= nc n)
  (cond ((> nc n)
	 (cond ((= (fix (rem (* S (expt 10 pot)) 10)) 0);se greg� la funci�n fix para corregir posibles errores generados al usar la funci�n rem
		(setq S (+ S (/ 5.00 (expt 10 pot))));si S*10^pot es multiplo de 10, se suma 5/10^pot a S
		(setq nc (+ 1 (fix (/ L S)))));se determina el nuevo valor de nc con el nuevo valor de S
	       
	       ((= (fix (rem (* S (expt 10 pot)) 5)) 0);se greg� la funci�n fix para corregir posibles errores generados al usar la funci�n rem
		(setq S (+ S (/ 1.00 (expt 10 pot)))); si S*10^pot es multiplo de 5, se suma 1/10^pot a S
		(setq nc (+ 1 (fix (/ L S)))))
	       
	       (t
		(setq S (+ S (/ 1.00 (expt 10 pot)))); si S*10^pot no es multiplo de 5 ni de 10 , igual se le suma 1/10^pot a S
		(cond ((= (fix (rem (* S (expt 10 pot)) 5)) 0);se greg� la funci�n fix para corregir posibles errores generados al usar la funci�n rem
		       (setq pot (+ 1 pot))
		       (setq S (- S (/ 1.00 (expt 10 pot))))))    
		(setq nc (+ 1 (fix (/ L S)))))))		
	(t
	 (cond ((= (fix (rem (* S (expt 10 pot)) 10)) 0);se greg� la funci�n fix para corregir posibles errores generados al usar la funci�n rem
		(setq S (- S (/ 5.00 (expt 10 pot)))); si S*10^pot es multiplo de 10, se resta 5/10^pot a S
		(setq nc (+ 1 (fix (/ L S)))))
	       
	       ((= (fix (rem (* S (expt 10 pot)) 5.00)) 0);se greg� la funci�n fix para corregir posibles errores generados al usar la funci�n rem
		(setq S (- S (/ 4.00 (expt 10 pot)))); si S*10^pot es multiplo de 5, se resta 4/10^pot a S
		(setq nc (+ 1 (fix (/ L S)))))
	       (t (setq pot (+ 1 pot))))))) 
		
  S)

								;FUNCI�N
								;-------
(defun txtRefCant (desc key keytxt / N);obtiene una cantidad de barras de refuerzo con descripci�n "desc" o devuelve la palabra clave "key"
(initget 7 key)(setq N (getint (strcat "\n Cantidad de barras " desc": [" keytxt "]")))
  N)

;(command (txtrefcant "Hola" "Gola" "/Gola") 5) <----- Pruebas, Borrar


;************************************************************************************************************************************
;*************************************************INICIO funci�n principal)**********************************************************
;************************************************************************************************************************************
(defun c:Zapata (/ osnap_ punto A B Dx Dy Sx Sy r Nbarras_A Nbarras_B d_A d_B entidad capa_ant punto_1x punto_2x text_est ayudante1)

 ;ingreso de datos
  (setq punto (getpoint "\n Punto de inserci�n: "))
  (initget 7)(setq A (getreal "\n Longitud de la zapata (m): "))
  (initget 7)(setq B (getreal "\n Ancho de la zapata (m): "))
  
  ;*****obteniendo los di�metros del refuerzo
  (setq ayudante1 1)
  (while (/= ayudante1 0)
    (cond ((= ayudante1 1)
	   (setq Dx (txtRefDiam "" "DDiam" "/Dos di�metros"));txtRefDiam es una funci�n personalizada, ver m�s arriba
	   	(cond ((/= Dx "DDiam") (setq Dy Dx)(setq ayudante1 0))
		      (t (setq ayudante1 2))))
	  ((= ayudante1 2)
	   (setq Dx (txtRefDiam "(Direcci�n X)" "Volver" "/Volver"))
	   	(cond ((= Dx "Volver")(setq ayudante1 1))
		      (t (setq ayudante1 3))))
	  ((= ayudante1 3)
	   (setq Dy (txtRefDiam "(Direcci�n Y)" "Volver" "/Volver"))
	   	(cond ((= Dy "Volver")(setq ayudante1 2))
		      (t (setq ayudante1 0))))))
  ;******fin de obtener di�metro de refuerzo
  
   (initget 7)(setq r (getreal "\n Recubrimiento (cm): [7cm/7.5cm] ")) ;obteniendo el recubrimiento
  
  ;(initget 7)(setq S (getreal "\n Separaci�n del refuerzo (m): "))<----anterior borrar
  ;*********obteniendo la separaci�n del refuerzo
  (setq ayudante1 1)
  (while (/= ayudante1 0)
    (cond ((= ayudante1 1)
	   (setq Sx (txtRefSep "" "DSep NdBarras" "/Dos separaciones /N�mero de barras"));txtRefSep es una funci�n personalizada, ver m�s arriba
	   	(cond ((= Sx "DSep") (setq ayudante1 2))
		      ((= Sx "NdBarras")(setq ayudante1 4))
		      (t (setq Sy Sx)(setq ayudante1 0))))
	  ((= ayudante1 2)
	   (setq Sy (txtRefSep "(Barras horizontales)" "NdBarras Volver" "/N�mero de barras /Volver"))
	   	(cond ((= Sy "NdBarras")(setq ayudante1 5))
		      ((= Sy "Volver")(setq ayudante1 1))		      
		      (t (setq ayudante1 3))))
	  ((= ayudante1 3)
	   (setq Sx (txtRefSep "(Barras verticales)" "NdBarras Volver" "/N�mero de barras /Volver"))
	   	(cond ((= Sx "NdBarras")(setq ayudante1 6))
		      ((= Sx "Volver")(setq ayudante1 2))		      
		      (t (setq ayudante1 0))))
	  ((= ayudante1 4)
	   (setq Sx (txtRefCant "" "Sep DCant" "/Separaci�n /Dos cantidades"));Sx obtendr� a priori el valor de la cantidad de elementos, con este valor deber� obtenerse la separaci�n de elementos
	   	(cond ((= Sx "Sep")(setq ayudante1 1))
		      ((= Sx "DCant")(setq ayudante1 5))		      
		      (t (setq Sy (Cant2Sep Sx (- B (/ (* r 2.00) 100.00))));Cant2Sep es una funci�n personalizada, ver m�s arriba
		         (setq Sx (Cant2Sep Sx (- A (/ (* r 2.00) 100.00))));aqui el valor de Sx se reemplaza por la separaci�n entre elementos
		       	 (setq ayudante1 0))));roundmin es funci�n personalizada ver operadores varios
	  ((= ayudante1 5)
	   (setq Sy (txtRefCant "(Barras horizontales)" "Sep Volver" "/Separaci�n /Volver"));Sy obtendr� a priori el valor de la cantidad de elementos, con este valor deber� obtenerse la separaci�n de elementos
	   	(cond ((= Sy "Sep")(setq ayudante1 2))
		      ((= Sy "Volver")(setq ayudante1 4))		      
		      (t (setq Sy (Cant2Sep Sy (- B (/ (* r 2.00) 100.00)))) (setq ayudante1 6))))
	  ((= ayudante1 6)
	   (setq Sx (txtRefCant "(Barras verticales)" "Sep Volver" "/Separaci�n /Volver"));Sx obtendr� a priori el valor de la cantidad de elementos, con este valor deber� obtenerse la separaci�n de elementos
	   	(cond ((= Sx "Sep")(setq ayudante1 3))
		      ((= Sx "Volver")(setq ayudante1 5))		      
		      (t (setq Sx (Cant2Sep Sx (- A (/ (* r 2.00) 100.00)))) (setq ayudante1 0))))))	  
  ;*********fin obteniendo la separaci�n del refuerzo
  
 

  ;configuraciones previas
  (setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que ten�a
  (setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  ;fin de configuraciones previas
  
  ;dibujando el borde de la zapata como un rect�ngulo
  (if   (null (tblobjname "LAYER" "C-ZAPATAS")) ;crear la capa "C-ZAPATAS" si no existe
	(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-ZAPATAS")'(70 . 0)'(62 . 240)'(370 . -3)))				
   )
  (command "_rectang" punto "dimensions" A B punto);dibuja el rectangulo en el punto de inserci�n con los lados especificados
  (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujado el rect�ngulo
  (setq entidad (subst '(8 . "C-ZAPATAS") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
  (entmod entidad);se actualiza el dibujo, para que los cambios surjan efecto
  ;fin de dibujo de borde de zapata
  
  ;dibuja las barras de acero
   (if   (null (tblobjname "LAYER" "As-ZAPATAS")) ;crear la capa "As-ZAPATAS" si no existe
	(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "As-ZAPATAS")'(70 . 0)'(62 . 136)'(370 . -3)))				
   )  
  (setq Nbarras_A (+ 1 (fix (/ (- A (/ (* r 2) 100)) Sx))));obtiene el n�mero de barras en la direcci�n A (longitud x)
  (setq Nbarras_B (+ 1 (fix (/ (- B (/ (* r 2) 100)) Sy))));obtiene el n�mero de barras en la direcci�n B (Ancho y)
  (setq d_A (/ (- A (* Sx (- Nbarras_A 1))) 2));obtiene la distancia del borde donde se dibujar� la primera barra en la direcci�n x
  (setq d_B (/ (- B (* Sy (- Nbarras_B 1))) 2));obtiene la distancia del borde donde se dibujar� la primera barra en la direcci�n y

  (setq punto_1x (list (+ (car punto) d_A) (+ (cadr punto) (/ r 100)) 0));coordenadas del punto inicial de la l�nea vertical
  (setq punto_2x (list (+ (car punto) d_A) (- (+ (cadr punto) B) (/ r 100)) 0));coordenadas del punto final de la l�nea vertical

  (While (> Nbarras_A 0);dibuja barras verticales
   	(command "_line" punto_1x punto_2x "")
        (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
        (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada la l�nea
 	(setq entidad (subst '(8 . "As-ZAPATAS") capa_ant entidad));sustituye la capa anterior por la nueva capa "As-ZAPATAS" creada
 	(entmod entidad);se actualiza el dibujo, para que los cambios surjan efecto
   	(setq punto_1x (list (+ (car punto_1x) Sx) (+ (cadr punto) (/ r 100)) 0));se determinan las coordenadas de la siguiente linea vertical
  	(setq punto_2x (list (+ (car punto_2x) Sx) (- (+ (cadr punto) B) (/ r 100)) 0))
    	(setq Nbarras_A (- Nbarras_A 1));se disminuye en 1 el n�mero de barras
   )
  
  (setq punto_1x (list (+ (car punto) (/ r 100)) (+ (cadr punto) d_B) 0));coordenadas del punto inicial de la l�nea horizontal
  (setq punto_2x (list (- (+ (car punto) A) (/ r 100)) (+ (cadr punto) d_B) 0));coordenadas del punto final de la l�nea horizontal

  (While (> Nbarras_B 0);dibuja barras horizontales
   	(command "_line" punto_1x punto_2x "")
        (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
        (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada la l�nea
 	(setq entidad (subst '(8 . "As-ZAPATAS") capa_ant entidad));sustituye la capa anterior por la nueva capa "As-ZAPATAS" creada
 	(entmod entidad);se actualiza el dibujo, para que los cambios surjan efecto
   	(setq punto_1x (list (+ (car punto) (/ r 100)) (+ (cadr punto_1x) Sy) 0));se determinan las coordenadas de la siguiente linea vertical
  	(setq punto_2x (list (- (+ (car punto) A) (/ r 100)) (+ (cadr punto_2x) Sy) 0))
    	(setq Nbarras_B (- Nbarras_B 1));se disminuye en 1 el n�mero de barras
   )
  ;fin de dibujo de barras de acero 

  ;dibujando las cotas a escala 1:25
     (if   (null (tblobjname "style" "texto_lisp")) ;crear estilo de texto para textos en general inc. las acotaciones
	(entmake (list '(0 . "style") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "texto_lisp") '(70 . 0) '(40 . 0.0)'(3 . "romans.shx")))
     )
     (if   (null (tblobjname "style" "texto_lisp_subt�tulo")) ;crear estilo de texto para subt�tulos
	(entmake (list '(0 . "style") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "texto_lisp_subt�tulo") '(70 . 0) '(40 . 0.0)'(3 . "romand.shx")))
     )
     (setq Text_est (tblobjname "style" "texto_lisp"));se guarda el nombre de la entidad de estilo de texto para usarla al definir el estilo de cota
  
     (if   (null (tblobjname "dimstyle" "cota_lisp_25")) ;crear estilo de cota
       ;40:dimscale, 140:tama�o de texto(es afectado por 40:dimscale), 271: presici�n de la cota, 41:tama�o de la flecha o s�mbolo usado para la cota
       ;44:tama�o de la linea exterior, 49:tama�o de la linea interior, 77:ubicaci�n del texto respecto a la linea de cota, 147:offset del texto, 290:determina si la linea interior sera de dimensi�n fija o variable
       ;73: alineaci�n con la l�nea de cota con 0 est� alineado con la linea de cota, 340: estilo de texto
	(entmake (list 	'(0 . "dimstyle")'(100 . "AcDbSymbolTableRecord") '(100 . "AcDbDimStyleTableRecord")'(2 . "cota_lisp_25")'(70 . 0)'(40 . 0.025)'(140 . 2.0)
			'(271 . 2)'(41 . 2.0)'(44 . 2.0)'(49 . 2.5)'(77 . 1)'(147 . 1.0)'(290 . 1)'(73 . 0)(cons 340 Text_est)))
			  				
     )
     (if   (null (tblobjname "LAYER" "C-Cota-Lisp")) ;crear la capa "C-Cota-Lisp" si no existe
	(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-Cota-Lisp")'(70 . 0)'(62 . 7)'(370 . -3)))				
     )
  
   (setq text_est (est_cota_actual));guarda el estilo de cota actual para retomarla al finalizar el acotamiento, la funci�n est_cota_actual es personalizada  
   (command "_-dimstyle" "restore" "cota_lisp_25");activa el estilo de cota creado
  ;para cota vertical

   (setq punto_1x punto); para la cota vertical el primer punto es el punto de inserci�n de la zapata
   (setq punto_2x (list (car punto) (+ (cadr punto) B) 0));el segundo punto
   (setq d_A (list (- (car punto) 0.1) (cadr punto) 0));offset
 
   (command "_dimlinear" punto_1x punto_2x d_A)
   (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
   (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada la cota
   (setq entidad (subst '(8 . "C-Cota-Lisp") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
   (entmod entidad)  
;para cota horizotal
   (setq punto_1x punto_2x); para la cota vertical el primer punto es el punto de inserci�n de la zapata
   (setq punto_2x (list (+ (car punto_1x) A) (cadr punto_1x) 0));el segundo punto
   (setq d_B (list (car punto_1x) (+ (cadr punto_1x) 0.1) 0));offset
 
   (command "_dimlinear" punto_1x punto_2x d_B)
   (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
   (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada la cota
   (setq entidad (subst '(8 . "C-Cota-Lisp") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
   (entmod entidad) 
   (command "_-dimstyle" "restore" text_est);vuelve al estilo de cota con el que estaba el dibujo
;fin de dibujo de cota

;insertando el texto de descripci�n del acero
     (if   (null (tblobjname "LAYER" "C-Texto-Lisp")) ;crear la capa "C-Texto-Lisp" si no existe
	(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-Texto-Lisp")'(70 . 0)'(62 . 7)'(370 . -3)))				
     )
     (if   (null (tblobjname "LAYER" "C-Texto-Lisp-sub")) ;crear la capa "C-Texto-Lisp-sub" si no existe
	(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-Texto-Lisp-sub")'(70 . 0)'(62 . 3)'(370 . -3)))				
     )
   (setq text_est (est_texto_actual));guarda el estilo de texto actual para retomarla al finalizar la inserci�n de textos, la funci�n est_texto_actual es personalizada  
   (command "_-style" "Texto_lisp" "" "" "" "" "" "" "");activa el estilo de texto creado

  (setq Nbarras_A (+ 1 (fix (/ (- A (/ (* r 2) 100)) Sx))));obtiene el n�mero de barras en la direcci�n A (longitud x)
  (setq Nbarras_B (+ 1 (fix (/ (- B (/ (* r 2) 100)) Sy))));obtiene el n�mero de barras en la direcci�n B (Ancho y)
  (setq d_A (/ (- A (* Sx (- Nbarras_A 1))) 2));obtiene la distancia del borde donde se dibujar� la primera barra en la direcci�n x
  (setq d_B (/ (- B (* Sy (- Nbarras_B 1))) 2));obtiene la distancia del borde donde se dibujar� la primera barra en la direcci�n y
  ;texto vertical
  (setq punto_1x (list (+ (car punto) d_A 0.01) (+ (cadr punto) d_B Sx 0.01) 0));punto de inserci�n
  (setq punto_2x (list (+ (car punto) d_A 0.01) (+ (cadr punto) d_B Sx B 0.01) 0))
  (command "_mtext" punto_1x "rotation" "90" punto_2x (strcat (rtos Nbarras_A 2 0) " %%c" Dy "@ " (rtos Sx 2 2)) "")
  (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada el texto
  (setq entidad (subst '(8 . "C-Texto-Lisp") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
  (setq capa_ant (assoc 40 entidad));guarda el tama�o en la que fue dibujada el texto
  (setq entidad (subst '(40 . 0.05) capa_ant entidad));sustituye el tama�o anterior 
  (entmod entidad)

    ;texto horizontal
  (setq punto_1x (list (+ (car punto) d_A Sy 0.01) (+ (cadr punto) d_B 0.07) 0));punto de inserci�n
  (setq punto_2x (list (+ (car punto) d_A Sy A 0.01) (+ (cadr punto) d_B 0.07) 0))
  (command "_mtext" punto_1x "rotation" "0" punto_2x (strcat (rtos Nbarras_B 2 0) " %%c" Dx "@ " (rtos Sy 2 2)) "")
  (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada el texto
  (setq entidad (subst '(8 . "C-Texto-Lisp") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
  (setq capa_ant (assoc 40 entidad));guarda el tama�o en la que fue dibujada el texto
  (setq entidad (subst '(40 . 0.05) capa_ant entidad));sustituye el tama�o anterior 
  (entmod entidad)
  
   ;texto de escala
  (setq punto_1x (list (- (+ (car punto) A) 0.32) (- (cadr punto) 0.20) 0));punto de inserci�n
  (setq punto_2x (list (+ (car punto) A) (- (cadr punto) 0.20) 0))
  (command "_mtext" punto_1x "rotation" "0" punto_2x "Esc:1/25" "")
  (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada el texto
  (setq entidad (subst '(8 . "C-Texto-Lisp") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
  (setq capa_ant (assoc 40 entidad));guarda el tama�o en la que fue dibujada el texto
  (setq entidad (subst '(40 . 0.05) capa_ant entidad));sustituye el tama�o anterior 
  (entmod entidad)
  (command "_-style" text_est "" "" "" "" "" "" "");vuelve al estilo de texto actual antes de ejecutado el comando
 ;SUBT�TULO
  (setq text_est (est_texto_actual));guarda el estilo de texto actual para retomarla al finalizar la inserci�n de textos, la funci�n est_texto_actual es personalizada  
  (command "_-style" "Texto_lisp_subt�tulo" "" "" "" "" "" "" "");activa el estilo de texto creado
  
  (setq punto_1x (list (- (+ (car punto) A) 0.59) (- (cadr punto) 0.065) 0));punto de inserci�n
  (setq punto_2x (list (+ (car punto) A 0.2) (- (cadr punto) 0.065) 0))
  (command "_mtext" punto_1x "rotation" "0" punto_2x "{\\LZAPATA 01}" "")
  (setq entidad (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  (setq capa_ant (assoc 8 entidad));guarda la capa en la que fue dibujada el texto
  (setq entidad (subst '(8 . "C-Texto-Lisp-sub") capa_ant entidad));sustituye la capa anterior por la nueva capa creada
  (setq capa_ant (assoc 40 entidad));guarda el tama�o en la que fue dibujada el texto
  (setq entidad (subst '(40 . 0.075) capa_ant entidad));sustituye el tama�o anterior 
  (entmod entidad)
  (command "_-style" text_est "" "" "" "" "" "" "");vuelve al estilo de texto actual antes de ejecutado el comando  
  
;fin de inserci�n de texto  

  (setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que ten�a antes de iniciar el programa
  (PRINC)
)


;************************************************************************************************************************************
;*************************************************FIN funci�n principal)**********************************************************
;************************************************************************************************************************************