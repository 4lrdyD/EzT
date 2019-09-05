








;****************************************************************************************************************************************************************
;****************************************************************************************************************************************************************
;***********************************************************FUNCIÓN PARA DIBUJAR ESTRIBOS EN UNA DIRECCIÓN****************************************************
(defun Estribos1d (punto1l punto2l destribo unitario1 unitario2 diam / ayudante1 ayudante2 ayudante3 ayudante4 ayudante5 ayudante6 ayudante7 ayudante8 ayudante9 distanciap relavs texto nestribos nestribosp cantidad separacion osnap_ punto1 punto2)

  ;****DIBUJANDO ESTRIBOS****
  (setq ayudante1 punto1l);usaremos este valor para guardar el punto central de cada estribo en la dirección punto1l punto2l
  (setq ayudante2 punto2l);usaremos este valor para guardar el punto central de cada estribo en la dirección punto2l punto1l
  (setq ayudante3 0);usaremos este valor para terminar con la rutina principal si cambia de valor
  (setq ayudante4 0);usaremos este valor para acumular la distancia según se dibujan los estribos
  (setq ayudante5 0);usaremos estos valores (ayudante5 y ayudante6) para terminar con las subrutinas si cambian de valor
  (setq ayudante6 0)
  (setq ayudante7 0);usaremos este valor para manejar el tipo de entrada por número de estribos o por distancia parcial
  (setq ayudante8 0);usaremos este valor para manejar el tipo de entrada por separación o por relación Av/s
  (setq ayudante9 0); usaremos este valor para terminar con las subrutinas si cambian de valor
  (setq texto "");se guardará la distribución de estribos como texto, para insertarlo si se desea
  (setq nestribos 0); contará la cantidad de estribos que se esten dibujando
  (setq nestribosp 0); contará la cantidad de estribos que se esten dibujando, empezará desde cero cada que cambie la separación

  (while (= ayudante3 0)
    (while (= ayudante5 0)
      (cond ((= ayudante7 0)
	     (initget 7 "Restante Distanciap")(setq cantidad (getint (strcat "-Distancia cubierta: " (rtos ayudante4 2 2) "\n-Distancia máxima: " (rtos (- (distance punto1l punto2l) 0.04) 2 2) "\n Ingrese cantidad de estribos: [Restante /Distancia parcial] ")))
	     (cond ((= cantidad "Distanciap")
		    (setq ayudante7 1))
		   (t (while (= ayudante6 0)
			(cond ((= ayudante8 0)
			       (initget 7 "Relavs")(setq separacion (getreal "\n Separación (m): [/Relación Av-s]"))
			       (cond ((= separacion "Relavs")
				      (setq ayudante8 1))
				     (t (setq ayudante6 1))))
			      (t (initget 7 "Separacion")(setq relavs (getreal "\n Relación Av/s (cm): [/Separación]"))
			       (cond ((= relavs "Separacion")
				      (setq ayudante8 0))
				     (t (setq separacion (dcr2s diam relavs))
				      (setq ayudante6 1))))))
		    (cond ((= cantidad "Restante")
			   (setq distanciap "Restante")
			   (setq ayudante5 1))
			  (t (cond ((>= (- (distance punto1l punto2l) 0.04) (+ ayudante4 (* cantidad separacion)))
				    (setq distanciap (+ ayudante4 (* cantidad separacion)))
				    (setq ayudante5 1))
				   (t (setq cantidad (fix (/ (- (distance ayudante1 ayudante2) 0.04) separacion)))
				    (cond ((= cantidad 0)
					   (setq distanciap "Restante")
					   (setq ayudante5 1))
					  (t (initget "Terminar")(getkword (strcat "\n No es posible terminar de dibujar la última cantidad de estribos ingresada, se dibujarán " (rtos cantidad 2 0) " estribos : [Terminar] "))
					   (setq distanciap "Restante")
					   (setq ayudante5 1))))))))))
	    (t (initget 7 "Restante Cantidad")(setq distanciap (getreal (strcat "-Distancia cubierta: " (rtos ayudante4 2 2) "\n-Distancia máxima: " (rtos (- (distance punto1l punto2l) 0.04) 2 2) "\n Ingrese distancia parcial: [Restante /Cantidad de estribos] ")))
	     (cond ((= distanciap "Cantidad")
		    (setq ayudante7 0))
		   (t (while (= ayudante9 0)
			(cond ((= distanciap "Restante")
			       (setq ayudante9 1))
			      ((and (>= (- (distance punto1l punto2l) 0.04) distanciap) (> distanciap ayudante4))
			       (setq ayudante9 1))
			      (t (initget 7 "Restante Cantidad")(setq distanciap (getreal (strcat "Debe cumplirse: " (rtos (- (distance punto1l punto2l) 0.04) 2 2)" \U+2265 (distancia parcial) > "(rtos ayudante4 2 2) "\n Ingrese distancia parcial (m): [Restante /Cantidad de estribos] ")))
			       (if (= distanciap "Cantidad")
				 (setq ayudante9 1)))))
		    (cond ((= distanciap "Cantidad")
			   (setq ayudante9 0)
			   (setq ayudante7 0))
			  (t (while (= ayudante6 0)
				(cond ((= ayudante8 0)
			       	  	(initget 7 "Relavs")(setq separacion (getreal "\n Separación (m): [/Relación Av-s]"))
			          	(cond ((= separacion "Relavs")
				      		(setq ayudante8 1))
				     	      (t (setq ayudante6 1))))
			      	      (t (initget 7 "Separacion")(setq relavs (getreal "\n Relación Av/s (cm): [/Separación]"))
			       		(cond ((= relavs "Separacion")
				      		(setq ayudante8 0))
				     	      (t (setq separacion (dcr2s diam relavs))
				      		(setq ayudante6 1))))))
			   (setq ayudante5 1))))))))
    (setq ayudante5 0)
    (setq ayudante6 0)
    (setq ayudante9 0)
    
    (cond ((= distanciap "Restante")
	  		(setq cantidad (fix (/ (- (distance ayudante1 ayudante2) 0.04) separacion)))
	       		(cond ((= cantidad 0)
			       		(initget "Terminar")(getkword "\n No es posible dibujar más estribos: [Terminar] " )
			       		(setq ayudante3 1))
			      (t (while (> cantidad 0)
				   (setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  			   (setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                   (setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                   (setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  		                   (setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  		                   (command "_line" punto1 punto2 "")
  		                   (setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
		                   (setq nestribos (+ nestribos 1))
	  	                   (setq nestribosp (+ nestribosp 1))
	  	                   (setq ayudante4 (+ ayudante4 separacion))
	  	                   (setq cantidad (- cantidad 1)))
    				(setq texto (strcat texto (rtos nestribosp 2 0) "@" (rtos separacion 2 2) ", "))
    				(setq nestribosp 0) 
			        (setq ayudante3 1))))
	      (t (setq cantidad (fix (/ (- distanciap ayudante4) separacion)))
	       (if (/= (+ ayudante4 (* cantidad separacion)) distanciap )
		 (setq cantidad (+ cantidad 1)))
	       (cond ((>= (- (distance punto1l punto2l) 0.04) (+ ayudante4 (* cantidad separacion)))
				(while (> cantidad 0)
				   (setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  			   (setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                   (setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                   (setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  		                   (setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  		                   (command "_line" punto1 punto2 "")
  		                   (setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
		                   (setq nestribos (+ nestribos 1))
	  	                   (setq nestribosp (+ nestribosp 1))
	  	                   (setq ayudante4 (+ ayudante4 separacion))
	  	                   (setq cantidad (- cantidad 1)))
    				(setq texto (strcat texto (rtos nestribosp 2 0) "@" (rtos separacion 2 2) ", "))
    				(setq nestribosp 0)				
				(setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 (* cantidad separacion)))))
				(setq ayudante4 (+ ayudante4 (* cantidad separacion))))
		       (t (setq cantidad (- cantidad 1))
			  (cond ((= cantidad 0)
			       		(initget "Terminar")(getkword "\n No es posible dibujar más estribos: [Terminar] " )
			       		(setq ayudante3 1))
			      	(t (while (> cantidad 0)
				     (setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  			     (setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                     (setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  	                     (setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  		                     (setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  		                     (command "_line" punto1 punto2 "")
  		                     (setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
		                     (setq nestribos (+ nestribos 1))
	  	                     (setq nestribosp (+ nestribosp 1))
	  	                     (setq ayudante4 (+ ayudante4 separacion))
	  	                     (setq cantidad (- cantidad 1)))
    				  (setq texto (strcat texto (rtos nestribosp 2 0) "@" (rtos separacion 2 2) ", "))
    				  (setq nestribosp 0)
			          (setq ayudante3 1))))))))
  
  ;determinando el sentido de la distribución, solo para el texto E/I Extremo izquierdo, E/D Extremo derecho, E/I extremo inferior, E/S extremo superior
  (cond ((or (and (>= (vang (vdif punto2l punto1l)) 0) (< (vang (vdif punto2l punto1l)) 135)) (and (>= (vang (vdif punto2l punto1l)) 315) (< (vang (vdif punto2l punto1l)) 360))); condición para E/I, el ángulo debe pertenecer a [0, 135> U [315, 360> 
	 (setq ayudante3 " E/I"))
	((and (>= (vang (vdif punto2l punto1l)) 135) (<= (vang (vdif punto2l punto1l)) 225)) ; condición para E/D, el ángulo debe pertenecer a [135, 225] 
	 (setq ayudante3 " E/D"))
	((and (> (vang (vdif punto2l punto1l)) 225) (< (vang (vdif punto2l punto1l)) 315)) ; condición para E/S, el ángulo debe pertenecer a <225, 315> 
	 (setq ayudante3 " E/S")))
  (if (= (read (substr texto 1 1)) nestribos)
    	(setq texto (strcat "\U+EC75 " (rtos nestribos 2 0) "%%c" diam "@" (rtos separacion 2 2) ayudante3))
    	(setq texto (strcat "\U+EC75 " (rtos nestribos 2 0) "%%c" diam ": " texto ayudante3)))

  
  ;fin DIBUJANDO ESTRIBOS****
  
  ;insertando texto de distribución de estribos******************
   (initget "Si No" )(setq ayudante1 (getkword "\n Insertar texto de distribución? [Si/No]: "));pregunta si se insertará la descripción
  	(cond ((= ayudante1 \n)(setq ayudante1 "Si"));si se presiona enter se asume que es Si
   )
  
   (cond ((= ayudante1 "Si")
   	 	(if   (null (tblobjname "style" "texto_lisp")) ;crear estilo de texto para textos
          	(entmake (list '(0 . "style") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "texto_lisp") '(70 . 0) '(40 . 0.0)'(3 . "romans.shx")))
   		)
  
   		(if   (null (tblobjname "LAYER" "C-Texto-Lisp")) ;crear la capa "C-Texto-Lisp" si no existe
		(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-Texto-Lisp")'(70 . 0)'(62 . 7)'(370 . -3)))				
    		)
	  	(setq ayudante3 (est_texto_actual));guarda el estilo de texto actual para retomarla al finalizar la inserción de textos, la función est_texto_actual es personalizada, ver lisp de zapatas  
   		(command "_-style" "Texto_lisp" "" "" "" "" "" "" "");activa el estilo de texto creado
	  	(initget "Default" )(setq punto1 (getpoint "\n Punto de inserción [por Defecto]: "));pide el punto de inserción
	  	(cond ((= punto1 \n)(setq punto1 "Default")));si se presiona enter se asume que es el por defecto
	  	(initget "Default" )(setq ayudante1 (getreal "\n Rotación [por Defecto]: "));pide la rotación del texto
	  	(cond ((= ayudante1 \n)(setq ayudante1 "Default")));si se presiona enter se asume que es el por defecto
	  	(cond ((= punto1 "Default")
		       (setq punto1 (vdif punto1l (vprod unitario2 (+ (/ destribo 2) 0.14))))
		       (setq ayudante4 "MC"); para la justificación del texto MC= medio central
		       )
		      (t (setq ayudante4 "TL"); para la justificación del texto TL= top left-superior izquierdo
		      ) 
		)
	  	(cond ((= ayudante1 "Default")
		       (setq ayudante1 (Vang (Vdif punto2l punto1l)))	       
		       )
		)
	  	(setq punto2 (vsum punto1 (vprod (list (cosg ayudante1) (seng ayudante1) 0) (distance punto1l punto2l))))
		(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  		(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  	(command "_mtext" punto1 "rotation" ayudante1 "justify" ayudante4 punto2 texto "")
	  	(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
  		(setq ayudante1 (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  		(setq ayudante2 (assoc 8 ayudante1));guarda la capa en la que fue dibujada el texto
  		(setq ayudante1 (subst '(8 . "C-Texto-Lisp") ayudante2 ayudante1));sustituye la capa anterior por la nueva capa creada
  		(setq ayudante2 (assoc 40 ayudante1));guarda el tamaño en la que fue dibujado el texto
 		(setq ayudante1 (subst '(40 . 0.05) ayudante2 ayudante1));sustituye el tamaño anterior 
  		(entmod ayudante1);se actualiza la entidad con las últimas propiedades asignadas
	  	(command "_-style" ayudante3 "" "" "" "" "" "" "");vuelve al estilo de texto actual antes de ejecutado el comando
	)
   )
    ;corrigiendo la rotación del texto*******
  	(setq ayudante1 (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada, la última entidad dibujada será el texto siempre y cuando se haya elegido insertarlo
  	(setq ayudante2 (cdr (assoc 0 ayudante1)));guarda el tipo de elemento de la última entidad dibujada, si insertamos el texto será del tipo "MTEXT"
  	(if (= ayudante2 "MTEXT")
    		(cond ((and (> (cdr (assoc 50 ayudante1)) (radianes 90)) (<= (cdr (assoc 50 ayudante1))  (radianes 270)));verifica si la rotación está entre 90 y 270 con ésos ángulos la rotación debe corregirse
			(setq ayudante3 (cdr (assoc 50 ayudante1)))
			(setq ayudante1 (subst (cons 50 (+ ayudante3 pi))(cons 50 ayudante3) ayudante1));sustituye la capa anterior por la nueva capa creada
  			(entmod ayudante1))));se actualiza la entidad con las últimas propiedades asignadas
  
  ;fin de inserción de texto de distribución de estribos*********
)
;*************************************************FIN DE FUNCIÓN: DIBUJANDO ESTRIBOS EN UNA DIRECCIÓN************************************************************
;****************************************************************************************************************************************************************
;****************************************************************************************************************************************************************








;****************************************************************************************************************************************************************
;****************************************************************************************************************************************************************
;*************************************************FUNCIÓN: DIBUJANDO ESTRIBOS EN DOS DIRECCIONES*****************************************************************
(defun Estribos2d (punto1l punto2l destribo unitario1 unitario2 diam / ayudante1 ayudante2 ayudante3 ayudante4 ayudante5 ayudante6 ayudante7 ayudante8 ayudante9 relavs distanciap texto nestribos nestribosp cantidad separacion osnap_ punto1 punto2)

  ;****DIBUJANDO ESTRIBOS****
  (setq ayudante1 punto1l);usaremos este valor para guardar el punto central de cada estribo en la dirección punto1l punto2l
  (setq ayudante2 punto2l);usaremos este valor para guardar el punto central de cada estribo en la dirección punto2l punto1l
  (setq ayudante3 0);usaremos este valor para terminar con la rutina principal si cambia de valor
  (setq ayudante4 0);usaremos este valor para acumular la distancia según se dibujan los estribos
  (setq ayudante5 0);usaremos estos valores (ayudante5 y ayudante6) para terminar con las subrutinas si cambian de valor, también se usa ayudante 5 para guardar conjuntos de selección
  (setq ayudante6 0)
  (setq ayudante7 0);usaremos este valor para manejar el tipo de entrada por número de estribos o por distancia parcial
  (setq ayudante8 0);usaremos este valor para manejar el tipo de entrada por separación o por relación Av/s
  (setq ayudante9 0); usaremos este valor para terminar con las subrutinas si cambian de valor
  (setq texto "");se guardará la distribución de estribos como texto, para insertarlo si se desea
  (setq nestribos 0); contará la cantidad de estribos que se esten dibujando
  (setq nestribosp 0); contará la cantidad de estribos que se esten dibujando, empezará desde cero cada que cambie la separación

  (while (= ayudante3 0)
    (while (= ayudante5 0)
      (cond ((= ayudante7 0)
	     (initget 7 "Restante Distanciap")(setq cantidad (getint (strcat "-Distancia cubierta: " (rtos ayudante4 2 2) "\n-Distancia máxima: " (rtos (/ (distance punto1l punto2l) 2) 2 2) "\n Ingrese cantidad de estribos: [Restante /Distancia parcial] ")))
	     (cond ((= cantidad "Distanciap")
		    (setq ayudante7 1))
		   (t (while (= ayudante6 0)
			(cond ((= ayudante8 0)
			       (initget 7 "Relavs")(setq separacion (getreal "\n Separación (m): [/Relación Av-s]"))
			       (cond ((= separacion "Relavs")
				      (setq ayudante8 1))
				     (t (setq ayudante6 1))))
			      (t (initget 7 "Separacion")(setq relavs (getreal "\n Relación Av/s (cm): [/Separación]"))
			       (cond ((= relavs "Separacion")
				      (setq ayudante8 0))
				     (t (setq separacion (dcr2s diam relavs))
				      (setq ayudante6 1))))))
		    (cond ((= cantidad "Restante")
			   (setq distanciap "Restante")
			   (setq ayudante5 1))
			  (t (cond ((>= (/ (distance punto1l punto2l) 2) (+ ayudante4 (* cantidad separacion)))
				    (setq distanciap (+ ayudante4 (* cantidad separacion)))
				    (setq ayudante5 1))
				   (t (setq cantidad (fix (/ (/ (distance ayudante1 ayudante2) 2) separacion)))
				    (cond ((= cantidad 0)
					   (setq distanciap "Restante")
					   (setq ayudante5 1))
					  (t (initget "Terminar")(getkword (strcat "\n No es posible terminar de dibujar la última cantidad de estribos ingresada, se dibujarán máximo " (rtos cantidad 2 0) " estribos : [Terminar] "))
					   (setq distanciap "Restante")
					   (setq ayudante5 1))))))))))
	    (t (initget 7 "Restante Cantidad")(setq distanciap (getreal (strcat "-Distancia cubierta: " (rtos ayudante4 2 2) "\n-Distancia máxima: " (rtos (/ (distance punto1l punto2l) 2) 2 2) "\n Ingrese distancia parcial: [Restante /Cantidad de estribos] ")))
	     (cond ((= distanciap "Cantidad")
		    (setq ayudante7 0))
		   (t (while (= ayudante9 0)
			(cond ((= distanciap "Restante")
			       (setq ayudante9 1))
			      ((and (>= (/ (distance punto1l punto2l) 2) distanciap) (> distanciap ayudante4))
			       (setq ayudante9 1))
			      (t (initget 7 "Restante Cantidad")(setq distanciap (getreal (strcat "Debe cumplirse: " (rtos (/ (distance punto1l punto2l) 2) 2 2)" \U+2265 (distancia parcial) > "(rtos ayudante4 2 2) "\n Ingrese distancia parcial (m): [Restante /Cantidad de estribos] ")))
			       (if (= distanciap "Cantidad")
				 (setq ayudante9 1)))))
		    (cond ((= distanciap "Cantidad")
			   (setq ayudante9 0)
			   (setq ayudante7 0))
			  (t (while (= ayudante6 0)
				(cond ((= ayudante8 0)
			       	  	(initget 7 "Relavs")(setq separacion (getreal "\n Separación (m): [/Relación Av-s]"))
			          	(cond ((= separacion "Relavs")
				      		(setq ayudante8 1))
				     	      (t (setq ayudante6 1))))
			      	      (t (initget 7 "Separacion")(setq relavs (getreal "\n Relación Av/s (cm): [/Separación]"))
			       		(cond ((= relavs "Separacion")
				      		(setq ayudante8 0))
				     	      (t (setq separacion (dcr2s diam relavs))
				      		(setq ayudante6 1))))))
			   (setq ayudante5 1))))))))
    (setq ayudante5 0)
    (setq ayudante6 0)
    (setq ayudante9 0)

    	(cond ((= distanciap "Restante")
	  		(setq cantidad (fix (/ (/ (distance ayudante1 ayudante2) 2) separacion)))
	       		(cond ((= cantidad 0)
			       		(cond ((> (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0)))
			       			(setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", "))
    						(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 					(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
						(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 					(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  						(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  					(command "_line" punto1 punto2 "")
	  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
	 					(setq nestribos (+ nestribos 1))
			       			(setq ayudante3 1))
					      (t (setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", "))
					       	(setq ayudante3 1))))
			      (t (while (> cantidad 0)
					(setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
					(setq ayudante2 (vdif punto2l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
	  				(setq nestribos (+ nestribos 2))
	 			 	(setq nestribosp (+ nestribosp 1))
	  				(setq ayudante4 (+ ayudante4 separacion))
	  				(setq cantidad (- cantidad 1)))
			       
				 (cond ((> (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0)))
    					(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
					(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  				(command "_line" punto1 punto2 "")
	  				(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
	 				(setq nestribos (+ nestribos 1))
			       		(setq ayudante3 1))
				       ((and (<= (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0))) (> (distance ayudante1 ayudante2) (* separacion (/ 2.0 3.0))))
					(setq ayudante3 1))
				       (t (setq nestribos (- nestribos 1))
					(command "_erase" "L" "");borrará la última entidad dibujada
					(command "_erase" "L" "");borrará la última entidad dibujada, se borran 2 en total
    					(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
					(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  				(command "_line" punto1 punto2 "")
	  				(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
			       		(setq ayudante3 1)))
			       	(setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", ")))))
			       
	      (t (setq cantidad (fix (/ (- distanciap ayudante4) separacion)))
	       (if (/= (+ ayudante4 (* cantidad separacion)) distanciap )
		 (setq cantidad (+ cantidad 1)))
	       (cond ((> (/ (distance punto1l punto2l) 2) (+ ayudante4 (* cantidad separacion)))
                                (while (> cantidad 0)
					(setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
					(setq ayudante2 (vdif punto2l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
	  				(setq nestribos (+ nestribos 2))
	 			 	(setq nestribosp (+ nestribosp 1))
	  				(setq ayudante4 (+ ayudante4 separacion))
	  				(setq cantidad (- cantidad 1)))
		                 (setq texto (strcat texto (rtos nestribosp 2 0) "@" (rtos separacion 2 2) ", "))
		                 (setq nestribosp 0))
		     (t (setq cantidad (- cantidad 1))
			  (cond ((= cantidad 0)
			       		(cond ((> (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0)))
			       			(setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", "))
    						(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 					(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
						(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 					(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  						(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  					(command "_line" punto1 punto2 "")
	  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
	 					(setq nestribos (+ nestribos 1))
			       			(setq ayudante3 1))
					      (t (setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", "))
					       	(setq ayudante3 1))))

				(t (while (> cantidad 0)
					(setq ayudante1 (vsum punto1l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
					(setq ayudante2 (vdif punto2l (vprod unitario1 (+ ayudante4 separacion))))
	  				(setq punto1 (vsum ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq punto2 (vdif ayudante2 (vprod unitario2 (/ destribo 2))))
	  				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
  					(command "_line" punto1 punto2 "")
  					(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar el dibujo de los estribos		       
	  				(setq nestribos (+ nestribos 2))
	 			 	(setq nestribosp (+ nestribosp 1))
	  				(setq ayudante4 (+ ayudante4 separacion))
	  				(setq cantidad (- cantidad 1)))
			       
				 (cond ((> (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0)))
    					(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
					(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  				(command "_line" punto1 punto2 "")
	  				(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
	 				(setq nestribos (+ nestribos 1))
			       		(setq ayudante3 1))
				       ((and (<= (distance ayudante1 ayudante2) (* separacion (/ 4.0 3.0))) (> (distance ayudante1 ayudante2) (* separacion (/ 2.0 3.0))))
					(setq ayudante3 1))
				       (t (setq nestribos (- nestribos 1))
					(command "_erase" "L" "");borrará la última entidad dibujada
					(command "_erase" "L" "");borrará la última entidad dibujada, se borran 2 en total
    					(setq ayudante1 (vavg punto1l punto2l));se dibujará estribo central
	 				(setq punto1 (vsum ayudante1 (vprod unitario2 (/ destribo 2))))
					(setq punto2 (vdif ayudante1 (vprod unitario2 (/ destribo 2))))
	 				(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  					(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  				(command "_line" punto1 punto2 "")
	  				(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
			       		(setq ayudante3 1)))
			       	(setq texto (strcat texto "Rto" "@" (rtos separacion 2 2) ", ")))))))))	

  (if (= (substr texto 1 1) "R")
    	(setq texto (strcat "\U+EC75 " (rtos nestribos 2 0) "%%c" diam "@" (rtos separacion 2 2) " C/E"))
    	(setq texto (strcat "\U+EC75 " (rtos nestribos 2 0) "%%c" diam ": " texto " C/E")))
	 
   ;fin DIBUJANDO ESTRIBOS****
  ;insertando texto de distribución de estribos******************
   (initget "Si No" )(setq ayudante1 (getkword "\n Insertar texto de distribución? [Si/No]: "));pregunta si se insertará la descripción
  	(cond ((= ayudante1 \n)(setq ayudante1 "Si"));si se presiona enter se asume que es Si
   )
  
   (cond ((= ayudante1 "Si")
   	 	(if   (null (tblobjname "style" "texto_lisp")) ;crear estilo de texto para textos
          	(entmake (list '(0 . "style") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "texto_lisp") '(70 . 0) '(40 . 0.0)'(3 . "romans.shx")))
   		)
  
   		(if   (null (tblobjname "LAYER" "C-Texto-Lisp")) ;crear la capa "C-Texto-Lisp" si no existe
		(entmake (list 	'(0 . "LAYER")'(100 . "AcDbSymbolTableRecord")'(100 . "AcDbLayerTableRecord")'(2 . "C-Texto-Lisp")'(70 . 0)'(62 . 7)'(370 . -3)))				
    		)
	  	(setq ayudante3 (est_texto_actual));guarda el estilo de texto actual para retomarla al finalizar la inserción de textos, la función est_texto_actual es personalizada, ver lisp de zapatas  
   		(command "_-style" "Texto_lisp" "" "" "" "" "" "" "");activa el estilo de texto creado
	  	(initget "Default" )(setq punto1 (getpoint "\n Punto de inserción [por Defecto]: "));pide el punto de inserción
	  	(cond ((= punto1 \n)(setq punto1 "Default")));si se presiona enter se asume que es el por defecto
	  	(initget "Default" )(setq ayudante1 (getreal "\n Rotación [por Defecto]: "));pide la rotación del texto
	  	(cond ((= ayudante1 \n)(setq ayudante1 "Default")));si se presiona enter se asume que es el por defecto
	  	(cond ((= punto1 "Default")
		       (setq punto1 (vdif punto1l (vprod unitario2 (+ (/ destribo 2) 0.14))))
		       (setq ayudante4 "MC"); para la justificación del texto MC= medio central
		       )
		      (t (setq ayudante4 "TL"); para la justificación del texto TL= top left-superior izquierdo
		      ) 
		)
	  	(cond ((= ayudante1 "Default")
		       (setq ayudante1 (Vang (Vdif punto2l punto1l)))	       
		       )
		)
	  	(setq punto2 (vsum punto1 (vprod (list (cosg ayudante1) (seng ayudante1) 0) (distance punto1l punto2l))))
		(setq osnap_ (getvar "osmode"));aqui se guarda el valor de osnap para que al final se regrese al valor que tenía
  		(setvar "osmode" 0)	;debe desactivarse el Osnap para evitar errores en el dibujo.
	  	(command "_mtext" punto1 "rotation" ayudante1 "justify" ayudante4 punto2 texto "")
	  	(setvar "osmode" osnap_) ;se devuelve el Osnap (Osmode) al valor que tenía antes de iniciar la inserción del texto
  		(setq ayudante1 (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada
  		(setq ayudante2 (assoc 8 ayudante1));guarda la capa en la que fue dibujada el texto
  		(setq ayudante1 (subst '(8 . "C-Texto-Lisp") ayudante2 ayudante1));sustituye la capa anterior por la nueva capa creada
  		(setq ayudante2 (assoc 40 ayudante1));guarda el tamaño en la que fue dibujado el texto
 		(setq ayudante1 (subst '(40 . 0.05) ayudante2 ayudante1));sustituye el tamaño anterior 
  		(entmod ayudante1);se actualiza la entidad con las últimas propiedades asignadas
	  	(command "_-style" ayudante3 "" "" "" "" "" "" "");vuelve al estilo de texto actual antes de ejecutado el comando
	)
   )
  ;corrigiendo la rotación del texto*******
  	(setq ayudante1 (entget (ssname (ssget "L") 0)));se obtiene la lista de la entidad dibujada, la última entidad dibujada será el texto siempre y cuando se haya elegido insertarlo
  	(setq ayudante2 (cdr (assoc 0 ayudante1)));guarda el tipo de elemento de la última entidad dibujada, si insertamos el texto será del tipo "MTEXT"
  	(if (= ayudante2 "MTEXT")
    		(cond ((and (> (cdr (assoc 50 ayudante1)) (radianes 90)) (<= (cdr (assoc 50 ayudante1))  (radianes 270)));verifica si la rotación está entre 90 y 270 con ésos ángulos la rotación debe corregirse
			(setq ayudante3 (cdr (assoc 50 ayudante1)))
			(setq ayudante1 (subst (cons 50 (+ ayudante3 pi))(cons 50 ayudante3) ayudante1));sustituye la capa anterior por la nueva capa creada
  			(entmod ayudante1))));se actualiza la entidad con las últimas propiedades asignadas
  
  ;fin de inserción de texto de distribución de estribos*********
)

;*************************************************FIN DE FUNCIÓN: DIBUJANDO ESTRIBOS EN DOS DIRECCIONES*****************************************************************
;****************************************************************************************************************************************************************
;****************************************************************************************************************************************************************








;****************************************************************************************************************************************************************
;****************************************************************************************************************************************************************
;*************************************************FUNCIÓN PRINCIPAL***********************************************************************************************
(defun c:Estribos (/ D punto1L punto2L punto1T punto2T diam unitario1 ayudante1 ayudante2 ayudante3 ayudante4 unitario2 pmedioestribo punto1 punto2 cantidad separación)
	
  (initget "Uno Dos" )(setq D (getkword "\n Tipo de distribución [Un sentido/Dos sentidos]: "));pregunta si los estribos serán dibujados en un solo sentido o en dos sentidos (típico)
  	(cond ((= D \n)(setq D "Dos")))
	
  (setq punto1L (getpoint "\n Extremo de referencia inicial: "))
  (setq punto2L (getpoint "\n Extremo de referencia final: "))
  (setq punto2T (getpoint "\n  Ancho de estribo punto final: " (setq punto1T (getpoint "\n Ancho de estribo punto inicial: "))))
  
  (initget "8mm 1-4'' 3-8'' 1-2'' 5-8'' 3-4'' 7-8'' 1''" )(setq diam (getkword "\n Diámetro del estribo [1-4''/8mm/3-8''/1-2''/5-8''/3-4''/7-8''/1'']: "))
  	(cond ((= diam \n)(setq diam "3/8''"))
		((= diam "1-4''")(setq diam "1/4''"))
		((= diam "3-8''")(setq diam "3/8''"))
		((= diam "1-2''")(setq diam "1/2''"))
	        ((= diam "5-8''")(setq diam "5/8''"))
	      	((= diam "3-4''")(setq diam "3/4''"))
	      	((= diam "7-8''")(setq diam "7/8''")))
   
  (setq unitario2 (vunit (vdif punto2t punto1t))); vector unitario en dirección punto1t punto2t; los comandos vunit y vdif, son personalizados, ver archivo "operadores varios"
  ;aqui determinamos un vector unitario perpendicular a unitario2, en el plano son 2 vectores unitarios, se elige uno, tal que el vector punto1t punto2t
  ;arroje una proyección positiva sobre el vector unitario.
  (setq ayudante1 (list (cadr unitario2) (- 0 (car unitario2)) 0));vector unitario perpendicular a unitario2
  (setq ayudante2 (list (- 0 (cadr unitario2)) (car unitario2) 0));vector unitario perpendicular a unitario2
  
  (if (> (vmesc (vdif punto2l punto1l) ayudante1) 0);vmesc es un comando personalizado
    	(setq unitario1 ayudante1)
    	(setq unitario1 ayudante2))
  ;determinando los puntos eextremos del eje donde se distribuirán los estribos; vdif, vavg, vmesc, vprod y vsum son comandos personalizados
  (setq punto1l (vsum punto1l (vprod unitario2 (vmesc unitario2 (vdif (vavg punto1t punto2t) punto1l))))); P1'=P1+unitario2.[((puntomedio punto1t punto2t)-punto1l).unitario2]	
  (setq punto2l (vsum punto2l (vprod unitario2 (vmesc unitario2 (vdif (vavg punto1t punto2t) punto2l))))); P2'=P2+unitario2.[((puntomedio punto1t punto2t)-punto2l).unitario2]	
  ;determinando la longitud de los estribos
  (setq destribo (distance punto1t punto2t))
 
  (cond ((= D "Dos")
    	 (estribos2d punto1l punto2l destribo unitario1 unitario2 diam))
	(t (estribos1d punto1l punto2l destribo unitario1 unitario2 diam)));estribos1d y estribos2d son personalizadas, ver más arriba
)
;*************************************************FIN DE FUNCIÓN PRINCIPAL***********************************************************************************************

