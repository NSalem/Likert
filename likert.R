v_escala_likert = function(data, vmin = 1, vmax = 5, mostrar_todo = FALSE){
    #Devuelve los valores de escala del procedimiento de Edwards para un conjunto de �tems
    #tipo likert.Para cada �tem se muestra la correlaci�n de los valores hallados con los 
    #valores que se pretende utilizar.
    
    #data es un dataframe en el que cada celda representa la respuesta en una escala
    #tipo Likert de un sujeto (fila) a un �tem (columna).Cada columna debe ser una variable 
    #num�rica o un factor (esto �ltimo si s�lo contiene n�meros o se trata de un factor ordenado.
    #Introducir ?factor en consola para ver ayuda de R sobre factores ordenados) 

    #vmin y vmax son los n�meros enteros que representan el m�nimo y el m�ximo nivel de respuesta 
    #posible en los datos si las variables son num�ricas. 
    #Ejemplo: Si tenemos unos datos en los que las respuestas a los �tems aparecen como n�meros de 1 a 5
    #vmin debe ser 1 y vmax debe ser 5. Si estos aparecen de 0 a 4, vmin debe ser 0 y vmax 4.
    
    #Si los datos contienen factores ordenados, vmin y vmax tan s�lo deben ser dos valores de 
    #forma que vmax - vmin sea igual al n�mero de categor�as de respuesta. 
    #En este caso los niveles de respuesta ser�n etiquetados con n�meros desde vmin a vmax.
    
    #vmax - vmin debe ser igual al n�mero de categor�as de respuesta existentes.
    
    #mostrar_todo es un argumento opcional que especifica si se quieren obtener los    
    #pasos intermedios hasta llegar a los valores de escala (frecuencias, frecuencias
    #normalizadas, etc)
    
    
    total = data.frame() #dataframe vac�o al que se van a�adiendo los valores de escala
                         #de cada �tem
    
    if (mostrar_todo == TRUE){
    #Si se ha especificado mostrar_todo = TRUE, crear dataframes vac�os para conservar 
    #los datos de de frecuencias, frecuencias acumuladas,media p acumulada y media p acumulada 
    #normalizada para cada �tem.
    
    #dataframes vac�os, para cada �tem se a�adir� una fila.
    fr_list = data.frame() 
    fr_acum_list = data.frame()
    media_p_acum_list = data.frame()
    z1_list = data.frame()
    }
    
    
    for (i in (1:ncol(data))){
    #Iterar para todas las columnas(�tems) de data. Hallar valores de escala de dicho �tem
    #y a�adirlos a total. A�adir filas a los dataframes "intermedios" si se ha especificado
    #mostrar_todo = TRUE
        
        if (is.factor(data[,i])){ #si la variable es un factor
            fr = table(data[,i])/nrow(data) #frecuencias relativas
        }
        
        else{ #si la variable no es un factor
        fr = table(factor(data[,i], levels=vmin:vmax))/nrow(data) #frecuencias relativas
        #convierte variable en factor y busca las frecuencias para los valores enteros que van 
        #de vmin a vmax. Esto se hace para incluir las frecuencias de 0.
        
        }
        
        fr_acum = cumsum(fr) #relativas acumuladas
        
    
        media_p_acum = fr/2+(c(0,fr_acum[c(-length(fr_acum))])) # media p acumulada = 
        #relativas entre 2 + acumuladas "desplazadas"(poniendo un 0 al principio y 
        #eliminando el �ltimo valor)
        
        z1 = qnorm(media_p_acum) #normalizar lo anterior
        
        min_z1 = min(subset(z1, z1!=-Inf)) # m�nimo de z1 sin contar -Inf.
                                           #(ocurren -Inf al normalizar proporciones de 0)
        
        valores_escala = z1 - rep(min_z1, length(z1)) + vmin
        #resta a cada valor de z1 el m�nimo no infinito de z1, y le suma vmin para situar el 
        #origen en vmin
        
        
        for (i in 1:length(valores_escala)){
            #Convertir valores de infinito y menos infinito en NA para cada valor de escala
            if ((valores_escala[i] == Inf) | (valores_escala[i] == -Inf)) {valores_escala[i] = NA}
        }
        
    
        valores_escala = round(valores_escala) #redondear valores de escala
             
        total = rbind (total, valores_escala) #a�adir filas a total
        
        if (mostrar_todo == TRUE){
            #a�adir filas a los dataframes para pasos intermedios si 
            #mostrar_todo es verdadero.
            
            fr_list = rbind(fr_list, fr)
            
            fr_acum_list = rbind(fr_acum_list, fr_acum)
            
            media_p_acum_list = rbind(media_p_acum_list, media_p_acum)
            
            z1_list = rbind(z1_list, z1) 
            names(fr_list) = c(vmin:vmax) #poner nombres de n�meros a los niveles
            names(fr_acum_list) = c(vmin:vmax) #poner nombres de n�meros a los niveles
            names(media_p_acum_list) = c(vmin:vmax) #poner nombres de n�meros a los niveles
            names(z1_list) = c(vmin:vmax) #poner nombres de n�meros a los niveles
        } 
        
    }
    
    names(total) = c(vmin:vmax) #poner nombres de n�meros a los niveles
    
    rownames(total) = colnames(data) # poner nombre a los �tems
    
    correlaciones = apply(total,1,function(x) cor(x,c(vmin:vmax))) 
    #correlaci�n de los valores hallados para cada �tem con los valores en secuencia de
    #1 a niveles.
    
    total = cbind(total, correlaciones)
    #a�adir columna con las correlaciones a los valores de escala de los �tems
    
    if (mostrar_todo == FALSE){
        #Si mostrar_todo es falso (por defecto) devolver lista de valores
    return (total) 
    }
    
    if (mostrar_todo == TRUE){
        #Si mostrar_todo es verdadero, devolver lista con frecuencias, frecuencias 
        #acumuladas, media p acumulada, media p acumulada normalizada y valores de 
        #escala
        return (list("frecuencias relativas" = fr_list, "frecuencias relativas acumuladas"=
                     fr_acum_list, "media p acumulada" = media_p_acum_list, 
                     "media p acumulada normalizada" = z1_list, "valores de escala" = total))
        
    } 
}