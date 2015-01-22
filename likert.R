v_escala_likert = function(data, niveles, mostrar_todo = FALSE){
    
    total = data.frame() # dataframe vacio
    fr_list = data.frame()
    fr_acum_list = data.frame()
    media_p_acum_list = data.frame()
    z1_list = data.frame()
    
    for (i in (1:ncol(data))){ #proceso para cada ítem
        
        if (is.factor(data[,i])){
            fr = table(data[,i])/nrow(data) #frecuencias relativas
        }
        
        else{
        fr = table(factor(data[,i], levels=1:niveles))/nrow(data) #frecuencias relativas    
        }
        fr_acum = cumsum(fr) #relativas acumuladas
        
        media_p_acum = fr/2+(c(0,fr_acum[c(-length(fr_acum))])) # media p acumulada. relativas entre 2 + acumuladas "desplazadas"
        
        z1 = qnorm(media_p_acum) #z de lo anterior
        
        min_z1 = min(subset(z1, z1!=-Inf)) # mínimo de z1 evitando infinitos por hacer qnorm de 0
        
        valores_escala = z1 #valores de escala antes de poner 0 como mínimo.
        
        if (min_z1<0){
            valores_escala = z1 - rep(min_z1, length(z1)) #poner el mínimo en 0.
        }
        
        #for (i in 1:length(valores_escala)){
        #    if ((valores_escala[i] == Inf) | (valores_escala[i] == -Inf)) {valores_escala[i] = NA}
        #}
        
        for (i in 1:length(valores_escala)){
            
            if (valores_escala[i] == Inf) {valores_escala[i] = valores_escala[(i-1)]}
            if (valores_escala[i] == -Inf){valores_escala[i] = valores_escala[(i+1)]}

        }
        
        for (i in 1:length(valores_escala)){
            
            if (valores_escala[i] == Inf) {valores_escala[i] = valores_escala[(i-1)]}
            if (valores_escala[i] == -Inf){valores_escala[i] = valores_escala[(i+1)]}
            
        }
        
        valores_escala = round(valores_escala) #redondear
         
         for (i in 1:length(valores_escala)){
             
             if (valores_escala[i] == Inf) {valores_escala[i] = valores_escala[(i-1)]}
             if (valores_escala[i] == -Inf){valores_escala[i] = valores_escala[(i+1)]}
             
         }
        
        total = rbind (total, valores_escala) 
        
        fr_list = rbind(fr_list, fr)
        
        fr_acum_list = rbind(fr_acum_list, fr_acum)
        
        media_p_acum_list = rbind(media_p_acum_list, media_p_acum)
        
        z1_list = rbind(z1_list, z1)
        
        names(fr_list) = c(1:niveles)
        names(fr_acum_list) = c(1:niveles)
        names(media_p_acum_list) = c(1:niveles)
        names(z1_list) = c(1:niveles)
        
        
    }
    names(total) = c(1:ncol(total)) #poner nombres de números a los niveles
    
    rownames(total) = colnames(data) # poner nombre a los ítems
    
    correlaciones = apply(total,1,function(x) cor(x,c(1:niveles)))
    
    total = cbind(total, correlaciones)
    
    if (mostrar_todo == FALSE){
    return (total) # devolver lista de valores
    }
    
    if (mostrar_todo == TRUE){
        return (list("frecuencias relativas" = fr_list, "frecuencias relativas acumuladas"=
                     fr_acum_list, "media p acumulada" = media_p_acum_list, 
                     "media p acumulada normalizada" = z1_list, total))
    } 
}
