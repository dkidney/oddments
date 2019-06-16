my.model.matrix = function(formula, data){
    
    terms = colnames(attr(terms(formula), "factors")) ; terms
    
    X = sapply(terms, function(i){ # i = terms[3] ; i
        
        if(grepl(":", i)){
            
            j = strsplit(i, ":")[[1]] ; j
            
            apply(data[,j],1,prod)
            
        }else{
            
            data[,terms == i]
            
        }
        
    })
    
    colnames(X) = terms
    
    return(X)
    
}

if(0){
    
    form = ~ x * z
    
    n = 100000
    
    data = data.frame(a = 1:n, b = 1:n, c = 1:n, x = 1:n, z = n:1)
    
    X = model.matrix(form, data) ; head(X) ; dim(X)
    
    X = my.model.matrix(form, data) ; head(X) ; dim(X)
    
    
    
}