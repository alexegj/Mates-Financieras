1. Valor futuro

```{r}
InteresCompuestoVF = function(va, r, n){
  VF = va*(1 + r) ^ n 
  return(VF)
}
```

2. Valor actual

```{r}
InteresCompuestoVA = function(vf, r, n){
  VA = vf/(1 + r) ^ n
  return(VA)
}
```

3. Tasa de interes

```{r}
InteresCompuestoTasa = function(vf, va, n){
  r = ((vf/va) ^ (1/n)) - 1
  return(r)
}
```

4. Número de Periodos

```{r}
InteresCompuestoNPer = function(vf, va, r){
  n = log(vf/va)/log(1 + r)
  return(n)
}
```

5. Función para llamar a las demás
```{r}
InteresCompuesto = function(vf = NA, va = NA, r = NA, n = NA){
  if (is.na(vf)){
    resultado = InteresCompuestoVF(va, r, n)
    return(resultado)
    
  } else if (is.na(va)){
    resultado = InteresCompuestoVA(vf, r, n)
    return(resultado)
    
  } else if (is.na(r)){
    resultado = InteresCompuestoTasa(vf, va, n)
    return(resultado)
    
  } else if (is.na(n)){
    resultado = InteresCompuestoNPer(vf, va, r)
    return(resultado)
    
  } else {
    print("Tienes que dejar un valor con NA para que la función pueda calcular")
  }
}


