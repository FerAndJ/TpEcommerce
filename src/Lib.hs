type Producto = (String, Int) 

nombreDelProducto :: Producto -> String
nombreDelProducto (unNombre, _) = unNombre

precioDelProducto :: Producto -> Int
precioDelProducto (_, unPrecio) = unPrecio

precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precioUnitario cantidad descuento costoDeEnvio = aplicarCostoDeEnvio (aplicarDescuento precioUnitario descuento * cantidad) costoDeEnvio

productoDeElite :: Producto -> Bool                                         
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto && (not . productoCorriente) unProducto

aplicarDescuento :: Float -> Float -> Float                                          
aplicarDescuento unPrecio unDescuento = unPrecio * ((0.01*). (100-) $ unDescuento)

entregaSencilla :: String -> Bool                                           
entregaSencilla unDia = even . length $ unDia

descodiciarProducto :: Producto -> Producto                                   
descodiciarProducto unProducto = (take 10 (nombreDelProducto unProducto), precioDelProducto unProducto)

productoDeLujo :: Producto -> Bool                                          
productoDeLujo unProducto = elem 'x' (nombreDelProducto unProducto) || elem 'z' (nombreDelProducto unProducto)  

aplicarCostoDeEnvio :: Float -> Float -> Float                              
aplicarCostoDeEnvio unPrecio unCostoDeEnvio = unPrecio + unCostoDeEnvio

productoCodiciado :: Producto -> Bool                                       
productoCodiciado unProducto = length (nombreDelProducto unProducto) > 10

productoCorriente :: Producto -> Bool                                       
productoCorriente unProducto = esVocal . head . nombreDelProducto $ unProducto

esVocal :: Char -> Bool                                                     
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> Producto                                           
productoXL unProducto = (nombreDelProducto unProducto ++ " XL", precioDelProducto unProducto)

versionBarata :: Producto -> Producto                                         
versionBarata unProducto = (reverse . nombreDelProducto . descodiciarProducto $ unProducto, precioDelProducto unProducto)
