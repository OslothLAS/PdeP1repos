
class Campo {
	const cultivos = #{}
	//const parcelas = #{}	
	var tamano
	
	method maximoPlantas()
	
	method costoMantenimiento() 
	
	method esCampoAbierto()
	
	method esRico()
	
	method mediaNutricional(){
		return cultivos.sum{cultivo => cultivo.valorNutricional(self)}
	}
	
	method valorNeto(){
		return self.valorTotalDeCultivos() - self.costoMantenimiento()
	}
	
	method valorTotalDeCultivos(){				
		return cultivos.sum{cultivo => cultivo.precioVenta(self)}
	}
	
	method plantarCultivo(planta){
		if(self.cantidadPlantadas() < self.maximoPlantas()){
		self.agregarPlanta(planta)
		}
		throw new Exception(message = "Excede su capacidad")
	}
	
	method agregarPlanta(planta){
		cultivos.add(planta)
	}
	
	method cantidadPlantadas(){
		return cultivos.size()
	}
	
	
}

class CampoAbierto inherits Campo{
	var riquezaDelSuelo

	override method esCampoAbierto() = true

	override method costoMantenimiento() {
		return tamano * 500
	}
	
	override method maximoPlantas() = 4*tamano
	
	override method esRico() = riquezaDelSuelo > 100
	
	
}

class Invernaderos inherits Campo {	
	var maximoVariable
	var dispositivo
	const mantenimientoBase = 50000
	
	override method esCampoAbierto() = true
	
	override method costoMantenimiento() {
		return mantenimientoBase + dispositivo.costoDispositivo()
	}
	
	override method maximoPlantas() = maximoVariable 
	//Por el momento, esta solucion sirve
	
	
	override method esRico() {
		return 
		not self.alcanzaCapacidadMaxima() or
		dispositivo.equals(reguladorNutricional) or		
		dispositivo.humedadEntre(20, 40)
	}

	method alcanzaCapacidadMaxima(){
		return cultivos.size() < self.maximoPlantas()
	}
	

}

class Dispositivo {
	
	method costoDispositivo(){return 0}	
}

object reguladorNutricional inherits Dispositivo{
	
	override method costoDispositivo() = 2000 //Posible repeticion de logica
}

object panelesSolares inherits Dispositivo{
	
	override method costoDispositivo() = -25000 //Posible repeticion de logica
}

class Humidificador inherits Dispositivo{	
	const humedad 
	override method costoDispositivo() = if(humedad <=30) 1000 else 4500
	
	method humedadEntre(valorMinimo, valorMaximo){
		return humedad.between(valorMinimo, valorMaximo)
	}
	
}


class Cultivo {

	
	method puedePlantarse(terreno)
	
	method precioDeVenta(terreno)
	
	method valorNutricional(terreno) 
}

class Papa inherits Cultivo {
		const valorNutricionalBase = 1500
	
	override method precioDeVenta(terreno) = self.valorNutricional(terreno)/2
			
	override method valorNutricional(terreno) {
		if(terreno.esRico()){		
			return valorNutricionalBase *2
		}
		else{return valorNutricionalBase}
	}
	override method puedePlantarse(terreno) = true //Probablemente sea muy funcional
	
}

class Algodon inherits Cultivo {
	
	override method precioDeVenta(terreno) = 500
	
	override method puedePlantarse(terreno)= terreno.esRico()
	
	override method valorNutricional(terreno) = 0

}

class ArbolFrutal  inherits Cultivo {
	const edad
	const fruta
	
	override method valorNutricional(terreno) = if(edad*3 < 4000) 3*edad else 4000
	//El maximo valor es el de el valorNutricional
	
	override method puedePlantarse(terreno)= terreno.esCampoAbierto()
	
	override method precioDeVenta(terreno) = return fruta.precio()
	
}

class PalmeraTropical inherits ArbolFrutal{
		
	override method puedePlantarse(terreno)= super(terreno) and terreno.esRico()
	
	override method precioDeVenta(terreno) = super(terreno)*5
}

class Fruta{
	const property precio
}




